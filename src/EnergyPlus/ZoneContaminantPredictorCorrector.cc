// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::ZoneContaminantPredictorCorrector {

// MODULE INFORMATION:
//       AUTHOR         Lixing Gu
//       DATE WRITTEN   May, 2010
//       MODIFIED       None
//       RE-ENGINEERED  None

// PURPOSE OF THIS MODULE:
// This module contains routines to predict and correct zone contaminants.
//  also includes zone contaminant controlling

// METHODOLOGY EMPLOYED:
// Similar approach to ZoneTempPredictorCorrector

// Using/Aliasing
using namespace DataHVACGlobals;
using namespace DataHeatBalance;
using namespace DataHeatBalFanSys;
using namespace Psychrometrics;
using namespace HybridModel;
using ScheduleManager::GetCurrentScheduleValue;
using ZoneTempPredictorCorrector::DownInterpolate4HistoryValues;

void ManageZoneContaminanUpdates(EnergyPlusData &state,
                                 DataHeatBalFanSys::PredictorCorrectorCtrl const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                                 bool const ShortenTimeStepSys,
                                 bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                                 Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July, 2010
    //       MODIFIED       na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine predicts or corrects the zone air temperature
    // depending on the simulation status and determines the correct
    // temperature setpoint for each zone from the schedule manager.
    // This module is revised from subroutine ManageZoneAirUpdates in
    // ZoneTempPredictorCorrector module.

    if (state.dataZoneContaminantPredictorCorrector->GetZoneAirContamInputFlag) {
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) GetZoneContaminanInputs(state);
        GetZoneContaminanSetPoints(state);
        state.dataZoneContaminantPredictorCorrector->GetZoneAirContamInputFlag = false;
    }

    if (!state.dataContaminantBalance->Contaminant.SimulateContaminants) return;

    switch (UpdateType) {
    case DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints: {
        InitZoneContSetPoints(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep: {
        PredictZoneContaminants(state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep: {
        CorrectZoneContaminants(state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::RevertZoneTimestepHistories: {
        RevertZoneTimestepHistories(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories: {
        PushZoneTimestepHistories(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories: {
        PushSystemTimestepHistories(state);
    } break;
    default:
        break;
    }
}

void GetZoneContaminanInputs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   Dec. 2011
    //       MODIFIED       NA
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the inputs related to generic contaminant internal gain.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    using DataSurfaces::ExternalEnvironment;
    using General::FindNumberInList;

    using ScheduleManager::CheckScheduleValue;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMaxValue;
    using ScheduleManager::GetScheduleMinValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetSourcesAndSinks: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_string AlphaName;
    Array1D<Real64> IHGNumbers;
    Real64 SchMin;
    Real64 SchMax;
    int NumAlpha;
    int NumNumber;
    int IOStat;
    int MaxAlpha;
    int MaxNumber;
    int Loop;
    int ZonePtr;
    bool ErrorsFound(false);
    Array1D_bool RepVarSet;
    std::string CurrentModuleObject;

    RepVarSet.dimension(state.dataGlobal->NumOfZones, true);

    MaxAlpha = -100;
    MaxNumber = -100;
    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:Constant";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:PressureDriven";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:CutoffModel";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DecaySource";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DepositionRateSink";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
    MaxAlpha = max(MaxAlpha, NumAlpha);
    MaxNumber = max(MaxNumber, NumNumber);
    IHGNumbers.allocate(MaxNumber);
    AlphaName.allocate(MaxAlpha);
    IHGNumbers = 0.0;
    AlphaName = "";

    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:Constant";
    int TotGCGenConstant = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericConstant.allocate(TotGCGenConstant);

    for (Loop = 1; Loop <= TotGCGenConstant; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).ZoneName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).ActualZoneNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
        if (state.dataContaminantBalance->ZoneContamGenericConstant(Loop).ActualZoneNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenerateRate = IHGNumbers(1);
        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCRemovalCoef = IHGNumbers(2);

        state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr = GetScheduleIndex(state, AlphaName(4));
        if (state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " entered=" + AlphaName(4));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(4), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(4) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(4), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        if (state.dataContaminantBalance->ZoneContamGenericConstant(Loop).ActualZoneNum <= 0) continue; // Error, will be caught and terminated later

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Constant Source Generation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericConstant(Loop).Name);

        // Zone total report variables
        ZonePtr = state.dataContaminantBalance->ZoneContamGenericConstant(Loop).ActualZoneNum;
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericConstant(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericConstant(Loop).GCGenRate);
    }

    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:PressureDriven";
    int TotGCGenPDriven = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericPDriven.allocate(TotGCGenPDriven);

    for (Loop = 1; Loop <= TotGCGenPDriven; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.afn->MultizoneSurfaceData, &AirflowNetwork::MultizoneSurfaceProp::SurfName);
        if (state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ShowContinueError(state, "which is not listed in AirflowNetwork:MultiZone:Surface.");
            ErrorsFound = true;
        }
        // Ensure external surface
        if (state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum > 0 &&
            state.dataSurface->Surface(state.afn->MultizoneSurfaceData(state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum).SurfNum)
                    .ExtBoundCond != ExternalEnvironment) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + ". The entered surface (" + AlphaName(2) +
                                ") is not an exterior surface");
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRateCoef = IHGNumbers(1);
        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCExpo = IHGNumbers(2);
        if (IHGNumbers(2) <= 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative or zero value is not allowed for " + state.dataIPShortCut->cNumericFieldNames(2) +
                                " in " + CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(2)));
            ErrorsFound = true;
        }
        if (IHGNumbers(2) > 1.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "The value greater than 1.0 is not allowed for " +
                                state.dataIPShortCut->cNumericFieldNames(2) + " in " + CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(2)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Pressure Driven Generation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).Name);

        if (state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum > 0) {
            ZonePtr = state.dataSurface
                          ->Surface(state.afn->MultizoneSurfaceData(state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).SurfNum).SurfNum)
                          .Zone;
        } else {
            ZonePtr = 0;
        }
        // Zone total report variables
        if (ZonePtr > 0 && RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        if (ZonePtr > 0)
            SetupZoneInternalGain(state,
                                  ZonePtr,
                                  state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).Name,
                                  DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &state.dataContaminantBalance->ZoneContamGenericPDriven(Loop).GCGenRate);
    }

    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:CutoffModel";
    int TotGCGenCutoff = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericCutoff.allocate(TotGCGenCutoff);

    for (Loop = 1; Loop <= TotGCGenCutoff; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).ZoneName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).ActualZoneNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
        if (state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).ActualZoneNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenerateRate = IHGNumbers(1);
        state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCCutoffValue = IHGNumbers(2);

        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }
        if (IHGNumbers(2) <= 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values or zero are not allowed for " + state.dataIPShortCut->cNumericFieldNames(2) +
                                " in " + CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(2)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Cutoff Model Generation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).Name);

        // Zone total report variables
        ZonePtr = state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).ActualZoneNum;
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericCutoff(Loop).GCGenRate);
    }

    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DecaySource";
    int TotGCGenDecay = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericDecay.allocate(TotGCGenDecay);

    for (Loop = 1; Loop <= TotGCGenDecay; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).ZoneName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).ActualZoneNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
        if (state.dataContaminantBalance->ZoneContamGenericDecay(Loop).ActualZoneNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCInitEmiRate = IHGNumbers(1);
        state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCDelayTime = IHGNumbers(2);

        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }
        if (IHGNumbers(2) <= 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values or zero are not allowed for " + state.dataIPShortCut->cNumericFieldNames(2) +
                                " in " + CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(2)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Decay Model Generation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericDecay(Loop).Name);
        SetupOutputVariable(state,
                            "Generic Air Contaminant Decay Model Generation Emission Start Elapsed Time",
                            OutputProcessor::Unit::s,
                            state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCTime,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericDecay(Loop).Name);

        // Zone total report variables
        ZonePtr = state.dataContaminantBalance->ZoneContamGenericDecay(Loop).ActualZoneNum;
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericDecay(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericDecay(Loop).GCGenRate);
    }

    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion";
    int TotGCBLDiff = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericBLDiff.allocate(TotGCBLDiff);

    for (Loop = 1; Loop <= TotGCBLDiff; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.dataSurface->Surface);
        if (state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCTranCoef = IHGNumbers(1);
        state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCHenryCoef = IHGNumbers(2);
        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }
        if (IHGNumbers(2) <= 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values or zero are not allowed for " + state.dataIPShortCut->cNumericFieldNames(2) +
                                " in " + CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(2)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Boundary Layer Diffusion Generation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).Name);
        if (state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfNum > 0) {
            SetupOutputVariable(state,
                                "Generic Air Contaminant Boundary Layer Diffusion Inside Face Concentration",
                                OutputProcessor::Unit::ppm,
                                state.dataSurface->SurfGenericContam(state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfNum),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfName);
        }

        ZonePtr = state.dataSurface->Surface(state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).SurfNum).Zone;
        // Zone total report variables
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericBLDiff(Loop).GCGenRate);
    }

    CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink";
    int TotGCDVS = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericDVS.allocate(TotGCDVS);

    for (Loop = 1; Loop <= TotGCDVS; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericDVS(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericDVS(Loop).SurfName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericDVS(Loop).SurfNum = UtilityRoutines::FindItemInList(AlphaName(2), state.dataSurface->Surface);
        if (state.dataContaminantBalance->ZoneContamGenericDVS(Loop).SurfNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCDepoVeloPtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCDepoVeloPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCDepoVeloPtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCDepoVeloPtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCDepoVelo = IHGNumbers(1);
        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Deposition Velocity Removal Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericDVS(Loop).Name);

        ZonePtr = state.dataSurface->Surface(state.dataContaminantBalance->ZoneContamGenericDVS(Loop).SurfNum).Zone;
        // Zone total report variables
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericDVS(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericDVS(Loop).GCGenRate);
    }

    CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DepositionRateSink";
    int TotGCDRS = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataContaminantBalance->ZoneContamGenericDRS.allocate(TotGCDRS);

    for (Loop = 1; Loop <= TotGCDRS; ++Loop) {
        AlphaName = "";
        IHGNumbers = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 AlphaName,
                                                                 NumAlpha,
                                                                 IHGNumbers,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);
        state.dataContaminantBalance->ZoneContamGenericDRS(Loop).Name = AlphaName(1);

        state.dataContaminantBalance->ZoneContamGenericDRS(Loop).ZoneName = AlphaName(2);
        state.dataContaminantBalance->ZoneContamGenericDRS(Loop).ActualZoneNum =
            UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
        if (state.dataContaminantBalance->ZoneContamGenericDRS(Loop).ActualZoneNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
            ErrorsFound = true;
        }

        state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCDepoRatePtr = GetScheduleIndex(state, AlphaName(3));
        if (state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCDepoRatePtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
            }
            ErrorsFound = true;
        } else { // check min/max on schedule
            SchMin = GetScheduleMinValue(state, state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCDepoRatePtr);
            SchMax = GetScheduleMaxValue(state, state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCDepoRatePtr);
            if (SchMin < 0.0 || SchMax < 0.0) {
                if (SchMin < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                    ErrorsFound = true;
                }
                if (SchMax < 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                    ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                    ErrorsFound = true;
                }
            }
        }

        state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCDepoRate = IHGNumbers(1);

        if (IHGNumbers(1) < 0.0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "Negative values are not allowed for " + state.dataIPShortCut->cNumericFieldNames(1) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
            ShowContinueError(state, format("The input value is {:.2R}", IHGNumbers(1)));
            ErrorsFound = true;
        }

        // Object report variables
        SetupOutputVariable(state,
                            "Generic Air Contaminant Deposition Rate Removal Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCGenRate,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataContaminantBalance->ZoneContamGenericDRS(Loop).Name);

        ZonePtr = state.dataContaminantBalance->ZoneContamGenericDRS(Loop).ActualZoneNum;
        // Zone total report variables
        if (RepVarSet(ZonePtr)) {
            RepVarSet(ZonePtr) = false;
            SetupOutputVariable(state,
                                "Zone Generic Air Contaminant Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnRpt(ZonePtr).GCRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(ZonePtr).Name);
        }
        SetupZoneInternalGain(state,
                              ZonePtr,
                              state.dataContaminantBalance->ZoneContamGenericDRS(Loop).Name,
                              DataHeatBalance::IntGainType::ZoneContaminantSourceAndSinkGenericContam,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              &state.dataContaminantBalance->ZoneContamGenericDRS(Loop).GCGenRate);
    }

    RepVarSet.deallocate();
    IHGNumbers.deallocate();
    AlphaName.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, "Errors getting Zone Contaminant Sources and Sinks input data.  Preceding condition(s) cause termination.");
    }
}

void GetZoneContaminanSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2010
    //       MODIFIED       NA
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the inputs related to contaminant control.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing

    using ScheduleManager::CheckScheduleValue;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMaxValue;
    using ScheduleManager::GetScheduleMinValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ContControlledZoneNum; // The Splitter that you are currently loading input into
    int NumAlphas;
    int NumNums;
    int IOStat;
    bool ErrorsFound(false);
    bool ValidScheduleType;

    struct NeededControlTypes
    {
        // Members
        Array1D_bool MustHave; // 4= the four control types
        Array1D_bool DidHave;

        // Default Constructor
        NeededControlTypes() : MustHave(4, false), DidHave(4, false)
        {
        }
    };

    struct NeededComfortControlTypes
    {
        // Members
        Array1D_bool MustHave; // 4= the four control types
        Array1D_bool DidHave;

        // Default Constructor
        NeededComfortControlTypes() : MustHave(12, false), DidHave(12, false)
        {
        }
    };
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "ZoneControl:ContaminantController";
    int NumContControlledZones = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (NumContControlledZones > 0) {
        state.dataContaminantBalance->ContaminantControlledZone.allocate(NumContControlledZones);
    }

    for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 ContControlledZoneNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneName = state.dataIPShortCut->cAlphaArgs(2);
        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum =
            UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == 0) {
            ShowSevereError(state,
                            cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        } else {
            //      Zone(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)%TempControlledZoneIndex = ContControlledZoneNum
        }

        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedule = state.dataIPShortCut->cAlphaArgs(3);
        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr =
                DataGlobalConstants::ScheduleAlwaysOn; // (Returns 1.0)
        } else {
            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\" not found.");
                ErrorsFound = true;
            } else {
                // Check validity of control types.
                ValidScheduleType = CheckScheduleValueMinMax(
                    state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr, ">=", 0.0, "<=", 1.0);
                if (!ValidScheduleType) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid range " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\"");
                    ShowContinueError(state, "..contains values outside of range [0,1].");
                    ErrorsFound = true;
                } else {
                    state.dataHeatBal->Zone(state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum)
                        .ZoneContamControllerSchedIndex = state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr;
                }
            }
        }

        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).SetPointSchedName = state.dataIPShortCut->cAlphaArgs(4);
        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex =
            GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex == 0) {
            ShowSevereError(state,
                            cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\" not found.");
            ErrorsFound = true;
        } else {
            // Check validity of control types.
            ValidScheduleType = CheckScheduleValueMinMax(
                state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex, ">=", 0.0, "<=", 2000.0);
            if (!ValidScheduleType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid range " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\"");
                ShowContinueError(state, "..contains values outside of range [0,2000 ppm].");
                ErrorsFound = true;
            }
        }

        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedName = state.dataIPShortCut->cAlphaArgs(5);
        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex =
            GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex > 0) {
            // Check validity of control types.
            ValidScheduleType = CheckScheduleValueMinMax(
                state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex, ">=", 0.0, "<=", 2000.0);
            if (!ValidScheduleType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid range " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\"");
                ShowContinueError(state, "..contains values outside of range [0,2000 ppm].");
                ErrorsFound = true;
            } else {
                state.dataHeatBal->Zone(state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum)
                    .ZoneMinCO2SchedIndex = state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex;
            }
        }

        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedName = state.dataIPShortCut->cAlphaArgs(6);
        state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex =
            GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex > 0) {
            // Check validity of control types.
            ValidScheduleType = CheckScheduleValueMinMax(
                state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex, ">=", 0.0, "<=", 2000.0);
            if (!ValidScheduleType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid range " +
                                    state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + state.dataIPShortCut->cAlphaArgs(6) + "\"");
                ShowContinueError(state, "..contains values outside of range [0,2000 ppm].");
                ErrorsFound = true;
            } else {
                state.dataHeatBal->Zone(state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum)
                    .ZoneMaxCO2SchedIndex = state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex;
            }
        }

        if (NumAlphas > 6) {
            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedule = state.dataIPShortCut->cAlphaArgs(7);
            if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr =
                    GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7));
                if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Check validity of control types.
                    ValidScheduleType = CheckScheduleValueMinMax(
                        state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr, ">=", 0.0, "<=", 1.0);
                    if (!ValidScheduleType) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid range " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\"");
                        ShowContinueError(state, "..contains values outside of range [0,1].");
                        ErrorsFound = true;
                    }
                }
            }
            if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataIPShortCut->cAlphaArgs(8) + "\" is required, but blank.");
                ErrorsFound = true;
            } else {
                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCSetPointSchedName =
                    state.dataIPShortCut->cAlphaArgs(8);
                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCSPSchedIndex =
                    GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
                if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).GCSPSchedIndex == 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) + "\" not found.");
                    ErrorsFound = true;
                }
            }
        }

    } // ContControlledZoneNum

    if (ErrorsFound) {
        ShowFatalError(state, "Errors getting Zone Contaminant Control input data.  Preceding condition(s) cause termination.");
    }
}

void InitZoneContSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2010
    //       MODIFIED       NA
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the data for the zone air contaminant setpoints.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    using InternalHeatGains::SumAllInternalCO2Gains;
    using InternalHeatGains::SumAllInternalCO2GainsExceptPeople; // Added for hybrid model
    using InternalHeatGains::SumInternalCO2GainsByTypes;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int SurfNum;
    Real64 GCGain; // Zone generic contaminant gain
    Real64 Pi;     // Pressue at zone i
    Real64 Pj;     // Pressue at zone j
    Real64 Sch;    // Schedule value
    Real64 Cs;     // Surface concentration level for the Boundary Layer Diffusion Controlled Model
    int ContZoneNum;
    int I;
    bool ErrorsFound(false);

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataContaminantBalance->OutdoorCO2 = GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr);
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataContaminantBalance->OutdoorGC =
            GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr);
    }

    if (state.dataZoneContaminantPredictorCorrector->MyOneTimeFlag) {
        // CO2
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->ZoneCO2SetPoint.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2PredictedRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus1Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus2Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CO2ZoneTimeMinus3Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneCO2MX.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneCO2M2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneCO21.dimension(state.dataGlobal->NumOfZones, 0.0);

            state.dataContaminantBalance->ZoneSysContDemand.allocate(state.dataGlobal->NumOfZones);
            state.dataContaminantBalance->ZoneCO2Gain.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneCO2GainFromPeople.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneCO2GainExceptPeople.dimension(state.dataGlobal->NumOfZones, 0.0); // Added for hybrid model
            state.dataContaminantBalance->MixingMassFlowCO2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneAirDensityCO.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->AZ.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->BZ.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CZ.dimension(state.dataGlobal->NumOfZones, 0.0);
        }

        state.dataContaminantBalance->CONTRAT.dimension(state.dataGlobal->NumOfZones, 0.0);

        // Allocate Derived Types

        for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            // Zone CO2
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                SetupOutputVariable(state,
                                    "Zone Air CO2 Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataContaminantBalance->ZoneAirCO2(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                                    "Zone Air CO2 Predicted Load to Setpoint Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataContaminantBalance->CO2PredictedRate(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                                    "Zone Air CO2 Setpoint Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataContaminantBalance->ZoneCO2SetPoint(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                                    "Zone Air CO2 Internal Gain Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataContaminantBalance->ZoneCO2Gain(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
            }

        } // Loop

        // Generic contaminant
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->ZoneGCSetPoint.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCPredictedRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSGCZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSGCZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSGCZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->DSGCZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus1Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus2Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->GCZoneTimeMinus3Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneGCMX.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneGCM2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneGC1.dimension(state.dataGlobal->NumOfZones, 0.0);

            if (!allocated(state.dataContaminantBalance->ZoneSysContDemand))
                state.dataContaminantBalance->ZoneSysContDemand.allocate(state.dataGlobal->NumOfZones);
            state.dataContaminantBalance->ZoneGCGain.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->MixingMassFlowGC.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->ZoneAirDensityGC.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->AZGC.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->BZGC.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataContaminantBalance->CZGC.dimension(state.dataGlobal->NumOfZones, 0.0);
        }

        state.dataContaminantBalance->CONTRATGC.dimension(state.dataGlobal->NumOfZones, 0.0);

        // Allocate Derived Types

        for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            // Zone CO2
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                SetupOutputVariable(state,
                                    "Zone Air Generic Air Contaminant Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataContaminantBalance->ZoneAirGC(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                                    "Zone Generic Air Contaminant Predicted Load to Setpoint Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataContaminantBalance->GCPredictedRate(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                                    "Zone Generic Air Contaminant Setpoint Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataContaminantBalance->ZoneGCSetPoint(Loop),
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(Loop).Name);
            }
        } // Loop

        state.dataZoneContaminantPredictorCorrector->MyOneTimeFlag = false;
    }

    // Do the Begin Environment initializations
    if (state.dataZoneContaminantPredictorCorrector->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->CONTRAT = 0.0;
            state.dataContaminantBalance->CO2ZoneTimeMinus1 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->CO2ZoneTimeMinus2 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->CO2ZoneTimeMinus3 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->CO2ZoneTimeMinus4 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->DSCO2ZoneTimeMinus1 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->DSCO2ZoneTimeMinus2 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->DSCO2ZoneTimeMinus3 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->DSCO2ZoneTimeMinus4 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->CO2ZoneTimeMinus1Temp = 0.0;
            state.dataContaminantBalance->CO2ZoneTimeMinus2Temp = 0.0;
            state.dataContaminantBalance->CO2ZoneTimeMinus3Temp = 0.0;
            state.dataContaminantBalance->ZoneAirCO2Temp = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->ZoneCO2SetPoint = 0.0;
            state.dataContaminantBalance->CO2PredictedRate = 0.0;
            state.dataContaminantBalance->ZoneAirCO2 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->ZoneCO21 = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->ZoneCO2MX = state.dataContaminantBalance->OutdoorCO2;
            state.dataContaminantBalance->ZoneCO2M2 = state.dataContaminantBalance->OutdoorCO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->CONTRAT = 0.0;
            state.dataContaminantBalance->GCZoneTimeMinus1 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->GCZoneTimeMinus2 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->GCZoneTimeMinus3 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->GCZoneTimeMinus4 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->DSGCZoneTimeMinus1 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->DSGCZoneTimeMinus2 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->DSGCZoneTimeMinus3 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->DSGCZoneTimeMinus4 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->GCZoneTimeMinus1Temp = 0.0;
            state.dataContaminantBalance->GCZoneTimeMinus2Temp = 0.0;
            state.dataContaminantBalance->GCZoneTimeMinus3Temp = 0.0;
            state.dataContaminantBalance->ZoneAirGCTemp = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->ZoneGCSetPoint = 0.0;
            state.dataContaminantBalance->GCPredictedRate = 0.0;
            state.dataContaminantBalance->ZoneAirGC = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->ZoneGC1 = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->ZoneGCMX = state.dataContaminantBalance->OutdoorGC;
            state.dataContaminantBalance->ZoneGCM2 = state.dataContaminantBalance->OutdoorGC;
            for (auto &con : state.dataContaminantBalance->ZoneContamGenericBLDiff) {
                state.dataSurface->SurfGenericContam(con.SurfNum) = state.dataContaminantBalance->OutdoorGC;
            }
            if (!state.dataContaminantBalance->ZoneContamGenericDecay.empty())
                for (auto &e : state.dataContaminantBalance->ZoneContamGenericDecay)
                    e.GCTime = 0.0;
        }
        state.dataZoneContaminantPredictorCorrector->MyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZoneContaminantPredictorCorrector->MyEnvrnFlag = true;
    }

    if (allocated(state.dataZoneEquip->ZoneEquipConfig) && state.dataZoneContaminantPredictorCorrector->MyConfigOneTimeFlag) {
        for (ContZoneNum = 1; ContZoneNum <= (int)state.dataContaminantBalance->ContaminantControlledZone.size(); ++ContZoneNum) {
            ZoneNum = state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).ActualZoneNum;
            for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes; ++zoneInNode) {
                int AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(zoneInNode);
                state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).NumOfZones = 0;
                for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(Loop).IsControlled) continue;
                    for (int zoneInNode2 = 1; zoneInNode2 <= state.dataZoneEquip->ZoneEquipConfig(Loop).NumInletNodes; ++zoneInNode2) {
                        if (AirLoopNum == state.dataZoneEquip->ZoneEquipConfig(Loop).InletNodeAirLoopNum(zoneInNode2)) {
                            ++state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).NumOfZones;
                            break; // only count a zone once
                        }
                    }
                }
                if (state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).NumOfZones > 0) {
                    state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum)
                        .ControlZoneNum.allocate(state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).NumOfZones);
                    I = 1;
                    for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(Loop).IsControlled) continue;
                        for (int zoneInNode2 = 1; zoneInNode2 <= state.dataZoneEquip->ZoneEquipConfig(Loop).NumInletNodes; ++zoneInNode2) {
                            if (AirLoopNum == state.dataZoneEquip->ZoneEquipConfig(Loop).InletNodeAirLoopNum(zoneInNode2)) {
                                state.dataContaminantBalance->ContaminantControlledZone(ContZoneNum).ControlZoneNum(I) = Loop;
                                ++I;
                                break; // only count a zone once
                            }
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    "ZoneControl:ContaminantController: a corresponding AirLoopHVAC is not found for the controlled zone =" +
                                        state.dataHeatBal->Zone(ZoneNum).Name);
                    ErrorsFound = true;
                }
            }
        }
        state.dataZoneContaminantPredictorCorrector->MyConfigOneTimeFlag = false;
        if (ErrorsFound) {
            ShowFatalError(state, "ZoneControl:ContaminantController: Program terminates for preceding reason(s).");
        }
    }

    for (int Loop = 1; Loop <= (int)state.dataContaminantBalance->ContaminantControlledZone.size(); ++Loop) {
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            ZoneNum = state.dataContaminantBalance->ContaminantControlledZone(Loop).ActualZoneNum;
            state.dataContaminantBalance->ZoneCO2SetPoint(ZoneNum) =
                GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(Loop).SPSchedIndex);
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            ZoneNum = state.dataContaminantBalance->ContaminantControlledZone(Loop).ActualZoneNum;
            state.dataContaminantBalance->ZoneGCSetPoint(ZoneNum) =
                GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(Loop).GCSPSchedIndex);
        }
    }

    // CO2 gain
    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            state.dataContaminantBalance->ZoneCO2Gain(Loop) = SumAllInternalCO2Gains(state, Loop);
            if (state.dataHybridModel->FlagHybridModel_PC) {
                state.dataContaminantBalance->ZoneCO2GainExceptPeople(Loop) = SumAllInternalCO2GainsExceptPeople(state, Loop);
            }
            std::array<DataHeatBalance::IntGainType, 1> IntGainPeopleArray = {DataHeatBalance::IntGainType::People};
            state.dataContaminantBalance->ZoneCO2GainFromPeople(Loop) = SumInternalCO2GainsByTypes(state, Loop, IntGainPeopleArray);
        }
    }

    // Generic contaminant gain
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataContaminantBalance->ZoneGCGain = 0.0;
        // from constant model
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericConstant) {
            ZoneNum = con.ActualZoneNum;
            GCGain = con.GCGenerateRate * GetCurrentScheduleValue(state, con.GCGenerateRateSchedPtr) -
                     con.GCRemovalCoef * GetCurrentScheduleValue(state, con.GCRemovalCoefSchedPtr) *
                         state.dataContaminantBalance->ZoneAirGC(ZoneNum) * 1.0e-6;
            con.GCGenRate = GCGain;
        }

        // from pressure driven model
        if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
            for (auto &con : state.dataContaminantBalance->ZoneContamGenericPDriven) {
                SurfNum = con.SurfNum;
                Pi = state.afn->AirflowNetworkNodeSimu(state.afn->MultizoneSurfaceData(SurfNum).NodeNums[0]).PZ;
                Pj = state.afn->AirflowNetworkNodeSimu(state.afn->MultizoneSurfaceData(SurfNum).NodeNums[1]).PZ;
                if (Pj >= Pi) {
                    GCGain = con.GCGenRateCoef * GetCurrentScheduleValue(state, con.GCGenRateCoefSchedPtr) * std::pow(Pj - Pi, con.GCExpo);
                } else {
                    GCGain = 0.0;
                }
                con.GCGenRate = GCGain;
            }
        }

        // from cutoff model
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericCutoff) {
            ZoneNum = con.ActualZoneNum;
            if (state.dataContaminantBalance->ZoneAirGC(ZoneNum) < con.GCCutoffValue) {
                GCGain = con.GCGenerateRate * GetCurrentScheduleValue(state, con.GCGenerateRateSchedPtr) *
                         (1.0 - state.dataContaminantBalance->ZoneAirGC(ZoneNum) / con.GCCutoffValue);
            } else {
                GCGain = 0.0;
            }
            con.GCGenRate = GCGain;
        }

        // From decay model
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericDecay) {
            Sch = GetCurrentScheduleValue(state, con.GCEmiRateSchedPtr);
            ZoneNum = con.ActualZoneNum;
            if (Sch == 0.0 || state.dataGlobal->BeginEnvrnFlag || state.dataGlobal->WarmupFlag) {
                con.GCTime = 0.0;
            } else {
                con.GCTime += state.dataGlobal->TimeStepZoneSec;
            }
            GCGain = con.GCInitEmiRate * Sch * std::exp(-con.GCTime / con.GCDelayTime);
            con.GCGenRate = GCGain;
        }

        // From boudary layer diffusion
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericBLDiff) {
            SurfNum = con.SurfNum;
            ZoneNum = state.dataSurface->Surface(SurfNum).Zone;
            Cs = state.dataSurface->SurfGenericContam(SurfNum);
            Sch = GetCurrentScheduleValue(state, con.GCTranCoefSchedPtr);
            GCGain = con.GCTranCoef * Sch * state.dataSurface->Surface(SurfNum).Area * state.dataSurface->Surface(SurfNum).Multiplier *
                     (Cs / con.GCHenryCoef - state.dataContaminantBalance->ZoneAirGC(ZoneNum)) * 1.0e-6;
            con.GCGenRate = GCGain;
            // Surface concentration level based on steady-state assumption
            state.dataSurface->SurfGenericContam(SurfNum) =
                Cs - GCGain * 1.0e6 / state.dataSurface->Surface(SurfNum).Multiplier / state.dataSurface->Surface(SurfNum).Area;
        }

        // From deposition velocity sink model
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericDVS) {
            SurfNum = con.SurfNum;
            ZoneNum = state.dataSurface->Surface(SurfNum).Zone;
            Sch = GetCurrentScheduleValue(state, con.GCDepoVeloPtr);
            GCGain = -con.GCDepoVelo * state.dataSurface->Surface(SurfNum).Area * Sch * state.dataContaminantBalance->ZoneAirGC(ZoneNum) *
                     state.dataSurface->Surface(SurfNum).Multiplier * 1.0e-6;
            con.GCGenRate = GCGain;
        }

        // From deposition rate sink model
        for (auto &con : state.dataContaminantBalance->ZoneContamGenericDRS) {
            ZoneNum = con.ActualZoneNum;
            Sch = GetCurrentScheduleValue(state, con.GCDepoRatePtr);
            GCGain = -con.GCDepoRate * state.dataHeatBal->Zone(ZoneNum).Volume * Sch * state.dataContaminantBalance->ZoneAirGC(ZoneNum) * 1.0e-6;
            con.GCGenRate = GCGain;
        }
    }
}

void PredictZoneContaminants(EnergyPlusData &state,
                             bool const ShortenTimeStepSys,
                             bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                             Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does the prediction step for contaminant control

    // METHODOLOGY EMPLOYED:
    // This solves for the required outdoor airflow to achieve the desired
    // contaminant setpoint in the Zone

    using ScheduleManager::GetCurrentScheduleValue;

    static constexpr std::string_view RoutineName("PredictZoneContaminants");

    Real64 CO2Gain;              // Zone CO2 internal load
    Real64 RhoAir;               // Zone air density
    Real64 A;                    // Coefficient of storage term in a zone balance equation
    Real64 B;                    // Coefficient of variable term in a zone balance equation
    Real64 C;                    // Coefficient of constnat term in a zone balance equation
    Real64 SysTimeStepInSeconds; // System time step lenght [s]
    bool ControlledCO2ZoneFlag;  // This determines whether this is a CO2 controlled zone or not
    Real64 LoadToCO2SetPoint;    // CO2 load at CO2 set point
    int ContControlledZoneNum;   // The Splitter that you are currently loading input into
    int ZoneNum;
    int I;
    Real64 ZoneAirCO2SetPoint; // Zone CO2 setpoint
    Real64 LoadToGCSetPoint;   // Generic contaminant load at generic contaminant set point
    bool ControlledGCZoneFlag; // This determines whether this is a generic contaminant controlled zone or not
    Real64 ZoneAirGCSetPoint;  // Zone generic contaminant setpoint
    Real64 GCGain;             // Zone generic contaminant internal load

    // Update zone CO2
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        if (ShortenTimeStepSys) {

            if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                    state.dataLoopNodes->Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).CO2 =
                        state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum);
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                    state.dataLoopNodes->Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).GenContam =
                        state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum);
            }

            if (state.dataHVACGlobal->NumOfSysTimeSteps !=
                state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time

                if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  state.dataHVACGlobal->TimeStepSys,
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->ZoneAirCO2(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus4(ZoneNum));
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  state.dataHVACGlobal->TimeStepSys,
                                                  state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->ZoneAirGC(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus4(ZoneNum));

            } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                     // do nothing because DS history would have been pushed prior and should be ready
            }
        }
        // now update the variables actually used in the balance equations.
        if (UseZoneTimeStepHistory) {

            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum);
            }

        } else { // use down-stepped history

            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum);
            }
        }

        if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                if (ShortenTimeStepSys && state.dataHVACGlobal->TimeStepSys < state.dataGlobal->TimeStepZone) {
                    if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                        state.dataContaminantBalance->ZoneCO21(ZoneNum) = state.dataContaminantBalance->ZoneCO2M2(ZoneNum);
                    } else {
                        state.dataContaminantBalance->ZoneCO21(ZoneNum) = state.dataContaminantBalance->ZoneCO2MX(ZoneNum);
                    }
                    state.dataHVACGlobal->ShortenTimeStepSysRoomAir = true;
                } else {
                    state.dataContaminantBalance->ZoneCO21(ZoneNum) = state.dataContaminantBalance->ZoneAirCO2(ZoneNum);
                }
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                if (ShortenTimeStepSys && state.dataHVACGlobal->TimeStepSys < state.dataGlobal->TimeStepZone) {
                    if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                        state.dataContaminantBalance->ZoneGC1(ZoneNum) = state.dataContaminantBalance->ZoneGCM2(ZoneNum);
                    } else {
                        state.dataContaminantBalance->ZoneGC1(ZoneNum) = state.dataContaminantBalance->ZoneGCMX(ZoneNum);
                    }
                    state.dataHVACGlobal->ShortenTimeStepSysRoomAir = true;
                } else {
                    state.dataContaminantBalance->ZoneGC1(ZoneNum) = state.dataContaminantBalance->ZoneAirGC(ZoneNum);
                }
            }
        }

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {

            state.dataContaminantBalance->CO2PredictedRate(ZoneNum) = 0.0;
            LoadToCO2SetPoint = 0.0;
            state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP = 0.0;

            // Check to see if this is a "CO2 controlled zone"
            ControlledCO2ZoneFlag = false;
            // Check all the controlled zones to see if it matches the zone simulated
            for (ContControlledZoneNum = 1; ContControlledZoneNum <= (int)state.dataContaminantBalance->ContaminantControlledZone.size();
                 ++ContControlledZoneNum) {
                if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == ZoneNum) {
                    if (GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) >
                        0.0) {
                        ZoneAirCO2SetPoint = state.dataContaminantBalance->ZoneCO2SetPoint(
                            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                            ZoneAirCO2SetPoint =
                                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointValue;
                        }
                        ControlledCO2ZoneFlag = true;
                        break;
                    }
                }
            }
            if (!ControlledCO2ZoneFlag) {
                for (ContControlledZoneNum = 1; ContControlledZoneNum <= (int)state.dataContaminantBalance->ContaminantControlledZone.size();
                     ++ContControlledZoneNum) {
                    if (GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) >
                        0.0) {
                        ZoneAirCO2SetPoint = state.dataContaminantBalance->ZoneCO2SetPoint(
                            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                            ZoneAirCO2SetPoint =
                                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointValue;
                        }
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).NumOfZones >= 1) {
                            if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum != ZoneNum) {
                                for (I = 1; I <= state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).NumOfZones; ++I) {
                                    if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ControlZoneNum(I) == ZoneNum) {
                                        ControlledCO2ZoneFlag = true;
                                        break;
                                    }
                                }
                                if (ControlledCO2ZoneFlag) break;
                            } else {
                                ControlledCO2ZoneFlag = true;
                                break;
                            }
                        }
                    }
                }
            } // CO2ControlledZoneNum

            if (ControlledCO2ZoneFlag) {
                // The density of air
                RhoAir = PsyRhoAirFnPbTdbW(state,
                                           state.dataEnvrn->OutBaroPress,
                                           state.dataHeatBalFanSys->ZT(ZoneNum),
                                           state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
                                           RoutineName);

                // Calculate Co2 from infiltration + humidity added from latent load
                // to determine system added/subtracted moisture.
                CO2Gain = state.dataContaminantBalance->ZoneCO2Gain(ZoneNum) * RhoAir * 1.0e6;

                SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

                // Calculate the coefficients for the 3rd Order derivative for final
                // zone CO2.  The A, B, C coefficients are analogous to the CO2 balance.
                // Assume that the system will have flow
                if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                    state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                    (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
                    // Multizone airflow calculated in AirflowNetwork
                    B = CO2Gain + state.afn->exchangeData(ZoneNum).SumMHrCO + state.afn->exchangeData(ZoneNum).SumMMHrCO;
                    A = state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr;
                } else {
                    B = CO2Gain +
                        ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                          state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                         state.dataContaminantBalance->OutdoorCO2) +
                        state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                        state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;
                    A = state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                        state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                        state.dataHeatBalFanSys->MDotOA(ZoneNum);
                }
                C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;

                // Use a 3rd Order derivative to predict zone moisture addition or removal and
                // smooth the changes using the zone air capacitance.  Positive values of CO2 Load means that
                // this amount of CO2 must be added to the zone to reach the setpoint.  Negative values represent
                // the amount of CO2 that must be removed by the system.
                switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
                case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                    LoadToCO2SetPoint = ((11.0 / 6.0) * C + A) * ZoneAirCO2SetPoint -
                                        (B + C * (3.0 * state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) -
                                                  (3.0 / 2.0) * state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) +
                                                  (1.0 / 3.0) * state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum)));
                    // Exact solution
                } break;
                case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                    if (A == 0.0) { // B=0
                        LoadToCO2SetPoint = C * (ZoneAirCO2SetPoint - state.dataContaminantBalance->ZoneCO21(ZoneNum)) - B;
                    } else {
                        LoadToCO2SetPoint =
                            A * (ZoneAirCO2SetPoint - state.dataContaminantBalance->ZoneCO21(ZoneNum) * std::exp(min(700.0, -A / C))) /
                                (1.0 - std::exp(min(700.0, -A / C))) -
                            B;
                    }
                } break;
                case DataHeatBalance::SolutionAlgo::EulerMethod: {
                    LoadToCO2SetPoint = C * (ZoneAirCO2SetPoint - state.dataContaminantBalance->ZoneCO21(ZoneNum)) + A * ZoneAirCO2SetPoint - B;
                } break;
                default:
                    break;
                }
                if (ZoneAirCO2SetPoint > state.dataContaminantBalance->OutdoorCO2 && LoadToCO2SetPoint < 0.0) {
                    state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP =
                        LoadToCO2SetPoint / (state.dataContaminantBalance->OutdoorCO2 - ZoneAirCO2SetPoint);
                }
            }

            // Apply the Zone Multiplier to the total zone moisture load
            state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP *=
                state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
            state.dataContaminantBalance->CO2PredictedRate(ZoneNum) = state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP;
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {

            state.dataContaminantBalance->GCPredictedRate(ZoneNum) = 0.0;
            LoadToGCSetPoint = 0.0;
            state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP = 0.0;

            // Check to see if this is a "GC controlled zone"
            ControlledGCZoneFlag = false;
            // Check all the controlled zones to see if it matches the zone simulated
            for (ContControlledZoneNum = 1; ContControlledZoneNum <= (int)state.dataContaminantBalance->ContaminantControlledZone.size();
                 ++ContControlledZoneNum) {
                if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == ZoneNum) {
                    if (GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) >
                        0.0) {
                        ZoneAirGCSetPoint = state.dataContaminantBalance->ZoneGCSetPoint(
                            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                            ZoneAirGCSetPoint =
                                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideGCSetPointValue;
                        }
                        ControlledGCZoneFlag = true;
                        break;
                    }
                }
            }
            if (!ControlledGCZoneFlag) {
                for (ContControlledZoneNum = 1; ContControlledZoneNum <= (int)state.dataContaminantBalance->ContaminantControlledZone.size();
                     ++ContControlledZoneNum) {
                    if (GetCurrentScheduleValue(state, state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) >
                        0.0) {
                        ZoneAirGCSetPoint = state.dataContaminantBalance->ZoneGCSetPoint(
                            state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                            ZoneAirGCSetPoint =
                                state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).EMSOverrideGCSetPointValue;
                        }
                        if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).NumOfZones >= 1) {
                            if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum != ZoneNum) {
                                for (I = 1; I <= state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).NumOfZones; ++I) {
                                    if (state.dataContaminantBalance->ContaminantControlledZone(ContControlledZoneNum).ControlZoneNum(I) == ZoneNum) {
                                        ControlledGCZoneFlag = true;
                                        break;
                                    }
                                }
                                if (ControlledGCZoneFlag) break;
                            } else {
                                ControlledGCZoneFlag = true;
                                break;
                            }
                        }
                    }
                }
            } // GCControlledZoneNum

            if (ControlledGCZoneFlag) {
                // The density of air
                RhoAir = PsyRhoAirFnPbTdbW(state,
                                           state.dataEnvrn->OutBaroPress,
                                           state.dataHeatBalFanSys->ZT(ZoneNum),
                                           state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
                                           RoutineName);

                // Calculate generic contaminant from infiltration + humidity added from latent load
                // to determine system added/subtracted moisture.
                GCGain = state.dataContaminantBalance->ZoneGCGain(ZoneNum) * RhoAir * 1.0e6;

                SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

                // Calculate the coefficients for the 3rd Order derivative for final
                // zone GC.  The A, B, C coefficients are analogous to the GC balance.
                // Assume that the system will have flow
                if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                    state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                    (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
                    // Multizone airflow calculated in AirflowNetwork
                    B = GCGain + state.afn->exchangeData(ZoneNum).SumMHrGC + state.afn->exchangeData(ZoneNum).SumMMHrGC;
                    A = state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr;
                } else {
                    B = GCGain +
                        ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                          state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                         state.dataContaminantBalance->OutdoorGC) +
                        state.dataContaminantBalance->MixingMassFlowGC(ZoneNum) +
                        state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorGC;
                    A = state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                        state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                        state.dataHeatBalFanSys->MDotOA(ZoneNum);
                }
                C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam /
                    SysTimeStepInSeconds;

                // Use a 3rd Order derivative to predict zone moisture addition or removal and
                // smooth the changes using the zone air capacitance.  Positive values of GC Load means that
                // this amount of GC must be added to the zone to reach the setpoint.  Negative values represent
                // the amount of GC that must be removed by the system.
                switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
                case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                    LoadToGCSetPoint = ((11.0 / 6.0) * C + A) * ZoneAirGCSetPoint -
                                       (B + C * (3.0 * state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) -
                                                 (3.0 / 2.0) * state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) +
                                                 (1.0 / 3.0) * state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum)));
                    // Exact solution
                } break;
                case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                    if (A == 0.0) { // B=0
                        LoadToGCSetPoint = C * (ZoneAirGCSetPoint - state.dataContaminantBalance->ZoneGC1(ZoneNum)) - B;
                    } else {
                        LoadToGCSetPoint = A * (ZoneAirGCSetPoint - state.dataContaminantBalance->ZoneGC1(ZoneNum) * std::exp(min(700.0, -A / C))) /
                                               (1.0 - std::exp(min(700.0, -A / C))) -
                                           B;
                    }
                } break;
                case DataHeatBalance::SolutionAlgo::EulerMethod: {
                    LoadToGCSetPoint = C * (ZoneAirGCSetPoint - state.dataContaminantBalance->ZoneGC1(ZoneNum)) + A * ZoneAirGCSetPoint - B;
                } break;
                default:
                    break;
                }
                if (ZoneAirGCSetPoint > state.dataContaminantBalance->OutdoorGC && LoadToGCSetPoint < 0.0) {
                    state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP =
                        LoadToGCSetPoint / (state.dataContaminantBalance->OutdoorGC - ZoneAirGCSetPoint);
                }
            }

            // Apply the Zone Multiplier to the total zone moisture load
            state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP *=
                state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
            state.dataContaminantBalance->GCPredictedRate(ZoneNum) = state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP;
        }
    }
}

void PushZoneTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July, 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // push histories for timestep advancing.
    // This subroutine is modified from PushZoneTimestepHistories in ZoneTempPredictorCorrector module

    int ZoneNum;

    // Push the temperature and humidity ratio histories

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->CO2ZoneTimeMinus4(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum);
            state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum) =
                state.dataContaminantBalance->ZoneAirCO2Avg(ZoneNum); // using average for whole zone time step.
            state.dataContaminantBalance->ZoneAirCO2(ZoneNum) = state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum);

            if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
                state.dataContaminantBalance->ZoneCO2M2(ZoneNum) = state.dataContaminantBalance->ZoneCO2MX(ZoneNum);
                state.dataContaminantBalance->ZoneCO2MX(ZoneNum) =
                    state.dataContaminantBalance->ZoneAirCO2Avg(ZoneNum); // using average for whole zone time step.
            }
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->GCZoneTimeMinus4(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum);
            state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum) =
                state.dataContaminantBalance->ZoneAirGCAvg(ZoneNum); // using average for whole zone time step.
            state.dataContaminantBalance->ZoneAirGC(ZoneNum) = state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum);

            if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
                state.dataContaminantBalance->ZoneGCM2(ZoneNum) = state.dataContaminantBalance->ZoneGCMX(ZoneNum);
                state.dataContaminantBalance->ZoneGCMX(ZoneNum) =
                    state.dataContaminantBalance->ZoneAirGCAvg(ZoneNum); // using average for whole zone time step.
            }
        }
    } // zone loop
}

void PushSystemTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July, 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // push histories
    // This subroutine is modified from PushSystemTimestepHistories in ZoneTempPredictorCorrector module

    int ZoneNum;

    // Push the temperature and humidity ratio histories back in time

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->DSCO2ZoneTimeMinus4(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum);
            state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum) = state.dataContaminantBalance->ZoneAirCO2(ZoneNum);
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->DSGCZoneTimeMinus4(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum);
            state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum) = state.dataContaminantBalance->ZoneAirGC(ZoneNum);
        }
    } // zone loop

    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->ZoneCO2M2(ZoneNum) = state.dataContaminantBalance->ZoneCO2MX(ZoneNum);
                state.dataContaminantBalance->ZoneCO2MX(ZoneNum) =
                    state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum); // using average for whole zone time step.
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->ZoneGCM2(ZoneNum) = state.dataContaminantBalance->ZoneGCMX(ZoneNum);
                state.dataContaminantBalance->ZoneGCMX(ZoneNum) =
                    state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum); // using average for whole zone time step.
            }
        } // zone loop
    }
}

void RevertZoneTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July, 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // rewind histories to undo inadvertent pushing
    // This subroutine is modified from RevertZoneTimestepHistories in ZoneTempPredictorCorrector module

    int ZoneNum;

    // REvert the contaminnants histories

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus4(ZoneNum);
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum);
            state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum);
            state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus4(ZoneNum);
        }
    } // zone loop
}

void InverseModelCO2(EnergyPlusData &state,
                     int const ZoneNum,           // Zone number
                     Real64 &CO2Gain,             // Zone total CO2 gain
                     Real64 &CO2GainExceptPeople, // ZOne total CO2 gain from sources except for people
                     Real64 &ZoneMassFlowRate,    // Zone air mass flow rate
                     Real64 &CO2MassFlowRate,     // Zone air CO2 mass flow rate
                     Real64 &RhoAir               // Air density
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Han Li
    //       DATE WRITTEN   February 2019
    //       MODIFIED        na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine inversely solve infiltration airflow rate or people count with zone air CO2 concentration measurements.

    Real64 AA(0.0);
    Real64 BB(0.0);
    Real64 CC(0.0);
    Real64 DD(0.0);
    Real64 zone_M_CO2(0.0);
    Real64 delta_CO2(0.0);
    Real64 AirDensity(0.0);
    Real64 CpAir(0.0);
    Real64 M_inf(0.0);   // Reversely solved infiltration mass flow rate
    Real64 ACH_inf(0.0); // Reversely solved infiltration air change rate
    Real64 SumSysM_HM(0.0);
    Real64 SumSysMxCO2_HM(0.0);
    Real64 CO2GainPeople(0.0); // Inversely solved convectice heat gain from people (m^3/s)
    Real64 NumPeople(0.0);     // Inversely solved number of people in the zone
    Real64 CO2GenRate(0.0);
    Real64 ActivityLevel(0.0);
    Real64 UpperBound(0.0); // Upper bound of number of people

    Real64 SysTimeStepInSeconds(0.0);
    SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration =
        GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZoneMeasuredCO2ConcentrationSchedulePtr);

    if (state.dataEnvrn->DayOfYear >= state.dataHybridModel->HybridModelZone(ZoneNum).HybridStartDayOfYear &&
        state.dataEnvrn->DayOfYear <= state.dataHybridModel->HybridModelZone(ZoneNum).HybridEndDayOfYear) {
        state.dataContaminantBalance->ZoneAirCO2(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration;

        if (state.dataHybridModel->HybridModelZone(ZoneNum).InfiltrationCalc_C && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            static constexpr std::string_view RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");
            // Conditionally calculate the CO2-dependent and CO2-independent terms.
            if (state.dataHybridModel->HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration =
                    GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZoneSupplyAirCO2ConcentrationSchedulePtr);

                SumSysM_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                SumSysMxCO2_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate *
                                 state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration;

                AA = SumSysM_HM + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = SumSysMxCO2_HM + CO2Gain +
                     ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataContaminantBalance->OutdoorCO2) +
                     state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;

            } else {
                AA = state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = CO2Gain +
                     ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataContaminantBalance->OutdoorCO2) +
                     state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;
            }

            CC = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
            DD = (3.0 * state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) -
                  (3.0 / 2.0) * state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) +
                  (1.0 / 3.0) * state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum));

            zone_M_CO2 = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration;
            delta_CO2 = (state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration - state.dataContaminantBalance->OutdoorCO2) / 1000;
            CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
            AirDensity = PsyRhoAirFnPbTdbW(state,
                                           state.dataEnvrn->OutBaroPress,
                                           state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp,
                                           state.dataEnvrn->OutHumRat,
                                           RoutineNameInfiltration);

            if (state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration == state.dataContaminantBalance->OutdoorCO2) {
                M_inf = 0.0;
            } else {
                M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration) / delta_CO2;
            }

            // Add threshold for air change rate
            ACH_inf = max(0.0, min(10.0, M_inf / (CpAir * AirDensity / DataGlobalConstants::SecInHour * state.dataHeatBal->Zone(ZoneNum).Volume)));
            M_inf = ACH_inf * state.dataHeatBal->Zone(ZoneNum).Volume * AirDensity / DataGlobalConstants::SecInHour;
            state.dataHeatBal->Zone(ZoneNum).MCPIHM = M_inf;
            state.dataHeatBal->Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;
        }

        // Hybrid Model calculate people count
        if (state.dataHybridModel->HybridModelZone(ZoneNum).PeopleCountCalc_C && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            state.dataHeatBal->Zone(ZoneNum).ZonePeopleActivityLevel =
                GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
            ActivityLevel = GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
            CO2GenRate = GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZonePeopleCO2GenRateSchedulePtr);
            if (ActivityLevel <= 0.0) {
                ActivityLevel = 130.0; // 130.0 is the default people activity level [W]
            }
            if (CO2GenRate <= 0.0) {
                CO2GenRate = 0.0000000382; // 0.0000000382 is the default CO2
                                           // generation rate [m3/(s*W)]
            }

            // Conditionally calculate the CO2-dependent and CO2-independent terms.
            if (state.dataHybridModel->HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration =
                    GetCurrentScheduleValue(state, state.dataHybridModel->HybridModelZone(ZoneNum).ZoneSupplyAirCO2ConcentrationSchedulePtr);

                SumSysM_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                SumSysMxCO2_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate *
                                 state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration;

                AA = SumSysM_HM + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = CO2GainExceptPeople +
                     ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                       state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataContaminantBalance->OutdoorCO2) +
                     (SumSysMxCO2_HM) + state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;

            } else {
                AA = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = CO2GainExceptPeople +
                     ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                       state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataContaminantBalance->OutdoorCO2) +
                     (CO2MassFlowRate) + state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;
            }

            CC = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
            DD = (3.0 * state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) -
                  (3.0 / 2.0) * state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) +
                  (1.0 / 3.0) * state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum));

            CO2GainPeople =
                (((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration - BB - CC * DD) / (1000000 * RhoAir);

            // Make sure the results are reasonable
            UpperBound = CO2Gain / (1000000 * RhoAir * CO2GenRate * ActivityLevel);
            NumPeople = min(UpperBound, CO2GainPeople / (CO2GenRate * ActivityLevel));

            NumPeople = floor(NumPeople * 100.00 + 0.5) / 100.00;
            if (NumPeople < 0.05) {
                NumPeople = 0;
            }
            state.dataHeatBal->Zone(ZoneNum).NumOccHM = NumPeople;
        }
    }

    // Update zone humidity ratio in the previous steps
    state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum);
    state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum);
    state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredCO2Concentration;
}

void CorrectZoneContaminants(EnergyPlusData &state,
                             bool const ShortenTimeStepSys,
                             bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
                             Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July, 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the zone contaminants.
    // This subroutine is modified from CorrectZoneHumRat in ZoneTempPredictorCorrector module

    // REFERENCES:
    // Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
    // for BLAST.

    static constexpr std::string_view RoutineName("CorrectZoneContaminants");

    int NodeNum;
    int ZoneNodeNum;
    int ZoneEquipConfigNum;
    bool ControlledZoneAirFlag;
    int ZoneRetPlenumNum;
    int ZoneSupPlenumNum;
    bool ZoneRetPlenumAirFlag;
    bool ZoneSupPlenumAirFlag;
    Real64 CO2Gain;             // Zone CO2 internal gain
    Real64 CO2GainExceptPeople; // Added for hybrid model, Zone CO2 internal gain
    Real64 GCGain;              // Zone generic contaminant internal gain
    Real64 RhoAir;
    Real64 A;
    Real64 B;
    Real64 C;
    Real64 CO2MassFlowRate;
    Real64 GCMassFlowRate;
    Real64 ZoneMassFlowRate;
    Real64 SysTimeStepInSeconds;
    Real64 ZoneMult;
    int ADUListIndex;
    int ADUNum;
    int ADUInNode;
    int ADUOutNode;
    int ZoneNum;

    auto &Node(state.dataLoopNodes->Node);

    // Update zone CO2
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->AZ(ZoneNum) = 0.0;
            state.dataContaminantBalance->BZ(ZoneNum) = 0.0;
            state.dataContaminantBalance->CZ(ZoneNum) = 0.0;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->AZGC(ZoneNum) = 0.0;
            state.dataContaminantBalance->BZGC(ZoneNum) = 0.0;
            state.dataContaminantBalance->CZGC(ZoneNum) = 0.0;
        }
        // Update variables
        if (ShortenTimeStepSys) {
            // time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
            if (state.dataHVACGlobal->NumOfSysTimeSteps !=
                state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  state.dataHVACGlobal->TimeStepSys,
                                                  state.dataContaminantBalance->ZoneAirCO2(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->CO2ZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->ZoneAirCO2(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->DSCO2ZoneTimeMinus4(ZoneNum));
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  state.dataHVACGlobal->TimeStepSys,
                                                  state.dataContaminantBalance->ZoneAirGC(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->GCZoneTimeMinus4(ZoneNum),
                                                  state.dataContaminantBalance->ZoneAirGC(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum),
                                                  state.dataContaminantBalance->DSGCZoneTimeMinus4(ZoneNum));
                }

            } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                     // do nothing because DS history would have been pushed prior and should be ready?
            }
        }

        // now update the variables actually used in the balance equations.
        if (!UseZoneTimeStepHistory) {
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->DSCO2ZoneTimeMinus3(ZoneNum);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->DSGCZoneTimeMinus3(ZoneNum);
            }
        } else {
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->CO2ZoneTimeMinus3(ZoneNum);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus1(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus2(ZoneNum);
                state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum) = state.dataContaminantBalance->GCZoneTimeMinus3(ZoneNum);
            }
        }

        // Start to calculate zone CO2 and genric contaminant levels
        CO2MassFlowRate = 0.0;
        GCMassFlowRate = 0.0;
        ZoneMassFlowRate = 0.0;
        ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;

        // Check to see if this is a controlled zone
        ControlledZoneAirFlag = false;
        for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigNum) {
            if (!state.dataHeatBal->Zone(ZoneEquipConfigNum).IsControlled) continue;
            if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != ZoneNum) continue;
            ControlledZoneAirFlag = true;
            break;
        } // ZoneEquipConfigNum

        // Check to see if this is a plenum zone
        ZoneRetPlenumAirFlag = false;
        for (ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++ZoneRetPlenumNum) {
            if (state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ActualZoneNum != ZoneNum) continue;
            ZoneRetPlenumAirFlag = true;
            break;
        } // ZoneRetPlenumNum
        ZoneSupPlenumAirFlag = false;
        for (ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= state.dataZonePlenum->NumZoneSupplyPlenums; ++ZoneSupPlenumNum) {
            if (state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).ActualZoneNum != ZoneNum) continue;
            ZoneSupPlenumAirFlag = true;
            break;
        } // ZoneSupPlenumNum

        if (ControlledZoneAirFlag) { // If there is system flow then calculate the flow rates

            // Calculate moisture flow rate into each zone
            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {

                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    CO2MassFlowRate += (Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                        Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).CO2) /
                                       ZoneMult;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    GCMassFlowRate += (Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                       Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).GenContam) /
                                      ZoneMult;
                }
                ZoneMassFlowRate += Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
            } // NodeNum

            // Do the calculations for the plenum zone
        } else if (ZoneRetPlenumAirFlag) {
            for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {

                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    CO2MassFlowRate += (Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                        Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).CO2) /
                                       ZoneMult;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    GCMassFlowRate += (Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                       Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).GenContam) /
                                      ZoneMult;
                }
                ZoneMassFlowRate += Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
            } // NodeNum
              // add in the leak flow
            for (ADUListIndex = 1; ADUListIndex <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        CO2MassFlowRate += (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).CO2) / ZoneMult;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        GCMassFlowRate += (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).GenContam) / ZoneMult;
                    }
                    ZoneMassFlowRate += state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk / ZoneMult;
                }
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        CO2MassFlowRate += (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).CO2) / ZoneMult;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        GCMassFlowRate +=
                            (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).GenContam) / ZoneMult;
                    }
                    ZoneMassFlowRate += state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk / ZoneMult;
                }
            }

        } else if (ZoneSupPlenumAirFlag) {

            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                CO2MassFlowRate += (Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate *
                                    Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).CO2) /
                                   ZoneMult;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                GCMassFlowRate += (Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate *
                                   Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).GenContam) /
                                  ZoneMult;
            }
            ZoneMassFlowRate += Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate / ZoneMult;
        }

        SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

        // Calculate the coefficients for the 3rd order derivative for final
        // zone humidity ratio.  The A, B, C coefficients are analogous to the
        // CO2 balance.  There are 2 cases that should be considered, system
        // operating and system shutdown.

        RhoAir = PsyRhoAirFnPbTdbW(
            state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), RoutineName);
        //    RhoAir = ZoneAirDensityCO(ZoneNum)

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) state.dataContaminantBalance->ZoneAirDensityCO(ZoneNum) = RhoAir;
        // Calculate Co2 internal gain
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) CO2Gain = state.dataContaminantBalance->ZoneCO2Gain(ZoneNum) * RhoAir * 1.0e6;
        if (state.dataContaminantBalance->Contaminant.CO2Simulation)
            CO2GainExceptPeople = state.dataContaminantBalance->ZoneCO2GainExceptPeople(ZoneNum) * RhoAir * 1.0e6; // Addded for hybrid model
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
            GCGain = state.dataContaminantBalance->ZoneGCGain(ZoneNum) * RhoAir * 1.0e6;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            B = CO2Gain +
                ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                  state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                 state.dataContaminantBalance->OutdoorCO2) +
                (CO2MassFlowRate) + state.dataContaminantBalance->MixingMassFlowCO2(ZoneNum) +
                state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorCO2;
            A = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
            if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
                // Multizone airflow calculated in AirflowNetwork
                B = CO2Gain + (state.afn->exchangeData(ZoneNum).SumMHrCO + state.afn->exchangeData(ZoneNum).SumMMHrCO) + CO2MassFlowRate;
                A = ZoneMassFlowRate + state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr;
            }
            C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
        }

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                B += state.afn->exchangeData(ZoneNum).TotalCO2;
            }

            state.dataContaminantBalance->AZ(ZoneNum) = A;
            state.dataContaminantBalance->BZ(ZoneNum) = B;
            state.dataContaminantBalance->CZ(ZoneNum) = C;

            // Use a 3rd order derivative to predict final zone CO2 and
            // smooth the changes using the zone air capacitance.
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) =
                    (B + C * (3.0 * state.dataContaminantBalance->CO2ZoneTimeMinus1Temp(ZoneNum) -
                              (3.0 / 2.0) * state.dataContaminantBalance->CO2ZoneTimeMinus2Temp(ZoneNum) +
                              (1.0 / 3.0) * state.dataContaminantBalance->CO2ZoneTimeMinus3Temp(ZoneNum))) /
                    ((11.0 / 6.0) * C + A);
                // Exact solution
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (A == 0.0) { // B=0
                    state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) = state.dataContaminantBalance->ZoneCO21(ZoneNum) + B / C;
                } else {
                    state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) =
                        (state.dataContaminantBalance->ZoneCO21(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) = (C * state.dataContaminantBalance->ZoneCO21(ZoneNum) + B) / (C + A);
            } break;
            default:
                break;
            }

            // Set the CO2 to zero if the zone has been large sinks
            if (state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) < 0.0) state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum) = 0.0;

            state.dataContaminantBalance->ZoneAirCO2(ZoneNum) = state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum);

            if ((state.dataHybridModel->HybridModelZone(ZoneNum).InfiltrationCalc_C ||
                 state.dataHybridModel->HybridModelZone(ZoneNum).PeopleCountCalc_C) &&
                (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
                InverseModelCO2(state, ZoneNum, CO2Gain, CO2GainExceptPeople, ZoneMassFlowRate, CO2MassFlowRate, RhoAir);
            }
            // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
            ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
            if (ZoneNodeNum > 0) {
                Node(ZoneNodeNum).CO2 = state.dataContaminantBalance->ZoneAirCO2Temp(ZoneNum);
            }
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            B = GCGain +
                ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                  state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                 state.dataContaminantBalance->OutdoorGC) +
                (GCMassFlowRate) + state.dataContaminantBalance->MixingMassFlowGC(ZoneNum) +
                state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataContaminantBalance->OutdoorGC;
            A = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
            if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
                // Multizone airflow calculated in AirflowNetwork
                B = GCGain + (state.afn->exchangeData(ZoneNum).SumMHrGC + state.afn->exchangeData(ZoneNum).SumMMHrGC) + GCMassFlowRate;
                A = ZoneMassFlowRate + state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr;
            }
            C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam / SysTimeStepInSeconds;
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                B += state.afn->exchangeData(ZoneNum).TotalGC;
            }

            state.dataContaminantBalance->AZGC(ZoneNum) = A;
            state.dataContaminantBalance->BZGC(ZoneNum) = B;
            state.dataContaminantBalance->CZGC(ZoneNum) = C;

            // Use a 3rd order derivative to predict final zone generic contaminant and
            // smooth the changes using the zone air capacitance.
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) =
                    (B + C * (3.0 * state.dataContaminantBalance->GCZoneTimeMinus1Temp(ZoneNum) -
                              (3.0 / 2.0) * state.dataContaminantBalance->GCZoneTimeMinus2Temp(ZoneNum) +
                              (1.0 / 3.0) * state.dataContaminantBalance->GCZoneTimeMinus3Temp(ZoneNum))) /
                    ((11.0 / 6.0) * C + A);
                // Exact solution
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (A == 0.0) { // B=0
                    state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) = state.dataContaminantBalance->ZoneGC1(ZoneNum) + B / C;
                } else {
                    state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) =
                        (state.dataContaminantBalance->ZoneGC1(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) = (C * state.dataContaminantBalance->ZoneGC1(ZoneNum) + B) / (C + A);
            } break;
            default:
                break;
            }

            // Set the generic contaminant to zero if the zone has been large sinks
            if (state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) < 0.0) state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum) = 0.0;

            state.dataContaminantBalance->ZoneAirGC(ZoneNum) = state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum);

            // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
            ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
            if (ZoneNodeNum > 0) {
                Node(ZoneNodeNum).GenContam = state.dataContaminantBalance->ZoneAirGCTemp(ZoneNum);
            }
        }
    }
}

} // namespace EnergyPlus::ZoneContaminantPredictorCorrector
