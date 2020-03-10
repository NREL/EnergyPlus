// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
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

namespace EnergyPlus {
namespace ZoneContaminantPredictorCorrector {

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May, 2010
    //       MODIFIED       None
    //       RE-ENGINEERED  None

    // PURPOSE OF THIS MODULE:
    // This module contains routines to predict and correct zone contaminants.
    //  also includes zone contaminant controlling

    // METHODOLOGY EMPLOYED:
    // Similar apporach to ZoneTempPredictorCorrector

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataGlobals;
    using namespace DataHVACGlobals;
    using namespace DataHeatBalance;
    using namespace DataHeatBalFanSys;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutHumRat;
    using namespace Psychrometrics;
    using namespace DataZoneControls;
    using namespace DataContaminantBalance;
    using namespace HybridModel;
    using ScheduleManager::GetCurrentScheduleValue;
    using ZoneTempPredictorCorrector::DownInterpolate4HistoryValues;
    //  iGetZoneSetPoints, iPredictStep, iCorrectStep, &
    //                                        iPushZoneTimestepHistories, iRevertZoneTimestepHistories, &
    //                                        iPushSystemTimestepHistories,

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:

    bool GetZoneAirContamInputFlag(true); // True when need to get input
    int TotGCGenConstant(0);              // Number of constant generic contaminant sources and sinks
    int TotGCGenPDriven(0);               // Number of pressure driven generic contaminant sources and sinks
    int TotGCGenCutoff(0);                // Number of cutoff model generic contaminant sources and sinks
    int TotGCGenDecay(0);                 // Number of decay model generic contaminant sources and sinks
    int TotGCBLDiff(0);                   // Number of boudary layer diffusion generic contaminant model
    int TotGCDVS(0);                      // Number of deposition velocity sink generic contaminant model
    int TotGCDRS(0);                      // Number of deposition rate sink generic contaminant model

    // SUBROUTINE SPECIFICATIONS:

    // Functions

    void clear_state()
    {
        GetZoneAirContamInputFlag = true;
        TotGCGenConstant = 0;
        TotGCGenPDriven = 0;
        TotGCGenCutoff = 0;
        TotGCGenDecay = 0;
        TotGCBLDiff = 0;
        TotGCDVS = 0;
        TotGCDRS = 0;
        Contaminant.CO2Simulation = false;
        Contaminant.GenericContamSimulation = false;
    }

    void ManageZoneContaminanUpdates(int const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                                     bool const ShortenTimeStepSys,
                                     bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                                     Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // unused1208  INTEGER :: zoneloop

        if (GetZoneAirContamInputFlag) {
            if (Contaminant.GenericContamSimulation) GetZoneContaminanInputs();
            GetZoneContaminanSetPoints();
            GetZoneAirContamInputFlag = false;
        }

        if (!Contaminant.SimulateContaminants) return;

        {
            auto const SELECT_CASE_var(UpdateType);

            if (SELECT_CASE_var == iGetZoneSetPoints) {
                InitZoneContSetPoints();

            } else if (SELECT_CASE_var == iPredictStep) {
                PredictZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iCorrectStep) {
                CorrectZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iRevertZoneTimestepHistories) {
                RevertZoneTimestepHistories();

            } else if (SELECT_CASE_var == iPushZoneTimestepHistories) {
                PushZoneTimestepHistories();

            } else if (SELECT_CASE_var == iPushSystemTimestepHistories) {
                PushSystemTimestepHistories();
            }
        }
    }

    void GetZoneContaminanInputs()
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
        using namespace DataIPShortCuts;
        using DataSurfaces::ExternalEnvironment;
        using DataSurfaces::Surface;
        using General::FindNumberInList;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using ScheduleManager::CheckScheduleValue;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSourcesAndSinks: ");

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
        static bool ErrorsFound(false);
        //  LOGICAL :: ValidScheduleType
        Array1D_bool RepVarSet;
        std::string CurrentModuleObject;

        // FLOW:

        RepVarSet.dimension(NumOfZones, true);

        MaxAlpha = -100;
        MaxNumber = -100;
        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:Constant";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:PressureDriven";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:CutoffModel";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DecaySource";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DepositionRateSink";
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        IHGNumbers.allocate(MaxNumber);
        AlphaName.allocate(MaxAlpha);
        IHGNumbers = 0.0;
        AlphaName = "";

        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:Constant";
        TotGCGenConstant = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericConstant.allocate(TotGCGenConstant);

        for (Loop = 1; Loop <= TotGCGenConstant; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericConstant(Loop).Name = AlphaName(1);

            ZoneContamGenericConstant(Loop).ZoneName = AlphaName(2);
            ZoneContamGenericConstant(Loop).ActualZoneNum = UtilityRoutines::FindItemInList(AlphaName(2), Zone);
            if (ZoneContamGenericConstant(Loop).ActualZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericConstant(Loop).GCGenerateRate = IHGNumbers(1);
            ZoneContamGenericConstant(Loop).GCRemovalCoef = IHGNumbers(2);

            ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr = GetScheduleIndex(AlphaName(4));
            if (ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(4) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(4) +
                                    " entered=" + AlphaName(4));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(4) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(4) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(4) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(4) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            if (ZoneContamGenericConstant(Loop).ActualZoneNum <= 0) continue; // Error, will be caught and terminated later

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Constant Source Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericConstant(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericConstant(Loop).Name);

            // Zone total report variables
            ZonePtr = ZoneContamGenericConstant(Loop).ActualZoneNum;
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericConstant(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericConstant(Loop).GCGenRate);
        }

        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:PressureDriven";
        TotGCGenPDriven = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericPDriven.allocate(TotGCGenPDriven);

        for (Loop = 1; Loop <= TotGCGenPDriven; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericPDriven(Loop).Name = AlphaName(1);

            ZoneContamGenericPDriven(Loop).SurfName = AlphaName(2);
            ZoneContamGenericPDriven(Loop).SurfNum =
                UtilityRoutines::FindItemInList(AlphaName(2), AirflowNetwork::MultizoneSurfaceData, &AirflowNetwork::MultizoneSurfaceProp::SurfName);
            if (ZoneContamGenericPDriven(Loop).SurfNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ShowContinueError("which is not listed in AirflowNetwork:MultiZone:Surface.");
                ErrorsFound = true;
            }
            // Ensure external surface
            if (ZoneContamGenericPDriven(Loop).SurfNum > 0 &&
                Surface(AirflowNetwork::MultizoneSurfaceData(ZoneContamGenericPDriven(Loop).SurfNum).SurfNum).ExtBoundCond != ExternalEnvironment) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + ". The entered surface (" + AlphaName(2) +
                                ") is not an exterior surface");
                ErrorsFound = true;
            }

            ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericPDriven(Loop).GCGenRateCoef = IHGNumbers(1);
            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }

            ZoneContamGenericPDriven(Loop).GCExpo = IHGNumbers(2);
            if (IHGNumbers(2) <= 0.0) {
                ShowSevereError(RoutineName + "Negative or zero value is not allowed for " + cNumericFieldNames(2) + " in " + CurrentModuleObject +
                                " = " + AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(2), 2));
                ErrorsFound = true;
            }
            if (IHGNumbers(2) > 1.0) {
                ShowSevereError(RoutineName + "The value greater than 1.0 is not allowed for " + cNumericFieldNames(2) + " in " +
                                CurrentModuleObject + " = " + AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(2), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Pressure Driven Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericPDriven(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericPDriven(Loop).Name);

            if (ZoneContamGenericPDriven(Loop).SurfNum > 0) {
                ZonePtr = Surface(AirflowNetwork::MultizoneSurfaceData(ZoneContamGenericPDriven(Loop).SurfNum).SurfNum).Zone;
            } else {
                ZonePtr = 0;
            }
            // Zone total report variables
            if (ZonePtr > 0 && RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            if (ZonePtr > 0)
                SetupZoneInternalGain(ZonePtr,
                                      "ZoneContaminantSourceAndSink:GenericContaminant",
                                      ZoneContamGenericPDriven(Loop).Name,
                                      IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      &ZoneContamGenericPDriven(Loop).GCGenRate);
        }

        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:CutoffModel";
        TotGCGenCutoff = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericCutoff.allocate(TotGCGenCutoff);

        for (Loop = 1; Loop <= TotGCGenCutoff; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericCutoff(Loop).Name = AlphaName(1);

            ZoneContamGenericCutoff(Loop).ZoneName = AlphaName(2);
            ZoneContamGenericCutoff(Loop).ActualZoneNum = UtilityRoutines::FindItemInList(AlphaName(2), Zone);
            if (ZoneContamGenericCutoff(Loop).ActualZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericCutoff(Loop).GCGenerateRate = IHGNumbers(1);
            ZoneContamGenericCutoff(Loop).GCCutoffValue = IHGNumbers(2);

            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }
            if (IHGNumbers(2) <= 0.0) {
                ShowSevereError(RoutineName + "Negative values or zero are not allowed for " + cNumericFieldNames(2) + " in " + CurrentModuleObject +
                                " = " + AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(2), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Cutoff Model Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericCutoff(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericCutoff(Loop).Name);

            // Zone total report variables
            ZonePtr = ZoneContamGenericCutoff(Loop).ActualZoneNum;
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericCutoff(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericCutoff(Loop).GCGenRate);
        }

        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DecaySource";
        TotGCGenDecay = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericDecay.allocate(TotGCGenDecay);

        for (Loop = 1; Loop <= TotGCGenDecay; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericDecay(Loop).Name = AlphaName(1);

            ZoneContamGenericDecay(Loop).ZoneName = AlphaName(2);
            ZoneContamGenericDecay(Loop).ActualZoneNum = UtilityRoutines::FindItemInList(AlphaName(2), Zone);
            if (ZoneContamGenericDecay(Loop).ActualZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericDecay(Loop).GCInitEmiRate = IHGNumbers(1);
            ZoneContamGenericDecay(Loop).GCDelayTime = IHGNumbers(2);

            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }
            if (IHGNumbers(2) <= 0.0) {
                ShowSevereError(RoutineName + "Negative values or zero are not allowed for " + cNumericFieldNames(2) + " in " + CurrentModuleObject +
                                " = " + AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(2), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Decay Model Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericDecay(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericDecay(Loop).Name);
            SetupOutputVariable("Generic Air Contaminant Decay Model Generation Emission Start Elapsed Time",
                                OutputProcessor::Unit::s,
                                ZoneContamGenericDecay(Loop).GCTime,
                                "Zone",
                                "Average",
                                ZoneContamGenericDecay(Loop).Name);

            // Zone total report variables
            ZonePtr = ZoneContamGenericDecay(Loop).ActualZoneNum;
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericDecay(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericDecay(Loop).GCGenRate);
        }

        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion";
        TotGCBLDiff = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericBLDiff.allocate(TotGCBLDiff);

        for (Loop = 1; Loop <= TotGCBLDiff; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericBLDiff(Loop).Name = AlphaName(1);

            ZoneContamGenericBLDiff(Loop).SurfName = AlphaName(2);
            ZoneContamGenericBLDiff(Loop).SurfNum = UtilityRoutines::FindItemInList(AlphaName(2), Surface);
            if (ZoneContamGenericBLDiff(Loop).SurfNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericBLDiff(Loop).GCTranCoef = IHGNumbers(1);
            ZoneContamGenericBLDiff(Loop).GCHenryCoef = IHGNumbers(2);
            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }
            if (IHGNumbers(2) <= 0.0) {
                ShowSevereError(RoutineName + "Negative values or zero are not allowed for " + cNumericFieldNames(2) + " in " + CurrentModuleObject +
                                " = " + AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(2), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Boundary Layer Diffusion Generation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericBLDiff(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericBLDiff(Loop).Name);
            if (ZoneContamGenericBLDiff(Loop).SurfNum > 0) {
                SetupOutputVariable("Generic Air Contaminant Boundary Layer Diffusion Inside Face Concentration",
                                    OutputProcessor::Unit::ppm,
                                    Surface(ZoneContamGenericBLDiff(Loop).SurfNum).GenericContam,
                                    "Zone",
                                    "Average",
                                    ZoneContamGenericBLDiff(Loop).SurfName);
            }

            ZonePtr = Surface(ZoneContamGenericBLDiff(Loop).SurfNum).Zone;
            // Zone total report variables
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericBLDiff(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericBLDiff(Loop).GCGenRate);
        }

        CurrentModuleObject = "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink";
        TotGCDVS = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericDVS.allocate(TotGCDVS);

        for (Loop = 1; Loop <= TotGCDVS; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericDVS(Loop).Name = AlphaName(1);

            ZoneContamGenericDVS(Loop).SurfName = AlphaName(2);
            ZoneContamGenericDVS(Loop).SurfNum = UtilityRoutines::FindItemInList(AlphaName(2), Surface);
            if (ZoneContamGenericDVS(Loop).SurfNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericDVS(Loop).GCDepoVeloPtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericDVS(Loop).GCDepoVeloPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericDVS(Loop).GCDepoVeloPtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericDVS(Loop).GCDepoVeloPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericDVS(Loop).GCDepoVelo = IHGNumbers(1);
            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Deposition Velocity Removal Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericDVS(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericDVS(Loop).Name);

            ZonePtr = Surface(ZoneContamGenericDVS(Loop).SurfNum).Zone;
            // Zone total report variables
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericDVS(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericDVS(Loop).GCGenRate);
        }

        CurrentModuleObject = "ZoneContaminantSourceAndSink:Generic:DepositionRateSink";
        TotGCDRS = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        ZoneContamGenericDRS.allocate(TotGCDRS);

        for (Loop = 1; Loop <= TotGCDRS; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Loop,
                                          AlphaName,
                                          NumAlpha,
                                          IHGNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphaName(1), CurrentModuleObject, ErrorsFound);
            ZoneContamGenericDRS(Loop).Name = AlphaName(1);

            ZoneContamGenericDRS(Loop).ZoneName = AlphaName(2);
            ZoneContamGenericDRS(Loop).ActualZoneNum = UtilityRoutines::FindItemInList(AlphaName(2), Zone);
            if (ZoneContamGenericDRS(Loop).ActualZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(2) +
                                " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            ZoneContamGenericDRS(Loop).GCDepoRatePtr = GetScheduleIndex(AlphaName(3));
            if (ZoneContamGenericDRS(Loop).GCDepoRatePtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " + cAlphaFieldNames(3) +
                                    " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ZoneContamGenericDRS(Loop).GCDepoRatePtr);
                SchMax = GetScheduleMaxValue(ZoneContamGenericDRS(Loop).GCDepoRatePtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", minimum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + cAlphaFieldNames(3) +
                                        ", maximum is < 0.0");
                        ShowContinueError("Schedule=\"" + AlphaName(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) +
                                          "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            ZoneContamGenericDRS(Loop).GCDepoRate = IHGNumbers(1);

            if (IHGNumbers(1) < 0.0) {
                ShowSevereError(RoutineName + "Negative values are not allowed for " + cNumericFieldNames(1) + " in " + CurrentModuleObject + " = " +
                                AlphaName(1));
                ShowContinueError("The input value is " + RoundSigDigits(IHGNumbers(1), 2));
                ErrorsFound = true;
            }

            // Object report variables
            SetupOutputVariable("Generic Air Contaminant Deposition Rate Removal Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ZoneContamGenericDRS(Loop).GCGenRate,
                                "Zone",
                                "Average",
                                ZoneContamGenericDRS(Loop).Name);

            ZonePtr = ZoneContamGenericDRS(Loop).ActualZoneNum;
            // Zone total report variables
            if (RepVarSet(ZonePtr)) {
                RepVarSet(ZonePtr) = false;
                SetupOutputVariable("Zone Generic Air Contaminant Generation Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    ZnRpt(ZonePtr).GCRate,
                                    "Zone",
                                    "Average",
                                    Zone(ZonePtr).Name);
            }
            SetupZoneInternalGain(ZonePtr,
                                  "ZoneContaminantSourceAndSink:GenericContaminant",
                                  ZoneContamGenericDRS(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &ZoneContamGenericDRS(Loop).GCGenRate);
        }

        RepVarSet.deallocate();
        IHGNumbers.deallocate();
        AlphaName.deallocate();

        if (ErrorsFound) {
            ShowFatalError("Errors getting Zone Contaminant Sources and Sinks input data.  Preceding condition(s) cause termination.");
        }
    }

    void GetZoneContaminanSetPoints()
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
        using namespace DataIPShortCuts;
        using General::TrimSigDigits;
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
        // unused1208  REAL(r64), DIMENSION(2) :: NumArray
        // unused1208  CHARACTER(len=MaxNameLength), DIMENSION(29) :: AlphArray
        static bool ErrorsFound(false);
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

        // FLOW:
        cCurrentModuleObject = "ZoneControl:ContaminantController";
        NumContControlledZones = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumContControlledZones > 0) {
            ContaminantControlledZone.allocate(NumContControlledZones);
        }

        for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          ContControlledZoneNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            ContaminantControlledZone(ContControlledZoneNum).Name = cAlphaArgs(1);
            ContaminantControlledZone(ContControlledZoneNum).ZoneName = cAlphaArgs(2);
            ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            if (ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                //      Zone(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)%TempControlledZoneIndex = ContControlledZoneNum
            }

            ContaminantControlledZone(ContControlledZoneNum).AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr = ScheduleAlwaysOn; // (Returns 1.0)
            } else {
                ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr = GetScheduleIndex(cAlphaArgs(3));
                if (ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Check validity of control types.
                    ValidScheduleType = CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr, ">=", 0.0, "<=", 1.0);
                    if (!ValidScheduleType) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(3) + "=\"" +
                                        cAlphaArgs(3) + "\"");
                        ShowContinueError("..contains values outside of range [0,1].");
                        ErrorsFound = true;
                    } else {
                        Zone(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum).ZoneContamControllerSchedIndex =
                            ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr;
                    }
                }
            }

            ContaminantControlledZone(ContControlledZoneNum).SetPointSchedName = cAlphaArgs(4);
            ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex = GetScheduleIndex(cAlphaArgs(4));
            if (ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                // Check validity of control types.
                ValidScheduleType = CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum).SPSchedIndex, ">=", 0.0, "<=", 2000.0);
                if (!ValidScheduleType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                    "\"");
                    ShowContinueError("..contains values outside of range [0,2000 ppm].");
                    ErrorsFound = true;
                }
            }

            ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedName = cAlphaArgs(5);
            ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex = GetScheduleIndex(cAlphaArgs(5));
            if (ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex > 0) {
                // Check validity of control types.
                ValidScheduleType =
                    CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex, ">=", 0.0, "<=", 2000.0);
                if (!ValidScheduleType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                    "\"");
                    ShowContinueError("..contains values outside of range [0,2000 ppm].");
                    ErrorsFound = true;
                } else {
                    Zone(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum).ZoneMinCO2SchedIndex =
                        ContaminantControlledZone(ContControlledZoneNum).ZoneMinCO2SchedIndex;
                }
            }

            ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedName = cAlphaArgs(6);
            ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex = GetScheduleIndex(cAlphaArgs(6));
            if (ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex > 0) {
                // Check validity of control types.
                ValidScheduleType =
                    CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex, ">=", 0.0, "<=", 2000.0);
                if (!ValidScheduleType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) +
                                    "\"");
                    ShowContinueError("..contains values outside of range [0,2000 ppm].");
                    ErrorsFound = true;
                } else {
                    Zone(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum).ZoneMaxCO2SchedIndex =
                        ContaminantControlledZone(ContControlledZoneNum).ZoneMaxCO2SchedIndex;
                }
            }

            if (NumAlphas > 6) {
                ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedule = cAlphaArgs(7);
                if (lAlphaFieldBlanks(7)) {
                    ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr = ScheduleAlwaysOn;
                } else {
                    ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr = GetScheduleIndex(cAlphaArgs(7));
                    if (ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(7) +
                                        "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Check validity of control types.
                        ValidScheduleType =
                            CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum).GCAvaiSchedPtr, ">=", 0.0, "<=", 1.0);
                        if (!ValidScheduleType) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(3) + "=\"" +
                                            cAlphaArgs(7) + "\"");
                            ShowContinueError("..contains values outside of range [0,1].");
                            ErrorsFound = true;
                        }
                    }
                }
                if (lAlphaFieldBlanks(8)) {
                    ShowSevereError(cCurrentModuleObject + " \"" + cAlphaArgs(8) + "\" is required, but blank.");
                    ErrorsFound = true;
                } else {
                    ContaminantControlledZone(ContControlledZoneNum).GCSetPointSchedName = cAlphaArgs(8);
                    ContaminantControlledZone(ContControlledZoneNum).GCSPSchedIndex = GetScheduleIndex(cAlphaArgs(8));
                    if (ContaminantControlledZone(ContControlledZoneNum).GCSPSchedIndex == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(8) + "=\"" + cAlphaArgs(8) +
                                        "\" not found.");
                        ErrorsFound = true;
                    }
                }
            }

        } // ContControlledZoneNum

        if (ErrorsFound) {
            ShowFatalError("Errors getting Zone Contaminant Control input data.  Preceding condition(s) cause termination.");
        }
    }

    void InitZoneContSetPoints()
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSurfaces::Surface;
        using DataZoneEquipment::ZoneEquipConfig;
        using InternalHeatGains::SumAllInternalCO2Gains;
        using InternalHeatGains::SumAllInternalCO2GainsExceptPeople; // Added for hybrid model
        using InternalHeatGains::SumAllInternalGenericContamGains;
        using InternalHeatGains::SumInternalCO2GainsByTypes;
        using ScheduleManager::GetCurrentScheduleValue;

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
        int ZoneNum;
        int SurfNum;
        static bool MyOneTimeFlag(true);
        static bool MyEnvrnFlag(true);
        static bool MyDayFlag(true);
        //  REAL(r64)      :: CO2Gain                  ! Zone CO2 gain
        Real64 GCGain; // Zone generic contaminant gain
        Real64 Pi;     // Pressue at zone i
        Real64 Pj;     // Pressue at zone j
        Real64 Sch;    // Schedule value
        Real64 Cs;     // Surface concentration level for the Boundary Layer Diffusion Controlled Model
        static bool MyConfigOneTimeFlag(true);
        int ContZoneNum;
        int I;
        static bool ErrorsFound(false);

        // FLOW:

        if (Contaminant.CO2Simulation) {
            OutdoorCO2 = GetCurrentScheduleValue(Contaminant.CO2OutdoorSchedPtr);
        }

        if (Contaminant.GenericContamSimulation) {
            OutdoorGC = GetCurrentScheduleValue(Contaminant.GenericContamOutdoorSchedPtr);
        }

        if (MyOneTimeFlag) {
            // CO2
            if (Contaminant.CO2Simulation) {
                ZoneCO2SetPoint.dimension(NumOfZones, 0.0);
                CO2PredictedRate.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus1.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus2.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus3.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus4.dimension(NumOfZones, 0.0);
                DSCO2ZoneTimeMinus1.dimension(NumOfZones, 0.0);
                DSCO2ZoneTimeMinus2.dimension(NumOfZones, 0.0);
                DSCO2ZoneTimeMinus3.dimension(NumOfZones, 0.0);
                DSCO2ZoneTimeMinus4.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus1Temp.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus2Temp.dimension(NumOfZones, 0.0);
                CO2ZoneTimeMinus3Temp.dimension(NumOfZones, 0.0);
                ZoneCO2MX.dimension(NumOfZones, 0.0);
                ZoneCO2M2.dimension(NumOfZones, 0.0);
                ZoneCO21.dimension(NumOfZones, 0.0);

                ZoneSysContDemand.allocate(NumOfZones);
                ZoneCO2Gain.dimension(NumOfZones, 0.0);
                ZoneCO2GainFromPeople.dimension(NumOfZones, 0.0);
                ZoneCO2GainExceptPeople.dimension(NumOfZones, 0.0); // Added for hybrid model
                MixingMassFlowCO2.dimension(NumOfZones, 0.0);
                ZoneAirDensityCO.dimension(NumOfZones, 0.0);
                AZ.dimension(NumOfZones, 0.0);
                BZ.dimension(NumOfZones, 0.0);
                CZ.dimension(NumOfZones, 0.0);
            }

            CONTRAT.dimension(NumOfZones, 0.0);

            // Allocate Derived Types

            for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                // Zone CO2
                if (Contaminant.CO2Simulation) {
                    SetupOutputVariable(
                        "Zone Air CO2 Concentration", OutputProcessor::Unit::ppm, ZoneAirCO2(Loop), "System", "Average", Zone(Loop).Name);
                    SetupOutputVariable("Zone Air CO2 Predicted Load to Setpoint Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        CO2PredictedRate(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable("Zone Air CO2 Setpoint Concentration",
                                        OutputProcessor::Unit::ppm,
                                        ZoneCO2SetPoint(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable("Zone Air CO2 Internal Gain Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        ZoneCO2Gain(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                }

            } // Loop

            // Generic contaminant
            if (Contaminant.GenericContamSimulation) {
                ZoneGCSetPoint.dimension(NumOfZones, 0.0);
                GCPredictedRate.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus1.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus2.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus3.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus4.dimension(NumOfZones, 0.0);
                DSGCZoneTimeMinus1.dimension(NumOfZones, 0.0);
                DSGCZoneTimeMinus2.dimension(NumOfZones, 0.0);
                DSGCZoneTimeMinus3.dimension(NumOfZones, 0.0);
                DSGCZoneTimeMinus4.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus1Temp.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus2Temp.dimension(NumOfZones, 0.0);
                GCZoneTimeMinus3Temp.dimension(NumOfZones, 0.0);
                ZoneGCMX.dimension(NumOfZones, 0.0);
                ZoneGCM2.dimension(NumOfZones, 0.0);
                ZoneGC1.dimension(NumOfZones, 0.0);

                if (!allocated(ZoneSysContDemand)) ZoneSysContDemand.allocate(NumOfZones);
                ZoneGCGain.dimension(NumOfZones, 0.0);
                MixingMassFlowGC.dimension(NumOfZones, 0.0);
                ZoneAirDensityGC.dimension(NumOfZones, 0.0);
                AZGC.dimension(NumOfZones, 0.0);
                BZGC.dimension(NumOfZones, 0.0);
                CZGC.dimension(NumOfZones, 0.0);
            }

            CONTRATGC.dimension(NumOfZones, 0.0);

            // Allocate Derived Types

            for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                // Zone CO2
                if (Contaminant.GenericContamSimulation) {
                    SetupOutputVariable("Zone Air Generic Air Contaminant Concentration",
                                        OutputProcessor::Unit::ppm,
                                        ZoneAirGC(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable("Zone Generic Air Contaminant Predicted Load to Setpoint Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        GCPredictedRate(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable("Zone Generic Air Contaminant Setpoint Concentration",
                                        OutputProcessor::Unit::ppm,
                                        ZoneGCSetPoint(Loop),
                                        "System",
                                        "Average",
                                        Zone(Loop).Name);
                }
            } // Loop

            MyOneTimeFlag = false;
        }

        // Do the Begin Environment initializations
        if (MyEnvrnFlag && BeginEnvrnFlag) {
            if (Contaminant.CO2Simulation) {
                CONTRAT = 0.0;
                CO2ZoneTimeMinus1 = OutdoorCO2;
                CO2ZoneTimeMinus2 = OutdoorCO2;
                CO2ZoneTimeMinus3 = OutdoorCO2;
                CO2ZoneTimeMinus4 = OutdoorCO2;
                DSCO2ZoneTimeMinus1 = OutdoorCO2;
                DSCO2ZoneTimeMinus2 = OutdoorCO2;
                DSCO2ZoneTimeMinus3 = OutdoorCO2;
                DSCO2ZoneTimeMinus4 = OutdoorCO2;
                CO2ZoneTimeMinus1Temp = 0.0;
                CO2ZoneTimeMinus2Temp = 0.0;
                CO2ZoneTimeMinus3Temp = 0.0;
                ZoneAirCO2Temp = OutdoorCO2;
                ZoneCO2SetPoint = 0.0;
                CO2PredictedRate = 0.0;
                ZoneAirCO2 = OutdoorCO2;
                ZoneCO21 = OutdoorCO2;
                ZoneCO2MX = OutdoorCO2;
                ZoneCO2M2 = OutdoorCO2;
            }
            if (Contaminant.GenericContamSimulation) {
                CONTRAT = 0.0;
                GCZoneTimeMinus1 = OutdoorGC;
                GCZoneTimeMinus2 = OutdoorGC;
                GCZoneTimeMinus3 = OutdoorGC;
                GCZoneTimeMinus4 = OutdoorGC;
                DSGCZoneTimeMinus1 = OutdoorGC;
                DSGCZoneTimeMinus2 = OutdoorGC;
                DSGCZoneTimeMinus3 = OutdoorGC;
                DSGCZoneTimeMinus4 = OutdoorGC;
                GCZoneTimeMinus1Temp = 0.0;
                GCZoneTimeMinus2Temp = 0.0;
                GCZoneTimeMinus3Temp = 0.0;
                ZoneAirGCTemp = OutdoorGC;
                ZoneGCSetPoint = 0.0;
                GCPredictedRate = 0.0;
                ZoneAirGC = OutdoorGC;
                ZoneGC1 = OutdoorGC;
                ZoneGCMX = OutdoorGC;
                ZoneGCM2 = OutdoorGC;
                for (Loop = 1; Loop <= TotGCBLDiff; ++Loop) {
                    Surface(ZoneContamGenericBLDiff(Loop).SurfNum).GenericContam = OutdoorGC;
                }
                if (TotGCGenDecay > 0)
                    for (auto &e : ZoneContamGenericDecay)
                        e.GCTime = 0.0;
            }
            MyEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) {
            MyEnvrnFlag = true;
        }

        // Do the Begin Day initializations
        if (MyDayFlag && BeginDayFlag) {
            MyDayFlag = false;
        }

        if (!BeginDayFlag) {
            MyDayFlag = true;
        }

        if (allocated(ZoneEquipConfig) && MyConfigOneTimeFlag) {
            for (ContZoneNum = 1; ContZoneNum <= NumContControlledZones; ++ContZoneNum) {
                ZoneNum = ContaminantControlledZone(ContZoneNum).ActualZoneNum;
                for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++zoneInNode) {
                    int AirLoopNum = ZoneEquipConfig(ZoneNum).InletNodeAirLoopNum(zoneInNode);
                    ContaminantControlledZone(ContZoneNum).NumOfZones = 0;
                    for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                        if (!ZoneEquipConfig(Loop).IsControlled) continue;
                        for (int zoneInNode2 = 1; zoneInNode2 <= ZoneEquipConfig(Loop).NumInletNodes; ++zoneInNode2) {
                            if (AirLoopNum == ZoneEquipConfig(Loop).InletNodeAirLoopNum(zoneInNode2)) {
                                ++ContaminantControlledZone(ContZoneNum).NumOfZones;
                                break; // only count a zone once
                            }
                        }
                    }
                    if (ContaminantControlledZone(ContZoneNum).NumOfZones > 0) {
                        ContaminantControlledZone(ContZoneNum).ControlZoneNum.allocate(ContaminantControlledZone(ContZoneNum).NumOfZones);
                        I = 1;
                        for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                            if (!ZoneEquipConfig(Loop).IsControlled) continue;
                            for (int zoneInNode2 = 1; zoneInNode2 <= ZoneEquipConfig(Loop).NumInletNodes; ++zoneInNode2) {
                                if (AirLoopNum == ZoneEquipConfig(Loop).InletNodeAirLoopNum(zoneInNode2)) {
                                    ContaminantControlledZone(ContZoneNum).ControlZoneNum(I) = Loop;
                                    ++I;
                                    break; // only count a zone once
                                }
                            }
                        }
                    } else {
                        ShowSevereError("ZoneControl:ContaminantController: a corresponding AirLoopHVAC is not found for the controlled zone =" +
                                        Zone(ZoneNum).Name);
                        ErrorsFound = true;
                    }
                }
            }
            MyConfigOneTimeFlag = false;
            if (ErrorsFound) {
                ShowFatalError("ZoneControl:ContaminantController: Program terminates for preceding reason(s).");
            }
        }

        for (Loop = 1; Loop <= NumContControlledZones; ++Loop) {
            if (Contaminant.CO2Simulation) {
                ZoneNum = ContaminantControlledZone(Loop).ActualZoneNum;
                ZoneCO2SetPoint(ZoneNum) = GetCurrentScheduleValue(ContaminantControlledZone(Loop).SPSchedIndex);
            }
            if (Contaminant.GenericContamSimulation) {
                ZoneNum = ContaminantControlledZone(Loop).ActualZoneNum;
                ZoneGCSetPoint(ZoneNum) = GetCurrentScheduleValue(ContaminantControlledZone(Loop).GCSPSchedIndex);
            }
        }

        // CO2 gain
        if (Contaminant.CO2Simulation) {
            for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                SumAllInternalCO2Gains(Loop, ZoneCO2Gain(Loop));
                if (HybridModel::FlagHybridModel_PC) {
                    SumAllInternalCO2GainsExceptPeople(Loop, ZoneCO2GainExceptPeople(Loop));
                }
                SumInternalCO2GainsByTypes(Loop, Array1D_int(1, IntGainTypeOf_People), ZoneCO2GainFromPeople(Loop));
            }
        }

        // Generic contaminant gain
        if (Contaminant.GenericContamSimulation) {
            ZoneGCGain = 0.0;
            // from constant model
            for (Loop = 1; Loop <= TotGCGenConstant; ++Loop) {
                ZoneNum = ZoneContamGenericConstant(Loop).ActualZoneNum;
                GCGain =
                    ZoneContamGenericConstant(Loop).GCGenerateRate * GetCurrentScheduleValue(ZoneContamGenericConstant(Loop).GCGenerateRateSchedPtr) -
                    ZoneContamGenericConstant(Loop).GCRemovalCoef * GetCurrentScheduleValue(ZoneContamGenericConstant(Loop).GCRemovalCoefSchedPtr) *
                        ZoneAirGC(ZoneNum) * 1.0e-6;
                ZoneContamGenericConstant(Loop).GCGenRate = GCGain;
            }

            // from pressure driven model
            if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
                for (Loop = 1; Loop <= TotGCGenPDriven; ++Loop) {
                    SurfNum = ZoneContamGenericPDriven(Loop).SurfNum;
                    Pi = AirflowNetwork::AirflowNetworkNodeSimu(AirflowNetwork::MultizoneSurfaceData(SurfNum).NodeNums[0]).PZ;
                    Pj = AirflowNetwork::AirflowNetworkNodeSimu(AirflowNetwork::MultizoneSurfaceData(SurfNum).NodeNums[1]).PZ;
                    if (Pj >= Pi) {
                        GCGain = ZoneContamGenericPDriven(Loop).GCGenRateCoef *
                                 GetCurrentScheduleValue(ZoneContamGenericPDriven(Loop).GCGenRateCoefSchedPtr) *
                                 std::pow(Pj - Pi, ZoneContamGenericPDriven(Loop).GCExpo);
                    } else {
                        GCGain = 0.0;
                    }
                    ZoneContamGenericPDriven(Loop).GCGenRate = GCGain;
                }
            }

            // from cutoff model
            for (Loop = 1; Loop <= TotGCGenCutoff; ++Loop) {
                ZoneNum = ZoneContamGenericCutoff(Loop).ActualZoneNum;
                if (ZoneAirGC(ZoneNum) < ZoneContamGenericCutoff(Loop).GCCutoffValue) {
                    GCGain = ZoneContamGenericCutoff(Loop).GCGenerateRate *
                             GetCurrentScheduleValue(ZoneContamGenericCutoff(Loop).GCGenerateRateSchedPtr) *
                             (1.0 - ZoneAirGC(ZoneNum) / ZoneContamGenericCutoff(Loop).GCCutoffValue);
                } else {
                    GCGain = 0.0;
                }
                ZoneContamGenericCutoff(Loop).GCGenRate = GCGain;
            }

            // From decay model
            for (Loop = 1; Loop <= TotGCGenDecay; ++Loop) {
                Sch = GetCurrentScheduleValue(ZoneContamGenericDecay(Loop).GCEmiRateSchedPtr);
                ZoneNum = ZoneContamGenericDecay(Loop).ActualZoneNum;
                if (Sch == 0.0 || BeginEnvrnFlag || WarmupFlag) {
                    ZoneContamGenericDecay(Loop).GCTime = 0.0;
                } else {
                    ZoneContamGenericDecay(Loop).GCTime += TimeStepZoneSec;
                }
                GCGain = ZoneContamGenericDecay(Loop).GCInitEmiRate * Sch *
                         std::exp(-ZoneContamGenericDecay(Loop).GCTime / ZoneContamGenericDecay(Loop).GCDelayTime);
                ZoneContamGenericDecay(Loop).GCGenRate = GCGain;
            }

            // From boudary layer diffusion
            for (Loop = 1; Loop <= TotGCBLDiff; ++Loop) {
                SurfNum = ZoneContamGenericBLDiff(Loop).SurfNum;
                ZoneNum = Surface(SurfNum).Zone;
                Cs = Surface(SurfNum).GenericContam;
                Sch = GetCurrentScheduleValue(ZoneContamGenericBLDiff(Loop).GCTranCoefSchedPtr);
                GCGain = ZoneContamGenericBLDiff(Loop).GCTranCoef * Sch * Surface(SurfNum).Area * Surface(SurfNum).Multiplier *
                         (Cs / ZoneContamGenericBLDiff(Loop).GCHenryCoef - ZoneAirGC(ZoneNum)) * 1.0e-6;
                ZoneContamGenericBLDiff(Loop).GCGenRate = GCGain;
                // Surface concentration level based on steady-state assumption
                Surface(SurfNum).GenericContam = Cs - GCGain * 1.0e6 / Surface(SurfNum).Multiplier / Surface(SurfNum).Area;
            }

            // From deposition velocity sink model
            for (Loop = 1; Loop <= TotGCDVS; ++Loop) {
                SurfNum = ZoneContamGenericDVS(Loop).SurfNum;
                ZoneNum = Surface(SurfNum).Zone;
                Sch = GetCurrentScheduleValue(ZoneContamGenericDVS(Loop).GCDepoVeloPtr);
                GCGain =
                    -ZoneContamGenericDVS(Loop).GCDepoVelo * Surface(SurfNum).Area * Sch * ZoneAirGC(ZoneNum) * Surface(SurfNum).Multiplier * 1.0e-6;
                ZoneContamGenericDVS(Loop).GCGenRate = GCGain;
            }

            // From deposition rate sink model
            for (Loop = 1; Loop <= TotGCDRS; ++Loop) {
                ZoneNum = ZoneContamGenericDRS(Loop).ActualZoneNum;
                Sch = GetCurrentScheduleValue(ZoneContamGenericDRS(Loop).GCDepoRatePtr);
                GCGain = -ZoneContamGenericDRS(Loop).GCDepoRate * Zone(ZoneNum).Volume * Sch * ZoneAirGC(ZoneNum) * 1.0e-6;
                ZoneContamGenericDRS(Loop).GCGenRate = GCGain;
            }
        }
    }

    void PredictZoneContaminants(bool const ShortenTimeStepSys,
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::Node;
        using General::RoundSigDigits;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("PredictZoneContaminants");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
                                   //  REAL(r64) :: Temp                      ! Zone generic contaminant internal load

        // FLOW:

        // Update zone CO2
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            if (ShortenTimeStepSys) {

                if (Zone(ZoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                    if (Contaminant.CO2Simulation) Node(Zone(ZoneNum).SystemZoneNodeNumber).CO2 = CO2ZoneTimeMinus1(ZoneNum);
                    if (Contaminant.GenericContamSimulation) Node(Zone(ZoneNum).SystemZoneNodeNumber).GenContam = GCZoneTimeMinus1(ZoneNum);
                }

                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time

                    if (Contaminant.CO2Simulation)
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      CO2ZoneTimeMinus1(ZoneNum),
                                                      CO2ZoneTimeMinus2(ZoneNum),
                                                      CO2ZoneTimeMinus3(ZoneNum),
                                                      CO2ZoneTimeMinus4(ZoneNum),
                                                      CO2ZoneTimeMinus4(ZoneNum),
                                                      ZoneAirCO2(ZoneNum),
                                                      DSCO2ZoneTimeMinus1(ZoneNum),
                                                      DSCO2ZoneTimeMinus2(ZoneNum),
                                                      DSCO2ZoneTimeMinus3(ZoneNum),
                                                      DSCO2ZoneTimeMinus4(ZoneNum));
                    if (Contaminant.GenericContamSimulation)
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      GCZoneTimeMinus1(ZoneNum),
                                                      GCZoneTimeMinus2(ZoneNum),
                                                      GCZoneTimeMinus3(ZoneNum),
                                                      GCZoneTimeMinus4(ZoneNum),
                                                      GCZoneTimeMinus4(ZoneNum),
                                                      ZoneAirGC(ZoneNum),
                                                      DSGCZoneTimeMinus1(ZoneNum),
                                                      DSGCZoneTimeMinus2(ZoneNum),
                                                      DSGCZoneTimeMinus3(ZoneNum),
                                                      DSGCZoneTimeMinus4(ZoneNum));

                } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                         // do nothing because DS history would have been pushed prior and should be ready
                }
            }
            // now update the variables actually used in the balance equations.
            if (UseZoneTimeStepHistory) {

                if (Contaminant.CO2Simulation) {
                    CO2ZoneTimeMinus1Temp(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum);
                    CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum);
                    CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum);
                }
                if (Contaminant.GenericContamSimulation) {
                    GCZoneTimeMinus1Temp(ZoneNum) = GCZoneTimeMinus1(ZoneNum);
                    GCZoneTimeMinus2Temp(ZoneNum) = GCZoneTimeMinus2(ZoneNum);
                    GCZoneTimeMinus3Temp(ZoneNum) = GCZoneTimeMinus3(ZoneNum);
                }

            } else { // use down-stepped history

                if (Contaminant.CO2Simulation) {
                    CO2ZoneTimeMinus1Temp(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum);
                    CO2ZoneTimeMinus2Temp(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum);
                    CO2ZoneTimeMinus3Temp(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum);
                }
                if (Contaminant.GenericContamSimulation) {
                    GCZoneTimeMinus1Temp(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum);
                    GCZoneTimeMinus2Temp(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum);
                    GCZoneTimeMinus3Temp(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum);
                }
            }

            if (ZoneAirSolutionAlgo != Use3rdOrder) {
                if (Contaminant.CO2Simulation) {
                    if (ShortenTimeStepSys && TimeStepSys < TimeStepZone) {
                        if (PreviousTimeStep < TimeStepZone) {
                            ZoneCO21(ZoneNum) = ZoneCO2M2(ZoneNum);
                        } else {
                            ZoneCO21(ZoneNum) = ZoneCO2MX(ZoneNum);
                        }
                        ShortenTimeStepSysRoomAir = true;
                    } else {
                        ZoneCO21(ZoneNum) = ZoneAirCO2(ZoneNum);
                    }
                }
                if (Contaminant.GenericContamSimulation) {
                    if (ShortenTimeStepSys && TimeStepSys < TimeStepZone) {
                        if (PreviousTimeStep < TimeStepZone) {
                            ZoneGC1(ZoneNum) = ZoneGCM2(ZoneNum);
                        } else {
                            ZoneGC1(ZoneNum) = ZoneGCMX(ZoneNum);
                        }
                        ShortenTimeStepSysRoomAir = true;
                    } else {
                        ZoneGC1(ZoneNum) = ZoneAirGC(ZoneNum);
                    }
                }
            }

            if (Contaminant.CO2Simulation) {

                CO2PredictedRate(ZoneNum) = 0.0;
                LoadToCO2SetPoint = 0.0;
                ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP = 0.0;

                // Check to see if this is a "CO2 controlled zone"
                ControlledCO2ZoneFlag = false;
                // Check all the controlled zones to see if it matches the zone simulated
                for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
                    if (ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == ZoneNum) {
                        if (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) > 0.0) {
                            ZoneAirCO2SetPoint = ZoneCO2SetPoint(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                            if (ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                                ZoneAirCO2SetPoint = ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointValue;
                            }
                            ControlledCO2ZoneFlag = true;
                            break;
                        }
                    }
                }
                if (!ControlledCO2ZoneFlag) {
                    for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
                        if (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) > 0.0) {
                            ZoneAirCO2SetPoint = ZoneCO2SetPoint(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                            if (ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                                ZoneAirCO2SetPoint = ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointValue;
                            }
                            if (ContaminantControlledZone(ContControlledZoneNum).NumOfZones >= 1) {
                                if (ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum != ZoneNum) {
                                    for (I = 1; I <= ContaminantControlledZone(ContControlledZoneNum).NumOfZones; ++I) {
                                        if (ContaminantControlledZone(ContControlledZoneNum).ControlZoneNum(I) == ZoneNum) {
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
                    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);

                    // Calculate Co2 from infiltration + humidity added from latent load
                    // to determine system added/subtracted moisture.
                    CO2Gain = ZoneCO2Gain(ZoneNum) * RhoAir * 1.0e6;

                    SysTimeStepInSeconds = SecInHour * TimeStepSys;

                    // Calculate the coefficients for the 3rd Order derivative for final
                    // zone CO2.  The A, B, C coefficients are analogous to the CO2 balance.
                    // Assume that the system will have flow
                    if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                        AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                        (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                         AirflowNetwork::AirflowNetworkFanActivated)) {
                        // Multizone airflow calculated in AirflowNetwork
                        B = CO2Gain + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHrCO +
                            AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHrCO;
                        A = AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHr + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHr;
                    } else {
                        B = CO2Gain + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) +
                            MixingMassFlowCO2(ZoneNum) + MDotOA(ZoneNum) * OutdoorCO2;
                        A = OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    }
                    C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;

                    // Use a 3rd Order derivative to predict zone moisture addition or removal and
                    // smooth the changes using the zone air capacitance.  Positive values of CO2 Load means that
                    // this amount of CO2 must be added to the zone to reach the setpoint.  Negative values represent
                    // the amount of CO2 that must be removed by the system.
                    {
                        auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                        if (SELECT_CASE_var == Use3rdOrder) {
                            LoadToCO2SetPoint = ((11.0 / 6.0) * C + A) * ZoneAirCO2SetPoint -
                                                (B + C * (3.0 * CO2ZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * CO2ZoneTimeMinus2Temp(ZoneNum) +
                                                          (1.0 / 3.0) * CO2ZoneTimeMinus3Temp(ZoneNum)));
                            // Exact solution
                        } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                            if (A == 0.0) { // B=0
                                LoadToCO2SetPoint = C * (ZoneAirCO2SetPoint - ZoneCO21(ZoneNum)) - B;
                            } else {
                                LoadToCO2SetPoint = A * (ZoneAirCO2SetPoint - ZoneCO21(ZoneNum) * std::exp(min(700.0, -A / C))) /
                                                        (1.0 - std::exp(min(700.0, -A / C))) -
                                                    B;
                            }
                        } else if (SELECT_CASE_var == UseEulerMethod) {
                            LoadToCO2SetPoint = C * (ZoneAirCO2SetPoint - ZoneCO21(ZoneNum)) + A * ZoneAirCO2SetPoint - B;
                        }
                    }
                    if (ZoneAirCO2SetPoint > OutdoorCO2 && LoadToCO2SetPoint < 0.0) {
                        ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP = LoadToCO2SetPoint / (OutdoorCO2 - ZoneAirCO2SetPoint);
                    }
                }

                // Apply the Zone Multiplier to the total zone moisture load
                ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP *= Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
                CO2PredictedRate(ZoneNum) = ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP;
            }

            if (Contaminant.GenericContamSimulation) {

                GCPredictedRate(ZoneNum) = 0.0;
                LoadToGCSetPoint = 0.0;
                ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP = 0.0;

                // Check to see if this is a "GC controlled zone"
                ControlledGCZoneFlag = false;
                // Check all the controlled zones to see if it matches the zone simulated
                for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
                    if (ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum == ZoneNum) {
                        if (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) > 0.0) {
                            ZoneAirGCSetPoint = ZoneGCSetPoint(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                            if (ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                                ZoneAirGCSetPoint = ContaminantControlledZone(ContControlledZoneNum).EMSOverrideGCSetPointValue;
                            }
                            ControlledGCZoneFlag = true;
                            break;
                        }
                    }
                }
                if (!ControlledGCZoneFlag) {
                    for (ContControlledZoneNum = 1; ContControlledZoneNum <= NumContControlledZones; ++ContControlledZoneNum) {
                        if (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum).AvaiSchedPtr) > 0.0) {
                            ZoneAirGCSetPoint = ZoneGCSetPoint(ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum);
                            if (ContaminantControlledZone(ContControlledZoneNum).EMSOverrideCO2SetPointOn) {
                                ZoneAirGCSetPoint = ContaminantControlledZone(ContControlledZoneNum).EMSOverrideGCSetPointValue;
                            }
                            if (ContaminantControlledZone(ContControlledZoneNum).NumOfZones >= 1) {
                                if (ContaminantControlledZone(ContControlledZoneNum).ActualZoneNum != ZoneNum) {
                                    for (I = 1; I <= ContaminantControlledZone(ContControlledZoneNum).NumOfZones; ++I) {
                                        if (ContaminantControlledZone(ContControlledZoneNum).ControlZoneNum(I) == ZoneNum) {
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
                    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);

                    // Calculate generic contaminant from infiltration + humidity added from latent load
                    // to determine system added/subtracted moisture.
                    GCGain = ZoneGCGain(ZoneNum) * RhoAir * 1.0e6;

                    SysTimeStepInSeconds = SecInHour * TimeStepSys;

                    // Calculate the coefficients for the 3rd Order derivative for final
                    // zone GC.  The A, B, C coefficients are analogous to the GC balance.
                    // Assume that the system will have flow
                    if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                        AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                        (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                         AirflowNetwork::AirflowNetworkFanActivated)) {
                        // Multizone airflow calculated in AirflowNetwork
                        B = GCGain + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHrGC +
                            AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHrGC;
                        A = AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHr + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHr;
                    } else {
                        B = GCGain + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorGC) + MixingMassFlowGC(ZoneNum) +
                            MDotOA(ZoneNum) * OutdoorGC;
                        A = OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    }
                    C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpGenContam / SysTimeStepInSeconds;

                    // Use a 3rd Order derivative to predict zone moisture addition or removal and
                    // smooth the changes using the zone air capacitance.  Positive values of GC Load means that
                    // this amount of GC must be added to the zone to reach the setpoint.  Negative values represent
                    // the amount of GC that must be removed by the system.
                    {
                        auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                        if (SELECT_CASE_var == Use3rdOrder) {
                            LoadToGCSetPoint = ((11.0 / 6.0) * C + A) * ZoneAirGCSetPoint -
                                               (B + C * (3.0 * GCZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * GCZoneTimeMinus2Temp(ZoneNum) +
                                                         (1.0 / 3.0) * GCZoneTimeMinus3Temp(ZoneNum)));
                            // Exact solution
                        } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                            if (A == 0.0) { // B=0
                                LoadToGCSetPoint = C * (ZoneAirGCSetPoint - ZoneGC1(ZoneNum)) - B;
                            } else {
                                LoadToGCSetPoint =
                                    A * (ZoneAirGCSetPoint - ZoneGC1(ZoneNum) * std::exp(min(700.0, -A / C))) / (1.0 - std::exp(min(700.0, -A / C))) -
                                    B;
                            }
                        } else if (SELECT_CASE_var == UseEulerMethod) {
                            LoadToGCSetPoint = C * (ZoneAirGCSetPoint - ZoneGC1(ZoneNum)) + A * ZoneAirGCSetPoint - B;
                        }
                    }
                    if (ZoneAirGCSetPoint > OutdoorGC && LoadToGCSetPoint < 0.0) {
                        ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP = LoadToGCSetPoint / (OutdoorGC - ZoneAirGCSetPoint);
                    }
                }

                // Apply the Zone Multiplier to the total zone moisture load
                ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP *= Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
                GCPredictedRate(ZoneNum) = ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP;
            }
        }
    }

    void PushZoneTimestepHistories()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July, 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // push histories for timestep advancing.
        // This subroutine is modified from PushZoneTimestepHistories in ZoneTempPredictorCorrector module

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

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
        int ZoneNum;

        // Push the temperature and humidity ratio histories

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (Contaminant.CO2Simulation) {
                CO2ZoneTimeMinus4(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum);
                CO2ZoneTimeMinus3(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum);
                CO2ZoneTimeMinus2(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum);
                CO2ZoneTimeMinus1(ZoneNum) = ZoneAirCO2Avg(ZoneNum); // using average for whole zone time step.
                ZoneAirCO2(ZoneNum) = ZoneAirCO2Temp(ZoneNum);

                if (ZoneAirSolutionAlgo != Use3rdOrder) {
                    ZoneCO2M2(ZoneNum) = ZoneCO2MX(ZoneNum);
                    ZoneCO2MX(ZoneNum) = ZoneAirCO2Avg(ZoneNum); // using average for whole zone time step.
                }
            }

            if (Contaminant.GenericContamSimulation) {
                GCZoneTimeMinus4(ZoneNum) = GCZoneTimeMinus3(ZoneNum);
                GCZoneTimeMinus3(ZoneNum) = GCZoneTimeMinus2(ZoneNum);
                GCZoneTimeMinus2(ZoneNum) = GCZoneTimeMinus1(ZoneNum);
                GCZoneTimeMinus1(ZoneNum) = ZoneAirGCAvg(ZoneNum); // using average for whole zone time step.
                ZoneAirGC(ZoneNum) = ZoneAirGCTemp(ZoneNum);

                if (ZoneAirSolutionAlgo != Use3rdOrder) {
                    ZoneGCM2(ZoneNum) = ZoneGCMX(ZoneNum);
                    ZoneGCMX(ZoneNum) = ZoneAirGCAvg(ZoneNum); // using average for whole zone time step.
                }
            }
        } // zone loop
    }

    void PushSystemTimestepHistories()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July, 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // push histories
        // This subroutine is modified from PushSystemTimestepHistories in ZoneTempPredictorCorrector module

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

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
        int ZoneNum;

        // Push the temperature and humidity ratio histories back in time

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (Contaminant.CO2Simulation) {
                DSCO2ZoneTimeMinus4(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum);
                DSCO2ZoneTimeMinus3(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum);
                DSCO2ZoneTimeMinus2(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum);
                DSCO2ZoneTimeMinus1(ZoneNum) = ZoneAirCO2(ZoneNum);
            }
            if (Contaminant.GenericContamSimulation) {
                DSGCZoneTimeMinus4(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum);
                DSGCZoneTimeMinus3(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum);
                DSGCZoneTimeMinus2(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum);
                DSGCZoneTimeMinus1(ZoneNum) = ZoneAirGC(ZoneNum);
            }
        } // zone loop

        if (ZoneAirSolutionAlgo != Use3rdOrder) {
            for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                if (Contaminant.CO2Simulation) {
                    ZoneCO2M2(ZoneNum) = ZoneCO2MX(ZoneNum);
                    ZoneCO2MX(ZoneNum) = ZoneAirCO2Temp(ZoneNum); // using average for whole zone time step.
                }
                if (Contaminant.GenericContamSimulation) {
                    ZoneGCM2(ZoneNum) = ZoneGCMX(ZoneNum);
                    ZoneGCMX(ZoneNum) = ZoneAirGCTemp(ZoneNum); // using average for whole zone time step.
                }
            } // zone loop
        }
    }

    void RevertZoneTimestepHistories()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July, 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // rewind histories to undo inadvertent pushing
        // This subroutine is modified from RevertZoneTimestepHistories in ZoneTempPredictorCorrector module

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

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
        int ZoneNum;

        // REvert the contaminnants histories

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (Contaminant.CO2Simulation) {
                CO2ZoneTimeMinus1(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum);
                CO2ZoneTimeMinus2(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum);
                CO2ZoneTimeMinus3(ZoneNum) = CO2ZoneTimeMinus4(ZoneNum);
            }
            if (Contaminant.GenericContamSimulation) {
                GCZoneTimeMinus1(ZoneNum) = GCZoneTimeMinus2(ZoneNum);
                GCZoneTimeMinus2(ZoneNum) = GCZoneTimeMinus3(ZoneNum);
                GCZoneTimeMinus3(ZoneNum) = GCZoneTimeMinus4(ZoneNum);
            }
        } // zone loop
    }

    void InverseModelCO2(int const ZoneNum,           // Zone number
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
        //       MODIFIED		na
        //       RE-ENGINEERED	na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine inversely solve infiltration airflow rate or people count with zone air CO2 concentration measurements.

        // Using/Aliasing
        using DataEnvironment::DayOfYear;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InverseModelCO2");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
        SysTimeStepInSeconds = SecInHour * TimeStepSys;

        Zone(ZoneNum).ZoneMeasuredCO2Concentration = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneMeasuredCO2ConcentrationSchedulePtr);

        if (DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear && DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
            ZoneAirCO2(ZoneNum) = Zone(ZoneNum).ZoneMeasuredCO2Concentration;

            if (HybridModelZone(ZoneNum).InfiltrationCalc_C && UseZoneTimeStepHistory) {
                static std::string const RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");

                // Conditionally calculate the CO2-dependent and CO2-independent terms.
                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirCO2ConcentrationSchedulePtr);

                    SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMxCO2_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration;

                    AA = SumSysM_HM + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    BB = SumSysMxCO2_HM + CO2Gain + ((VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) + MixingMassFlowCO2(ZoneNum) +
                         MDotOA(ZoneNum) * OutdoorCO2;

                } else {
                    AA = VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    BB = CO2Gain + ((VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) + MixingMassFlowCO2(ZoneNum) +
                         MDotOA(ZoneNum) * OutdoorCO2;
                }

                CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
                DD = (3.0 * CO2ZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * CO2ZoneTimeMinus2Temp(ZoneNum) +
                      (1.0 / 3.0) * CO2ZoneTimeMinus3Temp(ZoneNum));

                zone_M_CO2 = Zone(ZoneNum).ZoneMeasuredCO2Concentration;
                delta_CO2 = (Zone(ZoneNum).ZoneMeasuredCO2Concentration - OutdoorCO2) / 1000;
                CpAir = PsyCpAirFnW(OutHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Zone(ZoneNum).OutDryBulbTemp, OutHumRat, RoutineNameInfiltration);

                if (Zone(ZoneNum).ZoneMeasuredCO2Concentration == OutdoorCO2) {
                    M_inf = 0.0;
                } else {
                    M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredCO2Concentration) / delta_CO2;
                }

                // Add threshold for air change rate
                ACH_inf = max(0.0, min(10.0, M_inf / (CpAir * AirDensity / SecInHour * Zone(ZoneNum).Volume)));
                M_inf = ACH_inf * Zone(ZoneNum).Volume * AirDensity / SecInHour;
                Zone(ZoneNum).MCPIHM = M_inf;
                Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;
            }

            // Hybrid Model calculate people count
            if (HybridModelZone(ZoneNum).PeopleCountCalc_C && UseZoneTimeStepHistory) {
                Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                ActivityLevel = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                CO2GenRate = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleCO2GenRateSchedulePtr);

                if (ActivityLevel <= 0.0) {
                    ActivityLevel = 130.0; // 130.0 is the default people activity level [W]
                }
                if (CO2GenRate <= 0.0) {
                    CO2GenRate = 0.0000000382; // 0.0000000382 is the default CO2
                                               // generation rate [m3/(s*W)]
                }

                // Conditionally calculate the CO2-dependent and CO2-independent terms.
                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirCO2ConcentrationSchedulePtr);

                    SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMxCO2_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirCO2Concentration;

                    AA = SumSysM_HM + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                         MDotOA(ZoneNum);
                    BB = CO2GainExceptPeople + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) + (SumSysMxCO2_HM) +
                         MixingMassFlowCO2(ZoneNum) + MDotOA(ZoneNum) * OutdoorCO2;

                } else {
                    AA = ZoneMassFlowRate + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                         MDotOA(ZoneNum);
                    BB = CO2GainExceptPeople + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) +
                         (CO2MassFlowRate) + MixingMassFlowCO2(ZoneNum) + MDotOA(ZoneNum) * OutdoorCO2;
                }

                CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
                DD = (3.0 * CO2ZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * CO2ZoneTimeMinus2Temp(ZoneNum) +
                      (1.0 / 3.0) * CO2ZoneTimeMinus3Temp(ZoneNum));

                CO2GainPeople = (((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredCO2Concentration - BB - CC * DD) / (1000000 * RhoAir);

                // Make sure the results are reasonable
                UpperBound = CO2Gain / (1000000 * RhoAir * CO2GenRate * ActivityLevel);
                NumPeople = min(UpperBound, CO2GainPeople / (CO2GenRate * ActivityLevel));

                NumPeople = floor(NumPeople * 100.00 + 0.5) / 100.00;
                if (NumPeople < 0.05) {
                    NumPeople = 0;
                }
                Zone(ZoneNum).NumOccHM = NumPeople;
            }
        }

        // Update zone humidity ratio in the previous steps
        CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus2Temp(ZoneNum);
        CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus1Temp(ZoneNum);
        CO2ZoneTimeMinus1Temp(ZoneNum) = Zone(ZoneNum).ZoneMeasuredCO2Concentration;
    }

    void CorrectZoneContaminants(bool const ShortenTimeStepSys,
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
        // for BLAST.

        // Using/Aliasing
        using DataDefineEquip::AirDistUnit;
        using DataEnvironment::DayOfYear;
        using DataLoopNode::Node;
        using DataZoneEquipment::ZoneEquipConfig;
        using ZonePlenum::NumZoneReturnPlenums;
        using ZonePlenum::NumZoneSupplyPlenums;
        using ZonePlenum::ZoneRetPlenCond;
        using ZonePlenum::ZoneSupPlenCond;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CorrectZoneContaminants");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

        // FLOW:
        // Update zone CO2
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            if (Contaminant.CO2Simulation) {
                AZ(ZoneNum) = 0.0;
                BZ(ZoneNum) = 0.0;
                CZ(ZoneNum) = 0.0;
            }
            if (Contaminant.GenericContamSimulation) {
                AZGC(ZoneNum) = 0.0;
                BZGC(ZoneNum) = 0.0;
                CZGC(ZoneNum) = 0.0;
            }
            // Update variables
            if (ShortenTimeStepSys) {
                // time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
                    if (Contaminant.CO2Simulation) {
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ZoneAirCO2(ZoneNum),
                                                      CO2ZoneTimeMinus1(ZoneNum),
                                                      CO2ZoneTimeMinus2(ZoneNum),
                                                      CO2ZoneTimeMinus3(ZoneNum),
                                                      CO2ZoneTimeMinus4(ZoneNum),
                                                      ZoneAirCO2(ZoneNum),
                                                      DSCO2ZoneTimeMinus1(ZoneNum),
                                                      DSCO2ZoneTimeMinus2(ZoneNum),
                                                      DSCO2ZoneTimeMinus3(ZoneNum),
                                                      DSCO2ZoneTimeMinus4(ZoneNum));
                    }
                    if (Contaminant.GenericContamSimulation) {
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ZoneAirGC(ZoneNum),
                                                      GCZoneTimeMinus1(ZoneNum),
                                                      GCZoneTimeMinus2(ZoneNum),
                                                      GCZoneTimeMinus3(ZoneNum),
                                                      GCZoneTimeMinus4(ZoneNum),
                                                      ZoneAirGC(ZoneNum),
                                                      DSGCZoneTimeMinus1(ZoneNum),
                                                      DSGCZoneTimeMinus2(ZoneNum),
                                                      DSGCZoneTimeMinus3(ZoneNum),
                                                      DSGCZoneTimeMinus4(ZoneNum));
                    }

                } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                         // do nothing because DS history would have been pushed prior and should be ready?
                }
            }

            // now update the variables actually used in the balance equations.
            if (!UseZoneTimeStepHistory) {
                if (Contaminant.CO2Simulation) {
                    CO2ZoneTimeMinus1Temp(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum);
                    CO2ZoneTimeMinus2Temp(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum);
                    CO2ZoneTimeMinus3Temp(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum);
                }
                if (Contaminant.GenericContamSimulation) {
                    GCZoneTimeMinus1Temp(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum);
                    GCZoneTimeMinus2Temp(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum);
                    GCZoneTimeMinus3Temp(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum);
                }
            } else {
                if (Contaminant.CO2Simulation) {
                    CO2ZoneTimeMinus1Temp(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum);
                    CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum);
                    CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum);
                }
                if (Contaminant.GenericContamSimulation) {
                    GCZoneTimeMinus1Temp(ZoneNum) = GCZoneTimeMinus1(ZoneNum);
                    GCZoneTimeMinus2Temp(ZoneNum) = GCZoneTimeMinus2(ZoneNum);
                    GCZoneTimeMinus3Temp(ZoneNum) = GCZoneTimeMinus3(ZoneNum);
                }
            }

            // Start to calculate zone CO2 and genric contaminant levels
            CO2MassFlowRate = 0.0;
            GCMassFlowRate = 0.0;
            ZoneMassFlowRate = 0.0;
            ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

            // Check to see if this is a controlled zone
            ControlledZoneAirFlag = false;
            for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum) {
                if (!Zone(ZoneEquipConfigNum).IsControlled) continue;
                if (ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != ZoneNum) continue;
                ControlledZoneAirFlag = true;
                break;
            } // ZoneEquipConfigNum

            // Check to see if this is a plenum zone
            ZoneRetPlenumAirFlag = false;
            for (ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum) {
                if (ZoneRetPlenCond(ZoneRetPlenumNum).ActualZoneNum != ZoneNum) continue;
                ZoneRetPlenumAirFlag = true;
                break;
            } // ZoneRetPlenumNum
            ZoneSupPlenumAirFlag = false;
            for (ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum) {
                if (ZoneSupPlenCond(ZoneSupPlenumNum).ActualZoneNum != ZoneNum) continue;
                ZoneSupPlenumAirFlag = true;
                break;
            } // ZoneSupPlenumNum

            if (ControlledZoneAirFlag) { // If there is system flow then calculate the flow rates

                // Calculate moisture flow rate into each zone
                for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {

                    if (Contaminant.CO2Simulation) {
                        CO2MassFlowRate += (Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                            Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).CO2) /
                                           ZoneMult;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        GCMassFlowRate += (Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                           Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).GenContam) /
                                          ZoneMult;
                    }
                    ZoneMassFlowRate += Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
                } // NodeNum

                // Do the calculations for the plenum zone
            } else if (ZoneRetPlenumAirFlag) {
                for (NodeNum = 1; NodeNum <= ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {

                    if (Contaminant.CO2Simulation) {
                        CO2MassFlowRate += (Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                            Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).CO2) /
                                           ZoneMult;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        GCMassFlowRate += (Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                           Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).GenContam) /
                                          ZoneMult;
                    }
                    ZoneMassFlowRate += Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
                } // NodeNum
                  // add in the leak flow
                for (ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                    ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                    if (AirDistUnit(ADUNum).UpStreamLeak) {
                        ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
                        if (Contaminant.CO2Simulation) {
                            CO2MassFlowRate += (AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).CO2) / ZoneMult;
                        }
                        if (Contaminant.GenericContamSimulation) {
                            GCMassFlowRate += (AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).GenContam) / ZoneMult;
                        }
                        ZoneMassFlowRate += AirDistUnit(ADUNum).MassFlowRateUpStrLk / ZoneMult;
                    }
                    if (AirDistUnit(ADUNum).DownStreamLeak) {
                        ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
                        if (Contaminant.CO2Simulation) {
                            CO2MassFlowRate += (AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).CO2) / ZoneMult;
                        }
                        if (Contaminant.GenericContamSimulation) {
                            GCMassFlowRate += (AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).GenContam) / ZoneMult;
                        }
                        ZoneMassFlowRate += AirDistUnit(ADUNum).MassFlowRateDnStrLk / ZoneMult;
                    }
                }

            } else if (ZoneSupPlenumAirFlag) {

                if (Contaminant.CO2Simulation) {
                    CO2MassFlowRate +=
                        (Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate * Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).CO2) /
                        ZoneMult;
                }
                if (Contaminant.GenericContamSimulation) {
                    GCMassFlowRate += (Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate *
                                       Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).GenContam) /
                                      ZoneMult;
                }
                ZoneMassFlowRate += Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate / ZoneMult;
            }

            SysTimeStepInSeconds = SecInHour * TimeStepSys;

            // Calculate the coefficients for the 3rd order derivative for final
            // zone humidity ratio.  The A, B, C coefficients are analogous to the
            // CO2 balance.  There are 2 cases that should be considered, system
            // operating and system shutdown.

            RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);
            //    RhoAir = ZoneAirDensityCO(ZoneNum)

            if (Contaminant.CO2Simulation) ZoneAirDensityCO(ZoneNum) = RhoAir;
            // Calculate Co2 internal gain
            if (Contaminant.CO2Simulation) CO2Gain = ZoneCO2Gain(ZoneNum) * RhoAir * 1.0e6;
            if (Contaminant.CO2Simulation) CO2GainExceptPeople = ZoneCO2GainExceptPeople(ZoneNum) * RhoAir * 1.0e6; // Addded for hybrid model
            if (Contaminant.GenericContamSimulation) GCGain = ZoneGCGain(ZoneNum) * RhoAir * 1.0e6;

            if (Contaminant.CO2Simulation) {
                B = CO2Gain + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorCO2) + (CO2MassFlowRate) +
                    MixingMassFlowCO2(ZoneNum) + MDotOA(ZoneNum) * OutdoorCO2;
                A = ZoneMassFlowRate + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                    MDotOA(ZoneNum);
                if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                    AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                    (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                     AirflowNetwork::AirflowNetworkFanActivated)) {
                    // Multizone airflow calculated in AirflowNetwork
                    B = CO2Gain +
                        (AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHrCO +
                         AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHrCO) +
                        CO2MassFlowRate;
                    A = ZoneMassFlowRate + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHr +
                        AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHr;
                }
                C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpCO2 / SysTimeStepInSeconds;
            }

            if (Contaminant.CO2Simulation) {
                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    B += AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).TotalCO2;
                }

                AZ(ZoneNum) = A;
                BZ(ZoneNum) = B;
                CZ(ZoneNum) = C;

                // Use a 3rd order derivative to predict final zone CO2 and
                // smooth the changes using the zone air capacitance.
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZoneAirCO2Temp(ZoneNum) = (B + C * (3.0 * CO2ZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * CO2ZoneTimeMinus2Temp(ZoneNum) +
                                                            (1.0 / 3.0) * CO2ZoneTimeMinus3Temp(ZoneNum))) /
                                                  ((11.0 / 6.0) * C + A);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (A == 0.0) { // B=0
                            ZoneAirCO2Temp(ZoneNum) = ZoneCO21(ZoneNum) + B / C;
                        } else {
                            ZoneAirCO2Temp(ZoneNum) = (ZoneCO21(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZoneAirCO2Temp(ZoneNum) = (C * ZoneCO21(ZoneNum) + B) / (C + A);
                    }
                }

                // Set the CO2 to zero if the zone has been large sinks
                if (ZoneAirCO2Temp(ZoneNum) < 0.0) ZoneAirCO2Temp(ZoneNum) = 0.0;

                ZoneAirCO2(ZoneNum) = ZoneAirCO2Temp(ZoneNum);

                if ((HybridModelZone(ZoneNum).InfiltrationCalc_C || HybridModelZone(ZoneNum).PeopleCountCalc_C) && (!WarmupFlag) && (!DoingSizing)) {
                    InverseModelCO2(ZoneNum, CO2Gain, CO2GainExceptPeople, ZoneMassFlowRate, CO2MassFlowRate, RhoAir);
                }
                // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
                ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
                if (ZoneNodeNum > 0) {
                    Node(ZoneNodeNum).CO2 = ZoneAirCO2Temp(ZoneNum);
                }
            }

            if (Contaminant.GenericContamSimulation) {
                B = GCGain + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutdoorGC) + (GCMassFlowRate) +
                    MixingMassFlowGC(ZoneNum) + MDotOA(ZoneNum) * OutdoorGC;
                A = ZoneMassFlowRate + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                    MDotOA(ZoneNum);
                if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                    AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                    (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                     AirflowNetwork::AirflowNetworkFanActivated)) {
                    // Multizone airflow calculated in AirflowNetwork
                    B = GCGain +
                        (AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHrGC +
                         AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHrGC) +
                        GCMassFlowRate;
                    A = ZoneMassFlowRate + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMHr +
                        AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMHr;
                }
                C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpGenContam / SysTimeStepInSeconds;
            }

            if (Contaminant.GenericContamSimulation) {
                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    B += AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).TotalGC;
                }

                AZGC(ZoneNum) = A;
                BZGC(ZoneNum) = B;
                CZGC(ZoneNum) = C;

                // Use a 3rd order derivative to predict final zone generic contaminant and
                // smooth the changes using the zone air capacitance.
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZoneAirGCTemp(ZoneNum) = (B + C * (3.0 * GCZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * GCZoneTimeMinus2Temp(ZoneNum) +
                                                           (1.0 / 3.0) * GCZoneTimeMinus3Temp(ZoneNum))) /
                                                 ((11.0 / 6.0) * C + A);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (A == 0.0) { // B=0
                            ZoneAirGCTemp(ZoneNum) = ZoneGC1(ZoneNum) + B / C;
                        } else {
                            ZoneAirGCTemp(ZoneNum) = (ZoneGC1(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZoneAirGCTemp(ZoneNum) = (C * ZoneGC1(ZoneNum) + B) / (C + A);
                    }
                }

                // Set the generic contaminant to zero if the zone has been large sinks
                if (ZoneAirGCTemp(ZoneNum) < 0.0) ZoneAirGCTemp(ZoneNum) = 0.0;

                ZoneAirGC(ZoneNum) = ZoneAirGCTemp(ZoneNum);

                // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
                ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
                if (ZoneNodeNum > 0) {
                    Node(ZoneNodeNum).GenContam = ZoneAirGCTemp(ZoneNum);
                }
            }
        }
    }

} // namespace ZoneContaminantPredictorCorrector

} // namespace EnergyPlus
