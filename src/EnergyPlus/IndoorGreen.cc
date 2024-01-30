// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/IndoorGreen.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/api/datatransfer.h>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/EMSManager.hh>
#include <iostream>
#include <fstream>

namespace EnergyPlus {

namespace IndoorGreen {
    // Module containing the routines dealing with the Indoor Greenery Systems.

    // MODULE INFORMATION:  Liping Wang
    //       DATE WRITTEN   Oct 2023
    //       RE-ENGINEERED  na

    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;

    void SimIndoorGreen(EnergyPlusData &state)
    {

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Indoor Greenery Systems.
        // Assumptions: 1) one system per zone; 2) the effects of indoor greenery systems on surface heat balance are currently ignored. 

        if (state.dataIndoorGreen->getInputFlag) { 
            bool ErrorsFound(false);
            const char *RoutineName("IndoorGreenInputs: "); // include trailing blank space
            GetIndoorGreenInput(state,ErrorsFound);
            if (ErrorsFound) {
                ShowFatalError(state, format("{}Errors found in input.  Program terminates.", RoutineName));
            }
            state.dataIndoorGreen->getInputFlag = false;
        }
        if (state.dataIndoorGreen->NumIndoorGreen > 0) {
            InitIndoorGreen(state);

            // Simulate evapotranspiration from indoor greenery systems
            ETModel(state);
        } 
    }

    void GetIndoorGreenInput(EnergyPlusData &state, bool &ErrorsFound)
    {
        // PURPOSE OF THIS SUBROUTINE:
        // Get the input for the indoor greenery system data and store the input data in the indoorgreen array.

        // METHODOLOGY EMPLOYED:
        // Use the Get routines from the InputProcessor module.

        static constexpr std::string_view RoutineName("GetIndoorGreenInput: ");
        std::string_view cCurrentModuleObject = "IndoorLivingWall"; //match the idd
        int NumNums;        // Number of real numbers returned by GetObjectItem
        int NumAlphas;      // Number of alphanumerics returned by GetObjectItem
        int IndoorGreenNum; // Indoor Green index
        int IOStat;         // Status flag from GetObjectItem
        Real64 SchMin;
        Real64 SchMax;

        state.dataIndoorGreen->NumIndoorGreen = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        // Get input for IndoorGreen objects
        if (state.dataIndoorGreen->NumIndoorGreen > 0)
            state.dataIndoorGreen->indoorgreen.allocate(state.dataIndoorGreen->NumIndoorGreen); // Allocate the IndoorGreen input data array

        // Input the data for each Indoor Greenery System
        for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     IndoorGreenNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            Util::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName = state.dataIPShortCut->cAlphaArgs(1);
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfName = state.dataIPShortCut->cAlphaArgs(2);
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr =
                Util::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataSurface->Surface);
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr <= 0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(2),
                                       state.dataIPShortCut->cAlphaArgs(2)));
                ErrorsFound = true;
            } else {
                // check for SurfaceProperty:HeatBalanceSourceTerm
                if (state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).InsideHeatSourceTermSchedule > 0) {
                    ShowSevereError(state,
                                    format("The indoor green surface {} has an Inside Face Heat Source Term Schedule defined. This surface cannot "
                                           "also be used for indoor green.",
                                           state.dataIPShortCut->cAlphaArgs(2)));
                    ErrorsFound = true;
                }
                // get zone pointer
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr =
                    state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).Zone;
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SpacePtr =
                    state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).spaceNum;
                
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr <= 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {} entered={}",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           state.dataIPShortCut->cAlphaArgs(2)));
                    ErrorsFound = true;
                } else if (state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).ExtBoundCond < 0 ||
                           state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).HeatTransferAlgorithm !=
                               DataSurfaces::HeatTransferModel::CTF) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {} entered={}, not a valid surface for indoor green module",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(2),
                                           state.dataIPShortCut->cAlphaArgs(2)));
                    ErrorsFound = true;
                }
            }
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedPtr =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedPtr == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    format("{} =\"{}\", {} is required.",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(3)));
                } else {
                    ShowSevereError(state,
                                    format("{} =\"{}\", invalid {} entered={}",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(3),
                                           state.dataIPShortCut->cAlphaArgs(3)));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = ScheduleManager::GetScheduleMinValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedPtr);
                SchMax = ScheduleManager::GetScheduleMaxValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {}, minimum is < 0.0",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(3)));
                        ShowContinueError(
                            state,
                            format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {}, maximum is < 0.0",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(3)));
                        ShowContinueError(
                            state,
                            format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                }
            }
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod = 1; // default
            if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "PENMAN-MONTEITH")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod = 1; // default
            } else if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "STANGHELLINI")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod = 2;
            } else if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "DATA-DRIVEN")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod = 3;
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(4),
                                       state.dataIPShortCut->cAlphaArgs(4)));
                ErrorsFound = true;
            }
            // read lighting method (LED=1; Daylight=2; LED-Daylight=3)
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod = 1; // default
            if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(5)), "LED")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod = 1; // default
            } else if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(5)), "DAYLIGHT")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod = 2;
            } else if (Util::SameString(Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(5)), "LED-DAYLIGHT")) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod = 3;
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(5),
                                       state.dataIPShortCut->cAlphaArgs(5)));
                ErrorsFound = true;
            }

            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 1) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDPtr =
                    ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDPtr == 0) {
                    if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                        ShowSevereError(state,
                                        format("{} =\"{}\", {} is required.",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(6)));
                    } else {
                        ShowSevereError(state,
                                        format("{} =\"{}\", invalid {} entered={}",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(6),
                                               state.dataIPShortCut->cAlphaArgs(6)));
                    }
                    ErrorsFound = true;
                } else { // check min/max on schedule
                    SchMin = ScheduleManager::GetScheduleMinValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDPtr);
                    SchMax = ScheduleManager::GetScheduleMaxValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDPtr);
                    if (SchMin < 0.0 || SchMax < 0.0) {
                        if (SchMin < 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", {}, minimum is < 0.0",
                                                   RoutineName,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaFieldNames(6)));
                            ShowContinueError(
                                state,
                                format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(6), SchMin));
                            ErrorsFound = true;
                        }
                        if (SchMax < 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", {}, maximum is < 0.0",
                                                   RoutineName,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaFieldNames(6)));
                            ShowContinueError(
                                state,
                                format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(6), SchMin));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 2 ||
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 3) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr =
                    Util::FindItemInList(state.dataIPShortCut->cAlphaArgs(7),
                                         state.dataDayltg->DaylRefPt,
                                         &EnergyPlus::Dayltg::RefPointData::Name); // Field: Daylighting Reference Point Name
                //state.dataDaylightingData->daylightControl
                //if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr == 0) {
                //    ShowSevereError(state,
                //                    format("{}: invalid {}=\"{}\" for object named: {}",
                //                           state.dataIPShortCut->cCurrentModuleObject,
                //                           state.dataIPShortCut->cAlphaFieldNames(7),
                //                           state.dataIPShortCut->cAlphaArgs(7),
                //                           state.dataIPShortCut->cAlphaArgs(1)));
                //    ErrorsFound = true;
                //    continue;
                //}
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightControlPtr =
                    Util::FindItemInList(state.dataIPShortCut->cAlphaArgs(7),
                                         state.dataDayltg->daylightControl,
                                         &EnergyPlus::Dayltg::DaylightingControl::Name); // Field: Daylighting Control Name
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightControlPtr == 0) {
                    ShowSevereError(state,
                                    format("{}: invalid {}=\"{}\" for object named: {}",
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaFieldNames(7),
                                           state.dataIPShortCut->cAlphaArgs(7),
                                           state.dataIPShortCut->cAlphaArgs(1)));
                    ErrorsFound = true;
                    continue;
                }
            }
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 3) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDDaylightTargetPtr =
                    ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDDaylightTargetPtr == 0) {
                    if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                        ShowSevereError(state,
                                        format("{} =\"{}\", {} is required.",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(8)));
                    } else {
                        ShowSevereError(state,
                                        format("{} =\"{}\", invalid {} entered={}",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(8),
                                               state.dataIPShortCut->cAlphaArgs(8)));
                    }
                    ErrorsFound = true;
                } else { // check min/max on schedule
                    SchMin =
                        ScheduleManager::GetScheduleMinValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDDaylightTargetPtr);
                    SchMax =
                        ScheduleManager::GetScheduleMaxValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDDaylightTargetPtr);
                    if (SchMin < 0.0 || SchMax < 0.0) {
                        if (SchMin < 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", {}, minimum is < 0.0",
                                                   RoutineName,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaFieldNames(8)));
                            ShowContinueError(
                                state,
                                format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(8), SchMin));
                            ErrorsFound = true;
                        }
                        if (SchMax < 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", {}, maximum is < 0.0",
                                                   RoutineName,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaFieldNames(8)));
                            ShowContinueError(
                                state,
                                format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(8), SchMin));
                            ErrorsFound = true;
                        }
                    }
                }
            }


            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LeafArea = state.dataIPShortCut->rNumericArgs(1);
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LeafArea < 0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(1),
                                       state.dataIPShortCut->rNumericArgs(1)));
                ErrorsFound = true;
            }
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD <0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(2),
                                       state.dataIPShortCut->rNumericArgs(2)));
                ErrorsFound = true;
            }
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP = state.dataIPShortCut->rNumericArgs(3);
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP< 0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(3),
                                       state.dataIPShortCut->rNumericArgs(3)));
                ErrorsFound = true;
            }
            state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDRadFraction= state.dataIPShortCut->rNumericArgs(4);
            if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDRadFraction < 0 ||
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDRadFraction >1.0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(4),
                                       state.dataIPShortCut->rNumericArgs(4)));
                ErrorsFound = true;
            }
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "IndoorLivingWall",
                                 state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName,
                                 "ETCaldatadriven",
                                 "[kg_m2s]",
                                 state.dataIndoorGreen->indoorgreen(IndoorGreenNum).EMSETCalOverrideOn,
                                 state.dataIndoorGreen->indoorgreen(IndoorGreenNum).EMSET);
            } // EMS
        }
            // Set up output variables
        for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
                SetupZoneInternalGain(state,
                                          state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr,
                                          state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName,
                                          DataHeatBalance::IntGainType::IndoorGreen,
                                          &state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate,
                                          nullptr,
                                          &state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LatentRate,
                                          nullptr,
                                          nullptr,
                                          nullptr);
                
                 SetupOutputVariable(state,
                                    "Indoor Living Wall Plant Surface Temperature",
                                    Constant::Units::C,
                                    state.dataHeatBalSurf->SurfTempIn(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall Sensible Heat Gain Rate",
                                    Constant::Units::W,
                                    //state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate,
                                    //state.dataHeatBalSurf->SurfQConvInRep(loop),// loop: surface number
                                    state.dataHeatBalSurf->SurfQConvInRep(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr), // positive sign: heat loss from plants; negative sign: heat gain from plants
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall Latent Heat Gain Rate",
                                    Constant::Units::W,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LatentRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall Evapotranspiration Rate",
                                    Constant::Units::kg_m2s, 
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall Energy Required For Evapotranspiration Per Unit Area",
                                    Constant::Units::W_m2, 
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LambdaET,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall LED Operational PPFD",
                                    Constant::Units::umol_m2s, 
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall PPFD",
                                    Constant::Units::umol_m2s,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall VPD",
                                    Constant::Units::Pa,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZVPD,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall LED Sensible Heat Gain Rate",
                                    Constant::Units::W,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall LED Operational Power",
                                    Constant::Units::W,  
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName);
                SetupOutputVariable(state,
                                    "Indoor Living Wall LED Electricity Energy",
                                    Constant::Units::J,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleCon,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName,
                                    Constant::eResource::Electricity,
                                    OutputProcessor::SOVEndUseCat::InteriorLights,
                                    "IndoorLivingWall",//End Use subcategory
                                    OutputProcessor::SOVGroup::Building,
                                    state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).Name,
                                    state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).Multiplier,
                                    state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).ListMultiplier,
                                    {},
                                    {},
                                    state.dataHeatBal->space(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SpacePtr).spaceType);
            }
    }

    void InitIndoorGreen(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Liping Wang
        //       DATE WRITTEN   Oct 2023

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Indoor Greenery System objects.

        int IndoorGreenNum; // Indoor Green index
           
            for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
            // Set the reporting variables to zero at each timestep.
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate = 0.0;
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LatentRate = 0.0;
            // Set indoor environment conditions at the every time step initializations 
                //LW to do ZoneList, check if this is the right parameter for temperature
                //state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPreTemp = 0.0;
                    //        state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).ZoneMeasuredTemperature;
                //state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPreHum = 0.0;
                    //        state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).ZoneMeasuredHumidityRatio;
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZCO2 = 400; 
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD =0;       
                //state.dataDaylightingManager->DaylIllum.allocate(state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
            }
  
    }
    void ETModel(EnergyPlusData &state)
    {
            // SUBROUTINE INFORMATION:
            //       AUTHOR         Liping Wang
            //       DATE WRITTEN   Oct 2023
            // Reference 
            //  Penman-Monteith model  
            //  https://www.nexsel.tech/convert-lux-to-ppdf.php
            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine is for the calculation of evapotranspiration effects from the Indoor Greenery System objects.
            // SUBROUTINE PARAMETER DEFINITIONS:
            static constexpr std::string_view RoutineName("ETModel: ");
            int IndoorGreenNum; // Indoor Green index
            Real64 ZonePreTemp;    // Indoor air temprature (C)
            Real64 ZonePreHum;     // Indoor humidity ratio (kg moisture / kg dry air)
            Real64 ZoneNewTemp;    // Indoor air temprature (C) after ET
            Real64 ZoneNewHum;     // Indoor humidity ratio (kg moisture / kg dry air) after ET
            Real64 ZoneSatHum;     // Saturated humidity ratio 
            Real64 ZoneCO2;        // Indoor zone co2 concentration (ppm)
            Real64 ZonePPFD;       // Indoor net radiation (PPFD)
            Real64 ZoneVPD;        // vapor pressure deficit (kpa); local variable
            //Real64 ETRate=0.0;         // mm/s; kg/(m2s)
            Real64 Timestep;       // s
            Real64 ETTotal;        // kg
            Real64 rhoair;         // kg/m3
            Real64 Tdp;            // dew point temperature
            Real64 Twb;            // wet bulb temperature
            Real64 HCons;           // enthalpy (J/kg)
            Real64 HMid;           // enthalpy 3rd point (J/kg)
            Real64 ZoneAirVol;     // zone air volume (m3)
            Real64 LAI;            // leaf area index, the ratio of one-side leaf area per unit plant growing area, maximum LAI =2 if LAI_cal>2.0    
            Real64 LAI_Cal;        // calculated leaf area index based on users's input on total leaf area
            Real64 OutPb;          // outdoor pressure (kPa)
            Real64 vp;             // actual vapor pressure of the air (kpa)
            Real64 vpSat;          // saturated vapor pressure at air temperature (kpa)
            std::string_view cCurrentModuleObject = "IndoorLivingWall";
            Timestep = state.dataHVACGlobal->TimeStepSysSec; // unit s
            // Method for ET calculation: Penman-Monteith=1, Stanghellini=2, Data-driven=3
            for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
                ZonePreTemp = state.dataZoneTempPredictorCorrector->zoneHeatBalance(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).ZT;
                ZonePreHum = state.dataZoneTempPredictorCorrector->zoneHeatBalance(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).airHumRat;
                ZoneCO2 = 400;  
                OutPb = state.dataEnvrn->OutBaroPress / 1000;                       
                Tdp = Psychrometrics::PsyTdpFnWPb(state, ZonePreHum, OutPb*1000); 
                vp = Psychrometrics::PsyPsatFnTemp(state, Tdp, RoutineName) / 1000; 
                vpSat = Psychrometrics::PsyPsatFnTemp(state, ZonePreTemp, RoutineName) / 1000; 
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZVPD = (vpSat - vp)*1000;  //Pa                                              
                LAI_Cal =state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LeafArea/state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).Area;
                LAI = LAI_Cal;
                if (LAI_Cal > 2.0) {
                LAI = 2.0; // maximum LAI=2.0 in the surface heat balance
                ShowSevereError(state,
                                format("Maximum indoor living wall leaf area index (LAI) =2.0 is used,calculated LAI is {}",
                                       LAI_Cal));
                }
                //ZonePPFD
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 1) {
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD =
                    ScheduleManager::GetCurrentScheduleValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDPtr) *
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD; // PPFD
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD =
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD;
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP =
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP;
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleCon =
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP*Timestep;
                }
                else if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 2) {  
                 
                   state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD = 0;
                   state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD =0;
                   state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP =0;
                   state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleCon =0;

                   if (!state.dataDayltg->CalcDayltghCoefficients_firstTime && state.dataEnvrn->SunIsUp) {
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD = 
                    //state.dataDayltg->daylightControl(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightControlPtr).DaylIllumAtRefPt(1) / 77; 
                    state.dataDayltg->DaylIllum(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr)/77; // To be updated currently only take one reference point; PPFD
                             //state.dataDaylightingManager->DaylIllum(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr) /77; // PPFD
                        Real64 diffSR = state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr) /
                                        state.dataHeatBalSurf->SurfAbsSolarInt(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr) * 4.6;
                        Real64 RadfromLights =
                            state.dataHeatBalSurf->SurfQdotRadLightsInPerArea(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr) /
                            state.dataConstruction
                                ->Construct(state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).Construction)
                                .InsideAbsorpSolar * 4.6;
                        Real64 comparePPFD = diffSR + RadfromLights ; 
                       // std::ofstream fout("indoorgreenvariables.txt", std::fstream::app);
                       // fout << comparePPFD << "," << diffSR<< "," << RadfromLights << "," << state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD
                       //      << std::endl;
                   }
                }
                else if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightingMethod == 3) {
                Real64 a = ScheduleManager::GetCurrentScheduleValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedLEDDaylightTargetPtr);
                Real64 b = 0;
                if (!state.dataDayltg->CalcDayltghCoefficients_firstTime && state.dataEnvrn->SunIsUp) {
                       //b= state.dataDaylightingManager->DaylIllum(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr)/77; // PPFD
                       //b = state.dataDayltg->daylightControl(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightControlPtr)
                       //         .DaylIllumAtRefPt(1) / 77; // To be updated currently only take one reference point; PPFD
                        b = state.dataDayltg->daylightControl(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightControlPtr)
                                .refPts(1).lums [DataSurfaces::iLum_Illum]/77;
                       // DaylIllum(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LightRefPtr) / 77;
                }
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD = max((a - b),0.0); 
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD >=
                    state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD) {
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD =
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD + b;// LED Nominal + Daylight
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP =
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP;
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleCon =
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP * Timestep;    
                }
                else
                {
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD = a;//Targeted PPFD
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP =
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalEleP *
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualPPFD /
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDNominalPPFD;
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleCon =
                           state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP * Timestep;   
                }                   
                }  
                ZonePPFD = state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZPPFD;
                ZoneVPD = state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZVPD/1000;//kPa            
                //ET Calculation
                if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod == 1) {
                        state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate =
                            ETPenmanMonteith(state, ZonePreTemp, ZonePreHum, ZoneCO2, ZonePPFD, ZoneVPD, LAI);
                } 
                else if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod == 2) {
                        state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate =
                            ETStanghellini(state, ZonePreTemp, ZonePreHum, ZoneCO2, ZonePPFD, ZoneVPD, LAI);
                } 
                else if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETCalculationMethod == 3) {
                // set with EMS value if being called for.
                   if (state.dataIndoorGreen->indoorgreen(IndoorGreenNum).EMSETCalOverrideOn) {
                       state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate = state.dataIndoorGreen->indoorgreen(IndoorGreenNum).EMSET;
                   }
                //ETRate=ETDatadriven(state);
                //   else {
                //   ShowSevereError(state,
                //                   format("EMS/Python Plugin for ET Data Driven Model not find in {}={}",
                //                          cCurrentModuleObject,
                //                          state.dataIndoorGreen->indoorgreen(IndoorGreenNum).IndoorGreenName));
                //   }
                } 


                //debug 
                //state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate = 0.0;
                Real64 effectivearea = std::min(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LeafArea,
                                                LAI*state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).Area); 
                ETTotal = state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ETRate * Timestep *effectivearea*
                           ScheduleManager::GetCurrentScheduleValue(state, state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SchedPtr); //kg; this unit area should be surface area instead of total leaf area
                Real64 hfg = Psychrometrics::PsyHfgAirFnWTdb(ZonePreHum, ZonePreTemp) / std::pow(10, 6); // Latent heat of vaporization (MJ/kg)
               state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LambdaET =
                    ETTotal * hfg * std::pow(10, 6) / state.dataSurface->Surface(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr).Area /
                    Timestep; // (W/m2))
                rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, ZonePreTemp, ZonePreHum);
                ZoneAirVol = state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).ZonePtr).Volume;
                ZoneNewHum =
                ZonePreHum + ETTotal / (rhoair * ZoneAirVol);
                //Tdp = Psychrometrics::PsyTdpFnWPb(state, ZonePreHum, state.dataEnvrn->OutBaroPress); 
                Twb = Psychrometrics::PsyTwbFnTdbWPb(state, ZonePreTemp, ZonePreHum, state.dataEnvrn->OutBaroPress);
                ZoneSatHum = Psychrometrics::PsyWFnTdpPb(state, ZonePreTemp, state.dataEnvrn->OutBaroPress); //saturated humidity ratio
                HCons = Psychrometrics::PsyHFnTdbW(ZonePreTemp, ZonePreHum);
                if (ZoneNewHum <= ZoneSatHum) {
                ZoneNewTemp = Psychrometrics::PsyTdbFnHW(HCons, ZoneNewHum);
                } else {
                ZoneNewTemp = Twb;
                ZoneNewHum = ZoneSatHum;
                }
                HMid = Psychrometrics::PsyHFnTdbW(ZoneNewTemp, ZoneNewHum);
                //LW adjustment for LAI
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate =
                    (1-state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDRadFraction) * state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LEDActualEleP; // convective heat gain from LED lights when LED is on; heat convection from plants was considered and counted from plant surface heat balance.
                //state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate = 0.95* ZoneAirVol * rhoair * (HMid - HCons) / Timestep; // unit W
                state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LatentRate = ZoneAirVol * rhoair * (HCons - HMid) / Timestep;   // unit W
                state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SurfPtr) =
                    -1.0*state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LambdaET;
                //   0.05 * ZoneAirVol * rhoair * (HMid - HCons) / Timestep; // unit W // negative sign indicates heat loss from inside surface          
                std::ofstream fout("indoorgreenvariables.txt", std::fstream::app);
                fout << Timestep << "," << state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LeafArea << ","
                     << state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LambdaET << ","
                     << state.dataIndoorGreen->indoorgreen(IndoorGreenNum).SensibleRate << ","
                     << state.dataIndoorGreen->indoorgreen(IndoorGreenNum).LatentRate << ","<< std::endl;

            }
    }


    Real64 ETPenmanMonteith(EnergyPlusData &state, Real64 &ZonePreTemp, Real64 &ZonePreHum, Real64 &ZoneCO2, Real64 &ZonePPFD, Real64 &ZoneVPD, Real64 &LAI)
    {
            // SUBROUTINE INFORMATION:
            //       AUTHOR         Liping Wang
            //       DATE WRITTEN   Oct 2023
            
            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine is for using Penman-Monteith ET model to calculate evapotranspiration rates from the Indoor Greenery System objects.
            
            // SUBROUTINE PARAMETER DEFINITIONS:
            static constexpr std::string_view RoutineName("ETPenmanMonteith: ");
            Real64 hfg = Psychrometrics::PsyHfgAirFnWTdb(ZonePreHum, ZonePreTemp)/std::pow(10,6); // Latent heat of vaporization (MJ/kg)
            Real64 slopepat = 0.200 * std::pow((0.00738 * ZonePreTemp + 0.8072), 7) - 0.000116; //Slope of the saturation vapor pressure-temperature curve (kPa/°C)
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(ZonePreHum) / std::pow(10, 6); // specific heat of air at constant pressure (MJ kg−1 °C−1)
            Real64 OutPb = state.dataEnvrn->OutBaroPress / 1000;    // outdoor pressure (kPa)
            Real64 constexpr mw(0.622);//ratio molecular weight of water vapor / dry air = 0.622.
            Real64 psyconst = CpAir * OutPb / (hfg * mw); // Psychrometric constant (kPa/°C)
            Real64 rs =0.0; //stomatal resistance s/m
            Real64 ra =0.0; //aerodynamic resistance s/m
            Real64 In = ZonePPFD * 0.327 / std::pow(10, 6); // net radiation MW/m2
            Real64 G = 0.0; //soil heat flux (MJ/(m2s))
            Real64 rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, OutPb * 1000, ZonePreTemp, ZonePreHum); //kg/m3
            // Real64 Tdp = Psychrometrics::PsyTdpFnWPb(state, ZonePreHum, OutPb * 1000); //dew point temperature
            // Real64 vp = Psychrometrics::PsyPsatFnTemp(state, Tdp, RoutineName)/1000; // actual vapor pressure of the air (kpa)
            // Real64 vpSat = Psychrometrics::PsyPsatFnTemp(state, ZonePreTemp, RoutineName)/1000;// saturation vapor pressure at air temperature (kpa)
            // Real64 vpd = vpSat - vp; // vapor pressure deficit (kpa)
            Real64 ETRate; //mm/s; kg/(m2s)
            rs =60*(1500+ZonePPFD)/(200+ZonePPFD);
            ra =350*std::pow((0.1/0.1),0.5)*(1/(LAI+1e-10));
            ETRate = (1 / hfg) * (slopepat * (In - G) + (rhoair * CpAir * ZoneVPD) / ra) / (slopepat + psyconst * (1 + rs / ra));//Penman-Monteith ET model
            std::ofstream fout("ETindoorgreenvariables.txt", std::fstream::app);
            fout << state.dataGlobal->DayOfSim << "," << state.dataGlobal->HourOfDay << "," << ETRate << "," << ZonePreTemp << "," << ZonePreHum << ","
                 << hfg
                 << "," << slopepat << "," << In << "," << rhoair << "," << CpAir << "," << ZoneVPD << "," << ra << "," << slopepat << "," << psyconst
                 << "," << rs << ","
                 << std::endl;
            return ETRate; //mm/s; kg/(m2s)
    }
    Real64
    ETStanghellini(EnergyPlusData &state, Real64 &ZonePreTemp, Real64 &ZonePreHum, Real64 &ZoneCO2, Real64 &ZonePPFD, Real64 &ZoneVPD, Real64 &LAI)
    {
            // SUBROUTINE INFORMATION:
            //       AUTHOR         Liping Wang
            //       DATE WRITTEN   Oct 2023

            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine is for using Stanghellini ET model to calculate evapotranspiration rates from the Indoor Greenery System objects.

            // SUBROUTINE PARAMETER DEFINITIONS:
            static constexpr std::string_view RoutineName("ETStanghellini: ");
            Real64 hfg = Psychrometrics::PsyHfgAirFnWTdb(ZonePreHum, ZonePreTemp) / std::pow(10, 6); // Latent heat of vaporization (MJ/kg)
            Real64 slopepat =
                0.200 * std::pow((0.00738 * ZonePreTemp + 0.8072), 7) - 0.000116; // Slope of the saturation vapor pressure-temperature curve (kPa/°C)
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(ZonePreHum) / std::pow(10, 6); // specific heat of air at constant pressure (MJ kg−1 °C−1)
            Real64 OutPb = state.dataEnvrn->OutBaroPress / 1000;                      // outdoor pressure (kPa)
            Real64 constexpr mw(0.622);                                                  // ratio molecular weight of water vapor / dry air = 0.622.
            Real64 psyconst = CpAir * OutPb / (hfg * mw);                             // Psychrometric constant (kPa/°C)
            Real64 rs = 0.0;                                                          // stomatal resistance s/m
            Real64 ra = 0.0;                                                          // aerodynamic resistance s/m
            Real64 In = ZonePPFD * 0.327 / std::pow(10, 6);                           // net radiation MW/m2
            Real64 G = 0.0;                                                           // soil heat flux (MJ/(m2s))
            Real64 rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, OutPb * 1000, ZonePreTemp, ZonePreHum); // kg/m3
            Real64 ETRate; // mm/s; kg/(m2s)
            rs = 60 * (1500 + ZonePPFD) / (200 + ZonePPFD);
            ra = 350 * std::pow((0.1 / 0.1), 0.5) * (1 / LAI);
            ETRate = (1 / hfg) * (slopepat * (In - G) + (2*LAI*rhoair * CpAir * ZoneVPD) / ra) /
                     (slopepat + psyconst * (1 + rs / ra)); // Penman-Monteith ET model
            // std::ofstream fout("indoorgreenvariables.txt", std::fstream::app);
            // fout << state.dataGlobal->DayOfSim << "," << state.dataGlobal->HourOfDay << "," << hfg << "," << slopepat << ","
            //      <<In << "," <<vpd << ","
            //      << vp << "," << vpSat << ",";
            return ETRate; // mm/s; kg/(m2s)
    }
  
} // namespace Indoor Green

} // namespace EnergyPlus
