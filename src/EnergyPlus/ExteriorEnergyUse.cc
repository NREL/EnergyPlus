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

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ExteriorEnergyUse {

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   January 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module provides the reporting for exterior energy usage.  This usage does not directly
    // affect simulation results for the energy usage in a building but may affect the "metered"
    // usage of a facility.

    void ManageExteriorEnergyUse(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine provides the usual call for the Simulation Manager.

        if (state.dataExteriorEnergyUse->GetExteriorEnergyInputFlag) {
            ExteriorEnergyUse::GetExteriorEnergyUseInput(state);
            state.dataExteriorEnergyUse->GetExteriorEnergyInputFlag = false;
        }

        ExteriorEnergyUse::ReportExteriorEnergyUse(state);
    }

    void GetExteriorEnergyUseInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the Exterior Lights and Equipment.

        // Using/Aliasing

        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;
        using ScheduleManager::GetScheduleName;
        using namespace OutputReportPredefined;
        // SUBROUTINE PARAMETER DEFINITIONS:
        std::string_view constexpr routineName = "GetExteriorEnergyUseInput";

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        std::string EndUseSubcategoryName;
        Real64 SchMax; // Max value of schedule for item
        Real64 SchMin; // Min value of schedule for item

        auto &ipsc = state.dataIPShortCut;

        state.dataExteriorEnergyUse->NumExteriorLights = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Exterior:Lights");
        state.dataExteriorEnergyUse->ExteriorLights.allocate(state.dataExteriorEnergyUse->NumExteriorLights);

        int NumFuelEq = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Exterior:FuelEquipment");
        int NumWtrEq = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Exterior:WaterEquipment");
        state.dataExteriorEnergyUse->ExteriorEquipment.allocate(NumFuelEq + NumWtrEq);
        state.dataExteriorEnergyUse->UniqueExteriorEquipNames.reserve(NumFuelEq + NumWtrEq);

        state.dataExteriorEnergyUse->GetExteriorEnergyInputFlag = false;
        state.dataExteriorEnergyUse->NumExteriorEqs = 0;

        // =================================  Get Exterior Lights
        std::string_view cCurrentModuleObject = "Exterior:Lights";
        for (int Item = 1; Item <= state.dataExteriorEnergyUse->NumExteriorLights; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            state.dataExteriorEnergyUse->ExteriorLights(Item).Name = ipsc->cAlphaArgs(1);
            state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(2));
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr == 0) {
                if (ipsc->lAlphaFieldBlanks(2)) {
                    ShowSevereError(state,
                                    format("{}: {}: {} is required, missing for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(2),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                } else {
                    ShowSevereError(state,
                                    format("{}: {}: invalid {} entered={} for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(2),
                                           ipsc->cAlphaArgs(2),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                SchMax = GetScheduleMaxValue(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} minimum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(2),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(2), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} maximum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(2),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(2), SchMax));
                        ErrorsFound = true;
                    }
                }
            }
            if (ipsc->lAlphaFieldBlanks(3)) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::ScheduleOnly;
            } else if (Util::SameString(state.dataIPShortCut->cAlphaArgs(3), "ScheduleNameOnly")) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::ScheduleOnly;
            } else if (Util::SameString(state.dataIPShortCut->cAlphaArgs(3), "AstronomicalClock")) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::AstroClockOverride;
            } else {
                ShowSevereError(state,
                                format("{}: {}: invalid {} entered={} for {}={}",
                                       routineName,
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaFieldNames(3),
                                       state.dataIPShortCut->cAlphaArgs(3),
                                       state.dataIPShortCut->cAlphaFieldNames(1),
                                       state.dataIPShortCut->cAlphaArgs(1)));
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel = state.dataIPShortCut->rNumericArgs(1);
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "ExteriorLights",
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                                 "Electricity Rate",
                                 "W",
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorOn,
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorValue);
            }

            SetupOutputVariable(state,
                                "Exterior Lights Electricity Rate",
                                Constant::Units::W,
                                state.dataExteriorEnergyUse->ExteriorLights(Item).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataExteriorEnergyUse->ExteriorLights(Item).Name);

            SetupOutputVariable(state,
                                "Exterior Lights Electricity Energy",
                                Constant::Units::J,
                                state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                                Constant::eResource::Electricity,
                                OutputProcessor::SOVEndUseCat::ExteriorLights,
                                EndUseSubcategoryName);

            // entries for predefined tables
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchExLtPower,
                             state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                             state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel);
            state.dataExteriorEnergyUse->sumDesignLevel += state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel;
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode ==
                ExteriorEnergyUse::LightControlType::AstroClockOverride) { // photocell/schedule
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchExLtClock, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "AstronomicalClock");
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchExLtSchd, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "-");
            } else {
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchExLtClock, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "Schedule");
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchExLtSchd,
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                                 GetScheduleName(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr));
            }
        }
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchExLtPower, "Exterior Lighting Total", state.dataExteriorEnergyUse->sumDesignLevel);

        // =================================  Get Exterior Fuel Equipment

        cCurrentModuleObject = "Exterior:FuelEquipment";
        for (int Item = 1; Item <= NumFuelEq; ++Item) {
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
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataExteriorEnergyUse->UniqueExteriorEquipNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            ++state.dataExteriorEnergyUse->NumExteriorEqs;

            auto &exteriorEquip = state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs);
            exteriorEquip.Name = state.dataIPShortCut->cAlphaArgs(1);

            if (NumAlphas > 3) {
                EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(2));
                ErrorsFound = true;

            } else if ((exteriorEquip.FuelType = static_cast<Constant::eFuel>(
                            getEnumValue(Constant::eFuelNamesUC, Util::makeUPPER(ipsc->cAlphaArgs(2))))) == Constant::eFuel::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;

            } else if (exteriorEquip.FuelType != Constant::eFuel::Water) {
                SetupOutputVariable(state,
                                    "Exterior Equipment Fuel Rate",
                                    Constant::Units::W,
                                    exteriorEquip.Power,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    exteriorEquip.Name);
                SetupOutputVariable(state,
                                    format("Exterior Equipment {} Energy", Constant::eFuelNames[(int)exteriorEquip.FuelType]),
                                    Constant::Units::J,
                                    exteriorEquip.CurrentUse,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    exteriorEquip.Name,
                                    Constant::eFuel2eResource[(int)exteriorEquip.FuelType],
                                    OutputProcessor::SOVEndUseCat::ExteriorEquipment,
                                    EndUseSubcategoryName);
            } else {
                SetupOutputVariable(state,
                                    "Exterior Equipment Water Volume Flow Rate",
                                    Constant::Units::m3_s,
                                    exteriorEquip.Power,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    exteriorEquip.Name);
                SetupOutputVariable(state,
                                    format("Exterior Equipment {} Volume", Constant::eFuelNames[(int)exteriorEquip.FuelType]),
                                    Constant::Units::m3,
                                    exteriorEquip.CurrentUse,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    exteriorEquip.Name,
                                    Constant::eFuel2eResource[(int)exteriorEquip.FuelType],
                                    OutputProcessor::SOVEndUseCat::ExteriorEquipment,
                                    EndUseSubcategoryName);
            }

            exteriorEquip.SchedPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
            if (exteriorEquip.SchedPtr == 0) {
                if (ipsc->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    format("{}: {}: {} is required, missing for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                } else {
                    ShowSevereError(state,
                                    format("{}: {}: invalid {} entered={} for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaArgs(3),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, exteriorEquip.SchedPtr);
                SchMax = GetScheduleMaxValue(state, exteriorEquip.SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} minimum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(3),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} maximum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(3),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(3), SchMax));
                        ErrorsFound = true;
                    }
                }
            }
            exteriorEquip.DesignLevel = ipsc->rNumericArgs(1);
        }

        // =================================  Get Exterior Water Equipment

        cCurrentModuleObject = "Exterior:WaterEquipment";
        for (int Item = 1; Item <= NumWtrEq; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataExteriorEnergyUse->UniqueExteriorEquipNames,
                                                     ipsc->cAlphaArgs(1),
                                                     cCurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound);

            ++state.dataExteriorEnergyUse->NumExteriorEqs;

            auto &exteriorEquip = state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs);
            exteriorEquip.Name = ipsc->cAlphaArgs(1);
            exteriorEquip.FuelType = Constant::eFuel::Water;
            exteriorEquip.SchedPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
            if (exteriorEquip.SchedPtr == 0) {
                if (ipsc->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    format("{}: {}: {} is required, missing for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                } else {
                    ShowSevereError(state,
                                    format("{}: {}: invalid {} entered={} for {}={}",
                                           routineName,
                                           cCurrentModuleObject,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaArgs(3),
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, exteriorEquip.SchedPtr);
                SchMax = GetScheduleMaxValue(state, exteriorEquip.SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} minimum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(3),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        format("{}: {}: invalid {} maximum, is < 0.0 for {}={}",
                                               routineName,
                                               cCurrentModuleObject,
                                               ipsc->cAlphaFieldNames(3),
                                               ipsc->cAlphaFieldNames(1),
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", ipsc->cAlphaArgs(3), SchMax));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = ipsc->cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            exteriorEquip.DesignLevel = ipsc->rNumericArgs(1);

            SetupOutputVariable(state,
                                "Exterior Equipment Water Volume Flow Rate",
                                Constant::Units::m3_s,
                                exteriorEquip.Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                exteriorEquip.Name);

            SetupOutputVariable(state,
                                "Exterior Equipment Water Volume",
                                Constant::Units::m3,
                                exteriorEquip.CurrentUse,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                exteriorEquip.Name,
                                Constant::eResource::Water,
                                OutputProcessor::SOVEndUseCat::ExteriorEquipment,
                                EndUseSubcategoryName);
            SetupOutputVariable(state,
                                "Exterior Equipment Mains Water Volume",
                                Constant::Units::m3,
                                exteriorEquip.CurrentUse,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                exteriorEquip.Name,
                                Constant::eResource::MainsWater,
                                OutputProcessor::SOVEndUseCat::ExteriorEquipment,
                                EndUseSubcategoryName);
        }

        if (ErrorsFound) {
            ShowFatalError(state, format("{}Errors found in input.  Program terminates.", routineName));
        }
    }

    void ReportExteriorEnergyUse(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs the calculations necessary to report
        // the exterior energy use types.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
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
        int Item; // Loop Control

        for (Item = 1; Item <= state.dataExteriorEnergyUse->NumExteriorLights; ++Item) {
            switch (state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode) {
            case ExteriorEnergyUse::LightControlType::ScheduleOnly:
                state.dataExteriorEnergyUse->ExteriorLights(Item).Power =
                    state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel *
                    GetCurrentScheduleValue(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse =
                    state.dataExteriorEnergyUse->ExteriorLights(Item).Power * state.dataGlobal->TimeStepZoneSec;
                break;
            case ExteriorEnergyUse::LightControlType::AstroClockOverride:
                if (state.dataEnvrn->SunIsUp) {
                    state.dataExteriorEnergyUse->ExteriorLights(Item).Power = 0.0;
                    state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = 0.0;
                } else {
                    state.dataExteriorEnergyUse->ExteriorLights(Item).Power =
                        state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel *
                        GetCurrentScheduleValue(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                    state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse =
                        state.dataExteriorEnergyUse->ExteriorLights(Item).Power * state.dataGlobal->TimeStepZoneSec;
                }
                break;
            default:
                // should not happen
                break;
            }

            // Reduce lighting power due to demand limiting
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).ManageDemand &&
                (state.dataExteriorEnergyUse->ExteriorLights(Item).Power > state.dataExteriorEnergyUse->ExteriorLights(Item).DemandLimit)) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).DemandLimit;
                state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse =
                    state.dataExteriorEnergyUse->ExteriorLights(Item).Power * state.dataGlobal->TimeStepZoneSec;
            }
            // EMS controls
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorOn)
                state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorValue;

            state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse =
                state.dataExteriorEnergyUse->ExteriorLights(Item).Power * state.dataGlobal->TimeStepZoneSec;

            // gather for tabular reports
            if (!state.dataGlobal->WarmupFlag) {
                //      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
                if (state.dataGlobal->DoOutputReporting &&
                    (state.dataGlobal->KindOfSim == Constant::KindOfSim::RunPeriodWeather)) { // for weather simulations only
                    // for tabular report, accumua the total electricity used for each ExteriorLights object
                    state.dataExteriorEnergyUse->ExteriorLights(Item).SumConsumption += state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse;
                    // for tabular report, accumulate the time when each ExteriorLights has consumption
                    //(using a very small threshold instead of zero)
                    if (state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse > 0.01) {
                        state.dataExteriorEnergyUse->ExteriorLights(Item).SumTimeNotZeroCons += state.dataGlobal->TimeStepZone;
                    }
                }
            }
        }

        for (Item = 1; Item <= state.dataExteriorEnergyUse->NumExteriorEqs; ++Item) {
            state.dataExteriorEnergyUse->ExteriorEquipment(Item).Power =
                state.dataExteriorEnergyUse->ExteriorEquipment(Item).DesignLevel *
                GetCurrentScheduleValue(state, state.dataExteriorEnergyUse->ExteriorEquipment(Item).SchedPtr);
            state.dataExteriorEnergyUse->ExteriorEquipment(Item).CurrentUse =
                state.dataExteriorEnergyUse->ExteriorEquipment(Item).Power * state.dataGlobal->TimeStepZoneSec;
        }
    }

} // namespace ExteriorEnergyUse

} // namespace EnergyPlus
