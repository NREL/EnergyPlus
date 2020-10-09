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

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/General.hh>
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

    using DataGlobals::TimeStepZone;
    using DataGlobals::TimeStepZoneSec;

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
        using namespace DataIPShortCuts;
        using General::RoundSigDigits;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;
        using ScheduleManager::GetScheduleName;
        using namespace OutputReportPredefined;
        using DataGlobals::AnyEnergyManagementSystemInModel;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetExteriorEnergyUseInput: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;                       // Item to be "gotten"
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call
        int IOStatus;                   // Used in GetObjectItem
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int NumFuelEq;                  // Temporary -- number of ExteriorFuelEquipment statements
        int NumWtrEq;                   // Temporary -- number of ExteriorWaterEquipment statements
        std::string TypeString;         // Fuel Type string (returned from Validation)
        std::string EndUseSubcategoryName;
        Real64 SchMax;                     // Max value of schedule for item
        Real64 SchMin;                     // Min value of schedule for item
        static Real64 sumDesignLevel(0.0); // for predefined report of design level total

        state.dataExteriorEnergyUse->NumExteriorLights = inputProcessor->getNumObjectsFound("Exterior:Lights");
        state.dataExteriorEnergyUse->ExteriorLights.allocate(state.dataExteriorEnergyUse->NumExteriorLights);

        NumFuelEq = inputProcessor->getNumObjectsFound("Exterior:FuelEquipment");
        NumWtrEq = inputProcessor->getNumObjectsFound("Exterior:WaterEquipment");
        state.dataExteriorEnergyUse->ExteriorEquipment.allocate(NumFuelEq + NumWtrEq);
        state.dataExteriorEnergyUse->UniqueExteriorEquipNames.reserve(NumFuelEq + NumWtrEq);

        state.dataExteriorEnergyUse->GetExteriorEnergyInputFlag = false;
        state.dataExteriorEnergyUse->NumExteriorEqs = 0;

        // =================================  Get Exterior Lights

        cCurrentModuleObject = "Exterior:Lights";
        for (Item = 1; Item <= state.dataExteriorEnergyUse->NumExteriorLights; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) continue;

            state.dataExteriorEnergyUse->ExteriorLights(Item).Name = cAlphaArgs(1);
            state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                SchMax = GetScheduleMaxValue(state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " minimum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(2) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " maximum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(2) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }
            if (lAlphaFieldBlanks(3)) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "ScheduleNameOnly")) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "AstronomicalClock")) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode = ExteriorEnergyUse::LightControlType::AstroClockOverride;
            } else {
                ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3) + " for " +
                                cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel = rNumericArgs(1);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("ExteriorLights",
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                                 "Electricity Rate",
                                 "W",
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorOn,
                                 state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorValue);
            }

            SetupOutputVariable(state,
                "Exterior Lights Electricity Rate", OutputProcessor::Unit::W, state.dataExteriorEnergyUse->ExteriorLights(Item).Power, "Zone", "Average", state.dataExteriorEnergyUse->ExteriorLights(Item).Name);

            SetupOutputVariable(state, "Exterior Lights Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse,
                                "Zone",
                                "Sum",
                                state.dataExteriorEnergyUse->ExteriorLights(Item).Name,
                                _,
                                "Electricity",
                                "Exterior Lights",
                                EndUseSubcategoryName);

            // entries for predefined tables
            PreDefTableEntry(pdchExLtPower, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel);
            sumDesignLevel += state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel;
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).ControlMode == ExteriorEnergyUse::LightControlType::AstroClockOverride) { // photocell/schedule
                PreDefTableEntry(pdchExLtClock, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "AstronomicalClock");
                PreDefTableEntry(pdchExLtSchd, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "-");
            } else {
                PreDefTableEntry(pdchExLtClock, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, "Schedule");
                PreDefTableEntry(pdchExLtSchd, state.dataExteriorEnergyUse->ExteriorLights(Item).Name, GetScheduleName(state, state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr));
            }
        }
        PreDefTableEntry(pdchExLtPower, "Exterior Lighting Total", sumDesignLevel);

        // =================================  Get Exterior Fuel Equipment

        cCurrentModuleObject = "Exterior:FuelEquipment";
        for (Item = 1; Item <= NumFuelEq; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) continue;
            GlobalNames::VerifyUniqueInterObjectName(state.dataExteriorEnergyUse->UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++state.dataExteriorEnergyUse->NumExteriorEqs;
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name = cAlphaArgs(1);

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            ExteriorEnergyUse::ValidateFuelType(state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).FuelType, cAlphaArgs(2), TypeString, cCurrentModuleObject, cAlphaFieldNames(2), cAlphaArgs(2));
            if (state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).FuelType == ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else {
                if (state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).FuelType != ExteriorEnergyUse::ExteriorFuelUsage::WaterUse) {
                    SetupOutputVariable(state, "Exterior Equipment Fuel Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name);
                    SetupOutputVariable(state, "Exterior Equipment " + TypeString + " Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                } else {
                    SetupOutputVariable(state, "Exterior Equipment Water Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name);
                    SetupOutputVariable(state, "Exterior Equipment " + TypeString + " Volume",
                                        OutputProcessor::Unit::m3,
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                }
            }
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " minimum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " maximum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).DesignLevel = rNumericArgs(1);
        }

        // =================================  Get Exterior Water Equipment

        cCurrentModuleObject = "Exterior:WaterEquipment";
        for (Item = 1; Item <= NumWtrEq; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) continue;
            GlobalNames::VerifyUniqueInterObjectName(state.dataExteriorEnergyUse->UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++state.dataExteriorEnergyUse->NumExteriorEqs;
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name = cAlphaArgs(1);
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).FuelType = ExteriorEnergyUse::ExteriorFuelUsage::WaterUse;
            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " minimum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(3) + "\". Minimum is [" + RoundSigDigits(SchMin, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " maximum, is < 0.0 for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(cAlphaArgs(3) + "\". Maximum is [" + RoundSigDigits(SchMax, 1) + "]. Values must be >= 0.0.");
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).DesignLevel = rNumericArgs(1);

            SetupOutputVariable(state, "Exterior Equipment Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Power,
                                "Zone",
                                "Average",
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name);

            SetupOutputVariable(state, "Exterior Equipment Water Volume",
                                OutputProcessor::Unit::m3,
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name,
                                _,
                                "Water",
                                "ExteriorEquipment",
                                EndUseSubcategoryName);
            SetupOutputVariable(state, "Exterior Equipment Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                state.dataExteriorEnergyUse->ExteriorEquipment(state.dataExteriorEnergyUse->NumExteriorEqs).Name,
                                _,
                                "MainsWater",
                                "ExteriorEquipment",
                                EndUseSubcategoryName);
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input.  Program terminates.");
        }
    }

    void ValidateFuelType(ExteriorEnergyUse::ExteriorFuelUsage &FuelTypeNumber,                    // Fuel Type to be set in structure.
                          std::string const &FuelTypeAlpha,       // Fuel Type String
                          std::string &FuelTypeString,            // Standardized Fuel Type String (for variable naming)
                          std::string const &CurrentModuleObject, // object being parsed
                          std::string const &CurrentField,        // current field being parsed
                          std::string const &CurrentName          // current object name being parsed
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine compares the input Fuel Type value against the
        // valid values and sets the correct in the returned FuelTypeNumber.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ValidateFuelType: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::Unknown;
        FuelTypeString = "";

        // Select the correct Number for the associated ascii name for the fuel type
        if (UtilityRoutines::SameString(FuelTypeAlpha, "Electricity")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::ElecUse;
            FuelTypeString = "Electricity";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "NaturalGas")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::GasUse;
            FuelTypeString = "NaturalGas";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Coal")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::CoalUse;
            FuelTypeString = "Coal";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOilNo1")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::FuelOil1Use;
            FuelTypeString = "FuelOilNo1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Propane")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::PropaneUse;
            FuelTypeString = "Propane";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Gasoline")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::GasolineUse;
            FuelTypeString = "Gasoline";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Diesel")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::DieselUse;
            FuelTypeString = "Diesel";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOilNo2")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::FuelOil2Use;
            FuelTypeString = "FuelOilNo2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel1")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::OtherFuel1Use;
            FuelTypeString = "OtherFuel1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel2")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::OtherFuel1Use;
            FuelTypeString = "OtherFuel2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Water")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::WaterUse;
            FuelTypeString = "Water";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Steam")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::SteamUse;
            FuelTypeString = "Steam";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictCooling")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::DistrictCoolUse;
            FuelTypeString = "DistrictCooling";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictHeating")) {
            FuelTypeNumber = ExteriorEnergyUse::ExteriorFuelUsage::DistrictHeatUse;
            FuelTypeString = "DistrictHeating";
        } else {
            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + CurrentName + "\".");
            ShowFatalError("Heating source/fuel type not recognized. Check input field " + CurrentField + "=\"" + FuelTypeAlpha);
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
        using DataEnvironment::SunIsUp;
        using DataGlobals::DoOutputReporting;
        using DataGlobals::KindOfSim;
        using DataGlobals::ksRunPeriodWeather;
        using DataGlobals::WarmupFlag;
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
                    state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                    state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = state.dataExteriorEnergyUse->ExteriorLights(Item).Power * TimeStepZoneSec;
                    break;
                case ExteriorEnergyUse::LightControlType::AstroClockOverride:
                    if (SunIsUp) {
                        state.dataExteriorEnergyUse->ExteriorLights(Item).Power = 0.0;
                        state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = 0.0;
                    } else {
                        state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(state.dataExteriorEnergyUse->ExteriorLights(Item).SchedPtr);
                        state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = state.dataExteriorEnergyUse->ExteriorLights(Item).Power * TimeStepZoneSec;
                    }
                    break;
                default:
                    // should not happen
                    break;
            }

            // Reduce lighting power due to demand limiting
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).ManageDemand && (state.dataExteriorEnergyUse->ExteriorLights(Item).Power > state.dataExteriorEnergyUse->ExteriorLights(Item).DemandLimit)) {
                state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).DemandLimit;
                state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = state.dataExteriorEnergyUse->ExteriorLights(Item).Power * TimeStepZoneSec;
            }
            // EMS controls
            if (state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorOn) state.dataExteriorEnergyUse->ExteriorLights(Item).Power = state.dataExteriorEnergyUse->ExteriorLights(Item).PowerActuatorValue;

            state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse = state.dataExteriorEnergyUse->ExteriorLights(Item).Power * TimeStepZoneSec;

            // gather for tabular reports
            if (!WarmupFlag) {
                //      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
                if (DoOutputReporting && (KindOfSim == ksRunPeriodWeather)) { // for weather simulations only
                    // for tabular report, accumua the total electricity used for each ExteriorLights object
                    state.dataExteriorEnergyUse->ExteriorLights(Item).SumConsumption += state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse;
                    // for tabular report, accumulate the time when each ExteriorLights has consumption
                    //(using a very small threshold instead of zero)
                    if (state.dataExteriorEnergyUse->ExteriorLights(Item).CurrentUse > 0.01) {
                        state.dataExteriorEnergyUse->ExteriorLights(Item).SumTimeNotZeroCons += TimeStepZone;
                    }
                }
            }
        }

        for (Item = 1; Item <= state.dataExteriorEnergyUse->NumExteriorEqs; ++Item) {
            state.dataExteriorEnergyUse->ExteriorEquipment(Item).Power = state.dataExteriorEnergyUse->ExteriorEquipment(Item).DesignLevel * GetCurrentScheduleValue(state.dataExteriorEnergyUse->ExteriorEquipment(Item).SchedPtr);
            state.dataExteriorEnergyUse->ExteriorEquipment(Item).CurrentUse = state.dataExteriorEnergyUse->ExteriorEquipment(Item).Power * TimeStepZoneSec;
        }
    }

} // namespace ExteriorEnergyUse

} // namespace EnergyPlus
