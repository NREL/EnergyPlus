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
#include <EnergyPlus/DataPrecisionGlobals.hh>
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

    using namespace DataPrecisionGlobals;
    using DataGlobals::TimeStepZone;
    using DataGlobals::TimeStepZoneSec;

    void ManageExteriorEnergyUse(ExteriorEnergyUseData &exteriorEnergyUse)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine provides the usual call for the Simulation Manager.

        if (exteriorEnergyUse.GetExteriorEnergyInputFlag) {
            GetExteriorEnergyUseInput(exteriorEnergyUse);
            exteriorEnergyUse.GetExteriorEnergyInputFlag = false;
        }

        ReportExteriorEnergyUse(exteriorEnergyUse);
    }

    void GetExteriorEnergyUseInput(ExteriorEnergyUseData &exteriorEnergyUse)
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

        exteriorEnergyUse.NumExteriorLights = inputProcessor->getNumObjectsFound("Exterior:Lights");
        exteriorEnergyUse.ExteriorLights.allocate(exteriorEnergyUse.NumExteriorLights);

        NumFuelEq = inputProcessor->getNumObjectsFound("Exterior:FuelEquipment");
        NumWtrEq = inputProcessor->getNumObjectsFound("Exterior:WaterEquipment");
        exteriorEnergyUse.ExteriorEquipment.allocate(NumFuelEq + NumWtrEq);
        exteriorEnergyUse.UniqueExteriorEquipNames.reserve(NumFuelEq + NumWtrEq);

        exteriorEnergyUse.GetExteriorEnergyInputFlag = false;
        exteriorEnergyUse.NumExteriorEqs = 0;

        // =================================  Get Exterior Lights

        cCurrentModuleObject = "Exterior:Lights";
        for (Item = 1; Item <= exteriorEnergyUse.NumExteriorLights; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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

            exteriorEnergyUse.ExteriorLights(Item).Name = cAlphaArgs(1);
            exteriorEnergyUse.ExteriorLights(Item).SchedPtr = GetScheduleIndex(cAlphaArgs(2));
            if (exteriorEnergyUse.ExteriorLights(Item).SchedPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(exteriorEnergyUse.ExteriorLights(Item).SchedPtr);
                SchMax = GetScheduleMaxValue(exteriorEnergyUse.ExteriorLights(Item).SchedPtr);
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
                exteriorEnergyUse.ExteriorLights(Item).ControlMode = LightControlType::ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "ScheduleNameOnly")) {
                exteriorEnergyUse.ExteriorLights(Item).ControlMode = LightControlType::ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "AstronomicalClock")) {
                exteriorEnergyUse.ExteriorLights(Item).ControlMode = LightControlType::AstroClockOverride;
            } else {
                ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3) + " for " +
                                cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            exteriorEnergyUse.ExteriorLights(Item).DesignLevel = rNumericArgs(1);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("ExteriorLights",
                                 exteriorEnergyUse.ExteriorLights(Item).Name,
                                 "Electricity Power",
                                 "W",
                                 exteriorEnergyUse.ExteriorLights(Item).PowerActuatorOn,
                                 exteriorEnergyUse.ExteriorLights(Item).PowerActuatorValue);
            }

            SetupOutputVariable(
                "Exterior Lights Electricity Power", OutputProcessor::Unit::W, exteriorEnergyUse.ExteriorLights(Item).Power, "Zone", "Average", exteriorEnergyUse.ExteriorLights(Item).Name);

            SetupOutputVariable("Exterior Lights Electricity Energy",
                                OutputProcessor::Unit::J,
                                exteriorEnergyUse.ExteriorLights(Item).CurrentUse,
                                "Zone",
                                "Sum",
                                exteriorEnergyUse.ExteriorLights(Item).Name,
                                _,
                                "Electricity",
                                "Exterior Lights",
                                EndUseSubcategoryName);

            // entries for predefined tables
            PreDefTableEntry(pdchExLtPower, exteriorEnergyUse.ExteriorLights(Item).Name, exteriorEnergyUse.ExteriorLights(Item).DesignLevel);
            sumDesignLevel += exteriorEnergyUse.ExteriorLights(Item).DesignLevel;
            if (exteriorEnergyUse.ExteriorLights(Item).ControlMode == LightControlType::AstroClockOverride) { // photocell/schedule
                PreDefTableEntry(pdchExLtClock, exteriorEnergyUse.ExteriorLights(Item).Name, "AstronomicalClock");
                PreDefTableEntry(pdchExLtSchd, exteriorEnergyUse.ExteriorLights(Item).Name, "-");
            } else {
                PreDefTableEntry(pdchExLtClock, exteriorEnergyUse.ExteriorLights(Item).Name, "Schedule");
                PreDefTableEntry(pdchExLtSchd, exteriorEnergyUse.ExteriorLights(Item).Name, GetScheduleName(exteriorEnergyUse.ExteriorLights(Item).SchedPtr));
            }
        }
        PreDefTableEntry(pdchExLtPower, "Exterior Lighting Total", sumDesignLevel);

        // =================================  Get Exterior Fuel Equipment

        cCurrentModuleObject = "Exterior:FuelEquipment";
        for (Item = 1; Item <= NumFuelEq; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            GlobalNames::VerifyUniqueInterObjectName(exteriorEnergyUse.UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++exteriorEnergyUse.NumExteriorEqs;
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name = cAlphaArgs(1);

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            ValidateFuelType(exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).FuelType, cAlphaArgs(2), TypeString, cCurrentModuleObject, cAlphaFieldNames(2), cAlphaArgs(2));
            if (exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).FuelType == ExteriorFuelUsage::Unknown) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else {
                if (exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).FuelType != ExteriorFuelUsage::WaterUse) {
                    SetupOutputVariable("Exterior Equipment Fuel Rate",
                                        OutputProcessor::Unit::W,
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name);
                    SetupOutputVariable("Exterior Equipment " + TypeString + " Energy",
                                        OutputProcessor::Unit::J,
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                } else {
                    SetupOutputVariable("Exterior Equipment Water Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name);
                    SetupOutputVariable("Exterior Equipment " + TypeString + " Volume",
                                        OutputProcessor::Unit::m3,
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                }
            }
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr = GetScheduleIndex(cAlphaArgs(3));
            if (exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr);
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
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).DesignLevel = rNumericArgs(1);
        }

        // =================================  Get Exterior Water Equipment

        cCurrentModuleObject = "Exterior:WaterEquipment";
        for (Item = 1; Item <= NumWtrEq; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            GlobalNames::VerifyUniqueInterObjectName(exteriorEnergyUse.UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++exteriorEnergyUse.NumExteriorEqs;
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name = cAlphaArgs(1);
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).FuelType = ExteriorFuelUsage::WaterUse;
            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr = GetScheduleIndex(cAlphaArgs(3));
            if (exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).SchedPtr);
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

            exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).DesignLevel = rNumericArgs(1);

            SetupOutputVariable("Exterior Equipment Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Power,
                                "Zone",
                                "Average",
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name);

            SetupOutputVariable("Exterior Equipment Water Volume",
                                OutputProcessor::Unit::m3,
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name,
                                _,
                                "Water",
                                "ExteriorEquipment",
                                EndUseSubcategoryName);
            SetupOutputVariable("Exterior Equipment Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                exteriorEnergyUse.ExteriorEquipment(exteriorEnergyUse.NumExteriorEqs).Name,
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

        FuelTypeNumber = ExteriorFuelUsage::Unknown;
        FuelTypeString = "";

        // Select the correct Number for the associated ascii name for the fuel type
        if (UtilityRoutines::SameString(FuelTypeAlpha, "Electricity")) {
            FuelTypeNumber = ExteriorFuelUsage::ElecUse;
            FuelTypeString = "Electricity";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "NaturalGas")) {
            FuelTypeNumber = ExteriorFuelUsage::GasUse;
            FuelTypeString = "NaturalGas";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Coal")) {
            FuelTypeNumber = ExteriorFuelUsage::CoalUse;
            FuelTypeString = "Coal";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOilNo1")) {
            FuelTypeNumber = ExteriorFuelUsage::FuelOil1Use;
            FuelTypeString = "FuelOilNo1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Propane")) {
            FuelTypeNumber = ExteriorFuelUsage::PropaneUse;
            FuelTypeString = "Propane";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Gasoline")) {
            FuelTypeNumber = ExteriorFuelUsage::GasolineUse;
            FuelTypeString = "Gasoline";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Diesel")) {
            FuelTypeNumber = ExteriorFuelUsage::DieselUse;
            FuelTypeString = "Diesel";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOilNo2")) {
            FuelTypeNumber = ExteriorFuelUsage::FuelOil2Use;
            FuelTypeString = "FuelOilNo2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel1")) {
            FuelTypeNumber = ExteriorFuelUsage::OtherFuel1Use;
            FuelTypeString = "OtherFuel1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel2")) {
            FuelTypeNumber = ExteriorFuelUsage::OtherFuel1Use;
            FuelTypeString = "OtherFuel2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Water")) {
            FuelTypeNumber = ExteriorFuelUsage::WaterUse;
            FuelTypeString = "Water";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Steam")) {
            FuelTypeNumber = ExteriorFuelUsage::SteamUse;
            FuelTypeString = "Steam";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictCooling")) {
            FuelTypeNumber = ExteriorFuelUsage::DistrictCoolUse;
            FuelTypeString = "DistrictCooling";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictHeating")) {
            FuelTypeNumber = ExteriorFuelUsage::DistrictHeatUse;
            FuelTypeString = "DistrictHeating";
        } else {
            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + CurrentName + "\".");
            ShowFatalError("Heating source/fuel type not recognized. Check input field " + CurrentField + "=\"" + FuelTypeAlpha);
        }
    }

    void ReportExteriorEnergyUse(ExteriorEnergyUseData &exteriorEnergyUse)
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

        for (Item = 1; Item <= exteriorEnergyUse.NumExteriorLights; ++Item) {
            switch (exteriorEnergyUse.ExteriorLights(Item).ControlMode) {
                case LightControlType::ScheduleOnly:
                    exteriorEnergyUse.ExteriorLights(Item).Power = exteriorEnergyUse.ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(exteriorEnergyUse.ExteriorLights(Item).SchedPtr);
                    exteriorEnergyUse.ExteriorLights(Item).CurrentUse = exteriorEnergyUse.ExteriorLights(Item).Power * TimeStepZoneSec;
                    break;
                case LightControlType::AstroClockOverride:
                    if (SunIsUp) {
                        exteriorEnergyUse.ExteriorLights(Item).Power = 0.0;
                        exteriorEnergyUse.ExteriorLights(Item).CurrentUse = 0.0;
                    } else {
                        exteriorEnergyUse.ExteriorLights(Item).Power = exteriorEnergyUse.ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(exteriorEnergyUse.ExteriorLights(Item).SchedPtr);
                        exteriorEnergyUse.ExteriorLights(Item).CurrentUse = exteriorEnergyUse.ExteriorLights(Item).Power * TimeStepZoneSec;
                    }
                    break;
                default:
                    // should not happen
                    break;
            }

            // Reduce lighting power due to demand limiting
            if (exteriorEnergyUse.ExteriorLights(Item).ManageDemand && (exteriorEnergyUse.ExteriorLights(Item).Power > exteriorEnergyUse.ExteriorLights(Item).DemandLimit)) {
                exteriorEnergyUse.ExteriorLights(Item).Power = exteriorEnergyUse.ExteriorLights(Item).DemandLimit;
                exteriorEnergyUse.ExteriorLights(Item).CurrentUse = exteriorEnergyUse.ExteriorLights(Item).Power * TimeStepZoneSec;
            }
            // EMS controls
            if (exteriorEnergyUse.ExteriorLights(Item).PowerActuatorOn) exteriorEnergyUse.ExteriorLights(Item).Power = exteriorEnergyUse.ExteriorLights(Item).PowerActuatorValue;

            exteriorEnergyUse.ExteriorLights(Item).CurrentUse = exteriorEnergyUse.ExteriorLights(Item).Power * TimeStepZoneSec;

            // gather for tabular reports
            if (!WarmupFlag) {
                //      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
                if (DoOutputReporting && (KindOfSim == ksRunPeriodWeather)) { // for weather simulations only
                    // for tabular report, accumlate the total electricity used for each ExteriorLights object
                    exteriorEnergyUse.ExteriorLights(Item).SumConsumption += exteriorEnergyUse.ExteriorLights(Item).CurrentUse;
                    // for tabular report, accumulate the time when each ExteriorLights has consumption
                    //(using a very small threshold instead of zero)
                    if (exteriorEnergyUse.ExteriorLights(Item).CurrentUse > 0.01) {
                        exteriorEnergyUse.ExteriorLights(Item).SumTimeNotZeroCons += TimeStepZone;
                    }
                }
            }
        }

        for (Item = 1; Item <= exteriorEnergyUse.NumExteriorEqs; ++Item) {
            exteriorEnergyUse.ExteriorEquipment(Item).Power = exteriorEnergyUse.ExteriorEquipment(Item).DesignLevel * GetCurrentScheduleValue(exteriorEnergyUse.ExteriorEquipment(Item).SchedPtr);
            exteriorEnergyUse.ExteriorEquipment(Item).CurrentUse = exteriorEnergyUse.ExteriorEquipment(Item).Power * TimeStepZoneSec;
        }
    }

} // namespace ExteriorEnergyUse

} // namespace EnergyPlus
