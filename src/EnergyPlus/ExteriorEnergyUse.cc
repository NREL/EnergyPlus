// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <EMSManager.hh>
#include <ExteriorEnergyUse.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ExteriorEnergyUse {

    // Module containing the routines dealing with the reporting of Exterior Energy Usage Elements

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   January 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module provides the reporting for exterior energy usage.  This usage does not directly
    // affect simulation results for the energy usage in a building but may affect the "metered"
    // usage of a facility.

    // METHODOLOGY EMPLOYED:
    // No simulation, this is just reporting consumption.

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::TimeStepZone;
    using DataGlobals::TimeStepZoneSec;

    // Use statements for access to subroutines in other modules

    // Data
    // MODULE PARAMETER DEFINITIONS:
    int const ElecUse(1);                  // Electricity
    int const GasUse(2);                   // Gas (Natural)
    int const WaterUse(3);                 // Water
    int const CoalUse(4);                  // Coal
    int const FuelOil1Use(5);              // FuelOil#1
    int const FuelOil2Use(6);              // FuelOil#2
    int const LPGUse(7);                   // PropaneGas
    int const GasolineUse(8);              // Gasoline
    int const DieselUse(9);                // Diesel
    int const SteamUse(10);                // Steam
    int const DistrictCoolUse(11);         // Purchased Cooling
    int const DistrictHeatUse(12);         // Purchased Heating
    int const OtherFuel1Use(13);           // OtherFuel1
    int const OtherFuel2Use(14);           // OtherFuel2
    bool GetExteriorEnergyInputFlag(true); // First time, input is "gotten"

    int const ScheduleOnly(1);       // exterior lights only on schedule
    int const AstroClockOverride(2); // exterior lights controlled to turn off during day.

    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    int NumExteriorLights; // Number of Exterior Light Inputs
    int NumExteriorEqs;    // Number of Exterior Equipment Inputs

    // SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

    // Name Public routines, optionally name Private routines within this module

    // Object Data
    Array1D<ExteriorLightUsage> ExteriorLights;        // Structure for Exterior Light reporting
    Array1D<ExteriorEquipmentUsage> ExteriorEquipment; // Structure for Exterior Equipment Reporting
    std::unordered_map<std::string, std::string> UniqueExteriorEquipNames;

    // Functions

    // Clears the global data in ExteriorEnergyUse.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        NumExteriorLights = 0;
        NumExteriorEqs = 0;
        ExteriorLights.deallocate();
        ExteriorEquipment.deallocate();
        UniqueExteriorEquipNames.clear();
        GetExteriorEnergyInputFlag = true;
    }

    void ManageExteriorEnergyUse()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine provides the usual call for the Simulation Manager.

        if (GetExteriorEnergyInputFlag) {
            GetExteriorEnergyUseInput();
            GetExteriorEnergyInputFlag = false;
        }

        ReportExteriorEnergyUse();
    }

    void GetExteriorEnergyUseInput()
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

        NumExteriorLights = inputProcessor->getNumObjectsFound("Exterior:Lights");
        ExteriorLights.allocate(NumExteriorLights);

        NumFuelEq = inputProcessor->getNumObjectsFound("Exterior:FuelEquipment");
        NumWtrEq = inputProcessor->getNumObjectsFound("Exterior:WaterEquipment");
        ExteriorEquipment.allocate(NumFuelEq + NumWtrEq);
        UniqueExteriorEquipNames.reserve(NumFuelEq + NumWtrEq);

        GetExteriorEnergyInputFlag = false;
        NumExteriorEqs = 0;

        // =================================  Get Exterior Lights

        cCurrentModuleObject = "Exterior:Lights";
        for (Item = 1; Item <= NumExteriorLights; ++Item) {
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

            ExteriorLights(Item).Name = cAlphaArgs(1);
            ExteriorLights(Item).SchedPtr = GetScheduleIndex(cAlphaArgs(2));
            if (ExteriorLights(Item).SchedPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ExteriorLights(Item).SchedPtr);
                SchMax = GetScheduleMaxValue(ExteriorLights(Item).SchedPtr);
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
                ExteriorLights(Item).ControlMode = ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "ScheduleNameOnly")) {
                ExteriorLights(Item).ControlMode = ScheduleOnly;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "AstronomicalClock")) {
                ExteriorLights(Item).ControlMode = AstroClockOverride;
            } else {
                ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3) + " for " +
                                cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
            }

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            ExteriorLights(Item).DesignLevel = rNumericArgs(1);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("ExteriorLights",
                                 ExteriorLights(Item).Name,
                                 "Electric Power",
                                 "W",
                                 ExteriorLights(Item).PowerActuatorOn,
                                 ExteriorLights(Item).PowerActuatorValue);
            }

            SetupOutputVariable(
                "Exterior Lights Electric Power", OutputProcessor::Unit::W, ExteriorLights(Item).Power, "Zone", "Average", ExteriorLights(Item).Name);

            SetupOutputVariable("Exterior Lights Electric Energy",
                                OutputProcessor::Unit::J,
                                ExteriorLights(Item).CurrentUse,
                                "Zone",
                                "Sum",
                                ExteriorLights(Item).Name,
                                _,
                                "Electricity",
                                "Exterior Lights",
                                EndUseSubcategoryName);

            // entries for predefined tables
            PreDefTableEntry(pdchExLtPower, ExteriorLights(Item).Name, ExteriorLights(Item).DesignLevel);
            sumDesignLevel += ExteriorLights(Item).DesignLevel;
            if (ExteriorLights(Item).ControlMode == AstroClockOverride) { // photocell/schedule
                PreDefTableEntry(pdchExLtClock, ExteriorLights(Item).Name, "AstronomicalClock");
                PreDefTableEntry(pdchExLtSchd, ExteriorLights(Item).Name, "-");
            } else {
                PreDefTableEntry(pdchExLtClock, ExteriorLights(Item).Name, "Schedule");
                PreDefTableEntry(pdchExLtSchd, ExteriorLights(Item).Name, GetScheduleName(ExteriorLights(Item).SchedPtr));
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
            GlobalNames::VerifyUniqueInterObjectName(UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++NumExteriorEqs;
            ExteriorEquipment(NumExteriorEqs).Name = cAlphaArgs(1);

            if (NumAlphas > 3) {
                EndUseSubcategoryName = cAlphaArgs(4);
            } else {
                EndUseSubcategoryName = "General";
            }

            ValidateFuelType(
                ExteriorEquipment(NumExteriorEqs).FuelType, cAlphaArgs(2), TypeString, cCurrentModuleObject, cAlphaFieldNames(2), cAlphaArgs(2));
            if (ExteriorEquipment(NumExteriorEqs).FuelType == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else {
                if (ExteriorEquipment(NumExteriorEqs).FuelType != WaterUse) {
                    SetupOutputVariable("Exterior Equipment Fuel Rate",
                                        OutputProcessor::Unit::W,
                                        ExteriorEquipment(NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        ExteriorEquipment(NumExteriorEqs).Name);
                    SetupOutputVariable("Exterior Equipment " + TypeString + " Energy",
                                        OutputProcessor::Unit::J,
                                        ExteriorEquipment(NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        ExteriorEquipment(NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                } else {
                    SetupOutputVariable("Exterior Equipment Water Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        ExteriorEquipment(NumExteriorEqs).Power,
                                        "Zone",
                                        "Average",
                                        ExteriorEquipment(NumExteriorEqs).Name);
                    SetupOutputVariable("Exterior Equipment " + TypeString + " Volume",
                                        OutputProcessor::Unit::m3,
                                        ExteriorEquipment(NumExteriorEqs).CurrentUse,
                                        "Zone",
                                        "Sum",
                                        ExteriorEquipment(NumExteriorEqs).Name,
                                        _,
                                        TypeString,
                                        "ExteriorEquipment",
                                        EndUseSubcategoryName);
                }
            }
            ExteriorEquipment(NumExteriorEqs).SchedPtr = GetScheduleIndex(cAlphaArgs(3));
            if (ExteriorEquipment(NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ExteriorEquipment(NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(ExteriorEquipment(NumExteriorEqs).SchedPtr);
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
            ExteriorEquipment(NumExteriorEqs).DesignLevel = rNumericArgs(1);
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
            GlobalNames::VerifyUniqueInterObjectName(UniqueExteriorEquipNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            ++NumExteriorEqs;
            ExteriorEquipment(NumExteriorEqs).Name = cAlphaArgs(1);
            ExteriorEquipment(NumExteriorEqs).FuelType = WaterUse;
            ExteriorEquipment(NumExteriorEqs).SchedPtr = GetScheduleIndex(cAlphaArgs(3));
            if (ExteriorEquipment(NumExteriorEqs).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": " + cAlphaFieldNames(3) + " is required, missing for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(3) + " entered=" + cAlphaArgs(3) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(ExteriorEquipment(NumExteriorEqs).SchedPtr);
                SchMax = GetScheduleMaxValue(ExteriorEquipment(NumExteriorEqs).SchedPtr);
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

            ExteriorEquipment(NumExteriorEqs).DesignLevel = rNumericArgs(1);

            SetupOutputVariable("Exterior Equipment Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                ExteriorEquipment(NumExteriorEqs).Power,
                                "Zone",
                                "Average",
                                ExteriorEquipment(NumExteriorEqs).Name);

            SetupOutputVariable("Exterior Equipment Water Volume",
                                OutputProcessor::Unit::m3,
                                ExteriorEquipment(NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                ExteriorEquipment(NumExteriorEqs).Name,
                                _,
                                "Water",
                                "ExteriorEquipment",
                                EndUseSubcategoryName);
            SetupOutputVariable("Exterior Equipment Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                ExteriorEquipment(NumExteriorEqs).CurrentUse,
                                "Zone",
                                "Sum",
                                ExteriorEquipment(NumExteriorEqs).Name,
                                _,
                                "MainsWater",
                                "ExteriorEquipment",
                                EndUseSubcategoryName);
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input.  Program terminates.");
        }
    }

    void ValidateFuelType(int &FuelTypeNumber,                    // Fuel Type to be set in structure.
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

        FuelTypeNumber = 0;
        FuelTypeString = "";

        // Select the correct Number for the associated ascii name for the fuel type
        if (UtilityRoutines::SameString(FuelTypeAlpha, "Electricity") || UtilityRoutines::SameString(FuelTypeAlpha, "Electric")) {
            FuelTypeNumber = ElecUse;
            FuelTypeString = "Electric";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "NaturalGas")) {
            FuelTypeNumber = GasUse;
            FuelTypeString = "Gas";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Coal")) {
            FuelTypeNumber = CoalUse;
            FuelTypeString = "Coal";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOil#1")) {
            FuelTypeNumber = FuelOil1Use;
            FuelTypeString = "FuelOil#1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "PropaneGas")) {
            FuelTypeNumber = LPGUse;
            FuelTypeString = "Propane";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Gasoline")) {
            FuelTypeNumber = GasolineUse;
            FuelTypeString = "Gasoline";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Diesel")) {
            FuelTypeNumber = DieselUse;
            FuelTypeString = "Diesel";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "FuelOil#2")) {
            FuelTypeNumber = FuelOil2Use;
            FuelTypeString = "FuelOil#2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel1")) {
            FuelTypeNumber = OtherFuel1Use;
            FuelTypeString = "OtherFuel1";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "OtherFuel2")) {
            FuelTypeNumber = OtherFuel1Use;
            FuelTypeString = "OtherFuel2";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Water")) {
            FuelTypeNumber = WaterUse;
            FuelTypeString = "Water";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "Steam")) {
            FuelTypeNumber = SteamUse;
            FuelTypeString = "Steam";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictCooling")) {
            FuelTypeNumber = DistrictCoolUse;
            FuelTypeString = "DistrictCooling";
        } else if (UtilityRoutines::SameString(FuelTypeAlpha, "DistrictHeating")) {
            FuelTypeNumber = DistrictHeatUse;
            FuelTypeString = "DistrictHeating";
        } else {
            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + CurrentName + "\".");
            ShowFatalError("Heating source/fuel type not recognized. Check input field " + CurrentField + "=\"" + FuelTypeAlpha);
        }
    }

    void ReportExteriorEnergyUse()
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

        for (Item = 1; Item <= NumExteriorLights; ++Item) {
            {
                auto const SELECT_CASE_var(ExteriorLights(Item).ControlMode);

                if (SELECT_CASE_var == ScheduleOnly) {
                    ExteriorLights(Item).Power = ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(ExteriorLights(Item).SchedPtr);
                    ExteriorLights(Item).CurrentUse = ExteriorLights(Item).Power * TimeStepZoneSec;

                } else if (SELECT_CASE_var == AstroClockOverride) {

                    if (SunIsUp) {
                        ExteriorLights(Item).Power = 0.0;
                        ExteriorLights(Item).CurrentUse = 0.0;
                    } else {
                        ExteriorLights(Item).Power = ExteriorLights(Item).DesignLevel * GetCurrentScheduleValue(ExteriorLights(Item).SchedPtr);
                        ExteriorLights(Item).CurrentUse = ExteriorLights(Item).Power * TimeStepZoneSec;
                    }

                } else {
                    // should not occur
                }
            }

            // Reduce lighting power due to demand limiting
            if (ExteriorLights(Item).ManageDemand && (ExteriorLights(Item).Power > ExteriorLights(Item).DemandLimit)) {
                ExteriorLights(Item).Power = ExteriorLights(Item).DemandLimit;
                ExteriorLights(Item).CurrentUse = ExteriorLights(Item).Power * TimeStepZoneSec;
            }
            // EMS controls
            if (ExteriorLights(Item).PowerActuatorOn) ExteriorLights(Item).Power = ExteriorLights(Item).PowerActuatorValue;

            ExteriorLights(Item).CurrentUse = ExteriorLights(Item).Power * TimeStepZoneSec;

            // gather for tabular reports
            if (!WarmupFlag) {
                //      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
                if (DoOutputReporting && (KindOfSim == ksRunPeriodWeather)) { // for weather simulations only
                    // for tabular report, accumlate the total electricity used for each ExteriorLights object
                    ExteriorLights(Item).SumConsumption += ExteriorLights(Item).CurrentUse;
                    // for tabular report, accumulate the time when each ExteriorLights has consumption
                    //(using a very small threshold instead of zero)
                    if (ExteriorLights(Item).CurrentUse > 0.01) {
                        ExteriorLights(Item).SumTimeNotZeroCons += TimeStepZone;
                    }
                }
            }
        }

        for (Item = 1; Item <= NumExteriorEqs; ++Item) {
            ExteriorEquipment(Item).Power = ExteriorEquipment(Item).DesignLevel * GetCurrentScheduleValue(ExteriorEquipment(Item).SchedPtr);
            ExteriorEquipment(Item).CurrentUse = ExteriorEquipment(Item).Power * TimeStepZoneSec;
        }
    }

} // namespace ExteriorEnergyUse

} // namespace EnergyPlus
