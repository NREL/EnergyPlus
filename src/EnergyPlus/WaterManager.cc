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
#include <cassert>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/EcoRoofManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

namespace WaterManager {

    // Module containing the routines dealing with the management of water

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2006
    //       MODIFIED       DJS to add ecoroof irrigation Jan 2007
    //       RE-ENGINEERED  na

    using namespace DataWater;

    void ManageWater(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is the top-level driver subroutine for managine water systems in the building
        // Routine is called at the system timestep level from ManageHVAC
        //  (somewhat analogous to SimHVAC)

        // METHODOLOGY EMPLOYED:
        // State variables are continually recalculated each system iteration
        // except when appropriate to update them.  IF this module is moved up
        // to a different timestep (with less iteration), then numerical solution
        // may need to be added.  Iteration is being used to solve interdependencies
        // of storage, supply, and demand modeling of water system.
        // Most data are declared in data-only module DataWater.hh
        // Calling order,
        //   storage tanks
        //   supply
        //   demands
        //  IF first/last timestep, then do an update.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RainColNum(0);
        int TankNum(0);
        int WellNum(0);

        if (state.dataWaterManager->GetInputFlag) {
            GetWaterManagerInput(state);
            state.dataWaterManager->GetInputFlag = false;
        }

        if (!(state.dataWaterData->AnyWaterSystemsInModel)) return;

        // this is the main water manager
        // first call all the water storage tanks
        //    (these called first to make control decisions)
        for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {
            CalcWaterStorageTank(state, TankNum);
        } // tank loop

        for (RainColNum = 1; RainColNum <= state.dataWaterData->NumRainCollectors; ++RainColNum) {
            CalcRainCollector(state, RainColNum);
        }

        for (WellNum = 1; WellNum <= state.dataWaterData->NumGroundWaterWells; ++WellNum) {
            CalcGroundwaterWell(state, WellNum);
        }

        // call the tanks again to get updated rain and well activity
        for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {
            CalcWaterStorageTank(state, TankNum);
        } // tank loop
    }

    void ManageWaterInits(EnergyPlusData &state)
    {
        if (!(state.dataWaterData->AnyWaterSystemsInModel)) return;

        UpdateWaterManager(state);
        UpdateIrrigation(state);
    }

    void GetWaterManagerInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        using ScheduleManager::CheckScheduleValue;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;                // Item to be "gotten"
        int NumAlphas(0);        // Number of Alphas for each GetObjectItem call
        int NumNumbers(0);       // Number of Numbers for each GetObjectItem call
        int IOStatus(0);         // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int MaxNumAlphas(0);     // argument for call to GetObjectDefMaxArgs
        int MaxNumNumbers(0);    // argument for call to GetObjectDefMaxArgs
        int TotalArgs(0);        // argument for call to GetObjectDefMaxArgs
        int alphaOffset(0);
        std::string objNameMsg;
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        int NumIrrigation;
        int Dummy;

        if ((state.dataWaterManager->MyOneTimeFlag) && (!(state.dataWaterData->WaterSystemGetInputCalled))) { // big block for entire subroutine
            // initialize rainfall model
            state.dataWaterData->RainFall.ModeID = DataWater::RainfallMode::None;

            cCurrentModuleObject = "WaterUse:Storage";
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
            MaxNumNumbers = NumNumbers;
            MaxNumAlphas = NumAlphas;
            cCurrentModuleObject = "WaterUse:RainCollector";
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
            MaxNumNumbers = max(MaxNumNumbers, NumNumbers);
            MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
            cCurrentModuleObject = "WaterUse:Well";
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
            MaxNumNumbers = max(MaxNumNumbers, NumNumbers);
            MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
            cCurrentModuleObject = "Site:Precipitation";
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
            MaxNumNumbers = max(MaxNumNumbers, NumNumbers);
            MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
            cCurrentModuleObject = "RoofIrrigation";
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
            MaxNumNumbers = max(MaxNumNumbers, NumNumbers);
            MaxNumAlphas = max(MaxNumAlphas, NumAlphas);

            cAlphaFieldNames.allocate(MaxNumAlphas);
            cAlphaArgs.allocate(MaxNumAlphas);
            lAlphaFieldBlanks.dimension(MaxNumAlphas, false);
            cNumericFieldNames.allocate(MaxNumNumbers);
            rNumericArgs.dimension(MaxNumNumbers, 0.0);
            lNumericFieldBlanks.dimension(MaxNumNumbers, false);

            state.dataWaterManager->MyOneTimeFlag = false;
            cCurrentModuleObject = "WaterUse:Storage";
            state.dataWaterData->NumWaterStorageTanks = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (state.dataWaterData->NumWaterStorageTanks > 0) {
                state.dataWaterData->AnyWaterSystemsInModel = true;
                if (!(allocated(state.dataWaterData->WaterStorage)))
                    state.dataWaterData->WaterStorage.allocate(state.dataWaterData->NumWaterStorageTanks);

                for (Item = 1; Item <= state.dataWaterData->NumWaterStorageTanks; ++Item) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             Item,
                                                                             cAlphaArgs,
                                                                             NumAlphas,
                                                                             rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    state.dataWaterData->AnyWaterSystemsInModel = true;
                    state.dataWaterData->WaterStorage(Item).Name = cAlphaArgs(1);
                    UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                    objNameMsg = cCurrentModuleObject + " = " + cAlphaArgs(1);

                    state.dataWaterData->WaterStorage(Item).QualitySubCategoryName = cAlphaArgs(2);

                    state.dataWaterData->WaterStorage(Item).MaxCapacity = rNumericArgs(1);
                    if (state.dataWaterData->WaterStorage(Item).MaxCapacity == 0.0) { // default
                        state.dataWaterData->WaterStorage(Item).MaxCapacity = DataGlobalConstants::BigNumber;
                    }

                    state.dataWaterData->WaterStorage(Item).InitialVolume = rNumericArgs(2);
                    state.dataWaterData->WaterStorage(Item).MaxInFlowRate = rNumericArgs(3);
                    if (state.dataWaterData->WaterStorage(Item).MaxInFlowRate == 0.0) { // default
                        state.dataWaterData->WaterStorage(Item).MaxInFlowRate = DataGlobalConstants::BigNumber;
                    }

                    state.dataWaterData->WaterStorage(Item).MaxOutFlowRate = rNumericArgs(4);
                    if (state.dataWaterData->WaterStorage(Item).MaxOutFlowRate == 0.0) { // default
                        state.dataWaterData->WaterStorage(Item).MaxOutFlowRate = DataGlobalConstants::BigNumber;
                    }

                    state.dataWaterData->WaterStorage(Item).OverflowTankName = cAlphaArgs(3); // setup later

                    if (UtilityRoutines::SameString(cAlphaArgs(4), "None")) {
                        state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::NoControlLevel;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(4), "Mains")) {
                        state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::MainsFloatValve;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(4), "GroundwaterWell")) {
                        state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::WellFloatValve;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(4), "OtherTank")) {
                        state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::OtherTankFloatValve;
                    } else {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                    state.dataWaterData->WaterStorage(Item).ValveOnCapacity = rNumericArgs(5);
                    state.dataWaterData->WaterStorage(Item).ValveOffCapacity = rNumericArgs(6);
                    if (state.dataWaterData->WaterStorage(Item).ControlSupply != DataWater::ControlSupplyType::NoControlLevel) {
                        if (state.dataWaterData->WaterStorage(Item).ValveOffCapacity < state.dataWaterData->WaterStorage(Item).ValveOnCapacity) {
                            ShowSevereError(state, "Invalid " + cNumericFieldNames(5) + " and/or " + cNumericFieldNames(6));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError(state, cNumericFieldNames(6) + " must be greater than " + cNumericFieldNames(5));
                            ShowContinueError(state,
                                              format("Check value for {} = {:.5R}",
                                                     cNumericFieldNames(5),
                                                     state.dataWaterData->WaterStorage(Item).ValveOnCapacity));
                            ShowContinueError(state,
                                              format("which must be lower than {} = {:.5R}",
                                                     cNumericFieldNames(6),
                                                     state.dataWaterData->WaterStorage(Item).ValveOffCapacity));
                            ErrorsFound = true;
                        }
                    }

                    state.dataWaterData->WaterStorage(Item).BackupMainsCapacity = rNumericArgs(7);
                    if (state.dataWaterData->WaterStorage(Item).BackupMainsCapacity > 0.0) { // add backup to well and other thank supply
                        if (state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::WellFloatValve) {
                            state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::WellFloatMainsBackup;
                        }
                        if (state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::OtherTankFloatValve) {
                            state.dataWaterData->WaterStorage(Item).ControlSupply = DataWater::ControlSupplyType::TankMainsBackup;
                        }
                    }

                    state.dataWaterData->WaterStorage(Item).SupplyTankName = cAlphaArgs(5); // set up later

                    if (UtilityRoutines::SameString(cAlphaArgs(6), "ScheduledTemperature")) {
                        state.dataWaterData->WaterStorage(Item).ThermalMode = DataWater::TankThermalMode::Scheduled;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(6), "ThermalModel")) {
                        state.dataWaterData->WaterStorage(Item).ThermalMode = DataWater::TankThermalMode::ZoneCoupled;
                    } else {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(6) + '=' + cAlphaArgs(6));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                    }

                    if (state.dataWaterData->WaterStorage(Item).ThermalMode == DataWater::TankThermalMode::Scheduled) {
                        state.dataWaterData->WaterStorage(Item).TempSchedID = GetScheduleIndex(state, cAlphaArgs(7));
                        if (state.dataWaterData->WaterStorage(Item).TempSchedID == 0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                        Real64 tmpMin = GetScheduleMinValue(state, state.dataWaterData->WaterStorage(Item).TempSchedID);
                        if (tmpMin < 0.0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError(state, "Found storage tank temperature schedule value less than 0.0 in " + objNameMsg);
                            ErrorsFound = true;
                        }
                        Real64 tmpMax = GetScheduleMaxValue(state, state.dataWaterData->WaterStorage(Item).TempSchedID);
                        if (tmpMax > 100.0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError(state, "found storage tank temperature schedule value greater than 100.0 in " + objNameMsg);
                            ErrorsFound = true;
                        }
                    }

                    if (state.dataWaterData->WaterStorage(Item).ThermalMode == DataWater::TankThermalMode::ZoneCoupled) {
                        if (UtilityRoutines::SameString(cAlphaArgs(8), "Schedule")) {
                            state.dataWaterData->WaterStorage(Item).AmbientTempIndicator = DataWater::AmbientTempType::Schedule;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(8), "Zone")) {
                            state.dataWaterData->WaterStorage(Item).AmbientTempIndicator = DataWater::AmbientTempType::Zone;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(8), "Outdoors")) {
                            state.dataWaterData->WaterStorage(Item).AmbientTempIndicator = DataWater::AmbientTempType::Exterior;
                        } else {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(8) + '=' + cAlphaArgs(8));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                        state.dataWaterData->WaterStorage(Item).AmbientTempSchedule = GetScheduleIndex(state, cAlphaArgs(9));
                        if ((state.dataWaterData->WaterStorage(Item).AmbientTempSchedule == 0) &&
                            (state.dataWaterData->WaterStorage(Item).AmbientTempIndicator == DataWater::AmbientTempType::Schedule)) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(9) + '=' + cAlphaArgs(9));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                        state.dataWaterData->WaterStorage(Item).ZoneID = UtilityRoutines::FindItemInList(cAlphaArgs(10), state.dataHeatBal->Zone);
                        if ((state.dataWaterData->WaterStorage(Item).ZoneID == 0) &&
                            (state.dataWaterData->WaterStorage(Item).AmbientTempIndicator == DataWater::AmbientTempType::Zone)) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(10) + '=' + cAlphaArgs(10));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                        state.dataWaterData->WaterStorage(Item).SurfArea = rNumericArgs(8);
                        state.dataWaterData->WaterStorage(Item).UValue = rNumericArgs(9);
                        state.dataWaterData->WaterStorage(Item).SurfMaterialName = cAlphaArgs(11);
                        // todo verify material collect and store useful data from it.
                    }
                }
            } // num water storage tanks > 0

            cCurrentModuleObject = "WaterUse:RainCollector";
            state.dataWaterData->NumRainCollectors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (state.dataWaterData->NumRainCollectors > 0) {
                if (!(allocated(state.dataWaterData->RainCollector)))
                    state.dataWaterData->RainCollector.allocate(state.dataWaterData->NumRainCollectors);
                // allow extensible reference to surfaces.
                state.dataWaterData->AnyWaterSystemsInModel = true;

                if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::None) {
                    state.dataWaterData->RainFall.ModeID = DataWater::RainfallMode::EPWPrecipitation;
                }

                for (Item = 1; Item <= state.dataWaterData->NumRainCollectors; ++Item) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             Item,
                                                                             cAlphaArgs,
                                                                             NumAlphas,
                                                                             rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    state.dataWaterData->RainCollector(Item).Name = cAlphaArgs(1);
                    UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                    objNameMsg = cCurrentModuleObject + " Named " + cAlphaArgs(1);

                    state.dataWaterData->RainCollector(Item).StorageTankName = cAlphaArgs(2);
                    state.dataWaterData->RainCollector(Item).StorageTankID =
                        UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataWaterData->WaterStorage);
                    if (state.dataWaterData->RainCollector(Item).StorageTankID == 0) {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }

                    if (UtilityRoutines::SameString(cAlphaArgs(3), "Constant")) {
                        state.dataWaterData->RainCollector(Item).LossFactorMode = DataWater::RainLossFactor::Constant;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Scheduled")) {
                        state.dataWaterData->RainCollector(Item).LossFactorMode = DataWater::RainLossFactor::Scheduled;
                    } else {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                    state.dataWaterData->RainCollector(Item).LossFactor = rNumericArgs(1);
                    if (state.dataWaterData->RainCollector(Item).LossFactor > 1.0) {
                        ShowWarningError(state, format("Invalid {}={:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError(state, "found rain water collection loss factor greater than 1.0, simulation continues");
                    }
                    if (state.dataWaterData->RainCollector(Item).LossFactor < 0.0) {
                        ShowSevereError(state, format("Invalid {}={:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError(state, "found rain water collection loss factor less than 0.0");
                        ErrorsFound = true;
                    }

                    if (state.dataWaterData->RainCollector(Item).LossFactorMode == DataWater::RainLossFactor::Scheduled) {
                        state.dataWaterData->RainCollector(Item).LossFactorSchedID = GetScheduleIndex(state, cAlphaArgs(4));
                        if (state.dataWaterData->RainCollector(Item).LossFactorSchedID == 0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                        if (GetScheduleMinValue(state, state.dataWaterData->RainCollector(Item).LossFactorSchedID) < 0.0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError(state, "found rain water collection loss factor schedule value less than 0.0 in " + objNameMsg);
                            ErrorsFound = true;
                        }
                        if (GetScheduleMaxValue(state, state.dataWaterData->RainCollector(Item).LossFactorSchedID) > 1.0) {
                            ShowWarningError(state, "Potentially invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError(state, "found rain water collection loss factor schedule value greater than 1.0, simulation continues");
                            // allowing it to continue
                        }
                    }
                    state.dataWaterData->RainCollector(Item).MaxCollectRate = rNumericArgs(1);
                    if (state.dataWaterData->RainCollector(Item).MaxCollectRate == 0.0)
                        state.dataWaterData->RainCollector(Item).MaxCollectRate = 100000000000.0;

                    // number of surfaces is extensible and = NumAlphas - alphaOffset
                    alphaOffset = 4; // update this if more alphas inserted ahead of extensible surface listing
                    state.dataWaterData->RainCollector(Item).NumCollectSurfs = NumAlphas - alphaOffset;
                    state.dataWaterData->RainCollector(Item).SurfName.allocate(state.dataWaterData->RainCollector(Item).NumCollectSurfs);
                    state.dataWaterData->RainCollector(Item).SurfID.allocate(state.dataWaterData->RainCollector(Item).NumCollectSurfs);
                    for (int SurfNum = 1; SurfNum <= state.dataWaterData->RainCollector(Item).NumCollectSurfs; ++SurfNum) {
                        state.dataWaterData->RainCollector(Item).SurfName(SurfNum) = cAlphaArgs(SurfNum + alphaOffset);
                        state.dataWaterData->RainCollector(Item).SurfID(SurfNum) =
                            UtilityRoutines::FindItemInList(cAlphaArgs(SurfNum + alphaOffset), state.dataSurface->Surface);
                        if (state.dataWaterData->RainCollector(Item).SurfID(SurfNum) == 0) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(SurfNum + alphaOffset) + '=' + cAlphaArgs(SurfNum + alphaOffset));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    }

                    // now setup horizontal surface area
                    Real64 tmpArea = 0.0;
                    Real64 tmpNumerator = 0.0;
                    Real64 tmpDenominator = 0.0;
                    for (int SurfNum = 1; SurfNum <= state.dataWaterData->RainCollector(Item).NumCollectSurfs; ++SurfNum) {
                        int ThisSurf = state.dataWaterData->RainCollector(Item).SurfID(SurfNum);
                        tmpArea += state.dataSurface->Surface(ThisSurf).GrossArea * state.dataSurface->Surface(ThisSurf).CosTilt;
                        tmpNumerator += state.dataSurface->Surface(ThisSurf).Centroid.z * state.dataSurface->Surface(ThisSurf).GrossArea;
                        tmpDenominator += state.dataSurface->Surface(ThisSurf).GrossArea;
                    }
                    state.dataWaterData->RainCollector(Item).HorizArea = tmpArea;
                    // now setup vertical hieght above ground for height dependent outdoor temps
                    state.dataWaterData->RainCollector(Item).MeanHeight = tmpNumerator / tmpDenominator;

                    // now set up tank supply connection
                    InternalSetupTankSupplyComponent(state,
                                                     state.dataWaterData->RainCollector(Item).Name,
                                                     cCurrentModuleObject,
                                                     state.dataWaterData->RainCollector(Item).StorageTankName,
                                                     ErrorsFound,
                                                     state.dataWaterData->RainCollector(Item).StorageTankID,
                                                     state.dataWaterData->RainCollector(Item).StorageTankSupplyARRID);
                }
            } // (NumRainCollectors > 0)

            cCurrentModuleObject = "WaterUse:Well";
            state.dataWaterData->NumGroundWaterWells = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (state.dataWaterData->NumGroundWaterWells > 0) {
                state.dataWaterData->AnyWaterSystemsInModel = true;
                state.dataWaterData->GroundwaterWell.allocate(state.dataWaterData->NumGroundWaterWells);
                for (Item = 1; Item <= state.dataWaterData->NumGroundWaterWells; ++Item) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             Item,
                                                                             cAlphaArgs,
                                                                             NumAlphas,
                                                                             rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             lAlphaFieldBlanks,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    state.dataWaterData->GroundwaterWell(Item).Name = cAlphaArgs(1);
                    UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                    objNameMsg = cCurrentModuleObject + " Named " + cAlphaArgs(1);
                    state.dataWaterData->GroundwaterWell(Item).StorageTankName = cAlphaArgs(2);

                    InternalSetupTankSupplyComponent(state,
                                                     state.dataWaterData->GroundwaterWell(Item).Name,
                                                     cCurrentModuleObject,
                                                     state.dataWaterData->GroundwaterWell(Item).StorageTankName,
                                                     ErrorsFound,
                                                     state.dataWaterData->GroundwaterWell(Item).StorageTankID,
                                                     state.dataWaterData->GroundwaterWell(Item).StorageTankSupplyARRID);

                    if (allocated(state.dataWaterData->WaterStorage))
                        state.dataWaterData->WaterStorage(state.dataWaterData->GroundwaterWell(Item).StorageTankID).GroundWellID = Item;

                    state.dataWaterData->GroundwaterWell(Item).PumpDepth = rNumericArgs(1);
                    state.dataWaterData->GroundwaterWell(Item).PumpNomVolFlowRate = rNumericArgs(2);
                    state.dataWaterData->GroundwaterWell(Item).PumpNomHead = rNumericArgs(3);
                    state.dataWaterData->GroundwaterWell(Item).PumpNomPowerUse = rNumericArgs(4);
                    state.dataWaterData->GroundwaterWell(Item).PumpEfficiency = rNumericArgs(5);
                    state.dataWaterData->GroundwaterWell(Item).WellRecoveryRate = rNumericArgs(6);
                    state.dataWaterData->GroundwaterWell(Item).NomWellStorageVol = rNumericArgs(7);
                    if (UtilityRoutines::SameString(cAlphaArgs(3), "Constant")) {
                        state.dataWaterData->GroundwaterWell(Item).GroundwaterTableMode = DataWater::GroundWaterTable::Constant;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Scheduled")) {
                        state.dataWaterData->GroundwaterWell(Item).GroundwaterTableMode = DataWater::GroundWaterTable::Scheduled;
                    } else if (lAlphaFieldBlanks(3)) {
                        state.dataWaterData->GroundwaterWell(Item).GroundwaterTableMode = DataWater::GroundWaterTable::Invalid;
                    } else {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }

                    //  N8, \field water table depth
                    state.dataWaterData->GroundwaterWell(Item).WaterTableDepth = rNumericArgs(8);
                    // A4; \field water table depth schedule
                    state.dataWaterData->GroundwaterWell(Item).WaterTableDepthSchedID = GetScheduleIndex(state, cAlphaArgs(4));
                    if ((state.dataWaterData->GroundwaterWell(Item).GroundwaterTableMode == DataWater::GroundWaterTable::Scheduled) &&
                        (state.dataWaterData->GroundwaterWell(Item).WaterTableDepthSchedID == 0)) {
                        ShowSevereError(state, "Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }
            } //(NumGroundWaterWells > 0)

            // do some water tank setup
            cCurrentModuleObject = "WaterUse:Storage";
            if (state.dataWaterData->NumWaterStorageTanks > 0) {
                for (Item = 1; Item <= state.dataWaterData->NumWaterStorageTanks; ++Item) {
                    // check that all storage tanks with ground well controls actually had wells pointing to them
                    if ((state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::WellFloatValve) ||
                        (state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::WellFloatMainsBackup)) {
                        if (state.dataWaterData->WaterStorage(Item).GroundWellID == 0) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "= \"" + state.dataWaterData->WaterStorage(Item).Name +
                                                "\" does not have a WaterUse:Well (groundwater well) that names it.");
                            ErrorsFound = true;
                        }
                    }

                    // setup tanks whose level is controlled by supply from another tank
                    if ((state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::OtherTankFloatValve) ||
                        (state.dataWaterData->WaterStorage(Item).ControlSupply == DataWater::ControlSupplyType::TankMainsBackup)) {
                        state.dataWaterData->WaterStorage(Item).SupplyTankID = UtilityRoutines::FindItemInList(
                            state.dataWaterData->WaterStorage(Item).SupplyTankName, state.dataWaterData->WaterStorage);
                        if (state.dataWaterData->WaterStorage(Item).SupplyTankID == 0) {
                            ShowSevereError(state,
                                            "Other tank called " + state.dataWaterData->WaterStorage(Item).SupplyTankName + " not found for " +
                                                cCurrentModuleObject + " Named " + state.dataWaterData->WaterStorage(Item).Name); // TODO rename point
                            ErrorsFound = true;
                        }
                        InternalSetupTankDemandComponent(state,
                                                         state.dataWaterData->WaterStorage(Item).Name,
                                                         cCurrentModuleObject,
                                                         state.dataWaterData->WaterStorage(Item).SupplyTankName,
                                                         ErrorsFound,
                                                         state.dataWaterData->WaterStorage(Item).SupplyTankID,
                                                         state.dataWaterData->WaterStorage(Item).SupplyTankDemandARRID);
                        // call to setup tank supply as well
                        InternalSetupTankSupplyComponent(state,
                                                         state.dataWaterData->WaterStorage(Item).SupplyTankName,
                                                         cCurrentModuleObject,
                                                         state.dataWaterData->WaterStorage(Item).Name,
                                                         ErrorsFound,
                                                         Dummy,
                                                         Dummy);
                    }
                    // setup overflow inputs
                    state.dataWaterData->WaterStorage(Item).OverflowTankID =
                        UtilityRoutines::FindItemInList(state.dataWaterData->WaterStorage(Item).OverflowTankName, state.dataWaterData->WaterStorage);
                    if (state.dataWaterData->WaterStorage(Item).OverflowTankID == 0) {
                        // if blank, then okay it is discarded.  but if not blank then error
                        if (is_blank(state.dataWaterData->WaterStorage(Item).OverflowTankName)) {
                            state.dataWaterData->WaterStorage(Item).OverflowMode = DataWater::Overflow::Discarded;
                        } else {
                            ShowSevereError(state,
                                            "Overflow tank name of " + state.dataWaterData->WaterStorage(Item).OverflowTankName + " not found for " +
                                                cCurrentModuleObject + " Named " + state.dataWaterData->WaterStorage(Item).Name);
                            ErrorsFound = true;
                        }
                    } else {
                        state.dataWaterData->WaterStorage(Item).OverflowMode = DataWater::Overflow::ToTank;
                    }
                    if (state.dataWaterData->WaterStorage(Item).OverflowMode == DataWater::Overflow::ToTank) {
                        InternalSetupTankSupplyComponent(state,
                                                         state.dataWaterData->WaterStorage(Item).Name,
                                                         cCurrentModuleObject,
                                                         state.dataWaterData->WaterStorage(Item).OverflowTankName,
                                                         ErrorsFound,
                                                         state.dataWaterData->WaterStorage(Item).OverflowTankID,
                                                         state.dataWaterData->WaterStorage(Item).OverflowTankSupplyARRID);
                    }
                }
            }

            cCurrentModuleObject = "Site:Precipitation";
            state.dataWaterData->NumSiteRainFall = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (state.dataWaterData->NumSiteRainFall > 1) { // throw error
                ShowSevereError(state, "Only one " + cCurrentModuleObject + " object is allowed");
                ErrorsFound = true;
            }

            if (state.dataWaterData->NumSiteRainFall == 1) {
                state.dataWaterData->AnyWaterSystemsInModel = true;
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);

                if (UtilityRoutines::SameString(cAlphaArgs(1), "ScheduleAndDesignLevel")) {
                    state.dataWaterData->RainFall.ModeID = DataWater::RainfallMode::RainSchedDesign;
                } else {
                    ShowSevereError(state, "Precipitation Model Type of " + cCurrentModuleObject + " is incorrect.");
                    ShowContinueError(state, "Only available option is ScheduleAndDesignLevel.");
                    ErrorsFound = true;
                }
                state.dataWaterData->RainFall.RainSchedID = GetScheduleIndex(state, cAlphaArgs(2));
                if ((state.dataWaterData->RainFall.RainSchedID == 0) &&
                    (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign)) {
                    ShowSevereError(state, "Schedule not found for " + cCurrentModuleObject + " object");
                    ErrorsFound = true;
                } else if ((state.dataWaterData->RainFall.RainSchedID != 0) &&
                           (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign)) {
                    if (!CheckScheduleValueMinMax(state, state.dataWaterData->RainFall.RainSchedID, ">=", 0.0)) {
                        ShowSevereError(state, "Schedule=" + cAlphaArgs(2) + " for " + cCurrentModuleObject + " object has values < 0.");
                        ErrorsFound = true;
                    }
                }

                state.dataWaterData->RainFall.DesignAnnualRain = rNumericArgs(1);
                state.dataWaterData->RainFall.NomAnnualRain = rNumericArgs(2);
            }

            cCurrentModuleObject = "RoofIrrigation";
            NumIrrigation = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (NumIrrigation > 1) {
                ShowSevereError(state, "Only one " + cCurrentModuleObject + " object is allowed");
                ErrorsFound = true;
            }

            if (NumIrrigation == 1) {
                state.dataWaterData->AnyIrrigationInModel = true;
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
                if (UtilityRoutines::SameString(cAlphaArgs(1), "Schedule")) {
                    state.dataWaterData->Irrigation.ModeID = DataWater::IrrigationMode::IrrSchedDesign;
                } else if (UtilityRoutines::SameString(cAlphaArgs(1), "SmartSchedule")) {
                    state.dataWaterData->Irrigation.ModeID = DataWater::IrrigationMode::IrrSmartSched;
                } else {
                    ShowSevereError(state, "Type of " + cCurrentModuleObject + " is incorrect. Options are Schedule or SmartSchedule");
                    ErrorsFound = true;
                }
                if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::None) {
                    state.dataWaterData->RainFall.ModeID = DataWater::RainfallMode::EPWPrecipitation;
                }
                state.dataWaterData->Irrigation.IrrSchedID = GetScheduleIndex(state, cAlphaArgs(2));
                if ((state.dataWaterData->Irrigation.IrrSchedID == 0) &&
                    ((state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSchedDesign) ||
                     state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSmartSched)) {
                    ShowSevereError(state, "Schedule not found for " + cCurrentModuleObject + " object");
                    ErrorsFound = true;
                } else if ((state.dataWaterData->Irrigation.IrrSchedID == 0) &&
                           (state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSchedDesign)) {
                    if (!CheckScheduleValueMinMax(state, state.dataWaterData->Irrigation.IrrSchedID, ">=", 0.0)) {
                        ShowSevereError(state, "Schedule=" + cAlphaArgs(2) + " for " + cCurrentModuleObject + " object has values < 0.");
                        ErrorsFound = true;
                    }
                }

                // If we later add a designannualirrigation and a nominalannualirrigation variable (for scaling) those
                // would be assigned here... as with the Rainfall...
                state.dataWaterData->Irrigation.IrrigationThreshold = 0.4;
                if (state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSmartSched && NumNumbers > 0) {
                    if (rNumericArgs(1) > 100.0 || rNumericArgs(1) < 0.0) {
                        ShowSevereError(state, "Irrigation threshold for " + cCurrentModuleObject + " object has values > 100 or < 0.");
                        ErrorsFound = true;
                    } else {
                        state.dataWaterData->Irrigation.IrrigationThreshold = rNumericArgs(1) / 100.0;
                    }
                }

            } // NumIrrigation ==1

            if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::EPWPrecipitation) {
                ShowWarningError(state,
                                 "Precipitation depth from the weather file will be used. Please make sure this .epw field has valid data. "
                                 "Site:Precipitation may be used to override the weather file data.");
            }

            state.dataWaterData->AnyWaterSystemsInModel = true;
            state.dataWaterData->WaterSystemGetInputCalled = true;
            state.dataWaterManager->MyOneTimeFlag = false;

            cAlphaFieldNames.deallocate();
            cAlphaArgs.deallocate();
            lAlphaFieldBlanks.deallocate();
            cNumericFieldNames.deallocate();
            rNumericArgs.deallocate();
            lNumericFieldBlanks.deallocate();

            if (ErrorsFound) {
                ShowFatalError(state, "Errors found in processing input for water manager objects");
            }
            // <SetupOutputVariables here...>, CurrentModuleObject='WaterUse:Storage'
            for (Item = 1; Item <= state.dataWaterData->NumWaterStorageTanks; ++Item) {
                // this next one is a measure of the state of water in the tank, not a flux of m3 that needs to be summed
                SetupOutputVariable(state,
                                    "Water System Storage Tank Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataWaterData->WaterStorage(Item).ThisTimeStepVolume,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Net Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->WaterStorage(Item).NetVdot,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Inlet Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->WaterStorage(Item).VdotToTank,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Outlet Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->WaterStorage(Item).VdotFromTank,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataWaterData->WaterStorage(Item).MainsDrawVol,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataWaterData->WaterStorage(Item).Name,
                                    _,
                                    "MainsWater",
                                    "WaterSystem",
                                    state.dataWaterData->WaterStorage(Item).QualitySubCategoryName,
                                    "System");
                SetupOutputVariable(state,
                                    "Water System Storage Tank Mains Water Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->WaterStorage(Item).MainsDrawVdot,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Water Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterData->WaterStorage(Item).Twater,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Storage Tank Overflow Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->WaterStorage(Item).VdotOverflow,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
                if (state.dataWaterData->WaterStorage(Item).OverflowMode == DataWater::Overflow::Discarded) {
                    SetupOutputVariable(state,
                                        "Water System Storage Tank Overflow Water Volume",
                                        OutputProcessor::Unit::m3,
                                        state.dataWaterData->WaterStorage(Item).VolOverflow,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataWaterData->WaterStorage(Item).Name);
                } else {
                    SetupOutputVariable(state,
                                        "Water System Storage Tank Overflow Water Volume",
                                        OutputProcessor::Unit::m3,
                                        state.dataWaterData->WaterStorage(Item).VolOverflow,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataWaterData->WaterStorage(Item).Name);
                }
                SetupOutputVariable(state,
                                    "Water System Storage Tank Overflow Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterData->WaterStorage(Item).TwaterOverflow,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->WaterStorage(Item).Name);
            }

            if (NumIrrigation == 1) { // CurrentModuleObject='RoofIrrigation'
                SetupOutputVariable(state,
                                    "Water System Roof Irrigation Scheduled Depth",
                                    OutputProcessor::Unit::m,
                                    state.dataWaterData->Irrigation.ScheduledAmount,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "RoofIrrigation");
                SetupOutputVariable(state,
                                    "Water System Roof Irrigation Actual Depth",
                                    OutputProcessor::Unit::m,
                                    state.dataWaterData->Irrigation.ActualAmount,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "RoofIrrigation");
            }

            for (Item = 1; Item <= state.dataWaterData->NumRainCollectors; ++Item) { // CurrentModuleObject='WaterUse:RainCollector'
                SetupOutputVariable(state,
                                    "Water System Rainwater Collector Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->RainCollector(Item).VdotAvail,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->RainCollector(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Rainwater Collector Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataWaterData->RainCollector(Item).VolCollected,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataWaterData->RainCollector(Item).Name,
                                    _,
                                    "OnSiteWater",
                                    "Rainwater",
                                    _,
                                    "System");
            }

            for (Item = 1; Item <= state.dataWaterData->NumGroundWaterWells; ++Item) { // CurrentModuleObject='WaterUse:Well'
                SetupOutputVariable(state,
                                    "Water System Groundwater Well Requested Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->GroundwaterWell(Item).VdotRequest,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->GroundwaterWell(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Groundwater Well Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataWaterData->GroundwaterWell(Item).VdotDelivered,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->GroundwaterWell(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Groundwater Well Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataWaterData->GroundwaterWell(Item).VolDelivered,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataWaterData->GroundwaterWell(Item).Name,
                                    _,
                                    "OnSiteWater",
                                    "Wellwater",
                                    _,
                                    "System");
                SetupOutputVariable(state,
                                    "Water System Groundwater Well Pump Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterData->GroundwaterWell(Item).PumpPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataWaterData->GroundwaterWell(Item).Name);
                SetupOutputVariable(state,
                                    "Water System Groundwater Well Pump Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataWaterData->GroundwaterWell(Item).PumpEnergy,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataWaterData->GroundwaterWell(Item).Name,
                                    _,
                                    "Electricity",
                                    "WaterSystems",
                                    _,
                                    "System");
            }

        } // my one time flag block
    }

    void UpdatePrecipitation(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update the current rate of precipitation

        using ScheduleManager::GetCurrentScheduleValue;

        Real64 schedRate;
        Real64 ScaleFactor;

        // when the site:precipitation exists, use the precipitation schedule
        if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign) {
            schedRate = GetCurrentScheduleValue(state, state.dataWaterData->RainFall.RainSchedID); // m/hr
            if (state.dataWaterData->RainFall.NomAnnualRain > 0.0) {
                ScaleFactor = state.dataWaterData->RainFall.DesignAnnualRain / state.dataWaterData->RainFall.NomAnnualRain;
            } else {
                ScaleFactor = 0.0;
            }
            state.dataWaterData->RainFall.CurrentRate = schedRate * ScaleFactor / DataGlobalConstants::SecInHour; // convert to m/s
        } else {
            // placeholder: add EP checks for out of range precipitation value later -- yujie
            // when there's no site:precipitation but non-zero epw precipitation, uset the epw precipitation as the CurrentRate
            if (state.dataEnvrn->LiquidPrecipitation > 0.0) {
                // LiquidPrecipitation is for a certain timestep in an hour, the rate = depth / seconds in a timestep
                state.dataWaterData->RainFall.CurrentRate = state.dataEnvrn->LiquidPrecipitation / state.dataGlobal->TimeStepZoneSec;
            } else {
                state.dataWaterData->RainFall.CurrentRate = 0.0;
            }
        }
        state.dataWaterData->RainFall.CurrentAmount = state.dataWaterData->RainFall.CurrentRate * state.dataGlobal->TimeStepZoneSec;
        state.dataEcoRoofMgr->CurrentPrecipitation = state.dataWaterData->RainFall.CurrentAmount; //  units of m

        if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign) {
            if ((state.dataEnvrn->RunPeriodEnvironment) && (!state.dataGlobal->WarmupFlag)) {
                int month = state.dataEnvrn->Month;
                state.dataWaterData->RainFall.MonthlyTotalPrecInSitePrec.at(month - 1) += state.dataWaterData->RainFall.CurrentAmount * 1000.0;
            }
        }
    }

    void UpdateIrrigation(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         D. Sailor
        //       DATE WRITTEN   Dec 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update the current rate of irrigation

        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using ScheduleManager::GetCurrentScheduleValue;

        Real64 schedRate;

        state.dataWaterData->Irrigation.ScheduledAmount = 0.0;

        if (state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSchedDesign) {
            schedRate = GetCurrentScheduleValue(state, state.dataWaterData->Irrigation.IrrSchedID); // m/hr
            state.dataWaterData->Irrigation.ScheduledAmount =
                schedRate * (TimeStepSys * DataGlobalConstants::SecInHour) / DataGlobalConstants::SecInHour; // convert to m/timestep

        } else if (state.dataWaterData->Irrigation.ModeID == DataWater::IrrigationMode::IrrSmartSched) {
            schedRate = GetCurrentScheduleValue(state, state.dataWaterData->Irrigation.IrrSchedID); // m/hr
            state.dataWaterData->Irrigation.ScheduledAmount =
                schedRate * (TimeStepSys * DataGlobalConstants::SecInHour) / DataGlobalConstants::SecInHour; // convert to m/timestep
        }
    }

    void CalcWaterStorageTank(EnergyPlusData &state, int const TankNum) // Index of storage tank
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Collect the calculations used to update the modeled values
        // for the storage tanks at each system timestep

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OrigVdotDemandRequest(0.0);
        Real64 TotVdotDemandAvail(0.0);
        Real64 OrigVolDemandRequest(0.0);
        Real64 TotVolDemandAvail(0.0);
        Real64 OrigVdotSupplyAvail(0.0);
        Real64 TotVdotSupplyAvail(0.0);
        Real64 TotVolSupplyAvail(0.0);
        Real64 overflowVdot(0.0);
        Real64 overflowVol(0.0);
        Real64 NetVdotAdd(0.0);
        Real64 NetVolAdd(0.0);
        Real64 FillVolRequest(0.0);
        Real64 TotVolAllowed(0.0);
        Real64 underflowVdot(0.0);
        Real64 VolumePredict(0.0);

        if (state.dataGlobal->BeginTimeStepFlag) {
            // initializations are done in UpdateWaterManager
        }

        overflowVdot = 0.0;
        if (state.dataWaterData->WaterStorage(TankNum).NumWaterSupplies > 0) {
            OrigVdotSupplyAvail = sum(state.dataWaterData->WaterStorage(TankNum).VdotAvailSupply);
        } else {
            OrigVdotSupplyAvail = 0.0;
        }
        TotVdotSupplyAvail = OrigVdotSupplyAvail; // Init
        if (TotVdotSupplyAvail > state.dataWaterData->WaterStorage(TankNum).MaxInFlowRate) {
            // pipe/filter rate constraints on inlet
            overflowVdot = TotVdotSupplyAvail - state.dataWaterData->WaterStorage(TankNum).MaxInFlowRate;
            state.dataWaterManager->overflowTwater =
                sum(state.dataWaterData->WaterStorage(TankNum).VdotAvailSupply * state.dataWaterData->WaterStorage(TankNum).TwaterSupply) /
                sum(state.dataWaterData->WaterStorage(TankNum).VdotAvailSupply);
            TotVdotSupplyAvail = state.dataWaterData->WaterStorage(TankNum).MaxInFlowRate;
        }
        TotVolSupplyAvail = TotVdotSupplyAvail * TimeStepSys * DataGlobalConstants::SecInHour;
        overflowVol = overflowVdot * TimeStepSys * DataGlobalConstants::SecInHour;

        underflowVdot = 0.0;
        if (state.dataWaterData->WaterStorage(TankNum).NumWaterDemands > 0) {
            OrigVdotDemandRequest = sum(state.dataWaterData->WaterStorage(TankNum).VdotRequestDemand);
        } else {
            OrigVdotDemandRequest = 0.0;
        }
        OrigVolDemandRequest = OrigVdotDemandRequest * TimeStepSys * DataGlobalConstants::SecInHour;
        TotVdotDemandAvail = OrigVdotDemandRequest; // initialize to satisfied then modify if needed
        if (TotVdotDemandAvail > state.dataWaterData->WaterStorage(TankNum).MaxOutFlowRate) {
            // pipe/filter rate constraints on outlet
            underflowVdot = OrigVdotDemandRequest - state.dataWaterData->WaterStorage(TankNum).MaxOutFlowRate;
            TotVdotDemandAvail = state.dataWaterData->WaterStorage(TankNum).MaxOutFlowRate;
        }
        TotVolDemandAvail = TotVdotDemandAvail * (TimeStepSys * DataGlobalConstants::SecInHour);

        NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail;
        NetVolAdd = NetVdotAdd * (TimeStepSys * DataGlobalConstants::SecInHour);

        VolumePredict = state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume + NetVolAdd;

        // would tank capacity be exceeded?
        TotVolAllowed = state.dataWaterData->WaterStorage(TankNum).MaxCapacity - state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume;
        if (VolumePredict > state.dataWaterData->WaterStorage(TankNum).MaxCapacity) { // too much
            // added overflow to inlet rate limit, new temperature model
            Real64 OverFillVolume = (VolumePredict - state.dataWaterData->WaterStorage(TankNum).MaxCapacity);
            state.dataWaterManager->overflowTwater =
                (state.dataWaterManager->overflowTwater * overflowVol + OverFillVolume * state.dataWaterData->WaterStorage(TankNum).Twater) /
                (overflowVol + OverFillVolume);
            overflowVol += OverFillVolume;
            NetVolAdd -= OverFillVolume;
            NetVdotAdd = NetVolAdd / (TimeStepSys * DataGlobalConstants::SecInHour);
            VolumePredict = state.dataWaterData->WaterStorage(TankNum).MaxCapacity;
        }

        // Is tank too low to meet the request?
        if (VolumePredict < 0.0) {
            Real64 AvailVolume = state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume + TotVolSupplyAvail;
            AvailVolume = max(0.0, AvailVolume);
            TotVolDemandAvail = AvailVolume;
            TotVdotDemandAvail = AvailVolume / (TimeStepSys * DataGlobalConstants::SecInHour);
            underflowVdot = OrigVdotDemandRequest - TotVdotDemandAvail;
            NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail;
            NetVolAdd = NetVdotAdd * (TimeStepSys * DataGlobalConstants::SecInHour);
            VolumePredict = 0.0;
        }

        if (TotVdotDemandAvail < OrigVdotDemandRequest) { // starvation
            // even distribution
            if (OrigVdotDemandRequest > 0.0) {
                state.dataWaterData->WaterStorage(TankNum).VdotAvailDemand =
                    (TotVdotDemandAvail / OrigVdotDemandRequest) * state.dataWaterData->WaterStorage(TankNum).VdotRequestDemand;
            } else {
                state.dataWaterData->WaterStorage(TankNum).VdotAvailDemand = 0.0;
            }
        } else { // requested demand can be served
            if (state.dataWaterData->WaterStorage(TankNum).NumWaterDemands > 0) {
                state.dataWaterData->WaterStorage(TankNum).VdotAvailDemand = state.dataWaterData->WaterStorage(TankNum).VdotRequestDemand;
            }
        }

        // is tank lower than float valve on capacity and requesting fill from controlled supplier?
        FillVolRequest = 0.0;

        if (((VolumePredict) < state.dataWaterData->WaterStorage(TankNum).ValveOnCapacity) ||
            state.dataWaterData->WaterStorage(TankNum).LastTimeStepFilling) { // turn on supply to fill tank
            FillVolRequest = state.dataWaterData->WaterStorage(TankNum).ValveOffCapacity - VolumePredict;

            state.dataWaterData->WaterStorage(TankNum).LastTimeStepFilling = true;

            // set mains draws for float on (all the way to Float off)
            if (state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::MainsFloatValve) {

                state.dataWaterData->WaterStorage(TankNum).MainsDrawVdot = FillVolRequest / (TimeStepSys * DataGlobalConstants::SecInHour);
                NetVolAdd = FillVolRequest;
            }
            // set demand request in supplying tank if needed
            if ((state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::OtherTankFloatValve) ||
                (state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::TankMainsBackup)) {
                state.dataWaterData->WaterStorage(state.dataWaterData->WaterStorage(TankNum).SupplyTankID)
                    .VdotRequestDemand(state.dataWaterData->WaterStorage(TankNum).SupplyTankDemandARRID) =
                    FillVolRequest / (TimeStepSys * DataGlobalConstants::SecInHour);
            }

            // set demand request in groundwater well if needed
            if ((state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::WellFloatValve) ||
                (state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::WellFloatMainsBackup)) {
                state.dataWaterData->GroundwaterWell(state.dataWaterData->WaterStorage(TankNum).GroundWellID).VdotRequest =
                    FillVolRequest / (TimeStepSys * DataGlobalConstants::SecInHour);
            }
        }

        // set mains flow if mains backup active
        if ((VolumePredict) < state.dataWaterData->WaterStorage(TankNum).BackupMainsCapacity) { // turn on supply
            if ((state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::WellFloatMainsBackup) ||
                (state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::TankMainsBackup)) {
                FillVolRequest = state.dataWaterData->WaterStorage(TankNum).ValveOffCapacity - VolumePredict;
                state.dataWaterData->WaterStorage(TankNum).MainsDrawVdot = FillVolRequest / (TimeStepSys * DataGlobalConstants::SecInHour);
                NetVolAdd = FillVolRequest;
            }
        }

        state.dataWaterData->WaterStorage(TankNum).ThisTimeStepVolume = state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume + NetVolAdd;
        if (state.dataWaterData->WaterStorage(TankNum).ThisTimeStepVolume >= state.dataWaterData->WaterStorage(TankNum).ValveOffCapacity) {
            state.dataWaterData->WaterStorage(TankNum).LastTimeStepFilling = false;
        }

        state.dataWaterData->WaterStorage(TankNum).VdotOverflow = overflowVol / (TimeStepSys * DataGlobalConstants::SecInHour);
        state.dataWaterData->WaterStorage(TankNum).VolOverflow = overflowVol;
        state.dataWaterData->WaterStorage(TankNum).TwaterOverflow = state.dataWaterManager->overflowTwater;
        state.dataWaterData->WaterStorage(TankNum).NetVdot = NetVolAdd / (TimeStepSys * DataGlobalConstants::SecInHour);
        state.dataWaterData->WaterStorage(TankNum).MainsDrawVol =
            state.dataWaterData->WaterStorage(TankNum).MainsDrawVdot * (TimeStepSys * DataGlobalConstants::SecInHour);
        state.dataWaterData->WaterStorage(TankNum).VdotToTank = TotVdotSupplyAvail;
        state.dataWaterData->WaterStorage(TankNum).VdotFromTank = TotVdotDemandAvail;

        switch (state.dataWaterData->WaterStorage(TankNum).ThermalMode) {
        case DataWater::TankThermalMode::Scheduled: {
            state.dataWaterData->WaterStorage(TankNum).Twater =
                GetCurrentScheduleValue(state, state.dataWaterData->WaterStorage(TankNum).TempSchedID);
            state.dataWaterData->WaterStorage(TankNum).TouterSkin = state.dataWaterData->WaterStorage(TankNum).Twater;
        } break;
        case DataWater::TankThermalMode::ZoneCoupled: {
            ShowFatalError(state, "WaterUse:Storage (Water Storage Tank) zone thermal model incomplete");
        } break;
        default:
            break;
        }

        // set supply avail data from overflows in Receiving tank
        if (state.dataWaterData->WaterStorage(TankNum).OverflowMode == DataWater::Overflow::ToTank) {
            state.dataWaterData->WaterStorage(state.dataWaterData->WaterStorage(TankNum).OverflowTankID)
                .VdotAvailSupply(state.dataWaterData->WaterStorage(TankNum).OverflowTankSupplyARRID) =
                state.dataWaterData->WaterStorage(TankNum).VdotOverflow;
            state.dataWaterData->WaterStorage(state.dataWaterData->WaterStorage(TankNum).OverflowTankID)
                .TwaterSupply(state.dataWaterData->WaterStorage(TankNum).OverflowTankSupplyARRID) =
                state.dataWaterData->WaterStorage(TankNum).TwaterOverflow;
        }
    }

    void SetupTankSupplyComponent(EnergyPlusData &state,
                                  std::string_view CompName,
                                  std::string_view CompType,
                                  std::string_view TankName,
                                  bool &ErrorsFound,
                                  int &TankIndex,
                                  int &WaterSupplyIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Each simulated component that can supply water to a tank
        // makes one call to this subroutine to obtain the data
        // array index it should use to set values in the
        // VdotAvailSupply

        // METHODOLOGY EMPLOYED:
        // push the VdotAvailToTank array and return

        if (!(state.dataWaterData->WaterSystemGetInputCalled)) {
            GetWaterManagerInput(state);
        }

        InternalSetupTankSupplyComponent(state, CompName, CompType, TankName, ErrorsFound, TankIndex, WaterSupplyIndex);
    }

    void InternalSetupTankSupplyComponent(EnergyPlusData &state,
                                          std::string_view CompName,
                                          std::string_view CompType,
                                          std::string_view TankName,
                                          bool &ErrorsFound,
                                          int &TankIndex,
                                          int &WaterSupplyIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Each simulated component that can supply water to a tank
        // makes one call to this subroutine to obtain the data
        // array index it should use to set values in the
        // VdotAvailSupply

        // METHODOLOGY EMPLOYED:
        // push the VdotAvailToTank array and return

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int oldNumSupply;
        Array1D_string oldSupplyCompNames;
        Array1D_string oldSupplyCompTypes;

        TankIndex = UtilityRoutines::FindItemInList(TankName, state.dataWaterData->WaterStorage);
        if (TankIndex == 0) {
            ShowSevereError(state,
                            "WaterUse:Storage (Water Storage Tank) =\"" + std::string{TankName} + "\" not found in " + std::string{CompType} +
                                " called " + std::string{CompName});
            ErrorsFound = true;
            return; // So we don't pass TankIndex=0
        }
        oldNumSupply = state.dataWaterData->WaterStorage(TankIndex).NumWaterSupplies;
        if (oldNumSupply > 0) { // do array push
            if (allocated(oldSupplyCompNames)) oldSupplyCompNames.deallocate();
            oldSupplyCompNames.allocate(oldNumSupply);
            if (allocated(oldSupplyCompTypes)) oldSupplyCompTypes.deallocate();
            oldSupplyCompTypes.allocate(oldNumSupply);
            if (allocated(state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames)) {
                oldSupplyCompNames = state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames;
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames.deallocate();
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames.allocate(oldNumSupply + 1);
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames({1, oldNumSupply}) = oldSupplyCompNames; // array assignment
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames(oldNumSupply + 1) = CompName;
            }
            if (allocated(state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes)) {
                oldSupplyCompTypes = state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes;
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes.deallocate();
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes.allocate(oldNumSupply + 1);
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes({1, oldNumSupply}) = oldSupplyCompTypes; // array assignment
                state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes(oldNumSupply + 1) = CompType;
            }
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailSupply.deallocate();
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailSupply.allocate(oldNumSupply + 1);
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailSupply = 0.0; // initialize
            state.dataWaterData->WaterStorage(TankIndex).TwaterSupply.deallocate();
            state.dataWaterData->WaterStorage(TankIndex).TwaterSupply.allocate(oldNumSupply + 1);
            state.dataWaterData->WaterStorage(TankIndex).TwaterSupply = 0.0; // initialize
            WaterSupplyIndex = oldNumSupply + 1;
            ++state.dataWaterData->WaterStorage(TankIndex).NumWaterSupplies;
        } else { // first time (no push)

            state.dataWaterData->WaterStorage(TankIndex).VdotAvailSupply.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailSupply = 0.0; // initialize
            state.dataWaterData->WaterStorage(TankIndex).TwaterSupply.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).TwaterSupply = 0.0; // initialize
            state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).SupplyCompNames(1) = CompName;
            state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).SupplyCompTypes(1) = CompType;
            WaterSupplyIndex = 1;
            state.dataWaterData->WaterStorage(TankIndex).NumWaterSupplies = 1;
        }
    }

    void SetupTankDemandComponent(EnergyPlusData &state,
                                  std::string_view CompName,
                                  std::string_view const CompType,
                                  std::string_view TankName,
                                  bool &ErrorsFound,
                                  int &TankIndex,
                                  int &WaterDemandIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Each simulated component that can supply water to a tank
        // makes one call to this subroutine to obtain the data
        // array index it should use to set values in the
        // VdotAvailSupply

        // METHODOLOGY EMPLOYED:
        // push the VdotAvailToTank array and return

        if (!(state.dataWaterData->WaterSystemGetInputCalled)) {
            GetWaterManagerInput(state);
        }

        InternalSetupTankDemandComponent(state, CompName, CompType, TankName, ErrorsFound, TankIndex, WaterDemandIndex);
    }

    void InternalSetupTankDemandComponent(EnergyPlusData &state,
                                          std::string_view CompName,
                                          std::string_view const CompType,
                                          std::string_view TankName,
                                          bool &ErrorsFound,
                                          int &TankIndex,
                                          int &WaterDemandIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Each simulated component that can supply water to a tank
        // makes one call to this subroutine to obtain the data
        // array index it should use to set values in the
        // VdotAvailSupply

        // METHODOLOGY EMPLOYED:
        // push the VdotAvailToTank array and return

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int oldNumDemand;
        Array1D_string oldDemandCompNames;
        Array1D_string oldDemandCompTypes;

        TankIndex = UtilityRoutines::FindItemInList(TankName, state.dataWaterData->WaterStorage);
        if (TankIndex == 0) {
            ShowSevereError(state,
                            "WaterUse:Storage (Water Storage Tank) =\"" + std::string{TankName} + "\" not found in " + std::string{CompType} +
                                " called " + std::string{CompName});
            ErrorsFound = true;
            return;
        }
        oldNumDemand = state.dataWaterData->WaterStorage(TankIndex).NumWaterDemands;
        if (oldNumDemand > 0) { // do array push
            if (allocated(oldDemandCompNames)) oldDemandCompNames.deallocate();
            oldDemandCompNames.allocate(oldNumDemand);
            if (allocated(oldDemandCompTypes)) oldDemandCompTypes.deallocate();
            oldDemandCompTypes.allocate(oldNumDemand);
            if (allocated(state.dataWaterData->WaterStorage(TankIndex).DemandCompNames)) {
                oldDemandCompNames = state.dataWaterData->WaterStorage(TankIndex).DemandCompNames;
                state.dataWaterData->WaterStorage(TankIndex).DemandCompNames.deallocate();
                state.dataWaterData->WaterStorage(TankIndex).DemandCompNames.allocate(oldNumDemand + 1);
                state.dataWaterData->WaterStorage(TankIndex).DemandCompNames({1, oldNumDemand}) = oldDemandCompNames; // array assignment
                state.dataWaterData->WaterStorage(TankIndex).DemandCompNames(oldNumDemand + 1) = CompName;
            }
            if (allocated(state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes)) {
                oldDemandCompTypes = state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes;
                state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes.deallocate();
                state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes.allocate(oldNumDemand + 1);
                state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes({1, oldNumDemand}) = oldDemandCompTypes; // array assignment
                state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes(oldNumDemand + 1) = CompType;
            }

            state.dataWaterData->WaterStorage(TankIndex).VdotRequestDemand.deallocate();
            state.dataWaterData->WaterStorage(TankIndex).VdotRequestDemand.allocate(oldNumDemand + 1);
            state.dataWaterData->WaterStorage(TankIndex).VdotRequestDemand = 0.0; // initialize

            state.dataWaterData->WaterStorage(TankIndex).VdotAvailDemand.deallocate();
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailDemand.allocate(oldNumDemand + 1);
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailDemand = 0.0; // initialize

            WaterDemandIndex = oldNumDemand + 1;
            ++state.dataWaterData->WaterStorage(TankIndex).NumWaterDemands;
        } else { // first time (no push)

            state.dataWaterData->WaterStorage(TankIndex).VdotRequestDemand.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).VdotRequestDemand = 0.0; // initialize
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailDemand.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).VdotAvailDemand = 0.0; // initialize
            state.dataWaterData->WaterStorage(TankIndex).DemandCompNames.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).DemandCompNames(1) = CompName;
            state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes.allocate(1);
            state.dataWaterData->WaterStorage(TankIndex).DemandCompTypes(1) = CompType;
            state.dataWaterData->WaterStorage(TankIndex).NumWaterDemands = 1;
            WaterDemandIndex = 1;
        }
    }

    void CalcRainCollector(EnergyPlusData &state, int const RainColNum) // Index of rain collector
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Collect the calculations used to update the modeled values
        // for the rain collector at each system timestep

        using DataEnvironment::OutWetBulbTempAt;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LossFactor(0.0);
        Real64 VdotAvail;

        // If (.NOT.(IsRain)) Then ! is it raining now? No don't use this flag since precip schedule might differ from weather file
        if (state.dataWaterData->RainFall.CurrentRate <= 0.0) {
            // set available supply rate in WaterStorage
            state.dataWaterData->WaterStorage(state.dataWaterData->RainCollector(RainColNum).StorageTankID)
                .VdotAvailSupply(state.dataWaterData->RainCollector(RainColNum).StorageTankSupplyARRID) = 0.0;
            // temperature of water supply is modeled as the same as outdoor drybulb.
            state.dataWaterData->WaterStorage(state.dataWaterData->RainCollector(RainColNum).StorageTankID)
                .TwaterSupply(state.dataWaterData->RainCollector(RainColNum).StorageTankSupplyARRID) = 0.0;

            state.dataWaterData->RainCollector(RainColNum).VdotAvail = 0.0;
            state.dataWaterData->RainCollector(RainColNum).VolCollected = 0.0;
        } else {

            switch (state.dataWaterData->RainCollector(RainColNum).LossFactorMode) {
            case DataWater::RainLossFactor::Constant: {
                LossFactor = state.dataWaterData->RainCollector(RainColNum).LossFactor;
            } break;
            case DataWater::RainLossFactor::Scheduled: {
                LossFactor = GetCurrentScheduleValue(state, state.dataWaterData->RainCollector(RainColNum).LossFactorSchedID);
            } break;
            default: {
                assert(false);
            } break;
            }

            VdotAvail = state.dataWaterData->RainFall.CurrentRate * state.dataWaterData->RainCollector(RainColNum).HorizArea * (1.0 - LossFactor);

            int month = state.dataEnvrn->Month;

            if (VdotAvail > state.dataWaterData->RainCollector(RainColNum).MaxCollectRate) {
                VdotAvail = state.dataWaterData->RainCollector(RainColNum).MaxCollectRate;
            }

            // set available supply rate in WaterStorage
            state.dataWaterData->WaterStorage(state.dataWaterData->RainCollector(RainColNum).StorageTankID)
                .VdotAvailSupply(state.dataWaterData->RainCollector(RainColNum).StorageTankSupplyARRID) = VdotAvail;

            // temperature of water supply is modeled as the same as outdoor drybulb.
            state.dataWaterData->WaterStorage(state.dataWaterData->RainCollector(RainColNum).StorageTankID)
                .TwaterSupply(state.dataWaterData->RainCollector(RainColNum).StorageTankSupplyARRID) =
                OutWetBulbTempAt(state, state.dataWaterData->RainCollector(RainColNum).MeanHeight);

            state.dataWaterData->RainCollector(RainColNum).VdotAvail = VdotAvail;
            state.dataWaterData->RainCollector(RainColNum).VolCollected = VdotAvail * TimeStepSys * DataGlobalConstants::SecInHour;
            if ((state.dataEnvrn->RunPeriodEnvironment) && (!state.dataGlobal->WarmupFlag)) {
                state.dataWaterData->RainCollector(RainColNum).VolCollectedMonthly.at(month - 1) +=
                    state.dataWaterData->RainCollector(RainColNum).VolCollected;
            }
        }
    }

    void CalcGroundwaterWell(EnergyPlusData &state, int const WellNum) // Index of well
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Collect the calculations used to update the modeled values
        // for the groundwater wells at each system timestep

        // METHODOLOGY EMPLOYED:
        // starting simple and ignoring well storage and complex rate restrictions.
        // just uses nominal pump rate and power (assuming well designed well).

        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 VdotDelivered;
        Real64 PumpPower;

        VdotDelivered = 0.0;
        PumpPower = 0.0;
        if (state.dataWaterData->GroundwaterWell(WellNum).VdotRequest > 0.0) {

            if (state.dataWaterData->GroundwaterWell(WellNum).VdotRequest >=
                state.dataWaterData->GroundwaterWell(WellNum).PumpNomVolFlowRate) { // run flat out
                state.dataWaterData->WaterStorage(state.dataWaterData->GroundwaterWell(WellNum).StorageTankID)
                    .VdotAvailSupply(state.dataWaterData->GroundwaterWell(WellNum).StorageTankSupplyARRID) =
                    state.dataWaterData->GroundwaterWell(WellNum).PumpNomVolFlowRate;
                state.dataWaterData->WaterStorage(state.dataWaterData->GroundwaterWell(WellNum).StorageTankID)
                    .TwaterSupply(state.dataWaterData->GroundwaterWell(WellNum).StorageTankSupplyARRID) = state.dataEnvrn->GroundTemp_Deep;
                VdotDelivered = state.dataWaterData->GroundwaterWell(WellNum).PumpNomVolFlowRate;
                PumpPower = state.dataWaterData->GroundwaterWell(WellNum).PumpNomPowerUse;
            }

            // the run at part load to just meet request
            if (state.dataWaterData->GroundwaterWell(WellNum).VdotRequest < state.dataWaterData->GroundwaterWell(WellNum).PumpNomVolFlowRate) {
                state.dataWaterData->WaterStorage(state.dataWaterData->GroundwaterWell(WellNum).StorageTankID)
                    .VdotAvailSupply(state.dataWaterData->GroundwaterWell(WellNum).StorageTankSupplyARRID) =
                    state.dataWaterData->GroundwaterWell(WellNum).VdotRequest;
                state.dataWaterData->WaterStorage(state.dataWaterData->GroundwaterWell(WellNum).StorageTankID)
                    .TwaterSupply(state.dataWaterData->GroundwaterWell(WellNum).StorageTankSupplyARRID) = state.dataEnvrn->GroundTemp_Deep;

                VdotDelivered = state.dataWaterData->GroundwaterWell(WellNum).VdotRequest;
                PumpPower = state.dataWaterData->GroundwaterWell(WellNum).PumpNomPowerUse *
                            state.dataWaterData->GroundwaterWell(WellNum).VdotRequest /
                            state.dataWaterData->GroundwaterWell(WellNum).PumpNomVolFlowRate;
            }
        }

        state.dataWaterData->GroundwaterWell(WellNum).VdotDelivered = VdotDelivered;
        state.dataWaterData->GroundwaterWell(WellNum).VolDelivered = VdotDelivered * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataWaterData->GroundwaterWell(WellNum).PumpPower = PumpPower;
        state.dataWaterData->GroundwaterWell(WellNum).PumpEnergy = PumpPower * TimeStepSys * DataGlobalConstants::SecInHour;
    }

    void UpdateWaterManager(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The water manger is iterating and
        // we need to do the timestep record keeping
        // for tracking state variables.
        //  this routine updates variables
        // that hold the value of the Last Timestep

        // Using/Aliasing
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TankNum;
        int RainColNum;
        int WellNum;

        if (state.dataGlobal->BeginEnvrnFlag && state.dataWaterManager->MyEnvrnFlag) {
            for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {

                state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume = state.dataWaterData->WaterStorage(TankNum).InitialVolume;
                state.dataWaterData->WaterStorage(TankNum).ThisTimeStepVolume = state.dataWaterData->WaterStorage(TankNum).InitialVolume;
            }
            if ((!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation) && state.dataWaterManager->MyTankDemandCheckFlag) {
                if (state.dataWaterData->NumWaterStorageTanks > 0) {
                    for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {
                        if (state.dataWaterData->WaterStorage(TankNum).NumWaterDemands == 0) {
                            ShowWarningError(state, "Found WaterUse:Storage that has nothing connected to draw water from it.");
                            ShowContinueError(state, "Occurs for WaterUse:Storage = " + state.dataWaterData->WaterStorage(TankNum).Name);
                            ShowContinueError(state, "Check that input for water consuming components specifies a water supply tank.");
                        }
                    }
                }
                state.dataWaterManager->MyTankDemandCheckFlag = false;
            }

            state.dataWaterManager->MyEnvrnFlag = false;
            state.dataWaterManager->MyWarmupFlag = true;
        } // end environmental inits
        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataWaterManager->MyEnvrnFlag = true;
        }

        if (state.dataWaterManager->MyWarmupFlag && (!state.dataGlobal->WarmupFlag)) { // do environment inits.  just went out of warmup mode
            for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {
                state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume = state.dataWaterData->WaterStorage(TankNum).InitialVolume;
                state.dataWaterData->WaterStorage(TankNum).ThisTimeStepVolume = state.dataWaterData->WaterStorage(TankNum).InitialVolume;
                state.dataWaterData->WaterStorage(TankNum).LastTimeStepTemp = state.dataWaterData->WaterStorage(TankNum).InitialTankTemp;
            }
            state.dataWaterManager->MyWarmupFlag = false;
        }

        for (TankNum = 1; TankNum <= state.dataWaterData->NumWaterStorageTanks; ++TankNum) {
            // main location for inits for new timestep.
            state.dataWaterData->WaterStorage(TankNum).LastTimeStepVolume = max(state.dataWaterData->WaterStorage(TankNum).ThisTimeStepVolume, 0.0);
            state.dataWaterData->WaterStorage(TankNum).MainsDrawVdot = 0.0;
            state.dataWaterData->WaterStorage(TankNum).MainsDrawVol = 0.0;
            state.dataWaterData->WaterStorage(TankNum).NetVdot = 0.0;
            state.dataWaterData->WaterStorage(TankNum).VdotFromTank = 0.0;
            state.dataWaterData->WaterStorage(TankNum).VdotToTank = 0.0;
            if (state.dataWaterData->WaterStorage(TankNum).NumWaterDemands > 0) {
                // don't reset the requested demand, it is up to the other components to update it themselves
                // WaterStorage( TankNum ).VdotRequestDemand = 0.0;
                // the available demand is calculated here in the calc routine, so its fine to initialize it
                state.dataWaterData->WaterStorage(TankNum).VdotAvailDemand = 0.0;
            }
            state.dataWaterData->WaterStorage(TankNum).VdotOverflow = 0.0;
            if (state.dataWaterData->WaterStorage(TankNum).NumWaterSupplies > 0) {
                // TODO: Figure out what to do with this...the available supply should be updated by the components
                //       This was an issue because the coil supply was being stomped by this assignment to zero, so no tank action was happening
                state.dataWaterData->WaterStorage(TankNum).VdotAvailSupply = 0.0;
            }
            if ((state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::WellFloatValve) ||
                (state.dataWaterData->WaterStorage(TankNum).ControlSupply == DataWater::ControlSupplyType::WellFloatMainsBackup)) {
                if (allocated(state.dataWaterData->GroundwaterWell))
                    state.dataWaterData->GroundwaterWell(state.dataWaterData->WaterStorage(TankNum).GroundWellID).VdotRequest = 0.0;
            }
        } // tank loop

        for (RainColNum = 1; RainColNum <= state.dataWaterData->NumRainCollectors; ++RainColNum) {

            state.dataWaterData->RainCollector(RainColNum).VdotAvail = 0.0;
            state.dataWaterData->RainCollector(RainColNum).VolCollected = 0.0;
        }

        for (WellNum = 1; WellNum <= state.dataWaterData->NumGroundWaterWells; ++WellNum) {
            state.dataWaterData->GroundwaterWell(WellNum).VdotRequest = 0.0;
            state.dataWaterData->GroundwaterWell(WellNum).VdotDelivered = 0.0;
            state.dataWaterData->GroundwaterWell(WellNum).VolDelivered = 0.0;
            state.dataWaterData->GroundwaterWell(WellNum).PumpPower = 0.0;
            state.dataWaterData->GroundwaterWell(WellNum).PumpEnergy = 0.0;
        }
    }

    void ReportRainfall(EnergyPlusData &state)
    {
        constexpr std::array<std::string_view, 12> Months{"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
        for (int i = 0; i < 12; i++) {
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchMonthlyTotalPrecInWeather,
                                                     Months[i],
                                                     state.dataWaterData->RainFall.MonthlyTotalPrecInWeather[i]);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchMonthlyTotalHrRain, Months[i], state.dataWaterData->RainFall.numRainyHoursInWeather[i]);
        }
        // report site:precipitation
        if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign) {
            for (int i = 0; i < 12; i++) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchMonthlyTotalPrecInSitePrec,
                                                         Months[i],
                                                         state.dataWaterData->RainFall.MonthlyTotalPrecInSitePrec[i]);
            }
        }
        // report rain collector
        if (state.dataWaterData->NumWaterStorageTanks > 0) {
            for (int i = 0; i < 12; i++) {
                Real64 accVolCollectedMonthly = 0.0;
                for (int Item = 1; Item <= state.dataWaterData->NumRainCollectors; Item++) {
                    accVolCollectedMonthly += state.dataWaterData->RainCollector(Item).VolCollectedMonthly[i];
                }
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchMonthlyTotalRainCol, Months[i], accVolCollectedMonthly);
            }
        }
        // report eco roof
        for (auto const &c : state.dataConstruction->Construct) {
            if (c.TypeIsEcoRoof) {
                for (int i = 0; i < 12; i++) {
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchMonthlyTotalIrrDep, Months[i], state.dataEcoRoofMgr->MonthlyIrrigation[i]);
                }
                break;
            }
        }
    }

} // namespace WaterManager

} // namespace EnergyPlus
