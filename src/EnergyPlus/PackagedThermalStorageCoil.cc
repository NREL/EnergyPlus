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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedThermalStorageCoil.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WaterThermalTanks.hh>

namespace EnergyPlus::PackagedThermalStorageCoil {

// Module containing the routines dealing with the packaged thermal storage cooling

// MODULE INFORMATION:
//       AUTHOR         Brent Griffith
//       DATE WRITTEN   April 2013
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// encapsulate the data and algorithms for modeling packaged thermals storage cooling coils

// Using/Aliasing
using namespace DataLoopNode;
using namespace Psychrometrics;
using namespace CurveManager;

void SimTESCoil(EnergyPlusData &state,
                std::string_view CompName, // name of the fan coil unit
                int &CompIndex,
                int const FanOpMode, // allows parent object to control fan mode
                int &TESOpMode,
                Optional<Real64 const> PartLoadRatio // part load ratio (for single speed cycling unit)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int TESCoilNum;

    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) {
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag = false;
    }

    if (CompIndex == 0) {
        TESCoilNum = UtilityRoutines::FindItemInList(CompName, state.dataPackagedThermalStorageCoil->TESCoil);
        if (TESCoilNum == 0) {
            ShowFatalError(state, "Thermal Energy Storage Cooling Coil not found=" + std::string{CompName});
        }
        CompIndex = TESCoilNum;
    } else {
        TESCoilNum = CompIndex;
        if (TESCoilNum > state.dataPackagedThermalStorageCoil->NumTESCoils || TESCoilNum < 1) {
            ShowFatalError(state,
                           format("SimTESCoil: Invalid CompIndex passed={}, Number of Thermal Energy Storage Cooling Coil Coils={}, Coil name={}",
                                  TESCoilNum,
                                  state.dataPackagedThermalStorageCoil->NumTESCoils,
                                  CompName));
        }
        if (state.dataPackagedThermalStorageCoil->CheckEquipName(TESCoilNum)) {
            if (!CompName.empty() && CompName != state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name) {
                ShowFatalError(state,
                               format("SimTESCoil: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                                      TESCoilNum,
                                      CompName,
                                      state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name));
            }
            state.dataPackagedThermalStorageCoil->CheckEquipName(TESCoilNum) = false;
        }
    }

    TESOpMode = CoolingOnlyMode;

    InitTESCoil(state, TESCoilNum);

    TESOpMode = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode;
    {
        auto const SELECT_CASE_var(TESOpMode);
        if (SELECT_CASE_var == OffMode) {
            CalcTESCoilOffMode(state, TESCoilNum);
        } else if (SELECT_CASE_var == CoolingOnlyMode) {
            CalcTESCoilCoolingOnlyMode(state, TESCoilNum, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndChargeMode) {
            CalcTESCoilCoolingAndChargeMode(state, TESCoilNum, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
            CalcTESCoilCoolingAndDischargeMode(state, TESCoilNum, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == ChargeOnlyMode) {
            CalcTESCoilChargeOnlyMode(state, TESCoilNum);
        } else if (SELECT_CASE_var == DischargeOnlyMode) {
            CalcTESCoilDischargeOnlyMode(state, TESCoilNum, PartLoadRatio);
        }
    }
}

void GetTESCoilInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using DataHeatBalance::IntGainTypeOf_PackagedTESCoilTank;
    using DataZoneEquipment::FindControlledZoneIndexFromSystemNodeNumberForZone;
    using FluidProperties::CheckFluidPropertyName;
    using FluidProperties::FindGlycol;
    using FluidProperties::GetFluidDensityTemperatureLimits;
    using FluidProperties::GetFluidSpecificHeatTemperatureLimits;
    using GlobalNames::VerifyUniqueCoilName;
    using NodeInputManager::GetOnlySingleNode;
    using ScheduleManager::GetScheduleIndex;
    using WaterManager::SetupTankDemandComponent;
    using WaterManager::SetupTankSupplyComponent;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetTESCoilInput: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int item;                // do loop counter
    int NumAlphas;           // Number of alphas in input
    int NumNumbers;          // Number of numeric items in input
    int IOStatus;            // Input status returned from GetObjectItem
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    Real64 TminRho;
    Real64 TmaxRho;
    Real64 TminCp;
    Real64 TmaxCp;
    int ZoneIndexTrial;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Coil:Cooling:DX:SingleSpeed:ThermalStorage";
    state.dataPackagedThermalStorageCoil->NumTESCoils = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataPackagedThermalStorageCoil->TESCoil.allocate(state.dataPackagedThermalStorageCoil->NumTESCoils);
    state.dataPackagedThermalStorageCoil->CheckEquipName.dimension(state.dataPackagedThermalStorageCoil->NumTESCoils, true);

    for (item = 1; item <= state.dataPackagedThermalStorageCoil->NumTESCoils; ++item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 item,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueCoilName(state, cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

        state.dataPackagedThermalStorageCoil->TESCoil(item).Name = state.dataIPShortCut->cAlphaArgs(1);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).AvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).AvailSchedNum = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).AvailSchedNum == 0) {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            }
        }
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
            if (SELECT_CASE_var == "SCHEDULEDMODES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).ModeControlType = iModeCtrlType::ScheduledOpModes;
            } else if (SELECT_CASE_var == "EMSCONTROLLED") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).ModeControlType = iModeCtrlType::EMSActuatedOpModes;
            } else {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                ShowContinueError(state, "Available choices are ScheduledModes or EMSControlled");
                ErrorsFound = true;
            }
        }
        if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).ModeControlType == iModeCtrlType::ScheduledOpModes) {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(4) + " is blank but a schedule is needed");
                ErrorsFound = true;
            }
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).ControlModeSchedNum = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).ControlModeSchedNum == 0 &&
                state.dataPackagedThermalStorageCoil->TESCoil(item).ModeControlType == iModeCtrlType::ScheduledOpModes) {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
                ErrorsFound = true;
            }
        }
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(5));
            if (SELECT_CASE_var == "ICE") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia = iMedia::IceBased;
            } else if (SELECT_CASE_var == "WATER") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia = iMedia::FluidBased;
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidName = "WATER";
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex = FindGlycol(state, "WATER");
            } else if (SELECT_CASE_var == "USERDEFINEDFLUIDTYPE") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia = iMedia::FluidBased;
            } else {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
                ShowContinueError(state, "Available choices are Ice, Water, or UserDefindedFluidType");
                ErrorsFound = true;
            }
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "USERDEFINEDFLUIDTYPE")) {
            if (!(state.dataIPShortCut->lAlphaFieldBlanks(6))) {
                state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidName = state.dataIPShortCut->cAlphaArgs(6);
                if (CheckFluidPropertyName(state, state.dataIPShortCut->cAlphaArgs(6)) == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name +
                                        "\", missing fluid data");
                    ShowContinueError(state,
                                      "Check that fluid property data have been input for fluid name = " + state.dataIPShortCut->cAlphaArgs(6));
                    ErrorsFound = true;
                } else {
                    state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex = FindGlycol(state, state.dataIPShortCut->cAlphaArgs(6));
                    if (state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name +
                                            "\", invalid fluid data");
                        ShowContinueError(
                            state, "Check that correct fluid property data have been input for fluid name = " + state.dataIPShortCut->cAlphaArgs(6));
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "Storage Type is set to UserDefinedFluidType but no name of fluid was entered.");
                ErrorsFound = true;
            }
        }

        if ((state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::FluidBased) &&
            (!state.dataIPShortCut->lNumericFieldBlanks(1))) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).FluidStorageVolume = state.dataIPShortCut->rNumericArgs(1);
        } else if ((state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::FluidBased) &&
                   (state.dataIPShortCut->lNumericFieldBlanks(1))) {
            ShowSevereError(state,
                            std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
            ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(1) + " cannot be blank for Water storage type");
            ShowContinueError(state, "Enter fluid storage tank volume in m3/s.");
            ErrorsFound = true;
        }

        if ((state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::IceBased) &&
            (!state.dataIPShortCut->lNumericFieldBlanks(2))) {
            if (state.dataIPShortCut->rNumericArgs(2) == DataGlobalConstants::AutoCalculate) {
                state.dataPackagedThermalStorageCoil->TESCoil(item).IceStorageCapacity = state.dataIPShortCut->rNumericArgs(2);
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).IceStorageCapacity =
                    state.dataIPShortCut->rNumericArgs(2) * 1.e+09; // input in giga joules, used as joules internally
            }
        } else if ((state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::IceBased) &&
                   (state.dataIPShortCut->lNumericFieldBlanks(2))) {
            ShowSevereError(state,
                            std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
            ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(2) + " cannot be blank for Ice storage type");
            ShowContinueError(state, "Enter ice storage tank capacity in GJ.");
            ErrorsFound = true;
        }

        state.dataPackagedThermalStorageCoil->TESCoil(item).StorageCapacitySizingFactor = state.dataIPShortCut->rNumericArgs(3);

        state.dataPackagedThermalStorageCoil->TESCoil(item).StorageAmbientNodeNum = GetOnlySingleNode(state,
                                                                                                      state.dataIPShortCut->cAlphaArgs(7),
                                                                                                      ErrorsFound,
                                                                                                      cCurrentModuleObject,
                                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Sensor,
                                                                                                      1,
                                                                                                      ObjectIsNotParent);

        ZoneIndexTrial =
            FindControlledZoneIndexFromSystemNodeNumberForZone(state, state.dataPackagedThermalStorageCoil->TESCoil(item).StorageAmbientNodeNum);
        if (ZoneIndexTrial > 0) { // tank is inside a zone so setup internal gains
            SetupZoneInternalGain(state,
                                  ZoneIndexTrial,
                                  "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                  state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                  IntGainTypeOf_PackagedTESCoilTank,
                                  &state.dataPackagedThermalStorageCoil->TESCoil(item).QdotAmbient);
        }

        state.dataPackagedThermalStorageCoil->TESCoil(item).StorageUA = state.dataIPShortCut->rNumericArgs(4);
        state.dataPackagedThermalStorageCoil->TESCoil(item).RatedFluidTankTemp = state.dataIPShortCut->rNumericArgs(5);
        state.dataPackagedThermalStorageCoil->TESCoil(item).RatedEvapAirVolFlowRate = state.dataIPShortCut->rNumericArgs(6);

        state.dataPackagedThermalStorageCoil->TESCoil(item).EvapAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                    state.dataIPShortCut->cAlphaArgs(8),
                                                                                                    ErrorsFound,
                                                                                                    cCurrentModuleObject,
                                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                                    1,
                                                                                                    ObjectIsNotParent);
        state.dataPackagedThermalStorageCoil->TESCoil(item).EvapAirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                     state.dataIPShortCut->cAlphaArgs(9),
                                                                                                     ErrorsFound,
                                                                                                     cCurrentModuleObject,
                                                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                                                     1,
                                                                                                     ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(8),
                    state.dataIPShortCut->cAlphaArgs(9),
                    "Air Nodes");

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(10));
            if (SELECT_CASE_var == "YES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyModeIsAvailable = true;
            } else if (SELECT_CASE_var == "NO") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyModeIsAvailable = false;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyModeIsAvailable = false;
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(10) + "=\"" + state.dataIPShortCut->cAlphaArgs(10) + "\".");
                ShowContinueError(state, "Available choices are Yes or No.");
                ErrorsFound = true;
            }
        }

        state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyRatedTotCap = state.dataIPShortCut->rNumericArgs(7);
        if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyModeIsAvailable) { // get input data for this mode

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyRatedSHR = state.dataIPShortCut->rNumericArgs(8);
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyRatedCOP = state.dataIPShortCut->rNumericArgs(9);

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFTempCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(11));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(11) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(11) + "=\"" + state.dataIPShortCut->cAlphaArgs(11) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFTempCurve, // Curve index
                                                 {2},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(11));                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFFlowCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(12));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(12) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(12) + "=\"" + state.dataIPShortCut->cAlphaArgs(12) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyCapFFlowCurve, // Curve index
                                                 {1},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(12));                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFTempCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(13));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(13) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(13) + "=\"" + state.dataIPShortCut->cAlphaArgs(13) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFTempCurve, // Curve index
                                                 {2},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(13));                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFFlowCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(14));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(14) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(14) + "=\"" + state.dataIPShortCut->cAlphaArgs(14) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyEIRFFlowCurve, // Curve index
                                                 {1},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(14));                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyPLFFPLRCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(15));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(15) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(15) + "=\"" + state.dataIPShortCut->cAlphaArgs(15) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlyPLFFPLRCurve, // Curve index
                                                 {1},                                                                         // Valid dimensions
                                                 RoutineName,                                                                 // Routine name
                                                 cCurrentModuleObject,                                                        // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                    // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(15));                                 // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFTempCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(16));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(16) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(16) + "=\"" + state.dataIPShortCut->cAlphaArgs(16) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFTempCurve, // Curve index
                                                 {2},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(16));                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFFlowCurve = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(17));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(17) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(17) + "=\"" + state.dataIPShortCut->cAlphaArgs(17) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingOnlySHRFFlowCurve, // Curve index
                                                 {1},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 cCurrentModuleObject,                                                         // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                     // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(17));                                  // Field Name
            }
        }

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(18));
            if (SELECT_CASE_var == "YES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeModeAvailable = true;
            } else if (SELECT_CASE_var == "NO") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeModeAvailable = false;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeModeAvailable = false;
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(18) + "=\"" + state.dataIPShortCut->cAlphaArgs(18) + "\".");
                ShowContinueError(state, "Available choices are Yes or No.");
                ErrorsFound = true;
            }
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeModeAvailable) {

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeRatedTotCap =
                state.dataIPShortCut->rNumericArgs(10); // gross total evaporator cooling capacity [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeRatedTotCapSizingFactor =
                state.dataIPShortCut->rNumericArgs(11); // sizing factor for gross total evaporator [ ]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeRatedChargeCap =
                state.dataIPShortCut->rNumericArgs(12); // net storage charging capacity at rating conditions [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeRatedChargeCapSizingFactor =
                state.dataIPShortCut->rNumericArgs(13); // sizing factor for charging capacity [ ]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeRatedSHR =
                state.dataIPShortCut->rNumericArgs(14); // Sensible heat ratio (sens cap/total cap)  [W/W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingRatedCOP =
                state.dataIPShortCut->rNumericArgs(15); // Coefficient of performance , for cooling [W/W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingRatedCOP =
                state.dataIPShortCut->rNumericArgs(16); // Coefficient of performance , for charging [W/W]

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(19));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(19) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(19) + "=\"" + state.dataIPShortCut->cAlphaArgs(19) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFTempCurve, // Curve index
                    {3},                                                                                      // Valid dimensions
                    RoutineName,                                                                              // Routine name
                    cCurrentModuleObject,                                                                     // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                 // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(19));                                              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(20));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(20)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(20) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(20) + "=\"" + state.dataIPShortCut->cAlphaArgs(20) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve, // Curve index
                    {1},                                                                                      // Valid dimensions
                    RoutineName,                                                                              // Routine name
                    cCurrentModuleObject,                                                                     // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                 // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(20));                                              // Field Name
            }
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(21));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(21)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(21) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(21) + "=\"" + state.dataIPShortCut->cAlphaArgs(21) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve, // Curve index
                    {3},                                                                                      // Valid dimensions
                    RoutineName,                                                                              // Routine name
                    cCurrentModuleObject,                                                                     // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                 // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(21));                                              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(22));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(22)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(22) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(22) + "=\"" + state.dataIPShortCut->cAlphaArgs(22) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve, // Curve index
                    {1},                                                                                      // Valid dimensions
                    RoutineName,                                                                              // Routine name
                    cCurrentModuleObject,                                                                     // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                 // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(22));                                              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(23));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(23)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(23) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(23) + "=\"" + state.dataIPShortCut->cAlphaArgs(23) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve, // Curve index
                    {1},                                                                                     // Valid dimensions
                    RoutineName,                                                                             // Routine name
                    cCurrentModuleObject,                                                                    // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(23));                                             // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(24));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(24)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(24) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(24) + "=\"" + state.dataIPShortCut->cAlphaArgs(24) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFTempCurve, // Curve index
                    {3},                                                                                       // Valid dimensions
                    RoutineName,                                                                               // Routine name
                    cCurrentModuleObject,                                                                      // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                  // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(24));                                               // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(25));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(25)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(25) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(25) + "=\"" + state.dataIPShortCut->cAlphaArgs(25) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve, // Curve index
                    {1},                                                                                          // Valid dimensions
                    RoutineName,                                                                                  // Routine name
                    cCurrentModuleObject,                                                                         // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                     // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(25));                                                  // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(26));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(26)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(26) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(26) + "=\"" + state.dataIPShortCut->cAlphaArgs(26) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFTempCurve, // Curve index
                    {3},                                                                                       // Valid dimensions
                    RoutineName,                                                                               // Routine name
                    cCurrentModuleObject,                                                                      // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                  // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(26));                                               // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(27));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(27)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(27) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(27) + "=\"" + state.dataIPShortCut->cAlphaArgs(27) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve, // Curve index
                    {1},                                                                                       // Valid dimensions
                    RoutineName,                                                                               // Routine name
                    cCurrentModuleObject,                                                                      // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                  // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(27));                                               // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(28));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(28)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(28) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(28) + "=\"" + state.dataIPShortCut->cAlphaArgs(28) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve, // Curve index
                    {1},                                                                                      // Valid dimensions
                    RoutineName,                                                                              // Routine name
                    cCurrentModuleObject,                                                                     // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                 // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(28));                                              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(29));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(29)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(29) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(29) + "=\"" + state.dataIPShortCut->cAlphaArgs(29) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFTempCurve, // Curve index
                                                 {2, 3},                                                   // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(29));              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(30));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(30)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(30) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(30) + "=\"" + state.dataIPShortCut->cAlphaArgs(30) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndChargeSHRFFlowCurve, // Curve index
                                                 {1},                                                      // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(30));              // Field Name
            }

        } // Cooling and Charge Mode available

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(31));
            if (SELECT_CASE_var == "YES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeModeAvailable = true;
            } else if (SELECT_CASE_var == "NO") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeModeAvailable = false;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeModeAvailable = false;
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(31) + "=\"" + state.dataIPShortCut->cAlphaArgs(31) + "\".");
                ShowContinueError(state, "Available choices are Yes or No.");
                ErrorsFound = true;
            }
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeModeAvailable) {

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeRatedTotCap =
                state.dataIPShortCut->rNumericArgs(17); // gross total evaporator cooling capacity  [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeRatedTotCapSizingFactor =
                state.dataIPShortCut->rNumericArgs(18); // sizing factor gross total cooling capacity []
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeRatedDischargeCap =
                state.dataIPShortCut->rNumericArgs(19); // net storage discharging capacity  [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeRatedDischargeCapSizingFactor =
                state.dataIPShortCut->rNumericArgs(20); // sizing factor discharging capacity []
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeRatedSHR =
                state.dataIPShortCut->rNumericArgs(21); // Sensible heat ratio (sens cap/total cap) [W/W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingRatedCOP =
                state.dataIPShortCut->rNumericArgs(22); // Coefficient of performance , for cooling [W/W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingRatedCOP =
                state.dataIPShortCut->rNumericArgs(23); // Coefficient of performance , for charging [W/W]

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(32));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(32)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(32) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(32) + "=\"" + state.dataIPShortCut->cAlphaArgs(32) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve, // Curve index
                    {3},                                                                                         // Valid dimensions
                    RoutineName,                                                                                 // Routine name
                    cCurrentModuleObject,                                                                        // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                    // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(32));                                                 // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(33));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(33)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(33) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(33) + "=\"" + state.dataIPShortCut->cAlphaArgs(33) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve, // Curve index
                    {1},                                                                                         // Valid dimensions
                    RoutineName,                                                                                 // Routine name
                    cCurrentModuleObject,                                                                        // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                    // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(33));                                                 // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(34));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(34)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(34) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(34) + "=\"" + state.dataIPShortCut->cAlphaArgs(34) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve, // Curve index
                    {3},                                                                                         // Valid dimensions
                    RoutineName,                                                                                 // Routine name
                    cCurrentModuleObject,                                                                        // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                    // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(34));                                                 // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(35));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(35)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(35) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(35) + "=\"" + state.dataIPShortCut->cAlphaArgs(35) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve, // Curve index
                    {1},                                                                                         // Valid dimensions
                    RoutineName,                                                                                 // Routine name
                    cCurrentModuleObject,                                                                        // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                    // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(35));                                                 // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(36));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(36)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(36) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(36) + "=\"" + state.dataIPShortCut->cAlphaArgs(36) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve, // Curve index
                    {1},                                                                                        // Valid dimensions
                    RoutineName,                                                                                // Routine name
                    cCurrentModuleObject,                                                                       // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                   // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(36));                                                // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(37));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(37)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(37) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(37) + "=\"" + state.dataIPShortCut->cAlphaArgs(37) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve, // Curve index
                    {3},                                                                                             // Valid dimensions
                    RoutineName,                                                                                     // Routine name
                    cCurrentModuleObject,                                                                            // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                        // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(37));                                                     // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(38));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(38)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(38) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(38) + "=\"" + state.dataIPShortCut->cAlphaArgs(38) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve, // Curve index
                    {1},                                                                                             // Valid dimensions
                    RoutineName,                                                                                     // Routine name
                    cCurrentModuleObject,                                                                            // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                        // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(38));                                                     // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(39));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(39)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(39) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(39) + "=\"" + state.dataIPShortCut->cAlphaArgs(39) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve, // Curve index
                    {1},                                                                                                // Valid dimensions
                    RoutineName,                                                                                        // Routine name
                    cCurrentModuleObject,                                                                               // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                           // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(39));                                                        // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(40));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(40)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(40) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(40) + "=\"" + state.dataIPShortCut->cAlphaArgs(40) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve, // Curve index
                    {3},                                                                                             // Valid dimensions
                    RoutineName,                                                                                     // Routine name
                    cCurrentModuleObject,                                                                            // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                        // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(40));                                                     // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(41));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(41)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(41) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(41) + "=\"" + state.dataIPShortCut->cAlphaArgs(41) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve, // Curve index
                    {1},                                                                                             // Valid dimensions
                    RoutineName,                                                                                     // Routine name
                    cCurrentModuleObject,                                                                            // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                        // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(41));                                                     // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(42));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(42)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(42) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(42) + "=\"" + state.dataIPShortCut->cAlphaArgs(42) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve, // Curve index
                    {1},                                                                                            // Valid dimensions
                    RoutineName,                                                                                    // Routine name
                    cCurrentModuleObject,                                                                           // Object Type
                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                                       // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(42));                                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(43));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(43)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(43) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(43) + "=\"" + state.dataIPShortCut->cAlphaArgs(43) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFTempCurve, // Curve index
                                                 {2, 3},                                                   // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(43));              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(44));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(44)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(44) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(44) + "=\"" + state.dataIPShortCut->cAlphaArgs(44) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).CoolingAndDischargeSHRFFlowCurve, // Curve index
                                                 {1},                                                      // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(44));              // Field Name
            }

        } // cooling and discharge mode available

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(45));
            if (SELECT_CASE_var == "YES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyModeAvailable = true;
            } else if (SELECT_CASE_var == "NO") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyModeAvailable = false;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyModeAvailable = false;
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(45) + "=\"" + state.dataIPShortCut->cAlphaArgs(45) + "\".");
                ShowContinueError(state, "Available choices are Yes or No.");
                ErrorsFound = true;
            }
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyModeAvailable) {

            state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyRatedCapacity =
                state.dataIPShortCut->rNumericArgs(24); // net storage charging capacity at rating conditions [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyRatedCapacitySizingFactor =
                state.dataIPShortCut->rNumericArgs(25); // sizing factor for charging capacity []
            state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyRatedCOP =
                state.dataIPShortCut->rNumericArgs(26); // coefficient of performance at rating conditions [W/W]

            state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(46));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(46)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(46) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(46) + "=\"" + state.dataIPShortCut->cAlphaArgs(46) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingCapFTempCurve, // Curve index
                                                 {2},                                                      // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(46));              // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(47));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(47)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(47) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(47) + "=\"" + state.dataIPShortCut->cAlphaArgs(47) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).ChargeOnlyChargingEIRFTempCurve, // Curve index
                                                 {2},                                                      // Valid dimensions
                                                 RoutineName,                                              // Routine name
                                                 cCurrentModuleObject,                                     // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(47));              // Field Name
            }

        } // Charge only mode available

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(48));
            if (SELECT_CASE_var == "YES") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyModeAvailable = true;
            } else if (SELECT_CASE_var == "NO") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyModeAvailable = false;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyModeAvailable = false;
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(48) + "=\"" + state.dataIPShortCut->cAlphaArgs(48) + "\".");
                ShowContinueError(state, "Available choices are Yes or No.");
                ErrorsFound = true;
            }
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyModeAvailable) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyRatedDischargeCap =
                state.dataIPShortCut->rNumericArgs(27); // gross total evaporator cooling capacity  [W]
            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyRatedDischargeCapSizingFactor =
                state.dataIPShortCut->rNumericArgs(28); // sizing factor for cooling capacity []
            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyRatedSHR =
                state.dataIPShortCut->rNumericArgs(29); // sensible heat ratio (sens cap/total cap)
            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyRatedCOP =
                state.dataIPShortCut->rNumericArgs(30); // coefficient of performance  for discharging [W/W]

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(49));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(49)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(49) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(49) + "=\"" + state.dataIPShortCut->cAlphaArgs(49) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFTempCurve, // Curve index
                                                 {2},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(49));                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(50));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(50)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(50) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(50) + "=\"" + state.dataIPShortCut->cAlphaArgs(50) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyCapFFlowCurve, // Curve index
                                                 {1},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(50));                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(51));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(51)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(51) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(51) + "=\"" + state.dataIPShortCut->cAlphaArgs(51) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFTempCurve, // Curve index
                                                 {2},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(51));                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFFlowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(52));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFFlowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(52)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(52) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(52) + "=\"" + state.dataIPShortCut->cAlphaArgs(52) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyEIRFFlowCurve, // Curve index
                                                 {1},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(52));                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyPLFFPLRCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(53));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyPLFFPLRCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(53)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(53) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(53) + "=\"" + state.dataIPShortCut->cAlphaArgs(53) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlyPLFFPLRCurve, // Curve index
                                                 {1},                                                                           // Valid dimensions
                                                 RoutineName,                                                                   // Routine name
                                                 cCurrentModuleObject,                                                          // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                      // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(53));                                   // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFTempCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(54));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFTempCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(54)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(54) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(54) + "=\"" + state.dataIPShortCut->cAlphaArgs(54) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFTempCurve, // Curve index
                                                 {2, 3},                                                                         // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(54));                                    // Field Name
            }

            state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFFLowCurve =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(55));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFFLowCurve == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(55)) {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(state, "Required " + state.dataIPShortCut->cAlphaFieldNames(55) + "is blank.");
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                    ShowContinueError(
                        state, "Not found " + state.dataIPShortCut->cAlphaFieldNames(55) + "=\"" + state.dataIPShortCut->cAlphaArgs(55) + "\".");
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, any curve with just x as single independent variable
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).DischargeOnlySHRFFLowCurve, // Curve index
                                                 {1},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 cCurrentModuleObject,                                                           // Object Type
                                                 state.dataPackagedThermalStorageCoil->TESCoil(item).Name,                       // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(55));                                    // Field Name
            }

        } // Discharge Only mode available

        state.dataPackagedThermalStorageCoil->TESCoil(item).AncillaryControlsPower = state.dataIPShortCut->rNumericArgs(31);
        state.dataPackagedThermalStorageCoil->TESCoil(item).ColdWeatherMinimumTempLimit = state.dataIPShortCut->rNumericArgs(32);
        state.dataPackagedThermalStorageCoil->TESCoil(item).ColdWeatherAncillaryPower = state.dataIPShortCut->rNumericArgs(33);
        state.dataPackagedThermalStorageCoil->TESCoil(item).CondAirInletNodeNum =
            GetOnlySingleNode(state,
                              state.dataIPShortCut->cAlphaArgs(56),
                              ErrorsFound,
                              cCurrentModuleObject,
                              state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::OutsideAirReference,
                              1,
                              ObjectIsNotParent);
        state.dataPackagedThermalStorageCoil->TESCoil(item).CondAirOutletNodeNum =
            GetOnlySingleNode(state,
                              state.dataIPShortCut->cAlphaArgs(57),
                              ErrorsFound,
                              cCurrentModuleObject,
                              state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::ReliefAir,
                              1,
                              ObjectIsNotParent);

        state.dataPackagedThermalStorageCoil->TESCoil(item).CondenserAirVolumeFlow = state.dataIPShortCut->rNumericArgs(34);
        state.dataPackagedThermalStorageCoil->TESCoil(item).CondenserAirFlowSizingFactor = state.dataIPShortCut->rNumericArgs(35);
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(58));

            if (SELECT_CASE_var == "AIRCOOLED") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CondenserType = AirCooled;
            } else if (SELECT_CASE_var == "EVAPORATIVELYCOOLED") {
                state.dataPackagedThermalStorageCoil->TESCoil(item).CondenserType = EvapCooled;
            } else {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(58) + "=\"" + state.dataIPShortCut->cAlphaArgs(58) + "\".");
                ShowContinueError(state, "Available choices are AirCooled or EvaporativelyCooled.");
                ErrorsFound = true;
            }
        }
        state.dataPackagedThermalStorageCoil->TESCoil(item).EvapCondEffect = state.dataIPShortCut->rNumericArgs(36);
        state.dataPackagedThermalStorageCoil->TESCoil(item).EvapCondPumpElecNomPower = state.dataIPShortCut->rNumericArgs(37);
        state.dataPackagedThermalStorageCoil->TESCoil(item).BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(38);
        state.dataPackagedThermalStorageCoil->TESCoil(item).BasinHeaterSetpointTemp = state.dataIPShortCut->rNumericArgs(39);

        if (state.dataIPShortCut->lAlphaFieldBlanks(59)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).BasinHeaterAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).BasinHeaterAvailSchedNum =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(59));
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).BasinHeaterAvailSchedNum == 0) {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(59) + "=\"" + state.dataIPShortCut->cAlphaArgs(59) + "\".");
                ErrorsFound = true;
            }
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(60)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyMode = iWaterSupply::WaterSupplyFromMains;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(60);
            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyMode = iWaterSupply::WaterSupplyFromTank;
            SetupTankDemandComponent(state,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                     cCurrentModuleObject,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyName,
                                     ErrorsFound,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupTankID,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterTankDemandARRID);
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(61)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateCollectMode = iWaterSys::CondensateDiscarded;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateCollectName = state.dataIPShortCut->cAlphaArgs(61);
            state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateCollectMode = iWaterSys::CondensateToTank;
            SetupTankSupplyComponent(state,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                     cCurrentModuleObject,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateCollectName,
                                     ErrorsFound,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateTankID,
                                     state.dataPackagedThermalStorageCoil->TESCoil(item).CondensateTankSupplyARRID);
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(62)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantInletNodeNum = GetOnlySingleNode(state,
                                                                                                         state.dataIPShortCut->cAlphaArgs(62),
                                                                                                         ErrorsFound,
                                                                                                         cCurrentModuleObject,
                                                                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                                                                         DataLoopNode::NodeFluidType::Water,
                                                                                                         DataLoopNode::NodeConnectionType::Inlet,
                                                                                                         2,
                                                                                                         ObjectIsNotParent);

            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantConnectionAvailable = true;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantConnectionAvailable = false;
        }
        if (!state.dataIPShortCut->lAlphaFieldBlanks(63)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantOutletNodeNum = GetOnlySingleNode(state,
                                                                                                          state.dataIPShortCut->cAlphaArgs(63),
                                                                                                          ErrorsFound,
                                                                                                          cCurrentModuleObject,
                                                                                                          state.dataIPShortCut->cAlphaArgs(1),
                                                                                                          DataLoopNode::NodeFluidType::Water,
                                                                                                          DataLoopNode::NodeConnectionType::Outlet,
                                                                                                          2,
                                                                                                          ObjectIsNotParent);
        } else {
            if (state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantConnectionAvailable) {
                ShowSevereError(
                    state, std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPackagedThermalStorageCoil->TESCoil(item).Name + "\", invalid");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(63) + " cannot be blank.");
                ErrorsFound = true;
            }
        }
        if (state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantConnectionAvailable) {
            TestCompSet(state,
                        cCurrentModuleObject,
                        state.dataIPShortCut->cAlphaArgs(1),
                        state.dataIPShortCut->cAlphaArgs(62),
                        state.dataIPShortCut->cAlphaArgs(63),
                        "Water Nodes");
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(40)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantDesignVolumeFlowRate = state.dataIPShortCut->rNumericArgs(40);
        }
        if (!state.dataIPShortCut->lNumericFieldBlanks(41)) {
            state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantEffectiveness = state.dataIPShortCut->rNumericArgs(41);
        }
        if (state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::FluidBased) {
            if (!state.dataIPShortCut->lNumericFieldBlanks(42)) {
                state.dataPackagedThermalStorageCoil->TESCoil(item).MinimumFluidTankTempLimit = state.dataIPShortCut->rNumericArgs(42);
            } else {

                GetFluidDensityTemperatureLimits(state, state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex, TminRho, TmaxRho);
                GetFluidSpecificHeatTemperatureLimits(state, state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex, TminCp, TmaxCp);
                state.dataPackagedThermalStorageCoil->TESCoil(item).MinimumFluidTankTempLimit = max(TminRho, TminCp);
            }
            if (!state.dataIPShortCut->lNumericFieldBlanks(43)) {
                state.dataPackagedThermalStorageCoil->TESCoil(item).MaximumFluidTankTempLimit = state.dataIPShortCut->rNumericArgs(43);
            } else {
                GetFluidDensityTemperatureLimits(state, state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex, TminRho, TmaxRho);
                GetFluidSpecificHeatTemperatureLimits(state, state.dataPackagedThermalStorageCoil->TESCoil(item).StorageFluidIndex, TminCp, TmaxCp);
                state.dataPackagedThermalStorageCoil->TESCoil(item).MaximumFluidTankTempLimit = min(TmaxRho, TmaxCp);
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition(s) causes termination.");
    }

    // setup reporting
    for (item = 1; item <= state.dataPackagedThermalStorageCoil->NumTESCoils; ++item) {
        SetupOutputVariable(state,
                            "Cooling Coil Operating Mode Index",
                            OutputProcessor::Unit::None,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).CurControlMode,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        // cCurrentModuleObject = "Coil:Cooling:DX:SingleSpeed:ThermalStorage"
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapTotCoolingRate,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapTotCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapSensCoolingRate,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapSensCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapLatCoolingRate,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).EvapLatCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).ElecCoolingPower,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).ElecCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                            _,
                            "Electricity",
                            "COOLING",
                            _,
                            "System");

        SetupOutputVariable(state,
                            "Cooling Coil Runtime Fraction",
                            OutputProcessor::Unit::None,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).RuntimeFraction,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Cold Weather Protection Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).ElectColdWeatherEnergy,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                            _,
                            "Electricity",
                            "COOLING",
                            "Thermal Protection",
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Cold Weather Protection Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).ElectColdWeatherPower,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        SetupOutputVariable(state,
                            "Cooling Coil Thermal Storage Mechanical Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).QdotTES,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        SetupOutputVariable(state,
                            "Cooling Coil Thermal Storage Mechanical Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Q_TES,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        SetupOutputVariable(state,
                            "Cooling Coil Thermal Storage Ambient Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).QdotAmbient,
                            "System",
                            "Average",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        SetupOutputVariable(state,
                            "Cooling Coil Thermal Storage Ambient Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Q_Ambient,
                            "System",
                            "Sum",
                            state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).TESPlantConnectionAvailable) {
            SetupOutputVariable(state,
                                "Cooling Coil Thermal Storage Plant Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).QdotPlant,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Thermal Storage Plant Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Q_Plant,
                                "System",
                                "Sum",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).CondenserType == EvapCooled) {
            SetupOutputVariable(state,
                                "Cooling Coil Condenser Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).CondInletTemp,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

            if (state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyMode == iWaterSupply::WaterSupplyFromMains) {
                SetupOutputVariable(state,
                                    "Cooling Coil Evaporative Condenser Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterConsump,
                                    "System",
                                    "Sum",
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                    _,
                                    "Water",
                                    "Cooling",
                                    _,
                                    "System");
                SetupOutputVariable(state,
                                    "Cooling Coil Evaporative Condenser Mains Supply Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterConsump,
                                    "System",
                                    "Sum",
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                    _,
                                    "MainsWater",
                                    "Cooling",
                                    _,
                                    "System");
            } else if (state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterSupplyMode == iWaterSupply::WaterSupplyFromTank) {
                SetupOutputVariable(state,
                                    "Cooling Coil Evaporative Condenser Storage Tank Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterConsump,
                                    "System",
                                    "Sum",
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                    _,
                                    "Water",
                                    "Cooling",
                                    _,
                                    "System");
                SetupOutputVariable(state,
                                    "Cooling Coil Evaporative Condenser Starved Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterStarvMakup,
                                    "System",
                                    "Sum",
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                    _,
                                    "Water",
                                    "Cooling",
                                    _,
                                    "System");
                SetupOutputVariable(state,
                                    "Cooling Coil Evaporative Condenser Starved Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).EvapWaterStarvMakup,
                                    "System",
                                    "Sum",
                                    state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                    _,
                                    "MainsWater",
                                    "Cooling",
                                    _,
                                    "System");
            }

            SetupOutputVariable(state,
                                "Cooling Coil Evaporative Condenser Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).EvapCondPumpElecPower,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Evaporative Condenser Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).EvapCondPumpElecConsumption,
                                "System",
                                "Sum",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                _,
                                "Electricity",
                                "COOLING",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Cooling Coil Basin Heater Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).ElectEvapCondBasinHeaterPower,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Basin Heater Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).ElectEvapCondBasinHeaterEnergy,
                                "System",
                                "Sum",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                                _,
                                "Electricity",
                                "COOLING",
                                "Thermal Protection",
                                "System");
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::FluidBased) {
            SetupOutputVariable(state,
                                "Cooling Coil Fluid Thermal Storage End Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).FluidTankTempFinal,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);

        } else if (state.dataPackagedThermalStorageCoil->TESCoil(item).StorageMedia == iMedia::IceBased) {
            SetupOutputVariable(state,
                                "Cooling Coil Ice Thermal Storage End Fraction",
                                OutputProcessor::Unit::None,
                                state.dataPackagedThermalStorageCoil->TESCoil(item).IceFracRemain,
                                "System",
                                "Average",
                                state.dataPackagedThermalStorageCoil->TESCoil(item).Name);
        }
    }

    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
        for (item = 1; item <= state.dataPackagedThermalStorageCoil->NumTESCoils; ++item) {
            // setup EMS actuator for control mode
            SetupEMSActuator(state,
                             "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                             state.dataPackagedThermalStorageCoil->TESCoil(item).Name,
                             "Operating Mode",
                             "[ ]",
                             state.dataPackagedThermalStorageCoil->TESCoil(item).EMSControlModeOn,
                             state.dataPackagedThermalStorageCoil->TESCoil(item).EMSControlModeValue);
        }
    }
}

void InitTESCoil(EnergyPlusData &state, int &TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using DataPlant::TypeOf_PackagedTESCoolingCoil;
    using PlantUtilities::ScanPlantLoopsForObject;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &MyFlag = state.dataPackagedThermalStorageCoil->MyFlag;
    auto &MySizeFlag = state.dataPackagedThermalStorageCoil->MySizeFlag;
    auto &MyEnvrnFlag = state.dataPackagedThermalStorageCoil->MyEnvrnFlag;
    auto &MyWarmupFlag = state.dataPackagedThermalStorageCoil->MyWarmupFlag;
    bool errFlag;
    int plloopnum;
    int lsnum;
    int brnum;
    int cpnum;
    Real64 tmpSchedValue;

    if (state.dataPackagedThermalStorageCoil->MyOneTimeFlag) {
        // initialize the environment and sizing flags
        MyFlag.allocate(state.dataPackagedThermalStorageCoil->NumTESCoils);
        MySizeFlag.allocate(state.dataPackagedThermalStorageCoil->NumTESCoils);
        MyEnvrnFlag.allocate(state.dataPackagedThermalStorageCoil->NumTESCoils);
        MyWarmupFlag.allocate(state.dataPackagedThermalStorageCoil->NumTESCoils);
        MyFlag = true;
        MySizeFlag = true;
        MyEnvrnFlag = true;
        state.dataPackagedThermalStorageCoil->MyOneTimeFlag = false;
        MyWarmupFlag = false;
    }

    if (MyFlag(TESCoilNum)) {

        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                    TypeOf_PackagedTESCoolingCoil,
                                    plloopnum,
                                    lsnum,
                                    brnum,
                                    cpnum,
                                    errFlag);

            // double check node names match
            if (errFlag) {
                ShowFatalError(state, "InitTESCoil: Program terminated due to previous condition(s).");
            }
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum = plloopnum;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopSideNum = lsnum;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantBranchNum = brnum;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantCompNum = cpnum;

            if ((state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn !=
                 state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum) ||
                (state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut !=
                 state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantOutletNodeNum)) {
                ShowSevereError(state,
                                "InitTESCoil: Coil:Cooling:DX:SingleSpeed:ThermalStorage =\"" +
                                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name + "\", non-matching plant nodes.");
                ShowContinueError(state,
                                  "...in Branch=\"" +
                                      state.dataPlnt->PlantLoop(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum)
                                          .LoopSide(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopSideNum)
                                          .Branch(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantBranchNum)
                                          .Name +
                                      "\", Component referenced with:");
                ShowContinueError(state,
                                  "...Inlet Node=\"" + state.dataLoopNodes->NodeID(
                                                           state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn));
                ShowContinueError(
                    state,
                    "...Outlet Node=\"" +
                        state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut));
                ShowContinueError(state,
                                  "...TES Inlet Node=\"" +
                                      state.dataLoopNodes->NodeID(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum));
                ShowContinueError(state,
                                  "...TES Outlet Node=\"" +
                                      state.dataLoopNodes->NodeID(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantOutletNodeNum));
                errFlag = true;
            }
            if (errFlag) {
                ShowFatalError(state, "InitTESCoil: Program terminated due to previous condition(s).");
            }

        } // any plant connection to TES
        MyFlag(TESCoilNum) = false;
    }

    if (MySizeFlag(TESCoilNum)) {

        SizeTESCoil(state, TESCoilNum);

        MySizeFlag(TESCoilNum) = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(TESCoilNum)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Plant = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Ambient = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TimeElapsed = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinal =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedFluidTankTemp;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedFluidTankTemp;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower = 0.0;     // electric power for cooling [W]
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy = 0.0;    // electric energy for cooling [J], metered
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;   // evaporator coil total cooling rate [W]
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0; // evaporatory coil total cooling energy [J], metered
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0; // electric power for cold weather protection [W]
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherEnergy =
            0.0; // electric energy for cold weather protection [J], metered
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy = 0.0;

        MyEnvrnFlag(TESCoilNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) MyEnvrnFlag(TESCoilNum) = true;

    if (MyWarmupFlag(TESCoilNum) && (!state.dataGlobal->WarmupFlag)) {
        // reset to initial condition once warm up is over.
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinal =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedFluidTankTemp;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedFluidTankTemp;
        MyWarmupFlag(TESCoilNum) = false;
    }

    if (state.dataGlobal->WarmupFlag) MyWarmupFlag(TESCoilNum) = true;

    // determine control mode
    if (GetCurrentScheduleValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AvailSchedNum) != 0.0) {
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ModeControlType == iModeCtrlType::ScheduledOpModes) {
            tmpSchedValue = GetCurrentScheduleValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ControlModeSchedNum);
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = tmpSchedValue;
            // check if value is valid
            {
                auto const SELECT_CASE_var(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode);
                if ((SELECT_CASE_var == OffMode) || (SELECT_CASE_var == CoolingOnlyMode) || (SELECT_CASE_var == CoolingAndChargeMode) ||
                    (SELECT_CASE_var == CoolingAndDischargeMode) || (SELECT_CASE_var == ChargeOnlyMode) || (SELECT_CASE_var == DischargeOnlyMode)) {
                    // do nothing, these are okay
                } else {
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ControlModeErrorIndex == 0) {
                        ShowSevereMessage(state, "InitTESCoil: Invalid control schedule value for operating mode");
                        ShowContinueError(state,
                                          "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                              state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                        ShowContinueError(state, format("Value returned from schedule ={:.8R}", tmpSchedValue));
                        ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                    }
                    ShowRecurringSevereErrorAtEnd(state,
                                                  "InitTESCoil: Invalid control schedule value for TES operating mode, set to Off",
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ControlModeErrorIndex,
                                                  tmpSchedValue,
                                                  tmpSchedValue);
                }
            }

        } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ModeControlType == iModeCtrlType::EMSActuatedOpModes) {
            if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EMSControlModeOn) {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode =
                    std::floor(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EMSControlModeValue);
                // check if value is valid
                {
                    auto const SELECT_CASE_var(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode);
                    if (SELECT_CASE_var == OffMode) {

                    } else if (SELECT_CASE_var == CoolingOnlyMode) {
                        if (!(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyModeIsAvailable)) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state, "Value returned from EMS indicates Cooling Only Mode but that mode is not available.");
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        }
                    } else if (SELECT_CASE_var == CoolingAndChargeMode) {
                        if (!(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeModeAvailable)) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state, "Value returned from EMS indicates Cooling And Charge Mode but that mode is not available.");
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        }
                    } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
                        if (!(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable)) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state, "Value returned from EMS indicates Cooling And Discharge Mode but that mode is not available.");
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        }
                    } else if (SELECT_CASE_var == ChargeOnlyMode) {
                        if (!(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyModeAvailable)) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state, "Value returned from EMS indicates Charge Only Mode but that mode is not available.");
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        }
                    } else if (SELECT_CASE_var == DischargeOnlyMode) {
                        if (!(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyModeAvailable)) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state, "Value returned from EMS indicates Discharge Only Mode but that mode is not available.");
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        }
                    } else {
                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
                        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ControlModeErrorIndex == 0) {
                            ShowSevereMessage(state, "InitTESCoil: Invalid control value for operating mode");
                            ShowContinueError(state,
                                              "Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " +
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
                            ShowContinueError(state,
                                              format("Value returned from EMS ={:.8R}",
                                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EMSControlModeValue));
                            ShowContinueError(state, "Operating mode will be set to Off, and the simulation continues");
                        }
                        ShowRecurringSevereErrorAtEnd(state,
                                                      "InitTESCoil: Invalid control schedule value for TES operating mode, set to Off",
                                                      state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ControlModeErrorIndex,
                                                      state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EMSControlModeValue,
                                                      state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EMSControlModeValue);
                    }
                }
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
            }
        }
    } else {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CurControlMode = OffMode;
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant = 0.0; // heat exchange rate for plant connection to TES tank [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Plant = 0.0;   //  heat exchange energy for plant connection to TES tank [J]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient =
        0.0; // heat exchange rate for skin losses/gains for TES tank to surroundings [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Ambient =
        0.0; // heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES =
        0.0; // heat exchange rate by mechanical systems to charge or discharge TES [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES =
        0.0; // heat exchange energy by mechanical systems to charge or discharge TES [J]

    // dynamic calculated data
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower = 0.0;     // electric power for cooling [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy = 0.0;    // electric energy for cooling [J], metered
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;   // evaporator coil total cooling rate [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0; // evaporatory coil total cooling energy [J], metered
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0; // electric power for cold weather protection [W]
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherEnergy =
        0.0; // electric energy for cold weather protection [J], metered
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy = 0.0;
}

void SizeTESCoil(EnergyPlusData &state, int &TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using namespace DataSizing;
    using namespace OutputReportPredefined;
    using CurveManager::CurveValue;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeTESCoil ");
    static constexpr std::string_view calcTESWaterStorageTank("CalcTESWaterStorageTank");
    Real64 const FluidTankSizingDeltaT(10.0);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MixTemp;
    Real64 MixHumRat;
    Real64 MixEnth;
    Real64 MixWetBulb;
    Real64 SupTemp;
    Real64 SupHumRat;
    Real64 SupEnth;
    Real64 OutTemp;
    Real64 OutAirFrac;
    Real64 VolFlowRate;
    Real64 CoolCapAtPeak;
    Real64 TotCapTempModFac;
    int TimeStepNumAtMax;
    int DDNum;
    Real64 rhoair;
    Real64 rho;
    Real64 deltaT;
    Real64 Cp;

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate == AutoSize) {

        if (state.dataSize->CurSysNum > 0) {
            CheckSysSizing(state, "Coil:Cooling:DX:SingleSpeed:ThermalStorage", state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
            if (state.dataSize->CurOASysNum > 0) {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate =
                    state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow;
            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate =
                    state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
            }
        } else if (state.dataSize->CurZoneEqNum > 0) {
            CheckZoneSizing(state, "Coil:Cooling:DX:SingleSpeed:ThermalStorage", state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate =
                max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                    state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);
        }

        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate < SmallAirVolFlow) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate = 0.0;
        }
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Rated Evaporator Air Flow Rate [m3/s]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate);
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate =
        state.dataEnvrn->StdRhoAir * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirVolumeFlow == DataGlobalConstants::AutoCalculate) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirVolumeFlow =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirFlowSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Condenser Air Flow Rate [m3/s]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirVolumeFlow);
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow =
        state.dataEnvrn->StdRhoAir * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirVolumeFlow;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap == AutoSize) {
        if (state.dataSize->CurSysNum > 0) {
            CheckSysSizing(state, "Coil:Cooling:DX:SingleSpeed:ThermalStorage", state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
            VolFlowRate = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;
            if (VolFlowRate >= SmallAirVolFlow) {
                if (state.dataSize->CurOASysNum > 0) { // coil is in the OA stream
                    MixTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutTempAtCoolPeak;
                    MixHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutHumRatAtCoolPeak;
                    SupTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolTemp;
                    SupHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolHumRat;
                } else { // coil is on the main air loop
                    //     MixTemp = FinalSysSizing(CurSysNum)%MixTempAtCoolPeak
                    //     MixHumRat = FinalSysSizing(CurSysNum)%MixHumRatAtCoolPeak
                    SupTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).CoolSupTemp;
                    SupHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).CoolSupHumRat;
                    if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).NumOACoolCoils ==
                        0) { // there is no precooling of the OA stream
                        MixTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).MixTempAtCoolPeak;
                        MixHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).MixHumRatAtCoolPeak;
                    } else { // there is precooling of OA stream
                        if (VolFlowRate > 0.0) {
                            OutAirFrac = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow / VolFlowRate;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                        MixTemp = OutAirFrac * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolTemp +
                                  (1.0 - OutAirFrac) * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).RetTempAtCoolPeak;
                        MixHumRat = OutAirFrac * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolHumRat +
                                    (1.0 - OutAirFrac) * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).RetHumRatAtCoolPeak;
                    }
                }
                OutTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutTempAtCoolPeak;
                rhoair = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                MixWetBulb = PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);
                TotCapTempModFac =
                    CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, MixWetBulb, OutTemp);
                CoolCapAtPeak = max(0.0, (rhoair * VolFlowRate * (MixEnth - SupEnth)));
                if (TotCapTempModFac > 0.0) {
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac;
                } else {
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak;
                }

            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = 0.0;
            }
        } else if (state.dataSize->CurZoneEqNum > 0) {
            CheckZoneSizing(state, "Coil:Cooling:DX:SingleSpeed:ThermalStorage", state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name);
            VolFlowRate = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;
            if (VolFlowRate >= SmallAirVolFlow) {
                if (state.dataSize->ZoneEqDXCoil) {
                    if (ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow > 0.0) {
                        MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                        MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                    } else {
                        MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneRetTempAtCoolPeak;
                        MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtCoolPeak;
                    }
                } else {
                    MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                    MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                }
                SupTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesTemp;
                SupHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesHumRat;
                TimeStepNumAtMax = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).TimeStepNumAtCoolMax;
                DDNum = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDDNum;
                if (DDNum > 0 && TimeStepNumAtMax > 0) {
                    OutTemp = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                } else {
                    OutTemp = 0.0;
                }
                rhoair = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                MixWetBulb = PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);
                TotCapTempModFac =
                    CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, MixWetBulb, OutTemp);
                CoolCapAtPeak = max(0.0, (rhoair * VolFlowRate * (MixEnth - SupEnth)));
                if (TotCapTempModFac > 0.0) {
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac;
                } else {
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak;
                }

            } else {
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = 0.0;
            }
        }

        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Cooling Only Mode Rated Total Evaporator Cooling Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCapSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Cooling And Charge Mode Rated Total Evaporator Cooling Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCapSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Cooling And Charge Mode Rated Storage Charging Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCapSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCapSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Cooling And Discharge Mode Rated Storage Discharging Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCapacity == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCapacity =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCapacitySizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Charge Only Mode Rated Storage Charging Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCapacity);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyModeAvailable &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap == DataGlobalConstants::AutoCalculate)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCapSizingFactor;
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Discharge Only Mode Rated Storage Discharging Capacity [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap);
    }

    if ((state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume == DataGlobalConstants::AutoCalculate)) {
        // for fluid tanks, assume a 10C deltaT or diff between max and min, whichever is smaller
        deltaT = min(FluidTankSizingDeltaT,
                     (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit -
                      state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit));

        rho = GetDensityGlycol(state,
                               state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                               DataGlobalConstants::CWInitConvTemp,
                               state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                               calcTESWaterStorageTank);
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   DataGlobalConstants::CWInitConvTemp,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   calcTESWaterStorageTank);
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap > 0.0 &&
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyModeAvailable) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume =
                (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap *
                 state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageCapacitySizingFactor * DataGlobalConstants::SecInHour) /
                (rho * Cp * deltaT);
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume =
                (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
                 state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageCapacitySizingFactor * DataGlobalConstants::SecInHour) /
                (rho * Cp * deltaT);
        }
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Fluid Storage Volume [m3]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume);
    }
    if ((state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity == DataGlobalConstants::AutoCalculate)) {

        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap > 0.0 &&
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyModeAvailable) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap *
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageCapacitySizingFactor * DataGlobalConstants::SecInHour;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageCapacitySizingFactor * DataGlobalConstants::SecInHour;
        }
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Ice Storage Capacity [GJ]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity / 1.e+09);
    }

    if ((state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) &&
        (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecNomPower == AutoSize)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecNomPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * 0.004266; // w/w (15 w/ton)
        BaseSizer::reportSizerOutput(state,
                                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                                     "Evaporative Condenser Pump Rated Power Consumption [W]",
                                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecNomPower);
    }

    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilType,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     "Coil:Cooling:DX:SingleSpeed:ThermalStorage");

    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilTotCap,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap);
    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilSensCap,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilLatCap,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap -
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap *
                             state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilSHR,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
    PreDefTableEntry(state,
                     state.dataOutRptPredefined->pdchCoolCoilNomEff,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Name,
                     state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedCOP);
}

void CalcTESCoilOffMode(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 StandbyAncillaryPower;

    // coil is off; just pass through conditions
    if (GetCurrentScheduleValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AvailSchedNum) != 0.0) {
        StandbyAncillaryPower = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
    } else {
        StandbyAncillaryPower = 0.0;
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower = StandbyAncillaryPower;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
        StandbyAncillaryPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
        PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                   state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
        PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                   state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES = 0.0;

    UpdateTEStorage(state, TESCoilNum);

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondInletTemp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
    }
}

void CalcTESCoilCoolingOnlyMode(EnergyPlusData &state, int const TESCoilNum, [[maybe_unused]] int const FanOpMode, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using CurveManager::CurveValue;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIter(30);
    Real64 const RelaxationFactor(0.4);
    Real64 const Tolerance(0.1);
    static constexpr std::string_view RoutineName("CalcTESCoilCoolingOnlyMode");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
    // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
    Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
    // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
    Real64 CondAirMassFlow;           // Condenser air mass flow rate [kg/s]
    Real64 CondInletEnthalpy;         // condenser inlet enthalpy [J/kg]
    Real64 CondAirSidePressure;       // Outdoor barometric pressure at condenser (Pa)
    Real64 QdotCond;                  // condenser total heat rejection rate [W]
    Real64 CondOutletEnthalpy;        // condesner outlet enthalpy [J/kg]
    Real64 OutdoorDryBulb;            // outdoor air dry bulb local variable [C]
    Real64 OutdoorHumRat;             // outdoor air humidity ratio local [kg/kg]
    Real64 OutdoorWetBulb;            // outdoor air wetbulb local [C]
    Real64 EvapAirMassFlow;           // local for evaporator air mass flow [kg/s]
    Real64 EvapInletDryBulb;          // evaporator inlet air drybulb [C]
    Real64 EvapInletHumRat;           // evaporator inlet air humidity ratio [kg/kg]
    Real64 EvapInletWetBulb;          // evaporator inlet air wetbulb [C]
    Real64 EvapInletEnthalpy;         // evaporator inlet air enthalpy [J/kg]
    Real64 AirMassFlowRatio;          // evaporator inlet air mass flow divided by design mass flow [ ]
    Real64 TotCapTempModFac;          // total coolin capacity modification factor due to temps []
    Real64 TotCapFlowModFac;          // Total cooling capacity modification factor due to flow []
    Real64 TotCap;                    // total cooling capacity
    Real64 SHRTempFac;                // sensible heat ratio modification factor due to temps []
    Real64 SHRFlowFac;                // sensible heat ratio modification factor due to flow []
    Real64 SHR;                       // sensible heat ratio
    Real64 PLF;                       // part load factor
    Real64 RuntimeFraction;           // compressor running time divided by full time of timestep.
    Real64 FullLoadOutAirEnth;        // evaporator outlet full load enthalpy [J/kg]
    Real64 hTinwout;                  // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
    Real64 FullLoadOutAirHumRat;      // evaporator outlet humidity ratio at full load
    Real64 FullLoadOutAirTemp;        // evaporator outlet air temperature at full load [C]
    Real64 EvapOutletAirEnthalpy;     // evaporator outlet air enthalpy [J/kg]
    Real64 EvapOutletAirHumRat;       // evaporator outlet air humidity ratio [kg/kg]
    Real64 EvapOutletAirTemp;         // evaporator outlet drybulb [C]
    Real64 EIRTempModFac;             // energy input ratio modification factor due to temperatures []
    Real64 EIRFlowModFac;             // energy input ratio modification factor due to flow []
    Real64 EIR;                       // energy input ratio
    Real64 ElecCoolingPower;          // compressor electric power
    Real64 MinAirHumRat;              // minimum air humidity ratio
    Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
    Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
    bool CoilMightBeDry;
    int Counter;
    bool Converged;
    Real64 DryCoilTestEvapInletHumRat;
    Real64 DryCoilTestEvapInletWetBulb;
    Real64 hADP;
    Real64 tADP;
    Real64 wADP;
    Real64 hTinwADP;
    Real64 SHRadp;
    Real64 werror;

    // first deal with condenser
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == AirCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            CondInletTemp = state.dataEnvrn->OutDryBulbTemp;
            CondInletHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
        } else {
            CondInletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            CondInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            OutdoorDryBulb = state.dataEnvrn->OutDryBulbTemp;
            OutdoorHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
            OutdoorWetBulb = state.dataEnvrn->OutWetBulbTemp;
        } else {
            OutdoorDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            OutdoorHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            OutdoorWetBulb = PsyTwbFnTdbWPb(state, OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        // direct evap cool model
        CondInletTemp =
            OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondEffect);
        CondInletHumRat = PsyWFnTdbTwbPb(state, CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
    }

    EvapAirMassFlow = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    EvapInletDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    EvapInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    EvapInletWetBulb = PsyTwbFnTdbWPb(state, EvapInletDryBulb, EvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
    EvapInletEnthalpy = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
    CoilMightBeDry = false;

    if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

        AirMassFlowRatio = EvapAirMassFlow / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
        TotCapTempModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, EvapInletWetBulb, CondInletTemp);
        TotCapTempModFac = max(0.0, TotCapTempModFac); // could warn if negative, DXcoil does
        TotCapFlowModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFFlowCurve, AirMassFlowRatio);
        TotCapFlowModFac = max(0.0, TotCapFlowModFac); // could warn if negative, DXcoil does
        TotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac;
        // now see if coil might be running dry
        PartLoadOutAirEnth = EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow;
        PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
        if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(state, PartLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            CoilMightBeDry = true;
            // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
            DryCoilTestEvapInletHumRat = EvapInletHumRat;
            DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
            Counter = 0;
            Converged = false;
            while (!Converged) {
                TotCapTempModFac = CurveValue(state,
                                              state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve,
                                              DryCoilTestEvapInletWetBulb,
                                              CondInletTemp);
                TotCapTempModFac = max(0.0, TotCapTempModFac); // could warn if negative, DXcoil does
                TotCapFlowModFac =
                    CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyCapFFlowCurve, AirMassFlowRatio);
                TotCapFlowModFac = max(0.0, TotCapFlowModFac); // could warn if negative, DXcoil does
                TotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac;

                // coil bypass factor = 0.0
                hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow);
                tADP = PsyTsatFnHPb(state, hADP, state.dataEnvrn->OutBaroPress, RoutineName);
                wADP = min(EvapInletHumRat, PsyWFnTdbH(state, tADP, hADP, RoutineName));
                hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                    SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                } else {
                    SHRadp = 1.0;
                }

                if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                    if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                    werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                    DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                    DryCoilTestEvapInletWetBulb =
                        PsyTwbFnTdbWPb(state, EvapInletDryBulb, DryCoilTestEvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);

                    ++Counter;
                    if (std::abs(werror) <= Tolerance) {
                        Converged = true;
                    } else {
                        Converged = false;
                    }
                } else {
                    Converged = true;
                }
            }
        }

        SHRTempFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
        SHRFlowFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlySHRFFlowCurve, AirMassFlowRatio);
        SHR = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedSHR * SHRTempFac * SHRFlowFac;
        SHR = min(SHR, 1.0); // warn maybe
        SHR = max(SHR, 0.0); // warn maybe
        if (CoilMightBeDry) {
            if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                SHR = 1.0;
            } else if (SHRadp > SHR) {
                SHR = SHRadp;
            }
        }
        PLF = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyPLFFPLRCurve, PartLoadRatio);
        if (PLF >= PartLoadRatio && PLF > 0.0) {
            RuntimeFraction = PartLoadRatio / PLF;
        } else {
            RuntimeFraction = 1.0; // warn maybe
        }
        //  Calculate full load output conditions
        FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

        hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
        // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
        FullLoadOutAirHumRat = PsyWFnTdbH(state, EvapInletDryBulb, hTinwout, RoutineName, true);
        FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
        // Check for saturation error and modify temperature at constant enthalpy
        if (FullLoadOutAirTemp < PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            FullLoadOutAirTemp = PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName);
            FullLoadOutAirHumRat = PsyWFnTdbH(state, FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
        }

        // Continuous fan, cycling compressor
        EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
        EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
        EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
        if (EvapOutletAirTemp < PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName)) {
            EvapOutletAirTemp = PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);
            EvapOutletAirHumRat = PsyWFnTdbH(state, EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
        }
        // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
        EIRTempModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyEIRFTempCurve, EvapInletWetBulb, CondInletTemp);
        EIRTempModFac = max(EIRTempModFac, 0.0);
        EIRFlowModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyEIRFFlowCurve, AirMassFlowRatio);
        EIRFlowModFac = max(EIRFlowModFac, 0.0);
        EIR = EIRTempModFac * EIRFlowModFac / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingOnlyRatedCOP;

        ElecCoolingPower = TotCap * EIR * RuntimeFraction;

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

        // determine condenser leaving conditions
        QdotCond = TotCap * RuntimeFraction + ElecCoolingPower;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
        CondOutletEnthalpy = CondInletEnthalpy + QdotCond / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            ElecCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = RuntimeFraction;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = RuntimeFraction;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = TotCap * RuntimeFraction; // double check this
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy =
            TotCap * RuntimeFraction * TimeStepSys * DataGlobalConstants::SecInHour;
        MinAirHumRat = min(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat,
                           state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
            EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate >
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate;
        }
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate -
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;

    } else { // coil is off; just pass through conditions
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES = 0.0;

    UpdateTEStorage(state, TESCoilNum);

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondInletTemp = CondInletTemp;

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
        UpdateEvaporativeCondenserWaterUse(
            state, TESCoilNum, CondInletHumRat, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum);
    }
}

void CalcTESCoilCoolingAndChargeMode(EnergyPlusData &state, int const TESCoilNum, [[maybe_unused]] int const FanOpMode, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using CurveManager::CurveValue;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIter(30);
    Real64 const RelaxationFactor(0.4);
    Real64 const Tolerance(0.1);
    static constexpr std::string_view RoutineName("CalcTESCoilCoolingAndChargeMode");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
    // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
    Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
    // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
    Real64 CondAirMassFlow;       // Condenser air mass flow rate [kg/s]
    Real64 CondInletEnthalpy;     // condenser inlet enthalpy [J/kg]
    Real64 CondAirSidePressure;   // Outdoor barometric pressure at condenser (Pa)
    Real64 QdotCond;              // condenser total heat rejection rate [W]
    Real64 CondOutletEnthalpy;    // condesner outlet enthalpy [J/kg]
    Real64 OutdoorDryBulb;        // outdoor air dry bulb local variable [C]
    Real64 OutdoorHumRat;         // outdoor air humidity ratio local [kg/kg]
    Real64 OutdoorWetBulb;        // outdoor air wetbulb local [C]
    Real64 EvapAirMassFlow;       // local for evaporator air mass flow [kg/s]
    Real64 EvapInletDryBulb;      // evaporator inlet air drybulb [C]
    Real64 EvapInletHumRat;       // evaporator inlet air humidity ratio [kg/kg]
    Real64 EvapInletWetBulb;      // evaporator inlet air wetbulb [C]
    Real64 EvapInletEnthalpy;     // evaporator inlet air enthalpy [J/kg]
    Real64 AirMassFlowRatio;      // evaporator inlet air mass flow divided by design mass flow [ ]
    Real64 EvapTotCapTempModFac;  // total coolin capacity modification factor due to temps []
    Real64 EvapTotCapFlowModFac;  // Total cooling capacity modification factor due to flow []
    Real64 EvapTotCap;            // total cooling capacity
    Real64 SHRTempFac(0.0);       // sensible heat ratio modification factor due to temps []
    Real64 SHRFlowFac;            // sensible heat ratio modification factor due to flow []
    Real64 SHR;                   // sensible heat ratio
    Real64 PLF;                   // part load factor
    Real64 EvapRuntimeFraction;   // compressor running time divided by full time of timestep.
    Real64 FullLoadOutAirEnth;    // evaporator outlet full load enthalpy [J/kg]
    Real64 hTinwout;              // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
    Real64 FullLoadOutAirHumRat;  // evaporator outlet humidity ratio at full load
    Real64 FullLoadOutAirTemp;    // evaporator outlet air temperature at full load [C]
    Real64 EvapOutletAirEnthalpy; // evaporator outlet air enthalpy [J/kg]
    Real64 EvapOutletAirHumRat;   // evaporator outlet air humidity ratio [kg/kg]
    Real64 EvapOutletAirTemp;     // evaporator outlet drybulb [C]
    Real64 EIRTempModFac;         // energy input ratio modification factor due to temperatures []
    Real64 EIRFlowModFac;         // energy input ratio modification factor due to flow []
    Real64 EIR;                   // energy input ratio
    Real64 EvapElecCoolingPower;  // compressor electric power
    Real64 MinAirHumRat;          // minimum air humidity ratio
    Real64 sTES;                  // stat of Thermal energy storage [C or fraction of ice]
    bool TESCanBeCharged;
    Real64 rho;
    Real64 TankMass;        // Mass of fluid in tank (kg)
    Real64 CpTank;          // Specific heat of water in tank (J/kg K)
    Real64 QdotChargeLimit; // limit for charge cooling power to hit limit of storage.
    Real64 ChargeCapModFac;
    Real64 ChargeCapPLRModFac;
    Real64 TotChargeCap;
    Real64 ChargeEIRTempModFac;
    Real64 ChargeEIRFlowModFac;
    Real64 ChargeEIR;
    Real64 ChargeElectricCoolingPower;
    Real64 ChargeRuntimeFraction;
    Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
    Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
    bool CoilMightBeDry;
    int Counter;
    bool Converged;
    Real64 DryCoilTestEvapInletHumRat;
    Real64 DryCoilTestEvapInletWetBulb;
    Real64 hADP;
    Real64 tADP;
    Real64 wADP;
    Real64 hTinwADP;
    Real64 SHRadp;
    Real64 werror;

    // first deal with condenser
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == AirCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            CondInletTemp = state.dataEnvrn->OutDryBulbTemp;
            CondInletHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
        } else {
            CondInletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            CondInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            OutdoorDryBulb = state.dataEnvrn->OutDryBulbTemp;
            OutdoorHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
            OutdoorWetBulb = state.dataEnvrn->OutWetBulbTemp;
        } else {
            OutdoorDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            OutdoorHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            OutdoorWetBulb = PsyTwbFnTdbWPb(state, OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        // direct evap cool model
        CondInletTemp =
            OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondEffect);
        CondInletHumRat = PsyWFnTdbTwbPb(state, CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
    }

    EvapAirMassFlow = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    EvapInletDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    EvapInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    EvapInletWetBulb = PsyTwbFnTdbWPb(state, EvapInletDryBulb, EvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
    EvapInletEnthalpy = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
    CoilMightBeDry = false;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        if ((sTES > state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) &&
            (sTES < state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
            TESCanBeCharged = true;
            // find charge limit to reach limits
            rho = GetDensityGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   sTES,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   RoutineName);
            TankMass = rho * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume;
            CpTank = GetSpecificHeatGlycol(state,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                           sTES,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                           RoutineName);
            // simple linear approximation of DT/Dt term in McpDT/Dt
            QdotChargeLimit = TankMass * CpTank * (sTES - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) /
                              (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESCanBeCharged = false;
        }
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep;
        if (sTES < 1.0) {
            TESCanBeCharged = true;
            // find charge limit to reach limit
            QdotChargeLimit = (1.0 - sTES) * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity /
                              (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESCanBeCharged = false;
        }
    }

    if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

        AirMassFlowRatio = EvapAirMassFlow / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
        EvapTotCapTempModFac = CurveValue(state,
                                          state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFTempCurve,
                                          EvapInletWetBulb,
                                          CondInletTemp,
                                          sTES);
        EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
        EvapTotCapFlowModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio);
        EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
        EvapTotCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
        // now see if coil is running dry
        PartLoadOutAirEnth = EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow;
        PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
        if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(state, PartLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            CoilMightBeDry = true;
            // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
            DryCoilTestEvapInletHumRat = EvapInletHumRat;
            DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
            Counter = 0;
            Converged = false;
            while (!Converged) {
                EvapTotCapTempModFac = CurveValue(state,
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFTempCurve,
                                                  DryCoilTestEvapInletWetBulb,
                                                  CondInletTemp,
                                                  sTES);
                EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
                EvapTotCapFlowModFac = CurveValue(
                    state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio);
                EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
                EvapTotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap * EvapTotCapTempModFac *
                             EvapTotCapFlowModFac;
                // coil bypass factor = 0.0
                hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow);
                tADP = PsyTsatFnHPb(state, hADP, state.dataEnvrn->OutBaroPress, RoutineName);
                wADP = min(EvapInletHumRat, PsyWFnTdbH(state, tADP, hADP, RoutineName));
                hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                    SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                } else {
                    SHRadp = 1.0;
                }

                if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                    if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                    werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                    DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                    DryCoilTestEvapInletWetBulb =
                        PsyTwbFnTdbWPb(state, EvapInletDryBulb, DryCoilTestEvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);

                    ++Counter;
                    if (std::abs(werror) <= Tolerance) {
                        Converged = true;
                    } else {
                        Converged = false;
                    }
                } else {
                    Converged = true;
                }
            }
        }
        {
            if (state.dataCurveManager->PerfCurve(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve).NumDims ==
                2) {
                SHRTempFac = CurveValue(state,
                                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve,
                                        EvapInletWetBulb,
                                        EvapInletDryBulb);
            } else {
                SHRTempFac = CurveValue(state,
                                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve,
                                        EvapInletWetBulb,
                                        EvapInletDryBulb,
                                        sTES);
            }
        }
        SHRFlowFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeSHRFFlowCurve, AirMassFlowRatio);
        SHR = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedSHR * SHRTempFac * SHRFlowFac;
        SHR = min(SHR, 1.0); // warn maybe
        SHR = max(SHR, 0.0); // warn maybe
        if (CoilMightBeDry) {
            if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                SHR = 1.0;
            } else if (SHRadp > SHR) {
                SHR = SHRadp;
            }
        }
        PLF = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingPLFFPLRCurve, PartLoadRatio);
        if (PLF >= PartLoadRatio && PLF > 0.0) {
            EvapRuntimeFraction = PartLoadRatio / PLF;
        } else {
            EvapRuntimeFraction = 1.0; // warn maybe
        }

        // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
        EIRTempModFac = CurveValue(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingEIRFTempCurve,
                                   EvapInletWetBulb,
                                   CondInletTemp,
                                   sTES);
        EIRTempModFac = max(EIRTempModFac, 0.0);
        EIRFlowModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingEIRFFlowCurve, AirMassFlowRatio);
        EIRFlowModFac = max(EIRFlowModFac, 0.0);
        EIR = EIRTempModFac * EIRFlowModFac / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeCoolingRatedCOP;

        EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction;

        if (TESCanBeCharged) {
            ChargeCapModFac = CurveValue(state,
                                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingCapFTempCurve,
                                         EvapInletWetBulb,
                                         CondInletTemp,
                                         sTES);
            ChargeCapModFac = max(0.0, ChargeCapModFac);

            ChargeCapPLRModFac =
                CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio);
            ChargeCapPLRModFac = max(0.0, ChargeCapPLRModFac);

            TotChargeCap =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac;
            if (TotChargeCap > QdotChargeLimit) {
                ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap;
                TotChargeCap = min(TotChargeCap, QdotChargeLimit);
            } else {
                ChargeRuntimeFraction = 1.0;
            }
            ChargeEIRTempModFac = CurveValue(state,
                                             state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFTempCurve,
                                             EvapInletWetBulb,
                                             CondInletTemp,
                                             sTES);
            ChargeEIRTempModFac = max(0.0, ChargeEIRTempModFac);
            ChargeEIRFlowModFac =
                CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio);
            ChargeEIRFlowModFac = max(0.0, ChargeEIRFlowModFac);
            ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) /
                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingRatedCOP;
            ChargeElectricCoolingPower = TotChargeCap * ChargeEIR;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
        } else {
            TotChargeCap = 0.0;
            ChargeElectricCoolingPower = 0.0;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
            ChargeRuntimeFraction = 0.0;
        }

        //  Calculate full load output conditions
        FullLoadOutAirEnth = EvapInletEnthalpy - EvapTotCap / EvapAirMassFlow;

        hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (EvapTotCap / EvapAirMassFlow);
        // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
        FullLoadOutAirHumRat = PsyWFnTdbH(state, EvapInletDryBulb, hTinwout, RoutineName, true);
        FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
        // Check for saturation error and modify temperature at constant enthalpy
        if (FullLoadOutAirTemp < PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            FullLoadOutAirTemp = PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName);
            FullLoadOutAirHumRat = PsyWFnTdbH(state, FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
        }

        // Continuous fan, cycling compressor
        EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
        EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
        EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
        if (EvapOutletAirTemp < PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName)) {
            EvapOutletAirTemp = PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);
            EvapOutletAirHumRat = PsyWFnTdbH(state, EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
        }

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

        // determine condenser leaving conditions
        QdotCond = EvapTotCap * EvapRuntimeFraction + EvapElecCoolingPower + TotChargeCap + ChargeElectricCoolingPower;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
        CondOutletEnthalpy = CondInletEnthalpy + QdotCond / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            EvapElecCoolingPower + ChargeElectricCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = EvapRuntimeFraction;
        if (ChargeRuntimeFraction > 0.0) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = max(ChargeRuntimeFraction, EvapRuntimeFraction);
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = EvapRuntimeFraction;
        }

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = EvapTotCap * EvapRuntimeFraction; // double check this
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy =
            EvapTotCap * EvapRuntimeFraction * TimeStepSys * DataGlobalConstants::SecInHour;
        MinAirHumRat = min(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat,
                           state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
            EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate >
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate;
        }
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate -
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;

    } else {                   // Evap off, but may still charge
        if (TESCanBeCharged) { // coil is running to charge but not to cool at evaporator
            AirMassFlowRatio = EvapAirMassFlow / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
            ChargeCapModFac = CurveValue(state,
                                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingCapFTempCurve,
                                         EvapInletWetBulb,
                                         CondInletTemp,
                                         sTES);
            ChargeCapModFac = max(0.0, ChargeCapModFac);

            ChargeCapPLRModFac =
                CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio);
            ChargeCapPLRModFac = max(0.0, ChargeCapPLRModFac);

            TotChargeCap =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac;
            if (TotChargeCap > QdotChargeLimit) {
                ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap;
                TotChargeCap = min(TotChargeCap, QdotChargeLimit);
            } else {
                ChargeRuntimeFraction = 1.0;
            }
            ChargeEIRTempModFac = CurveValue(state,
                                             state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFTempCurve,
                                             EvapInletWetBulb,
                                             CondInletTemp,
                                             sTES);
            ChargeEIRTempModFac = max(0.0, ChargeEIRTempModFac);
            ChargeEIRFlowModFac =
                CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio);
            ChargeEIRFlowModFac = max(0.0, ChargeEIRFlowModFac);
            ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) /
                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndChargeChargingRatedCOP;
            ChargeElectricCoolingPower = TotChargeCap * ChargeEIR;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
        } else {
            TotChargeCap = 0.0;
            ChargeElectricCoolingPower = 0.0;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
            ChargeRuntimeFraction = 0.0;
        }

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            ChargeElectricCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

        if (TotChargeCap == 0.0) {
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
                state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
                state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
                state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                           state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
        } else {

            // determine condenser leaving conditions
            QdotCond = TotChargeCap + ChargeElectricCoolingPower;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
            CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CondOutletEnthalpy = CondInletEnthalpy + QdotCond / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
                PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction = 1.0;
        }
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES * TimeStepSys * DataGlobalConstants::SecInHour;

    UpdateTEStorage(state, TESCoilNum);

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondInletTemp = CondInletTemp;

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
        UpdateEvaporativeCondenserWaterUse(
            state, TESCoilNum, CondInletHumRat, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum);
    }
}

void CalcTESCoilCoolingAndDischargeMode(EnergyPlusData &state, int const TESCoilNum, [[maybe_unused]] int const FanOpMode, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using CurveManager::CurveValue;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIter(30);
    Real64 const RelaxationFactor(0.4);
    Real64 const Tolerance(0.1);
    static constexpr std::string_view RoutineName("CalcTESCoilCoolingAndDischargeMode");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
    // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
    Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
    // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
    Real64 CondAirMassFlow;     // Condenser air mass flow rate [kg/s]
    Real64 CondInletEnthalpy;   // condenser inlet enthalpy [J/kg]
    Real64 CondAirSidePressure; // Outdoor barometric pressure at condenser (Pa)
    Real64 CondOutletEnthalpy;  // condesner outlet enthalpy [J/kg]
    Real64 OutdoorDryBulb;      // outdoor air dry bulb local variable [C]
    Real64 OutdoorHumRat;       // outdoor air humidity ratio local [kg/kg]
    Real64 OutdoorWetBulb;      // outdoor air wetbulb local [C]
    Real64 EvapAirMassFlow;     // local for evaporator air mass flow [kg/s]
    Real64 EvapInletDryBulb;    // evaporator inlet air drybulb [C]
    Real64 EvapInletHumRat;     // evaporator inlet air humidity ratio [kg/kg]
    Real64 EvapInletWetBulb;    // evaporator inlet air wetbulb [C]
    Real64 EvapInletEnthalpy;   // evaporator inlet air enthalpy [J/kg]
    Real64 sTES;                // stat of Thermal energy storage [C or fraction of ice]
    bool TESHasSomeCharge;      // some charge available for discharge
    Real64 rho;
    Real64 TankMass;             // Mass of fluid in tank (kg)
    Real64 CpTank;               // Specific heat of water in tank (J/kg K)
    Real64 QdotDischargeLimit;   // limit for charge cooling power to hit limit of storage.
    Real64 AirMassFlowRatio;     // evaporator inlet air mass flow divided by design mass flow [ ]
    Real64 EvapTotCapTempModFac; // total coolin capacity modification factor due to temps []
    Real64 EvapTotCapFlowModFac; // Total cooling capacity modification factor due to flow []
    Real64 EvapTotCap;           // total cooling capacity
    Real64 SHRTempFac(0.0);      // sensible heat ratio modification factor due to temps []
    Real64 SHRFlowFac;           // sensible heat ratio modification factor due to flow []
    Real64 SHR;                  // sensible heat ratio
    Real64 PLF;                  // part load factor
    Real64 EvapRuntimeFraction;  // compressor running time divided by full time of timestep.
    Real64 EIRTempModFac;        // energy input ratio modification factor due to temperatures []
    Real64 EIRFlowModFac;        // energy input ratio modification factor due to flow []
    Real64 EIR;                  // energy input ratio
    Real64 DischargePLF;
    Real64 DischargeRuntimeFraction;
    Real64 TotDischargeCap;
    Real64 DischargeCapTempModFac;
    Real64 DischargeCapFlowModFac;
    Real64 DischargeEIRTempModFac;
    Real64 DischargeEIRFlowModFac;
    Real64 DischargeEIR;
    Real64 EvapElecCoolingPower; // compressor electric power
    Real64 DischargeElectricCoolingPower;
    Real64 TotCap;
    Real64 FullLoadOutAirEnth;        // evaporator outlet full load enthalpy [J/kg]
    Real64 hTinwout;                  // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
    Real64 FullLoadOutAirHumRat;      // evaporator outlet humidity ratio at full load
    Real64 FullLoadOutAirTemp;        // evaporator outlet air temperature at full load [C]
    Real64 EvapOutletAirEnthalpy;     // evaporator outlet air enthalpy [J/kg]
    Real64 EvapOutletAirHumRat;       // evaporator outlet air humidity ratio [kg/kg]
    Real64 EvapOutletAirTemp;         // evaporator outlet drybulb [C]
    Real64 QdotCond;                  // heat rejection rate at condenser [W]
    Real64 MinAirHumRat;              // minimum air humidity ratio
    Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
    Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
    bool CoilMightBeDry;
    int Counter;
    bool Converged;
    Real64 DryCoilTestEvapInletHumRat;
    Real64 DryCoilTestEvapInletWetBulb;
    Real64 hADP;
    Real64 tADP;
    Real64 wADP;
    Real64 hTinwADP;
    Real64 SHRadp;
    Real64 werror;

    // first deal with condenser
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == AirCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            CondInletTemp = state.dataEnvrn->OutDryBulbTemp;
            CondInletHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
        } else {
            CondInletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            CondInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            OutdoorDryBulb = state.dataEnvrn->OutDryBulbTemp;
            OutdoorHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
            OutdoorWetBulb = state.dataEnvrn->OutWetBulbTemp;
        } else {
            OutdoorDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            OutdoorHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            OutdoorWetBulb = PsyTwbFnTdbWPb(state, OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        // direct evap cool model
        CondInletTemp =
            OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondEffect);
        CondInletHumRat = PsyWFnTdbTwbPb(state, CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
    }
    EvapAirMassFlow = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    EvapInletDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    EvapInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    EvapInletWetBulb = PsyTwbFnTdbWPb(state, EvapInletDryBulb, EvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
    EvapInletEnthalpy = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
    CoilMightBeDry = false;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        if ((sTES >= state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) &&
            (sTES < state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
            TESHasSomeCharge = true;
            rho = GetDensityGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   sTES,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   RoutineName);
            TankMass = rho * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume;
            CpTank = GetSpecificHeatGlycol(state,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                           sTES,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                           RoutineName);
            // simple linear approximation of DT/Dt term in McpDT/Dt
            QdotDischargeLimit = TankMass * CpTank * (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit - sTES) /
                                 (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESHasSomeCharge = false;
        }
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep;
        if (sTES > 0.0) {
            TESHasSomeCharge = true;
            // discharge limit
            QdotDischargeLimit =
                (sTES)*state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESHasSomeCharge = false;
        }
    }

    if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

        AirMassFlowRatio = EvapAirMassFlow / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
        EvapTotCapTempModFac = CurveValue(state,
                                          state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFTempCurve,
                                          EvapInletWetBulb,
                                          CondInletTemp,
                                          sTES);
        EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
        EvapTotCapFlowModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio);
        EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
        EvapTotCap =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
        // now see if coil is running dry
        PartLoadOutAirEnth = EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow;
        PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
        if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(state, PartLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            CoilMightBeDry = true;
            // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
            DryCoilTestEvapInletHumRat = EvapInletHumRat;
            DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
            Counter = 0;
            Converged = false;
            while (!Converged) {
                EvapTotCapTempModFac = CurveValue(state,
                                                  state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFTempCurve,
                                                  DryCoilTestEvapInletWetBulb,
                                                  CondInletTemp,
                                                  sTES);
                EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
                EvapTotCapFlowModFac = CurveValue(
                    state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio);
                EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
                EvapTotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac *
                             EvapTotCapFlowModFac;
                // coil bypass factor = 0.0
                hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow);
                tADP = PsyTsatFnHPb(state, hADP, state.dataEnvrn->OutBaroPress, RoutineName);
                wADP = min(EvapInletHumRat, PsyWFnTdbH(state, tADP, hADP, RoutineName));
                hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                    SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                } else {
                    SHRadp = 1.0;
                }

                if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                    if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                    werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                    DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                    DryCoilTestEvapInletWetBulb =
                        PsyTwbFnTdbWPb(state, EvapInletDryBulb, DryCoilTestEvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);

                    ++Counter;
                    if (std::abs(werror) <= Tolerance) {
                        Converged = true;
                    } else {
                        Converged = false;
                    }
                } else {
                    Converged = true;
                }
            }
        }
        {
            if (state.dataCurveManager->PerfCurve(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve)
                    .NumDims == 2) {
                SHRTempFac = CurveValue(state,
                                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve,
                                        EvapInletWetBulb,
                                        EvapInletDryBulb);
            } else {
                SHRTempFac = CurveValue(state,
                                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve,
                                        EvapInletWetBulb,
                                        EvapInletDryBulb,
                                        sTES);
            }
        }
        SHRFlowFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeSHRFFlowCurve, AirMassFlowRatio);
        SHR = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedSHR * SHRTempFac * SHRFlowFac;
        SHR = min(SHR, 1.0); // warn maybe
        SHR = max(SHR, 0.0); // warn maybe
        if (CoilMightBeDry) {
            if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                SHR = 1.0;
            } else if (SHRadp > SHR) {
                SHR = SHRadp;
            }
        }
        PLF = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingPLFFPLRCurve, PartLoadRatio);
        if (PLF >= PartLoadRatio && PLF > 0.0) {
            EvapRuntimeFraction = PartLoadRatio / PLF;
        } else {
            EvapRuntimeFraction = 1.0; // warn maybe
        }
        // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
        EIRTempModFac = CurveValue(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingEIRFTempCurve,
                                   EvapInletWetBulb,
                                   CondInletTemp,
                                   sTES);
        EIRTempModFac = max(EIRTempModFac, 0.0);
        EIRFlowModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingEIRFFlowCurve, AirMassFlowRatio);
        EIRFlowModFac = max(EIRFlowModFac, 0.0);
        EIR = EIRTempModFac * EIRFlowModFac / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeCoolingRatedCOP;

        EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction;

        if (TESHasSomeCharge) {
            DischargeCapTempModFac = CurveValue(state,
                                                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFTempCurve,
                                                EvapInletWetBulb,
                                                CondInletTemp,
                                                sTES);
            DischargeCapTempModFac = max(0.0, DischargeCapTempModFac);
            DischargeCapFlowModFac = CurveValue(
                state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFFlowCurve, AirMassFlowRatio);
            DischargeCapFlowModFac = max(0.0, DischargeCapFlowModFac);

            DischargePLF = CurveValue(
                state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFEvapPLRCurve, PartLoadRatio);
            if (DischargePLF >= PartLoadRatio && DischargePLF > 0.0) {
                DischargeRuntimeFraction = PartLoadRatio / DischargePLF;
            } else {
                DischargeRuntimeFraction = 1.0; // warn maybe
            }

            TotDischargeCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap *
                              DischargeCapTempModFac * DischargeCapFlowModFac * DischargeRuntimeFraction;
            if (TotDischargeCap > QdotDischargeLimit) {
                TotDischargeCap = min(TotDischargeCap, QdotDischargeLimit);
            }
            DischargeEIRTempModFac = CurveValue(state,
                                                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingEIRFTempCurve,
                                                EvapInletWetBulb,
                                                CondInletTemp,
                                                sTES);
            DischargeEIRTempModFac = max(0.0, DischargeEIRTempModFac);
            DischargeEIRFlowModFac = CurveValue(
                state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingEIRFFLowCurve, AirMassFlowRatio);
            DischargeEIRFlowModFac = max(0.0, DischargeEIRFlowModFac);

            DischargeEIR = (DischargeEIRTempModFac * DischargeEIRFlowModFac) /
                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CoolingAndDischargeDischargingRatedCOP;
            DischargeElectricCoolingPower = TotDischargeCap * DischargeEIR * DischargeRuntimeFraction;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = TotDischargeCap;
        } else {
            TotDischargeCap = 0.0;
            DischargeRuntimeFraction = 0.0;
            DischargeElectricCoolingPower = 0.0;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
        }

        TotCap = EvapTotCap + TotDischargeCap;
        //  Calculate full load output conditions
        FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

        hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
        // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
        FullLoadOutAirHumRat = PsyWFnTdbH(state, EvapInletDryBulb, hTinwout, RoutineName, true);
        FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
        // Check for saturation error and modify temperature at constant enthalpy
        if (FullLoadOutAirTemp < PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            FullLoadOutAirTemp = PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName);
            FullLoadOutAirHumRat = PsyWFnTdbH(state, FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
        }
        // Continuous fan, cycling compressor
        EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
        EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
        EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
        if (EvapOutletAirTemp < PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName)) {
            EvapOutletAirTemp = PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);
            EvapOutletAirHumRat = PsyWFnTdbH(state, EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
        }

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

        // determine condenser leaving conditions
        QdotCond = EvapTotCap * EvapRuntimeFraction + EvapElecCoolingPower;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
        CondOutletEnthalpy = CondInletEnthalpy + QdotCond / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            EvapElecCoolingPower + DischargeElectricCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction =
            (EvapTotCap * EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction) / (EvapTotCap + TotDischargeCap);

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate =
            EvapTotCap * EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        MinAirHumRat = min(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat,
                           state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
            EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate >
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate;
        }
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate -
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;

    } else { // coil is off; just pass through conditions
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);
        // nothing happens at condenser
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondInletTemp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
    }
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES * TimeStepSys * DataGlobalConstants::SecInHour;
    UpdateTEStorage(state, TESCoilNum);

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
        UpdateEvaporativeCondenserWaterUse(
            state, TESCoilNum, CondInletHumRat, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum);
    }
}

void CalcTESCoilChargeOnlyMode(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   May 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using CurveManager::CurveValue;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcTESCoilChargeOnlyMode");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 sTES;          // local state of Thermal Energy Storage (C or ice fraction)
    Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
    // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
    Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
    // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
    Real64 CondAirMassFlow;      // Condenser air mass flow rate [kg/s]
    Real64 CondInletEnthalpy;    // condenser inlet enthalpy [J/kg]
    Real64 CondAirSidePressure;  // Outdoor barometric pressure at condenser (Pa)
    Real64 QdotCond;             // condenser total heat rejection rate [W]
    Real64 CondOutletEnthalpy;   // condesner outlet enthalpy [J/kg]
    Real64 OutdoorDryBulb;       // outdoor air dry bulb local variable [C]
    Real64 OutdoorHumRat;        // outdoor air humidity ratio local [kg/kg]
    Real64 OutdoorWetBulb;       // outdoor air wetbulb local [C]
    Real64 CapModFac;            // local capacity modifying factor
    Real64 TotCap;               // total cooling (charging) capacity
    Real64 EIRModFac;            // local energy input ratio modifying factor
    Real64 EIR;                  // energy input ratio
    Real64 ElecCoolingPower;     // compressor electric power
    bool TESCanBeCharged(false); // true if room for tank to be charged.
    Real64 QdotChargeLimit;      // limit for charge cooling power to hit limit of storage.
    Real64 rho;                  // density of fluid in tank (kg/m3)
    Real64 TankMass;             // Mass of fluid in tank (kg)
    Real64 CpTank;               // Specific heat of water in tank (J/kg K)

    // nothing happens at Evaporator
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
        PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                   state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

    // first deal with condenser
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == AirCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            CondInletTemp = state.dataEnvrn->OutDryBulbTemp;
            CondInletHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
        } else {
            CondInletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            CondInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        CondAirSidePressure = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
        if (CondAirSidePressure == state.dataLoopNodes->DefaultNodeValues.Press) {
            OutdoorDryBulb = state.dataEnvrn->OutDryBulbTemp;
            OutdoorHumRat = state.dataEnvrn->OutHumRat;
            CondAirSidePressure = state.dataEnvrn->OutBaroPress;
            OutdoorWetBulb = state.dataEnvrn->OutWetBulbTemp;
        } else {
            OutdoorDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            OutdoorHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            OutdoorWetBulb = PsyTwbFnTdbWPb(state, OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
        }
        CondAirMassFlow = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        // direct evap cool model
        CondInletTemp =
            OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondEffect);
        CondInletHumRat = PsyWFnTdbTwbPb(state, CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
    }

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        if ((sTES > state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) &&
            (sTES < state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
            TESCanBeCharged = true;
            // find charge limit to reach limits
            rho = GetDensityGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   sTES,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   RoutineName);
            TankMass = rho * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume;
            CpTank = GetSpecificHeatGlycol(state,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                           sTES,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                           RoutineName);
            // simple linear approximation of DT/Dt term in McpDT/Dt
            QdotChargeLimit = TankMass * CpTank * (sTES - state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) /
                              (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESCanBeCharged = false;
        }
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep;
        if (sTES < 1.0) {
            TESCanBeCharged = true;
            // find charge limit to reach limit
            QdotChargeLimit = (1.0 - sTES) * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity /
                              (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESCanBeCharged = false;
        }
    } else {
        assert(false);
    }

    if (TESCanBeCharged) { // coil is running
        CapModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyChargingCapFTempCurve, CondInletTemp, sTES);
        CapModFac = max(0.0, CapModFac);
        TotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCapacity * CapModFac;
        if (TotCap > QdotChargeLimit) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = QdotChargeLimit / TotCap;
            TotCap = min(TotCap, QdotChargeLimit);
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 1.0;
        }
        EIRModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyChargingEIRFTempCurve, CondInletTemp, sTES);
        EIRModFac = max(0.0, EIRModFac);
        EIR = EIRModFac / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ChargeOnlyRatedCOP;
        ElecCoolingPower = TotCap * EIR;
        QdotCond = TotCap + ElecCoolingPower;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
        CondOutletEnthalpy = CondInletEnthalpy + QdotCond / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            ElecCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = -TotCap; // negative for cooling

    } else { // not running
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
    }
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES * TimeStepSys * DataGlobalConstants::SecInHour;

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

    UpdateTEStorage(state, TESCoilNum);

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
        UpdateEvaporativeCondenserWaterUse(
            state, TESCoilNum, CondInletHumRat, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum);
    }
}

void CalcTESCoilDischargeOnlyMode(EnergyPlusData &state, int const TESCoilNum, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using CurveManager::CurveValue;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIter(30);
    Real64 const RelaxationFactor(0.4);
    Real64 const Tolerance(0.1);
    static constexpr std::string_view RoutineName("CalcTESCoilDischargeOnlyMode");
    static constexpr std::string_view StorageTankName("CalcTESWaterStorageTank");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AirMassFlowRatio;      // evaporator inlet air mass flow divided by design mass flow [ ]
    Real64 EvapAirMassFlow;       // local for evaporator air mass flow [kg/s]
    Real64 EvapInletDryBulb;      // evaporator inlet air drybulb [C]
    Real64 EvapInletHumRat;       // evaporator inlet air humidity ratio [kg/kg]
    Real64 EvapInletWetBulb;      // evaporator inlet air wetbulb [C]
    Real64 EvapInletEnthalpy;     // evaporator inlet air enthalpy [J/kg]
    Real64 sTES;                  // state of charge of Thermal Energy Storage
    Real64 TotCapTempModFac;      // total coolin capacity modification factor due to temps []
    Real64 TotCapFlowModFac;      // Total cooling capacity modification factor due to flow []
    Real64 TotCap;                // total cooling capacity
    Real64 SHRTempFac(0.0);       // sensible heat ratio modification factor due to temps []
    Real64 SHRFlowFac;            // sensible heat ratio modification factor due to flow []
    Real64 SHR;                   // sensible heat ratio
    Real64 PLF;                   // part load factor
    Real64 PLR;                   // part load ratio
    Real64 RuntimeFraction;       // compressor running time divided by full time of timestep.
    Real64 FullLoadOutAirEnth;    // evaporator outlet full load enthalpy [J/kg]
    Real64 FullLoadOutAirHumRat;  // evaporator outlet humidity ratio at full load
    Real64 FullLoadOutAirTemp;    // evaporator outlet air temperature at full load [C]
    Real64 hTinwout;              // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
    Real64 EvapOutletAirEnthalpy; // evaporator outlet air enthalpy [J/kg]
    Real64 EvapOutletAirHumRat;   // evaporator outlet air humidity ratio [kg/kg]
    Real64 EvapOutletAirTemp;     // evaporator outlet drybulb [C]
    Real64 EIRTempModFac;         // energy input ratio modification factor due to temperatures []
    Real64 EIRFlowModFac;         // energy input ratio modification factor due to flow []
    Real64 EIR;                   // energy input ratio
    Real64 ElecCoolingPower;      // compressor electric power
    Real64 MinAirHumRat;          // minimum air humidity ratio
    bool TESHasSomeCharge;        // true when there is something avaiable in storage
    Real64 QdotDischargeLimit;    // limit for how much storage can be discharged without overshooting
    Real64 rho;                   // density of water in tank (kg/m3)
    Real64 TankMass;              // Mass of water in tank (kg)
    Real64 CpTank;                // Specific heat of water in tank (J/kg K)
    Real64 QdotTEStest;
    Real64 RuntimeFractionLimit;
    Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
    Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
    bool CoilMightBeDry;
    int Counter;
    bool Converged;
    Real64 DryCoilTestEvapInletHumRat;
    Real64 DryCoilTestEvapInletWetBulb;
    Real64 hADP;
    Real64 tADP;
    Real64 wADP;
    Real64 hTinwADP;
    Real64 SHRadp;
    Real64 werror;

    PLR = PartLoadRatio;

    EvapAirMassFlow = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
    EvapInletDryBulb = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
    EvapInletHumRat = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
    EvapInletWetBulb = PsyTwbFnTdbWPb(state, EvapInletDryBulb, EvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
    EvapInletEnthalpy = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
    CoilMightBeDry = false;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        if ((sTES >= state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MinimumFluidTankTempLimit) &&
            (sTES < state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
            TESHasSomeCharge = true;
            rho = GetDensityGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   sTES,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   StorageTankName);
            TankMass = rho * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume;
            CpTank = GetSpecificHeatGlycol(state,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                           sTES,
                                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                           StorageTankName);
            // simple linear approximation of DT/Dt term in McpDT/Dt
            QdotDischargeLimit = TankMass * CpTank * (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).MaximumFluidTankTempLimit - sTES) /
                                 (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESHasSomeCharge = false;
        }
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) {
        sTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep;
        if (sTES > 0.0) {
            TESHasSomeCharge = true;
            // discharge limit
            QdotDischargeLimit =
                (sTES)*state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * DataGlobalConstants::SecInHour);
        } else {
            TESHasSomeCharge = false;
        }
    }

    if ((EvapAirMassFlow > SmallMassFlow) && (PLR > 0.0) && TESHasSomeCharge) { // coil is running
        AirMassFlowRatio = EvapAirMassFlow / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;

        TotCapTempModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyCapFTempCurve, EvapInletWetBulb, sTES);
        TotCapTempModFac = max(0.0, TotCapTempModFac);
        TotCapFlowModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyCapFFlowCurve, AirMassFlowRatio);
        TotCapFlowModFac = max(0.0, TotCapFlowModFac);
        TotCap = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TotCapTempModFac * TotCapFlowModFac;

        PLF = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyPLFFPLRCurve, PLR);
        if (PLF >= PLR && PLF > 0.0) {
            RuntimeFraction = PLR / PLF;
        } else {
            RuntimeFraction = 1.0; // warn maybe
        }
        // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
        EIRTempModFac =
            CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyEIRFTempCurve, EvapInletWetBulb, sTES);
        EIRTempModFac = max(EIRTempModFac, 0.0);
        EIRFlowModFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyEIRFFlowCurve, AirMassFlowRatio);
        EIRFlowModFac = max(EIRFlowModFac, 0.0);
        EIR = EIRTempModFac * EIRFlowModFac / state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedCOP;

        ElecCoolingPower = TotCap * EIR * RuntimeFraction;
        QdotTEStest = TotCap * RuntimeFraction + ElecCoolingPower;

        if (QdotTEStest > QdotDischargeLimit) {
            RuntimeFractionLimit = QdotDischargeLimit / (TotCap + TotCap * EIR);
            RuntimeFraction = min(RuntimeFraction, RuntimeFractionLimit);
            PLR = RuntimeFraction * PLF;
            ElecCoolingPower = TotCap * EIR * RuntimeFraction;
        }
        // now see if coil is running dry
        PartLoadOutAirEnth = EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow;
        PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
        if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(state, PartLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            CoilMightBeDry = true;
            // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
            DryCoilTestEvapInletHumRat = EvapInletHumRat;
            DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
            Counter = 0;
            Converged = false;
            while (!Converged) {
                TotCapTempModFac = CurveValue(
                    state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyCapFTempCurve, DryCoilTestEvapInletWetBulb, sTES);
                TotCapTempModFac = max(0.0, TotCapTempModFac);
                TotCapFlowModFac =
                    CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyCapFFlowCurve, AirMassFlowRatio);
                TotCapFlowModFac = max(0.0, TotCapFlowModFac);
                TotCap =
                    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TotCapTempModFac * TotCapFlowModFac;
                // coil bypass factor = 0.0
                hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow);
                tADP = PsyTsatFnHPb(state, hADP, state.dataEnvrn->OutBaroPress, RoutineName);
                wADP = min(EvapInletHumRat, PsyWFnTdbH(state, tADP, hADP, RoutineName));
                hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                    SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                } else {
                    SHRadp = 1.0;
                }

                if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                    if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                    werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                    DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                    DryCoilTestEvapInletWetBulb =
                        PsyTwbFnTdbWPb(state, EvapInletDryBulb, DryCoilTestEvapInletHumRat, state.dataEnvrn->OutBaroPress, RoutineName);

                    ++Counter;
                    if (std::abs(werror) <= Tolerance) {
                        Converged = true;
                    } else {
                        Converged = false;
                    }
                } else {
                    Converged = true;
                }
            }
        } // coil will be wet so use SHR curves
        {
            if (state.dataCurveManager->PerfCurve(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve).NumDims ==
                2) {
                SHRTempFac = CurveValue(
                    state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
            } else {
                SHRTempFac = CurveValue(state,
                                        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve,
                                        EvapInletWetBulb,
                                        EvapInletDryBulb,
                                        sTES);
            }
        }

        SHRFlowFac = CurveValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlySHRFFLowCurve, AirMassFlowRatio);
        SHR = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).DischargeOnlyRatedSHR * SHRTempFac * SHRFlowFac;
        SHR = min(SHR, 1.0); // warn maybe
        SHR = max(SHR, 0.0); // warn maybe
        if (CoilMightBeDry) {
            if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                SHR = 1.0;
            } else if (SHRadp > SHR) {
                SHR = SHRadp;
            }
        }
        //  Calculate full load output conditions
        FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

        hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
        // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
        FullLoadOutAirHumRat = PsyWFnTdbH(state, EvapInletDryBulb, hTinwout, RoutineName, true);
        FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
        // Check for saturation error and modify temperature at constant enthalpy
        if (FullLoadOutAirTemp < PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName)) {
            FullLoadOutAirTemp = PsyTsatFnHPb(state, FullLoadOutAirEnth, state.dataEnvrn->OutBaroPress, RoutineName);
            FullLoadOutAirHumRat = PsyWFnTdbH(state, FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
        }

        // Continuous fan, cycling compressor
        EvapOutletAirEnthalpy = ((PLR)*FullLoadOutAirEnth + (1.0 - (PLR)) * EvapInletEnthalpy);
        EvapOutletAirHumRat = ((PLR)*FullLoadOutAirHumRat + (1.0 - (PLR)) * EvapInletHumRat);
        EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
        if (EvapOutletAirTemp < PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName)) {
            EvapOutletAirTemp = PsyTsatFnHPb(state, EvapOutletAirEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);
            EvapOutletAirHumRat = PsyWFnTdbH(state, EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
        }

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            ElecCoolingPower + state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = RuntimeFraction;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = TotCap * RuntimeFraction; // double check this
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy =
            TotCap * RuntimeFraction * TimeStepSys * DataGlobalConstants::SecInHour;
        MinAirHumRat = min(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat,
                           state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
            EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate >
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate;
        }
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate -
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES =
            TotCap * RuntimeFraction + ElecCoolingPower; // all heat rejection into storage

    } else { // coil is off; just pass through conditions
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES = 0.0;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AncillaryControlsPower;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingEnergy =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp,
                       state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);
    }

    // nothing happens at condenser
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
    state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
        PsyHFnTdbW(state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp,
                   state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondInletTemp =
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_TES =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES * TimeStepSys * DataGlobalConstants::SecInHour;
    UpdateTEStorage(state, TESCoilNum);

    UpdateColdWeatherProtection(state, TESCoilNum);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserType == EvapCooled) {
        UpdateEvaporativeCondenserBasinHeater(state, TESCoilNum);
        UpdateEvaporativeCondenserWaterUse(
            state,
            TESCoilNum,
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat,
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondAirInletNodeNum);
    }
}

void ControlTESIceStorageTankCoil(
    EnergyPlusData &state,
    std::string const &CoilName,               // child object coil name
    int CoilIndex,                             // child object coil index
    std::string SystemType,                    // parent object system type
    int const FanOpMode,                       // parent object fan operating mode
    Real64 const DesiredOutletTemp,            // desired outlet temperature [C]
    Real64 const DesiredOutletHumRat,          // desired outlet humidity ratio [kg/kg]
    Real64 &PartLoadFrac,                      // value based on coil operation, if possible, as PLR required to meet T or w set point
    int &TESOpMode,                            // value determined in InitTESCoil and passed back to parent for use in iteration routines
    HVACDXSystem::DehumidControl &ControlType, // parent object dehumidification control type (e.g., None, Multimode, CoolReheat)
    int &SensPLRIter,                          // iteration number of Sensible PLR Iteration warning message
    int &SensPLRIterIndex,                     // index to Sensible PLR Iteration warning message
    int &SensPLRFail,                          // iteration number of Sensible PLR Iteration fail warning message
    int &SensPLRFailIndex,                     // index to Sensible PLR Iteration fail warning message
    int &LatPLRIter,                           // iteration number of Latent PLR Iteration warning message
    int &LatPLRIterIndex,                      // index to Latent PLR Iteration warning message
    int &LatPLRFail,                           // iteration number of Latent PLR Iteration fail warning message
    int &LatPLRFailIndex                       // index to Latent PLR Iteration fail warning message
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad (based on HVACDXSystem code)
    //       DATE WRITTEN   July 13, 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides a common routine for parent objects. Parent objects will call this routine to determine the coil PLR.

    using General::SolveRoot;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIte(500);         // Maximum number of iterations for solver
    Real64 const Acc(1.e-3);       // Accuracy of solver result
    Real64 const HumRatAcc(1.e-6); // Accuracy of solver result

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int OutletNode;
    Real64 NoOutput;
    Real64 NoLoadHumRatOut;
    Real64 FullOutput;
    Real64 FullLoadHumRatOut;
    Real64 ReqOutput;
    Real64 OutletHumRatDXCoil;
    int SolFlag;            // return flag from RegulaFalsi for sensible load
    Array1D<Real64> Par(5); // Parameter array passed to solver

    InletNode = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).EvapAirInletNodeNum;
    OutletNode = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).EvapAirOutletNodeNum;

    // First get the control mode that the child coil is in
    SimTESCoil(state, CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
    if (TESOpMode == OffMode || TESOpMode == ChargeOnlyMode) { // cannot cool
        PartLoadFrac = 0.0;
    } else {
        // Get no load result
        PartLoadFrac = 0.0;
        SimTESCoil(state, CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
        NoOutput = state.dataLoopNodes->Node(InletNode).MassFlowRate *
                   (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat) -
                    PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat));
        NoLoadHumRatOut = state.dataLoopNodes->Node(OutletNode).HumRat;

        // Get full load result
        PartLoadFrac = 1.0;
        SimTESCoil(state, CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
        FullOutput = state.dataLoopNodes->Node(InletNode).MassFlowRate *
                     (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat) -
                      PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat));
        FullLoadHumRatOut = state.dataLoopNodes->Node(OutletNode).HumRat;

        ReqOutput = state.dataLoopNodes->Node(InletNode).MassFlowRate *
                    (PsyHFnTdbW(DesiredOutletTemp, state.dataLoopNodes->Node(OutletNode).HumRat) -
                     PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat));
        //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
        if ((NoOutput - ReqOutput) < Acc) {
            PartLoadFrac = 0.0;
            //         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
            //         run the compressor at PartLoadFrac = 1.
        } else if ((FullOutput - ReqOutput) > Acc) {
            PartLoadFrac = 1.0;
            //         Else find the PLR to meet the load
        } else {
            if (state.dataLoopNodes->Node(OutletNode).Temp > DesiredOutletTemp) {
                PartLoadFrac = 1.0;
            } else {
                Par(1) = double(CoilIndex);
                Par(2) = DesiredOutletTemp;
                Par(3) = TESOpMode;
                Par(4) = OutletNode;
                Par(5) = double(FanOpMode);
                General::SolveRoot(state, Acc, MaxIte, SolFlag, PartLoadFrac, TESCoilResidualFunction, 0.0, 1.0, Par);
                if (SolFlag == -1) {
                    if (!state.dataGlobal->WarmupFlag) {
                        if (SensPLRIter < 1) {
                            ++SensPLRIter;
                            ShowWarningError(state,
                                             SystemType +
                                                 " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + CoilName);
                            ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", ReqOutput / FullOutput));
                            ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                            ShowContinueErrorTimeStamp(state,
                                                       "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       SystemType + " \"" + CoilName +
                                                           "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. "
                                                           "Sensible PLR statistics follow.",
                                                       SensPLRIterIndex,
                                                       PartLoadFrac,
                                                       PartLoadFrac);
                    }
                } else if (SolFlag == -2) {
                    PartLoadFrac = ReqOutput / FullOutput;
                    if (!state.dataGlobal->WarmupFlag) {
                        if (SensPLRFail < 1) {
                            ++SensPLRFail;
                            ShowWarningError(
                                state,
                                SystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                    CoilName);
                            ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                            ShowContinueErrorTimeStamp(state,
                                                       "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            SystemType + " \"" + CoilName +
                                "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                            SensPLRFailIndex,
                            PartLoadFrac,
                            PartLoadFrac);
                    }
                }
            }
            //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
            //         else use operating humidity ratio to test against humidity setpoint
            if (PartLoadFrac == 0.0) {
                OutletHumRatDXCoil = NoLoadHumRatOut;
            } else {
                OutletHumRatDXCoil = state.dataLoopNodes->Node(OutletNode).HumRat;
            }
            // If humidity setpoint is not satisfied and humidity control type is CoolReheat,
            // then overcool to meet moisture load

            if ((OutletHumRatDXCoil > DesiredOutletHumRat) && (PartLoadFrac < 1.0) && (ControlType == HVACDXSystem::DehumidControl::CoolReheat)) {
                //           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                //           do not run the compressor
                if ((NoLoadHumRatOut - DesiredOutletHumRat) < HumRatAcc) {
                    // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                    //           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                    //           run the compressor at PartLoadFrac = 1.
                } else if ((DesiredOutletHumRat - FullLoadHumRatOut) < HumRatAcc) {
                    PartLoadFrac = 1.0;
                    //           Else find the PLR to meet the load
                } else {
                    Par(1) = double(CoilIndex);
                    Par(2) = DesiredOutletHumRat;
                    Par(3) = TESOpMode;
                    Par(4) = OutletNode;
                    Par(5) = double(FanOpMode);
                    General::SolveRoot(state, HumRatAcc, MaxIte, SolFlag, PartLoadFrac, TESCoilHumRatResidualFunction, 0.0, 1.0, Par);
                    if (SolFlag == -1) {
                        if (!state.dataGlobal->WarmupFlag) {
                            if (LatPLRIter < 1) {
                                ++LatPLRIter;
                                ShowWarningError(state,
                                                 SystemType +
                                                     " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + CoilName);
                                ShowContinueError(state, format("Estimated part-load ratio   = {:.3R}", ReqOutput / FullOutput));
                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                ShowContinueErrorTimeStamp(
                                    state, "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           SystemType + " \"" + CoilName +
                                                               "\" - Iteration limit exceeded calculating latent part-load ratio error "
                                                               "continues. Latent PLR statistics follow.",
                                                           LatPLRIterIndex,
                                                           PartLoadFrac,
                                                           PartLoadFrac);
                        }
                    } else if (SolFlag == -2) {
                        //               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
                        if (NoLoadHumRatOut - FullLoadHumRatOut != 0.0) {
                            PartLoadFrac = (NoLoadHumRatOut - DesiredOutletHumRat) / (NoLoadHumRatOut - FullLoadHumRatOut);
                        } else {
                            PartLoadFrac = 1.0;
                        }
                        if (!state.dataGlobal->WarmupFlag) {
                            if (LatPLRFail < 1) {
                                ++LatPLRFail;
                                ShowWarningError(
                                    state,
                                    SystemType +
                                        " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                        CoilName);
                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                ShowContinueErrorTimeStamp(
                                    state, "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                            }
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                SystemType + " \"" + CoilName +
                                    "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.",
                                LatPLRFailIndex,
                                PartLoadFrac,
                                PartLoadFrac);
                        }
                    }
                }
            } // End if humidity ratio setpoint not met - CoolReheat humidity control

        } // operating mode can cool
        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }
    }
}

Real64 TESCoilResidualFunction(EnergyPlusData &state,
                               Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                               Array1D<Real64> const &Par  // par(1) = DX coil number
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (desired outlet temp - actual outlet temp)
    // TES Coil output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Calls appropriate calculation routine depending on operating mode
    // to get outlet temperature at the given cycling ratio
    // and calculates the residual as defined above

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = desired air outlet temperature [C]
    // par(3) = TES coil operating mode
    // par(4) = outlet node number
    // par(5) = supply air fan operating mode (ContFanCycCoil)

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;        // index of this coil
    Real64 OutletAirTemp; // outlet air temperature [C]
    int FanOpMode;        // Supply air fan operating mode
    int TESOpMode;
    int OutletNodeNum;

    CoilIndex = int(Par(1));
    FanOpMode = int(Par(5));
    OutletNodeNum = int(Par(4));
    TESOpMode = int(Par(3));

    {
        auto const SELECT_CASE_var(TESOpMode);
        if (SELECT_CASE_var == CoolingOnlyMode) {
            CalcTESCoilCoolingOnlyMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndChargeMode) {
            CalcTESCoilCoolingAndChargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
            CalcTESCoilCoolingAndDischargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == DischargeOnlyMode) {
            CalcTESCoilDischargeOnlyMode(state, CoilIndex, PartLoadRatio);
        }
    }

    OutletAirTemp = state.dataLoopNodes->Node(OutletNodeNum).Temp;
    Residuum = Par(2) - OutletAirTemp;

    return Residuum;
}

Real64 TESCoilHumRatResidualFunction(EnergyPlusData &state,
                                     Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                     Array1D<Real64> const &Par  // par(1) = DX coil number
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (desired outlet humrat - actual outlet humrat)
    // TES Coil output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Calls appropriate calculation routine depending on operating mode
    // to get outlet hum rat at the given cycling ratio
    // and calculates the residual as defined above

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = desired air outlet hum rat [kgWater/kgDryAir]
    // par(3) = TES coil operating mode
    // par(4) = outlet node number
    // par(5) = supply air fan operating mode (ContFanCycCoil)

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;          // index of this coil
    Real64 OutletAirHumRat; // outlet air humidity ratio [kgWater/kgDryAir]
    int FanOpMode;          // Supply air fan operating mode
    int TESOpMode;
    int OutletNodeNum;

    CoilIndex = int(Par(1));
    FanOpMode = int(Par(5));
    OutletNodeNum = int(Par(4));
    TESOpMode = int(Par(3));

    {
        auto const SELECT_CASE_var(TESOpMode);
        if (SELECT_CASE_var == CoolingOnlyMode) {
            CalcTESCoilCoolingOnlyMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndChargeMode) {
            CalcTESCoilCoolingAndChargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
            CalcTESCoilCoolingAndDischargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
        } else if (SELECT_CASE_var == DischargeOnlyMode) {
            CalcTESCoilDischargeOnlyMode(state, CoilIndex, PartLoadRatio);
        }
    }

    OutletAirHumRat = state.dataLoopNodes->Node(OutletNodeNum).HumRat;
    Residuum = Par(2) - OutletAirHumRat;

    return Residuum;
}

void UpdateTEStorage(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::FluidBased) {
        CalcTESWaterStorageTank(state, TESCoilNum);
    } else if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageMedia == iMedia::IceBased) {
        CalcTESIceStorageTank(state, TESCoilNum);
    }
}

void CalcTESWaterStorageTank(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using WaterThermalTanks::WaterThermalTankData;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcTESWaterStorageTank");
    static constexpr std::string_view calcTESIceStorageTank("CalcTESIceStorageTank");

    Real64 TimeElapsed;        // Fraction of the current hour that has elapsed (h)
    Real64 AmbientTemp;        // Current ambient air temperature around tank (C)
    Real64 TankMass;           // Mass of water in tank (kg)
    Real64 LossCoeff;          // Loss coefficient to ambient environment (W/K)
    Real64 TankTemp;           // Instantaneous tank temperature (C)
    Real64 NewTankTemp;        // Predicted new tank temperature (C)
    Real64 CpTank;             // Specific heat of water in tank (J/kg K)
    Real64 UseInletTemp;       // Use side inlet temperature (C)
    Real64 UseMassFlowRate;    // Use side flow rate, including effectiveness factor (kg/s)
    Real64 SourceInletTemp;    // Source side inlet temperature (C)
    Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
    Real64 TimeRemaining;      // Time remaining in the current timestep (s)
    Real64 CpPlantConnection;  // Specific heat of fluid in plant connection (J/kg K)
    Real64 deltaTsum;          // Change in integrated tank temperature, dividing by time gives the average (C s)
    Real64 SecInTimeStep;      // Seconds in one timestep (s)
    Real64 rho;                // density of water in tank (kg/m3)
    Real64 QdotTES;            // heat exchange directly into tank from charging system [W]
    Real64 NewOutletTemp;      // calculated new tankoutlet temp (C)

    SecInTimeStep = TimeStepSys * DataGlobalConstants::SecInHour;
    TimeRemaining = SecInTimeStep;

    TimeElapsed = state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + SysTimeElapsed;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TimeElapsed != TimeElapsed) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinal;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TimeElapsed = TimeElapsed;
    }

    TankTemp = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
    AmbientTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp;
    UseInletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp;
    SourceInletTemp = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
    rho = GetDensityGlycol(state,
                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                           TankTemp,
                           state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                           RoutineName);
    TankMass = rho * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidStorageVolume;
    CpTank = GetSpecificHeatGlycol(state,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidName,
                                   TankTemp,
                                   state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageFluidIndex,
                                   RoutineName);

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
        UseMassFlowRate = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate *
                          state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantEffectiveness;
    } else {
        UseMassFlowRate = 0.0;
    }
    SourceMassFlowRate = 0.0;
    LossCoeff = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageUA;
    QdotTES = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES;

    NewTankTemp = WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp,
                                                                        AmbientTemp,
                                                                        UseInletTemp,
                                                                        SourceInletTemp,
                                                                        TankMass,
                                                                        CpTank,
                                                                        UseMassFlowRate,
                                                                        SourceMassFlowRate,
                                                                        LossCoeff,
                                                                        QdotTES,
                                                                        TimeRemaining);

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).FluidTankTempFinal = NewTankTemp;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
        CpPlantConnection =
            GetSpecificHeatGlycol(state,
                                  state.dataPlnt->PlantLoop(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum).FluidName,
                                  state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp,
                                  state.dataPlnt->PlantLoop(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum).FluidIndex,
                                  calcTESIceStorageTank);

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate *
            CpPlantConnection * state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantEffectiveness * (UseInletTemp - NewTankTemp);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Plant =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant * TimeStepSys * DataGlobalConstants::SecInHour;
        // now get correct outlet temp with actual massflow (not modified by effectiveness)
        if (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate >
            DataBranchAirLoopPlant::MassFlowTolerance) {
            NewOutletTemp =
                UseInletTemp -
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant /
                    (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate *
                     CpPlantConnection);
        } else {
            NewOutletTemp = UseInletTemp;
        }
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantOutletNodeNum).Temp = NewOutletTemp;
    }

    deltaTsum = WaterThermalTankData::CalcTempIntegral(TankTemp,
                                                       NewTankTemp,
                                                       AmbientTemp,
                                                       UseInletTemp,
                                                       SourceInletTemp,
                                                       TankMass,
                                                       CpTank,
                                                       UseMassFlowRate,
                                                       SourceMassFlowRate,
                                                       LossCoeff,
                                                       QdotTES,
                                                       TimeRemaining);
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient = (LossCoeff * (AmbientTemp * TimeRemaining - deltaTsum)) / SecInTimeStep;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Ambient =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient * TimeStepSys * DataGlobalConstants::SecInHour;
}

void CalcTESIceStorageTank(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr Real64 FreezingTemp(0.0); // zero degrees C
    static constexpr std::string_view RoutineName("CalcTESIceStorageTank");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Cp;            // local specific heat
    Real64 QdotIce;       // local rate of heat transfer to ice (negative cooling) [W]
    Real64 TimeElapsed;   // Fraction of the current hour that has elapsed (h)
    Real64 NewOutletTemp; // calculated new tank outlet temp (C)

    TimeElapsed = state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + SysTimeElapsed;

    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TimeElapsed != TimeElapsed) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TimeElapsed = TimeElapsed;
    }

    // update plant connection (if any)
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum).FluidName,
                                   state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp,
                                   state.dataPlnt->PlantLoop(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantLoopNum).FluidIndex,
                                   RoutineName);

        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant =
            state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * Cp *
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantEffectiveness *
            (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp - FreezingTemp);
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Plant =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant * TimeStepSys * DataGlobalConstants::SecInHour;
        // now get correct outlet temp with actual massflow (not modified by effectiveness)
        if (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate >
            DataBranchAirLoopPlant::MassFlowTolerance) {
            NewOutletTemp =
                state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp +
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant /
                    (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * Cp);
        } else {
            NewOutletTemp = state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp;
        }
        state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).TESPlantOutletNodeNum).Temp = NewOutletTemp;
    } else {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant = 0.0;
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Plant = 0.0;
    }

    // update ambient heat transfer

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageUA *
        (state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp - FreezingTemp);
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).Q_Ambient =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient * TimeStepSys * DataGlobalConstants::SecInHour;

    QdotIce = state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotPlant +
              state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotAmbient +
              state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).QdotTES;

    if (QdotIce < 0.0) { // charging ice level
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep +
            std::abs(QdotIce) /
                (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * DataGlobalConstants::SecInHour));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain > 1.0)
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain = 1.0;
    } else { // not charging,but discharging
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemainLastTimestep -
            QdotIce / (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * DataGlobalConstants::SecInHour));
        if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain < 0.0)
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).IceFracRemain = 0.0;
    }
}

void UpdateColdWeatherProtection(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    if ((state.dataLoopNodes->Node(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp <
         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ColdWeatherMinimumTempLimit) &&
        (GetCurrentScheduleValue(state, state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).AvailSchedNum) != 0.0)) {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherPower =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ColdWeatherAncillaryPower;

    } else {
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0;
    }
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherEnergy =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectColdWeatherPower * state.dataHVACGlobal->TimeStepSys *
        DataGlobalConstants::SecInHour;
}

void UpdateEvaporativeCondenserBasinHeater(EnergyPlusData &state, int const TESCoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // determine basin heater electrical power and energy

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    CalcBasinHeaterPower(state,
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).BasinHeaterPowerFTempDiff,
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).BasinHeaterAvailSchedNum,
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).BasinHeaterSetpointTemp,
                         state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower);

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower * TimeStepSys * DataGlobalConstants::SecInHour;
}

void UpdateEvaporativeCondenserWaterUse(EnergyPlusData &state, int const TESCoilNum, Real64 const HumRatAfterEvap, int const InletNodeNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // update and calculate water consumption for evaporatively cooled condensers

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AvailWaterRate;
    Real64 RhoWater;

    RhoWater = RhoH2O(state.dataLoopNodes->Node(InletNodeNum).Temp);
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate =
        (HumRatAfterEvap - state.dataLoopNodes->Node(InletNodeNum).HumRat) * state.dataLoopNodes->Node(InletNodeNum).MassFlowRate / RhoWater *
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction;

    // Set the demand request for supply water from water storage tank (if needed)
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterSupplyMode == iWaterSupply::WaterSupplyFromTank) {
        state.dataWaterData->WaterStorage(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterSupTankID)
            .VdotRequestDemand(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterTankDemandARRID) =
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate;
    }

    // check if should be starved by restricted flow from tank
    if (state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterSupplyMode == iWaterSupply::WaterSupplyFromTank) {
        AvailWaterRate = state.dataWaterData->WaterStorage(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterSupTankID)
                             .VdotAvailDemand(state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterTankDemandARRID);
        if (AvailWaterRate < state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate) {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterStarvMakupRate =
                state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate - AvailWaterRate;
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate = AvailWaterRate;
        } else {
            state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterStarvMakupRate = 0.0;
        }
    }

    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsump =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterConsumpRate * state.dataHVACGlobal->TimeStepSys *
        DataGlobalConstants::SecInHour;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterStarvMakup =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapWaterStarvMakupRate * state.dataHVACGlobal->TimeStepSys *
        DataGlobalConstants::SecInHour;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecPower =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecNomPower *
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).CondenserRuntimeFraction;
    state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecConsumption =
        state.dataPackagedThermalStorageCoil->TESCoil(TESCoilNum).EvapCondPumpElecPower * state.dataHVACGlobal->TimeStepSys *
        DataGlobalConstants::SecInHour;
}

void GetTESCoilIndex(EnergyPlusData &state, std::string const &CoilName, int &CoilIndex, bool &ErrorsFound, Optional_string_const CurrentModuleObject)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   August 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets an index for a given TES Cooling Coil -- issues error message if that
    // coil is not a legal TES Cooling Coil.

    // Obtains and allocates TESCoil related parameters from input file
    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) { // First time subroutine has been called, get input data
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag =
            false; // Set logic flag to disallow getting the input data on future calls to this subroutine
    }

    if (state.dataPackagedThermalStorageCoil->NumTESCoils > 0) {
        CoilIndex = UtilityRoutines::FindItem(CoilName, state.dataPackagedThermalStorageCoil->TESCoil);
    } else {
        CoilIndex = 0;
    }

    if (CoilIndex == 0) {
        if (present(CurrentModuleObject)) {
            ShowSevereError(state, CurrentModuleObject() + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
        } else {
            ShowSevereError(state, "GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
        }
        ErrorsFound = true;
    }
}

void GetTESCoilAirInletNode(
    EnergyPlusData &state, std::string const &CoilName, int &CoilAirInletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets a given TES Cooling Coil's air inlet node -- issues error message if that
    // coil is not a legal TES Cooling Coil and sets air node to 0, otherwise, returns inlet air node number.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;

    // Obtains and allocates TESCoil related parameters from input file
    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) { // First time subroutine has been called, get input data
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag =
            false; // Set logic flag to disallow getting the input data on future calls to this subroutine
    }

    if (state.dataPackagedThermalStorageCoil->NumTESCoils > 0) {
        CoilIndex =
            UtilityRoutines::FindItem(CoilName, state.dataPackagedThermalStorageCoil->TESCoil, state.dataPackagedThermalStorageCoil->NumTESCoils);
    } else {
        CoilIndex = 0;
    }

    if (CoilIndex == 0) {
        ShowSevereError(state, CurrentModuleObject + ", GetTESCoilAirInletNode: TES Cooling Coil not found=" + CoilName);
        ErrorsFound = true;
        CoilAirInletNode = 0;
    } else {
        CoilAirInletNode = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).EvapAirInletNodeNum;
    }
}

void GetTESCoilAirOutletNode(
    EnergyPlusData &state, std::string const &CoilName, int &CoilAirOutletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets a given TES Cooling Coil's air outlet node -- issues error message if that
    // coil is not a legal TES Cooling Coil and sets air node to 0, otherwise, returns outlet air node number.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;

    // Obtains and allocates TESCoil related parameters from input file
    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) { // First time subroutine has been called, get input data
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag =
            false; // Set logic flag to disallow getting the input data on future calls to this subroutine
    }

    if (state.dataPackagedThermalStorageCoil->NumTESCoils > 0) {
        CoilIndex =
            UtilityRoutines::FindItem(CoilName, state.dataPackagedThermalStorageCoil->TESCoil, state.dataPackagedThermalStorageCoil->NumTESCoils);
    } else {
        CoilIndex = 0;
    }

    if (CoilIndex == 0) {
        ShowSevereError(state, CurrentModuleObject + ", GetTESCoilAirOutletNode: TES Cooling Coil not found=" + CoilName);
        ErrorsFound = true;
        CoilAirOutletNode = 0;
    } else {
        CoilAirOutletNode = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).EvapAirOutletNodeNum;
    }
}

void GetTESCoilCoolingCapacity(
    EnergyPlusData &state, std::string const &CoilName, Real64 &CoilCoolCapacity, bool &ErrorsFound, std::string const &CurrentModuleObject)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets a given TES Cooling Coil's cooling only capacity -- issues error message if that
    // coil is not a legal TES Cooling Coil and sets capacity to 0, otherwise, returns cooling capacity.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;

    // Obtains and allocates TESCoil related parameters from input file
    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) { // First time subroutine has been called, get input data
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag =
            false; // Set logic flag to disallow getting the input data on future calls to this subroutine
    }

    if (state.dataPackagedThermalStorageCoil->NumTESCoils > 0) {
        CoilIndex =
            UtilityRoutines::FindItem(CoilName, state.dataPackagedThermalStorageCoil->TESCoil, state.dataPackagedThermalStorageCoil->NumTESCoils);
    } else {
        CoilIndex = 0;
    }

    if (CoilIndex == 0) {
        ShowSevereError(state, CurrentModuleObject + ", GetTESCoilCoolingCapacity: TES Cooling Coil not found=" + CoilName);
        ErrorsFound = true;
        CoilCoolCapacity = 0.0;
    } else {
        if (state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingOnlyModeIsAvailable) { // get input data for this mode
            CoilCoolCapacity = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingOnlyRatedTotCap;
        } else if (state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingAndChargeModeAvailable) {
            CoilCoolCapacity = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingAndChargeRatedTotCap;
        } else if (state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingAndDischargeModeAvailable) {
            CoilCoolCapacity = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).CoolingAndDischargeRatedTotCap;
        } else {
            CoilCoolCapacity = 0.0;
        }
    }
}

void GetTESCoilCoolingAirFlowRate(
    EnergyPlusData &state, std::string const &CoilName, Real64 &CoilCoolAirFlow, bool &ErrorsFound, std::string const &CurrentModuleObject)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets a given TES Cooling Coil's evaporator air flow rate -- issues error message if that
    // coil is not a legal TES Cooling Coil and sets air flow to 0, otherwise, returns cooling air flow rate.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;

    // Obtains and allocates TESCoil related parameters from input file
    if (state.dataPackagedThermalStorageCoil->GetTESInputFlag) { // First time subroutine has been called, get input data
        GetTESCoilInput(state);
        state.dataPackagedThermalStorageCoil->GetTESInputFlag =
            false; // Set logic flag to disallow getting the input data on future calls to this subroutine
    }

    if (state.dataPackagedThermalStorageCoil->NumTESCoils > 0) {
        CoilIndex =
            UtilityRoutines::FindItem(CoilName, state.dataPackagedThermalStorageCoil->TESCoil, state.dataPackagedThermalStorageCoil->NumTESCoils);
    } else {
        CoilIndex = 0;
    }

    if (CoilIndex == 0) {
        ShowSevereError(state, CurrentModuleObject + ", GetTESCoilCoolingCapacity: TES Cooling Coil not found=" + CoilName);
        ErrorsFound = true;
        CoilCoolAirFlow = 0.0;
    } else {
        CoilCoolAirFlow = state.dataPackagedThermalStorageCoil->TESCoil(CoilIndex).RatedEvapAirVolFlowRate;
    }
}

} // namespace EnergyPlus::PackagedThermalStorageCoil
