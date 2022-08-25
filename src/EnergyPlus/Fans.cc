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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Fans {
// Module containing the fan simulation routines

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen
//       DATE WRITTEN   April 1998
//       MODIFIED       Shirey, May 2001
//                      Griffith, May 2009, EMS changes
//                      Craig Wray 22Aug2010 Added Fan Component Model
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the Fan System Component

// Using/Aliasing
using namespace DataLoopNode;
using DataHVACGlobals::cFanTypes;
using DataHVACGlobals::Cooling;
using DataHVACGlobals::FanType_ComponentModel;
using DataHVACGlobals::FanType_SimpleConstVolume;
using DataHVACGlobals::FanType_SimpleOnOff;
using DataHVACGlobals::FanType_SimpleVAV;
using DataHVACGlobals::FanType_ZoneExhaust;
using DataHVACGlobals::FixedMin;
using DataHVACGlobals::Heating;
using DataHVACGlobals::Main;
using DataHVACGlobals::MinFrac;
using DataHVACGlobals::Other;
using DataHVACGlobals::SmallAirVolFlow;
using EMSManager::ManageEMS;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTdbFnHW;
using namespace ScheduleManager;

constexpr std::array<std::string_view, static_cast<int>(AvailabilityManagerCoupling::Num)> couplingsUC = {"COUPLED", "DECOUPLED"};

void SimulateFanComponents(EnergyPlusData &state,
                           std::string_view const CompName,
                           bool const FirstHVACIteration,
                           int &CompIndex,
                           Optional<Real64 const> SpeedRatio,
                           Optional_bool_const ZoneCompTurnFansOn,  // Turn fans ON signal from ZoneHVAC component
                           Optional_bool_const ZoneCompTurnFansOff, // Turn Fans OFF signal from ZoneHVAC component
                           Optional<Real64 const> PressureRise      // Pressure difference to use for DeltaPress
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       Chandan Sharma, March 2011 - FSEC: Added logic for ZoneHVAC sys avail managers
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages Fan component simulation.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanNum; // current fan number

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (CompIndex == 0) {
        FanNum = UtilityRoutines::FindItemInList(CompName, Fan, &FanEquipConditions::FanName);
        if (FanNum == 0) {
            ShowFatalError(state, "SimulateFanComponents: Fan not found=" + std::string{CompName});
        }
        CompIndex = FanNum;
    } else {
        FanNum = CompIndex;
        if (FanNum > state.dataFans->NumFans || FanNum < 1) {
            ShowFatalError(
                state,
                format(
                    "SimulateFanComponents: Invalid CompIndex passed={}, Number of Fans={}, Fan name={}", FanNum, state.dataFans->NumFans, CompName));
        }
        if (state.dataFans->CheckEquipName(FanNum)) {
            if (!CompName.empty() && CompName != Fan(FanNum).FanName) {
                ShowFatalError(state,
                               format("SimulateFanComponents: Invalid CompIndex passed={}, Fan name={}, stored Fan Name for that index={}",
                                      FanNum,
                                      CompName,
                                      Fan(FanNum).FanName));
            }
            state.dataFans->CheckEquipName(FanNum) = false;
        }
    }

    state.dataFans->LocalTurnFansOn = false;
    state.dataFans->LocalTurnFansOff = false;
    // With the correct FanNum Initialize
    InitFan(state, FanNum, FirstHVACIteration); // Initialize all fan related parameters

    if (present(ZoneCompTurnFansOn) && present(ZoneCompTurnFansOff)) {
        // Set module-level logic flags equal to ZoneCompTurnFansOn and ZoneCompTurnFansOff values passed into this routine
        // for ZoneHVAC components with system availability managers defined.
        // The module-level flags get used in the other subroutines (e.g., SimSimpleFan,SimVariableVolumeFan and SimOnOffFan)
        state.dataFans->LocalTurnFansOn = ZoneCompTurnFansOn;
        state.dataFans->LocalTurnFansOff = ZoneCompTurnFansOff;
    } else {
        // Set module-level logic flags equal to the global LocalTurnFansOn and LocalTurnFansOff variables for all other cases.
        state.dataFans->LocalTurnFansOn = state.dataHVACGlobal->TurnFansOn;
        state.dataFans->LocalTurnFansOff = state.dataHVACGlobal->TurnFansOff;
    }

    // Calculate the Correct Fan Model with the current FanNum
    if (Fan(FanNum).FanType_Num == FanType_SimpleConstVolume) {
        SimSimpleFan(state, FanNum);
    } else if (Fan(FanNum).FanType_Num == FanType_SimpleVAV) {
        if (present(PressureRise)) {
            SimVariableVolumeFan(state, FanNum, PressureRise);
        } else {
            SimVariableVolumeFan(state, FanNum);
        }
    } else if (Fan(FanNum).FanType_Num == FanType_SimpleOnOff) {
        SimOnOffFan(state, FanNum, SpeedRatio);
    } else if (Fan(FanNum).FanType_Num == FanType_ZoneExhaust) {
        SimZoneExhaustFan(state, FanNum);
    } else if (Fan(FanNum).FanType_Num == FanType_ComponentModel) {
        SimComponentModelFan(state, FanNum);
    }

    // Update the current fan to the outlet nodes
    UpdateFan(state, FanNum);

    // Report the current fan
    ReportFan(state, FanNum);
}

// Get Input Section of the Module
//******************************************************************************

void GetFanInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       Shirey, May 2001
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for fans and stores it in fan data structures

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using CurveManager::GetCurveIndex;
    using NodeInputManager::GetOnlySingleNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanNum;       // The fan that you are currently loading input into
    int NumSimpFan;   // The number of Simple Const Vol Fans
    int NumVarVolFan; // The number of Simple Variable Vol Fans
    int NumOnOff;     // The number of Simple on-off Fans
    int NumZoneExhFan;
    int SimpFanNum;
    int OnOffFanNum;
    int VarVolFanNum;
    int ExhFanNum;
    int NVPerfNum;
    bool NVPerfFanFound;
    int NumCompModelFan;
    int CompModelFanNum;
    int NumAlphas;
    int NumNums;
    int checkNum;
    int IOStat;
    bool ErrorsFound(false);                                        // If errors detected in input
    static constexpr std::string_view RoutineName("GetFanInput: "); // include trailing blank space
    Array1D_string cAlphaFieldNames;
    Array1D_string cNumericFieldNames;
    Array1D_bool lNumericFieldBlanks;
    Array1D_bool lAlphaFieldBlanks;
    Array1D_string cAlphaArgs;
    Array1D<Real64> rNumericArgs;
    std::string cCurrentModuleObject;
    int NumParams;
    int MaxAlphas;
    int MaxNumbers;

    auto &Fan(state.dataFans->Fan);
    auto &FanNumericFields(state.dataFans->FanNumericFields);
    auto &UniqueFanNames(state.dataFans->UniqueFanNames);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    state.dataFans->GetFanInputFlag = false;

    MaxAlphas = 0;
    MaxNumbers = 0;
    NumSimpFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ConstantVolume");
    if (NumSimpFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:ConstantVolume", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    NumVarVolFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:VariableVolume");
    if (NumVarVolFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:VariableVolume", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    NumOnOff = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:OnOff");
    if (NumOnOff > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:OnOff", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    NumZoneExhFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ZoneExhaust");
    if (NumZoneExhFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:ZoneExhaust", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    state.dataFans->NumNightVentPerf = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FanPerformance:NightVentilation");
    if (state.dataFans->NumNightVentPerf > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FanPerformance:NightVentilation", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }

    NumCompModelFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ComponentModel");
    if (NumCompModelFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:ComponentModel", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }

    cAlphaArgs.allocate(MaxAlphas);
    cAlphaFieldNames.allocate(MaxAlphas);
    lAlphaFieldBlanks.dimension(MaxAlphas, false);
    cNumericFieldNames.allocate(MaxNumbers);
    lNumericFieldBlanks.dimension(MaxNumbers, false);
    rNumericArgs.dimension(MaxNumbers, 0.0);

    state.dataFans->NumFans = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + NumCompModelFan;
    if (state.dataFans->NumFans > 0) {
        Fan.allocate(state.dataFans->NumFans);
        FanNumericFields.allocate(state.dataFans->NumFans);
        UniqueFanNames.reserve(state.dataFans->NumFans);
    }
    state.dataFans->CheckEquipName.dimension(state.dataFans->NumFans, true);

    for (SimpFanNum = 1; SimpFanNum <= NumSimpFan; ++SimpFanNum) {
        FanNum = SimpFanNum;
        cCurrentModuleObject = "Fan:ConstantVolume";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SimpFanNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        FanNumericFields(FanNum).FieldNames.allocate(MaxNumbers);
        FanNumericFields(FanNum).FieldNames = "";
        FanNumericFields(FanNum).FieldNames = cNumericFieldNames;

        GlobalNames::VerifyUniqueInterObjectName(state, UniqueFanNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        Fan(FanNum).FanName = cAlphaArgs(1);
        Fan(FanNum).FanType = cCurrentModuleObject;
        Fan(FanNum).AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            Fan(FanNum).AvailSchedPtrNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Fan(FanNum).AvailSchedPtrNum = GetScheduleIndex(state, cAlphaArgs(2));
            if (Fan(FanNum).AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered =" + cAlphaArgs(2) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        }
        Fan(FanNum).FanType_Num = FanType_SimpleConstVolume;

        Fan(FanNum).FanEff = rNumericArgs(1);
        Fan(FanNum).DeltaPress = rNumericArgs(2);
        Fan(FanNum).MaxAirFlowRate = rNumericArgs(3);
        if (Fan(FanNum).MaxAirFlowRate == 0.0) {
            ShowWarningError(state,
                             cCurrentModuleObject + "=\"" + Fan(FanNum).FanName +
                                 "\" has specified 0.0 max air flow rate. It will not be used in the simulation.");
        }
        Fan(FanNum).MaxAirFlowRateIsAutosizable = true;
        Fan(FanNum).MotEff = rNumericArgs(4);
        Fan(FanNum).MotInAirFrac = rNumericArgs(5);
        Fan(FanNum).MinAirFlowRate = 0.0;

        Fan(FanNum).InletNodeNum = GetOnlySingleNode(state,
                                                     cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::FanConstantVolume,
                                                     cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsNotParent);
        Fan(FanNum).OutletNodeNum = GetOnlySingleNode(state,
                                                      cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::FanConstantVolume,
                                                      cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsNotParent);

        if (NumAlphas > 4) {
            Fan(FanNum).EndUseSubcategoryName = cAlphaArgs(5);
        } else {
            Fan(FanNum).EndUseSubcategoryName = "General";
        }

        TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Air Nodes");

    } // end Number of Simple FAN Loop

    for (VarVolFanNum = 1; VarVolFanNum <= NumVarVolFan; ++VarVolFanNum) {
        FanNum = NumSimpFan + VarVolFanNum;
        cCurrentModuleObject = "Fan:VariableVolume";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 VarVolFanNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        FanNumericFields(FanNum).FieldNames.allocate(MaxNumbers);
        FanNumericFields(FanNum).FieldNames = "";
        FanNumericFields(FanNum).FieldNames = cNumericFieldNames;

        GlobalNames::VerifyUniqueInterObjectName(state, UniqueFanNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        Fan(FanNum).FanName = cAlphaArgs(1);
        Fan(FanNum).FanType = cCurrentModuleObject;
        Fan(FanNum).AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            Fan(FanNum).AvailSchedPtrNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Fan(FanNum).AvailSchedPtrNum = GetScheduleIndex(state, cAlphaArgs(2));
            if (Fan(FanNum).AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered =" + cAlphaArgs(2) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        }
        Fan(FanNum).FanType_Num = FanType_SimpleVAV;

        Fan(FanNum).FanEff = rNumericArgs(1);
        Fan(FanNum).DeltaPress = rNumericArgs(2);
        Fan(FanNum).MaxAirFlowRate = rNumericArgs(3);
        if (Fan(FanNum).MaxAirFlowRate == 0.0) {
            ShowWarningError(state,
                             cCurrentModuleObject + "=\"" + Fan(FanNum).FanName +
                                 "\" has specified 0.0 max air flow rate. It will not be used in the simulation.");
        }
        Fan(FanNum).MaxAirFlowRateIsAutosizable = true;
        if (UtilityRoutines::SameString(cAlphaArgs(3), "Fraction")) {
            Fan(FanNum).FanMinAirFracMethod = MinFrac;
        } else if (UtilityRoutines::SameString(cAlphaArgs(3), "FixedFlowRate")) {
            Fan(FanNum).FanMinAirFracMethod = FixedMin;
        } else {
            ShowSevereError(state, cAlphaFieldNames(3) + " should be either Fraction or FixedFlowRate.");
            ShowContinueError(state, "Occurs in " + Fan(FanNum).FanName + " object.");
            ErrorsFound = true;
        }
        //        Fan(FanNum)%MinAirFlowRate= rNumericArgs(4)
        Fan(FanNum).FanMinFrac = rNumericArgs(4);
        Fan(FanNum).FanFixedMin = rNumericArgs(5);
        Fan(FanNum).MotEff = rNumericArgs(6);
        Fan(FanNum).MotInAirFrac = rNumericArgs(7);
        Fan(FanNum).FanCoeff(1) = rNumericArgs(8);
        Fan(FanNum).FanCoeff(2) = rNumericArgs(9);
        Fan(FanNum).FanCoeff(3) = rNumericArgs(10);
        Fan(FanNum).FanCoeff(4) = rNumericArgs(11);
        Fan(FanNum).FanCoeff(5) = rNumericArgs(12);
        if (Fan(FanNum).FanCoeff(1) == 0.0 && Fan(FanNum).FanCoeff(2) == 0.0 && Fan(FanNum).FanCoeff(3) == 0.0 && Fan(FanNum).FanCoeff(4) == 0.0 &&
            Fan(FanNum).FanCoeff(5) == 0.0) {
            ShowWarningError(state, "Fan Coefficients are all zero.  No Fan power will be reported.");
            ShowContinueError(state, "For " + cCurrentModuleObject + ", Fan=" + cAlphaArgs(1));
        }
        Fan(FanNum).InletNodeNum = GetOnlySingleNode(state,
                                                     cAlphaArgs(4),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::FanVariableVolume,
                                                     cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsNotParent);
        Fan(FanNum).OutletNodeNum = GetOnlySingleNode(state,
                                                      cAlphaArgs(5),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::FanVariableVolume,
                                                      cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsNotParent);

        if (NumAlphas > 5) {
            Fan(FanNum).EndUseSubcategoryName = cAlphaArgs(6);
        } else {
            Fan(FanNum).EndUseSubcategoryName = "General";
        }

        TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Air Nodes");

    } // end Number of Variable Volume FAN Loop

    for (ExhFanNum = 1; ExhFanNum <= NumZoneExhFan; ++ExhFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + ExhFanNum;
        cCurrentModuleObject = "Fan:ZoneExhaust";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 ExhFanNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        FanNumericFields(FanNum).FieldNames.allocate(MaxNumbers);
        FanNumericFields(FanNum).FieldNames = "";
        FanNumericFields(FanNum).FieldNames = cNumericFieldNames;

        GlobalNames::VerifyUniqueInterObjectName(state, UniqueFanNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        Fan(FanNum).FanName = cAlphaArgs(1);
        Fan(FanNum).FanType = cCurrentModuleObject;
        Fan(FanNum).AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            Fan(FanNum).AvailSchedPtrNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Fan(FanNum).AvailSchedPtrNum = GetScheduleIndex(state, cAlphaArgs(2));
            if (Fan(FanNum).AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered =" + cAlphaArgs(2) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                if (HasFractionalScheduleValue(state, Fan(FanNum).AvailSchedPtrNum)) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + "=\"" + Fan(FanNum).FanName + "\" has fractional values in Schedule=" + cAlphaArgs(2) +
                                         ". Only 0.0 in the schedule value turns the fan off.");
                }
            }
        }
        Fan(FanNum).FanType_Num = FanType_ZoneExhaust;

        Fan(FanNum).FanEff = rNumericArgs(1);
        Fan(FanNum).DeltaPress = rNumericArgs(2);
        Fan(FanNum).MaxAirFlowRate = rNumericArgs(3);
        Fan(FanNum).MaxAirFlowRateIsAutosizable = false;
        Fan(FanNum).MotEff = 1.0;
        Fan(FanNum).MotInAirFrac = 1.0;
        Fan(FanNum).MinAirFlowRate = 0.0;
        Fan(FanNum).RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        Fan(FanNum).MaxAirMassFlowRate = Fan(FanNum).MaxAirFlowRate * Fan(FanNum).RhoAirStdInit;

        if (Fan(FanNum).MaxAirFlowRate == 0.0) {
            ShowWarningError(state,
                             cCurrentModuleObject + "=\"" + Fan(FanNum).FanName +
                                 "\" has specified 0.0 max air flow rate. It will not be used in the simulation.");
        }

        Fan(FanNum).InletNodeNum = GetOnlySingleNode(state,
                                                     cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::FanZoneExhaust,
                                                     cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsNotParent);
        Fan(FanNum).OutletNodeNum = GetOnlySingleNode(state,
                                                      cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::FanZoneExhaust,
                                                      cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsNotParent);

        if (NumAlphas > 4 && !lAlphaFieldBlanks(5)) {
            Fan(FanNum).EndUseSubcategoryName = cAlphaArgs(5);
        } else {
            Fan(FanNum).EndUseSubcategoryName = "General";
        }

        if (NumAlphas > 5 && !lAlphaFieldBlanks(6)) {
            Fan(FanNum).FlowFractSchedNum = GetScheduleIndex(state, cAlphaArgs(6));
            if (Fan(FanNum).FlowFractSchedNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(6) + " entered =" + cAlphaArgs(6) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            } else if (Fan(FanNum).FlowFractSchedNum > 0) {
                if (!CheckScheduleValueMinMax(state, Fan(FanNum).FlowFractSchedNum, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(6) + " for " +
                                        cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                    ShowContinueError(state, "Error found in " + cAlphaFieldNames(6) + " = " + cAlphaArgs(6));
                    ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                    ErrorsFound = true;
                }
            }
        } else {
            Fan(FanNum).FlowFractSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        }

        if (NumAlphas > 6 && !lAlphaFieldBlanks(7)) {
            Fan(FanNum).AvailManagerMode = static_cast<AvailabilityManagerCoupling>(getEnumerationValue(couplingsUC, cAlphaArgs(7)));
            if (Fan(FanNum).AvailManagerMode == AvailabilityManagerCoupling::Invalid) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(7) + " entered =" + cAlphaArgs(7) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        } else {
            Fan(FanNum).AvailManagerMode = AvailabilityManagerCoupling::Coupled;
        }

        if (NumAlphas > 7 && !lAlphaFieldBlanks(8)) {
            Fan(FanNum).MinTempLimitSchedNum = GetScheduleIndex(state, cAlphaArgs(8));
            if (Fan(FanNum).MinTempLimitSchedNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(8) + " entered =" + cAlphaArgs(8) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        } else {
            Fan(FanNum).MinTempLimitSchedNum = 0;
        }

        if (NumAlphas > 8 && !lAlphaFieldBlanks(9)) {

            if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing) {
                // do not include adjusted for "balanced" exhaust flow in the zone total return calculation
                ShowWarningError(state,
                                 std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(9) + " = " + cAlphaArgs(9) +
                                     " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ShowContinueError(state, "When zone air mass flow balance is enforced, this input field should be left blank.");
                ShowContinueError(state, "This schedule will be ignored in the simulation.");
                Fan(FanNum).BalancedFractSchedNum = 0;
            } else {
                Fan(FanNum).BalancedFractSchedNum = GetScheduleIndex(state, cAlphaArgs(9));
                if (Fan(FanNum).BalancedFractSchedNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(9) +
                                        " entered =" + cAlphaArgs(9) + " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (Fan(FanNum).BalancedFractSchedNum > 0) {
                    if (!CheckScheduleValueMinMax(state, Fan(FanNum).BalancedFractSchedNum, ">=", 0.0, "<=", 1.0)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(9) + " for " +
                                            cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError(state, "Error found in " + cAlphaFieldNames(9) + " = " + cAlphaArgs(9));
                        ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                        ErrorsFound = true;
                    }
                }
            }
        } else {
            Fan(FanNum).BalancedFractSchedNum = 0;
        }

    } // end of Zone Exhaust Fan loop

    for (OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
        cCurrentModuleObject = "Fan:OnOff";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 OnOffFanNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        FanNumericFields(FanNum).FieldNames.allocate(MaxNumbers);
        FanNumericFields(FanNum).FieldNames = "";
        FanNumericFields(FanNum).FieldNames = cNumericFieldNames;

        GlobalNames::VerifyUniqueInterObjectName(state, UniqueFanNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        Fan(FanNum).FanName = cAlphaArgs(1);
        Fan(FanNum).FanType = cCurrentModuleObject;
        Fan(FanNum).AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            Fan(FanNum).AvailSchedPtrNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Fan(FanNum).AvailSchedPtrNum = GetScheduleIndex(state, cAlphaArgs(2));
            if (Fan(FanNum).AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered =" + cAlphaArgs(2) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        }
        Fan(FanNum).FanType_Num = FanType_SimpleOnOff;

        Fan(FanNum).FanEff = rNumericArgs(1);
        Fan(FanNum).DeltaPress = rNumericArgs(2);
        Fan(FanNum).MaxAirFlowRate = rNumericArgs(3);
        if (Fan(FanNum).MaxAirFlowRate == 0.0) {
            ShowWarningError(state,
                             cCurrentModuleObject + "=\"" + Fan(FanNum).FanName +
                                 "\" has specified 0.0 max air flow rate. It will not be used in the simulation.");
        }
        Fan(FanNum).MaxAirFlowRateIsAutosizable = true;
        //       the following two structure variables are set here, as well as in InitFan, for the Heat Pump:Water Heater object
        //       (Standard Rating procedure may be called before BeginEnvirFlag is set to TRUE, if so MaxAirMassFlowRate = 0)
        Fan(FanNum).RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        Fan(FanNum).MaxAirMassFlowRate = Fan(FanNum).MaxAirFlowRate * Fan(FanNum).RhoAirStdInit;

        Fan(FanNum).MotEff = rNumericArgs(4);
        Fan(FanNum).MotInAirFrac = rNumericArgs(5);
        Fan(FanNum).MinAirFlowRate = 0.0;

        Fan(FanNum).InletNodeNum = GetOnlySingleNode(state,
                                                     cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::FanOnOff,
                                                     cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsNotParent);
        Fan(FanNum).OutletNodeNum = GetOnlySingleNode(state,
                                                      cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::FanOnOff,
                                                      cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsNotParent);

        if (NumAlphas > 4 && !lAlphaFieldBlanks(5)) {
            Fan(FanNum).FanPowerRatAtSpeedRatCurveIndex = GetCurveIndex(state, cAlphaArgs(5));
        }

        if (NumAlphas > 5 && !lAlphaFieldBlanks(6)) {
            Fan(FanNum).FanEffRatioCurveIndex = GetCurveIndex(state, cAlphaArgs(6));
        }

        if (NumAlphas > 6 && !lAlphaFieldBlanks(7)) {
            Fan(FanNum).EndUseSubcategoryName = cAlphaArgs(7);
        } else {
            Fan(FanNum).EndUseSubcategoryName = "General";
        }

        TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Air Nodes");

    } // end Number of Simple  ON-OFF FAN Loop

    cCurrentModuleObject = "FanPerformance:NightVentilation";
    state.dataFans->NumNightVentPerf = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataFans->NumNightVentPerf > 0) {
        NightVentPerf.allocate(state.dataFans->NumNightVentPerf);
        for (auto &e : NightVentPerf) {
            e.FanName.clear();
            e.FanEff = 0.0;
            e.DeltaPress = 0.0;
            e.MaxAirFlowRate = 0.0;
            e.MotEff = 0.0;
            e.MotInAirFrac = 0.0;
            e.MaxAirMassFlowRate = 0.0;
        }
    }
    // input the night ventilation performance objects
    for (NVPerfNum = 1; NVPerfNum <= state.dataFans->NumNightVentPerf; ++NVPerfNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NVPerfNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        NightVentPerf(NVPerfNum).FanName = cAlphaArgs(1);
        NightVentPerf(NVPerfNum).FanEff = rNumericArgs(1);
        NightVentPerf(NVPerfNum).DeltaPress = rNumericArgs(2);
        NightVentPerf(NVPerfNum).MaxAirFlowRate = rNumericArgs(3);
        NightVentPerf(NVPerfNum).MotEff = rNumericArgs(4);
        NightVentPerf(NVPerfNum).MotInAirFrac = rNumericArgs(5);
        // find the corresponding fan
        NVPerfFanFound = false;
        for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            if (NightVentPerf(NVPerfNum).FanName == Fan(FanNum).FanName) {
                NVPerfFanFound = true;
                Fan(FanNum).NVPerfNum = NVPerfNum;
                break;
            }
        }
        if (!NVPerfFanFound) {
            ShowSevereError(state, cCurrentModuleObject + ", fan name not found=" + cAlphaArgs(1));
            ErrorsFound = true;
        }
    }

    for (CompModelFanNum = 1; CompModelFanNum <= NumCompModelFan; ++CompModelFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + CompModelFanNum;

        cCurrentModuleObject = "Fan:ComponentModel";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 CompModelFanNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        FanNumericFields(FanNum).FieldNames.allocate(MaxNumbers);
        FanNumericFields(FanNum).FieldNames = "";
        FanNumericFields(FanNum).FieldNames = cNumericFieldNames;

        GlobalNames::VerifyUniqueInterObjectName(state, UniqueFanNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        Fan(FanNum).FanName = cAlphaArgs(1); // Fan name
        Fan(FanNum).FanType = cCurrentModuleObject;

        Fan(FanNum).InletNodeNum = GetOnlySingleNode(state,
                                                     cAlphaArgs(2),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::FanComponentModel,
                                                     cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsNotParent); // Air inlet node name
        Fan(FanNum).OutletNodeNum = GetOnlySingleNode(state,
                                                      cAlphaArgs(3),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::FanComponentModel,
                                                      cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsNotParent); // Air outlet node name

        TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Air Nodes");

        Fan(FanNum).AvailSchedName = cAlphaArgs(4); // Availability schedule name
        if (lAlphaFieldBlanks(4)) {
            Fan(FanNum).AvailSchedPtrNum = 0;
        } else {
            Fan(FanNum).AvailSchedPtrNum = GetScheduleIndex(state, cAlphaArgs(4));
            if (Fan(FanNum).AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(4) + " entered =" + cAlphaArgs(4) +
                                    " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        Fan(FanNum).FanType_Num = FanType_ComponentModel;

        Fan(FanNum).MaxAirFlowRate = rNumericArgs(1);
        if (Fan(FanNum).MaxAirFlowRate == 0.0) {
            ShowWarningError(state,
                             cCurrentModuleObject + "=\"" + Fan(FanNum).FanName +
                                 "\" has specified 0.0 max air flow rate. It will not be used in the simulation.");
        }
        Fan(FanNum).MaxAirFlowRateIsAutosizable = true;
        Fan(FanNum).MinAirFlowRate = rNumericArgs(2);

        Fan(FanNum).FanSizingFactor = rNumericArgs(3);                              // Fan max airflow sizing factor [-]
        Fan(FanNum).FanWheelDia = rNumericArgs(4);                                  // Fan wheel outer diameter [m]
        Fan(FanNum).FanOutletArea = rNumericArgs(5);                                // Fan outlet area [m2]
        Fan(FanNum).FanMaxEff = rNumericArgs(6);                                    // Fan maximum static efficiency [-]
        Fan(FanNum).EuMaxEff = rNumericArgs(7);                                     // Euler number at Fan maximum static efficiency [-]
        Fan(FanNum).FanMaxDimFlow = rNumericArgs(8);                                // Fan maximum dimensionless airflow [-]
        Fan(FanNum).PulleyDiaRatio = rNumericArgs(9);                               // Motor/fan pulley diameter ratio [-]
        Fan(FanNum).BeltMaxTorque = rNumericArgs(10);                               // Belt maximum torque [N-m, autosizable]
        Fan(FanNum).BeltSizingFactor = rNumericArgs(11);                            // Belt sizing factor [-]
        Fan(FanNum).BeltTorqueTrans = rNumericArgs(12);                             // Belt fractional torque transition Region 1-2 [-]
        Fan(FanNum).MotorMaxSpd = rNumericArgs(13);                                 // Motor maximum speed [rpm]
        Fan(FanNum).MotorMaxOutPwr = rNumericArgs(14);                              // Motor maximum output power [W, autosizable]
        Fan(FanNum).MotorSizingFactor = rNumericArgs(15);                           // Motor sizing factor [-]
        Fan(FanNum).MotInAirFrac = rNumericArgs(16);                                // Fraction of fan and motor losses to airstream [-]
        Fan(FanNum).VFDEffType = cAlphaArgs(5);                                     // VFD efficiency type [Speed or Power]
        Fan(FanNum).VFDMaxOutPwr = rNumericArgs(17);                                // VFD maximum output power [W, autosizable]
        Fan(FanNum).VFDSizingFactor = rNumericArgs(18);                             // VFD sizing factor [-]
        Fan(FanNum).PressRiseCurveIndex = GetCurveIndex(state, cAlphaArgs(6));      // Fan pressure rise curve
        Fan(FanNum).PressResetCurveIndex = GetCurveIndex(state, cAlphaArgs(7));     // Duct static pressure reset curve
        Fan(FanNum).PLFanEffNormCurveIndex = GetCurveIndex(state, cAlphaArgs(8));   // Fan part-load eff (normal) curve
        Fan(FanNum).PLFanEffStallCurveIndex = GetCurveIndex(state, cAlphaArgs(9));  // Fan part-load eff (stall) curve
        Fan(FanNum).DimFlowNormCurveIndex = GetCurveIndex(state, cAlphaArgs(10));   // Fan dim airflow (normal) curve
        Fan(FanNum).DimFlowStallCurveIndex = GetCurveIndex(state, cAlphaArgs(11));  // Fan dim airflow (stall) curve
        Fan(FanNum).BeltMaxEffCurveIndex = GetCurveIndex(state, cAlphaArgs(12));    // Belt max eff curve
        Fan(FanNum).PLBeltEffReg1CurveIndex = GetCurveIndex(state, cAlphaArgs(13)); // Belt part-load eff Region 1 curve
        Fan(FanNum).PLBeltEffReg2CurveIndex = GetCurveIndex(state, cAlphaArgs(14)); // Belt part-load eff Region 2 curve
        Fan(FanNum).PLBeltEffReg3CurveIndex = GetCurveIndex(state, cAlphaArgs(15)); // Belt part-load eff Region 3 curve
        Fan(FanNum).MotorMaxEffCurveIndex = GetCurveIndex(state, cAlphaArgs(16));   // Motor max eff curve
        Fan(FanNum).PLMotorEffCurveIndex = GetCurveIndex(state, cAlphaArgs(17));    // Motor part-load eff curve
        Fan(FanNum).VFDEffCurveIndex = GetCurveIndex(state, cAlphaArgs(18));        // VFD eff curve

        if (NumAlphas > 18) {
            Fan(FanNum).EndUseSubcategoryName = cAlphaArgs(19);
        } else {
            Fan(FanNum).EndUseSubcategoryName = "General";
        }

    } // end Number of Component Model FAN Loop

    cAlphaArgs.deallocate();
    cAlphaFieldNames.deallocate();
    lAlphaFieldBlanks.deallocate();
    cNumericFieldNames.deallocate();
    lNumericFieldBlanks.deallocate();
    rNumericArgs.deallocate();

    // Check Fans
    for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
        for (checkNum = FanNum + 1; checkNum <= state.dataFans->NumFans; ++checkNum) {
            if (Fan(FanNum).InletNodeNum == Fan(checkNum).InletNodeNum) {
                ErrorsFound = true;
                ShowSevereError(state, "GetFanInput, duplicate fan inlet node names, must be unique for fans.");
                ShowContinueError(state,
                                  "Fan=" + Fan(FanNum).FanType + ':' + Fan(FanNum).FanName + " and Fan=" + Fan(checkNum).FanType + ':' +
                                      Fan(checkNum).FanName + '.');
                ShowContinueError(state, "Inlet Node Name=\"" + state.dataLoopNodes->NodeID(Fan(FanNum).InletNodeNum) + "\".");
            }
            if (Fan(FanNum).OutletNodeNum == Fan(checkNum).OutletNodeNum) {
                ErrorsFound = true;
                ShowSevereError(state, "GetFanInput, duplicate fan outlet node names, must be unique for fans.");
                ShowContinueError(state,
                                  "Fan=" + Fan(FanNum).FanType + ':' + Fan(FanNum).FanName + " and Fan=" + Fan(checkNum).FanType + ':' +
                                      Fan(checkNum).FanName + '.');
                ShowContinueError(state, "Outlet Node Name=\"" + state.dataLoopNodes->NodeID(Fan(FanNum).OutletNodeNum) + "\".");
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Program terminates.");
    }

    for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
        // Setup Report variables for the Fans  CurrentModuleObject='Fans'
        SetupOutputVariable(state,
                            "Fan Electricity Rate",
                            OutputProcessor::Unit::W,
                            Fan(FanNum).FanPower,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            Fan(FanNum).FanName);
        SetupOutputVariable(state,
                            "Fan Rise in Air Temperature",
                            OutputProcessor::Unit::deltaC,
                            Fan(FanNum).DeltaTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            Fan(FanNum).FanName);
        SetupOutputVariable(state,
                            "Fan Heat Gain to Air",
                            OutputProcessor::Unit::W,
                            Fan(FanNum).PowerLossToAir,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            Fan(FanNum).FanName);
        SetupOutputVariable(state,
                            "Fan Electricity Energy",
                            OutputProcessor::Unit::J,
                            Fan(FanNum).FanEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            Fan(FanNum).FanName,
                            _,
                            "Electricity",
                            "Fans",
                            Fan(FanNum).EndUseSubcategoryName,
                            "System");
        SetupOutputVariable(state,
                            "Fan Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            Fan(FanNum).OutletAirMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            Fan(FanNum).FanName);
        if ((Fan(FanNum).FanType_Num == FanType_ZoneExhaust) && (Fan(FanNum).BalancedFractSchedNum > 0)) {
            SetupOutputVariable(state,
                                "Fan Unbalanced Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                Fan(FanNum).UnbalancedOutletMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Fan(FanNum).FanName);
            SetupOutputVariable(state,
                                "Fan Balanced Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                Fan(FanNum).BalancedOutletMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Fan(FanNum).FanName);
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {

            SetupEMSInternalVariable(state, "Fan Maximum Mass Flow Rate", Fan(FanNum).FanName, "[kg/s]", Fan(FanNum).MaxAirMassFlowRate);
            SetupEMSActuator(state,
                             "Fan",
                             Fan(FanNum).FanName,
                             "Fan Air Mass Flow Rate",
                             "[kg/s]",
                             Fan(FanNum).EMSMaxMassFlowOverrideOn,
                             Fan(FanNum).EMSAirMassFlowValue);
            SetupEMSInternalVariable(state, "Fan Nominal Pressure Rise", Fan(FanNum).FanName, "[Pa]", Fan(FanNum).DeltaPress);
            SetupEMSActuator(state,
                             "Fan",
                             Fan(FanNum).FanName,
                             "Fan Pressure Rise",
                             "[Pa]",
                             Fan(FanNum).EMSFanPressureOverrideOn,
                             Fan(FanNum).EMSFanPressureValue);
            SetupEMSInternalVariable(state, "Fan Nominal Total Efficiency", Fan(FanNum).FanName, "[fraction]", Fan(FanNum).FanEff);
            SetupEMSActuator(
                state, "Fan", Fan(FanNum).FanName, "Fan Total Efficiency", "[fraction]", Fan(FanNum).EMSFanEffOverrideOn, Fan(FanNum).EMSFanEffValue);

            SetupEMSActuator(state,
                             "Fan",
                             Fan(FanNum).FanName,
                             "Fan Autosized Air Flow Rate",
                             "[m3/s]",
                             Fan(FanNum).MaxAirFlowRateEMSOverrideOn,
                             Fan(FanNum).MaxAirFlowRateEMSOverrideValue);
        }
    }

    for (OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
        SetupOutputVariable(state,
                            "Fan Runtime Fraction",
                            OutputProcessor::Unit::None,
                            Fan(FanNum).FanRuntimeFraction,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            Fan(FanNum).FanName);
    }

    bool anyRan;
    ManageEMS(state, EMSManager::EMSCallFrom::ComponentGetInput, anyRan, ObjexxFCL::Optional_int_const());
    state.dataFans->MySizeFlag.dimension(state.dataFans->NumFans, true);
}

// End of Get Input subroutines for the HB Module
//******************************************************************************

// Beginning Initialization Section of the Module
//******************************************************************************

void InitFan(EnergyPlusData &state,
             int const FanNum,
             [[maybe_unused]] bool const FirstHVACIteration // unused1208
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the Fan Components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // Using/Aliasing
    using DataZoneEquipment::CheckZoneEquipmentList;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int OutletNode;
    int OutNode;
    int Loop;

    auto &Fan(state.dataFans->Fan);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    if (state.dataFans->MyOneTimeFlag) {

        state.dataFans->MyEnvrnFlag.dimension(state.dataFans->NumFans, true);

        state.dataFans->MyOneTimeFlag = false;
    }

    // need to check all fans to see if they are on Zone Equipment List or issue warning
    if (!state.dataFans->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataFans->ZoneEquipmentListChecked = true;
        for (Loop = 1; Loop <= state.dataFans->NumFans; ++Loop) {
            if (!UtilityRoutines::SameString(Fan(Loop).FanType, "Fan:ZoneExhaust")) continue;
            if (CheckZoneEquipmentList(state, Fan(Loop).FanType, Fan(Loop).FanName)) continue;
            ShowSevereError(state,
                            "InitFans: Fan=[" + Fan(Loop).FanType + ',' + Fan(Loop).FanName +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
        }
    }

    if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(FanNum)) {

        SizeFan(state, FanNum);
        // Set the loop cycling flag
        if (Fan(FanNum).FanType_Num == FanType_SimpleOnOff) {
            if (state.dataSize->CurSysNum > 0) {
                state.dataAirLoop->AirLoopControlInfo(state.dataSize->CurSysNum).CyclingFan = true;
            }
        }

        state.dataFans->MySizeFlag(FanNum) = false;
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataFans->MyEnvrnFlag(FanNum)) {

        // For all Fan inlet nodes convert the Volume flow to a mass flow
        // unused0909    InNode = Fan(FanNum)%InletNodeNum
        OutNode = Fan(FanNum).OutletNodeNum;
        Fan(FanNum).RhoAirStdInit = state.dataEnvrn->StdRhoAir;

        // Change the Volume Flow Rates to Mass Flow Rates

        Fan(FanNum).MaxAirMassFlowRate = Fan(FanNum).MaxAirFlowRate * Fan(FanNum).RhoAirStdInit;
        if (Fan(FanNum).FanMinAirFracMethod == MinFrac) {
            Fan(FanNum).MinAirFlowRate = Fan(FanNum).MaxAirFlowRate * Fan(FanNum).FanMinFrac;
            Fan(FanNum).MinAirMassFlowRate = Fan(FanNum).MinAirFlowRate * Fan(FanNum).RhoAirStdInit;
        } else if (Fan(FanNum).FanMinAirFracMethod == FixedMin) {
            Fan(FanNum).MinAirFlowRate = Fan(FanNum).FanFixedMin;
            Fan(FanNum).MinAirMassFlowRate = Fan(FanNum).MinAirFlowRate * Fan(FanNum).RhoAirStdInit;
        }
        if (Fan(FanNum).NVPerfNum > 0) {
            NightVentPerf(Fan(FanNum).NVPerfNum).MaxAirMassFlowRate = NightVentPerf(Fan(FanNum).NVPerfNum).MaxAirFlowRate * Fan(FanNum).RhoAirStdInit;
        }

        // Init the Node Control variables
        state.dataLoopNodes->Node(OutNode).MassFlowRateMax = Fan(FanNum).MaxAirMassFlowRate;
        // According to the IO Ref guide:
        // "Note that this field is only used to calculate the fan power.
        // This field does not enforce the system air flow rate during simulation"
        // Node(OutNode).MassFlowRateMin = Fan(FanNum).MinAirMassFlowRate;

        // Initialize all report variables to a known state at beginning of simulation
        Fan(FanNum).FanPower = 0.0;
        Fan(FanNum).DeltaTemp = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).FanEnergy = 0.0;

        state.dataFans->MyEnvrnFlag(FanNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataFans->MyEnvrnFlag(FanNum) = true;
    }

    // Do the Begin Day initializations
    // none

    // Do the begin HVAC time step initializations
    // none

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.

    // Do a check and make sure that the max and min available(control) flow is
    // between the physical max and min for the Fan while operating.

    InletNode = Fan(FanNum).InletNodeNum;
    OutletNode = Fan(FanNum).OutletNodeNum;

    Fan(FanNum).MassFlowRateMaxAvail =
        min(state.dataLoopNodes->Node(OutletNode).MassFlowRateMax, state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail);
    Fan(FanNum).MassFlowRateMinAvail =
        min(max(state.dataLoopNodes->Node(OutletNode).MassFlowRateMin, state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail),
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail);

    // Load the node data in this section for the component simulation
    // First need to make sure that the MassFlowRate is between the max and min avail.
    if (Fan(FanNum).FanType_Num != FanType_ZoneExhaust) {
        Fan(FanNum).InletAirMassFlowRate = min(state.dataLoopNodes->Node(InletNode).MassFlowRate, Fan(FanNum).MassFlowRateMaxAvail);
        Fan(FanNum).InletAirMassFlowRate = max(Fan(FanNum).InletAirMassFlowRate, Fan(FanNum).MassFlowRateMinAvail);
    } else { // zone exhaust fans
        Fan(FanNum).MassFlowRateMaxAvail = Fan(FanNum).MaxAirMassFlowRate;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;
        if (Fan(FanNum).FlowFractSchedNum > 0) { // modulate flow
            Fan(FanNum).InletAirMassFlowRate = Fan(FanNum).MassFlowRateMaxAvail * GetCurrentScheduleValue(state, Fan(FanNum).FlowFractSchedNum);
            Fan(FanNum).InletAirMassFlowRate = max(0.0, Fan(FanNum).InletAirMassFlowRate);
        } else { // always run at max
            Fan(FanNum).InletAirMassFlowRate = Fan(FanNum).MassFlowRateMaxAvail;
        }
        if (Fan(FanNum).EMSMaxMassFlowOverrideOn)
            Fan(FanNum).InletAirMassFlowRate = min(Fan(FanNum).EMSAirMassFlowValue, Fan(FanNum).MassFlowRateMaxAvail);
    }

    // Then set the other conditions
    Fan(FanNum).InletAirTemp = state.dataLoopNodes->Node(InletNode).Temp;
    Fan(FanNum).InletAirHumRat = state.dataLoopNodes->Node(InletNode).HumRat;
    Fan(FanNum).InletAirEnthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;
}

void SizeFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2001
    //       MODIFIED       Craig Wray August 2010 - added fan, belt, motor, and VFD component sizing
    //                      August 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing fans for which flow rates have not been
    // specified in the input, or when fan component sizes have not been specified

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays.

    // Using/Aliasing
    using namespace DataSizing;
    using namespace OutputReportPredefined;
    using CurveManager::CurveValue;
    using CurveManager::GetCurveIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeFan: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NVPerfNum;              // Index to night ventialation performance object
    std::string equipName;      // Equipment name
    Real64 RatedPower;          // Rated fan power [W]
    Real64 RhoAir;              // Air density [kg/m3]
    Real64 FanVolFlow;          // Fan volumetric airflow [m3/s]
    Real64 DuctStaticPress;     // Duct static pressure setpoint [Pa]
    Real64 DeltaPressTot;       // Total pressure rise across fan [N/m2 = Pa]
    Real64 FanOutletVelPress;   // Fan outlet velocity pressure [Pa]
    Real64 EulerNum;            // Fan Euler number [-]
    Real64 NormalizedEulerNum;  // Normalized Fan Euler number [-]
    Real64 FanDimFlow;          // Fan dimensionless airflow [-]
    Real64 FanSpdRadS;          // Fan shaft rotational speed [rad/s]
    Real64 MotorSpeed;          // Motor shaft rotational speed [rpm]
    Real64 XbeltMax;            // Factor for belt max eff curve [ln hp]
    Real64 FanTrqRatio;         // Ratio of fan torque to max fan torque [-]
    Real64 BeltPLEff;           // Belt normalized (part-load) efficiency [-]
    Real64 XmotorMax;           // Factor for motor max eff curve [ln hp]
    Real64 MotorOutPwrRatio;    // Ratio of motor output power to max motor output power [-]
    Real64 MotorPLEff;          // Motor normalized (part-load) efficiency [-]
    Real64 VFDSpdRatio(0.0);    // Ratio of motor speed to motor max speed [-]
    Real64 VFDOutPwrRatio(0.0); // Ratio of VFD output power to max VFD output power [-]
    std::string CompName;       // component name
    std::string CompType;       // component type
    std::string SizingString;   // input field sizing description (e.g., Nominal Capacity)
    bool bPRINT = true;         // TRUE if sizing is reported to output (eio)
    Real64 TempFlow;            // autosized flow rate of fan [m3/s]
    int FieldNum = 2;           // IDD numeric field number where input field description is found
    int NumFansSized = 0;       // counter used to deallocate temporary string array after all fans have been sized

    auto &Fan(state.dataFans->Fan);
    auto &FanNumericFields(state.dataFans->FanNumericFields);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    if (Fan(FanNum).FanType_Num == FanType_ComponentModel) {
        FieldNum = 1;
    } else {
        FieldNum = 3;
    }
    SizingString = FanNumericFields(FanNum).FieldNames(FieldNum) + " [m3/s]";

    TempFlow = Fan(FanNum).MaxAirFlowRate;
    state.dataSize->DataAutosizable = Fan(FanNum).MaxAirFlowRateIsAutosizable;
    CompType = Fan(FanNum).FanType;
    CompName = Fan(FanNum).FanName;
    state.dataSize->DataEMSOverrideON = Fan(FanNum).MaxAirFlowRateEMSOverrideOn;
    state.dataSize->DataEMSOverride = Fan(FanNum).MaxAirFlowRateEMSOverrideValue;

    bool errorsFound = false;
    SystemAirFlowSizer sizerSystemAirFlow;
    sizerSystemAirFlow.overrideSizingString(SizingString);
    sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
    Fan(FanNum).MaxAirFlowRate = sizerSystemAirFlow.size(state, TempFlow, errorsFound);

    state.dataSize->DataAutosizable = true;
    state.dataSize->DataEMSOverrideON = false;
    state.dataSize->DataEMSOverride = 0.0;

    FanVolFlow = Fan(FanNum).MaxAirFlowRate; // Maximum volumetric airflow through fan [m3/s at standard conditions]
    if (Fan(FanNum).FanType_Num == FanType_ComponentModel) {
        // Get air density at standard conditions and get mass airflow through fan
        // From WeatherManager:
        //   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
        //   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
        // From PsychRoutines:
        //   w=MAX(dw,1.0d-5)
        //   rhoair = pb/(287.d0*(tdb+DataGlobalConstants::KelvinConv())*(1.0d0+1.6077687d0*w))
        RhoAir = state.dataEnvrn->StdRhoAir;

        // Adjust max fan volumetric airflow using fan sizing factor
        FanVolFlow *= Fan(FanNum).FanSizingFactor; //[m3/s at standard conditions]

        // Calculate max fan static pressure rise using max fan volumetric flow, std air density, air-handling system characteristics,
        //   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
        DuctStaticPress = CurveValue(state, Fan(FanNum).PressResetCurveIndex, FanVolFlow);               // Duct static pressure setpoint [Pa]
        DeltaPressTot = CurveValue(state, Fan(FanNum).PressRiseCurveIndex, FanVolFlow, DuctStaticPress); // Max fan total pressure rise [Pa]
        FanOutletVelPress = 0.5 * RhoAir * pow_2(FanVolFlow / Fan(FanNum).FanOutletArea);                // Max fan outlet velocity pressure [Pa]
        // Outlet velocity pressure cannot exceed total pressure rise
        FanOutletVelPress = min(FanOutletVelPress, DeltaPressTot);
        Fan(FanNum).DeltaPress = DeltaPressTot - FanOutletVelPress; // Max fan static pressure rise [Pa]

        // Calculate max fan air power using volumetric flow abd corresponding fan static pressure rise
        Fan(FanNum).FanAirPower = FanVolFlow * Fan(FanNum).DeltaPress; //[W]

        // Calculate fan wheel efficiency at max fan volumetric flow and corresponding fan static pressure rise,
        //   using fan characteristics and Wray dimensionless fan static efficiency model
        EulerNum = (Fan(FanNum).DeltaPress * pow_4(Fan(FanNum).FanWheelDia)) / (RhoAir * pow_2(FanVolFlow)); //[-]
        NormalizedEulerNum = std::log10(EulerNum / Fan(FanNum).EuMaxEff);
        if (NormalizedEulerNum <= 0.0) {
            Fan(FanNum).FanWheelEff = CurveValue(state, Fan(FanNum).PLFanEffNormCurveIndex, NormalizedEulerNum);
        } else {
            Fan(FanNum).FanWheelEff = CurveValue(state, Fan(FanNum).PLFanEffStallCurveIndex, NormalizedEulerNum);
        }
        Fan(FanNum).FanWheelEff *= Fan(FanNum).FanMaxEff;             // [-]
        Fan(FanNum).FanWheelEff = max(Fan(FanNum).FanWheelEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate max fan shaft power using fan air power and fan efficiency
        // at max fan static pressure rise and max fan volumetric flow
        Fan(FanNum).FanShaftPower = (Fan(FanNum).FanAirPower / Fan(FanNum).FanWheelEff); //[W]
        Fan(FanNum).FanShaftPwrMax = Fan(FanNum).FanShaftPower;                          //[W]

        // Calculate fan shaft speed, motor speed, and fan torque using Wray dimensionless fan airflow model
        if (NormalizedEulerNum <= 0.0) {
            FanDimFlow = CurveValue(state, Fan(FanNum).DimFlowNormCurveIndex, NormalizedEulerNum); //[-]
        } else {
            FanDimFlow = CurveValue(state, Fan(FanNum).DimFlowStallCurveIndex, NormalizedEulerNum); //[-]
        }
        FanSpdRadS = FanVolFlow / (FanDimFlow * Fan(FanNum).FanMaxDimFlow * pow_3(Fan(FanNum).FanWheelDia)); //[rad/s]
        Fan(FanNum).FanSpd = FanSpdRadS * 9.549296586;                                                       //[rpm, conversion factor is 30/PI]

        if (Fan(FanNum).PulleyDiaRatio == AutoSize) {
            // WRITE(*,*) 'Autosizing pulley drive ratio'
            Fan(FanNum).PulleyDiaRatio = Fan(FanNum).FanSpd / Fan(FanNum).MotorMaxSpd; //[-]
        }

        // For direct-drive, should have PulleyDiaRatio = 1
        MotorSpeed = Fan(FanNum).FanSpd / Fan(FanNum).PulleyDiaRatio; //[rpm]

        // Check for inconsistent drive ratio and motor speed, and report design fan speed with warning
        if (MotorSpeed > (Fan(FanNum).MotorMaxSpd + 1.e-5)) {
            ShowWarningError(state,
                             "Drive ratio for " + Fan(FanNum).FanType + ": " + Fan(FanNum).FanName +
                                 " is too low at design conditions -- check motor speed and drive ratio inputs");
            ShowContinueError(state, format("...Design fan speed [rev/min]: {:.2R}", Fan(FanNum).FanSpd));
        }

        Fan(FanNum).FanTrq = Fan(FanNum).FanShaftPower / FanSpdRadS; //[N-m]

        if (Fan(FanNum).BeltMaxTorque == AutoSize) {
            // WRITE(*,*) 'Autosizing fan belt'
            Fan(FanNum).BeltMaxTorque = Fan(FanNum).FanTrq; //[N-m]
        }
        // Adjust max belt torque using belt sizing factor
        Fan(FanNum).BeltMaxTorque *= Fan(FanNum).BeltSizingFactor; //[N-m]

        // Check for undersized belt and report design size with warning
        if (Fan(FanNum).FanTrq > (Fan(FanNum).BeltMaxTorque + 1.e-5)) {
            ShowWarningError(
                state, "Belt for " + Fan(FanNum).FanType + ": " + Fan(FanNum).FanName + " is undersized at design conditions -- check belt inputs");
            ShowContinueError(state, format("...Design belt output torque (without oversizing) [Nm]: {:.2R}", Fan(FanNum).FanTrq));
        }

        // Calculate belt max efficiency using correlations and coefficients based on AMCA data
        // Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
        XbeltMax = std::log(Fan(FanNum).FanShaftPwrMax / 746.0); // Natural log of belt output power in hp
        if (Fan(FanNum).BeltMaxEffCurveIndex != 0) {
            Fan(FanNum).BeltMaxEff = std::exp(CurveValue(state, Fan(FanNum).BeltMaxEffCurveIndex, XbeltMax)); //[-]
        } else {
            Fan(FanNum).BeltMaxEff = 1.0; // No curve specified - use constant efficiency
        }

        // Calculate belt part-load drive efficiency and input power using correlations and coefficients based on ACEEE data
        FanTrqRatio = Fan(FanNum).FanTrq / Fan(FanNum).BeltMaxTorque; //[-]
        if ((FanTrqRatio <= Fan(FanNum).BeltTorqueTrans) && (Fan(FanNum).PLBeltEffReg1CurveIndex != 0)) {
            BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg1CurveIndex, FanTrqRatio); //[-]
        } else {
            if ((FanTrqRatio > Fan(FanNum).BeltTorqueTrans) && (FanTrqRatio <= 1.0) && (Fan(FanNum).PLBeltEffReg2CurveIndex != 0)) {
                BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg2CurveIndex, FanTrqRatio); //[-]
            } else {
                if ((FanTrqRatio > 1.0) && (Fan(FanNum).PLBeltEffReg3CurveIndex != 0)) {
                    BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg3CurveIndex, FanTrqRatio); //[-]
                } else {
                    BeltPLEff = 1.0; // Direct drive or no curve specified - use constant efficiency
                }
            }
        }
        Fan(FanNum).BeltEff = Fan(FanNum).BeltMaxEff * BeltPLEff;                     //[-]
        Fan(FanNum).BeltEff = max(Fan(FanNum).BeltEff, 0.01);                         // Minimum efficiency is 1% to avoid numerical errors
        Fan(FanNum).BeltInputPower = Fan(FanNum).FanShaftPower / Fan(FanNum).BeltEff; //[W]

        if (Fan(FanNum).MotorMaxOutPwr == AutoSize) {
            // WRITE(*,*) 'Autosizing fan motor'
            Fan(FanNum).MotorMaxOutPwr = Fan(FanNum).BeltInputPower;
        }
        // Adjust max motor output power using motor sizing factor
        Fan(FanNum).MotorMaxOutPwr *= Fan(FanNum).MotorSizingFactor; //[W]

        // Check for undersized motor and report design size with warning
        if (Fan(FanNum).BeltInputPower > (Fan(FanNum).MotorMaxOutPwr + 1.e-5)) {
            ShowWarningError(
                state, "Motor for " + Fan(FanNum).FanType + ": " + Fan(FanNum).FanName + " is undersized at design conditions -- check motor inputs");
            ShowContinueError(state, format("...Design motor output power (without oversizing) [W]: {:.2R}", Fan(FanNum).BeltInputPower));
        }

        // Calculate motor max efficiency using correlations and coefficients based on MotorMaster+ data
        XmotorMax = std::log(Fan(FanNum).MotorMaxOutPwr / 746.0); // Natural log of motor output power in hp
        if (Fan(FanNum).MotorMaxEffCurveIndex != 0) {
            Fan(FanNum).MotorMaxEff = CurveValue(state, Fan(FanNum).MotorMaxEffCurveIndex, XmotorMax); //[-]
        } else {
            Fan(FanNum).MotorMaxEff = 1.0; // No curve specified - use constant efficiency
        }

        // Calculate motor part-load efficiency and input power using correlations and coefficients based on MotorMaster+ data
        MotorOutPwrRatio = Fan(FanNum).BeltInputPower / Fan(FanNum).MotorMaxOutPwr; //[-]
        if (Fan(FanNum).PLMotorEffCurveIndex != 0) {
            MotorPLEff = CurveValue(state, Fan(FanNum).PLMotorEffCurveIndex, MotorOutPwrRatio); //[-]
        } else {
            MotorPLEff = 1.0; // No curve specified - use constant efficiency
        }
        Fan(FanNum).MotEff = Fan(FanNum).MotorMaxEff * MotorPLEff; //[-]
        Fan(FanNum).MotEff = max(Fan(FanNum).MotEff, 0.01);        // Minimum efficiency is 1% to avoid numerical errors

        // Calculate motor input power using belt input power and motor efficiency
        Fan(FanNum).MotorInputPower = Fan(FanNum).BeltInputPower / Fan(FanNum).MotEff; //[W]

        // Calculate max VFD efficiency and input power using correlations and coefficients based on VFD type
        if ((Fan(FanNum).VFDEffType == "SPEED") && (Fan(FanNum).VFDEffCurveIndex != 0)) {
            VFDSpdRatio = MotorSpeed / Fan(FanNum).MotorMaxSpd;                                //[-]
            Fan(FanNum).VFDEff = CurveValue(state, Fan(FanNum).VFDEffCurveIndex, VFDSpdRatio); //[-]
        } else {
            if ((Fan(FanNum).VFDEffType == "POWER") && (Fan(FanNum).VFDEffCurveIndex != 0)) {
                if (Fan(FanNum).VFDMaxOutPwr == AutoSize) {
                    // WRITE(*,*) 'Autosizing fan VFD'
                    Fan(FanNum).VFDMaxOutPwr = Fan(FanNum).MotorInputPower;
                }
                // Adjust max VFD output power using VFD sizing factor
                Fan(FanNum).VFDMaxOutPwr *= Fan(FanNum).VFDSizingFactor; //[W]

                // Check for undersized VFD and report design size with warning
                if (Fan(FanNum).MotorInputPower > (Fan(FanNum).VFDMaxOutPwr + 1.e-5)) {
                    ShowWarningError(state,
                                     "VFD for " + Fan(FanNum).FanType + ": " + Fan(FanNum).FanName +
                                         " is undersized at design conditions -- check VFD inputs");
                    ShowContinueError(state, format("...Design VFD output power (without oversizing) [W]: {:.2R}", Fan(FanNum).MotorInputPower));
                }

                VFDOutPwrRatio = Fan(FanNum).MotorInputPower / Fan(FanNum).VFDMaxOutPwr;              //[-]
                Fan(FanNum).VFDEff = CurveValue(state, Fan(FanNum).VFDEffCurveIndex, VFDOutPwrRatio); //[-]
            } else {
                // No curve specified - use constant efficiency
                Fan(FanNum).VFDMaxOutPwr = 0.0;
                Fan(FanNum).VFDEff = 0.97;
            }
        }
        Fan(FanNum).VFDEff = max(Fan(FanNum).VFDEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate VFD "rated" input power using motor input power and VFD efficiency
        RatedPower = Fan(FanNum).MotorInputPower / Fan(FanNum).VFDEff; //[W]

        // Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
        // Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
        Fan(FanNum).FanEff = Fan(FanNum).FanWheelEff * Fan(FanNum).BeltEff * Fan(FanNum).MotEff * Fan(FanNum).VFDEff;

        // Report fan, belt, motor, and VFD characteristics at design condition to .eio file
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Airflow [m3/s]", FanVolFlow);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Static Pressure Rise [Pa]", Fan(FanNum).DeltaPress);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Shaft Power [W]", Fan(FanNum).FanShaftPower);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Motor Output Power [W]", Fan(FanNum).MotorMaxOutPwr);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design VFD Output Power [W]", Fan(FanNum).VFDMaxOutPwr);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Rated Power [W]", RatedPower);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Drive Ratio []", Fan(FanNum).PulleyDiaRatio);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Belt Output Torque [Nm]", Fan(FanNum).BeltMaxTorque);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Efficiency  []", Fan(FanNum).FanWheelEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Maximum Belt Efficiency []", Fan(FanNum).BeltMaxEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Belt Efficiency []", Fan(FanNum).BeltEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Maximum Motor Efficiency []", Fan(FanNum).MotorMaxEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Motor Efficiency []", Fan(FanNum).MotEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design VFD Efficiency []", Fan(FanNum).VFDEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Combined Efficiency []", Fan(FanNum).FanEff);
    } // End fan component sizing

    equipName = Fan(FanNum).FanName;

    // Rearrange order to match table and use FanVolFlow to calculate RatedPower
    // ALSO generates values if Component Model fan, for which DeltaPress and FanEff vary with flow
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanType, equipName, Fan(FanNum).FanType);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanTotEff, equipName, Fan(FanNum).FanEff);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanDeltaP, equipName, Fan(FanNum).DeltaPress);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanVolFlow, equipName, FanVolFlow);
    RatedPower = FanVolFlow * Fan(FanNum).DeltaPress / Fan(FanNum).FanEff; // total fan power
    if (Fan(FanNum).FanType_Num != FanType_ComponentModel) {
        Fan(FanNum).DesignPointFEI =
            HVACFan::FanSystem::report_fei(state, FanVolFlow, RatedPower, Fan(FanNum).DeltaPress, state.dataEnvrn->StdRhoAir);
    }
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwr, equipName, RatedPower);
    if (FanVolFlow != 0.0) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwrPerFlow, equipName, RatedPower / FanVolFlow);
    }
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorIn, equipName, Fan(FanNum).MotInAirFrac);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEndUse, equipName, Fan(FanNum).EndUseSubcategoryName);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEnergyIndex, equipName, Fan(FanNum).DesignPointFEI);

    NVPerfNum = Fan(FanNum).NVPerfNum;
    if (NVPerfNum > 0) {
        if (NightVentPerf(NVPerfNum).MaxAirFlowRate == AutoSize) {
            NightVentPerf(NVPerfNum).MaxAirFlowRate = Fan(FanNum).MaxAirFlowRate;
        }
    }

    // Now that sizing is done, do check if the design point of fan is covered in the fault Fan Curve
    if (Fan(FanNum).FaultyFilterFlag) {
        int jFault_AirFilter = Fan(FanNum).FaultyFilterIndex;

        // Check fault availability schedules
        if (!state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).CheckFaultyAirFilterFanCurve(state)) {
            ShowSevereError(state, "FaultModel:Fouling:AirFilter = \"" + state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).Name + "\"");
            ShowContinueError(state,
                              "Invalid Fan Curve Name = \"" + state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurve +
                                  "\" does not cover ");
            ShowContinueError(state, "the operational point of Fan " + Fan(FanNum).FanName);
            ShowFatalError(state,
                           "SizeFan: Invalid FaultModel:Fouling:AirFilter=" + state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).Name);
        }
    }

    if (++NumFansSized == state.dataFans->NumFans) FanNumericFields.deallocate(); // remove temporary array for field names at end of sizing
}

void SimSimpleFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Brent Griffith, May 2009, added EMS override
    //                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the simple constant volume fan.

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RhoAir;
    Real64 DeltaPress; // [N/m2]
    Real64 FanEff;
    Real64 MotInAirFrac;
    Real64 MotEff;
    Real64 MassFlow;      // [kg/sec]
    Real64 FanShaftPower; // power delivered to fan shaft

    int NVPerfNum;

    auto &Fan(state.dataFans->Fan);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    NVPerfNum = Fan(FanNum).NVPerfNum;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        DeltaPress = NightVentPerf(NVPerfNum).DeltaPress;
        FanEff = NightVentPerf(NVPerfNum).FanEff;
        MotEff = NightVentPerf(NVPerfNum).MotEff;
        MotInAirFrac = NightVentPerf(NVPerfNum).MotInAirFrac;
    } else {
        DeltaPress = Fan(FanNum).DeltaPress;
        FanEff = Fan(FanNum).FanEff;
        MotEff = Fan(FanNum).MotEff;
        MotInAirFrac = Fan(FanNum).MotInAirFrac;
    }

    // For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
    // unused0909   Tin        = Fan(FanNum)%InletAirTemp
    // unused0909   Win        = Fan(FanNum)%InletAirHumRat
    RhoAir = Fan(FanNum).RhoAirStdInit;
    MassFlow = Fan(FanNum).InletAirMassFlowRate;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (Fan(FanNum).FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
        (!state.dataGlobal->KickOffSimulation)) {

        int iFault = Fan(FanNum).FaultyFilterIndex;

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec = CalFaultyFanAirFlowReduction(
                state,
                Fan(FanNum).FanName,
                Fan(FanNum).MaxAirFlowRate,
                Fan(FanNum).DeltaPress,
                (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) - 1) *
                    Fan(FanNum).DeltaPress,
                state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MassFlow = min(MassFlow, Fan(FanNum).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir);
            DeltaPress = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                         Fan(FanNum).DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (Fan(FanNum).EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum).EMSAirMassFlowValue;
    if (Fan(FanNum).EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum).EMSFanPressureValue;
    if (Fan(FanNum).EMSFanEffOverrideOn) FanEff = Fan(FanNum).EMSFanEffValue;

    MassFlow = min(MassFlow, Fan(FanNum).MaxAirMassFlowRate);
    MassFlow = max(MassFlow, Fan(FanNum).MinAirMassFlowRate);

    // Determine the Fan Schedule for the Time step
    if ((GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0 || state.dataFans->LocalTurnFansOn) &&
        !state.dataFans->LocalTurnFansOff && MassFlow > 0.0) {
        // Fan is operating
        Fan(FanNum).FanPower = max(0.0, MassFlow * DeltaPress / (FanEff * RhoAir)); // total fan power
        FanShaftPower = MotEff * Fan(FanNum).FanPower;                              // power delivered to shaft
        Fan(FanNum).PowerLossToAir = FanShaftPower + (Fan(FanNum).FanPower - FanShaftPower) * MotInAirFrac;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + Fan(FanNum).PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirMassFlowRate = MassFlow;
        Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        Fan(FanNum).FanPower = 0.0;
        FanShaftPower = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).OutletAirMassFlowRate = 0.0;
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy;
        Fan(FanNum).OutletAirTemp = Fan(FanNum).InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        Fan(FanNum).MassFlowRateMaxAvail = 0.0;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;
    }
}

void SimVariableVolumeFan(EnergyPlusData &state, int const FanNum, Optional<Real64 const> PressureRise)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Phil Haves
    //                      Brent Griffith, May 2009 for EMS
    //                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the simple variable volume fan.

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.
    // Uses curves of fan power fraction vs. fan part load to determine fan power at
    // off design conditions.

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RhoAir;
    Real64 DeltaPress; // [N/m2 = Pa]
    Real64 FanEff;     // Total fan efficiency - combined efficiency of fan, drive train,
    // motor and variable speed controller (if any)
    Real64 MaxAirFlowRate;
    Real64 MaxAirMassFlowRate;
    Real64 MotInAirFrac;
    Real64 MotEff;
    Real64 MassFlow; // [kg/sec]
    Real64 PartLoadFrac;
    Real64 MinFlowFrac;           // Variable Volume Fan Min Flow Fraction [-]
    Real64 FlowFracForPower(0.0); // Variable Volume Fan Flow Fraction for power calcs[-]
    Real64 FlowFracActual(0.0);   // actual VAV fan flow fraction
    Real64 FanShaftPower;         // power delivered to fan shaft
    int NVPerfNum;

    // added to address the fan heat issue during low air flow conditions
    Real64 MinFlowFracLimitFanHeat; // Minimum Fan Flow Fraction Limit for Fan Heat at Low Airflow [-]
    Real64 FanPoweratLowMinimum;    // Fan Power at Low Minimum Airflow [W]
    Real64 PartLoadFracatLowMin;
    Real64 DeltaTAcrossFan; // Air temperature rise across the fan due to fan heat [C]

    // Simple Variable Volume Fan - default values from DOE-2
    // Type of Fan          Coeff1       Coeff2       Coeff3        Coeff4      Coeff5
    // INLET VANE DAMPERS   0.35071223   0.30850535   -0.54137364   0.87198823  0.000
    // DISCHARGE DAMPERS    0.37073425   0.97250253   -0.34240761   0.000       0.000
    // VARIABLE SPEED MOTOR 0.0015302446 0.0052080574  1.1086242   -0.11635563  0.000

    auto &Fan(state.dataFans->Fan);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    NVPerfNum = Fan(FanNum).NVPerfNum;
    MaxAirFlowRate = Fan(FanNum).MaxAirFlowRate;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        DeltaPress = NightVentPerf(NVPerfNum).DeltaPress;
        FanEff = NightVentPerf(NVPerfNum).FanEff;
        MotEff = NightVentPerf(NVPerfNum).MotEff;
        MotInAirFrac = NightVentPerf(NVPerfNum).MotInAirFrac;
        MaxAirMassFlowRate = NightVentPerf(NVPerfNum).MaxAirMassFlowRate;
    } else {
        if (present(PressureRise)) {
            DeltaPress = PressureRise;
        } else {
            DeltaPress = Fan(FanNum).DeltaPress;
        }
        FanEff = Fan(FanNum).FanEff;
        MotEff = Fan(FanNum).MotEff;
        MotInAirFrac = Fan(FanNum).MotInAirFrac;
        MaxAirMassFlowRate = Fan(FanNum).MaxAirMassFlowRate;
    }

    // unused0909  Tin         = Fan(FanNum)%InletAirTemp
    // unused0909  Win         = Fan(FanNum)%InletAirHumRat
    RhoAir = Fan(FanNum).RhoAirStdInit;
    MassFlow = Fan(FanNum).InletAirMassFlowRate;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (Fan(FanNum).FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
        (!state.dataGlobal->KickOffSimulation) && (!Fan(FanNum).EMSMaxMassFlowOverrideOn)) {

        int iFault = Fan(FanNum).FaultyFilterIndex;

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec = CalFaultyFanAirFlowReduction(
                state,
                Fan(FanNum).FanName,
                Fan(FanNum).MaxAirFlowRate,
                Fan(FanNum).DeltaPress,
                (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) - 1) *
                    Fan(FanNum).DeltaPress,
                state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MaxAirFlowRate = Fan(FanNum).MaxAirFlowRate - FanDesignFlowRateDec;
            MaxAirMassFlowRate = Fan(FanNum).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
            DeltaPress = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                         Fan(FanNum).DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (Fan(FanNum).EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum).EMSFanPressureValue;
    if (Fan(FanNum).EMSFanEffOverrideOn) FanEff = Fan(FanNum).EMSFanEffValue;
    if (Fan(FanNum).EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum).EMSAirMassFlowValue;

    MassFlow = min(MassFlow, MaxAirMassFlowRate);

    // Determine the Fan Schedule for the Time step
    if ((GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0 || state.dataFans->LocalTurnFansOn) &&
        !state.dataFans->LocalTurnFansOff && MassFlow > 0.0) {
        // Fan is operating - calculate power loss and enthalpy rise
        //  Fan(FanNum)%FanPower = PartLoadFrac*FullMassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
        // Calculate and check limits on fraction of system flow
        // unused0909    MaxFlowFrac = 1.0
        // MinFlowFrac is calculated from the ration of the volume flows and is non-dimensional
        MinFlowFrac = Fan(FanNum).MinAirFlowRate / MaxAirFlowRate;
        // The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
        FlowFracActual = MassFlow / MaxAirMassFlowRate;

        // Calculate the part Load Fraction             (PH 7/13/03)

        FlowFracForPower = max(MinFlowFrac, min(FlowFracActual, 1.0)); // limit flow fraction to allowed range
        if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
            PartLoadFrac = 1.0;
        } else {
            PartLoadFrac = Fan(FanNum).FanCoeff(1) + Fan(FanNum).FanCoeff(2) * FlowFracForPower + Fan(FanNum).FanCoeff(3) * pow_2(FlowFracForPower) +
                           Fan(FanNum).FanCoeff(4) * pow_3(FlowFracForPower) + Fan(FanNum).FanCoeff(5) * pow_4(FlowFracForPower);
        }

        Fan(FanNum).FanPower = max(0.0, PartLoadFrac * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir)); // total fan power (PH 7/13/03)

        FanShaftPower = MotEff * Fan(FanNum).FanPower; // power delivered to shaft
        Fan(FanNum).PowerLossToAir = FanShaftPower + (Fan(FanNum).FanPower - FanShaftPower) * MotInAirFrac;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + Fan(FanNum).PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirMassFlowRate = MassFlow;
        Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);

        // KHL/FB, 2/10/2011. NFP implemented as CR 8338.
        // When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
        //  avoid the unrealistic high temperature rise across the fan.
        // TH, 2/15/2011
        // This change caused diffs for VAV systems when fan runs at less than 10% flow conditions.
        //  A potential way to improve is to check the temperature rise across the fan first,
        //  if it is too high (say > 20C) then applies the code.
        DeltaTAcrossFan = Fan(FanNum).OutletAirTemp - Fan(FanNum).InletAirTemp;
        if (DeltaTAcrossFan > 20.0) {
            MinFlowFracLimitFanHeat = 0.10;
            if (FlowFracForPower < MinFlowFracLimitFanHeat) {
                PartLoadFracatLowMin = Fan(FanNum).FanCoeff(1) + Fan(FanNum).FanCoeff(2) * MinFlowFracLimitFanHeat +
                                       Fan(FanNum).FanCoeff(3) * pow_2(MinFlowFracLimitFanHeat) +
                                       Fan(FanNum).FanCoeff(4) * pow_3(MinFlowFracLimitFanHeat) +
                                       Fan(FanNum).FanCoeff(5) * pow_4(MinFlowFracLimitFanHeat);
                FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir);
                Fan(FanNum).FanPower = max(0.0, FlowFracForPower * FanPoweratLowMinimum / MinFlowFracLimitFanHeat);
            } else if (FlowFracActual < MinFlowFracLimitFanHeat) {
                PartLoadFracatLowMin = Fan(FanNum).FanCoeff(1) + Fan(FanNum).FanCoeff(2) * MinFlowFracLimitFanHeat +
                                       Fan(FanNum).FanCoeff(3) * pow_2(MinFlowFracLimitFanHeat) +
                                       Fan(FanNum).FanCoeff(4) * pow_3(MinFlowFracLimitFanHeat) +
                                       Fan(FanNum).FanCoeff(5) * pow_4(MinFlowFracLimitFanHeat);
                FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir);
                Fan(FanNum).FanPower = max(0.0, FlowFracActual * FanPoweratLowMinimum / MinFlowFracLimitFanHeat);
            }
            FanShaftPower = MotEff * Fan(FanNum).FanPower; // power delivered to shaft
            Fan(FanNum).PowerLossToAir = FanShaftPower + (Fan(FanNum).FanPower - FanShaftPower) * MotInAirFrac;
            Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + Fan(FanNum).PowerLossToAir / MassFlow;
            // This fan does not change the moisture or Mass Flow across the component
            Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
            Fan(FanNum).OutletAirMassFlowRate = MassFlow;
            Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);
        }

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        Fan(FanNum).FanPower = 0.0;
        FanShaftPower = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).OutletAirMassFlowRate = 0.0;
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy;
        Fan(FanNum).OutletAirTemp = Fan(FanNum).InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        Fan(FanNum).MassFlowRateMaxAvail = 0.0;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;
    }
}

void SimOnOffFan(EnergyPlusData &state, int const FanNum, Optional<Real64 const> SpeedRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Shirey, May 2001
    //                      R. Raustad - FSEC, Jan 2009 - added SpeedRatio for multi-speed fans
    //                      Brent Griffith, May 2009 for EMS
    //                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the simple on/off fan.

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.
    // Uses curves of fan power fraction vs. fan part load to determine fan power at
    // off design conditions.
    // Same as simple (constant volume) fan, except added part-load curve input

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // Using/Aliasing
    using CurveManager::CurveValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RhoAir;
    Real64 DeltaPress; // [N/m2]
    Real64 FanEff;
    Real64 MassFlow;             // [kg/sec]
    Real64 MaxAirMassFlowRate;   // [kg/sec]
    Real64 PartLoadRatio;        // Ratio of actual mass flow rate to max mass flow rate
    Real64 FlowFrac;             // Actual Fan Flow Fraction = actual mass flow rate / max air mass flow rate
    Real64 FanShaftPower;        // power delivered to fan shaft
    Real64 SpeedRaisedToPower;   // Result of the speed ratio raised to the power of n (Curve object)
    Real64 EffRatioAtSpeedRatio; // Efficiency ratio at current speed ratio (Curve object)

    auto &Fan(state.dataFans->Fan);

    MassFlow = Fan(FanNum).InletAirMassFlowRate;
    MaxAirMassFlowRate = Fan(FanNum).MaxAirMassFlowRate;
    DeltaPress = Fan(FanNum).DeltaPress;
    FanEff = Fan(FanNum).FanEff;
    RhoAir = Fan(FanNum).RhoAirStdInit;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (Fan(FanNum).FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
        (!state.dataGlobal->KickOffSimulation) && (!Fan(FanNum).EMSMaxMassFlowOverrideOn)) {

        int iFault = Fan(FanNum).FaultyFilterIndex;

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec = CalFaultyFanAirFlowReduction(
                state,
                Fan(FanNum).FanName,
                Fan(FanNum).MaxAirFlowRate,
                Fan(FanNum).DeltaPress,
                (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) - 1) *
                    Fan(FanNum).DeltaPress,
                state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MaxAirMassFlowRate = Fan(FanNum).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
            DeltaPress = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                         Fan(FanNum).DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (Fan(FanNum).EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum).EMSAirMassFlowValue;
    if (Fan(FanNum).EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum).EMSFanPressureValue;
    if (Fan(FanNum).EMSFanEffOverrideOn) FanEff = Fan(FanNum).EMSFanEffValue;

    MassFlow = min(MassFlow, MaxAirMassFlowRate);
    MassFlow = max(MassFlow, Fan(FanNum).MinAirMassFlowRate);
    Fan(FanNum).FanRuntimeFraction = 0.0;

    // Determine the Fan Schedule for the Time step
    if ((GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0 || state.dataFans->LocalTurnFansOn) &&
        !state.dataFans->LocalTurnFansOff && MassFlow > 0.0 && Fan(FanNum).MaxAirMassFlowRate > 0.0) {
        // The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
        FlowFrac = MassFlow / MaxAirMassFlowRate;

        // Calculate the part load ratio, can't be greater than 1
        PartLoadRatio = min(1.0, FlowFrac);
        // Fan is operating
        if (state.dataHVACGlobal->OnOffFanPartLoadFraction <= 0.0) {
            ShowRecurringWarningErrorAtEnd(state, "Fan:OnOff, OnOffFanPartLoadFraction <= 0.0, Reset to 1.0", state.dataFans->ErrCount);
            state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0; // avoid divide by zero or negative PLF
        }

        if (state.dataHVACGlobal->OnOffFanPartLoadFraction < 0.7) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = 0.7; // a warning message is already issued from the DX coils or gas heating coil
        }

        // Keep fan runtime fraction between 0.0 and 1.0, and RTF >= PLR
        if (state.dataHVACGlobal->OnOffFanPartLoadFraction >= 1.0) {
            Fan(FanNum).FanRuntimeFraction = PartLoadRatio;
        } else {
            Fan(FanNum).FanRuntimeFraction = max(0.0, min(1.0, PartLoadRatio / state.dataHVACGlobal->OnOffFanPartLoadFraction));
        }
        // The fan speed ratio (passed from parent) determines the fan power according to fan laws
        if (present(SpeedRatio)) {
            //    Fan(FanNum)%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir*OnOffFanPartLoadFraction)! total fan power
            Fan(FanNum).FanPower = max(0.0, MaxAirMassFlowRate * Fan(FanNum).FanRuntimeFraction * DeltaPress / (FanEff * RhoAir));

            //    Do not modify fan power calculation unless fan power vs speed ratio curve is used.
            if (Fan(FanNum).FanPowerRatAtSpeedRatCurveIndex > 0) {

                //      adjust RTF to be in line with speed ratio (i.e., MaxAirMassFlowRate is not MAX when SpeedRatio /= 1)
                //      PLR = Mdot/MAXFlow => Mdot/(MAXFlow * SpeedRatio), RTF = PLR/PLF => PLR/SpeedRatio/PLF = RTF / SpeedRatio
                if (SpeedRatio > 0.0) Fan(FanNum).FanRuntimeFraction = min(1.0, Fan(FanNum).FanRuntimeFraction / SpeedRatio);

                SpeedRaisedToPower = CurveValue(state, Fan(FanNum).FanPowerRatAtSpeedRatCurveIndex, SpeedRatio);
                if (SpeedRaisedToPower < 0.0) {
                    if (Fan(FanNum).OneTimePowerRatioCheck && !state.dataGlobal->WarmupFlag) {
                        ShowSevereError(state, cFanTypes(Fan(FanNum).FanType_Num) + " = " + Fan(FanNum).FanName + "\"");
                        ShowContinueError(state, "Error in Fan Power Ratio curve. Curve output less than 0.0.");
                        ShowContinueError(state, format("Curve output = {:.5T}, fan speed ratio = {:.5T}", SpeedRaisedToPower, SpeedRatio));
                        ShowContinueError(state, "Check curve coefficients to ensure proper power ratio as a function of fan speed ratio.");
                        ShowContinueError(state, "Resetting Fan Power Ratio curve output to 0.0 and the simulation continues.");
                        ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        Fan(FanNum).OneTimePowerRatioCheck = false;
                    }
                    SpeedRaisedToPower = 0.0;
                }
                if (Fan(FanNum).FanEffRatioCurveIndex > 0 && !state.dataGlobal->WarmupFlag) {
                    EffRatioAtSpeedRatio = CurveValue(state, Fan(FanNum).FanEffRatioCurveIndex, SpeedRatio);
                    if (EffRatioAtSpeedRatio < 0.01) {
                        if (Fan(FanNum).OneTimeEffRatioCheck && !state.dataGlobal->WarmupFlag) {
                            ShowSevereError(state, cFanTypes(Fan(FanNum).FanType_Num) + " = " + Fan(FanNum).FanName + "\"");
                            ShowContinueError(state, "Error in Fan Efficiency Ratio curve. Curve output less than 0.01.");
                            ShowContinueError(state, format("Curve output = {:.5T}, fan speed ratio = {:.5T}", EffRatioAtSpeedRatio, SpeedRatio));
                            ShowContinueError(state, "Check curve coefficients to ensure proper efficiency ratio as a function of fan speed ratio.");
                            ShowContinueError(state, "Resetting Fan Efficiency Ratio curve output to 0.01 and the simulation continues.");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                            Fan(FanNum).OneTimeEffRatioCheck = false;
                        }
                        EffRatioAtSpeedRatio = 0.01;
                    }
                } else {
                    EffRatioAtSpeedRatio = 1.0;
                }
                Fan(FanNum).FanPower *= SpeedRaisedToPower / EffRatioAtSpeedRatio;
            }
        } else {
            Fan(FanNum).FanPower = max(0.0, MaxAirMassFlowRate * Fan(FanNum).FanRuntimeFraction * DeltaPress / (FanEff * RhoAir)); // total fan power
        }

        // OnOffFanPartLoadFraction is passed via DataHVACGlobals from the cooling or heating coil that is
        //   requesting the fan to operate in cycling fan/cycling coil mode
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;      // reset to 1 in case other on/off fan is called without a part load curve
        FanShaftPower = Fan(FanNum).MotEff * Fan(FanNum).FanPower; // power delivered to shaft
        Fan(FanNum).PowerLossToAir = FanShaftPower + (Fan(FanNum).FanPower - FanShaftPower) * Fan(FanNum).MotInAirFrac;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + Fan(FanNum).PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirMassFlowRate = MassFlow;
        //   Fan(FanNum)%OutletAirTemp = Tin + PowerLossToAir/(MassFlow*PsyCpAirFnW(Win,Tin))
        Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);
    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        Fan(FanNum).FanPower = 0.0;
        FanShaftPower = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).OutletAirMassFlowRate = 0.0;
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy;
        Fan(FanNum).OutletAirTemp = Fan(FanNum).InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        Fan(FanNum).MassFlowRateMaxAvail = 0.0;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;
    }
}

void SimZoneExhaustFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Jan 2000
    //       MODIFIED       Brent Griffith, May 2009 for EMS
    //                      Brent Griffith, Feb 2013 controls upgrade
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the Zone Exhaust Fan

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RhoAir;
    Real64 DeltaPress; // [N/m2]
    Real64 FanEff;
    Real64 MassFlow;           // [kg/sec]
    Real64 Tin;                // [C]
    bool FanIsRunning = false; // There seems to be a missing else case below unless false is assumed

    auto &Fan(state.dataFans->Fan);

    DeltaPress = Fan(FanNum).DeltaPress;
    if (Fan(FanNum).EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum).EMSFanPressureValue;

    FanEff = Fan(FanNum).FanEff;
    if (Fan(FanNum).EMSFanEffOverrideOn) FanEff = Fan(FanNum).EMSFanEffValue;

    // For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
    Tin = Fan(FanNum).InletAirTemp;
    RhoAir = Fan(FanNum).RhoAirStdInit;
    MassFlow = Fan(FanNum).InletAirMassFlowRate;

    //  When the AvailManagerMode == ExhaustFanCoupledToAvailManagers then the
    //  Exhaust Fan is  interlocked with air loop availability via global TurnFansOn and TurnFansOff variables.
    //  There is now the option to control if user wants to decouple air loop operation and exhaust fan operation
    //  (zone air mass balance issues). If in the future want to allow for zone level local availability manager
    //  then the optional arguments ZoneCompTurnFansOn and ZoneCompTurnFansOff will need
    //  to be passed to SimulateFanComponents, and TurnFansOn must be changed to LocalTurnFansOn
    //  and TurnFansOff to LocalTurnFansOff in the IF statement below.

    // apply controls to determine if operating
    if (Fan(FanNum).AvailManagerMode == AvailabilityManagerCoupling::Coupled) {
        if (((GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0) || state.dataHVACGlobal->TurnFansOn) &&
            !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0) { // available
            if (Fan(FanNum).MinTempLimitSchedNum > 0) {
                if (Tin >= GetCurrentScheduleValue(state, Fan(FanNum).MinTempLimitSchedNum)) {
                    FanIsRunning = true;
                } else {
                    FanIsRunning = false;
                }
            } else {
                FanIsRunning = true;
            }
        } else {
            FanIsRunning = false;
        }

    } else if (Fan(FanNum).AvailManagerMode == AvailabilityManagerCoupling::Decoupled) {
        if (GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0 && MassFlow > 0.0) {
            if (Fan(FanNum).MinTempLimitSchedNum > 0) {
                if (Tin >= GetCurrentScheduleValue(state, Fan(FanNum).MinTempLimitSchedNum)) {
                    FanIsRunning = true;
                } else {
                    FanIsRunning = false;
                }
            } else {
                FanIsRunning = true;
            }
        } else {
            FanIsRunning = false;
        }
    }

    if (FanIsRunning) {
        // Fan is operating
        Fan(FanNum).FanPower = max(0.0, MassFlow * DeltaPress / (FanEff * RhoAir)); // total fan power
        Fan(FanNum).PowerLossToAir = Fan(FanNum).FanPower;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + Fan(FanNum).PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirMassFlowRate = MassFlow;
        Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        Fan(FanNum).FanPower = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).OutletAirMassFlowRate = 0.0;
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy;
        Fan(FanNum).OutletAirTemp = Fan(FanNum).InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        Fan(FanNum).MassFlowRateMaxAvail = 0.0;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;
        Fan(FanNum).InletAirMassFlowRate = 0.0;
    }
}

void SimComponentModelFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Craig Wray, LBNL
    //       DATE WRITTEN   Feb 2010
    //       MODIFIED       Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the component model fan.

    // METHODOLOGY EMPLOYED:
    // Calculate fan volumetric flow and corresponding fan static pressure rise,
    //    using air-handling system characteristics and Sherman-Wray system curve model
    // Calculate fan air power using volumetric flow and fan static pressure rise
    // Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
    //   fan characteristics, and Wray dimensionless fan static efficiency model
    // Calculate fan shaft power using fan air power and fan static efficiency
    // Calculate fan shaft speed and torque using Wray dimensionless fan airflow model
    // Calculate belt part-load efficiency using correlations and coefficients based on ACEEE data
    // Calculate belt input power using fan shaft power and belt efficiency
    // Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
    // Calculate motor input power using belt input power and motor efficiency
    // Calculate VFD efficiency using correlations and coefficients based on DOE data
    // Calculate VFD input power using motor input power and VFD efficiency
    // Calculate combined efficiency of fan, belt, motor, and VFD
    // Calculate air temperature rise due to fan (and belt+motor if in airstream) power entering air-handler airflow
    // Calculate output node conditions

    // Using/Aliasing
    using CurveManager::CurveValue;
    using CurveManager::GetCurveIndex;
    using namespace OutputReportPredefined;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NVPerfNum;

    Real64 MaxAirMassFlowRate; // Fan Max mass airflow [kg/s]
    Real64 MotInAirFrac;       // Fraction of fan power input to airstream

    // Local variables
    Real64 RhoAir;              // Air density [kg/m3]
    Real64 MassFlow;            // Fan mass airflow [kg/s]
    Real64 FanVolFlow;          // Fan volumetric airflow [m3/s]
    Real64 DuctStaticPress;     // Duct static pressure setpoint [Pa]
    Real64 DeltaPressTot;       // Total pressure rise across fan [N/m2 = Pa]
    Real64 FanOutletVelPress;   // Fan outlet velocity pressure [Pa]
    Real64 EulerNum;            // Fan Euler number [-]
    Real64 NormalizedEulerNum;  // Normalized Fan Euler number [-]
    Real64 FanDimFlow;          // Fan dimensionless airflow [-]
    Real64 FanSpdRadS;          // Fan shaft rotational speed [rad/s]
    Real64 MotorSpeed;          // Motor shaft rotational speed [rpm]
    Real64 FanTrqRatio;         // Ratio of fan torque to max fan torque [-]
    Real64 BeltPLEff;           // Belt normalized (part-load) efficiency [-]
    Real64 MotorOutPwrRatio;    // Ratio of motor output power to max motor output power [-]
    Real64 MotorPLEff;          // Motor normalized (part-load) efficiency [-]
    Real64 VFDSpdRatio(0.0);    // Ratio of motor speed to motor max speed [-]
    Real64 VFDOutPwrRatio(0.0); // Ratio of VFD output power to max VFD output power [-]
    Real64 FanEnthalpyChange;   // Air enthalpy change due to fan, belt, and motor losses [kJ/kg]

    auto &NightVentPerf(state.dataFans->NightVentPerf);
    auto &Fan(state.dataFans->Fan);

    // Get inputs for night ventilation option
    NVPerfNum = Fan(FanNum).NVPerfNum;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        MotInAirFrac = NightVentPerf(NVPerfNum).MotInAirFrac;
        MaxAirMassFlowRate = NightVentPerf(NVPerfNum).MaxAirMassFlowRate;
    } else {
        MotInAirFrac = Fan(FanNum).MotInAirFrac;
        MaxAirMassFlowRate = Fan(FanNum).MaxAirMassFlowRate;
    }

    //  IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
    //  IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

    // Get air density at standard conditions and get mass airflow through fan
    // From WeatherManager:
    //   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
    //   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
    // From PsychRoutines:
    //   w=MAX(dw,1.0d-5)
    //   rhoair = pb/(287.d0*(tdb+DataGlobalConstants::KelvinConv())*(1.0d0+1.6077687d0*w))
    RhoAir = Fan(FanNum).RhoAirStdInit;
    MassFlow = min(Fan(FanNum).InletAirMassFlowRate, Fan(FanNum).MaxAirMassFlowRate);

    //  IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow   = Fan(FanNum)%EMSAirMassFlowValue

    // Determine the Fan Schedule for the Time step
    if ((GetCurrentScheduleValue(state, Fan(FanNum).AvailSchedPtrNum) > 0.0 || state.dataFans->LocalTurnFansOn) &&
        !state.dataFans->LocalTurnFansOff && MassFlow > 0.0) {
        // Fan is operating - calculate fan pressure rise, component efficiencies and power, and also air enthalpy rise

        // Calculate fan static pressure rise using fan volumetric flow, std air density, air-handling system characteristics,
        //   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
        FanVolFlow = MassFlow / RhoAir;                                                                  //[m3/s at standard conditions]
        DuctStaticPress = CurveValue(state, Fan(FanNum).PressResetCurveIndex, FanVolFlow);               // Duct static pressure setpoint [Pa]
        DeltaPressTot = CurveValue(state, Fan(FanNum).PressRiseCurveIndex, FanVolFlow, DuctStaticPress); // Fan total pressure rise [Pa]
        FanOutletVelPress = 0.5 * RhoAir * pow_2(FanVolFlow / Fan(FanNum).FanOutletArea);                // Fan outlet velocity pressure [Pa]
        // Outlet velocity pressure cannot exceed total pressure rise
        FanOutletVelPress = min(FanOutletVelPress, DeltaPressTot);
        Fan(FanNum).DeltaPress = DeltaPressTot - FanOutletVelPress; // Fan static pressure rise [Pa]

        //    IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue

        // Calculate fan static air power using volumetric flow and fan static pressure rise
        Fan(FanNum).FanAirPower = FanVolFlow * Fan(FanNum).DeltaPress; //[W]

        // Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
        //   fan characteristics, and Wray dimensionless fan static efficiency model
        EulerNum = (Fan(FanNum).DeltaPress * pow_4(Fan(FanNum).FanWheelDia)) / (RhoAir * pow_2(FanVolFlow)); //[-]
        NormalizedEulerNum = std::log10(EulerNum / Fan(FanNum).EuMaxEff);
        if (NormalizedEulerNum <= 0.0) {
            Fan(FanNum).FanWheelEff = CurveValue(state, Fan(FanNum).PLFanEffNormCurveIndex, NormalizedEulerNum);
        } else {
            Fan(FanNum).FanWheelEff = CurveValue(state, Fan(FanNum).PLFanEffStallCurveIndex, NormalizedEulerNum);
        }
        Fan(FanNum).FanWheelEff *= Fan(FanNum).FanMaxEff;             // [-]
        Fan(FanNum).FanWheelEff = max(Fan(FanNum).FanWheelEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate fan shaft power using fan static air power and fan static efficiency
        Fan(FanNum).FanShaftPower = Fan(FanNum).FanAirPower / Fan(FanNum).FanWheelEff; //[W]

        // Calculate fan shaft speed, fan torque, and motor speed using Wray dimensionless fan airflow model
        if (NormalizedEulerNum <= 0.0) {
            FanDimFlow = CurveValue(state, Fan(FanNum).DimFlowNormCurveIndex, NormalizedEulerNum); //[-]
        } else {
            FanDimFlow = CurveValue(state, Fan(FanNum).DimFlowStallCurveIndex, NormalizedEulerNum); //[-]
        }
        FanSpdRadS = FanVolFlow / (FanDimFlow * Fan(FanNum).FanMaxDimFlow * pow_3(Fan(FanNum).FanWheelDia)); //[rad/s]
        Fan(FanNum).FanTrq = Fan(FanNum).FanShaftPower / FanSpdRadS;                                         //[N-m]
        Fan(FanNum).FanSpd = FanSpdRadS * 9.549296586;                                                       //[rpm, conversion factor is 30/PI]
        MotorSpeed = Fan(FanNum).FanSpd * Fan(FanNum).PulleyDiaRatio;                                        //[rpm]

        // Calculate belt part-load drive efficiency using correlations and coefficients based on ACEEE data
        // Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
        FanTrqRatio = Fan(FanNum).FanTrq / Fan(FanNum).BeltMaxTorque; //[-]
        if ((FanTrqRatio <= Fan(FanNum).BeltTorqueTrans) && (Fan(FanNum).PLBeltEffReg1CurveIndex != 0)) {
            BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg1CurveIndex, FanTrqRatio); //[-]
        } else {
            if ((FanTrqRatio > Fan(FanNum).BeltTorqueTrans) && (FanTrqRatio <= 1.0) && (Fan(FanNum).PLBeltEffReg2CurveIndex != 0)) {
                BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg2CurveIndex, FanTrqRatio); //[-]
            } else {
                if ((FanTrqRatio > 1.0) && (Fan(FanNum).PLBeltEffReg3CurveIndex != 0)) {
                    BeltPLEff = CurveValue(state, Fan(FanNum).PLBeltEffReg3CurveIndex, FanTrqRatio); //[-]
                } else {
                    BeltPLEff = 1.0; // Direct drive or no curve specified - use constant efficiency
                }
            }
        }
        Fan(FanNum).BeltEff = Fan(FanNum).BeltMaxEff * BeltPLEff; //[-]
        Fan(FanNum).BeltEff = max(Fan(FanNum).BeltEff, 0.01);     // Minimum efficiency is 1% to avoid numerical errors

        // Calculate belt input power using fan shaft power and belt efficiency
        Fan(FanNum).BeltInputPower = Fan(FanNum).FanShaftPower / Fan(FanNum).BeltEff; //[W]

        // Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
        MotorOutPwrRatio = Fan(FanNum).BeltInputPower / Fan(FanNum).MotorMaxOutPwr; //[-]
        if (Fan(FanNum).PLMotorEffCurveIndex != 0) {
            MotorPLEff = CurveValue(state, Fan(FanNum).PLMotorEffCurveIndex, MotorOutPwrRatio); //[-]
        } else {
            MotorPLEff = 1.0; // No curve specified - use constant efficiency
        }
        Fan(FanNum).MotEff = Fan(FanNum).MotorMaxEff * MotorPLEff; //[-]
        Fan(FanNum).MotEff = max(Fan(FanNum).MotEff, 0.01);        // Minimum efficiency is 1% to avoid numerical errors

        // Calculate motor input power using belt input power and motor efficiency
        Fan(FanNum).MotorInputPower = Fan(FanNum).BeltInputPower / Fan(FanNum).MotEff; //[W]

        // Calculate VFD efficiency using correlations and coefficients based on VFD type
        if ((Fan(FanNum).VFDEffType == "SPEED") && (Fan(FanNum).VFDEffCurveIndex != 0)) {
            VFDSpdRatio = MotorSpeed / Fan(FanNum).MotorMaxSpd;                                //[-]
            Fan(FanNum).VFDEff = CurveValue(state, Fan(FanNum).VFDEffCurveIndex, VFDSpdRatio); //[-]
        } else {
            if ((Fan(FanNum).VFDEffType == "POWER") && (Fan(FanNum).VFDEffCurveIndex != 0)) {
                VFDOutPwrRatio = Fan(FanNum).MotorInputPower / Fan(FanNum).VFDMaxOutPwr;              //[-]
                Fan(FanNum).VFDEff = CurveValue(state, Fan(FanNum).VFDEffCurveIndex, VFDOutPwrRatio); //[-]
            } else {
                // No curve specified - use constant efficiency
                Fan(FanNum).VFDMaxOutPwr = 0.0;
                Fan(FanNum).VFDEff = 0.97;
            }
        }
        Fan(FanNum).VFDEff = max(Fan(FanNum).VFDEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate VFD input power using motor input power and VFD efficiency
        Fan(FanNum).VFDInputPower = Fan(FanNum).MotorInputPower / Fan(FanNum).VFDEff; //[W]
        Fan(FanNum).FanPower = Fan(FanNum).VFDInputPower;                             //[W]

        // Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
        // Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
        Fan(FanNum).FanEff = Fan(FanNum).FanWheelEff * Fan(FanNum).BeltEff * Fan(FanNum).MotEff * Fan(FanNum).VFDEff;

        //    IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

        // Calculate air enthalpy and temperature rise from power entering air stream from fan wheel, belt, and motor
        // Assumes MotInAirFrac applies to belt and motor but NOT to VFD
        Fan(FanNum).PowerLossToAir =
            Fan(FanNum).FanShaftPower + (Fan(FanNum).MotorInputPower - Fan(FanNum).FanShaftPower) * Fan(FanNum).MotInAirFrac; //[W]
        FanEnthalpyChange = Fan(FanNum).PowerLossToAir / MassFlow;                                                            //[kJ/kg]
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy + FanEnthalpyChange;                                     //[kJ/kg]

        // This fan does not change the moisture or mass flow across the component
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat; //[-]
        Fan(FanNum).OutletAirMassFlowRate = MassFlow;             //[kg/s]
        Fan(FanNum).OutletAirTemp = PsyTdbFnHW(Fan(FanNum).OutletAirEnthalpy, Fan(FanNum).OutletAirHumRat);
    } else {
        // Fan is OFF and not operating -- no power consumed and zero mass flow rate
        Fan(FanNum).FanPower = 0.0;
        Fan(FanNum).FanShaftPower = 0.0;
        Fan(FanNum).PowerLossToAir = 0.0;
        Fan(FanNum).OutletAirMassFlowRate = 0.0;
        Fan(FanNum).OutletAirHumRat = Fan(FanNum).InletAirHumRat;
        Fan(FanNum).OutletAirEnthalpy = Fan(FanNum).InletAirEnthalpy;
        Fan(FanNum).OutletAirTemp = Fan(FanNum).InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        Fan(FanNum).MassFlowRateMaxAvail = 0.0;
        Fan(FanNum).MassFlowRateMinAvail = 0.0;

        Fan(FanNum).DeltaPress = 0.0;
        Fan(FanNum).FanAirPower = 0.0;
        Fan(FanNum).FanWheelEff = 0.0;
        Fan(FanNum).FanSpd = 0.0;
        Fan(FanNum).FanTrq = 0.0;
        Fan(FanNum).BeltEff = 0.0;
        Fan(FanNum).BeltInputPower = 0.0;
        Fan(FanNum).MotEff = 0.0;
        Fan(FanNum).MotorInputPower = 0.0;
        Fan(FanNum).VFDEff = 0.0;
        Fan(FanNum).VFDInputPower = 0.0;
        Fan(FanNum).FanEff = 0.0;
    }
}

void UpdateFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       L. Gu, Feb. 1, 2007, No unbalance airflow when Zone Exhaust Fans are used in the AirflowNetwork
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the fan outlet nodes.

    // METHODOLOGY EMPLOYED:
    // Data is moved from the fan data structure to the fan outlet nodes.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;
    int InletNode;

    auto &Fan(state.dataFans->Fan);

    OutletNode = Fan(FanNum).OutletNodeNum;
    InletNode = Fan(FanNum).InletNodeNum;

    // Set the outlet air nodes of the fan
    state.dataLoopNodes->Node(OutletNode).MassFlowRate = Fan(FanNum).OutletAirMassFlowRate;
    state.dataLoopNodes->Node(OutletNode).Temp = Fan(FanNum).OutletAirTemp;
    state.dataLoopNodes->Node(OutletNode).HumRat = Fan(FanNum).OutletAirHumRat;
    state.dataLoopNodes->Node(OutletNode).Enthalpy = Fan(FanNum).OutletAirEnthalpy;
    // Set the outlet nodes for properties that just pass through & not used
    state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
    state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;

    // Set the Node Flow Control Variables from the Fan Control Variables
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = Fan(FanNum).MassFlowRateMaxAvail;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = Fan(FanNum).MassFlowRateMinAvail;

    if (Fan(FanNum).FanType_Num == FanType_ZoneExhaust) {
        state.dataLoopNodes->Node(InletNode).MassFlowRate = Fan(FanNum).InletAirMassFlowRate;
        if (state.afn->AirflowNetworkNumOfExhFan == 0) {
            state.dataHVACGlobal->UnbalExhMassFlow = Fan(FanNum).InletAirMassFlowRate;
            if (Fan(FanNum).BalancedFractSchedNum > 0) {
                state.dataHVACGlobal->BalancedExhMassFlow =
                    state.dataHVACGlobal->UnbalExhMassFlow * GetCurrentScheduleValue(state, Fan(FanNum).BalancedFractSchedNum);
                state.dataHVACGlobal->UnbalExhMassFlow = state.dataHVACGlobal->UnbalExhMassFlow - state.dataHVACGlobal->BalancedExhMassFlow;
            } else {
                state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
            }
        } else {
            state.dataHVACGlobal->UnbalExhMassFlow = 0.0;
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
        }
        Fan(FanNum).UnbalancedOutletMassFlowRate = state.dataHVACGlobal->UnbalExhMassFlow;
        Fan(FanNum).BalancedOutletMassFlowRate = state.dataHVACGlobal->BalancedExhMassFlow;
    }

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataLoopNodes->Node(OutletNode).CO2 = state.dataLoopNodes->Node(InletNode).CO2;
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataLoopNodes->Node(OutletNode).GenContam = state.dataLoopNodes->Node(InletNode).GenContam;
    }
}

void ReportFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the fans.

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    auto &Fan(state.dataFans->Fan);

    Fan(FanNum).FanEnergy = Fan(FanNum).FanPower * TimeStepSys * DataGlobalConstants::SecInHour;
    Fan(FanNum).DeltaTemp = Fan(FanNum).OutletAirTemp - Fan(FanNum).InletAirTemp;

    if (Fan(FanNum).FanType_Num == FanType_SimpleOnOff) {
        if (Fan(FanNum).AirLoopNum > 0) {
            state.dataAirLoop->AirLoopAFNInfo(Fan(FanNum).AirLoopNum).AFNLoopOnOffFanRTF = Fan(FanNum).FanRuntimeFraction;
        }
    }
}

void GetFanIndex(EnergyPlusData &state, std::string const &FanName, int &FanIndex, bool &ErrorsFound, std::string_view ThisObjectType)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets an index for a given fan -- issues error message if that fan
    // is not legal fan.

    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    FanIndex = UtilityRoutines::FindItemInList(FanName, state.dataFans->Fan, &FanEquipConditions::FanName);
    if (FanIndex == 0) {
        if (!ThisObjectType.empty()) {
            ShowSevereError(state, fmt::format("{}, GetFanIndex: Fan not found={}", ThisObjectType, FanName));
        } else {
            ShowSevereError(state, "GetFanIndex: Fan not found=" + FanName);
        }
        ErrorsFound = true;
    }
}

void GetFanVolFlow(EnergyPlusData &state, int const FanIndex, Real64 &FanVolFlow)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the fan volumetric flow for use by zone equipment (e.g. Packaged Terminal Heat Pump)
    // Zone equipment must ensure that a properly sized fan is used to meet the maximum supply air flow rate

    if (FanIndex == 0) {
        FanVolFlow = 0.0;
    } else {
        FanVolFlow = state.dataFans->Fan(FanIndex).MaxAirFlowRate;
    }
}

Real64 GetFanPower(EnergyPlusData &state, int const FanIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the fan power draw

    if (FanIndex == 0) {
        return 0.0;
    } else {
        return state.dataFans->Fan(FanIndex).FanPower;
    }
}

void GetFanType(EnergyPlusData &state,
                std::string const &FanName,            // Fan name
                int &FanType,                          // returned fantype number
                bool &ErrorsFound,                     // error indicator
                std::string_view const ThisObjectType, // parent object type (for error message)
                std::string_view const ThisObjectName  // parent object name (for error message)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets an integer type for a given fan -- issues error message if that fan
    // is not a legal fan.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanIndex;

    auto &Fan(state.dataFans->Fan);

    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    FanIndex = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    if (FanIndex == 0) {
        if ((!ThisObjectType.empty()) && (!ThisObjectName.empty())) {
            ShowSevereError(state, fmt::format("GetFanType: {}=\"{}\", invalid Fan specified=\"{}\".", ThisObjectType, ThisObjectName, FanName));
        } else if (!ThisObjectType.empty()) {
            ShowSevereError(state, fmt::format("{}, GetFanType: Fan not found={}", ThisObjectType, FanName));
        } else {
            ShowSevereError(state, "GetFanType: Fan not found=" + FanName);
        }
        FanType = 0;
        ErrorsFound = true;
    } else {
        FanType = Fan(FanIndex).FanType_Num;
    }
}

Real64 GetFanDesignVolumeFlowRate(EnergyPlusData &state,
                                  std::string_view FanType,   // must match fan types in this module
                                  std::string_view FanName,   // must match fan names for the fan type
                                  bool &ErrorsFound,          // set to true if problem
                                  Optional_int_const FanIndex // index to fan
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006
    //       MODIFIED       R. Raustad, Aug 2007 - added optional fan index
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the design volume flow rate for the given fan and returns it.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as negative.

    // Return value
    Real64 DesignVolumeFlowRate; // returned flow rate of matched fan

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichFan;

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (present(FanIndex)) {
        DesignVolumeFlowRate = Fan(FanIndex).MaxAirFlowRate;
    } else {
        WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
        if (WhichFan != 0) {
            DesignVolumeFlowRate = Fan(WhichFan).MaxAirFlowRate;
        } else {
            ShowSevereError(
                state, "GetFanDesignVolumeFlowRate: Could not find Fan, Type=\"" + std::string{FanType} + "\" Name=\"" + std::string{FanName} + "\"");
            ShowContinueError(state, "... Design Volume Flow rate returned as -1000.");
            ErrorsFound = true;
            DesignVolumeFlowRate = -1000.0;
        }
    }

    return DesignVolumeFlowRate;
}

int GetFanInletNode(EnergyPlusData &state,
                    std::string_view FanType, // must match fan types in this module
                    std::string_view FanName, // must match fan names for the fan type
                    bool &ErrorsFound         // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the inlet node.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int NodeNumber; // returned outlet node of matched fan

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichFan;

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    if (WhichFan != 0) {
        NodeNumber = Fan(WhichFan).InletNodeNum;
    } else {
        ShowSevereError(state, "GetFanInletNode: Could not find Fan, Type=\"" + std::string{FanType} + "\" Name=\"" + std::string{FanName} + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int getFanInNodeIndex(EnergyPlusData &state,
                      int const FanIndex, // fan index
                      bool &ErrorsFound   // set to true if problem
)
{

    int NodeNumber = 0; // returned outlet node of matched fan

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (FanIndex != 0) {
        NodeNumber = Fan(FanIndex).InletNodeNum;
    } else {
        ShowSevereError(state, "getFanInNodeIndex: Could not find Fan");
        ErrorsFound = true;
    }

    return NodeNumber;
}

int GetFanOutletNode(EnergyPlusData &state,
                     std::string const &FanType, // must match fan types in this module
                     std::string const &FanName, // must match fan names for the fan type
                     bool &ErrorsFound           // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the outlet node.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int NodeNumber; // returned outlet node of matched fan

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichFan;

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    if (WhichFan != 0) {
        NodeNumber = Fan(WhichFan).OutletNodeNum;
    } else {
        ShowSevereError(state, "GetFanOutletNode: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetFanAvailSchPtr(EnergyPlusData &state,
                      std::string const &FanType, // must match fan types in this module
                      std::string const &FanName, // must match fan names for the fan type
                      bool &ErrorsFound           // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the availability schedule pointer.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int FanAvailSchPtr; // returned availability schedule pointer of matched fan

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichFan;

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    if (WhichFan != 0) {
        FanAvailSchPtr = Fan(WhichFan).AvailSchedPtrNum;
    } else {
        ShowSevereError(state, "GetFanAvailSchPtr: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"");
        ErrorsFound = true;
        FanAvailSchPtr = 0;
    }

    return FanAvailSchPtr;
}

int GetFanSpeedRatioCurveIndex(EnergyPlusData &state,
                               std::string &FanType, // must match fan types in this module (set if nonzero index passed)
                               std::string &FanName, // must match fan names for the fan type (set if nonzero index passed)
                               Optional_int IndexIn  // optional fan index if fan type and name are unknown or index needs setting
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the fan speed curve pointer.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero. If optional index argument is passed along with fan type and name, the index is set.

    // Return value
    int FanSpeedRatioCurveIndex; // index to fan speed ratio curve object

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichFan;

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (present(IndexIn)) {
        if (IndexIn > 0) {
            WhichFan = IndexIn;
            FanType = Fan(WhichFan).FanType;
            FanName = Fan(WhichFan).FanName;
        } else {
            WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
            IndexIn = WhichFan;
        }
    } else {
        WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    }

    if (WhichFan != 0) {
        FanSpeedRatioCurveIndex = Fan(WhichFan).FanPowerRatAtSpeedRatCurveIndex;
    } else {
        ShowSevereError(state, "GetFanSpeedRatioCurveIndex: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"");
        FanSpeedRatioCurveIndex = 0;
    }

    return FanSpeedRatioCurveIndex;
}

void SetFanData(EnergyPlusData &state,
                int const FanNum,                     // Index of fan
                bool &ErrorsFound,                    // Set to true if certain errors found
                std::string const &FanName,           // Name of fan
                Optional<Real64 const> MaxAirVolFlow, // Fan air volumetric flow rate    [m3/s]
                Optional<Real64 const> MinAirVolFlow  // Fan air volumetric flow rate    [m3/s]
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   October 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine was designed for to autosize the HeatExchanger:AirToAir:SensibleAndLatent using
    // information from the ZoneHVAC:EnergyRecoveryVentilator object.
    // This is an illustration of setting data from an outside source.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WhichFan; // index to generic HX

    auto &Fan(state.dataFans->Fan);

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (FanNum == 0) {
        WhichFan = UtilityRoutines::FindItemInList(FanName, Fan, &FanEquipConditions::FanName);
    } else {
        WhichFan = FanNum;
    }

    if (WhichFan <= 0 || WhichFan > state.dataFans->NumFans) {
        ShowSevereError(state, "SetFanData: Could not find fan = \"" + FanName + "\"");
        ErrorsFound = true;
        return;
    }

    if (present(MaxAirVolFlow)) {
        Fan(WhichFan).MaxAirFlowRate = MaxAirVolFlow;
    }

    if (present(MinAirVolFlow)) {
        Fan(WhichFan).MinAirFlowRate = MinAirVolFlow;
    }
}

[[maybe_unused]] Real64 FanDesDT(EnergyPlusData &state,
                                 int const FanNum,                        // index of fan in Fan array
                                 [[maybe_unused]] Real64 const FanVolFlow // fan volumetric flow rate [m3/s]
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2014
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function calculates and returns the design fan delta T from the fan input data

    // METHODOLOGY EMPLOYED:
    // Simple fan:  Qdot,tot = (Vdot*deltaP)/Eff,tot
    //              Qdot,air = Eff,mot*Qdot,tot + (Qdot,tot - Eff,mot*Qdot,tot)*Frac,mot-in-airstream
    //              Qdot,air = cp,air*rho,air*Vdot*deltaT

    // REFERENCES: EnergyPlus Engineering Reference

    // Return value
    Real64 DesignDeltaT; // returned delta T of matched fan [delta deg C]

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 RhoAir;       // density of air [kg/m3]
    Real64 CpAir;        // specific heat of air [J/kg-K]
    Real64 DeltaP;       // fan design pressure rise [N/m2]
    Real64 TotEff;       // fan design total efficiency
    Real64 MotEff;       // fan design motor efficiency
    Real64 MotInAirFrac; // fraction of motor in the air stream

    auto &Fan(state.dataFans->Fan);

    if (FanNum == 0) {
        DesignDeltaT = 0.0;
    } else if (Fan(FanNum).FanType_Num != FanType_ComponentModel) {
        DeltaP = Fan(FanNum).DeltaPress;
        TotEff = Fan(FanNum).FanEff;
        MotEff = Fan(FanNum).MotEff;
        MotInAirFrac = Fan(FanNum).MotInAirFrac;
        RhoAir = state.dataEnvrn->StdRhoAir;
        CpAir = PsyCpAirFnW(DataPrecisionGlobals::constant_zero);
        DesignDeltaT = (DeltaP / (RhoAir * CpAir * TotEff)) * (MotEff + MotInAirFrac * (1.0 - MotEff));
    } else {
        DesignDeltaT = 0.0;
    }

    return DesignDeltaT;

} // FanDesDT

Real64 CalFaultyFanAirFlowReduction(EnergyPlusData &state,
                                    std::string const &FanName,          // name of the fan
                                    Real64 const FanDesignAirFlowRate,   // Fan Design Volume Flow Rate [m3/sec]
                                    Real64 const FanDesignDeltaPress,    // Fan Design Delta Pressure [Pa]
                                    Real64 const FanFaultyDeltaPressInc, // Increase of Fan Delta Pressure in the Faulty Case [Pa]
                                    int const FanCurvePtr                // Fan Curve Index
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rongpeng Zhang
    //       DATE WRITTEN   Apr. 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the decrease of the fan air flow rate, given the fan curve
    // and the increase of fan pressure rise due to fouling air filters

    // Using/Aliasing
    using namespace CurveManager;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FanFaultyAirFlowRate; // Fan Volume Flow Rate in the Faulty Case [m3/sec]
    Real64 FanCalDeltaPress;     // Calculated Fan Delta Pressure for temp use [Pa]
    Real64 FanCalDeltaPresstemp; // Calculated Fan Delta Pressure for temp use [Pa]

    // Check whether the fan curve covers the design operational point of the fan
    FanCalDeltaPress = CurveValue(state, FanCurvePtr, FanDesignAirFlowRate);
    if ((FanCalDeltaPress < 0.9 * FanDesignDeltaPress) || (FanCalDeltaPress > 1.1 * FanDesignDeltaPress)) {
        ShowWarningError(state, "The design operational point of the fan " + FanName + " does not fall ");
        ShowContinueError(state, "on the fan curve provided in the FaultModel:Fouling:AirFilter object. ");
        return 0.0;
    }

    // Calculate the Fan Volume Flow Rate in the Faulty Case
    FanFaultyAirFlowRate = FanDesignAirFlowRate;
    FanCalDeltaPresstemp = CurveValue(state, FanCurvePtr, FanFaultyAirFlowRate);
    FanCalDeltaPress = FanCalDeltaPresstemp;

    while (FanCalDeltaPress < (FanDesignDeltaPress + FanFaultyDeltaPressInc)) {
        FanFaultyAirFlowRate = FanFaultyAirFlowRate - 0.005;
        FanCalDeltaPresstemp = CurveValue(state, FanCurvePtr, FanFaultyAirFlowRate);

        if ((FanCalDeltaPresstemp <= FanCalDeltaPress) || (FanFaultyAirFlowRate <= state.dataCurveManager->PerfCurve(FanCurvePtr).Var1Min)) {
            // The new operational point of the fan go beyond the fan selection range
            ShowWarningError(state, "The operational point of the fan " + FanName + " may go beyond the fan selection ");
            ShowContinueError(state, "range in the faulty fouling air filter cases");
            break;
        }

        FanCalDeltaPress = FanCalDeltaPresstemp;
    }

    return FanDesignAirFlowRate - FanFaultyAirFlowRate;
}

Real64 FanDesHeatGain(EnergyPlusData &state,
                      int const FanNum,       // index of fan in Fan array
                      Real64 const FanVolFlow // fan volumetric flow rate [m3/s]
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2014
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function calculates and returns the design fan heat gain from the fan input data

    // METHODOLOGY EMPLOYED:
    // Simple fan:  Qdot,tot = (Vdot*deltaP)/Eff,tot
    //              Qdot,air = Eff,mot*Qdot,tot + (Qdot,tot - Eff,mot*Qdot,tot)*Frac,mot-in-airstream

    // REFERENCES: EnergyPlus Engineering Reference

    // Return value
    Real64 DesignHeatGain; // returned heat gain of matched fan [W]

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 DeltaP;       // fan design pressure rise [N/m2]
    Real64 TotEff;       // fan design total efficiency
    Real64 MotEff;       // fan design motor efficiency
    Real64 MotInAirFrac; // fraction of motor in the air stream
    Real64 FanPowerTot;  // total fan power consumption [W]

    auto &Fan(state.dataFans->Fan);

    if (FanNum == 0) {
        DesignHeatGain = 0.0;
    } else if (Fan(FanNum).FanType_Num != FanType_ComponentModel) {
        DeltaP = Fan(FanNum).DeltaPress;
        TotEff = Fan(FanNum).FanEff;
        MotEff = Fan(FanNum).MotEff;
        MotInAirFrac = Fan(FanNum).MotInAirFrac;
        FanPowerTot = (FanVolFlow * DeltaP) / TotEff;
        DesignHeatGain = MotEff * FanPowerTot + (FanPowerTot - MotEff * FanPowerTot) * MotInAirFrac;
    } else {
        if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(FanNum)) {
            SizeFan(state, FanNum);
            state.dataFans->MySizeFlag(FanNum) = false;
        }
        DesignHeatGain = Fan(FanNum).FanShaftPower + (Fan(FanNum).MotorInputPower - Fan(FanNum).FanShaftPower) * Fan(FanNum).MotInAirFrac;
    }

    return DesignHeatGain;

} // FanDesHeatGain

void FanInputsForDesHeatGain(EnergyPlusData &state,
                             int const fanIndex,
                             Real64 &deltaP,
                             Real64 &motEff,
                             Real64 &totEff,
                             Real64 &motInAirFrac,
                             Real64 &fanShaftPow,
                             Real64 &motInPower,
                             bool &fanCompModel)
{
    deltaP = 0.0;
    motEff = 0.0;
    totEff = 0.0;
    motInAirFrac = 0.0;
    fanShaftPow = 0.0;
    motInPower = 0.0;
    fanCompModel = false;

    auto &Fan(state.dataFans->Fan);

    if (fanIndex <= 0) {
        return;
    } else if (Fan(fanIndex).FanType_Num != FanType_ComponentModel) {
        deltaP = Fan(fanIndex).DeltaPress;
        motEff = Fan(fanIndex).MotEff;
        totEff = Fan(fanIndex).FanEff;
        motInAirFrac = Fan(fanIndex).MotInAirFrac;
    } else {
        if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(fanIndex)) {
            SizeFan(state, fanIndex);
            state.dataFans->MySizeFlag(fanIndex) = false;
        }
        fanCompModel = true;
        fanShaftPow = Fan(fanIndex).FanShaftPower;
        motInPower = Fan(fanIndex).MotorInputPower;
        motInAirFrac = Fan(fanIndex).MotInAirFrac;
    }
}

void SetFanAirLoopNumber(EnergyPlusData &state, int const FanIndex, int const AirLoopNum)
{
    state.dataFans->Fan(FanIndex).AirLoopNum = AirLoopNum;
}

} // namespace EnergyPlus::Fans
