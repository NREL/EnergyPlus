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
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
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

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the Fan System Component

constexpr std::array<std::string_view, static_cast<int>(AvailabilityManagerCoupling::Num)> couplingsUC = {"COUPLED", "DECOUPLED"};

void SimulateFanComponents(EnergyPlusData &state,
                           std::string_view const CompName,
                           bool const FirstHVACIteration,
                           int &CompIndex,
                           ObjexxFCL::Optional<Real64 const> SpeedRatio,
                           ObjexxFCL::Optional<Real64 const> PressureRise // Pressure difference to use for DeltaPress
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       Chandan Sharma, March 2011 - FSEC: Added logic for ZoneHVAC sys avail managers

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages Fan component simulation.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanNum; // current fan number

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (CompIndex == 0) {
        FanNum = Util::FindItemInList(CompName, state.dataFans->Fan, &FanEquipConditions::Name);
        if (FanNum == 0) {
            ShowFatalError(state, format("SimulateFanComponents: Fan not found={}", CompName));
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
            if (!CompName.empty() && CompName != state.dataFans->Fan(FanNum).Name) {
                ShowFatalError(state,
                               format("SimulateFanComponents: Invalid CompIndex passed={}, Fan name={}, stored Fan Name for that index={}",
                                      FanNum,
                                      CompName,
                                      state.dataFans->Fan(FanNum).Name));
            }
            state.dataFans->CheckEquipName(FanNum) = false;
        }
    }

    // With the correct FanNum Initialize
    InitFan(state, FanNum, FirstHVACIteration); // Initialize all fan related parameters

    // Calculate the Correct Fan Model with the current FanNum
    switch (state.dataFans->Fan(FanNum).fanType) {
    case HVAC::FanType::Constant: {
        SimSimpleFan(state, FanNum);
    } break;
    case HVAC::FanType::VAV: {
        if (present(PressureRise)) {
            SimVariableVolumeFan(state, FanNum, PressureRise);
        } else {
            SimVariableVolumeFan(state, FanNum);
        }
    } break;
    case HVAC::FanType::OnOff: {
        SimOnOffFan(state, FanNum, SpeedRatio);
    } break;
    case HVAC::FanType::Exhaust: {
        SimZoneExhaustFan(state, FanNum);
    } break;
    case HVAC::FanType::ComponentModel: {
        SimComponentModelFan(state, FanNum);
    } break;
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

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for fans and stores it in fan data structures

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanNum; // The fan that you are currently loading input into
    int NumAlphas;
    int NumNums;
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

    auto &Fan(state.dataFans->Fan);
    auto &FanNumericFields(state.dataFans->FanNumericFields);
    auto &UniqueFanNames(state.dataFans->UniqueFanNames);
    auto &NightVentPerf(state.dataFans->NightVentPerf);

    state.dataFans->GetFanInputFlag = false;

    int MaxAlphas = 0;
    int MaxNumbers = 0;
    int NumSimpFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ConstantVolume");
    if (NumSimpFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:ConstantVolume", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    int NumVarVolFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:VariableVolume");
    if (NumVarVolFan > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:VariableVolume", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    int NumOnOff = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:OnOff");
    if (NumOnOff > 0) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Fan:OnOff", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNums);
    }
    int NumZoneExhFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ZoneExhaust");
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

    int NumCompModelFan = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ComponentModel");
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

    for (int SimpFanNum = 1; SimpFanNum <= NumSimpFan; ++SimpFanNum) {
        FanNum = SimpFanNum;
        auto &thisFan = Fan(FanNum);
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
        thisFan.Name = cAlphaArgs(1);
        thisFan.fanType = HVAC::FanType::Constant;
        thisFan.AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            thisFan.AvailSchedPtrNum = ScheduleManager::ScheduleAlwaysOn;
        } else {
            thisFan.AvailSchedPtrNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
            if (thisFan.AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(2),
                                       cAlphaArgs(2),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        thisFan.FanEff = rNumericArgs(1);
        thisFan.DeltaPress = rNumericArgs(2);
        thisFan.MaxAirFlowRate = rNumericArgs(3);
        if (thisFan.MaxAirFlowRate == 0.0) {
            ShowWarningError(
                state,
                format("{}=\"{}\" has specified 0.0 max air flow rate. It will not be used in the simulation.", cCurrentModuleObject, thisFan.Name));
        }
        thisFan.MaxAirFlowRateIsAutosizable = true;
        thisFan.MotEff = rNumericArgs(4);
        thisFan.MotInAirFrac = rNumericArgs(5);
        thisFan.MinAirFlowRate = 0.0;

        thisFan.InletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                   cAlphaArgs(3),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::FanConstantVolume,
                                                                   cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent);
        thisFan.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                    cAlphaArgs(4),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::FanConstantVolume,
                                                                    cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent);

        thisFan.EndUseSubcategoryName = (NumAlphas > 4) ? cAlphaArgs(5) : "General";

        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Air Nodes");

    } // end Number of Simple FAN Loop

    for (int VarVolFanNum = 1; VarVolFanNum <= NumVarVolFan; ++VarVolFanNum) {
        FanNum = NumSimpFan + VarVolFanNum;
        auto &thisFan = Fan(FanNum);
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
        thisFan.Name = cAlphaArgs(1);
        thisFan.fanType = HVAC::FanType::VAV;
        thisFan.AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            thisFan.AvailSchedPtrNum = ScheduleManager::ScheduleAlwaysOn;
        } else {
            thisFan.AvailSchedPtrNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
            if (thisFan.AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(2),
                                       cAlphaArgs(2),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        thisFan.FanEff = rNumericArgs(1);
        thisFan.DeltaPress = rNumericArgs(2);
        thisFan.MaxAirFlowRate = rNumericArgs(3);
        if (thisFan.MaxAirFlowRate == 0.0) {
            ShowWarningError(
                state,
                format("{}=\"{}\" has specified 0.0 max air flow rate. It will not be used in the simulation.", cCurrentModuleObject, thisFan.Name));
        }
        thisFan.MaxAirFlowRateIsAutosizable = true;
        if (Util::SameString(cAlphaArgs(3), "Fraction")) {
            thisFan.FanMinAirFracMethod = HVAC::MinFrac;
        } else if (Util::SameString(cAlphaArgs(3), "FixedFlowRate")) {
            thisFan.FanMinAirFracMethod = HVAC::FixedMin;
        }
        thisFan.FanMinFrac = rNumericArgs(4);
        thisFan.FanFixedMin = rNumericArgs(5);
        thisFan.MotEff = rNumericArgs(6);
        thisFan.MotInAirFrac = rNumericArgs(7);
        thisFan.FanCoeff(1) = rNumericArgs(8);
        thisFan.FanCoeff(2) = rNumericArgs(9);
        thisFan.FanCoeff(3) = rNumericArgs(10);
        thisFan.FanCoeff(4) = rNumericArgs(11);
        thisFan.FanCoeff(5) = rNumericArgs(12);
        if (thisFan.FanCoeff(1) == 0.0 && thisFan.FanCoeff(2) == 0.0 && thisFan.FanCoeff(3) == 0.0 && thisFan.FanCoeff(4) == 0.0 &&
            thisFan.FanCoeff(5) == 0.0) {
            ShowWarningError(state, "Fan Coefficients are all zero.  No Fan power will be reported.");
            ShowContinueError(state, format("For {}, Fan={}", cCurrentModuleObject, cAlphaArgs(1)));
        }
        thisFan.InletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                   cAlphaArgs(4),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::FanVariableVolume,
                                                                   cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent);
        thisFan.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                    cAlphaArgs(5),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::FanVariableVolume,
                                                                    cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent);

        thisFan.EndUseSubcategoryName = (NumAlphas > 5) ? cAlphaArgs(6) : "General";

        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Air Nodes");

    } // end Number of Variable Volume FAN Loop

    for (int ExhFanNum = 1; ExhFanNum <= NumZoneExhFan; ++ExhFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + ExhFanNum;
        auto &thisFan = Fan(FanNum);
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
        thisFan.Name = cAlphaArgs(1);
        thisFan.fanType = HVAC::FanType::Exhaust;
        thisFan.AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            thisFan.AvailSchedPtrNum = ScheduleManager::ScheduleAlwaysOn;
        } else {
            thisFan.AvailSchedPtrNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
            if (thisFan.AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(2),
                                       cAlphaArgs(2),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            } else {
                if (ScheduleManager::HasFractionalScheduleValue(state, thisFan.AvailSchedPtrNum)) {
                    ShowWarningError(state,
                                     format("{}=\"{}\" has fractional values in Schedule={}. Only 0.0 in the schedule value turns the fan off.",
                                            cCurrentModuleObject,
                                            thisFan.Name,
                                            cAlphaArgs(2)));
                }
            }
        }

        thisFan.FanEff = rNumericArgs(1);
        thisFan.DeltaPress = rNumericArgs(2);
        thisFan.MaxAirFlowRate = rNumericArgs(3);
        thisFan.MaxAirFlowRateIsAutosizable = false;
        thisFan.MotEff = 1.0;
        thisFan.MotInAirFrac = 1.0;
        thisFan.MinAirFlowRate = 0.0;
        thisFan.RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        thisFan.MaxAirMassFlowRate = thisFan.MaxAirFlowRate * thisFan.RhoAirStdInit;

        if (thisFan.MaxAirFlowRate == 0.0) {
            ShowWarningError(
                state,
                format("{}=\"{}\" has specified 0.0 max air flow rate. It will not be used in the simulation.", cCurrentModuleObject, thisFan.Name));
        }

        thisFan.InletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                   cAlphaArgs(3),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::FanZoneExhaust,
                                                                   cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent);
        thisFan.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                    cAlphaArgs(4),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::FanZoneExhaust,
                                                                    cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent);

        thisFan.EndUseSubcategoryName = (NumAlphas > 4 && !lAlphaFieldBlanks(5)) ? cAlphaArgs(5) : "General";

        if (NumAlphas > 5 && !lAlphaFieldBlanks(6)) {
            thisFan.FlowFractSchedNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(6));
            if (thisFan.FlowFractSchedNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(6),
                                       cAlphaArgs(6),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            } else if (thisFan.FlowFractSchedNum > 0) {
                if (!ScheduleManager::CheckScheduleValueMinMax(state, thisFan.FlowFractSchedNum, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(state,
                                    format("{}{}: invalid {} for {}={}",
                                           RoutineName,
                                           cCurrentModuleObject,
                                           cAlphaFieldNames(6),
                                           cAlphaFieldNames(1),
                                           cAlphaArgs(1)));
                    ShowContinueError(state, format("Error found in {} = {}", cAlphaFieldNames(6), cAlphaArgs(6)));
                    ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                    ErrorsFound = true;
                }
            }
        } else {
            thisFan.FlowFractSchedNum = ScheduleManager::ScheduleAlwaysOn;
        }

        if (NumAlphas > 6 && !lAlphaFieldBlanks(7)) {
            thisFan.AvailManagerMode = static_cast<AvailabilityManagerCoupling>(getEnumValue(couplingsUC, cAlphaArgs(7)));
            if (thisFan.AvailManagerMode == AvailabilityManagerCoupling::Invalid) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(7),
                                       cAlphaArgs(7),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        } else {
            thisFan.AvailManagerMode = AvailabilityManagerCoupling::Coupled;
        }

        if (NumAlphas > 7 && !lAlphaFieldBlanks(8)) {
            thisFan.MinTempLimitSchedNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(8));
            if (thisFan.MinTempLimitSchedNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(8),
                                       cAlphaArgs(8),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        } else {
            thisFan.MinTempLimitSchedNum = 0;
        }

        if (NumAlphas > 8 && !lAlphaFieldBlanks(9)) {

            if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing) {
                // do not include adjusted for "balanced" exhaust flow in the zone total return calculation
                ShowWarningError(state,
                                 format("{}{}: invalid {} = {} for {}={}",
                                        RoutineName,
                                        cCurrentModuleObject,
                                        cAlphaFieldNames(9),
                                        cAlphaArgs(9),
                                        cAlphaFieldNames(1),
                                        cAlphaArgs(1)));
                ShowContinueError(state, "When zone air mass flow balance is enforced, this input field should be left blank.");
                ShowContinueError(state, "This schedule will be ignored in the simulation.");
                thisFan.BalancedFractSchedNum = 0;
            } else {
                thisFan.BalancedFractSchedNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(9));
                if (thisFan.BalancedFractSchedNum == 0) {
                    ShowSevereError(state,
                                    format("{}{}: invalid {} entered ={} for {}={}",
                                           RoutineName,
                                           cCurrentModuleObject,
                                           cAlphaFieldNames(9),
                                           cAlphaArgs(9),
                                           cAlphaFieldNames(1),
                                           cAlphaArgs(1)));
                    ErrorsFound = true;
                } else if (thisFan.BalancedFractSchedNum > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(state, thisFan.BalancedFractSchedNum, ">=", 0.0, "<=", 1.0)) {
                        ShowSevereError(state,
                                        format("{}{}: invalid {} for {}={}",
                                               RoutineName,
                                               cCurrentModuleObject,
                                               cAlphaFieldNames(9),
                                               cAlphaFieldNames(1),
                                               cAlphaArgs(1)));
                        ShowContinueError(state, format("Error found in {} = {}", cAlphaFieldNames(9), cAlphaArgs(9)));
                        ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
                        ErrorsFound = true;
                    }
                }
            }
        } else {
            thisFan.BalancedFractSchedNum = 0;
        }

    } // end of Zone Exhaust Fan loop

    for (int OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
        auto &thisFan = Fan(FanNum);
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
        thisFan.Name = cAlphaArgs(1);
        thisFan.fanType = HVAC::FanType::OnOff;
        thisFan.AvailSchedName = cAlphaArgs(2);
        if (lAlphaFieldBlanks(2)) {
            thisFan.AvailSchedPtrNum = ScheduleManager::ScheduleAlwaysOn;
        } else {
            thisFan.AvailSchedPtrNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
            if (thisFan.AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(2),
                                       cAlphaArgs(2),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        thisFan.FanEff = rNumericArgs(1);
        thisFan.DeltaPress = rNumericArgs(2);
        thisFan.MaxAirFlowRate = rNumericArgs(3);
        if (thisFan.MaxAirFlowRate == 0.0) {
            ShowWarningError(
                state,
                format("{}=\"{}\" has specified 0.0 max air flow rate. It will not be used in the simulation.", cCurrentModuleObject, thisFan.Name));
        }
        thisFan.MaxAirFlowRateIsAutosizable = true;
        //       the following two structure variables are set here, as well as in InitFan, for the Heat Pump:Water Heater object
        //       (Standard Rating procedure may be called before BeginEnvirFlag is set to TRUE, if so MaxAirMassFlowRate = 0)
        thisFan.RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        thisFan.MaxAirMassFlowRate = thisFan.MaxAirFlowRate * thisFan.RhoAirStdInit;

        thisFan.MotEff = rNumericArgs(4);
        thisFan.MotInAirFrac = rNumericArgs(5);
        thisFan.MinAirFlowRate = 0.0;

        thisFan.InletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                   cAlphaArgs(3),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::FanOnOff,
                                                                   cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent);
        thisFan.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                    cAlphaArgs(4),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::FanOnOff,
                                                                    cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent);

        if (NumAlphas > 4 && !lAlphaFieldBlanks(5)) {
            thisFan.FanPowerRatAtSpeedRatCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(5));
        }

        if (NumAlphas > 5 && !lAlphaFieldBlanks(6)) {
            thisFan.FanEffRatioCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(6));
        }

        thisFan.EndUseSubcategoryName = (NumAlphas > 6 && !lAlphaFieldBlanks(7)) ? cAlphaArgs(7) : "General";

        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Air Nodes");

    } // end Number of Simple  ON-OFF FAN Loop

    cCurrentModuleObject = "FanPerformance:NightVentilation";

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
    for (int NVPerfNum = 1; NVPerfNum <= state.dataFans->NumNightVentPerf; ++NVPerfNum) {
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
        NightVentPerf(NVPerfNum).FanName = cAlphaArgs(1);
        NightVentPerf(NVPerfNum).FanEff = rNumericArgs(1);
        NightVentPerf(NVPerfNum).DeltaPress = rNumericArgs(2);
        NightVentPerf(NVPerfNum).MaxAirFlowRate = rNumericArgs(3);
        NightVentPerf(NVPerfNum).MotEff = rNumericArgs(4);
        NightVentPerf(NVPerfNum).MotInAirFrac = rNumericArgs(5);
        // find the corresponding fan
        bool NVPerfFanFound = false;
        for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            if (NightVentPerf(NVPerfNum).FanName == Fan(FanNum).Name) {
                NVPerfFanFound = true;
                Fan(FanNum).NVPerfNum = NVPerfNum;
                break;
            }
        }
        if (!NVPerfFanFound) {
            ShowSevereError(state, format("{}, fan name not found={}", cCurrentModuleObject, cAlphaArgs(1)));
            ErrorsFound = true;
        }
    }

    for (int CompModelFanNum = 1; CompModelFanNum <= NumCompModelFan; ++CompModelFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + CompModelFanNum;
        auto &thisFan = Fan(FanNum);

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
        thisFan.Name = cAlphaArgs(1); // Fan name
        thisFan.fanType = HVAC::FanType::ComponentModel;

        thisFan.InletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                   cAlphaArgs(2),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::FanComponentModel,
                                                                   cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent); // Air inlet node name
        thisFan.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                    cAlphaArgs(3),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::FanComponentModel,
                                                                    cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent); // Air outlet node name

        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Air Nodes");

        thisFan.AvailSchedName = cAlphaArgs(4); // Availability schedule name
        if (lAlphaFieldBlanks(4)) {
            thisFan.AvailSchedPtrNum = 0;
        } else {
            thisFan.AvailSchedPtrNum = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(4));
            if (thisFan.AvailSchedPtrNum == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       cAlphaFieldNames(4),
                                       cAlphaArgs(4),
                                       cAlphaFieldNames(1),
                                       cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        thisFan.MaxAirFlowRate = rNumericArgs(1);
        if (thisFan.MaxAirFlowRate == 0.0) {
            ShowWarningError(
                state,
                format("{}=\"{}\" has specified 0.0 max air flow rate. It will not be used in the simulation.", cCurrentModuleObject, thisFan.Name));
        }
        thisFan.MaxAirFlowRateIsAutosizable = true;
        thisFan.MinAirFlowRate = rNumericArgs(2);

        thisFan.FanSizingFactor = rNumericArgs(3);                                     // Fan max airflow sizing factor [-]
        thisFan.FanWheelDia = rNumericArgs(4);                                         // Fan wheel outer diameter [m]
        thisFan.FanOutletArea = rNumericArgs(5);                                       // Fan outlet area [m2]
        thisFan.FanMaxEff = rNumericArgs(6);                                           // Fan maximum static efficiency [-]
        thisFan.EuMaxEff = rNumericArgs(7);                                            // Euler number at Fan maximum static efficiency [-]
        thisFan.FanMaxDimFlow = rNumericArgs(8);                                       // Fan maximum dimensionless airflow [-]
        thisFan.PulleyDiaRatio = rNumericArgs(9);                                      // Motor/fan pulley diameter ratio [-]
        thisFan.BeltMaxTorque = rNumericArgs(10);                                      // Belt maximum torque [N-m, autosizable]
        thisFan.BeltSizingFactor = rNumericArgs(11);                                   // Belt sizing factor [-]
        thisFan.BeltTorqueTrans = rNumericArgs(12);                                    // Belt fractional torque transition Region 1-2 [-]
        thisFan.MotorMaxSpd = rNumericArgs(13);                                        // Motor maximum speed [rpm]
        thisFan.MotorMaxOutPwr = rNumericArgs(14);                                     // Motor maximum output power [W, autosizable]
        thisFan.MotorSizingFactor = rNumericArgs(15);                                  // Motor sizing factor [-]
        thisFan.MotInAirFrac = rNumericArgs(16);                                       // Fraction of fan and motor losses to airstream [-]
        thisFan.VFDEffType = cAlphaArgs(5);                                            // VFD efficiency type [Speed or Power]
        thisFan.VFDMaxOutPwr = rNumericArgs(17);                                       // VFD maximum output power [W, autosizable]
        thisFan.VFDSizingFactor = rNumericArgs(18);                                    // VFD sizing factor [-]
        thisFan.PressRiseCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(6));      // Fan pressure rise curve
        thisFan.PressResetCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(7));     // Duct static pressure reset curve
        thisFan.PLFanEffNormCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(8));   // Fan part-load eff (normal) curve
        thisFan.PLFanEffStallCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(9));  // Fan part-load eff (stall) curve
        thisFan.DimFlowNormCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(10));   // Fan dim airflow (normal) curve
        thisFan.DimFlowStallCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(11));  // Fan dim airflow (stall) curve
        thisFan.BeltMaxEffCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(12));    // Belt max eff curve
        thisFan.PLBeltEffReg1CurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(13)); // Belt part-load eff Region 1 curve
        thisFan.PLBeltEffReg2CurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(14)); // Belt part-load eff Region 2 curve
        thisFan.PLBeltEffReg3CurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(15)); // Belt part-load eff Region 3 curve
        thisFan.MotorMaxEffCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(16));   // Motor max eff curve
        thisFan.PLMotorEffCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(17));    // Motor part-load eff curve
        thisFan.VFDEffCurveIndex = Curve::GetCurveIndex(state, cAlphaArgs(18));        // VFD eff curve

        thisFan.EndUseSubcategoryName = (NumAlphas > 18) ? cAlphaArgs(19) : "General";

    } // end Number of Component Model FAN Loop

    cAlphaArgs.deallocate();
    cAlphaFieldNames.deallocate();
    lAlphaFieldBlanks.deallocate();
    cNumericFieldNames.deallocate();
    lNumericFieldBlanks.deallocate();
    rNumericArgs.deallocate();

    // Check Fans
    for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
        for (int checkNum = FanNum + 1; checkNum <= state.dataFans->NumFans; ++checkNum) {
            if (Fan(FanNum).InletNodeNum == Fan(checkNum).InletNodeNum) {
                ErrorsFound = true;
                ShowSevereError(state, "GetFanInput, duplicate fan inlet node names, must be unique for fans.");
                ShowContinueError(state,
                                  format("Fan={}:{} and Fan={}:{}.",
                                         HVAC::fanTypeNames[(int)Fan(FanNum).fanType],
                                         Fan(FanNum).Name,
                                         HVAC::fanTypeNames[(int)Fan(checkNum).fanType],
                                         Fan(checkNum).Name));
                ShowContinueError(state, format("Inlet Node Name=\"{}\".", state.dataLoopNodes->NodeID(Fan(FanNum).InletNodeNum)));
            }
            if (Fan(FanNum).OutletNodeNum == Fan(checkNum).OutletNodeNum) {
                ErrorsFound = true;
                ShowSevereError(state, "GetFanInput, duplicate fan outlet node names, must be unique for fans.");
                ShowContinueError(state,
                                  format("Fan={}:{} and Fan={}:{}.",
                                         HVAC::fanTypeNames[(int)Fan(FanNum).fanType],
                                         Fan(FanNum).Name,
                                         HVAC::fanTypeNames[(int)Fan(checkNum).fanType],
                                         Fan(checkNum).Name));
                ShowContinueError(state, format("Outlet Node Name=\"{}\".", state.dataLoopNodes->NodeID(Fan(FanNum).OutletNodeNum)));
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, format("{}Errors found in input.  Program terminates.", RoutineName));
    }

    for (auto &thisFan : state.dataFans->Fan) {
        // Setup Report variables for the Fans  CurrentModuleObject='Fans'
        SetupOutputVariable(state,
                            "Fan Electricity Rate",
                            Constant::Units::W,
                            thisFan.FanPower,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            thisFan.Name);
        SetupOutputVariable(state,
                            "Fan Rise in Air Temperature",
                            Constant::Units::deltaC,
                            thisFan.DeltaTemp,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            thisFan.Name);
        SetupOutputVariable(state,
                            "Fan Heat Gain to Air",
                            Constant::Units::W,
                            thisFan.PowerLossToAir,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            thisFan.Name);
        SetupOutputVariable(state,
                            "Fan Electricity Energy",
                            Constant::Units::J,
                            thisFan.FanEnergy,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Sum,
                            thisFan.Name,
                            Constant::eResource::Electricity,
                            OutputProcessor::Group::HVAC,
                            OutputProcessor::EndUseCat::Fans,
                            thisFan.EndUseSubcategoryName);
        SetupOutputVariable(state,
                            "Fan Air Mass Flow Rate",
                            Constant::Units::kg_s,
                            thisFan.OutletAirMassFlowRate,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            thisFan.Name);
        if ((thisFan.fanType == HVAC::FanType::Exhaust) && (thisFan.BalancedFractSchedNum > 0)) {
            SetupOutputVariable(state,
                                "Fan Unbalanced Air Mass Flow Rate",
                                Constant::Units::kg_s,
                                thisFan.UnbalancedOutletMassFlowRate,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisFan.Name);
            SetupOutputVariable(state,
                                "Fan Balanced Air Mass Flow Rate",
                                Constant::Units::kg_s,
                                thisFan.BalancedOutletMassFlowRate,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisFan.Name);
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Fan Maximum Mass Flow Rate", thisFan.Name, "[kg/s]", thisFan.MaxAirMassFlowRate);
            SetupEMSActuator(
                state, "Fan", thisFan.Name, "Fan Air Mass Flow Rate", "[kg/s]", thisFan.EMSMaxMassFlowOverrideOn, thisFan.EMSAirMassFlowValue);
            SetupEMSInternalVariable(state, "Fan Nominal Pressure Rise", thisFan.Name, "[Pa]", thisFan.DeltaPress);
            SetupEMSActuator(state, "Fan", thisFan.Name, "Fan Pressure Rise", "[Pa]", thisFan.EMSFanPressureOverrideOn, thisFan.EMSFanPressureValue);
            SetupEMSInternalVariable(state, "Fan Nominal Total Efficiency", thisFan.Name, "[fraction]", thisFan.FanEff);
            SetupEMSActuator(state, "Fan", thisFan.Name, "Fan Total Efficiency", "[fraction]", thisFan.EMSFanEffOverrideOn, thisFan.EMSFanEffValue);

            SetupEMSActuator(state,
                             "Fan",
                             thisFan.Name,
                             "Fan Autosized Air Flow Rate",
                             "[m3/s]",
                             thisFan.MaxAirFlowRateEMSOverrideOn,
                             thisFan.MaxAirFlowRateEMSOverrideValue);
        }
    }

    for (int OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum) {
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
        SetupOutputVariable(state,
                            "Fan Runtime Fraction",
                            Constant::Units::None,
                            Fan(FanNum).FanRuntimeFraction,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            Fan(FanNum).Name);
    }

    bool anyRan;
    EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::ComponentGetInput, anyRan, ObjexxFCL::Optional_int_const());
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

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the Fan Components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    auto &fan = state.dataFans->Fan(FanNum);

    if (state.dataFans->MyOneTimeFlag) {
        state.dataFans->MyEnvrnFlag.dimension(state.dataFans->NumFans, true);
        state.dataFans->MyOneTimeFlag = false;
    }

    // need to check all fans to see if they are on Zone Equipment List or issue warning
    if (!state.dataFans->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataFans->ZoneEquipmentListChecked = true;
        for (int Loop = 1; Loop <= state.dataFans->NumFans; ++Loop) {
            if (state.dataFans->Fan(Loop).fanType != HVAC::FanType::Exhaust) continue;
            if (DataZoneEquipment::CheckZoneEquipmentList(
                    state, HVAC::fanTypeNames[(int)state.dataFans->Fan(Loop).fanType], state.dataFans->Fan(Loop).Name))
                continue;
            ShowSevereError(state,
                            format("InitFans: Fan=[{},{}] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.",
                                   HVAC::fanTypeNames[(int)state.dataFans->Fan(Loop).fanType],
                                   state.dataFans->Fan(Loop).Name));
        }
    }

    if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(FanNum)) {

        SizeFan(state, FanNum);
        // Set the loop cycling flag
        if (fan.fanType == HVAC::FanType::OnOff) {
            if (state.dataSize->CurSysNum > 0) {
                state.dataAirLoop->AirLoopControlInfo(state.dataSize->CurSysNum).CyclingFan = true;
            }
        }

        state.dataFans->MySizeFlag(FanNum) = false;
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataFans->MyEnvrnFlag(FanNum)) {

        // For all Fan inlet nodes convert the Volume flow to a mass flow
        int OutNode = fan.OutletNodeNum;
        fan.RhoAirStdInit = state.dataEnvrn->StdRhoAir;

        // Change the Volume Flow Rates to Mass Flow Rates

        fan.MaxAirMassFlowRate = fan.MaxAirFlowRate * fan.RhoAirStdInit;
        if (fan.FanMinAirFracMethod == HVAC::MinFrac) {
            fan.MinAirFlowRate = fan.MaxAirFlowRate * fan.FanMinFrac;
            fan.MinAirMassFlowRate = fan.MinAirFlowRate * fan.RhoAirStdInit;
        } else if (fan.FanMinAirFracMethod == HVAC::FixedMin) {
            fan.MinAirFlowRate = fan.FanFixedMin;
            fan.MinAirMassFlowRate = fan.MinAirFlowRate * fan.RhoAirStdInit;
        }
        if (fan.NVPerfNum > 0) {
            state.dataFans->NightVentPerf(fan.NVPerfNum).MaxAirMassFlowRate =
                state.dataFans->NightVentPerf(fan.NVPerfNum).MaxAirFlowRate * fan.RhoAirStdInit;
        }

        // Init the Node Control variables
        state.dataLoopNodes->Node(OutNode).MassFlowRateMax = fan.MaxAirMassFlowRate;
        // According to the IO Ref guide:
        // "Note that this field is only used to calculate the fan power.
        // This field does not enforce the system air flow rate during simulation"
        // Node(OutNode).MassFlowRateMin = fan.MinAirMassFlowRate;

        // Initialize all report variables to a known state at beginning of simulation
        fan.FanPower = 0.0;
        fan.DeltaTemp = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.FanEnergy = 0.0;

        state.dataFans->MyEnvrnFlag(FanNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataFans->MyEnvrnFlag(FanNum) = true;
    }

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.

    // Do a check and make sure that the max and min available(control) flow is
    // between the physical max and min for the Fan while operating.

    auto const &inletNode = state.dataLoopNodes->Node(fan.InletNodeNum);
    auto const &outletNode = state.dataLoopNodes->Node(fan.OutletNodeNum);

    fan.MassFlowRateMaxAvail = min(outletNode.MassFlowRateMax, inletNode.MassFlowRateMaxAvail);
    fan.MassFlowRateMinAvail = min(max(outletNode.MassFlowRateMin, inletNode.MassFlowRateMinAvail), inletNode.MassFlowRateMaxAvail);

    // Load the node data in this section for the component simulation
    // First need to make sure that the MassFlowRate is between the max and min avail.
    if (fan.fanType != HVAC::FanType::Exhaust) {
        fan.InletAirMassFlowRate = min(inletNode.MassFlowRate, fan.MassFlowRateMaxAvail);
        fan.InletAirMassFlowRate = max(fan.InletAirMassFlowRate, fan.MassFlowRateMinAvail);
    } else { // zone exhaust fans
        fan.MassFlowRateMaxAvail = fan.MaxAirMassFlowRate;
        fan.MassFlowRateMinAvail = 0.0;
        if (fan.FlowFractSchedNum > 0) { // modulate flow
            fan.InletAirMassFlowRate = fan.MassFlowRateMaxAvail * ScheduleManager::GetCurrentScheduleValue(state, fan.FlowFractSchedNum);
            fan.InletAirMassFlowRate = max(0.0, fan.InletAirMassFlowRate);
        } else { // always run at max
            fan.InletAirMassFlowRate = fan.MassFlowRateMaxAvail;
        }
        if (fan.EMSMaxMassFlowOverrideOn) fan.InletAirMassFlowRate = min(fan.EMSAirMassFlowValue, fan.MassFlowRateMaxAvail);
    }

    // Then set the other conditions
    fan.InletAirTemp = inletNode.Temp;
    fan.InletAirHumRat = inletNode.HumRat;
    fan.InletAirEnthalpy = inletNode.Enthalpy;
}

void SizeFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2001
    //       MODIFIED       Craig Wray August 2010 - added fan, belt, motor, and VFD component sizing
    //                      August 2013 Daeho Kang, add component sizing table entries

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing fans for which flow rates have not been
    // specified in the input, or when fan component sizes have not been specified

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeFan: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool bPRINT = true;   // TRUE if sizing is reported to output (eio)
    int FieldNum = 2;     // IDD numeric field number where input field description is found
    int NumFansSized = 0; // counter used to deallocate temporary string array after all fans have been sized

    auto &fan = state.dataFans->Fan(FanNum);

    if (fan.fanType == HVAC::FanType::ComponentModel) {
        FieldNum = 1;
    } else {
        FieldNum = 3;
    }
    std::string SizingString = state.dataFans->FanNumericFields(FanNum).FieldNames(FieldNum) + " [m3/s]";

    Real64 TempFlow = fan.MaxAirFlowRate; // autosized flow rate of fan [m3/s]
    state.dataSize->DataAutosizable = fan.MaxAirFlowRateIsAutosizable;
    std::string CompName = fan.Name;
    state.dataSize->DataEMSOverrideON = fan.MaxAirFlowRateEMSOverrideOn;
    state.dataSize->DataEMSOverride = fan.MaxAirFlowRateEMSOverrideValue;

    bool errorsFound = false;
    SystemAirFlowSizer sizerSystemAirFlow;
    sizerSystemAirFlow.overrideSizingString(SizingString);
    sizerSystemAirFlow.initializeWithinEP(state, HVAC::fanTypeNames[(int)fan.fanType], CompName, bPRINT, RoutineName);
    fan.MaxAirFlowRate = sizerSystemAirFlow.size(state, TempFlow, errorsFound);

    state.dataSize->DataAutosizable = true;
    state.dataSize->DataEMSOverrideON = false;
    state.dataSize->DataEMSOverride = 0.0;

    Real64 FanVolFlow = fan.MaxAirFlowRate; // Maximum volumetric airflow through fan [m3/s at standard conditions]
    if (fan.fanType == HVAC::FanType::ComponentModel) {
        // Get air density at standard conditions and get mass airflow through fan
        // From WeatherManager:
        //   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
        //   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
        // From PsychRoutines:
        //   w=MAX(dw,1.0d-5)
        //   rhoair = pb/(287.d0*(tdb+Constant::Kelvin())*(1.0d0+1.6077687d0*w))
        Real64 RhoAir = state.dataEnvrn->StdRhoAir;

        // Adjust max fan volumetric airflow using fan sizing factor
        FanVolFlow *= fan.FanSizingFactor; //[m3/s at standard conditions]

        // Calculate max fan static pressure rise using max fan volumetric flow, std air density, air-handling system characteristics,
        //   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
        Real64 DuctStaticPress = Curve::CurveValue(state, fan.PressResetCurveIndex, FanVolFlow);               // Duct static pressure setpoint [Pa]
        Real64 DeltaPressTot = Curve::CurveValue(state, fan.PressRiseCurveIndex, FanVolFlow, DuctStaticPress); // Max fan total pressure rise [Pa]
        Real64 FanOutletVelPress = 0.5 * RhoAir * pow_2(FanVolFlow / fan.FanOutletArea); // Max fan outlet velocity pressure [Pa]
        // Outlet velocity pressure cannot exceed total pressure rise
        FanOutletVelPress = min(FanOutletVelPress, DeltaPressTot);
        fan.DeltaPress = DeltaPressTot - FanOutletVelPress; // Max fan static pressure rise [Pa]

        // Calculate max fan air power using volumetric flow abd corresponding fan static pressure rise
        fan.FanAirPower = FanVolFlow * fan.DeltaPress; //[W]

        // Calculate fan wheel efficiency at max fan volumetric flow and corresponding fan static pressure rise,
        //   using fan characteristics and Wray dimensionless fan static efficiency model
        Real64 EulerNum = (fan.DeltaPress * pow_4(fan.FanWheelDia)) / (RhoAir * pow_2(FanVolFlow)); //[-]
        Real64 NormalizedEulerNum = std::log10(EulerNum / fan.EuMaxEff);
        if (NormalizedEulerNum <= 0.0) {
            fan.FanWheelEff = Curve::CurveValue(state, fan.PLFanEffNormCurveIndex, NormalizedEulerNum);
        } else {
            fan.FanWheelEff = Curve::CurveValue(state, fan.PLFanEffStallCurveIndex, NormalizedEulerNum);
        }
        fan.FanWheelEff *= fan.FanMaxEff;             // [-]
        fan.FanWheelEff = max(fan.FanWheelEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate max fan shaft power using fan air power and fan efficiency
        // at max fan static pressure rise and max fan volumetric flow
        fan.FanShaftPower = (fan.FanAirPower / fan.FanWheelEff); //[W]
        fan.FanShaftPwrMax = fan.FanShaftPower;                  //[W]

        Real64 FanDimFlow; // Fan dimensionless airflow [-]
        // Calculate fan shaft speed, motor speed, and fan torque using Wray dimensionless fan airflow model
        if (NormalizedEulerNum <= 0.0) {
            FanDimFlow = Curve::CurveValue(state, fan.DimFlowNormCurveIndex, NormalizedEulerNum); //[-]
        } else {
            FanDimFlow = Curve::CurveValue(state, fan.DimFlowStallCurveIndex, NormalizedEulerNum); //[-]
        }
        Real64 FanSpdRadS = FanVolFlow / (FanDimFlow * fan.FanMaxDimFlow * pow_3(fan.FanWheelDia)); //[rad/s]
        fan.FanSpd = FanSpdRadS * 9.549296586;                                                      //[rpm, conversion factor is 30/PI]

        if (fan.PulleyDiaRatio == DataSizing::AutoSize) {
            // WRITE(*,*) 'Autosizing pulley drive ratio'
            fan.PulleyDiaRatio = fan.FanSpd / fan.MotorMaxSpd; //[-]
        }

        // For direct-drive, should have PulleyDiaRatio = 1
        Real64 MotorSpeed = fan.FanSpd / fan.PulleyDiaRatio; //[rpm]

        // Check for inconsistent drive ratio and motor speed, and report design fan speed with warning
        if (MotorSpeed > (fan.MotorMaxSpd + 1.e-5)) {
            ShowWarningError(state,
                             format("Drive ratio for {}: {} is too low at design conditions -- check motor speed and drive ratio inputs",
                                    HVAC::fanTypeNames[(int)fan.fanType],
                                    fan.Name));
            ShowContinueError(state, format("...Design fan speed [rev/min]: {:.2R}", fan.FanSpd));
        }

        fan.FanTrq = fan.FanShaftPower / FanSpdRadS; //[N-m]

        if (fan.BeltMaxTorque == DataSizing::AutoSize) {
            // WRITE(*,*) 'Autosizing fan belt'
            fan.BeltMaxTorque = fan.FanTrq; //[N-m]
        }
        // Adjust max belt torque using belt sizing factor
        fan.BeltMaxTorque *= fan.BeltSizingFactor; //[N-m]

        // Check for undersized belt and report design size with warning
        if (fan.FanTrq > (fan.BeltMaxTorque + 1.e-5)) {
            ShowWarningError(state,
                             format("Belt for {}: {} is undersized at design conditions -- check belt inputs",
                                    HVAC::fanTypeNames[(int)fan.fanType],
                                    fan.Name));
            ShowContinueError(state, format("...Design belt output torque (without oversizing) [Nm]: {:.2R}", fan.FanTrq));
        }

        // Calculate belt max efficiency using correlations and coefficients based on AMCA data
        // Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
        Real64 XbeltMax = std::log(fan.FanShaftPwrMax / 746.0); // Natural log of belt output power in hp
        if (fan.BeltMaxEffCurveIndex != 0) {
            fan.BeltMaxEff = std::exp(Curve::CurveValue(state, fan.BeltMaxEffCurveIndex, XbeltMax)); //[-]
        } else {
            fan.BeltMaxEff = 1.0; // No curve specified - use constant efficiency
        }

        // Calculate belt part-load drive efficiency and input power using correlations and coefficients based on ACEEE data
        Real64 FanTrqRatio = fan.FanTrq / fan.BeltMaxTorque; //[-]
        Real64 BeltPLEff;                                    // Belt normalized (part-load) efficiency [-]
        if ((FanTrqRatio <= fan.BeltTorqueTrans) && (fan.PLBeltEffReg1CurveIndex != 0)) {
            BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg1CurveIndex, FanTrqRatio); //[-]
        } else {
            if ((FanTrqRatio > fan.BeltTorqueTrans) && (FanTrqRatio <= 1.0) && (fan.PLBeltEffReg2CurveIndex != 0)) {
                BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg2CurveIndex, FanTrqRatio); //[-]
            } else {
                if ((FanTrqRatio > 1.0) && (fan.PLBeltEffReg3CurveIndex != 0)) {
                    BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg3CurveIndex, FanTrqRatio); //[-]
                } else {
                    BeltPLEff = 1.0; // Direct drive or no curve specified - use constant efficiency
                }
            }
        }
        fan.BeltEff = fan.BeltMaxEff * BeltPLEff;             //[-]
        fan.BeltEff = max(fan.BeltEff, 0.01);                 // Minimum efficiency is 1% to avoid numerical errors
        fan.BeltInputPower = fan.FanShaftPower / fan.BeltEff; //[W]

        if (fan.MotorMaxOutPwr == DataSizing::AutoSize) {
            // WRITE(*,*) 'Autosizing fan motor'
            fan.MotorMaxOutPwr = fan.BeltInputPower;
        }
        // Adjust max motor output power using motor sizing factor
        fan.MotorMaxOutPwr *= fan.MotorSizingFactor; //[W]

        // Check for undersized motor and report design size with warning
        if (fan.BeltInputPower > (fan.MotorMaxOutPwr + 1.e-5)) {
            ShowWarningError(state,
                             format("Motor for {}: {} is undersized at design conditions -- check motor inputs",
                                    HVAC::fanTypeNames[(int)fan.fanType],
                                    fan.Name));
            ShowContinueError(state, format("...Design motor output power (without oversizing) [W]: {:.2R}", fan.BeltInputPower));
        }

        // Calculate motor max efficiency using correlations and coefficients based on MotorMaster+ data
        Real64 XmotorMax = std::log(fan.MotorMaxOutPwr / 746.0); // Natural log of motor output power in hp
        if (fan.MotorMaxEffCurveIndex != 0) {
            fan.MotorMaxEff = Curve::CurveValue(state, fan.MotorMaxEffCurveIndex, XmotorMax); //[-]
        } else {
            fan.MotorMaxEff = 1.0; // No curve specified - use constant efficiency
        }

        Real64 MotorPLEff; // Motor normalized (part-load) efficiency [-]
        // Calculate motor part-load efficiency and input power using correlations and coefficients based on MotorMaster+ data
        Real64 MotorOutPwrRatio = fan.BeltInputPower / fan.MotorMaxOutPwr; //[-]
        if (fan.PLMotorEffCurveIndex != 0) {
            MotorPLEff = Curve::CurveValue(state, fan.PLMotorEffCurveIndex, MotorOutPwrRatio); //[-]
        } else {
            MotorPLEff = 1.0; // No curve specified - use constant efficiency
        }
        fan.MotEff = fan.MotorMaxEff * MotorPLEff; //[-]
        fan.MotEff = max(fan.MotEff, 0.01);        // Minimum efficiency is 1% to avoid numerical errors

        // Calculate motor input power using belt input power and motor efficiency
        fan.MotorInputPower = fan.BeltInputPower / fan.MotEff; //[W]

        // Calculate max VFD efficiency and input power using correlations and coefficients based on VFD type
        if ((fan.VFDEffType == "SPEED") && (fan.VFDEffCurveIndex != 0)) {
            Real64 VFDSpdRatio = MotorSpeed / fan.MotorMaxSpd; // Ratio of motor speed to motor max speed [-]
            fan.VFDEff = Curve::CurveValue(state, fan.VFDEffCurveIndex, VFDSpdRatio);
        } else {
            if ((fan.VFDEffType == "POWER") && (fan.VFDEffCurveIndex != 0)) {
                if (fan.VFDMaxOutPwr == DataSizing::AutoSize) {
                    // WRITE(*,*) 'Autosizing fan VFD'
                    fan.VFDMaxOutPwr = fan.MotorInputPower;
                }
                // Adjust max VFD output power using VFD sizing factor
                fan.VFDMaxOutPwr *= fan.VFDSizingFactor; //[W]

                // Check for undersized VFD and report design size with warning
                if (fan.MotorInputPower > (fan.VFDMaxOutPwr + 1.e-5)) {
                    ShowWarningError(state,
                                     format("VFD for {}: {} is undersized at design conditions -- check VFD inputs",
                                            HVAC::fanTypeNames[(int)fan.fanType],
                                            fan.Name));
                    ShowContinueError(state, format("...Design VFD output power (without oversizing) [W]: {:.2R}", fan.MotorInputPower));
                }

                Real64 VFDOutPwrRatio = fan.MotorInputPower / fan.VFDMaxOutPwr; // Ratio of VFD output power to max VFD output power [-]
                fan.VFDEff = Curve::CurveValue(state, fan.VFDEffCurveIndex, VFDOutPwrRatio);
            } else {
                // No curve specified - use constant efficiency
                fan.VFDMaxOutPwr = 0.0;
                fan.VFDEff = 0.97;
            }
        }
        fan.VFDEff = max(fan.VFDEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate VFD "rated" input power using motor input power and VFD efficiency
        Real64 RatedPower = fan.MotorInputPower / fan.VFDEff; //[W]

        // Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
        // Equivalent to fan%FanAirPower / fan%FanPower
        fan.FanEff = fan.FanWheelEff * fan.BeltEff * fan.MotEff * fan.VFDEff;

        // Report fan, belt, motor, and VFD characteristics at design condition to .eio file
        std::string_view fanTypeName = HVAC::fanTypeNames[(int)fan.fanType];

        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Fan Airflow [m3/s]", FanVolFlow);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Fan Static Pressure Rise [Pa]", fan.DeltaPress);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Fan Shaft Power [W]", fan.FanShaftPower);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Motor Output Power [W]", fan.MotorMaxOutPwr);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design VFD Output Power [W]", fan.VFDMaxOutPwr);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Rated Power [W]", RatedPower);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Drive Ratio []", fan.PulleyDiaRatio);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Belt Output Torque [Nm]", fan.BeltMaxTorque);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Fan Efficiency  []", fan.FanWheelEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Maximum Belt Efficiency []", fan.BeltMaxEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Belt Efficiency []", fan.BeltEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Maximum Motor Efficiency []", fan.MotorMaxEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Motor Efficiency []", fan.MotEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design VFD Efficiency []", fan.VFDEff);
        BaseSizer::reportSizerOutput(state, fanTypeName, fan.Name, "Design Combined Efficiency []", fan.FanEff);
    } // End fan component sizing

    // Rearrange order to match table and use FanVolFlow to calculate RatedPower
    // ALSO generates values if Component Model fan, for which DeltaPress and FanEff vary with flow
    OutputReportPredefined::PreDefTableEntry(
        state, state.dataOutRptPredefined->pdchFanType, fan.Name, HVAC::fanTypeNames[(int)fan.fanType]);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanTotEff, fan.Name, fan.FanEff);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanDeltaP, fan.Name, fan.DeltaPress);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanVolFlow, fan.Name, FanVolFlow);
    Real64 RatedPower = FanVolFlow * fan.DeltaPress / fan.FanEff; // total fan power
    if (fan.fanType != HVAC::FanType::ComponentModel) {
        fan.DesignPointFEI = Fans::FanSystem::report_fei(state, FanVolFlow, RatedPower, fan.DeltaPress, state.dataEnvrn->StdRhoAir);
    }
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwr, fan.Name, RatedPower);
    if (FanVolFlow != 0.0) {
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwrPerFlow, fan.Name, RatedPower / FanVolFlow);
    }
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorIn, fan.Name, fan.MotInAirFrac);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEndUse, fan.Name, fan.EndUseSubcategoryName);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEnergyIndex, fan.Name, fan.DesignPointFEI);

    // Std 229 Fans (Fans.cc)
    OutputReportPredefined::PreDefTableEntry(
        state, state.dataOutRptPredefined->pdchFanPurpose, fan.Name, "N/A"); // fan.FanType); // purpose? not the same
    OutputReportPredefined::PreDefTableEntry(state,
                                             state.dataOutRptPredefined->pdchFanAutosized,
                                             fan.Name,
                                             fan.MaxAirFlowRateIsAutosizable ? "Yes" : "No"); // autosizable vs. autosized equivalent?
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorEff, fan.Name, fan.MotEff);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorHeatToZoneFrac, fan.Name, fan.MotInAirFrac);
    OutputReportPredefined::PreDefTableEntry(state,
                                             state.dataOutRptPredefined->pdchFanAirLoopName,
                                             fan.Name,
                                             fan.AirLoopNum > 0 ? state.dataAirSystemsData->PrimaryAirSystems(fan.AirLoopNum).Name : "N/A");

    if (fan.NVPerfNum > 0) {
        if (state.dataFans->NightVentPerf(fan.NVPerfNum).MaxAirFlowRate == DataSizing::AutoSize) {
            state.dataFans->NightVentPerf(fan.NVPerfNum).MaxAirFlowRate = fan.MaxAirFlowRate;
        }
    }

    // Now that sizing is done, do check if the design point of fan is covered in the fault Fan Curve
    if (fan.FaultyFilterFlag) {
        int jFault_AirFilter = fan.FaultyFilterIndex;

        // Check fault availability schedules
        if (!state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).CheckFaultyAirFilterFanCurve(state)) {
            ShowSevereError(state,
                            format("FaultModel:Fouling:AirFilter = \"{}\"", state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).Name));
            ShowContinueError(state,
                              format("Invalid Fan Curve Name = \"{}\" does not cover ",
                                     state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurve));
            ShowContinueError(state, format("the operational point of Fan {}", fan.Name));
            ShowFatalError(
                state,
                format("SizeFan: Invalid FaultModel:Fouling:AirFilter={}", state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).Name));
        }
    }

    if (++NumFansSized == state.dataFans->NumFans)
        state.dataFans->FanNumericFields.deallocate(); // remove temporary array for field names at end of sizing
}

void SimSimpleFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Brent Griffith, May 2009, added EMS override
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the simple constant volume fan.

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 DeltaPress; // [N/m2]
    Real64 FanEff;
    Real64 MotInAirFrac;
    Real64 MotEff;
    Real64 FanShaftPower; // power delivered to fan shaft

    auto &fan = state.dataFans->Fan(FanNum);

    int NVPerfNum = fan.NVPerfNum;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        auto const &nightVentPerf = state.dataFans->NightVentPerf(NVPerfNum);
        DeltaPress = nightVentPerf.DeltaPress;
        FanEff = nightVentPerf.FanEff;
        MotEff = nightVentPerf.MotEff;
        MotInAirFrac = nightVentPerf.MotInAirFrac;
    } else {
        DeltaPress = fan.DeltaPress;
        FanEff = fan.FanEff;
        MotEff = fan.MotEff;
        MotInAirFrac = fan.MotInAirFrac;
    }

    // For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
    Real64 RhoAir = fan.RhoAirStdInit;
    Real64 MassFlow = fan.InletAirMassFlowRate;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (fan.FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {

        int iFault = fan.FaultyFilterIndex;

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec =
                CalFaultyFanAirFlowReduction(state,
                                             fan.Name,
                                             fan.MaxAirFlowRate,
                                             fan.DeltaPress,
                                             (ScheduleManager::GetCurrentScheduleValue(
                                                  state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) -
                                              1) *
                                                 fan.DeltaPress,
                                             state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MassFlow = min(MassFlow, fan.MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir);
            DeltaPress =
                ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                fan.DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (fan.EMSMaxMassFlowOverrideOn) MassFlow = fan.EMSAirMassFlowValue;
    if (fan.EMSFanPressureOverrideOn) DeltaPress = fan.EMSFanPressureValue;
    if (fan.EMSFanEffOverrideOn) FanEff = fan.EMSFanEffValue;

    MassFlow = min(MassFlow, fan.MaxAirMassFlowRate);
    MassFlow = max(MassFlow, fan.MinAirMassFlowRate);

    // Determine the Fan Schedule for the Time step
    if ((ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0 || state.dataHVACGlobal->TurnFansOn) &&
        !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0) {
        // Fan is operating
        fan.FanPower = max(0.0, MassFlow * DeltaPress / (FanEff * RhoAir)); // total fan power
        FanShaftPower = MotEff * fan.FanPower;                              // power delivered to shaft
        fan.PowerLossToAir = FanShaftPower + (fan.FanPower - FanShaftPower) * MotInAirFrac;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy + fan.PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirMassFlowRate = MassFlow;
        fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        fan.FanPower = 0.0;
        FanShaftPower = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.OutletAirMassFlowRate = 0.0;
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy;
        fan.OutletAirTemp = fan.InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        fan.MassFlowRateMaxAvail = 0.0;
        fan.MassFlowRateMinAvail = 0.0;
    }
}

void SimVariableVolumeFan(EnergyPlusData &state, int const FanNum, ObjexxFCL::Optional<Real64 const> PressureRise)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Phil Haves
    //                      Brent Griffith, May 2009 for EMS
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters

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
    Real64 DeltaPress; // [N/m2 = Pa]
    Real64 FanEff;     // Total fan efficiency - combined efficiency of fan, drive train,
    Real64 MaxAirMassFlowRate;
    Real64 MotInAirFrac;
    Real64 MotEff;
    Real64 PartLoadFrac;

    // Simple Variable Volume Fan - default values from DOE-2
    // Type of Fan          Coeff1       Coeff2       Coeff3        Coeff4      Coeff5
    // INLET VANE DAMPERS   0.35071223   0.30850535   -0.54137364   0.87198823  0.000
    // DISCHARGE DAMPERS    0.37073425   0.97250253   -0.34240761   0.000       0.000
    // VARIABLE SPEED MOTOR 0.0015302446 0.0052080574  1.1086242   -0.11635563  0.000

    auto &fan = state.dataFans->Fan(FanNum);

    int NVPerfNum = fan.NVPerfNum;
    Real64 MaxAirFlowRate = fan.MaxAirFlowRate;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        auto const &nightVentPerf = state.dataFans->NightVentPerf(NVPerfNum);
        DeltaPress = nightVentPerf.DeltaPress;
        FanEff = nightVentPerf.FanEff;
        MotEff = nightVentPerf.MotEff;
        MotInAirFrac = nightVentPerf.MotInAirFrac;
        MaxAirMassFlowRate = nightVentPerf.MaxAirMassFlowRate;
    } else {
        if (present(PressureRise)) {
            DeltaPress = PressureRise;
        } else {
            DeltaPress = fan.DeltaPress;
        }
        FanEff = fan.FanEff;
        MotEff = fan.MotEff;
        MotInAirFrac = fan.MotInAirFrac;
        MaxAirMassFlowRate = fan.MaxAirMassFlowRate;
    }

    Real64 RhoAir = fan.RhoAirStdInit;
    Real64 MassFlow = fan.InletAirMassFlowRate;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (fan.FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation) &&
        (!fan.EMSMaxMassFlowOverrideOn)) {

        int iFault = fan.FaultyFilterIndex;

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec =
                CalFaultyFanAirFlowReduction(state,
                                             fan.Name,
                                             fan.MaxAirFlowRate,
                                             fan.DeltaPress,
                                             (ScheduleManager::GetCurrentScheduleValue(
                                                  state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) -
                                              1) *
                                                 fan.DeltaPress,
                                             state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MaxAirFlowRate = fan.MaxAirFlowRate - FanDesignFlowRateDec;
            MaxAirMassFlowRate = fan.MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
            DeltaPress =
                ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                fan.DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (fan.EMSFanPressureOverrideOn) DeltaPress = fan.EMSFanPressureValue;
    if (fan.EMSFanEffOverrideOn) FanEff = fan.EMSFanEffValue;
    if (fan.EMSMaxMassFlowOverrideOn) MassFlow = fan.EMSAirMassFlowValue;

    MassFlow = min(MassFlow, MaxAirMassFlowRate);

    // Determine the Fan Schedule for the Time step
    if ((ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0 || state.dataHVACGlobal->TurnFansOn) &&
        !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0) {
        // Fan is operating - calculate power loss and enthalpy rise
        //  fan%FanPower = PartLoadFrac*FullMassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
        // Calculate and check limits on fraction of system flow
        // unused0909    MaxFlowFrac = 1.0
        // MinFlowFrac is calculated from the ration of the volume flows and is non-dimensional
        Real64 MinFlowFrac = fan.MinAirFlowRate / MaxAirFlowRate;
        // The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
        Real64 FlowFracActual = MassFlow / MaxAirMassFlowRate;

        // Calculate the part Load Fraction             (PH 7/13/03)

        Real64 FlowFracForPower = max(MinFlowFrac, min(FlowFracActual, 1.0)); // limit flow fraction to allowed range
        if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
            PartLoadFrac = 1.0;
        } else {
            PartLoadFrac = fan.FanCoeff(1) + fan.FanCoeff(2) * FlowFracForPower + fan.FanCoeff(3) * pow_2(FlowFracForPower) +
                           fan.FanCoeff(4) * pow_3(FlowFracForPower) + fan.FanCoeff(5) * pow_4(FlowFracForPower);
        }

        fan.FanPower = max(0.0, PartLoadFrac * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir)); // total fan power (PH 7/13/03)

        Real64 FanShaftPower = MotEff * fan.FanPower; // power delivered to shaft
        fan.PowerLossToAir = FanShaftPower + (fan.FanPower - FanShaftPower) * MotInAirFrac;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy + fan.PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirMassFlowRate = MassFlow;
        fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);

        // KHL/FB, 2/10/2011. NFP implemented as CR 8338.
        // When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
        //  avoid the unrealistic high temperature rise across the fan.
        // TH, 2/15/2011
        // This change caused diffs for VAV systems when fan runs at less than 10% flow conditions.
        //  A potential way to improve is to check the temperature rise across the fan first,
        //  if it is too high (say > 20C) then applies the code.
        Real64 DeltaTAcrossFan = fan.OutletAirTemp - fan.InletAirTemp;
        if (DeltaTAcrossFan > 20.0) {
            // added to address the fan heat issue during low air flow conditions
            Real64 FanPoweratLowMinimum; // Fan Power at Low Minimum Airflow [W]
            Real64 PartLoadFracatLowMin;
            Real64 MinFlowFracLimitFanHeat = 0.10;
            if (FlowFracForPower < MinFlowFracLimitFanHeat) {
                PartLoadFracatLowMin = fan.FanCoeff(1) + fan.FanCoeff(2) * MinFlowFracLimitFanHeat +
                                       fan.FanCoeff(3) * pow_2(MinFlowFracLimitFanHeat) + fan.FanCoeff(4) * pow_3(MinFlowFracLimitFanHeat) +
                                       fan.FanCoeff(5) * pow_4(MinFlowFracLimitFanHeat);
                FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir);
                fan.FanPower = max(0.0, FlowFracForPower * FanPoweratLowMinimum / MinFlowFracLimitFanHeat);
            } else if (FlowFracActual < MinFlowFracLimitFanHeat) {
                PartLoadFracatLowMin = fan.FanCoeff(1) + fan.FanCoeff(2) * MinFlowFracLimitFanHeat +
                                       fan.FanCoeff(3) * pow_2(MinFlowFracLimitFanHeat) + fan.FanCoeff(4) * pow_3(MinFlowFracLimitFanHeat) +
                                       fan.FanCoeff(5) * pow_4(MinFlowFracLimitFanHeat);
                FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / (FanEff * RhoAir);
                fan.FanPower = max(0.0, FlowFracActual * FanPoweratLowMinimum / MinFlowFracLimitFanHeat);
            }
            FanShaftPower = MotEff * fan.FanPower; // power delivered to shaft
            fan.PowerLossToAir = FanShaftPower + (fan.FanPower - FanShaftPower) * MotInAirFrac;
            fan.OutletAirEnthalpy = fan.InletAirEnthalpy + fan.PowerLossToAir / MassFlow;
            // This fan does not change the moisture or Mass Flow across the component
            fan.OutletAirHumRat = fan.InletAirHumRat;
            fan.OutletAirMassFlowRate = MassFlow;
            fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);
        }

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        fan.FanPower = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.OutletAirMassFlowRate = 0.0;
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy;
        fan.OutletAirTemp = fan.InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        fan.MassFlowRateMaxAvail = 0.0;
        fan.MassFlowRateMinAvail = 0.0;
    }
}

void SimOnOffFan(EnergyPlusData &state, int const FanNum, ObjexxFCL::Optional<Real64 const> SpeedRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Unknown
    //       DATE WRITTEN   Unknown
    //       MODIFIED       Shirey, May 2001
    //                      R. Raustad - FSEC, Jan 2009 - added SpeedRatio for multi-speed fans
    //                      Brent Griffith, May 2009 for EMS
    //                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters

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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 EffRatioAtSpeedRatio; // Efficiency ratio at current speed ratio (Curve object)

    auto &fan = state.dataFans->Fan(FanNum);

    Real64 MassFlow = fan.InletAirMassFlowRate;
    Real64 MaxAirMassFlowRate = fan.MaxAirMassFlowRate;
    Real64 DeltaPress = fan.DeltaPress; // [N/m2]
    Real64 FanEff = fan.FanEff;
    Real64 RhoAir = fan.RhoAirStdInit;

    // Faulty fan operations
    // Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
    if (fan.FaultyFilterFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation) &&
        (!fan.EMSMaxMassFlowOverrideOn)) {

        int iFault = fan.FaultyFilterIndex;

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).AvaiSchedPtr) > 0.0) {
            Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

            FanDesignFlowRateDec =
                CalFaultyFanAirFlowReduction(state,
                                             fan.Name,
                                             fan.MaxAirFlowRate,
                                             fan.DeltaPress,
                                             (ScheduleManager::GetCurrentScheduleValue(
                                                  state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) -
                                              1) *
                                                 fan.DeltaPress,
                                             state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterFanCurvePtr);

            // Update MassFlow & DeltaPress of the fan
            MaxAirMassFlowRate = fan.MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
            DeltaPress =
                ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(iFault).FaultyAirFilterPressFracSchePtr) *
                fan.DeltaPress;
        }
    }

    // EMS overwrite MassFlow, DeltaPress, and FanEff
    if (fan.EMSMaxMassFlowOverrideOn) MassFlow = fan.EMSAirMassFlowValue;
    if (fan.EMSFanPressureOverrideOn) DeltaPress = fan.EMSFanPressureValue;
    if (fan.EMSFanEffOverrideOn) FanEff = fan.EMSFanEffValue;

    MassFlow = min(MassFlow, MaxAirMassFlowRate);
    MassFlow = max(MassFlow, fan.MinAirMassFlowRate);
    fan.FanRuntimeFraction = 0.0;

    // Determine the Fan Schedule for the Time step
    if ((ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0 || state.dataHVACGlobal->TurnFansOn) &&
        !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0 && fan.MaxAirMassFlowRate > 0.0) {
        // The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
        Real64 FlowFrac = MassFlow / MaxAirMassFlowRate;

        // Calculate the part load ratio, can't be greater than 1
        Real64 PartLoadRatio = min(1.0, FlowFrac);
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
            fan.FanRuntimeFraction = PartLoadRatio;
        } else {
            fan.FanRuntimeFraction = max(0.0, min(1.0, PartLoadRatio / state.dataHVACGlobal->OnOffFanPartLoadFraction));
        }
        // The fan speed ratio (passed from parent) determines the fan power according to fan laws
        if (present(SpeedRatio)) {
            //    fan%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir*OnOffFanPartLoadFraction)! total fan power
            fan.FanPower = max(0.0, MaxAirMassFlowRate * fan.FanRuntimeFraction * DeltaPress / (FanEff * RhoAir));

            //    Do not modify fan power calculation unless fan power vs speed ratio curve is used.
            if (fan.FanPowerRatAtSpeedRatCurveIndex > 0) {

                //      adjust RTF to be in line with speed ratio (i.e., MaxAirMassFlowRate is not MAX when SpeedRatio /= 1)
                //      PLR = Mdot/MAXFlow => Mdot/(MAXFlow * SpeedRatio), RTF = PLR/PLF => PLR/SpeedRatio/PLF = RTF / SpeedRatio
                if (SpeedRatio > 0.0) fan.FanRuntimeFraction = min(1.0, fan.FanRuntimeFraction / SpeedRatio);

                Real64 SpeedRaisedToPower = Curve::CurveValue(state, fan.FanPowerRatAtSpeedRatCurveIndex, SpeedRatio);
                if (SpeedRaisedToPower < 0.0) {
                    if (fan.OneTimePowerRatioCheck && !state.dataGlobal->WarmupFlag) {
                        ShowSevereError(state, format("{} = {}\"", HVAC::fanTypeNames[(int)fan.fanType], fan.Name));
                        ShowContinueError(state, "Error in Fan Power Ratio curve. Curve output less than 0.0.");
                        ShowContinueError(state, format("Curve output = {:.5T}, fan speed ratio = {:.5T}", SpeedRaisedToPower, SpeedRatio));
                        ShowContinueError(state, "Check curve coefficients to ensure proper power ratio as a function of fan speed ratio.");
                        ShowContinueError(state, "Resetting Fan Power Ratio curve output to 0.0 and the simulation continues.");
                        ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        fan.OneTimePowerRatioCheck = false;
                    }
                    SpeedRaisedToPower = 0.0;
                }
                if (fan.FanEffRatioCurveIndex > 0 && !state.dataGlobal->WarmupFlag) {
                    EffRatioAtSpeedRatio = Curve::CurveValue(state, fan.FanEffRatioCurveIndex, SpeedRatio);
                    if (EffRatioAtSpeedRatio < 0.01) {
                        if (fan.OneTimeEffRatioCheck && !state.dataGlobal->WarmupFlag) {
                            ShowSevereError(state, format("{} = {}\"", HVAC::fanTypeNames[(int)fan.fanType], fan.Name));
                            ShowContinueError(state, "Error in Fan Efficiency Ratio curve. Curve output less than 0.01.");
                            ShowContinueError(state, format("Curve output = {:.5T}, fan speed ratio = {:.5T}", EffRatioAtSpeedRatio, SpeedRatio));
                            ShowContinueError(state, "Check curve coefficients to ensure proper efficiency ratio as a function of fan speed ratio.");
                            ShowContinueError(state, "Resetting Fan Efficiency Ratio curve output to 0.01 and the simulation continues.");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                            fan.OneTimeEffRatioCheck = false;
                        }
                        EffRatioAtSpeedRatio = 0.01;
                    }
                } else {
                    EffRatioAtSpeedRatio = 1.0;
                }
                fan.FanPower *= SpeedRaisedToPower / EffRatioAtSpeedRatio;
            }
        } else {
            fan.FanPower = max(0.0, MaxAirMassFlowRate * fan.FanRuntimeFraction * DeltaPress / (FanEff * RhoAir)); // total fan power
        }

        // OnOffFanPartLoadFraction is passed via DataHVACGlobals from the cooling or heating coil that is
        //   requesting the fan to operate in cycling fan/cycling coil mode
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0; // reset to 1 in case other on/off fan is called without a part load curve
        Real64 FanShaftPower = fan.MotEff * fan.FanPower;     // power delivered to shaft
        fan.PowerLossToAir = FanShaftPower + (fan.FanPower - FanShaftPower) * fan.MotInAirFrac;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy + fan.PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirMassFlowRate = MassFlow;
        //   fan%OutletAirTemp = Tin + PowerLossToAir/(MassFlow*PsyCpAirFnW(Win,Tin))
        fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);
    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        fan.FanPower = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.OutletAirMassFlowRate = 0.0;
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy;
        fan.OutletAirTemp = fan.InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        fan.MassFlowRateMaxAvail = 0.0;
        fan.MassFlowRateMinAvail = 0.0;
    }
}

void SimZoneExhaustFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Jan 2000
    //       MODIFIED       Brent Griffith, May 2009 for EMS
    //                      Brent Griffith, Feb 2013 controls upgrade

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the Zone Exhaust Fan

    // METHODOLOGY EMPLOYED:
    // Converts design pressure rise and efficiency into fan power and temperature rise
    // Constant fan pressure rise is assumed.

    // REFERENCES:
    // ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool FanIsRunning = false; // There seems to be a missing else case below unless false is assumed

    auto &fan = state.dataFans->Fan(FanNum);

    Real64 DeltaPress = fan.DeltaPress; // [N/m2]
    if (fan.EMSFanPressureOverrideOn) DeltaPress = fan.EMSFanPressureValue;

    Real64 FanEff = fan.FanEff;
    if (fan.EMSFanEffOverrideOn) FanEff = fan.EMSFanEffValue;

    // For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
    Real64 Tin = fan.InletAirTemp;
    Real64 RhoAir = fan.RhoAirStdInit;
    Real64 MassFlow = fan.InletAirMassFlowRate;

    //  When the AvailManagerMode == ExhaustFanCoupledToAvailManagers then the
    //  Exhaust Fan is  interlocked with air loop availability via global TurnFansOn and TurnFansOff variables.
    //  There is now the option to control if user wants to decouple air loop operation and exhaust fan operation
    //  (zone air mass balance issues).

    // apply controls to determine if operating
    if (fan.AvailManagerMode == AvailabilityManagerCoupling::Coupled) {
        if (((ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0) || state.dataHVACGlobal->TurnFansOn) &&
            !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0) { // available
            if (fan.MinTempLimitSchedNum > 0) {
                if (Tin >= ScheduleManager::GetCurrentScheduleValue(state, fan.MinTempLimitSchedNum)) {
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

    } else if (fan.AvailManagerMode == AvailabilityManagerCoupling::Decoupled) {
        if (ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0 && MassFlow > 0.0) {
            if (fan.MinTempLimitSchedNum > 0) {
                if (Tin >= ScheduleManager::GetCurrentScheduleValue(state, fan.MinTempLimitSchedNum)) {
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
        fan.FanPower = max(0.0, MassFlow * DeltaPress / (FanEff * RhoAir)); // total fan power
        fan.PowerLossToAir = fan.FanPower;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy + fan.PowerLossToAir / MassFlow;
        // This fan does not change the moisture or Mass Flow across the component
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirMassFlowRate = MassFlow;
        fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);

    } else {
        // Fan is off and not operating no power consumed and mass flow rate.
        fan.FanPower = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.OutletAirMassFlowRate = 0.0;
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy;
        fan.OutletAirTemp = fan.InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        fan.MassFlowRateMaxAvail = 0.0;
        fan.MassFlowRateMinAvail = 0.0;
        fan.InletAirMassFlowRate = 0.0;
    }
}

void SimComponentModelFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Craig Wray, LBNL
    //       DATE WRITTEN   Feb 2010

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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MaxAirMassFlowRate; // Fan Max mass airflow [kg/s]
    Real64 MotInAirFrac;       // Fraction of fan power input to airstream

    // Local variables
    Real64 FanDimFlow;          // Fan dimensionless airflow [-]
    Real64 BeltPLEff;           // Belt normalized (part-load) efficiency [-]
    Real64 MotorPLEff;          // Motor normalized (part-load) efficiency [-]
    Real64 VFDSpdRatio(0.0);    // Ratio of motor speed to motor max speed [-]
    Real64 VFDOutPwrRatio(0.0); // Ratio of VFD output power to max VFD output power [-]

    auto &fan = state.dataFans->Fan(FanNum);

    // Get inputs for night ventilation option
    int NVPerfNum = fan.NVPerfNum;

    if (state.dataHVACGlobal->NightVentOn && NVPerfNum > 0) {
        MotInAirFrac = state.dataFans->NightVentPerf(NVPerfNum).MotInAirFrac;
        MaxAirMassFlowRate = state.dataFans->NightVentPerf(NVPerfNum).MaxAirMassFlowRate;
    } else {
        MotInAirFrac = fan.MotInAirFrac;
        MaxAirMassFlowRate = fan.MaxAirMassFlowRate;
    }

    // Get air density at standard conditions and get mass airflow through fan
    // From WeatherManager:
    //   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
    //   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
    // From PsychRoutines:
    //   w=MAX(dw,1.0d-5)
    //   rhoair = pb/(287.d0*(tdb+Constant::Kelvin())*(1.0d0+1.6077687d0*w))
    Real64 RhoAir = fan.RhoAirStdInit;
    Real64 MassFlow = min(fan.InletAirMassFlowRate, fan.MaxAirMassFlowRate);

    //  IF (fan%EMSMaxMassFlowOverrideOn) MassFlow   = fan%EMSAirMassFlowValue

    // Determine the Fan Schedule for the Time step
    if ((ScheduleManager::GetCurrentScheduleValue(state, fan.AvailSchedPtrNum) > 0.0 || state.dataHVACGlobal->TurnFansOn) &&
        !state.dataHVACGlobal->TurnFansOff && MassFlow > 0.0) {
        // Fan is operating - calculate fan pressure rise, component efficiencies and power, and also air enthalpy rise

        // Calculate fan static pressure rise using fan volumetric flow, std air density, air-handling system characteristics,
        //   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
        Real64 FanVolFlow = MassFlow / RhoAir;                                                                 //[m3/s at standard conditions]
        Real64 DuctStaticPress = Curve::CurveValue(state, fan.PressResetCurveIndex, FanVolFlow);               // Duct static pressure setpoint [Pa]
        Real64 DeltaPressTot = Curve::CurveValue(state, fan.PressRiseCurveIndex, FanVolFlow, DuctStaticPress); // Fan total pressure rise [Pa]
        Real64 FanOutletVelPress = 0.5 * RhoAir * pow_2(FanVolFlow / fan.FanOutletArea);                       // Fan outlet velocity pressure [Pa]
        // Outlet velocity pressure cannot exceed total pressure rise
        FanOutletVelPress = min(FanOutletVelPress, DeltaPressTot);
        fan.DeltaPress = DeltaPressTot - FanOutletVelPress; // Fan static pressure rise [Pa]

        //    IF (fan%EMSFanPressureOverrideOn) DeltaPress = fan%EMSFanPressureValue

        // Calculate fan static air power using volumetric flow and fan static pressure rise
        fan.FanAirPower = FanVolFlow * fan.DeltaPress; //[W]

        // Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
        //   fan characteristics, and Wray dimensionless fan static efficiency model
        Real64 EulerNum = (fan.DeltaPress * pow_4(fan.FanWheelDia)) / (RhoAir * pow_2(FanVolFlow)); //[-]
        Real64 NormalizedEulerNum = std::log10(EulerNum / fan.EuMaxEff);
        if (NormalizedEulerNum <= 0.0) {
            fan.FanWheelEff = Curve::CurveValue(state, fan.PLFanEffNormCurveIndex, NormalizedEulerNum);
        } else {
            fan.FanWheelEff = Curve::CurveValue(state, fan.PLFanEffStallCurveIndex, NormalizedEulerNum);
        }
        fan.FanWheelEff *= fan.FanMaxEff;             // [-]
        fan.FanWheelEff = max(fan.FanWheelEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate fan shaft power using fan static air power and fan static efficiency
        fan.FanShaftPower = fan.FanAirPower / fan.FanWheelEff; //[W]

        // Calculate fan shaft speed, fan torque, and motor speed using Wray dimensionless fan airflow model
        if (NormalizedEulerNum <= 0.0) {
            FanDimFlow = Curve::CurveValue(state, fan.DimFlowNormCurveIndex, NormalizedEulerNum); //[-]
        } else {
            FanDimFlow = Curve::CurveValue(state, fan.DimFlowStallCurveIndex, NormalizedEulerNum); //[-]
        }
        Real64 FanSpdRadS = FanVolFlow / (FanDimFlow * fan.FanMaxDimFlow * pow_3(fan.FanWheelDia)); //[rad/s]
        fan.FanTrq = fan.FanShaftPower / FanSpdRadS;                                                //[N-m]
        fan.FanSpd = FanSpdRadS * 9.549296586;                                                      //[rpm, conversion factor is 30/PI]
        Real64 MotorSpeed = fan.FanSpd * fan.PulleyDiaRatio;                                        //[rpm]

        // Calculate belt part-load drive efficiency using correlations and coefficients based on ACEEE data
        // Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
        Real64 FanTrqRatio = fan.FanTrq / fan.BeltMaxTorque; //[-]
        if ((FanTrqRatio <= fan.BeltTorqueTrans) && (fan.PLBeltEffReg1CurveIndex != 0)) {
            BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg1CurveIndex, FanTrqRatio); //[-]
        } else {
            if ((FanTrqRatio > fan.BeltTorqueTrans) && (FanTrqRatio <= 1.0) && (fan.PLBeltEffReg2CurveIndex != 0)) {
                BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg2CurveIndex, FanTrqRatio); //[-]
            } else {
                if ((FanTrqRatio > 1.0) && (fan.PLBeltEffReg3CurveIndex != 0)) {
                    BeltPLEff = Curve::CurveValue(state, fan.PLBeltEffReg3CurveIndex, FanTrqRatio); //[-]
                } else {
                    BeltPLEff = 1.0; // Direct drive or no curve specified - use constant efficiency
                }
            }
        }
        fan.BeltEff = fan.BeltMaxEff * BeltPLEff; //[-]
        fan.BeltEff = max(fan.BeltEff, 0.01);     // Minimum efficiency is 1% to avoid numerical errors

        // Calculate belt input power using fan shaft power and belt efficiency
        fan.BeltInputPower = fan.FanShaftPower / fan.BeltEff; //[W]

        // Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
        Real64 MotorOutPwrRatio = fan.BeltInputPower / fan.MotorMaxOutPwr; //[-]
        if (fan.PLMotorEffCurveIndex != 0) {
            MotorPLEff = Curve::CurveValue(state, fan.PLMotorEffCurveIndex, MotorOutPwrRatio); //[-]
        } else {
            MotorPLEff = 1.0; // No curve specified - use constant efficiency
        }
        fan.MotEff = fan.MotorMaxEff * MotorPLEff; //[-]
        fan.MotEff = max(fan.MotEff, 0.01);        // Minimum efficiency is 1% to avoid numerical errors

        // Calculate motor input power using belt input power and motor efficiency
        fan.MotorInputPower = fan.BeltInputPower / fan.MotEff; //[W]

        // Calculate VFD efficiency using correlations and coefficients based on VFD type
        if ((fan.VFDEffType == "SPEED") && (fan.VFDEffCurveIndex != 0)) {
            VFDSpdRatio = MotorSpeed / fan.MotorMaxSpd;                               //[-]
            fan.VFDEff = Curve::CurveValue(state, fan.VFDEffCurveIndex, VFDSpdRatio); //[-]
        } else {
            if ((fan.VFDEffType == "POWER") && (fan.VFDEffCurveIndex != 0)) {
                VFDOutPwrRatio = fan.MotorInputPower / fan.VFDMaxOutPwr;                     //[-]
                fan.VFDEff = Curve::CurveValue(state, fan.VFDEffCurveIndex, VFDOutPwrRatio); //[-]
            } else {
                // No curve specified - use constant efficiency
                fan.VFDMaxOutPwr = 0.0;
                fan.VFDEff = 0.97;
            }
        }
        fan.VFDEff = max(fan.VFDEff, 0.01); // Minimum efficiency is 1% to avoid numerical errors

        // Calculate VFD input power using motor input power and VFD efficiency
        fan.VFDInputPower = fan.MotorInputPower / fan.VFDEff; //[W]
        fan.FanPower = fan.VFDInputPower;                     //[W]

        // Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
        // Equivalent to fan%FanAirPower / fan%FanPower
        fan.FanEff = fan.FanWheelEff * fan.BeltEff * fan.MotEff * fan.VFDEff;

        //    IF (fan%EMSFanEffOverrideOn) FanEff = fan%EMSFanEffValue

        // Calculate air enthalpy and temperature rise from power entering air stream from fan wheel, belt, and motor
        // Assumes MotInAirFrac applies to belt and motor but NOT to VFD
        fan.PowerLossToAir = fan.FanShaftPower + (fan.MotorInputPower - fan.FanShaftPower) * fan.MotInAirFrac; //[W]
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy + (fan.PowerLossToAir / MassFlow);                        //[kJ/kg]

        // This fan does not change the moisture or mass flow across the component
        fan.OutletAirHumRat = fan.InletAirHumRat; //[-]
        fan.OutletAirMassFlowRate = MassFlow;     //[kg/s]
        fan.OutletAirTemp = Psychrometrics::PsyTdbFnHW(fan.OutletAirEnthalpy, fan.OutletAirHumRat);
    } else {
        // Fan is OFF and not operating -- no power consumed and zero mass flow rate
        fan.FanPower = 0.0;
        fan.FanShaftPower = 0.0;
        fan.PowerLossToAir = 0.0;
        fan.OutletAirMassFlowRate = 0.0;
        fan.OutletAirHumRat = fan.InletAirHumRat;
        fan.OutletAirEnthalpy = fan.InletAirEnthalpy;
        fan.OutletAirTemp = fan.InletAirTemp;
        // Set the Control Flow variables to 0.0 flow when OFF.
        fan.MassFlowRateMaxAvail = 0.0;
        fan.MassFlowRateMinAvail = 0.0;

        fan.DeltaPress = 0.0;
        fan.FanAirPower = 0.0;
        fan.FanWheelEff = 0.0;
        fan.FanSpd = 0.0;
        fan.FanTrq = 0.0;
        fan.BeltEff = 0.0;
        fan.BeltInputPower = 0.0;
        fan.MotEff = 0.0;
        fan.MotorInputPower = 0.0;
        fan.VFDEff = 0.0;
        fan.VFDInputPower = 0.0;
        fan.FanEff = 0.0;
    }
}

void UpdateFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       L. Gu, Feb. 1, 2007, No unbalance airflow when Zone Exhaust Fans are used in the AirflowNetwork

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the fan outlet nodes.

    // METHODOLOGY EMPLOYED:
    // Data is moved from the fan data structure to the fan outlet nodes.

    auto &fan = state.dataFans->Fan(FanNum);
    auto &inletNode = state.dataLoopNodes->Node(fan.InletNodeNum);
    auto &outletNode = state.dataLoopNodes->Node(fan.OutletNodeNum);

    // Set the outlet air nodes of the fan
    outletNode.MassFlowRate = fan.OutletAirMassFlowRate;
    outletNode.Temp = fan.OutletAirTemp;
    outletNode.HumRat = fan.OutletAirHumRat;
    outletNode.Enthalpy = fan.OutletAirEnthalpy;
    // Set the outlet nodes for properties that just pass through & not used
    outletNode.Quality = inletNode.Quality;
    outletNode.Press = inletNode.Press;

    // Set the Node Flow Control Variables from the Fan Control Variables
    outletNode.MassFlowRateMaxAvail = fan.MassFlowRateMaxAvail;
    outletNode.MassFlowRateMinAvail = fan.MassFlowRateMinAvail;

    if (fan.fanType == HVAC::FanType::Exhaust) {
        inletNode.MassFlowRate = fan.InletAirMassFlowRate;
        if (state.afn->AirflowNetworkNumOfExhFan == 0) {
            state.dataHVACGlobal->UnbalExhMassFlow = fan.InletAirMassFlowRate;
            if (fan.BalancedFractSchedNum > 0) {
                state.dataHVACGlobal->BalancedExhMassFlow =
                    state.dataHVACGlobal->UnbalExhMassFlow * ScheduleManager::GetCurrentScheduleValue(state, fan.BalancedFractSchedNum);
                state.dataHVACGlobal->UnbalExhMassFlow = state.dataHVACGlobal->UnbalExhMassFlow - state.dataHVACGlobal->BalancedExhMassFlow;
            } else {
                state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
            }
        } else {
            state.dataHVACGlobal->UnbalExhMassFlow = 0.0;
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
        }
        fan.UnbalancedOutletMassFlowRate = state.dataHVACGlobal->UnbalExhMassFlow;
        fan.BalancedOutletMassFlowRate = state.dataHVACGlobal->BalancedExhMassFlow;
    }

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        outletNode.CO2 = inletNode.CO2;
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        outletNode.GenContam = inletNode.GenContam;
    }
}

void ReportFan(EnergyPlusData &state, int const FanNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the fans.

    auto &fan = state.dataFans->Fan(FanNum);

    fan.FanEnergy = fan.FanPower * state.dataHVACGlobal->TimeStepSysSec;
    fan.DeltaTemp = fan.OutletAirTemp - fan.InletAirTemp;

    if (fan.fanType == HVAC::FanType::OnOff) {
        if (fan.AirLoopNum > 0) {
            state.dataAirLoop->AirLoopAFNInfo(fan.AirLoopNum).AFNLoopOnOffFanRTF = fan.FanRuntimeFraction;
        }
    }
}

int GetFanIndex(EnergyPlusData &state, std::string const &FanName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets an index for a given fan -- issues error message if that fan
    // is not legal fan.

    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    return Util::FindItemInList(FanName, state.dataFans->Fan, &FanEquipConditions::Name);
}

Real64 GetFanVolFlow(EnergyPlusData &state, int const FanIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   August 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the fan volumetric flow for use by zone equipment (e.g. Packaged Terminal Heat Pump)
    // Zone equipment must ensure that a properly sized fan is used to meet the maximum supply air flow rate

    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).MaxAirFlowRate;
}

Real64 GetFanPower(EnergyPlusData &state, int const FanIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2012

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the fan power draw

    if (FanIndex == 0) {
        return 0.0;
    } else {
        return state.dataFans->Fan(FanIndex).FanPower;
    }
}

HVAC::FanType GetFanType(EnergyPlusData &state, int FanIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   August 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets an integer type for a given fan -- issues error message if that fan
    // is not a legal fan.
    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).fanType;
}

Real64 GetFanDesignVolumeFlowRate(EnergyPlusData &state,
                                  int FanIndex // index to fan
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006
    //       MODIFIED       R. Raustad, Aug 2007 - added optional fan index

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the design volume flow rate for the given fan and returns it.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as negative.

    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).MaxAirFlowRate;
}

int GetFanInletNode(EnergyPlusData &state, int FanIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the inlet node.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).InletNodeNum;
}

int GetFanOutletNode(EnergyPlusData &state, int FanIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2006

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the outlet node.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).OutletNodeNum;
}

int GetFanAvailSchPtr(EnergyPlusData &state, int FanIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2007

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the availability schedule pointer.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.
    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).AvailSchedPtrNum;
}

int GetFanSpeedRatioCurveIndex(EnergyPlusData &state, int FanIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2009

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given fan and returns the fan speed curve pointer.  If
    // incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
    // as zero. If optional index argument is passed along with fan type and name, the index is set.

    assert(FanIndex > 0 && FanIndex <= state.dataFans->Fan.size());
    return state.dataFans->Fan(FanIndex).FanPowerRatAtSpeedRatCurveIndex;
}

void SetFanData(EnergyPlusData &state,
                int const FanNum,                                // Index of fan
                bool &ErrorsFound,                               // Set to true if certain errors found
                std::string const &FanName,                      // Name of fan
                ObjexxFCL::Optional<Real64 const> MaxAirVolFlow, // Fan air volumetric flow rate    [m3/s]
                ObjexxFCL::Optional<Real64 const> MinAirVolFlow  // Fan air volumetric flow rate    [m3/s]
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   October 2007

    // PURPOSE OF THIS SUBROUTINE:
    // This routine was designed for to autosize the HeatExchanger:AirToAir:SensibleAndLatent using
    // information from the ZoneHVAC:EnergyRecoveryVentilator object.
    // This is an illustration of setting data from an outside source.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WhichFan; // index to generic HX

    // Obtains and Allocates fan related parameters from input file
    if (state.dataFans->GetFanInputFlag) { // First time subroutine has been entered
        GetFanInput(state);
        state.dataFans->GetFanInputFlag = false;
    }

    if (FanNum == 0) {
        WhichFan = Util::FindItemInList(FanName, state.dataFans->Fan, &FanEquipConditions::Name);
    } else {
        WhichFan = FanNum;
    }

    if (WhichFan <= 0 || WhichFan > state.dataFans->NumFans) {
        ShowSevereError(state, format("SetFanData: Could not find fan = \"{}\"", FanName));
        ErrorsFound = true;
        return;
    }

    auto &fan = state.dataFans->Fan(WhichFan);

    if (present(MaxAirVolFlow)) {
        fan.MaxAirFlowRate = MaxAirVolFlow;
    }

    if (present(MinAirVolFlow)) {
        fan.MinAirFlowRate = MinAirVolFlow;
    }
}

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

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the decrease of the fan air flow rate, given the fan curve
    // and the increase of fan pressure rise due to fouling air filters

    // Check whether the fan curve covers the design operational point of the fan
    Real64 FanCalDeltaPress = Curve::CurveValue(state, FanCurvePtr, FanDesignAirFlowRate); // [Pa]
    if ((FanCalDeltaPress < 0.9 * FanDesignDeltaPress) || (FanCalDeltaPress > 1.1 * FanDesignDeltaPress)) {
        ShowWarningError(state, format("The design operational point of the fan {} does not fall ", FanName));
        ShowContinueError(state, "on the fan curve provided in the FaultModel:Fouling:AirFilter object. ");
        return 0.0;
    }

    // Calculate the Fan Volume Flow Rate in the Faulty Case
    Real64 FanFaultyAirFlowRate = FanDesignAirFlowRate;                                        // Fan Volume Flow Rate in the Faulty Case [m3/sec]
    Real64 FanCalDeltaPresstemp = Curve::CurveValue(state, FanCurvePtr, FanFaultyAirFlowRate); // Calculated Fan Delta Pressure for temp use [Pa]
    FanCalDeltaPress = FanCalDeltaPresstemp;

    while (FanCalDeltaPress < (FanDesignDeltaPress + FanFaultyDeltaPressInc)) {
        FanFaultyAirFlowRate = FanFaultyAirFlowRate - 0.005;
        FanCalDeltaPresstemp = Curve::CurveValue(state, FanCurvePtr, FanFaultyAirFlowRate);

        if ((FanCalDeltaPresstemp <= FanCalDeltaPress) ||
            (FanFaultyAirFlowRate <= state.dataCurveManager->PerfCurve(FanCurvePtr)->inputLimits[0].min)) {
            // The new operational point of the fan go beyond the fan selection range
            ShowWarningError(state, format("The operational point of the fan {} may go beyond the fan selection ", FanName));
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

    // PURPOSE OF THIS FUNCTION:
    // This function calculates and returns the design fan heat gain from the fan input data

    // METHODOLOGY EMPLOYED:
    // Simple fan:  Qdot,tot = (Vdot*deltaP)/Eff,tot
    //              Qdot,air = Eff,mot*Qdot,tot + (Qdot,tot - Eff,mot*Qdot,tot)*Frac,mot-in-airstream

    if (FanNum <= 0) {
        return 0.0;
    }
    auto const &fan = state.dataFans->Fan(FanNum);

    if (fan.fanType != HVAC::FanType::ComponentModel) {
        Real64 DeltaP = fan.DeltaPress; // fan design pressure rise [N/m2]
        Real64 TotEff = fan.FanEff;     // fan design total efficiency
        Real64 MotEff = fan.MotEff;     // fan design motor efficiency
        Real64 MotInAirFrac = fan.MotInAirFrac;
        Real64 FanPowerTot = (FanVolFlow * DeltaP) / TotEff;
        return MotEff * FanPowerTot + (FanPowerTot - MotEff * FanPowerTot) * MotInAirFrac;
    } else {
        if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(FanNum)) {
            SizeFan(state, FanNum);
            state.dataFans->MySizeFlag(FanNum) = false;
        }
        return fan.FanShaftPower + (fan.MotorInputPower - fan.FanShaftPower) * fan.MotInAirFrac;
    }
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

    if (fanIndex <= 0) {
        return;
    }
    auto const &fan = state.dataFans->Fan(fanIndex);

    if (fan.fanType != HVAC::FanType::ComponentModel) {
        deltaP = fan.DeltaPress;
        motEff = fan.MotEff;
        totEff = fan.FanEff;
        motInAirFrac = fan.MotInAirFrac;
    } else {
        if (!state.dataGlobal->SysSizingCalc && state.dataFans->MySizeFlag(fanIndex)) {
            SizeFan(state, fanIndex);
            state.dataFans->MySizeFlag(fanIndex) = false;
        }
        fanCompModel = true;
        fanShaftPow = fan.FanShaftPower;
        motInPower = fan.MotorInputPower;
        motInAirFrac = fan.MotInAirFrac;
    }
}

void SetFanAirLoopNumber(EnergyPlusData &state, int const FanIndex, int const AirLoopNum)
{
    state.dataFans->Fan(FanIndex).AirLoopNum = AirLoopNum;
}

    int getFanObjectVectorIndex( // lookup vector index for fan object name in object array EnergyPlus::HVACFan::fanObjs
        EnergyPlusData &state,
        std::string const &objectName, // IDF name in input
        bool const ErrorCheck)
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < state.dataFans->fanObjs.size(); ++loop) {
            if (objectName == state.dataFans->fanObjs[loop]->name) {
                if (!found) {
                    index = loop;
                    found = true;
                } else { // found duplicate
                    // TODO throw warning?
                    index = -1;
                    ShowSevereError(state,
                                    format("getFanObjectVectorIndex: Found duplicate Fan:SystemModel inputs of name ={}. Check inputs", objectName));
                }
            }
        }

        // TODO: should not print error here
        if (!found && ErrorCheck) {
            ShowSevereError(state, format("getFanObjectVectorIndex: did not find Fan:SystemModel name ={}. Check inputs", objectName));
        }
        return index;
    }

    bool checkIfFanNameIsAFanSystem( // look up to see if input contains a Fan:SystemModel with the name (for use before object construction
        EnergyPlusData &state,
        std::string const &objectName)
    {

        int testNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Fan:SystemModel", objectName);
        if (testNum > 0) {
            return true;
        } else {
            return false;
        }
    }

    void FanSystem::simulate(EnergyPlusData &state,
                             ObjexxFCL::Optional<Real64 const> flowFraction,  // when used, this directs the fan to set the flow at this flow fraction
                                                                              // = current flow/ max design flow rate.  It is not exactly the same as
                                                                              // the legacy speed ratio that was used with SimulateFanComponents.
                             ObjexxFCL::Optional<Real64 const> pressureRise,  // Pressure difference to use for DeltaPress, for rating DX coils at a
                                                                              // different pressure without entire duct system
                             ObjexxFCL::Optional<Real64 const> massFlowRate1, // Mass flow rate in operating mode 1 [kg/s]
                             ObjexxFCL::Optional<Real64 const> runTimeFraction1, // Run time fraction in operating mode 1
                             ObjexxFCL::Optional<Real64 const> massFlowRate2,    // Mass flow rate in operating mode 2 [kg/s]
                             ObjexxFCL::Optional<Real64 const> runTimeFraction2, // Run time fraction in opearating mode 2
                             ObjexxFCL::Optional<Real64 const> pressureRise2     // Pressure difference for operating mode 2
    )
    {

        init(state);

        if (m_objSizingFlag) {
            return; // can't run calculations until sizing is completed
        }

        if (present(pressureRise) && present(massFlowRate1) && present(runTimeFraction1) && present(massFlowRate2) && present(runTimeFraction2) &&
            present(pressureRise2)) {
            Real64 flowRatio1 = massFlowRate1 / m_maxAirMassFlowRate;
            Real64 flowRatio2 = massFlowRate2 / m_maxAirMassFlowRate;
            calcSimpleSystemFan(state, _, pressureRise, flowRatio1, runTimeFraction1, flowRatio2, runTimeFraction2, pressureRise2);
        } else if (!present(pressureRise) && present(massFlowRate1) && present(runTimeFraction1) && present(massFlowRate2) &&
                   present(runTimeFraction2) && !present(pressureRise2)) {
            Real64 flowRatio1 = massFlowRate1 / m_maxAirMassFlowRate;
            Real64 flowRatio2 = massFlowRate2 / m_maxAirMassFlowRate;
            calcSimpleSystemFan(state, flowFraction, _, flowRatio1, runTimeFraction1, flowRatio2, runTimeFraction2, _);
        } else if (present(pressureRise) && present(flowFraction)) {
            calcSimpleSystemFan(state, flowFraction, pressureRise, _, _, _, _, _);
        } else if (present(pressureRise) && !present(flowFraction)) {
            calcSimpleSystemFan(state, _, pressureRise, _, _, _, _, _);
        } else if (!present(pressureRise) && present(flowFraction)) {
            calcSimpleSystemFan(state, flowFraction, _, _, _, _, _, _);
        } else {
            calcSimpleSystemFan(state, _, _, _, _, _, _, _);
        }

        update(state);

        report(state);
    }

    void FanSystem::init(EnergyPlusData &state)
    {
        if (!state.dataGlobal->SysSizingCalc && m_objSizingFlag) {
            set_size(state);
            m_objSizingFlag = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && m_objEnvrnFlag) {

            // Currently, fan does not force minimum mass flow, only used for power calculation
            // m_minAirFlowRate = designAirVolFlowRate * m_minPowerFlowFrac;
            // m_minAirMassFlowRate = m_minAirFlowRate * m_rhoAirStdInit;

            // Init the Node Control variables
            state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMax = m_maxAirMassFlowRate;
            // Currently, fan does not force minimum mass flow, only used for power calculation
            // DataLoopNode::Node( outletNodeNum ).MassFlowRateMin = m_minAirMassFlowRate;

            // Initialize all report variables to a known state at beginning of simulation
            m_fanPower = 0.0;
            m_deltaTemp = 0.0;
            m_powerLossToAir = 0.0;
            m_fanEnergy = 0.0;
            for (int loop = 0; loop < m_numSpeeds; ++loop) {
                m_fanRunTimeFractionAtSpeed[loop] = 0.0;
            }
            m_objEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            m_objEnvrnFlag = true;
        }

        m_massFlowRateMaxAvail =
            min(state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMax, state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMaxAvail);
        m_massFlowRateMinAvail =
            min(max(state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMin, state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMinAvail),
                state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMaxAvail);

        // Load the node data in this section for the component simulation
        // First need to make sure that the MassFlowRate is between the max and min avail.
        m_inletAirMassFlowRate = min(state.dataLoopNodes->Node(inletNodeNum).MassFlowRate, m_massFlowRateMaxAvail);
        m_inletAirMassFlowRate = max(m_inletAirMassFlowRate, m_massFlowRateMinAvail);

        // Then set the other conditions
        m_inletAirTemp = state.dataLoopNodes->Node(inletNodeNum).Temp;
        m_inletAirHumRat = state.dataLoopNodes->Node(inletNodeNum).HumRat;
        m_inletAirEnthalpy = state.dataLoopNodes->Node(inletNodeNum).Enthalpy;
    }

    void FanSystem::set_size(EnergyPlusData &state)
    {
        static constexpr std::string_view routineName = "FanSystem::set_size ";

        Real64 tempFlow = designAirVolFlowRate;
        bool bPRINT = true;
        state.dataSize->DataAutosizable = true;
        state.dataSize->DataEMSOverrideON = m_maxAirFlowRateEMSOverrideOn;
        state.dataSize->DataEMSOverride = m_maxAirFlowRateEMSOverrideValue;

        bool errorsFound = false;
        SystemAirFlowSizer sizerSystemAirFlow;
        sizerSystemAirFlow.initializeWithinEP(state, HVAC::fanTypeNames[(int)m_fanType], name, bPRINT, routineName);
        designAirVolFlowRate = sizerSystemAirFlow.size(state, tempFlow, errorsFound);

        state.dataSize->DataAutosizable = true; // should be false?
        state.dataSize->DataEMSOverrideON = false;
        state.dataSize->DataEMSOverride = 0.0;

        if (m_designElecPowerWasAutosized) {

            switch (m_powerSizingMethod) {

            case PowerSizingMethod::PowerPerFlow: {
                designElecPower = designAirVolFlowRate * m_elecPowerPerFlowRate;
                break;
            }
            case PowerSizingMethod::PowerPerFlowPerPressure: {
                designElecPower = designAirVolFlowRate * deltaPress * m_elecPowerPerFlowRatePerPressure;
                break;
            }
            case PowerSizingMethod::TotalEfficiencyAndPressure: {
                designElecPower = designAirVolFlowRate * deltaPress / m_fanTotalEff;
                break;
            }
            case PowerSizingMethod::Invalid: {
                // do nothing
                break;
            }
            default:
                assert(false);

            } // end switch

            // report design power
            BaseSizer::reportSizerOutput(
                state, HVAC::fanTypeNames[(int)m_fanType], name, "Design Electric Power Consumption [W]", designElecPower);

        } // end if power was autosized

        m_rhoAirStdInit = state.dataEnvrn->StdRhoAir;
        m_maxAirMassFlowRate = designAirVolFlowRate * m_rhoAirStdInit;

        // calculate total fan system efficiency at design, else set to 1 to avoid div by zero
        if (designElecPower > 0.0) {
            m_fanTotalEff = designAirVolFlowRate * deltaPress / designElecPower;
        } else {
            m_fanTotalEff = 1.0;
        }

        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) { // set up values at speeds
            m_massFlowAtSpeed.resize(m_numSpeeds, 0.0);
            m_totEfficAtSpeed.resize(m_numSpeeds, 0.0);
            for (int loop = 0; loop < m_numSpeeds; ++loop) {
                m_massFlowAtSpeed[loop] = m_maxAirMassFlowRate * m_flowFractionAtSpeed[loop];
                if (m_powerFractionInputAtSpeed[loop]) { // use speed power fraction
                    if (designElecPower > 0.0) {
                        m_totEfficAtSpeed[loop] =
                            m_flowFractionAtSpeed[loop] * designAirVolFlowRate * deltaPress / (designElecPower * m_powerFractionAtSpeed[loop]);
                    } else {
                        m_totEfficAtSpeed[loop] = 1.0;
                    }
                } else { // use power curve
                    m_totEfficAtSpeed[loop] =
                        m_flowFractionAtSpeed[loop] * designAirVolFlowRate * deltaPress /
                        (designElecPower * Curve::CurveValue(state, powerModFuncFlowFractionCurveIndex, m_flowFractionAtSpeed[loop]));
                    m_powerFractionAtSpeed[loop] = Curve::CurveValue(state, powerModFuncFlowFractionCurveIndex, m_flowFractionAtSpeed[loop]);
                }
            }
        }
        Real64 rhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataLoopNodes->Node(inletNodeNum).Press, m_inletAirTemp, m_inletAirHumRat);
        m_designPointFEI = report_fei(state, designAirVolFlowRate, designElecPower, deltaPress, rhoAir);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanType, name, HVAC::fanTypeNames[(int)m_fanType]);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanTotEff, name, m_fanTotalEff);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanDeltaP, name, deltaPress);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanVolFlow, name, designAirVolFlowRate);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwr, name, designElecPower);
        if (designAirVolFlowRate != 0.0) {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchFanPwrPerFlow, name, designElecPower / designAirVolFlowRate);
        }
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorIn, name, m_motorInAirFrac);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEnergyIndex, name, m_designPointFEI);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEndUse, name, m_endUseSubcategoryName);

        m_objSizingFlag = false;

        // Std 229 Fans (HVACFan.cc)
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchFanPurpose, name, "N/A"); // m_fanType); // purpose? not the same
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchFanAutosized, name, m_designAirVolFlowRateWasAutosized ? "Yes" : "No");
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorEff, name, m_motorEff);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorHeatToZoneFrac, name, m_motorInAirFrac);
        OutputReportPredefined::PreDefTableEntry(state,
                                                 state.dataOutRptPredefined->pdchFanAirLoopName,
                                                 name,
                                                 AirLoopNum > 0 ? state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name : "N/A");
    }

    Real64 FanSystem::report_fei(
        EnergyPlusData &state, Real64 const designFlowRate, Real64 const designElecPower, Real64 const designDeltaPress, Real64 inletRhoAir)
    {
        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the Fan Energy Index

        // REFERENCES:
        // ANSI/AMCA Standard 207-17: Fan System Efficiency and Fan System Input Power Calculation, 2017.
        // AANSI / AMCA Standard 208 - 18: Calculation of the Fan Energy Index, 2018.

        assert(state.dataEnvrn->StdRhoAir > 0.0);
        // Calculate reference fan shaft power
        Real64 refFanShaftPower = (designFlowRate + 0.118) * (designDeltaPress + 100 * inletRhoAir / state.dataEnvrn->StdRhoAir) / (1000 * 0.66);

        // Calculate reference reference fan transmission efficiency
        Real64 refFanTransEff = 0.96 * pow((refFanShaftPower / (refFanShaftPower + 1.64)), 0.05);

        // Calculate reference reference fan motor efficiency
        Real64 refFanMotorOutput = refFanShaftPower / refFanTransEff;

        Real64 refFanMotorEff;
        if (refFanMotorOutput < 185.0) {
            refFanMotorEff = -0.003812 * pow(std::log10(refFanMotorOutput), 4) + 0.025834 * pow(std::log10(refFanMotorOutput), 3) -
                             0.072577 * pow(std::log10(refFanMotorOutput), 2) + 0.125559 * std::log10(refFanMotorOutput) + 0.850274;
        } else {
            refFanMotorEff = 0.962;
        }

        // Calculate reference reference fan motor controller  efficiency
        Real64 refFanMotorCtrlEff = 1;

        Real64 refFanElecPower = refFanShaftPower / (refFanTransEff * refFanMotorEff * refFanMotorCtrlEff);

        if (designElecPower > 0.0) {
            return refFanElecPower * 1000 / designElecPower;
        } else {
            return 0.0;
        }
    }

    FanSystem::FanSystem(EnergyPlusData &state, std::string const &objectName)
        : availSchedIndex(0), inletNodeNum(0), outletNodeNum(0), designAirVolFlowRate(0.0), speedControl(SpeedControlMethod::NotSet), deltaPress(0.0),
          designElecPower(0.0), powerModFuncFlowFractionCurveIndex(0), AirLoopNum(0), AirPathFlag(false), m_numSpeeds(0), fanIsSecondaryDriver(false),
          m_fanType(HVAC::FanType::Invalid), m_designAirVolFlowRateWasAutosized(false), m_minPowerFlowFrac(0.0), m_motorEff(0.0),
          m_motorInAirFrac(0.0), m_designElecPowerWasAutosized(false), m_powerSizingMethod(PowerSizingMethod::Invalid), m_elecPowerPerFlowRate(0.0),
          m_elecPowerPerFlowRatePerPressure(0.0), m_fanTotalEff(0.0), m_nightVentPressureDelta(0.0), m_nightVentFlowFraction(0.0), m_zoneNum(0),
          m_zoneRadFract(0.0), m_heatLossesDestination(ThermalLossDestination::Invalid), m_qdotConvZone(0.0), m_qdotRadZone(0.0),
          m_inletAirMassFlowRate(0.0), m_outletAirMassFlowRate(0.0), m_maxAirMassFlowRate(0.0), m_inletAirTemp(0.0), m_outletAirTemp(0.0),
          m_inletAirHumRat(0.0), m_outletAirHumRat(0.0), m_inletAirEnthalpy(0.0), m_outletAirEnthalpy(0.0), m_objEnvrnFlag(true),
          m_objSizingFlag(true), m_fanPower(0.0), m_fanEnergy(0.0), m_maxAirFlowRateEMSOverrideOn(false), m_maxAirFlowRateEMSOverrideValue(0.0),
          m_eMSFanPressureOverrideOn(false), m_eMSFanPressureValue(0.0), m_eMSFanEffOverrideOn(false), m_eMSFanEffValue(0.0),
          m_eMSMaxMassFlowOverrideOn(false), m_eMSAirMassFlowValue(0.0), m_faultyFilterFlag(false), m_faultyFilterIndex(0),

          m_massFlowRateMaxAvail(0.0), m_massFlowRateMinAvail(0.0), m_rhoAirStdInit(0.0), m_designPointFEI(0.0)
    // oneTimePowerCurveCheck_( true )
    {

        static constexpr std::string_view routineName = "HVACFan constructor ";
        int numAlphas;    // Number of elements in the alpha array
        int numNums;      // Number of elements in the numeric array
        int numTotFields; // Total number of alpha and numeric fields
        int IOStat;       // IO Status when calling get input subroutine
        bool errorsFound = false;
        std::string locCurrentModuleObject = "Fan:SystemModel";
        Array1D_string alphaArgs;
        Array1D_string alphaFieldNames;
        Array1D_bool isAlphaFieldBlank;
        Array1D<Real64> numericArgs;
        Array1D_string numericFieldNames;
        Array1D_bool isNumericFieldBlank;
        int objectNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, locCurrentModuleObject, objectName);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, locCurrentModuleObject, numTotFields, numAlphas, numNums);
        if (numAlphas > 0) {
            alphaArgs.allocate(numAlphas);
            alphaFieldNames.allocate(numAlphas);
            isAlphaFieldBlank.allocate(numAlphas);
        }
        if (numNums > 0) {
            numericArgs.allocate(numNums);
            numericFieldNames.allocate(numNums);
            isNumericFieldBlank.allocate(numNums);
        }

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 locCurrentModuleObject,
                                                                 objectNum,
                                                                 alphaArgs,
                                                                 numAlphas,
                                                                 numericArgs,
                                                                 numNums,
                                                                 IOStat,
                                                                 isNumericFieldBlank,
                                                                 isAlphaFieldBlank,
                                                                 alphaFieldNames,
                                                                 numericFieldNames);

        name = alphaArgs(1);
        // TODO how to check for unique names across objects during get input?
        m_fanType = HVAC::FanType::SystemModel;
        if (isAlphaFieldBlank(2)) {
            availSchedIndex = ScheduleManager::ScheduleAlwaysOn;
        } else {
            availSchedIndex = ScheduleManager::GetScheduleIndex(state, alphaArgs(2));
            if (availSchedIndex == 0) {
                ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, format("Invalid {} = {}", alphaFieldNames(2), alphaArgs(2)));
                errorsFound = true;
            }
        }
        inletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                           alphaArgs(3),
                                                           errorsFound,
                                                           DataLoopNode::ConnectionObjectType::FanSystemModel,
                                                           alphaArgs(1),
                                                           DataLoopNode::NodeFluidType::Air,
                                                           DataLoopNode::ConnectionType::Inlet,
                                                           NodeInputManager::CompFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
        outletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                            alphaArgs(4),
                                                            errorsFound,
                                                            DataLoopNode::ConnectionObjectType::FanSystemModel,
                                                            alphaArgs(1),
                                                            DataLoopNode::NodeFluidType::Air,
                                                            DataLoopNode::ConnectionType::Outlet,
                                                            NodeInputManager::CompFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state, locCurrentModuleObject, alphaArgs(1), alphaArgs(3), alphaArgs(4), "Air Nodes");

        designAirVolFlowRate = numericArgs(1);
        if (designAirVolFlowRate == DataSizing::AutoSize) {
            m_designAirVolFlowRateWasAutosized = true;
        }

        if (isAlphaFieldBlank(5)) {
            speedControl = SpeedControlMethod::Discrete;
        } else if (Util::SameString(alphaArgs(5), "Continuous")) {
            speedControl = SpeedControlMethod::Continuous;
        } else if (Util::SameString(alphaArgs(5), "Discrete")) {
            speedControl = SpeedControlMethod::Discrete;
        } else {
            ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
            ShowContinueError(state, format("Invalid {} = {}", alphaFieldNames(5), alphaArgs(5)));
            errorsFound = true;
        }

        m_minPowerFlowFrac = numericArgs(2);
        deltaPress = numericArgs(3);
        if (deltaPress <= 0.0) {
            ShowSevereError(state, format("{}{} zero or negative, invalid entry in {}", routineName, locCurrentModuleObject, numericFieldNames(3)));
            errorsFound = true;
        }
        m_motorEff = numericArgs(4);
        m_motorInAirFrac = numericArgs(5);
        designElecPower = numericArgs(6);
        if (designElecPower == DataSizing::AutoSize) {
            m_designElecPowerWasAutosized = true;
        }
        if (m_designElecPowerWasAutosized) {
            if (isAlphaFieldBlank(6)) {
                m_powerSizingMethod = PowerSizingMethod::PowerPerFlowPerPressure;
            } else if (Util::SameString(alphaArgs(6), "PowerPerFlow")) {
                m_powerSizingMethod = PowerSizingMethod::PowerPerFlow;
            } else if (Util::SameString(alphaArgs(6), "PowerPerFlowPerPressure")) {
                m_powerSizingMethod = PowerSizingMethod::PowerPerFlowPerPressure;
            } else if (Util::SameString(alphaArgs(6), "TotalEfficiencyAndPressure")) {
                m_powerSizingMethod = PowerSizingMethod::TotalEfficiencyAndPressure;
            } else {
                ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, format("Invalid {} = {}", alphaFieldNames(6), alphaArgs(6)));
                errorsFound = true;
            }
            m_elecPowerPerFlowRate = numericArgs(7);
            m_elecPowerPerFlowRatePerPressure = numericArgs(8);
            m_fanTotalEff = numericArgs(9);
        }
        if (!isAlphaFieldBlank(7)) {
            powerModFuncFlowFractionCurveIndex = Curve::GetCurveIndex(state, alphaArgs(7));
            if (powerModFuncFlowFractionCurveIndex == 0) {
                ShowWarningError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, format("Invalid {} = {}", alphaFieldNames(7), alphaArgs(7)));
                ShowContinueError(state, "Curve not found.");
                if (speedControl == SpeedControlMethod::Continuous) {
                    errorsFound = true;
                }
            }
        } else { // blank
            if (speedControl == SpeedControlMethod::Continuous) {
                ShowWarningError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, format("Continuous speed control requires a fan power curve in {} = {}", alphaFieldNames(7), alphaArgs(7)));
                errorsFound = true;
            }
        }
        m_nightVentPressureDelta = numericArgs(10);
        m_nightVentFlowFraction = numericArgs(11); // not used
        m_zoneNum = Util::FindItemInList(alphaArgs(8), state.dataHeatBal->Zone);
        if (m_zoneNum > 0) m_heatLossesDestination = ThermalLossDestination::ZoneGains;
        if (m_zoneNum == 0) {
            if (isAlphaFieldBlank(8)) {
                m_heatLossesDestination = ThermalLossDestination::LostToOutside;
            } else {
                m_heatLossesDestination = ThermalLossDestination::LostToOutside;
                ShowWarningError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, format("Invalid {} = {}", alphaFieldNames(8), alphaArgs(8)));
                ShowContinueError(state, "Zone name not found. Fan motor heat losses will not be added to a zone");
                // continue with simulation but motor losses not sent to a zone.
            }
        }
        m_zoneRadFract = numericArgs(12);
        if (!isAlphaFieldBlank(9)) {
            m_endUseSubcategoryName = alphaArgs(9);
        } else {
            m_endUseSubcategoryName = "General";
        }

        if (!isNumericFieldBlank(13)) {
            m_numSpeeds = numericArgs(13);
        } else {
            m_numSpeeds = 1;
        }
        m_fanRunTimeFractionAtSpeed.resize(m_numSpeeds, 0.0);
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) {
            // should have field sets
            m_flowFractionAtSpeed.resize(m_numSpeeds, 0.0);
            m_powerFractionAtSpeed.resize(m_numSpeeds, 0.0);
            m_powerFractionInputAtSpeed.resize(m_numSpeeds, false);
            if (m_numSpeeds == ((numNums - 13) / 2) || m_numSpeeds == ((numNums + 1 - 13) / 2)) {
                for (int loopSet = 0; loopSet < m_numSpeeds; ++loopSet) {
                    m_flowFractionAtSpeed[loopSet] = numericArgs(13 + loopSet * 2 + 1);
                    if (!isNumericFieldBlank(13 + loopSet * 2 + 2)) {
                        m_powerFractionAtSpeed[loopSet] = numericArgs(13 + loopSet * 2 + 2);
                        m_powerFractionInputAtSpeed[loopSet] = true;
                    } else {
                        m_powerFractionInputAtSpeed[loopSet] = false;
                    }
                }
            } else {
                // field set input does not match number of speeds, throw warning
                ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state, "Fan with Discrete speed control does not have input for speed data that matches the number of speeds.");
                errorsFound = true;
            }
            // check that flow fractions are increasing
            bool increasingOrderError = false;
            for (int loop = 0; loop < (m_numSpeeds - 1); ++loop) {
                if (m_flowFractionAtSpeed[loop] > m_flowFractionAtSpeed[loop + 1]) {
                    increasingOrderError = true;
                }
            }
            if (increasingOrderError) {
                ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(state,
                                  "Fan with Discrete speed control and multiple speed levels does not have input with flow fractions arranged in "
                                  "increasing order.");
                errorsFound = true;
            }
        }

        // check if power curve present when any speeds have no power fraction
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1 && powerModFuncFlowFractionCurveIndex == 0) {
            bool foundMissingPowerFraction = false;
            for (int loop = 0; loop < m_numSpeeds; ++loop) {
                if (!m_powerFractionInputAtSpeed[loop]) {
                    foundMissingPowerFraction = true;
                }
            }
            if (foundMissingPowerFraction) {
                // field set input does not match number of speeds, throw warning
                ShowSevereError(state, format("{}{}=\"{}\", invalid entry.", routineName, locCurrentModuleObject, alphaArgs(1)));
                ShowContinueError(
                    state,
                    "Fan with Discrete speed control does not have input for power fraction at all speed levels and does not have a power curve.");
                errorsFound = true;
            }
        }

        if (errorsFound) {
            ShowFatalError(state, format("{}Errors found in input for fan name = {}.  Program terminates.", routineName, name));
        }

        SetupOutputVariable(state,
                            "Fan Electricity Rate",
                            Constant::Units::W,
                            m_fanPower,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Rise in Air Temperature",
                            Constant::Units::deltaC,
                            m_deltaTemp,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Heat Gain to Air",
                            Constant::Units::W,
                            m_powerLossToAir,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Electricity Energy",
                            Constant::Units::J,
                            m_fanEnergy,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Sum,
                            name,
                            Constant::eResource::Electricity,
                            OutputProcessor::Group::HVAC,
                            OutputProcessor::EndUseCat::Fans,
                            m_endUseSubcategoryName);
        SetupOutputVariable(state,
                            "Fan Air Mass Flow Rate",
                            Constant::Units::kg_s,
                            m_outletAirMassFlowRate,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            name);
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds == 1) {
            SetupOutputVariable(state,
                                "Fan Runtime Fraction",
                                Constant::Units::None,
                                m_fanRunTimeFractionAtSpeed[0],
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                name);
        } else if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) {
            for (int speedLoop = 0; speedLoop < m_numSpeeds; ++speedLoop) {
                SetupOutputVariable(state,
                                    "Fan Runtime Fraction Speed " + fmt::to_string(speedLoop + 1),
                                    Constant::Units::None,
                                    m_fanRunTimeFractionAtSpeed[speedLoop],
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    name);
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Fan Maximum Mass Flow Rate", name, "[kg/s]", m_maxAirMassFlowRate);
            SetupEMSActuator(state, "Fan", name, "Fan Air Mass Flow Rate", "[kg/s]", m_eMSMaxMassFlowOverrideOn, m_eMSAirMassFlowValue);
            SetupEMSInternalVariable(state, "Fan Nominal Pressure Rise", name, "[Pa]", deltaPress);
            SetupEMSActuator(state, "Fan", name, "Fan Pressure Rise", "[Pa]", m_eMSFanPressureOverrideOn, m_eMSFanPressureValue);
            SetupEMSInternalVariable(state, "Fan Nominal Total Efficiency", name, "[fraction]", m_fanTotalEff);
            SetupEMSActuator(state, "Fan", name, "Fan Total Efficiency", "[fraction]", m_eMSFanEffOverrideOn, m_eMSFanEffValue);
            SetupEMSActuator(
                state, "Fan", name, "Fan Autosized Air Flow Rate", "[m3/s]", m_maxAirFlowRateEMSOverrideOn, m_maxAirFlowRateEMSOverrideValue);
        }

        if (m_heatLossesDestination == ThermalLossDestination::ZoneGains) {
            SetupZoneInternalGain(state, m_zoneNum, name, DataHeatBalance::IntGainType::FanSystemModel, &m_qdotConvZone, nullptr, &m_qdotRadZone);
        }

        alphaArgs.deallocate();
        alphaFieldNames.deallocate();
        isAlphaFieldBlank.deallocate();
        numericArgs.deallocate();
        numericFieldNames.deallocate();
        isNumericFieldBlank.deallocate();

        bool anyEMSRan = false;
        EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::ComponentGetInput, anyEMSRan, ObjexxFCL::Optional_int_const());
    }

    void FanSystem::calcSimpleSystemFan(
        EnergyPlusData &state,
        ObjexxFCL::Optional<Real64 const> flowFraction, // Flow fraction for entire timestep (not used if flow ratios are present)
        ObjexxFCL::Optional<Real64 const> pressureRise, // Pressure difference to use for DeltaPress
        ObjexxFCL::Optional<Real64 const> flowRatio1,   // Flow ratio in operating mode 1
        ObjexxFCL::Optional<Real64 const> runTimeFrac1, // Run time fraction in operating mode 1
        ObjexxFCL::Optional<Real64 const> flowRatio2,   // Flow ratio in operating mode 2
        ObjexxFCL::Optional<Real64 const> runTimeFrac2, // Run time fraction in operating mode 2
        ObjexxFCL::Optional<Real64 const> pressureRise2 // Pressure difference to use for operating mode 2
    )
    {
        std::vector<Real64> localPressureRise; // [0] is operating mode 1, [1] is operating mode 2
        Real64 localFlowFraction;
        Real64 localFanTotEff;
        std::vector<Real64> localAirMassFlow;
        std::vector<Real64> localFlowRatio;
        std::vector<Real64> localRunTimeFrac;
        bool localUseFlowRatiosAndRunTimeFracs = false;

        int localNumModes = 1; // Number of operating modes, 1 or 2 ( e.g. heating, ventilating, cooling)
        if (present(flowRatio2) && present(runTimeFrac2)) localNumModes = 2;
        localPressureRise.resize(2, 0.0);
        localAirMassFlow.resize(2, 0.0);
        localFlowRatio.resize(2, 0.0);
        localRunTimeFrac.resize(2, 1.0);

        if (state.dataHVACGlobal->NightVentOn) {
            // assume if non-zero inputs for night data then this fan is to be used with that data
            if (m_nightVentPressureDelta > 0.0) {
                localPressureRise[0] = m_nightVentPressureDelta;
                localPressureRise[1] = m_nightVentPressureDelta;
            }

            if (m_maxAirMassFlowRate > 0.0) { // protect div by 0
                localFlowFraction = m_inletAirMassFlowRate / m_maxAirMassFlowRate;
            } else {
                localFlowFraction = 1.0;
            }
            localAirMassFlow[0] = m_inletAirMassFlowRate;

        } else { // not in night mode
            if (present(pressureRise)) {
                localPressureRise[0] = pressureRise;
            } else {
                localPressureRise[0] = deltaPress;
            }
            if (present(pressureRise2)) {
                localPressureRise[1] = pressureRise2;
            } else {
                localPressureRise[1] = deltaPress;
            }
            if (present(flowFraction)) {
                localFlowFraction = flowFraction;
                localAirMassFlow[0] = localFlowFraction * m_maxAirMassFlowRate;
            } else {
                if (m_maxAirMassFlowRate > 0.0) { // protect div by 0
                    localFlowFraction = m_inletAirMassFlowRate / m_maxAirMassFlowRate;
                } else {
                    localFlowFraction = 1.0;
                }
                localAirMassFlow[0] = m_inletAirMassFlowRate;
            }
            if (present(flowRatio1) && present(flowRatio2) && present(runTimeFrac1) && present(runTimeFrac2)) {
                localUseFlowRatiosAndRunTimeFracs = true;
                localRunTimeFrac[0] = runTimeFrac1;
                localRunTimeFrac[1] = runTimeFrac2;
                localFlowRatio[0] = flowRatio1;
                localAirMassFlow[0] = localFlowRatio[0] * m_maxAirMassFlowRate * localRunTimeFrac[0];
                localFlowRatio[1] = flowRatio2;
                localAirMassFlow[1] = localFlowRatio[1] * m_maxAirMassFlowRate * localRunTimeFrac[1];
            } else {
                localRunTimeFrac[0] = 1.0; // if runTimeFracs are not present, assume single-mode operation
                localRunTimeFrac[1] = 0.0; // if runTimeFracs are not present, assume single-mode operation
            }
        }

        Real64 localFaultMaxAirMassFlow = 0.0;
        bool faultActive = false;
        Real64 localFaultPressureRise = 0.0;
        if (m_faultyFilterFlag && (state.dataFaultsMgr->NumFaultyAirFilter > 0) && (!state.dataGlobal->WarmupFlag) &&
            (!state.dataGlobal->DoingSizing) && state.dataGlobal->DoWeathSim && (!m_eMSMaxMassFlowOverrideOn) && (!m_eMSFanPressureOverrideOn)) {
            if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).AvaiSchedPtr) > 0) {
                faultActive = true;
                Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]
                FanDesignFlowRateDec = Fans::CalFaultyFanAirFlowReduction(
                    state,
                    name,
                    designAirVolFlowRate,
                    deltaPress,
                    (ScheduleManager::GetCurrentScheduleValue(
                         state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterPressFracSchePtr) -
                     1) *
                        deltaPress,
                    state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterFanCurvePtr);

                localFaultMaxAirMassFlow = m_maxAirMassFlowRate - FanDesignFlowRateDec * m_rhoAirStdInit;

                localFaultPressureRise =
                    ScheduleManager::GetCurrentScheduleValue(
                        state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterPressFracSchePtr) *
                    deltaPress;
            }
        }

        for (int mode = 0; mode < localNumModes; ++mode) {
            // EMS override MassFlow, DeltaPress, and FanEff
            if (m_eMSFanPressureOverrideOn) localPressureRise[mode] = m_eMSFanPressureValue;
            if (m_eMSFanEffOverrideOn) localFanTotEff = m_eMSFanEffValue;
            if (m_eMSMaxMassFlowOverrideOn) {
                localAirMassFlow[mode] = m_eMSAirMassFlowValue;
            }

            localAirMassFlow[mode] = min(localAirMassFlow[mode], m_maxAirMassFlowRate);
            if (faultActive) {
                localAirMassFlow[mode] = min(localAirMassFlow[mode], localFaultMaxAirMassFlow);
                localPressureRise[mode] = localFaultPressureRise;
            }
            localFlowFraction = localAirMassFlow[0] / m_maxAirMassFlowRate;
            localFlowFraction = min(1.0, localFlowFraction);

            if (localRunTimeFrac[mode] > 0.0) {
                localFlowRatio[mode] = localAirMassFlow[mode] / (m_maxAirMassFlowRate * localRunTimeFrac[mode]);
            }
            localFlowRatio[mode] = min(1.0, localFlowRatio[mode]);
        }

        // zero these now, because the may accumulate across multiple operating modes
        m_powerLossToAir = 0.0;
        m_fanPower = 0.0;
        m_outletAirMassFlowRate = 0.0;
        if (speedControl == SpeedControlMethod::Discrete) {
            for (int loop = 0; loop < m_numSpeeds; ++loop) {
                m_fanRunTimeFractionAtSpeed[loop] = 0.0;
            }
        }

        if ((ScheduleManager::GetCurrentScheduleValue(state, availSchedIndex) > 0.0 || state.dataHVACGlobal->TurnFansOn) &&
            !state.dataHVACGlobal->TurnFansOff && ((localAirMassFlow[0] + localAirMassFlow[1]) > 0.0)) {
            // fan is running

            for (int mode = 0; mode < localNumModes; ++mode) {

                // if no flow for this mode then continue to the next mode
                if (localAirMassFlow[mode] == 0.0) continue;

                switch (speedControl) {

                case SpeedControlMethod::Discrete: {
                    //
                    if (state.dataHVACGlobal->OnOffFanPartLoadFraction <= 0.0) {
                        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
                    }
                    if (state.dataHVACGlobal->OnOffFanPartLoadFraction < 0.7) {
                        state.dataHVACGlobal->OnOffFanPartLoadFraction =
                            0.7; // a warning message is already issued from the DX coils or gas heating coil
                    }
                    if (localUseFlowRatiosAndRunTimeFracs) {
                        // Use flow ratios and runtimefractions pass from parent (allows fan to cycle at a specified speed)
                        Real64 locRunTimeFraction(0.0);
                        if (state.dataHVACGlobal->OnOffFanPartLoadFraction >= 1.0) {
                            locRunTimeFraction = localRunTimeFrac[mode];
                        } else {
                            locRunTimeFraction = max(0.0, min(1.0, localRunTimeFrac[mode] / state.dataHVACGlobal->OnOffFanPartLoadFraction));
                        }
                        Real64 locFlowRatio = localFlowRatio[mode]; // Current mode flow rate / max flow rate
                        Real64 locLowSpeedFanRunTimeFrac = 0.0;
                        Real64 locHiSpeedFanRunTimeFrac = 0.0;
                        if (m_numSpeeds == 1) { // CV or OnOff
                            localFanTotEff = m_fanTotalEff;
                            locHiSpeedFanRunTimeFrac = locRunTimeFraction * locFlowRatio;
                            m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            m_fanPower += max(
                                0.0, locHiSpeedFanRunTimeFrac * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit));
                        } else if (m_numSpeeds > 1) { // multi speed

                            // find which two speed levels bracket flow ratios and calculate runtimefraction at each speed
                            // ideally the flow ratios passed in will match one of the fan m_flowFractionAtSpeed but it is not required
                            int lowSideSpeed = -1;
                            int hiSideSpeed = -1;

                            if (locFlowRatio <= m_flowFractionAtSpeed[0]) { // on/off at lowest speed
                                hiSideSpeed = 0;
                                locHiSpeedFanRunTimeFrac = locFlowRatio * locRunTimeFraction / m_flowFractionAtSpeed[0];
                                m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            } else {
                                lowSideSpeed = 0; // hush up cppcheck
                                hiSideSpeed = 0;  // hush up cppcheck
                                for (int loop = 0; loop < m_numSpeeds - 1; ++loop) {
                                    if ((m_flowFractionAtSpeed[loop] <= locFlowRatio) && (locFlowRatio <= m_flowFractionAtSpeed[loop + 1])) {
                                        lowSideSpeed = loop;
                                        hiSideSpeed = loop + 1;
                                        break;
                                    }
                                }
                                Real64 locLowSpeedTimeFrac = (m_flowFractionAtSpeed[hiSideSpeed] - locFlowRatio) /
                                                             (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                locLowSpeedFanRunTimeFrac = locLowSpeedTimeFrac * localRunTimeFrac[mode];
                                locHiSpeedFanRunTimeFrac = (1 - locLowSpeedTimeFrac) * localRunTimeFrac[mode];
                                m_fanRunTimeFractionAtSpeed[lowSideSpeed] += locLowSpeedFanRunTimeFrac;
                                m_fanRunTimeFractionAtSpeed[hiSideSpeed] += locHiSpeedFanRunTimeFrac;
                            }
                            if (lowSideSpeed != -1 && hiSideSpeed != -1) {
                                m_fanPower += max(0.0,
                                                  locLowSpeedFanRunTimeFrac * m_massFlowAtSpeed[lowSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[lowSideSpeed] * m_rhoAirStdInit) +
                                                      locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            } else if (lowSideSpeed == -1 && hiSideSpeed == 0) {
                                m_fanPower += max(0.0,
                                                  locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                      (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            }
                        }
                    } else {
                        // Use localFlowFraction which is not locked at a particular flow ratio (legacy method for fan:onoff)
                        Real64 locFanRunTimeFraction(0.0);
                        Real64 locLowSpeedFanRunTimeFrac = 0.0;
                        Real64 locHiSpeedFanRunTimeFrac = 0.0;
                        if (state.dataHVACGlobal->OnOffFanPartLoadFraction >= 1.0) {
                            locFanRunTimeFraction = localFlowFraction;
                        } else {
                            locFanRunTimeFraction = max(0.0, min(1.0, localFlowFraction / state.dataHVACGlobal->OnOffFanPartLoadFraction));
                        }
                        if (m_numSpeeds == 1) { // CV or OnOff
                            localFanTotEff = m_fanTotalEff;
                            locHiSpeedFanRunTimeFrac = locFanRunTimeFraction;
                            m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            m_fanPower += max(
                                0.0, locHiSpeedFanRunTimeFrac * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit));
                        } else if (m_numSpeeds > 1) { // multi speed

                            // find which two speed levels bracket flow fraction and calculate runtimefraction
                            int lowSideSpeed = -1;
                            int hiSideSpeed = -1;

                            if (locFanRunTimeFraction < m_flowFractionAtSpeed[0]) { // on/off between zero and lowest speed
                                hiSideSpeed = 0;
                                locHiSpeedFanRunTimeFrac = locFanRunTimeFraction / m_flowFractionAtSpeed[0];
                                m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            } else {
                                lowSideSpeed = 0; // hush up cppcheck
                                hiSideSpeed = 0;  // hush up cppcheck
                                for (int loop = 0; loop < m_numSpeeds - 1; ++loop) {
                                    if ((m_flowFractionAtSpeed[loop] <= locFanRunTimeFraction) &&
                                        (locFanRunTimeFraction <= m_flowFractionAtSpeed[loop + 1])) {
                                        lowSideSpeed = loop;
                                        hiSideSpeed = loop + 1;
                                        break;
                                    }
                                }
                                locLowSpeedFanRunTimeFrac = (m_flowFractionAtSpeed[hiSideSpeed] - locFanRunTimeFraction) /
                                                            (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                locHiSpeedFanRunTimeFrac = (locFanRunTimeFraction - m_flowFractionAtSpeed[lowSideSpeed]) /
                                                           (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                m_fanRunTimeFractionAtSpeed[lowSideSpeed] += locLowSpeedFanRunTimeFrac;
                                m_fanRunTimeFractionAtSpeed[hiSideSpeed] += locHiSpeedFanRunTimeFrac;
                            }
                            if (lowSideSpeed != -1 && hiSideSpeed != -1) {
                                m_fanPower += max(0.0,
                                                  locLowSpeedFanRunTimeFrac * m_massFlowAtSpeed[lowSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[lowSideSpeed] * m_rhoAirStdInit) +
                                                      locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            } else if (lowSideSpeed == -1 && hiSideSpeed == 0) {
                                m_fanPower += max(0.0,
                                                  locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                      (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            }
                        }
                    }
                    localFanTotEff = m_fanTotalEff;
                    break;
                }
                case SpeedControlMethod::Continuous: {
                    localFanTotEff = m_fanTotalEff;
                    Real64 locFlowRatio(0.0);
                    Real64 locFanRunTimeFraction(0.0);
                    if (localUseFlowRatiosAndRunTimeFracs) {
                        locFlowRatio = localFlowRatio[mode];
                        locFanRunTimeFraction = localRunTimeFrac[mode];
                    } else {
                        locFlowRatio = localFlowFraction;
                        locFanRunTimeFraction = 1.0;
                    }

                    Real64 localFlowFractionForPower = max(m_minPowerFlowFrac, locFlowRatio);
                    Real64 localPowerFraction(0.0);
                    if (state.dataHVACGlobal->NightVentOn) {
                        localPowerFraction = 1.0; // not sure why, but legacy fan had this for night ventilation
                    } else {
                        localPowerFraction = Curve::CurveValue(state, powerModFuncFlowFractionCurveIndex, localFlowFractionForPower);
                    }
                    Real64 localfanPower = max(0.0,
                                               locFanRunTimeFraction * localPowerFraction * m_maxAirMassFlowRate * localPressureRise[mode] /
                                                   (localFanTotEff * m_rhoAirStdInit));
                    Real64 fanShaftPower = m_motorEff * localfanPower;
                    Real64 localpowerLossToAir = fanShaftPower + (localfanPower - fanShaftPower) * m_motorInAirFrac;
                    m_outletAirEnthalpy = m_inletAirEnthalpy + localpowerLossToAir / localAirMassFlow[mode]; // this will get revised later
                    m_outletAirHumRat = m_inletAirHumRat;                                                    // this will get revised later
                    m_outletAirTemp = Psychrometrics::PsyTdbFnHW(m_outletAirEnthalpy, m_outletAirHumRat);    // this will get revised later
                    // When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
                    //  avoid the unrealistic high temperature rise across the fan.
                    Real64 deltaTAcrossFan = m_outletAirTemp - m_inletAirTemp;
                    if (deltaTAcrossFan > 20.0) {
                        Real64 minFlowFracLimitFanHeat = 0.10;
                        Real64 powerFractionAtLowMin = 0.0;
                        Real64 fanPoweratLowMinimum = 0.0;
                        if (localFlowFractionForPower < minFlowFracLimitFanHeat) {
                            powerFractionAtLowMin = Curve::CurveValue(state, powerModFuncFlowFractionCurveIndex, minFlowFracLimitFanHeat);
                            fanPoweratLowMinimum =
                                powerFractionAtLowMin * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit);
                            localfanPower = max(0.0, localFlowFractionForPower * fanPoweratLowMinimum / minFlowFracLimitFanHeat);
                        } else if (locFlowRatio < minFlowFracLimitFanHeat) {
                            powerFractionAtLowMin = Curve::CurveValue(state, powerModFuncFlowFractionCurveIndex, minFlowFracLimitFanHeat);
                            fanPoweratLowMinimum =
                                powerFractionAtLowMin * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit);
                            localfanPower = max(0.0, locFlowRatio * fanPoweratLowMinimum / minFlowFracLimitFanHeat);
                        }
                    }
                    m_fanPower += localfanPower;
                    break;
                } // continuous speed control case
                case SpeedControlMethod::NotSet: {
                    // do nothing
                    break;
                }
                default:
                    assert(false);
                } // end switch
                m_outletAirMassFlowRate += localAirMassFlow[mode];

            } // end of operating mode loop

            if (m_outletAirMassFlowRate > 0.0) {
                Real64 fanShaftPower = m_motorEff * m_fanPower; // power delivered to shaft
                m_powerLossToAir = fanShaftPower + (m_fanPower - fanShaftPower) * m_motorInAirFrac;
                m_outletAirEnthalpy = m_inletAirEnthalpy + m_powerLossToAir / m_outletAirMassFlowRate;
                // This fan does not change the moisture or Mass Flow across the component
                m_outletAirHumRat = m_inletAirHumRat;
                m_outletAirTemp = Psychrometrics::PsyTdbFnHW(m_outletAirEnthalpy, m_outletAirHumRat);
            } else {
                m_fanPower = 0.0;
                m_powerLossToAir = 0.0;
                m_outletAirHumRat = m_inletAirHumRat;
                m_outletAirEnthalpy = m_inletAirEnthalpy;
                m_outletAirTemp = m_inletAirTemp;
                m_massFlowRateMaxAvail = 0.0;
                m_massFlowRateMinAvail = 0.0;
            }

        } else { // fan is off
            // Fan is off and not operating no power consumed and mass flow rate.
            m_fanPower = 0.0;
            m_powerLossToAir = 0.0;
            m_outletAirHumRat = m_inletAirHumRat;
            m_outletAirEnthalpy = m_inletAirEnthalpy;
            m_outletAirTemp = m_inletAirTemp;
            // Set the Control Flow variables to 0.0 flow when OFF.
            if (fanIsSecondaryDriver) {
                m_outletAirMassFlowRate =
                    localAirMassFlow[0] +
                    localAirMassFlow[1]; // sometimes the air is moving with the fan off, eg. AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan
                if (m_outletAirMassFlowRate == 0.0) {
                    m_massFlowRateMaxAvail = 0.0;
                    m_massFlowRateMinAvail = 0.0;
                }
            } else {
                m_outletAirMassFlowRate = 0.0;
                m_massFlowRateMaxAvail = 0.0;
                m_massFlowRateMinAvail = 0.0;
            }
        }

        if (m_heatLossesDestination == ThermalLossDestination::ZoneGains) {
            Real64 powerLossToZone = m_fanPower - m_powerLossToAir;
            m_qdotConvZone = powerLossToZone * (1.0 - m_zoneRadFract);
            m_qdotRadZone = powerLossToZone * m_zoneRadFract;
        }
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0; // reset to 1
    }

    void FanSystem::update(EnergyPlusData &state) const // does not change state of object, only update elsewhere
    {
        // Set the outlet air node of the fan
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRate = m_outletAirMassFlowRate;
        state.dataLoopNodes->Node(outletNodeNum).Temp = m_outletAirTemp;
        state.dataLoopNodes->Node(outletNodeNum).HumRat = m_outletAirHumRat;
        state.dataLoopNodes->Node(outletNodeNum).Enthalpy = m_outletAirEnthalpy;
        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(outletNodeNum).Quality = state.dataLoopNodes->Node(inletNodeNum).Quality;
        state.dataLoopNodes->Node(outletNodeNum).Press = state.dataLoopNodes->Node(inletNodeNum).Press;

        // Set the Node Flow Control Variables from the Fan Control Variables
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMaxAvail = m_massFlowRateMaxAvail;
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMinAvail = m_massFlowRateMinAvail;

        // make sure inlet has the same mass flow
        state.dataLoopNodes->Node(inletNodeNum).MassFlowRate = m_outletAirMassFlowRate;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(outletNodeNum).CO2 = state.dataLoopNodes->Node(inletNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(outletNodeNum).GenContam = state.dataLoopNodes->Node(inletNodeNum).GenContam;
        }

        if (speedControl == SpeedControlMethod::Continuous) {
            if (AirLoopNum > 0) {
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_fanRunTimeFractionAtSpeed[0];
            }
        } else {
            if (AirLoopNum > 0) {
                if (m_numSpeeds == 1) {
                    state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_outletAirMassFlowRate / m_maxAirMassFlowRate;
                } else {
                    if (m_outletAirMassFlowRate <= m_massFlowAtSpeed[0]) {
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_outletAirMassFlowRate / m_massFlowAtSpeed[0];
                    } else {
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = 1.0;
                    }
                }
            }
        }
    }

    void FanSystem::report(EnergyPlusData &state)
    {
        m_fanEnergy = m_fanPower * state.dataHVACGlobal->TimeStepSysSec;
        m_deltaTemp = m_outletAirTemp - m_inletAirTemp;
    }

    Real64 FanSystem::fanPower() const
    {
        return m_fanPower;
    }

    Real64 FanSystem::powerLossToAir() const
    {
        return m_powerLossToAir;
    }

    Real64 FanSystem::maxAirMassFlowRate() const
    {
        return m_maxAirMassFlowRate;
    }

    Real64 FanSystem::getFanDesignTemperatureRise(EnergyPlusData &state) const
    {
        if (!m_objSizingFlag) {
            Real64 cpAir = Psychrometrics::PsyCpAirFnW(DataPrecisionGlobals::constant_zero);
            Real64 designDeltaT = (deltaPress / (m_rhoAirStdInit * cpAir * m_fanTotalEff)) * (m_motorEff + m_motorInAirFrac * (1.0 - m_motorEff));
            return designDeltaT;
        } else {
            // TODO throw warning, exception, call sizing?
            ShowWarningError(state, "FanSystem::getFanDesignTemperatureRise called before fan sizing completed ");
            return 0.0;
        }
    }

    Real64 FanSystem::getFanDesignHeatGain(EnergyPlusData &state, Real64 const FanVolFlow // fan volume flow rate [m3/s]
    )
    {
        if (!m_objSizingFlag) {
            Real64 fanPowerTot = (FanVolFlow * deltaPress) / m_fanTotalEff;
            Real64 designHeatGain = m_motorEff * fanPowerTot + (fanPowerTot - m_motorEff * fanPowerTot) * m_motorInAirFrac;
            return designHeatGain;
        } else {
            set_size(state);
            Real64 fanPowerTot = (FanVolFlow * deltaPress) / m_fanTotalEff;
            Real64 designHeatGain = m_motorEff * fanPowerTot + (fanPowerTot - m_motorEff * fanPowerTot) * m_motorInAirFrac;
            return designHeatGain;
        }
    }

    void FanSystem::FanInputsForDesignHeatGain(EnergyPlusData &state, Real64 &deltaP, Real64 &motEff, Real64 &totEff, Real64 &motInAirFrac)
    {
        if (!m_objSizingFlag) {
            deltaP = deltaPress;
            motEff = m_motorEff;
            totEff = m_fanTotalEff;
            motInAirFrac = m_motorInAirFrac;
            return;
        } else {
            set_size(state);
            deltaP = deltaPress;
            motEff = m_motorEff;
            totEff = m_fanTotalEff;
            motInAirFrac = m_motorInAirFrac;
            return;
        }
    }
} // namespace EnergyPlus::Fans
