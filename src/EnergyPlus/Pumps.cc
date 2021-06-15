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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantPressureSystem.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Pumps.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Pumps {

// MODULE INFORMATION:
//       AUTHOR         Dan Fisher
//       DATE WRITTEN   Sept 1998
//       MODIFIED       July 2001, Richard Liesen
//                      July 2001, Rick Strand (new "local" pump control method)
//                      Feb 2005, Rahul Chillar(added condensate pump for steam systems)
//                      Jan 2006, Sankaranarayanan (Added pump banks to the library of pumps)
//                      May 2009, Brent Griffith (added support for EMS override of massflow)
//                      Aug 2010, Edwin Lee (refactored code, significant clean-up)
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// Encapsulates the data and algorithms to simulate pumps.

// REFERENCES:
// HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
// Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

// Using/Aliasing
using DataHVACGlobals::CycleOn;
using DataHVACGlobals::ForceOff;
using DataHVACGlobals::SmallWaterVolFlow;
using DataLoopNode::ObjectIsNotParent;

std::string const cPump_VarSpeed("Pump:VariableSpeed");
std::string const cPump_ConSpeed("Pump:ConstantSpeed");
std::string const cPump_Cond("Pump:VariableSpeed:Condensate");
std::string const cPumpBank_VarSpeed("HeaderedPumps:VariableSpeed");
std::string const cPumpBank_ConSpeed("HeaderedPumps:ConstantSpeed");

std::map<PumpType, std::string> cPumpTypes = {
    {PumpType::VarSpeed, cPump_VarSpeed},
    {PumpType::ConSpeed, cPump_ConSpeed},
    {PumpType::Cond, cPump_Cond},
    {PumpType::Bank_VarSpeed, cPumpBank_VarSpeed},
    {PumpType::Bank_ConSpeed, cPumpBank_ConSpeed},
};

static constexpr std::string_view fluidNameSteam("STEAM");
static constexpr std::string_view fluidNameWater("WATER");

void SimPumps(EnergyPlusData &state,
              std::string const &PumpName, // Name of pump to be managed
              int const LoopNum,           // Plant loop number
              Real64 const FlowRequest,    // requested flow from adjacent demand side
              bool &PumpRunning,           // .TRUE. if the loop pump is actually operating
              int &PumpIndex,
              Real64 &PumpHeat)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   July 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the pump operation based on the type of
    // pump and the pump controls (continuous, intermittent, etc.).  The
    // result of this subroutine is that the pump has been simulated for
    // the necessary loop and the PumpRunning has been correctly set.

    int PumpNum; // Pump index within PumpEquip derived type

    // Get input from IDF one time
    if (state.dataPumps->GetInputFlag) {
        GetPumpInput(state);
        state.dataPumps->GetInputFlag = false;
    }

    // Exit early if no pumps found
    if (state.dataPumps->NumPumps == 0) {
        PumpHeat = 0.0;
        return;
    }

    // Setup pump component index if needed
    if (PumpIndex == 0) {
        PumpNum = UtilityRoutines::FindItemInList(PumpName, state.dataPumps->PumpEquip); // Determine which pump to simulate
        if (PumpNum == 0) {
            ShowFatalError(state, "ManagePumps: Pump requested not found =" + PumpName); // Catch any bad names before crashing
        }
        PumpIndex = PumpNum;
    } else {
        PumpNum = PumpIndex;
        if (state.dataPumps->PumpEquip(PumpNum).CheckEquipName) {
            if (PumpNum > state.dataPumps->NumPumps || PumpNum < 1) {
                ShowFatalError(
                    state,
                    format(
                        "ManagePumps: Invalid PumpIndex passed={}, Number of Pumps={}, Pump name={}", PumpNum, state.dataPumps->NumPumps, PumpName));
            }
            if (PumpName != state.dataPumps->PumpEquip(PumpNum).Name) {
                ShowFatalError(state,
                               format("ManagePumps: Invalid PumpIndex passed={}, Pump name={}, stored Pump Name for that index={}",
                                      PumpNum,
                                      PumpName,
                                      state.dataPumps->PumpEquip(PumpNum).Name));
            }
            state.dataPumps->PumpEquip(PumpNum).CheckEquipName = false;
        }
    }

    // Perform one-time and begin-environment initialization
    InitializePumps(state, PumpNum);

    // If all we need is to set outlet min/max avail, then just do it and get out.  Also, we only do min/max avail on flow query
    if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum).FlowLock == DataPlant::iFlowLock::PumpQuery) {
        SetupPumpMinMaxFlows(state, LoopNum, PumpNum);
        return;
    }

    // Set pump flow rate and calculate power
    CalcPumps(state, PumpNum, FlowRequest, PumpRunning);

    // Update pump reporting data
    ReportPumps(state, PumpNum);

    // Send this up to the calling routine
    PumpHeat = state.dataPumps->PumpHeattoFluid;
}

void GetPumpInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    April 1998
    //       MODIFIED:        July 2001, Rick Strand (addition of pump controls)
    //                        May 2009, Brent Griffith (added EMS calls)

    // PURPOSE OF THIS SUBROUTINE:
    // This routine will get the input
    // required by the pump simulation.

    // PUMP:VARIABLE SPEED,
    // This pump model is described in the ASHRAE secondary HVAC toolkit.

    // REFERENCES:
    // HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
    //  Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using CurveManager::GetCurveIndex;
    using CurveManager::GetCurveMinMaxValues;
    using DataHeatBalance::IntGainTypeOf_Pump_Cond;
    using DataHeatBalance::IntGainTypeOf_Pump_ConSpeed;
    using DataHeatBalance::IntGainTypeOf_Pump_VarSpeed;
    using DataHeatBalance::IntGainTypeOf_PumpBank_ConSpeed;
    using DataHeatBalance::IntGainTypeOf_PumpBank_VarSpeed;
    using DataPlant::TypeOf_PumpBankConstantSpeed;
    using DataPlant::TypeOf_PumpBankVariableSpeed;
    using DataPlant::TypeOf_PumpCondensate;
    using DataPlant::TypeOf_PumpConstantSpeed;
    using DataPlant::TypeOf_PumpVariableSpeed;
    using DataSizing::AutoSize;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;
    using NodeInputManager::GetOnlySingleNode;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const StartTemp(100.0); // Standard Temperature across code to calculated Steam density
    static constexpr std::string_view RoutineName("GetPumpInput: ");
    static constexpr std::string_view RoutineNameNoColon("GetPumpInput");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PumpNum;
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool ErrorsFound;
    int TempCurveIndex;
    std::string TempCurveType;
    int NumVarSpeedPumps;
    int NumConstSpeedPumps;
    int NumCondensatePumps;
    int NumVarPump;
    int NumConstPump;
    int NumCondPump;
    int NumPumpBankSimpleVar;
    int NumPumpBankSimpleConst;
    int NumVarPumpBankSimple;
    int NumConstPumpBankSimple;
    Real64 SteamDensity;
    Real64 TempWaterDensity;
    int DummyWaterIndex(1);

    ErrorsFound = false;

    // GET NUMBER OF ALL EQUIPMENT TYPES
    NumVarSpeedPumps = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cPump_VarSpeed);
    NumConstSpeedPumps = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cPump_ConSpeed);
    NumCondensatePumps = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cPump_Cond);
    NumPumpBankSimpleVar = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cPumpBank_VarSpeed);
    NumPumpBankSimpleConst = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cPumpBank_ConSpeed);
    state.dataPumps->NumPumps = NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar + NumPumpBankSimpleConst;

    if (state.dataPumps->NumPumps <= 0) {
        ShowWarningError(state, "No Pumping Equipment Found");
        return;
    }

    state.dataPumps->PumpEquip.allocate(state.dataPumps->NumPumps);
    state.dataPumps->PumpUniqueNames.reserve(static_cast<unsigned>(state.dataPumps->NumPumps));
    state.dataPumps->PumpEquipReport.allocate(state.dataPumps->NumPumps);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = cPump_VarSpeed;

    for (NumVarPump = 1; NumVarPump <= NumVarSpeedPumps; ++NumVarPump) {
        PumpNum = NumVarPump;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NumVarPump,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPumps->PumpUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPumps->PumpEquip(PumpNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPumps->PumpEquip(PumpNum).pumpType = PumpType::VarSpeed; //'Pump:VariableSpeed'
        state.dataPumps->PumpEquip(PumpNum).TypeOf_Num = TypeOf_PumpVariableSpeed;

        state.dataPumps->PumpEquip(PumpNum).InletNodeNum = GetOnlySingleNode(state,
                                                                             state.dataIPShortCut->cAlphaArgs(2),
                                                                             ErrorsFound,
                                                                             cCurrentModuleObject,
                                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                                             DataLoopNode::NodeFluidType::Water,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             1,
                                                                             ObjectIsNotParent);

        state.dataPumps->PumpEquip(PumpNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                              state.dataIPShortCut->cAlphaArgs(3),
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                                              DataLoopNode::NodeFluidType::Water,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Water Nodes");

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Continuous")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Intermittent")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Intermittent;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(4));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(4) + "]. " + state.dataIPShortCut->cAlphaFieldNames(4) +
                                  " has been set to Continuous for this pump.");
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        }

        // Input the optional schedule for the pump
        state.dataPumps->PumpEquip(PumpNum).PumpSchedule = state.dataIPShortCut->cAlphaArgs(5);
        state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (!state.dataIPShortCut->lAlphaFieldBlanks(5) && !(state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex > 0)) {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(5));
            ShowContinueError(state, "Schedule named =[" + state.dataIPShortCut->cAlphaArgs(5) + "]. was not found and will not be used.");
        }

        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).NomPumpHead = state.dataIPShortCut->rNumericArgs(2);
        state.dataPumps->PumpEquip(PumpNum).NomPowerUse = state.dataIPShortCut->rNumericArgs(3);
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).MotorEffic = state.dataIPShortCut->rNumericArgs(4);
        state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid = state.dataIPShortCut->rNumericArgs(5);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) = state.dataIPShortCut->rNumericArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) = state.dataIPShortCut->rNumericArgs(7);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) = state.dataIPShortCut->rNumericArgs(8);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) = state.dataIPShortCut->rNumericArgs(9);
        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate = state.dataIPShortCut->rNumericArgs(10);
        if (state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).minVolFlowRateWasAutosized = true;
        } else if (!state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized &&
                   (state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate > state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate)) {
            // Check that the minimum isn't greater than the maximum
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid '" +
                                 state.dataIPShortCut->cNumericFieldNames(10) + "'");
            ShowContinueError(state,
                              format("Entered Value=[{:.5T}] is above the {}=[{:.5T}].",
                                     state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate,
                                     state.dataIPShortCut->cNumericFieldNames(1),
                                     state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate));
            ShowContinueError(state,
                              "Reseting value of '" + state.dataIPShortCut->cNumericFieldNames(10) + "' to the value of '" +
                                  state.dataIPShortCut->cNumericFieldNames(1) + "'.");
            // Set min to roughly max, but not quite, otherwise it can't turn on, ever
            state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate = 0.99 * state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate;
        }
        // Probably the following two lines will be used if the team agrees on changing the F10 value from min flow rate to
        // minimum flow as a fraction of nominal flow.

        // Input pressure related data such as pressure curve and impeller size/rotational speed
        state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name = state.dataIPShortCut->cAlphaArgs(6);
        if (state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name == "") {
            state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = -1;
        } else {
            TempCurveIndex = GetCurveIndex(state, state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name);
            if (TempCurveIndex == 0) {
                state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = -1;
            } else {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            TempCurveIndex,                             // Curve index
                                                            {1},                                        // Valid dimensions
                                                            RoutineName,                                // Routine name
                                                            cCurrentModuleObject,                       // Object Type
                                                            state.dataPumps->PumpEquip(PumpNum).Name,   // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(6)); // Field Name

                if (!ErrorsFound) {
                    state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = TempCurveIndex;
                    GetCurveMinMaxValues(
                        state, TempCurveIndex, state.dataPumps->PumpEquip(PumpNum).MinPhiValue, state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);
                }
            }
        }

        // read in the rest of the pump pressure characteristics
        state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter = state.dataIPShortCut->rNumericArgs(11);

        // Input VFD related data
        if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
            state.dataPumps->PumpEquip(PumpNum).HasVFD = false;
        } else {
            state.dataPumps->PumpEquip(PumpNum).HasVFD = true;
            if (state.dataIPShortCut->cAlphaArgs(7) == "MANUALCONTROL") {
                state.dataPumps->PumpEquip(PumpNum).VFD.VFDControlType = ControlTypeVFD::VFDManual;
                state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedName = state.dataIPShortCut->cAlphaArgs(8);
                state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
                if (state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedIndex <= 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                        "\", At least one scheduled VFD schedule input was invalid.");
                    ShowContinueError(state, "Verify that all of the pressure and rpm schedules referenced in the input fields actually exist.");
                    ErrorsFound = true;
                } else if (!CheckScheduleValueMinMax(state, state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedIndex, ">", 0.0) ||
                           !CheckScheduleValueMinMax(state, state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedIndex, ">", 0.0)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                        "\", A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero.");
                    ErrorsFound = true;
                }
            } else if (state.dataIPShortCut->cAlphaArgs(7) == "PRESSURESETPOINTCONTROL") {
                state.dataPumps->PumpEquip(PumpNum).VFD.VFDControlType = ControlTypeVFD::VFDAutomatic;
                state.dataPumps->PumpEquip(PumpNum).VFD.LowerPsetSchedName = state.dataIPShortCut->cAlphaArgs(9);
                state.dataPumps->PumpEquip(PumpNum).VFD.LowerPsetSchedIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
                state.dataPumps->PumpEquip(PumpNum).VFD.UpperPsetSchedName = state.dataIPShortCut->cAlphaArgs(10);
                state.dataPumps->PumpEquip(PumpNum).VFD.UpperPsetSchedIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
                state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedName = state.dataIPShortCut->cAlphaArgs(11);
                state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
                state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedName = state.dataIPShortCut->cAlphaArgs(12);
                state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(12));
                if (min(state.dataPumps->PumpEquip(PumpNum).VFD.LowerPsetSchedIndex,
                        state.dataPumps->PumpEquip(PumpNum).VFD.UpperPsetSchedIndex,
                        state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedIndex,
                        state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedIndex) <= 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                        "\", At least one scheduled VFD schedule input was invalid.");
                    ShowContinueError(state, "Verify that all of the pressure and rpm schedules referenced in the input fields actually exist.");
                    ErrorsFound = true;
                } else if (!CheckScheduleValueMinMax(state, state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedIndex, ">", 0.0) ||
                           !CheckScheduleValueMinMax(state, state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedIndex, ">", 0.0)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                        "\", A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero.");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", VFD Control type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) { // zone named for pump skin losses
            state.dataPumps->PumpEquip(PumpNum).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(13), state.dataHeatBal->Zone);
            if (state.dataPumps->PumpEquip(PumpNum).ZoneNum > 0) {
                state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone = true;
                if (!state.dataIPShortCut->lNumericFieldBlanks(12)) {
                    state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction = state.dataIPShortCut->rNumericArgs(12);
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(13) + "=\"" + state.dataIPShortCut->cAlphaArgs(13) + "\" not found.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
            if (state.dataIPShortCut->cAlphaArgs(14) == "POWERPERFLOW") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlow;
            } else if (state.dataIPShortCut->cAlphaArgs(14) == "POWERPERFLOWPERPRESSURE") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlowPerPressure;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", sizing method type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(13)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor = state.dataIPShortCut->rNumericArgs(13);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(14)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor = state.dataIPShortCut->rNumericArgs(14);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(15)) {
            state.dataPumps->PumpEquip(PumpNum).MinVolFlowRateFrac = state.dataIPShortCut->rNumericArgs(15);
        }

        if (NumAlphas > 14) {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(15);
        } else {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = "General";
        }

        // Is this really necessary for each pump GetInput loop?
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;
    }

    cCurrentModuleObject = cPump_ConSpeed;

    for (NumConstPump = 1; NumConstPump <= NumConstSpeedPumps; ++NumConstPump) {
        PumpNum = NumVarSpeedPumps + NumConstPump;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NumConstPump,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPumps->PumpUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPumps->PumpEquip(PumpNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPumps->PumpEquip(PumpNum).pumpType = PumpType::ConSpeed; //'Pump:ConstantSpeed'
        state.dataPumps->PumpEquip(PumpNum).TypeOf_Num = TypeOf_PumpConstantSpeed;

        state.dataPumps->PumpEquip(PumpNum).InletNodeNum = GetOnlySingleNode(state,
                                                                             state.dataIPShortCut->cAlphaArgs(2),
                                                                             ErrorsFound,
                                                                             cCurrentModuleObject,
                                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                                             DataLoopNode::NodeFluidType::Water,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             1,
                                                                             ObjectIsNotParent);

        state.dataPumps->PumpEquip(PumpNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                              state.dataIPShortCut->cAlphaArgs(3),
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                                              DataLoopNode::NodeFluidType::Water,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Water Nodes");

        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).NomPumpHead = state.dataIPShortCut->rNumericArgs(2);
        state.dataPumps->PumpEquip(PumpNum).NomPowerUse = state.dataIPShortCut->rNumericArgs(3);
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).MotorEffic = state.dataIPShortCut->rNumericArgs(4);
        state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid = state.dataIPShortCut->rNumericArgs(5);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) = 1.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) = 0.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) = 0.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) = 0.0;
        // In a constant volume pump we previously set the minimum to the nominal capacity
        // Now we model the pump as constant speed and set flow by riding the pump curve.
        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Continuous")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Intermittent")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Intermittent;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(4));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(4) + "]. " + state.dataIPShortCut->cAlphaFieldNames(4) +
                                  " has been set to Continuous for this pump.");
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        }

        // Input the optional schedule for the pump
        state.dataPumps->PumpEquip(PumpNum).PumpSchedule = state.dataIPShortCut->cAlphaArgs(5);
        state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (!state.dataIPShortCut->lAlphaFieldBlanks(5) && !(state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex > 0)) {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(5));
            ShowContinueError(state, "Schedule named =[" + state.dataIPShortCut->cAlphaArgs(5) + "]. was not found and will not be used.");
        }

        // Input pressure related data such as pressure curve and impeller size/rotational speed
        state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name = state.dataIPShortCut->cAlphaArgs(6);
        if (state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name == "") {
            state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = -1;
        } else {
            TempCurveIndex = GetCurveIndex(state, state.dataPumps->PumpEquip(PumpNum).PressureCurve_Name);
            if (TempCurveIndex == 0) {
                state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = -1;
            } else {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            TempCurveIndex,                             // Curve index
                                                            {1},                                        // Valid dimensions
                                                            RoutineName,                                // Routine name
                                                            cCurrentModuleObject,                       // Object Type
                                                            state.dataPumps->PumpEquip(PumpNum).Name,   // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(6)); // Field Name

                if (!ErrorsFound) {
                    state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index = TempCurveIndex;
                    GetCurveMinMaxValues(
                        state, TempCurveIndex, state.dataPumps->PumpEquip(PumpNum).MinPhiValue, state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);
                }
            }
        }

        // read in the rest of the pump pressure characteristics
        state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter = state.dataIPShortCut->rNumericArgs(6);
        state.dataPumps->PumpEquip(PumpNum).RotSpeed_RPM = state.dataIPShortCut->rNumericArgs(7); // retrieve the input rotational speed, in revs/min
        state.dataPumps->PumpEquip(PumpNum).RotSpeed =
            state.dataPumps->PumpEquip(PumpNum).RotSpeed_RPM / 60.0; // convert input[rpm] to calculation units[rps]

        if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) { // zone named for pump skin losses
            state.dataPumps->PumpEquip(PumpNum).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(7), state.dataHeatBal->Zone);
            if (state.dataPumps->PumpEquip(PumpNum).ZoneNum > 0) {
                state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone = true;
                if (!state.dataIPShortCut->lNumericFieldBlanks(8)) {
                    state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction = state.dataIPShortCut->rNumericArgs(8);
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOW") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlow;
            } else if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOWPERPRESSURE") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlowPerPressure;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", sizing method type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(9)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor = state.dataIPShortCut->rNumericArgs(9);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(10)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor = state.dataIPShortCut->rNumericArgs(10);
        }

        if (NumAlphas > 8) {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(9);
        } else {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = "General";
        }
    }

    // pumps for steam system pumping condensate
    cCurrentModuleObject = cPump_Cond;
    for (NumCondPump = 1; NumCondPump <= NumCondensatePumps; ++NumCondPump) {
        PumpNum = NumCondPump + NumVarSpeedPumps + NumConstSpeedPumps;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NumCondPump,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPumps->PumpUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPumps->PumpEquip(PumpNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPumps->PumpEquip(PumpNum).pumpType = PumpType::Cond; //'Pump:VariableSpeed:Condensate'
        state.dataPumps->PumpEquip(PumpNum).TypeOf_Num = TypeOf_PumpCondensate;

        state.dataPumps->PumpEquip(PumpNum).InletNodeNum = GetOnlySingleNode(state,
                                                                             state.dataIPShortCut->cAlphaArgs(2),
                                                                             ErrorsFound,
                                                                             cCurrentModuleObject,
                                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                                             DataLoopNode::NodeFluidType::Steam,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             1,
                                                                             ObjectIsNotParent);

        state.dataPumps->PumpEquip(PumpNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                              state.dataIPShortCut->cAlphaArgs(3),
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                                              DataLoopNode::NodeFluidType::Steam,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Water Nodes");

        state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Intermittent;

        // Input the optional schedule for the pump
        state.dataPumps->PumpEquip(PumpNum).PumpSchedule = state.dataIPShortCut->cAlphaArgs(4);
        state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
        if (!state.dataIPShortCut->lAlphaFieldBlanks(4) && !(state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex > 0)) {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(4));
            ShowContinueError(state, "Schedule named =[" + state.dataIPShortCut->cAlphaArgs(4) + "]. was not found and will not be used.");
        }

        state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRateWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).NomPumpHead = state.dataIPShortCut->rNumericArgs(2);
        state.dataPumps->PumpEquip(PumpNum).NomPowerUse = state.dataIPShortCut->rNumericArgs(3);
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).MotorEffic = state.dataIPShortCut->rNumericArgs(4);
        state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid = state.dataIPShortCut->rNumericArgs(5);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) = state.dataIPShortCut->rNumericArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) = state.dataIPShortCut->rNumericArgs(7);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) = state.dataIPShortCut->rNumericArgs(8);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) = state.dataIPShortCut->rNumericArgs(9);

        if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) { // zone named for pump skin losses
            state.dataPumps->PumpEquip(PumpNum).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(5), state.dataHeatBal->Zone);
            if (state.dataPumps->PumpEquip(PumpNum).ZoneNum > 0) {
                state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone = true;
                if (!state.dataIPShortCut->lNumericFieldBlanks(10)) {
                    state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction = state.dataIPShortCut->rNumericArgs(10);
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\" not found.");
                ErrorsFound = true;
            }
        }

        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;

        if (state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRateWasAutoSized) {
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = AutoSize;
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized = true;
        } else {
            // Calc Condensate Pump Water Volume Flow Rate
            SteamDensity =
                GetSatDensityRefrig(state, fluidNameSteam, StartTemp, 1.0, state.dataPumps->PumpEquip(PumpNum).FluidIndex, RoutineNameNoColon);
            TempWaterDensity = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, DummyWaterIndex, RoutineName);
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate =
                (state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate * SteamDensity) / TempWaterDensity;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            if (state.dataIPShortCut->cAlphaArgs(6) == "POWERPERFLOW") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlow;
            } else if (state.dataIPShortCut->cAlphaArgs(6) == "POWERPERFLOWPERPRESSURE") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlowPerPressure;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", sizing method type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(11)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor = state.dataIPShortCut->rNumericArgs(11);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(12)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor = state.dataIPShortCut->rNumericArgs(12);
        }

        if (NumAlphas > 6) {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(7);
        } else {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = "General";
        }
    }

    // LOAD Variable Speed Pump Bank ARRAYS WITH VARIABLE SPEED CURVE FIT PUMP DATA
    cCurrentModuleObject = cPumpBank_VarSpeed;
    for (NumVarPumpBankSimple = 1; NumVarPumpBankSimple <= NumPumpBankSimpleVar; ++NumVarPumpBankSimple) {
        PumpNum = NumVarPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NumVarPumpBankSimple,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPumps->PumpUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPumps->PumpEquip(PumpNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPumps->PumpEquip(PumpNum).pumpType = PumpType::Bank_VarSpeed; //'HeaderedPumps:VariableSpeed'
        state.dataPumps->PumpEquip(PumpNum).TypeOf_Num = TypeOf_PumpBankVariableSpeed;

        state.dataPumps->PumpEquip(PumpNum).InletNodeNum = GetOnlySingleNode(state,
                                                                             state.dataIPShortCut->cAlphaArgs(2),
                                                                             ErrorsFound,
                                                                             cCurrentModuleObject,
                                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                                             DataLoopNode::NodeFluidType::Water,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             1,
                                                                             ObjectIsNotParent);

        state.dataPumps->PumpEquip(PumpNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                              state.dataIPShortCut->cAlphaArgs(3),
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                                              DataLoopNode::NodeFluidType::Water,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Water Nodes");

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Optimal")) {
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::OptimalScheme;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Sequential")) {
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::SequentialScheme;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "SupplyEquipmentAssigned")) {
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::UserDefined;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(4));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(4) + "]. " + state.dataIPShortCut->cAlphaFieldNames(4) +
                                  " has been set to Sequential for this pump.");
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::SequentialScheme;
        }

        //    PumpEquip(PumpNum)%PumpControlType = state.dataIPShortCut->cAlphaArgs(5)
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Continuous")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Intermittent")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Intermittent;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(5));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(5) + "]. " + state.dataIPShortCut->cAlphaFieldNames(5) +
                                  " has been set to Continuous for this pump.");
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        }

        // Input the optional schedule for the pump
        state.dataPumps->PumpEquip(PumpNum).PumpSchedule = state.dataIPShortCut->cAlphaArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (!state.dataIPShortCut->lAlphaFieldBlanks(6) && !(state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex > 0)) {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(6));
            ShowContinueError(state, "Schedule named =[" + state.dataIPShortCut->cAlphaArgs(6) + "]. was not found and will not be used.");
        }

        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank = state.dataIPShortCut->rNumericArgs(2);
        state.dataPumps->PumpEquip(PumpNum).NomPumpHead = state.dataIPShortCut->rNumericArgs(3);
        state.dataPumps->PumpEquip(PumpNum).NomPowerUse = state.dataIPShortCut->rNumericArgs(4);
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).MotorEffic = state.dataIPShortCut->rNumericArgs(5);
        state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid = state.dataIPShortCut->rNumericArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) = state.dataIPShortCut->rNumericArgs(7);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) = state.dataIPShortCut->rNumericArgs(8);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) = state.dataIPShortCut->rNumericArgs(9);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) = state.dataIPShortCut->rNumericArgs(10);
        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRateFrac = state.dataIPShortCut->rNumericArgs(11);
        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate =
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate * state.dataPumps->PumpEquip(PumpNum).MinVolFlowRateFrac;

        if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) { // zone named for pump skin losses
            state.dataPumps->PumpEquip(PumpNum).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(7), state.dataHeatBal->Zone);
            if (state.dataPumps->PumpEquip(PumpNum).ZoneNum > 0) {
                state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone = true;
                if (!state.dataIPShortCut->lNumericFieldBlanks(12)) {
                    state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction = state.dataIPShortCut->rNumericArgs(12);
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOW") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlow;
            } else if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOWPERPRESSURE") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlowPerPressure;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", sizing method type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(13)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor = state.dataIPShortCut->rNumericArgs(13);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(14)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor = state.dataIPShortCut->rNumericArgs(14);
        }

        if (NumAlphas > 8) {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(9);
        } else {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = "General";
        }

        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;
    }

    cCurrentModuleObject = cPumpBank_ConSpeed;
    for (NumConstPumpBankSimple = 1; NumConstPumpBankSimple <= NumPumpBankSimpleConst; ++NumConstPumpBankSimple) {
        PumpNum = NumConstPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 NumConstPumpBankSimple,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPumps->PumpUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPumps->PumpEquip(PumpNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPumps->PumpEquip(PumpNum).pumpType = PumpType::Bank_ConSpeed; //'HeaderedPumps:ConstantSpeed'
        state.dataPumps->PumpEquip(PumpNum).TypeOf_Num = TypeOf_PumpBankConstantSpeed;

        state.dataPumps->PumpEquip(PumpNum).InletNodeNum = GetOnlySingleNode(state,
                                                                             state.dataIPShortCut->cAlphaArgs(2),
                                                                             ErrorsFound,
                                                                             cCurrentModuleObject,
                                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                                             DataLoopNode::NodeFluidType::Water,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             1,
                                                                             ObjectIsNotParent);

        state.dataPumps->PumpEquip(PumpNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                              state.dataIPShortCut->cAlphaArgs(3),
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                                              DataLoopNode::NodeFluidType::Water,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Water Nodes");

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Optimal")) {
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::OptimalScheme;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Sequential")) {
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::SequentialScheme;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(4));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(4) + "]. " + state.dataIPShortCut->cAlphaFieldNames(4) +
                                  " has been set to Sequential for this pump.");
            state.dataPumps->PumpEquip(PumpNum).SequencingScheme = PumpBankControlSeq::SequentialScheme;
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Continuous")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Intermittent")) {
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Intermittent;
        } else {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(5));
            ShowContinueError(state,
                              "Entered Value=[" + state.dataIPShortCut->cAlphaArgs(5) + "]. " + state.dataIPShortCut->cAlphaFieldNames(5) +
                                  " has been set to Continuous for this pump.");
            state.dataPumps->PumpEquip(PumpNum).PumpControl = PumpControlType::Continuous;
        }

        // Input the optional schedule for the pump
        state.dataPumps->PumpEquip(PumpNum).PumpSchedule = state.dataIPShortCut->cAlphaArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (!state.dataIPShortCut->lAlphaFieldBlanks(6) && !(state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex > 0)) {
            ShowWarningError(state,
                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name + "\", Invalid " +
                                 state.dataIPShortCut->cAlphaFieldNames(6));
            ShowContinueError(state, "Schedule named =[" + state.dataIPShortCut->cAlphaArgs(6) + "]. was not found and will not be used.");
        }

        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank = state.dataIPShortCut->rNumericArgs(2);
        state.dataPumps->PumpEquip(PumpNum).NomPumpHead = state.dataIPShortCut->rNumericArgs(3);
        state.dataPumps->PumpEquip(PumpNum).NomPowerUse = state.dataIPShortCut->rNumericArgs(4);
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse == AutoSize) {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized = true;
        }
        state.dataPumps->PumpEquip(PumpNum).MotorEffic = state.dataIPShortCut->rNumericArgs(5);
        state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid = state.dataIPShortCut->rNumericArgs(6);
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) = 1.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) = 0.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) = 0.0;
        state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) = 0.0;

        if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) { // zone named for pump skin losses
            state.dataPumps->PumpEquip(PumpNum).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(7), state.dataHeatBal->Zone);
            if (state.dataPumps->PumpEquip(PumpNum).ZoneNum > 0) {
                state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone = true;
                if (!state.dataIPShortCut->lNumericFieldBlanks(7)) {
                    state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction = state.dataIPShortCut->rNumericArgs(7);
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                ErrorsFound = true;
            }
        }
        if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOW") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlow;
            } else if (state.dataIPShortCut->cAlphaArgs(8) == "POWERPERFLOWPERPRESSURE") {
                state.dataPumps->PumpEquip(PumpNum).powerSizingMethod = PowerSizingMethod::sizePowerPerFlowPerPressure;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataPumps->PumpEquip(PumpNum).Name +
                                    "\", sizing method type entered is invalid.  Use one of the key choice entries.");
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(8)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor = state.dataIPShortCut->rNumericArgs(8);
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(9)) {
            state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor = state.dataIPShortCut->rNumericArgs(9);
        }

        if (NumAlphas > 8) {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(9);
        } else {
            state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName = "General";
        }

        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in getting Pump input");
    }

    for (PumpNum = 1; PumpNum <= state.dataPumps->NumPumps; ++PumpNum) { // CurrentModuleObject='Pumps'
        switch (state.dataPumps->PumpEquip(PumpNum).pumpType) {
        case (PumpType::VarSpeed):
        case (PumpType::ConSpeed):
        case (PumpType::Cond): {

            SetupOutputVariable(state,
                                "Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataPumps->PumpEquip(PumpNum).Energy,
                                "System",
                                "Sum",
                                state.dataPumps->PumpEquip(PumpNum).Name,
                                _,
                                "Electricity",
                                "Pumps",
                                state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName,
                                "Plant");
            SetupOutputVariable(state,
                                "Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquip(PumpNum).Power,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Shaft Power",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).ShaftPower,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Fluid Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluid,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Fluid Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluidEnergy,
                                "System",
                                "Sum",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPumps->PumpEquipReport(PumpNum).OutletTemp,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpMassFlowRate,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
        } break;

        case (PumpType::Bank_VarSpeed):
        case (PumpType::Bank_ConSpeed): { // CurrentModuleObject='HeaderedPumps'

            SetupOutputVariable(state,
                                "Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataPumps->PumpEquip(PumpNum).Energy,
                                "System",
                                "Sum",
                                state.dataPumps->PumpEquip(PumpNum).Name,
                                _,
                                "Electricity",
                                "Pumps",
                                state.dataPumps->PumpEquip(PumpNum).EndUseSubcategoryName,
                                "Plant");
            SetupOutputVariable(state,
                                "Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquip(PumpNum).Power,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Shaft Power",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).ShaftPower,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Fluid Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluid,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Fluid Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluidEnergy,
                                "System",
                                "Sum",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPumps->PumpEquipReport(PumpNum).OutletTemp,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataPumps->PumpEquipReport(PumpNum).PumpMassFlowRate,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Operating Pumps Count",
                                OutputProcessor::Unit::None,
                                state.dataPumps->PumpEquipReport(PumpNum).NumPumpsOperating,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
        } break;
        default: {
            ShowFatalError(state, format("Invalid Pump Type = {}", state.dataPumps->PumpEquip(PumpNum).pumpType));
        } break;
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state,
                                     "Pump Maximum Mass Flow Rate",
                                     state.dataPumps->PumpEquip(PumpNum).Name,
                                     "[kg/s]",
                                     state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax);
            SetupEMSActuator(state,
                             "Pump",
                             state.dataPumps->PumpEquip(PumpNum).Name,
                             "Pump Mass Flow Rate",
                             "[kg/s]",
                             state.dataPumps->PumpEquip(PumpNum).EMSMassFlowOverrideOn,
                             state.dataPumps->PumpEquip(PumpNum).EMSMassFlowValue);
            SetupEMSActuator(state,
                             "Pump",
                             state.dataPumps->PumpEquip(PumpNum).Name,
                             "Pump Pressure Rise",
                             "[Pa]",
                             state.dataPumps->PumpEquip(PumpNum).EMSPressureOverrideOn,
                             state.dataPumps->PumpEquip(PumpNum).EMSPressureOverrideValue);
        }

        if (state.dataPumps->PumpEquip(PumpNum).HeatLossesToZone) {
            // setup skin loss output vars
            SetupOutputVariable(state,
                                "Pump Zone Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Zone Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainEnergy,
                                "System",
                                "Sum",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Zone Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);
            SetupOutputVariable(state,
                                "Pump Zone Radiative Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate,
                                "System",
                                "Average",
                                state.dataPumps->PumpEquip(PumpNum).Name);

            // setup internal gains
            {
                auto const SELECT_CASE_var(state.dataPumps->PumpEquip(PumpNum).pumpType);
                if (SELECT_CASE_var == PumpType::VarSpeed) {
                    SetupZoneInternalGain(state,
                                          state.dataPumps->PumpEquip(PumpNum).ZoneNum,
                                          "Pump:VariableSpeed",
                                          state.dataPumps->PumpEquip(PumpNum).Name,
                                          IntGainTypeOf_Pump_VarSpeed,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                          nullptr,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate);
                } else if (SELECT_CASE_var == PumpType::ConSpeed) {
                    SetupZoneInternalGain(state,
                                          state.dataPumps->PumpEquip(PumpNum).ZoneNum,
                                          "Pump:ConstantSpeed",
                                          state.dataPumps->PumpEquip(PumpNum).Name,
                                          IntGainTypeOf_Pump_ConSpeed,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                          nullptr,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate);
                } else if (SELECT_CASE_var == PumpType::Cond) {
                    SetupZoneInternalGain(state,
                                          state.dataPumps->PumpEquip(PumpNum).ZoneNum,
                                          "Pump:VariableSpeed:Condensate",
                                          state.dataPumps->PumpEquip(PumpNum).Name,
                                          IntGainTypeOf_Pump_Cond,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                          nullptr,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate);
                } else if (SELECT_CASE_var == PumpType::Bank_VarSpeed) {
                    SetupZoneInternalGain(state,
                                          state.dataPumps->PumpEquip(PumpNum).ZoneNum,
                                          "HeaderedPumps:VariableSpeed",
                                          state.dataPumps->PumpEquip(PumpNum).Name,
                                          IntGainTypeOf_PumpBank_VarSpeed,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                          nullptr,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate);
                } else if (SELECT_CASE_var == PumpType::Bank_ConSpeed) {
                    SetupZoneInternalGain(state,
                                          state.dataPumps->PumpEquip(PumpNum).ZoneNum,
                                          "HeaderedPumps:ConstantSpeed",
                                          state.dataPumps->PumpEquip(PumpNum).Name,
                                          IntGainTypeOf_PumpBank_ConSpeed,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate,
                                          nullptr,
                                          &state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate);
                }
            }
        }
    }
}

void InitializePumps(EnergyPlusData &state, int const PumpNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:        Edwin Lee
    //       DATE WRITTEN:  August 2010
    //       MODIFIED       Based on the INIT section of InitSimVars, credits here:
    //                        Author:
    //                          Oct 1998 Dan Fisher
    //                        Modifications:
    //                          Jul 2001 Richard Liesen
    //                          July 2001, Rick Strand (implemented new pump controls)
    //                          May 2009, Brent Griffith (added EMS override capability)
    //                          Nov 2010, Brent Griffith (call InitComponentNodes, generalize fluid props)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does one-time and begin-envrn inits for the pump

    // Using/Aliasing
    using DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;

    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const StartTemp(100.0); // Standard Temperature across code to calculated Steam density
    Real64 const ZeroPowerTol(0.0000001);
    static constexpr std::string_view RoutineName("PlantPumps::InitializePumps ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;  // pump inlet node number
    int OutletNode; // pump outlet node number
    Real64 TotalEffic;
    Real64 SteamDensity; // Density of working fluid
    int DummyWaterIndex(1);
    Real64 TempWaterDensity;
    bool errFlag;
    Real64 mdotMax; // local fluid mass flow rate maximum
    Real64 mdotMin; // local fluid mass flow rate minimum
    int plloopnum;
    int lsnum;
    int brnum;
    int cpnum;

    // Set some variables for convenience
    InletNode = state.dataPumps->PumpEquip(PumpNum).InletNodeNum;
    OutletNode = state.dataPumps->PumpEquip(PumpNum).OutletNodeNum;

    // One time inits
    if (state.dataPumps->PumpEquip(PumpNum).PumpOneTimeFlag) {

        errFlag = false;
        ScanPlantLoopsForObject(state,
                                state.dataPumps->PumpEquip(PumpNum).Name,
                                state.dataPumps->PumpEquip(PumpNum).TypeOf_Num,
                                state.dataPumps->PumpEquip(PumpNum).LoopNum,
                                state.dataPumps->PumpEquip(PumpNum).LoopSideNum,
                                state.dataPumps->PumpEquip(PumpNum).BranchNum,
                                state.dataPumps->PumpEquip(PumpNum).CompNum,
                                errFlag,
                                _,
                                _,
                                _,
                                _,
                                _);
        plloopnum = state.dataPumps->PumpEquip(PumpNum).LoopNum;
        lsnum = state.dataPumps->PumpEquip(PumpNum).LoopSideNum;
        brnum = state.dataPumps->PumpEquip(PumpNum).BranchNum;
        cpnum = state.dataPumps->PumpEquip(PumpNum).CompNum;
        if (plloopnum > 0 && lsnum > 0 && brnum > 0 && cpnum > 0) {
            if (state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn != InletNode ||
                state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut != OutletNode) {
                ShowSevereError(state,
                                "InitializePumps: " + cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType] + "=\"" +
                                    state.dataPumps->PumpEquip(PumpNum).Name + "\", non-matching nodes.");
                ShowContinueError(state,
                                  "...in Branch=\"" + state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Name +
                                      "\", Component referenced with:");
                ShowContinueError(state,
                                  "...Inlet Node=\"" + state.dataLoopNodes->NodeID(
                                                           state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn));
                ShowContinueError(
                    state,
                    "...Outlet Node=\"" +
                        state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut));
                ShowContinueError(state, "...Pump Inlet Node=\"" + state.dataLoopNodes->NodeID(InletNode));
                ShowContinueError(state, "...Pump Outlet Node=\"" + state.dataLoopNodes->NodeID(OutletNode));
                errFlag = true;
            }
        } else { // CR9292
            ShowSevereError(state,
                            "InitializePumps: " + cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType] + "=\"" +
                                state.dataPumps->PumpEquip(PumpNum).Name + "\", component missing.");
            errFlag = true; // should have received warning/severe earlier, will reiterate
        }

        if (errFlag) {
            ShowFatalError(state, "InitializePumps: Program terminated due to previous condition(s).");
        }
        state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
            .LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum)
            .Branch(state.dataPumps->PumpEquip(PumpNum).BranchNum)
            .Comp(state.dataPumps->PumpEquip(PumpNum).CompNum)
            .CompNum = PumpNum;

        SizePump(state, PumpNum);

        // calculate the efficiency for each pump
        // by calculating the efficiency for each pump being simulated.  The calculation
        // is based on the PMPSIM code in the ASHRAE Secondary Toolkit
        if (state.dataPumps->PumpEquip(PumpNum).NomPowerUse > ZeroPowerTol && state.dataPumps->PumpEquip(PumpNum).MotorEffic > ZeroPowerTol) {
            TotalEffic = state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate * state.dataPumps->PumpEquip(PumpNum).NomPumpHead /
                         state.dataPumps->PumpEquip(PumpNum).NomPowerUse;
            state.dataPumps->PumpEquip(PumpNum).PumpEffic = TotalEffic / state.dataPumps->PumpEquip(PumpNum).MotorEffic;
            if (state.dataPumps->PumpEquip(PumpNum).PumpEffic < 0.50) {
                ShowWarningError(state,
                                 format("Check input. Calculated Pump Efficiency={:.2R}% which is less than 50%, for pump={}",
                                        state.dataPumps->PumpEquip(PumpNum).PumpEffic * 100.0,
                                        state.dataPumps->PumpEquip(PumpNum).Name));
                ShowContinueError(state,
                                  format("Calculated Pump_Efficiency % =Total_Efficiency % [{:.1R}] / Motor_Efficiency % [{:.1R}]",
                                         TotalEffic * 100.0,
                                         state.dataPumps->PumpEquip(PumpNum).MotorEffic * 100.0));
                ShowContinueError(
                    state,
                    format("Total_Efficiency % =(Rated_Volume_Flow_Rate [{:.1R}] * Rated_Pump_Head [{:.1R}] / Rated_Power_Use [{:.1R}]) * 100.",
                           state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate,
                           state.dataPumps->PumpEquip(PumpNum).NomPumpHead,
                           state.dataPumps->PumpEquip(PumpNum).NomPowerUse));
            } else if ((state.dataPumps->PumpEquip(PumpNum).PumpEffic > 0.95) && (state.dataPumps->PumpEquip(PumpNum).PumpEffic <= 1.0)) {
                ShowWarningError(state,
                                 format("Check input.  Calculated Pump Efficiency={:.2R}% is approaching 100%, for pump={}",
                                        state.dataPumps->PumpEquip(PumpNum).PumpEffic * 100.0,
                                        state.dataPumps->PumpEquip(PumpNum).Name));
                ShowContinueError(state,
                                  format("Calculated Pump_Efficiency % =Total_Efficiency % [{:.1R}] / Motor_Efficiency % [{:.1R}]",
                                         TotalEffic * 100.0,
                                         state.dataPumps->PumpEquip(PumpNum).MotorEffic * 100.0));
                ShowContinueError(
                    state,
                    format("Total_Efficiency % =(Rated_Volume_Flow_Rate [{:.1R}] * Rated_Pump_Head [{:.1R}] / Rated_Power_Use [{:.1R}]) * 100.",
                           state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate,
                           state.dataPumps->PumpEquip(PumpNum).NomPumpHead,
                           state.dataPumps->PumpEquip(PumpNum).NomPowerUse));
            } else if (state.dataPumps->PumpEquip(PumpNum).PumpEffic > 1.0) {
                ShowSevereError(state,
                                format("Check input.  Calculated Pump Efficiency={:.3R}% which is bigger than 100%, for pump={}",
                                       state.dataPumps->PumpEquip(PumpNum).PumpEffic * 100.0,
                                       state.dataPumps->PumpEquip(PumpNum).Name));
                ShowContinueError(state,
                                  format("Calculated Pump_Efficiency % =Total_Efficiency % [{:.1R}] / Motor_Efficiency % [{:.1R}]",
                                         TotalEffic * 100.0,
                                         state.dataPumps->PumpEquip(PumpNum).MotorEffic * 100.0));
                ShowContinueError(
                    state,
                    format("Total_Efficiency % =(Rated_Volume_Flow_Rate [{:.1R}] * Rated_Pump_Head [{:.1R}] / Rated_Power_Use [{:.1R}]) * 100.",
                           state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate,
                           state.dataPumps->PumpEquip(PumpNum).NomPumpHead,
                           state.dataPumps->PumpEquip(PumpNum).NomPowerUse));
                ShowFatalError(state, "Errors found in Pump input");
            }
        } else {
            ShowWarningError(state,
                             "Check input. Pump nominal power or motor efficiency is set to 0, for pump=" + state.dataPumps->PumpEquip(PumpNum).Name);
        }

        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate <= SmallWaterVolFlow) {
            ShowWarningError(state,
                             "Check input. Pump nominal flow rate is set or calculated = 0, for pump=" + state.dataPumps->PumpEquip(PumpNum).Name);
        }

        if (state.dataPumps->PumpEquip(PumpNum).PumpControl == PumpControlType::Continuous) {
            // reset flow priority appropriately (default was for Intermittent)
            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                .LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum)
                .Branch(state.dataPumps->PumpEquip(PumpNum).BranchNum)
                .Comp(state.dataPumps->PumpEquip(PumpNum).CompNum)
                .FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
        }

        state.dataPumps->PumpEquip(PumpNum).PumpOneTimeFlag = false;
    }

    // HVAC Sizing Simulation resizing calls if needed
    if (state.dataGlobal->RedoSizesHVACSimulation && !state.dataPlnt->PlantReSizingCompleted) {
        SizePump(state, PumpNum);
    }

    // Begin environment inits
    if (state.dataPumps->PumpEquip(PumpNum).PumpInitFlag && state.dataGlobal->BeginEnvrnFlag) {
        if (state.dataPumps->PumpEquip(PumpNum).pumpType == PumpType::Cond) {

            TempWaterDensity = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, DummyWaterIndex, RoutineName);
            SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, StartTemp, 1.0, state.dataPumps->PumpEquip(PumpNum).FluidIndex, RoutineName);
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate =
                (state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate * SteamDensity) / TempWaterDensity;

            // set the maximum flow rate on the outlet node
            mdotMax = state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate * SteamDensity;
            // mdotMin = PumpEquip(PumpNum)%MinVolFlowRate      * SteamDensity
            // On a pump the 'hardware min' (MassFlowRateMin) must be defined as zero and not
            // confused with the desired pump operating scheme or the user specified
            //'minimum flow rate'.  The user specified 'minimum flow rate' determines the minimum
            // flow rate under normal operating conditions.  For cases when 'MaxAvail' on the pump
            // inlet node actually less than the 'minimum flow rate' specified by the user, than a
            // loop shutdown must  be triggered.
            mdotMin = 0.0;
            InitComponentNodes(state,
                               mdotMin,
                               mdotMax,
                               InletNode,
                               OutletNode,
                               state.dataPumps->PumpEquip(PumpNum).LoopNum,
                               state.dataPumps->PumpEquip(PumpNum).LoopSideNum,
                               state.dataPumps->PumpEquip(PumpNum).BranchNum,
                               state.dataPumps->PumpEquip(PumpNum).CompNum);
            state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax = mdotMax;
            state.dataPumps->PumpEquip(PumpNum).MassFlowRateMin = state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate * SteamDensity;

        } else {
            TempWaterDensity = GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidName,
                                                DataGlobalConstants::InitConvTemp,
                                                state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidIndex,
                                                RoutineName);
            mdotMax = state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate * TempWaterDensity;
            // mdotMin = PumpEquip(PumpNum)%MinVolFlowRate * TempWaterDensity
            // see note above
            mdotMin = 0.0;
            InitComponentNodes(state,
                               mdotMin,
                               mdotMax,
                               InletNode,
                               OutletNode,
                               state.dataPumps->PumpEquip(PumpNum).LoopNum,
                               state.dataPumps->PumpEquip(PumpNum).LoopSideNum,
                               state.dataPumps->PumpEquip(PumpNum).BranchNum,
                               state.dataPumps->PumpEquip(PumpNum).CompNum);
            state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax = mdotMax;
            state.dataPumps->PumpEquip(PumpNum).MassFlowRateMin = state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate * TempWaterDensity;
        }
        // zero out report variables
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ShaftPower = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluid = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluidEnergy = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).OutletTemp = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).PumpMassFlowRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).NumPumpsOperating = 0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainEnergy = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate = 0.0;

        state.dataPumps->PumpEquip(PumpNum).PumpInitFlag = false;
    }

    // Reset the local environment flag for the next environment
    if (!state.dataGlobal->BeginEnvrnFlag) state.dataPumps->PumpEquip(PumpNum).PumpInitFlag = true;

    // zero out module level working variables
    state.dataPumps->PumpMassFlowRate = 0.0;
    state.dataPumps->PumpHeattoFluid = 0.0;
    state.dataPumps->Power = 0.0;
    state.dataPumps->ShaftPower = 0.0;
}

//*************************************************************************!

//*************************************************************************!

void SetupPumpMinMaxFlows(EnergyPlusData &state, int const LoopNum, int const PumpNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:        Edwin Lee
    //       DATE WRITTEN:  Aug 2010
    //       MODIFIED       Based on the Flow control portion of what was previously Pumps::InitSimVars, by:
    //                        Dan Fisher October 1998
    //                        Richard Liesen July 2001
    //                        July 2001, Rick Strand (implemented new pump controls)
    //                        May 2009, Brent Griffith (added EMS override capability)
    //                        B. Griffith, Nov 2011 Pump control: Intermittent vs Continuous
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the pump minAvail and maxAvail flow rates, and assigns them to the
    //  outlet min/max avail according to inlet min/max constraints and zero flow request
    // The loop solver then uses this information to set up the flow bounds for the loop side
    //  for the current iteration.

    // METHODOLOGY EMPLOYED:
    //  Design flow rate and user specified minimum flow rate is compared in the inlet node
    //  min/maxavail.  The pump output is appropriately constrained.
    //  Design flow is rated flow times schedule fraction
    //  Inlet node max will represent the rated flow rate according to pump init routines.
    //  These values are bounded by hardware min constraints on the inlet node, which is likely zero.
    //  These values are also bounded by EMS overridable limit of max flow rate.

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using PlantPressureSystem::ResolveLoopFlowVsPressure;
    using PlantUtilities::BoundValueToWithinTwoValues;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("PlantPumps:SetupPumpMinMaxFlows: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;  // pump inlet node number
    int OutletNode; // pump outlet node number
    Real64 InletNodeMax;
    Real64 InletNodeMin;
    Real64 PumpMassFlowRateMax; // max allowable flow rate at the pump
    Real64 PumpMassFlowRateMin; // min allowable flow rate at the pump
    Real64 PumpSchedFraction;
    Real64 PumpOverridableMaxLimit;
    Real64 PumpMassFlowRateMinLimit;
    Real64 PumpSchedRPM; // Pump RPM Optional Input

    // Inlet/Outlet Node Numbers
    InletNode = state.dataPumps->PumpEquip(PumpNum).InletNodeNum;
    OutletNode = state.dataPumps->PumpEquip(PumpNum).OutletNodeNum;

    // Inlet node Min/MaxAvail
    InletNodeMax = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
    InletNodeMin = state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;

    // Retrive the pump speed fraction from the pump schedule
    if (state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex != 0) {
        PumpSchedFraction = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).PumpScheduleIndex);
        PumpSchedFraction = BoundValueToWithinTwoValues(PumpSchedFraction, 0.0, 1.0);
    } else {
        PumpSchedFraction = 1.0;
    }

    // User specified min/max mass flow rates for pump
    PumpOverridableMaxLimit = state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax;

    // override the user specified min to allow pump to turn off when no flow is required.
    if (state.dataPumps->PumpEquip(PumpNum).LoopSolverOverwriteFlag) {
        PumpMassFlowRateMinLimit = 0.0;
    } else {
        PumpMassFlowRateMinLimit = state.dataPumps->PumpEquip(PumpNum).MassFlowRateMin;
    }

    // The pump outlet node Min/MaxAvail
    PumpMassFlowRateMin = max(InletNodeMin, PumpMassFlowRateMinLimit);
    PumpMassFlowRateMax = min(InletNodeMax, PumpOverridableMaxLimit * PumpSchedFraction);

    // Check for conflicts (MaxAvail < MinAvail)
    if (PumpMassFlowRateMin > PumpMassFlowRateMax) { // the demand side wants to operate outside of the pump range
        // shut the pump (and the loop) down
        PumpMassFlowRateMin = 0.0;
        PumpMassFlowRateMax = 0.0;
        // Let the user know that his input file is overconstrained
    }

    {
        auto const SELECT_CASE_var(state.dataPumps->PumpEquip(PumpNum).pumpType);

        if (SELECT_CASE_var == PumpType::VarSpeed) {

            if (state.dataPumps->PumpEquip(PumpNum).HasVFD) {
                {
                    ControlTypeVFD SELECT_CASE_var1(state.dataPumps->PumpEquip(PumpNum).VFD.VFDControlType);
                    if (SELECT_CASE_var1 == ControlTypeVFD::VFDManual) {

                        // Evaluate the schedule if it exists and put the fraction into a local variable
                        PumpSchedRPM = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.ManualRPMSchedIndex);
                        // Convert the RPM to rot/sec for calculation routine
                        state.dataPumps->PumpEquip(PumpNum).RotSpeed = PumpSchedRPM / 60.0;
                        // Resolve the new mass flow rate based on current pressure characteristics
                        if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs &&
                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureSimType ==
                                DataPlant::iPressSimType::FlowCorrection &&
                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureDrop > 0.0) {

                            state.dataPumps->PumpMassFlowRate =
                                ResolveLoopFlowVsPressure(state,
                                                          state.dataPumps->PumpEquip(PumpNum).LoopNum,
                                                          state.dataLoopNodes->Node(state.dataPumps->PumpEquip(PumpNum).InletNodeNum).MassFlowRate,
                                                          state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index,
                                                          state.dataPumps->PumpEquip(PumpNum).RotSpeed,
                                                          state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter,
                                                          state.dataPumps->PumpEquip(PumpNum).MinPhiValue,
                                                          state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);

                            PumpMassFlowRateMax = state.dataPumps->PumpMassFlowRate;
                            PumpMassFlowRateMin = state.dataPumps->PumpMassFlowRate;
                        }

                    } else if (SELECT_CASE_var1 == ControlTypeVFD::VFDAutomatic) {

                        if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs &&
                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureSimType ==
                                DataPlant::iPressSimType::FlowCorrection &&
                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureDrop > 0.0) {

                            GetRequiredMassFlowRate(state,
                                                    LoopNum,
                                                    PumpNum,
                                                    state.dataLoopNodes->Node(state.dataPumps->PumpEquip(PumpNum).InletNodeNum).MassFlowRate,
                                                    state.dataPumps->PumpMassFlowRate,
                                                    PumpMassFlowRateMin,
                                                    PumpMassFlowRateMax);
                        }
                    }
                } // VFDControlType
            }

            if (state.dataPumps->PumpEquip(PumpNum).PumpControl == PumpControlType::Continuous) {
                state.dataLoopNodes->Node(InletNode).MassFlowRateRequest = PumpMassFlowRateMin;
            }

        } else if (SELECT_CASE_var == PumpType::ConSpeed) {

            if (state.dataPumps->PumpEquip(PumpNum).PumpControl == PumpControlType::Continuous) {
                PumpMassFlowRateMin = PumpMassFlowRateMax;
                state.dataLoopNodes->Node(InletNode).MassFlowRateRequest = PumpMassFlowRateMin;
            }

            // Override (lock down flow) for pressure drop if applicable
            if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
                if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs &&
                    state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureSimType ==
                        DataPlant::iPressSimType::FlowCorrection &&
                    state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureDrop > 0.0) {
                    state.dataPumps->PumpMassFlowRate =
                        ResolveLoopFlowVsPressure(state,
                                                  state.dataPumps->PumpEquip(PumpNum).LoopNum,
                                                  state.dataLoopNodes->Node(state.dataPumps->PumpEquip(PumpNum).InletNodeNum).MassFlowRate,
                                                  state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index,
                                                  state.dataPumps->PumpEquip(PumpNum).RotSpeed,
                                                  state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter,
                                                  state.dataPumps->PumpEquip(PumpNum).MinPhiValue,
                                                  state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);
                    PumpMassFlowRateMax = state.dataPumps->PumpMassFlowRate;
                    PumpMassFlowRateMin = state.dataPumps->PumpMassFlowRate;
                }
            }
        }
    }

    // Override pump operation based on System Availability Managers, should be done elsewhere?  I suppose this should be OK though
    if (allocated(state.dataPlnt->PlantAvailMgr)) {
        if (state.dataPlnt->PlantAvailMgr(LoopNum).AvailStatus == ForceOff) {
            PumpMassFlowRateMax = 0.0;
            PumpMassFlowRateMin = 0.0;
        }
    }

    // Check if EMS is overriding flow
    if (state.dataPumps->PumpEquip(PumpNum).EMSMassFlowOverrideOn) {
        PumpMassFlowRateMax = state.dataPumps->PumpEquip(PumpNum).EMSMassFlowValue;
        PumpMassFlowRateMin = state.dataPumps->PumpEquip(PumpNum).EMSMassFlowValue;
    }

    // Update outlet node to allow loop solver to get data
    // could avoid this by passing data in/out to avoid putting things on nodes
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = PumpMassFlowRateMin;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = PumpMassFlowRateMax;
}

void CalcPumps(EnergyPlusData &state, int const PumpNum, Real64 const FlowRequest, bool &PumpRunning)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Sept. 1998
    //       MODIFIED       July 2001, Rick Strand
    //       RE-ENGINEERED  Sept 2010, Edwin Lee

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutines simulates a pump following
    // the methodology oulined in ASHRAE's secondary toolkit.

    // METHODOLOGY EMPLOYED:
    // Calculates power and updates other pump things.

    // REFERENCES:
    // HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
    // Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    using PlantUtilities::SetComponentFlowRate;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("PlantPumps:CalcPumps: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int OutletNode;
    Real64 LoopDensity;
    Real64 VolFlowRate;
    Real64 PartLoadRatio;
    Real64 FracFullLoadPower;
    Real64 FullLoadVolFlowRate;
    Real64 PartLoadVolFlowRate;
    Real64 FullLoadPower;
    Real64 FullLoadPowerRatio;
    Real64 TotalEffic;
    PumpType pumpType;
    Real64 RotSpeed_Min;
    Real64 RotSpeed_Max;
    Real64 PumpActualRPMValueOne;
    Real64 PumpActualRPMValueTwo;

    InletNode = state.dataPumps->PumpEquip(PumpNum).InletNodeNum;
    OutletNode = state.dataPumps->PumpEquip(PumpNum).OutletNodeNum;
    pumpType = state.dataPumps->PumpEquip(PumpNum).pumpType;

    //****************************!
    //** SETTING PUMP FLOW RATE **!
    //****************************!
    // So the loop solver always passes in the full loop side flow request to each pump called
    // The pump will try to use this value according to its inlet conditions via the SetComponentFlowRate routine.
    // If the loop solver is doing branch pumps, then individual parallel branch inlet nodes would have been previously
    // constrained, so even though we pass in a full flow request, each pump will "pull down" to the min/max avail.
    // Also, on flowlock == locked, we will just use the inlet node flow rate
    // The flow resolver can take care of argument resolution beyond that.
    // For a typical situation, the flow request should be within the values of min/max avail, so the pump will get this flow rate.
    if (FlowRequest > DataBranchAirLoopPlant::MassFlowTolerance) {
        state.dataPumps->PumpMassFlowRate = FlowRequest;
    } else {
        state.dataPumps->PumpMassFlowRate = 0.0;
    }

    // For variable speed branch pumps, with other components
    //  on the branch, we are not going to assign a request.
    // Other components on this branch will request flow for this branch

    //  ! If this is a variable speed pump
    if (BITF_TEST_ANY(BITF(state.dataPumps->PumpEquip(PumpNum).pumpType),
                      BITF(PumpType::VarSpeed) | BITF(PumpType::Bank_VarSpeed) | BITF(PumpType::Cond))) {
        if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                .LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum)
                .Branch(state.dataPumps->PumpEquip(PumpNum).BranchNum)
                .Comp(state.dataPumps->PumpEquip(PumpNum).CompNum)
                .FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
            state.dataPumps->PumpMassFlowRate = 0.0;
        }
    }

    // bound flow request by pump max limit, the Flow Request is total loop flow and if this is a branch pump that is not appropriate
    state.dataPumps->PumpMassFlowRate = min(state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax, state.dataPumps->PumpMassFlowRate);
    state.dataPumps->PumpMassFlowRate = max(state.dataPumps->PumpEquip(PumpNum).MassFlowRateMin, state.dataPumps->PumpMassFlowRate);

    SetComponentFlowRate(state,
                         state.dataPumps->PumpMassFlowRate,
                         InletNode,
                         OutletNode,
                         state.dataPumps->PumpEquip(PumpNum).LoopNum,
                         state.dataPumps->PumpEquip(PumpNum).LoopSideNum,
                         state.dataPumps->PumpEquip(PumpNum).BranchNum,
                         state.dataPumps->PumpEquip(PumpNum).CompNum);

    // Get RPM value for reporting as output
    // RPM is calculated using pump affinity laws for rotation speed
    if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs &&
        state.dataPumps->PumpEquip(PumpNum).HasVFD) {
        RotSpeed_Min = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedIndex);
        RotSpeed_Max = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedIndex);
        if (state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM < DataBranchAirLoopPlant::MassFlowTolerance ||
            state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM < DataBranchAirLoopPlant::MassFlowTolerance) {
            state.dataPumps->PumpEquip(PumpNum).VFD.PumpActualRPM = 0.0;
        } else {
            PumpActualRPMValueOne = (state.dataPumps->PumpMassFlowRate / state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM) * RotSpeed_Max;
            PumpActualRPMValueTwo = (state.dataPumps->PumpMassFlowRate / state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM) * RotSpeed_Min;
            state.dataPumps->PumpEquip(PumpNum).VFD.PumpActualRPM = (PumpActualRPMValueOne + PumpActualRPMValueTwo) / 2;
        }
    }

    //****************************!
    //** DETERMINE IF PUMP IS ON *!
    //****************************!
    // Since we don't allow series pumping, if there is ANY flow rate for this pump, THIS PUMP is driving the flow!  Therefore...
    PumpRunning = (state.dataPumps->PumpMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance);

    //****************************!
    //** UPDATE PUMP BANK USAGE **!
    //****************************!
    {
        auto const SELECT_CASE_var(state.dataPumps->PumpEquip(PumpNum).pumpType);
        if (BITF_TEST_ANY(BITF(SELECT_CASE_var), BITF(PumpType::Bank_VarSpeed) | BITF(PumpType::Bank_ConSpeed))) {
            // previously, pumps did whatever they wanted
            // because of this a constant speed pump bank could adjust the flow rate as-desired
            //  even if it was not allowed
            // since pumps now must behave nicely like all other components, the calculation of number
            //  of running pumps in a pump bank is the same for both bank types
            // the pumps are loaded sequentially, and the last pump can have full or non-full part load
            //  status...this is just how it works now.  The pump cannot *bump* up the flow on the loop
            //  to make sure the last running pump is fully loaded anymore for constant speed pumps...sorry
            if (state.dataPumps->PumpMassFlowRate >= state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax) {
                // running full on
                state.dataPumps->NumPumpsRunning = state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank;
            } else {
                // running at some sort of part load
                state.dataPumps->NumPumpsRunning =
                    CEILING((state.dataPumps->PumpMassFlowRate / (state.dataPumps->PumpEquip(PumpNum).MassFlowRateMax) *
                             state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank));
                state.dataPumps->NumPumpsRunning = min(state.dataPumps->NumPumpsRunning, state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank);
            }
        }
    }

    //****************************!
    //***** EXIT IF NO FLOW ******!
    //****************************!
    if (state.dataPumps->PumpMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
        state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
        state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;
        state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
        return;
    }

    // density used for volumetric flow calculations
    LoopDensity = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidName,
                                   state.dataLoopNodes->Node(InletNode).Temp,
                                   state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidIndex,
                                   RoutineName);

    //****************************!
    //***** CALCULATE POWER (1) **!
    //****************************!
    switch (pumpType) {
    case (PumpType::ConSpeed):
    case (PumpType::VarSpeed):
    case (PumpType::Cond): {
        VolFlowRate = state.dataPumps->PumpMassFlowRate / LoopDensity;
        PartLoadRatio = min(1.0, (VolFlowRate / state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate));
        FracFullLoadPower = state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) * PartLoadRatio +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) * pow_2(PartLoadRatio) +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) * pow_3(PartLoadRatio);
        state.dataPumps->Power = FracFullLoadPower * state.dataPumps->PumpEquip(PumpNum).NomPowerUse;

    } break;
    case (PumpType::Bank_ConSpeed):
    case (PumpType::Bank_VarSpeed): {
        // now just assume the last one is (or is not) running at part load
        // if it is actually at full load, the calculations work out to PLR = 1
        // for the last pump, so all is OK
        state.dataPumps->NumPumpsFullLoad = state.dataPumps->NumPumpsRunning - 1;
        FullLoadVolFlowRate = state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate / state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank;
        PartLoadVolFlowRate = state.dataPumps->PumpMassFlowRate / LoopDensity - FullLoadVolFlowRate * state.dataPumps->NumPumpsFullLoad;
        FullLoadPower = state.dataPumps->PumpEquip(PumpNum).NomPowerUse / state.dataPumps->PumpEquip(PumpNum).NumPumpsInBank;
        FullLoadPowerRatio = state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) + state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) +
                             state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) + state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4);
        PartLoadRatio = min(1.0, (PartLoadVolFlowRate / FullLoadVolFlowRate));
        FracFullLoadPower = state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(1) +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(2) * PartLoadRatio +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(3) * pow_2(PartLoadRatio) +
                            state.dataPumps->PumpEquip(PumpNum).PartLoadCoef(4) * pow_3(PartLoadRatio);
        state.dataPumps->Power = (FullLoadPowerRatio * state.dataPumps->NumPumpsFullLoad + FracFullLoadPower) * FullLoadPower;
    } break;
    default: {
        ShowFatalError(state, format("Invalid Pump Type = {}", pumpType));
    } break;
    }

    //****************************!
    //***** CALCULATE POWER (2) **!
    //****************************!
    if (state.dataPumps->Power < 0.0) {
        if (state.dataPumps->PumpEquip(PumpNum).PowerErrIndex1 == 0) {
            ShowWarningMessage(state,
                               std::string{RoutineName} + " Calculated Pump Power < 0, Type=" + cPumpTypes[pumpType] + ", Name=\"" +
                                   state.dataPumps->PumpEquip(PumpNum).Name + "\".");
            ShowContinueErrorTimeStamp(state, "");
            ShowContinueError(state, format("...PartLoadRatio=[{:.4R}], Fraction Full Load Power={:.4R}]", PartLoadRatio, FracFullLoadPower));
            ShowContinueError(state, "...Power is set to 0 for continuing the simulation.");
            ShowContinueError(state, "...Pump coefficients should be checked for producing this negative value.");
        }
        state.dataPumps->Power = 0.0;
        ShowRecurringWarningErrorAtEnd(state,
                                       std::string{RoutineName} + " Calculated Pump Power < 0, " + cPumpTypes[pumpType] + ", Name=\"" +
                                           state.dataPumps->PumpEquip(PumpNum).Name + "\", PLR=",
                                       state.dataPumps->PumpEquip(PumpNum).PowerErrIndex1,
                                       PartLoadRatio,
                                       PartLoadRatio);
        ShowRecurringContinueErrorAtEnd(
            state, "...Fraction Full Load Power=", state.dataPumps->PumpEquip(PumpNum).PowerErrIndex2, FracFullLoadPower, FracFullLoadPower);
    }

    //****************************!
    //***** CALCULATE POWER (3) **!
    //****************************!
    // Now if we are doing pressure-based simulation, then we have a means to calculate power exactly based on current
    // simulation conditions (flow rate and pressure drop) along with knowledge about pump impeller and motor efficiencies
    // Thus we will override the power that was calculated based on nominal values with the corrected pressure-based power
    if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
        if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs) {
            TotalEffic = state.dataPumps->PumpEquip(PumpNum).PumpEffic * state.dataPumps->PumpEquip(PumpNum).MotorEffic;
            // Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
            if (TotalEffic == 0.0) {
                ShowSevereError(
                    state,
                    std::string{RoutineName} + " Plant pressure simulation encountered a pump with zero efficiency: " + state.dataPumps->PumpEquip(PumpNum).Name);
                ShowContinueError(state, "Check efficiency inputs for this pump component.");
                ShowFatalError(state, "Errors in plant calculation would result in divide-by-zero cause program termination.");
            }
            state.dataPumps->Power = VolFlowRate * state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureDrop / TotalEffic;
        }
    }

    // if user has specified a pressure value, then use it, same as for pressure-based simulation
    if (state.dataPumps->PumpEquip(PumpNum).EMSPressureOverrideOn) {
        TotalEffic = state.dataPumps->PumpEquip(PumpNum).PumpEffic * state.dataPumps->PumpEquip(PumpNum).MotorEffic;
        // Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
        if (TotalEffic == 0.0) {
            ShowSevereError(
                state, std::string{RoutineName} + " Plant pump simulation encountered a pump with zero efficiency: " + state.dataPumps->PumpEquip(PumpNum).Name);
            ShowContinueError(state, "Check efficiency inputs for this pump component.");
            ShowFatalError(state, "Errors in plant calculation would result in divide-by-zero cause program termination.");
        }
        state.dataPumps->Power = VolFlowRate * state.dataPumps->PumpEquip(PumpNum).EMSPressureOverrideValue / TotalEffic;
    }

    //****************************!
    //***** CALCULATE POWER (4) **!
    //****************************!
    // This adds the pump heat based on User input for the pump
    // We assume that all of the heat ends up in the fluid eventually since this is a closed loop
    state.dataPumps->ShaftPower = state.dataPumps->Power * state.dataPumps->PumpEquip(PumpNum).MotorEffic;
    state.dataPumps->PumpHeattoFluid = state.dataPumps->ShaftPower + (state.dataPumps->Power - state.dataPumps->ShaftPower) *
                                                                         state.dataPumps->PumpEquip(PumpNum).FracMotorLossToFluid;

    //****************************!
    //***** UPDATE INFORMATION ***!
    //****************************!
    // Update data structure variables
    state.dataPumps->PumpEquip(PumpNum).Power = state.dataPumps->Power;

    // Update outlet node conditions
    state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
    state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;
    state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
}

void SizePump(EnergyPlusData &state, int const PumpNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing Pump Components for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the plant sizing array.

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const StartTemp(100.0); // Standard Temperature across code to calculated Steam density
    static constexpr std::string_view RoutineName("PlantPumps::InitSimVars ");
    static constexpr std::string_view RoutineNameSizePumps("SizePumps");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlantSizNum; // index of Plant Sizing array
    bool ErrorsFound;
    Real64 TotalEffic; // pump total efficiency
    int Side;          // half loop index
    int BranchNum;     // index of branch
    int CompNum;       // index of component on branch
    Real64 PumpSizFac; // pump sizing factor
    Real64 SteamDensity;
    Real64 TempWaterDensity;
    int DummyWaterIndex(1);
    Real64 DesVolFlowRatePerBranch; // local temporary for split of branch pumps

    // Calculate density at InitConvTemp once here, to remove RhoH2O calls littered throughout
    if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
        TempWaterDensity = GetDensityGlycol(state,
                                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidName,
                                            DataGlobalConstants::InitConvTemp,
                                            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).FluidIndex,
                                            RoutineName);
    } else {
        TempWaterDensity = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, DummyWaterIndex, RoutineName);
    }

    PlantSizNum = 0;
    PumpSizFac = 1.0;
    ErrorsFound = false;

    if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
        PlantSizNum = state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PlantSizNum;
    }
    // use pump sizing factor stored in plant sizing data structure
    if (PlantSizNum > 0) {
        PumpSizFac = state.dataSize->PlantSizData(PlantSizNum).PlantSizFac;
    } else {
        // might be able to remove this next block
        if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
            for (Side = 1; Side <= 2; ++Side) {
                for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).LoopSide(Side).TotalBranches;
                     ++BranchNum) {
                    for (CompNum = 1;
                         CompNum <=
                         state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).LoopSide(Side).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        if (state.dataPumps->PumpEquip(PumpNum).InletNodeNum == state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                                                                                    .LoopSide(Side)
                                                                                    .Branch(BranchNum)
                                                                                    .Comp(CompNum)
                                                                                    .NodeNumIn &&
                            state.dataPumps->PumpEquip(PumpNum).OutletNodeNum ==
                                state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                                    .LoopSide(Side)
                                    .Branch(BranchNum)
                                    .Comp(CompNum)
                                    .NodeNumOut) {
                            if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).LoopSide(Side).Branch(BranchNum).PumpSizFac >
                                0.0) {
                                PumpSizFac = state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                                                 .LoopSide(Side)
                                                 .Branch(BranchNum)
                                                 .PumpSizFac;
                            } else {
                                PumpSizFac = 1.0;
                            }
                            goto SideLoop_exit;
                        }
                    }
                }
            }
        SideLoop_exit:;
        }
    }

    if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRateWasAutoSized) {

        if (PlantSizNum > 0) {
            if (state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                if (!state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                         .LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum)
                         .BranchPumpsExist) {
                    // size pump to full flow of plant loop
                    if (state.dataPumps->PumpEquip(PumpNum).pumpType == PumpType::Cond) {
                        TempWaterDensity = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, DummyWaterIndex, RoutineName);
                        SteamDensity = GetSatDensityRefrig(
                            state, fluidNameSteam, StartTemp, 1.0, state.dataPumps->PumpEquip(PumpNum).FluidIndex, RoutineNameSizePumps);
                        state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate =
                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * PumpSizFac;
                        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate =
                            state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate * SteamDensity / TempWaterDensity;
                    } else {
                        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * PumpSizFac;
                    }
                } else {
                    // Distribute sizes evenly across all branch pumps
                    DesVolFlowRatePerBranch = state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate /
                                              state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum)
                                                  .LoopSide(state.dataPumps->PumpEquip(PumpNum).LoopSideNum)
                                                  .TotalPumps;
                    if (state.dataPumps->PumpEquip(PumpNum).pumpType == PumpType::Cond) {
                        TempWaterDensity = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, DummyWaterIndex, RoutineName);
                        SteamDensity = GetSatDensityRefrig(
                            state, fluidNameSteam, StartTemp, 1.0, state.dataPumps->PumpEquip(PumpNum).FluidIndex, RoutineNameSizePumps);
                        state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate = DesVolFlowRatePerBranch * PumpSizFac;
                        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate =
                            state.dataPumps->PumpEquip(PumpNum).NomSteamVolFlowRate * SteamDensity / TempWaterDensity;
                    } else {
                        state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = DesVolFlowRatePerBranch * PumpSizFac;
                    }
                }

            } else {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate = 0.0;
                    ShowWarningError(state,
                                     format("SizePump: Calculated Pump Nominal Volume Flow Rate=[{:.2R}] is too small. Set to 0.0",
                                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate));
                    ShowContinueError(state, "..occurs for Pump=" + state.dataPumps->PumpEquip(PumpNum).Name);
                }
            }
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                             state.dataPumps->PumpEquip(PumpNum).Name,
                                             "Design Flow Rate [m3/s]",
                                             state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                             state.dataPumps->PumpEquip(PumpNum).Name,
                                             "Initial Design Flow Rate [m3/s]",
                                             state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate);
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                ShowSevereError(state, "Autosizing of plant loop pump flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in plant pump object=" + state.dataPumps->PumpEquip(PumpNum).Name);
                ErrorsFound = true;
            }
        }
    }

    // Note that autocalculation of power is based on nominal volume flow, regardless of whether the flow was
    //  auto sized or manually sized.  Thus, this must go after the flow sizing block above.
    if (state.dataPumps->PumpEquip(PumpNum).NomPowerUseWasAutoSized) {
        if (state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate >= SmallWaterVolFlow) {
            switch (state.dataPumps->PumpEquip(PumpNum).powerSizingMethod) {

            case PowerSizingMethod::sizePowerPerFlow: {
                TotalEffic = state.dataPumps->PumpEquip(PumpNum).NomPumpHead / state.dataPumps->PumpEquip(PumpNum).powerPerFlowScalingFactor;
                break;
            }

            case PowerSizingMethod::sizePowerPerFlowPerPressure: {
                TotalEffic =
                    (1 / state.dataPumps->PumpEquip(PumpNum).powerPerFlowPerPressureScalingFactor) * state.dataPumps->PumpEquip(PumpNum).MotorEffic;
                break;
            }
            }

            state.dataPumps->PumpEquip(PumpNum).NomPowerUse =
                (state.dataPumps->PumpEquip(PumpNum).NomPumpHead * state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate) / TotalEffic;
        } else {
            state.dataPumps->PumpEquip(PumpNum).NomPowerUse = 0.0;
        }
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state,
                                         cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                         state.dataPumps->PumpEquip(PumpNum).Name,
                                         "Design Power Consumption [W]",
                                         state.dataPumps->PumpEquip(PumpNum).NomPowerUse);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state,
                                         cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                         state.dataPumps->PumpEquip(PumpNum).Name,
                                         "Initial Design Power Consumption [W]",
                                         state.dataPumps->PumpEquip(PumpNum).NomPowerUse);
        }
    }

    if (state.dataPumps->PumpEquip(PumpNum).minVolFlowRateWasAutosized) {
        state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate =
            state.dataPumps->PumpEquip(PumpNum).NomVolFlowRate * state.dataPumps->PumpEquip(PumpNum).MinVolFlowRateFrac;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state,
                                         cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                         state.dataPumps->PumpEquip(PumpNum).Name,
                                         "Design Minimum Flow Rate [m3/s]",
                                         state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state,
                                         cPumpTypes[state.dataPumps->PumpEquip(PumpNum).pumpType],
                                         state.dataPumps->PumpEquip(PumpNum).Name,
                                         "Initial Design Minimum Flow Rate [m3/s]",
                                         state.dataPumps->PumpEquip(PumpNum).MinVolFlowRate);
        }
    }

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        PumpDataForTable(state, PumpNum);
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void ReportPumps(EnergyPlusData &state, int const PumpNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    October 1998
    //       MODIFIED         July 2001, Rick Strand (revision of pump module)
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets the pump reporting variables.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;     // pump inlet node number
    int OutletNode;    // pump outlet node number
    PumpType PumpType; // Current pump type

    PumpType = state.dataPumps->PumpEquip(PumpNum).pumpType;
    InletNode = state.dataPumps->PumpEquip(PumpNum).InletNodeNum;
    OutletNode = state.dataPumps->PumpEquip(PumpNum).OutletNodeNum;

    if (state.dataPumps->PumpMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
        state.dataPumps->PumpEquipReport(PumpNum).PumpMassFlowRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluid = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).OutletTemp = state.dataLoopNodes->Node(OutletNode).Temp;
        state.dataPumps->PumpEquip(PumpNum).Power = 0.0;
        state.dataPumps->PumpEquip(PumpNum).Energy = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ShaftPower = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluidEnergy = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainEnergy = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate = 0.0;
        state.dataPumps->PumpEquipReport(PumpNum).NumPumpsOperating = 0;
    } else {
        state.dataPumps->PumpEquipReport(PumpNum).PumpMassFlowRate = state.dataPumps->PumpMassFlowRate;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluid = state.dataPumps->PumpHeattoFluid;
        state.dataPumps->PumpEquipReport(PumpNum).OutletTemp = state.dataLoopNodes->Node(OutletNode).Temp;
        state.dataPumps->PumpEquip(PumpNum).Power = state.dataPumps->Power;
        state.dataPumps->PumpEquip(PumpNum).Energy =
            state.dataPumps->PumpEquip(PumpNum).Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPumps->PumpEquipReport(PumpNum).ShaftPower = state.dataPumps->ShaftPower;
        state.dataPumps->PumpEquipReport(PumpNum).PumpHeattoFluidEnergy =
            state.dataPumps->PumpHeattoFluid * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        switch (PumpType) {
        case (PumpType::ConSpeed):
        case (PumpType::VarSpeed):
        case (PumpType::Cond):
            state.dataPumps->PumpEquipReport(PumpNum).NumPumpsOperating = 1;
            break;

        case (PumpType::Bank_ConSpeed):
        case (PumpType::Bank_VarSpeed):
            state.dataPumps->PumpEquipReport(PumpNum).NumPumpsOperating = state.dataPumps->NumPumpsRunning;
            break;
        default:
            ShowFatalError(state, format("Invalid Pump Type = {}", PumpType));
            break;
        }
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate = state.dataPumps->Power - state.dataPumps->PumpHeattoFluid;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainEnergy =
            state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneConvGainRate =
            (1 - state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction) * state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate;
        state.dataPumps->PumpEquipReport(PumpNum).ZoneRadGainRate =
            state.dataPumps->PumpEquip(PumpNum).SkinLossRadFraction * state.dataPumps->PumpEquipReport(PumpNum).ZoneTotalGainRate;
    }
}

void PumpDataForTable(EnergyPlusData &state, int const NumPump)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Jason Glazer
    //       DATE WRITTEN:    September 2006
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Pull data together for predefined tables.

    // Using/Aliasing
    using namespace OutputReportPredefined;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string equipName;

    equipName = state.dataPumps->PumpEquip(NumPump).Name;
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpType, equipName, cPumpTypes[state.dataPumps->PumpEquip(NumPump).pumpType]);
    if (state.dataPumps->PumpEquip(NumPump).PumpControl == PumpControlType::Continuous) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpControl, equipName, "Continuous");
    } else if (state.dataPumps->PumpEquip(NumPump).PumpControl == PumpControlType::Intermittent) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpControl, equipName, "Intermittent");
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpControl, equipName, "Unknown");
    }
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpHead, equipName, state.dataPumps->PumpEquip(NumPump).NomPumpHead);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpFlow, equipName, state.dataPumps->PumpEquip(NumPump).NomVolFlowRate, 6);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpPower, equipName, state.dataPumps->PumpEquip(NumPump).NomPowerUse);
    if (state.dataPumps->PumpEquip(NumPump).NomVolFlowRate != 0) {
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchPumpPwrPerFlow,
                         equipName,
                         state.dataPumps->PumpEquip(NumPump).NomPowerUse / state.dataPumps->PumpEquip(NumPump).NomVolFlowRate);
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpPwrPerFlow, equipName, "-");
    }
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchPumpEndUse, equipName, state.dataPumps->PumpEquip(NumPump).EndUseSubcategoryName);
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchMotEff, equipName, state.dataPumps->PumpEquip(NumPump).MotorEffic);
}

void GetRequiredMassFlowRate(EnergyPlusData &state,
                             int const LoopNum,
                             int const PumpNum,
                             Real64 const InletNodeMassFlowRate,
                             Real64 &ActualFlowRate,
                             Real64 &PumpMinMassFlowRateVFDRange,
                             Real64 &PumpMaxMassFlowRateVFDRange)
{
    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    using PlantPressureSystem::ResolveLoopFlowVsPressure;
    using PlantUtilities::SetComponentFlowRate;
    using ScheduleManager::GetCurrentScheduleValue;

    Real64 PumpMassFlowRateMaxPress(0.0); // Maximum mass flow rate associated with maximum pressure limit
    Real64 PumpMassFlowRateMinPress(0.0); // Minimum mass flow rate associated with minimum pressure limit
    Real64 RotSpeed_Max(0.0);             // Maximum rotational speed in rps
    Real64 RotSpeed_Min(0.0);             // Minimum rotational speed in rps
    Real64 MinPress(0.0);                 // Minimum pressure
    Real64 MaxPress(0.0);                 // Maximum pressure

    RotSpeed_Min = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.MinRPMSchedIndex);
    RotSpeed_Max = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.MaxRPMSchedIndex);
    MinPress = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.LowerPsetSchedIndex);
    MaxPress = GetCurrentScheduleValue(state, state.dataPumps->PumpEquip(PumpNum).VFD.UpperPsetSchedIndex);

    // Calculate maximum and minimum mass flow rate associated with maximun and minimum RPM
    if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
        if (state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).UsePressureForPumpCalcs &&
            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureSimType == DataPlant::iPressSimType::FlowCorrection &&
            state.dataPlnt->PlantLoop(state.dataPumps->PumpEquip(PumpNum).LoopNum).PressureDrop > 0.0) {
            state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM =
                ResolveLoopFlowVsPressure(state,
                                          state.dataPumps->PumpEquip(PumpNum).LoopNum,
                                          InletNodeMassFlowRate,
                                          state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index,
                                          RotSpeed_Max,
                                          state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter,
                                          state.dataPumps->PumpEquip(PumpNum).MinPhiValue,
                                          state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);
            state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM =
                ResolveLoopFlowVsPressure(state,
                                          state.dataPumps->PumpEquip(PumpNum).LoopNum,
                                          InletNodeMassFlowRate,
                                          state.dataPumps->PumpEquip(PumpNum).PressureCurve_Index,
                                          RotSpeed_Min,
                                          state.dataPumps->PumpEquip(PumpNum).ImpellerDiameter,
                                          state.dataPumps->PumpEquip(PumpNum).MinPhiValue,
                                          state.dataPumps->PumpEquip(PumpNum).MaxPhiValue);
        }
    }

    // Not correct necessarily, but values are coming out way wrong here, maxRPMmdot~3, minRPMmdot~62!
    if (state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM < state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM) {
        state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM = state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM;
    }

    // Calculate maximum and minimum mass flow rate associated with operating pressure range
    if (state.dataPumps->PumpEquip(PumpNum).LoopNum > 0) {
        if (state.dataPlnt->PlantLoop(LoopNum).PressureEffectiveK > 0.0) {
            PumpMassFlowRateMaxPress = std::sqrt(MaxPress / state.dataPlnt->PlantLoop(LoopNum).PressureEffectiveK);
            PumpMassFlowRateMinPress = std::sqrt(MinPress / state.dataPlnt->PlantLoop(LoopNum).PressureEffectiveK);
        }
    }

    // Decide operating range for mass flow rate
    // Maximum mass flow rate value of the range
    if (state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM > PumpMassFlowRateMaxPress) {
        // Maximum pressure value governs maximum VFD range value
        PumpMaxMassFlowRateVFDRange = PumpMassFlowRateMaxPress;
    } else {
        // Maximum RPM value governs maximum VFD range value
        PumpMaxMassFlowRateVFDRange = state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMaxRPM;
    }

    // Minimum mass flow rate value of the range
    if (state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM > PumpMassFlowRateMinPress) {
        // Minimum pressure value governs minimum VFD range value
        PumpMinMassFlowRateVFDRange = state.dataPumps->PumpEquip(PumpNum).PumpMassFlowRateMinRPM;
    } else {
        // Minimum pressure range value governs minimum VFD range value
        PumpMinMassFlowRateVFDRange = PumpMassFlowRateMinPress;
    }

    // Set the mass flow rate within VFD operating range
    if (InletNodeMassFlowRate > PumpMinMassFlowRateVFDRange) {
        if (InletNodeMassFlowRate < PumpMaxMassFlowRateVFDRange) {
            // Flow request is within VFD operating range
            ActualFlowRate = InletNodeMassFlowRate;
        } else {
            // Flow request is outside VFD operating range
            // Flow is set to maximum VFD operating range
            ActualFlowRate = PumpMaxMassFlowRateVFDRange;
        }
    } else {
        // Flow request is outside VFD operating range
        // Flow is set to minimum VFD operating Range
        ActualFlowRate = PumpMinMassFlowRateVFDRange;
    }
}

} // namespace EnergyPlus::Pumps
