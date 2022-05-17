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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/numeric.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/RootFinder.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus::HVACControllers {
// Module containing the controller simulation routines for the air loop

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen
//       DATE WRITTEN   July 1998
//       MODIFIED       Feb 2006, Dimitri Curtil (LBNL)
//                      - Added tracing mechanism for debugging convergence process.
//                        - Trace operation of each individual controller in a file named
//                          'controller.<Controller Name>.csv'
//                        - Trace operation of all controllers per air loop in a file named
//                          'controller.<Air Loop Name>.csv'
//                      - Added operations to enable cold start/speculative warm restart
//                        and final check.
//       MODIFIED       March 2006, Dimitri Curtil (LBNL)
//                      - Added mechanism to track runtime performance statistics.
//                      - Added routine to dump controller statistics to a file named
//                        'statistics.HVACControllers.csv'
//                      - Integrated smart root finder from MODULE RootFinder implemented in
//                        file RootFinder.cc.
//       MODIFIED       April 2006, Dimitri Curtil (LBNL)
//                      - Added speedup optimization scheme to reuse air loop solution
//                        obtained at the current HVAC iteration from solving the previous controller
//                        on the loop (see ReuseIntermediateSolutionFlag). Of course this works only
//                        if there are 2 or more controllers on the same air loop.
//                      - Added speedup optimization scheme to reuse solution obtained
//                        at the previous HVAC iteration for this controller during the
//                        bracketing phase (see ReusePreviousSolutionFlag).
//       MODIFIED       May 2006, Dimitri Curtil (LBNL)
//                      - Added mechanism to monitor min/max bounds to ensure that they remain invariant
//                        between successive controller iterations.
//                      - Modified setpoint calculation to force the setpoint to be computed only once.
//                      - Modified setpoint calculation for TEMPandHUMRAT control strategy to
//                        force the setpoint to be computed once the air loop has been evaluated with
//                        the max actuated value.
//       MODIFIED       June 2006, Dimitri Curtil (LBNL)
//                      - Renamed parameter variables so as to use lower caps.
//                      - Replaced $ edit descriptor in WRITE statements with ADVANCE='No'
//                      - Replaced the preprocessing directives TRACK_AIRLOOP, TRACE_AIRLOOP,
//                        TRACE_CONTROLLER with corresponding environment variables defined
//                        in DataSystemVariables.cc.
//       MODIFIED       Feb. 2010, Brent Griffith (NREL)
//                       - changed plant loop interactions, Demand Side Update Phase 3
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the Controller System Component.

// METHODOLOGY EMPLOYED:
// The main entry point if the SUBROUTINE ManageControllers().
// 1. For proper operation, the subroutine must first be called with either the
//    iControllerOpColdStart or iControllerOpWarmRestart operation code to initialize
//    the various controllers.
// 2. Then the actuated variable for each controller is computed iteratively using
//    root finding techniques that aim at forcing the sensed variable to be
//    "equal" (within the user-specified tolerance) to the desired setpoint.
//    This step is achieved by calling ManageController() with the iControllerOpIterate
//    operation code.
// 3. Finally, after all controllers have been successfully simulated,  the subroutine has
//    to be called one last time with the iControllerOpEnd operation code to ensure that
//    the sequential solution indeed represents a valid global solution across all controllers
//    simultaneously.
// The following pseudo-code shows the typical calling sequence for the SUBROUTINE
// ManageControllers :
// - for each controller on air loop
//   - CALL ManageControllers( Operation=<iControllerOpColdStart or iControllerOpWarmRestart> )
// - simulate air loop components with the initial values for all actuated variables
// - for each controller on air loop
//   - CALL ManageControllers( Operation=iControllerOpIterate, IsConvergedFlag )
//   - if NOT IsConvergedFlag then
//     - exit loop with error if too many iterations performed
//     - simulate air loop components with the new candidate value for the actuated variable of
//       the current controller
// - simulate air loop components with the final values for all actuated variables
// - for each controller on air loop
//   - CALL ManageControllers( Operation=iControllerOpEnd, IsConvergedFlag )
//   - if NOT IsConvergedFlag then
//     - exit loop with error indicating no "global" convergence with final solution.
// Check the subroutines SolveAirLoopControllers() and ReSolveAirLoopControllers()
// invoked in the subroutine SimAirLoop() for the actual calling sequences.

// OTHER NOTES:
// To enable runtime statistics tracking for each air loop, define the environment variable
// TRACK_AIRLOOP=YES or TRACK_AIRLOOP=Y.
// To enable generating a trace file with the converged solution for all controllers on each air loop,
// define the environment variable TRACE_AIRLOOP=YES or TRACE_AIRLOOP=Y.
// To enable generating an individual, detailed trace file for each controller, define the
// environment variable TRACE_CONTROLLER=YES or TRACE_CONTROLLER=Y.
// See DataSystemVariables.cc for the definitions of the environment variables used to debug
// the air loop simulation.

// USE STATEMENTS:
// Use statements for data only modules
// Using/Aliasing
using namespace DataLoopNode;
using DataHVACGlobals::SmallWaterVolFlow;
using namespace DataHVACControllers;
using namespace DataRootFinder;

// Number of significant digits to display in error messages for floating-point numbers
constexpr int NumSigDigits = 15;
constexpr std::array<std::string_view, static_cast<int>(CtrlVarType::Num)> ctrlVarNamesUC = {
    "INVALID-NONE", "TEMPERATURE", "HUMIDITYRATIO", "TEMPERATUREANDHUMIDITYRATIO", "INVALID-FLOW"};
constexpr std::array<std::string_view, static_cast<int>(ControllerAction::Num)> actionNamesUC = {"", "REVERSE", "NORMAL"};

std::string ControlVariableTypes(CtrlVarType const &c)
{
    switch (c) {
    case CtrlVarType::NoControlVariable:
        return "No control variable";
    case CtrlVarType::Temperature:
        return "Temperature";
    case CtrlVarType::HumidityRatio:
        return "Humidity ratio";
    case CtrlVarType::TemperatureAndHumidityRatio:
        return "Temperature and humidity ratio";
    case CtrlVarType::Flow:
        return "Flow rate";
    default:
        assert(false);
    }
    return "no controller type found";
}

void ManageControllers(EnergyPlusData &state,
                       std::string const &ControllerName,
                       int &ControllerIndex,
                       bool const FirstHVACIteration,
                       int const AirLoopNum,
                       ControllerOperation const Operation,
                       bool &IsConvergedFlag,
                       bool &IsUpToDateFlag,
                       bool &BypassOAController,
                       Optional_bool AllowWarmRestartFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   July 1998
    //       MODIFIED       Dimitri Curtil, February 2006
    //                      - Added air loop information
    //                      - Added tracing to csv files
    //                      - Added primitive operations to replace mixed
    //                        bag of ResetController, FirstCallConvergenceTest, ...
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages Controller component simulation.

    // Using/Aliasing
    using namespace DataSystemVariables;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC time step
    // Current air loop num 1...NumPrimaryAirSys
    // Current pass counter in SimAirLoop()
    // Operation to execute
    // TRUE if controller is converged
    // TRUE if air loop is up-to-date meaning that the current node values are consistent (air loop evaluated)
    // Only used within the Calc routines
    // TRUE if speculative warm restart is supported by this controller

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // The Controller that you are currently loading input into
    int ControlNum;
    int ControllerType;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &NumControllers(state.dataHVACControllers->NumControllers);
    auto &CheckEquipName(state.dataHVACControllers->CheckEquipName);
    auto &RootFinders(state.dataHVACControllers->RootFinders);

    // Obtains and Allocates Controller related parameters from input file
    if (state.dataHVACControllers->GetControllerInputFlag) { // First time subroutine has been entered
        GetControllerInput(state);
        state.dataHVACControllers->GetControllerInputFlag = false;
    }

    if (ControllerIndex == 0) {
        ControlNum = UtilityRoutines::FindItemInList(ControllerName, ControllerProps, &ControllerPropsType::ControllerName);
        if (ControlNum == 0) {
            ShowFatalError(state,
                           "ManageControllers: Invalid controller=" + ControllerName +
                               ". The only valid controller type for an AirLoopHVAC is Controller:WaterCoil.");
        }
        ControllerIndex = ControlNum;
    } else {
        ControlNum = ControllerIndex;
        if (ControlNum > NumControllers || ControlNum < 1) {
            ShowFatalError(state,
                           format("ManageControllers: Invalid ControllerIndex passed={}, Number of controllers={}, Controller name={}",
                                  ControlNum,
                                  NumControllers,
                                  ControllerName));
        }
        if (CheckEquipName(ControlNum)) {
            if (ControllerName != ControllerProps(ControlNum).ControllerName) {
                ShowFatalError(
                    state,
                    format("ManageControllers: Invalid ControllerIndex passed={}, Controller name={}, stored Controller Name for that index={}",
                           ControlNum,
                           ControllerName,
                           ControllerProps(ControlNum).ControllerName));
            }
            CheckEquipName(ControlNum) = false;
        }
    }

    if (ControllerProps(ControlNum).BypassControllerCalc && BypassOAController) {
        IsUpToDateFlag = true;
        IsConvergedFlag = true;
        if (present(AllowWarmRestartFlag)) AllowWarmRestartFlag = true;
        return;
    }

    // Find the correct ControllerNumber with the AirLoop & CompNum from AirLoop Derived Type
    // ControlNum = AirLoopEquip(AirLoopNum)%ComponentOfTypeNum(CompNum)

    // detect if plant is locked and flow cannot change
    if (ControllerProps(ControlNum).ActuatedNodePlantLoc.loopNum > 0) {

        if (state.dataPlnt->PlantLoop(ControllerProps(ControlNum).ActuatedNodePlantLoc.loopNum)
                .LoopSide(ControllerProps(ControlNum).ActuatedNodePlantLoc.loopSideNum)
                .FlowLock == DataPlant::FlowLock::Locked) {
            // plant is rigid so controller cannot change anything.
            // Update the current Controller to the outlet nodes
            UpdateController(state, ControlNum);

            IsConvergedFlag = true;
            return;
        }
    }

    // Detect if speculative warm restart is supported by this computer
    if (present(AllowWarmRestartFlag)) {
        // NOTE: Never allow speculative warm restart with dual humidity ratio and temperature control
        //       because the actual setpoint depends on the current temperature and max hum ratio at
        //       the sensed node, and therefore might not be known until after one air loop simulation.
        if (ControllerProps(ControlNum).ControlVar == CtrlVarType::TemperatureAndHumidityRatio) {
            AllowWarmRestartFlag = false;
        } else {
            AllowWarmRestartFlag = true;
        }
    }

    if (ControllerProps(ControlNum).InitFirstPass) {
        // Coil must first be sized to:
        // Initialize ControllerProps(ControlNum)%MinActuated and ControllerProps(ControlNum)%MaxActuated
        InitController(state, ControlNum, IsConvergedFlag);
        ControllerProps(ControlNum).InitFirstPass = false;
    }

    // Perform requested operation
    // Note that InitController() is not called upon START/RESTART ops in order to avoid
    // side-effects on the calculation of Node(ActuatedNode)%MassFlowRateMaxAvail used to
    // determine ControllerProps(ControlNum)%MaxAvailActuated.
    // Plant upgrades for V7 added init to these cases because MassFlowRateMaxAvail is better controlled
    switch (Operation) {
    case ControllerOperation::ColdStart: {
        // For temperature and humidity control reset humidity control override if it was set
        if (ControllerProps(ControlNum).HumRatCtrlOverride) {
            ControllerProps(ControlNum).HumRatCtrlOverride = false;
            // Put the controller tolerance (offset) back to it's original value
            RootFinder::SetupRootFinder(state,
                                        RootFinders(ControlNum),
                                        DataRootFinder::Slope::Decreasing,
                                        DataRootFinder::RootFinderMethod::Brent,
                                        DataPrecisionGlobals::constant_zero,
                                        1.0e-6,
                                        ControllerProps(ControlNum).Offset);
        }

        // If a iControllerOpColdStart call, reset the actuator inlet flows
        ResetController(state, ControlNum, false, IsConvergedFlag);
        // Update the current Controller to the outlet nodes
        UpdateController(state, ControlNum);
    } break;
    case ControllerOperation::WarmRestart: {
        // If a iControllerOpWarmRestart call, set the actuator inlet flows to previous solution
        ResetController(state, ControlNum, true, IsConvergedFlag);
        // Update the current Controller to the outlet nodes
        UpdateController(state, ControlNum);
    } break;
    case ControllerOperation::Iterate: {
        // With the correct ControlNum Initialize all Controller related parameters
        InitController(state, ControlNum, IsConvergedFlag);

        // No initialization needed: should have been done before
        // Simulate the correct Controller with the current ControlNum
        ControllerType = ControllerProps(ControlNum).ControllerType_Num;

        if (ControllerType == ControllerSimple_Type) { // 'Controller:WaterCoil'
            CalcSimpleController(state, ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName);
        } else {
            ShowFatalError(state, "Invalid controller type in ManageControllers=" + ControllerProps(ControlNum).ControllerType);
        }

        // Update the current Controller to the outlet nodes
        UpdateController(state, ControlNum);

        CheckTempAndHumRatCtrl(state, ControlNum, IsConvergedFlag);

    } break;
    case ControllerOperation::End: {
        // With the correct ControlNum Initialize all Controller related parameters
        InitController(state, ControlNum, IsConvergedFlag);

        // No initialization needed: should have been done before
        // Check convergence for the correct Controller with the current ControlNum
        ControllerType = ControllerProps(ControlNum).ControllerType_Num;

        if (ControllerType == ControllerSimple_Type) { // 'Controller:WaterCoil'
            CheckSimpleController(state, ControlNum, IsConvergedFlag);
            SaveSimpleController(state, ControlNum, FirstHVACIteration, IsConvergedFlag);
        } else {
            ShowFatalError(state, "Invalid controller type in ManageControllers=" + ControllerProps(ControlNum).ControllerType);
        }

    } break;
    default: {
        ShowFatalError(state, format("ManageControllers: Invalid Operation passed={}, Controller name={}", Operation, ControllerName));
    } break;
    }

    // Write detailed diagnostic for individual controller
    // To enable generating an individual, detailed trace file for each controller on each air loop,
    // define the environment variable TRACE_CONTROLLER=YES or TRACE_CONTROLLER=Y
    if (state.dataSysVars->TraceHVACControllerEnvFlag) {
        TraceIndividualController(
            state, ControlNum, FirstHVACIteration, state.dataAirLoop->AirLoopControlInfo(AirLoopNum).AirLoopPass, Operation, IsConvergedFlag);
    }
}

// Get Input Section of the Module
//******************************************************************************

void GetControllerInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   July 1998
    //       MODIFIED       February 2006, Dimitri Curtil
    //                      - Added processing for air loop controller stats
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main routine to call other input routines and Get routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // REFERENCES:
    // Gets the object:
    // Controller:WaterCoil,
    //   \min-fields 9
    //   A1 , \field Name
    //        \type alpha
    //        \required-field
    //        \reference AirLoopControllers
    //   A2 , \field Control Variable
    //        \type choice
    //        \key Temperature
    //        \key HumidityRatio
    //        \key TemperatureAndHumidityRatio
    //        \key Flow
    //        \note TemperatureAndHumidityRatio requires a SetpointManager:SingleZone:Humidity:Maximum object
    //   A3 , \field Action
    //        \type choice
    //        \key Normal
    //        \key Reverse
    //   A4 , \field Actuator Variable
    //        \type choice
    //        \key Flow
    //   A5 , \field Sensor Node Name
    //        \type alpha
    //   A6 , \field Actuator Node Name
    //        \type alpha
    //   N1 , \field Controller Convergence Tolerance
    //        \units deltaC
    //        \type real
    //        \default AutoSize
    //        \autosizable
    //   N2 , \field Maximum Actuated Flow
    //        \type real
    //        \units m3/s
    //        \autosizable
    //   N3 ; \field Minimum Actuated Flow
    //        \type real
    //        \default 0.0000001
    //        \units m3/s

    // Using/Aliasing
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    using EMSManager::CheckIfNodeSetPointManagedByEMS;
    using MixedAir::CheckForControllerWaterCoil;
    using NodeInputManager::GetOnlySingleNode;
    using SetPointManager::CtrlVarType;
    using SetPointManager::NodeHasSPMCtrlVarType;
    using SetPointManager::ResetHumidityRatioCtrlVarType;
    using WaterCoils::CheckActuatorNode;
    using WaterCoils::CheckForSensorAndSetPointNode;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("HVACControllers: GetControllerInput: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Num; // The Controller that you are currently loading input into
    int NumSimpleControllers;
    int NumAlphas;
    int NumNums;
    int NumArgs;
    int IOStat;
    int AirLoopNum;            // DO index for each air loop
    bool ActuatorNodeNotFound; // true if no water coil inlet node match for actuator node
    Array1D<Real64> NumArray;
    Array1D_string AlphArray;
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    std::string CurrentModuleObject; // for ease in getting objects
    bool ErrorsFound(false);
    bool NodeNotFound;         // flag true if the sensor node is on the coil air outlet node
    bool EMSSetPointErrorFlag; // flag true is EMS is used to set node setpoints

    auto &NumControllers(state.dataHVACControllers->NumControllers);
    auto &NumAirLoopStats(state.dataHVACControllers->NumAirLoopStats);
    auto &AirLoopStats(state.dataHVACControllers->AirLoopStats);
    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);
    auto &CheckEquipName(state.dataHVACControllers->CheckEquipName);

    // All the controllers are loaded into the same derived type, both the PI and Limit
    // These controllers are separate objects and loaded sequentially, but will
    // be retrieved by name as they are needed.

    CurrentModuleObject = "Controller:WaterCoil";
    NumSimpleControllers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    NumControllers = NumSimpleControllers;

    // Allocate stats data structure for each air loop and controller if needed
    if (state.dataSysVars->TrackAirLoopEnvFlag || state.dataSysVars->TraceAirLoopEnvFlag || state.dataSysVars->TraceHVACControllerEnvFlag) {
        if (NumPrimaryAirSys > 0) {
            NumAirLoopStats = NumPrimaryAirSys;
            AirLoopStats.allocate(NumAirLoopStats);

            // Allocate controller statistics data for each controller on each air loop
            for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
                AirLoopStats(AirLoopNum).ControllerStats.allocate(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers);
            }
        }
    }

    if (NumControllers == 0) return;
    // Condition of no controllers will be taken care of elsewhere, if necessary

    ControllerProps.allocate(NumControllers);
    RootFinders.allocate(NumControllers);
    CheckEquipName.dimension(NumControllers, true);

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumArgs, NumAlphas, NumNums);
    AlphArray.allocate(NumAlphas);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNums);
    NumArray.dimension(NumNums, 0.0);
    lAlphaBlanks.dimension(NumAlphas, true);
    lNumericBlanks.dimension(NumNums, true);

    // Now find and load all of the simple controllers.
    if (NumSimpleControllers > 0) {
        for (Num = 1; Num <= NumSimpleControllers; ++Num) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Num,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

            ControllerProps(Num).ControllerName = AlphArray(1);
            ControllerProps(Num).ControllerType = CurrentModuleObject;

            ControllerProps(Num).ControlVar =
                static_cast<EnergyPlus::HVACControllers::CtrlVarType>(getEnumerationValue(ctrlVarNamesUC, AlphArray(2)));
            if (ControllerProps(Num).ControlVar == HVACControllers::CtrlVarType::Invalid) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ShowSevereError(state,
                                "...Invalid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                    "\", must be Temperature, HumidityRatio, or TemperatureAndHumidityRatio.");
                ErrorsFound = true;
            }

            ControllerProps(Num).Action = static_cast<ControllerAction>(getEnumerationValue(actionNamesUC, AlphArray(3)));
            if (ControllerProps(Num).Action == ControllerAction::Invalid) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ShowSevereError(state, "...Invalid " + cAlphaFields(3) + "=\"" + AlphArray(3) + R"(", must be "Normal", "Reverse" or blank.)");
                ErrorsFound = true;
            }

            if (AlphArray(4) == "FLOW") {
                ControllerProps(Num).ActuatorVar = HVACControllers::CtrlVarType::Flow;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ShowContinueError(state, "...Invalid " + cAlphaFields(4) + "=\"" + AlphArray(4) + "\", only FLOW is allowed.");
                ErrorsFound = true;
            }
            ControllerProps(Num).SensedNode = GetOnlySingleNode(state,
                                                                AlphArray(5),
                                                                ErrorsFound,
                                                                DataLoopNode::ConnectionObjectType::ControllerWaterCoil,
                                                                AlphArray(1),
                                                                DataLoopNode::NodeFluidType::Blank,
                                                                DataLoopNode::ConnectionType::Sensor,
                                                                NodeInputManager::CompFluidStream::Primary,
                                                                ObjectIsNotParent);
            ControllerProps(Num).ActuatedNode = GetOnlySingleNode(state,
                                                                  AlphArray(6),
                                                                  ErrorsFound,
                                                                  DataLoopNode::ConnectionObjectType::ControllerWaterCoil,
                                                                  AlphArray(1),
                                                                  DataLoopNode::NodeFluidType::Blank,
                                                                  DataLoopNode::ConnectionType::Actuator,
                                                                  NodeInputManager::CompFluidStream::Primary,
                                                                  ObjectIsNotParent);
            ControllerProps(Num).Offset = NumArray(1);
            ControllerProps(Num).MaxVolFlowActuated = NumArray(2);
            ControllerProps(Num).MinVolFlowActuated = NumArray(3);

            if (!CheckForControllerWaterCoil(state, CurrentModuleObject, AlphArray(1))) {
                ShowSevereError(
                    state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\" not found on any AirLoopHVAC:ControllerList.");
                ErrorsFound = true;
            }

            if (ControllerProps(Num).SensedNode > 0) {

                if (ControllerProps(Num).ControlVar == HVACControllers::CtrlVarType::HumidityRatio ||
                    ControllerProps(Num).ControlVar == HVACControllers::CtrlVarType::TemperatureAndHumidityRatio) {
                    ResetHumidityRatioCtrlVarType(state, ControllerProps(Num).SensedNode);
                }
                CheckForSensorAndSetPointNode(state, ControllerProps(Num).SensedNode, ControllerProps(Num).ControlVar, NodeNotFound);

                if (NodeNotFound) {
                    // the sensor node is not on the water coil air outlet node
                    ShowWarningError(
                        state, std::string{RoutineName} + ControllerProps(Num).ControllerType + "=\"" + ControllerProps(Num).ControllerName + "\". ");
                    ShowContinueError(state, " ..Sensor node not found on water coil air outlet node.");
                    ShowContinueError(state,
                                      " ..The sensor node may have been placed on a node downstream of the coil or on an airloop outlet node.");
                } else {
                    // check if the setpoint is also on the same node where the sensor is placed on
                    EMSSetPointErrorFlag = false;
                    switch (ControllerProps(Num).ControlVar) {
                    case HVACControllers::CtrlVarType::Temperature: {
                        CheckIfNodeSetPointManagedByEMS(
                            state, ControllerProps(Num).SensedNode, EMSManager::SPControlType::TemperatureSetPoint, EMSSetPointErrorFlag);
                        state.dataLoopNodes->NodeSetpointCheck(ControllerProps(Num).SensedNode).needsSetpointChecking = false;
                        if (EMSSetPointErrorFlag) {
                            if (!NodeHasSPMCtrlVarType(state, ControllerProps(Num).SensedNode, CtrlVarType::Temp)) {
                                ShowContinueError(state, " ..Temperature setpoint not found on coil air outlet node.");
                                ShowContinueError(
                                    state, " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                                ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                            }
                        }
                    } break;
                    case HVACControllers::CtrlVarType::HumidityRatio: {
                        CheckIfNodeSetPointManagedByEMS(
                            state, ControllerProps(Num).SensedNode, EMSManager::SPControlType::HumidityRatioMaxSetPoint, EMSSetPointErrorFlag);
                        state.dataLoopNodes->NodeSetpointCheck(ControllerProps(Num).SensedNode).needsSetpointChecking = false;
                        if (EMSSetPointErrorFlag) {
                            if (!NodeHasSPMCtrlVarType(state, ControllerProps(Num).SensedNode, CtrlVarType::MaxHumRat)) {
                                ShowContinueError(state, " ..Humidity ratio setpoint not found on coil air outlet node.");
                                ShowContinueError(
                                    state, " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                                ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                            }
                        }
                    } break;
                    case HVACControllers::CtrlVarType::TemperatureAndHumidityRatio: {
                        CheckIfNodeSetPointManagedByEMS(
                            state, ControllerProps(Num).SensedNode, EMSManager::SPControlType::TemperatureSetPoint, EMSSetPointErrorFlag);
                        state.dataLoopNodes->NodeSetpointCheck(ControllerProps(Num).SensedNode).needsSetpointChecking = false;
                        if (EMSSetPointErrorFlag) {
                            if (!NodeHasSPMCtrlVarType(state, ControllerProps(Num).SensedNode, CtrlVarType::Temp)) {
                                ShowContinueError(state, " ..Temperature setpoint not found on coil air outlet node.");
                                ShowContinueError(
                                    state, " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                                ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                            }
                        }
                        EMSSetPointErrorFlag = false;
                        CheckIfNodeSetPointManagedByEMS(
                            state, ControllerProps(Num).SensedNode, EMSManager::SPControlType::HumidityRatioMaxSetPoint, EMSSetPointErrorFlag);
                        state.dataLoopNodes->NodeSetpointCheck(ControllerProps(Num).SensedNode).needsSetpointChecking = false;
                        if (EMSSetPointErrorFlag) {
                            if (!NodeHasSPMCtrlVarType(state, ControllerProps(Num).SensedNode, CtrlVarType::MaxHumRat)) {
                                ShowContinueError(state, " ..Humidity ratio setpoint not found on coil air outlet node.");
                                ShowContinueError(
                                    state, " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                                ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                            }
                        }
                    } break;
                    default:
                        break;
                    }
                }
            }
        }
    }

    // check that actuator nodes are matched by a water coil inlet node
    for (Num = 1; Num <= NumSimpleControllers; ++Num) {
        DataPlant::PlantEquipmentType WaterCoilType{};
        CheckActuatorNode(state, ControllerProps(Num).ActuatedNode, WaterCoilType, ActuatorNodeNotFound);
        if (ActuatorNodeNotFound) {
            ErrorsFound = true;
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + ControllerProps(Num).ControllerName + "\":");
            ShowContinueError(state, "...the actuator node must also be a water inlet node of a water coil");
        } else { // Node found, check type and action
            if (WaterCoilType == DataPlant::PlantEquipmentType::CoilWaterCooling) {
                if (ControllerProps(Num).Action == ControllerAction::NoAction) {
                    ControllerProps(Num).Action = ControllerAction::Reverse;
                } else if (ControllerProps(Num).Action == ControllerAction::NormalAction) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + ControllerProps(Num).ControllerName + "\":");
                    ShowContinueError(state, "...Normal action has been specified for a cooling coil - should be Reverse.");
                    ShowContinueError(state, "...overriding user input action with Reverse Action.");
                    ControllerProps(Num).Action = ControllerAction::Reverse;
                }
            } else if (WaterCoilType == DataPlant::PlantEquipmentType::CoilWaterSimpleHeating) {
                if (ControllerProps(Num).Action == ControllerAction::NoAction) {
                    ControllerProps(Num).Action = ControllerAction::NormalAction;
                } else if (ControllerProps(Num).Action == ControllerAction::Reverse) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + ControllerProps(Num).ControllerName + "\":");
                    ShowContinueError(state, "...Reverse action has been specified for a heating coil - should be Normal.");
                    ShowContinueError(state, "...overriding user input action with Normal Action.");
                    ControllerProps(Num).Action = ControllerAction::NormalAction;
                }
            }
        }
    }

    AlphArray.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    NumArray.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    // CR 8253 check that the sensed nodes in the controllers are in flow order in controller List
    CheckControllerListOrder(state);

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting " + CurrentModuleObject + " input.");
    }
}

// End of Get Input subroutines for the Module
//******************************************************************************

// Beginning Initialization Section of the Module
//******************************************************************************

void ResetController(EnergyPlusData &state, int const ControlNum, bool const DoWarmRestartFlag, bool &IsConvergedFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 2004
    //       MODIFIED       Dimitri Curtil (LBNL), Feb 2006
    //                      - Added capability for speculative warm restart
    //                      Brent Griffith (NREL), Feb 2010
    //                      - use SetActuatedBranchFlowRate in Plant Utilities (honor hardware min > 0.0)
    //                      - add FirstHVACIteration logic, don't reset if false,
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine resets the actuator inlet flows.

    // Using/Aliasing
    using PlantUtilities::SetActuatedBranchFlowRate;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;
    Real64 NoFlowResetValue;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);

    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    NoFlowResetValue = 0.0;
    SetActuatedBranchFlowRate(
        state, NoFlowResetValue, ControllerProps(ControlNum).ActuatedNode, ControllerProps(ControlNum).ActuatedNodePlantLoc, true);

    //  ENDIF

    // Reset iteration counter and internal variables
    ControllerProps(ControlNum).NumCalcCalls = 0;

    ControllerProps(ControlNum).DeltaSensed = 0.0;
    ControllerProps(ControlNum).SensedValue = 0.0;
    ControllerProps(ControlNum).ActuatedValue = 0.0;

    // Reset setpoint-related quantities
    ControllerProps(ControlNum).SetPointValue = 0.0;
    ControllerProps(ControlNum).IsSetPointDefinedFlag = false;

    // MinAvailActuated and MaxAvailActuated set in InitController()
    ControllerProps(ControlNum).MinAvailActuated = 0.0;
    ControllerProps(ControlNum).MinAvailSensed = 0.0;
    ControllerProps(ControlNum).MaxAvailActuated = 0.0;
    ControllerProps(ControlNum).MaxAvailSensed = 0.0;

    // Restart from previous solution if speculative warm restart flag set
    // Keep same mode and next actuated value unchanged from last controller simulation.
    if (DoWarmRestartFlag) {
        ControllerProps(ControlNum).DoWarmRestartFlag = true;
    } else {
        ControllerProps(ControlNum).DoWarmRestartFlag = false;
        // If no speculative warm restart then reset stored mode and actucated value
        ControllerProps(ControlNum).Mode = ControllerMode::None;
        ControllerProps(ControlNum).NextActuatedValue = 0.0;
    }

    // Only set once per HVAC iteration.
    // Might be overwritten in the InitController() routine.
    // Allow reusing the previous solution while identifying brackets if
    // this is not the first HVAC step of the environment
    ControllerProps(ControlNum).ReusePreviousSolutionFlag = true;
    // Always reset to false by default. Set in CalcSimpleController() on the first controller iteration.
    ControllerProps(ControlNum).ReuseIntermediateSolutionFlag = false;
    // By default not converged
    IsConvergedFlag = false;

    // Reset root finder
    // This is independent of the processing in InitializeRootFinder() performed in Calc() routine.
    RootFinders(ControlNum).StatusFlag = RootFinderStatus::None;
    RootFinders(ControlNum).CurrentMethodType = DataRootFinder::RootFinderMethod::None;

    RootFinders(ControlNum).CurrentPoint.DefinedFlag = false;
    RootFinders(ControlNum).CurrentPoint.X = 0.0;
    RootFinders(ControlNum).CurrentPoint.Y = 0.0;

    RootFinders(ControlNum).MinPoint.DefinedFlag = false;
    RootFinders(ControlNum).MaxPoint.DefinedFlag = false;
    RootFinders(ControlNum).LowerPoint.DefinedFlag = false;
    RootFinders(ControlNum).UpperPoint.DefinedFlag = false;
}

void InitController(EnergyPlusData &state, int const ControlNum, bool &IsConvergedFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   July 1998
    //       MODIFIED       Jan. 2004, Shirey/Raustad (FSEC),
    //       MODIFIED       Feb. 2006, Dimitri Curtil (LBNL), Moved first call convergence test code to ResetController()
    //                      Jul. 2016, R. Zhang (LBNL), Applied the water coil supply air temperature sensor offset fault model

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for  initializations of the Controller Components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;
    using EMSManager::CheckIfNodeSetPointManagedByEMS;
    using FluidProperties::GetDensityGlycol;
    using PlantUtilities::ScanPlantLoopsForNodeNum;
    using PlantUtilities::SetActuatedBranchFlowRate;
    using RootFinder::SetupRootFinder;
    using SetPointManager::CtrlVarType;
    using SetPointManager::GetHumidityRatioVariableType;

    static constexpr std::string_view RoutineName("InitController");

    int ActuatedNode;
    int SensedNode;
    int ControllerIndex;
    Real64 rho; // local fluid density

    auto &NumControllers(state.dataHVACControllers->NumControllers);
    auto &InitControllerOneTimeFlag(state.dataHVACControllers->InitControllerOneTimeFlag);
    auto &InitControllerSetPointCheckFlag(state.dataHVACControllers->InitControllerSetPointCheckFlag);
    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);
    auto &MyEnvrnFlag(state.dataHVACControllers->MyEnvrnFlag);
    auto &MySizeFlag(state.dataHVACControllers->MySizeFlag);
    auto &MyPlantIndexsFlag(state.dataHVACControllers->MyPlantIndexsFlag);

    if (InitControllerOneTimeFlag) {

        MyEnvrnFlag.allocate(NumControllers);
        MySizeFlag.allocate(NumControllers);
        MyPlantIndexsFlag.allocate(NumControllers);
        MyEnvrnFlag = true;
        MySizeFlag = true;
        MyPlantIndexsFlag = true;
        InitControllerOneTimeFlag = false;
    }

    if (!state.dataGlobal->SysSizingCalc && InitControllerSetPointCheckFlag && DoSetPointTest) {
        // check for missing setpoints
        for (ControllerIndex = 1; ControllerIndex <= NumControllers; ++ControllerIndex) {
            SensedNode = ControllerProps(ControllerIndex).SensedNode;
            switch (ControllerProps(ControllerIndex).ControlVar) {
            case HVACControllers::CtrlVarType::Temperature: { // 'Temperature'
                if (state.dataLoopNodes->Node(SensedNode).TempSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(
                            state,
                            "HVACControllers: Missing temperature setpoint for controller type=" + ControllerProps(ControllerIndex).ControllerType +
                                " Name=\"" + ControllerProps(ControllerIndex).ControllerName + "\"");
                        ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the "
                                          "controller sensed node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // call to check node is actuated by EMS
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::TemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "HVACControllers: Missing temperature setpoint for controller type=" +
                                                ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                                ControllerProps(ControllerIndex).ControllerName + "\"");
                            ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                            ShowContinueError(state,
                                              "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at "
                                              "the controller sensed node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                        }
                    }
                } else {
                    //           Warn if humidity setpoint is detected (only for cooling coils) and control variable is TEMP.
                    if (state.dataLoopNodes->Node(SensedNode).HumRatMax != SensedNodeFlagValue &&
                        ControllerProps(ControllerIndex).Action == ControllerAction::Reverse) {
                        ShowWarningError(state,
                                         "HVACControllers: controller type=" + ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                             ControllerProps(ControllerIndex).ControllerName +
                                             "\" has detected a maximum humidity ratio setpoint at the control node.");
                        ShowContinueError(state, "Node referenced (by controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  set the controller control variable to TemperatureAndHumidityRatio if humidity control is desired.");
                        //              SetPointErrorFlag = .TRUE.
                    }
                }
            } break;
            case HVACControllers::CtrlVarType::HumidityRatio: { // 'HumidityRatio'
                ControllerProps(ControllerIndex).HumRatCntrlType = GetHumidityRatioVariableType(state, SensedNode);
                if ((ControllerProps(ControlNum).HumRatCntrlType == CtrlVarType::HumRat &&
                     state.dataLoopNodes->Node(SensedNode).HumRatSetPoint == SensedNodeFlagValue) ||
                    (ControllerProps(ControlNum).HumRatCntrlType == CtrlVarType::MaxHumRat &&
                     state.dataLoopNodes->Node(SensedNode).HumRatMax == SensedNodeFlagValue)) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state,
                                        "HVACControllers: Missing humidity ratio setpoint for controller type=" +
                                            ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                            ControllerProps(ControllerIndex).ControllerName + "\"");
                        ShowContinueError(state, "Node referenced (by controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  use a SetpointManager with the field Control Variable = \"MaximumHumidityRatio\" to establish a "
                                          "setpoint at the controller sensed node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::HumidityRatioSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "HVACControllers: Missing humidity ratio setpoint for controller type=" +
                                                ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                                ControllerProps(ControllerIndex).ControllerName + "\"");
                            ShowContinueError(state, "Node referenced (by controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                            ShowContinueError(state,
                                              "  use a SetpointManager with the field Control Variable = \"MaximumHumidityRatio\" to "
                                              "establish a setpoint at the controller sensed node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide Humidity Ratio setpoint at this node");
                        }
                    }

                } else if (ControllerProps(ControlNum).HumRatCntrlType == CtrlVarType::MinHumRat) {
                    ShowSevereError(
                        state,
                        "HVACControllers: incorrect humidity ratio setpoint for controller type=" + ControllerProps(ControllerIndex).ControllerType +
                            " Name=\"" + ControllerProps(ControllerIndex).ControllerName + "\"");
                    ShowContinueError(state, "Node referenced (by controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                    ShowContinueError(state,
                                      "  use a SetpointManager with the field Control Variable = \"MaximumHumidityRatio\" to establish a "
                                      "setpoint at the controller sensed node.");
                    state.dataHVACGlobal->SetPointErrorFlag = true;
                }
            } break;
            case HVACControllers::CtrlVarType::TemperatureAndHumidityRatio: { // 'TemperatureAndHumidityRatio'
                if (state.dataLoopNodes->Node(SensedNode).TempSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(
                            state,
                            "HVACControllers: Missing temperature setpoint for controller type=" + ControllerProps(ControllerIndex).ControllerType +
                                " Name=\"" + ControllerProps(ControllerIndex).ControllerName + "\"");
                        ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the "
                                          "controller sensed node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // call to check node is actuated by EMS
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::TemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "HVACControllers: Missing temperature setpoint for controller type=" +
                                                ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                                ControllerProps(ControllerIndex).ControllerName + "\"");
                            ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                            ShowContinueError(state,
                                              "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at "
                                              "the controller sensed node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                        }
                    }
                }
                if (state.dataLoopNodes->Node(SensedNode).HumRatMax == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state,
                                        "HVACControllers: Missing maximum humidity ratio setpoint for controller type=" +
                                            ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                            ControllerProps(ControllerIndex).ControllerName + "\"");
                        ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  use a SetpointManager with the field Control Variable = \"MaximumHumidityRatio\" to establish a "
                                          "setpoint at the controller sensed node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // call to check node is actuated by EMS
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::HumidityRatioMaxSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "HVACControllers: Missing maximum humidity ratio setpoint for controller type=" +
                                                ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                                ControllerProps(ControllerIndex).ControllerName + "\"");
                            ShowContinueError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                            ShowContinueError(state,
                                              "  use a SetpointManager with the field Control Variable = \"MaximumHumidityRatio\" to "
                                              "establish a setpoint at the controller sensed node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide maximum Humidity Ratio setpoint at this node");
                        }
                    }
                }
            } break;
            case HVACControllers::CtrlVarType::Flow: { // 'Flow'
                if (state.dataLoopNodes->Node(SensedNode).MassFlowRateSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state,
                                        "HVACControllers: Missing mass flow rate setpoint for controller type=" +
                                            ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                            ControllerProps(ControllerIndex).ControllerName + "\"");
                        ShowContinueError(state, "Node Referenced (in Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                        ShowContinueError(state,
                                          "  use a SetpointManager with the field Control Variable = \"MassFlowRate\" to establish a "
                                          "setpoint at the controller sensed node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // call to check node is actuated by EMS
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::MassFlowRateSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "HVACControllers: Missing mass flow rate setpoint for controller type=" +
                                                ControllerProps(ControllerIndex).ControllerType + " Name=\"" +
                                                ControllerProps(ControllerIndex).ControllerName + "\"");
                            ShowContinueError(state, "Node Referenced (in Controller)=" + state.dataLoopNodes->NodeID(SensedNode));
                            ShowContinueError(state,
                                              "  use a SetpointManager with the field Control Variable = \"MassFlowRate\" to establish a "
                                              "setpoint at the controller sensed node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide Mass Flow Rate setpoint at this node");
                        }
                    }
                }
            } break;
            default:
                break;
            }
        }

        InitControllerSetPointCheckFlag = false;
    }

    if (allocated(state.dataPlnt->PlantLoop) && MyPlantIndexsFlag(ControlNum)) {
        ScanPlantLoopsForNodeNum(state,
                                 ControllerProps(ControlNum).ControllerName,
                                 ControllerProps(ControlNum).ActuatedNode,
                                 ControllerProps(ControlNum).ActuatedNodePlantLoc);
        MyPlantIndexsFlag(ControlNum) = false;
    }

    if (!state.dataGlobal->SysSizingCalc && MySizeFlag(ControlNum)) {

        SizeController(state, ControlNum);

        // Check to make sure that the Minimum Flow rate is less than the max.
        if (ControllerProps(ControlNum).MaxVolFlowActuated == 0.0) {
            ShowWarningError(state,
                             std::string{RoutineName} + ": Controller:WaterCoil=\"" + ControllerProps(ControlNum).ControllerName +
                                 "\", Maximum Actuated Flow is zero.");
            ControllerProps(ControlNum).MinVolFlowActuated = 0.0;
        } else if (ControllerProps(ControlNum).MinVolFlowActuated >= ControllerProps(ControlNum).MaxVolFlowActuated) {
            ShowFatalError(state,
                           std::string{RoutineName} + ": Controller:WaterCoil=\"" + ControllerProps(ControlNum).ControllerName +
                               "\", Minimum control flow is > or = Maximum control flow.");
        }

        // Setup root finder after sizing calculation
        switch (ControllerProps(ControlNum).Action) {
        case ControllerAction::NormalAction: {
            SetupRootFinder(state,
                            RootFinders(ControlNum),
                            DataRootFinder::Slope::Increasing,
                            DataRootFinder::RootFinderMethod::Brent,
                            DataPrecisionGlobals::constant_zero,
                            1.0e-6,
                            ControllerProps(ControlNum).Offset); // Slope type | Method type | TolX: no relative tolerance for X variables |
                                                                 // ATolX: absolute tolerance for X variables | ATolY: absolute tolerance for
                                                                 // Y variables

        } break;
        case ControllerAction::Reverse: {
            SetupRootFinder(state,
                            RootFinders(ControlNum),
                            DataRootFinder::Slope::Decreasing,
                            DataRootFinder::RootFinderMethod::Brent,
                            DataPrecisionGlobals::constant_zero,
                            1.0e-6,
                            ControllerProps(ControlNum).Offset); // Slope type | Method type | TolX: no relative tolerance for X variables |
                                                                 // ATolX: absolute tolerance for X variables | ATolY: absolute tolerance for
                                                                 // Y variables
        } break;
        default: {
            ShowFatalError(state, R"(InitController: Invalid controller action. Valid choices are "Normal" or "Reverse")");
        } break;
        }

        MySizeFlag(ControlNum) = false;
    }

    // Set the sensed and actuated node numbers
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(ControlNum)) {

        rho = GetDensityGlycol(state,
                               state.dataPlnt->PlantLoop(ControllerProps(ControlNum).ActuatedNodePlantLoc.loopNum).FluidName,
                               DataGlobalConstants::CWInitConvTemp,
                               state.dataPlnt->PlantLoop(ControllerProps(ControlNum).ActuatedNodePlantLoc.loopNum).FluidIndex,
                               RoutineName);

        ControllerProps(ControlNum).MinActuated = rho * ControllerProps(ControlNum).MinVolFlowActuated;
        ControllerProps(ControlNum).MaxActuated = rho * ControllerProps(ControlNum).MaxVolFlowActuated;

        // Turn off scheme to reuse previous solution obtained at last SimAirLoop() call
        ControllerProps(ControlNum).ReusePreviousSolutionFlag = false;
        // Reset solution trackers
        for (auto &e : ControllerProps(ControlNum).SolutionTrackers) {
            e.DefinedFlag = false;
            e.Mode = ControllerMode::None;
            e.ActuatedValue = 0.0;
        }

        MyEnvrnFlag(ControlNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        MyEnvrnFlag(ControlNum) = true;
    }

    SetActuatedBranchFlowRate(
        state, ControllerProps(ControlNum).NextActuatedValue, ActuatedNode, ControllerProps(ControlNum).ActuatedNodePlantLoc, false);

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.
    // Load the node data in this section for the component simulation
    IsConvergedFlag = false;

    switch (ControllerProps(ControlNum).ControlVar) {
    case HVACControllers::CtrlVarType::Temperature: { // 'Temperature'
        ControllerProps(ControlNum).SensedValue = state.dataLoopNodes->Node(SensedNode).Temp;
        // Done once per HVAC step
        if (!ControllerProps(ControlNum).IsSetPointDefinedFlag) {
            ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).TempSetPoint;
            ControllerProps(ControlNum).IsSetPointDefinedFlag = true;

            // If there is a fault of water coil SAT sensor
            if (ControllerProps(ControlNum).FaultyCoilSATFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
                (!state.dataGlobal->KickOffSimulation)) {
                // calculate the sensor offset using fault information
                int FaultIndex = ControllerProps(ControlNum).FaultyCoilSATIndex;
                ControllerProps(ControlNum).FaultyCoilSATOffset = state.dataFaultsMgr->FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct(state);
                // update the SetPointValue
                ControllerProps(ControlNum).SetPointValue =
                    state.dataLoopNodes->Node(SensedNode).TempSetPoint - ControllerProps(ControlNum).FaultyCoilSATOffset;
            }
        }
    } break;
    case HVACControllers::CtrlVarType::TemperatureAndHumidityRatio: { // 'TemperatureAndHumidityRatio'
        if (ControllerProps(ControlNum).HumRatCtrlOverride) {
            // Humidity ratio control
            ControllerProps(ControlNum).SensedValue = state.dataLoopNodes->Node(SensedNode).HumRat;
        } else {
            // Temperature control
            ControllerProps(ControlNum).SensedValue = state.dataLoopNodes->Node(SensedNode).Temp;
        }
        if (!ControllerProps(ControlNum).IsSetPointDefinedFlag) {
            if (ControllerProps(ControlNum).HumRatCtrlOverride) {
                // Humidity ratio control
                ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).HumRatMax;
            } else {
                // Pure temperature setpoint control strategy
                ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).TempSetPoint;
            }
            // Finally indicate thate the setpoint has been computed
            ControllerProps(ControlNum).IsSetPointDefinedFlag = true;
        }
    } break;
    case HVACControllers::CtrlVarType::HumidityRatio: { // 'HumidityRatio'
        ControllerProps(ControlNum).SensedValue = state.dataLoopNodes->Node(SensedNode).HumRat;
        // Done once per HVAC step
        if (!ControllerProps(ControlNum).IsSetPointDefinedFlag) {
            switch (ControllerProps(ControlNum).HumRatCntrlType) {
            case CtrlVarType::MaxHumRat: {
                ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).HumRatMax;
            } break;
            default: {
                ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).HumRatSetPoint;
            } break;
            }
            ControllerProps(ControlNum).IsSetPointDefinedFlag = true;
        }
    } break;
    case HVACControllers::CtrlVarType::Flow: { // 'Flow'
        ControllerProps(ControlNum).SensedValue = state.dataLoopNodes->Node(SensedNode).MassFlowRate;
        // Done once per HVAC step
        if (!ControllerProps(ControlNum).IsSetPointDefinedFlag) {
            ControllerProps(ControlNum).SetPointValue = state.dataLoopNodes->Node(SensedNode).MassFlowRateSetPoint;
            ControllerProps(ControlNum).IsSetPointDefinedFlag = true;
        }
    } break;
    default: {
        ShowFatalError(state, "Invalid Controller Variable Type=" + ControlVariableTypes(ControllerProps(ControlNum).ControlVar));
    } break;
    }

    switch (ControllerProps(ControlNum).ActuatorVar) {
    case HVACControllers::CtrlVarType::Flow: { // 'Flow'
        // At the beginning of every time step the value is reset to the User Input
        // The interface managers can reset the Max or Min to available values during the time step
        // and these will then be the new setpoint limits for the controller to work within.
        ControllerProps(ControlNum).ActuatedValue = state.dataLoopNodes->Node(ActuatedNode).MassFlowRate;
        // Compute the currently available min and max bounds for controller.
        // Done only once per HVAC step, as it would not make any sense to modify the min/max
        // bounds during successive iterations of the root finder.
        if (ControllerProps(ControlNum).NumCalcCalls == 0) {
            ControllerProps(ControlNum).MinAvailActuated =
                max(state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail, ControllerProps(ControlNum).MinActuated);
            ControllerProps(ControlNum).MaxAvailActuated =
                min(state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail, ControllerProps(ControlNum).MaxActuated);
            // MinActuated is user input for minimum actuated flow, use that value if allowed
            // (i.e., reset MinAvailActuated based on Node%MassFlowRateMaxAvail)
            ControllerProps(ControlNum).MinAvailActuated =
                min(ControllerProps(ControlNum).MinAvailActuated, ControllerProps(ControlNum).MaxAvailActuated);
        }
    } break;
    default: {
        ShowFatalError(state, "Invalid Actuator Variable Type=" + ControlVariableTypes(ControllerProps(ControlNum).ActuatorVar));
    } break;
    }

    // Compute residual for control function using desired setpoint value and current sensed value
    // NOTE: The delta sensed value might be wrong if the setpoint has not yet been computed.
    //       Make sure not to use it until the setpoint has been computed.
    if (ControllerProps(ControlNum).IsSetPointDefinedFlag) {
        ControllerProps(ControlNum).DeltaSensed = ControllerProps(ControlNum).SensedValue - ControllerProps(ControlNum).SetPointValue;
    } else {
        ControllerProps(ControlNum).DeltaSensed = 0.0;
    }
}

void SizeController(EnergyPlusData &state, int const ControlNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   November 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing Controller Components for which max flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the actuated node. Should have been set by the water coils.

    // Using/Aliasing
    using namespace DataSizing;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode; // node number of actuated node
    int WaterCompNum;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;

    if (ControllerProps(ControlNum).MaxVolFlowActuated == AutoSize) {
        for (WaterCompNum = 1; WaterCompNum <= state.dataSize->SaveNumPlantComps; ++WaterCompNum) {
            if (state.dataSize->CompDesWaterFlow(WaterCompNum).SupNode == ActuatedNode) {
                ControllerProps(ControlNum).MaxVolFlowActuated = state.dataSize->CompDesWaterFlow(WaterCompNum).DesVolFlowRate;
            }
        }

        if (ControllerProps(ControlNum).MaxVolFlowActuated < SmallWaterVolFlow) {
            ControllerProps(ControlNum).MaxVolFlowActuated = 0.0;
        }
        BaseSizer::reportSizerOutput(state,
                                     ControllerProps(ControlNum).ControllerType,
                                     ControllerProps(ControlNum).ControllerName,
                                     "Maximum Actuated Flow [m3/s]",
                                     ControllerProps(ControlNum).MaxVolFlowActuated);
    }

    if (ControllerProps(ControlNum).Offset == AutoSize) {
        // 2100 = 0.5 * 4.2 * 1000/1.2 * 1.2 where 0.5 is the ratio of chilled water delta T to supply air delta T,
        //   4.2 is the ratio of water density to air density, 1000/1.2 is the ratio of water specific heat to
        //   air specific heat, and 1.2 converts the result from air volumetric flow rate to air mass flow rate.
        //   The assumption is that a temperature tolerance of 0.001 C is good for an air mass flow rate of 1 kg/s.
        //   So we divide .001 by the air mass flow rate estimated from the water volumetric flow rate to come up
        //   with a temperature tolerance that won't exceed the loop energy error tolerance (10 W).
        // Finally we need to take into account the fact that somebody might change the energy tolerance.
        ControllerProps(ControlNum).Offset =
            (0.001 / (2100.0 * max(ControllerProps(ControlNum).MaxVolFlowActuated, SmallWaterVolFlow))) * (DataConvergParams::HVACEnergyToler / 10.0);
        // do not let the controller tolerance exceed 1/10 of the loop temperature tolerance.
        ControllerProps(ControlNum).Offset = min(0.1 * DataConvergParams::HVACTemperatureToler, ControllerProps(ControlNum).Offset);
        BaseSizer::reportSizerOutput(state,
                                     ControllerProps(ControlNum).ControllerType,
                                     ControllerProps(ControlNum).ControllerName,
                                     "Controller Convergence Tolerance",
                                     ControllerProps(ControlNum).Offset);
    }
}

void CalcSimpleController(EnergyPlusData &state,
                          int const ControlNum,
                          bool const FirstHVACIteration,
                          bool &IsConvergedFlag,
                          bool &IsUpToDateFlag,
                          std::string const &ControllerName // used when errors occur
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   May 2006
    //       MODIFIED       Dimitri Curtil (LBNL), May 2006
    //                      - Added IsPointFlagDefinedFlag to control when the setpoiont should be
    //                        computed depending on the control strategy. This was needed to
    //                        trigger the setpoint calculation for the dual temperature and
    //                        humidity ratio control strategy only once the air loop has been
    //                        evaluated with the max actuated flow.
    //                        See the routine InitController() for more details on the setpoint
    //                        calculation.
    //       MODIFIED       Dimitri Curtil (LBNL), March 2006
    //                      - Added IsUpToDateFlag to detect whether or not the air loop
    //                        has been evaluated prior the first iteration, which allows
    //                        to use the current node values as the first iterate for the root
    //                        finder (for COLD RESTART ONLY).
    //       MODIFIED       Dimitri Curtil (LBNL), Feb 2006
    //                      - Added mode detection capability.
    //                      - Now trying min actuated variable first to
    //                        detect min-constrained cases in 1 iteration.
    //                      - Trying max actuated variable second.
    //                        Checks for max-constrained here instead of in
    //                        NormActuatedCalc mode.
    //                      - Checking for inactive mode as soon as min and max
    //                        support points are known instead of in NormActuatedCalc
    //                        mode.
    //       RE-ENGINEERED  na

    using RootFinder::CheckRootFinderCandidate;
    using RootFinder::InitializeRootFinder;

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // Set to TRUE if current controller is converged; FALSE if more iteration are needed.
    // Note that an error in the root finding process can be mapped onto IsConvergedFlag=TRUE
    // to avoid continue iterating.
    // TRUE if air loop is up-to-date meaning that the current node values are consistent (air loop evaluated)
    // Only used within the Calc routines

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);

    // Increment counter
    ++ControllerProps(ControlNum).NumCalcCalls;

    // Obtain actuated and sensed nodes
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Check to see if the component is running; if not converged and return.  This check will be done
    // by looking at the component mass flow rate at the sensed node.
    if (state.dataLoopNodes->Node(SensedNode).MassFlowRate == 0.0) {
        ExitCalcController(state, ControlNum, DataPrecisionGlobals::constant_zero, ControllerMode::Off, IsConvergedFlag, IsUpToDateFlag);
        return;
    }

    // Initialize root finder
    if (ControllerProps(ControlNum).NumCalcCalls == 1) {
        // Set min/max boundaries for root finder on first iteration
        InitializeRootFinder(state,
                             RootFinders(ControlNum),
                             ControllerProps(ControlNum).MinAvailActuated,
                             ControllerProps(ControlNum).MaxAvailActuated); // XMin | XMax

        // Only allow to reuse initial evaluation if the air loop is up-to-date.
        // Set in SolveAirLoopControllers()
        // Only reuse initial evaluation if setpoint is already available for the current controller
        // Note that in the case of dual temperature and humidity ratio control strategy since the
        // setpoint at a later iteration, the initial solution cannot be reused.
        // Make sure that the initial candidate value lies within range
        ControllerProps(ControlNum).ReuseIntermediateSolutionFlag =
            IsUpToDateFlag && ControllerProps(ControlNum).IsSetPointDefinedFlag &&
            CheckRootFinderCandidate(RootFinders(ControlNum), ControllerProps(ControlNum).ActuatedValue);

        if (ControllerProps(ControlNum).ReuseIntermediateSolutionFlag) {

            // Reuse intermediate solution obtained with previous controller for the current HVAC step
            // and fire root finder to get next root candidate
            FindRootSimpleController(state, ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName);

        } else {
            // Always start with min point by default
            ControllerProps(ControlNum).NextActuatedValue = RootFinders(ControlNum).MinPoint.X;
        }

        // Process current iterate and compute next candidate if needed
        // We assume that after the first controller iteration:
        // - the setpoint is defined
        // - the min and max available bounds are defined
        // NOTE: Not explicitly checked but the air mass flow rate must remain constant across successive
        //       controller iterations to ensure that the root finder converges.
    } else {
        // Check that the setpoint is defined
        if (!ControllerProps(ControlNum).IsSetPointDefinedFlag) {
            ShowSevereError(state, "CalcSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
            ShowContinueError(state, " Controller name=\"" + ControllerName + "\"");
            ShowContinueError(state, " Setpoint is not available/defined.");
            ShowFatalError(state, "Preceding error causes program termination.");
        }
        // Monitor invariants across successive controller iterations
        // - min bound
        // - max bound
        if (RootFinders(ControlNum).MinPoint.X != ControllerProps(ControlNum).MinAvailActuated) {
            ShowSevereError(state, "CalcSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
            ShowContinueError(state, " Controller name=\"" + ControllerName + "\"");
            ShowContinueError(state, " Minimum bound must remain invariant during successive iterations.");
            ShowContinueError(state, format(" Minimum root finder point={:.{}T}", RootFinders(ControlNum).MinPoint.X, NumSigDigits));
            ShowContinueError(state, format(" Minimum avail actuated={:.{}T}", ControllerProps(ControlNum).MinAvailActuated, NumSigDigits));
            ShowFatalError(state, "Preceding error causes program termination.");
        }
        if (RootFinders(ControlNum).MaxPoint.X != ControllerProps(ControlNum).MaxAvailActuated) {
            ShowSevereError(state, "CalcSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
            ShowContinueError(state, " Controller name=\"" + ControllerName + "\"");
            ShowContinueError(state, " Maximum bound must remain invariant during successive iterations.");
            ShowContinueError(state, format(" Maximum root finder point={:.{}T}", RootFinders(ControlNum).MaxPoint.X, NumSigDigits));
            ShowContinueError(state, format(" Maximum avail actuated={:.{}T}", ControllerProps(ControlNum).MaxAvailActuated, NumSigDigits));
            ShowFatalError(state, "Preceding error causes program termination.");
        }

        // Updates root finder with current iterate and computes next one if needed
        FindRootSimpleController(state, ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName);
    }
}

void FindRootSimpleController(EnergyPlusData &state,
                              int const ControlNum,
                              bool const FirstHVACIteration,
                              bool &IsConvergedFlag,
                              bool &IsUpToDateFlag,
                              std::string const &ControllerName // used when errors occur
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil (LBNL)
    //       DATE WRITTEN   March 2006
    //       MODIFIED       na
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // New routine to fire the root finder using the current actuated and sensed values.
    // - Updates IsConvergedFlag depending ou iteration status.
    // - Sets next actuated value to try in ControllerProps(ControlNum)%NextActuatedValue

    using RootFinder::CheckRootFinderCandidate;
    using RootFinder::IterateRootFinder;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;
    bool IsDoneFlag; // TRUE if root finder needs to continue iterating, FALSE otherwise.
    bool ReusePreviousSolutionFlag;
    int PreviousSolutionIndex;
    bool PreviousSolutionDefinedFlag;
    ControllerMode PreviousSolutionMode;
    Real64 PreviousSolutionValue;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);

    // Obtain actuated and sensed nodes
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Update root finder with latest solution point
    // Check for unconstrained/constrained convergence
    // Compute next candidate if not converged yet.
    IterateRootFinder(state,
                      RootFinders(ControlNum),
                      ControllerProps(ControlNum).ActuatedValue,
                      ControllerProps(ControlNum).DeltaSensed,
                      IsDoneFlag); // root finder's data | X | Y | not used

    // Process root finder if converged or error
    // Map root finder status onto controller mode
    switch (RootFinders(ControlNum).StatusFlag) {
    case RootFinderStatus::None:
    case RootFinderStatus::WarningNonMonotonic:
    case RootFinderStatus::WarningSingular: {
        // We need to keep iterating...
        IsConvergedFlag = false;

        if (FirstHVACIteration) {
            PreviousSolutionIndex = 1;
        } else {
            PreviousSolutionIndex = 2;
        }

        PreviousSolutionDefinedFlag = ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).DefinedFlag;
        PreviousSolutionMode = ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).Mode;
        PreviousSolutionValue = ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).ActuatedValue;

        // Attempt to use root at previous HVAC step in place of the candidate produced by the
        // root finder.
        // Set in InitController() depending on controller mode at previous HVAC step iteration
        // Only attempted during bracketing phase of root finder.
        // Check that a previous solution is available
        // Make sure that mode of previous solution was active
        // Make sure that proposed candidate does not conflict with current min/max range and lower/upper brackets
        ReusePreviousSolutionFlag = ControllerProps(ControlNum).ReusePreviousSolutionFlag &&
                                    (RootFinders(ControlNum).CurrentMethodType == DataRootFinder::RootFinderMethod::Bracket) &&
                                    PreviousSolutionDefinedFlag && (PreviousSolutionMode == ControllerMode::Active) &&
                                    CheckRootFinderCandidate(RootFinders(ControlNum), PreviousSolutionValue);

        if (ReusePreviousSolutionFlag) {
            // Try to reuse saved solution from previous call to SolveAirLoopControllers()
            // instead of candidate proposed by the root finder
            ControllerProps(ControlNum).NextActuatedValue = PreviousSolutionValue;

            // Turn off flag since we can only use the previous solution once per HVAC iteration
            ControllerProps(ControlNum).ReusePreviousSolutionFlag = false;
        } else {
            // By default, use candidate value computed by root finder
            ControllerProps(ControlNum).NextActuatedValue = RootFinders(ControlNum).XCandidate;
        }

    } break;
    case RootFinderStatus::OK:
    case RootFinderStatus::OKRoundOff: {
        // Indicate convergence with base value (used to obtain DeltaSensed!)
        ExitCalcController(state, ControlNum, RootFinders(ControlNum).XCandidate, ControllerMode::Active, IsConvergedFlag, IsUpToDateFlag);
    } break;
    case RootFinderStatus::OKMin: {
        // Indicate convergence with min value
        // Should be the same as ControllerProps(ControlNum)%MinAvailActuated
        ExitCalcController(state, ControlNum, RootFinders(ControlNum).MinPoint.X, ControllerMode::MinActive, IsConvergedFlag, IsUpToDateFlag);
    } break;
    case RootFinderStatus::OKMax: {
        // Indicate convergence with max value
        // Should be the same as ControllerProps(ControlNum)%MaxAvailActuated
        ExitCalcController(state, ControlNum, RootFinders(ControlNum).MaxPoint.X, ControllerMode::MaxActive, IsConvergedFlag, IsUpToDateFlag);

    } break;
    case RootFinderStatus::ErrorSingular: {
        // Indicate inactive mode with min actuated value
        // NOTE: Original code returned Node(ActuatedNode)%MassFlowRateMinAvail
        //       This was not portable in case the actuated variable was NOT a mass flow rate!
        //       Replaced   Node(ActuatedNode)%MassFlowRateMinAvail
        //       with       RootFinders(ControlNum)%MinPoint%X
        //       which is the same as (see SUBROUTINE InitController)
        //                  ControllerProps(ControlNum)%MinAvailActuated
        ExitCalcController(state, ControlNum, RootFinders(ControlNum).MinPoint.X, ControllerMode::Inactive, IsConvergedFlag, IsUpToDateFlag);

        // Abnormal case: should never happen
    } break;
    case RootFinderStatus::ErrorRange: {
        ShowSevereError(state, "FindRootSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
        ShowContinueError(state, " Controller name=\"" + ControllerName + "\"");
        ShowContinueError(
            state,
            format(" Root candidate x={:.{}T} does not lie within the min/max bounds.", ControllerProps(ControlNum).ActuatedValue, NumSigDigits));
        ShowContinueError(state, format(" Min bound is x={:.{}T}", RootFinders(ControlNum).MinPoint.X, NumSigDigits));
        ShowContinueError(state, format(" Max bound is x={:.{}T}", RootFinders(ControlNum).MaxPoint.X, NumSigDigits));
        ShowFatalError(state, "Preceding error causes program termination.");

        // Abnormal case: should never happen
    } break;
    case RootFinderStatus::ErrorBracket: {
        ShowSevereError(state, "FindRootSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
        ShowContinueError(state, " Controller name=" + ControllerProps(ControlNum).ControllerName);
        ShowContinueError(
            state, fmt::format(" Controller action={}", state.dataHVACCtrl->ActionTypes[static_cast<int>(ControllerProps(ControlNum).Action)]));
        ShowContinueError(state,
                          format(" Root candidate x={:.{}T} does not lie within the lower/upper brackets.",
                                 ControllerProps(ControlNum).ActuatedValue,
                                 NumSigDigits));
        if (RootFinders(ControlNum).LowerPoint.DefinedFlag) {
            ShowContinueError(state, format(" Lower bracket is x={:.{}T}", RootFinders(ControlNum).LowerPoint.X, NumSigDigits));
        }
        if (RootFinders(ControlNum).UpperPoint.DefinedFlag) {
            ShowContinueError(state, format(" Upper bracket is x={:.{}T}", RootFinders(ControlNum).UpperPoint.X, NumSigDigits));
        }
        ShowFatalError(state, "Preceding error causes program termination.");

        // Detected control function with wrong action between the min and max points.
        // Should never happen: probably indicative of some serious problems in IDFs
        // NOTE: This approach is more robust and consistent than what was done in version 1.3.
        //       Indeed, such a function with the wrong action characteristic would have silently returned
        //       either of the following values depending on the specified action:
        //       - NORMAL ACTION:
        //         - If y(xMin) > ySetPoint && y(xMax) < y(xMin), then  x = xMin
        //         - If y(xMin) < ySetPoint && y(xMax) < y(xMin), then  x = xMax
        //       - REVERSE ACTION:
        //         - If y(xMin) < ySetPoint && y(xMax) > y(xMin), then  x = xMin
        //         - If y(xMin) > ySetPoint && y(xMax) > y(xMin), then  x = xMax
    } break;
    case RootFinderStatus::ErrorSlope: {
        if (!state.dataGlobal->WarmupFlag && ControllerProps(ControlNum).BadActionErrCount == 0) {
            ++ControllerProps(ControlNum).BadActionErrCount;
            ShowSevereError(state, "FindRootSimpleController: Controller error for controller = \"" + ControllerName + "\"");
            ShowContinueErrorTimeStamp(state, "");
            ShowContinueError(state,
                              fmt::format("  Controller function is inconsistent with user specified controller action = {}",
                                          state.dataHVACCtrl->ActionTypes[static_cast<int>(ControllerProps(ControlNum).Action)]));
            ShowContinueError(state, "  Actuator will be set to maximum action");
            ShowContinueError(state, "Controller control type=" + ControlVariableTypes(ControllerProps(ControlNum).ControlVar));
            if (ControllerProps(ControlNum).ControlVar == CtrlVarType::Temperature) {
                ShowContinueError(state, format("Controller temperature setpoint = {:.2T} [C]", ControllerProps(ControlNum).SetPointValue));
                ShowContinueError(state, format("Controller sensed temperature = {:.2T} [C]", ControllerProps(ControlNum).SensedValue));
            } else if (ControllerProps(ControlNum).ControlVar == CtrlVarType::HumidityRatio) {
                ShowContinueError(
                    state, format("Controller humidity ratio setpoint = {:.2T} [kgWater/kgDryAir]", ControllerProps(ControlNum).SetPointValue));
                ShowContinueError(state,
                                  format("Controller sensed humidity ratio = {:.2T} [kgWater/kgDryAir]", ControllerProps(ControlNum).SensedValue));
            } else if (ControllerProps(ControlNum).ControlVar == CtrlVarType::TemperatureAndHumidityRatio) {
                ShowContinueError(state, format("Controller temperature setpoint = {:.2T} [C]", ControllerProps(ControlNum).SetPointValue));
                ShowContinueError(state, format("Controller sensed temperature = {:.2T} [C]", ControllerProps(ControlNum).SensedValue));
                ShowContinueError(state,
                                  format("Controller humidity ratio setpoint = {:.2T} [kgWater/kgDryAir]",
                                         state.dataLoopNodes->Node(ControllerProps(ControlNum).SensedNode).HumRatMax));
                ShowContinueError(state,
                                  format("Controller sensed humidity ratio = {:.2T} [kgWater/kgDryAir]",
                                         state.dataLoopNodes->Node(ControllerProps(ControlNum).SensedNode).HumRat));
            } else if (ControllerProps(ControlNum).ControlVar == CtrlVarType::Flow) {
                ShowContinueError(state, format("Controller mass flow rate setpoint = {:.2T} [kg/s]", ControllerProps(ControlNum).SetPointValue));
                ShowContinueError(state, format("Controller sensed mass flow rate = {:.2T} [kg/s]", ControllerProps(ControlNum).SensedValue));
            } else {
                // bad control variable input checked in input routine
            }
            if (ControllerProps(ControlNum).ActuatorVar == CtrlVarType::Flow) {
                ShowContinueError(state,
                                  format("Controller actuator mass flow rate set to {:.2T} [kg/s]", ControllerProps(ControlNum).MaxAvailActuated));
                if (ControllerProps(ControlNum).ControlVar == CtrlVarType::Temperature) {
                    ShowContinueError(state,
                                      format("Controller actuator temperature = {:.2T} [C]",
                                             state.dataLoopNodes->Node(ControllerProps(ControlNum).ActuatedNode).Temp));
                    ShowContinueError(state, "  Note: Chilled water coils should be reverse action and the entering chilled");
                    ShowContinueError(state, "        water temperature (controller actuator temperature) should be below the setpoint temperature");
                    ShowContinueError(state, "  Note: Hot water coils should be normal action and the entering hot");
                    ShowContinueError(state, "        water temperature (controller actuator temperature) should be above the setpoint temperature");
                }
            } else {
                // bad actuator variable input checked in input routine
            }
        } else if (!state.dataGlobal->WarmupFlag) {
            ++ControllerProps(ControlNum).BadActionErrCount;
            ShowRecurringSevereErrorAtEnd(state,
                                          "FindRootSimpleController: Previous controller action error continues for controller = " + ControllerName,
                                          ControllerProps(ControlNum).BadActionErrIndex);
        } else {
            // do nothing
        }
        // Indicate convergence with min value
        // Should be the same as ControllerProps(ControlNum)%MaxAvailActuated
        ExitCalcController(state, ControlNum, RootFinders(ControlNum).MaxPoint.X, ControllerMode::MaxActive, IsConvergedFlag, IsUpToDateFlag);
    } break;
    default: {
        // Should never happen
        ShowSevereError(state, "FindRootSimpleController: Root finder failed at " + CreateHVACStepFullString(state));
        ShowContinueError(state, " Controller name=" + ControllerName);
        ShowContinueError(state, format(" Unrecognized root finder status flag={}", RootFinders(ControlNum).StatusFlag));
        ShowFatalError(state, "Preceding error causes program termination.");
    } break;
    }
}

void CheckSimpleController(EnergyPlusData &state, int const ControlNum, bool &IsConvergedFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil (LBNL)
    //       DATE WRITTEN   Feb 2006
    //       MODIFIED       na
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // New routine used to detect whether controller can be considered converged
    // depending on its mode of operation.
    // Used after all controllers on an air loop have been solved in order
    // to make sure that final air loop state still represents a converged
    // state.
    // PRECONDITION: Setpoint must be known. See ControllerProps%IsSetPointDefinedFlag

    using RootFinder::CheckRootFinderConvergence;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);
    auto &RootFinders(state.dataHVACControllers->RootFinders);

    // Obtain actuated and sensed nodes
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Default initialization: assuming no convergence unless detected in the following code!
    IsConvergedFlag = false;

    switch (ControllerProps(ControlNum).Mode) {
    case ControllerMode::Off: {
        // Check whether the component is running
        // This check is performed by looking at the component mass flow rate at the sensed node.
        // Since the components have been simulated before getting here, if they are zero they should be OFF.
        if (state.dataLoopNodes->Node(SensedNode).MassFlowRate == 0.0) {
            if (ControllerProps(ControlNum).ActuatedValue == 0.0) {
                IsConvergedFlag = true;
                return;
            }
        }
    } break;
    case ControllerMode::Inactive: {
        // Controller component NOT available (ie, inactive)
        // Make sure that the actuated variable is still equal to the node min avail
        // NOTE: Replaced Node(ActuatedNode)%MassFlowRateMinAvail         in release 1.3
        //       with     ControllerProps(ControlNum)%MinAvailActuated    in release 1.4
        if (ControllerProps(ControlNum).ActuatedValue == ControllerProps(ControlNum).MinAvailActuated) {
            IsConvergedFlag = true;
            return;
        }
    } break;
    case ControllerMode::MinActive: {
        // Check for min constrained convergence
        if (CheckMinActiveController(state, ControlNum)) {
            IsConvergedFlag = true;
            return;
        }
        // Check for unconstrained convergence assuming that there is more than one controller controlling
        // the same sensed node and that the other controller was able to meet the setpoint although this one
        // was min-constrained.
        if (CheckRootFinderConvergence(RootFinders(ControlNum), ControllerProps(ControlNum).DeltaSensed)) {
            // Indicate convergence with base value (used to compute DeltaSensed!)
            IsConvergedFlag = true;
            return;
        }
    } break;
    case ControllerMode::MaxActive: {
        // Check for max constrained convergence
        if (CheckMaxActiveController(state, ControlNum)) {
            IsConvergedFlag = true;
            return;
        }
        // Check for unconstrained convergence assuming that there is more than one controller controlling
        // the same sensed node and that the other controller was able to meet the setpoint although this one
        // was max-constrained.
        if (CheckRootFinderConvergence(RootFinders(ControlNum), ControllerProps(ControlNum).DeltaSensed)) {
            // Indicate convergence with base value (used to compute DeltaSensed!)
            IsConvergedFlag = true;
            return;
        }
    } break;
    case ControllerMode::Active: {
        // Check min constraint on actuated variable
        if (ControllerProps(ControlNum).ActuatedValue < ControllerProps(ControlNum).MinAvailActuated) {
            IsConvergedFlag = false;
            return;
        }
        // Check max constraint on actuated variable
        if (ControllerProps(ControlNum).ActuatedValue > ControllerProps(ControlNum).MaxAvailActuated) {
            IsConvergedFlag = false;
            return;
        }

        // Check for unconstrained convergence
        // Equivalent to:
        // IF ((ABS(ControllerProps(ControlNum)%DeltaSensed) .LE. ControllerProps(ControlNum)%Offset)) THEN
        // NOTE: If setpoint has changed since last call, then the following test will most likely fail.
        if (CheckRootFinderConvergence(RootFinders(ControlNum), ControllerProps(ControlNum).DeltaSensed)) {
            // Indicate convergence with base value (used to compute DeltaSensed!)
            IsConvergedFlag = true;
            return;
        }
        // Check for min constrained convergence
        if (CheckMinActiveController(state, ControlNum)) {
            IsConvergedFlag = true;
            return;
        }
        // Check for max constrained convergence
        if (CheckMaxActiveController(state, ControlNum)) {
            IsConvergedFlag = true;
            return;
        }
    } break;
    default: {
        // Can only happen if controller is not converged after MaxIter in SolveAirLoopControllers()
        // which will produce ControllerProps(ControlNum)%Mode = iModeNone
        IsConvergedFlag = false;
    } break;
    }
}

bool CheckMinActiveController(EnergyPlusData &state, int const ControlNum)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Returns true if controller is min-constrained. false otherwise.

    // Return value
    bool CheckMinActiveController;

    CheckMinActiveController = false;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Check that actuated value is the min avail actuated value
    if (ControllerProps(ControlNum).ActuatedValue != ControllerProps(ControlNum).MinAvailActuated) {
        CheckMinActiveController = false;
        return CheckMinActiveController;
    }

    switch (ControllerProps(ControlNum).Action) {
    case ControllerAction::NormalAction: { // "NORMAL"
        // Check for min constrained convergence
        if (ControllerProps(ControlNum).SetPointValue <= ControllerProps(ControlNum).SensedValue) {
            CheckMinActiveController = true;
            return CheckMinActiveController;
        }
    } break;
    case ControllerAction::Reverse: { // "REVERSE"
        // Check for min constrained convergence
        if (ControllerProps(ControlNum).SetPointValue >= ControllerProps(ControlNum).SensedValue) {
            CheckMinActiveController = true;
            return CheckMinActiveController;
        }
    } break;
    default: {
        // Should never happen
        ShowSevereError(state, "CheckMinActiveController: Invalid controller action during " + CreateHVACStepFullString(state) + '.');
        ShowContinueError(state, "CheckMinActiveController: Controller name=" + ControllerProps(ControlNum).ControllerName);
        ShowContinueError(state, R"(CheckMinActiveController: Valid choices are "NORMAL" or "REVERSE")");
        ShowFatalError(state, "CheckMinActiveController: Preceding error causes program termination.");
    } break;
    }

    return CheckMinActiveController;
}

bool CheckMaxActiveController(EnergyPlusData &state, int const ControlNum)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Returns true if controller is max-constrained. false otherwise.

    // Return value
    bool CheckMaxActiveController;

    CheckMaxActiveController = false;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Check that actuated value is the max avail actuated value
    if (ControllerProps(ControlNum).ActuatedValue != ControllerProps(ControlNum).MaxAvailActuated) {
        CheckMaxActiveController = false;
        return CheckMaxActiveController;
    }

    switch (ControllerProps(ControlNum).Action) {
    case ControllerAction::NormalAction: { // "NORMAL"
        // Check for max constrained convergence
        if (ControllerProps(ControlNum).SetPointValue >= ControllerProps(ControlNum).SensedValue) {
            CheckMaxActiveController = true;
            return CheckMaxActiveController;
        }
    } break;
    case ControllerAction::Reverse: { // "REVERSE"
        // Check for max constrained convergence
        if (ControllerProps(ControlNum).SetPointValue <= ControllerProps(ControlNum).SensedValue) {
            CheckMaxActiveController = true;
            return CheckMaxActiveController;
        }
    } break;
    default: {
        // Should never happen
        ShowSevereError(state, "CheckMaxActiveController: Invalid controller action during " + CreateHVACStepFullString(state) + '.');
        ShowContinueError(state, "CheckMaxActiveController: Controller name=" + ControllerProps(ControlNum).ControllerName);
        ShowContinueError(state, R"(CheckMaxActiveController: Valid choices are "NORMAL" or "REVERSE")");
        ShowFatalError(state, "CheckMaxActiveController: Preceding error causes program termination.");
    } break;
    }

    return CheckMaxActiveController;
}

void SaveSimpleController(EnergyPlusData &state, int const ControlNum, bool const FirstHVACIteration, bool const IsConvergedFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Updates solution trackers if simple controller is converged.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PreviousSolutionIndex;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Save solution and mode for next call only if converged
    if (IsConvergedFlag) {
        if (FirstHVACIteration) {
            PreviousSolutionIndex = 1;
        } else {
            PreviousSolutionIndex = 2;
        }

        if (ControllerProps(ControlNum).Mode == ControllerMode::Active) {
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).DefinedFlag = true;
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).Mode = ControllerProps(ControlNum).Mode;
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).ActuatedValue = ControllerProps(ControlNum).NextActuatedValue;
        } else {
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).DefinedFlag = false;
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).Mode = ControllerProps(ControlNum).Mode;
            ControllerProps(ControlNum).SolutionTrackers(PreviousSolutionIndex).ActuatedValue = ControllerProps(ControlNum).NextActuatedValue;
        }
    }
}

void UpdateController(EnergyPlusData &state, int const ControlNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the actuated node with the next candidate value.

    // Using/Aliasing
    using PlantUtilities::SetActuatedBranchFlowRate;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Set the sensed and actuated node numbers
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Set the actuated node of the Controller
    switch (ControllerProps(ControlNum).ActuatorVar) {
    case CtrlVarType::Flow: { // 'Flow'
        SetActuatedBranchFlowRate(state,
                                  ControllerProps(ControlNum).NextActuatedValue,
                                  ControllerProps(ControlNum).ActuatedNode,
                                  ControllerProps(ControlNum).ActuatedNodePlantLoc,
                                  false);
        //     Node(ActuatedNode)%MassFlowRate = ControllerProps(ControlNum)%NextActuatedValue
    } break;
    default: {
        ShowFatalError(state, "UpdateController: Invalid Actuator Variable Type=" + ControlVariableTypes(ControllerProps(ControlNum).ActuatorVar));
    } break;
    }
}

void CheckTempAndHumRatCtrl(EnergyPlusData &state, int const ControlNum, bool &IsConvergedFlag)
{

    {
        auto &thisController(state.dataHVACControllers->ControllerProps(ControlNum));
        if (IsConvergedFlag) {
            if (thisController.ControlVar == CtrlVarType::TemperatureAndHumidityRatio) {
                // For temperature and humidity control, after temperature control is converged, check if humidity setpoint is met
                if (!thisController.HumRatCtrlOverride) {
                    // For humidity control tolerance, always use 0.0001 which is roughly equivalent to a 0.015C change in dewpoint
                    if (state.dataLoopNodes->Node(thisController.SensedNode).HumRat >
                        (state.dataLoopNodes->Node(thisController.SensedNode).HumRatMax + 1.0e-5)) {
                        // Turn on humidity control and restart controller
                        IsConvergedFlag = false;
                        thisController.HumRatCtrlOverride = true;
                        if (thisController.Action == ControllerAction::Reverse) {
                            // Cooling coil controller should always be Reverse, but skip this if not
                            RootFinder::SetupRootFinder(state,
                                                        state.dataHVACControllers->RootFinders(ControlNum),
                                                        DataRootFinder::Slope::Decreasing,
                                                        DataRootFinder::RootFinderMethod::FalsePosition,
                                                        DataPrecisionGlobals::constant_zero,
                                                        1.0e-6,
                                                        1.0e-5);
                        }
                        // Do a cold start reset, same as iControllerOpColdStart
                        ResetController(state, ControlNum, false, IsConvergedFlag);
                    }
                }
            }
        }
    }
}

void ExitCalcController(EnergyPlusData &state,
                        int const ControlNum,
                        Real64 const NextActuatedValue,
                        ControllerMode const Mode,
                        bool &IsConvergedFlag,
                        bool &IsUpToDateFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   February 06
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Only called when controller is considered as "converged", meaning that we do no longer
    // need to continue iterating.

    // METHODOLOGY EMPLOYED:
    // Updates:
    // - next actuated value
    // - controller mode
    // - IsConvergedFlag
    // - IsUpToDateFlag

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    ControllerProps(ControlNum).NextActuatedValue = NextActuatedValue;
    ControllerProps(ControlNum).Mode = Mode;
    IsConvergedFlag = true;

    // Set IsUpToDateFlag upon exiting to indicate caller whether or not the air loop needs to be
    // re-simulated with the current candidate value, ie ControllerProps(ControlNum)%NextActuatedValue
    if (ControllerProps(ControlNum).ActuatedValue != ControllerProps(ControlNum).NextActuatedValue) {
        IsUpToDateFlag = false;
    } else {
        IsUpToDateFlag = true;
    }
}

void TrackAirLoopControllers(EnergyPlusData &state,
                             int const AirLoopNum,
                             ControllerWarmRestart const WarmRestartStatus,
                             int const AirLoopIterMax,
                             int const AirLoopIterTot,
                             int const AirLoopNumCalls)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Updates runtime statistics for controllers on the specified air loop.
    // Used to produce objective metrics when analyzing runtime performance
    // of HVAC controllers for different implementations.

    // SUBROUTINE PARAMETER DEFINITIONS:
    // See CONTROLLER_WARM_RESTART_<> parameters in DataHVACControllers.cc
    // If Status<0, no speculative warm restart.
    // If Status==0, speculative warm restart failed.
    // If Status>0, speculative warm restart succeeded.
    // Max number of iterations performed by controllers on this air loop (per call to SimAirLoop)
    // Aggregated number of iterations performed by controllers on this air loop (per call to SimAirLoop)
    // Number of times SimAirLoopComponents() has been invoked

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ControllerNum;

    auto &AirLoopStats(state.dataHVACControllers->AirLoopStats);

    // If no controllers on this air loop then we have nothing to do
    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers == 0) return;
    // To avoid tracking statistics in case of no air loop or no HVAC controllers are defined
    if (state.dataHVACControllers->NumAirLoopStats == 0) return;

    // Update performance statistics for air loop
    ++AirLoopStats(AirLoopNum).NumCalls;

    switch (WarmRestartStatus) {
    case ControllerWarmRestart::Success: {
        ++AirLoopStats(AirLoopNum).NumSuccessfulWarmRestarts;
    } break;
    case ControllerWarmRestart::Fail: {
        ++AirLoopStats(AirLoopNum).NumFailedWarmRestarts;
    } break;
    default: {
        // Nothing to do if no speculative warm restart used
    } break;
    }

    AirLoopStats(AirLoopNum).TotSimAirLoopComponents += AirLoopNumCalls;

    AirLoopStats(AirLoopNum).MaxSimAirLoopComponents = max(AirLoopStats(AirLoopNum).MaxSimAirLoopComponents, AirLoopNumCalls);

    AirLoopStats(AirLoopNum).TotIterations += AirLoopIterTot;

    AirLoopStats(AirLoopNum).MaxIterations = max(AirLoopStats(AirLoopNum).MaxIterations, AirLoopIterMax);

    // Update performance statistics for each controller on air loop
    for (ControllerNum = 1; ControllerNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers; ++ControllerNum) {
        TrackAirLoopController(state, AirLoopNum, ControllerNum);
    }
}

void TrackAirLoopController(EnergyPlusData &state,
                            int const AirLoopNum,       // Air loop index
                            int const AirLoopControlNum // Controller index on this air loop
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Updates runtime statistics for the specified controller.
    // Used to produce objective metrics when analyzing runtime performance
    // of HVAC controllers for different implementations.

    // Corresponding index in ControllerProps array
    int ControlIndex;
    // Number of iterations needed to solve this controller
    int IterationCount;
    // Current operating mode
    ControllerMode Mode;

    auto &AirLoopStats(state.dataHVACControllers->AirLoopStats);
    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    ControlIndex = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum);

    // We use NumCalcCalls instead of the iteration counter used in SolveAirLoopControllers()
    // to avoid having to call TrackAirLoopController() directly from SolveAirLoopControllers().
    // The 2 counters should be the same anyway as NumCalcCalls is first reset to zero and
    // incremented each time ManageControllers() is invoked with iControllerOpIterate
    IterationCount = ControllerProps(ControlIndex).NumCalcCalls;
    Mode = ControllerProps(ControlIndex).Mode;

    if (Mode != ControllerMode::None) {

        ++AirLoopStats(AirLoopNum).ControllerStats(AirLoopControlNum).NumCalls(static_cast<int>(Mode));

        AirLoopStats(AirLoopNum).ControllerStats(AirLoopControlNum).TotIterations(static_cast<int>(Mode)) += IterationCount;

        AirLoopStats(AirLoopNum).ControllerStats(AirLoopControlNum).MaxIterations(static_cast<int>(Mode)) =
            max(AirLoopStats(AirLoopNum).ControllerStats(AirLoopControlNum).MaxIterations(static_cast<int>(Mode)), IterationCount);
    }
}

void DumpAirLoopStatistics(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Writes runtime statistics for controllers on all air loops
    // to a CSV file named "statistics.HVACControllers.csv".

    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // Detect if statistics have been generated or not for this run
    if (!state.dataSysVars->TrackAirLoopEnvFlag) {
        return;
    }

    InputOutputFilePath StatisticsFilePath{"statistics.HVACControllers.csv"};
    auto statisticsFile = StatisticsFilePath.open(state, "DumpAirLoopStatistics");

    // note that the AirLoopStats object does not seem to be initialized when this code
    // is executed and it causes a crash here
    for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
        WriteAirLoopStatistics(
            state, statisticsFile, state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum), state.dataHVACControllers->AirLoopStats(AirLoopNum));
    }
}

void WriteAirLoopStatistics(EnergyPlusData &state,
                            InputOutputFile &statisticsFile,
                            DefinePrimaryAirSystem const &ThisPrimaryAirSystem,
                            AirLoopStatsType const &ThisAirLoopStats)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Writes runtime statistics for controllers on the specified air loop
    // to the specified file.

    using namespace DataAirSystems;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopControlNum;
    int NumWarmRestarts;
    Real64 WarmRestartSuccessRatio;
    int NumCalls;
    int TotIterations;
    int MaxIterations;
    Real64 AvgIterations;
    int iModeNum;

    print(statisticsFile, "{},\n", ThisPrimaryAirSystem.Name);

    // Number of calls to SimAirLoop() has been invoked over the course of the simulation
    // to simulate the specified air loop
    print(statisticsFile, "NumCalls,{}\n", ThisAirLoopStats.NumCalls);

    // Warm restart success ratio
    NumWarmRestarts = ThisAirLoopStats.NumSuccessfulWarmRestarts + ThisAirLoopStats.NumFailedWarmRestarts;
    if (NumWarmRestarts == 0) {
        WarmRestartSuccessRatio = 0.0;
    } else {
        WarmRestartSuccessRatio = double(ThisAirLoopStats.NumSuccessfulWarmRestarts) / double(NumWarmRestarts);
    }

    print(statisticsFile, "NumWarmRestarts,{}\n", NumWarmRestarts);
    print(statisticsFile, "NumSuccessfulWarmRestarts,{}\n", ThisAirLoopStats.NumSuccessfulWarmRestarts);
    print(statisticsFile, "NumFailedWarmRestarts,{}\n", ThisAirLoopStats.NumFailedWarmRestarts);
    print(statisticsFile, "WarmRestartSuccessRatio,{:.10T}\n", WarmRestartSuccessRatio);

    // Total number of times SimAirLoopComponents() has been invoked over the course of the simulation
    // to simulate the specified air loop
    print(statisticsFile, "TotSimAirLoopComponents,{}\n", ThisAirLoopStats.TotSimAirLoopComponents);
    // Maximum number of times SimAirLoopComponents() has been invoked over the course of the simulation
    // to simulate the specified air loop
    print(statisticsFile, "MaxSimAirLoopComponents,{}\n", ThisAirLoopStats.MaxSimAirLoopComponents);

    // Aggregated number of iterations needed by all controllers to simulate the specified air loop
    print(statisticsFile, "TotIterations,{}\n", ThisAirLoopStats.TotIterations);
    // Maximum number of iterations needed by controllers to simulate the specified air loop
    print(statisticsFile, "MaxIterations,{}\n", ThisAirLoopStats.MaxIterations);

    // Average number of iterations needed by controllers to simulate the specified air loop
    if (ThisAirLoopStats.NumCalls == 0) {
        AvgIterations = 0.0;
    } else {
        AvgIterations = double(ThisAirLoopStats.TotIterations) / double(ThisAirLoopStats.NumCalls);
    }

    print(statisticsFile, "AvgIterations,{:.10T}\n", AvgIterations);

    // Dump statistics for each controller on this air loop
    for (AirLoopControlNum = 1; AirLoopControlNum <= ThisPrimaryAirSystem.NumControllers; ++AirLoopControlNum) {

        print(statisticsFile, "{},\n", ThisPrimaryAirSystem.ControllerName(AirLoopControlNum));

        // Aggregate iteration trackers across all operating modes
        NumCalls = 0;
        TotIterations = 0;
        MaxIterations = 0;

        for (iModeNum = iFirstMode; iModeNum <= iLastMode; ++iModeNum) {
            NumCalls += ThisAirLoopStats.ControllerStats(AirLoopControlNum).NumCalls(iModeNum);

            TotIterations += ThisAirLoopStats.ControllerStats(AirLoopControlNum).TotIterations(iModeNum);

            MaxIterations = max(MaxIterations, ThisAirLoopStats.ControllerStats(AirLoopControlNum).MaxIterations(iModeNum));
        }

        // Number of times this controller was simulated (should match air loop num calls)
        print(statisticsFile, "NumCalls,{}\n", NumCalls);
        // Aggregated number of iterations needed by this controller
        print(statisticsFile, "TotIterations,{}\n", TotIterations);
        // Aggregated number of iterations needed by this controller
        print(statisticsFile, "MaxIterations,{}\n", MaxIterations);

        // Average number of iterations needed by controllers to simulate the specified air loop
        if (NumCalls == 0) {
            AvgIterations = 0.0;
        } else {
            AvgIterations = double(TotIterations) / double(NumCalls);
        }
        print(statisticsFile, "AvgIterations,{:.10T}\n", AvgIterations);

        // Dump iteration trackers for each operating mode
        for (iModeNum = iFirstMode; iModeNum <= iLastMode; ++iModeNum) {

            print(statisticsFile, "{},\n", state.dataHVACCtrl->ControllerModeTypes(iModeNum));

            // Number of times this controller operated in this mode
            print(statisticsFile, "NumCalls,{}\n", ThisAirLoopStats.ControllerStats(AirLoopControlNum).NumCalls(iModeNum));

            // Aggregated number of iterations needed by this controller
            print(statisticsFile, "TotIterations,{}\n", ThisAirLoopStats.ControllerStats(AirLoopControlNum).TotIterations(iModeNum));
            // Aggregated number of iterations needed by this controller
            print(statisticsFile, "MaxIterations,{}\n", ThisAirLoopStats.ControllerStats(AirLoopControlNum).MaxIterations(iModeNum));

            // Average number of iterations needed by controllers to simulate the specified air loop
            if (ThisAirLoopStats.ControllerStats(AirLoopControlNum).NumCalls(iModeNum) == 0) {
                AvgIterations = 0.0;
            } else {
                AvgIterations = double(ThisAirLoopStats.ControllerStats(AirLoopControlNum).TotIterations(iModeNum)) /
                                double(ThisAirLoopStats.ControllerStats(AirLoopControlNum).NumCalls(iModeNum));
            }
            print(statisticsFile, "AvgIterations,{:.10T}\n", AvgIterations);
        }
    }
}

void SetupAirLoopControllersTracer(EnergyPlusData &state, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   February 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Opens main trace file for controllers on specific air loop
    // and writes header row with titles.

    int ControllerNum;

    // Open main controller trace file for each air loop
    const auto TraceFilePath = "controller." + state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name + ".csv";

    auto &AirLoopStats(state.dataHVACControllers->AirLoopStats);

    // Store file unit in air loop stats
    AirLoopStats(AirLoopNum).TraceFile->filePath = TraceFilePath;
    AirLoopStats(AirLoopNum).TraceFile->open();

    if (!AirLoopStats(AirLoopNum).TraceFile->good()) {
        ShowFatalError(state, "SetupAirLoopControllersTracer: Failed to open air loop trace file \"" + TraceFilePath + "\" for output (write).");
        return;
    }

    auto &TraceFile = *AirLoopStats(AirLoopNum).TraceFile;

    // List all controllers and their corresponding handles into main trace file
    print(TraceFile, "Num,Name,\n");

    for (ControllerNum = 1; ControllerNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers; ++ControllerNum) {
        print(TraceFile, "{},{},\n", ControllerNum, state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).ControllerName(ControllerNum));
        // SAME AS ControllerProps(ControllerIndex)%ControllerName BUT NOT YET AVAILABLE
    }

    // Skip a bunch of lines
    print(TraceFile, "\n\n\n");

    // Write column header in main controller trace file
    print(TraceFile,
          "ZoneSizingCalc,SysSizingCalc,EnvironmentNum,WarmupFlag,SysTimeStamp,SysTimeInterval,BeginTimeStepFlag,FirstTimeStepSysFlag,"
          "FirstHVACIteration,AirLoopPass,AirLoopNumCallsTot,AirLoopConverged,");

    // Write headers for final state
    for (ControllerNum = 1; ControllerNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers; ++ControllerNum) {
        print(
            TraceFile, "Mode{},IterMax{},XRoot{},YRoot{},YSetPoint{},\n", ControllerNum, ControllerNum, ControllerNum, ControllerNum, ControllerNum);
    }

    print(TraceFile, "\n");
}

void TraceAirLoopControllers(EnergyPlusData &state,
                             bool const FirstHVACIteration,
                             int const AirLoopNum,
                             int const AirLoopPass,
                             bool const AirLoopConverged,
                             int const AirLoopNumCalls)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes diagnostic to the trace file attached to each air loop.

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE when primary air system & controllers simulation has converged;
    // Number of times SimAirLoopComponents() has been invoked

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ControllerNum;

    auto &AirLoopStats(state.dataHVACControllers->AirLoopStats);

    // IF no controllers on this air loop then we have nothing to do
    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers == 0) return;
    // To avoid tracking statistics in case of no air loop or no HVAC controllers are defined
    if (state.dataHVACControllers->NumAirLoopStats == 0) return;

    // Setup trace file on first call only
    if (AirLoopStats(AirLoopNum).FirstTraceFlag) {
        SetupAirLoopControllersTracer(state, AirLoopNum);

        AirLoopStats(AirLoopNum).FirstTraceFlag = false;
    }

    auto &TraceFile = *AirLoopStats(AirLoopNum).TraceFile;

    if (!TraceFile.good()) return;

    // Write iteration stamp first
    TraceIterationStamp(state, TraceFile, FirstHVACIteration, AirLoopPass, AirLoopConverged, AirLoopNumCalls);

    // Loop over the air sys controllers and write diagnostic to trace file
    for (ControllerNum = 1; ControllerNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumControllers; ++ControllerNum) {
        TraceAirLoopController(state, TraceFile, state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).ControllerIndex(ControllerNum));
    }

    // Go to next line
    print(TraceFile, "\n");
}

void TraceIterationStamp(EnergyPlusData &state,
                         InputOutputFile &TraceFile,
                         bool const FirstHVACIteration,
                         int const AirLoopPass,
                         bool const AirLoopConverged,
                         int const AirLoopNumCalls)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   February 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Writes current iteration time stamp to specified trace file.

    // Using/Aliasing
    using General::LogicalToInteger;

    // SUBROUTINE PARAMETER DEFINITIONS:
    // TRUE when primary air system and controllers simulation has converged;
    // Number of times SimAirLoopComponents() has been invoked

    // Write step stamp to air loop trace file after reset
    // Note that we do not go to the next line
    print(TraceFile,
          "{},{},{},{},{},{},{},{},{},{},{},{},",
          LogicalToInteger(state.dataGlobal->ZoneSizingCalc),
          LogicalToInteger(state.dataGlobal->SysSizingCalc),
          state.dataEnvrn->CurEnvirNum,
          LogicalToInteger(state.dataGlobal->WarmupFlag),
          CreateHVACTimeString(state),
          MakeHVACTimeIntervalString(state),
          LogicalToInteger(state.dataGlobal->BeginTimeStepFlag),
          LogicalToInteger(state.dataHVACGlobal->FirstTimeStepSysFlag),
          LogicalToInteger(FirstHVACIteration),
          AirLoopPass,
          AirLoopNumCalls,
          LogicalToInteger(AirLoopConverged));
}

void TraceAirLoopController(EnergyPlusData &state, InputOutputFile &TraceFile, int const ControlNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes convergence diagnostic to the air loop trace file
    // for the specified controller index.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Set the sensed and actuated node numbers
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    print(TraceFile,
          "{},{},{:.10T},{:.10T},{:.10T},",
          ControllerProps(ControlNum).Mode,
          ControllerProps(ControlNum).NumCalcCalls,
          state.dataLoopNodes->Node(ActuatedNode).MassFlowRate,
          state.dataLoopNodes->Node(SensedNode).Temp,
          state.dataLoopNodes->Node(SensedNode).TempSetPoint);
}

void SetupIndividualControllerTracer(EnergyPlusData &state, int const ControlNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   February 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Opens individual controller trace file for the specified controller
    // and writes header row.

    using RootFinder::WriteRootFinderTraceHeader;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    const auto TraceFilePath = "controller." + ControllerProps(ControlNum).ControllerName + ".csv";
    auto &TraceFile = *ControllerProps(ControlNum).TraceFile;
    TraceFile.filePath = TraceFilePath;
    TraceFile.open();

    if (!TraceFile.good()) {
        ShowFatalError(state, "SetupIndividualControllerTracer: Failed to open controller trace file \"" + TraceFilePath + "\" for output (write).");
        return;
    }

    // Write header row
    // Masss flow rate
    // Convergence analysis
    print(TraceFile,
          "EnvironmentNum,WarmupFlag,SysTimeStamp,SysTimeInterval,AirLoopPass,FirstHVACIteration,Operation,NumCalcCalls,SensedNode%MassFlowRate,"
          "ActuatedNode%MassFlowRateMinAvail,ActuatedNode%MassFlowRateMaxAvail,X,Y,Setpoint,DeltaSensed,Offset,Mode,IsConvergedFlag,"
          "NextActuatedValue");

    WriteRootFinderTraceHeader(TraceFile);

    // Finally skip line
    print(TraceFile, "\n");
}

void TraceIndividualController(EnergyPlusData &state,
                               int const ControlNum,
                               bool const FirstHVACIteration,
                               int const AirLoopPass,
                               ControllerOperation const Operation, // Operation to execute
                               bool const IsConvergedFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes convergence diagnostic to the trace file for the specified
    // controller.

    using General::LogicalToInteger;
    using RootFinder::WriteRootFinderTrace;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActuatedNode;
    int SensedNode;
    bool SkipLineFlag;

    auto &ControllerProps(state.dataHVACControllers->ControllerProps);

    // Setup individual trace file on first trace only
    if (ControllerProps(ControlNum).FirstTraceFlag) {
        SetupIndividualControllerTracer(state, ControlNum);

        ControllerProps(ControlNum).FirstTraceFlag = false;
        SkipLineFlag = false;
    } else {
        SkipLineFlag = FirstHVACIteration && (ControllerProps(ControlNum).NumCalcCalls == 0);
    }

    auto &TraceFile = *ControllerProps(ControlNum).TraceFile;

    // Nothing to do if trace file not registered
    if (!TraceFile.good()) return;

    // Skip a line before each new HVAC step
    if (SkipLineFlag) {
        print(TraceFile, "\n");
    }

    // Set the sensed and actuated node numbers
    ActuatedNode = ControllerProps(ControlNum).ActuatedNode;
    SensedNode = ControllerProps(ControlNum).SensedNode;

    // Write iteration stamp
    print(TraceFile,
          "{},{},{},{},{},{},{},{},",
          state.dataEnvrn->CurEnvirNum,
          LogicalToInteger(state.dataGlobal->WarmupFlag),
          CreateHVACTimeString(state),
          MakeHVACTimeIntervalString(state),
          AirLoopPass,
          LogicalToInteger(FirstHVACIteration),
          Operation,
          ControllerProps(ControlNum).NumCalcCalls);

    // Write detailed diagnostic
    switch (Operation) {
    case ControllerOperation::ColdStart:
    case ControllerOperation::WarmRestart: {
        print(TraceFile,
              "{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{},{},{},{},{:.10T},",
              state.dataLoopNodes->Node(SensedNode).MassFlowRate,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail,
              ControllerProps(ControlNum).ActuatedValue,
              state.dataLoopNodes->Node(SensedNode).Temp,
              ControllerProps(ControlNum).SetPointValue,
              ' ',
              ' ',
              ControllerProps(ControlNum).Mode,
              LogicalToInteger(IsConvergedFlag),
              ControllerProps(ControlNum).NextActuatedValue);
        // X | Y | setpoint | DeltaSensed = Y - YRoot | Offset | Mode | IsConvergedFlag

        // No trace available for root finder yet
        // Skip call to WriteRootFinderTrace()

        // Finally skip line
        print(TraceFile, "\n");
    } break;
    case ControllerOperation::Iterate: {
        // Masss flow rate
        // Convergence analysis

        print(TraceFile,
              "{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{},{},{:.10T},",
              state.dataLoopNodes->Node(SensedNode).MassFlowRate,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail,
              ControllerProps(ControlNum).ActuatedValue,
              state.dataLoopNodes->Node(SensedNode).Temp,
              ControllerProps(ControlNum).SetPointValue,
              ControllerProps(ControlNum).DeltaSensed,
              ControllerProps(ControlNum).Offset,
              ControllerProps(ControlNum).Mode,
              LogicalToInteger(IsConvergedFlag),
              ControllerProps(ControlNum).NextActuatedValue);

        // X | Y | setpoint | DeltaSensed = Y - YRoot | Offset | Mode | IsConvergedFlag

        // Append trace for root finder
        WriteRootFinderTrace(TraceFile, state.dataHVACControllers->RootFinders(ControlNum));

        // Finally skip line
        print(TraceFile, "\n");

    } break;
    case ControllerOperation::End: {
        // Masss flow rate
        // Convergence analysis
        print(TraceFile,
              "{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{:.10T},{},{},{:.10T},",
              state.dataLoopNodes->Node(SensedNode).MassFlowRate,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail,
              state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail,
              ControllerProps(ControlNum).ActuatedValue,
              state.dataLoopNodes->Node(SensedNode).Temp,
              ControllerProps(ControlNum).SetPointValue,
              ControllerProps(ControlNum).DeltaSensed,
              ControllerProps(ControlNum).Offset,
              ControllerProps(ControlNum).Mode,
              LogicalToInteger(IsConvergedFlag),
              ControllerProps(ControlNum).NextActuatedValue);

        // X | Y | setpoint | DeltaSensed = Y - YRoot | Offset | Mode | IsConvergedFlag

        // No trace available for root finder yet
        // Skip call to WriteRootFinderTrace()

        // Finally skip line
        print(TraceFile, "\n");

        // Skip an additional line to indicate end of current HVAC step
        print(TraceFile, "\n");

    } break;
    default: {
        // Should never happen
        ShowFatalError(state,
                       format("TraceIndividualController: Invalid Operation passed={}, Controller name={}",
                              Operation,
                              ControllerProps(ControlNum).ControllerName));
    } break;
    }

    TraceFile.flush();
}

std::string CreateHVACTimeString(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates a string describing the current time stamp of the system
    // time step.

    using General::CreateTimeString;
    using General::GetCurrentHVACTime;

    // Return value
    std::string OutputString;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    std::string Buffer;

    Buffer = CreateTimeString(GetCurrentHVACTime(state));
    OutputString = state.dataEnvrn->CurMnDy + ' ' + stripped(Buffer);

    return OutputString;
}

std::string CreateHVACStepFullString(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   April 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates a string describing the current HVAC step.
    // It includes the environment name, the current day/month and the current
    // time stamp for the system time step.
    // It is used in error messages only.

    // Return value
    std::string OutputString;

    OutputString = state.dataEnvrn->EnvironmentName + ", " + MakeHVACTimeIntervalString(state);

    return OutputString;
}

std::string MakeHVACTimeIntervalString(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates a string describing the current time interval of the system
    // time step.

    // Using/Aliasing
    using General::CreateHVACTimeIntervalString;

    // Return value
    std::string OutputString;

    OutputString = stripped(CreateHVACTimeIntervalString(state));

    return OutputString;
}

void CheckControllerListOrder(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Oct 10.
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // check that if multiple controllers on an air loop, that they aren't listed in a bad order
    // CR 8253

    // METHODOLOGY EMPLOYED:
    // setup data for sensed nodes and compare positions if on the same branch

    // Using/Aliasing
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirSysNum;
    int ContrlNum;
    int WaterCoilContrlCount;
    Array2D_int ContrlSensedNodeNums; // array for storing sense node info
    int SensedNodeIndex;
    int BranchNodeIndex;
    int BranchNum;
    int foundControl;

    for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {

        if (state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).NumControllers > 1) {
            // first see how many are water coil controllers
            WaterCoilContrlCount = 0; // init
            for (ContrlNum = 1; ContrlNum <= state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).NumControllers; ++ContrlNum) {
                if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).ControllerType(ContrlNum),
                                                "CONTROLLER:WATERCOIL")) {
                    ++WaterCoilContrlCount;
                }
            }

            if (WaterCoilContrlCount > 1) {
                ContrlSensedNodeNums.allocate(3, WaterCoilContrlCount);
                ContrlSensedNodeNums = 0;
                SensedNodeIndex = 0;
                for (ContrlNum = 1; ContrlNum <= state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).NumControllers; ++ContrlNum) {
                    if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).ControllerType(ContrlNum),
                                                    "CONTROLLER:WATERCOIL")) {
                        ++SensedNodeIndex;
                        foundControl =
                            UtilityRoutines::FindItemInList(state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).ControllerName(ContrlNum),
                                                            state.dataHVACControllers->ControllerProps,
                                                            &ControllerPropsType::ControllerName);
                        if (foundControl > 0) {
                            ContrlSensedNodeNums(1, SensedNodeIndex) = state.dataHVACControllers->ControllerProps(foundControl).SensedNode;
                        }
                    }
                }
            }

            // fill branch index for sensed nodes
            if (allocated(ContrlSensedNodeNums)) {
                for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                    for (SensedNodeIndex = 1; SensedNodeIndex <= WaterCoilContrlCount; ++SensedNodeIndex) {
                        for (BranchNodeIndex = 1;
                             BranchNodeIndex <= state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalNodes;
                             ++BranchNodeIndex) {
                            if (ContrlSensedNodeNums(1, SensedNodeIndex) ==
                                state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNum(BranchNodeIndex)) {
                                ContrlSensedNodeNums(2, SensedNodeIndex) = BranchNodeIndex;
                                ContrlSensedNodeNums(3, SensedNodeIndex) = BranchNum;
                            }
                        }
                    }
                }
            }
            // check if flow order doesn't agree with controller order
            if (allocated(ContrlSensedNodeNums)) {
                for (SensedNodeIndex = 1; SensedNodeIndex <= WaterCoilContrlCount; ++SensedNodeIndex) {
                    if (SensedNodeIndex == 1) continue;
                    if (ContrlSensedNodeNums(2, SensedNodeIndex) < ContrlSensedNodeNums(2, SensedNodeIndex - 1)) {
                        // now see if on the same branch
                        if (ContrlSensedNodeNums(3, SensedNodeIndex) == ContrlSensedNodeNums(3, SensedNodeIndex - 1)) {
                            // we have a flow order problem with water coil controllers
                            ShowSevereError(state, "CheckControllerListOrder: A water coil controller list has the wrong order");
                            ShowContinueError(state,
                                              "Check the AirLoopHVAC:ControllerList for the air loop called \"" +
                                                  state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Name + "\"");
                            ShowContinueError(state,
                                              "When there are multiple Controller:WaterCoil objects for the same air loop, they need to be "
                                              "listed in the proper order.");
                            ShowContinueError(state,
                                              "The controllers should be listed in natural flow order with those for upstream coils listed "
                                              "before those for downstream coils.");
                            ShowContinueError(state, "The sensed nodes specified for the respective controllers should also reflect this order.");
                        }
                    }
                }
            }

            if (allocated(ContrlSensedNodeNums)) ContrlSensedNodeNums.deallocate();

        } // controllers > 1
    }
}

void CheckCoilWaterInletNode(EnergyPlusData &state,
                             int const WaterInletNodeNum, // input actuator node number
                             bool &NodeNotFound           // true if matching actuator node not found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Heejin Cho
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine checks that the water inlet node number is matched by
    // the actuator node number of some water coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ControlNum;

    if (state.dataHVACControllers->GetControllerInputFlag) {
        GetControllerInput(state);
        state.dataHVACControllers->GetControllerInputFlag = false;
    }

    NodeNotFound = true;
    for (ControlNum = 1; ControlNum <= state.dataHVACControllers->NumControllers; ++ControlNum) {
        if (state.dataHVACControllers->ControllerProps(ControlNum).ActuatedNode == WaterInletNodeNum) {
            NodeNotFound = false;
        }
    }
}

void GetControllerNameAndIndex(EnergyPlusData &state,
                               int const WaterInletNodeNum, // input actuator node number
                               std::string &ControllerName, // controller name used by water coil
                               int &ControllerIndex,        // controller index used by water coil
                               bool &ErrorsFound            // true if matching actuator node not found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   June 2017

    // PURPOSE OF THIS FUNCTION:
    // This subroutine checks that the water inlet node number is matched by
    // the actuator node number of some water coil and passed back controller name and index

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ControlNum;

    if (state.dataHVACControllers->GetControllerInputFlag) {
        GetControllerInput(state);
        state.dataHVACControllers->GetControllerInputFlag = false;
    }

    ControllerName = " ";
    ControllerIndex = 0;
    for (ControlNum = 1; ControlNum <= state.dataHVACControllers->NumControllers; ++ControlNum) {
        if (state.dataHVACControllers->ControllerProps(ControlNum).ActuatedNode == WaterInletNodeNum) {
            ControllerIndex = ControlNum;
            ControllerName = state.dataHVACControllers->ControllerProps(ControlNum).ControllerName;
            break;
        }
    }

    if (ControllerIndex == 0) {
        ErrorsFound = true;
    }
}

void GetControllerActuatorNodeNum(EnergyPlusData &state,
                                  std::string const &ControllerName, // name of coil controller
                                  int &WaterInletNodeNum,            // input actuator node number
                                  bool &NodeNotFound                 // true if matching actuator node not found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   September 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine finds the controllers actuator node number

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ControlNum;

    if (state.dataHVACControllers->GetControllerInputFlag) {
        GetControllerInput(state);
        state.dataHVACControllers->GetControllerInputFlag = false;
    }

    NodeNotFound = true;
    ControlNum = UtilityRoutines::FindItemInList(ControllerName, state.dataHVACControllers->ControllerProps, &ControllerPropsType::ControllerName);
    if (ControlNum > 0 && ControlNum <= state.dataHVACControllers->NumControllers) {
        WaterInletNodeNum = state.dataHVACControllers->ControllerProps(ControlNum).ActuatedNode;
        NodeNotFound = false;
    }
}

int GetControllerIndex(EnergyPlusData &state, std::string const &ControllerName // name of coil controller
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   January 2018

    // This subroutine finds the controllers actuator node number

    if (state.dataHVACControllers->GetControllerInputFlag) {
        GetControllerInput(state);
        state.dataHVACControllers->GetControllerInputFlag = false;
    }

    int ControllerIndex =
        UtilityRoutines::FindItemInList(ControllerName, state.dataHVACControllers->ControllerProps, &ControllerPropsType::ControllerName);
    if (ControllerIndex == 0) {
        ShowFatalError(state,
                       "ManageControllers: Invalid controller=" + ControllerName +
                           ". The only valid controller type for an AirLoopHVAC is Controller:WaterCoil.");
    }

    return ControllerIndex;
}

} // namespace EnergyPlus::HVACControllers
