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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>

namespace EnergyPlus {

namespace HeatRecovery {

    // Module containing the routines dealing with heat recovery from exhaust or relief air

    // MODULE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   March 1999
    //       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad April 2003
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and routines required to model heat
    // recovery components in the EnergyPlus HVAC simulation

    // METHODOLOGY EMPLOYED:
    // Heat exchanger effectiveness - NTU models are used.

    // REFERENCES:
    // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger,LBNL Report 42354, 1999.
    // ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
    // ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
    // U.S. Environmental Protection Agency software "SAVES" -
    //  School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataHVACGlobals;
    using namespace DataLoopNode;
    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;

    using General::SolveRoot;
    using namespace Psychrometrics;

    // Data
    // MODULE PARAMETER DEFINITIONS:

    // Heat exchanger performance data type

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:

    // SUBROUTINE SPECIFICATIONS FOR MODULE:

    // Driver/Manager Routines

    // Get Input routines for module

    // Initialization routines for module

    // Sizing routine for the module

    // Update routines to check convergence and update nodes

    // Common routines

    // External function calls

    // Functions

    void SimHeatRecovery(EnergyPlusData &state,
                         std::string_view CompName,               // name of the heat exchanger unit
                         bool const FirstHVACIteration,           // TRUE if 1st HVAC simulation of system timestep
                         int &CompIndex,                          // Pointer to Component
                         int const FanOpMode,                     // Supply air fan operating mode
                         Optional<Real64 const> HXPartLoadRatio,  // Part load ratio requested of DX compressor
                         Optional_bool_const HXUnitEnable,        // Flag to operate heat exchanger
                         Optional_int_const CompanionCoilIndex,   // index of companion cooling coil
                         Optional_bool_const RegenInletIsOANode,  // flag to determine if supply inlet is OA node, if so air flow cycles
                         Optional_bool_const EconomizerFlag,      // economizer operation flag passed by airloop or OA sys
                         Optional_bool_const HighHumCtrlFlag,     // high humidity control flag passed by airloop or OA sys
                         Optional_int_const CompanionCoilType_Num // cooling coil type of coil
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad FSEC - Feb 2009
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage the simulation of a heat recovery unit

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HeatExchNum; // index of unit being simulated
        bool HXUnitOn;   // flag to enable heat exchanger
        // unused0509  INTEGER      :: FanModeOperation          ! supply air fan operating mode
        Real64 PartLoadRatio; // Part load ratio requested of DX compressor
        bool RegInIsOANode;   // local variable to set RegenInletIsOANode optional argument
        int CompanionCoilNum; // Index to companion cooling coil

        if (state.dataHeatRecovery->GetInputFlag) {
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        // Find the correct unit index
        if (CompIndex == 0) {
            HeatExchNum = UtilityRoutines::FindItemInList(CompName, state.dataHeatRecovery->ExchCond);
            if (HeatExchNum == 0) {
                ShowFatalError(state, "SimHeatRecovery: Unit not found=" + std::string{CompName});
            }
            CompIndex = HeatExchNum;
        } else {
            HeatExchNum = CompIndex;
            if (HeatExchNum > state.dataHeatRecovery->NumHeatExchangers || HeatExchNum < 1) {
                ShowFatalError(state,
                               format("SimHeatRecovery:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      HeatExchNum,
                                      state.dataHeatRecovery->NumHeatExchangers,
                                      CompName));
            }
            if (state.dataHeatRecovery->CheckEquipName(HeatExchNum)) {
                if (CompName != state.dataHeatRecovery->ExchCond(HeatExchNum).Name) {
                    ShowFatalError(state,
                                   format("SimHeatRecovery: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          HeatExchNum,
                                          CompName,
                                          state.dataHeatRecovery->ExchCond(HeatExchNum).Name));
                }
                state.dataHeatRecovery->CheckEquipName(HeatExchNum) = false;
            }
        }

        if (present(CompanionCoilIndex)) {
            CompanionCoilNum = CompanionCoilIndex;
        } else {
            CompanionCoilNum = 0;
        }

        int companionCoilType(0);
        if (present(CompanionCoilType_Num)) {
            companionCoilType = CompanionCoilType_Num;
        } else {
            companionCoilType = 0;
        }

        if (present(HXUnitEnable)) {
            HXUnitOn = HXUnitEnable;
            //   When state.dataHeatRecovery->CalledFromParentObject is TRUE, this SIM routine was called by a parent object that passed in
            //   HXUnitEnable. HX will use the DX coil part-load ratio (optional CompanionCoilIndex must be present) or PLR passed in if not used with
            //   DX coil (optional CompanionCoilIndex must not be present).
            state.dataHeatRecovery->CalledFromParentObject = true;
        } else {
            //   HX is placed on a BRANCH, optional arguments are not passed in from SimAirServingZones.
            //   HX will calculate its own part-load ratio if optional HXUnitEnable flag is not present
            HXUnitOn = true;
            state.dataHeatRecovery->CalledFromParentObject = false;
        }

        InitHeatRecovery(state, HeatExchNum, CompanionCoilNum, companionCoilType);

        // call the correct heat exchanger calculation routine
        {
            auto const SELECT_CASE_var(state.dataHeatRecovery->ExchCond(HeatExchNum).ExchTypeNum);

            if (SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE) {

                CalcAirToAirPlateHeatExch(state, HeatExchNum, HXUnitOn, EconomizerFlag, HighHumCtrlFlag);

            } else if (SELECT_CASE_var == HX_AIRTOAIR_GENERIC) {

                CalcAirToAirGenericHeatExch(
                    state, HeatExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag, HXPartLoadRatio);

            } else if (SELECT_CASE_var == HX_DESICCANT_BALANCED) {

                if (present(HXPartLoadRatio)) {
                    PartLoadRatio = HXPartLoadRatio;
                } else {
                    PartLoadRatio = 1.0;
                }

                if (present(RegenInletIsOANode)) {
                    RegInIsOANode = RegenInletIsOANode;
                } else {
                    RegInIsOANode = false;
                }

                CalcDesiccantBalancedHeatExch(state,
                                              HeatExchNum,
                                              HXUnitOn,
                                              FirstHVACIteration,
                                              FanOpMode,
                                              PartLoadRatio,
                                              CompanionCoilNum,
                                              RegInIsOANode,
                                              EconomizerFlag,
                                              HighHumCtrlFlag);
            }
        }

        UpdateHeatRecovery(state, HeatExchNum);

        ReportHeatRecovery(state, HeatExchNum);
    }

    void GetHeatRecoveryInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad FSEC - Feb 2009 (EconoLockout inputs)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for heat recovery units and stores it in
        // appropriate data structures.

        // METHODOLOGY EMPLOYED:
        // Uses InputProcessor "Get" routines to obtain data.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using NodeInputManager::GetOnlySingleNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ExchIndex;                                               // loop index
        int ExchNum;                                                 // current heat exchanger number
        int PerfDataIndex;                                           // desiccant balance heat exchanger performance data loop index
        int PerfDataNum;                                             // current desiccant balanced heat exchanger performance data set number
        int NumAlphas;                                               // Number of Alphas for each GetObjectItem call
        int NumNumbers;                                              // Number of Numbers for each GetObjectItem call
        int IOStatus;                                                // Used in GetObjectItem
        bool ErrorsFound(false);                                     // Set to true if errors in input, fatal at end of routine
        std::string HeatExchPerfType;                                // Desiccant balanced heat exchanger performance data type
        constexpr const char *RoutineName("GetHeatRecoveryInput: "); // include trailing blank space
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        state.dataHeatRecovery->NumAirToAirPlateExchs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "HeatExchanger:AirToAir:FlatPlate");
        state.dataHeatRecovery->NumAirToAirGenericExchs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "HeatExchanger:AirToAir:SensibleAndLatent");
        state.dataHeatRecovery->NumDesiccantBalancedExchs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "HeatExchanger:Desiccant:BalancedFlow");
        state.dataHeatRecovery->NumDesBalExchsPerfDataType1 =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1");
        state.dataHeatRecovery->NumHeatExchangers = state.dataHeatRecovery->NumAirToAirPlateExchs + state.dataHeatRecovery->NumAirToAirGenericExchs +
                                                    state.dataHeatRecovery->NumDesiccantBalancedExchs;

        // allocate the data array
        state.dataHeatRecovery->ExchCond.allocate(state.dataHeatRecovery->NumHeatExchangers);
        state.dataHeatRecovery->HeatExchangerUniqueNames.reserve(state.dataHeatRecovery->NumHeatExchangers);
        state.dataHeatRecovery->CheckEquipName.dimension(state.dataHeatRecovery->NumHeatExchangers, true);
        state.dataHeatRecovery->HeatExchCondNumericFields.allocate(state.dataHeatRecovery->NumHeatExchangers);

        if (state.dataHeatRecovery->NumDesBalExchsPerfDataType1 > 0) {
            state.dataHeatRecovery->BalDesDehumPerfData.allocate(state.dataHeatRecovery->NumDesBalExchsPerfDataType1);
            state.dataHeatRecovery->BalDesDehumPerfNumericFields.allocate(state.dataHeatRecovery->NumDesBalExchsPerfDataType1);
        }

        // loop over the air to air plate heat exchangers and load their input data
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumAirToAirPlateExchs; ++ExchIndex) {
            cCurrentModuleObject = "HeatExchanger:AirToAir:FlatPlate";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     ExchIndex,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            ExchNum = ExchIndex;

            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames.allocate(NumNumbers);
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = "";
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = state.dataIPShortCut->cNumericFieldNames;

            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatRecovery->HeatExchangerUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            state.dataHeatRecovery->ExchCond(ExchNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum = HX_AIRTOAIR_FLATPLATE;
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                        " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                        '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
                if (SELECT_CASE_var == "COUNTERFLOW") {
                    state.dataHeatRecovery->ExchCond(ExchNum).FlowArr = HXConfiguration::CounterFlow;
                } else if (SELECT_CASE_var == "PARALLELFLOW") {
                    state.dataHeatRecovery->ExchCond(ExchNum).FlowArr = HXConfiguration::ParallelFlow;
                } else if (SELECT_CASE_var == "CROSSFLOWBOTHUNMIXED") {
                    state.dataHeatRecovery->ExchCond(ExchNum).FlowArr = HXConfiguration::CrossFlowBothUnmixed;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + ": incorrect flow arrangement: " + state.dataIPShortCut->cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(4));
                if (SELECT_CASE_var == "YES") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::Yes;
                } else if (SELECT_CASE_var == "NO") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::No;
                } else {
                    if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                        state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::Yes;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + ": incorrect econo lockout: " + state.dataIPShortCut->cAlphaArgs(4));
                        ErrorsFound = true;
                    }
                }
            }
            state.dataHeatRecovery->ExchCond(ExchNum).hARatio = state.dataIPShortCut->rNumericArgs(1);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirInTemp = state.dataIPShortCut->rNumericArgs(3);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirOutTemp = state.dataIPShortCut->rNumericArgs(4);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSecAirVolFlow = state.dataIPShortCut->rNumericArgs(5);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSecAirInTemp = state.dataIPShortCut->rNumericArgs(6);
            state.dataHeatRecovery->ExchCond(ExchNum).NomElecPower = state.dataIPShortCut->rNumericArgs(7);
            state.dataHeatRecovery->ExchCond(ExchNum).SupInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(5),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SupOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(6),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Primary,
                                                                                        ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SecInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(7),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Secondary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(8),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                        ObjectIsNotParent);

            TestCompSet(state,
                        cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum),
                        state.dataHeatRecovery->ExchCond(ExchNum).Name,
                        state.dataIPShortCut->cAlphaArgs(5),
                        state.dataIPShortCut->cAlphaArgs(6),
                        "Process Air Nodes");

        } // end of input loop over air to air plate heat exchangers

        // loop over the air to air generic heat exchangers and load their input data
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumAirToAirGenericExchs; ++ExchIndex) {
            cCurrentModuleObject = "HeatExchanger:AirToAir:SensibleAndLatent";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     ExchIndex,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            ExchNum = ExchIndex + state.dataHeatRecovery->NumAirToAirPlateExchs;

            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames.allocate(NumNumbers);
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = "";
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = state.dataIPShortCut->cNumericFieldNames;

            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatRecovery->HeatExchangerUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            state.dataHeatRecovery->ExchCond(ExchNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum = HX_AIRTOAIR_GENERIC;
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                        " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                        '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
            state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
            state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectSensible100 = state.dataIPShortCut->rNumericArgs(2);
            state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectLatent100 = state.dataIPShortCut->rNumericArgs(3);
            state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectSensible75 = state.dataIPShortCut->rNumericArgs(4);
            state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectLatent75 = state.dataIPShortCut->rNumericArgs(5);
            if (state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectSensible75 < state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectSensible100) {
                ShowWarningError(state,
                                 cCurrentModuleObject + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                                     "\" sensible heating effectiveness at 75% rated flow is less than at 100% rated flow.");
                ShowContinueError(state, "Sensible heating effectiveness at 75% rated flow is usually greater than at 100% rated flow.");
            }
            if (state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectLatent75 < state.dataHeatRecovery->ExchCond(ExchNum).HeatEffectLatent100) {
                ShowWarningError(state,
                                 cCurrentModuleObject + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                                     "\" latent heating effectiveness at 75% rated flow is less than at 100% rated flow.");
                ShowContinueError(state, "Latent heating effectiveness at 75% rated flow is usually greater than at 100% rated flow.");
            }
            state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectSensible100 = state.dataIPShortCut->rNumericArgs(6);
            state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectLatent100 = state.dataIPShortCut->rNumericArgs(7);
            state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectSensible75 = state.dataIPShortCut->rNumericArgs(8);
            state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectLatent75 = state.dataIPShortCut->rNumericArgs(9);
            if (state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectSensible75 < state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectSensible100) {
                ShowWarningError(state,
                                 cCurrentModuleObject + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                                     "\" sensible cooling effectiveness at 75% rated flow is less than at 100% rated flow.");
                ShowContinueError(state, "Sensible cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow.");
            }
            if (state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectLatent75 < state.dataHeatRecovery->ExchCond(ExchNum).CoolEffectLatent100) {
                ShowWarningError(state,
                                 cCurrentModuleObject + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                                     "\" latent cooling effectiveness at 75% rated flow is less than at 100% rated flow.");
                ShowContinueError(state, "Latent cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow.");
            }
            state.dataHeatRecovery->ExchCond(ExchNum).SupInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(3),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SupOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(4),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Primary,
                                                                                        ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SecInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(5),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Secondary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(6),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                        ObjectIsNotParent);

            state.dataHeatRecovery->ExchCond(ExchNum).NomElecPower = state.dataIPShortCut->rNumericArgs(10);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Yes")) {
                state.dataHeatRecovery->ExchCond(ExchNum).ControlToTemperatureSetPoint = true;
            } else {
                if (!UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "No")) {
                    ShowSevereError(state, "Rotary HX Speed Modulation or Plate Bypass for Temperature Control for ");
                    ShowContinueError(state, state.dataHeatRecovery->ExchCond(ExchNum).Name + " must be set to Yes or No");
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "Plate")) {
                state.dataHeatRecovery->ExchCond(ExchNum).ExchConfig = HXConfigurationType::Plate;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "Rotary")) {
                state.dataHeatRecovery->ExchCond(ExchNum).ExchConfig = HXConfigurationType::Rotary;
            } else {
                ShowSevereError(state, cCurrentModuleObject + " configuration not found= " + state.dataIPShortCut->cAlphaArgs(8));
                ShowContinueError(state, "HX configuration must be either Plate or Rotary");
                ErrorsFound = true;
            }

            // Added additional inputs for frost control
            state.dataHeatRecovery->ExchCond(ExchNum).FrostControlType = state.dataIPShortCut->cAlphaArgs(9);
            if (!UtilityRoutines::SameString(state.dataHeatRecovery->ExchCond(ExchNum).FrostControlType, "None")) {
                if (!UtilityRoutines::SameString(state.dataHeatRecovery->ExchCond(ExchNum).FrostControlType, "ExhaustOnly")) {
                    if (!UtilityRoutines::SameString(state.dataHeatRecovery->ExchCond(ExchNum).FrostControlType, "ExhaustAirRecirculation")) {
                        if (!UtilityRoutines::SameString(state.dataHeatRecovery->ExchCond(ExchNum).FrostControlType, "MinimumExhaustTemperature")) {
                            ShowSevereError(state,
                                            "Invalid Frost Control method for " + state.dataHeatRecovery->ExchCond(ExchNum).Name + " =  " +
                                                state.dataIPShortCut->cAlphaArgs(9));
                            ErrorsFound = true;
                        }
                    }
                }
            }

            if (!UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "None")) {
                state.dataHeatRecovery->ExchCond(ExchNum).ThresholdTemperature = state.dataIPShortCut->rNumericArgs(11);
                state.dataHeatRecovery->ExchCond(ExchNum).InitialDefrostTime = state.dataIPShortCut->rNumericArgs(12);
                state.dataHeatRecovery->ExchCond(ExchNum).RateofDefrostTimeIncrease = state.dataIPShortCut->rNumericArgs(13);
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(10));
                if (SELECT_CASE_var == "YES") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::Yes;
                } else if (SELECT_CASE_var == "NO") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::No;
                } else {
                    if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                        state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::Yes;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + ": incorrect econo lockout: " + state.dataIPShortCut->cAlphaArgs(10));
                        ErrorsFound = true;
                    }
                }
            }

            TestCompSet(state,
                        cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum),
                        state.dataHeatRecovery->ExchCond(ExchNum).Name,
                        state.dataIPShortCut->cAlphaArgs(3),
                        state.dataIPShortCut->cAlphaArgs(4),
                        "Process Air Nodes");
        } // end of input loop over air to air generic heat exchangers

        // loop over the desiccant balanced heat exchangers and load their input data
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumDesiccantBalancedExchs; ++ExchIndex) {
            cCurrentModuleObject = "HeatExchanger:Desiccant:BalancedFlow";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     ExchIndex,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            ExchNum = ExchIndex + state.dataHeatRecovery->NumAirToAirPlateExchs + state.dataHeatRecovery->NumAirToAirGenericExchs;

            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames.allocate(NumNumbers);
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = "";
            state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames = state.dataIPShortCut->cNumericFieldNames;

            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatRecovery->HeatExchangerUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            state.dataHeatRecovery->ExchCond(ExchNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum = HX_DESICCANT_BALANCED;
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (state.dataHeatRecovery->ExchCond(ExchNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                        " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                        '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
            // desiccant HX's usually refer to process and regeneration air streams
            // In this module, Sup = Regeneration nodes and Sec = Process nodes
            // regeneration air inlet and outlet nodes
            state.dataHeatRecovery->ExchCond(ExchNum).SupInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(3),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SupOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(4),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Primary,
                                                                                        ObjectIsNotParent);
            // process air inlet and outlet nodes
            state.dataHeatRecovery->ExchCond(ExchNum).SecInletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(5),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Secondary,
                                                                                       ObjectIsNotParent);
            state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(6),
                                                                                        ErrorsFound,
                                                                                        cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                        ObjectIsNotParent);

            // Set up the component set for the process side of the HX (Sec = Process)
            TestCompSet(state,
                        cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum),
                        state.dataHeatRecovery->ExchCond(ExchNum).Name,
                        state.dataLoopNodes->NodeID(state.dataHeatRecovery->ExchCond(ExchNum).SecInletNode),
                        state.dataLoopNodes->NodeID(state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode),
                        "Process Air Nodes");

            HeatExchPerfType = state.dataIPShortCut->cAlphaArgs(7);
            if (UtilityRoutines::SameString(HeatExchPerfType, "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1")) {
                state.dataHeatRecovery->ExchCond(ExchNum).HeatExchPerfTypeNum = BALANCEDHX_PERFDATATYPE1;
            } else {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name + "\"");
                ShowContinueError(state, "Invalid performance data type selected.");
                ShowContinueError(state, "...performance data type selected = " + HeatExchPerfType);
                ErrorsFound = true;
            }

            state.dataHeatRecovery->ExchCond(ExchNum).HeatExchPerfName = state.dataIPShortCut->cAlphaArgs(8);

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(9));
                if (SELECT_CASE_var == "YES") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::Yes;
                } else if (SELECT_CASE_var == "NO") {
                    state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::No;
                } else {
                    if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                        state.dataHeatRecovery->ExchCond(ExchNum).EconoLockOut = EconomizerLockout::No;
                    } else {
                        ShowSevereError(state, cCurrentModuleObject + ": incorrect econo lockout: " + state.dataIPShortCut->cAlphaArgs(9));
                        ErrorsFound = true;
                    }
                }
            }

        } // end of input loop over desiccant balanced heat exchangers

        // get performance data set for balanced desiccant heat exchanger

        for (PerfDataIndex = 1; PerfDataIndex <= state.dataHeatRecovery->NumDesBalExchsPerfDataType1; ++PerfDataIndex) {
            cCurrentModuleObject = "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     PerfDataIndex,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            PerfDataNum = PerfDataIndex;

            state.dataHeatRecovery->BalDesDehumPerfNumericFields(PerfDataNum).NumericFieldNames.allocate(NumNumbers);
            state.dataHeatRecovery->BalDesDehumPerfNumericFields(PerfDataNum).NumericFieldNames = "";
            state.dataHeatRecovery->BalDesDehumPerfNumericFields(PerfDataNum).NumericFieldNames = state.dataIPShortCut->cNumericFieldNames;

            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).PerfType = cCurrentModuleObject;
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomSupAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
            // check validity
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomSupAirVolFlow <= 0.0 &&
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomSupAirVolFlow != DataSizing::AutoSize) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Nominal air flow rate must be greater than zero.");
                ShowContinueError(state,
                                  format("... value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomSupAirVolFlow));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomProcAirFaceVel = state.dataIPShortCut->rNumericArgs(2);
            // check validity
            if ((state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomProcAirFaceVel <= 0.0 &&
                 state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomProcAirFaceVel != DataSizing::AutoSize) ||
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomProcAirFaceVel > 6.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Nominal air face velocity cannot be less than or equal to zero or greater than 6 m/s.");
                ShowContinueError(state,
                                  format("... value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomProcAirFaceVel));
                ErrorsFound = true;
            }
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomElecPower = state.dataIPShortCut->rNumericArgs(3);
            // check validity
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomElecPower < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Nominal electric power cannot be less than zero.");
                ShowContinueError(state, format("... value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).NomElecPower));
                ErrorsFound = true;
            }

            // regen outlet temp variables
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B1 = state.dataIPShortCut->rNumericArgs(4);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B2 = state.dataIPShortCut->rNumericArgs(5);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B3 = state.dataIPShortCut->rNumericArgs(6);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B4 = state.dataIPShortCut->rNumericArgs(7);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B5 = state.dataIPShortCut->rNumericArgs(8);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B6 = state.dataIPShortCut->rNumericArgs(9);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B7 = state.dataIPShortCut->rNumericArgs(10);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).B8 = state.dataIPShortCut->rNumericArgs(11);

            //     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInHumRat = state.dataIPShortCut->rNumericArgs(12);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInHumRat = state.dataIPShortCut->rNumericArgs(13);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInHumRat >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInHumRat) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air humidity ratio must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInHumRat));
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInHumRat < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air humidity ratio must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInHumRat > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1.");
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInHumRat));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInTemp = state.dataIPShortCut->rNumericArgs(14);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInTemp = state.dataIPShortCut->rNumericArgs(15);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInTemp >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInTemp) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air temperature must be less than the maximum.");
                ShowContinueError(
                    state,
                    format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInTemp));
                ShowContinueError(
                    state,
                    format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInTemp));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInHumRat = state.dataIPShortCut->rNumericArgs(16);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInHumRat = state.dataIPShortCut->rNumericArgs(17);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInHumRat >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInHumRat) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of process inlet air humidity ratio must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInHumRat));
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInHumRat < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of process inlet air humidity ratio must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInHumRat > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the maximum value of process inlet air humidity ratio must be less than or equal to 1.");
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInHumRat));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInTemp = state.dataIPShortCut->rNumericArgs(18);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInTemp = state.dataIPShortCut->rNumericArgs(19);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInTemp >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInTemp) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of process inlet air temperature must be less than the maximum.");
                ShowContinueError(
                    state, format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInTemp));
                ShowContinueError(
                    state, format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInTemp));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinFaceVel = state.dataIPShortCut->rNumericArgs(20);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxFaceVel = state.dataIPShortCut->rNumericArgs(21);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinFaceVel >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxFaceVel) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regen air velocity must be less than the maximum.");
                ShowContinueError(
                    state, format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinFaceVel));
                ShowContinueError(
                    state, format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxFaceVel));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutTemp = state.dataIPShortCut->rNumericArgs(22);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutTemp = state.dataIPShortCut->rNumericArgs(23);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutTemp >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutTemp) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regen outlet air temperature must be less than the maximum.");
                ShowContinueError(
                    state, format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutTemp));
                ShowContinueError(
                    state, format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutTemp));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInRelHum = state.dataIPShortCut->rNumericArgs(24) / 100.0;
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInRelHum = state.dataIPShortCut->rNumericArgs(25) / 100.0;
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInRelHum >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInRelHum) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regen inlet air relative humidity must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInRelHum * 100.0));
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInRelHum < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of regen inlet air relative humidity must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInRelHum > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the maximum value of regen inlet air relative humidity must be less than or equal to 100.");
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInRelHum = state.dataIPShortCut->rNumericArgs(26) / 100.0;
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInRelHum = state.dataIPShortCut->rNumericArgs(27) / 100.0;
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInRelHum >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInRelHum) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of process inlet air relative humidity must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInRelHum * 100.0));
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInRelHum < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the minimum value of process inlet air relative humidity must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MinProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInRelHum > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air temperature equation.");
                ShowContinueError(state, "... the maximum value of process inlet air relative humidity must be less than or equal to 100.");
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).T_MaxProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }

            // regen outlet humidity ratio variables
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C1 = state.dataIPShortCut->rNumericArgs(28);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C2 = state.dataIPShortCut->rNumericArgs(29);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C3 = state.dataIPShortCut->rNumericArgs(30);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C4 = state.dataIPShortCut->rNumericArgs(31);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C5 = state.dataIPShortCut->rNumericArgs(32);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C6 = state.dataIPShortCut->rNumericArgs(33);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C7 = state.dataIPShortCut->rNumericArgs(34);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).C8 = state.dataIPShortCut->rNumericArgs(35);

            //     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInHumRat = state.dataIPShortCut->rNumericArgs(36);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInHumRat = state.dataIPShortCut->rNumericArgs(37);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInHumRat >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInHumRat) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air humidity ratio must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInHumRat));
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInHumRat < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air humidity ratio must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInHumRat > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1.");
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInHumRat));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInTemp = state.dataIPShortCut->rNumericArgs(38);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInTemp = state.dataIPShortCut->rNumericArgs(39);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInTemp >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInTemp) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regeneration inlet air temperature must be less than the maximum.");
                ShowContinueError(
                    state,
                    format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInTemp));
                ShowContinueError(
                    state,
                    format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInTemp));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInHumRat = state.dataIPShortCut->rNumericArgs(40);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInHumRat = state.dataIPShortCut->rNumericArgs(41);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInHumRat >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInHumRat) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of process inlet air humidity ratio must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInHumRat));
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInHumRat < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of process inlet air humidity ratio must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInHumRat > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the maximum value of process inlet air humidity ratio must be less than or equal to 1.");
                ShowContinueError(state,
                                  format("... maximum value entered by user = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInHumRat));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInTemp = state.dataIPShortCut->rNumericArgs(42);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInTemp = state.dataIPShortCut->rNumericArgs(43);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInTemp >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInTemp) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of process inlet air temperature must be less than the maximum.");
                ShowContinueError(
                    state, format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInTemp));
                ShowContinueError(
                    state, format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInTemp));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinFaceVel = state.dataIPShortCut->rNumericArgs(44);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxFaceVel = state.dataIPShortCut->rNumericArgs(45);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinFaceVel >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxFaceVel) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regen air velocity must be less than the maximum.");
                ShowContinueError(
                    state, format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinFaceVel));
                ShowContinueError(
                    state, format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxFaceVel));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutHumRat = state.dataIPShortCut->rNumericArgs(46);
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutHumRat = state.dataIPShortCut->rNumericArgs(47);
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutHumRat >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutHumRat) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regen outlet air humidity ratio must be less than the maximum.");
                ShowContinueError(
                    state,
                    format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutHumRat));
                ShowContinueError(
                    state,
                    format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutHumRat < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regen outlet air humidity ratio must be greater than or equal to 0.");
                ShowContinueError(
                    state,
                    format("... minimum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MinRegenAirOutHumRat));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutHumRat > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the maximum value of regen outlet air humidity ratio must be less or equal to 1.");
                ShowContinueError(
                    state,
                    format("... maximum value entered = {:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).MaxRegenAirOutHumRat));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInRelHum = state.dataIPShortCut->rNumericArgs(48) / 100.0;
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInRelHum = state.dataIPShortCut->rNumericArgs(49) / 100.0;
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInRelHum >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInRelHum) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regen inlet air relative humidity must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInRelHum * 100.0));
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInRelHum < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of regen inlet air relative humidity must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInRelHum > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the maximum value of regen inlet air relative humidity must be less or equal to 100.");
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxRegenAirInRelHum * 100.0));
                ErrorsFound = true;
            }

            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInRelHum = state.dataIPShortCut->rNumericArgs(50) / 100.0;
            state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInRelHum = state.dataIPShortCut->rNumericArgs(51) / 100.0;
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInRelHum >=
                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInRelHum) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min/max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of process inlet air relative humidity must be less than the maximum.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInRelHum * 100.0));
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInRelHum < 0.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in min boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the minimum value of process inlet air relative humidity must be greater than or equal to 0.");
                ShowContinueError(state,
                                  format("... minimum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MinProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }
            if (state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInRelHum > 1.0) {
                ShowSevereError(state, cCurrentModuleObject + " \"" + state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name + "\"");
                ShowContinueError(state, "Error found in max boundary for the regen outlet air humidity ratio equation.");
                ShowContinueError(state, "... the maximum value of process inlet air relative humidity must be less than or equal to 100.");
                ShowContinueError(state,
                                  format("... maximum value entered = {:.6R}",
                                         state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).H_MaxProcAirInRelHum * 100.0));
                ErrorsFound = true;
            }
        }
        // getting performance data set for balanced desiccant heat exchanger ends

        // match desiccant heat exchanger index to performance data index
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumDesiccantBalancedExchs; ++ExchIndex) {
            ExchNum = ExchIndex + state.dataHeatRecovery->NumAirToAirPlateExchs + state.dataHeatRecovery->NumAirToAirGenericExchs;
            for (PerfDataNum = 1; PerfDataNum <= state.dataHeatRecovery->NumDesBalExchsPerfDataType1; ++PerfDataNum) {
                if (UtilityRoutines::SameString(state.dataHeatRecovery->ExchCond(ExchNum).HeatExchPerfName,
                                                state.dataHeatRecovery->BalDesDehumPerfData(PerfDataNum).Name)) {
                    state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex = PerfDataNum;
                    break;
                }
            }
            if (state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex == 0) {
                ShowSevereError(state,
                                cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum) + " \"" +
                                    state.dataHeatRecovery->ExchCond(ExchNum).Name + "\"");
                ShowContinueError(state, "... Performance data set not found = " + state.dataHeatRecovery->ExchCond(ExchNum).HeatExchPerfName);
                ErrorsFound = true;
            } else {
                if (!ErrorsFound) {
                    state.dataHeatRecovery->ExchCond(ExchNum).FaceArea =
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).NomSupAirVolFlow /
                        (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).NomProcAirFaceVel);
                }
            }
        }
        // matching done

        // setup common report variables for heat exchangers
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumHeatExchangers; ++ExchIndex) {
            ExchNum = ExchIndex;
            // CurrentModuleObject='HeatExchanger:AirToAir:FlatPlate/AirToAir:SensibleAndLatent/Desiccant:BalancedFlow')
            SetupOutputVariable(state,
                                "Heat Exchanger Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).SensHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).SensHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).LatHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).LatHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).TotHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).TotHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEAT RECOVERY FOR HEATING",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heat Exchanger Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).SensCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).SensCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).LatCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).LatCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).TotCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).TotCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEAT RECOVERY FOR COOLING",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Heat Exchanger Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatRecovery->ExchCond(ExchNum).ElecUseRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatRecovery->ExchCond(ExchNum).ElecUseEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name,
                                _,
                                "ELECTRICITY",
                                "HEATRECOVERY",
                                _,
                                "System");
        }

        // setup additional report variables for generic heat exchangers
        for (ExchIndex = 1; ExchIndex <= state.dataHeatRecovery->NumAirToAirGenericExchs; ++ExchIndex) {
            // generic heat exchangers are read in after flat plate heat exchanger objects (index needs to be set correctly)
            // CurrentModuleObject=HeatExchanger:AirToAir:SensibleAndLatent
            ExchNum = ExchIndex + state.dataHeatRecovery->NumAirToAirPlateExchs;
            SetupOutputVariable(state,
                                "Heat Exchanger Sensible Effectiveness",
                                OutputProcessor::Unit::None,
                                state.dataHeatRecovery->ExchCond(ExchNum).SensEffectiveness,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Latent Effectiveness",
                                OutputProcessor::Unit::None,
                                state.dataHeatRecovery->ExchCond(ExchNum).LatEffectiveness,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Supply Air Bypass Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatRecovery->ExchCond(ExchNum).SupBypassMassFlow,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Exhaust Air Bypass Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatRecovery->ExchCond(ExchNum).SecBypassMassFlow,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
            SetupOutputVariable(state,
                                "Heat Exchanger Defrost Time Fraction",
                                OutputProcessor::Unit::None,
                                state.dataHeatRecovery->ExchCond(ExchNum).DefrostFraction,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatRecovery->ExchCond(ExchNum).Name);
        }

        if (ErrorsFound) {
            ShowFatalError(state, format("{}Errors found in input.  Program terminates.", RoutineName));
        }
    }

    void InitHeatRecovery(EnergyPlusData &state,
                          int const ExchNum, // number of the current heat exchanger being simulated
                          int const CompanionCoilIndex,
                          int const CompanionCoilType_Num)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003
        //                      B Griffith May 2009, EMS setpoint check
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Heat Recovery Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        //  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
        using EMSManager::CheckIfNodeSetPointManagedByEMS;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ExIndex;   // do loop index
        int SupInNode; // supply air inlet node number
        int SecInNode; // secondary air inlet node number
        Real64 CMin0;  // minimum capacity flow
        Real64 CMax0;  // maximum capacity flow
        Real64 Eps0;   // effectiveness at rated conditions
        Real64 NTU0;   // NTU at rated conditions
        Real64 RhoAir; // air density at outside pressure & standard temperature and humidity
        Real64 CpAir;  // heat capacity of air
        // of humidity ratio and temperature
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool state.dataHeatRecovery->MyOneTimeAllocate( true );
        ////////////////////////////////////////////////////////////////////////////////////
        int ErrStat;            // error status returned by CalculateNTUfromEpsAndZ
        bool FatalError;        // fatal error flag
        bool LocalWarningError; // warning error flag
        Real64 Z;               // Min/max flow ratio
        //  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items

        if (state.dataHeatRecovery->MyOneTimeAllocate) {
            state.dataHeatRecovery->MySetPointTest.allocate(state.dataHeatRecovery->NumHeatExchangers);
            state.dataHeatRecovery->MySizeFlag.allocate(state.dataHeatRecovery->NumHeatExchangers);
            state.dataHeatRecovery->MySetPointTest = true;
            state.dataHeatRecovery->MySizeFlag = true;
            state.dataHeatRecovery->MyOneTimeAllocate = false;
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataHeatRecovery->MySizeFlag(ExchNum)) {

            SizeHeatRecovery(state, ExchNum);
            state.dataHeatRecovery->MySizeFlag(ExchNum) = false;
        }

        FatalError = false;
        LocalWarningError = false;

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataHeatRecovery->ExchCond(ExchNum).myEnvrnFlag) {
            // I believe that all of these initializations should be taking place at the SCFM conditions
            RhoAir = state.dataEnvrn->StdRhoAir;
            //    RhoAir = PsyRhoAirFnPbTdbW(101325.0,20.0,0.0)  do we want standard air density at sea level for generic ERVs per ARI 1060?
            CpAir = PsyCpAirFnW(0.0);

            ExIndex = ExchNum; // this replaces the loop that went over multiple at once

            {
                auto const SELECT_CASE_var(state.dataHeatRecovery->ExchCond(ExIndex).ExchTypeNum);

                if (SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE) {

                    state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow = RhoAir * state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirVolFlow;
                    state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow = RhoAir * state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirVolFlow;
                    // Note: the capacity stream is here simply the mass flow
                    //       since the thermal capacity can be assumed to be
                    //       equal for both streams
                    if (state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow > state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow) {
                        CMin0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow;
                        CMax0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow;
                    } else {
                        CMin0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow;
                        CMax0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow;
                    }

                    Eps0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow *
                           SafeDiv(state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirOutTemp -
                                       state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirInTemp,
                                   CMin0 * (state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirInTemp -
                                            state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirInTemp));
                    Z = CMin0 / CMax0;

                    ErrStat = 0;
                    CalculateNTUfromEpsAndZ(state, NTU0, ErrStat, Z, state.dataHeatRecovery->ExchCond(ExIndex).FlowArr, Eps0);

                    if (ErrStat == 1) {

                        FatalError = true;
                        ShowSevereError(state, "In the HeatExchanger:AirToAir:FlatPlate component " + state.dataHeatRecovery->ExchCond(ExIndex).Name);
                        ShowContinueError(state, "  the mass flow ratio is out of bounds");
                        ShowContinueError(state, format("The mass flow ratio is (Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) = {:.2R}", Z));
                        ShowContinueError(state, "The mass flow ratio should be >= 0.0 and <= 1.0");
                        ShowContinueError(state,
                                          format("Min_Mass_Flow_Rate = {:.2R} [air density] * {:.1R} [Min_Vol_Flow_Rate]",
                                                 RhoAir,
                                                 min(state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirVolFlow,
                                                     state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirVolFlow)));
                        ShowContinueError(state,
                                          format("Max_Mass_Flow_Rate = {:.2R} [air density] * {:.1R} [Max_Vol_Flow_Rate]",
                                                 RhoAir,
                                                 max(state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirVolFlow,
                                                     state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirVolFlow)));
                    } else if (ErrStat == 2) {
                        FatalError = true;
                        ShowSevereError(state, "In the HeatExchanger:AirToAir:FlatPlate component " + state.dataHeatRecovery->ExchCond(ExIndex).Name);
                        ShowContinueError(state, "  the calculated nominal effectiveness is out of bounds");
                        ShowContinueError(state, format("The effectiveness is {:.3R}", Eps0));
                        ShowContinueError(state, format("The effectiveness should be >= 0.0 and <= {:.3R}", 1.0 / (1.0 + Z)));
                        ShowContinueError(
                            state, "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)");
                        ShowContinueError(state, "The temperatures are user inputs. The mass flow rates are user input volume flow rates");
                        ShowContinueError(state, format("  times the density of air [{:.2R} kg/m3]", RhoAir));
                        ShowContinueError(state, "Change these inputs to obtain a physically realizable heat exchanger effectiveness");
                    } else if (ErrStat == 3) {
                        FatalError = true;
                        ShowSevereError(state, "In the HeatExchanger:AirToAir:FlatPlate component " + state.dataHeatRecovery->ExchCond(ExIndex).Name);
                        ShowContinueError(state, "  the calculated nominal effectiveness is out of bounds");
                        ShowContinueError(state, format("The effectiveness is {:.3R}", Eps0));
                        ShowContinueError(state, format("The effectiveness should be >= 0.0 and <= {:.3R}", (1.0 - std::exp(-Z)) / Z));
                        ShowContinueError(
                            state, "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)");
                        ShowContinueError(state, "The temperatures are user inputs. The mass flow rates are user input volume flow rates");
                        ShowContinueError(state, format("  times the density of air [{:.2R} kg/m3]", RhoAir));
                        ShowContinueError(state, "Change these inputs to obtain a physically realizable heat exchanger effectiveness");
                    } else if (ErrStat == 4) {
                        FatalError = true;
                        ShowSevereError(state, "In the HeatExchanger:AirToAir:FlatPlate component " + state.dataHeatRecovery->ExchCond(ExIndex).Name);
                        ShowContinueError(state, "  the quantity Eff_nom*(Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) is out of bounds");
                        ShowContinueError(state, format("The value is {:.3R}", Eps0 * Z));
                        ShowContinueError(state, format("The value should be >= 0.0 and <= {:.3R}", 1.0 - std::exp(Z * (SMALL - 1.0))));
                        ShowContinueError(
                            state,
                            "Eff_nom = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate) * (T_nom_sup_out - T_nom_sup_in)/(T_nom_sec_in - T_nom_sup_in)");
                        ShowContinueError(state, "The temperatures are user inputs. The mass flow rates are user input volume flow rates");
                        ShowContinueError(state, format("  times the density of air [{:.2R} kg/m3]", RhoAir));
                        ShowContinueError(state,
                                          "Change these inputs to obtain a physically realizable product of effectiveness times min/max mass ratio "
                                          "for this heat exchanger");
                    } else if (ErrStat == 5) {
                        FatalError = true;
                        ShowSevereError(state, "In the HeatExchanger:AirToAir:FlatPlate component " + state.dataHeatRecovery->ExchCond(ExIndex).Name);
                        ShowContinueError(state, "  the calculated nominal effectiveness is out of bounds");
                        ShowContinueError(state, format("The effectiveness is {:.3R}", Eps0));
                        ShowContinueError(state, "The effectiveness should be >= 0.0 and <= 1.0");
                        ShowContinueError(
                            state, "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)");
                        ShowContinueError(state, "The temperatures are user inputs. The mass flow rates are user input volume flow rates");
                        ShowContinueError(state, format("  times the density of air [{:.2R} kg/m3]", RhoAir));
                        ShowContinueError(state, "Change these inputs to obtain a physically realizable heat exchanger effectiveness");
                    }

                    if (FatalError) {
                        ShowFatalError(state, "Heat exchanger design calculation caused fatal error: program terminated.");
                    }

                    state.dataHeatRecovery->ExchCond(ExIndex).UA0 = NTU0 * CMin0 * CpAir;
                    state.dataHeatRecovery->ExchCond(ExIndex).mTSup0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow *
                                                                       (state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirInTemp + KELVZERO);
                    state.dataHeatRecovery->ExchCond(ExIndex).mTSec0 = state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow *
                                                                       (state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirInTemp + KELVZERO);

                    // check validity
                    if (state.dataHeatRecovery->ExchCond(ExIndex).NomSupAirMassFlow * state.dataHeatRecovery->ExchCond(ExIndex).NomSecAirMassFlow <
                        SmallMassFlow * SmallMassFlow) {
                        ShowFatalError(state, "Mass flow in HeatExchanger:AirToAir:FlatPlate too small in initialization.");
                    }

                    if (state.dataHeatRecovery->ExchCond(ExIndex).mTSup0 < SmallMassFlow) {
                        ShowFatalError(state, "(m*T)Sup,in in HeatExchanger:AirToAir:FlatPlate too small in initialization.");
                    }

                    if (state.dataHeatRecovery->ExchCond(ExIndex).mTSec0 < SmallMassFlow) {
                        ShowFatalError(state, "(m*T)Sec,in in HeatExchanger:AirToAir:FlatPlate too small in initialization.");
                    }

                    if (CMin0 < SmallMassFlow) {
                        ShowFatalError(state, "CMin0 in HeatExchanger:AirToAir:FlatPlate too small in initialization.");
                    }

                } else if (SELECT_CASE_var == HX_AIRTOAIR_GENERIC) {

                    if (state.dataHeatRecovery->ExchCond(ExIndex).SupOutletNode > 0 &&
                        state.dataHeatRecovery->ExchCond(ExIndex).ControlToTemperatureSetPoint) {
                        if (state.dataLoopNodes->Node(state.dataHeatRecovery->ExchCond(ExIndex).SupOutletNode).TempSetPoint == SensedNodeFlagValue) {
                            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                ShowSevereError(state,
                                                "Missing temperature setpoint for " +
                                                    cHXTypes(state.dataHeatRecovery->ExchCond(ExIndex).ExchTypeNum) + " \"" +
                                                    state.dataHeatRecovery->ExchCond(ExIndex).Name + "\" :");
                                ShowContinueError(
                                    state, "  use a Setpoint Manager to establish a setpoint at the supply air outlet node of the Heat Exchanger.");
                                ShowFatalError(state, " Previous condition causes program termination.");
                            } else {
                                // need call to EMS to check node
                                CheckIfNodeSetPointManagedByEMS(state,
                                                                state.dataHeatRecovery->ExchCond(ExIndex).SupOutletNode,
                                                                EMSManager::SPControlType::iTemperatureSetPoint,
                                                                FatalError);
                                if (FatalError) {
                                    ShowSevereError(state,
                                                    "Missing temperature setpoint for " +
                                                        cHXTypes(state.dataHeatRecovery->ExchCond(ExIndex).ExchTypeNum) + " \"" +
                                                        state.dataHeatRecovery->ExchCond(ExIndex).Name + "\" :");
                                    ShowContinueError(
                                        state,
                                        "  use a Setpoint Manager to establish a setpoint at the supply air outlet node of the Heat Exchanger.");
                                    ShowContinueError(
                                        state,
                                        "  or use an EMS actuator to establish a setpoint at the supply air outlet node of the Heat Exchanger.");
                                    ShowFatalError(state, " Previous condition causes program termination.");
                                }
                            }
                        }
                    }

                } else if (SELECT_CASE_var == HX_DESICCANT_BALANCED) {

                } else {
                    //       Will never get here
                }
            }

            state.dataHeatRecovery->ExchCond(ExchNum).myEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataHeatRecovery->ExchCond(ExchNum).myEnvrnFlag = true;
        }

        // Do these initializations every time step
        SupInNode = state.dataHeatRecovery->ExchCond(ExchNum).SupInletNode;
        SecInNode = state.dataHeatRecovery->ExchCond(ExchNum).SecInletNode;

        // Get information from inlet nodes

        state.dataHeatRecovery->ExchCond(ExchNum).SupInTemp = state.dataLoopNodes->Node(SupInNode).Temp;
        state.dataHeatRecovery->ExchCond(ExchNum).SupInHumRat = state.dataLoopNodes->Node(SupInNode).HumRat;
        state.dataHeatRecovery->ExchCond(ExchNum).SupInEnth = state.dataLoopNodes->Node(SupInNode).Enthalpy;
        state.dataHeatRecovery->ExchCond(ExchNum).SupInMassFlow = state.dataLoopNodes->Node(SupInNode).MassFlowRate;
        state.dataHeatRecovery->ExchCond(ExchNum).SecInTemp = state.dataLoopNodes->Node(SecInNode).Temp;
        state.dataHeatRecovery->ExchCond(ExchNum).SecInHumRat = state.dataLoopNodes->Node(SecInNode).HumRat;
        state.dataHeatRecovery->ExchCond(ExchNum).SecInEnth = state.dataLoopNodes->Node(SecInNode).Enthalpy;
        state.dataHeatRecovery->ExchCond(ExchNum).SecInMassFlow = state.dataLoopNodes->Node(SecInNode).MassFlowRate;

        // initialize the output variables
        state.dataHeatRecovery->ExchCond(ExchNum).SensHeatingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SensHeatingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).LatHeatingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).LatHeatingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).TotHeatingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).TotHeatingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SensCoolingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SensCoolingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).LatCoolingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).LatCoolingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).TotCoolingRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).TotCoolingEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).ElecUseRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).ElecUseEnergy = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SensEffectiveness = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).LatEffectiveness = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SupBypassMassFlow = 0.0;
        state.dataHeatRecovery->ExchCond(ExchNum).SecBypassMassFlow = 0.0;

        //  Initialize inlet conditions

        {
            auto const SELECT_CASE_var(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum);

            if (SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE) {

            } else if (SELECT_CASE_var == HX_AIRTOAIR_GENERIC) {

            } else if (SELECT_CASE_var == HX_DESICCANT_BALANCED) {

                if (state.dataHeatRecovery->MySetPointTest(ExchNum)) {
                    if (!state.dataGlobal->SysSizingCalc && state.dataHVACGlobal->DoSetPointTest) {
                        if (!state.dataHeatRecovery->CalledFromParentObject) {
                            if (state.dataLoopNodes->Node(state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode).HumRatMax == SensedNodeFlagValue) {
                                if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                    ShowWarningError(state,
                                                     "Missing optional HumRatMax setpoint for " +
                                                         cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum) + " \"" +
                                                         state.dataHeatRecovery->ExchCond(ExchNum).Name + "\"");
                                    ShowContinueError(state,
                                                      "...the simulation will continue without control of the desiccant heat exchanger to a maximum "
                                                      "humidity ratio setpoint.");
                                    ShowContinueError(state,
                                                      "...use a Setpoint Manager to establish a setpoint at the process air outlet node of the "
                                                      "desiccant Heat Exchanger if control is desired.");
                                } else {
                                    // need call to EMS to check node
                                    CheckIfNodeSetPointManagedByEMS(state,
                                                                    state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode,
                                                                    EMSManager::SPControlType::iHumidityRatioMaxSetPoint,
                                                                    LocalWarningError);
                                    state.dataLoopNodes->NodeSetpointCheck(state.dataHeatRecovery->ExchCond(ExchNum).SecOutletNode)
                                        .needsSetpointChecking = false;
                                    if (LocalWarningError) {
                                        ShowWarningError(state,
                                                         "Missing optional HumRatMax setpoint for " +
                                                             cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum) + " \"" +
                                                             state.dataHeatRecovery->ExchCond(ExchNum).Name + "\"");
                                        ShowContinueError(state,
                                                          "...the simulation will continue without control of the desiccant heat exchanger to a "
                                                          "maximum humidity ratio setpoint.");
                                        ShowContinueError(state,
                                                          "...use a Setpoint Manager to establish a setpoint at the process air outlet node of the "
                                                          "desiccant Heat Exchanger if control is desired.");
                                        ShowContinueError(state,
                                                          "...or use an EMS Actuator to establish a maximum humidity ratio setpoint at the process "
                                                          "air outlet node of the desiccant Heat Exchanger if control is desired.");
                                    }
                                }
                            }
                        }
                        state.dataHeatRecovery->MySetPointTest(ExchNum) = false;
                    }
                }

                if (CompanionCoilIndex > 0) {

                    if (CompanionCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        CompanionCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        if (state.dataDXCoils->DXCoilFullLoadOutAirTemp(CompanionCoilIndex) == 0.0 ||
                            state.dataDXCoils->DXCoilFullLoadOutAirHumRat(CompanionCoilIndex) == 0.0) {
                            //       DX Coil is OFF, read actual inlet conditions
                            state.dataHeatRecovery->FullLoadOutAirTemp = state.dataHeatRecovery->ExchCond(ExchNum).SecInTemp;
                            state.dataHeatRecovery->FullLoadOutAirHumRat = state.dataHeatRecovery->ExchCond(ExchNum).SecInHumRat;
                        } else {
                            //       DX Coil is ON, read full load DX coil outlet conditions (conditions HX sees when ON)
                            state.dataHeatRecovery->FullLoadOutAirTemp = state.dataDXCoils->DXCoilFullLoadOutAirTemp(CompanionCoilIndex);
                            state.dataHeatRecovery->FullLoadOutAirHumRat = state.dataDXCoils->DXCoilFullLoadOutAirHumRat(CompanionCoilIndex);
                        }
                    } else if (CompanionCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        // how to support VS dx coil here?
                        state.dataHeatRecovery->FullLoadOutAirTemp = state.dataVariableSpeedCoils->VarSpeedCoil(CompanionCoilIndex).OutletAirDBTemp;
                        state.dataHeatRecovery->FullLoadOutAirHumRat = state.dataVariableSpeedCoils->VarSpeedCoil(CompanionCoilIndex).OutletAirHumRat;
                    }

                } else {

                    //     HX only (not used in conjunction with DX coil), read inlet conditions
                    state.dataHeatRecovery->FullLoadOutAirTemp = state.dataHeatRecovery->ExchCond(ExchNum).SecInTemp;
                    state.dataHeatRecovery->FullLoadOutAirHumRat = state.dataHeatRecovery->ExchCond(ExchNum).SecInHumRat;
                }

            } else {
                //   Will never get here
            }
        }
    }

    void SizeHeatRecovery(EnergyPlusData &state, int const ExchNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2007
        //       MODIFIED       February 2014 Daeho Kang, enable sizing multiple HX types and add additional sizing fields
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Heat Exchanger components for which flow rates have not been
        // specified in the input. Currently, only nominal supply air flow rate for the generic HX can be autosized.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the system or OA system sizing arrays

        // Using/Aliasing
        using namespace DataSizing;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr const char *RoutineName("SizeHeatRecovery");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool PrintFlag;           // true when sizing information is reported in the eio file
        int BalDesDehumPerfIndex; // index of dehum performance data1 object
        int SizingMethod;         // integer representation of sizing method (e.g., CoolingAirflowSizing, HeatingCapacitySizing, etc.)
        int FieldNum;             // IDD numeric field index where input field description is found
        Real64 TempSize;          // autosized value of coil input field
        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

        state.dataSize->HRFlowSizingFlag = true;
        PrintFlag = true;
        FieldNum = 0;
        if (state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum == HX_DESICCANT_BALANCED) {
            PrintFlag = false;
        } else if (state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum == HX_AIRTOAIR_GENERIC) {
            FieldNum = 1;
        } else if (state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum == HX_AIRTOAIR_FLATPLATE) {
            FieldNum = 2;
        } else {
            assert(0);
        }

        CompName = state.dataHeatRecovery->ExchCond(ExchNum).Name;
        CompType = cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum);
        if (FieldNum > 0) {
            SizingString = state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames(FieldNum) + " [m3/s]";
        } else {
            SizingString = "Nominal Supply Air Flow Rate [m3/s]"; // desiccant balanced flow does not have an input for air volume flow rate
        }
        if (state.dataSize->CurZoneEqNum > 0) {
            if (state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow == AutoSize) {
                SizingMethod = AutoCalculateSizing;
                if (ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent) {
                    // Heat recovery heat exchanger in zoneHVAC equipment should have been sized to OA flow in the parent equipment
                    state.dataSize->DataConstantUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                } else {
                    state.dataSize->DataConstantUsedForSizing =
                        std::max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                                 state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);
                }
                state.dataSize->DataFractionUsedForSizing = 1.0;
            } else {
                if (state.dataSize->ZoneSizingRunDone) {
                    SizingMethod = AutoCalculateSizing;
                    if (ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent) {
                        // Heat recovery heat exchanger in zoneHVAC equipment should have been sized to OA flow in the parent equipment
                        state.dataSize->DataConstantUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    } else {
                        state.dataSize->DataConstantUsedForSizing =
                            std::max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                                     state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);
                    }
                    state.dataSize->DataFractionUsedForSizing = 1.0;
                }
            }
        }
        TempSize = state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow;
        bool errorsFound = false;
        SystemAirFlowSizer sizerSystemAirFlow;
        sizerSystemAirFlow.overrideSizingString(SizingString);
        // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
        sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
        state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow = sizerSystemAirFlow.size(state, TempSize, errorsFound);
        state.dataSize->DataConstantUsedForSizing = 0.0;
        state.dataSize->DataFractionUsedForSizing = 0.0;
        if (state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum == HX_AIRTOAIR_FLATPLATE) {
            PrintFlag = true;
            FieldNum = 5;
            CompName = state.dataHeatRecovery->ExchCond(ExchNum).Name;
            CompType = cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum);
            SizingString = state.dataHeatRecovery->HeatExchCondNumericFields(ExchNum).NumericFieldNames(FieldNum) + " [m3/s]";
            if (state.dataHeatRecovery->ExchCond(ExchNum).NomSecAirVolFlow == AutoSize) {
                state.dataSize->DataConstantUsedForSizing = state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow;
                state.dataSize->DataFractionUsedForSizing = 1.0;
            } else {
                if (state.dataSize->ZoneSizingRunDone || state.dataSize->SysSizingRunDone) {
                    state.dataSize->DataConstantUsedForSizing = state.dataHeatRecovery->ExchCond(ExchNum).NomSupAirVolFlow;
                    state.dataSize->DataFractionUsedForSizing = 1.0;
                }
            }
            TempSize = state.dataHeatRecovery->ExchCond(ExchNum).NomSecAirVolFlow;
            bool errorsFound = false;
            SystemAirFlowSizer sizerSystemAirFlow2;
            sizerSystemAirFlow2.overrideSizingString(SizingString);
            // sizerSystemAirFlow2.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizerSystemAirFlow2.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataHeatRecovery->ExchCond(ExchNum).NomSecAirVolFlow = sizerSystemAirFlow2.size(state, TempSize, errorsFound);
            state.dataSize->DataConstantUsedForSizing = 0.0;
            state.dataSize->DataFractionUsedForSizing = 0.0;
        }
        state.dataSize->HRFlowSizingFlag = false;
        if (state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum == HX_DESICCANT_BALANCED &&
            state.dataHeatRecovery->ExchCond(ExchNum).HeatExchPerfTypeNum == BALANCEDHX_PERFDATATYPE1) {

            BalDesDehumPerfIndex = state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex;

            FieldNum = 1;
            PrintFlag = true;
            CompName = state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).Name;
            CompType = state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).PerfType;
            SizingString = state.dataHeatRecovery->BalDesDehumPerfNumericFields(BalDesDehumPerfIndex).NumericFieldNames(FieldNum) + " [m3/s]";
            TempSize = state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).NomSupAirVolFlow;
            bool errorsFound = false;
            SystemAirFlowSizer sizerSystemAirFlow3;
            sizerSystemAirFlow3.overrideSizingString(SizingString);
            // sizerSystemAirFlow3.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizerSystemAirFlow3.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).NomSupAirVolFlow =
                sizerSystemAirFlow3.size(state, TempSize, errorsFound);

            state.dataSize->DataAirFlowUsedForSizing = state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).NomSupAirVolFlow;
            TempSize = state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).NomProcAirFaceVel;
            bool ErrorsFound = false;
            DesiccantDehumidifierBFPerfDataFaceVelocitySizer sizerDesDehumBFFaceVel;
            sizerDesDehumBFFaceVel.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataHeatRecovery->BalDesDehumPerfData(BalDesDehumPerfIndex).NomProcAirFaceVel =
                sizerDesDehumBFFaceVel.size(state, TempSize, ErrorsFound);

            state.dataSize->DataAirFlowUsedForSizing = 0.0;
        }
    }

    void CalcAirToAirPlateHeatExch(EnergyPlusData &state,
                                   int const ExNum,                    // number of the current heat exchanger being simulated
                                   bool const HXUnitOn,                // flag to simulate heat exchager heat recovery
                                   Optional_bool_const EconomizerFlag, // economizer flag pass by air loop or OA sys
                                   Optional_bool_const HighHumCtrlFlag // high humidity control flag passed by airloop or OA sys
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       F. Buhl Nov 2000, R. Raustad - FSEC, Feb 2009 - added economizer flags
        //                      Both the economizer and high humidity control flags can disable the HX
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the outlet conditions for an air to air plate heat
        // exchanger given the inlet conditions.

        // METHODOLOGY EMPLOYED:
        // This is a static heat exchanger model. No geometrical input data
        // is needed. No knowledge of h*A values is needed except the ratio
        // of the primary side to the secondary side convective heat transfer
        // coefficient times the exchanger surface area. Effectiveness - NTU
        // heat exchanger formulas are used.

        // The time varying load is calculated based on the variation of the
        // convective heat transfer coefficient.The variation is a function of
        // mass flow rate and inlet temperature. An iterative solution is only
        // required during initialization in one specific flow arrangement. During
        // the time steps the solution is explicit. The iteration is done with
        // the Regula Falsi algorithm. Convergence is always achieved.

        // REFERENCES:
        // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
        // LBNL Report 42354, 1999.

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool UnitOn;                // unit on flag
        Real64 SupBypassMassFlow;   // supply air mass flow rate bypassing unit [kg/s]
        Real64 UnitSupMassFlow;     // supply air mass flow rate passing through the unit [kg/s]
        Real64 SecBypassMassFlow;   // secondary air mass flow rate bypassing unit [kg/s]
        Real64 UnitSecMassFlow;     // secondary air mass flow rate passing through the unit [kg/s]
        Real64 QuotSup;             // ratio of supply nominal m*T to actual m*T
        Real64 QuotExh;             // ratio of secondary nominal m*T to actual m*T
        Real64 Deno;                // denominator of UA calculation
        Real64 CSup;                // supply air capacitance rate [J/C/s]
        Real64 CSec;                // secondary air capacitance rate [J/C/s]
        Real64 CMin;                // minimum air capacitance rate [J/C/s]
        Real64 Z;                   // Ratio of minimum air capacitance rate to maximum air capacitance rate
        Real64 NTU;                 // Number of heat transfer units
        Real64 Eps;                 // epsilon, the unit effectiveness
        Real64 UA;                  // present UA
        Real64 TempSupOut;          // unit supply outlet temperature [C]
        Real64 HumRatSupOut;        // unit supply outlet humidity ratio [kg water / kg dry air]
        Real64 EnthSupOut;          // unit supply outlet enthalpy [J/kg]
        Real64 TempSupOutSat;       // unit supply outlet temperature at saturation (at EnthSupOut) [C]
        Real64 QTrans;              // heat transferred in the heat exchanger [W]
        Real64 ElecCons;            // electricity consumption rate [W]
        Real64 TempSecOut;          // unit secondary outlet temperature [C]
        Real64 HumRatSecOut;        // unit secondary outlet humidity ratio [kg water / kg dry air]
        Real64 EnthSecOut;          // unit secondary outlet enthalpy [J/kgC]
        Real64 TempSecOutSat;       // unit secondary outlet temperature at saturation (at EnthsSecOut) [C]
        Real64 SensHeatRecRate;     // sensible heat recovery rate to supply air (heating +, cooling -)
        Real64 LatHeatRecRate;      // latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
        Real64 TotHeatRecRate;      // total heat recovery rate to supply air (heating +, cooling -)
        bool EconomizerActiveFlag;  // local representing the economizer status when PRESENT
        bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT

        UnitOn = true;
        QTrans = 0.0;
        ElecCons = 0.0;

        if (present(EconomizerFlag)) {
            EconomizerActiveFlag = EconomizerFlag;
        } else {
            EconomizerActiveFlag = false;
        }

        if (present(HighHumCtrlFlag)) {
            HighHumCtrlActiveFlag = HighHumCtrlFlag;
        } else {
            HighHumCtrlActiveFlag = false;
        }

        if ((EconomizerActiveFlag || HighHumCtrlActiveFlag) && state.dataHeatRecovery->ExchCond(ExNum).EconoLockOut == EconomizerLockout::Yes) {
            UnitSupMassFlow = 0.0; // set HX supply flow to 0, all supply air will go through supply bypass
            UnitSecMassFlow = 0.0; // set HX secondary flow to 0, all secondary air will got through secondary bypass
            UnitOn = false;        // turn off HX calculations when in economizer mode
        } else {
            // if economizer operation is not allowed, air always passes through HX
            // if CompanionCoilNum > 0, air always passes through HX (no economizer operation allowed)
            UnitSupMassFlow = min(state.dataHeatRecovery->ExchCond(ExNum).NomSupAirMassFlow, state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow);
            UnitSecMassFlow = min(state.dataHeatRecovery->ExchCond(ExNum).NomSecAirMassFlow, state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow);
        }

        SupBypassMassFlow = max(0.0, state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow - UnitSupMassFlow);
        SecBypassMassFlow = max(0.0, state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow - UnitSecMassFlow);
        if (GetCurrentScheduleValue(state, state.dataHeatRecovery->ExchCond(ExNum).SchedPtr) <= 0.0) UnitOn = false;
        if (state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow <= SmallMassFlow) UnitOn = false;
        if (state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow <= SmallMassFlow) UnitOn = false;
        if (!HXUnitOn) UnitOn = false;

        if (UnitOn) {
            // unit is on
            // calculate the UA for this time step
            QuotSup = SafeDiv(state.dataHeatRecovery->ExchCond(ExNum).mTSup0,
                              UnitSupMassFlow * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp + KELVZERO));
            QuotExh = SafeDiv(state.dataHeatRecovery->ExchCond(ExNum).mTSec0,
                              UnitSecMassFlow * (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + KELVZERO));
            Deno = std::pow(QuotSup, 0.78) + state.dataHeatRecovery->ExchCond(ExNum).hARatio * std::pow(QuotExh, 0.78);
            UA = state.dataHeatRecovery->ExchCond(ExNum).UA0 * (state.dataHeatRecovery->ExchCond(ExNum).hARatio + 1.0) / Deno;
            // calculate the NTU
            CSup = UnitSupMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            CSec = UnitSecMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
            // note: no C can be zero since otherwise we wouldn't be here
            if (CSup < CSec) {
                CMin = CSup;
                Z = CMin / CSec;
            } else {
                CMin = CSec;
                Z = CMin / CSup;
            }
            NTU = UA / CMin;
            // Get the effectiveness
            CalculateEpsFromNTUandZ(state, NTU, Z, state.dataHeatRecovery->ExchCond(ExNum).FlowArr, Eps);
            // use the effectiveness to calculate the unit outlet conditions
            TempSupOut = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp +
                         Eps * CMin / CSup * (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
            QTrans = CSup * (TempSupOut - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
            TempSecOut = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - QTrans / CSec;
            HumRatSupOut = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
            EnthSupOut = PsyHFnTdbW(TempSupOut, HumRatSupOut);
            // check for saturation in supply outlet
            TempSupOutSat = PsyTsatFnHPb(state, EnthSupOut, state.dataEnvrn->OutBaroPress);
            if (TempSupOutSat > TempSupOut) {
                TempSupOut = TempSupOutSat;
                HumRatSupOut = PsyWFnTdbH(state, TempSupOut, EnthSupOut);
            }
            HumRatSecOut = state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat;
            EnthSecOut = PsyHFnTdbW(TempSecOut, HumRatSecOut);
            // check for saturation in secondary outlet
            TempSecOutSat = PsyTsatFnHPb(state, EnthSecOut, state.dataEnvrn->OutBaroPress);
            if (TempSecOutSat > TempSecOut) {
                TempSecOut = TempSecOutSat;
                HumRatSecOut = PsyWFnTdbH(state, TempSecOut, EnthSecOut);
            }
            // calculate outlet conditions by mixing bypass air stream with air that went through the
            // heat exchanger core.
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                (UnitSupMassFlow * EnthSupOut + SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInEnth) /
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                (UnitSupMassFlow * HumRatSupOut + SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat) /
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                (UnitSecMassFlow * EnthSecOut + SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInEnth) /
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                (UnitSecMassFlow * HumRatSecOut + SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat) /
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp =
                PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            ElecCons = state.dataHeatRecovery->ExchCond(ExNum).NomElecPower;

        } else {
            // the unit is off. Pass through the air streams with no change
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SupInEnth;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SecInEnth;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
        }
        CSup = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
        SensHeatRecRate = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
        TotHeatRecRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                         (state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth - state.dataHeatRecovery->ExchCond(ExNum).SupInEnth);
        LatHeatRecRate = TotHeatRecRate - SensHeatRecRate;

        if (SensHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = SensHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = std::abs(SensHeatRecRate);
        }
        if (LatHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = LatHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = std::abs(LatHeatRecRate);
        }
        if (TotHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = TotHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = std::abs(TotHeatRecRate);
        }

        state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate = ElecCons;
    }

    void CalcAirToAirGenericHeatExch(EnergyPlusData &state,
                                     int const ExNum,                       // number of the current heat exchanger being simulated
                                     bool const HXUnitOn,                   // flag to simulate heat exchanger heat recovery
                                     bool const FirstHVACIteration,         // first HVAC iteration flag
                                     int const FanOpMode,                   // Supply air fan operating mode (1=cycling, 2=constant)
                                     Optional_bool_const EconomizerFlag,    // economizer flag pass by air loop or OA sys
                                     Optional_bool_const HighHumCtrlFlag,   // high humidity control flag passed by airloop or OA sys
                                     Optional<Real64 const> HXPartLoadRatio //
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey
        //       DATE WRITTEN   February 2003
        //       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
        //                      Both the economizer and high humidity control flags can disable the HX
        //       RE-ENGINEERED  Richard Raustad, June 2003

        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the outlet conditions for an air to air generic heat
        //  exchanger given the inlet conditions.

        // METHODOLOGY EMPLOYED:
        //  This is a standard heat exchanger effectiveness model. No geometrical input data
        //  is needed. The model uses heat exchanger effectiveness performance data
        //  to calculate the air temperature and humidity ratio of the leaving
        //  supply and secondary air streams. Linear interpolation (or extrapolation)
        //  is assumed to obtain heat exchanger effectiveness at off-rated conditions.
        //  Economizer operation is allowed through the use of a Controller: Outside Air
        //  object.

        // REFERENCES:
        //  ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
        //  ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
        //  U.S. Environmental Protection Agency software "SAVES" -
        //   School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html

        // USE STATEMENTS:
        using DataHVACGlobals::CycFanCycCoil;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ErrorTol(0.001); // error tolerence

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool UnitOn;           // unit on flag
        bool FrostControlFlag; // unit is in frost control mode when TRUE
        int SupOutNode;
        Real64 Error;           // iteration loop error variable
        Real64 Iter;            // iteration counter
        Real64 ControlFraction; // fraction of effectiveness when rotary HX speed or plate bypass modulation is used for
        // temperature control
        Real64 RhoSup;              // supply air density at actual pressure, temperature and humidity conditions [kg/m3]
        Real64 RhoSec;              // secondary air density at actual pressure, temperature and humidity conditions [kg/m3]
        Real64 RhoStd;              // standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
        Real64 CSup;                // supply air heat capacity rate [W/K]
        Real64 CSec;                // secondary air heat capacity rate [W/K]
        Real64 CMin;                // minimum air heat capacity rate [W/K]
        Real64 QSensTrans;          // sensible heat transferred by the heat exchanger [W]
        Real64 QTotTrans;           // total heat (sensible + latent) transferred by the heat exchanger [W]
        Real64 TempSecOutSat;       // secondary air outlet temperature at saturation (at EnthsSecOut) [C]
        Real64 HXSecAirVolFlowRate; // air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
        Real64 HXSupAirVolFlowRate; // air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
        Real64 HXAvgAirVolFlowRate; // average air volume flow rate through the heat exchanger [m3/sec]
        Real64 HXAirVolFlowRatio;   // ratio of avg actual air volume flow through HX to nominal HX air volume flow [-]
        Real64 HXTempSetPoint;      // setpoint temperature at supply outlet node of HX when ControlToTemperatureSetPoint = Yes
        Real64 MassFlowSecIn;       // secondary air mass flow rate at HX inlet
        //  REAL(r64)    :: MassFlowSecOut      ! secondary air mass flow rate at HX outlet
        Real64 MassFlowSupIn;       // supply air mass flow rate at HX inlet
        Real64 MassFlowSupOut;      // supply air mass flow rate through HX core outlet
        Real64 MassFlowSupBypass;   // supply air bypass mass flow rate around HX core
        Real64 TempSupIn;           // supply side temperature of air entering HX
        Real64 TempSupOut;          // supply side temperature of air leaving HX core
        Real64 HumRatSupIn;         // supply side humidity ratio of air entering HX
        Real64 TempSecIn;           // secondary side temperature of air entering HX
        Real64 SensHeatRecRate;     // sensible heat recovery rate to supply air (heating +, cooling -)
        Real64 LatHeatRecRate;      // latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
        Real64 TotHeatRecRate;      // total heat recovery rate to supply air (heating +, cooling -)
        bool EconomizerActiveFlag;  // local representing the economizer status when PRESENT
        bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT
        Real64 AirSidePLR;

        // Initialize local variables
        UnitOn = true;
        FrostControlFlag = false;
        QSensTrans = 0.0;
        QTotTrans = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).DefrostFraction = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SupInEnth;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SecInEnth;
        SupOutNode = state.dataHeatRecovery->ExchCond(ExNum).SupOutletNode;
        HXTempSetPoint = state.dataLoopNodes->Node(SupOutNode).TempSetPoint;

        if (present(EconomizerFlag)) {
            EconomizerActiveFlag = EconomizerFlag;
        } else {
            EconomizerActiveFlag = false;
        }

        if (present(HighHumCtrlFlag)) {
            HighHumCtrlActiveFlag = HighHumCtrlFlag;
        } else {
            HighHumCtrlActiveFlag = false;
        }

        // Determine mass flow through heat exchanger and mass flow being bypassed (only flat plate bypasses flow)
        if (((EconomizerActiveFlag || HighHumCtrlActiveFlag) && state.dataHeatRecovery->ExchCond(ExNum).EconoLockOut == EconomizerLockout::Yes) &&
            state.dataHeatRecovery->ExchCond(ExNum).ExchConfig == HXConfigurationType::Plate) {
            state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
        } else { // No bypass mass flow
            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow = 0.0;
        }
        // Unit is scheduled OFF, so bypass heat exchange calcs
        if (GetCurrentScheduleValue(state, state.dataHeatRecovery->ExchCond(ExNum).SchedPtr) <= 0.0) UnitOn = false;
        //! Economizer is active, so bypass heat exchange calcs. This applies to both flat plate and rotary HX's
        if ((EconomizerActiveFlag || HighHumCtrlActiveFlag) && state.dataHeatRecovery->ExchCond(ExNum).EconoLockOut == EconomizerLockout::Yes) {
            UnitOn = false;
        }
        // Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
        if (state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow <= SmallMassFlow) UnitOn = false;
        if (state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow <= SmallMassFlow) UnitOn = false;
        if (!HXUnitOn) UnitOn = false;
        if (state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow == 0.0) UnitOn = false;

        if (UnitOn) {
            // Unit is on.
            if (present(HXPartLoadRatio) && FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                if (HXPartLoadRatio > 0) {
                    AirSidePLR = HXPartLoadRatio;
                } else {
                    AirSidePLR = 1.0;
                }
            } else {
                AirSidePLR = 1.0;
            }

            if (FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow /= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow /= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow /= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow /= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow /= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow /= AirSidePLR;
            }

            // In the future, use actual node pressures in the following air density calls
            RhoStd = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, 20.0, 0.0);
            HXSupAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow / RhoStd; // volume flow using standard density
            HXSecAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow / RhoStd;
            // Limit unbalanced volumetric flow ratio to 2:1
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                if (HXSupAirVolFlowRate != 0.0 && HXSecAirVolFlowRate != 0.0) {
                    if (((HXSupAirVolFlowRate / HXSecAirVolFlowRate) > 2.0) || ((HXSecAirVolFlowRate / HXSupAirVolFlowRate) > 2.0)) {
                        ++state.dataHeatRecovery->ExchCond(ExNum).UnBalancedErrCount;
                        if (state.dataHeatRecovery->ExchCond(ExNum).UnBalancedErrCount <= 2) {
                            ShowSevereError(state,
                                            cHXTypes(state.dataHeatRecovery->ExchCond(ExNum).ExchTypeNum) + ": \"" +
                                                state.dataHeatRecovery->ExchCond(ExNum).Name +
                                                "\" unbalanced air volume flow ratio through the heat exchanger is greater than 2:1.");
                            ShowContinueErrorTimeStamp(
                                state, format("...HX Supply air to Exhaust air flow ratio = {:.5R}.", HXSupAirVolFlowRate / HXSecAirVolFlowRate));
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                cHXTypes(state.dataHeatRecovery->ExchCond(ExNum).ExchTypeNum) + " \"" + state.dataHeatRecovery->ExchCond(ExNum).Name +
                                    "\":  Unbalanced air volume flow ratio exceeds 2:1 warning continues. HX flow ratio statistics follow.",
                                state.dataHeatRecovery->ExchCond(ExNum).UnBalancedErrIndex,
                                HXSupAirVolFlowRate / HXSecAirVolFlowRate,
                                HXSupAirVolFlowRate / HXSecAirVolFlowRate);
                        }
                    }
                }
            }
            // Calculate average volumetric flow rate of the two air streams
            HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate) / 2.0;
            HXAirVolFlowRatio = HXAvgAirVolFlowRate / state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow;
            // Average air volume flow rate must be between 50% and 130% of nominal supply air volume flow
            if (HXAirVolFlowRatio > 1.3 || HXAirVolFlowRatio < 0.5) {
                if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                    ++state.dataHeatRecovery->ExchCond(ExNum).LowFlowErrCount;
                    if (state.dataHeatRecovery->ExchCond(ExNum).LowFlowErrCount == 1) {
                        ShowWarningError(state,
                                         cHXTypes(state.dataHeatRecovery->ExchCond(ExNum).ExchTypeNum) + " \"" +
                                             state.dataHeatRecovery->ExchCond(ExNum).Name + "\"");
                        ShowContinueError(state, "Average air volume flow rate is <50% or >130% of the nominal HX supply air volume flow rate.");
                        ShowContinueErrorTimeStamp(state, format("Air volume flow rate ratio = {:.3R}.", HXAirVolFlowRatio));
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            cHXTypes(state.dataHeatRecovery->ExchCond(ExNum).ExchTypeNum) + " \"" + state.dataHeatRecovery->ExchCond(ExNum).Name +
                                "\":  Average air volume flow rate is <50% or >130% warning continues. Air flow rate ratio statistics follow.",
                            state.dataHeatRecovery->ExchCond(ExNum).LowFlowErrIndex,
                            HXAirVolFlowRatio,
                            HXAirVolFlowRatio);
                    }
                }
            }

            // Determine heat exchanger effectiveness using avg air volume flow rate based on actual inlet air density
            // Linearly interpolate and extrapolate (within limits) from effectiveness input values
            RhoSup = PsyRhoAirFnPbTdbW(state,
                                       state.dataEnvrn->OutBaroPress,
                                       state.dataHeatRecovery->ExchCond(ExNum).SupInTemp,
                                       state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            RhoSec = PsyRhoAirFnPbTdbW(state,
                                       state.dataEnvrn->OutBaroPress,
                                       state.dataHeatRecovery->ExchCond(ExNum).SecInTemp,
                                       state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
            HXSupAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow / RhoSup;
            HXSecAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow / RhoSec;
            HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate) / 2.0;
            HXAirVolFlowRatio = HXAvgAirVolFlowRate / state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow;

            if (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) {
                // Use heating effectiveness values
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness =
                    state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75 +
                    (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100 - state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75) *
                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness =
                    state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75 +
                    (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100 - state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75) *
                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
            } else {
                // Use cooling effectiveness values
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness =
                    state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75 +
                    (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible100 - state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75) *
                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness =
                    state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75 +
                    (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent100 - state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75) *
                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
            }

            //     Keep effectiveness between 0 and 1.0 ??
            //     HXOpSensEffect = MAX(MIN(HXOpSensEffect,1.0),0.0)
            //     HXOpLatEffect =  MAX(MIN(HXOpLatEffect,1.0),0.0)
            if (state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness < 0.0) {
                //   The model should at least guard against negative numbers
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness = 0.0;
                if (!state.dataHeatRecovery->ExchCond(ExNum).SensEffectivenessFlag) {
                    ShowWarningError(state,
                                     "HeatExchanger:AirToAir:SensibleAndLatent =\"" + state.dataHeatRecovery->ExchCond(ExNum).Name + "\"" +
                                         " sensible effectiveness is less than zero. Check the following inputs.");
                    if (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) {
                        ShowContinueError(state,
                                          format("...Sensible Effectiveness at 100% Heating Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100));
                        ShowContinueError(state,
                                          format("...Sensible Effectiveness at 75% Heating Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75));
                        ShowContinueError(state, "...Sensible effectiveness reset to zero and the simulation continues.");
                    } else {
                        ShowContinueError(state,
                                          format("...Sensible Effectiveness at 100% Cooling Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible100));
                        ShowContinueError(state,
                                          format("...Sensible Effectiveness at 75% Cooling Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75));
                        ShowContinueError(state, "...Sensible effectiveness reset to zero and the simulation continues.");
                    }
                    ShowContinueError(state, format("...Heat Exchanger Air Volume Flow Ratio = {:.2R}", HXAirVolFlowRatio));
                    state.dataHeatRecovery->ExchCond(ExNum).SensEffectivenessFlag = true;
                }
            }
            if (state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness < 0.0) {
                // The model should at least guard against negative numbers
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = 0.0;
                if (!state.dataHeatRecovery->ExchCond(ExNum).LatEffectivenessFlag) {
                    ShowWarningError(state,
                                     "HeatExchanger:AirToAir:SensibleAndLatent =\"" + state.dataHeatRecovery->ExchCond(ExNum).Name + "\"" +
                                         " latent effectiveness is less than zero. Check the following inputs.");
                    if (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) {
                        ShowContinueError(state,
                                          format("...Latent Effectiveness at 100% Heating Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100));
                        ShowContinueError(state,
                                          format("...Latent Effectiveness at 75% Heating Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75));
                        ShowContinueError(state, "...Latent effectiveness reset to zero and the simulation continues.");
                    } else {
                        ShowContinueError(state,
                                          format("...Latent Effectiveness at 100% Cooling Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent100));
                        ShowContinueError(state,
                                          format("...Latent Effectiveness at 75% Cooling Air Flow = {:.2R}",
                                                 state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75));
                        ShowContinueError(state, "...Latent effectiveness reset to zero and the simulation continues.");
                    }
                    ShowContinueError(state, format("...Heat Exchanger Air Volume Flow Ratio = {:.2R}", HXAirVolFlowRatio));
                    state.dataHeatRecovery->ExchCond(ExNum).LatEffectivenessFlag = true;
                }
            }
            // Use the effectiveness to calculate the air conditions exiting the heat exchanger (all air flow through the HX)
            // Include EATR and OACF in the following calculations at some point

            CSup = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            CSec = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
            CMin = min(CSup, CSec);

            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                state.dataHeatRecovery->ExchCond(ExNum).SupInTemp +
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * CMin / CSup *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat +
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness * CMin / CSup *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);

            //   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
            if (PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress) >
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            }
            QSensTrans = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + QSensTrans / CSec;
            QTotTrans = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                state.dataHeatRecovery->ExchCond(ExNum).SecInEnth + QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);
            //   Control the supply air outlet temperature to a setpoint for Heating Mode only
            //   (ControlFraction = 0 HX fully bypassed, ControlFraction = 1 air passed entirely through HX)
            //   (supply air stream bypass mass flow rate proportional to ControlFraction except when frost control is active)
            if (state.dataHeatRecovery->ExchCond(ExNum).ControlToTemperatureSetPoint) {
                if ((state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) != 0.0) {
                    if ((state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < HXTempSetPoint &&
                         state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp > HXTempSetPoint) ||
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp > HXTempSetPoint &&
                         state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp < HXTempSetPoint)) {
                        ControlFraction = max(
                            0.0,
                            min(1.0,
                                std::abs((state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - HXTempSetPoint) /
                                         (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp))));
                    } else if ((state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp &&
                                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp < HXTempSetPoint) ||
                               (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp > state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp &&
                                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp > HXTempSetPoint)) {
                        ControlFraction = 1.0;
                    } else {
                        ControlFraction = 0.0;
                    }
                } else {
                    //     ELSE fully bypass HX to maintain supply outlet temp as high as possible
                    ControlFraction = 0.0;
                }
                if (state.dataHeatRecovery->ExchCond(ExNum).ExchConfig == HXConfigurationType::Rotary) {
                    //       Rotory HX's never get bypassed, rotational speed is modulated
                    state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness *= ControlFraction;
                    state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness *= ControlFraction;
                } else { // HX is a plate heat exchanger, bypass air to control SA temperature
                    Error = 1.0;
                    Iter = 0.0;
                    MassFlowSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
                    MassFlowSupOut = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;
                    MassFlowSupBypass = state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow;
                    MassFlowSecIn = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
                    TempSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                    TempSupOut = state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp;
                    HumRatSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
                    TempSecIn = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp;
                    while ((std::abs(Error) > ErrorTol && Iter < 10 && ControlFraction < 1.0) || Iter == 1) {
                        MassFlowSupOut = MassFlowSupIn * ControlFraction;
                        MassFlowSupBypass = MassFlowSupIn * (1.0 - ControlFraction);
                        HXSupAirVolFlowRate = MassFlowSupOut / RhoSup;
                        HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate) / 2.0;
                        HXAirVolFlowRatio = HXAvgAirVolFlowRate / state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow;
                        CSup = MassFlowSupOut * PsyCpAirFnW(HumRatSupIn);
                        CMin = min(CSup, CSec);
                        if (TempSupIn < TempSecIn) {
                            //          Use heating effectiveness values
                            state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness =
                                state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75 +
                                (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100 -
                                 state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75) *
                                    (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                            state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75 +
                                                                                       (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100 -
                                                                                        state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75) *
                                                                                           (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                        } else {
                            //          Use cooling effectiveness values
                            state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness =
                                state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75 +
                                (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible100 -
                                 state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75) *
                                    (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                            state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75 +
                                                                                       (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent100 -
                                                                                        state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75) *
                                                                                           (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                        }

                        if (state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness < 0.0) {
                            //   The model should at least guard against negative numbers
                            state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness = 0.0;
                            if (!state.dataHeatRecovery->ExchCond(ExNum).SensEffectivenessFlag) {
                                ShowWarningError(state,
                                                 "HeatExchanger:AirToAir:SensibleAndLatent =\"" + state.dataHeatRecovery->ExchCond(ExNum).Name +
                                                     "\"" + " sensible effectiveness is less than zero. Check the following inputs.");
                                if (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) {
                                    ShowContinueError(state,
                                                      format("...Sensible Effectiveness at 100% Heating Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100));
                                    ShowContinueError(state,
                                                      format("...Sensible Effectiveness at 75% Heating Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75));
                                    ShowContinueError(state, "...Sensible effectiveness reset to zero and the simulation continues.");
                                } else {
                                    ShowContinueError(state,
                                                      format("...Sensible Effectiveness at 100% Cooling Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible100));
                                    ShowContinueError(state,
                                                      format("...Sensible Effectiveness at 75% Cooling Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75));
                                    ShowContinueError(state, "...Sensible effectiveness reset to zero and the simulation continues.");
                                }
                                ShowContinueError(state, format("...Heat Exchanger Air Volume Flow Ratio = {:.2R}", HXAirVolFlowRatio));
                                state.dataHeatRecovery->ExchCond(ExNum).SensEffectivenessFlag = true;
                            }
                        }
                        if (state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness < 0.0) {
                            // The model should at least guard against negative numbers
                            state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = 0.0;
                            if (!state.dataHeatRecovery->ExchCond(ExNum).LatEffectivenessFlag) {
                                ShowWarningError(state,
                                                 "HeatExchanger:AirToAir:SensibleAndLatent =\"" + state.dataHeatRecovery->ExchCond(ExNum).Name +
                                                     "\"" + " latent effectiveness is less than zero. Check the following inputs.");
                                if (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp < state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) {
                                    ShowContinueError(state,
                                                      format("...Latent Effectiveness at 100% Heating Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100));
                                    ShowContinueError(state,
                                                      format("...Latent Effectiveness at 75% Heating Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75));
                                    ShowContinueError(state, "...Latent effectiveness reset to zero and the simulation continues.");
                                } else {
                                    ShowContinueError(state,
                                                      format("...Latent Effectiveness at 100% Cooling Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent100));
                                    ShowContinueError(state,
                                                      format("...Latent Effectiveness at 75% Cooling Air Flow = {:.2R}",
                                                             state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75));
                                    ShowContinueError(state, "...Latent effectiveness reset to zero and the simulation continues.");
                                }
                                ShowContinueError(state, format("...Heat Exchanger Air Volume Flow Ratio = {:.2R}", HXAirVolFlowRatio));
                                state.dataHeatRecovery->ExchCond(ExNum).LatEffectivenessFlag = true;
                            }
                        }

                        if (CSup == 0.0) {
                            //          IF CSup = 0, then supply air mass flow rate = 0 and HX is fully bypassed. Fix divide by 0 error below DO loop.
                            CSup = 1.0;
                            CMin = 0.0;
                            break;
                        }
                        TempSupOut = (MassFlowSupOut * (TempSupIn + state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * CMin / CSup *
                                                                        (TempSecIn - TempSupIn)) +
                                      MassFlowSupBypass * TempSupIn) /
                                     MassFlowSupIn;
                        Error = (TempSupOut - HXTempSetPoint);
                        //         IF supply inlet temp = supply outlet temp, fully bypass HX - ELSE control to SP
                        if (TempSupIn != TempSupOut) {
                            ControlFraction = max(0.0, min(1.0, std::abs(ControlFraction * (TempSupIn - HXTempSetPoint) / (TempSupIn - TempSupOut))));
                        } else if (std::abs(TempSupOut - HXTempSetPoint) < ErrorTol) {
                            //           IF TempSupIn = TempSupOut then TempSecIn = TempSupIn (ControlFraction = ?)
                            //           Do nothing, variables in ELSE below have already been calculated
                            break;
                        } else {
                            //           or HX is fully bypassed (ControlFraction = 0) which actually should be caught in IF(CSup .EQ. 0.0)THEN above.
                            ControlFraction = 0.0;
                            MassFlowSupOut = MassFlowSupIn * ControlFraction;
                            MassFlowSupBypass = MassFlowSupIn * (1.0 - ControlFraction);
                            CSup = 1.0;
                            CMin = 0.0;
                            break;
                        }
                        ++Iter;
                    }

                    state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow = MassFlowSupIn;
                    state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = MassFlowSupOut;
                    state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = MassFlowSupBypass;
                    state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow = MassFlowSecIn;
                    state.dataHeatRecovery->ExchCond(ExNum).SupInTemp = TempSupIn;
                    state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = TempSupOut;
                    state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat = HumRatSupIn;
                    state.dataHeatRecovery->ExchCond(ExNum).SecInTemp = TempSecIn;

                } // ENDIF for "IF (state.dataHeatRecovery->ExchCond(ExNum)%ExchConfig == 'ROTARY') THEN"
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    state.dataHeatRecovery->ExchCond(ExNum).SupInTemp +
                    state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * CMin / CSup *
                        (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat +
                    state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness * CMin / CSup *
                        (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                    PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);

                //     Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
                if (PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress) >
                    state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) {
                    state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                        PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress);
                    state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                        PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
                }

                QSensTrans = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + QSensTrans / CSec;
                QTotTrans = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                            (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                    state.dataHeatRecovery->ExchCond(ExNum).SecInEnth + QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);

            } // ENDIF for "IF(state.dataHeatRecovery->ExchCond(ExNum)%ControlToTemperatureSetPoint .AND... THEN, ELSE"

            if (FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow *= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow *= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow *= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow *= AirSidePLR;
                state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow *= AirSidePLR;
            } else if (FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp * AirSidePLR +
                                                                     state.dataHeatRecovery->ExchCond(ExNum).SupInTemp * (1.0 - AirSidePLR);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat * AirSidePLR +
                                                                       state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat * (1.0 - AirSidePLR);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth * AirSidePLR +
                                                                     state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth * (1.0 - AirSidePLR);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp * AirSidePLR +
                                                                     state.dataHeatRecovery->ExchCond(ExNum).SecInTemp * (1.0 - AirSidePLR);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat * AirSidePLR +
                                                                       state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat * (1.0 - AirSidePLR);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth * AirSidePLR +
                                                                     state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth * (1.0 - AirSidePLR);
            }

            if ((state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "MINIMUMEXHAUSTTEMPERATURE" &&
                 state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp < state.dataHeatRecovery->ExchCond(ExNum).ThresholdTemperature) ||
                (state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "EXHAUSTAIRRECIRCULATION" &&
                 state.dataHeatRecovery->ExchCond(ExNum).SupInTemp <= state.dataHeatRecovery->ExchCond(ExNum).ThresholdTemperature) ||
                (state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "EXHAUSTONLY" &&
                 state.dataHeatRecovery->ExchCond(ExNum).SupInTemp <= state.dataHeatRecovery->ExchCond(ExNum).ThresholdTemperature)) {
                FrostControl(state, ExNum);
                FrostControlFlag = true;
            }

            // check for saturation in secondary outlet
            TempSecOutSat = PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth, state.dataEnvrn->OutBaroPress);
            if (TempSecOutSat > state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp) {
                state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = TempSecOutSat;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);
            }

            // calculate outlet conditions by mixing bypass air stream with air that went through the
            // heat exchanger core.  Perform this mixing only when no frost control is used or
            // heat exchanger is not in frost control mode.  Mixing similar to this is performed
            // in the frost control subroutine when in frost control mode.
            if (!FrostControlFlag) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                    (state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth +
                     state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInEnth) /
                    state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    (state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat +
                     state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat) /
                    state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                    (state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth +
                     state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInEnth) /
                    state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                    (state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat +
                     state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat) /
                    state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
                state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp =
                    PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat);
                state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            }

            state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate = state.dataHeatRecovery->ExchCond(ExNum).NomElecPower;

        } // ENDIF for "IF (UnitOn) THEN"

        // Calculate heat transfer from the unit using the final supply inlet and supply outlet air conditions
        CSup = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
        SensHeatRecRate = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
        TotHeatRecRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                         (state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth - state.dataHeatRecovery->ExchCond(ExNum).SupInEnth);
        LatHeatRecRate = TotHeatRecRate - SensHeatRecRate;

        // Set report variables based on sign of recovery rate
        if (SensHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = SensHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = std::abs(SensHeatRecRate);
        }
        if (LatHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = LatHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = std::abs(LatHeatRecRate);
        }
        if (TotHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = TotHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = std::abs(TotHeatRecRate);
        }
    }

    void CalcDesiccantBalancedHeatExch(EnergyPlusData &state,
                                       int const ExNum,               // number of the current heat exchanger being simulated
                                       bool const HXUnitOn,           // flag to simulate heat exchager heat recovery
                                       bool const FirstHVACIteration, // First HVAC iteration flag
                                       int const FanOpMode,           // Supply air fan operating mode (1=cycling, 2=constant)
                                       Real64 const PartLoadRatio,    // Part load ratio requested of DX compressor
                                       int const CompanionCoilIndex,  // index of companion cooling coil
                                       bool const RegenInletIsOANode, // Flag to determine if regen side inlet is OANode, if so this air stream cycles
                                       Optional_bool_const EconomizerFlag, // economizer flag pass by air loop or OA sys
                                       Optional_bool_const HighHumCtrlFlag // high humidity control flag passed by airloop or OA sys
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
        //                      Both the economizer and high humidity control flags can disable the HX
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the outlet conditions for a balanced air-to-air desiccant heat exchanger
        //  given the inlet conditions and face velocity. Performance map is provided by user.

        // METHODOLOGY EMPLOYED:
        //  This is an empirical heat exchanger model. The model uses heat exchanger performance data to
        //  calculate the air temperature and humidity ratio of the leaving upply and secondary air streams.
        //  Humidity control can enable/disable heat recovery through the use of the HXUnitOn Subroutine argument.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::SensedNodeFlagValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool UnitOn;                   // unit on flag
        Real64 RhoStd;                 // standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
        Real64 CSup;                   // supply air heat capacity rate [W/K]
        Real64 CSec;                   // secondary air heat capacity rate [W/K]
        Real64 TempSecOutSat;          // secondary air outlet temperature at saturation (at EnthsSecOut) [C]
        Real64 SensHeatRecRate;        // sensible heat recovery rate to supply air (heating +, cooling -)
        Real64 TotHeatRecRate;         // total heat recovery rate to supply air (heating +, cooling -)
        Real64 ProcessSensHeatRecRate; // process sensible heat recovery rate (heating +, cooling -)
        Real64 ProcessTotHeatRecRate;  // process total heat recovery rate (heating +, cooling -)
        Real64 ProcessLatHeatRecRate;  // process latent heat recovery rate (heating [humidify] +, cooling [dehumidify] -)
        Real64 SupInMassFlow;          // Supply side HX mass flow rate
        Real64 SecInMassFlow;          // Secondary side HX mass flow rate

        Real64 Coeff1;                  // coefficient1 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff2;                  // coefficient2 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff3;                  // coefficient3 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff4;                  // coefficient4 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff5;                  // coefficient5 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff6;                  // coefficient6 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff7;                  // coefficient7 to empirical model (used for both temperature and humidity ratio equations)
        Real64 Coeff8;                  // coefficient8 to empirical model (used for both temperature and humidity ratio equations)
        Real64 BalFaceVelActual;        // operating face velocity [m/s]
        Real64 FullLoadSupOutTemp(0);   // empirical model supply outlet temperature [C]
        Real64 FullLoadSupOutHumRat(0); // empirical model supply outlet humidity ratio [kg/kg]
        Real64 FullLoadDeltaT;          // empirical model heat exchanger delta temperature [C]
        Real64 FullLoadDeltaW;          // empirical model heat exchanger delta humidity ratio [kg/kg]
        Real64 T_RegenInTemp;           // empirical model supply (regen) inlet temperature for temperature equation [C]
        Real64 T_RegenInHumRat;         // empirical model supply (regen) inlet humidity ratio for temperature equation [kg/kg]
        Real64 T_ProcInTemp;            // empirical model secondary (process) inlet temperature for temperature equation [C]
        Real64 T_ProcInHumRat;          // empirical model secondary (process) inlet humidity ratio for temperature equation [kg/kg]
        Real64 T_FaceVel;               // empirical model face velocity for temperature equation [m/s]
        Real64 H_RegenInTemp;           // empirical model supply (regen) inlet temperature for humidity ratio equation [C]
        Real64 H_RegenInHumRat;         // empirical model supply (regen) inlet humidity ratio for humidity ratio equation [kg/kg]
        Real64 H_ProcInTemp;            // empirical model secondary (process) inlet temperature for humidity ratio equation [C]
        Real64 H_ProcInHumRat;          // empirical model secondary (process) inlet humidity ratio for humidity ratio equation [kg/kg]
        Real64 H_FaceVel;               // empirical model face velocity for humidity ratio equation [m/s]
        Real64 MaxHumRatNeeded;         // maximum humidity ratio setpoint for balanced desiccant HX [kg/kg]
        Real64 MinHumRatNeeded;         // minimum humidity ratio setpoint for balanced desiccant HX [kg/kg]
        Real64 HXPartLoadRatio;         // local heat exchanger part-load ratio
        Real64 TestSaturationEnthalpy;  // enthalpy used to test for regeneration outlet condition over saturation curve (J/kg)
        constexpr const char *ThisSubTSat("CalcDesiccantBalancedHeatExch:   TSat");
        constexpr const char *ThisSubTSatFullLoadOutTemp("CalcDesiccantBalancedHeatExch:   TSat-FullLoadOutTemp");
        constexpr const char *ThisSubTSatFullLoadOutHumRat("CalcDesiccantBalancedHeatExch:   TSat-FullLoadOutHumRat");
        constexpr const char *ThisSubSecOutHumRat("CalcDesiccantBalancedHeatExch:   SecOutHumRat");
        constexpr const char *ThisSubTestSatSec("CalcDesiccantBalancedHeatExch:   TestSatSec");
        constexpr const char *ThisSubTSatSecOutHumRat("CalcDesiccantBalancedHeatExch:   TSat-SecOutHumRat");

        Real64 AverageMassFlowRate; // average of supply (regen) and secondary (process) mass flow rates [kg/s]
        bool EconomizerActiveFlag;  // local representing the economizer status when PRESENT
        bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT

        // Initialize local variables
        UnitOn = true;
        SensHeatRecRate = 0.0;
        TotHeatRecRate = 0.0;
        HXPartLoadRatio = PartLoadRatio;
        state.dataHeatRecovery->ExchCond(ExNum).DefrostFraction = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SupInEnth;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth = state.dataHeatRecovery->ExchCond(ExNum).SecInEnth;
        state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
        AverageMassFlowRate = (state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow + state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow) / 2.0;

        if (present(EconomizerFlag)) {
            EconomizerActiveFlag = EconomizerFlag;
        } else {
            EconomizerActiveFlag = false;
        }

        if (present(HighHumCtrlFlag)) {
            HighHumCtrlActiveFlag = HighHumCtrlFlag;
        } else {
            HighHumCtrlActiveFlag = false;
        }

        // Unit is scheduled OFF, so bypass heat exchange calcs
        if (GetCurrentScheduleValue(state, state.dataHeatRecovery->ExchCond(ExNum).SchedPtr) <= 0.0) UnitOn = false;
        // Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
        if (state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow <= SmallMassFlow) UnitOn = false;
        if (state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow <= SmallMassFlow) UnitOn = false;
        if (HXPartLoadRatio == 0.0) UnitOn = false;
        if (!HXUnitOn) UnitOn = false;
        if ((EconomizerActiveFlag || HighHumCtrlActiveFlag) && state.dataHeatRecovery->ExchCond(ExNum).EconoLockOut == EconomizerLockout::Yes)
            UnitOn = false;

        if (UnitOn) {

            //   Use local variables to perform checks
            SecInMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            SupInMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;

            // In constant fan mode, process air mass flow rate is full flow and supply (regen) air cycles based on PLR.
            // If supply (regen) inlet is OA node, regen mass flow rate is proportional to PLR.
            // If both of the above is true then boost local variable up to full flow
            if ((FanOpMode == ContFanCycCoil) && RegenInletIsOANode) {
                SupInMassFlow /= HXPartLoadRatio;
            }
            // for cycling fan case, boost both local variables up to full flow
            if (FanOpMode == CycFanCycCoil) {
                SupInMassFlow /= HXPartLoadRatio; // supply = regen
                SecInMassFlow /= HXPartLoadRatio; // secondary = process
            }

            // Check for balanced flow condition
            CheckForBalancedFlow(state, ExNum, SecInMassFlow, SupInMassFlow, FirstHVACIteration);

            {
                auto const SELECT_CASE_var(state.dataHeatRecovery->ExchCond(ExNum).HeatExchPerfTypeNum);

                if (SELECT_CASE_var == BALANCEDHX_PERFDATATYPE1) {

                    Coeff1 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B1;
                    Coeff2 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B2;
                    Coeff3 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B3;
                    Coeff4 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B4;
                    Coeff5 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B5;
                    Coeff6 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B6;
                    Coeff7 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B7;
                    Coeff8 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).B8;

                    T_ProcInTemp = state.dataHeatRecovery->FullLoadOutAirTemp;
                    T_ProcInHumRat = state.dataHeatRecovery->FullLoadOutAirHumRat;
                    T_RegenInTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                    T_RegenInHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;

                    // Must use the same density used to convert volumetric flow rate to mass flow rate to get back to velocity
                    RhoStd = state.dataEnvrn->StdRhoAir; // PsyRhoAirFnPbTdbW(StdBaroPress,20.0d0, 0.0d0)
                    BalFaceVelActual = SupInMassFlow / (RhoStd * state.dataHeatRecovery->ExchCond(ExNum).FaceArea);

                    T_FaceVel = BalFaceVelActual;

                    //     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
                    //     Check RH limits and warn user if out of bounds (T_* not modified in subroutine)

                    CheckModelBoundsRH_TempEq(state, ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, FirstHVACIteration);
                    //     Check model boundaries and cap empirical model independent variables as needed (T_* may be modified on return from sub)
                    CheckModelBoundsTempEq(state, ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, T_FaceVel, FirstHVACIteration);

                    if (T_ProcInTemp != 0.0 && T_RegenInTemp != 0.0) {
                        FullLoadSupOutTemp = Coeff1 + Coeff2 * T_RegenInHumRat + Coeff3 * T_RegenInTemp + Coeff4 * (T_RegenInHumRat / T_RegenInTemp) +
                                             Coeff5 * T_ProcInHumRat + Coeff6 * T_ProcInTemp + Coeff7 * (T_ProcInHumRat / T_ProcInTemp) +
                                             Coeff8 * T_FaceVel;

                        // Check model boundary for supply (regen) temp and do not cap value if out of bounds, check that supply in temp > out temp
                        CheckModelBoundOutput_Temp(
                            state, ExNum, state.dataHeatRecovery->ExchCond(ExNum).SupInTemp, FullLoadSupOutTemp, FirstHVACIteration);
                        FullLoadDeltaT = FullLoadSupOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                    } else {
                        FullLoadDeltaT = 0.0;
                    }

                    Coeff1 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C1;
                    Coeff2 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C2;
                    Coeff3 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C3;
                    Coeff4 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C4;
                    Coeff5 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C5;
                    Coeff6 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C6;
                    Coeff7 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C7;
                    Coeff8 = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).C8;

                    H_ProcInTemp = state.dataHeatRecovery->FullLoadOutAirTemp;
                    H_ProcInHumRat = state.dataHeatRecovery->FullLoadOutAirHumRat;
                    H_RegenInTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                    H_RegenInHumRat = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
                    H_FaceVel = BalFaceVelActual;

                    //     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
                    //     Check RH limits and warn user if out of bounds (H_* not modified in subroutine)

                    CheckModelBoundsRH_HumRatEq(state, ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, FirstHVACIteration);
                    //     Check model boundaries and cap empirical model independent variables as needed (H_* may be modified on return from sub)
                    CheckModelBoundsHumRatEq(
                        state, ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, H_FaceVel, FirstHVACIteration);

                    //     Calc curve
                    if (H_ProcInTemp != 0.0 && H_RegenInTemp != 0.0) {
                        FullLoadSupOutHumRat = Coeff1 + Coeff2 * H_RegenInHumRat + Coeff3 * H_RegenInTemp +
                                               Coeff4 * (H_RegenInHumRat / H_RegenInTemp) + Coeff5 * H_ProcInHumRat + Coeff6 * H_ProcInTemp +
                                               Coeff7 * (H_ProcInHumRat / H_ProcInTemp) + Coeff8 * H_FaceVel;

                        // Check model boundary for supply (regen) hum rat and do not cap value if out of bounds, check that supply in HR < out HR
                        CheckModelBoundOutput_HumRat(
                            state, ExNum, state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat, FullLoadSupOutHumRat, FirstHVACIteration);
                        FullLoadDeltaW = FullLoadSupOutHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
                    } else {
                        FullLoadDeltaW = 0.0;
                    }

                    //     Check for saturation in the model's calculated supply outlet and reset temp, then humidity ratio at constant enthalpy
                    //     Reset delta T and delta W such that the model does not allow an outlet condition over saturation
                    TestSaturationEnthalpy = PsyHFnTdbW(FullLoadSupOutTemp, FullLoadSupOutHumRat);
                    if (PsyTsatFnHPb(state, TestSaturationEnthalpy, state.dataEnvrn->OutBaroPress, ThisSubTSat) > FullLoadSupOutTemp) {
                        FullLoadSupOutTemp = PsyTsatFnHPb(state, TestSaturationEnthalpy, state.dataEnvrn->OutBaroPress, ThisSubTSatFullLoadOutTemp);
                        FullLoadSupOutHumRat = PsyWFnTdbH(state, FullLoadSupOutTemp, TestSaturationEnthalpy, ThisSubTSatFullLoadOutHumRat);
                        FullLoadDeltaT = FullLoadSupOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                        FullLoadDeltaW = FullLoadSupOutHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
                    }

                    if (!state.dataHeatRecovery->CalledFromParentObject) {
                        //       calculate part-load ratio for HX
                        MaxHumRatNeeded = state.dataLoopNodes->Node(state.dataHeatRecovery->ExchCond(ExNum).SecOutletNode).HumRatMax;
                        MinHumRatNeeded = state.dataLoopNodes->Node(state.dataHeatRecovery->ExchCond(ExNum).SecOutletNode).HumRatMin;
                        // Calculate partload fraction of dehumidification capacity required to meet setpoint

                        //       check the model output, if the regen delta W is positive, the process air stream is dehumidified
                        if (FullLoadDeltaW > 0) {
                            //         check for a setpoint, if no setpoint then PLR remains at 1
                            if (MaxHumRatNeeded != SensedNodeFlagValue) {
                                if (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat > MaxHumRatNeeded && MaxHumRatNeeded > 0.0) {
                                    HXPartLoadRatio = (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - MaxHumRatNeeded) / FullLoadDeltaW;
                                } else {
                                    HXPartLoadRatio = 0.0;
                                }
                            }
                            //       check the model output, if the regen delta W is negative, the process air stream is humidified
                        } else if (FullLoadDeltaW < 0) {
                            //         check for a setpoint, if no setpoint then PLR remains at 1
                            if (MinHumRatNeeded != SensedNodeFlagValue) {
                                if (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat < MinHumRatNeeded && MinHumRatNeeded > 0.0) {
                                    HXPartLoadRatio = (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - MinHumRatNeeded) / FullLoadDeltaW;
                                } else {
                                    HXPartLoadRatio = 0.0;
                                }
                            }
                        }

                        HXPartLoadRatio = max(0.0, HXPartLoadRatio);
                        HXPartLoadRatio = min(1.0, HXPartLoadRatio);

                    } else if (CompanionCoilIndex > 0) {
                        // VS coil issue here?
                        HXPartLoadRatio = state.dataDXCoils->DXCoilPartLoadRatio(CompanionCoilIndex);
                    }

                    if (FanOpMode == CycFanCycCoil || RegenInletIsOANode) {
                        //       Supply (regen) air stream mass flow rate is cycling and proportional to PLR, outlet conditions are full load
                        //       conditions
                        state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp + FullLoadDeltaT;
                        state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                            min(1.0, max(1.e-5, state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat + FullLoadDeltaW));
                    } else {
                        //       Supply (regen) air stream mass flow rate is constant and outlet conditions are averaged
                        state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                            state.dataHeatRecovery->ExchCond(ExNum).SupInTemp + (FullLoadDeltaT * HXPartLoadRatio);
                        state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                            min(1.0, max(1.e-5, state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat + (FullLoadDeltaW * HXPartLoadRatio)));
                    }

                    //     for a balanced flow HX, use average mass flow rate and actual node conditions to calculate CSup and CSec
                    //     the mass flow rate on the process and secondary side of HX may be imbalanced when the HX is used in the OA branch
                    //     use the average mass flow rate to avoid psych warnings, mass flow rates will converge at the end of the iteration
                    //     if the air mass flow rates do not converge, this model should not be used
                    CSup = AverageMassFlowRate * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
                    CSec = AverageMassFlowRate * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);

                    state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                        PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);

                    SensHeatRecRate = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);

                    TotHeatRecRate = AverageMassFlowRate *
                                     (state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth - state.dataHeatRecovery->ExchCond(ExNum).SupInEnth);

                    //     now calculate process side heat transfer

                    state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                        state.dataHeatRecovery->ExchCond(ExNum).SecInEnth - TotHeatRecRate / AverageMassFlowRate;

                    state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - SensHeatRecRate / CSec;

                    state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = PsyWFnTdbH(state,
                                                                                      state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp,
                                                                                      state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth,
                                                                                      ThisSubSecOutHumRat);

                    // check for saturation in process (secondary) outlet
                    // The process outlet conditions should never be over the saturation curve for the balanced desiccant model
                    // although this may occur during warmup. This check is included here for consistency.
                    TempSecOutSat =
                        PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth, state.dataEnvrn->OutBaroPress, ThisSubTestSatSec);
                    if (TempSecOutSat > state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp) {
                        state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = TempSecOutSat;
                        state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat = PsyWFnTdbH(state,
                                                                                          state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp,
                                                                                          state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth,
                                                                                          ThisSubTSatSecOutHumRat);
                    }

                    state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate =
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExNum).PerfDataIndex).NomElecPower *
                        HXPartLoadRatio;

                } else {
                }
            }

        } // ENDIF for "IF (UnitOn) THEN"

        // Report the process side heat transfer
        CSec = AverageMassFlowRate * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
        ProcessSensHeatRecRate = CSec * (state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp - state.dataHeatRecovery->ExchCond(ExNum).SecInTemp);

        ProcessTotHeatRecRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow *
                                (state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth - state.dataHeatRecovery->ExchCond(ExNum).SecInEnth);

        ProcessLatHeatRecRate = ProcessTotHeatRecRate - ProcessSensHeatRecRate;

        // Set report variables based on sign of recovery rate
        if (ProcessSensHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = ProcessSensHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate = std::abs(ProcessSensHeatRecRate);
        }
        if (ProcessLatHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = ProcessLatHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate = std::abs(ProcessLatHeatRecRate);
        }
        if (ProcessTotHeatRecRate > 0.0) {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = ProcessTotHeatRecRate;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = 0.0;
        } else {
            state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate = 0.0;
            state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate = std::abs(ProcessTotHeatRecRate);
        }
    }

    void FrostControl(EnergyPlusData &state, int const ExNum) // number of the current heat exchanger being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates fraction of timestep necessary to eliminate frost on ERV surface
        // by comparing secondary outlet or outdoor temperature to a frost control threshold
        // temperature.  Supply air and secondary air outlet conditions are calculated
        // based on frost control method selected.

        // METHODOLOGY EMPLOYED:
        // NA

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ErrorTol(0.001); // error tolerence for iteration loop
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 DFFraction;          // fraction of timestep ERV is in frost control mode
        Real64 RhoSup;              // density of supply air [kg/m3]
        Real64 RhoSec;              // density of secondary air [kg/m3]
        Real64 Error;               // iteration loop error variable
        Real64 Iter;                // iteration counter
        Real64 CSup;                // mdot Cp of supply air [W/K]
        Real64 CSec;                // mdot Cp of secondary air [W/K]
        Real64 CMin;                // minimum mdot Cp of supply or secondary air [W/K]
        Real64 QTotTrans;           // total heat transfer by ERV [W]
        Real64 QSensTrans;          // sensible heat transfer by ERV [W]
        Real64 HXSecAirVolFlowRate; // air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
        Real64 HXSupAirVolFlowRate; // air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
        Real64 HXAvgAirVolFlowRate; // average air volume flow rate through the heat exchanger [m3/sec]
        Real64 HXAirVolFlowRatio;   // nominal to actual air volume flow ratio
        Real64 MassFlowSupIn;       // supply air mass flow rate at HX inlet
        Real64 MassFlowSupOut;      // supply air mass flow rate through HX core outlet
        Real64 MassFlowSupBypass;   // supply air bypass mass flow rate around HX core
        Real64 TempSupIn;           // supply side temperature of air entering HX
        Real64 TempSupOut;          // supply side temperature of air leaving HX core
        Real64 HumRatSupIn;         // supply side humidity ratio of air entering HX
        Real64 TempSecIn;           // secondary side temperature of air entering HX
        Real64 TempSecOut;          // secondary side temperature of air leaving HX core
        Real64 TempThreshold;       // threshold temperature below which frost control is active

        state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
        state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
        state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = 0.0;
        state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow = 0.0;
        RhoSup = PsyRhoAirFnPbTdbW(state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataHeatRecovery->ExchCond(ExNum).SupInTemp,
                                   state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
        RhoSec = PsyRhoAirFnPbTdbW(state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataHeatRecovery->ExchCond(ExNum).SecInTemp,
                                   state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
        CSup = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
        CSec = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * PsyCpAirFnW(state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat);
        CMin = min(CSup, CSec);
        TempThreshold = state.dataHeatRecovery->ExchCond(ExNum).ThresholdTemperature;

        if (state.dataHeatRecovery->ExchCond(ExNum).ControlToTemperatureSetPoint) {
            // Recalculate HX outlet conditions as if control to temperature setpoint was not activated,
            // because defrost will override those results

            HXSupAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow / RhoSup;
            HXSecAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow / RhoSec;
            HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate) / 2.0;
            HXAirVolFlowRatio = HXAvgAirVolFlowRate / state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness =
                state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75 +
                (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100 - state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75) *
                    (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
            state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness =
                state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75 +
                (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100 - state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75) *
                    (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                state.dataHeatRecovery->ExchCond(ExNum).SupInTemp +
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * CMin / CSup *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat +
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness * CMin / CSup *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);

            //   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
            if (PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress) >
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            }

            QSensTrans = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + QSensTrans / CSec;
            QTotTrans = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                state.dataHeatRecovery->ExchCond(ExNum).SecInEnth + QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);
        }

        // Check frost control by type

        if (state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "MINIMUMEXHAUSTTEMPERATURE") {
            //   A plate HX will bypass air on the supply side to keep exhaust temp above a
            //   threshold temperature and requires recalculating effectiveness based on
            //   the reduced air flow rate. A rotary HX modulates rotational speed to try to keep the
            //   exhaust air temperature above the threshold temperature. Assume that
            //   sensible and latent effectiveness decrease proportionally with rotary HX speed.

            DFFraction = max(0.0,
                             min(1.0,
                                 SafeDiv((TempThreshold - state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp),
                                         (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp))));
            if (state.dataHeatRecovery->ExchCond(ExNum).ExchConfig == HXConfigurationType::Rotary) {
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness *= (1.0 - DFFraction);
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness *= (1.0 - DFFraction);
            } else { // HX is a plate heat exchanger, bypass air to eliminate frost
                Error = 1.0;
                Iter = 0.0;
                MassFlowSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
                MassFlowSupOut = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;
                MassFlowSupBypass = state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow;
                TempSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp;
                HumRatSupIn = state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat;
                TempSecIn = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp;

                while (std::abs(Error) > ErrorTol && Iter < 10) {
                    MassFlowSupOut = MassFlowSupIn * (1.0 - DFFraction);
                    MassFlowSupBypass = MassFlowSupIn * DFFraction;
                    HXSupAirVolFlowRate = MassFlowSupOut / RhoSup;
                    HXSecAirVolFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow / RhoSec;
                    HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate) / 2.0;
                    HXAirVolFlowRatio = HXAvgAirVolFlowRate / state.dataHeatRecovery->ExchCond(ExNum).NomSupAirVolFlow;
                    CSup = MassFlowSupOut * PsyCpAirFnW(HumRatSupIn);
                    CMin = min(CSup, CSec);
                    if (TempSupIn < TempSecIn) {
                        //         Use heating effectiveness values
                        state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75 +
                                                                                    (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible100 -
                                                                                     state.dataHeatRecovery->ExchCond(ExNum).HeatEffectSensible75) *
                                                                                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                        state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75 +
                                                                                   (state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent100 -
                                                                                    state.dataHeatRecovery->ExchCond(ExNum).HeatEffectLatent75) *
                                                                                       (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                    } else {
                        //         Use cooling effectiveness values
                        state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75 +
                                                                                    (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible100 -
                                                                                     state.dataHeatRecovery->ExchCond(ExNum).CoolEffectSensible75) *
                                                                                        (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                        state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness = state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75 +
                                                                                   (state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent100 -
                                                                                    state.dataHeatRecovery->ExchCond(ExNum).CoolEffectLatent75) *
                                                                                       (HXAirVolFlowRatio - 0.75) / (1.0 - 0.75);
                    }
                    //         calculation of local variable Csup can be 0, gaurd against divide by 0.
                    TempSupOut =
                        TempSupIn + state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * SafeDiv(CMin, CSup) * (TempSecIn - TempSupIn);
                    QSensTrans = CSup * (TempSupIn - TempSupOut);
                    //         Csec cannot be 0 in this subroutine
                    TempSecOut = TempSecIn + QSensTrans / CSec;
                    Error = (TempSecOut - TempThreshold);
                    //         recalculate DFFraction until convergence, gaurd against divide by 0 (unlikely).
                    DFFraction = max(0.0, min(1.0, DFFraction * SafeDiv((TempSecIn - TempSecOut), (TempSecIn - TempThreshold))));
                    ++Iter;
                }
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow = MassFlowSupIn;
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = MassFlowSupOut;
                state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = MassFlowSupBypass;
            }
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                state.dataHeatRecovery->ExchCond(ExNum).SupInTemp +
                state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness * SafeDiv(CMin, CSup) *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat +
                state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness * SafeDiv(CMin, CSup) *
                    (state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat - state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);

            //   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
            if (PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress) >
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            }

            QSensTrans = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + QSensTrans / CSec;
            QTotTrans = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                state.dataHeatRecovery->ExchCond(ExNum).SecInEnth + QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);

            //   Perform mixing of core air stream and bypass air stream and set mass flow rates at outlet nodes
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                (state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth +
                 state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInEnth) /
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                (state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat +
                 state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupInHumRat) /
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                (state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth +
                 state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInEnth) /
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                (state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat +
                 state.dataHeatRecovery->ExchCond(ExNum).SecBypassMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat) /
                state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp =
                PsyTdbFnHW(state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth, state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;

        } // End of IF (Minimum Exhaust Temperature)

        if (state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "EXHAUSTAIRRECIRCULATION") {
            // Directing exhaust outlet air back across the HX core on the supply side
            // Assume no heat exchange when in frost control mode, full heat exchange otherwise
            DFFraction = max(0.0,
                             min((state.dataHeatRecovery->ExchCond(ExNum).InitialDefrostTime +
                                  state.dataHeatRecovery->ExchCond(ExNum).RateofDefrostTimeIncrease *
                                      (TempThreshold - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp)),
                                 1.0));

            //    Calculate derated heat transfer using outlet air conditions assuming no defrost (calculated earlier)
            //    and (1-DefrostFraction)
            QSensTrans =
                (1.0 - DFFraction) * CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
            QTotTrans = (1.0 - DFFraction) * state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);

            state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow = (1.0 - DFFraction) * state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow +
                                                                     DFFraction * state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow;

            //    Blend supply outlet condition of HX core with exhaust air inlet to get final supply air outlet conditions
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                ((1.0 - DFFraction) * state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp +
                 DFFraction * state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInTemp) /
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;

            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                ((1.0 - DFFraction) * state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat +
                 DFFraction * state.dataHeatRecovery->ExchCond(ExNum).SecInMassFlow * state.dataHeatRecovery->ExchCond(ExNum).SecInHumRat) /
                state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;

            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                PsyHFnTdbW(state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat);
            //    No need to check for saturation after SA out and EA inlet are blended

            //    Derate effectiveness based on frost control time fraction for reporting purposes
            state.dataHeatRecovery->ExchCond(ExNum).SensEffectiveness *= (1.0 - DFFraction);
            state.dataHeatRecovery->ExchCond(ExNum).LatEffectiveness *= (1.0 - DFFraction);

            //    Secondary air outlet conditions are previously calculated as the conditions when not
            //    in defrost, and this is what we want to report so no changes here.
            //    Average SupInMassFlow and SecOutMassFlow rates have been reduced due to frost control
            //      Equipment attached to the supply inlet node may have problems with our setting the
            //      mass flow rate in the next statement. This is done only to simulate exhaust air recirc.
            state.dataLoopNodes->Node(state.dataHeatRecovery->ExchCond(ExNum).SupInletNode).MassFlowRate =
                state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow * (1.0 - DFFraction);
            state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow *= (1.0 - DFFraction);

        } // End of IF (Exhaust Air Recirculation)

        if (state.dataHeatRecovery->ExchCond(ExNum).FrostControlType == "EXHAUSTONLY") {

            //   Perform frost control by bypassing the supply air around the HX core during the defrost
            //   time period. HX heat transfer is reduced proportionally to (1 - defrosttimefraction)

            DFFraction = max(0.0,
                             min((state.dataHeatRecovery->ExchCond(ExNum).InitialDefrostTime +
                                  state.dataHeatRecovery->ExchCond(ExNum).RateofDefrostTimeIncrease *
                                      (TempThreshold - state.dataHeatRecovery->ExchCond(ExNum).SupInTemp)),
                                 1.0));

            //   Calculate derated heat transfer based on defrost time
            QSensTrans =
                (1.0 - DFFraction) * CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
            QTotTrans = (1.0 - DFFraction) * state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow *
                        (state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);

            //   Calculate the air conditions leaving heat exchanger unit
            //   Heat exchanger effectiveness is not derated, HX is fully bypassed during frost control

            state.dataHeatRecovery->ExchCond(ExNum).SupBypassMassFlow = state.dataHeatRecovery->ExchCond(ExNum).SupInMassFlow * DFFraction;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - QSensTrans / CSup;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth =
                state.dataHeatRecovery->ExchCond(ExNum).SupInEnth - QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);

            if (PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress) >
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp) {
                state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp =
                    PsyTsatFnHPb(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth, state.dataEnvrn->OutBaroPress);
                state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat =
                    PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth);
                QSensTrans = CSup * (state.dataHeatRecovery->ExchCond(ExNum).SupInTemp - state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp);
                // Should we be updating the sensible and latent effectiveness values also?
            }

            state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth =
                state.dataHeatRecovery->ExchCond(ExNum).SecInEnth + QTotTrans / state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp = state.dataHeatRecovery->ExchCond(ExNum).SecInTemp + QSensTrans / CSec;
            state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat =
                PsyWFnTdbH(state, state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp, state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth);
        } // End of IF (Exhaust Only)

        state.dataHeatRecovery->ExchCond(ExNum).DefrostFraction = DFFraction;
    }

    void UpdateHeatRecovery(EnergyPlusData &state, int const ExNum) // number of the current heat exchanger being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Moves heat exchanger output to the outlet nodes.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SupInNode;  // supply inlet node number
        int SupOutNode; // supply outlet node number
        int SecInNode;  // secondary inlet node number
        int SecOutNode; // secondary outlet node number

        SupInNode = state.dataHeatRecovery->ExchCond(ExNum).SupInletNode;
        SupOutNode = state.dataHeatRecovery->ExchCond(ExNum).SupOutletNode;
        SecInNode = state.dataHeatRecovery->ExchCond(ExNum).SecInletNode;
        SecOutNode = state.dataHeatRecovery->ExchCond(ExNum).SecOutletNode;

        // Set the outlet air nodes of the heat exchanger
        state.dataLoopNodes->Node(SupOutNode).Temp = state.dataHeatRecovery->ExchCond(ExNum).SupOutTemp;
        state.dataLoopNodes->Node(SupOutNode).HumRat = state.dataHeatRecovery->ExchCond(ExNum).SupOutHumRat;
        state.dataLoopNodes->Node(SupOutNode).Enthalpy = state.dataHeatRecovery->ExchCond(ExNum).SupOutEnth;
        state.dataLoopNodes->Node(SupOutNode).MassFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SupOutMassFlow;
        state.dataLoopNodes->Node(SecOutNode).Temp = state.dataHeatRecovery->ExchCond(ExNum).SecOutTemp;
        state.dataLoopNodes->Node(SecOutNode).HumRat = state.dataHeatRecovery->ExchCond(ExNum).SecOutHumRat;
        state.dataLoopNodes->Node(SecOutNode).Enthalpy = state.dataHeatRecovery->ExchCond(ExNum).SecOutEnth;
        state.dataLoopNodes->Node(SecOutNode).MassFlowRate = state.dataHeatRecovery->ExchCond(ExNum).SecOutMassFlow;

        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(SupOutNode).Quality = state.dataLoopNodes->Node(SupInNode).Quality;
        state.dataLoopNodes->Node(SupOutNode).Press = state.dataLoopNodes->Node(SupInNode).Press;
        state.dataLoopNodes->Node(SupOutNode).MassFlowRateMin = state.dataLoopNodes->Node(SupInNode).MassFlowRateMin;
        state.dataLoopNodes->Node(SupOutNode).MassFlowRateMax = state.dataLoopNodes->Node(SupInNode).MassFlowRateMax;
        state.dataLoopNodes->Node(SupOutNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(SupInNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(SupOutNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(SupInNode).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(SecOutNode).Quality = state.dataLoopNodes->Node(SecInNode).Quality;
        state.dataLoopNodes->Node(SecOutNode).Press = state.dataLoopNodes->Node(SecInNode).Press;
        state.dataLoopNodes->Node(SecOutNode).MassFlowRateMin = state.dataLoopNodes->Node(SecInNode).MassFlowRateMin;
        state.dataLoopNodes->Node(SecOutNode).MassFlowRateMax = state.dataLoopNodes->Node(SecInNode).MassFlowRateMax;
        state.dataLoopNodes->Node(SecOutNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(SecInNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(SecOutNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(SecInNode).MassFlowRateMaxAvail;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(SupOutNode).CO2 = state.dataLoopNodes->Node(SupInNode).CO2;
            state.dataLoopNodes->Node(SecOutNode).CO2 = state.dataLoopNodes->Node(SecInNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(SupOutNode).GenContam = state.dataLoopNodes->Node(SupInNode).GenContam;
            state.dataLoopNodes->Node(SecOutNode).GenContam = state.dataLoopNodes->Node(SecInNode).GenContam;
        }
    }

    void ReportHeatRecovery(EnergyPlusData &state, int const ExNum) // number of the current heat exchanger being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       F Buhl Nov 2000, D Shirey Feb/June 2003
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fill remaining report variables

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataHeatRecovery->ExchCond(ExNum).ElecUseEnergy = state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).SensHeatingEnergy = state.dataHeatRecovery->ExchCond(ExNum).SensHeatingRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).LatHeatingEnergy = state.dataHeatRecovery->ExchCond(ExNum).LatHeatingRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).TotHeatingEnergy = state.dataHeatRecovery->ExchCond(ExNum).TotHeatingRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).SensCoolingEnergy = state.dataHeatRecovery->ExchCond(ExNum).SensCoolingRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).LatCoolingEnergy = state.dataHeatRecovery->ExchCond(ExNum).LatCoolingRate * ReportingConstant;
        state.dataHeatRecovery->ExchCond(ExNum).TotCoolingEnergy = state.dataHeatRecovery->ExchCond(ExNum).TotCoolingRate * ReportingConstant;

        state.dataHVACGlobal->AirToAirHXElecPower = state.dataHeatRecovery->ExchCond(ExNum).ElecUseRate;
    }

    Real64 SafeDiv(Real64 const a, Real64 const b)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Returns a / b while preventing division by zero

        // METHODOLOGY EMPLOYED:
        // Check for small or zero values before performing division

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 c;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        if (std::abs(b) < SMALL) {
            c = a / sign(SMALL, b);
        } else {
            c = a / b;
        }

        return c;
    }

    void CalculateEpsFromNTUandZ(EnergyPlusData &state,
                                 Real64 const NTU,              // number of transfer units
                                 Real64 const Z,                // capacity rate ratio
                                 HXConfiguration const FlowArr, // flow arrangement
                                 Real64 &Eps                    // heat exchanger effectiveness
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates eps, the exchanger effectiveness,
        // from NTU, the number of transfer units,
        // from Z, the capacity rate ratio, and
        // from the flow arrangement

        // METHODOLOGY EMPLOYED:
        // Uses the effectiveness - NTU heat exchanger formulas

        // REFERENCES:
        // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
        // LBNL Report 42354, 1999.
        // Also see:
        // ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // 1: COUNTER FLOW
        // 2: PARALLEL FLOW
        // 3: CROSS FLOW BOTH UNMIXED
        // 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
        //    (coil with one row)

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Temp; // temporary variable

        // check input validity
        if (Z < 0.0 || Z > 1.0) {
            ShowFatalError(state, format("Variable Z ({:.2R}) out of range [0.0,1.0] in CalculateEpsFromNTUandZ", Z));
        }

        // effectiveness
        if (NTU < SMALL) {
            Eps = 0.0;
        } else if (Z < SMALL) { // Eps independent of flow arrangement
            Eps = 1.0 - std::exp(-NTU);
        } else {
            {
                auto const SELECT_CASE_var(FlowArr);
                if (SELECT_CASE_var == HXConfiguration::CounterFlow) { // COUNTER FLOW
                    if (std::abs(Z - 1.0) < SMALL) {
                        Eps = NTU / (NTU + 1.0);
                    } else {
                        Temp = std::exp(-NTU * (1.0 - Z));
                        Eps = (1.0 - Temp) / (1.0 - Z * Temp);
                    }
                } else if (SELECT_CASE_var == HXConfiguration::ParallelFlow) { // PARALLEL FLOW
                    Temp = (1.0 + Z);
                    Eps = (1.0 - std::exp(-NTU * Temp)) / Temp;
                } else if (SELECT_CASE_var == HXConfiguration::CrossFlowBothUnmixed) { // CROSS FLOW BOTH UNMIXED
                    Temp = Z * std::pow(NTU, -0.22);
                    Eps = 1.0 - std::exp((std::exp(-NTU * Temp) - 1.0) / Temp);
                } else if (SELECT_CASE_var == HXConfiguration::CrossFlowOther) { // CROSS FLOW, Cmax MIXED, Cmin UNMIXED
                    Eps = (1.0 - std::exp(-Z * (1.0 - std::exp(-NTU)))) / Z;
                } else {
                    ShowFatalError(state, format("HeatRecovery: Illegal flow arrangement in CalculateEpsFromNTUandZ, Value={}", FlowArr));
                }
            }
        }
    }

    void CalculateNTUfromEpsAndZ(EnergyPlusData &state,
                                 Real64 &NTU,                   // number of transfer units
                                 int &Err,                      // error indicator
                                 Real64 const Z,                // capacity rate ratio
                                 HXConfiguration const FlowArr, // flow arrangement
                                 Real64 const Eps               // heat exchanger effectiveness
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates NTU, the number of transfer units,
        // based on eps, the exchanger effectiveness,
        // Z, the capacity rate ratio, and
        // from the flow arrangement

        // METHODOLOGY EMPLOYED:
        // Uses the effectiveness - NTU heat exchanger formulas

        // REFERENCES:
        // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
        // LBNL Report 42354, 1999.
        // Also see:
        // ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // 1: COUNTER FLOW
        // 2: PARALLEL FLOW
        // 3: CROSS FLOW BOTH UNMIXED
        // 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
        //    (coil with one row)

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        NTU = 0.0;
        // check input validity
        if (Z < 0.0 || Z > 1.0) {
            Err = 1;
            return;
        }

        if (FlowArr == HXConfiguration::ParallelFlow) {
            if (Eps < 0.0 || Eps > 1.0 / (1.0 + Z)) {
                Err = 2;
                return;
            }
        } else if (FlowArr == HXConfiguration::CrossFlowOther) {
            if (Eps < 0.0 || Eps > (1.0 - std::exp(-Z)) / Z) {
                Err = 3;
                return;
            }
            // check product (Eps*Z)
            if (Eps * Z < 0.0 || Eps * Z > 1.0 - std::exp(Z * (SMALL - 1.0))) {
                Err = 4;
                return;
            }
            // check product (Eps*Z)
        } else {
            if (Eps < 0.0 || Eps > 1.0) {
                Err = 5;
                return;
            }
        }

        if (Eps < SMALL) { // no effectiveness. Set NTU = 0
            NTU = 0.0;
        } else if (Z < SMALL) { // Eps independent of flow arrangement
            NTU = -std::log(1.0 - Eps);
        } else {
            // calculate based on configuration
            {
                auto const SELECT_CASE_var(FlowArr);
                if (SELECT_CASE_var == HXConfiguration::CounterFlow) { // COUNTER FLOW
                    if (std::abs(Z - 1.0) < SMALL) {
                        NTU = Eps / (1.0 - Eps);
                    } else {
                        NTU = 1.0 / (Z - 1.0) * std::log((1.0 - Eps) / (1.0 - Eps * Z));
                    }
                } else if (SELECT_CASE_var == HXConfiguration::ParallelFlow) { // PARALLEL FLOW
                    NTU = -std::log(-Eps - Eps * Z + 1.0) / (Z + 1.0);
                } else if (SELECT_CASE_var == HXConfiguration::CrossFlowBothUnmixed) { // CROSS FLOW BOTH UNMIXED
                    NTU = GetNTUforCrossFlowBothUnmixed(state, Eps, Z);
                } else if (SELECT_CASE_var == HXConfiguration::CrossFlowOther) { // CROSS FLOW, Cmax MIXED, Cmin UNMIXED
                    NTU = -std::log(1.0 + std::log(1.0 - Eps * Z) / Z);
                } else {
                    ShowFatalError(state, format("HeatRecovery: Illegal flow arrangement in CalculateNTUfromEpsAndZ, Value={}", FlowArr));
                }
            }
        }
    }

    Real64 GetNTUforCrossFlowBothUnmixed(EnergyPlusData &state,
                                         Real64 const Eps, // heat exchanger effectiveness
                                         Real64 const Z    // capacity rate ratio
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the NTU value based on the exchanger effectiveness
        // and the capacity ratio for cross flow exchanger, both
        // streams unmixed

        // METHODOLOGY EMPLOYED:
        // Uses a Regula Falsi solver function to numerically invert the formula
        // giving effectiveness as a function of NTU and Z..

        // REFERENCES:
        // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
        // LBNL Report 42354, 1999.
        // Also see:
        // ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

        // USE STATEMENTS:
        // na

        // Return value
        Real64 NTU; // result variable; number of transfer units

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        Real64 const Acc(0.0001); // Accuracy of result
        int const MaxIte(500);    // Maximum number of iterations

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        int SolFla;              // Flag of solver
        Real64 const NTU0(0.0);  // lower bound for NTU
        Real64 const NTU1(50.0); // upper bound for NTU
        std::array<Real64, 2> Par = {Eps, Z};

        SolveRoot(state, Acc, MaxIte, SolFla, NTU, GetResidCrossFlowBothUnmixed, NTU0, NTU1, Par);

        if (SolFla == -2) {
            ShowFatalError(state, "HeatRecovery: Bad initial bounds for NTU in GetNTUforCrossFlowBothUnmixed");
        } else if (SolFla == -1) {
            ShowFatalError(state, "HeatRecovery: No convergence in solving for NTU in GetNTUforCrossFlowBothUnmixed");
        }

        return NTU;
    }

    Real64 GetResidCrossFlowBothUnmixed([[maybe_unused]] EnergyPlusData &state,
                                        Real64 const NTU,                // number of transfer units
                                        std::array<Real64, 2> const &Par // par(1) = Eps, par(2) = Z
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // From the formula Eps = f(NTU,Z) this function finds the
        // residual of f(NTU,Z) - Eps for a cross flow heat exchanger,
        // both streams unmixed.

        // METHODOLOGY EMPLOYED:
        // Uses the effectiveness - NTU heat exchanger formula for cross
        // flow, both streams unmixed.

        // REFERENCES:
        // M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
        // LBNL Report 42354, 1999.
        // Also see:
        // ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        Residuum = 1.0 - std::exp((std::exp(-std::pow(NTU, 0.78) * Par[1]) - 1.0) / Par[1] * std::pow(NTU, 0.22)) - Par[0];

        return Residuum;
    }

    void CheckModelBoundsTempEq(EnergyPlusData &state,
                                int const ExchNum,            // number of the current heat exchanger being simulated
                                Real64 &T_RegenInTemp,        // current regen inlet temperature (C) for regen outlet temp eqn
                                Real64 &T_RegenInHumRat,      // current regen inlet hum rat for regen outlet temp eqn
                                Real64 &T_ProcInTemp,         // current process inlet temperature (C) for regen outlet temp eqn
                                Real64 &T_ProcInHumRat,       // current process inlet hum rat for regen outlet temp eqn
                                Real64 &T_FaceVel,            // current process and regen face velocity (m/s)
                                bool const FirstHVACIteration // First HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables are within the limits used during the
        // developement of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided by the user and are based on the limits of the
        // empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
        // routine.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS
        // regen outlet temp equation

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &OutputChar = state.dataHeatRecovery->OutputChar;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi;
        auto &CharValue = state.dataHeatRecovery->CharValue;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast;
        // current end time is compared with last to see if time step changed

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error for variables of regeneration outlet temperature equation
            // Regen inlet temp for temp eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInTempMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air temperatures that are outside the regeneration outlet air temperature "
                                      "equation model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air temp used in regen outlet air temperature equation is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempLast);
                }
            }
            // Regen inlet humidity ratio for temp eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInHumRatMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air humidity ratios that are outside the regeneration outlet air temperature "
                                      "equation model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air humidity ratio used in regen outlet temperature equation is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatLast);
                }
            }
            // Process inlet temp for temp eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempErrorCount < 2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air temperatures that are outside the regeneration outlet air temperature equation "
                                      "model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air temperature used in regen outlet temperature equation is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempLast);
                }
            }
            // Process inlet humidity ratio for temp eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air humidity ratios that are outside the regeneratoin outlet air temperature equation "
                                      "model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air humidity ratio used in regen outlet temperature equation is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatLast);
                }
            }
            // Process and regeneration face velocity for temp eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_FaceVelMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelErrorCount < 2) {
                    ShowWarningError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer1);
                    ShowContinueError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer2);
                    ShowContinueError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer3);
                    ShowContinueError(state,
                                      "...Using process and regeneration face velocities that are outside the regeneration outlet air temperature "
                                      "equation model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process and regen inlet air face velocity used in regen outlet temperature equation is "
                            "out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelocityErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelLast);
                }
            }
        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        //   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
        if (std::abs(T_RegenInTemp - T_ProcInTemp) < SMALL) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInTempMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInHumRatMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_FaceVelMessage = false;
            return;
        }

        //   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
        // checking model bounds for variables of regeneration outlet temperature equation
        // Regen inlet temp
        if (T_RegenInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInTemp ||
            T_RegenInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempLast = T_RegenInTemp;
            OutputChar = format("{:.2R}", T_RegenInTemp);
            OutputCharLo = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInTemp);
            OutputCharHi = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInTemp);
            if (T_RegenInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInTemp) {
                T_RegenInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInTemp;
            }
            if (T_RegenInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInTemp) {
                T_RegenInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInTemp;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInTempMessage = true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration inlet air temperature used in regen outlet air temperature equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", T_RegenInTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInTempBuffer3 =
                    "...Regeneration outlet air temperature equation: regeneration inlet air temperature passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInTempMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInTempMessage = false;
        }
        // regen inlet humidity ratio
        if (T_RegenInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInHumRat ||
            T_RegenInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatLast =
                T_RegenInHumRat;
            OutputChar = format("{:.6R}", T_RegenInHumRat);
            OutputCharLo = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInHumRat);
            OutputCharHi = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInHumRat);
            if (T_RegenInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInHumRat) {
                T_RegenInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInHumRat;
            }
            if (T_RegenInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInHumRat) {
                T_RegenInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInHumRat;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInHumRatMessage =
                    true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration inlet air humidity ratio used in regen outlet air temperature equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", T_RegenInHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_RegenInHumRatBuffer3 =
                    "...Regeneration outlet air temperature equation: regeneration inlet air humidity ratio passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInHumRatMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_RegenInHumRatMessage = false;
        }
        // process inlet temp
        if (T_ProcInTemp < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInTemp ||
            T_ProcInTemp > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempLast = T_ProcInTemp;
            OutputChar = format("{:.2R}", T_ProcInTemp);
            OutputCharLo = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInTemp);
            OutputCharHi = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInTemp);
            if (T_ProcInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInTemp) {
                T_ProcInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInTemp;
            }
            if (T_ProcInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInTemp) {
                T_ProcInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInTemp;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage = true;
                //       Suppress warning message when process inlet temperature = 0 (DX coil is off)
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempLast == 0.0)
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage =
                        false;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process inlet air temperature used in regen outlet air temperature equation is outside model boundaries at " + OutputChar +
                    '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ',' +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", T_ProcInTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInTempBuffer3 =
                    "...Regeneration outlet air temperature equation: process inlet air temperature passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInTempMessage = false;
        }
        // process inlet humidity ratio
        if (T_ProcInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInHumRat ||
            T_ProcInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatLast = T_ProcInHumRat;
            OutputChar = format("{:.6R}", T_ProcInHumRat);
            OutputCharLo = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInHumRat);
            OutputCharHi = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInHumRat);
            if (T_ProcInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInHumRat) {
                T_ProcInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInHumRat;
            }
            if (T_ProcInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInHumRat) {
                T_ProcInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInHumRat;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage =
                    true;
                //       Suppress warning message when process inlet humrat = 0 (DX coil is off)
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatLast == 0.0)
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage =
                        false;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process inlet air humidity ratio used in regen outlet air temperature equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", T_ProcInHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatBuffer3 =
                    "...Regeneration outlet air temperature equation: process inlet air humidity ratio passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_ProcInHumRatMessage = false;
        }
        // regeneration and process face velocity
        if (T_FaceVel < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinFaceVel ||
            T_FaceVel > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxFaceVel) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelLast = T_FaceVel;
            OutputChar = format("{:.6R}", T_FaceVel);
            OutputCharLo =
                format("{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinFaceVel);
            OutputCharHi =
                format("{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxFaceVel);
            if (T_FaceVel < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinFaceVel) {
                T_FaceVel = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinFaceVel;
            }
            if (T_FaceVel > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxFaceVel) {
                T_FaceVel = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxFaceVel;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_FaceVelMessage = true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process and regen inlet air face velocity used in regen outlet air temperature equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", T_FaceVel);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_FaceVelBuffer3 =
                    "...Regeneration outlet air temperature equation: process and regen face velocity passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_FaceVelMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintT_FaceVelMessage = false;
        }
    }

    void CheckModelBoundsHumRatEq(EnergyPlusData &state,
                                  int const ExchNum,            // number of the current heat exchanger being simulated
                                  Real64 &H_RegenInTemp,        // current regen inlet temperature (C) for regen outlet hum rat eqn
                                  Real64 &H_RegenInHumRat,      // current regen inlet hum rat for regen outlet hum rat eqn
                                  Real64 &H_ProcInTemp,         // current process inlet temperature (C) for regen outlet hum rat eqn
                                  Real64 &H_ProcInHumRat,       // current process inlet hum rat for regen outlet hum rat eqn
                                  Real64 &H_FaceVel,            // current process and regen face velocity (m/s)
                                  bool const FirstHVACIteration // First HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables are within the limits used during the
        // developement of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided by the user and are based on the limits of the
        // empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
        // routine.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // regen outlet humidity ratio equation

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &OutputChar = state.dataHeatRecovery->OutputChar2;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo2;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi2;
        auto &CharValue = state.dataHeatRecovery->CharValue2;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast2;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime2;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast2;

        // current end time is compared with last to see if time step changed

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error for variables of regeneration outlet humidity ratio equation
            // Regen inlet temp for humidity ratio eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInTempMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air temperatures that are outside the regeneration inlet air temperature equation "
                                      "model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air temperature used in regen outlet air humidity ratio equation is "
                            "out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempLast);
                }
            }
            // Regen inlet humidity ratio for humidity ratio eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInHumRatMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air humidity ratios that are outside the regeneration outlet air humidity ratio "
                                      "equation model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air humidity ratio used in regen outlet air humidity ratio equation "
                            "is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatLast);
                }
            }
            // Process inlet temp for humidity ratio eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempErrorCount < 2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air temperatures that are outside the regeneration outlet air humidity ratio equation "
                                      "model may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air temperature used in regen outlet air humidity ratio equation is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempLast);
                }
            }
            // Process inlet humidity ratio for humidity ratio eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air humidity ratios that are outside the regeneration outlet humidity ratio equation "
                                      "model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air humidity ratio used in regen outlet air humidity ratio equation is "
                            "out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_ProcInHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatLast);
                }
            }
            // Process and regeneration face velocity for humidity ratio eqn
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_FaceVelMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelErrorCount < 2) {
                    ShowWarningError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer1);
                    ShowContinueError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer2);
                    ShowContinueError(
                        state, state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer3);
                    ShowContinueError(state,
                                      "...Using process and regeneration face velocities that are outside the regeneration outlet air humidity ratio "
                                      "equation model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process and regen face velocity used in regen outlet air humidity ratio equation is out "
                            "of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelocityErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelLast);
                }
            }
        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        //   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
        if (std::abs(H_RegenInTemp - H_ProcInTemp) < SMALL) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInTempMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInHumRatMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_FaceVelMessage = false;
            return;
        }

        //   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
        // checking model bounds for variables of regeneration outlet humidity ratio equation
        // Regen inlet temp
        if (H_RegenInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInTemp ||
            H_RegenInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempLast = H_RegenInTemp;
            OutputChar = format("{:.2R}", H_RegenInTemp);
            OutputCharLo = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInTemp);
            OutputCharHi = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInTemp);
            if (H_RegenInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInTemp) {
                H_RegenInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInTemp;
            }
            if (H_RegenInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInTemp) {
                H_RegenInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInTemp;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInTempMessage = true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration inlet air temperature used in regen outlet air humidity ratio equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + " , " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.2R}", H_RegenInTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInTempBuffer3 =
                    "...Regeneration outlet air humidity ratio equation: regeneration inlet air temperature passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInTempMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInTempMessage = false;
        }
        // regen inlet humidity ratio
        if (H_RegenInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInHumRat ||
            H_RegenInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatLast =
                H_RegenInHumRat;
            OutputChar = format("{:.6R}", H_RegenInHumRat);
            OutputCharLo = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInHumRat);
            OutputCharHi = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInHumRat);
            if (H_RegenInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInHumRat) {
                H_RegenInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInHumRat;
            }
            if (H_RegenInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInHumRat) {
                H_RegenInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInHumRat;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInHumRatMessage =
                    true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration inlet air humidity ratio used in regen outlet air humidity ratio equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", H_RegenInHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_RegenInHumRatBuffer3 =
                    "...Regeneration outlet air humidity ratio equation: regeneration inlet air humidity ratio passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInHumRatMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_RegenInHumRatMessage = false;
        }
        // process inlet temp
        if (H_ProcInTemp < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInTemp ||
            H_ProcInTemp > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempLast = H_ProcInTemp;
            OutputChar = format("{:.2R}", H_ProcInTemp);
            OutputCharLo = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInTemp);
            OutputCharHi = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInTemp);
            if (H_ProcInTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInTemp) {
                H_ProcInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInTemp;
            }
            if (H_ProcInTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInTemp) {
                H_ProcInTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInTemp;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage = true;
                //       Suppress warning message when process inlet temperature = 0 (DX coil is off)
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempLast == 0.0)
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage =
                        false;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process inlet air temperature used in regen outlet air humidity ratio equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", H_ProcInTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInTempBuffer3 =
                    "...Regeneration outlet air humidity ratio equation: process inlet air temperature passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInTempMessage = false;
        }
        // process inlet humidity ratio
        if (H_ProcInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInHumRat ||
            H_ProcInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatLast = H_ProcInHumRat;
            OutputChar = format("{:.6R}", H_ProcInHumRat);
            OutputCharLo = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInHumRat);
            OutputCharHi = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInHumRat);
            if (H_ProcInHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInHumRat) {
                H_ProcInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInHumRat;
            }
            if (H_ProcInHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInHumRat) {
                H_ProcInHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInHumRat;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage =
                    true;
                //       Suppress warning message when process inlet humrat = 0 (DX coil is off)
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatLast == 0.0)
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage =
                        false;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process inlet air humidity ratio used in regen outlet air humidity ratio equation is outside model boundaries at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", H_ProcInHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_ProcInHumRatBuffer3 =
                    "...Regeneration outlet air humidity ratio equation: process inlet air humidity ratio passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_ProcInHumRatMessage = false;
        }
        // regeneration and process face velocity
        if (H_FaceVel < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinFaceVel ||
            H_FaceVel > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxFaceVel) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelLast = H_FaceVel;
            OutputChar = format("{:.6R}", H_FaceVel);
            OutputCharLo =
                format("{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinFaceVel);
            OutputCharHi =
                format("{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxFaceVel);
            if (H_FaceVel < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinFaceVel) {
                H_FaceVel = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinFaceVel;
            }
            if (H_FaceVel > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxFaceVel) {
                H_FaceVel = state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxFaceVel;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_FaceVelMessage = true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Process and regen inlet air face velocity used in regen outlet air humidity ratio equation is outside model boundaries "
                    "at " +
                    OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", H_FaceVel);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_FaceVelBuffer3 =
                    "...Regeneration outlet air humidity ratio equation: process and regeneration face velocity passed to the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_FaceVelMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintH_FaceVelMessage = false;
        }
    }

    void CheckModelBoundOutput_Temp(EnergyPlusData &state,
                                    int const ExchNum,            // number of the current heat exchanger being simulated
                                    Real64 const RegenInTemp,     // current regen inlet temp passed to eqn
                                    Real64 &RegenOutTemp,         // current regen outlet temp from eqn
                                    bool const FirstHVACIteration // First HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables are within the limits used during the
        // developement of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided by the user and are based on the limits of the
        // empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
        // routine.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &OutputChar = state.dataHeatRecovery->OutputChar3;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo3;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi3;
        auto &CharValue = state.dataHeatRecovery->CharValue3;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast3;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime3;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast3;
        // current end time is compared with last to see if time step changed

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error when regeneration outlet temperature is greater than regen inlet temperature
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempFailedMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                        .RegenOutTempFailedErrorCount < 2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .RegenOutTempFailedBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenOutTempFailedBuffer2);
                    ShowContinueError(state,
                                      "...Regeneration outlet air temperature should always be less than or equal to regen inlet air temperature. "
                                      "Verify correct model coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration outlet air temperature above regen inlet air temperature error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .RegenOutTempFailedErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedLast);
                }
            }

            // print error for variables of regeneration outlet temperature
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempErrorCount < 2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer3);
                    ShowContinueError(state,
                                      "...Regeneration outlet air temperature should always be less than or equal to regen inlet air temperature. "
                                      "Verify correct model coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration outlet air temperature should be less than regen inlet air temperature error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempLast);
                }
            }
        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        // checking model regeneration outlet temperature to always be less than or equal to regeneration inlet temperature
        if (RegenOutTemp > RegenInTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedLast =
                RegenOutTemp;
            OutputChar = format("{:.2R}", RegenOutTemp);
            OutputCharHi = format("{:.2R}", RegenInTemp);
            //      IF(RegenOutTemp .GT. RegenInTemp)THEN
            //        RegenOutTemp = RegenInTemp
            //      END IF
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempFailedMessage =
                    true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration outlet air temperature is greater than inlet temperature at " + OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedBuffer2 =
                    "...Regen inlet air temperature = " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", RegenOutTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempFailedBuffer3 =
                    "...Regen outlet air temperature equation: regeneration outlet air temperature allowed from the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage = false;
        }

        //   check boundaries of regen outlet temperature and post warnings to individual buffers to print at end of time step
        // checking model bounds for regeneration outlet temperature
        if (RegenOutTemp < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutTemp ||
            RegenOutTemp > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutTemp) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempLast = RegenOutTemp;
            OutputChar = format("{:.2R}", RegenOutTemp);
            OutputCharLo = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutTemp);
            OutputCharHi = format(
                "{:.2R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutTemp);
            if (RegenOutTemp <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutTemp) {
                RegenOutTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutTemp;
            }
            if (RegenOutTemp >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutTemp) {
                RegenOutTemp =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutTemp;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage = true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", RegenOutTemp);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutTempBuffer3 =
                    "...Regen outlet air temperature equation: regeneration outlet air temperature allowed from the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage = false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutTempMessage = false;
        }
    }

    void CheckModelBoundOutput_HumRat(EnergyPlusData &state,
                                      int const ExchNum,            // number of the current heat exchanger being simulated
                                      Real64 const RegenInHumRat,   // current regen inlet hum rat passed to eqn
                                      Real64 &RegenOutHumRat,       // current regen outlet hum rat from eqn
                                      bool const FirstHVACIteration // First HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables are within the limits used during the
        // developement of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided by the user and are based on the limits of the
        // empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
        // routine.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &OutputChar = state.dataHeatRecovery->OutputChar4;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo4;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi4;
        auto &CharValue = state.dataHeatRecovery->CharValue4;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast4;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime4;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast4;
        // current end time is compared with last to see if time step changed

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error when regeneration outlet humidity ratio is less than regeneration inlet humidity ratio
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatFailedMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                        .RegenOutHumRatFailedErrorCount < 2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .RegenOutHumRatFailedBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenOutHumRatFailedBuffer2);
                    ShowContinueError(state,
                                      "...Regeneration outlet air humidity ratio should always be greater than or equal to regen inlet air humidity "
                                      "ratio. Verify correct model coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration outlet air humidity ratio should be greater than regen inlet air humidity ratio error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .RegenOutHumRatFailedErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .RegenOutHumRatFailedLast);
                }
            }

            // print error for regeneration outlet humidity ratio
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatMessage) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer3);
                    ShowContinueError(
                        state,
                        "...Regeneration outlet air humidity ratio outside model boundaries may adversely affect desiccant model performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration outlet air humidity ratio is out of range error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatLast);
                }
            }
        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        // checking for regeneration outlet humidity ratio less than or equal to regeneration inlet humidity ratio
        if (RegenOutHumRat < RegenInHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedLast =
                RegenOutHumRat;
            OutputChar = format("{:.6R}", RegenOutHumRat);
            OutputCharHi = format("{:.6R}", RegenInHumRat);
            //      IF(RegenOutHumRat .LT. RegenInHumRat)THEN
            //        RegenOutHumRat = RegenInHumRat
            //      END IF
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatFailedMess =
                    true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration outlet air humidity ratio is less than the inlet air humidity ratio at " + OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedBuffer2 =
                    "...Regen inlet air humidity ratio = " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", RegenOutHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatFailedBuffer3 =
                    "...Regen outlet air humidity ratio equation: regeneration outlet air humidity ratio allowed from the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatFailedMess =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatFailedMess =
                false;
        }

        //   check boundaries of regen outlet humrat and post warnings to individual buffers to print at end of time step
        // checking model bounds for regeneration outlet humidity ratio
        if (RegenOutHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutHumRat ||
            RegenOutHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutHumRat) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatLast = RegenOutHumRat;
            OutputChar = format("{:.6R}", RegenOutHumRat);
            OutputCharLo = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutHumRat);
            OutputCharHi = format(
                "{:.6R}", state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutHumRat);
            if (RegenOutHumRat <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutHumRat) {
                RegenOutHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MinRegenAirOutHumRat;
            }
            if (RegenOutHumRat >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutHumRat) {
                RegenOutHumRat =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).MaxRegenAirOutHumRat;
            }
            if (!state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatMessage =
                    true;
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer1 =
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                    state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                    "\" - Regeneration outlet air humidity ratio is outside model boundaries at " + OutputChar + '.';
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer2 =
                    "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ", " + CreateSysTimeIntervalString(state);
                CharValue = format("{:.6R}", RegenOutHumRat);
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenOutHumRatBuffer3 =
                    "...Regen outlet air humidity ratio equation: regeneration outlet air humidity ratio allowed from the model = " + CharValue;
            } else {
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatMessage =
                    false;
            }
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenOutHumRatMessage = false;
        }
    }

    void CheckModelBoundsRH_TempEq(EnergyPlusData &state,
                                   int const ExchNum,            // number of the current heat exchanger being simulated
                                   Real64 const T_RegenInTemp,   // current regen inlet temperature passed to eqn
                                   Real64 const T_RegenInHumRat, // current regen inlet hum rat passed to eqn
                                   Real64 const T_ProcInTemp,    // current process inlet temperature passed to eqn
                                   Real64 const T_ProcInHumRat,  // current regen outlet hum rat from eqn
                                   bool const FirstHVACIteration // first HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables result in a relative humidity that is within the range
        // of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // In addition, the range of relative humidities in the original data set may influence the output of the
        // empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
        // user if these relative humidities are out of bounds based on the limits set by the user.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        using Psychrometrics::PsyRhFnTdbWPb;

        auto &RegenInletRH = state.dataHeatRecovery->RegenInletRH2;
        auto &ProcInletRH = state.dataHeatRecovery->ProcInletRH2;
        auto &OutputChar = state.dataHeatRecovery->OutputChar6;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo6;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi6;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast6;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime6;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast6;
        // current end time is compared with last to see if time step changed

        if (state.dataGlobal->WarmupFlag || FirstHVACIteration) return;

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error when regeneration inlet relative humidity is outside model boundaries
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumTempMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempErrorCount <
                    2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .RegenInRelHumTempBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenInRelHumTempBuffer2);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenInRelHumTempBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air relative humidities that are outside the regeneration outlet temperature "
                                      "equation model boundaries may adversely affect desiccant model performance. Verify correct model "
                                      "coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air relative humidity related to regen outlet air temperature "
                            "equation is outside model boundaries error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .RegenInRelHumTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempLast);
                }
            }

            // print error when process inlet relative humidity is outside model boundaries
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumTempMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempErrorCount <
                    2) {
                    ShowWarningError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer1);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer2);
                    ShowContinueError(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air relative humidities that are outside the regeneration outlet temperature equation "
                                      "model boundaries may adversely affect desiccant model performance. Verify correct model coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air relative humidity related to regen outlet air temperature equation is "
                            "outside model boundaries error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempLast);
                }
            }

        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //     save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        //     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
        //                           *
        //                          *
        //                  x------*---------- T_HumRat
        //                  |    *
        //                  |  *
        //                  *----------------- PsyWFnTdpPb(Tdp,Pb)
        //               *  |
        //                  |
        //                T_Temp
        if (T_RegenInHumRat > PsyWFnTdpPb(state, T_RegenInTemp, state.dataEnvrn->OutBaroPress) ||
            T_ProcInHumRat > PsyWFnTdpPb(state, T_ProcInTemp, state.dataEnvrn->OutBaroPress)) {
            //       reset RH print flags just in case
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumTempMess = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumTempMess = false;
            return;
        }

        //     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
        if (std::abs(T_RegenInTemp - T_ProcInTemp) < SMALL) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumTempMess = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumTempMess = false;
            return;
        }

        RegenInletRH = PsyRhFnTdbWPb(state, T_RegenInTemp, T_RegenInHumRat, state.dataEnvrn->OutBaroPress);
        ProcInletRH = min(1.0, PsyRhFnTdbWPb(state, T_ProcInTemp, T_ProcInHumRat, state.dataEnvrn->OutBaroPress));

        // checking if regeneration inlet relative humidity is within model boundaries
        if (RegenInletRH <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInRelHum ||
            RegenInletRH >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInRelHum) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempLast =
                RegenInletRH * 100.0;
            OutputChar = format("{:.1R}", RegenInletRH * 100.0);
            OutputCharLo = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinRegenAirInRelHum * 100.0);
            OutputCharHi = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxRegenAirInRelHum * 100.0);
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumTempMess = true;

            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempBuffer1 =
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                "\" - Regeneration inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries at " +
                OutputChar + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempBuffer2 =
                "...Model limit on regeneration inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumTempBuffer3 =
                "...Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ", " +
                CreateSysTimeIntervalString(state);
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumTempMess = false;
        }

        // checking if process inlet relative humidity is within model boundaries
        if (ProcInletRH < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInRelHum ||
            ProcInletRH > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInRelHum) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempLast =
                ProcInletRH * 100.0;
            OutputChar = format("{:.1R}", ProcInletRH * 100.0);
            OutputCharLo = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MinProcAirInRelHum * 100.0);
            OutputCharHi = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).T_MaxProcAirInRelHum * 100.0);
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumTempMess = true;

            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer1 =
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                "\" - Process inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries at " +
                OutputChar + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer2 =
                "...Model limit on process inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumTempBuffer3 =
                "...Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ", " +
                CreateSysTimeIntervalString(state);
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumTempMess = false;
        }
    }

    void CheckModelBoundsRH_HumRatEq(EnergyPlusData &state,
                                     int const ExchNum,            // number of the current heat exchanger being simulated
                                     Real64 const H_RegenInTemp,   // current regen inlet temperature passed to eqn
                                     Real64 const H_RegenInHumRat, // current regen inlet hum rat passed to eqn
                                     Real64 const H_ProcInTemp,    // current process inlet temperature passed to eqn
                                     Real64 const H_ProcInHumRat,  // current process inlet hum rat passed to eqn
                                     bool const FirstHVACIteration // first HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables result in a relative humidity that is within the range
        // of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // In addition, the range of relative humidities in the original data set may influence the output of the
        // empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
        // user if these relative humidities are out of bounds based on the limits set by the user.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        using Psychrometrics::PsyRhFnTdbWPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &RegenInletRH = state.dataHeatRecovery->RegenInletRH;
        auto &ProcInletRH = state.dataHeatRecovery->ProcInletRH;
        auto &OutputChar = state.dataHeatRecovery->OutputChar5;
        auto &OutputCharLo = state.dataHeatRecovery->OutputCharLo5;
        auto &OutputCharHi = state.dataHeatRecovery->OutputCharHi5;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast5;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime5;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast5;
        // current end time is compared with last to see if time step changed

        if (state.dataGlobal->WarmupFlag || FirstHVACIteration) return;

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error when regeneration inlet relative humidity is outside model boundaries
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumHumRatMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                        .RegenInRelHumHumRatErrorCount < 2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .RegenInRelHumHumRatBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenInRelHumHumRatBuffer2);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .RegenInRelHumHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using regeneration inlet air relative humidities that are outside the regeneration outlet humidity ratio "
                                      "equation model boundaries may adversely affect desiccant model performance. Verify correct model "
                                      "coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Regeneration inlet air relative humidity related to regen outlet air humidity ratio "
                            "equation is outside model boundaries error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .RegenInRelHumHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatLast);
                }
            }

            // print error when process inlet relative humidity is outside model boundaries
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumHumRatMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                        .ProcInRelHumHumRatErrorCount < 2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .ProcInRelHumHumRatBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .ProcInRelHumHumRatBuffer2);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .ProcInRelHumHumRatBuffer3);
                    ShowContinueError(state,
                                      "...Using process inlet air relative humidities that are outside the regeneration outlet humidity ratio "
                                      "equation model boundaries may adversely affect desiccant model performance. Verify correct model "
                                      "coefficients.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                            "\" - Process inlet air relative humidity related to regen outlet air humidity ratio equation "
                            "is outside model boundaries error continues...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                            .ProcInRelHumHumRatErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatLast,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatLast);
                }
            }

        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //     save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        //     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
        //                           *
        //                          *
        //                  x------*---------- H_HumRat
        //                  |    *
        //                  |  *
        //                  *----------------- PsyWFnTdpPb(Tdp,Pb)
        //               *  |
        //                  |
        //                H_Temp
        if (H_RegenInHumRat > PsyWFnTdpPb(state, H_RegenInTemp, state.dataEnvrn->OutBaroPress) ||
            H_ProcInHumRat > PsyWFnTdpPb(state, H_ProcInTemp, state.dataEnvrn->OutBaroPress)) {
            //       reset RH print flags just in case
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumHumRatMess = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumHumRatMess = false;
            return;
        }

        //     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
        if (std::abs(H_RegenInTemp - H_ProcInTemp) < SMALL) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumHumRatMess = false;
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumHumRatMess = false;
            return;
        }

        RegenInletRH = PsyRhFnTdbWPb(state, H_RegenInTemp, H_RegenInHumRat, state.dataEnvrn->OutBaroPress);
        ProcInletRH = min(1.0, PsyRhFnTdbWPb(state, H_ProcInTemp, H_ProcInHumRat, state.dataEnvrn->OutBaroPress));

        // checking if regeneration inlet relative humidity is within model boundaries
        if (RegenInletRH <
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInRelHum ||
            RegenInletRH >
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInRelHum) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatLast =
                RegenInletRH * 100.0;
            OutputChar = format("{:.1R}", RegenInletRH * 100.0);
            OutputCharLo = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinRegenAirInRelHum * 100.0);
            OutputCharHi = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxRegenAirInRelHum * 100.0);
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumHumRatMess = true;

            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatBuffer1 =
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                "\" - Regeneration inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries at " +
                OutputChar + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatBuffer2 =
                "...Model limit on regeneration inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).RegenInRelHumHumRatBuffer3 =
                "...Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ", " +
                CreateSysTimeIntervalString(state);
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintRegenInRelHumHumRatMess = false;
        }

        // checking if process inlet relative humidity is within model boundaries
        if (ProcInletRH < state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInRelHum ||
            ProcInletRH > state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInRelHum) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatLast =
                ProcInletRH * 100.0;
            OutputChar = format("{:.1R}", ProcInletRH * 100.0);
            OutputCharLo = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MinProcAirInRelHum * 100.0);
            OutputCharHi = format(
                "{:.1R}",
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).H_MaxProcAirInRelHum * 100.0);
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumHumRatMess = true;

            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatBuffer1 =
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PerfType + " \"" +
                state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).Name +
                "\" - Process inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries at " +
                OutputChar + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatBuffer2 =
                "...Model limit on process inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ProcInRelHumHumRatBuffer3 =
                "...Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ", " +
                CreateSysTimeIntervalString(state);
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintProcInRelHumHumRatMess = false;
        }
    }

    void CheckForBalancedFlow(EnergyPlusData &state,
                              int const ExchNum,              // number of the current heat exchanger being simulated
                              Real64 const ProcessInMassFlow, // current process inlet air mass flow rate (m3/s)
                              Real64 const RegenInMassFlow,   // current regeneration inlet air mass flow rate (m3/s)
                              bool const FirstHVACIteration   // first HVAC iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the balanced flow desiccant heat exchanger has the same regeneration and process air flow rates.

        // METHODOLOGY EMPLOYED:
        // Check that the regeneration and process air mass flow rates are within 2%.
        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using General::CreateSysTimeIntervalString;

        auto &OutputCharProc = state.dataHeatRecovery->OutputCharProc;
        auto &OutputCharRegen = state.dataHeatRecovery->OutputCharRegen;
        auto &TimeStepSysLast = state.dataHeatRecovery->TimeStepSysLast7;
        auto &CurrentEndTime = state.dataHeatRecovery->CurrentEndTime7;
        auto &CurrentEndTimeLast = state.dataHeatRecovery->CurrentEndTimeLast7;

        // current end time is compared with last to see if time step changed
        Real64 ABSImbalancedFlow; // absolute value of process and regeneration air flow imbalance fraction

        if (state.dataGlobal->WarmupFlag || FirstHVACIteration) return;

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {

            // print error when regeneration inlet relative humidity is outside model boundaries
            if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintImbalancedMassFlowMess) {
                ++state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ImbalancedMassFlowErrorCount;
                if (state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                        .ImbalancedMassFlowErrorCount < 2) {
                    ShowWarningError(state,
                                     state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                         .ImbalancedMassFlowBuffer1);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .ImbalancedMassFlowBuffer2);
                    ShowContinueError(state,
                                      state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex)
                                          .ImbalancedMassFlowBuffer3);
                    //           CALL ShowContinueError(state, '...Using regeneration inlet air relative humidities that are outside the regeneration
                    //           '&
                    //                 //'outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. '&
                    //                 //'Verify correct model coefficients.')
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum) + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                            "\" - unbalanced air flow rate is limited to 2% error continues with the imbalanced fraction statistics reported...",
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ImbalancedFlowErrIndex,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ABSImbalancedFlow,
                        state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ABSImbalancedFlow);
                }
            }

        } // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

        //     save last system time step and last end time of current time step (used to determine if warning is valid)
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;

        // checking if regeneration inlet relative humidity is within model boundaries
        ABSImbalancedFlow = std::abs(RegenInMassFlow - ProcessInMassFlow) / RegenInMassFlow;
        if (ABSImbalancedFlow > 0.02) {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ABSImbalancedFlow =
                ABSImbalancedFlow;
            OutputCharRegen = format("{:.6R}", RegenInMassFlow);
            OutputCharProc = format("{:.6R}", ProcessInMassFlow);
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintImbalancedMassFlowMess = true;

            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ImbalancedMassFlowBuffer1 =
                cHXTypes(state.dataHeatRecovery->ExchCond(ExchNum).ExchTypeNum) + " \"" + state.dataHeatRecovery->ExchCond(ExchNum).Name +
                "\" - unbalanced air flow rate is limited to 2%.";
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ImbalancedMassFlowBuffer2 =
                "...Regeneration air mass flow rate is " + OutputCharRegen + " and process air mass flow rate is " + OutputCharProc + '.';
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).ImbalancedMassFlowBuffer3 =
                "...Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ", " +
                CreateSysTimeIntervalString(state);
        } else {
            state.dataHeatRecovery->BalDesDehumPerfData(state.dataHeatRecovery->ExchCond(ExchNum).PerfDataIndex).PrintImbalancedMassFlowMess = false;
        }
    }

    int GetSupplyInletNode(EnergyPlusData &state,
                           std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                           bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   February 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given HX and returns the supply air inlet node number.
        // If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int GetSupplyInletNode; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetSupplyInletNode = state.dataHeatRecovery->ExchCond(WhichHX).SupInletNode;
        } else {
            ShowSevereError(state, "GetSupplyInletNode: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            GetSupplyInletNode = 0;
        }

        return GetSupplyInletNode;
    }

    int GetSupplyOutletNode(EnergyPlusData &state,
                            std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                            bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   February 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given HX and returns the supply air outlet node number.
        // If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int GetSupplyOutletNode; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetSupplyOutletNode = state.dataHeatRecovery->ExchCond(WhichHX).SupOutletNode;
        } else {
            ShowSevereError(state, "GetSupplyOutletNode: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            GetSupplyOutletNode = 0;
        }

        return GetSupplyOutletNode;
    }

    int GetSecondaryInletNode(EnergyPlusData &state,
                              std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                              bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   February 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given HX and returns the secondary air inlet node number.
        // If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int GetSecondaryInletNode; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetSecondaryInletNode = state.dataHeatRecovery->ExchCond(WhichHX).SecInletNode;
        } else {
            ShowSevereError(state, "GetSecondaryInletNode: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            GetSecondaryInletNode = 0;
        }

        return GetSecondaryInletNode;
    }

    int GetSecondaryOutletNode(EnergyPlusData &state,
                               std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                               bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   February 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given HX assisted cooling coil and returns the secondary air outlet node number.
        // If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int GetSecondaryOutletNode; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetSecondaryOutletNode = state.dataHeatRecovery->ExchCond(WhichHX).SecOutletNode;
        } else {
            ShowSevereError(state, "GetSecondaryOutletNode: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            GetSecondaryOutletNode = 0;
        }

        return GetSecondaryOutletNode;
    }

    Real64 GetSupplyAirFlowRate(EnergyPlusData &state,
                                std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                                bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Generic HX and the voluetric air flow rate.
        // If incorrect HX name is given, ErrorsFound is returned as true and air flow rate as zero.

        // Return value
        Real64 GetSupplyAirFlowRate; // air flow rate returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetSupplyAirFlowRate = state.dataHeatRecovery->ExchCond(WhichHX).NomSupAirVolFlow;
        } else {
            ShowSevereError(state, "GetSupplyAirFlowRate: Could not find heat exchanger = \"" + HXName + "\"");
            ShowContinueError(state, "... Supply Air Flow Rate returned as 0.");
            ErrorsFound = true;
            GetSupplyAirFlowRate = 0.0;
        }

        return GetSupplyAirFlowRate;
    }

    int GetHeatExchangerObjectTypeNum(EnergyPlusData &state,
                                      std::string const &HXName, // must match HX names for the state.dataHeatRecovery->ExchCond type
                                      bool &ErrorsFound          // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Generic HX and the voluetric air flow rate.
        // If incorrect HX name is given, ErrorsFound is returned as true and air flow rate as zero.

        // Return value
        int GetHeatExchangerObjectTypeNum; // object type parameter returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichHX;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        if (WhichHX != 0) {
            GetHeatExchangerObjectTypeNum = state.dataHeatRecovery->ExchCond(WhichHX).ExchTypeNum;
        } else {
            ShowSevereError(state, "GetHeatExchangerObjectTypeNum: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            GetHeatExchangerObjectTypeNum = 0;
        }

        return GetHeatExchangerObjectTypeNum;
    }

    void SetHeatExchangerData(EnergyPlusData &state,
                              int const HXNum,                     // Index of HX
                              bool &ErrorsFound,                   // Set to true if certain errors found
                              std::string const &HXName,           // Name of HX
                              Optional<Real64> SupplyAirVolFlow,   // HX supply air flow rate    [m3/s]
                              Optional<Real64> SecondaryAirVolFlow // HX secondary air flow rate [m3/s]
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WhichHX; // index to generic HX

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataHeatRecovery->GetInputFlag) { // First time subroutine has been entered
            GetHeatRecoveryInput(state);
            state.dataHeatRecovery->GetInputFlag = false;
        }

        if (HXNum == 0) {
            WhichHX = UtilityRoutines::FindItemInList(HXName, state.dataHeatRecovery->ExchCond);
        } else {
            WhichHX = HXNum;
        }

        if (WhichHX <= 0 || WhichHX > state.dataHeatRecovery->NumHeatExchangers) {
            ShowSevereError(state, "SetHeatExchangerData: Could not find heat exchanger = \"" + HXName + "\"");
            ErrorsFound = true;
            return;
        }

        if (present(SupplyAirVolFlow)) {
            state.dataHeatRecovery->ExchCond(WhichHX).NomSupAirVolFlow = SupplyAirVolFlow;
        }

        if (present(SecondaryAirVolFlow)) {
            state.dataHeatRecovery->ExchCond(WhichHX).NomSecAirVolFlow = SecondaryAirVolFlow;
        }
    }

} // namespace HeatRecovery

} // namespace EnergyPlus
