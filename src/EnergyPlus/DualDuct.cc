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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DualDuct {
    // Module containing the DualDuct simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 2000
    //       MODIFIED       Clayton Miller, Brent Griffith Aug. 2010 - Added DualDuctOA Terminal Unit to Simulate Decoupled OA/RA
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the DualDuct Systems Simulation

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallMassFlow;
    using namespace DataSizing;
    using namespace ScheduleManager;

    void SimulateDualDuct(
        EnergyPlusData &state, std::string_view CompName, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum, int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Damper component simulation.
        // It is called from the SimAirLoopComponent
        // at the system time step.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DDNum; // The Damper that you are currently loading input into

        // Obtains and Allocates Damper related parameters from input file
        if (state.dataDualDuct->GetDualDuctInputFlag) { // First time subroutine has been entered
            GetDualDuctInput(state);
            state.dataDualDuct->GetDualDuctInputFlag = false;
        }

        // Find the correct DDNumber with the AirLoop & CompNum from AirLoop Derived Type
        if (CompIndex == 0) {
            DDNum = UtilityRoutines::FindItemInList(CompName, state.dataDualDuct->dd_airterminal, &DualDuctAirTerminal::Name);
            if (DDNum == 0) {
                ShowFatalError(state, "SimulateDualDuct: Damper not found=" + std::string{CompName});
            }
            CompIndex = DDNum;
        } else {
            DDNum = CompIndex;
            if (DDNum > state.dataDualDuct->NumDDAirTerminal || DDNum < 1) {
                ShowFatalError(state,
                               format("SimulateDualDuct: Invalid CompIndex passed={}, Number of Dampers={}, Damper name={}",
                                      CompIndex,
                                      state.dataDualDuct->NumDDAirTerminal,
                                      CompName));
            }
            if (state.dataDualDuct->CheckEquipName(DDNum)) {
                if (CompName != state.dataDualDuct->dd_airterminal(DDNum).Name) {
                    ShowFatalError(state,
                                   format("SimulateDualDuct: Invalid CompIndex passed={}, Damper name={}, stored Damper Name for that index={}",
                                          CompIndex,
                                          CompName,
                                          state.dataDualDuct->dd_airterminal(DDNum).Name));
                }
                state.dataDualDuct->CheckEquipName(DDNum) = false;
            }
        }

        auto &thisDualDuct(state.dataDualDuct->dd_airterminal(DDNum));

        if (CompIndex > 0) {
            state.dataSize->CurTermUnitSizingNum = state.dataDefineEquipment->AirDistUnit(thisDualDuct.ADUNum).TermUnitSizingNum;
            // With the correct DDNum Initialize
            thisDualDuct.InitDualDuct(state, FirstHVACIteration); // Initialize all Damper related parameters

            // Calculate the Correct Damper Model with the current DDNum
            {
                auto const SELECT_CASE_var(thisDualDuct.DamperType);

                if (SELECT_CASE_var == DualDuctDamper::ConstantVolume) { // 'AirTerminal:DualDuct:ConstantVolume'

                    thisDualDuct.SimDualDuctConstVol(state, ZoneNum, ZoneNodeNum);

                } else if (SELECT_CASE_var == DualDuctDamper::VariableVolume) { // 'AirTerminal:DualDuct:VAV'

                    thisDualDuct.SimDualDuctVarVol(state, ZoneNum, ZoneNodeNum);

                } else if (SELECT_CASE_var == DualDuctDamper::OutdoorAir) {

                    thisDualDuct.SimDualDuctVAVOutdoorAir(state, ZoneNum, ZoneNodeNum); // 'AirTerminal:DualDuct:VAV:OutdoorAir'
                }
            }

            // Update the current Damper to the outlet nodes
            thisDualDuct.UpdateDualDuct(state);

            // Report the current Damper
            thisDualDuct.ReportDualDuct();
        } else {
            ShowFatalError(state, "SimulateDualDuct: Damper not found=" + std::string{CompName});
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetDualDuctInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   April 1998
        //       MODIFIED       Julien Marrec of EffiBEM, 2017-12-18
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main routine to call other input routines and Get routines

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataHeatBalance;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetDualDuctInput: "); // include trailing bla

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int DDNum;       // The Damper that you are currently loading input into
        int DamperIndex; // Loop index to Damper that you are currently loading input into
        int NumAlphas;
        int NumNums;
        int IOStat;
        Array1D<Real64> NumArray(2, 0.0);
        Array1D_string AlphArray(7);
        Array1D_string cAlphaFields(7);       // Alpha field names
        Array1D_string cNumericFields(2);     // Numeric field names
        Array1D_bool lAlphaBlanks(7, true);   // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks(2, true); // Logical array, numeric field input BLANK = .TRUE.
        std::string CurrentModuleObject;      // for ease in getting objects
        bool ErrorsFound(false);              // If errors detected in input
        int CtrlZone;                         // controlled zone do loop index
        int SupAirIn;                         // controlled zone supply air inlet index
        int ADUNum;                           // loop control to search Air Distribution Units
        Real64 DummyOAFlow(0.0);

        state.dataDualDuct->NumDualDuctConstVolDampers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataDualDuct->cCMO_DDConstantVolume);
        state.dataDualDuct->NumDualDuctVarVolDampers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataDualDuct->cCMO_DDVariableVolume);
        state.dataDualDuct->NumDualDuctVarVolOA =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataDualDuct->cCMO_DDVarVolOA);
        state.dataDualDuct->NumDDAirTerminal =
            state.dataDualDuct->NumDualDuctConstVolDampers + state.dataDualDuct->NumDualDuctVarVolDampers + state.dataDualDuct->NumDualDuctVarVolOA;
        state.dataDualDuct->dd_airterminal.allocate(state.dataDualDuct->NumDDAirTerminal);
        state.dataDualDuct->UniqueDualDuctAirTerminalNames.reserve(state.dataDualDuct->NumDDAirTerminal);
        state.dataDualDuct->CheckEquipName.dimension(state.dataDualDuct->NumDDAirTerminal, true);

        if (state.dataDualDuct->NumDualDuctConstVolDampers > 0) {
            for (DamperIndex = 1; DamperIndex <= state.dataDualDuct->NumDualDuctConstVolDampers; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = state.dataDualDuct->cCMO_DDConstantVolume;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         DamperIndex,
                                                                         AlphArray,
                                                                         NumAlphas,
                                                                         NumArray,
                                                                         NumNums,
                                                                         IOStat,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex;
                GlobalNames::VerifyUniqueInterObjectName(
                    state, state.dataDualDuct->UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                state.dataDualDuct->dd_airterminal(DDNum).Name = AlphArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).DamperType = DualDuctDamper::ConstantVolume;
                state.dataDualDuct->dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
                    if (state.dataDualDuct->dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + ", \"" + state.dataDualDuct->dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) +
                                            " = " + AlphArray(2) + " not found.");
                        ErrorsFound = true;
                    }
                }
                state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                                            AlphArray(3),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent,
                                                                                            cAlphaFields(3));
                state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                 AlphArray(4),
                                                                                                 ErrorsFound,
                                                                                                 CurrentModuleObject,
                                                                                                 AlphArray(1),
                                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                                 ObjectIsNotParent,
                                                                                                 cAlphaFields(4));
                state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                  AlphArray(5),
                                                                                                  ErrorsFound,
                                                                                                  CurrentModuleObject,
                                                                                                  AlphArray(1),
                                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                                                                  NodeInputManager::compFluidStream::Primary,
                                                                                                  ObjectIsNotParent,
                                                                                                  cAlphaFields(5));

                state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).ZoneMinAirFracDes = 0.0;

                // Register component set data - one for heat and one for cool
                TestCompSet(
                    state, CurrentModuleObject + ":HEAT", state.dataDualDuct->dd_airterminal(DDNum).Name, AlphArray(4), AlphArray(3), "Air Nodes");
                TestCompSet(
                    state, CurrentModuleObject + ":COOL", state.dataDualDuct->dd_airterminal(DDNum).Name, AlphArray(5), AlphArray(3), "Air Nodes");

                for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                    if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum;
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum2 = state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum;
                        state.dataDualDuct->dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (state.dataDualDuct->dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(state,
                                    std::string{RoutineName} + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" +
                                        CurrentModuleObject + ',' + state.dataDualDuct->dd_airterminal(DDNum).Name + "].");
                    ShowContinueError(
                        state, "...should have outlet node=" + state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum ==
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                    ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                                    ShowContinueError(state,
                                                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum) +
                                                          " already connects to another zone");
                                    ShowContinueError(state,
                                                      "Occurs for terminal unit " + CurrentModuleObject + " = " +
                                                          state.dataDualDuct->dd_airterminal(DDNum).Name);
                                    ShowContinueError(state, "Check terminal unit node names for errors");
                                    ErrorsFound = true;
                                } else {
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum;
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum;
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                    state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                    state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;
                                }
                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                state.dataDualDuct->dd_airterminal(DDNum).ActualZoneNum =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                // Setup the Average damper Position output variable
                // CurrentModuleObject='AirTerminal:DualDuct:ConstantVolume'
                SetupOutputVariable(state,
                                    "Zone Air Terminal Cold Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).ColdAirDamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Hot Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).HotAirDamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);

            } // end Number of Damper Loop
        }

        if (state.dataDualDuct->NumDualDuctVarVolDampers > 0) {
            for (DamperIndex = 1; DamperIndex <= state.dataDualDuct->NumDualDuctVarVolDampers; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = state.dataDualDuct->cCMO_DDVariableVolume;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         DamperIndex,
                                                                         AlphArray,
                                                                         NumAlphas,
                                                                         NumArray,
                                                                         NumNums,
                                                                         IOStat,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex + state.dataDualDuct->NumDualDuctConstVolDampers;
                GlobalNames::VerifyUniqueInterObjectName(
                    state, state.dataDualDuct->UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                state.dataDualDuct->dd_airterminal(DDNum).Name = AlphArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).DamperType = DualDuctDamper::VariableVolume;
                state.dataDualDuct->dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
                    if (state.dataDualDuct->dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + ", \"" + state.dataDualDuct->dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) +
                                            " = " + AlphArray(2) + " not found.");
                        ErrorsFound = true;
                    }
                }
                state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                                            AlphArray(3),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent,
                                                                                            cAlphaFields(3));
                state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                 AlphArray(4),
                                                                                                 ErrorsFound,
                                                                                                 CurrentModuleObject,
                                                                                                 AlphArray(1),
                                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                                 ObjectIsNotParent,
                                                                                                 cAlphaFields(4));
                state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                  AlphArray(5),
                                                                                                  ErrorsFound,
                                                                                                  CurrentModuleObject,
                                                                                                  AlphArray(1),
                                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                                                                  NodeInputManager::compFluidStream::Primary,
                                                                                                  ObjectIsNotParent,
                                                                                                  cAlphaFields(5));

                state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).ZoneMinAirFracDes = NumArray(2);

                // Register component set data - one for heat and one for cool
                TestCompSet(
                    state, CurrentModuleObject + ":HEAT", state.dataDualDuct->dd_airterminal(DDNum).Name, AlphArray(4), AlphArray(3), "Air Nodes");
                TestCompSet(
                    state, CurrentModuleObject + ":COOL", state.dataDualDuct->dd_airterminal(DDNum).Name, AlphArray(5), AlphArray(3), "Air Nodes");

                for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                    if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum;
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum2 = state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum;
                        state.dataDualDuct->dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (state.dataDualDuct->dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(state,
                                    std::string{RoutineName} + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" +
                                        CurrentModuleObject + ',' + state.dataDualDuct->dd_airterminal(DDNum).Name + "].");
                    ShowContinueError(
                        state, "...should have outlet node=" + state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum ==
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).ColdAirInletNodeNum;
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).HotAirInletNodeNum;
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;

                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                state.dataDualDuct->dd_airterminal(DDNum).ActualZoneNum =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                if (!lAlphaBlanks(6)) {
                    state.dataDualDuct->dd_airterminal(DDNum).OARequirementsPtr =
                        UtilityRoutines::FindItemInList(AlphArray(6), state.dataSize->OARequirements);
                    if (state.dataDualDuct->dd_airterminal(DDNum).OARequirementsPtr == 0) {
                        ShowSevereError(state, cAlphaFields(6) + " = " + AlphArray(6) + " not found.");
                        ShowContinueError(
                            state, "Occurs in " + state.dataDualDuct->cCMO_DDVariableVolume + " = " + state.dataDualDuct->dd_airterminal(DDNum).Name);
                        ErrorsFound = true;
                    } else {
                        state.dataDualDuct->dd_airterminal(DDNum).NoOAFlowInputFromUser = false;
                    }
                }

                if (lAlphaBlanks(7)) {
                    state.dataDualDuct->dd_airterminal(DDNum).ZoneTurndownMinAirFrac = 1.0;
                    state.dataDualDuct->dd_airterminal(DDNum).ZoneTurndownMinAirFracSchExist = false;
                } else {
                    state.dataDualDuct->dd_airterminal(DDNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, AlphArray(7));
                    if (state.dataDualDuct->dd_airterminal(DDNum).ZoneTurndownMinAirFracSchPtr == 0) {
                        ShowSevereError(state, cAlphaFields(7) + " = " + AlphArray(7) + " not found.");
                        ShowContinueError(
                            state, "Occurs in " + state.dataDualDuct->cCMO_DDVariableVolume + " = " + state.dataDualDuct->dd_airterminal(DDNum).Name);
                        ErrorsFound = true;
                    }
                    state.dataDualDuct->dd_airterminal(DDNum).ZoneTurndownMinAirFracSchExist = true;
                }

                // Setup the Average damper Position output variable
                // CurrentModuleObject='AirTerminal:DualDuct:VAV'
                SetupOutputVariable(state,
                                    "Zone Air Terminal Cold Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).ColdAirDamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Hot Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).HotAirDamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Outdoor Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataDualDuct->dd_airterminal(DDNum).OutdoorAirFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
            } // end Number of Damper Loop
        }

        if (state.dataDualDuct->NumDualDuctVarVolOA > 0) {
            for (DamperIndex = 1; DamperIndex <= state.dataDualDuct->NumDualDuctVarVolOA; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = state.dataDualDuct->cCMO_DDVarVolOA;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         DamperIndex,
                                                                         AlphArray,
                                                                         NumAlphas,
                                                                         NumArray,
                                                                         NumNums,
                                                                         IOStat,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex + state.dataDualDuct->NumDualDuctConstVolDampers + state.dataDualDuct->NumDualDuctVarVolDampers;
                GlobalNames::VerifyUniqueInterObjectName(
                    state, state.dataDualDuct->UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                state.dataDualDuct->dd_airterminal(DDNum).Name = AlphArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).DamperType = DualDuctDamper::OutdoorAir;
                state.dataDualDuct->dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataDualDuct->dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
                    if (state.dataDualDuct->dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + ", \"" + state.dataDualDuct->dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) +
                                            " = " + AlphArray(2) + " not found.");
                        ErrorsFound = true;
                    }
                }
                state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                                            AlphArray(3),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent,
                                                                                            cAlphaFields(3));
                state.dataDualDuct->dd_airterminal(DDNum).OAInletNodeNum = GetOnlySingleNode(state,
                                                                                             AlphArray(4),
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             AlphArray(1),
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                                             NodeInputManager::compFluidStream::Primary,
                                                                                             ObjectIsNotParent,
                                                                                             cAlphaFields(4));

                if (!lAlphaBlanks(5)) {
                    state.dataDualDuct->dd_airterminal(DDNum).RecircAirInletNodeNum = GetOnlySingleNode(state,
                                                                                                        AlphArray(5),
                                                                                                        ErrorsFound,
                                                                                                        CurrentModuleObject,
                                                                                                        AlphArray(1),
                                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                                        NodeInputManager::compFluidStream::Primary,
                                                                                                        ObjectIsNotParent,
                                                                                                        cAlphaFields(5));
                } else {
                    // for this model, we intentionally allow not using the recirc side
                    state.dataDualDuct->dd_airterminal(DDNum).RecircIsUsed = false;
                }

                state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                state.dataDualDuct->dd_airterminal(DDNum).MaxAirMassFlowRate =
                    state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;

                // Register component set data - one for OA and one for RA
                TestCompSet(state,
                            CurrentModuleObject + ":OutdoorAir",
                            state.dataDualDuct->dd_airterminal(DDNum).Name,
                            AlphArray(4),
                            AlphArray(3),
                            "Air Nodes");
                if (state.dataDualDuct->dd_airterminal(DDNum).RecircIsUsed) {
                    TestCompSet(state,
                                CurrentModuleObject + ":RecirculatedAir",
                                state.dataDualDuct->dd_airterminal(DDNum).Name,
                                AlphArray(5),
                                AlphArray(3),
                                "Air Nodes");
                }

                {
                    auto const SELECT_CASE_var(AlphArray(7));
                    if (SELECT_CASE_var == "CURRENTOCCUPANCY") {
                        state.dataDualDuct->dd_airterminal(DDNum).OAPerPersonMode = PerPersonMode::DCVByCurrentLevel;

                    } else if (SELECT_CASE_var == "DESIGNOCCUPANCY") {
                        state.dataDualDuct->dd_airterminal(DDNum).OAPerPersonMode = PerPersonMode::ByDesignLevel;
                    }
                }
                // checks on this are done later

                for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                    if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataDualDuct->dd_airterminal(DDNum).OAInletNodeNum;
                        state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum2 =
                            state.dataDualDuct->dd_airterminal(DDNum).RecircAirInletNodeNum;
                        state.dataDualDuct->dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (state.dataDualDuct->dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if (state.dataDualDuct->dd_airterminal(DDNum).DamperType == DualDuctDamper::OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(state,
                                    std::string{RoutineName} + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" +
                                        CurrentModuleObject + ',' + state.dataDualDuct->dd_airterminal(DDNum).Name + "].");
                    ShowContinueError(
                        state, "...should have outlet node=" + state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum ==
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                if (state.dataDualDuct->dd_airterminal(DDNum).RecircIsUsed) {
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).RecircAirInletNodeNum;
                                } else {
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                        state.dataDualDuct->dd_airterminal(DDNum).OAInletNodeNum;
                                }
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).OAInletNodeNum;
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode =
                                    state.dataDualDuct->dd_airterminal(DDNum).OutletNodeNum;
                                state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                state.dataDefineEquipment->AirDistUnit(state.dataDualDuct->dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;

                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                state.dataDualDuct->dd_airterminal(DDNum).ActualZoneNum =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                state.dataDualDuct->dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                state.dataDualDuct->dd_airterminal(DDNum).OARequirementsPtr =
                    UtilityRoutines::FindItemInList(AlphArray(6), state.dataSize->OARequirements);
                if (state.dataDualDuct->dd_airterminal(DDNum).OARequirementsPtr == 0) {
                    ShowSevereError(state, cAlphaFields(6) + " = " + AlphArray(6) + " not found.");
                    ShowContinueError(state,
                                      "Occurs in " + state.dataDualDuct->cCMO_DDVarVolOA + " = " + state.dataDualDuct->dd_airterminal(DDNum).Name);
                    ErrorsFound = true;
                } else {
                    state.dataDualDuct->dd_airterminal(DDNum).NoOAFlowInputFromUser = false;

                    // now fill design OA rate
                    state.dataDualDuct->dd_airterminal(DDNum).CalcOAOnlyMassFlow(
                        state, DummyOAFlow, state.dataDualDuct->dd_airterminal(DDNum).DesignOAFlowRate);

                    if (state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate != AutoSize) {
                        BaseSizer::reportSizerOutput(state,
                                                     CurrentModuleObject,
                                                     state.dataDualDuct->dd_airterminal(DDNum).Name,
                                                     "Maximum Outdoor Air Flow Rate [m3/s]",
                                                     state.dataDualDuct->dd_airterminal(DDNum).DesignOAFlowRate);

                        if (state.dataDualDuct->dd_airterminal(DDNum).RecircIsUsed) {
                            state.dataDualDuct->dd_airterminal(DDNum).DesignRecircFlowRate =
                                state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate -
                                state.dataDualDuct->dd_airterminal(DDNum).DesignOAFlowRate;
                            state.dataDualDuct->dd_airterminal(DDNum).DesignRecircFlowRate =
                                max(0.0, state.dataDualDuct->dd_airterminal(DDNum).DesignRecircFlowRate);
                            BaseSizer::reportSizerOutput(state,
                                                         CurrentModuleObject,
                                                         state.dataDualDuct->dd_airterminal(DDNum).Name,
                                                         "Maximum Recirculated Air Flow Rate [m3/s]",
                                                         state.dataDualDuct->dd_airterminal(DDNum).DesignRecircFlowRate);
                        } else {
                            if (state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate <
                                state.dataDualDuct->dd_airterminal(DDNum).DesignOAFlowRate) {
                                ShowSevereError(state,
                                                format("The value {:.5R} in {}is lower than the outdoor air requirement.",
                                                       state.dataDualDuct->dd_airterminal(DDNum).MaxAirVolFlowRate,
                                                       cNumericFields(1)));
                                ShowContinueError(state,
                                                  "Occurs in " + state.dataDualDuct->cCMO_DDVarVolOA + " = " +
                                                      state.dataDualDuct->dd_airterminal(DDNum).Name);
                                ShowContinueError(state,
                                                  format("The design outdoor air requirement is {:.5R}",
                                                         state.dataDualDuct->dd_airterminal(DDNum).DesignOAFlowRate));
                                ErrorsFound = true;
                            }
                        }
                    }
                }

                if (state.dataDualDuct->dd_airterminal(DDNum).OAPerPersonMode == PerPersonMode::ModeNotSet) {
                    DummyOAFlow = state.dataSize->OARequirements(state.dataDualDuct->dd_airterminal(DDNum).OARequirementsPtr).OAFlowPerPerson;
                    if ((DummyOAFlow == 0.0) && (lAlphaBlanks(7))) {       // no worries
                                                                           // do nothing, okay since no per person requirement involved
                    } else if ((DummyOAFlow > 0.0) && (lAlphaBlanks(7))) { // missing input
                        ShowSevereError(state, cAlphaFields(7) + " was blank.");
                        ShowContinueError(
                            state, "Occurs in " + state.dataDualDuct->cCMO_DDVarVolOA + " = " + state.dataDualDuct->dd_airterminal(DDNum).Name);
                        ShowContinueError(state, "Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"");
                        ErrorsFound = true;
                    } else if ((DummyOAFlow > 0.0) && !(lAlphaBlanks(7))) { // incorrect input
                        ShowSevereError(state, cAlphaFields(7) + " = " + AlphArray(7) + " not a valid key choice.");
                        ShowContinueError(
                            state, "Occurs in " + state.dataDualDuct->cCMO_DDVarVolOA + " = " + state.dataDualDuct->dd_airterminal(DDNum).Name);
                        ShowContinueError(state, "Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"");
                        ErrorsFound = true;
                    }
                }

                // Setup the Average damper Position output variable
                SetupOutputVariable(state,
                                    "Zone Air Terminal Outdoor Air Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).OADamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Recirculated Air Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).RecircAirDamperPosition,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Outdoor Air Fraction",
                                    OutputProcessor::Unit::None,
                                    state.dataDualDuct->dd_airterminal(DDNum).OAFraction,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataDualDuct->dd_airterminal(DDNum).Name);

            } // end Number of Damper Loop
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    // End of Get Input subroutines for the Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void DualDuctAirTerminal::InitDualDuct(EnergyPlusData &state, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   February 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for  initializations of the Damper Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HotInNode;
        int ColdInNode;
        int OAInNode; // Outdoor Air Inlet Node for VAV:OutdoorAir units
        int RAInNode; // Reciruclated Air Inlet Node for VAV:OutdoorAir units
        int OutNode;
        // static Array1D_bool MyEnvrnFlag;
        // static Array1D_bool MySizeFlag;
        // static Array1D_bool MyAirLoopFlag;
        int Loop;          // Loop checking control variable
        Real64 PeopleFlow; // local sum variable, m3/s

        // Do the Begin Simulation initializations
        if (state.dataDualDuct->InitDualDuctMyOneTimeFlag) {

            // MyEnvrnFlag.allocate(NumDDAirTerminal);
            // MySizeFlag.allocate(NumDDAirTerminal);
            // MyAirLoopFlag.dimension(NumDDAirTerminal, true);
            // MyEnvrnFlag = true;
            // MySizeFlag = true;
            state.dataDualDuct->MassFlowSetToler = DataConvergParams::HVACFlowRateToler * 0.00001;

            state.dataDualDuct->InitDualDuctMyOneTimeFlag = false;
        }

        if (!state.dataDualDuct->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataDualDuct->ZoneEquipmentListChecked = true;
            // Check to see if there is a Air Distribution Unit on the Zone Equipment List
            for (Loop = 1; Loop <= state.dataDualDuct->NumDDAirTerminal; ++Loop) {
                if (this->ADUNum == 0) continue;
                if (CheckZoneEquipmentList(state, "ZONEHVAC:AIRDISTRIBUTIONUNIT", state.dataDefineEquipment->AirDistUnit(this->ADUNum).Name))
                    continue;
                ShowSevereError(state,
                                "InitDualDuct: ADU=[Air Distribution Unit," + state.dataDefineEquipment->AirDistUnit(this->ADUNum).Name +
                                    "] is not on any ZoneHVAC:EquipmentList.");
                if (this->DamperType == DualDuctDamper::ConstantVolume) {
                    ShowContinueError(
                        state, "...Dual Duct Damper=[" + state.dataDualDuct->cCMO_DDConstantVolume + ',' + this->Name + "] will not be simulated.");
                } else if (this->DamperType == DualDuctDamper::VariableVolume) {
                    ShowContinueError(
                        state, "...Dual Duct Damper=[" + state.dataDualDuct->cCMO_DDVariableVolume + ',' + this->Name + "] will not be simulated.");
                } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
                    ShowContinueError(state,
                                      "...Dual Duct Damper=[" + state.dataDualDuct->cCMO_DDVarVolOA + ',' + this->Name + "] will not be simulated.");
                } else {
                    ShowContinueError(state, "...Dual Duct Damper=[unknown/invalid," + this->Name + "] will not be simulated.");
                }
            }
        }

        if (!state.dataGlobal->SysSizingCalc && this->MySizeFlag) {

            this->SizeDualDuct(state);

            this->MySizeFlag = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {

            if (this->DamperType == DualDuctDamper::ConstantVolume || this->DamperType == DualDuctDamper::VariableVolume) {
                OutNode = this->OutletNodeNum;
                HotInNode = this->HotAirInletNodeNum;
                ColdInNode = this->ColdAirInletNodeNum;
                state.dataLoopNodes->Node(OutNode).MassFlowRateMax = this->MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;
                if (this->DamperType == DualDuctDamper::ConstantVolume) {
                    state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
                } else if (this->DamperType == DualDuctDamper::VariableVolume) {
                    // get dual duct air terminal box minimum flow fraction value
                    if (this->ZoneTurndownMinAirFracSchExist) {
                        this->ZoneTurndownMinAirFrac = ScheduleManager::GetScheduleMinValue(state, this->ZoneTurndownMinAirFracSchPtr);
                    } else {
                        this->ZoneTurndownMinAirFrac = 1.0;
                    }
                    state.dataLoopNodes->Node(OutNode).MassFlowRateMin =
                        state.dataLoopNodes->Node(OutNode).MassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                } else {
                    state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
                }
                this->dd_airterminalHotAirInlet.AirMassFlowRateMax = state.dataLoopNodes->Node(OutNode).MassFlowRateMax;
                this->dd_airterminalColdAirInlet.AirMassFlowRateMax = state.dataLoopNodes->Node(OutNode).MassFlowRateMax;
                state.dataLoopNodes->Node(HotInNode).MassFlowRateMax = state.dataLoopNodes->Node(OutNode).MassFlowRateMax;
                state.dataLoopNodes->Node(ColdInNode).MassFlowRateMax = state.dataLoopNodes->Node(OutNode).MassFlowRateMax;
                state.dataLoopNodes->Node(HotInNode).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(ColdInNode).MassFlowRateMin = 0.0;
                this->MyEnvrnFlag = false;

            } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
                // Initialize for DualDuct:VAV:OutdoorAir
                OutNode = this->OutletNodeNum;
                OAInNode = this->OAInletNodeNum;
                if (this->RecircIsUsed) RAInNode = this->RecircAirInletNodeNum;
                state.dataLoopNodes->Node(OutNode).MassFlowRateMax = this->MaxAirMassFlowRate;
                state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
                this->dd_airterminalOAInlet.AirMassFlowRateMax = this->DesignOAFlowRate * state.dataEnvrn->StdRhoAir;
                if (this->RecircIsUsed) {
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateMax = this->MaxAirMassFlowRate - this->dd_airterminalOAInlet.AirMassFlowRateMax;
                    state.dataLoopNodes->Node(RAInNode).MassFlowRateMax = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    state.dataLoopNodes->Node(RAInNode).MassFlowRateMin = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag = 1.0e-10 * this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                }
                state.dataLoopNodes->Node(OAInNode).MassFlowRateMax = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                state.dataLoopNodes->Node(OAInNode).MassFlowRateMin = 0.0;
                // figure per person by design level for the OA duct.
                PeopleFlow = 0.0;
                for (Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
                    if (state.dataHeatBal->People(Loop).ZonePtr != this->ActualZoneNum) continue;
                    int damperOAFlowMethod = state.dataSize->OARequirements(this->OARequirementsPtr).OAFlowMethod;
                    if (damperOAFlowMethod == OAFlowPPer || damperOAFlowMethod == OAFlowSum || damperOAFlowMethod == OAFlowMax) {
                        PeopleFlow +=
                            state.dataHeatBal->People(Loop).NumberOfPeople * state.dataSize->OARequirements(this->OARequirementsPtr).OAFlowPerPerson;
                    }
                }
                this->OAPerPersonByDesignLevel = PeopleFlow;

                this->MyEnvrnFlag = false;
            }
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // Find air loop associated with this terminal unit
        if (this->MyAirLoopFlag) {
            if (this->AirLoopNum == 0) {
                if ((this->CtrlZoneNum > 0) && (this->CtrlZoneInNodeIndex > 0)) {
                    this->AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(this->CtrlZoneNum).InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
                    state.dataDefineEquipment->AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
                    // Don't set MyAirLoopFlag to false yet because airloopnums might not be populated yet
                }
            } else {
                this->MyAirLoopFlag = false;
            }
        }

        // Initialize the Inlet Nodes of the Sys
        if (this->DamperType == DualDuctDamper::ConstantVolume || this->DamperType == DualDuctDamper::VariableVolume) {
            HotInNode = this->HotAirInletNodeNum;
            ColdInNode = this->ColdAirInletNodeNum;
            OutNode = this->OutletNodeNum;
        } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
            OAInNode = this->OAInletNodeNum;
            if (this->RecircIsUsed) RAInNode = this->RecircAirInletNodeNum;
            OutNode = this->OutletNodeNum;
        }

        if (FirstHVACIteration) {
            //     CALL DisplayString('Init First HVAC Iteration {'//TRIM(  dd_airterminal(DDNum)%DamperName)//'}') !-For debugging - REMOVE
            // The first time through set the mass flow rate to the Max
            // Take care of the flow rates first. For Const Vol and VAV.
            if (this->DamperType == DualDuctDamper::ConstantVolume || this->DamperType == DualDuctDamper::VariableVolume) {
                if ((state.dataLoopNodes->Node(HotInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRate = 0.0;
                }
                if ((state.dataLoopNodes->Node(ColdInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRate = 0.0;
                }
                // Next take care of the Max Avail Flow Rates
                if ((state.dataLoopNodes->Node(HotInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRateMaxAvail = this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRateMaxAvail = 0.0;
                }
                if ((state.dataLoopNodes->Node(ColdInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRateMaxAvail = this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRateMaxAvail = 0.0;
                }
                // get current time step air terminal box turndown minimum flow fraction
                if (this->ZoneTurndownMinAirFracSchExist) {
                    this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(state, this->ZoneTurndownMinAirFracSchPtr);
                } else {
                    this->ZoneTurndownMinAirFrac = 1.0;
                }
                // update to the current dual duct minimum air flow fraction
                this->ZoneMinAirFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                // The last item is to take care of the Min Avail Flow Rates
                if ((state.dataLoopNodes->Node(HotInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRateMinAvail =
                        this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                } else {
                    state.dataLoopNodes->Node(HotInNode).MassFlowRateMinAvail = 0.0;
                }
                if ((state.dataLoopNodes->Node(ColdInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRateMinAvail =
                        this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                } else {
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRateMinAvail = 0.0;
                }

            } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
                // The first time through set the mass flow rate to the Max for VAV:OutdoorAir
                if ((state.dataLoopNodes->Node(OAInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(OAInNode).MassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(OAInNode).MassFlowRate = 0.0;
                }
                if (this->RecircIsUsed) {
                    if ((state.dataLoopNodes->Node(RAInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                        state.dataLoopNodes->Node(RAInNode).MassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    } else {
                        state.dataLoopNodes->Node(RAInNode).MassFlowRate = 0.0;
                    }
                    // clear flow history
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1 = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2 = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3 = 0.0;
                }
                // Next take care of the Max Avail Flow Rates
                if ((state.dataLoopNodes->Node(OAInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    state.dataLoopNodes->Node(OAInNode).MassFlowRateMaxAvail = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                } else {
                    state.dataLoopNodes->Node(OAInNode).MassFlowRateMaxAvail = 0.0;
                }
                if (this->RecircIsUsed) {
                    if ((state.dataLoopNodes->Node(RAInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                        state.dataLoopNodes->Node(RAInNode).MassFlowRateMaxAvail = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    } else {
                        state.dataLoopNodes->Node(RAInNode).MassFlowRateMaxAvail = 0.0;
                    }
                }
                // The last item is to take care of the Min Avail Flow Rates. VAV:OutdoorAir
                state.dataLoopNodes->Node(OAInNode).MassFlowRateMinAvail = 0.0;
                if (this->RecircIsUsed) state.dataLoopNodes->Node(RAInNode).MassFlowRateMinAvail = 0.0;
            }
        }

        // Initialize the Inlet Nodes of the Dampers for Const. Vol and VAV
        if (this->DamperType == DualDuctDamper::ConstantVolume || this->DamperType == DualDuctDamper::VariableVolume) {

            this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail =
                min(state.dataLoopNodes->Node(OutNode).MassFlowRateMax, state.dataLoopNodes->Node(HotInNode).MassFlowRateMaxAvail);
            this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail =
                min(max(state.dataLoopNodes->Node(OutNode).MassFlowRateMin, state.dataLoopNodes->Node(HotInNode).MassFlowRateMinAvail),
                    state.dataLoopNodes->Node(HotInNode).MassFlowRateMaxAvail);

            this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail =
                min(state.dataLoopNodes->Node(OutNode).MassFlowRateMax, state.dataLoopNodes->Node(ColdInNode).MassFlowRateMaxAvail);
            this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail =
                min(max(state.dataLoopNodes->Node(OutNode).MassFlowRateMin, state.dataLoopNodes->Node(ColdInNode).MassFlowRateMinAvail),
                    state.dataLoopNodes->Node(ColdInNode).MassFlowRateMaxAvail);

            // Do the following initializations (every time step): This should be the info from
            // the previous components outlets or the node data in this section.
            // Load the node data in this section for the component simulation
            this->dd_airterminalHotAirInlet.AirMassFlowRate = state.dataLoopNodes->Node(HotInNode).MassFlowRate;
            this->dd_airterminalHotAirInlet.AirTemp = state.dataLoopNodes->Node(HotInNode).Temp;
            this->dd_airterminalHotAirInlet.AirHumRat = state.dataLoopNodes->Node(HotInNode).HumRat;
            this->dd_airterminalHotAirInlet.AirEnthalpy = state.dataLoopNodes->Node(HotInNode).Enthalpy;
            this->dd_airterminalColdAirInlet.AirMassFlowRate = state.dataLoopNodes->Node(ColdInNode).MassFlowRate;
            this->dd_airterminalColdAirInlet.AirTemp = state.dataLoopNodes->Node(ColdInNode).Temp;
            this->dd_airterminalColdAirInlet.AirHumRat = state.dataLoopNodes->Node(ColdInNode).HumRat;
            this->dd_airterminalColdAirInlet.AirEnthalpy = state.dataLoopNodes->Node(ColdInNode).Enthalpy;

            // Initialize the Inlet Nodes of the Dampers for VAV:OutdoorAir
        } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
            this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail = state.dataLoopNodes->Node(OAInNode).MassFlowRateMaxAvail;
            this->dd_airterminalOAInlet.AirMassFlowRateMinAvail = state.dataLoopNodes->Node(OAInNode).MassFlowRateMinAvail;

            // Do the following initializations (every time step): This should be the info from
            // the previous components outlets or the node data in this section.
            // Load the node data in this section for the component simulation
            this->dd_airterminalOAInlet.AirMassFlowRate = state.dataLoopNodes->Node(OAInNode).MassFlowRate;
            this->dd_airterminalOAInlet.AirTemp = state.dataLoopNodes->Node(OAInNode).Temp;
            this->dd_airterminalOAInlet.AirHumRat = state.dataLoopNodes->Node(OAInNode).HumRat;
            this->dd_airterminalOAInlet.AirEnthalpy = state.dataLoopNodes->Node(OAInNode).Enthalpy;
            if (this->RecircIsUsed) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail = state.dataLoopNodes->Node(RAInNode).MassFlowRateMaxAvail;
                this->dd_airterminalRecircAirInlet.AirMassFlowRateMinAvail = state.dataLoopNodes->Node(RAInNode).MassFlowRateMinAvail;
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = state.dataLoopNodes->Node(RAInNode).MassFlowRate;
                this->dd_airterminalRecircAirInlet.AirTemp = state.dataLoopNodes->Node(RAInNode).Temp;
                this->dd_airterminalRecircAirInlet.AirHumRat = state.dataLoopNodes->Node(RAInNode).HumRat;
                this->dd_airterminalRecircAirInlet.AirEnthalpy = state.dataLoopNodes->Node(RAInNode).Enthalpy;
            }
        }
    }

    void DualDuctAirTerminal::SizeDualDuct(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   January 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Dual Duct air terminal units for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string DamperType;

        if (this->MaxAirVolFlowRate == AutoSize) {

            if ((state.dataSize->CurZoneEqNum > 0) && (state.dataSize->CurTermUnitSizingNum > 0)) {
                if (this->DamperType == DualDuctDamper::ConstantVolume) {
                    DamperType = state.dataDualDuct->cCMO_DDConstantVolume;
                } else if (this->DamperType == DualDuctDamper::VariableVolume) {
                    DamperType = state.dataDualDuct->cCMO_DDVariableVolume;
                } else if (this->DamperType == DualDuctDamper::OutdoorAir) {
                    DamperType = state.dataDualDuct->cCMO_DDVarVolOA;
                } else {
                    DamperType = "Invalid/Unknown";
                }
                CheckZoneSizing(state, DamperType, this->Name);
                this->MaxAirVolFlowRate = max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesCoolVolFlow,
                                              state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatVolFlow);
                if (this->DamperType == DualDuctDamper::OutdoorAir) {
                    if (this->RecircIsUsed) {
                        this->DesignRecircFlowRate =
                            max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesCoolVolFlow,
                                state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatVolFlow);
                        this->MaxAirVolFlowRate = this->DesignRecircFlowRate + this->DesignOAFlowRate;
                    } else {
                        this->MaxAirVolFlowRate = this->DesignOAFlowRate;
                        this->DesignRecircFlowRate = 0.0;
                    }
                    this->MaxAirMassFlowRate = this->MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;
                }

                if (this->MaxAirVolFlowRate < SmallAirVolFlow) {
                    this->MaxAirVolFlowRate = 0.0;
                    this->MaxAirMassFlowRate = 0.0;
                    this->DesignOAFlowRate = 0.0;
                    this->DesignRecircFlowRate = 0.0;
                }
                BaseSizer::reportSizerOutput(state, DamperType, this->Name, "Maximum Air Flow Rate [m3/s]", this->MaxAirVolFlowRate);
                if (this->DamperType == DualDuctDamper::OutdoorAir) {
                    BaseSizer::reportSizerOutput(state, DamperType, this->Name, "Maximum Outdoor Air Flow Rate [m3/s]", this->DesignOAFlowRate);
                    if (this->RecircIsUsed) {
                        BaseSizer::reportSizerOutput(
                            state, DamperType, this->Name, "Maximum Recirculated Air Flow Rate [m3/s]", this->DesignRecircFlowRate);
                    }
                }
            }
        }
    }

    // End Initialization Section of the Module
    //******************************************************************************

    // Begin Algorithm Section of the Module
    //******************************************************************************

    void DualDuctAirTerminal::SimDualDuctConstVol(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Jan 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple mixing damper.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused0909   USE DataHeatBalFanSys, ONLY: Mat
        using DataHVACGlobals::SmallTempDiff;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;    // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 HumRat;      // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;    // [Watts]
        Real64 Temperature; // [C]
        Real64 QTotLoad;    // [W]
        Real64 QZnReq;      // [W]
        Real64 CpAirZn;
        Real64 CpAirSysHot;
        Real64 CpAirSysCold;

        // Get the calculated load from the Heat Balance from ZoneSysEnergyDemand
        QTotLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        // Need the design MassFlowRate for calculations
        if (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0) {
            MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail / 2.0 + this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail / 2.0;
        } else {
            MassFlow = 0.0;
        }
        // If there is massflow then need to provide the correct amount of total
        //  required zone energy
        if (MassFlow > SmallMassFlow) {
            CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
            QZnReq = QTotLoad + MassFlow * CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp;
            // If the enthalpy is the same for the hot and cold duct then there would be a
            //  divide by zero so for heating or cooling set the damper to one max flow
            //  or the other.
            if (std::abs(this->dd_airterminalColdAirInlet.AirTemp - this->dd_airterminalHotAirInlet.AirTemp) > SmallTempDiff) {
                // CpAirSysHot = PsyCpAirFnWTdb(dd_airterminalHotAirInlet(DDNum)%AirHumRat,dd_airterminalHotAirInlet(DDNum)%AirTemp)
                // CpAirSysCold= PsyCpAirFnWTdb(dd_airterminalColdAirInlet(DDNum)%AirHumRat,dd_airterminalColdAirInlet(DDNum)%AirTemp)
                CpAirSysHot = CpAirZn;
                CpAirSysCold = CpAirZn;
                // Determine the Cold Air Mass Flow Rate
                this->dd_airterminalColdAirInlet.AirMassFlowRate =
                    (QZnReq - MassFlow * CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) /
                    (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp);
            } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRate > 0.0)) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }
            // Check to make sure that the calculated flow is not greater than the available flows
            if (this->dd_airterminalColdAirInlet.AirMassFlowRate > this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail;
            }
            // Using Mass Continuity to determine the other duct flow quantity
            this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow - this->dd_airterminalColdAirInlet.AirMassFlowRate;
            if (this->dd_airterminalHotAirInlet.AirMassFlowRate > this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            } else if (this->dd_airterminalHotAirInlet.AirMassFlowRate < this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail;
            }
            MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRate + this->dd_airterminalHotAirInlet.AirMassFlowRate;
        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }
        if (MassFlow > SmallMassFlow) {
            // After flows are calculated then calculate the mixed air flow properties.
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                      this->dd_airterminalColdAirInlet.AirHumRat * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                     MassFlow;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                        this->dd_airterminalColdAirInlet.AirEnthalpy * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                       MassFlow;

            // If there is no air flow than calculate the No Flow conditions
        } else {
            this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat + this->dd_airterminalColdAirInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy + this->dd_airterminalColdAirInlet.AirEnthalpy) / 2.0;
        }
        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        // Load all properties in the damper outlet
        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMinAvail =
            min(this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail, this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail);
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the hot and cold damper position in %
        if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMax == 0.0) || (this->dd_airterminalColdAirInlet.AirMassFlowRateMax == 0.0)) {
            this->ColdAirDamperPosition = 0.0;
            this->HotAirDamperPosition = 0.0;
        } else {
            this->ColdAirDamperPosition = this->dd_airterminalColdAirInlet.AirMassFlowRate / this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
            this->HotAirDamperPosition = this->dd_airterminalHotAirInlet.AirMassFlowRate / this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        }
    }

    void DualDuctAirTerminal::SimDualDuctVarVol(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Jan 2000
        //       MODIFIED       na
        //                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
        //                                 air fraction - a TRACE feature
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple mixing damper.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused0909   USE DataHeatBalFanSys, ONLY: Mat
        using DataHVACGlobals::SmallTempDiff;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;    // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 HumRat;      // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;    // [Watts]
        Real64 Temperature; // [C]
        Real64 QTotLoad;    // [W]
        Real64 QZnReq;      // [W]
        Real64 CpAirZn;     // specific heat of zone air
        Real64 CpAirSysHot;
        Real64 CpAirSysCold;
        Real64 MassFlowBasedOnOA; // Supply air flow rate based on minimum OA requirement
        Real64 AirLoopOAFrac;     // fraction of outdoor air entering air loop outside air system

        // The calculated load from the Heat Balance
        QTotLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        // Calculate all of the required Cp's
        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
        // CpAirSysHot = PsyCpAirFnW(DamperHotAirInlet(DDNum)%AirHumRat,DamperHotAirInlet(DDNum)%AirTemp)
        // CpAirSysCold= PsyCpAirFnW(DamperColdAirInlet(DDNum)%AirHumRat,DamperColdAirInlet(DDNum)%AirTemp)
        CpAirSysHot = CpAirZn;
        CpAirSysCold = CpAirZn;

        // calculate supply air flow rate based on user specified OA requirement
        this->CalcOAMassFlow(state, MassFlowBasedOnOA, AirLoopOAFrac);

        // Then depending on if the Load is for heating or cooling it is handled differently.  First
        // the massflow rate of either heating or cooling is determined to meet the entire load.  Then
        // if the massflow is below the minimum or greater than the Max it is set to either the Min
        // or the Max as specified for the VAV model.
        if (GetCurrentScheduleValue(state, this->SchedPtr) == 0.0) {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;

        } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // Then heating is needed
            // Next check for the denominator equal to zero
            if (std::abs((CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) - (CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp)) /
                    CpAirZn >
                SmallTempDiff) {
                MassFlow = QTotLoad / (CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp - CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp);
            } else {
                // If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            }
            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            if (MassFlow <= (this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac)) {
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                MassFlow = max(MassFlow, this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail);
            } else if (MassFlow >= this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail) {
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            }

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail);

        } else if ((QTotLoad < 0.0) && (this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // Then cooling is required
            // Next check for the denominator equal to zero
            if (std::abs((CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp) - (CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp)) /
                    CpAirZn >
                SmallTempDiff) {
                MassFlow =
                    QTotLoad / (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp);
            } else {
                // If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            }

            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            if ((MassFlow <= (this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac)) && (MassFlow >= 0.0)) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                MassFlow = max(MassFlow, this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail);
            } else if (MassFlow < 0.0) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            } else if (MassFlow >= this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            }

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail);

        } else if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail > 0.0) ||
                   (this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // No Load on Zone set to mixed condition
            MassFlow = (this->dd_airterminalHotAirInlet.AirMassFlowRateMax / 2.0) * this->ZoneMinAirFrac +
                       this->dd_airterminalColdAirInlet.AirMassFlowRateMax / 2.0 * this->ZoneMinAirFrac;

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow =
                min(MassFlow, (this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail + this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail));

        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }

        // Now the massflow for heating or cooling has been determined and if the massflow was reset to the
        // Min or Max we will need to mix the hot and cold deck to meet the zone load.  Knowing the enthalpy
        // of the zone and the hot and cold air flows we can determine exactly by using the Energy and Continuity
        // Eqns.  Of course we have to make sure that we are within the Min and Max flow conditions.
        if (MassFlow > SmallMassFlow) {
            // Determine the enthalpy required from Zone enthalpy and the zone load.
            QZnReq = QTotLoad + MassFlow * CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp;
            // Using the known enthalpies the cold air inlet mass flow is determined.  If the enthalpy of the hot and cold
            // air streams are equal the IF-Then block handles that condition.
            if (std::abs(this->dd_airterminalColdAirInlet.AirTemp - this->dd_airterminalHotAirInlet.AirTemp) > SmallTempDiff) {
                // Calculate the Cold air mass flow rate
                this->dd_airterminalColdAirInlet.AirMassFlowRate =
                    (QZnReq - MassFlow * CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) /
                    (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp);
            } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRate > 0.0)) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }

            // Need to make sure that the flows are within limits
            if (this->dd_airterminalColdAirInlet.AirMassFlowRate > this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;

                // These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < 0.0) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate > MassFlow) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }
            // Using Mass Continuity to determine the other duct flow quantity
            this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow - this->dd_airterminalColdAirInlet.AirMassFlowRate;

            if (this->dd_airterminalHotAirInlet.AirMassFlowRate < state.dataDualDuct->MassFlowSetToler) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < state.dataDualDuct->MassFlowSetToler) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
                this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow;
            }

            // After the flow rates are determined the properties are calculated.
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                      this->dd_airterminalColdAirInlet.AirHumRat * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                     MassFlow;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                        this->dd_airterminalColdAirInlet.AirEnthalpy * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                       MassFlow;

            // IF the system is OFF the properties are calculated for this special case.
        } else {
            this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat + this->dd_airterminalColdAirInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy + this->dd_airterminalColdAirInlet.AirEnthalpy) / 2.0;
        }
        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMinAvail = this->ZoneMinAirFrac * this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the hot and cold damper position in %
        if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMax == 0.0) || (this->dd_airterminalColdAirInlet.AirMassFlowRateMax == 0.0)) {
            this->ColdAirDamperPosition = 0.0;
            this->HotAirDamperPosition = 0.0;
        } else {
            this->ColdAirDamperPosition = this->dd_airterminalColdAirInlet.AirMassFlowRate / this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
            this->HotAirDamperPosition = this->dd_airterminalHotAirInlet.AirMassFlowRate / this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        }
    }

    void DualDuctAirTerminal::SimDualDuctVAVOutdoorAir(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Clayton Miller
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       B. Griffith, Dec 2010, major rework
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Designed to accommodate for systems with outdoor air (OA) and recirculated air (RA)
        // as two separate air streams to controlled at the zone level in a dual duct system.

        // METHODOLOGY EMPLOYED:
        // The terminal unit is be designed to set the airflow of the of the OA stream at the zone
        // level based on the zonal ventilation requirements and the RA stream flowrate of recirculated
        // cooling air stream in order to meet the remaining thermal load.
        // If the zone calls for cooling but the inlet air temperature is too warm, recirc side set to zero
        // if the zone calls for heating and the inlet air is warm enough, modulate damper to meet load
        // if the zone calls for heating and the inlet air is too cold, zero flow (will not control sans reheat)

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataHVACGlobals::SmallTempDiff;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlowMax;     // [kg/sec]   Maximum Mass Flow Rate from OA and Recirc Inlets
        Real64 HumRat;          // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;        // [Watts]
        Real64 Temperature;     // [C]
        Real64 QTotLoadRemain;  // [W]
        Real64 QtoHeatSPRemain; // [W]
        Real64 QtoCoolSPRemain; // [W]
        //  REAL(r64) :: QTotRemainAdjust  ! [W]
        Real64 QtoHeatSPRemainAdjust; // [W]
        Real64 QtoCoolSPRemainAdjust; // [W]
        Real64 QOALoadToHeatSP;       // [W]
        Real64 QOALoadToCoolSP;       // [W]
        Real64 QOALoad;               // Amount of cooling load accounted for by OA Stream [W]
        Real64 QRALoad;               // Amount of cooling load accounted for by Recirc Stream [W]
        Real64 CpAirZn;               // specific heat of zone air
        Real64 CpAirSysOA;            // specific heat of outdoor air
        Real64 CpAirSysRA;            // specific heat of recirculated air
        Real64 OAMassFlow;            // Supply air flow rate based on minimum OA requirement - for printing
        Real64 TotMassFlow;           // [kg/sec]   Total Mass Flow Rate from OA and Recirc Inlets
        int OAInletNodeNum;
        int RecircInletNodeNum;

        OAInletNodeNum = this->OAInletNodeNum;
        if (this->RecircIsUsed) {
            RecircInletNodeNum = this->RecircAirInletNodeNum;
        }
        // Calculate required ventilation air flow rate based on user specified OA requirement
        this->CalcOAOnlyMassFlow(state, OAMassFlow);

        // The calculated load from the Heat Balance, adjusted for any equipment sequenced before terminal
        QTotLoadRemain = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        QtoHeatSPRemain = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        QtoCoolSPRemain = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;

        // Calculate all of the required Cp's
        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
        CpAirSysOA = PsyCpAirFnW(state.dataLoopNodes->Node(OAInletNodeNum).HumRat);
        if (this->RecircIsUsed) CpAirSysRA = PsyCpAirFnW(state.dataLoopNodes->Node(RecircInletNodeNum).HumRat);

        // Set the OA Damper to the calculated ventilation flow rate
        this->dd_airterminalOAInlet.AirMassFlowRate = OAMassFlow;
        // Need to make sure that the OA flows are within limits
        if (this->dd_airterminalOAInlet.AirMassFlowRate > this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail) {
            this->dd_airterminalOAInlet.AirMassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail;
        } else if (this->dd_airterminalOAInlet.AirMassFlowRate < 0.0) {
            this->dd_airterminalOAInlet.AirMassFlowRate = 0.0;
        }

        //..Find the amount of load that the OAMassFlow accounted for
        if (std::abs((CpAirSysOA * this->dd_airterminalOAInlet.AirTemp) - (CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp)) / CpAirZn >
            SmallTempDiff) {
            QOALoad = this->dd_airterminalOAInlet.AirMassFlowRate *
                      (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp - CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp);

            QOALoadToHeatSP = this->dd_airterminalOAInlet.AirMassFlowRate * (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp -
                                                                             CpAirZn * state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum));
            QOALoadToCoolSP = this->dd_airterminalOAInlet.AirMassFlowRate * (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp -
                                                                             CpAirZn * state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum));

        } else {
            QOALoad = 0.0;
            QOALoadToHeatSP = 0.0;
            QOALoadToCoolSP = 0.0;
        }

        if (this->RecircIsUsed) {

            // correct load for recirc side to account for impact of OA side
            // QTotRemainAdjust      = QTotLoadRemain  - QOALoad
            QtoHeatSPRemainAdjust = QtoHeatSPRemain - QOALoadToHeatSP;
            QtoCoolSPRemainAdjust = QtoCoolSPRemain - QOALoadToCoolSP;

            if (QtoCoolSPRemainAdjust < 0.0) {
                QRALoad = QtoCoolSPRemainAdjust;
            } else if (QtoHeatSPRemainAdjust > 0.0) {
                QRALoad = QtoHeatSPRemainAdjust;
            } else {
                QRALoad = 0.0;
            }

            //  IF (QTotLoadRemain == 0.0d0) THEN  ! floating in deadband
            //    IF ((QTotRemainAdjust < 0.0d0) .AND. (QtoCoolSPRemainAdjust < 0.0d0)) THEN !really need cooling
            //      QRALoad = QtoCoolSPRemainAdjust
            //    ELSEIF ((QTotRemainAdjust > 0.0d0) .AND. (QtoHeatSPRemainAdjust > 0.0d0)) THEN ! really need heating
            //      QRALoad = QtoHeatSPRemainAdjust
            //    ELSE
            //      QRALoad = 0.0 ! still floating in deadband even with impact of OA side
            //    ENDIF
            //  ELSE
            //    QRALoad = QTotRemainAdjust
            //  ENDIF

            if (QRALoad < 0.0) {                                                                                         // cooling
                if ((this->dd_airterminalRecircAirInlet.AirTemp - state.dataLoopNodes->Node(ZoneNodeNum).Temp) < -0.5) { // can cool
                    //  Find the Mass Flow Rate of the RA Stream needed to meet the zone cooling load
                    if (std::abs((CpAirSysRA * this->dd_airterminalRecircAirInlet.AirTemp) -
                                 (CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp)) /
                            CpAirZn >
                        SmallTempDiff) {
                        this->dd_airterminalRecircAirInlet.AirMassFlowRate = QRALoad / (CpAirSysRA * this->dd_airterminalRecircAirInlet.AirTemp -
                                                                                        CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp);
                    }
                } else {
                    this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
                }

            } else if (QRALoad > 0.0) { // heating
                //    IF ((dd_airterminalRecircAirInlet(DDNum)%AirTemp - Node(ZoneNodeNum)%Temp) > 2.0d0)  THEN ! can heat
                //      dd_airterminalRecircAirInlet(DDNum)%AirMassFlowRate = QRALoad / &
                //                         (CpAirSysRA*dd_airterminalRecircAirInlet(DDNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
                //    ELSE
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
                //    ENDIF

            } else { // none needed.
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            }

            // Need to make sure that the RA flows are within limits
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRate > this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail;
                // These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
            } else if (this->dd_airterminalRecircAirInlet.AirMassFlowRate < 0.0) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            }

        } else {
            this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail = 0.0;
        } // recirc used

        // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
        // equipment iteration. If detected, set flow rate to previous value.
        if (((std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2) <
              this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag) ||
             (std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3) <
              this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag)) &&
            (std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1) >=
             this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag)) {
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRate > 0.0) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1;
            }
        }

        // Find the Max Box Flow Rate.
        MassFlowMax = this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail + this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail;
        if (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0) {
            TotMassFlow = this->dd_airterminalOAInlet.AirMassFlowRate + this->dd_airterminalRecircAirInlet.AirMassFlowRate;
        } else {
            TotMassFlow = 0.0;
        }

        if (TotMassFlow > SmallMassFlow) {

            // If the sum of the two air streams' flow is greater than the Max Box Flow Rate then reset the RA Stream
            if (TotMassFlow > MassFlowMax) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = MassFlowMax - this->dd_airterminalOAInlet.AirMassFlowRate;
            }
            // After the flow rates are determined the properties are calculated.
            TotMassFlow = this->dd_airterminalOAInlet.AirMassFlowRate + this->dd_airterminalRecircAirInlet.AirMassFlowRate;
            if (TotMassFlow > SmallMassFlow) {
                HumRat = (this->dd_airterminalOAInlet.AirHumRat * this->dd_airterminalOAInlet.AirMassFlowRate +
                          this->dd_airterminalRecircAirInlet.AirHumRat * this->dd_airterminalRecircAirInlet.AirMassFlowRate) /
                         TotMassFlow;
                Enthalpy = (this->dd_airterminalOAInlet.AirEnthalpy * this->dd_airterminalOAInlet.AirMassFlowRate +
                            this->dd_airterminalRecircAirInlet.AirEnthalpy * this->dd_airterminalRecircAirInlet.AirMassFlowRate) /
                           TotMassFlow;
            } else {
                HumRat = (this->dd_airterminalRecircAirInlet.AirHumRat + this->dd_airterminalOAInlet.AirHumRat) / 2.0;
                Enthalpy = (this->dd_airterminalRecircAirInlet.AirEnthalpy + this->dd_airterminalOAInlet.AirEnthalpy) / 2.0;
            }
        } else {

            // The Max Box Flow Rate is zero and the box is off.
            this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalOAInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalRecircAirInlet.AirHumRat + this->dd_airterminalOAInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalRecircAirInlet.AirEnthalpy + this->dd_airterminalOAInlet.AirEnthalpy) / 2.0;
        }

        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = TotMassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlowMax;
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the OA and RA damper position in %
        if (this->RecircIsUsed) {
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRateMax == 0.0) { // protect div by zero
                this->RecircAirDamperPosition = 0.0;
            } else {
                this->RecircAirDamperPosition =
                    this->dd_airterminalRecircAirInlet.AirMassFlowRate / this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
            }
        }

        if (this->dd_airterminalOAInlet.AirMassFlowRateMax == 0.0) { // protect div by zero
            this->OADamperPosition = 0.0;
        } else {
            this->OADamperPosition = this->dd_airterminalOAInlet.AirMassFlowRate / this->dd_airterminalOAInlet.AirMassFlowRateMax;
        }

        // Calculate OAFraction of mixed air after the box
        if (TotMassFlow > 0) {
            if (this->RecircIsUsed) {
                if (this->dd_airterminalOAInlet.AirMassFlowRate == 0.0) {
                    this->OAFraction = 0.0;
                } else if (this->dd_airterminalRecircAirInlet.AirMassFlowRate == 0.0) {
                    this->OAFraction = 1.0;
                } else {
                    this->OAFraction = this->dd_airterminalOAInlet.AirMassFlowRate / TotMassFlow;
                }
            } else {
                this->OAFraction = 1.0;
            }
        } else {
            this->OAFraction = 0.0;
        }

        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3 = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2;
        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2 = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1;
        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1 = this->dd_airterminalRecircAirInlet.AirMassFlowRate;
    }

    void DualDuctAirTerminal::CalcOAMassFlow(EnergyPlusData &state,
                                             Real64 &SAMassFlow,   // outside air based on optional user input
                                             Real64 &AirLoopOAFrac // outside air based on optional user input
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad (FSEC)
        //       DATE WRITTEN   Mar 2010
        //       MODIFIED       Mangesh Basarkar, 06/2011: Modifying outside air based on airloop DCV flag
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the amount of outside air required based on optional user input.
        // Zone multipliers are included and are applied in GetInput.

        // METHODOLOGY EMPLOYED:
        // User input defines method used to calculate OA.

        using Psychrometrics::PsyRhoAirFnPbTdbW;

        bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.

        Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
        Real64 OAMassFlow;       // outside air mass flow rate (kg/s)

        // initialize OA flow rate and OA report variable
        SAMassFlow = 0.0;
        AirLoopOAFrac = 0.0;
        int AirLoopNum = this->AirLoopNum;

        // Calculate the amount of OA based on optional user inputs
        if (AirLoopNum > 0) {
            AirLoopOAFrac = state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFrac;
            // If no additional input from user, RETURN from subroutine
            if (this->NoOAFlowInputFromUser) return;
            // Calculate outdoor air flow rate, zone multipliers are applied in GetInput
            if (AirLoopOAFrac > 0.0) {
                OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                                 this->OARequirementsPtr,
                                                                                 this->ActualZoneNum,
                                                                                 state.dataAirLoop->AirLoopControlInfo(AirLoopNum).AirLoopDCVFlag,
                                                                                 UseMinOASchFlag);
                OAMassFlow = OAVolumeFlowRate * state.dataEnvrn->StdRhoAir;

                // convert OA mass flow rate to supply air flow rate based on air loop OA fraction
                SAMassFlow = OAMassFlow / AirLoopOAFrac;
            }
        }
    }

    void DualDuctAirTerminal::CalcOAOnlyMassFlow(EnergyPlusData &state,
                                                 Real64 &OAMassFlow,           // outside air flow from user input kg/s
                                                 Optional<Real64> MaxOAVolFlow // design level for outside air m3/s
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         C. Miller (Mod of CaclOAMassFlow by R. Raustad (FSEC))
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       B. Griffith, Dec 2010 clean up, sizing optional, scheduled OA
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the amount of outside air required based on optional user input. Returns
        // ONLY calculated OAMassFlow without consideration of AirLoopOAFrac. Used for
        // the DualDuct:VAV:OutdoorAir object which does not mix OA with RA

        // METHODOLOGY EMPLOYED:
        // User input defines method used to calculate OA.

        // REFERENCES:

        // Using/Aliasing
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
        bool UseOccSchFlag;      // TRUE = use actual occupancy, FALSE = use total zone people
        bool PerPersonNotSet;

        // Calculate the amount of OA based on optional user inputs
        OAMassFlow = 0.0;

        // If no additional input from user, RETURN from subroutine
        if (this->NoOAFlowInputFromUser) {
            ShowSevereError(
                state, "CalcOAOnlyMassFlow: Problem in AirTerminal:DualDuct:VAV:OutdoorAir = " + this->Name + ", check outdoor air specification");
            if (present(MaxOAVolFlow)) MaxOAVolFlow = 0.0;
            return;
        }

        if (this->OAPerPersonMode == PerPersonMode::DCVByCurrentLevel) {
            UseOccSchFlag = true;
            PerPersonNotSet = false;
        } else {
            UseOccSchFlag = false;
            PerPersonNotSet = false;
            if (this->OAPerPersonMode == PerPersonMode::ModeNotSet) PerPersonNotSet = true;
        }

        OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(
            state, this->OARequirementsPtr, this->ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, PerPersonNotSet);

        OAMassFlow = OAVolumeFlowRate * state.dataEnvrn->StdRhoAir;

        if (present(MaxOAVolFlow)) {
            OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(
                state, this->OARequirementsPtr, this->ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, false, true);
            MaxOAVolFlow = OAVolumeFlowRate;
        }
    }

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the Damper Module
    // *****************************************************************************

    void DualDuctAirTerminal::UpdateDualDuct(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   February 2000
        //       MODIFIED       Aug 2010 Clayton Miller - Added DualDuctVAVOutdoorAir
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the dampers.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;
        int HotInletNode;
        int ColdInletNode;
        int OAInletNode; // Outdoor Air Duct Inlet Node - for DualDuctOutdoorAir
        int RAInletNode; // Recirculated Air Duct Inlet Node - for DualDuctOutdoorAir

        if (this->DamperType == DualDuctDamper::ConstantVolume || this->DamperType == DualDuctDamper::VariableVolume) {

            OutletNode = this->OutletNodeNum;
            HotInletNode = this->HotAirInletNodeNum;
            ColdInletNode = this->ColdAirInletNodeNum;

            // Set the outlet air nodes of the Damper
            state.dataLoopNodes->Node(HotInletNode).MassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRate;
            state.dataLoopNodes->Node(ColdInletNode).MassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRate = this->dd_airterminalOutlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = this->dd_airterminalOutlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = this->dd_airterminalOutlet.AirMassFlowRateMinAvail;
            state.dataLoopNodes->Node(OutletNode).Temp = this->dd_airterminalOutlet.AirTemp;
            state.dataLoopNodes->Node(OutletNode).HumRat = this->dd_airterminalOutlet.AirHumRat;
            state.dataLoopNodes->Node(OutletNode).Enthalpy = this->dd_airterminalOutlet.AirEnthalpy;
            // Set the outlet nodes for properties that just pass through & not used
            // FIX THIS LATER!!!!
            state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(HotInletNode).Quality;
            state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(HotInletNode).Press;

            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                if (state.dataLoopNodes->Node(OutletNode).MassFlowRate > 0.0) {
                    state.dataLoopNodes->Node(OutletNode).CO2 =
                        (state.dataLoopNodes->Node(HotInletNode).CO2 * state.dataLoopNodes->Node(HotInletNode).MassFlowRate +
                         state.dataLoopNodes->Node(ColdInletNode).CO2 * state.dataLoopNodes->Node(ColdInletNode).MassFlowRate) /
                        state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                } else {
                    state.dataLoopNodes->Node(OutletNode).CO2 =
                        max(state.dataLoopNodes->Node(HotInletNode).CO2, state.dataLoopNodes->Node(ColdInletNode).CO2);
                }
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                if (state.dataLoopNodes->Node(OutletNode).MassFlowRate > 0.0) {
                    state.dataLoopNodes->Node(OutletNode).GenContam =
                        (state.dataLoopNodes->Node(HotInletNode).GenContam * state.dataLoopNodes->Node(HotInletNode).MassFlowRate +
                         state.dataLoopNodes->Node(ColdInletNode).GenContam * state.dataLoopNodes->Node(ColdInletNode).MassFlowRate) /
                        state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                } else {
                    state.dataLoopNodes->Node(OutletNode).GenContam =
                        max(state.dataLoopNodes->Node(HotInletNode).GenContam, state.dataLoopNodes->Node(ColdInletNode).GenContam);
                }
            }

            this->CalcOutdoorAirVolumeFlowRate(state);

        } else if (this->DamperType == DualDuctDamper::OutdoorAir) {

            OutletNode = this->OutletNodeNum;
            OAInletNode = this->OAInletNodeNum;
            if (this->RecircIsUsed) {
                RAInletNode = this->RecircAirInletNodeNum;
                state.dataLoopNodes->Node(RAInletNode).MassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRate;
            }
            // Set the outlet air nodes of the Damper
            state.dataLoopNodes->Node(OAInletNode).MassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRate = this->dd_airterminalOutlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = this->dd_airterminalOutlet.AirMassFlowRate;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = this->dd_airterminalOutlet.AirMassFlowRateMinAvail;
            state.dataLoopNodes->Node(OutletNode).Temp = this->dd_airterminalOutlet.AirTemp;
            state.dataLoopNodes->Node(OutletNode).HumRat = this->dd_airterminalOutlet.AirHumRat;
            state.dataLoopNodes->Node(OutletNode).Enthalpy = this->dd_airterminalOutlet.AirEnthalpy;
            // Set the outlet nodes for properties that just pass through & not used
            // FIX THIS LATER!!!!
            state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(OAInletNode).Quality;
            state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(OAInletNode).Press;

            if (this->RecircIsUsed) {
                if (state.dataLoopNodes->Node(OutletNode).MassFlowRate > 0.0) {
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataLoopNodes->Node(OutletNode).CO2 =
                            (state.dataLoopNodes->Node(OAInletNode).CO2 * state.dataLoopNodes->Node(OAInletNode).MassFlowRate +
                             state.dataLoopNodes->Node(RAInletNode).CO2 * state.dataLoopNodes->Node(RAInletNode).MassFlowRate) /
                            state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataLoopNodes->Node(OutletNode).GenContam =
                            (state.dataLoopNodes->Node(OAInletNode).GenContam * state.dataLoopNodes->Node(OAInletNode).MassFlowRate +
                             state.dataLoopNodes->Node(RAInletNode).GenContam * state.dataLoopNodes->Node(RAInletNode).MassFlowRate) /
                            state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                    }
                } else {
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataLoopNodes->Node(OutletNode).CO2 =
                            max(state.dataLoopNodes->Node(OAInletNode).CO2, state.dataLoopNodes->Node(RAInletNode).CO2);
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataLoopNodes->Node(OutletNode).GenContam =
                            max(state.dataLoopNodes->Node(OAInletNode).GenContam, state.dataLoopNodes->Node(RAInletNode).GenContam);
                    }
                }

            } else {
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataLoopNodes->Node(OutletNode).CO2 = state.dataLoopNodes->Node(OAInletNode).CO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataLoopNodes->Node(OutletNode).GenContam = state.dataLoopNodes->Node(OAInletNode).GenContam;
                }
            }
        }
    }

    //        End of Update subroutines for the Damper Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the Damper Module
    // *****************************************************************************

    void DualDuctAirTerminal::ReportDualDuct() // unused1208
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Unknown
        //       DATE WRITTEN   Unknown
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the damper report variables.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Still needs to report the Damper power from this component
    }

    void ReportDualDuctConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte
        //       DATE WRITTEN   February 2004
        //       MODIFIED       B. Griffith, DOAS VAV dual duct
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Report dual duct damper connections to the BND file.

        // Using/Aliasing
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

        // Formats
        static constexpr std::string_view Format_100("! <#Dual Duct Damper Connections>,<Number of Dual Duct Damper Connections>");
        static constexpr std::string_view Format_102(
            "! <Dual Duct Damper>,<Dual Duct Damper Count>,<Dual Duct Damper Name>,<Inlet Node>,<Outlet Node>,<Inlet "
            "Node Type>,<AirLoopHVAC Name>");

        if (!allocated(state.dataDualDuct->dd_airterminal))
            return; // Autodesk Bug: Can arrive here with Damper unallocated (SimulateDualDuct not yet called) with NumDDAirTerminal either set >0 or
                    // uninitialized

        // Report Dual Duct Dampers to BND File
        print(state.files.bnd, "{}\n", "! ===============================================================");
        print(state.files.bnd, "{}\n", Format_100);
        print(state.files.bnd, " #Dual Duct Damper Connections,{}\n", state.dataDualDuct->NumDDAirTerminal * 2);
        print(state.files.bnd, "{}\n", Format_102);

        for (int Count1 = 1; Count1 <= state.dataDualDuct->NumDDAirTerminal; ++Count1) {

            // Determine if this damper is connected to a supply air path
            int Found = 0;
            int SupplyAirPathNum = 0;
            for (int Count2 = 1; Count2 <= state.dataZoneEquip->NumSupplyAirPaths; ++Count2) {
                SupplyAirPathNum = Count2;
                Found = 0;
                for (int Count3 = 1; Count3 <= state.dataZoneEquip->SupplyAirPath(Count2).NumOutletNodes; ++Count3) {
                    if (state.dataDualDuct->dd_airterminal(Count1).HotAirInletNodeNum ==
                        state.dataZoneEquip->SupplyAirPath(Count2).OutletNode(Count3))
                        Found = Count3;
                    if (state.dataDualDuct->dd_airterminal(Count1).ColdAirInletNodeNum ==
                        state.dataZoneEquip->SupplyAirPath(Count2).OutletNode(Count3))
                        Found = Count3;
                    if (state.dataDualDuct->dd_airterminal(Count1).OAInletNodeNum == state.dataZoneEquip->SupplyAirPath(Count2).OutletNode(Count3))
                        Found = Count3;
                    if (state.dataDualDuct->dd_airterminal(Count1).RecircAirInletNodeNum ==
                        state.dataZoneEquip->SupplyAirPath(Count2).OutletNode(Count3))
                        Found = Count3;
                }
                if (Found != 0) break;
            }
            if (Found == 0) SupplyAirPathNum = 0;

            // Determine which air loop this dual duct damper is connected to
            Found = 0;
            std::string ChrName;
            for (int Count2 = 1; Count2 <= NumPrimaryAirSys; ++Count2) {
                ChrName = state.dataAirLoop->AirToZoneNodeInfo(Count2).AirLoopName;
                Found = 0;
                for (int Count3 = 1; Count3 <= state.dataAirLoop->AirToZoneNodeInfo(Count2).NumSupplyNodes; ++Count3) {
                    if (SupplyAirPathNum != 0) {
                        if (state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).InletNodeNum ==
                            state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3))
                            Found = Count3;
                    } else {
                        if (state.dataDualDuct->dd_airterminal(Count1).HotAirInletNodeNum ==
                            state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3))
                            Found = Count3;
                        if (state.dataDualDuct->dd_airterminal(Count1).ColdAirInletNodeNum ==
                            state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3))
                            Found = Count3;
                        if (state.dataDualDuct->dd_airterminal(Count1).OAInletNodeNum ==
                            state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3))
                            Found = Count3;
                        if (state.dataDualDuct->dd_airterminal(Count1).RecircAirInletNodeNum ==
                            state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3))
                            Found = Count3;
                    }
                }
                if (Found != 0) break;
            }
            if (Found == 0) ChrName = "**Unknown**";

            std::string DamperType;
            if (state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::ConstantVolume) {
                DamperType = state.dataDualDuct->cCMO_DDConstantVolume;
            } else if (state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::VariableVolume) {
                DamperType = state.dataDualDuct->cCMO_DDVariableVolume;
            } else if (state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::OutdoorAir) {
                DamperType = state.dataDualDuct->cCMO_DDVarVolOA;
            } else {
                DamperType = "Invalid/Unknown";
            }

            if ((state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::ConstantVolume) ||
                (state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::VariableVolume)) {
                print(state.files.bnd,
                      " Dual Duct Damper,{},{},{},{},{},Hot Air,{}\n",
                      Count1,
                      DamperType,
                      state.dataDualDuct->dd_airterminal(Count1).Name,
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).HotAirInletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).OutletNodeNum),
                      ChrName);

                print(state.files.bnd,
                      " Dual Duct Damper,{},{},{},{},{},Cold Air,{}\n",
                      Count1,
                      DamperType,
                      state.dataDualDuct->dd_airterminal(Count1).Name,
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).ColdAirInletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).OutletNodeNum),
                      ChrName);

            } else if (state.dataDualDuct->dd_airterminal(Count1).DamperType == DualDuctDamper::OutdoorAir) {
                print(state.files.bnd,
                      "Dual Duct Damper, {},{},{},{},{},Outdoor Air,{}\n",
                      Count1,
                      DamperType,
                      state.dataDualDuct->dd_airterminal(Count1).Name,
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).OAInletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).OutletNodeNum),
                      ChrName);
                print(state.files.bnd,
                      "Dual Duct Damper, {},{},{},{},{},Recirculated Air,{}\n",
                      Count1,
                      DamperType,
                      state.dataDualDuct->dd_airterminal(Count1).Name,
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).RecircAirInletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataDualDuct->dd_airterminal(Count1).OutletNodeNum),
                      ChrName);
            }
        }
    }

    void GetDualDuctOutdoorAirRecircUse(EnergyPlusData &state,
                                        [[maybe_unused]] std::string const &CompTypeName,
                                        std::string_view CompName,
                                        bool &RecircIsUsed)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Aug 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get routine to learn if a dual duct outdoor air unit is using its recirc deck

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DamperIndex;                 // Loop index to Damper that you are currently loading input into
        std::string CurrentModuleObject; // for ease in getting objects

        int NumAlphas;
        int NumNums;
        int IOStat;

        RecircIsUsed = true;

        // this doesn't work because it fires code that depends on things being further along
        //  IF (GetDualDuctInputFlag) THEN  !First time subroutine has been entered
        //    CALL GetDualDuctInput
        //    GetDualDuctInputFlag=.FALSE.
        //  END IF

        if (state.dataDualDuct->GetDualDuctOutdoorAirRecircUseFirstTimeOnly) {
            state.dataDualDuct->NumDualDuctVarVolOA =
                state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataDualDuct->cCMO_DDVarVolOA);
            state.dataDualDuct->RecircIsUsedARR.allocate(state.dataDualDuct->NumDualDuctVarVolOA);
            state.dataDualDuct->DamperNamesARR.allocate(state.dataDualDuct->NumDualDuctVarVolOA);
            if (state.dataDualDuct->NumDualDuctVarVolOA > 0) {
                Array1D<Real64> NumArray(2, 0.0);
                Array1D_string AlphArray(7);
                Array1D_string cAlphaFields(7);       // Alpha field names
                Array1D_string cNumericFields(2);     // Numeric field names
                Array1D_bool lAlphaBlanks(7, true);   // Logical array, alpha field input BLANK = .TRUE.
                Array1D_bool lNumericBlanks(2, true); // Logical array, numeric field input BLANK = .TRUE.
                for (DamperIndex = 1; DamperIndex <= state.dataDualDuct->NumDualDuctVarVolOA; ++DamperIndex) {

                    CurrentModuleObject = state.dataDualDuct->cCMO_DDVarVolOA;

                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             DamperIndex,
                                                                             AlphArray,
                                                                             NumAlphas,
                                                                             NumArray,
                                                                             NumNums,
                                                                             IOStat,
                                                                             lNumericBlanks,
                                                                             lAlphaBlanks,
                                                                             cAlphaFields,
                                                                             cNumericFields);
                    state.dataDualDuct->DamperNamesARR(DamperIndex) = AlphArray(1);
                    if (!lAlphaBlanks(5)) {
                        state.dataDualDuct->RecircIsUsedARR(DamperIndex) = true;
                    } else {
                        state.dataDualDuct->RecircIsUsedARR(DamperIndex) = false;
                    }
                }
            }
            state.dataDualDuct->GetDualDuctOutdoorAirRecircUseFirstTimeOnly = false;
        }

        DamperIndex = UtilityRoutines::FindItemInList(CompName, state.dataDualDuct->DamperNamesARR, state.dataDualDuct->NumDualDuctVarVolOA);
        if (DamperIndex > 0) {
            RecircIsUsed = state.dataDualDuct->RecircIsUsedARR(DamperIndex);
        }
    }

    void DualDuctAirTerminal::CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state)
    {
        // calculates zone outdoor air volume flow rate using the supply air flow rate and OA fraction
        if (this->AirLoopNum > 0) {
            this->OutdoorAirFlowRate =
                (this->dd_airterminalOutlet.AirMassFlowRate / state.dataEnvrn->StdRhoAir) * state.dataAirLoop->AirLoopFlow(this->AirLoopNum).OAFrac;
        } else {
            // do nothing for now
        }
    }

    //        End of Reporting subroutines for the Damper Module
    // *****************************************************************************

} // namespace DualDuct

} // namespace EnergyPlus
