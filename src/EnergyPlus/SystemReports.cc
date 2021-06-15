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
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>
#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowAC.hh>

namespace EnergyPlus::SystemReports {

// Module containing the routines dealing with Mechanical Ventilation Loads and Energy Reporting (Outside Air)

// MODULE INFORMATION:
//       AUTHOR         Mike Witte, Linda Lawrie, Dan Fisher
//       DATE WRITTEN   Apr-Jul 2005
//       MODIFIED       22Aug2010 Craig Wray - added Fan:ComponentModel
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module embodies the scheme(s) for reporting ventilation loads and energy use.

// Using/Aliasing
using namespace DataLoopNode;
using namespace DataAirLoop;
using namespace DataHVACGlobals;
using namespace DataPlant;
using namespace DataZoneEquipment;
using namespace DataAirSystems;

// Functions

void InitEnergyReports(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   April 2005
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initializes the energy components of the data structures

    // METHODOLOGY EMPLOYED:
    // Once all compsets have been established (second iteration) find all components
    // subcomponents, etc.

    // Using/Aliasing
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace DataGlobalConstants;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr EnergyTransfer(1);

    int AirDistUnitNum;
    int MatchLoop;
    int MatchLoopType;
    int MatchBranch;
    int MatchComp;
    int AirLoopNum;
    int BranchNum;
    int ZoneInletNodeNum;
    int CompNum;
    int VarNum;
    int SubCompNum;
    int SubSubCompNum;
    int EquipNum;
    int SubEquipNum;
    int SubSubEquipNum;
    int CtrlZoneNum;
    int NodeIndex;
    int Idx;
    int ListNum;
    int SAPNum;
    int SAPOutNode;
    int MainBranchNum;
    int SupplyCoolBranchNum;
    int SupplyHeatBranchNum;
    int VarType;
    int VarIndex;
    int OutNum;
    int NodeCount;
    int PlantLoopNum;
    int NumZoneConnectComps;
    int NumZoneConnectSubComps;
    int NumZoneConnectSubSubComps;
    int NumAirSysConnectComps;
    int NumAirSysConnectSubComps;
    int NumAirSysConnectSubSubComps;
    int ArrayCount;
    int LoopType;
    int LoopNum;
    int FirstIndex;
    int LastIndex;
    int LoopCount;
    std::string CompType;
    std::string CompName;
    bool MatchFound;
    bool ConnectionFlag(false);

    if (!state.dataSysRpts->VentReportStructureCreated) return;

    if (state.dataSysRpts->OneTimeFlag_InitEnergyReports) {

        // ***I think we need to preprocess the main components on the branch to get them in order***
        // This needs to be done before we start in on the component loop
        // GetChildrenData will put all of the subcomponents in order for us

        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex =
                UtilityRoutines::FindItemInList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListName, state.dataZoneEquip->ZoneEquipList);
            ListNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex;
            for (ZoneInletNodeNum = 1; ZoneInletNodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInletNodeNum) {
                AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInletNodeNum);
                for (CompNum = 1; CompNum <= state.dataZoneEquip->ZoneEquipList(ListNum).NumOfEquipTypes; ++CompNum) {
                    for (NodeCount = 1; NodeCount <= state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(CompNum).NumOutlets; ++NodeCount) {
                        if (state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(CompNum).OutletNodeNums(NodeCount) ==
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).OutNode) {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).AirDistUnitIndex = CompNum;
                            if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).SupplyAirPathExists) {
                                for (SAPNum = 1; SAPNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SAPNum) {
                                    for (SAPOutNode = 1; SAPOutNode <= state.dataZoneEquip->SupplyAirPath(SAPNum).NumOutletNodes; ++SAPOutNode) {
                                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).InNode ==
                                            state.dataZoneEquip->SupplyAirPath(SAPNum).OutletNode(SAPOutNode)) {
                                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).SupplyAirPathIndex =
                                                SAPNum;
                                            for (OutNum = 1; OutNum <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++OutNum) {
                                                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum) ==
                                                    state.dataZoneEquip->SupplyAirPath(SAPNum).InletNodeNum) {
                                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                        .AirDistUnitCool(ZoneInletNodeNum)
                                                        .SupplyBranchIndex =
                                                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OutletBranchNum(OutNum);
                                                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.Exists) {
                                                        for (MainBranchNum = 1;
                                                             MainBranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                             ++MainBranchNum) {
                                                            if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                                    .Branch(MainBranchNum)
                                                                    .NodeNumOut ==
                                                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.NodeNumIn) {
                                                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                    .AirDistUnitCool(ZoneInletNodeNum)
                                                                    .MainBranchIndex = MainBranchNum;
                                                            }
                                                        }
                                                    } else { // no splitter
                                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                            .AirDistUnitCool(ZoneInletNodeNum)
                                                            .MainBranchIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                                   .AirDistUnitCool(ZoneInletNodeNum)
                                                                                   .SupplyBranchIndex;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else { // no supply air path
                                if (AirLoopNum > 0) {
                                    for (NodeIndex = 1; NodeIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++NodeIndex) {
                                        if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(NodeIndex) ==
                                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).InNode) {
                                            for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                 ++BranchNum) {
                                                if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut ==
                                                    state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(NodeIndex)) {
                                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                        .AirDistUnitCool(ZoneInletNodeNum)
                                                        .SupplyBranchIndex = BranchNum;
                                                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.Exists) {
                                                        for (MainBranchNum = 1;
                                                             MainBranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                             ++MainBranchNum) {
                                                            if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                                    .Branch(MainBranchNum)
                                                                    .NodeNumOut ==
                                                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.NodeNumIn) {
                                                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                    .AirDistUnitCool(ZoneInletNodeNum)
                                                                    .MainBranchIndex = MainBranchNum;
                                                            }
                                                        }
                                                    } else { // no splitter
                                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                            .AirDistUnitCool(ZoneInletNodeNum)
                                                            .MainBranchIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                                   .AirDistUnitCool(ZoneInletNodeNum)
                                                                                   .SupplyAirPathIndex;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else if (state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(CompNum).OutletNodeNums(NodeCount) ==
                                   state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).InNode) {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).AirDistUnitIndex = CompNum;
                            if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).SupplyAirPathExists) {
                                for (SAPNum = 1; SAPNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SAPNum) {
                                    for (NodeIndex = 1; NodeIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++NodeIndex) {
                                        if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(NodeIndex) ==
                                            state.dataZoneEquip->SupplyAirPath(SAPNum).InletNodeNum) {
                                            for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                 ++BranchNum) {
                                                if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut ==
                                                    state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(NodeIndex)) {
                                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                        .AirDistUnitHeat(ZoneInletNodeNum)
                                                        .SupplyBranchIndex = BranchNum;
                                                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.Exists) {
                                                        for (MainBranchNum = 1;
                                                             MainBranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                             ++MainBranchNum) {
                                                            if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                                    .Branch(MainBranchNum)
                                                                    .NodeNumOut ==
                                                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.NodeNumIn) {
                                                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                    .AirDistUnitHeat(ZoneInletNodeNum)
                                                                    .MainBranchIndex = MainBranchNum;
                                                            }
                                                        }
                                                    } else { // no splitter
                                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                            .AirDistUnitHeat(ZoneInletNodeNum)
                                                            .MainBranchIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                                   .AirDistUnitHeat(ZoneInletNodeNum)
                                                                                   .SupplyAirPathIndex;
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    for (SAPOutNode = 1; SAPOutNode <= state.dataZoneEquip->SupplyAirPath(SAPNum).NumOutletNodes; ++SAPOutNode) {
                                        if (ZoneInletNodeNum == state.dataZoneEquip->SupplyAirPath(SAPNum).OutletNode(SAPOutNode)) {
                                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).SupplyAirPathIndex =
                                                SAPNum;
                                        }
                                    }
                                }
                            } else { // no supply air path
                                if (AirLoopNum > 0) {
                                    for (NodeIndex = 1; NodeIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++NodeIndex) {
                                        if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(NodeIndex) ==
                                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).InNode) {
                                            for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                 ++BranchNum) {
                                                if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut ==
                                                    state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(NodeIndex)) {
                                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                        .AirDistUnitHeat(ZoneInletNodeNum)
                                                        .SupplyBranchIndex = BranchNum;
                                                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.Exists) {
                                                        for (MainBranchNum = 1;
                                                             MainBranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches;
                                                             ++MainBranchNum) {
                                                            if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                                    .Branch(MainBranchNum)
                                                                    .NodeNumOut ==
                                                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Splitter.NodeNumIn) {
                                                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                    .AirDistUnitHeat(ZoneInletNodeNum)
                                                                    .MainBranchIndex = MainBranchNum;
                                                            }
                                                        }
                                                    } else { // no splitter
                                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                            .AirDistUnitHeat(ZoneInletNodeNum)
                                                            .MainBranchIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum)
                                                                                   .AirDistUnitHeat(ZoneInletNodeNum)
                                                                                   .SupplyAirPathIndex;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {

                            // Can't tell if there's an error based on this code...need to check logical flags separately
                        }
                    }
                }
            }
        }

        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex =
                UtilityRoutines::FindItemInList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListName, state.dataZoneEquip->ZoneEquipList);
            ListNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex;
            // loop over the zone supply air path inlet nodes
            for (ZoneInletNodeNum = 1; ZoneInletNodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInletNodeNum) {
                AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInletNodeNum);

                // 1. Find HVAC component plant loop connections
                MainBranchNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).MainBranchIndex;
                MainBranchNum =
                    max(MainBranchNum, state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).MainBranchIndex);
                if (MainBranchNum > 0) MatchPlantSys(state, AirLoopNum, MainBranchNum);
                SupplyCoolBranchNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).SupplyBranchIndex;
                if (SupplyCoolBranchNum > 0 && (SupplyCoolBranchNum != MainBranchNum)) MatchPlantSys(state, AirLoopNum, SupplyCoolBranchNum);
                SupplyHeatBranchNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).SupplyBranchIndex;
                if (SupplyHeatBranchNum > 0 && (SupplyHeatBranchNum != MainBranchNum)) MatchPlantSys(state, AirLoopNum, SupplyHeatBranchNum);

                AirDistUnitNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInletNodeNum).AirDistUnitIndex;
                AirDistUnitNum =
                    max(AirDistUnitNum, state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInletNodeNum).AirDistUnitIndex);
                if (ListNum > 0 && AirDistUnitNum > 0) {
                    for (VarNum = 1; VarNum <= state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).NumMeteredVars; ++VarNum) {
                        if (state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).MeteredVar(VarNum).ResourceType ==
                            DataGlobalConstants::ResourceType::EnergyTransfer) {
                            state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).EnergyTransComp = EnergyTransfer;
                            CompType = state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).TypeOf;
                            CompName = state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).Name;
                            Idx = 0;
                            FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                            if (MatchFound)
                                UpdateZoneCompPtrArray(state, Idx, ListNum, AirDistUnitNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                            state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).ZoneEqToPlantPtr = Idx;
                            break;
                        }
                    }
                    for (SubEquipNum = 1; SubEquipNum <= state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).NumSubEquip;
                         ++SubEquipNum) {
                        for (VarNum = 1;
                             VarNum <= state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).NumMeteredVars;
                             ++VarNum) {
                            if (state.dataZoneEquip->ZoneEquipList(ListNum)
                                    .EquipData(AirDistUnitNum)
                                    .SubEquipData(SubEquipNum)
                                    .MeteredVar(VarNum)
                                    .ResourceType == DataGlobalConstants::ResourceType::EnergyTransfer) {
                                state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).EnergyTransComp =
                                    EnergyTransfer;
                                CompType = state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).TypeOf;
                                CompName = state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).Name;
                                Idx = 0;
                                FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                if (MatchFound)
                                    UpdateZoneSubCompPtrArray(
                                        state, Idx, ListNum, AirDistUnitNum, SubEquipNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).ZoneEqToPlantPtr =
                                    Idx;
                                break;
                            }
                        }
                        for (SubSubEquipNum = 1;
                             SubSubEquipNum <=
                             state.dataZoneEquip->ZoneEquipList(ListNum).EquipData(AirDistUnitNum).SubEquipData(SubEquipNum).NumSubSubEquip;
                             ++SubSubEquipNum) {
                            for (VarNum = 1; VarNum <= state.dataZoneEquip->ZoneEquipList(ListNum)
                                                           .EquipData(AirDistUnitNum)
                                                           .SubEquipData(SubEquipNum)
                                                           .SubSubEquipData(SubSubEquipNum)
                                                           .NumMeteredVars;
                                 ++VarNum) {
                                if (state.dataZoneEquip->ZoneEquipList(ListNum)
                                        .EquipData(AirDistUnitNum)
                                        .SubEquipData(SubEquipNum)
                                        .SubSubEquipData(SubSubEquipNum)
                                        .MeteredVar(VarNum)
                                        .ResourceType == DataGlobalConstants::ResourceType::EnergyTransfer) {
                                    state.dataZoneEquip->ZoneEquipList(ListNum)
                                        .EquipData(AirDistUnitNum)
                                        .SubEquipData(SubEquipNum)
                                        .SubSubEquipData(SubSubEquipNum)
                                        .EnergyTransComp = EnergyTransfer;
                                    CompType = state.dataZoneEquip->ZoneEquipList(ListNum)
                                                   .EquipData(AirDistUnitNum)
                                                   .SubEquipData(SubEquipNum)
                                                   .SubSubEquipData(SubSubEquipNum)
                                                   .TypeOf;
                                    CompName = state.dataZoneEquip->ZoneEquipList(ListNum)
                                                   .EquipData(AirDistUnitNum)
                                                   .SubEquipData(SubEquipNum)
                                                   .SubSubEquipData(SubSubEquipNum)
                                                   .Name;
                                    Idx = 0;
                                    FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                    if (MatchFound)
                                        UpdateZoneSubSubCompPtrArray(state,
                                                                     Idx,
                                                                     ListNum,
                                                                     AirDistUnitNum,
                                                                     SubEquipNum,
                                                                     SubSubEquipNum,
                                                                     MatchLoopType,
                                                                     MatchLoop,
                                                                     MatchBranch,
                                                                     MatchComp);
                                    state.dataZoneEquip->ZoneEquipList(ListNum)
                                        .EquipData(AirDistUnitNum)
                                        .SubEquipData(SubEquipNum)
                                        .SubSubEquipData(SubSubEquipNum)
                                        .ZoneEqToPlantPtr = Idx;
                                    break;
                                }
                            }
                        }
                    }
                }

                // Eliminate duplicates in the connection arrays
                if (allocated(state.dataAirSystemsData->ZoneCompToPlant)) {
                    EquipNum = isize(state.dataAirSystemsData->ZoneCompToPlant);
                } else {
                    EquipNum = 0;
                }
                if (allocated(state.dataAirSystemsData->ZoneSubCompToPlant)) {
                    SubEquipNum = isize(state.dataAirSystemsData->ZoneSubCompToPlant);
                } else {
                    SubEquipNum = 0;
                }
                if (allocated(state.dataAirSystemsData->ZoneSubSubCompToPlant)) {
                    SubSubEquipNum = isize(state.dataAirSystemsData->ZoneSubSubCompToPlant);
                } else {
                    SubSubEquipNum = 0;
                }
                if (allocated(state.dataAirSystemsData->AirSysCompToPlant)) {
                    CompNum = isize(state.dataAirSystemsData->AirSysCompToPlant);
                } else {
                    CompNum = 0;
                }
                if (allocated(state.dataAirSystemsData->AirSysSubCompToPlant)) {
                    SubCompNum = isize(state.dataAirSystemsData->AirSysSubCompToPlant);
                } else {
                    SubCompNum = 0;
                }
                if (allocated(state.dataAirSystemsData->AirSysSubSubCompToPlant)) {
                    SubSubCompNum = isize(state.dataAirSystemsData->AirSysSubSubCompToPlant);
                } else {
                    SubSubCompNum = 0;
                }

                if (EquipNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= EquipNum; ++i) {
                        auto const &zi(state.dataAirSystemsData->ZoneCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &zj(state.dataAirSystemsData->ZoneCompToPlant(j));
                            if ((zi.ZoneEqListNum == zj.ZoneEqListNum) && (zi.ZoneEqCompNum == zj.ZoneEqCompNum)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &za(state.dataAirSystemsData->ZoneCompToPlant(ArrayCount));
                                za.ZoneEqListNum = zi.ZoneEqListNum;
                                za.ZoneEqCompNum = zi.ZoneEqCompNum;
                                za.PlantLoopType = zi.PlantLoopType;
                                za.PlantLoopNum = zi.PlantLoopNum;
                                za.PlantLoopBranch = zi.PlantLoopBranch;
                                za.PlantLoopComp = zi.PlantLoopComp;
                                za.FirstDemandSidePtr = zi.FirstDemandSidePtr;
                                za.LastDemandSidePtr = zi.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= EquipNum; ++i) { // Zero the now-unused entries
                        auto &zi(state.dataAirSystemsData->ZoneCompToPlant(i));
                        zi.ZoneEqListNum = 0;
                        zi.ZoneEqCompNum = 0;
                        zi.PlantLoopType = 0;
                        zi.PlantLoopNum = 0;
                        zi.PlantLoopBranch = 0;
                        zi.PlantLoopComp = 0;
                        zi.FirstDemandSidePtr = 0;
                        zi.LastDemandSidePtr = 0;
                    }
                }

                if (SubEquipNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= SubEquipNum; ++i) {
                        auto const &zi(state.dataAirSystemsData->ZoneSubCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &zj(state.dataAirSystemsData->ZoneSubCompToPlant(j));
                            if ((zi.ZoneEqListNum == zj.ZoneEqListNum) && (zi.ZoneEqCompNum == zj.ZoneEqCompNum) &&
                                (zi.ZoneEqSubCompNum == zj.ZoneEqSubCompNum)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &za(state.dataAirSystemsData->ZoneSubCompToPlant(ArrayCount));
                                za.ZoneEqListNum = zi.ZoneEqListNum;
                                za.ZoneEqCompNum = zi.ZoneEqCompNum;
                                za.ZoneEqSubCompNum = zi.ZoneEqSubCompNum;
                                za.PlantLoopType = zi.PlantLoopType;
                                za.PlantLoopNum = zi.PlantLoopNum;
                                za.PlantLoopBranch = zi.PlantLoopBranch;
                                za.PlantLoopComp = zi.PlantLoopComp;
                                za.FirstDemandSidePtr = zi.FirstDemandSidePtr;
                                za.LastDemandSidePtr = zi.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= SubEquipNum; ++i) { // Zero the now-unused entries
                        auto &zi(state.dataAirSystemsData->ZoneSubCompToPlant(i));
                        zi.ZoneEqListNum = 0;
                        zi.ZoneEqCompNum = 0;
                        zi.ZoneEqSubCompNum = 0;
                        zi.PlantLoopType = 0;
                        zi.PlantLoopNum = 0;
                        zi.PlantLoopBranch = 0;
                        zi.PlantLoopComp = 0;
                        zi.FirstDemandSidePtr = 0;
                        zi.LastDemandSidePtr = 0;
                    }
                }

                if (SubSubEquipNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= SubSubEquipNum; ++i) {
                        auto const &zi(state.dataAirSystemsData->ZoneSubSubCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &zj(state.dataAirSystemsData->ZoneSubSubCompToPlant(j));
                            if ((zi.ZoneEqListNum == zj.ZoneEqListNum) && (zi.ZoneEqCompNum == zj.ZoneEqCompNum) &&
                                (zi.ZoneEqSubCompNum == zj.ZoneEqSubCompNum) && (zi.ZoneEqSubSubCompNum == zj.ZoneEqSubSubCompNum)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &za(state.dataAirSystemsData->ZoneSubSubCompToPlant(ArrayCount));
                                za.ZoneEqListNum = zi.ZoneEqListNum;
                                za.ZoneEqCompNum = zi.ZoneEqCompNum;
                                za.ZoneEqSubCompNum = zi.ZoneEqSubCompNum;
                                za.ZoneEqSubSubCompNum = zi.ZoneEqSubSubCompNum;
                                za.PlantLoopType = zi.PlantLoopType;
                                za.PlantLoopNum = zi.PlantLoopNum;
                                za.PlantLoopBranch = zi.PlantLoopBranch;
                                za.PlantLoopComp = zi.PlantLoopComp;
                                za.FirstDemandSidePtr = zi.FirstDemandSidePtr;
                                za.LastDemandSidePtr = zi.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= SubSubEquipNum; ++i) { // Zero the now-unused entries
                        auto &zi(state.dataAirSystemsData->ZoneSubSubCompToPlant(i));
                        zi.ZoneEqListNum = 0;
                        zi.ZoneEqCompNum = 0;
                        zi.ZoneEqSubCompNum = 0;
                        zi.ZoneEqSubSubCompNum = 0;
                        zi.PlantLoopType = 0;
                        zi.PlantLoopNum = 0;
                        zi.PlantLoopBranch = 0;
                        zi.PlantLoopComp = 0;
                        zi.FirstDemandSidePtr = 0;
                        zi.LastDemandSidePtr = 0;
                    }
                }

                if (CompNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= CompNum; ++i) {
                        auto const &ai(state.dataAirSystemsData->AirSysCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &aj(state.dataAirSystemsData->AirSysCompToPlant(j));
                            if ((ai.AirLoopNum == aj.AirLoopNum) && (ai.AirLoopBranch == aj.AirLoopBranch) &&
                                (ai.AirLoopComp == aj.AirLoopComp)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &aa(state.dataAirSystemsData->AirSysCompToPlant(ArrayCount));
                                aa.AirLoopNum = ai.AirLoopNum;
                                aa.AirLoopBranch = ai.AirLoopBranch;
                                aa.AirLoopComp = ai.AirLoopComp;
                                aa.PlantLoopType = ai.PlantLoopType;
                                aa.PlantLoopNum = ai.PlantLoopNum;
                                aa.PlantLoopBranch = ai.PlantLoopBranch;
                                aa.PlantLoopComp = ai.PlantLoopComp;
                                aa.FirstDemandSidePtr = ai.FirstDemandSidePtr;
                                aa.LastDemandSidePtr = ai.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= CompNum; ++i) { // Zero the now-unused entries
                        auto &ai(state.dataAirSystemsData->AirSysCompToPlant(i));
                        ai.AirLoopNum = 0;
                        ai.AirLoopBranch = 0;
                        ai.AirLoopComp = 0;
                        ai.PlantLoopType = 0;
                        ai.PlantLoopNum = 0;
                        ai.PlantLoopBranch = 0;
                        ai.PlantLoopComp = 0;
                        ai.FirstDemandSidePtr = 0;
                        ai.LastDemandSidePtr = 0;
                    }
                }

                if (SubCompNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= SubCompNum; ++i) {
                        auto const &ai(state.dataAirSystemsData->AirSysSubCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &aj(state.dataAirSystemsData->AirSysSubCompToPlant(j));
                            if ((ai.AirLoopNum == aj.AirLoopNum) && (ai.AirLoopBranch == aj.AirLoopBranch) && (ai.AirLoopComp == aj.AirLoopComp) &&
                                (ai.AirLoopSubComp == aj.AirLoopSubComp)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &aa(state.dataAirSystemsData->AirSysSubCompToPlant(ArrayCount));
                                aa.AirLoopNum = ai.AirLoopNum;
                                aa.AirLoopBranch = ai.AirLoopBranch;
                                aa.AirLoopComp = ai.AirLoopComp;
                                aa.AirLoopSubComp = ai.AirLoopSubComp;
                                aa.PlantLoopType = ai.PlantLoopType;
                                aa.PlantLoopNum = ai.PlantLoopNum;
                                aa.PlantLoopBranch = ai.PlantLoopBranch;
                                aa.PlantLoopComp = ai.PlantLoopComp;
                                aa.FirstDemandSidePtr = ai.FirstDemandSidePtr;
                                aa.LastDemandSidePtr = ai.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= SubCompNum; ++i) { // Zero the now-unused entries
                        auto &ai(state.dataAirSystemsData->AirSysSubCompToPlant(i));
                        ai.AirLoopNum = 0;
                        ai.AirLoopBranch = 0;
                        ai.AirLoopComp = 0;
                        ai.AirLoopSubComp = 0;
                        ai.PlantLoopType = 0;
                        ai.PlantLoopNum = 0;
                        ai.PlantLoopBranch = 0;
                        ai.PlantLoopComp = 0;
                        ai.FirstDemandSidePtr = 0;
                        ai.LastDemandSidePtr = 0;
                    }
                }

                if (SubSubCompNum > 0) {
                    ArrayCount = 0;
                    for (int i = 1; i <= SubCompNum; ++i) {
                        auto const &ai(state.dataAirSystemsData->AirSysSubSubCompToPlant(i));
                        bool duplicate(false);
                        for (int j = 1; j <= ArrayCount; ++j) {
                            auto const &aj(state.dataAirSystemsData->AirSysSubSubCompToPlant(j));
                            if ((ai.AirLoopNum == aj.AirLoopNum) && (ai.AirLoopBranch == aj.AirLoopBranch) && (ai.AirLoopComp == aj.AirLoopComp) &&
                                (ai.AirLoopSubComp == aj.AirLoopSubComp) && (ai.AirLoopSubSubComp == aj.AirLoopSubSubComp)) { // Duplicate
                                duplicate = true;
                                break;
                            }
                        }
                        if (!duplicate) {
                            ++ArrayCount;
                            if (i > ArrayCount) { // Copy to lower position
                                auto &aa(state.dataAirSystemsData->AirSysSubSubCompToPlant(ArrayCount));
                                aa.AirLoopNum = ai.AirLoopNum;
                                aa.AirLoopBranch = ai.AirLoopBranch;
                                aa.AirLoopComp = ai.AirLoopComp;
                                aa.AirLoopSubComp = ai.AirLoopSubComp;
                                aa.AirLoopSubSubComp = ai.AirLoopSubSubComp;
                                aa.PlantLoopType = ai.PlantLoopType;
                                aa.PlantLoopNum = ai.PlantLoopNum;
                                aa.PlantLoopBranch = ai.PlantLoopBranch;
                                aa.PlantLoopComp = ai.PlantLoopComp;
                                aa.FirstDemandSidePtr = ai.FirstDemandSidePtr;
                                aa.LastDemandSidePtr = ai.LastDemandSidePtr;
                            }
                        }
                    }
                    for (int i = ArrayCount + 1; i <= SubCompNum; ++i) { // Zero the now-unused entries
                        auto &ai(state.dataAirSystemsData->AirSysSubSubCompToPlant(i));
                        ai.AirLoopNum = 0;
                        ai.AirLoopBranch = 0;
                        ai.AirLoopComp = 0;
                        ai.AirLoopSubComp = 0;
                        ai.AirLoopSubSubComp = 0;
                        ai.PlantLoopType = 0;
                        ai.PlantLoopNum = 0;
                        ai.PlantLoopBranch = 0;
                        ai.PlantLoopComp = 0;
                        ai.FirstDemandSidePtr = 0;
                        ai.LastDemandSidePtr = 0;
                    }
                }

                // 2. Find Supply Side loop for every demand side component
                // The demand side components only need to know what supply side loop
                // they are connected to.  The input and plant data structure will
                // force the loop numbers to be the same.

                // 3. Find Demand Side Component Corresponding to Supply Side Component
                for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops; ++PlantLoopNum) {
                    for (BranchNum = 1; BranchNum <= state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum).TotalBranches; ++BranchNum) {
                        for (CompNum = 1; CompNum <= state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum).Branch(BranchNum).TotalComponents;
                             ++CompNum) {
                            {
                                auto &thisVentRepComp(state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum).Branch(BranchNum).Comp(CompNum));
                                CompType = thisVentRepComp.TypeOf;
                                CompName = thisVentRepComp.Name;
                                FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                thisVentRepComp.ConnectPlant.LoopType = MatchLoopType;
                                thisVentRepComp.ConnectPlant.LoopNum = MatchLoop;
                                thisVentRepComp.ConnectPlant.BranchNum = MatchBranch;
                                thisVentRepComp.ConnectPlant.CompNum = MatchComp;
                            }
                        }
                    }
                }

                for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumCondLoops; ++PlantLoopNum) {
                    for (BranchNum = 1; BranchNum <= state.dataPlnt->VentRepCondSupplySide(PlantLoopNum).TotalBranches; ++BranchNum) {
                        for (CompNum = 1; CompNum <= state.dataPlnt->VentRepCondSupplySide(PlantLoopNum).Branch(BranchNum).TotalComponents;
                             ++CompNum) {
                            {
                                auto &thisVentRepComp(state.dataPlnt->VentRepCondSupplySide(PlantLoopNum).Branch(BranchNum).Comp(CompNum));
                                CompType = thisVentRepComp.TypeOf;
                                CompName = thisVentRepComp.Name;
                                FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                thisVentRepComp.ConnectPlant.LoopType = MatchLoopType;
                                thisVentRepComp.ConnectPlant.LoopNum = MatchLoop;
                                thisVentRepComp.ConnectPlant.BranchNum = MatchBranch;
                                thisVentRepComp.ConnectPlant.CompNum = MatchComp;
                            }
                        }
                    }
                }
            }
        } // Controlled Zone Loop

        // 4.  Now Load all of the plant supply/demand side connections in a single array with pointers from the
        //    connection arrays (ZoneCompToPlant, ZoneSubCompToPlant, ZoneSubSubCompToPlant, AirSysCompToPlant, etc.)
        if (allocated(state.dataAirSystemsData->ZoneCompToPlant)) {
            NumZoneConnectComps = isize(state.dataAirSystemsData->ZoneCompToPlant);
        } else {
            NumZoneConnectComps = 0;
        }
        if (allocated(state.dataAirSystemsData->ZoneSubCompToPlant)) {
            NumZoneConnectSubComps = isize(state.dataAirSystemsData->ZoneSubCompToPlant);
        } else {
            NumZoneConnectSubComps = 0;
        }
        if (allocated(state.dataAirSystemsData->ZoneSubSubCompToPlant)) {
            NumZoneConnectSubSubComps = isize(state.dataAirSystemsData->ZoneSubSubCompToPlant);
        } else {
            NumZoneConnectSubSubComps = 0;
        }
        if (allocated(state.dataAirSystemsData->AirSysCompToPlant)) {
            NumAirSysConnectComps = isize(state.dataAirSystemsData->AirSysCompToPlant);
        } else {
            NumAirSysConnectComps = 0;
        }
        if (allocated(state.dataAirSystemsData->AirSysSubCompToPlant)) {
            NumAirSysConnectSubComps = isize(state.dataAirSystemsData->AirSysSubCompToPlant);
        } else {
            NumAirSysConnectSubComps = 0;
        }
        if (allocated(state.dataAirSystemsData->AirSysSubSubCompToPlant)) {
            NumAirSysConnectSubSubComps = isize(state.dataAirSystemsData->AirSysSubSubCompToPlant);
        } else {
            NumAirSysConnectSubSubComps = 0;
        }
        state.dataSysRpts->OneTimeFlag_InitEnergyReports = false;

        ArrayCount = 0;
        for (CompNum = 1; CompNum <= NumZoneConnectComps; ++CompNum) {
            LoopType = state.dataAirSystemsData->ZoneCompToPlant(CompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->ZoneCompToPlant(CompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->ZoneCompToPlant(CompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->ZoneCompToPlant(CompNum).LastDemandSidePtr = LastIndex;
            }
        }

        for (SubCompNum = 1; SubCompNum <= NumZoneConnectSubComps; ++SubCompNum) {
            LoopType = state.dataAirSystemsData->ZoneSubCompToPlant(SubCompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->ZoneSubCompToPlant(SubCompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->ZoneSubCompToPlant(SubCompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->ZoneSubCompToPlant(SubCompNum).LastDemandSidePtr = LastIndex;
            }
        }

        for (SubSubCompNum = 1; SubSubCompNum <= NumZoneConnectSubSubComps; ++SubSubCompNum) {
            LoopType = state.dataAirSystemsData->ZoneSubSubCompToPlant(SubSubCompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->ZoneSubSubCompToPlant(SubSubCompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->ZoneSubSubCompToPlant(SubSubCompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->ZoneSubSubCompToPlant(SubSubCompNum).LastDemandSidePtr = LastIndex;
            }
        }
        for (CompNum = 1; CompNum <= NumAirSysConnectComps; ++CompNum) {
            LoopType = state.dataAirSystemsData->AirSysCompToPlant(CompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->AirSysCompToPlant(CompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->AirSysCompToPlant(CompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->AirSysCompToPlant(CompNum).LastDemandSidePtr = LastIndex;
            }
        }

        for (SubCompNum = 1; SubCompNum <= NumAirSysConnectSubComps; ++SubCompNum) {
            LoopType = state.dataAirSystemsData->AirSysSubCompToPlant(SubCompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->AirSysSubCompToPlant(SubCompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->AirSysSubCompToPlant(SubCompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->AirSysSubCompToPlant(SubCompNum).LastDemandSidePtr = LastIndex;
            }
        }

        for (SubSubCompNum = 1; SubSubCompNum <= NumAirSysConnectSubSubComps; ++SubSubCompNum) {
            LoopType = state.dataAirSystemsData->AirSysSubSubCompToPlant(SubSubCompNum).PlantLoopType;
            LoopNum = state.dataAirSystemsData->AirSysSubSubCompToPlant(SubSubCompNum).PlantLoopNum;
            FirstIndex = ArrayCount + 1;
            LoopCount = 1;

            if (LoopType > 0 && LoopNum > 0) {
                FindFirstLastPtr(state, LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag);
            } else {
                ConnectionFlag = false;
            }

            LastIndex = ArrayCount;
            if (FirstIndex > LastIndex) FirstIndex = LastIndex;
            if (ConnectionFlag) {
                state.dataAirSystemsData->AirSysSubSubCompToPlant(SubSubCompNum).FirstDemandSidePtr = FirstIndex;
                state.dataAirSystemsData->AirSysSubSubCompToPlant(SubSubCompNum).LastDemandSidePtr = LastIndex;
            }
        }

        state.dataSysRpts->OneTimeFlag_InitEnergyReports = false;
    }

    // On every iteration, load the air loop energy data
    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        auto &pas = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum);
        for (BranchNum = 1; BranchNum <= pas.NumBranches; ++BranchNum) {
            auto &pasBranch = pas.Branch(BranchNum);
            for (CompNum = 1; CompNum <= pasBranch.TotalComponents; ++CompNum) {
                auto &pasBranchComp = pasBranch.Comp(CompNum);
                for (VarNum = 1; VarNum <= pasBranchComp.NumMeteredVars; ++VarNum) {
                    auto &pasBranchCompMeter = pasBranchComp.MeteredVar(VarNum);
                    VarType = pasBranchCompMeter.ReportVarType;
                    VarIndex = pasBranchCompMeter.ReportVarIndex;
                    pasBranchCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
                for (SubCompNum = 1; SubCompNum <= pasBranchComp.NumSubComps; ++SubCompNum) {
                    auto &pasBranchSubComp = pasBranchComp.SubComp(SubCompNum);
                    for (VarNum = 1; VarNum <= pasBranchSubComp.NumMeteredVars; ++VarNum) {
                        auto &pasBranchSubCompMeter = pasBranchSubComp.MeteredVar(VarNum);
                        VarType = pasBranchSubCompMeter.ReportVarType;
                        VarIndex = pasBranchSubCompMeter.ReportVarIndex;
                        pasBranchSubCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                    }
                    for (SubSubCompNum = 1; SubSubCompNum <= pasBranchSubComp.NumSubSubComps; ++SubSubCompNum) {
                        auto &pasBranchSubSubComp = pasBranchSubComp.SubSubComp(SubSubCompNum);
                        for (VarNum = 1; VarNum <= pasBranchSubSubComp.NumMeteredVars; ++VarNum) {
                            auto &pasBranchSubSubCompMeter = pasBranchSubSubComp.MeteredVar(VarNum);
                            VarType = pasBranchSubSubCompMeter.ReportVarType;
                            VarIndex = pasBranchSubSubCompMeter.ReportVarIndex;
                            pasBranchSubSubCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                        }
                    }
                }
            }
        }
    }

    // On every iteration, load the zone equipment energy data
    for (ListNum = 1; ListNum <= state.dataGlobal->NumOfZones; ++ListNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ListNum).IsControlled) continue;
        auto &zel = state.dataZoneEquip->ZoneEquipList(ListNum);
        for (CompNum = 1; CompNum <= zel.NumOfEquipTypes; ++CompNum) {
            auto &zelEquipData = zel.EquipData(CompNum);
            for (VarNum = 1; VarNum <= zelEquipData.NumMeteredVars; ++VarNum) {
                auto &zelEquipDataMeter = zelEquipData.MeteredVar(VarNum);
                VarType = zelEquipDataMeter.ReportVarType;
                VarIndex = zelEquipDataMeter.ReportVarIndex;
                zelEquipDataMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
            }
            for (SubCompNum = 1; SubCompNum <= zelEquipData.NumSubEquip; ++SubCompNum) {
                auto &zelSubEquipData = zelEquipData.SubEquipData(SubCompNum);
                for (VarNum = 1; VarNum <= zelSubEquipData.NumMeteredVars; ++VarNum) {
                    auto &zelSubEquipDataMeter = zelSubEquipData.MeteredVar(VarNum);
                    VarType = zelSubEquipDataMeter.ReportVarType;
                    VarIndex = zelSubEquipDataMeter.ReportVarIndex;
                    zelSubEquipDataMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
                for (SubSubCompNum = 1; SubSubCompNum <= zelSubEquipData.NumSubSubEquip; ++SubSubCompNum) {
                    auto &zelSubSubEquipData = zelSubEquipData.SubSubEquipData(SubSubCompNum);
                    for (VarNum = 1; VarNum <= zelSubSubEquipData.NumMeteredVars; ++VarNum) {
                        auto &zelSubSubEquipDataMeter = zelSubSubEquipData.MeteredVar(VarNum);
                        VarType = zelSubSubEquipDataMeter.ReportVarType;
                        VarIndex = zelSubSubEquipDataMeter.ReportVarIndex;
                        zelSubSubEquipDataMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex); // Sankar Corrected zone array
                    }
                }
            }
        }
    }

    // On every iteration, load the Plant Supply Side Data and load the Plant Demand Side Data
    for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops; ++PlantLoopNum) {
        auto &vrpss = state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum);
        for (BranchNum = 1; BranchNum <= vrpss.TotalBranches; ++BranchNum) {
            auto &vrpssBranch = vrpss.Branch(BranchNum);
            for (CompNum = 1; CompNum <= vrpssBranch.TotalComponents; ++CompNum) {
                auto &vrpssBranchComp = vrpssBranch.Comp(CompNum);
                for (VarNum = 1; VarNum <= vrpssBranchComp.NumMeteredVars; ++VarNum) {
                    auto &vrpssBranchCompMeter = vrpssBranchComp.MeteredVar(VarNum);
                    VarType = vrpssBranchCompMeter.ReportVarType;
                    VarIndex = vrpssBranchCompMeter.ReportVarIndex;
                    vrpssBranchCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
            }
        }
        auto &vrpds = state.dataPlnt->VentRepPlantDemandSide(PlantLoopNum);
        for (BranchNum = 1; BranchNum <= vrpds.TotalBranches; ++BranchNum) {
            auto &vrpdsBranch = vrpds.Branch(BranchNum);
            for (CompNum = 1; CompNum <= vrpdsBranch.TotalComponents; ++CompNum) {
                auto &vrpdsBranchComp = vrpdsBranch.Comp(CompNum);
                for (VarNum = 1; VarNum <= vrpdsBranchComp.NumMeteredVars; ++VarNum) {
                    auto &vrpdsBranchCompMeter = vrpdsBranchComp.MeteredVar(VarNum);
                    VarType = vrpdsBranchCompMeter.ReportVarType;
                    VarIndex = vrpdsBranchCompMeter.ReportVarIndex;
                    vrpdsBranchCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
            }
        }
    }

    // On every iteration, load the Condenser Supply Side Data and load the Condenser Demand Side Data
    for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumCondLoops; ++PlantLoopNum) {
        auto &vrcss = state.dataPlnt->VentRepCondSupplySide(PlantLoopNum);
        for (BranchNum = 1; BranchNum <= vrcss.TotalBranches; ++BranchNum) {
            auto &vrcssBranch = vrcss.Branch(BranchNum);
            for (CompNum = 1; CompNum <= vrcssBranch.TotalComponents; ++CompNum) {
                auto &vrcssBranchComp = vrcssBranch.Comp(CompNum);
                for (VarNum = 1; VarNum <= vrcssBranchComp.NumMeteredVars; ++VarNum) {
                    auto &vrcssBranchCompMeter = vrcssBranchComp.MeteredVar(VarNum);
                    VarType = vrcssBranchCompMeter.ReportVarType;
                    VarIndex = vrcssBranchCompMeter.ReportVarIndex;
                    vrcssBranchCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
            }
        }
        auto &vrcds = state.dataPlnt->VentRepCondSupplySide(PlantLoopNum);
        for (BranchNum = 1; BranchNum <= vrcds.TotalBranches; ++BranchNum) {
            auto &vrcdsBranch = vrcds.Branch(BranchNum);
            for (CompNum = 1; CompNum <= vrcdsBranch.TotalComponents; ++CompNum) {
                auto &vrcdsBranchComp = vrcdsBranch.Comp(CompNum);
                for (VarNum = 1; VarNum <= vrcdsBranchComp.NumMeteredVars; ++VarNum) {
                    auto &vrcdsBranchCompMeter = vrcdsBranchComp.MeteredVar(VarNum);
                    VarType = vrcdsBranchCompMeter.ReportVarType;
                    VarIndex = vrcdsBranchCompMeter.ReportVarIndex;
                    vrcdsBranchCompMeter.CurMeterReading = GetInternalVariableValue(state, VarType, VarIndex);
                }
            }
        }
    }

    // initialize energy report variables
}

void FindFirstLastPtr(EnergyPlusData &state, int &LoopType, int &LoopNum, int &ArrayCount, int &LoopCount, bool &ConnectionFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   July 2005
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initializes the energy components of the data structures

    // METHODOLOGY EMPLOYED:
    // Once all compsets have been established (second iteration) find all components
    // subcomponents, etc.

    int DemandSideLoopNum;
    int DemandSideBranchNum;
    int DemandSideCompNum;
    int SupplySideCompNum;
    int DemandSideLoopType;
    bool found;

    // Object Data
    auto &LoopStack = state.dataSysRpts->LoopStack;

    return; // Autodesk:? Is this routine now an intentional NOOP?

    if (state.dataSysRpts->OneTimeFlag_FindFirstLastPtr) {
        LoopStack.allocate(state.dataSysRpts->MaxLoopArraySize);
        state.dataAirSystemsData->DemandSideConnect.allocate(state.dataSysRpts->MaxCompArraySize);

        state.dataSysRpts->OneTimeFlag_FindFirstLastPtr = false;
    }
    for (auto &e : LoopStack) {
        e.LoopNum = 0;
        e.LoopType = 0;
    }

    ConnectionFlag = false;
    //    countloop=0
    //    write(outputfiledebug,*) '1228=lt,lc,lnum,cflag,arrcnt',looptype,loopcount,LoopNum,connectionflag,arraycount

    while (LoopCount > 0) {
        //        write(outputfiledebug,*) '1231==lt,lc,lnum,cflag,arrcnt',looptype,loopcount,LoopNum,connectionflag,arraycount
        //        write(outputfiledebug,*) 'loop=plname',TRIM(plantloop(LoopNum)%name)
        --LoopCount;
        //        countloop=countloop+1
        //        if (countloop > 100) exit
        if (LoopType == 1) {
            for (int BranchNum = 1; BranchNum <= state.dataPlnt->VentRepPlantSupplySide(LoopNum).TotalBranches; ++BranchNum) {
                for (int SupplySideCompNum = 1;
                     SupplySideCompNum <= state.dataPlnt->VentRepPlantSupplySide(LoopNum).Branch(BranchNum).TotalComponents;
                     ++SupplySideCompNum) {
                    {
                        auto &thisVentRepComp(state.dataPlnt->VentRepPlantSupplySide(LoopNum).Branch(BranchNum).Comp(SupplySideCompNum));
                        DemandSideLoopType = thisVentRepComp.ConnectPlant.LoopType;
                        DemandSideLoopNum = thisVentRepComp.ConnectPlant.LoopNum;
                        DemandSideBranchNum = thisVentRepComp.ConnectPlant.BranchNum;
                        DemandSideCompNum = thisVentRepComp.ConnectPlant.CompNum;
                    }
                    // If the connection is valid load the connection array
                    if (DemandSideLoopType == 1 || DemandSideLoopType == 2) {
                        ConnectionFlag = true;
                        ++ArrayCount;
                        if (ArrayCount > state.dataSysRpts->MaxCompArraySize) {
                            state.dataAirSystemsData->DemandSideConnect.redimension(state.dataSysRpts->MaxCompArraySize += 100);
                        }
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).LoopType = DemandSideLoopType;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).LoopNum = DemandSideLoopNum;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).BranchNum = DemandSideBranchNum;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).CompNum = DemandSideCompNum;

                        found = false;
                        print(state.files.debug, "1271=lstacksize {}\n", size(LoopStack));
                        for (int Idx = 1; Idx <= isize(LoopStack); ++Idx) {
                            if (DemandSideLoopNum == LoopStack(Idx).LoopNum && DemandSideLoopType == LoopStack(Idx).LoopType) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            ++LoopCount;
                            //       write(outputfiledebug,*) '1280=lc,mxsize',loopcount,maxlooparraysize
                            //       write(outputfiledebug,*) '1281=dsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
                            if (LoopCount > state.dataSysRpts->MaxLoopArraySize) {
                                LoopStack.redimension(state.dataSysRpts->MaxLoopArraySize += 100);
                            }
                            //               write(outputfiledebug,*)
                            //               '1294=lcnt,dsloopnum,dslooptype',loopcount,DemandSideLoopNum,DemandSideLoopType
                            LoopStack(LoopCount).LoopNum = DemandSideLoopNum;
                            LoopStack(LoopCount).LoopType = DemandSideLoopType;
                        }
                    }
                }
            }
        } else if (LoopType == 2) {
            for (int BranchNum = 1; BranchNum <= state.dataPlnt->VentRepCondSupplySide(LoopNum).TotalBranches; ++BranchNum) {
                for (SupplySideCompNum = 1; SupplySideCompNum <= state.dataPlnt->VentRepCondSupplySide(LoopNum).Branch(BranchNum).TotalComponents;
                     ++SupplySideCompNum) {
                    {
                        auto &thisVentRepComp(state.dataPlnt->VentRepCondSupplySide(LoopNum).Branch(BranchNum).Comp(SupplySideCompNum));
                        DemandSideLoopType = thisVentRepComp.ConnectPlant.LoopType;
                        DemandSideLoopNum = thisVentRepComp.ConnectPlant.LoopNum;
                        DemandSideBranchNum = thisVentRepComp.ConnectPlant.BranchNum;
                        DemandSideCompNum = thisVentRepComp.ConnectPlant.CompNum;
                    }
                    // If the connection is valid load the connection array
                    if (DemandSideLoopType == 1 || DemandSideLoopType == 2) {
                        ConnectionFlag = true;
                        ++ArrayCount;
                        if (ArrayCount > state.dataSysRpts->MaxCompArraySize) {
                            state.dataAirSystemsData->DemandSideConnect.redimension(state.dataSysRpts->MaxCompArraySize += 100);
                        }
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).LoopType = DemandSideLoopType;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).LoopNum = DemandSideLoopNum;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).BranchNum = DemandSideBranchNum;
                        state.dataAirSystemsData->DemandSideConnect(ArrayCount).CompNum = DemandSideCompNum;

                        found = false;
                        for (int Idx = 1; Idx <= isize(LoopStack); ++Idx) {
                            if (DemandSideLoopNum == LoopStack(Idx).LoopNum && DemandSideLoopType == LoopStack(Idx).LoopType) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            ++LoopCount;
                            //       write(outputfiledebug,*) '1341=lcnt,arrsize',loopcount,maxlooparraysize
                            //       write(outputfiledebug,*) '1342=lsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
                            if (LoopCount > state.dataSysRpts->MaxLoopArraySize) {
                                LoopStack.redimension(state.dataSysRpts->MaxLoopArraySize += 100);
                            }
                            LoopStack(LoopCount).LoopNum = DemandSideLoopNum;
                            LoopStack(LoopCount).LoopType = DemandSideLoopType;
                        }
                    }
                }
            }
        } else {
            print(state.files.debug, "{}\n", "1361=error");
            // error
        }

        // now unload the LoopNum and LoopType arrays
        if (LoopCount > 0) {
            LoopType = LoopStack(LoopCount).LoopType;
            LoopNum = LoopStack(LoopCount).LoopNum;
        }

    } // While loop
}

void UpdateZoneCompPtrArray(EnergyPlusData &state,
                            int &Idx,
                            int const ListNum,
                            int const AirDistUnitNum,
                            int const PlantLoopType,
                            int const PlantLoop,
                            int const PlantBranch,
                            int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Zone Component pointers

    if (state.dataSysRpts->OneTimeFlag_UpdateZoneCompPtrArray) {
        state.dataAirSystemsData->ZoneCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateZoneCompPtrArray);
        for (auto &e : state.dataAirSystemsData->ZoneCompToPlant) {
            e.ZoneEqListNum = 0;
            e.ZoneEqCompNum = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateZoneCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateZoneCompPtrArray >= state.dataSysRpts->ArrayLimit_UpdateZoneCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateZoneCompPtrArray);
        state.dataAirSystemsData->ZoneCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateZoneCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateZoneCompPtrArray; ++i) {
            auto &zctp(state.dataAirSystemsData->ZoneCompToPlant(i));
            zctp.ZoneEqListNum = 0;
            zctp.ZoneEqCompNum = 0;
            zctp.PlantLoopType = 0;
            zctp.PlantLoopNum = 0;
            zctp.PlantLoopBranch = 0;
            zctp.PlantLoopComp = 0;
            zctp.FirstDemandSidePtr = 0;
            zctp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateZoneCompPtrArray;
    auto &zctp(state.dataAirSystemsData->ZoneCompToPlant(Idx));
    zctp.ZoneEqListNum = ListNum;
    zctp.ZoneEqCompNum = AirDistUnitNum;
    zctp.PlantLoopType = PlantLoopType;
    zctp.PlantLoopNum = PlantLoop;
    zctp.PlantLoopBranch = PlantBranch;
    zctp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateZoneCompPtrArray;
}

void UpdateZoneSubCompPtrArray(EnergyPlusData &state,
                               int &Idx,
                               int const ListNum,
                               int const AirDistUnitNum,
                               int const SubCompNum,
                               int const PlantLoopType,
                               int const PlantLoop,
                               int const PlantBranch,
                               int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Zone Sub Component Pointer Array

    if (state.dataSysRpts->OneTimeFlag_UpdateZoneSubCompPtrArray) {
        state.dataAirSystemsData->ZoneSubCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateZoneSubCompPtrArray);
        for (auto &e : state.dataAirSystemsData->ZoneSubCompToPlant) {
            e.ZoneEqListNum = 0;
            e.ZoneEqCompNum = 0;
            e.ZoneEqSubCompNum = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateZoneSubCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateZoneSubCompPtrArray >= state.dataSysRpts->ArrayLimit_UpdateZoneSubCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateZoneSubCompPtrArray);
        state.dataAirSystemsData->ZoneSubCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateZoneSubCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateZoneSubCompPtrArray; ++i) {
            auto &zctp(state.dataAirSystemsData->ZoneSubCompToPlant(i));
            zctp.ZoneEqListNum = 0;
            zctp.ZoneEqCompNum = 0;
            zctp.ZoneEqSubCompNum = 0;
            zctp.PlantLoopType = 0;
            zctp.PlantLoopNum = 0;
            zctp.PlantLoopBranch = 0;
            zctp.PlantLoopComp = 0;
            zctp.FirstDemandSidePtr = 0;
            zctp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateZoneSubCompPtrArray;
    auto &zctp(state.dataAirSystemsData->ZoneSubCompToPlant(Idx));
    zctp.ZoneEqListNum = ListNum;
    zctp.ZoneEqCompNum = AirDistUnitNum;
    zctp.ZoneEqSubCompNum = SubCompNum;
    zctp.PlantLoopType = PlantLoopType;
    zctp.PlantLoopNum = PlantLoop;
    zctp.PlantLoopBranch = PlantBranch;
    zctp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateZoneSubCompPtrArray;
}

void UpdateZoneSubSubCompPtrArray(EnergyPlusData &state,
                                  int &Idx,
                                  int const ListNum,
                                  int const AirDistUnitNum,
                                  int const SubCompNum,
                                  int const SubSubCompNum,
                                  int const PlantLoopType,
                                  int const PlantLoop,
                                  int const PlantBranch,
                                  int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Zone Sub Component Pointer Array

    if (state.dataSysRpts->OneTimeFlag_UpdateZoneSubSubCompPtrArray) {
        state.dataAirSystemsData->ZoneSubSubCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateZoneSubSubCompPtrArray);
        for (auto &e : state.dataAirSystemsData->ZoneSubSubCompToPlant) {
            e.ZoneEqListNum = 0;
            e.ZoneEqCompNum = 0;
            e.ZoneEqSubCompNum = 0;
            e.ZoneEqSubSubCompNum = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateZoneSubSubCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateZoneSubSubCompPtrArray >=
        state.dataSysRpts->ArrayLimit_UpdateZoneSubSubCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateZoneSubSubCompPtrArray);
        state.dataAirSystemsData->ZoneSubSubCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateZoneSubSubCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateZoneSubSubCompPtrArray; ++i) {
            auto &zctp(state.dataAirSystemsData->ZoneSubSubCompToPlant(i));
            zctp.ZoneEqListNum = 0;
            zctp.ZoneEqCompNum = 0;
            zctp.ZoneEqSubCompNum = 0;
            zctp.ZoneEqSubSubCompNum = 0;
            zctp.PlantLoopType = 0;
            zctp.PlantLoopNum = 0;
            zctp.PlantLoopBranch = 0;
            zctp.PlantLoopComp = 0;
            zctp.FirstDemandSidePtr = 0;
            zctp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateZoneSubSubCompPtrArray;
    auto &zctp(state.dataAirSystemsData->ZoneSubSubCompToPlant(Idx));
    zctp.ZoneEqListNum = ListNum;
    zctp.ZoneEqCompNum = AirDistUnitNum;
    zctp.ZoneEqSubCompNum = SubCompNum;
    zctp.ZoneEqSubSubCompNum = SubSubCompNum;
    zctp.PlantLoopType = PlantLoopType;
    zctp.PlantLoopNum = PlantLoop;
    zctp.PlantLoopBranch = PlantBranch;
    zctp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateZoneSubSubCompPtrArray;
}

void UpdateAirSysCompPtrArray(EnergyPlusData &state,
                              int &Idx,
                              int const AirLoopNum,
                              int const BranchNum,
                              int const CompNum,
                              int const PlantLoopType,
                              int const PlantLoop,
                              int const PlantBranch,
                              int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Air System Component Pointer Array

    if (state.dataSysRpts->OneTimeFlag_UpdateAirSysCompPtrArray) {
        state.dataAirSystemsData->AirSysCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateAirSysCompPtrArray);
        for (auto &e : state.dataAirSystemsData->AirSysCompToPlant) {
            e.AirLoopNum = 0;
            e.AirLoopBranch = 0;
            e.AirLoopComp = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateAirSysCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateAirSysCompPtrArray >= state.dataSysRpts->ArrayLimit_UpdateAirSysCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateAirSysCompPtrArray);
        state.dataAirSystemsData->AirSysCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateAirSysCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateAirSysCompPtrArray; ++i) {
            auto &actp(state.dataAirSystemsData->AirSysCompToPlant(i));
            actp.AirLoopNum = 0;
            actp.AirLoopBranch = 0;
            actp.AirLoopComp = 0;
            actp.PlantLoopType = 0;
            actp.PlantLoopNum = 0;
            actp.PlantLoopBranch = 0;
            actp.PlantLoopComp = 0;
            actp.FirstDemandSidePtr = 0;
            actp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateAirSysCompPtrArray;
    auto &actp(state.dataAirSystemsData->AirSysCompToPlant(Idx));
    actp.AirLoopNum = AirLoopNum;
    actp.AirLoopBranch = BranchNum;
    actp.AirLoopComp = CompNum;
    actp.PlantLoopType = PlantLoopType;
    actp.PlantLoopNum = PlantLoop;
    actp.PlantLoopBranch = PlantBranch;
    actp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateAirSysCompPtrArray;
}

void UpdateAirSysSubCompPtrArray(EnergyPlusData &state,
                                 int &Idx,
                                 int const AirLoopNum,
                                 int const BranchNum,
                                 int const CompNum,
                                 int const SubCompNum,
                                 int const PlantLoopType,
                                 int const PlantLoop,
                                 int const PlantBranch,
                                 int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Air System Sub Component Pointer Array

    if (state.dataSysRpts->OneTimeFlag_UpdateAirSysSubCompPtrArray) {
        state.dataAirSystemsData->AirSysSubCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateAirSysSubCompPtrArray);
        for (auto &e : state.dataAirSystemsData->AirSysSubCompToPlant) {
            e.AirLoopNum = 0;
            e.AirLoopBranch = 0;
            e.AirLoopComp = 0;
            e.AirLoopSubComp = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateAirSysSubCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateAirSysSubCompPtrArray >=
        state.dataSysRpts->ArrayLimit_UpdateAirSysSubCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateAirSysSubCompPtrArray);
        state.dataAirSystemsData->AirSysSubCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateAirSysSubCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateAirSysSubCompPtrArray; ++i) {
            auto &actp(state.dataAirSystemsData->AirSysSubCompToPlant(i));
            actp.AirLoopNum = 0;
            actp.AirLoopBranch = 0;
            actp.AirLoopComp = 0;
            actp.AirLoopSubComp = 0;
            actp.PlantLoopType = 0;
            actp.PlantLoopNum = 0;
            actp.PlantLoopBranch = 0;
            actp.PlantLoopComp = 0;
            actp.FirstDemandSidePtr = 0;
            actp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateAirSysSubCompPtrArray;
    auto &actp(state.dataAirSystemsData->AirSysSubCompToPlant(Idx));
    actp.AirLoopNum = AirLoopNum;
    actp.AirLoopBranch = BranchNum;
    actp.AirLoopComp = CompNum;
    actp.AirLoopSubComp = SubCompNum;
    actp.PlantLoopType = PlantLoopType;
    actp.PlantLoopNum = PlantLoop;
    actp.PlantLoopBranch = PlantBranch;
    actp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateAirSysSubCompPtrArray;
}

void UpdateAirSysSubSubCompPtrArray(EnergyPlusData &state,
                                    int &Idx,
                                    int const AirLoopNum,
                                    int const BranchNum,
                                    int const CompNum,
                                    int const SubCompNum,
                                    int const SubSubCompNum,
                                    int const PlantLoopType,
                                    int const PlantLoop,
                                    int const PlantBranch,
                                    int const PlantComp)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update Air System Sub Sub Component Pointer Array

    if (state.dataSysRpts->OneTimeFlag_UpdateAirSysSubSubCompPtrArray) {
        state.dataAirSystemsData->AirSysSubSubCompToPlant.allocate(state.dataSysRpts->ArrayLimit_UpdateAirSysSubSubCompPtrArray);
        for (auto &e : state.dataAirSystemsData->AirSysSubSubCompToPlant) {
            e.AirLoopNum = 0;
            e.AirLoopBranch = 0;
            e.AirLoopComp = 0;
            e.AirLoopSubComp = 0;
            e.AirLoopSubSubComp = 0;
            e.PlantLoopType = 0;
            e.PlantLoopNum = 0;
            e.PlantLoopBranch = 0;
            e.PlantLoopComp = 0;
            e.FirstDemandSidePtr = 0;
            e.LastDemandSidePtr = 0;
        }

        state.dataSysRpts->OneTimeFlag_UpdateAirSysSubSubCompPtrArray = false;
    }

    if (state.dataSysRpts->ArrayCounter_UpdateAirSysSubSubCompPtrArray >=
        state.dataSysRpts->ArrayLimit_UpdateAirSysSubSubCompPtrArray) { // Redimension larger
        int const OldArrayLimit(state.dataSysRpts->ArrayLimit_UpdateAirSysSubSubCompPtrArray);
        state.dataAirSystemsData->AirSysSubSubCompToPlant.redimension(state.dataSysRpts->ArrayLimit_UpdateAirSysSubSubCompPtrArray *= 2);
        for (int i = OldArrayLimit + 1; i <= state.dataSysRpts->ArrayLimit_UpdateAirSysSubSubCompPtrArray; ++i) {
            auto &actp(state.dataAirSystemsData->AirSysSubSubCompToPlant(i));
            actp.AirLoopNum = 0;
            actp.AirLoopBranch = 0;
            actp.AirLoopComp = 0;
            actp.AirLoopSubComp = 0;
            actp.AirLoopSubSubComp = 0;
            actp.PlantLoopType = 0;
            actp.PlantLoopNum = 0;
            actp.PlantLoopBranch = 0;
            actp.PlantLoopComp = 0;
            actp.FirstDemandSidePtr = 0;
            actp.LastDemandSidePtr = 0;
        }
    }

    Idx = state.dataSysRpts->ArrayCounter_UpdateAirSysSubSubCompPtrArray;
    auto &actp(state.dataAirSystemsData->AirSysSubSubCompToPlant(Idx));
    actp.AirLoopNum = AirLoopNum;
    actp.AirLoopBranch = BranchNum;
    actp.AirLoopComp = CompNum;
    actp.AirLoopSubComp = SubCompNum;
    actp.AirLoopSubSubComp = SubSubCompNum;
    actp.PlantLoopType = PlantLoopType;
    actp.PlantLoopNum = PlantLoop;
    actp.PlantLoopBranch = PlantBranch;
    actp.PlantLoopComp = PlantComp;
    ++state.dataSysRpts->ArrayCounter_UpdateAirSysSubSubCompPtrArray;
}

void AllocateAndSetUpVentReports(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // PURPOSE OF THIS SUBROUTINE:
    // Allocates Arrays and setup output variables related to Ventilation reports.
    state.dataSysRpts->MaxCoolingLoadMetByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxCoolingLoadAddedByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxOvercoolingByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxHeatingLoadMetByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxHeatingLoadAddedByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxOverheatingByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxNoLoadHeatingByVent.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->MaxNoLoadCoolingByVent.allocate(state.dataGlobal->NumOfZones);

    state.dataSysRpts->ZoneOAMassFlow.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneOAMass.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneOAVolFlowStdRho.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneOAVolStdRho.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneOAVolFlowCrntRho.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneOAVolCrntRho.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneMechACH.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneTargetVentilationFlowVoz.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneTimeBelowVozDyn.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneTimeAtVozDyn.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneTimeAboveVozDyn.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->ZoneTimeVentUnocc.allocate(state.dataGlobal->NumOfZones);

    state.dataSysRpts->SysMechVentFlow.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysNatVentFlow.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTargetVentilationFlowVoz.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTimeBelowVozDyn.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTimeAtVozDyn.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTimeAboveVozDyn.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTimeVentUnocc.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysAnyZoneOccupied.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysPreDefRep.allocate(NumPrimaryAirSys);

    state.dataSysRpts->SysTotZoneLoadHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotZoneLoadCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysOALoadHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysOALoadCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotCLNG.allocate(NumPrimaryAirSys);

    state.dataSysRpts->SysTotElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotNaturalGas.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotPropane.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotSteam.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotH2OCOLD.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysTotH2OHOT.allocate(NumPrimaryAirSys);

    state.dataSysRpts->SysHumidHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHumidElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHumidNaturalGas.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHumidPropane.allocate(NumPrimaryAirSys);
    state.dataSysRpts->DesDehumidCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->DesDehumidElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysEvapCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysEvapElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHeatExHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHeatExCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysSolarCollectHeating.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysSolarCollectCooling.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysUserDefinedTerminalHeating.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysUserDefinedTerminalCooling.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysFANCompHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysFANCompElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysCCCompCLNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysCCCompH2OCOLD.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysCCCompElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompH2OHOT.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompElec.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompElecRes.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompHTNG.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompNaturalGas.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompPropane.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysHCCompSteam.allocate(NumPrimaryAirSys);
    state.dataSysRpts->SysDomesticH2O.allocate(NumPrimaryAirSys);

    state.dataSysRpts->SetBackCounter.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->HeatCoolFlag.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->LastHeatCoolFlag.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->FirstHeatCoolFlag.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->LastHeatCoolHour.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->FirstHeatCoolHour.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->NoLoadFlag.allocate(state.dataGlobal->NumOfZones);
    state.dataSysRpts->UnmetLoadFlag.allocate(state.dataGlobal->NumOfZones);

    for (int ZoneIndex = 1; ZoneIndex <= state.dataGlobal->NumOfZones; ++ZoneIndex) {
        state.dataSysRpts->UnmetLoadFlag(ZoneIndex) = false;
        state.dataSysRpts->SetBackCounter(ZoneIndex) = 0;
        state.dataSysRpts->HeatCoolFlag(ZoneIndex) = 0;
        state.dataSysRpts->LastHeatCoolFlag(ZoneIndex) = 0;
        state.dataSysRpts->FirstHeatCoolFlag(ZoneIndex) = 0;
        state.dataSysRpts->LastHeatCoolHour(ZoneIndex) = 0;
        state.dataSysRpts->FirstHeatCoolHour(ZoneIndex) = 0;
        state.dataSysRpts->NoLoadFlag(ZoneIndex) = false;

        state.dataSysRpts->MaxCoolingLoadMetByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxCoolingLoadAddedByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxOvercoolingByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxHeatingLoadMetByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxHeatingLoadAddedByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxOverheatingByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxNoLoadHeatingByVent(ZoneIndex) = 0.0;
        state.dataSysRpts->MaxNoLoadCoolingByVent(ZoneIndex) = 0.0;

        state.dataSysRpts->ZoneOAMassFlow(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneOAMass(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneOAVolFlowStdRho(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneOAVolStdRho(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneOAVolFlowCrntRho(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneOAVolCrntRho(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneMechACH(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneTargetVentilationFlowVoz(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneTimeBelowVozDyn(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneTimeAtVozDyn(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneTimeAboveVozDyn(ZoneIndex) = 0.0;
        state.dataSysRpts->ZoneTimeVentUnocc(ZoneIndex) = 0.0;
    }

    for (int SysIndex = 1; SysIndex <= NumPrimaryAirSys; ++SysIndex) {
        state.dataSysRpts->SysMechVentFlow(SysIndex) = 0.0;
        state.dataSysRpts->SysNatVentFlow(SysIndex) = 0.0;
        state.dataSysRpts->SysTargetVentilationFlowVoz(SysIndex) = 0.0;
        state.dataSysRpts->SysTimeBelowVozDyn(SysIndex) = 0.0;
        state.dataSysRpts->SysTimeAtVozDyn(SysIndex) = 0.0;
        state.dataSysRpts->SysTimeAboveVozDyn(SysIndex) = 0.0;
        state.dataSysRpts->SysTimeVentUnocc(SysIndex) = 0.0;
        state.dataSysRpts->SysAnyZoneOccupied(SysIndex) = false;

        // SYSTEM LOADS REPORT
        state.dataSysRpts->SysTotZoneLoadHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysTotZoneLoadCLNG(SysIndex) = 0.0;
        state.dataSysRpts->SysOALoadHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysOALoadCLNG(SysIndex) = 0.0;
        state.dataSysRpts->SysTotHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysTotCLNG(SysIndex) = 0.0;

        // SYSTEM ENERGY USE REPORT
        state.dataSysRpts->SysTotElec(SysIndex) = 0.0;
        state.dataSysRpts->SysTotNaturalGas(SysIndex) = 0.0;
        state.dataSysRpts->SysTotPropane(SysIndex) = 0.0;
        state.dataSysRpts->SysTotSteam(SysIndex) = 0.0;
        state.dataSysRpts->SysTotH2OCOLD(SysIndex) = 0.0;
        state.dataSysRpts->SysTotH2OHOT(SysIndex) = 0.0;

        // SYSTEM COMPONENT LOADS REPORT
        state.dataSysRpts->SysFANCompHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysCCCompCLNG(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysHeatExHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysHeatExCLNG(SysIndex) = 0.0;
        state.dataSysRpts->SysSolarCollectHeating(SysIndex) = 0.0;
        state.dataSysRpts->SysSolarCollectCooling(SysIndex) = 0.0;
        state.dataSysRpts->SysUserDefinedTerminalHeating(SysIndex) = 0.0;
        state.dataSysRpts->SysUserDefinedTerminalCooling(SysIndex) = 0.0;
        state.dataSysRpts->SysHumidHTNG(SysIndex) = 0.0;
        state.dataSysRpts->SysEvapCLNG(SysIndex) = 0.0;
        state.dataSysRpts->DesDehumidCLNG(SysIndex) = 0.0;
        state.dataSysRpts->SysDomesticH2O(SysIndex) = 0.0;

        // SYSTEM COMPONENT ENERGY REPORT
        state.dataSysRpts->SysFANCompElec(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompH2OHOT(SysIndex) = 0.0;
        state.dataSysRpts->SysCCCompH2OCOLD(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompElec(SysIndex) = 0.0;
        state.dataSysRpts->SysCCCompElec(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompElecRes(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompNaturalGas(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompPropane(SysIndex) = 0.0;
        state.dataSysRpts->SysHCCompSteam(SysIndex) = 0.0;
        state.dataSysRpts->SysHumidElec(SysIndex) = 0.0;
        state.dataSysRpts->SysHumidNaturalGas(SysIndex) = 0.0;
        state.dataSysRpts->SysHumidPropane(SysIndex) = 0.0;
        state.dataSysRpts->DesDehumidElec(SysIndex) = 0.0;
        state.dataSysRpts->SysEvapElec(SysIndex) = 0.0;
    }

    if (state.dataSysRpts->AirLoopLoadsReportEnabled) {
        for (int SysIndex = 1; SysIndex <= NumPrimaryAirSys; ++SysIndex) {

            // CurrentModuleObject='AirloopHVAC'
            // SYSTEM LOADS REPORT
            SetupOutputVariable(state,
                                "Air System Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotHTNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotCLNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            // SYSTEM ENERGY USE REPORT
            SetupOutputVariable(state,
                                "Air System Hot Water Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotH2OHOT(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Steam Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotSteam(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Chilled Water Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotH2OCOLD(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System NaturalGas Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotNaturalGas(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Propane Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysTotPropane(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Water Volume",
                                OutputProcessor::Unit::m3,
                                state.dataSysRpts->SysDomesticH2O(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            // SYSTEM COMPONENT LOAD REPORT
            SetupOutputVariable(state,
                                "Air System Fan Air Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysFANCompHTNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysCCCompCLNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompHTNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heat Exchanger Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHeatExHTNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heat Exchanger Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHeatExCLNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Solar Collector Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysSolarCollectHeating(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Solar Collector Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysSolarCollectCooling(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System User Defined Air Terminal Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysUserDefinedTerminalHeating(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System User Defined Air Terminal Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysUserDefinedTerminalCooling(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Humidifier Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHumidHTNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Evaporative Cooler Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysEvapCLNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Desiccant Dehumidifier Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->DesDehumidCLNG(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            // SYSTEM COMPONENT ENERGY REPORT
            SetupOutputVariable(state,
                                "Air System Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysFANCompElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil Hot Water Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompH2OHOT(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Cooling Coil Chilled Water Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysCCCompH2OCOLD(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System DX Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System DX Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysCCCompElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompElecRes(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil NaturalGas Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompNaturalGas(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil Propane Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompPropane(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Heating Coil Steam Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHCCompSteam(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Humidifier Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHumidElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Humidifier NaturalGas Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHumidNaturalGas(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Humidifier Propane Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysHumidPropane(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Evaporative Cooler Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->SysEvapElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Desiccant Dehumidifier Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->DesDehumidElec(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Mechanical Ventilation Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataSysRpts->SysMechVentFlow(SysIndex),
                                "HVAC",
                                "Average",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Natural Ventilation Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataSysRpts->SysNatVentFlow(SysIndex),
                                "HVAC",
                                "Average",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Target Voz Ventilation Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataSysRpts->SysTargetVentilationFlowVoz(SysIndex),
                                "HVAC",
                                "Average",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Ventilation Below Target Voz Time",
                                OutputProcessor::Unit::hr,
                                state.dataSysRpts->SysTimeBelowVozDyn(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Ventilation At Target Voz Time",
                                OutputProcessor::Unit::hr,
                                state.dataSysRpts->SysTimeAtVozDyn(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Ventilation Above Target Voz Time",
                                OutputProcessor::Unit::hr,
                                state.dataSysRpts->SysTimeAboveVozDyn(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);

            SetupOutputVariable(state,
                                "Air System Ventilation When Unoccupied Time",
                                OutputProcessor::Unit::hr,
                                state.dataSysRpts->SysTimeVentUnocc(SysIndex),
                                "HVAC",
                                "Sum",
                                state.dataAirSystemsData->PrimaryAirSystems(SysIndex).Name);
        }
    }
    for (int ZoneIndex = 1; ZoneIndex <= state.dataGlobal->NumOfZones; ++ZoneIndex) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).IsControlled) continue;
        // CurrentModuleObject='Zones(Controlled)'
        if (state.dataSysRpts->VentLoadsReportEnabled) {
            // Cooling Loads
            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation No Load Heat Removal Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxNoLoadCoolingByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Cooling Load Increase Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxCoolingLoadAddedByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxOverheatingByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Cooling Load Decrease Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxCoolingLoadMetByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);
            // Heating Loads
            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation No Load Heat Addition Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxNoLoadHeatingByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Heating Load Increase Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxHeatingLoadAddedByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxOvercoolingByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

            SetupOutputVariable(state,
                                "Zone Mechanical Ventilation Heating Load Decrease Energy",
                                OutputProcessor::Unit::J,
                                state.dataSysRpts->MaxHeatingLoadMetByVent(ZoneIndex),
                                "HVAC",
                                "Sum",
                                state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);
        }

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataSysRpts->ZoneOAMassFlow(ZoneIndex),
                            "HVAC",
                            "Average",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Mass",
                            OutputProcessor::Unit::kg,
                            state.dataSysRpts->ZoneOAMass(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataSysRpts->ZoneOAVolFlowStdRho(ZoneIndex),
                            "HVAC",
                            "Average",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Standard Density Volume",
                            OutputProcessor::Unit::m3,
                            state.dataSysRpts->ZoneOAVolStdRho(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Current Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataSysRpts->ZoneOAVolFlowCrntRho(ZoneIndex),
                            "HVAC",
                            "Average",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Current Density Volume",
                            OutputProcessor::Unit::m3,
                            state.dataSysRpts->ZoneOAVolCrntRho(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Mechanical Ventilation Air Changes per Hour",
                            OutputProcessor::Unit::ach,
                            state.dataSysRpts->ZoneMechACH(ZoneIndex),
                            "HVAC",
                            "Average",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Target Voz Ventilation Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataSysRpts->ZoneTargetVentilationFlowVoz(ZoneIndex),
                            "HVAC",
                            "Average",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Ventilation Below Target Voz Time",
                            OutputProcessor::Unit::hr,
                            state.dataSysRpts->ZoneTimeBelowVozDyn(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Ventilation At Target Voz Time",
                            OutputProcessor::Unit::hr,
                            state.dataSysRpts->ZoneTimeAtVozDyn(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Ventilation Above Target Voz Time",
                            OutputProcessor::Unit::hr,
                            state.dataSysRpts->ZoneTimeAboveVozDyn(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);

        SetupOutputVariable(state,
                            "Zone Ventilation When Unoccupied Time",
                            OutputProcessor::Unit::hr,
                            state.dataSysRpts->ZoneTimeVentUnocc(ZoneIndex),
                            "HVAC",
                            "Sum",
                            state.dataZoneEquip->ZoneEquipConfig(ZoneIndex).ZoneName);
    }

    // Facility outputs
    SetupOutputVariable(state,
                        "Facility Any Zone Ventilation Below Target Voz Time",
                        OutputProcessor::Unit::hr,
                        state.dataSysRpts->AnyZoneTimeBelowVozDyn,
                        "HVAC",
                        "Sum",
                        "Facility");

    SetupOutputVariable(state,
                        "Facility All Zones Ventilation At Target Voz Time",
                        OutputProcessor::Unit::hr,
                        state.dataSysRpts->AllZonesTimeAtVozDyn,
                        "HVAC",
                        "Sum",
                        "Facility");

    SetupOutputVariable(state,
                        "Facility Any Zone Ventilation Above Target Voz Time",
                        OutputProcessor::Unit::hr,
                        state.dataSysRpts->AnyZoneTimeAboveVozDyn,
                        "HVAC",
                        "Sum",
                        "Facility");

    SetupOutputVariable(state,
                        "Facility Any Zone Ventilation When Unoccupied Time",
                        OutputProcessor::Unit::hr,
                        state.dataSysRpts->AnyZoneTimeVentUnocc,
                        "HVAC",
                        "Sum",
                        "Facility");
}

void CreateEnergyReportStructure(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher/Linda Lawrie
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Creates the Energy Reporting Structure.  This routine is only called once --
    // so string compares have been left in.

    // METHODOLOGY EMPLOYED:
    // Once all compsets/nodes/connections have been established find all components
    // subcomponents, etc.

    // Using/Aliasing
    using BranchNodeConnections::GetChildrenData;
    using BranchNodeConnections::GetComponentData;
    using BranchNodeConnections::GetNumChildren;
    using BranchNodeConnections::IsParentObject;
    using namespace DataGlobalConstants;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopNum;
    int BranchNum;
    int CompNum;
    int SubCompNum;
    int SubSubCompNum;
    int VarNum;
    int VarNum1;
    int CtrlZoneNum;
    std::string TypeOfComp;
    std::string NameOfComp;
    bool ErrorsFound;
    bool ModeFlagOn;
    int NumInlets;
    int NumOutlets;
    int PlantLoopNum;

    // Dimension GetChildrenData arrays
    Array1D_string SubCompTypes;
    Array1D_string SubCompNames;
    Array1D_string InletNodeNames;
    Array1D_int InletNodeNumbers;
    Array1D_int InletFluidStreams;
    Array1D_string OutletNodeNames;
    Array1D_int OutletNodeNumbers;
    Array1D_int OutletFluidStreams;
    int NumChildren;
    int NumGrandChildren;
    bool IsParent;

    // Dimension GetMeteredVariables arrays
    Array1D_int VarIndexes;                                         // Variable Numbers
    Array1D_int VarTypes;                                           // Variable Types (1=integer, 2=real, 3=meter)
    Array1D_string UnitsStrings;                                    // UnitsStrings for each variable
    Array1D<OutputProcessor::TimeStepType> IndexTypes;              // Variable Idx Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar;                     // units from enum for each variable
    std::map<int, DataGlobalConstants::ResourceType> ResourceTypes; // ResourceTypes for each variable
    Array1D_string EndUses;                                         // EndUses for each variable
    Array1D_string Groups;                                          // Groups for each variable
    Array1D_string Names;                                           // Variable Names for each variable
    int NumFound;                                                   // Number Found
    int NumVariables;
    int NumLeft; // Counter for deeper components

    // some variables for setting up the plant data structures
    int LoopSideNum;

    state.dataSysRpts->VentReportStructureCreated = true;
    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                TypeOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf;
                NameOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name;
                // Get complete list of components for complex branches
                if (IsParentObject(state, TypeOfComp, NameOfComp)) {

                    state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Parent = true;
                    NumChildren = GetNumChildren(state, TypeOfComp, NameOfComp);

                    SubCompTypes.allocate(NumChildren);
                    SubCompNames.allocate(NumChildren);
                    InletNodeNames.allocate(NumChildren);
                    InletNodeNumbers.allocate(NumChildren);
                    OutletNodeNames.allocate(NumChildren);
                    OutletNodeNumbers.allocate(NumChildren);
                    state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp.allocate(NumChildren);

                    GetChildrenData(state,
                                    TypeOfComp,
                                    NameOfComp,
                                    NumChildren,
                                    SubCompTypes,
                                    SubCompNames,
                                    InletNodeNames,
                                    InletNodeNumbers,
                                    OutletNodeNames,
                                    OutletNodeNumbers,
                                    ErrorsFound);

                    for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                        {
                            auto &thisSubComponent(
                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp(SubCompNum));
                            thisSubComponent.TypeOf = SubCompTypes(SubCompNum);
                            thisSubComponent.Name = SubCompNames(SubCompNum);
                            thisSubComponent.NodeNameIn = InletNodeNames(SubCompNum);
                            thisSubComponent.NodeNameOut = OutletNodeNames(SubCompNum);
                            thisSubComponent.NodeNumIn = InletNodeNumbers(SubCompNum);
                            thisSubComponent.NodeNumOut = OutletNodeNumbers(SubCompNum);
                        }
                    }

                    SubCompTypes.deallocate();
                    SubCompNames.deallocate();
                    InletNodeNames.deallocate();
                    InletNodeNumbers.deallocate();
                    OutletNodeNames.deallocate();
                    OutletNodeNumbers.deallocate();

                } else {
                    NumChildren = 0;
                    state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Parent = false;
                }
                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NumSubComps = NumChildren;

                // check for 'grandchildren'
                for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                    TypeOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp(SubCompNum).TypeOf;
                    NameOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp(SubCompNum).Name;
                    if (IsParentObject(state, TypeOfComp, NameOfComp)) {
                        NumGrandChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                        SubCompTypes.allocate(NumGrandChildren);
                        SubCompNames.allocate(NumGrandChildren);
                        InletNodeNames.allocate(NumGrandChildren);
                        InletNodeNumbers.allocate(NumGrandChildren);
                        OutletNodeNames.allocate(NumGrandChildren);
                        OutletNodeNumbers.allocate(NumGrandChildren);
                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                            .Branch(BranchNum)
                            .Comp(CompNum)
                            .SubComp(SubCompNum)
                            .SubSubComp.allocate(NumGrandChildren);

                        GetChildrenData(state,
                                        TypeOfComp,
                                        NameOfComp,
                                        NumGrandChildren,
                                        SubCompTypes,
                                        SubCompNames,
                                        InletNodeNames,
                                        InletNodeNumbers,
                                        OutletNodeNames,
                                        OutletNodeNumbers,
                                        ErrorsFound);

                        for (SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum) {
                            {
                                auto &thisSubSubComponent(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                              .Branch(BranchNum)
                                                              .Comp(CompNum)
                                                              .SubComp(SubCompNum)
                                                              .SubSubComp(SubSubCompNum));
                                thisSubSubComponent.TypeOf = SubCompTypes(SubSubCompNum);
                                thisSubSubComponent.Name = SubCompNames(SubSubCompNum);
                                thisSubSubComponent.NodeNameIn = InletNodeNames(SubSubCompNum);
                                thisSubSubComponent.NodeNameOut = OutletNodeNames(SubSubCompNum);
                                thisSubSubComponent.NodeNumIn = InletNodeNumbers(SubSubCompNum);
                                thisSubSubComponent.NodeNumOut = OutletNodeNumbers(SubSubCompNum);
                                NumLeft = GetNumChildren(state, SubCompTypes(SubSubCompNum), SubCompNames(SubSubCompNum));
                                if (NumLeft > 0) {
                                    ShowSevereError(
                                        state, "Hanging Children for component=" + SubCompTypes(SubSubCompNum) + ':' + SubCompNames(SubSubCompNum));
                                }
                            }
                        }

                        SubCompTypes.deallocate();
                        SubCompNames.deallocate();
                        InletNodeNames.deallocate();
                        InletNodeNumbers.deallocate();
                        OutletNodeNames.deallocate();
                        OutletNodeNumbers.deallocate();
                    } else {
                        NumGrandChildren = 0;
                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp(SubCompNum).Parent = false;
                    }

                    state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).SubComp(SubCompNum).NumSubSubComps =
                        NumGrandChildren;
                }
            }
        }
    }

    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                // Get complete list of components for complex branches
                {
                    auto &thisComp(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum));
                    TypeOfComp = thisComp.TypeOf;
                    NameOfComp = thisComp.Name;
                    NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                    if (NumVariables > 0) {
                        VarIndexes.allocate(NumVariables);
                        VarTypes.allocate(NumVariables);
                        IndexTypes.allocate(NumVariables);
                        unitsForVar.allocate(NumVariables);

                        for (int idx = 1; idx <= NumVariables; ++idx) {
                            ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                        }

                        EndUses.allocate(NumVariables);
                        Groups.allocate(NumVariables);
                        Names.allocate(NumVariables);
                        thisComp.MeteredVar.allocate(NumVariables);

                        thisComp.NumMeteredVars = NumVariables;
                        GetMeteredVariables(state,
                                            TypeOfComp,
                                            NameOfComp,
                                            VarIndexes,
                                            VarTypes,
                                            IndexTypes,
                                            unitsForVar,
                                            ResourceTypes,
                                            EndUses,
                                            Groups,
                                            Names,
                                            NumFound);
                        ModeFlagOn = true;
                        for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                            {
                                auto &thisVar(thisComp.MeteredVar(VarNum));
                                thisVar.ReportVarName = Names(VarNum);
                                thisVar.ReportVarUnits = unitsForVar(VarNum);
                                thisVar.ReportVarIndex = VarIndexes(VarNum);
                                thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                thisVar.ReportVarType = VarTypes(VarNum);
                                thisVar.ResourceType = ResourceTypes.at(VarNum);
                                thisVar.EndUse = EndUses(VarNum);
                                if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                    for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                        thisComp.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                    }
                                    ModeFlagOn = false;
                                } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                    for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                        thisComp.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                    }
                                    ModeFlagOn = false;
                                } else if (ModeFlagOn) {
                                    thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                }
                                thisVar.Group = Groups(VarNum);
                            }
                        }

                        VarIndexes.deallocate();
                        VarTypes.deallocate();
                        IndexTypes.deallocate();
                        unitsForVar.deallocate();
                        EndUses.deallocate();
                        Groups.deallocate();
                        Names.deallocate();
                    }
                    for (SubCompNum = 1; SubCompNum <= thisComp.NumSubComps; ++SubCompNum) {
                        // Get complete list of components for complex branches
                        TypeOfComp = thisComp.SubComp(SubCompNum).TypeOf;
                        NameOfComp = thisComp.SubComp(SubCompNum).Name;
                        NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                        if (NumVariables > 0) {
                            VarIndexes.allocate(NumVariables);
                            VarTypes.allocate(NumVariables);
                            IndexTypes.allocate(NumVariables);
                            unitsForVar.allocate(NumVariables);
                            ResourceTypes.clear();
                            for (int idx = 1; idx <= NumVariables; ++idx) {
                                ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                            }
                            EndUses.allocate(NumVariables);
                            Groups.allocate(NumVariables);
                            Names.allocate(NumVariables);
                            thisComp.SubComp(SubCompNum).MeteredVar.allocate(NumVariables);

                            GetMeteredVariables(state,
                                                TypeOfComp,
                                                NameOfComp,
                                                VarIndexes,
                                                VarTypes,
                                                IndexTypes,
                                                unitsForVar,
                                                ResourceTypes,
                                                EndUses,
                                                Groups,
                                                Names,
                                                NumFound);

                            ModeFlagOn = true;
                            for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                                {
                                    auto &thisVar(thisComp.SubComp(SubCompNum).MeteredVar(VarNum));
                                    thisVar.ReportVarName = Names(VarNum);
                                    thisVar.ReportVarUnits = unitsForVar(VarNum);
                                    thisVar.ReportVarIndex = VarIndexes(VarNum);
                                    thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                    thisVar.ReportVarType = VarTypes(VarNum);
                                    thisVar.ResourceType = ResourceTypes.at(VarNum);
                                    thisVar.EndUse = EndUses(VarNum);
                                    if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisComp.SubComp(SubCompNum).MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisComp.SubComp(SubCompNum).MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (ModeFlagOn) {
                                        thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                    }
                                    thisVar.Group = Groups(VarNum);
                                }
                            }

                            VarIndexes.deallocate();
                            VarTypes.deallocate();
                            IndexTypes.deallocate();
                            unitsForVar.deallocate();
                            EndUses.deallocate();
                            Groups.deallocate();
                            Names.deallocate();
                        }

                        thisComp.SubComp(SubCompNum).NumMeteredVars = NumVariables;

                        for (SubSubCompNum = 1; SubSubCompNum <= thisComp.SubComp(SubCompNum).NumSubSubComps; ++SubSubCompNum) {
                            // Get complete list of components for complex branches
                            TypeOfComp = thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).TypeOf;
                            NameOfComp = thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).Name;
                            NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                            if (NumVariables > 0) {
                                VarIndexes.allocate(NumVariables);
                                VarTypes.allocate(NumVariables);
                                IndexTypes.allocate(NumVariables);
                                unitsForVar.allocate(NumVariables);
                                ResourceTypes.clear();
                                for (int idx = 1; idx <= NumVariables; ++idx) {
                                    ResourceTypes.insert(
                                        std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                                }
                                EndUses.allocate(NumVariables);
                                Groups.allocate(NumVariables);
                                Names.allocate(NumVariables);
                                thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).MeteredVar.allocate(NumVariables);

                                GetMeteredVariables(state,
                                                    TypeOfComp,
                                                    NameOfComp,
                                                    VarIndexes,
                                                    VarTypes,
                                                    IndexTypes,
                                                    unitsForVar,
                                                    ResourceTypes,
                                                    EndUses,
                                                    Groups,
                                                    Names,
                                                    NumFound);

                                ModeFlagOn = true;
                                for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                                    {
                                        auto &thisVar(thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).MeteredVar(VarNum));
                                        thisVar.ReportVarName = Names(VarNum);
                                        thisVar.ReportVarUnits = unitsForVar(VarNum);
                                        thisVar.ReportVarIndex = VarIndexes(VarNum);
                                        thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                        thisVar.ReportVarType = VarTypes(VarNum);
                                        thisVar.ResourceType = ResourceTypes.at(VarNum);
                                        thisVar.EndUse = EndUses(VarNum);
                                        if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                            for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                                thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).MeteredVar(VarNum1).EndUse_CompMode =
                                                    iEndUseType::HeatingOnly;
                                            }
                                            ModeFlagOn = false;
                                        } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                            for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                                thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).MeteredVar(VarNum1).EndUse_CompMode =
                                                    iEndUseType::CoolingOnly;
                                            }
                                            ModeFlagOn = false;
                                        } else if (ModeFlagOn) {
                                            thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                        }
                                        thisVar.Group = Groups(VarNum);
                                    }
                                }

                                VarIndexes.deallocate();
                                VarTypes.deallocate();
                                IndexTypes.deallocate();
                                unitsForVar.deallocate();
                                EndUses.deallocate();
                                Groups.deallocate();
                                Names.deallocate();
                            }
                            thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum).NumMeteredVars = NumVariables;
                        }
                    }
                }
            }
        }
    }

    // Allocate the system serving zone equipment component arrays
    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        // Set index of air loop serving zone
        for (CompNum = 1; CompNum <= state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).NumOfEquipTypes; ++CompNum) {
            TypeOfComp = state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).EquipType(CompNum);
            NameOfComp = state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).EquipName(CompNum);
            GetComponentData(state,
                             TypeOfComp,
                             NameOfComp,
                             IsParent,
                             NumInlets,
                             InletNodeNames,
                             InletNodeNumbers,
                             InletFluidStreams,
                             NumOutlets,
                             OutletNodeNames,
                             OutletNodeNumbers,
                             OutletFluidStreams,
                             ErrorsFound);
            {
                auto &thisEquipData(state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).EquipData(CompNum));
                thisEquipData.TypeOf = TypeOfComp;
                thisEquipData.Name = NameOfComp;
                thisEquipData.OutletNodeNums.allocate(NumOutlets);
                thisEquipData.NumOutlets = NumOutlets;
                thisEquipData.OutletNodeNums = OutletNodeNumbers;
                thisEquipData.InletNodeNums.allocate(NumInlets);
                thisEquipData.NumInlets = NumInlets;
                thisEquipData.InletNodeNums = InletNodeNumbers;
                thisEquipData.Parent = IsParent;
                NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                thisEquipData.NumMeteredVars = NumVariables;
                if (NumVariables > 0) {
                    InletNodeNames.deallocate();
                    InletNodeNumbers.deallocate();
                    InletFluidStreams.deallocate();
                    OutletNodeNames.deallocate();
                    OutletNodeNumbers.deallocate();
                    OutletFluidStreams.deallocate();

                    VarIndexes.allocate(NumVariables);
                    VarTypes.allocate(NumVariables);
                    IndexTypes.allocate(NumVariables);
                    unitsForVar.allocate(NumVariables);
                    ResourceTypes.clear();
                    for (int idx = 1; idx <= NumVariables; ++idx) {
                        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                    }
                    EndUses.allocate(NumVariables);
                    Groups.allocate(NumVariables);
                    Names.allocate(NumVariables);
                    thisEquipData.MeteredVar.allocate(NumVariables);

                    GetMeteredVariables(state,
                                        TypeOfComp,
                                        NameOfComp,
                                        VarIndexes,
                                        VarTypes,
                                        IndexTypes,
                                        unitsForVar,
                                        ResourceTypes,
                                        EndUses,
                                        Groups,
                                        Names,
                                        NumFound);

                    ModeFlagOn = true;
                    for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                        {
                            auto &thisVar(thisEquipData.MeteredVar(VarNum));
                            thisVar.ReportVarName = Names(VarNum);
                            thisVar.ReportVarUnits = unitsForVar(VarNum);
                            thisVar.ReportVarIndex = VarIndexes(VarNum);
                            thisVar.ReportVarIndexType = IndexTypes(VarNum);
                            thisVar.ReportVarType = VarTypes(VarNum);
                            thisVar.ResourceType = ResourceTypes.at(VarNum);
                            thisVar.EndUse = EndUses(VarNum);
                            if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                    thisEquipData.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                }
                                ModeFlagOn = false;
                            } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                    thisEquipData.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                }
                                ModeFlagOn = false;
                            } else if (ModeFlagOn) {
                                thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                            }
                            thisVar.Group = Groups(VarNum);
                        }
                    }

                    VarIndexes.deallocate();
                    VarTypes.deallocate();
                    IndexTypes.deallocate();
                    unitsForVar.deallocate();
                    EndUses.deallocate();
                    Groups.deallocate();
                    Names.deallocate();
                }

                if (IsParentObject(state, TypeOfComp, NameOfComp)) {
                    NumChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                    thisEquipData.NumSubEquip = NumChildren;

                    SubCompTypes.allocate(NumChildren);
                    SubCompNames.allocate(NumChildren);
                    InletNodeNames.allocate(NumChildren);
                    InletNodeNumbers.allocate(NumChildren);
                    OutletNodeNames.allocate(NumChildren);
                    OutletNodeNumbers.allocate(NumChildren);
                    thisEquipData.SubEquipData.allocate(NumChildren);

                    GetChildrenData(state,
                                    TypeOfComp,
                                    NameOfComp,
                                    NumChildren,
                                    SubCompTypes,
                                    SubCompNames,
                                    InletNodeNames,
                                    InletNodeNumbers,
                                    OutletNodeNames,
                                    OutletNodeNumbers,
                                    ErrorsFound);

                    for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                        thisEquipData.SubEquipData(SubCompNum).TypeOf = SubCompTypes(SubCompNum);
                        thisEquipData.SubEquipData(SubCompNum).Name = SubCompNames(SubCompNum);
                        thisEquipData.SubEquipData(SubCompNum).OutletNodeNum = OutletNodeNumbers(SubCompNum);
                        thisEquipData.SubEquipData(SubCompNum).InletNodeNum = InletNodeNumbers(SubCompNum);
                    }

                    SubCompTypes.deallocate();
                    SubCompNames.deallocate();
                    InletNodeNames.deallocate();
                    InletNodeNumbers.deallocate();
                    OutletNodeNames.deallocate();
                    OutletNodeNumbers.deallocate();
                } else {
                    NumChildren = 0;
                }

                for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                    TypeOfComp = thisEquipData.SubEquipData(SubCompNum).TypeOf;
                    NameOfComp = thisEquipData.SubEquipData(SubCompNum).Name;
                    if (IsParentObject(state, TypeOfComp, NameOfComp)) {
                        NumGrandChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                        thisEquipData.SubEquipData(SubCompNum).NumSubSubEquip = NumGrandChildren;
                        SubCompTypes.allocate(NumGrandChildren);
                        SubCompNames.allocate(NumGrandChildren);
                        InletNodeNames.allocate(NumGrandChildren);
                        InletNodeNumbers.allocate(NumGrandChildren);
                        OutletNodeNames.allocate(NumGrandChildren);
                        OutletNodeNumbers.allocate(NumGrandChildren);
                        thisEquipData.SubEquipData(SubCompNum).SubSubEquipData.allocate(NumGrandChildren);
                        // Sankar added the array number for EquipData
                        GetChildrenData(state,
                                        TypeOfComp,
                                        NameOfComp,
                                        NumGrandChildren,
                                        SubCompTypes,
                                        SubCompNames,
                                        InletNodeNames,
                                        InletNodeNumbers,
                                        OutletNodeNames,
                                        OutletNodeNumbers,
                                        ErrorsFound);

                        for (SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum) {
                            thisEquipData.SubEquipData(SubCompNum).SubSubEquipData(SubSubCompNum).TypeOf = SubCompTypes(SubSubCompNum);
                            thisEquipData.SubEquipData(SubCompNum).SubSubEquipData(SubSubCompNum).Name = SubCompNames(SubSubCompNum);
                            thisEquipData.SubEquipData(SubCompNum).SubSubEquipData(SubSubCompNum).OutletNodeNum = OutletNodeNumbers(SubSubCompNum);
                            thisEquipData.SubEquipData(SubCompNum).SubSubEquipData(SubSubCompNum).InletNodeNum = InletNodeNumbers(SubSubCompNum);
                        }
                        SubCompTypes.deallocate();
                        SubCompNames.deallocate();
                        InletNodeNames.deallocate();
                        InletNodeNumbers.deallocate();
                        OutletNodeNames.deallocate();
                        OutletNodeNumbers.deallocate();
                    } else {
                        NumGrandChildren = 0;
                    }
                }
            }
        }
    }

    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        for (CompNum = 1; CompNum <= state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).NumOfEquipTypes; ++CompNum) {
            for (SubCompNum = 1; SubCompNum <= state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).EquipData(CompNum).NumSubEquip; ++SubCompNum) {
                {
                    auto &thisSubEquipData(state.dataZoneEquip->ZoneEquipList(CtrlZoneNum).EquipData(CompNum).SubEquipData(SubCompNum));
                    TypeOfComp = thisSubEquipData.TypeOf;
                    NameOfComp = thisSubEquipData.Name;

                    NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                    thisSubEquipData.NumMeteredVars = NumVariables; // Sankar added this line
                    if (NumVariables > 0) {
                        VarIndexes.allocate(NumVariables);
                        VarTypes.allocate(NumVariables);
                        IndexTypes.allocate(NumVariables);
                        unitsForVar.allocate(NumVariables);
                        ResourceTypes.clear();
                        for (int idx = 1; idx <= NumVariables; ++idx) {
                            ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                        }
                        EndUses.allocate(NumVariables);
                        Groups.allocate(NumVariables);
                        Names.allocate(NumVariables);
                        thisSubEquipData.MeteredVar.allocate(NumVariables);

                        GetMeteredVariables(state,
                                            TypeOfComp,
                                            NameOfComp,
                                            VarIndexes,
                                            VarTypes,
                                            IndexTypes,
                                            unitsForVar,
                                            ResourceTypes,
                                            EndUses,
                                            Groups,
                                            Names,
                                            NumFound);

                        ModeFlagOn = true;
                        for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                            {
                                auto &thisVar(thisSubEquipData.MeteredVar(VarNum));
                                thisVar.ReportVarName = Names(VarNum);
                                thisVar.ReportVarUnits = unitsForVar(VarNum);
                                thisVar.ReportVarIndex = VarIndexes(VarNum);
                                thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                thisVar.ReportVarType = VarTypes(VarNum);
                                thisVar.ResourceType = ResourceTypes.at(VarNum);
                                thisVar.EndUse = EndUses(VarNum);
                                if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                    for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                        thisSubEquipData.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                    }
                                    ModeFlagOn = false;
                                } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                    for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                        thisSubEquipData.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                    }
                                    ModeFlagOn = false;
                                } else if (ModeFlagOn) {
                                    thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                }
                                thisVar.Group = Groups(VarNum);
                            }
                        }

                        VarIndexes.deallocate();
                        VarTypes.deallocate();
                        IndexTypes.deallocate();
                        unitsForVar.deallocate();
                        EndUses.deallocate();
                        Groups.deallocate();
                        Names.deallocate();
                    }

                    for (SubSubCompNum = 1; SubSubCompNum <= thisSubEquipData.NumSubSubEquip; ++SubSubCompNum) {
                        TypeOfComp = thisSubEquipData.SubSubEquipData(SubSubCompNum).TypeOf;
                        NameOfComp = thisSubEquipData.SubSubEquipData(SubSubCompNum).Name;

                        NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                        thisSubEquipData.SubSubEquipData(SubSubCompNum).NumMeteredVars = NumVariables; // Sankar added this line
                        if (NumVariables > 0) {
                            VarIndexes.allocate(NumVariables);
                            VarTypes.allocate(NumVariables);
                            IndexTypes.allocate(NumVariables);
                            unitsForVar.allocate(NumVariables);
                            ResourceTypes.clear();
                            for (int idx = 1; idx <= NumVariables; ++idx) {
                                ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                            }
                            EndUses.allocate(NumVariables);
                            Groups.allocate(NumVariables);
                            Names.allocate(NumVariables);
                            thisSubEquipData.SubSubEquipData(SubSubCompNum).MeteredVar.allocate(NumVariables);

                            GetMeteredVariables(state,
                                                TypeOfComp,
                                                NameOfComp,
                                                VarIndexes,
                                                VarTypes,
                                                IndexTypes,
                                                unitsForVar,
                                                ResourceTypes,
                                                EndUses,
                                                Groups,
                                                Names,
                                                NumFound);

                            ModeFlagOn = true;
                            for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                                {
                                    auto &thisVar(thisSubEquipData.SubSubEquipData(SubSubCompNum).MeteredVar(VarNum));
                                    thisVar.ReportVarName = Names(VarNum);
                                    thisVar.ReportVarUnits = unitsForVar(VarNum);
                                    thisVar.ReportVarIndex = VarIndexes(VarNum);
                                    thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                    thisVar.ReportVarType = VarTypes(VarNum);
                                    thisVar.ResourceType = ResourceTypes.at(VarNum);
                                    thisVar.EndUse = EndUses(VarNum);
                                    if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisSubEquipData.SubSubEquipData(SubSubCompNum).MeteredVar(VarNum1).EndUse_CompMode =
                                                iEndUseType::HeatingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisSubEquipData.SubSubEquipData(SubSubCompNum).MeteredVar(VarNum1).EndUse_CompMode =
                                                iEndUseType::CoolingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (ModeFlagOn) {
                                        thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                    }
                                    thisVar.Group = Groups(VarNum);
                                }
                            }

                            VarIndexes.deallocate();
                            VarTypes.deallocate();
                            IndexTypes.deallocate();
                            unitsForVar.deallocate();
                            EndUses.deallocate();
                            Groups.deallocate();
                            Names.deallocate();
                        }
                    }
                }
            }
        }
    }

    //***Plant Loops

    // previously, four separate huge DO loops all looking very very similar were used here
    // each individual block would operate on a single type of loop-side (plant demand, cond supply, etc.)
    // now, a bigger DO loop is applied iterating over all loops
    // a pointer (ThisReportData) is then directed to a particular item in the appropriate array
    // by operating on the pointer directly, we are actually operating on the item in the TARGET array item
    // in making this change, over 700 lines of code were dropped down to a single block

    for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops + state.dataHVACGlobal->NumCondLoops; ++PlantLoopNum) {
        for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {

            // Report selection
            ReportLoopData *select_ThisReportData(nullptr);

            if (PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops) {
                {
                    auto const SELECT_CASE_var(LoopSideNum);
                    if (SELECT_CASE_var == DemandSide) {
                        select_ThisReportData = &state.dataPlnt->VentRepPlantDemandSide(PlantLoopNum);
                    } else if (SELECT_CASE_var == SupplySide) {
                        select_ThisReportData = &state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum);
                    } else {
                        assert(false);
                    }
                }
            } else { // CondLoop
                {
                    auto const SELECT_CASE_var(LoopSideNum);
                    if (SELECT_CASE_var == DemandSide) {
                        select_ThisReportData = &state.dataPlnt->VentRepCondDemandSide(PlantLoopNum - state.dataHVACGlobal->NumPlantLoops);
                    } else if (SELECT_CASE_var == SupplySide) {
                        select_ThisReportData = &state.dataPlnt->VentRepCondSupplySide(PlantLoopNum - state.dataHVACGlobal->NumPlantLoops);
                    } else {
                        assert(false);
                    }
                }
            }

            // Object Data
            ReportLoopData &ThisReportData(*select_ThisReportData);

            for (BranchNum = 1; BranchNum <= ThisReportData.TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= ThisReportData.Branch(BranchNum).TotalComponents; ++CompNum) {
                    {
                        auto &thisComp(ThisReportData.Branch(BranchNum).Comp(CompNum));
                        TypeOfComp = thisComp.TypeOf;
                        NameOfComp = thisComp.Name;
                        // Get complete list of components for complex branches
                        if (IsParentObject(state, TypeOfComp, NameOfComp)) {

                            NumChildren = GetNumChildren(state, TypeOfComp, NameOfComp);

                            SubCompTypes.allocate(NumChildren);
                            SubCompNames.allocate(NumChildren);
                            InletNodeNames.allocate(NumChildren);
                            InletNodeNumbers.allocate(NumChildren);
                            OutletNodeNames.allocate(NumChildren);
                            OutletNodeNumbers.allocate(NumChildren);
                            thisComp.SubComp.allocate(NumChildren);

                            GetChildrenData(state,
                                            TypeOfComp,
                                            NameOfComp,
                                            NumChildren,
                                            SubCompTypes,
                                            SubCompNames,
                                            InletNodeNames,
                                            InletNodeNumbers,
                                            OutletNodeNames,
                                            OutletNodeNumbers,
                                            ErrorsFound);

                            for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                                thisComp.SubComp(SubCompNum).TypeOf = SubCompTypes(SubCompNum);
                                thisComp.SubComp(SubCompNum).Name = SubCompNames(SubCompNum);
                                thisComp.SubComp(SubCompNum).NodeNameIn = InletNodeNames(SubCompNum);
                                thisComp.SubComp(SubCompNum).NodeNameOut = OutletNodeNames(SubCompNum);
                                thisComp.SubComp(SubCompNum).NodeNumIn = InletNodeNumbers(SubCompNum);
                                thisComp.SubComp(SubCompNum).NodeNumOut = OutletNodeNumbers(SubCompNum);
                            }

                            SubCompTypes.deallocate();
                            SubCompNames.deallocate();
                            InletNodeNames.deallocate();
                            InletNodeNumbers.deallocate();
                            OutletNodeNames.deallocate();
                            OutletNodeNumbers.deallocate();

                        } else {
                            NumChildren = 0;
                        }
                        thisComp.NumSubComps = NumChildren;

                        // check for 'grandchildren'
                        for (SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum) {
                            TypeOfComp = thisComp.SubComp(SubCompNum).TypeOf;
                            NameOfComp = thisComp.SubComp(SubCompNum).Name;
                            if (IsParentObject(state, TypeOfComp, NameOfComp)) {
                                NumGrandChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                                SubCompTypes.allocate(NumGrandChildren);
                                SubCompNames.allocate(NumGrandChildren);
                                InletNodeNames.allocate(NumGrandChildren);
                                InletNodeNumbers.allocate(NumGrandChildren);
                                OutletNodeNames.allocate(NumGrandChildren);
                                OutletNodeNumbers.allocate(NumGrandChildren);
                                thisComp.SubComp(SubCompNum).SubSubComp.allocate(NumGrandChildren);

                                GetChildrenData(state,
                                                TypeOfComp,
                                                NameOfComp,
                                                NumGrandChildren,
                                                SubCompTypes,
                                                SubCompNames,
                                                InletNodeNames,
                                                InletNodeNumbers,
                                                OutletNodeNames,
                                                OutletNodeNumbers,
                                                ErrorsFound);

                                for (SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum) {
                                    {
                                        auto &thisSubSubComp(thisComp.SubComp(SubCompNum).SubSubComp(SubSubCompNum));
                                        thisSubSubComp.TypeOf = SubCompTypes(SubSubCompNum);
                                        thisSubSubComp.Name = SubCompNames(SubSubCompNum);
                                        thisSubSubComp.NodeNameIn = InletNodeNames(SubSubCompNum);
                                        thisSubSubComp.NodeNameOut = OutletNodeNames(SubSubCompNum);
                                        thisSubSubComp.NodeNumIn = InletNodeNumbers(SubSubCompNum);
                                        thisSubSubComp.NodeNumOut = OutletNodeNumbers(SubSubCompNum);
                                    }
                                }

                                SubCompTypes.deallocate();
                                SubCompNames.deallocate();
                                InletNodeNames.deallocate();
                                InletNodeNumbers.deallocate();
                                OutletNodeNames.deallocate();
                                OutletNodeNumbers.deallocate();
                            } else {
                                NumGrandChildren = 0;
                                thisComp.SubComp(SubCompNum).Parent = false;
                            }

                            thisComp.SubComp(SubCompNum).NumSubSubComps = NumGrandChildren;
                        }
                    }
                }
            }
        }
    }

    for (PlantLoopNum = 1; PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops + state.dataHVACGlobal->NumCondLoops; ++PlantLoopNum) {

        for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {

            // Report selection
            ReportLoopData *select_ThisReportData(nullptr);

            if (PlantLoopNum <= state.dataHVACGlobal->NumPlantLoops) {
                {
                    auto const SELECT_CASE_var(LoopSideNum);
                    if (SELECT_CASE_var == DemandSide) {
                        select_ThisReportData = &state.dataPlnt->VentRepPlantDemandSide(PlantLoopNum);
                    } else if (SELECT_CASE_var == SupplySide) {
                        select_ThisReportData = &state.dataPlnt->VentRepPlantSupplySide(PlantLoopNum);
                    } else {
                        assert(false);
                    }
                }
            } else { // CondLoop
                {
                    auto const SELECT_CASE_var(LoopSideNum);
                    if (SELECT_CASE_var == DemandSide) {
                        select_ThisReportData = &state.dataPlnt->VentRepCondDemandSide(PlantLoopNum - state.dataHVACGlobal->NumPlantLoops);
                    } else if (SELECT_CASE_var == SupplySide) {
                        select_ThisReportData = &state.dataPlnt->VentRepCondSupplySide(PlantLoopNum - state.dataHVACGlobal->NumPlantLoops);
                    } else {
                        assert(false);
                    }
                }
            }

            // Object Data
            ReportLoopData &ThisReportData(*select_ThisReportData);

            for (BranchNum = 1; BranchNum <= ThisReportData.TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= ThisReportData.Branch(BranchNum).TotalComponents; ++CompNum) {
                    // Get complete list of components for complex branches
                    {
                        auto &thisComp(ThisReportData.Branch(BranchNum).Comp(CompNum));
                        TypeOfComp = thisComp.TypeOf;
                        NameOfComp = thisComp.Name;
                        NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                        if (NumVariables > 0) {
                            VarIndexes.allocate(NumVariables);
                            VarTypes.allocate(NumVariables);
                            IndexTypes.allocate(NumVariables);
                            unitsForVar.allocate(NumVariables);
                            ResourceTypes.clear();
                            for (int idx = 1; idx <= NumVariables; ++idx) {
                                ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                            }
                            EndUses.allocate(NumVariables);
                            Groups.allocate(NumVariables);
                            Names.allocate(NumVariables);
                            thisComp.MeteredVar.allocate(NumVariables);

                            thisComp.NumMeteredVars = NumVariables;
                            GetMeteredVariables(state,
                                                TypeOfComp,
                                                NameOfComp,
                                                VarIndexes,
                                                VarTypes,
                                                IndexTypes,
                                                unitsForVar,
                                                ResourceTypes,
                                                EndUses,
                                                Groups,
                                                Names,
                                                NumFound);

                            ModeFlagOn = true;
                            for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                                {
                                    auto &thisVar(thisComp.MeteredVar(VarNum));
                                    thisVar.ReportVarName = Names(VarNum);
                                    thisVar.ReportVarUnits = unitsForVar(VarNum);
                                    thisVar.ReportVarIndex = VarIndexes(VarNum);
                                    thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                    thisVar.ReportVarType = VarTypes(VarNum);
                                    thisVar.ResourceType = ResourceTypes.at(VarNum);
                                    thisVar.EndUse = EndUses(VarNum);
                                    if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisComp.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                        for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                            thisComp.MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                        }
                                        ModeFlagOn = false;
                                    } else if (ModeFlagOn) {
                                        thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                    }
                                    thisVar.Group = Groups(VarNum);
                                }
                            }

                            VarIndexes.deallocate();
                            VarTypes.deallocate();
                            IndexTypes.deallocate();
                            unitsForVar.deallocate();
                            EndUses.deallocate();
                            Groups.deallocate();
                            Names.deallocate();
                        }
                        for (SubCompNum = 1; SubCompNum <= thisComp.NumSubComps; ++SubCompNum) {
                            // Get complete list of components for complex branches
                            TypeOfComp = thisComp.SubComp(SubCompNum).TypeOf;
                            NameOfComp = thisComp.SubComp(SubCompNum).Name;
                            NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
                            if (NumVariables > 0) {
                                VarIndexes.allocate(NumVariables);
                                VarTypes.allocate(NumVariables);
                                IndexTypes.allocate(NumVariables);
                                unitsForVar.allocate(NumVariables);
                                ResourceTypes.clear();
                                for (int idx = 1; idx <= NumVariables; ++idx) {
                                    ResourceTypes.insert(
                                        std::pair<int, DataGlobalConstants::ResourceType>(idx, DataGlobalConstants::ResourceType::None));
                                }
                                EndUses.allocate(NumVariables);
                                Groups.allocate(NumVariables);
                                Names.allocate(NumVariables);
                                thisComp.SubComp(SubCompNum).MeteredVar.allocate(NumVariables);

                                GetMeteredVariables(state,
                                                    TypeOfComp,
                                                    NameOfComp,
                                                    VarIndexes,
                                                    VarTypes,
                                                    IndexTypes,
                                                    unitsForVar,
                                                    ResourceTypes,
                                                    EndUses,
                                                    Groups,
                                                    Names,
                                                    NumFound);

                                ModeFlagOn = true;
                                for (VarNum = 1; VarNum <= NumVariables; ++VarNum) {
                                    {
                                        auto &thisVar(thisComp.SubComp(SubCompNum).MeteredVar(VarNum));
                                        thisVar.ReportVarName = Names(VarNum);
                                        thisVar.ReportVarUnits = unitsForVar(VarNum);
                                        thisVar.ReportVarIndex = VarIndexes(VarNum);
                                        thisVar.ReportVarIndexType = IndexTypes(VarNum);
                                        thisVar.ReportVarType = VarTypes(VarNum);
                                        thisVar.ResourceType = ResourceTypes.at(VarNum);
                                        thisVar.EndUse = EndUses(VarNum);
                                        if (thisVar.EndUse == "HEATINGCOILS" && ModeFlagOn) {
                                            for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                                thisComp.SubComp(SubCompNum).MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::HeatingOnly;
                                            }
                                            ModeFlagOn = false;
                                        } else if (thisVar.EndUse == "COOLINGCOILS" && ModeFlagOn) {
                                            for (VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1) {
                                                thisComp.SubComp(SubCompNum).MeteredVar(VarNum1).EndUse_CompMode = iEndUseType::CoolingOnly;
                                            }
                                            ModeFlagOn = false;
                                        } else if (ModeFlagOn) {
                                            thisVar.EndUse_CompMode = iEndUseType::NoHeatNoCool;
                                        }
                                        thisVar.Group = Groups(VarNum);
                                    }
                                }

                                VarIndexes.deallocate();
                                VarTypes.deallocate();
                                IndexTypes.deallocate();
                                unitsForVar.deallocate();
                                EndUses.deallocate();
                                Groups.deallocate();
                                Names.deallocate();
                            }
                            thisComp.SubComp(SubCompNum).NumMeteredVars = NumVariables;
                        }
                    }
                }
            }
        }
    }
}

// End Initialization Section of the Module
//******************************************************************************

// Beginning of Reporting subroutines for the SimAir Module
// *****************************************************************************

void ReportSystemEnergyUse(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   November 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate and report system loads and energy

    // METHODOLOGY EMPLOYED:
    // Accumulate meter data to appropriate report variables

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;
    using Psychrometrics::PsyHFnTdbW;
    using namespace DataGlobalConstants;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Idx;          // loop counter
    int nodes;        // loop counter
    int CtrlZoneNum;  // ZONE counter
    int ZoneInNum;    // counter for zone air distribution inlets
    int AirLoopNum;   // counter for zone air distribution inlets
    int BranchNum;    // counter for zone air distribution inlets
    int EquipListNum; // counter for zone air distribution inlets
    int VarNum;       // counter for zone air distribution inlets
    int CompNum;
    int SubCompNum;
    int SubSubCompNum;
    iEndUseType CompMode;
    int InletNodeNum;
    int OutletNodeNum;
    int ADUNum;
    int ADUCoolNum;
    int ADUHeatNum;
    int AirDistCoolInletNodeNum;
    int AirDistHeatInletNodeNum;
    DataGlobalConstants::ResourceType EnergyType;
    int ActualZoneNum;
    Real64 CompEnergyUse;
    Real64 ZoneLoad;
    Real64 CompLoad;
    Real64 ADUCoolFlowrate;
    Real64 ADUHeatFlowrate;
    bool CompLoadFlag;

    if (!state.dataSysRpts->AirLoopLoadsReportEnabled) return;

    // SYSTEM LOADS REPORT
    state.dataSysRpts->SysTotZoneLoadHTNG = 0.0;
    state.dataSysRpts->SysTotZoneLoadCLNG = 0.0;
    state.dataSysRpts->SysOALoadHTNG = 0.0;
    state.dataSysRpts->SysOALoadCLNG = 0.0;
    state.dataSysRpts->SysTotHTNG = 0.0;
    state.dataSysRpts->SysTotCLNG = 0.0;

    // SYSTEM ENERGY USE REPORT
    state.dataSysRpts->SysTotElec = 0.0;
    state.dataSysRpts->SysTotNaturalGas = 0.0;
    state.dataSysRpts->SysTotPropane = 0.0;
    state.dataSysRpts->SysTotSteam = 0.0;
    state.dataSysRpts->SysTotH2OCOLD = 0.0;
    state.dataSysRpts->SysTotH2OHOT = 0.0;

    // SYSTEM COMPONENT LOADS REPORT
    state.dataSysRpts->SysFANCompHTNG = 0.0;
    state.dataSysRpts->SysCCCompCLNG = 0.0;
    state.dataSysRpts->SysHCCompHTNG = 0.0;
    state.dataSysRpts->SysHeatExHTNG = 0.0;
    state.dataSysRpts->SysHeatExCLNG = 0.0;
    state.dataSysRpts->SysSolarCollectHeating = 0.0;
    state.dataSysRpts->SysSolarCollectCooling = 0.0;
    state.dataSysRpts->SysUserDefinedTerminalHeating = 0.0;
    state.dataSysRpts->SysUserDefinedTerminalCooling = 0.0;
    state.dataSysRpts->SysHumidHTNG = 0.0;
    state.dataSysRpts->SysEvapCLNG = 0.0;
    state.dataSysRpts->DesDehumidCLNG = 0.0;
    state.dataSysRpts->SysDomesticH2O = 0.0;

    // SYSTEM COMPONENT ENERGY REPORT
    state.dataSysRpts->SysFANCompElec = 0.0;
    state.dataSysRpts->SysHCCompH2OHOT = 0.0;
    state.dataSysRpts->SysCCCompH2OCOLD = 0.0;
    state.dataSysRpts->SysHCCompElec = 0.0;
    state.dataSysRpts->SysCCCompElec = 0.0;
    state.dataSysRpts->SysHCCompElecRes = 0.0;
    state.dataSysRpts->SysHCCompNaturalGas = 0.0;
    state.dataSysRpts->SysHCCompPropane = 0.0;
    state.dataSysRpts->SysHCCompSteam = 0.0;
    state.dataSysRpts->SysHumidElec = 0.0;
    state.dataSysRpts->SysHumidNaturalGas = 0.0;
    state.dataSysRpts->SysHumidPropane = 0.0;
    state.dataSysRpts->DesDehumidElec = 0.0;
    state.dataSysRpts->SysEvapElec = 0.0;

    auto &Node(state.dataLoopNodes->Node);

    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        auto const &pas = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum);
        for (BranchNum = 1; BranchNum <= pas.NumBranches; ++BranchNum) {
            auto const &pasBranch = pas.Branch(BranchNum);
            if (Node(pasBranch.NodeNumOut).MassFlowRate <= 0.0) continue;
            for (CompNum = 1; CompNum <= pasBranch.TotalComponents; ++CompNum) {
                auto const &pasBranchComp = pasBranch.Comp(CompNum);
                InletNodeNum = pasBranchComp.NodeNumIn;
                OutletNodeNum = pasBranchComp.NodeNumOut;
                if (InletNodeNum <= 0 || OutletNodeNum <= 0) continue;
                CompLoad = Node(OutletNodeNum).MassFlowRate * (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) -
                                                               PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat));
                CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                CompEnergyUse = 0.0;
                EnergyType = DataGlobalConstants::ResourceType::None;
                CompLoadFlag = true;
                CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                CompLoadFlag = false;
                for (VarNum = 1; VarNum <= pasBranchComp.NumMeteredVars; ++VarNum) {
                    auto const &pasBranchCompMeter = pasBranchComp.MeteredVar(VarNum);
                    CompMode = pasBranchCompMeter.EndUse_CompMode;
                    CompEnergyUse = pasBranchCompMeter.CurMeterReading;
                    EnergyType = pasBranchCompMeter.ResourceType;
                    CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                }

                for (SubCompNum = 1; SubCompNum <= pasBranchComp.NumSubComps; ++SubCompNum) {
                    auto const &pasBranchSubComp = pasBranchComp.SubComp(SubCompNum);
                    InletNodeNum = pasBranchSubComp.NodeNumIn;
                    OutletNodeNum = pasBranchSubComp.NodeNumOut;
                    if (InletNodeNum <= 0 || OutletNodeNum <= 0) continue;
                    CompLoad = Node(OutletNodeNum).MassFlowRate * (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) -
                                                                   PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat));
                    CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                    CompEnergyUse = 0.0;
                    EnergyType = DataGlobalConstants::ResourceType::None;
                    CompLoadFlag = true;
                    CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchSubComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                    CompLoadFlag = false;
                    for (VarNum = 1; VarNum <= pasBranchSubComp.NumMeteredVars; ++VarNum) {
                        auto const &pasBranchSubCompMeter = pasBranchSubComp.MeteredVar(VarNum);
                        CompMode = pasBranchSubCompMeter.EndUse_CompMode;
                        CompEnergyUse = pasBranchSubCompMeter.CurMeterReading;
                        EnergyType = pasBranchSubCompMeter.ResourceType;
                        CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchSubComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                    }

                    for (SubSubCompNum = 1; SubSubCompNum <= pasBranchSubComp.NumSubSubComps; ++SubSubCompNum) {
                        auto const &pasBranchSubSubComp = pasBranchSubComp.SubSubComp(SubSubCompNum);
                        InletNodeNum = pasBranchSubSubComp.NodeNumIn;
                        OutletNodeNum = pasBranchSubSubComp.NodeNumOut;
                        if (InletNodeNum <= 0 || OutletNodeNum <= 0) continue;
                        CompLoad = Node(OutletNodeNum).MassFlowRate * (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) -
                                                                       PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat));
                        CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                        CompEnergyUse = 0.0;
                        EnergyType = DataGlobalConstants::ResourceType::None;
                        CompLoadFlag = true;
                        CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchSubSubComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                        CompLoadFlag = false;
                        for (VarNum = 1; VarNum <= pasBranchSubSubComp.NumMeteredVars; ++VarNum) {
                            auto const &pasBranchSubSubCompMeter = pasBranchSubSubComp.MeteredVar(VarNum);
                            CompMode = pasBranchSubSubCompMeter.EndUse_CompMode;
                            CompEnergyUse = pasBranchSubSubCompMeter.CurMeterReading;
                            EnergyType = pasBranchSubSubCompMeter.ResourceType;
                            CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, pasBranchSubSubComp.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                        }
                    }
                }
            }
        }
    }

    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        auto const &zecCtrlZone = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum);
        if (!zecCtrlZone.IsControlled) continue;

        // retrieve the zone load for each zone
        ActualZoneNum = zecCtrlZone.ActualZoneNum;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).TotalOutputRequired;

        // if system operating in deadband reset zone load
        if (state.dataZoneEnergyDemand->DeadBandOrSetback(ActualZoneNum)) ZoneLoad = 0.0;

        // loop over the zone supply air path inlet nodes
        for (ZoneInNum = 1; ZoneInNum <= zecCtrlZone.NumInletNodes; ++ZoneInNum) {
            // retrieve air loop indexes
            AirLoopNum = zecCtrlZone.InletNodeAirLoopNum(ZoneInNum);
            if (AirLoopNum == 0) continue;

            // Zone cooling load - this will double count if there is more than one airloop serving the same zone - but not sure how to apportion
            if (ZoneLoad < -SmallLoad) {
                state.dataSysRpts->SysTotZoneLoadCLNG(AirLoopNum) += std::abs(ZoneLoad);

                // Zone heating load
            } else if (ZoneLoad > SmallLoad) {
                state.dataSysRpts->SysTotZoneLoadHTNG(AirLoopNum) += std::abs(ZoneLoad);
            }
            auto const &zecCtrlZoneCool = zecCtrlZone.AirDistUnitCool(ZoneInNum);
            auto const &zecCtrlZoneHeat = zecCtrlZone.AirDistUnitHeat(ZoneInNum);

            AirDistCoolInletNodeNum = max(zecCtrlZoneCool.InNode, 0);
            AirDistHeatInletNodeNum = max(zecCtrlZoneHeat.InNode, 0);

            // Set for cooling or heating path
            if (AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum == 0) {
                ADUCoolFlowrate = max(Node(zecCtrlZoneCool.InNode).MassFlowRate, 0.0);
            } else if (AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum == 0) {
                ADUHeatFlowrate = max(Node(zecCtrlZoneHeat.InNode).MassFlowRate, 0.0);
            } else {
                ADUCoolFlowrate = 0.0;
                ADUHeatFlowrate = 0.0;
            }

            EquipListNum = zecCtrlZone.EquipListIndex;
            auto const &zel = state.dataZoneEquip->ZoneEquipList(EquipListNum);

            for (Idx = 1; Idx <= 2; ++Idx) {
                if (Idx == 1) {
                    ADUCoolNum = max(zecCtrlZoneCool.AirDistUnitIndex, 0);
                    if (ADUCoolNum == 0) continue;
                    ADUNum = ADUCoolNum;
                } else { //(Idx =2)THEN
                    ADUHeatNum = max(zecCtrlZoneHeat.AirDistUnitIndex, 0);
                    if (ADUHeatNum == 0) continue;
                    ADUNum = ADUHeatNum;
                }

                auto const &zelEquipData = zel.EquipData(ADUNum);

                CompLoad = 0.0;
                if (zelEquipData.NumInlets > 0) {
                    for (nodes = 1; nodes <= zelEquipData.NumInlets; ++nodes) {
                        InletNodeNum = zelEquipData.InletNodeNums(Idx);
                        CompLoad += (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) * Node(InletNodeNum).MassFlowRate);
                    }
                    for (nodes = 1; nodes <= zelEquipData.NumOutlets; ++nodes) {
                        OutletNodeNum = zelEquipData.OutletNodeNums(Idx);
                        CompLoad -= (PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat) * Node(OutletNodeNum).MassFlowRate);
                    }
                }
                CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                CompEnergyUse = 0.0;
                EnergyType = DataGlobalConstants::ResourceType::None;
                CompLoadFlag = true;
                CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                CompLoadFlag = false;
                for (VarNum = 1; VarNum <= zelEquipData.NumMeteredVars; ++VarNum) {
                    CompEnergyUse = zelEquipData.MeteredVar(VarNum).CurMeterReading;
                    EnergyType = zelEquipData.MeteredVar(VarNum).ResourceType;
                    CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                }

                for (SubCompNum = 1; SubCompNum <= zelEquipData.NumSubEquip; ++SubCompNum) {
                    auto const &zelSubEquipData = zelEquipData.SubEquipData(SubCompNum);
                    InletNodeNum = zelSubEquipData.InletNodeNum;
                    OutletNodeNum = zelSubEquipData.OutletNodeNum;
                    if (InletNodeNum <= 0 || OutletNodeNum <= 0) continue;
                    CompLoad = Node(InletNodeNum).MassFlowRate * (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) -
                                                                  PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat));
                    CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                    CompEnergyUse = 0.0;
                    EnergyType = DataGlobalConstants::ResourceType::None;
                    CompLoadFlag = true;
                    CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelSubEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                    CompLoadFlag = false;
                    for (VarNum = 1; VarNum <= zelSubEquipData.NumMeteredVars; ++VarNum) {
                        CompEnergyUse = zelSubEquipData.MeteredVar(VarNum).CurMeterReading;
                        EnergyType = zelSubEquipData.MeteredVar(VarNum).ResourceType;
                        CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelSubEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                    }

                    for (SubSubCompNum = 1; SubSubCompNum <= zelSubEquipData.NumSubSubEquip; ++SubSubCompNum) {
                        auto const &zelSubSubEquipData = zelSubEquipData.SubSubEquipData(SubSubCompNum);
                        InletNodeNum = zelSubSubEquipData.InletNodeNum;
                        OutletNodeNum = zelSubSubEquipData.OutletNodeNum;
                        if (InletNodeNum <= 0 || OutletNodeNum <= 0) continue;
                        CompLoad = Node(InletNodeNum).MassFlowRate * (PsyHFnTdbW(Node(InletNodeNum).Temp, Node(InletNodeNum).HumRat) -
                                                                      PsyHFnTdbW(Node(OutletNodeNum).Temp, Node(OutletNodeNum).HumRat));
                        CompLoad *= state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                        CompEnergyUse = 0.0;
                        EnergyType = DataGlobalConstants::ResourceType::None;
                        CompLoadFlag = true;
                        CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelSubSubEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                        CompLoadFlag = false;
                        for (VarNum = 1; VarNum <= zelSubSubEquipData.NumMeteredVars; ++VarNum) {
                            CompEnergyUse = zelSubSubEquipData.MeteredVar(VarNum).CurMeterReading;
                            EnergyType = zelSubSubEquipData.MeteredVar(VarNum).ResourceType;
                            CalcSystemEnergyUse(state, CompLoadFlag, AirLoopNum, zelSubSubEquipData.TypeOf, EnergyType, CompLoad, CompEnergyUse);
                        }
                    } // SubSubCompNum
                }     // SubCompNum
            }         // Idx
        }             // ZoneInNum
    }                 // Controlled Zone Loop

    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        state.dataSysRpts->SysTotHTNG(AirLoopNum) = state.dataSysRpts->SysFANCompHTNG(AirLoopNum) + state.dataSysRpts->SysHCCompHTNG(AirLoopNum) +
                                                    state.dataSysRpts->SysHeatExHTNG(AirLoopNum) + state.dataSysRpts->SysHumidHTNG(AirLoopNum) +
                                                    state.dataSysRpts->SysSolarCollectHeating(AirLoopNum) +
                                                    state.dataSysRpts->SysUserDefinedTerminalHeating(AirLoopNum);
        state.dataSysRpts->SysTotCLNG(AirLoopNum) = state.dataSysRpts->SysCCCompCLNG(AirLoopNum) + state.dataSysRpts->SysHeatExCLNG(AirLoopNum) +
                                                    state.dataSysRpts->SysEvapCLNG(AirLoopNum) + state.dataSysRpts->DesDehumidCLNG(AirLoopNum) +
                                                    state.dataSysRpts->SysSolarCollectCooling(AirLoopNum) +
                                                    state.dataSysRpts->SysUserDefinedTerminalCooling(AirLoopNum);
        state.dataSysRpts->SysTotElec(AirLoopNum) = state.dataSysRpts->SysFANCompElec(AirLoopNum) + state.dataSysRpts->SysHCCompElec(AirLoopNum) +
                                                    state.dataSysRpts->SysCCCompElec(AirLoopNum) + state.dataSysRpts->SysHCCompElecRes(AirLoopNum) +
                                                    state.dataSysRpts->SysHumidElec(AirLoopNum) + state.dataSysRpts->DesDehumidElec(AirLoopNum) +
                                                    state.dataSysRpts->SysEvapElec(AirLoopNum);
        state.dataSysRpts->SysTotNaturalGas(AirLoopNum) =
            state.dataSysRpts->SysHCCompNaturalGas(AirLoopNum) + state.dataSysRpts->SysHumidNaturalGas(AirLoopNum);
        state.dataSysRpts->SysTotPropane(AirLoopNum) =
            state.dataSysRpts->SysHCCompPropane(AirLoopNum) + state.dataSysRpts->SysHumidPropane(AirLoopNum);
        state.dataSysRpts->SysTotSteam(AirLoopNum) = state.dataSysRpts->SysHCCompSteam(AirLoopNum);
        state.dataSysRpts->SysTotH2OCOLD(AirLoopNum) = state.dataSysRpts->SysCCCompH2OCOLD(AirLoopNum);
        state.dataSysRpts->SysTotH2OHOT(AirLoopNum) = state.dataSysRpts->SysHCCompH2OHOT(AirLoopNum);
    }
}

void CalcSystemEnergyUse(EnergyPlusData &state,
                         bool const CompLoadFlag,
                         int const AirLoopNum,
                         std::string const &CompType,
                         DataGlobalConstants::ResourceType const EnergyType,
                         Real64 const CompLoad,
                         Real64 const CompEnergy)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Nov. 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // accumulate system loads and energy to report variables

    // Using/Aliasing
    using Psychrometrics::PsyHFnTdbW;
    using namespace DataZoneEnergyDemands;
    using namespace DataGlobalConstants;

    // Tuned String comparisons were a big performance hit
    // ComponentTypes and component_strings must remain in sync
    enum ComponentTypes
    { // Using older enum style to avoid the name scoping cruft
        AIRLOOPHVAC_OUTDOORAIRSYSTEM,
        AIRLOOPHVAC_UNITARY_FURNACE_HEATCOOL,
        AIRLOOPHVAC_UNITARY_FURNACE_HEATONLY,
        AIRLOOPHVAC_UNITARYHEATCOOL,
        AIRLOOPHVAC_UNITARYHEATCOOL_VAVCHANGEOVERBYPASS,
        AIRLOOPHVAC_UNITARYHEATONLY,
        AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR,
        AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR_MULTISPEED,
        AIRLOOPHVAC_UNITARYHEATPUMP_WATERTOAIR,
        AIRLOOPHVAC_UNITARYSYSTEM,
        AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_COOL,
        AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_HEAT,
        AIRTERMINAL_DUALDUCT_VAV_COOL,
        AIRTERMINAL_DUALDUCT_VAV_HEAT,
        AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_OUTDOORAIR,
        AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_RECIRCULATEDAIR,
        AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_COOLEDBEAM,
        AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEBEAM,
        AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEINDUCTION,
        AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_REHEAT,
        AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_NOREHEAT,
        AIRTERMINAL_SINGLEDUCT_MIXER,
        AIRTERMINAL_SINGLEDUCT_PARALLELPIU_REHEAT,
        AIRTERMINAL_SINGLEDUCT_SERIESPIU_REHEAT,
        AIRTERMINAL_SINGLEDUCT_USERDEFINED,
        AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_NOREHEAT,
        AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_REHEAT,
        AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT,
        AIRTERMINAL_SINGLEDUCT_VAV_REHEAT,
        AIRTERMINAL_SINGLEDUCT_VAV_REHEAT_VARIABLESPEEDFAN,
        COIL_COOLING_DX,
        COIL_COOLING_DX_MULTISPEED,
        COIL_COOLING_DX_SINGLESPEED,
        COIL_COOLING_DX_SINGLESPEED_THERMALSTORAGE,
        COIL_COOLING_DX_TWOSPEED,
        COIL_COOLING_DX_TWOSTAGEWITHHUMIDITYCONTROLMODE,
        COIL_COOLING_DX_VARIABLESPEED,
        COIL_INTEGRATED_DX_VARIABLESPEED,
        COIL_COOLING_WATER,
        COIL_COOLING_WATER_DETAILEDGEOMETRY,
        COIL_COOLING_WATERTOAIRHEATPUMP_EQUATIONFIT,
        COIL_COOLING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION,
        COIL_COOLING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT,
        COIL_HEATING_DESUPERHEATER,
        COIL_HEATING_DX_MULTISPEED,
        COIL_HEATING_DX_SINGLESPEED,
        COIL_HEATING_DX_VARIABLESPEED,
        COIL_HEATING_ELECTRIC,
        COIL_HEATING_ELECTRIC_MULTISTAGE,
        COIL_HEATING_GAS,
        COIL_HEATING_GAS_MULTISTAGE,
        COIL_HEATING_STEAM,
        COIL_HEATING_WATER,
        COIL_HEATING_WATERTOAIRHEATPUMP_EQUATIONFIT,
        COIL_HEATING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION,
        COIL_HEATING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT,
        COIL_WATERHEATING_AIRTOWATERHEATPUMP_VARIABLESPEED,
        COIL_USERDEFINED,
        COILSYSTEM_COOLING_DX,
        COILSYSTEM_COOLING_DX_HEATEXCHANGERASSISTED,
        COILSYSTEM_COOLING_WATER_HEATEXCHANGERASSISTED,
        COILSYSTEM_HEATING_DX,
        DEHUMIDIFIER_DESICCANT_NOFANS,
        DEHUMIDIFIER_DESICCANT_SYSTEM,
        DUCT,
        EVAPORATIVECOOLER_DIRECT_CELDEKPAD,
        EVAPORATIVECOOLER_DIRECT_RESEARCHSPECIAL,
        EVAPORATIVECOOLER_INDIRECT_CELDEKPAD,
        EVAPORATIVECOOLER_INDIRECT_RESEARCHSPECIAL,
        EVAPORATIVECOOLER_INDIRECT_WETCOIL,
        FAN_COMPONENTMODEL,
        FAN_SYSTEMMODEL,
        FAN_CONSTANTVOLUME,
        FAN_ONOFF,
        FAN_VARIABLEVOLUME,
        HEATEXCHANGER_AIRTOAIR_FLATPLATE,
        HEATEXCHANGER_AIRTOAIR_SENSIBLEANDLATENT,
        HEATEXCHANGER_DESICCANT_BALANCEDFLOW,
        HUMIDIFIER_STEAM_ELECTRIC,
        HUMIDIFIER_STEAM_GAS,
        OUTDOORAIR_MIXER,
        SOLARCOLLECTOR_FLATPLATE_PHOTOVOLTAICTHERMAL,
        SOLARCOLLECTOR_UNGLAZEDTRANSPIRED,
        ZONEHVAC_AIRDISTRIBUTIONUNIT,
        ZONEHVAC_TERMINALUNIT_VRF,
        COIL_COOLING_VRF,
        COIL_HEATING_VRF,
        COIL_COOLING_VRF_FTC,
        COIL_HEATING_VRF_FTC,
        n_ComponentTypes,
        Unknown_ComponentType
    };

    static std::unordered_map<std::string, ComponentTypes> const component_map = {
        {"AIRLOOPHVAC:OUTDOORAIRSYSTEM", AIRLOOPHVAC_OUTDOORAIRSYSTEM},
        {"AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL", AIRLOOPHVAC_UNITARY_FURNACE_HEATCOOL},
        {"AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY", AIRLOOPHVAC_UNITARY_FURNACE_HEATONLY},
        {"AIRLOOPHVAC:UNITARYHEATCOOL", AIRLOOPHVAC_UNITARYHEATCOOL},
        {"AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS", AIRLOOPHVAC_UNITARYHEATCOOL_VAVCHANGEOVERBYPASS},
        {"AIRLOOPHVAC:UNITARYHEATONLY", AIRLOOPHVAC_UNITARYHEATONLY},
        {"AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR", AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR},
        {"AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED", AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR_MULTISPEED},
        {"AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR", AIRLOOPHVAC_UNITARYHEATPUMP_WATERTOAIR},
        {"AIRLOOPHVAC:UNITARYSYSTEM", AIRLOOPHVAC_UNITARYSYSTEM},
        {"AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:COOL", AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_COOL},
        {"AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:HEAT", AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_HEAT},
        {"AIRTERMINAL:DUALDUCT:VAV:COOL", AIRTERMINAL_DUALDUCT_VAV_COOL},
        {"AIRTERMINAL:DUALDUCT:VAV:HEAT", AIRTERMINAL_DUALDUCT_VAV_HEAT},
        {"AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:OUTDOORAIR", AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_OUTDOORAIR},
        {"AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:RECIRCULATEDAIR", AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_RECIRCULATEDAIR},
        {"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM", AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_COOLEDBEAM},
        {"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEBEAM", AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEBEAM},
        {"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEINDUCTION", AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEINDUCTION},
        {"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:REHEAT", AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_REHEAT},
        {"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:NOREHEAT", AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_NOREHEAT},
        {"AIRTERMINAL:SINGLEDUCT:MIXER", AIRTERMINAL_SINGLEDUCT_MIXER},
        {"AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT", AIRTERMINAL_SINGLEDUCT_PARALLELPIU_REHEAT},
        {"AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT", AIRTERMINAL_SINGLEDUCT_SERIESPIU_REHEAT},
        {"AIRTERMINAL:SINGLEDUCT:USERDEFINED", AIRTERMINAL_SINGLEDUCT_USERDEFINED},
        {"AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:NOREHEAT", AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_NOREHEAT},
        {"AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:REHEAT", AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_REHEAT},
        {"AIRTERMINAL:SINGLEDUCT:VAV:NOREHEAT", AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT},
        {"AIRTERMINAL:SINGLEDUCT:VAV:REHEAT", AIRTERMINAL_SINGLEDUCT_VAV_REHEAT},
        {"AIRTERMINAL:SINGLEDUCT:VAV:REHEAT:VARIABLESPEEDFAN", AIRTERMINAL_SINGLEDUCT_VAV_REHEAT_VARIABLESPEEDFAN},
        {"COIL:COOLING:DX", COIL_COOLING_DX},
        {"COIL:COOLING:DX:MULTISPEED", COIL_COOLING_DX_MULTISPEED},
        {"COIL:COOLING:DX:SINGLESPEED", COIL_COOLING_DX_SINGLESPEED},
        {"COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE", COIL_COOLING_DX_SINGLESPEED_THERMALSTORAGE},
        {"COIL:COOLING:DX:TWOSPEED", COIL_COOLING_DX_TWOSPEED},
        {"COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE", COIL_COOLING_DX_TWOSTAGEWITHHUMIDITYCONTROLMODE},
        {"COIL:COOLING:DX:VARIABLESPEED", COIL_COOLING_DX_VARIABLESPEED},
        {"COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", COIL_INTEGRATED_DX_VARIABLESPEED},
        {"COIL:COOLING:WATER", COIL_COOLING_WATER},
        {"COIL:COOLING:WATER:DETAILEDGEOMETRY", COIL_COOLING_WATER_DETAILEDGEOMETRY},
        {"COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT", COIL_COOLING_WATERTOAIRHEATPUMP_EQUATIONFIT},
        {"COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", COIL_COOLING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION},
        {"COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", COIL_COOLING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT},
        {"COIL:HEATING:DESUPERHEATER", COIL_HEATING_DESUPERHEATER},
        {"COIL:HEATING:DX:MULTISPEED", COIL_HEATING_DX_MULTISPEED},
        {"COIL:HEATING:DX:SINGLESPEED", COIL_HEATING_DX_SINGLESPEED},
        {"COIL:HEATING:DX:VARIABLESPEED", COIL_HEATING_DX_VARIABLESPEED},
        {"COIL:HEATING:ELECTRIC", COIL_HEATING_ELECTRIC},
        {"COIL:HEATING:ELECTRIC:MULTISTAGE", COIL_HEATING_ELECTRIC_MULTISTAGE},
        {"COIL:HEATING:FUEL", COIL_HEATING_GAS},
        {"COIL:HEATING:GAS:MULTISTAGE", COIL_HEATING_GAS_MULTISTAGE},
        {"COIL:HEATING:STEAM", COIL_HEATING_STEAM},
        {"COIL:HEATING:WATER", COIL_HEATING_WATER},
        {"COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT", COIL_HEATING_WATERTOAIRHEATPUMP_EQUATIONFIT},
        {"COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", COIL_HEATING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION},
        {"COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", COIL_HEATING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT},
        {"COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", COIL_WATERHEATING_AIRTOWATERHEATPUMP_VARIABLESPEED},
        {"COIL:USERDEFINED", COIL_USERDEFINED},
        {"COILSYSTEM:COOLING:DX", COILSYSTEM_COOLING_DX},
        {"COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED", COILSYSTEM_COOLING_DX_HEATEXCHANGERASSISTED},
        {"COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED", COILSYSTEM_COOLING_WATER_HEATEXCHANGERASSISTED},
        {"COILSYSTEM:HEATING:DX", COILSYSTEM_HEATING_DX},
        {"DEHUMIDIFIER:DESICCANT:NOFANS", DEHUMIDIFIER_DESICCANT_NOFANS},
        {"DEHUMIDIFIER:DESICCANT:SYSTEM", DEHUMIDIFIER_DESICCANT_SYSTEM},
        {"DUCT", DUCT},
        {"EVAPORATIVECOOLER:DIRECT:CELDEKPAD", EVAPORATIVECOOLER_DIRECT_CELDEKPAD},
        {"EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL", EVAPORATIVECOOLER_DIRECT_RESEARCHSPECIAL},
        {"EVAPORATIVECOOLER:INDIRECT:CELDEKPAD", EVAPORATIVECOOLER_INDIRECT_CELDEKPAD},
        {"EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL", EVAPORATIVECOOLER_INDIRECT_RESEARCHSPECIAL},
        {"EVAPORATIVECOOLER:INDIRECT:WETCOIL", EVAPORATIVECOOLER_INDIRECT_WETCOIL},
        {"FAN:COMPONENTMODEL", FAN_COMPONENTMODEL},
        {"FAN:SYSTEMMODEL", FAN_SYSTEMMODEL},
        {"FAN:CONSTANTVOLUME", FAN_CONSTANTVOLUME},
        {"FAN:ONOFF", FAN_ONOFF},
        {"FAN:VARIABLEVOLUME", FAN_VARIABLEVOLUME},
        {"HEATEXCHANGER:AIRTOAIR:FLATPLATE", HEATEXCHANGER_AIRTOAIR_FLATPLATE},
        {"HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT", HEATEXCHANGER_AIRTOAIR_SENSIBLEANDLATENT},
        {"HEATEXCHANGER:DESICCANT:BALANCEDFLOW", HEATEXCHANGER_DESICCANT_BALANCEDFLOW},
        {"HUMIDIFIER:STEAM:ELECTRIC", HUMIDIFIER_STEAM_ELECTRIC},
        {"HUMIDIFIER:STEAM:GAS", HUMIDIFIER_STEAM_GAS},
        {"OUTDOORAIR:MIXER", OUTDOORAIR_MIXER},
        {"SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL", SOLARCOLLECTOR_FLATPLATE_PHOTOVOLTAICTHERMAL},
        {"SOLARCOLLECTOR:UNGLAZEDTRANSPIRED", SOLARCOLLECTOR_UNGLAZEDTRANSPIRED},
        {"ZONEHVAC:AIRDISTRIBUTIONUNIT", ZONEHVAC_AIRDISTRIBUTIONUNIT},
        {"ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW", ZONEHVAC_TERMINALUNIT_VRF},
        {"COIL:COOLING:DX:VARIABLEREFRIGERANTFLOW", COIL_COOLING_VRF},
        {"COIL:HEATING:DX:VARIABLEREFRIGERANTFLOW", COIL_HEATING_VRF},
        {"COIL:COOLING:DX:VARIABLEREFRIGERANTFLOW:FLUIDTEMPERATURECONTROL", COIL_COOLING_VRF_FTC},
        {"COIL:HEATING:DX:VARIABLEREFRIGERANTFLOW:FLUIDTEMPERATURECONTROL", COIL_HEATING_VRF_FTC}};
    assert(component_map.size() == n_ComponentTypes);

    int found;

    if (!state.dataSysRpts->AirLoopLoadsReportEnabled) return;

    // Find enum for the component type string
    ComponentTypes comp_type;
    auto const it = component_map.find(CompType);
    if (it != component_map.end()) {
        comp_type = it->second;
    } else {
        comp_type = Unknown_ComponentType;
    }

    switch (comp_type) {
    case AIRLOOPHVAC_OUTDOORAIRSYSTEM: // Outside Air System
        if (CompLoadFlag) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysOALoadCLNG(AirLoopNum) += std::abs(CompLoad);
            } else {
                state.dataSysRpts->SysOALoadHTNG(AirLoopNum) += std::abs(CompLoad);
            }
        }
        break;
    case OUTDOORAIR_MIXER: // Outdoor Air Mixer
        // No energy transfers to account for
        break;
    case AIRTERMINAL_SINGLEDUCT_MIXER:
        // No energy transfers to account for
        break;
    case FAN_CONSTANTVOLUME:
    case FAN_VARIABLEVOLUME:
    case FAN_ONOFF:
    case FAN_SYSTEMMODEL:
    case FAN_COMPONENTMODEL:

        if (CompLoadFlag) state.dataSysRpts->SysFANCompHTNG(AirLoopNum) += std::abs(CompLoad);
        state.dataSysRpts->SysFANCompElec(AirLoopNum) += CompEnergy;

        // Cooling Coil Types for the air sys simulation
        break;
    case COILSYSTEM_COOLING_DX_HEATEXCHANGERASSISTED:
    case COIL_COOLING_DX_SINGLESPEED:
    case COIL_COOLING_DX_TWOSPEED:
    case COIL_COOLING_DX_TWOSTAGEWITHHUMIDITYCONTROLMODE:
    case COIL_COOLING_DX:
    case COIL_COOLING_DX_MULTISPEED:
    case COIL_COOLING_WATERTOAIRHEATPUMP_EQUATIONFIT:
    case COIL_COOLING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION:
    case COIL_COOLING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT:
    case COIL_COOLING_DX_VARIABLESPEED:
    case COILSYSTEM_COOLING_WATER_HEATEXCHANGERASSISTED:
    case COIL_COOLING_WATER_DETAILEDGEOMETRY:
    case COIL_COOLING_WATER:
    case COIL_COOLING_DX_SINGLESPEED_THERMALSTORAGE:
    case COIL_COOLING_VRF:
    case COIL_COOLING_VRF_FTC:
    case COIL_WATERHEATING_AIRTOWATERHEATPUMP_VARIABLESPEED:

        if (CompLoadFlag) state.dataSysRpts->SysCCCompCLNG(AirLoopNum) += std::abs(CompLoad);
        if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopCoolingDemand) ||
            (EnergyType == DataGlobalConstants::ResourceType::DistrictCooling)) {
            state.dataSysRpts->SysCCCompH2OCOLD(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->SysCCCompElec(AirLoopNum) += CompEnergy;
        }

        // Heating Coil Types for the air sys simulation
        break;
    case COIL_HEATING_WATER:
    case COIL_HEATING_DX_SINGLESPEED:
    case COIL_HEATING_DX_MULTISPEED:
    case COIL_HEATING_WATERTOAIRHEATPUMP_EQUATIONFIT:
    case COIL_HEATING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION:
    case COIL_HEATING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT:
    case COIL_HEATING_DX_VARIABLESPEED:
    case COIL_HEATING_STEAM:
    case COIL_HEATING_GAS:
    case COIL_HEATING_GAS_MULTISTAGE:
    case COIL_HEATING_DESUPERHEATER:

        if (CompLoadFlag) state.dataSysRpts->SysHCCompHTNG(AirLoopNum) += std::abs(CompLoad);
        if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopHeatingDemand) ||
            (EnergyType == DataGlobalConstants::ResourceType::DistrictHeating)) {
            state.dataSysRpts->SysHCCompH2OHOT(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Steam) {
            state.dataSysRpts->SysHCCompSteam(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->SysHCCompElec(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Natural_Gas) {
            state.dataSysRpts->SysHCCompNaturalGas(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Propane) {
            state.dataSysRpts->SysHCCompPropane(AirLoopNum) += CompEnergy;
        }

        break;
    case COIL_HEATING_ELECTRIC:
    case COIL_HEATING_ELECTRIC_MULTISTAGE:

        if (CompLoadFlag) state.dataSysRpts->SysHCCompHTNG(AirLoopNum) += std::abs(CompLoad);
        if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->SysHCCompElecRes(AirLoopNum) += CompEnergy;
        }

        break;
    case COIL_USERDEFINED:

        if (CompLoadFlag) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysCCCompCLNG(AirLoopNum) += std::abs(CompLoad);
            } else {
                state.dataSysRpts->SysHCCompHTNG(AirLoopNum) += std::abs(CompLoad);
            }
        }
        if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopHeatingDemand) ||
            (EnergyType == DataGlobalConstants::ResourceType::DistrictHeating)) {
            state.dataSysRpts->SysHCCompH2OHOT(AirLoopNum) += CompEnergy;
        } else if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopCoolingDemand) ||
                   (EnergyType == DataGlobalConstants::ResourceType::DistrictCooling)) {
            state.dataSysRpts->SysCCCompH2OCOLD(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Steam) {
            state.dataSysRpts->SysHCCompSteam(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysCCCompElec(AirLoopNum) += CompEnergy;
            } else {
                state.dataSysRpts->SysHCCompElec(AirLoopNum) += CompEnergy;
            }
        } else if (EnergyType == DataGlobalConstants::ResourceType::Natural_Gas) {
            state.dataSysRpts->SysHCCompNaturalGas(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Propane) {
            state.dataSysRpts->SysHCCompPropane(AirLoopNum) += CompEnergy;
        }

        // DX Systems
        break;
    case COIL_HEATING_VRF:
    case COIL_HEATING_VRF_FTC:
    case AIRLOOPHVAC_UNITARYSYSTEM:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATPUMP_WATERTOAIR:
        // All energy transfers accounted for in subcomponent models
        break;
    case COILSYSTEM_COOLING_DX:
        // All energy transfers accounted for in subcomponent models
        break;
    case COILSYSTEM_HEATING_DX:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARY_FURNACE_HEATONLY:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARY_FURNACE_HEATCOOL:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATONLY:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATCOOL:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATCOOL_VAVCHANGEOVERBYPASS:
        // All energy transfers accounted for in subcomponent models
        break;
    case AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR_MULTISPEED:
        // All energy transfers accounted for in subcomponent models
        break;
    case ZONEHVAC_TERMINALUNIT_VRF:
        // All energy transfers accounted for in subcomponent models
        break;
        // Humidifier Types for the air system simulation
    case HUMIDIFIER_STEAM_GAS:
    case HUMIDIFIER_STEAM_ELECTRIC:
        if (CompLoadFlag) state.dataSysRpts->SysHumidHTNG(AirLoopNum) += std::abs(CompLoad);
        if (EnergyType == DataGlobalConstants::ResourceType::Water) {
            state.dataSysRpts->SysDomesticH2O(AirLoopNum) += std::abs(CompEnergy);
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->SysHumidElec(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Natural_Gas) {
            state.dataSysRpts->SysHumidNaturalGas(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Propane) {
            state.dataSysRpts->SysHumidPropane(AirLoopNum) += CompEnergy;
        }

        // Evap Cooler Types for the air system simulation
        break;
    case EVAPORATIVECOOLER_DIRECT_CELDEKPAD:
    case EVAPORATIVECOOLER_INDIRECT_CELDEKPAD:
    case EVAPORATIVECOOLER_INDIRECT_WETCOIL:
    case EVAPORATIVECOOLER_DIRECT_RESEARCHSPECIAL:
    case EVAPORATIVECOOLER_INDIRECT_RESEARCHSPECIAL:
        if (CompLoadFlag) state.dataSysRpts->SysEvapCLNG(AirLoopNum) += std::abs(CompLoad);
        if (EnergyType == DataGlobalConstants::ResourceType::Water) {
            state.dataSysRpts->SysDomesticH2O(AirLoopNum) += std::abs(CompEnergy);
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->SysEvapElec(AirLoopNum) += CompEnergy;
        }

        // Desiccant Dehumidifier Types for the air system simulation
        break;
    case DEHUMIDIFIER_DESICCANT_NOFANS:
    case DEHUMIDIFIER_DESICCANT_SYSTEM:
        if (CompLoadFlag) state.dataSysRpts->DesDehumidCLNG(AirLoopNum) += std::abs(CompLoad);
        if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            state.dataSysRpts->DesDehumidElec(AirLoopNum) += CompEnergy;
        }

        // Heat Exchanger Types
        break;
    case HEATEXCHANGER_AIRTOAIR_FLATPLATE:
    case HEATEXCHANGER_AIRTOAIR_SENSIBLEANDLATENT:
    case HEATEXCHANGER_DESICCANT_BALANCEDFLOW:
        if (CompLoadFlag) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysHeatExCLNG(AirLoopNum) += std::abs(CompLoad);
            } else {
                state.dataSysRpts->SysHeatExHTNG(AirLoopNum) += std::abs(CompLoad);
            }
        }

        // Air Terminal Types
        break;
    case AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_COOL:
    case AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_HEAT:
    case AIRTERMINAL_DUALDUCT_VAV_COOL:
    case AIRTERMINAL_DUALDUCT_VAV_HEAT:
    case AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_OUTDOORAIR:
    case AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_RECIRCULATEDAIR:
    case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEINDUCTION:
    case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_REHEAT:
    case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_NOREHEAT:
    case AIRTERMINAL_SINGLEDUCT_PARALLELPIU_REHEAT:
    case AIRTERMINAL_SINGLEDUCT_SERIESPIU_REHEAT:
    case AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_NOREHEAT:
    case AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_REHEAT:
    case AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT:
    case AIRTERMINAL_SINGLEDUCT_VAV_REHEAT:
    case AIRTERMINAL_SINGLEDUCT_VAV_REHEAT_VARIABLESPEEDFAN:
    case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_COOLEDBEAM:
    case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEBEAM:
    case ZONEHVAC_AIRDISTRIBUTIONUNIT:
        // All energy transfers accounted for in component models

        // Duct Types
        break;
    case DUCT:
        // duct losses should be accounted for here ???
        // requires addition of a new variable to sum duct losses
        // Example:
        //      IF(CompLoad > 0.0d0)THEN
        //        SysDuctHTNG(AirLoopNum) =  SysDuctHTNG(AirLoopNum) + ABS(CompLoad)
        //      ELSE
        //        SysDuctCLNG(AirLoopNum) =  SysDuctCLNG(AirLoopNum) + ABS(CompLoad)
        //      ENDIF

        // Solar Collector Types
        break;
    case SOLARCOLLECTOR_FLATPLATE_PHOTOVOLTAICTHERMAL:
    case SOLARCOLLECTOR_UNGLAZEDTRANSPIRED:
        if (CompLoadFlag) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysSolarCollectCooling(AirLoopNum) += std::abs(CompLoad);
            } else {
                state.dataSysRpts->SysSolarCollectHeating(AirLoopNum) += std::abs(CompLoad);
            }
        }

        break;
    case AIRTERMINAL_SINGLEDUCT_USERDEFINED:
        // User component model energy use should be accounted for here
        if (CompLoadFlag) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysUserDefinedTerminalCooling(AirLoopNum) += std::abs(CompLoad);
            } else {
                state.dataSysRpts->SysUserDefinedTerminalHeating(AirLoopNum) += std::abs(CompLoad);
            }
        }
        if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopHeatingDemand) ||
            (EnergyType == DataGlobalConstants::ResourceType::DistrictHeating)) {
            state.dataSysRpts->SysHCCompH2OHOT(AirLoopNum) += CompEnergy;
        } else if ((EnergyType == DataGlobalConstants::ResourceType::PlantLoopCoolingDemand) ||
                   (EnergyType == DataGlobalConstants::ResourceType::DistrictCooling)) {
            state.dataSysRpts->SysCCCompH2OCOLD(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Steam) {
            state.dataSysRpts->SysHCCompSteam(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Electricity) {
            if (CompLoad > 0.0) {
                state.dataSysRpts->SysCCCompElec(AirLoopNum) += CompEnergy;
            } else {
                state.dataSysRpts->SysHCCompElec(AirLoopNum) += CompEnergy;
            }
        } else if (EnergyType == DataGlobalConstants::ResourceType::Natural_Gas) {
            state.dataSysRpts->SysHCCompNaturalGas(AirLoopNum) += CompEnergy;
        } else if (EnergyType == DataGlobalConstants::ResourceType::Propane) {
            state.dataSysRpts->SysHCCompPropane(AirLoopNum) += CompEnergy;
        }
        // Recurring warning for unaccounted equipment types
        // (should never happen, when this does happen enter appropriate equipment CASE statement above)
        break;
    case COIL_INTEGRATED_DX_VARIABLESPEED:
        // All energy transfers accounted for in component models
        break;
    default:
        found = 0;
        if (state.dataSysRpts->NumCompTypes > 0) {
            found = UtilityRoutines::FindItemInList(
                CompType, state.dataSysRpts->CompTypeErrors, &CompTypeError::CompType, state.dataSysRpts->NumCompTypes);
        }
        if (found == 0) {
            state.dataSysRpts->CompTypeErrors(++state.dataSysRpts->NumCompTypes).CompType = CompType;
            found = state.dataSysRpts->NumCompTypes;
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcSystemEnergyUse: Component Type=" + CompType + " not logged as one of allowable Component Types.",
                                      state.dataSysRpts->CompTypeErrors(found).CompErrIndex);
        break;
    } // switch
}

void ReportMaxVentilationLoads(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher (with minor assistance from RKS)
    //       DATE WRITTEN   July 2004
    //       MODIFIED       Dec. 2006, BG. reengineered to add zone forced air units to vent rates and loads
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate and report zone ventilation loads

    // METHODOLOGY EMPLOYED:
    // calculate energy contribution of outside air through mixing box and pro-rate to
    // zones according to zone mass flow rates.

    // Using/Aliasing
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace DataZoneEnergyDemands;
    using namespace DataGlobalConstants;
    using FanCoilUnits::GetFanCoilMixedAirNode;
    using FanCoilUnits::GetFanCoilOutAirNode;
    using FanCoilUnits::GetFanCoilReturnAirNode;
    using FanCoilUnits::GetFanCoilZoneInletAirNode;
    using HVACStandAloneERV::GetStandAloneERVOutAirNode;
    using HVACStandAloneERV::GetStandAloneERVReturnAirNode;
    using HVACStandAloneERV::GetStandAloneERVZoneInletAirNode;
    using HVACVariableRefrigerantFlow::GetVRFTUMixedAirNode;
    using HVACVariableRefrigerantFlow::GetVRFTUOutAirNode;
    using HVACVariableRefrigerantFlow::GetVRFTUReturnAirNode;
    using HVACVariableRefrigerantFlow::GetVRFTUZoneInletAirNode;
    using HybridUnitaryAirConditioners::GetHybridUnitaryACOutAirNode;
    using HybridUnitaryAirConditioners::GetHybridUnitaryACReturnAirNode;
    using HybridUnitaryAirConditioners::GetHybridUnitaryACZoneInletNode;
    using PackagedTerminalHeatPump::GetPTUnitMixedAirNode;
    using PackagedTerminalHeatPump::GetPTUnitOutAirNode;
    using PackagedTerminalHeatPump::GetPTUnitReturnAirNode;
    using PackagedTerminalHeatPump::GetPTUnitZoneInletAirNode;
    using PurchasedAirManager::GetPurchasedAirMixedAirHumRat;
    using PurchasedAirManager::GetPurchasedAirMixedAirTemp;
    using PurchasedAirManager::GetPurchasedAirOutAirMassFlow;
    using PurchasedAirManager::GetPurchasedAirReturnAirNode;
    using PurchasedAirManager::GetPurchasedAirZoneInletAirNode;
    using UnitVentilator::GetUnitVentilatorMixedAirNode;
    using UnitVentilator::GetUnitVentilatorOutAirNode;
    using UnitVentilator::GetUnitVentilatorReturnAirNode;
    using UnitVentilator::GetUnitVentilatorZoneInletAirNode;
    using WindowAC::GetWindowACMixedAirNode;
    using WindowAC::GetWindowACOutAirNode;
    using WindowAC::GetWindowACReturnAirNode;
    using WindowAC::GetWindowACZoneInletAirNode;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const SmallLoad(0.1); // (W)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CtrlZoneNum;             // ZONE counter
    int ZoneInNum;               // counter for zone air distribution inlets
    int ReturnAirNode;           // node number for return node on primary air loop
    int MixedAirNode;            // mixed air node number (right after the mixing box) on primary air loop
    int AirDistCoolInletNodeNum; // Air distribution unit inlet node number
    int AirDistHeatInletNodeNum; // Air distribution unit outlet node number

    Real64 ZFAUEnthReturnAir;  // Zone forced Air unit enthalpy of the return air [kJ/kgK]
    Real64 ZFAUTempMixedAir;   // Zone forced Air unit dry-bulb temperature of the mixed air [C]
    Real64 ZFAUHumRatMixedAir; // Zone forced Air unit humidity ratio of the mixed air [kg/kg]
    Real64 ZFAUEnthMixedAir;   // Zone forced Air unit enthalpy of the mixed air [kJ/kgK]
    Real64 ZFAUEnthOutdoorAir; // Zone forced Air unit enthalpy of the outdoor air [kJ/kgK]
    Real64 ZFAUFlowRate;       // Zone forced Air unit air mass flow rate [kg/s]
    Real64 ZFAUZoneVentLoad;   // ventilation load attributed to a particular zone from zone forced air units [J]
    Real64 ZFAUOutAirFlow;     // outside air flow rate for zone from zone forced air units.
    int ZoneInletAirNode;      // Zone forced Air unit zone inlet node number

    Real64 ZoneVentLoad;          // ventilation load attributed to a particular zone
    Real64 ZoneLoad;              // ventilation load attributed to a particular zone
    Real64 OutAirFlow;            // Total outside air mass flow from zone equipment and air loop equipment [kg/s]
    Real64 ZoneFlowFrac;          // fraction of mixed air flowing to a zone
    Real64 ZoneVolume;            // Volume of zone [m3]
    Real64 currentZoneAirDensity; // current zone air density (outside barometric pressure) [kg/m3]

    int ActualZoneNum;    // Zone forced Air zone number
    int OutAirNode;       // Zone forced Air unit outdoor air node number
    int thisZoneEquipNum; // loop counter

    auto &Node(state.dataLoopNodes->Node);
    auto &TimeStepSys(state.dataHVACGlobal->TimeStepSys);

    //  CALL GetComponentEnergyUse
    if (!state.dataSysRpts->VentReportStructureCreated) return;
    if (!state.dataSysRpts->VentLoadsReportEnabled) return;
    // following inits are array assignments across all controlled zones.
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataSysRpts->ZoneOAMassFlow(zoneNum) = 0.0;
        state.dataSysRpts->ZoneOAMass(zoneNum) = 0.0;
        state.dataSysRpts->ZoneOAVolFlowStdRho(zoneNum) = 0.0;
        state.dataSysRpts->ZoneOAVolStdRho(zoneNum) = 0.0;
        state.dataSysRpts->ZoneOAVolFlowCrntRho(zoneNum) = 0.0;
        state.dataSysRpts->ZoneOAVolCrntRho(zoneNum) = 0.0;
        state.dataSysRpts->ZoneMechACH(zoneNum) = 0.0;
        state.dataSysRpts->ZoneTargetVentilationFlowVoz(zoneNum) = 0.0;
        state.dataSysRpts->ZoneTimeBelowVozDyn(zoneNum) = 0.0;
        state.dataSysRpts->ZoneTimeAtVozDyn(zoneNum) = 0.0;
    }
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataSysRpts->ZoneTimeAboveVozDyn(zoneNum) = 0.0;
        state.dataSysRpts->ZoneTimeVentUnocc(zoneNum) = 0.0;
        state.dataSysRpts->MaxCoolingLoadMetByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxCoolingLoadAddedByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxOvercoolingByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxHeatingLoadMetByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxHeatingLoadAddedByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxOverheatingByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxNoLoadHeatingByVent(zoneNum) = 0.0;
        state.dataSysRpts->MaxNoLoadCoolingByVent(zoneNum) = 0.0;
    }

    state.dataSysRpts->AnyZoneTimeBelowVozDyn = 0.0;
    state.dataSysRpts->AllZonesTimeAtVozDyn = 0.0;
    state.dataSysRpts->AnyZoneTimeAboveVozDyn = 0.0;
    state.dataSysRpts->AnyZoneTimeVentUnocc = 0.0;
    state.dataSysRpts->AnyZoneTimeBelowVozDynOcc = 0.0;
    state.dataSysRpts->AllZonesTimeAtVozDynOcc = 0.0;
    state.dataSysRpts->AnyZoneTimeAboveVozDynOcc = 0.0;

    for (int sysNum = 1; sysNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++sysNum) {
        state.dataSysRpts->SysMechVentFlow(sysNum) = 0.0;
        state.dataSysRpts->SysNatVentFlow(sysNum) = 0.0;
        state.dataSysRpts->SysTargetVentilationFlowVoz(sysNum) = 0.0;
        state.dataSysRpts->SysTimeBelowVozDyn(sysNum) = 0.0;
        state.dataSysRpts->SysTimeAtVozDyn(sysNum) = 0.0;
        state.dataSysRpts->SysTimeAboveVozDyn(sysNum) = 0.0;
        state.dataSysRpts->SysTimeVentUnocc(sysNum) = 0.0;
        state.dataSysRpts->SysAnyZoneOccupied(sysNum) = false;
    }

    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        Real64 ZAirSysZoneVentLoad = 0.0; // ventilation load attributed to a particular zone from all primary air systems serving the zone [J]
        Real64 ZAirSysOutAirFlow = 0.0;   // outside air flow rate for zone from all primary air systems serving thezone [kg/s]
        // first clear out working variables from previous zone.
        ZFAUFlowRate = 0.0;
        ZFAUZoneVentLoad = 0.0;
        ZFAUOutAirFlow = 0.0; // kg/s
        OutAirFlow = 0.0;
        ZoneFlowFrac = 0.0;

        // retrieve the zone load for each zone
        ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).TotalOutputRequired;
        ZoneVolume = state.dataHeatBal->Zone(ActualZoneNum).Volume * state.dataHeatBal->Zone(ActualZoneNum).Multiplier *
                     state.dataHeatBal->Zone(ActualZoneNum).ListMultiplier; // CR 7170

        bool const UseOccSchFlag = true;
        bool const UseMinOASchFlag = true;
        state.dataSysRpts->ZoneTargetVentilationFlowVoz(CtrlZoneNum) = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(
            state, state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneDesignSpecOAIndex, ActualZoneNum, UseOccSchFlag, UseMinOASchFlag);
        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneAirDistributionIndex > 0) {
            state.dataSysRpts->ZoneTargetVentilationFlowVoz(CtrlZoneNum) =
                state.dataSysRpts->ZoneTargetVentilationFlowVoz(CtrlZoneNum) /
                state.dataSize->ZoneAirDistribution(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneAirDistributionIndex)
                    .calculateEz(state, ActualZoneNum);
        }

        // if system operating in deadband reset zone load
        if (state.dataZoneEnergyDemand->DeadBandOrSetback(ActualZoneNum)) ZoneLoad = 0.0;

        // first deal with any (and all) Zone Forced Air Units that might have outside air.
        for (thisZoneEquipNum = 1;
             thisZoneEquipNum <= state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex).NumOfEquipTypes;
             ++thisZoneEquipNum) {
            {
                auto const SELECT_CASE_var(state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                               .EquipType_Num(thisZoneEquipNum));
                // case statement to cover all possible zone forced air units that could have outside air

                if (SELECT_CASE_var == WindowAC_Num) { // Window Air Conditioner
                    OutAirNode =
                        GetWindowACOutAirNode(state,
                                              state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                  .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = GetWindowACZoneInletAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode =
                        GetWindowACMixedAirNode(state,
                                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                    .EquipIndex(thisZoneEquipNum));
                    ReturnAirNode =
                        GetWindowACReturnAirNode(state,
                                                 state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                     .EquipIndex(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == VRFTerminalUnit_Num) {
                    OutAirNode =
                        GetVRFTUOutAirNode(state,
                                           state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                               .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;
                    ZoneInletAirNode =
                        GetVRFTUZoneInletAirNode(state,
                                                 state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                     .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode =
                        GetVRFTUMixedAirNode(state,
                                             state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                 .EquipIndex(thisZoneEquipNum));
                    ReturnAirNode =
                        GetVRFTUReturnAirNode(state,
                                              state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                  .EquipIndex(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if ((SELECT_CASE_var == PkgTermHPAirToAir_Num) || (SELECT_CASE_var == PkgTermACAirToAir_Num) ||
                           (SELECT_CASE_var == PkgTermHPWaterToAir_Num)) {
                    OutAirNode =
                        GetPTUnitOutAirNode(state,
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                .EquipIndex(thisZoneEquipNum),
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                .EquipType_Num(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode =
                        GetPTUnitZoneInletAirNode(state,
                                                  state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                      .EquipIndex(thisZoneEquipNum),
                                                  state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                      .EquipType_Num(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode =
                        GetPTUnitMixedAirNode(state,
                                              state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                  .EquipIndex(thisZoneEquipNum),
                                              state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                  .EquipType_Num(thisZoneEquipNum));
                    ReturnAirNode =
                        GetPTUnitReturnAirNode(state,
                                               state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                   .EquipIndex(thisZoneEquipNum),
                                               state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                   .EquipType_Num(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == FanCoil4Pipe_Num) {
                    OutAirNode =
                        GetFanCoilOutAirNode(state,
                                             state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                 .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = GetFanCoilZoneInletAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode =
                        GetFanCoilMixedAirNode(state,
                                               state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                   .EquipIndex(thisZoneEquipNum));
                    ReturnAirNode =
                        GetFanCoilReturnAirNode(state,
                                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                                                    .EquipIndex(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == UnitVentilator_Num) {
                    OutAirNode = GetUnitVentilatorOutAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = GetUnitVentilatorZoneInletAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode = GetUnitVentilatorMixedAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    ReturnAirNode = GetUnitVentilatorReturnAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == PurchasedAir_Num) {
                    ZFAUOutAirFlow += GetPurchasedAirOutAirMassFlow(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    ZoneInletAirNode = GetPurchasedAirZoneInletAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    ZFAUTempMixedAir = GetPurchasedAirMixedAirTemp(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    ZFAUHumRatMixedAir = GetPurchasedAirMixedAirHumRat(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    ReturnAirNode = GetPurchasedAirReturnAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if ((ZFAUFlowRate > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(ZFAUTempMixedAir, ZFAUHumRatMixedAir);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == ERVStandAlone_Num) {
                    OutAirNode = GetStandAloneERVOutAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = GetStandAloneERVZoneInletAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    MixedAirNode = ZoneInletAirNode;
                    ReturnAirNode = GetStandAloneERVReturnAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if ((MixedAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthMixedAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == ZoneUnitarySys_Num) {
                    // add accounting for OA when unitary system is used as zone equipment

                } else if (SELECT_CASE_var == OutdoorAirUnit_Num) {
                    OutAirNode = OutdoorAirUnit::GetOutdoorAirUnitOutAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = OutdoorAirUnit::GetOutdoorAirUnitZoneInletNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);
                    ReturnAirNode = OutdoorAirUnit::GetOutdoorAirUnitReturnAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if ((OutAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        ZFAUEnthOutdoorAir = PsyHFnTdbW(Node(OutAirNode).Temp, Node(OutAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthOutdoorAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == ZoneHybridEvaporativeCooler_Num) {
                    OutAirNode = GetHybridUnitaryACOutAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (OutAirNode > 0) ZFAUOutAirFlow += Node(OutAirNode).MassFlowRate;

                    ZoneInletAirNode = GetHybridUnitaryACZoneInletNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if (ZoneInletAirNode > 0) ZFAUFlowRate = max(Node(ZoneInletAirNode).MassFlowRate, 0.0);

                    ReturnAirNode = GetHybridUnitaryACReturnAirNode(
                        state,
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).EquipListIndex)
                            .EquipIndex(thisZoneEquipNum));
                    if ((OutAirNode > 0) && (ReturnAirNode > 0)) {
                        ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                        ZFAUEnthOutdoorAir = PsyHFnTdbW(Node(OutAirNode).Temp, Node(OutAirNode).HumRat);
                        // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)

                        ZFAUZoneVentLoad +=
                            (ZFAUFlowRate) * (ZFAUEnthOutdoorAir - ZFAUEnthReturnAir) * TimeStepSys * DataGlobalConstants::SecInHour; //*KJperJ
                    } else {
                        ZFAUZoneVentLoad += 0.0;
                    }

                } else if (SELECT_CASE_var == UnitHeater_Num || SELECT_CASE_var == VentilatedSlab_Num ||
                           //    ZoneHVAC:EvaporativeCoolerUnit ?????
                           SELECT_CASE_var == ZoneEvaporativeCoolerUnit_Num || SELECT_CASE_var == AirDistUnit_Num ||
                           SELECT_CASE_var == BBWaterConvective_Num || SELECT_CASE_var == BBElectricConvective_Num ||
                           SELECT_CASE_var == HiTempRadiant_Num ||
                           //    not sure how HeatExchanger:* could be used as zone equipment ?????
                           SELECT_CASE_var == LoTempRadiant_Num || SELECT_CASE_var == ZoneExhaustFan_Num || SELECT_CASE_var == HeatXchngr_Num ||
                           // HPWaterHeater can be used as zone equipment
                           SELECT_CASE_var == HPWaterHeater_Num || SELECT_CASE_var == BBWater_Num || SELECT_CASE_var == ZoneDXDehumidifier_Num ||
                           SELECT_CASE_var == BBSteam_Num || SELECT_CASE_var == BBElectric_Num || SELECT_CASE_var == RefrigerationAirChillerSet_Num ||
                           SELECT_CASE_var == UserDefinedZoneHVACForcedAir_Num || SELECT_CASE_var == CoolingPanel_Num) {
                    // do nothing, OA not included

                } else {

                    ShowFatalError(
                        state, "ReportMaxVentilationLoads: Developer must either create accounting for OA or include in final else if to do nothing");
                }
            }
        }

        // loop over the zone supply air path inlet nodes
        for (ZoneInNum = 1; ZoneInNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInNum) {
            Real64 AirSysEnthReturnAir = 0.0;    // enthalpy of the return air (mixing box inlet node, return side) [kJ/kgK]
            Real64 AirSysEnthMixedAir = 0.0;     // enthalpy of the mixed air (mixing box outlet node, mixed air side) [kJ/kgK]
            Real64 AirSysZoneVentLoad = 0.0;     // ventilation load attributed to a particular zone from primary air system [J]
            Real64 ADUCoolFlowrate = 0.0;        // Air distribution unit cooling air mass flow rate [kg/s]
            Real64 ADUHeatFlowrate = 0.0;        // Air distribution unit heating air mass flow rate [kg/s]
            Real64 AirSysTotalMixFlowRate = 0.0; // Mixed air mass flow rate [kg/s]
            Real64 AirSysOutAirFlow = 0.0;       // outside air flow rate for zone from primary air system [kg/s]
            // retrieve air loop index
            int AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum);
            MixedAirNode = 0;
            ReturnAirNode = 0;
            AirDistCoolInletNodeNum = 0;
            AirDistHeatInletNodeNum = 0;
            if (AirLoopNum != 0) { // deal with primary air system
                AirDistCoolInletNodeNum = max(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode, 0);
                AirDistHeatInletNodeNum = max(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode, 0);
                // Set for cooling or heating path
                if (AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum == 0) {
                    ADUCoolFlowrate = max(Node(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode).MassFlowRate,
                                          0.0); // CR7244 need to accumulate flow across multiple inlets
                } else if (AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum == 0) {
                    ADUHeatFlowrate = max(Node(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode).MassFlowRate,
                                          0.0); // CR7244 need to accumulate flow across multiple inlets
                } else if (AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum != AirDistHeatInletNodeNum) {
                    // dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
                    ADUHeatFlowrate = max(Node(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode).MassFlowRate,
                                          0.0); // CR7244 need to accumulate flow across multiple inlets
                    ADUCoolFlowrate = max(Node(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode).MassFlowRate,
                                          0.0); // CR7244 need to accumulate flow across multiple inlets
                } else if (AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum > 0) {
                    // dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
                    ADUCoolFlowrate = max(Node(state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode).MassFlowRate,
                                          0.0); // CR7244 need to accumulate flow across multiple inlets
                } else {
                    // do nothing (already inits)
                }
                // Find the mixed air node and return air node of the system that supplies the zone
                MixedAirNode = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OASysOutletNodeNum;
                ReturnAirNode = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OASysInletNodeNum;

                // Collect air loop Voz-dyn and natural ventilation
                int ADUNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeADUNum(ZoneInNum);
                Real64 termUnitOAFrac = 1.0;
                if (ADUNum > 0) {
                    int termUnitSizingNum = state.dataDefineEquipment->AirDistUnit(ADUNum).TermUnitSizingNum;
                    if (termUnitSizingNum > 0) {
                        termUnitOAFrac = state.dataSize->TermUnitSizing(termUnitSizingNum).SpecMinOAFrac;
                    }
                }
                state.dataSysRpts->SysTargetVentilationFlowVoz(AirLoopNum) +=
                    termUnitOAFrac * state.dataSysRpts->ZoneTargetVentilationFlowVoz(CtrlZoneNum);
                Real64 naturalVentFlow = (state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeStdDensity +
                                          state.dataHeatBal->ZonePreDefRep(ActualZoneNum).AFNVentVolStdDen) /
                                         (TimeStepSys * DataGlobalConstants::SecInHour);
                state.dataSysRpts->SysNatVentFlow(AirLoopNum) += termUnitOAFrac * naturalVentFlow;

                if (state.dataHeatBal->ZonePreDefRep(ActualZoneNum).isOccupied) {
                    state.dataSysRpts->SysAnyZoneOccupied(AirLoopNum) = true;
                }
            }

            if (MixedAirNode == 0 || ReturnAirNode == 0) {
                AirSysZoneVentLoad = 0.0;
                AirSysOutAirFlow = 0.0;
            } else {
                // Calculate return and mixed air ethalpies
                AirSysEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode).Temp, Node(ReturnAirNode).HumRat);
                AirSysEnthMixedAir = PsyHFnTdbW(Node(MixedAirNode).Temp, Node(MixedAirNode).HumRat);

                if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OASysExists) {
                    OutAirNode = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OAMixOAInNodeNum;
                    AirSysOutAirFlow = Node(OutAirNode).MassFlowRate;
                } else {
                    AirSysOutAirFlow = 0.0;
                }

                AirSysTotalMixFlowRate = Node(MixedAirNode).MassFlowRate;

                if (AirSysTotalMixFlowRate != 0.0) {
                    ZoneFlowFrac = (ADUCoolFlowrate + ADUHeatFlowrate) / AirSysTotalMixFlowRate;
                    AirSysOutAirFlow *= ZoneFlowFrac;
                } else {
                    ZoneFlowFrac = 0.0;
                    AirSysOutAirFlow = 0.0;
                }
                // Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
                AirSysZoneVentLoad = (ADUCoolFlowrate + ADUHeatFlowrate) * (AirSysEnthMixedAir - AirSysEnthReturnAir) * TimeStepSys *
                                     DataGlobalConstants::SecInHour; //*KJperJ
            }
            ZAirSysZoneVentLoad += AirSysZoneVentLoad;
            ZAirSysOutAirFlow += AirSysOutAirFlow;
        } // primary air system present

        // now combine OA flow from zone forced air units with primary air system
        OutAirFlow = ZAirSysOutAirFlow + ZFAUOutAirFlow;
        // assign report variables
        state.dataSysRpts->ZoneOAMassFlow(CtrlZoneNum) = OutAirFlow;
        state.dataSysRpts->ZoneOAMass(CtrlZoneNum) = state.dataSysRpts->ZoneOAMassFlow(CtrlZoneNum) * TimeStepSys * DataGlobalConstants::SecInHour;

        // determine volumetric values from mass flow using standard density (adjusted for elevation)
        state.dataSysRpts->ZoneOAVolFlowStdRho(CtrlZoneNum) = state.dataSysRpts->ZoneOAMassFlow(CtrlZoneNum) / state.dataEnvrn->StdRhoAir;
        state.dataSysRpts->ZoneOAVolStdRho(CtrlZoneNum) =
            state.dataSysRpts->ZoneOAVolFlowStdRho(CtrlZoneNum) * TimeStepSys * DataGlobalConstants::SecInHour;

        // set time mechanical+natural ventilation is below, at, or above target Voz-dyn
        Real64 totMechNatVentVolStdRho = state.dataSysRpts->ZoneOAVolStdRho(CtrlZoneNum) +
                                         state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeStdDensity +
                                         state.dataHeatBal->ZonePreDefRep(ActualZoneNum).AFNVentVolStdDen;
        Real64 targetVoz = state.dataSysRpts->ZoneTargetVentilationFlowVoz(CtrlZoneNum) * TimeStepSys * DataGlobalConstants::SecInHour;
        // Allow 1% tolerance
        if (totMechNatVentVolStdRho < (0.99 * targetVoz)) {
            state.dataSysRpts->ZoneTimeBelowVozDyn(CtrlZoneNum) = TimeStepSys;
            state.dataSysRpts->AnyZoneTimeBelowVozDyn = TimeStepSys;
        } else if (totMechNatVentVolStdRho > (1.01 * targetVoz)) {
            state.dataSysRpts->ZoneTimeAboveVozDyn(CtrlZoneNum) = TimeStepSys;
            state.dataSysRpts->AnyZoneTimeAboveVozDyn = TimeStepSys;
        } else if (totMechNatVentVolStdRho > SmallAirVolFlow) {
            state.dataSysRpts->ZoneTimeAtVozDyn(CtrlZoneNum) = TimeStepSys;
            state.dataSysRpts->AllZonesTimeAtVozDyn = TimeStepSys;
        }

        // determine volumetric values from mass flow using current air density for zone (adjusted for elevation)
        currentZoneAirDensity = PsyRhoAirFnPbTdbW(state,
                                                  state.dataEnvrn->OutBaroPress,
                                                  state.dataHeatBalFanSys->MAT(ActualZoneNum),
                                                  state.dataHeatBalFanSys->ZoneAirHumRatAvg(ActualZoneNum));
        if (currentZoneAirDensity > 0.0)
            state.dataSysRpts->ZoneOAVolFlowCrntRho(CtrlZoneNum) = state.dataSysRpts->ZoneOAMassFlow(CtrlZoneNum) / currentZoneAirDensity;
        state.dataSysRpts->ZoneOAVolCrntRho(CtrlZoneNum) =
            state.dataSysRpts->ZoneOAVolFlowCrntRho(CtrlZoneNum) * TimeStepSys * DataGlobalConstants::SecInHour;
        if (ZoneVolume > 0.0)
            state.dataSysRpts->ZoneMechACH(CtrlZoneNum) = (state.dataSysRpts->ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys) / ZoneVolume;

        // store data for predefined tabular report on outside air
        if (state.dataHeatBal->ZonePreDefRep(ActualZoneNum).isOccupied) {
            // accumulate the occupied time
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).TotTimeOcc += TimeStepSys;
            // mechanical ventilation
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).MechVentVolTotalOcc += state.dataSysRpts->ZoneOAVolCrntRho(CtrlZoneNum);
            if ((state.dataSysRpts->ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys) < state.dataHeatBal->ZonePreDefRep(ActualZoneNum).MechVentVolMin) {
                state.dataHeatBal->ZonePreDefRep(ActualZoneNum).MechVentVolMin = state.dataSysRpts->ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys;
            }
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).MechVentVolTotalOccStdDen += state.dataSysRpts->ZoneOAVolStdRho(CtrlZoneNum);
            // infiltration
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).InfilVolTotalOcc += state.dataHeatBal->ZnAirRpt(ActualZoneNum).InfilVolumeCurDensity;
            if (state.dataHeatBal->ZnAirRpt(ActualZoneNum).InfilVolumeCurDensity < state.dataHeatBal->ZonePreDefRep(ActualZoneNum).InfilVolMin) {
                state.dataHeatBal->ZonePreDefRep(ActualZoneNum).InfilVolMin = state.dataHeatBal->ZnAirRpt(ActualZoneNum).InfilVolumeCurDensity;
            }
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).InfilVolTotalOccStdDen +=
                state.dataHeatBal->ZnAirRpt(ActualZoneNum).InfilVolumeStdDensity;
            // 'simple' natural ventilation
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).SimpVentVolTotalOcc += state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeCurDensity;
            if (state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeCurDensity < state.dataHeatBal->ZonePreDefRep(ActualZoneNum).SimpVentVolMin) {
                state.dataHeatBal->ZonePreDefRep(ActualZoneNum).SimpVentVolMin = state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeCurDensity;
            }
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).SimpVentVolTotalOccStdDen +=
                state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeStdDensity;
            // target ventilation Voz-dyn
            state.dataSysRpts->AnyZoneTimeBelowVozDynOcc = state.dataSysRpts->AnyZoneTimeBelowVozDyn;
            state.dataSysRpts->AllZonesTimeAtVozDynOcc = state.dataSysRpts->AllZonesTimeAtVozDyn;
            state.dataSysRpts->AnyZoneTimeAboveVozDynOcc = state.dataSysRpts->AnyZoneTimeAboveVozDyn;
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTotalOcc += targetVoz;

            // time mechanical+natural ventilation is below, at, or above target Voz-dyn
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeBelowOcc += state.dataSysRpts->ZoneTimeBelowVozDyn(CtrlZoneNum);
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeAtOcc += state.dataSysRpts->ZoneTimeAtVozDyn(CtrlZoneNum);
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeAboveOcc += state.dataSysRpts->ZoneTimeAboveVozDyn(CtrlZoneNum);
        } else if (totMechNatVentVolStdRho > SmallAirVolFlow) {
            state.dataSysRpts->ZoneTimeVentUnocc(CtrlZoneNum) = TimeStepSys;
            state.dataSysRpts->AnyZoneTimeVentUnocc = TimeStepSys;
            state.dataHeatBal->ZonePreDefRep(ActualZoneNum).TotVentTimeNonZeroUnocc += state.dataSysRpts->ZoneTimeVentUnocc(CtrlZoneNum);
        }
        // accumulate during occupancy or not
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).MechVentVolTotalStdDen += state.dataSysRpts->ZoneOAVolStdRho(CtrlZoneNum);
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).InfilVolTotalStdDen += state.dataHeatBal->ZnAirRpt(ActualZoneNum).InfilVolumeStdDensity;
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).SimpVentVolTotalStdDen += state.dataHeatBal->ZnAirRpt(ActualZoneNum).VentilVolumeStdDensity;
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTotal += targetVoz;
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeBelow += state.dataSysRpts->ZoneTimeBelowVozDyn(CtrlZoneNum);
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeAt += state.dataSysRpts->ZoneTimeAtVozDyn(CtrlZoneNum);
        state.dataHeatBal->ZonePreDefRep(ActualZoneNum).VozTargetTimeAbove += state.dataSysRpts->ZoneTimeAboveVozDyn(CtrlZoneNum);

        // now combine Vent load from zone forced air units with primary air system
        ZoneVentLoad = ZAirSysZoneVentLoad + ZFAUZoneVentLoad;
        // cycle if ZoneVentLoad is small
        if (std::abs(ZoneVentLoad) < SmallLoad) continue; // orig. had RETURN here, BG changed to CYCLE for next controlled zone in do loop.

        // Ventilation Heating
        if (ZoneVentLoad > SmallLoad) {
            // Zone cooling load
            if (ZoneLoad < -SmallLoad) {
                state.dataSysRpts->MaxCoolingLoadAddedByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
                // Zone heating load
            } else if (ZoneLoad > SmallLoad) {
                if (ZoneVentLoad > ZoneLoad) {
                    state.dataSysRpts->MaxHeatingLoadMetByVent(CtrlZoneNum) += std::abs(ZoneLoad);
                    state.dataSysRpts->MaxOverheatingByVent(CtrlZoneNum) += (ZoneVentLoad - ZoneLoad);
                } else {
                    state.dataSysRpts->MaxHeatingLoadMetByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
                }
                // No Zone Load
            } else {
                state.dataSysRpts->MaxNoLoadHeatingByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
            }

            // Ventilation Cooling
        } else if (ZoneVentLoad < -SmallLoad) {
            // Zone cooling load
            if (ZoneLoad < -SmallLoad) {
                if (ZoneVentLoad < ZoneLoad) {
                    state.dataSysRpts->MaxCoolingLoadMetByVent(CtrlZoneNum) += std::abs(ZoneLoad);
                    state.dataSysRpts->MaxOvercoolingByVent(CtrlZoneNum) += std::abs(ZoneVentLoad - ZoneLoad);
                } else {
                    state.dataSysRpts->MaxCoolingLoadMetByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
                }
                // Zone heating load
            } else if (ZoneLoad > SmallLoad) {
                state.dataSysRpts->MaxHeatingLoadAddedByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
                // No Zone Load
            } else {
                state.dataSysRpts->MaxNoLoadCoolingByVent(CtrlZoneNum) += std::abs(ZoneVentLoad);
            }

            // Ventilation No Load
        } else {
        }
    } // loop over controlled zones

    // loop over air loops
    for (int sysNum = 1; sysNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++sysNum) {
        Real64 mechVentFlow = state.dataAirLoop->AirLoopFlow(sysNum).OAFlow * state.dataEnvrn->StdRhoAir;
        state.dataSysRpts->SysMechVentFlow(sysNum) = mechVentFlow;
        state.dataSysRpts->SysPreDefRep(sysNum).SysMechVentTotal += mechVentFlow * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataSysRpts->SysPreDefRep(sysNum).SysNatVentTotal +=
            state.dataSysRpts->SysNatVentFlow(sysNum) * TimeStepSys * DataGlobalConstants::SecInHour;

        // set time mechanical+natural ventilation is below, at, or above target Voz-dyn
        Real64 totMechNatVentVolFlowStdRho = mechVentFlow + state.dataSysRpts->SysNatVentFlow(sysNum);

        Real64 targetFlowVoz = state.dataSysRpts->SysTargetVentilationFlowVoz(sysNum);
        state.dataSysRpts->SysPreDefRep(sysNum).SysTargetVentTotalVoz += targetFlowVoz * TimeStepSys * DataGlobalConstants::SecInHour;
        // Allow 1% tolerance
        if (totMechNatVentVolFlowStdRho < (0.99 * targetFlowVoz)) {
            state.dataSysRpts->SysTimeBelowVozDyn(sysNum) = TimeStepSys;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeBelowVozDynTotal += TimeStepSys;
        } else if (totMechNatVentVolFlowStdRho > (1.01 * targetFlowVoz)) {
            state.dataSysRpts->SysTimeAboveVozDyn(sysNum) = TimeStepSys;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAboveVozDynTotal += TimeStepSys;
        } else if (totMechNatVentVolFlowStdRho > SmallAirVolFlow) {
            state.dataSysRpts->SysTimeAtVozDyn(sysNum) = TimeStepSys;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAtVozDynTotal += TimeStepSys;
        }

        if (state.dataSysRpts->SysAnyZoneOccupied(sysNum)) {
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeOccupiedTotal += TimeStepSys;
            state.dataSysRpts->SysPreDefRep(sysNum).SysMechVentTotalOcc += mechVentFlow * TimeStepSys * DataGlobalConstants::SecInHour;
            state.dataSysRpts->SysPreDefRep(sysNum).SysNatVentTotalOcc +=
                state.dataSysRpts->SysNatVentFlow(sysNum) * TimeStepSys * DataGlobalConstants::SecInHour;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTargetVentTotalVozOcc += targetFlowVoz * TimeStepSys * DataGlobalConstants::SecInHour;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeBelowVozDynTotalOcc += state.dataSysRpts->SysTimeBelowVozDyn(sysNum);
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAboveVozDynTotalOcc += state.dataSysRpts->SysTimeAboveVozDyn(sysNum);
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAtVozDynTotalOcc += state.dataSysRpts->SysTimeAtVozDyn(sysNum);
        } else if (totMechNatVentVolFlowStdRho > SmallAirVolFlow) {
            state.dataSysRpts->SysTimeVentUnocc(sysNum) = TimeStepSys;
            state.dataSysRpts->SysPreDefRep(sysNum).SysTimeVentUnoccTotal += TimeStepSys;
        }

        // set time at OA limiting factors
        if (mechVentFlow > SmallAirVolFlow) {
            int thisOAControlNum = state.dataAirLoop->AirLoopControlInfo(sysNum).OACtrlNum;
            if (thisOAControlNum > 0) {
                int limitFactorIndex = state.dataMixedAir->OAController(thisOAControlNum).OALimitingFactor;
                state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAtOALimit[limitFactorIndex] += TimeStepSys;
                if (state.dataSysRpts->SysAnyZoneOccupied(sysNum)) {
                    state.dataSysRpts->SysPreDefRep(sysNum).SysTimeAtOALimitOcc[limitFactorIndex] += TimeStepSys;
                    state.dataSysRpts->SysPreDefRep(sysNum).SysMechVentTotAtLimitOcc[limitFactorIndex] +=
                        mechVentFlow * TimeStepSys * DataGlobalConstants::SecInHour;
                }
            }
        }
    }
    // Accumulate facility totals
    state.dataOutRptPredefined->TotalAnyZoneBelowVozDynForOA += state.dataSysRpts->AnyZoneTimeBelowVozDyn;
    state.dataOutRptPredefined->TotalAllZonesAtVozDynForOA += state.dataSysRpts->AllZonesTimeAtVozDyn;
    state.dataOutRptPredefined->TotalAnyZoneAboveVozDynForOA += state.dataSysRpts->AnyZoneTimeAboveVozDyn;
    state.dataOutRptPredefined->TotalAnyZoneVentUnoccForOA += state.dataSysRpts->AnyZoneTimeVentUnocc;
    state.dataOutRptPredefined->TotalAnyZoneBelowVozDynOccForOA += state.dataSysRpts->AnyZoneTimeBelowVozDynOcc;
    state.dataOutRptPredefined->TotalAllZonesAtVozDynOccForOA += state.dataSysRpts->AllZonesTimeAtVozDynOcc;
    state.dataOutRptPredefined->TotalAnyZoneAboveVozDynOccForOA += state.dataSysRpts->AnyZoneTimeAboveVozDynOcc;
}

void MatchPlantSys(EnergyPlusData &state,
                   int const AirLoopNum, // counter for zone air distribution inlets
                   int const BranchNum   // counter for zone air distribution inlets
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate and report zone ventilation loads

    // METHODOLOGY EMPLOYED:
    // calculate energy contribution of outside air through mixing box and pro-rate to
    // zones according to zone mass flow rates.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataGlobalConstants;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const EnergyTrans(1);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string CompType;
    std::string CompName;
    int CompNum; // counter for components on air loop branch connected to air distribution unit
    int VarNum;
    int SubCompNum;    // counter for components on air loop branch connected to air distribution unit
    int SubSubCompNum; // counter for components on air loop branch connected to air distribution unit
    bool MatchFound;   // Set to .TRUE. when a match is found
    int MatchLoop;     // Loop number of the match
    int MatchBranch;   // Branch number of the match
    int MatchComp;     // Component number of the match
    int MatchLoopType;
    int Idx;

    for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
        {
            auto &thisComp(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum));
            for (VarNum = 1; VarNum <= thisComp.NumMeteredVars; ++VarNum) {
                if (thisComp.MeteredVar(VarNum).ResourceType == DataGlobalConstants::ResourceType::EnergyTransfer) {
                    thisComp.EnergyTransComp = EnergyTrans;
                    CompType = thisComp.TypeOf;
                    CompName = thisComp.Name;
                    Idx = 0;
                    FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                    if (MatchFound)
                        UpdateAirSysCompPtrArray(state, Idx, AirLoopNum, BranchNum, CompNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                    thisComp.AirSysToPlantPtr = Idx;
                    break;
                }
            }
            for (SubCompNum = 1; SubCompNum <= thisComp.NumSubComps; ++SubCompNum) {
                //!!!!          IF(SysVentLoad == 0.0d0)EXIT
                {
                    auto &thisSubComp(thisComp.SubComp(SubCompNum));
                    for (VarNum = 1; VarNum <= thisSubComp.NumMeteredVars; ++VarNum) {
                        if (thisSubComp.MeteredVar(VarNum).ResourceType == DataGlobalConstants::ResourceType::EnergyTransfer) {
                            thisSubComp.EnergyTransComp = EnergyTrans;
                            CompType = thisSubComp.TypeOf;
                            CompName = thisSubComp.Name;
                            Idx = 0;
                            FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                            if (MatchFound)
                                UpdateAirSysSubCompPtrArray(
                                    state, Idx, AirLoopNum, BranchNum, CompNum, SubCompNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                            thisSubComp.AirSysToPlantPtr = Idx;
                            break;
                        }
                    }
                    for (SubSubCompNum = 1; SubSubCompNum <= thisSubComp.NumSubSubComps; ++SubSubCompNum) {
                        //!!!!            IF(SysVentLoad == 0.0d0)EXIT
                        {
                            auto &thisSubSubComp(thisSubComp.SubSubComp(SubSubCompNum));
                            for (VarNum = 1; VarNum <= thisSubSubComp.NumMeteredVars; ++VarNum) {
                                if (thisSubSubComp.MeteredVar(VarNum).ResourceType == DataGlobalConstants::ResourceType::EnergyTransfer) {
                                    thisSubSubComp.EnergyTransComp = EnergyTrans;
                                    CompType = thisSubSubComp.TypeOf;
                                    CompName = thisSubSubComp.Name;
                                    Idx = 0;
                                    FindDemandSideMatch(state, CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp);
                                    if (MatchFound)
                                        UpdateAirSysSubSubCompPtrArray(state,
                                                                       Idx,
                                                                       AirLoopNum,
                                                                       BranchNum,
                                                                       CompNum,
                                                                       SubCompNum,
                                                                       SubSubCompNum,
                                                                       MatchLoopType,
                                                                       MatchLoop,
                                                                       MatchBranch,
                                                                       MatchComp);
                                    thisSubSubComp.AirSysToPlantPtr = Idx;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

void FindDemandSideMatch(EnergyPlusData &state,
                         std::string const &CompType, // Inlet node of the component to find the match of
                         std::string_view CompName, // Outlet node of the component to find the match of
                         bool &MatchFound,            // Set to .TRUE. when a match is found
                         int &MatchLoopType,          // Loop number of the match
                         int &MatchLoop,              // Loop number of the match
                         int &MatchBranch,            // Branch number of the match
                         int &MatchComp               // Component number of the match
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   September 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine intializes the connections between various loops.
    // Due to the fact that this requires numerous string compares, it
    // is much more efficient to find this information once and then
    // store it in module level variables (LoopConnect derived type).

    // METHODOLOGY EMPLOYED:
    // Simply cycles through the plant and condenser demand sides until
    // a component is found that matches the component type and name

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PassBranchNum; // DO loop counter for branches
    int PassCompNum;   // DO loop counter for components
    int PassLoopNum;   // DO loop counter for loops or the top level of the hierarchy

    // Initialize all of the output variables

    MatchFound = false;
    MatchLoopType = 0;
    MatchLoop = 0;
    MatchLoop = 0;
    MatchBranch = 0;
    MatchComp = 0;

    // Now cycle through all of the demand side loops to see if we can find
    // a match for the component type and name.  Once a match is found,
    // record the type of loop and the loop, branch, and component numbers.
    if (!MatchFound) { // Go through the plant demand side loops
        for (PassLoopNum = 1; PassLoopNum <= state.dataHVACGlobal->NumPlantLoops; ++PassLoopNum) {
            for (PassBranchNum = 1; PassBranchNum <= state.dataPlnt->VentRepPlantDemandSide(PassLoopNum).TotalBranches; ++PassBranchNum) {
                for (PassCompNum = 1; PassCompNum <= state.dataPlnt->VentRepPlantDemandSide(PassLoopNum).Branch(PassBranchNum).TotalComponents;
                     ++PassCompNum) {
                    if (UtilityRoutines::SameString(
                            CompType, state.dataPlnt->VentRepPlantDemandSide(PassLoopNum).Branch(PassBranchNum).Comp(PassCompNum).TypeOf) &&
                        UtilityRoutines::SameString(
                            CompName, state.dataPlnt->VentRepPlantDemandSide(PassLoopNum).Branch(PassBranchNum).Comp(PassCompNum).Name)) {
                        // Found a match on the plant demand side--increment the counter
                        MatchFound = true;
                        MatchLoopType = 1;
                        MatchLoop = PassLoopNum;
                        MatchBranch = PassBranchNum;
                        MatchComp = PassCompNum;
                        break; // PassCompNum DO loop
                    }
                }
                if (MatchFound) break; // PassBranchNum DO loop
            }
            if (MatchFound) break; // PassLoopNum DO loop
        }
    }

    if (!MatchFound) { // Go through the condenser demand side loops
        for (PassLoopNum = 1; PassLoopNum <= state.dataHVACGlobal->NumCondLoops; ++PassLoopNum) {
            for (PassBranchNum = 1; PassBranchNum <= state.dataPlnt->VentRepCondDemandSide(PassLoopNum).TotalBranches; ++PassBranchNum) {
                for (PassCompNum = 1; PassCompNum <= state.dataPlnt->VentRepCondDemandSide(PassLoopNum).Branch(PassBranchNum).TotalComponents;
                     ++PassCompNum) {
                    if (UtilityRoutines::SameString(
                            CompType, state.dataPlnt->VentRepCondDemandSide(PassLoopNum).Branch(PassBranchNum).Comp(PassCompNum).TypeOf) &&
                        UtilityRoutines::SameString(
                            CompName, state.dataPlnt->VentRepCondDemandSide(PassLoopNum).Branch(PassBranchNum).Comp(PassCompNum).Name)) {
                        // Found a match on the plant demand side--increment the counter
                        MatchFound = true;
                        MatchLoopType = 2;
                        MatchLoop = PassLoopNum;
                        MatchBranch = PassBranchNum;
                        MatchComp = PassCompNum;
                        break; // PassCompNum DO loop
                    }
                }
                if (MatchFound) break; // PassBranchNum DO loop
            }
            if (MatchFound) break; // PassLoopNum DO loop
        }
    }
}

void ReportAirLoopConnections(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte, Linda K. Lawrie
    //       DATE WRITTEN   February 2004 (moved from BranchInputManager ReportLoopConnections)
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Report air loop splitter connections to the BND file.

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // REFERENCES:
    // na

    // Using/Aliasing
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view errstring("**error**");

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // Formats
    static constexpr fmt::string_view Format_706("! <#AirLoopHVACs>,<Number of AirLoopHVACs>");
    static constexpr fmt::string_view Format_708(
        "! <AirLoopHVAC>,<Air Loop Name>,<# Return Nodes>,<# Supply Nodes>,<# Zones Cooled>,<# Zones Heated>,<Outdoor Air Used>");
    static constexpr fmt::string_view Format_709("! <AirLoop Return Connections>,<Connection Count>,<AirLoopHVAC Name>,<Zn Eqp Return Node #>,<Zn Eqp Return "
                                     "Node Name>,<AirLoop Return Node #>,<Air Loop Return Node Name>");
    static constexpr fmt::string_view Format_710("! <AirLoop Supply Connections>,<Connection Count>,<AirLoopHVAC Name>,<Zn Eqp Supply Node #>,<Zn Eqp Supply "
                                     "Node Name>,<AirLoop Supply Node #>,<Air Loop Supply Node Name>");
    static constexpr fmt::string_view Format_711("! <Cooled Zone Info>,<Cooled Zone Count>,<Cooled Zone Name>,<Cooled Zone Inlet Node #>,<Cooled Zone Inlet "
                                     "Node Name>,<AirLoopHVAC Name>");
    static constexpr fmt::string_view Format_712("! <Heated Zone Info>,<Heated Zone Count>,<Heated Zone Name>,<Heated Zone Inlet Node #>,<Heated Zone Inlet "
                                     "Node Name>,<AirLoopHVAC Name>");
    static constexpr fmt::string_view Format_714("! <Outdoor Air Connections>,<OA Inlet Node #>,<OA Return Air Inlet Node Name>,<OA Outlet Node #>,<OA Mixed "
                                     "Air Outlet Node Name>,<AirLoopHVAC Name>");

    auto &NodeID(state.dataLoopNodes->NodeID);

    print(state.files.bnd, "{}\n", "! ===============================================================");
    print(state.files.bnd, "{}\n", Format_706);
    print(state.files.bnd, " #AirLoopHVACs,{}\n", NumPrimaryAirSys);
    print(state.files.bnd, "{}\n", Format_708);
    print(state.files.bnd, "{}\n", Format_709);
    print(state.files.bnd, "{}\n", Format_710);
    print(state.files.bnd, "{}\n", Format_711);
    print(state.files.bnd, "{}\n", Format_712);
    print(state.files.bnd, "{}\n", Format_714);
    print(state.files.bnd, "{}\n", "! <AirLoopHVAC Connector>,<Connector Type>,<Connector Name>,<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>");
    print(state.files.bnd,
          "{}\n",
          "! <AirLoopHVAC Connector Branches>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Branch>,<Outlet Branch>,<Loop "
          "Name>,<Loop Type>");
    print(state.files.bnd,
          "{}\n",
          "! <AirLoopHVAC Connector Nodes>,<Connector Node Count>,<Connector Type>,<Connector Name>,<Inlet Node>,<Outlet Node>,<Loop Name>,<Loop "
          "Type>");
    for (int Count = 1; Count <= NumPrimaryAirSys; ++Count) {
        const auto oaSysExists = [&]() {
            if (state.dataAirLoop->AirToOANodeInfo(Count).OASysExists) {
                return "Yes";
            } else {
                return "No";
            }
        }();

        print(state.files.bnd,
              " AirLoopHVAC,{},{},{},{},{},{}\n",
              state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName,
              state.dataAirLoop->AirToZoneNodeInfo(Count).NumReturnNodes,
              state.dataAirLoop->AirToZoneNodeInfo(Count).NumSupplyNodes,
              state.dataAirLoop->AirToZoneNodeInfo(Count).NumZonesCooled,
              state.dataAirLoop->AirToZoneNodeInfo(Count).NumZonesHeated,
              oaSysExists);
        for (int Count1 = 1; Count1 <= state.dataAirLoop->AirToZoneNodeInfo(Count).NumReturnNodes; ++Count1) {
            print(state.files.bnd, "   AirLoop Return Connections,{},{},", Count1, state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipReturnNodeNum(Count1) > 0) {
                print(state.files.bnd,
                      "{},{},",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipReturnNodeNum(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipReturnNodeNum(Count1)));
            } else {
                print(state.files.bnd, "{},{},", errstring, errstring);
            }
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopReturnNodeNum(Count1) > 0) {
                print(state.files.bnd,
                      "{},{}\n",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopReturnNodeNum(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopReturnNodeNum(Count1)));
            } else {
                print(state.files.bnd, "{},{}\n", errstring, errstring);
            }
        }
        for (int Count1 = 1; Count1 <= state.dataAirLoop->AirToZoneNodeInfo(Count).NumSupplyNodes; ++Count1) {
            print(state.files.bnd, "   AirLoop Supply Connections,{},{},", Count1, state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipSupplyNodeNum(Count1) > 0) {
                print(state.files.bnd,
                      "{},{},",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipSupplyNodeNum(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).ZoneEquipSupplyNodeNum(Count1)));
            } else {
                print(state.files.bnd, "{},{},", errstring, errstring);
            }
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopSupplyNodeNum(Count1) > 0) {
                print(state.files.bnd,
                      "{},{}\n",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopSupplyNodeNum(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopSupplyNodeNum(Count1)));
            } else {
                print(state.files.bnd, "{},{}\n", errstring, errstring);
            }
        }

        for (int Count1 = 1; Count1 <= state.dataAirLoop->AirToZoneNodeInfo(Count).NumZonesCooled; ++Count1) {
            const auto CtrldZoneNum = state.dataAirLoop->AirToZoneNodeInfo(Count).CoolCtrlZoneNums(Count1);
            const auto ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrldZoneNum).ActualZoneNum;
            print(state.files.bnd, "   Cooled Zone Info,{},{},", Count1, state.dataHeatBal->Zone(ZoneNum).Name);
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).CoolZoneInletNodes(Count1) > 0) {
                print(state.files.bnd,
                      "{},{},{}\n",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).CoolZoneInletNodes(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).CoolZoneInletNodes(Count1)),
                      state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            } else {
                print(state.files.bnd, "{},{},{}\n", errstring, errstring, state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            }
        }
        for (int Count1 = 1; Count1 <= state.dataAirLoop->AirToZoneNodeInfo(Count).NumZonesHeated; ++Count1) {
            const auto CtrldZoneNum = state.dataAirLoop->AirToZoneNodeInfo(Count).HeatCtrlZoneNums(Count1);
            const auto ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrldZoneNum).ActualZoneNum;
            print(state.files.bnd, "   Heated Zone Info,{},{},", Count1, state.dataHeatBal->Zone(ZoneNum).Name);
            if (state.dataAirLoop->AirToZoneNodeInfo(Count).HeatZoneInletNodes(Count1) > 0) {
                print(state.files.bnd,
                      "{},{},{}\n",
                      state.dataAirLoop->AirToZoneNodeInfo(Count).HeatZoneInletNodes(Count1),
                      NodeID(state.dataAirLoop->AirToZoneNodeInfo(Count).HeatZoneInletNodes(Count1)),
                      state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            } else {
                print(state.files.bnd, "{},{},{}\n", errstring, errstring, state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            }
        }
        if (state.dataAirLoop->AirToOANodeInfo(Count).OASysExists) {
            std::string ChrOut;
            std::string ChrOut2;
            if (state.dataAirLoop->AirToOANodeInfo(Count).OASysInletNodeNum > 0) {
                ChrOut = fmt::to_string(state.dataAirLoop->AirToOANodeInfo(Count).OASysInletNodeNum);
            } else {
                ChrOut = errstring;
            }
            if (state.dataAirLoop->AirToOANodeInfo(Count).OASysOutletNodeNum > 0) {
                ChrOut2 = fmt::to_string(state.dataAirLoop->AirToOANodeInfo(Count).OASysOutletNodeNum);
            } else {
                ChrOut2 = errstring;
            }

            print(state.files.bnd, "   Outdoor Air Connections,{},", ChrOut);
            if (ChrOut != errstring) {
                print(state.files.bnd, "{},", NodeID(state.dataAirLoop->AirToOANodeInfo(Count).OASysInletNodeNum));
            } else {
                print(state.files.bnd, "{},", errstring);
            }
            if (ChrOut2 != errstring) {
                print(state.files.bnd,
                      "{},{},{}\n",
                      ChrOut2,
                      NodeID(state.dataAirLoop->AirToOANodeInfo(Count).OASysOutletNodeNum),
                      state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            } else {
                print(state.files.bnd, "{},{},{}\n", errstring, errstring, state.dataAirLoop->AirToZoneNodeInfo(Count).AirLoopName);
            }
        }
        //  Report HVAC Air Loop Splitter to BND file
        if (state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.Exists) {
            print(state.files.bnd,
                  "   AirLoopHVAC Connector,Splitter,{},{},Air,{}\n",
                  state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.Name,
                  state.dataAirSystemsData->PrimaryAirSystems(Count).Name,
                  state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.TotalOutletNodes);
            for (int Count1 = 1; Count1 <= state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.TotalOutletNodes; ++Count1) {
                print(state.files.bnd,
                      "     AirLoopHVAC Connector Branches,{},Splitter,{},",
                      Count1,
                      state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.Name);

                if (state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.BranchNumIn <= 0) {
                    print(state.files.bnd, "{},", errstring);
                } else {
                    print(state.files.bnd,
                          "{},",
                          state.dataAirSystemsData->PrimaryAirSystems(Count)
                              .Branch(state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.BranchNumIn)
                              .Name);
                }

                if (state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.BranchNumOut(Count1) <= 0) {
                    print(state.files.bnd, "{},{},Air\n", errstring, state.dataAirSystemsData->PrimaryAirSystems(Count).Name);
                } else {
                    print(state.files.bnd,
                          "{},{},Air\n",
                          state.dataAirSystemsData->PrimaryAirSystems(Count)
                              .Branch(state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.BranchNumOut(Count1))
                              .Name,
                          state.dataAirSystemsData->PrimaryAirSystems(Count).Name);
                }

                print(state.files.bnd,
                      "     AirLoopHVAC Connector Nodes,   {},Splitter,{},{},{},{},Air\n",
                      Count1,
                      state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.Name,
                      state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.NodeNameIn,
                      state.dataAirSystemsData->PrimaryAirSystems(Count).Splitter.NodeNameOut(Count1),
                      state.dataAirSystemsData->PrimaryAirSystems(Count).Name);
            }
        }
    }
}

//        End of Reporting subroutines for the SimAir Module
// *****************************************************************************

} // namespace EnergyPlus::SystemReports
