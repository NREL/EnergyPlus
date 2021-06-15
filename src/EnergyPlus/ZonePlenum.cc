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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus::ZonePlenum {
// Module containing simulation routines for both zone return and zone supply plenums

// MODULE INFORMATION:
//       AUTHOR         Peter Graham Ellis
//       DATE WRITTEN   November 2000
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage Air Path Zone Return Plenum Components

// METHODOLOGY EMPLOYED:
// The Zone Plenum

// Using/Aliasing
using namespace DataLoopNode;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyTdbFnHW;

// Functions

void SimAirZonePlenum(EnergyPlusData &state,
                      std::string_view CompName,
                      int const iCompType,
                      int &CompIndex,
                      Optional_bool_const FirstHVACIteration, // Autodesk:OPTIONAL Used without PRESENT check
                      Optional_bool_const FirstCall,          // Autodesk:OPTIONAL Used without PRESENT check
                      Optional_bool PlenumInletChanged        // Autodesk:OPTIONAL Used without PRESENT check
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   November 2000
    //       MODIFIED       March 2000
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the ZonePlenum component simulation for both
    // return and supply plenums.
    // It is called from the SimAirLoopComponent at the system time step.

    // Using/Aliasing
    using DataZoneEquipment::ZoneReturnPlenum_Type;
    using DataZoneEquipment::ZoneSupplyPlenum_Type;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZonePlenumNum; // The ZonePlenum that you are currently loading input into

    // Obtains and Allocates ZonePlenum related parameters from input file
    if (state.dataZonePlenum->GetInputFlag) { // First time subroutine has been entered
        GetZonePlenumInput(state);
        state.dataZonePlenum->GetInputFlag = false;
    }

    if (iCompType == ZoneReturnPlenum_Type) { // 'AirLoopHVAC:ReturnPlenum'
        // Find the correct ZonePlenumNumber
        if (CompIndex == 0) {
            ZonePlenumNum =
                UtilityRoutines::FindItemInList(CompName, state.dataZonePlenum->ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZonePlenumName);
            if (ZonePlenumNum == 0) {
                ShowFatalError(state, "SimAirZonePlenum: AirLoopHVAC:ReturnPlenum not found=" + std::string{CompName});
            }
            CompIndex = ZonePlenumNum;
        } else {
            ZonePlenumNum = CompIndex;
            if (ZonePlenumNum > state.dataZonePlenum->NumZoneReturnPlenums || ZonePlenumNum < 1) {
                ShowFatalError(
                    state,
                    format("SimAirZonePlenum: Invalid CompIndex passed={}, Number of AirLoopHVAC:ReturnPlenum={}, AirLoopHVAC:ReturnPlenum name={}",
                           ZonePlenumNum,
                           state.dataZonePlenum->NumZoneReturnPlenums,
                           CompName));
            }
            if (state.dataZonePlenum->CheckRetEquipName(ZonePlenumNum)) {
                if (CompName != state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName) {
                    ShowFatalError(state,
                                   format("SimAirZonePlenum: Invalid CompIndex passed={}, AirLoopHVAC:ReturnPlenum name={}, stored "
                                          "AirLoopHVAC:ReturnPlenum Name for that index={}",
                                          ZonePlenumNum,
                                          CompName,
                                          state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName));
                }
                state.dataZonePlenum->CheckRetEquipName(ZonePlenumNum) = false;
            }
        }

        InitAirZoneReturnPlenum(state, ZonePlenumNum); // Initialize all ZonePlenum related parameters

        CalcAirZoneReturnPlenum(state, ZonePlenumNum);

        UpdateAirZoneReturnPlenum(state, ZonePlenumNum); // Update the current ZonePlenum to the outlet nodes

    } else if (iCompType == ZoneSupplyPlenum_Type) { // 'AirLoopHVAC:SupplyPlenum'
        // Find the correct ZonePlenumNumber
        if (CompIndex == 0) {
            ZonePlenumNum =
                UtilityRoutines::FindItemInList(CompName, state.dataZonePlenum->ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZonePlenumName);
            if (ZonePlenumNum == 0) {
                ShowFatalError(state, "SimAirZonePlenum: AirLoopHVAC:SupplyPlenum not found=" + std::string{CompName});
            }
            CompIndex = ZonePlenumNum;
        } else {
            ZonePlenumNum = CompIndex;
            if (ZonePlenumNum > state.dataZonePlenum->NumZoneSupplyPlenums || ZonePlenumNum < 1) {
                ShowFatalError(
                    state,
                    format("SimAirZonePlenum: Invalid CompIndex passed={}, Number of AirLoopHVAC:SupplyPlenum={}, AirLoopHVAC:SupplyPlenum name={}",
                           ZonePlenumNum,
                           state.dataZonePlenum->NumZoneReturnPlenums,
                           CompName));
            }
            if (state.dataZonePlenum->CheckSupEquipName(ZonePlenumNum)) {
                if (CompName != state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName) {
                    ShowFatalError(state,
                                   format("SimAirZonePlenum: Invalid CompIndex passed={}, AirLoopHVAC:SupplyPlenum name={}, stored "
                                          "AirLoopHVAC:SupplyPlenum Name for that index={}",
                                          ZonePlenumNum,
                                          CompName,
                                          state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName));
                }
                state.dataZonePlenum->CheckSupEquipName(ZonePlenumNum) = false;
            }
        }

        InitAirZoneSupplyPlenum(state, ZonePlenumNum, FirstHVACIteration, FirstCall); // Initialize all ZonePlenum related parameters

        CalcAirZoneSupplyPlenum(state, ZonePlenumNum, FirstCall);
        // Update the current ZonePlenum to the outlet nodes
        UpdateAirZoneSupplyPlenum(state, ZonePlenumNum, PlenumInletChanged, FirstCall);

    } else {
        ShowSevereError(state, "SimAirZonePlenum: Errors in Plenum=" + std::string{CompName});
        ShowContinueError(state, format("ZonePlenum: Unhandled plenum type found:{}", iCompType));
        ShowFatalError(state, "Preceding conditions cause termination.");
    }
}

void GetZonePlenumInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   November 2000
    //       MODIFIED       August 2003, FCW: For each zone with a return air plenum put the ZoneRetPlenCond
    //                       number for the return air plenum in the ZoneEquipConfig array for the zone
    //                       for later access to the zone's return air plenum conditions.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main routine to call other input routines and Get routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    using DataZoneEquipment::EquipConfiguration;
    using NodeInputManager::CheckUniqueNodes;
    using NodeInputManager::EndUniqueNodeCheck;
    using NodeInputManager::GetNodeNums;
    using NodeInputManager::GetOnlySingleNode;
    using NodeInputManager::InitUniqueNodeCheck;
    using PoweredInductionUnits::PIUInducesPlenumAir;
    using PurchasedAirManager::CheckPurchasedAirForReturnPlenum;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZonePlenumNum; // The ZonePlenum that you are currently loading input into
    int ZonePlenumLoop;
    int ZoneEquipConfigLoop;
    int NumAlphas;
    int NumNums;
    int NumArgs;
    int NumNodes;
    Array1D_int NodeNums;
    int MaxNums;
    int MaxAlphas;
    int NodeNum;
    int IOStat;
    Array1D<Real64> NumArray;        // Numeric input items for object
    std::string CurrentModuleObject; // for ease in getting objects
    Array1D_string AlphArray;        // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    bool ErrorsFound(false);
    bool NodeListError; // Flag for node list error
    bool UniqueNodeError;
    static constexpr std::string_view RoutineName("GetZonePlenumInput: "); // include trailing blank space
    std::string InducedNodeListName;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC:ReturnPlenum", NumArgs, NumAlphas, NumNums);
    MaxNums = NumNums;
    MaxAlphas = NumAlphas;
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC:SupplyPlenum", NumArgs, NumAlphas, NumNums);
    MaxNums = max(NumNums, MaxNums);
    MaxAlphas = max(NumAlphas, MaxAlphas);
    AlphArray.allocate(MaxAlphas);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNums);
    NumArray.dimension(MaxNums, 0.0);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNums, true);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumArgs, NumAlphas, NumNums);
    NodeNums.dimension(NumArgs, 0);

    InducedNodeListName = "";

    state.dataZonePlenum->NumZoneReturnPlenums = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:ReturnPlenum");
    state.dataZonePlenum->NumZoneSupplyPlenums = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:SupplyPlenum");
    state.dataZonePlenum->NumZonePlenums = state.dataZonePlenum->NumZoneReturnPlenums + state.dataZonePlenum->NumZoneSupplyPlenums;

    if (state.dataZonePlenum->NumZoneReturnPlenums > 0) state.dataZonePlenum->ZoneRetPlenCond.allocate(state.dataZonePlenum->NumZoneReturnPlenums);
    if (state.dataZonePlenum->NumZoneSupplyPlenums > 0) state.dataZonePlenum->ZoneSupPlenCond.allocate(state.dataZonePlenum->NumZoneSupplyPlenums);
    state.dataZonePlenum->CheckRetEquipName.dimension(state.dataZonePlenum->NumZoneReturnPlenums, true);
    state.dataZonePlenum->CheckSupEquipName.dimension(state.dataZonePlenum->NumZoneSupplyPlenums, true);

    ZonePlenumNum = 0;

    InitUniqueNodeCheck(state, "AirLoopHVAC:ReturnPlenum");
    for (ZonePlenumLoop = 1; ZonePlenumLoop <= state.dataZonePlenum->NumZoneReturnPlenums; ++ZonePlenumLoop) {
        ++ZonePlenumNum;

        CurrentModuleObject = "AirLoopHVAC:ReturnPlenum";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 ZonePlenumNum,
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
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName = AlphArray(1);

        // Check if this zone is also used in another return plenum
        IOStat = UtilityRoutines::FindItemInList(
            AlphArray(2), state.dataZonePlenum->ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName, ZonePlenumNum - 1);
        if (IOStat != 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject + '.');
            ShowContinueError(state, "..Only one " + CurrentModuleObject + " object may be connected to a given zone.");
            ShowContinueError(state, "..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
            ErrorsFound = true;
        }
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneName = AlphArray(2);
        // put the X-Ref to the zone heat balance data structure
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum = UtilityRoutines::FindItemInList(AlphArray(2), state.dataHeatBal->Zone);
        if (state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum == 0) {
            ShowSevereError(state,
                            "For " + CurrentModuleObject + " = " + AlphArray(1) + ", " + cAlphaFields(2) + " = " + AlphArray(2) + " not found.");
            ErrorsFound = true;
            continue;
        } else {
            state.dataHeatBal->Zone(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).IsReturnPlenum = true;
            state.dataHeatBal->Zone(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).PlenumCondNum = ZonePlenumNum;
        }
        //  Check if this zone is used as a controlled zone
        ZoneEquipConfigLoop = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
        if (ZoneEquipConfigLoop != 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is a controlled zone. It cannot be used as a " +
                                CurrentModuleObject);
            ShowContinueError(state, "..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneNodeName = AlphArray(3);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum = GetOnlySingleNode(state,
                                                                                             AlphArray(3),
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             AlphArray(1),
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::ZoneNode,
                                                                                             1,
                                                                                             ObjectIsNotParent);
        // Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
        state.dataHeatBal->Zone(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).SystemZoneNodeNumber =
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletNode = GetOnlySingleNode(state,
                                                                                            AlphArray(4),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            1,
                                                                                            ObjectIsNotParent);

        InducedNodeListName = AlphArray(5);
        NodeListError = false;
        GetNodeNums(state,
                    InducedNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    "AirLoopHVAC:ReturnPlenum",
                    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName,
                    DataLoopNode::NodeConnectionType::InducedAir,
                    1,
                    ObjectIsNotParent,
                    _,
                    cAlphaFields(5));

        if (!NodeListError) {
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes = NumNodes;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedNode.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedMassFlowRate.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedMassFlowRateMaxAvail.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedMassFlowRateMinAvail.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedTemp.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedHumRat.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedEnthalpy.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedPressure.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedCO2.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
                .InducedGenContam.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMinAvail = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedTemp = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedHumRat = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedPressure = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedCO2 = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedGenContam = 0.0;
            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum) = NodeNums(NodeNum);
                UniqueNodeError = false;
                if (!CheckPurchasedAirForReturnPlenum(state, ZonePlenumNum)) {
                    CheckUniqueNodes(state, "Return Plenum Induced Air Nodes", "NodeNumber", UniqueNodeError, _, NodeNums(NodeNum));
                    if (UniqueNodeError) {
                        ShowContinueError(state, "Occurs for ReturnPlenum = " + AlphArray(1));
                        ErrorsFound = true;
                    }
                    PIUInducesPlenumAir(state, state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum));
                }
            }
        } else {
            ShowContinueError(state,
                              "Invalid Induced Air Outlet Node or NodeList name in AirLoopHVAC:ReturnPlenum object = " +
                                  state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName);
            ErrorsFound = true;
        }

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes = NumAlphas - 5;

        for (auto &e : state.dataZonePlenum->ZoneRetPlenCond)
            e.InitFlag = true;

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
            .InletMassFlowRate.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
            .InletMassFlowRateMaxAvail.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
            .InletMassFlowRateMinAvail.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletTemp.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletHumRat.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
            .InletEnthalpy.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum)
            .InletPressure.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneEqNum.allocate(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode = 0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletTemp = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletHumRat = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletEnthalpy = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletPressure = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletTemp = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneTemp = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat = 0.0;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy = 0.0;

        for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++NodeNum) {

            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode(NodeNum) = GetOnlySingleNode(state,
                                                                                                        AlphArray(5 + NodeNum),
                                                                                                        ErrorsFound,
                                                                                                        CurrentModuleObject,
                                                                                                        AlphArray(1),
                                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                                        1,
                                                                                                        ObjectIsNotParent);
        }

    } // end AirLoopHVAC:ReturnPlenum Loop
    EndUniqueNodeCheck(state, "AirLoopHVAC:ReturnPlenum");

    ZonePlenumNum = 0;

    for (ZonePlenumLoop = 1; ZonePlenumLoop <= state.dataZonePlenum->NumZoneSupplyPlenums; ++ZonePlenumLoop) {
        ++ZonePlenumNum;

        CurrentModuleObject = "AirLoopHVAC:SupplyPlenum";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 ZonePlenumNum,
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
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName = AlphArray(1);

        // Check if this zone is also used in another plenum
        IOStat = UtilityRoutines::FindItemInList(
            AlphArray(2), state.dataZonePlenum->ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZoneName, ZonePlenumNum - 1);
        if (IOStat != 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject + '.');
            ShowContinueError(state, "..Only one " + CurrentModuleObject + " object may be connected to a given zone.");
            ShowContinueError(state, "..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
            ErrorsFound = true;
        }
        if (state.dataZonePlenum->NumZoneReturnPlenums > 0) { // Check if this zone is also used in another plenum
            IOStat = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZonePlenum->ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName);
            if (IOStat != 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject +
                                    " or AirLoopHVAC:ReturnPlenum.");
                ShowContinueError(state,
                                  "..Only one " + CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum object may be connected to a given zone.");
                ShowContinueError(state, "..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                ErrorsFound = true;
            }
        }
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneName = AlphArray(2);
        // put the X-Ref to the zone heat balance data structure
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum = UtilityRoutines::FindItemInList(AlphArray(2), state.dataHeatBal->Zone);
        if (state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum == 0) {
            ShowSevereError(state,
                            "For " + CurrentModuleObject + " = " + AlphArray(1) + ", " + cAlphaFields(2) + " = " + AlphArray(2) + " not found.");
            ErrorsFound = true;
            continue;
        } else {
            state.dataHeatBal->Zone(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).IsSupplyPlenum = true;
            state.dataHeatBal->Zone(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).PlenumCondNum = ZonePlenumNum;
        }
        //  Check if this zone is used as a controlled zone
        if (std::any_of(state.dataZoneEquip->ZoneEquipConfig.begin(), state.dataZoneEquip->ZoneEquipConfig.end(), [](EquipConfiguration const &e) {
                return e.IsControlled;
            })) {
            ZoneEquipConfigLoop = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
            if (ZoneEquipConfigLoop != 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is a controlled zone. It cannot be used as a " +
                                    CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum.");
                ShowContinueError(state, "..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                ErrorsFound = true;
            }
        }
        // Check if this is also used as a return plenum
        //  *** This next IF loop looks wrong.  Sent e-mail to Peter/Brent 8/14/08 for clarification ****
        //      IF (NumZoneReturnPlenums > 0) THEN
        //        IOSTAT=UtilityRoutines::FindItemInList(AlphArray(1),ZoneRetPlenCond%ZoneName,NumZoneReturnPlenums)
        //        IF (IOStat /= 0) THEN
        //          CALL ShowSevereError(state, RoutineName//'Plenum "'//TRIM(AlphArray(2))//  &
        //                               '" is a controlled zone.  It cannot be used as a '//  &
        //                               'SUPPLY PLENUM or RETURN PLENUM.')
        //          CALL ShowContinueError(state, '..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        //          ErrorsFound=.TRUE.
        //        ENDIF
        //      ENDIF

        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneNodeName = AlphArray(3);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum = GetOnlySingleNode(state,
                                                                                             AlphArray(3),
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             AlphArray(1),
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::ZoneNode,
                                                                                             1,
                                                                                             ObjectIsNotParent);
        // Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
        state.dataHeatBal->Zone(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).SystemZoneNodeNumber =
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletNode = GetOnlySingleNode(state,
                                                                                           AlphArray(4),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           AlphArray(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                                           1,
                                                                                           ObjectIsNotParent);

        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes = NumAlphas - 4;

        for (auto &e : state.dataZonePlenum->ZoneSupPlenCond)
            e.InitFlag = true;

        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletMassFlowRate.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletMassFlowRateMaxAvail.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletMassFlowRateMinAvail.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletTemp.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletHumRat.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletEnthalpy.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum)
            .OutletPressure.allocate(state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);

        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode = 0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletTemp = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletPressure = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletTemp = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletHumRat = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletEnthalpy = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletPressure = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneTemp = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy = 0.0;

        for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeNum) {

            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeNum) = GetOnlySingleNode(state,
                                                                                                         AlphArray(4 + NodeNum),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphArray(1),
                                                                                                         DataLoopNode::NodeFluidType::Air,
                                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                                         1,
                                                                                                         ObjectIsNotParent);
        }

    } // end AirLoopHVAC:SupplyPlenum Loop

    AlphArray.deallocate();
    NumArray.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();
    NodeNums.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
    }
}

void InitAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   November 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the ZonePlenum components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int InducedNode(0);
    int InletNodeLoop;
    int ZoneNodeNum;
    int NodeNum;
    int ZonePlenumLoop;
    int PlenumZoneNum;
    int ZoneEquipConfigLoop; // Loop number of ZoneEquipConfig derived type
    int ADUNum;              // air distribution unit index
    int NumADUsToPlen;       // number of ADUs that might leak to this plenum
    int ADUsToPlenIndex;     // index of an ADU that might leak to this plenum in the plenum ADU list

    // Do the one time initializations
    if (state.dataZonePlenum->InitAirZoneReturnPlenumOneTimeFlag) {

        // For each zone with a return air plenum put the ZoneRetPlenCond number for the return air plenum
        // in the ZoneEquipConfig array for the zone. This allows direct access of the zone's return air
        // plenum conditions, such as plenum temperature and air flow. Also establish and save connections
        // to the Air Distribution Units. This is needed for the simple duct leakage calculation.

        for (ZonePlenumLoop = 1; ZonePlenumLoop <= state.dataZonePlenum->NumZoneReturnPlenums; ++ZonePlenumLoop) {
            ADUsToPlenIndex = 0;
            NumADUsToPlen = 0;
            if (state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).NumInletNodes > 0) {
                for (InletNodeLoop = 1; InletNodeLoop <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).NumInletNodes; ++InletNodeLoop) {
                    InletNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).InletNode(InletNodeLoop);
                    // Loop through ZoneEquipConfig's and look for return air node value = InletNode
                    for (ZoneEquipConfigLoop = 1; ZoneEquipConfigLoop <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigLoop) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigLoop).IsControlled) continue;
                        for (int retNode = 1; retNode <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigLoop).NumReturnNodes; ++retNode) {
                            if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigLoop).ReturnNode(retNode) == InletNode) {
                                state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigLoop).ReturnNodePlenumNum = ZonePlenumLoop;
                                state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).ZoneEqNum(InletNodeLoop) = ZoneEquipConfigLoop;
                            }
                        }
                    }
                    // count the ADUs that can leak to this plenum
                    for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                        if (state.dataDefineEquipment->AirDistUnit(ADUNum).ZoneEqNum ==
                            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).ZoneEqNum(InletNodeLoop)) {
                            state.dataDefineEquipment->AirDistUnit(ADUNum).RetPlenumNum = ZonePlenumLoop;
                            ++NumADUsToPlen;
                        }
                    }
                }
            }
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).ADUIndex.allocate(NumADUsToPlen);
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).NumADUs = NumADUsToPlen;
            // fill the list of air distribution units that can leak to this plenum
            if (NumADUsToPlen > 0) {
                for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                    if (state.dataDefineEquipment->AirDistUnit(ADUNum).RetPlenumNum == ZonePlenumLoop) {
                        ++ADUsToPlenIndex;
                        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumLoop).ADUIndex(ADUsToPlenIndex) = ADUNum;
                    }
                }
            }
        }

        // Check that all ADUs with leakage found a return plenum
        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            auto &thisADU(state.dataDefineEquipment->AirDistUnit(ADUNum));
            // TODO: this is comparing the same thing twice
            if ((thisADU.DownStreamLeak || thisADU.DownStreamLeak) && (thisADU.RetPlenumNum == 0)) {
                ShowWarningError(state,
                                 "No return plenum found for simple duct leakage for ZoneHVAC:AirDistributionUnit=" + thisADU.Name +
                                     " in Zone=" + state.dataZoneEquip->ZoneEquipConfig(thisADU.ZoneEqNum).ZoneName);
                ShowContinueError(state, "Leakage will be ignored for this ADU.");
                thisADU.UpStreamLeak = false;
                thisADU.DownStreamLeak = false;
                thisADU.UpStreamLeakFrac = 0.0;
                thisADU.DownStreamLeakFrac = 0.0;
            }
        }

        state.dataZonePlenum->InitAirZoneReturnPlenumOneTimeFlag = false;
    }

    // Do the Begin Environment initializations
    if (state.dataZonePlenum->InitAirZoneReturnPlenumEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        for (PlenumZoneNum = 1; PlenumZoneNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++PlenumZoneNum) {

            ZoneNodeNum = state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).ZoneNodeNum;
            state.dataLoopNodes->Node(ZoneNodeNum).Temp = 20.0;
            state.dataLoopNodes->Node(ZoneNodeNum).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ZoneNodeNum).Quality = 1.0;
            state.dataLoopNodes->Node(ZoneNodeNum).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(ZoneNodeNum).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy =
                PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNodeNum).Temp, state.dataLoopNodes->Node(ZoneNodeNum).HumRat);

            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).ZoneTemp = 20.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).ZoneHumRat = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).ZoneEnthalpy = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletTemp = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletHumRat = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletEnthalpy = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletPressure = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRate = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum->ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRateMinAvail = 0.0;
        }

        state.dataZonePlenum->InitAirZoneReturnPlenumEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZonePlenum->InitAirZoneReturnPlenumEnvrnFlag = true;
    }

    // Transfer the node data to ZoneRetPlenCond data structure
    for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++NodeNum) {

        InletNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode(NodeNum);
        // Set all of the inlet mass flow variables from the nodes
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(NodeNum) = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail(NodeNum) =
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail(NodeNum) =
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;
        //    ! Set all of the inlet state variables from the inlet nodes
        //    ZoneRetPlenCond(ZonePlenumNum)%InletTemp(NodeNum)         = Node(InletNode)%Temp
        //    ZoneRetPlenCond(ZonePlenumNum)%InletHumRat(NodeNum)       = Node(InletNode)%HumRat
        //    ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy(NodeNum)     = Node(InletNode)%Enthalpy
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletPressure(NodeNum) = state.dataLoopNodes->Node(InletNode).Press;
    }

    ZoneNodeNum = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;
    // Set the induced air flow rates and conditions
    for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++NodeNum) {
        InducedNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate(NodeNum) = state.dataLoopNodes->Node(InducedNode).MassFlowRate;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMaxAvail(NodeNum) =
            state.dataLoopNodes->Node(InducedNode).MassFlowRateMaxAvail;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMinAvail(NodeNum) =
            state.dataLoopNodes->Node(InducedNode).MassFlowRateMinAvail;

        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedTemp(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).Temp;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedHumRat(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy;
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedPressure(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).Press;
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedCO2(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedGenContam(NodeNum) = state.dataLoopNodes->Node(ZoneNodeNum).GenContam;
        }
    }

    // Add stuff to calculate conduction inputs to the zone plenum
    // Now load the zone conditions
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneTemp = state.dataLoopNodes->Node(ZoneNodeNum).Temp;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat = state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy = state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy;
}

void InitAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool const FirstHVACIteration, bool const FirstCall)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the ZonePlenum components.

    // METHODOLOGY EMPLOYED:
    // Similar to the Zone Splitter component but with interactions to the plenum zone.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int OutletNode;
    int ZoneNodeNum;
    int PlenumZoneNum;
    int NodeIndex;

    auto &Node(state.dataLoopNodes->Node);

    // Do the Begin Environment initializations
    if (state.dataZonePlenum->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        for (PlenumZoneNum = 1; PlenumZoneNum <= state.dataZonePlenum->NumZoneSupplyPlenums; ++PlenumZoneNum) {

            ZoneNodeNum = state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).ZoneNodeNum;
            Node(ZoneNodeNum).Temp = 20.0;
            Node(ZoneNodeNum).MassFlowRate = 0.0;
            Node(ZoneNodeNum).Quality = 1.0;
            Node(ZoneNodeNum).Press = state.dataEnvrn->OutBaroPress;
            Node(ZoneNodeNum).HumRat = state.dataEnvrn->OutHumRat;
            Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum).Temp, Node(ZoneNodeNum).HumRat);

            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).ZoneTemp = 20.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).ZoneHumRat = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).ZoneEnthalpy = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletTemp = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletHumRat = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletEnthalpy = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletPressure = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRate = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum->ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRateMinAvail = 0.0;
        }

        state.dataZonePlenum->MyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZonePlenum->MyEnvrnFlag = true;
    }

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.

    InletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletNode;
    ZoneNodeNum = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

    if (FirstHVACIteration && FirstCall) {
        if (Node(InletNode).MassFlowRate > 0.0) {
            Node(ZoneNodeNum).MassFlowRate = Node(InletNode).MassFlowRate;
            for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                Node(OutletNode).MassFlowRate = Node(InletNode).MassFlowRate / state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes;
            }
        }
        if (Node(InletNode).MassFlowRateMaxAvail > 0.0) {
            Node(ZoneNodeNum).MassFlowRateMaxAvail = Node(InletNode).MassFlowRateMaxAvail;
            for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                Node(OutletNode).MassFlowRateMaxAvail =
                    Node(InletNode).MassFlowRateMaxAvail / state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes;
            }
        }

    } // For FirstHVACIteration and FirstCall

    if (FirstCall) {

        if (Node(InletNode).MassFlowRateMaxAvail == 0.0) { // For Node inlet Max Avail = 0.0

            for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                Node(OutletNode).MassFlowRate = 0.0;
                Node(OutletNode).MassFlowRateMaxAvail = 0.0;
                Node(OutletNode).MassFlowRateMinAvail = 0.0;
            }

            Node(ZoneNodeNum).MassFlowRate = 0.0;
            Node(ZoneNodeNum).MassFlowRateMaxAvail = 0.0;
            Node(ZoneNodeNum).MassFlowRateMinAvail = 0.0;

        } // For Node inlet Max Avail = 0.0

        // Add stuff to calculate conduction inputs to the zone plenum
        // Now load the zone conditions
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneTemp = Node(ZoneNodeNum).Temp;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat = Node(ZoneNodeNum).HumRat;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy = Node(ZoneNodeNum).Enthalpy;

        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
            Node(OutletNode).Press = Node(InletNode).Press;
            Node(OutletNode).Quality = Node(InletNode).Quality;
        }

        Node(ZoneNodeNum).Press = Node(InletNode).Press;
        Node(ZoneNodeNum).Quality = Node(InletNode).Quality;

    } else { // On the second call from the ZoneEquipManager this is where the flows are passed back to
        // the supply plenum inlet.
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate(NodeIndex) = Node(OutletNode).MassFlowRate;
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail(NodeIndex) = Node(OutletNode).MassFlowRateMaxAvail;
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail(NodeIndex) = Node(OutletNode).MassFlowRateMinAvail;
        }

    } // For FirstCall
}

void CalcAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   November 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNodeNum(0);            // inlet node number
    int IndNum(0);                  // induced air index
    int ADUNum(0);                  // air distribution unit number
    int ADUListIndex(0);            // air distribution unit index in zone return plenum data structure
    Real64 TotIndMassFlowRate(0.0); // total induced air mass flow rate [kg/s]

    // Reset the totals to zero before they are summed.
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletTemp = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure = 0.0;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
    TotIndMassFlowRate = 0.0;

    for (InletNodeNum = 1; InletNodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate +=
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail +=
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail(InletNodeNum);
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail +=
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail(InletNodeNum);
    }

    if (state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {

        // "Momentum balance" to get outlet air pressure
        for (InletNodeNum = 1; InletNodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {

            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure +=
                state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletPressure(InletNodeNum) *
                state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
        }

    } else {
        // Mass Flow in air loop is zero and loop is not operating.
        // Arbitrarily set the output to the first inlet leg
        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletPressure(1);
    }

    // add in the leak flow rate, if any. Don't alter the pressure calc (it is not used anyway)
    for (ADUListIndex = 1; ADUListIndex <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumADUs; ++ADUListIndex) {
        ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ADUIndex(ADUListIndex);
        if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak || state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate +=
                state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk +
                state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail +=
                state.dataDefineEquipment->AirDistUnit(ADUNum).MaxAvailDelta;
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail +=
                state.dataDefineEquipment->AirDistUnit(ADUNum).MinAvailDelta;
        }
    }
    // Sum up induced air flow rate
    for (IndNum = 1; IndNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++IndNum) {
        TotIndMassFlowRate += state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate(IndNum);
    }

    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate -= TotIndMassFlowRate;

    // Set the Plenum Outlet to the Zone Node conditions
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy;
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletTemp = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneTemp;
    // make sure the MassFlowMaxAvail >= MassFlowRate
    state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail =
        max(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail,
            state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate);
}

void CalcAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool const FirstCall)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // METHODOLOGY EMPLOYED:
    // Similar to the Zone Splitter component but with interactions to the plenum zone.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NodeIndex;

    // The first time through the State properties are passed through
    if (FirstCall) {
        // Moisture balance to get outlet air humidity ratio
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletHumRat(NodeIndex) =
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat;
        }

        // Energy balance to get outlet air enthalpy
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy(NodeIndex) =
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy;
        }

        // Set outlet temperatures equal to inlet temperature
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletTemp(NodeIndex) =
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneTemp;
        }

    } else {
        // This is the second time through and this is where the mass flows from the outlets are
        // summed and then assigned upstream to the inlet node.
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
        state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate +=
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate(NodeIndex);
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail +=
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail(NodeIndex);
            state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail +=
                state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail(NodeIndex);
        }
    }
}

// End Algorithm Section of the Module
// *****************************************************************************

// Beginning of Update subroutines for the ZonePlenum Module
// *****************************************************************************

void UpdateAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   November 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;
    int InletNode;
    int ZoneNode;
    int InletNodeNum;
    int InducedNode; // the node number of an induced air outlet node
    int IndNum;      // the induced air outlet index in ZoneRetPlenCond

    auto &Node(state.dataLoopNodes->Node);

    OutletNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletNode;
    InletNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode(1);
    ZoneNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;

    // Set the outlet air nodes of the ZonePlenum
    Node(OutletNode).MassFlowRate = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
    Node(OutletNode).MassFlowRateMaxAvail = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail;
    Node(OutletNode).MassFlowRateMinAvail = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail;

    Node(ZoneNode).MassFlowRate = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
    Node(ZoneNode).MassFlowRateMaxAvail = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail;
    Node(ZoneNode).MassFlowRateMinAvail = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail;
    Node(ZoneNode).Press = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure;

    Node(OutletNode).Temp = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletTemp;
    Node(OutletNode).HumRat = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletHumRat;
    Node(OutletNode).Enthalpy = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy;
    Node(OutletNode).Press = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletPressure;
    for (IndNum = 1; IndNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++IndNum) {
        InducedNode = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedNode(IndNum);
        Node(InducedNode).Temp = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedTemp(IndNum);
        Node(InducedNode).HumRat = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedHumRat(IndNum);
        Node(InducedNode).Enthalpy = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy(IndNum);
        Node(InducedNode).Press = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedPressure(IndNum);
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            Node(InducedNode).CO2 = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedCO2(IndNum);
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            Node(InducedNode).GenContam = state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InducedGenContam(IndNum);
        }
        Node(InducedNode).Quality = Node(InletNode).Quality;
    }

    // Set the outlet nodes for properties that are just pass through and not used
    Node(OutletNode).Quality = Node(InletNode).Quality;
    Node(ZoneNode).Quality = Node(InletNode).Quality;

    // Set the outlet node contaminant properties if needed. The zone contaminant conditions are calculated in ZoneContaminantPredictorCorrector
    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        if (state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {
            // CO2 balance to get outlet air CO2
            Node(OutletNode).CO2 = 0.0;
            for (InletNodeNum = 1; InletNodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
                Node(OutletNode).CO2 += Node(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode(InletNodeNum)).CO2 *
                                        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                                        state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
            }
            Node(ZoneNode).CO2 = Node(OutletNode).CO2;
        } else {
            Node(OutletNode).CO2 = Node(ZoneNode).CO2;
        }
    }
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        if (state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {
            // GenContam balance to get outlet air GenContam
            Node(OutletNode).GenContam = 0.0;
            for (InletNodeNum = 1; InletNodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
                Node(OutletNode).GenContam += Node(state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletNode(InletNodeNum)).GenContam *
                                              state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                                              state.dataZonePlenum->ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
            }
            Node(ZoneNode).GenContam = Node(OutletNode).GenContam;
        } else {
            Node(OutletNode).GenContam = Node(ZoneNode).GenContam;
        }
    }
}

void UpdateAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool &PlenumInletChanged, bool const FirstCall)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // METHODOLOGY EMPLOYED:
    // Similar to the Zone Splitter component but with interactions to the plenum zone.

    // Using/Aliasing
    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const FlowRateToler(0.01); // Tolerance for mass flow rate convergence (in kg/s)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;
    int InletNode;
    int ZoneNode;
    int NodeIndex;

    auto &Node(state.dataLoopNodes->Node);

    OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(1);
    InletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletNode;
    ZoneNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

    // On the FirstCall the State properties are passed through and the mass flows are not dealt with
    if (FirstCall) {
        // Set the outlet nodes for properties that just pass through and not used
        for (NodeIndex = 1; NodeIndex <= state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
            OutletNode = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
            Node(OutletNode).Temp = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletTemp(NodeIndex);
            Node(OutletNode).HumRat = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletHumRat(NodeIndex);
            Node(OutletNode).Enthalpy = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy(NodeIndex);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                Node(OutletNode).CO2 = Node(InletNode).CO2;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                Node(OutletNode).GenContam = Node(InletNode).GenContam;
            }
        }

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            Node(ZoneNode).CO2 = Node(InletNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            Node(ZoneNode).GenContam = Node(InletNode).GenContam;
        }

    } else {
        // The second time through just updates the mass flow conditions back upstream
        // to the inlet.

        if (std::abs(Node(InletNode).MassFlowRate - state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate) > FlowRateToler) {
            PlenumInletChanged = true;
        }

        Node(InletNode).MassFlowRate = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate;
        Node(InletNode).MassFlowRateMaxAvail = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail;
        Node(InletNode).MassFlowRateMinAvail = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail;

        Node(ZoneNode).MassFlowRate = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate;
        Node(ZoneNode).MassFlowRateMaxAvail = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail;
        Node(ZoneNode).MassFlowRateMinAvail = state.dataZonePlenum->ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail;

    } // For FirstCall
}

int GetReturnPlenumIndex(EnergyPlusData &state, int const &ExNodeNum)
{

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlenumNum;      // loop counter
    int InducedNodeNum; // loop counter
    int WhichPlenum;    // index to return plenum

    // Obtains and Allocates ZonePlenum related parameters from input file
    if (state.dataZonePlenum->GetInputFlag) { // First time subroutine has been entered
        GetZonePlenumInput(state);
        state.dataZonePlenum->GetInputFlag = false;
    }

    WhichPlenum = 0;
    if (state.dataZonePlenum->NumZoneReturnPlenums > 0) {
        for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++PlenumNum) {
            if (ExNodeNum != state.dataZonePlenum->ZoneRetPlenCond(PlenumNum).OutletNode) continue;
            WhichPlenum = PlenumNum;
            break;
        }
        if (WhichPlenum == 0) {
            for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++PlenumNum) {
                for (InducedNodeNum = 1; InducedNodeNum <= state.dataZonePlenum->ZoneRetPlenCond(PlenumNum).NumInducedNodes; ++InducedNodeNum) {
                    if (ExNodeNum != state.dataZonePlenum->ZoneRetPlenCond(PlenumNum).InducedNode(InducedNodeNum)) continue;
                    WhichPlenum = PlenumNum;
                    break;
                }
                if (WhichPlenum > 0) break;
            }
        }
    }

    return WhichPlenum;
}

void GetReturnPlenumName(EnergyPlusData &state, int const &ReturnPlenumIndex, std::string &ReturnPlenumName)
{

    // Obtains and Allocates ZonePlenum related parameters from input file
    if (state.dataZonePlenum->GetInputFlag) { // First time subroutine has been entered
        GetZonePlenumInput(state);
        state.dataZonePlenum->GetInputFlag = false;
    }

    ReturnPlenumName = " ";
    if (state.dataZonePlenum->NumZoneReturnPlenums > 0) {
        ReturnPlenumName = state.dataZonePlenum->ZoneRetPlenCond(ReturnPlenumIndex).ZonePlenumName;
    }
}

int getReturnPlenumIndexFromInletNode(EnergyPlusData &state, int const &InNodeNum)
{

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlenumNum; // loop counter
    int InNodeCtr; // loop counter
    int thisPlenum;

    // Obtains and Allocates ZonePlenum related parameters from input file
    if (state.dataZonePlenum->GetInputFlag) { // First time subroutine has been entered
        GetZonePlenumInput(state);
        state.dataZonePlenum->GetInputFlag = false;
    }

    thisPlenum = 0;
    if (state.dataZonePlenum->NumZoneReturnPlenums > 0) {
        for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++PlenumNum) {
            for (InNodeCtr = 1; InNodeCtr <= state.dataZonePlenum->ZoneRetPlenCond(PlenumNum).NumInletNodes; ++InNodeCtr) {
                if (InNodeNum != state.dataZonePlenum->ZoneRetPlenCond(PlenumNum).InletNode(InNodeCtr)) continue;
                thisPlenum = PlenumNum;
                break;
            }
            if (thisPlenum > 0) break;
        }
    }

    return thisPlenum;
}

} // namespace EnergyPlus::ZonePlenum
