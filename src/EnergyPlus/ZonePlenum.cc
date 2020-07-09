// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus {

namespace ZonePlenum {
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
    using namespace DataPrecisionGlobals;
    using DataGlobals::BeginDayFlag;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::NumOfZones;
    using namespace DataLoopNode;
    using namespace DataHVACGlobals;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutHumRat;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyTdbFnHW;

    // Functions

    void SimAirZonePlenum(EnergyPlusData &state, std::string const &CompName,
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
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZonePlenumNum; // The ZonePlenum that you are currently loading input into

        // Obtains and Allocates ZonePlenum related parameters from input file
        if (state.dataZonePlenum.GetInputFlag) { // First time subroutine has been entered
            GetZonePlenumInput(state);
            state.dataZonePlenum.GetInputFlag = false;
        }

        if (iCompType == ZoneReturnPlenum_Type) { // 'AirLoopHVAC:ReturnPlenum'
            // Find the correct ZonePlenumNumber
            if (CompIndex == 0) {
                ZonePlenumNum = UtilityRoutines::FindItemInList(CompName, state.dataZonePlenum.ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZonePlenumName);
                if (ZonePlenumNum == 0) {
                    ShowFatalError("SimAirZonePlenum: AirLoopHVAC:ReturnPlenum not found=" + CompName);
                }
                CompIndex = ZonePlenumNum;
            } else {
                ZonePlenumNum = CompIndex;
                if (ZonePlenumNum > state.dataZonePlenum.NumZoneReturnPlenums || ZonePlenumNum < 1) {
                    ShowFatalError("SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits(ZonePlenumNum) +
                                   ", Number of AirLoopHVAC:ReturnPlenum=" + TrimSigDigits(state.dataZonePlenum.NumZoneReturnPlenums) +
                                   ", AirLoopHVAC:ReturnPlenum name=" + CompName);
                }
                if (state.dataZonePlenum.CheckRetEquipName(ZonePlenumNum)) {
                    if (CompName != state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName) {
                        ShowFatalError("SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits(ZonePlenumNum) +
                                       ", AirLoopHVAC:ReturnPlenum name=" + CompName +
                                       ", stored AirLoopHVAC:ReturnPlenum Name for that index=" + state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName);
                    }
                    state.dataZonePlenum.CheckRetEquipName(ZonePlenumNum) = false;
                }
            }

            InitAirZoneReturnPlenum(state.dataZonePlenum, ZonePlenumNum); // Initialize all ZonePlenum related parameters

            CalcAirZoneReturnPlenum(state.dataZonePlenum, ZonePlenumNum);

            UpdateAirZoneReturnPlenum(state.dataZonePlenum, ZonePlenumNum); // Update the current ZonePlenum to the outlet nodes

        } else if (iCompType == ZoneSupplyPlenum_Type) { // 'AirLoopHVAC:SupplyPlenum'
            // Find the correct ZonePlenumNumber
            if (CompIndex == 0) {
                ZonePlenumNum = UtilityRoutines::FindItemInList(CompName, state.dataZonePlenum.ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZonePlenumName);
                if (ZonePlenumNum == 0) {
                    ShowFatalError("SimAirZonePlenum: AirLoopHVAC:SupplyPlenum not found=" + CompName);
                }
                CompIndex = ZonePlenumNum;
            } else {
                ZonePlenumNum = CompIndex;
                if (ZonePlenumNum > state.dataZonePlenum.NumZoneSupplyPlenums || ZonePlenumNum < 1) {
                    ShowFatalError("SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits(ZonePlenumNum) +
                                   ", Number of AirLoopHVAC:SupplyPlenum=" + TrimSigDigits(state.dataZonePlenum.NumZoneReturnPlenums) +
                                   ", AirLoopHVAC:SupplyPlenum name=" + CompName);
                }
                if (state.dataZonePlenum.CheckSupEquipName(ZonePlenumNum)) {
                    if (CompName != state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName) {
                        ShowFatalError("SimAirZonePlenum: Invalid CompIndex passed=" + TrimSigDigits(ZonePlenumNum) +
                                       ", AirLoopHVAC:SupplyPlenum name=" + CompName +
                                       ", stored AirLoopHVAC:SupplyPlenum Name for that index=" + state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName);
                    }
                    state.dataZonePlenum.CheckSupEquipName(ZonePlenumNum) = false;
                }
            }

            InitAirZoneSupplyPlenum(state.dataZonePlenum, ZonePlenumNum, FirstHVACIteration, FirstCall); // Initialize all ZonePlenum related parameters

            CalcAirZoneSupplyPlenum(state.dataZonePlenum, ZonePlenumNum, FirstCall);
            // Update the current ZonePlenum to the outlet nodes
            UpdateAirZoneSupplyPlenum(state.dataZonePlenum, ZonePlenumNum, PlenumInletChanged, FirstCall);

        } else {
            ShowSevereError("SimAirZonePlenum: Errors in Plenum=" + CompName);
            ShowContinueError("ZonePlenum: Unhandled plenum type found:" + TrimSigDigits(iCompType));
            ShowFatalError("Preceding conditions cause termination.");
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
        using DataHeatBalance::Zone;
        using DataZoneEquipment::EquipConfiguration;
        using DataZoneEquipment::ZoneEquipConfig;
        using NodeInputManager::CheckUniqueNodes;
        using NodeInputManager::EndUniqueNodeCheck;
        using NodeInputManager::GetNodeNums;
        using NodeInputManager::GetOnlySingleNode;
        using NodeInputManager::InitUniqueNodeCheck;
        using namespace DataIPShortCuts;
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
        static bool ErrorsFound(false);
        bool NodeListError; // Flag for node list error
        bool UniqueNodeError;
        static std::string const RoutineName("GetZonePlenumInput: "); // include trailing blank space
        std::string InducedNodeListName;

        inputProcessor->getObjectDefMaxArgs("AirLoopHVAC:ReturnPlenum", NumArgs, NumAlphas, NumNums);
        MaxNums = NumNums;
        MaxAlphas = NumAlphas;
        inputProcessor->getObjectDefMaxArgs("AirLoopHVAC:SupplyPlenum", NumArgs, NumAlphas, NumNums);
        MaxNums = max(NumNums, MaxNums);
        MaxAlphas = max(NumAlphas, MaxAlphas);
        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        NumArray.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);
        inputProcessor->getObjectDefMaxArgs("NodeList", NumArgs, NumAlphas, NumNums);
        NodeNums.dimension(NumArgs, 0);

        InducedNodeListName = "";

        state.dataZonePlenum.NumZoneReturnPlenums = inputProcessor->getNumObjectsFound("AirLoopHVAC:ReturnPlenum");
        state.dataZonePlenum.NumZoneSupplyPlenums = inputProcessor->getNumObjectsFound("AirLoopHVAC:SupplyPlenum");
        state.dataZonePlenum.NumZonePlenums = state.dataZonePlenum.NumZoneReturnPlenums + state.dataZonePlenum.NumZoneSupplyPlenums;

        if (state.dataZonePlenum.NumZoneReturnPlenums > 0) state.dataZonePlenum.ZoneRetPlenCond.allocate(state.dataZonePlenum.NumZoneReturnPlenums);
        if (state.dataZonePlenum.NumZoneSupplyPlenums > 0) state.dataZonePlenum.ZoneSupPlenCond.allocate(state.dataZonePlenum.NumZoneSupplyPlenums);
        state.dataZonePlenum.CheckRetEquipName.dimension(state.dataZonePlenum.NumZoneReturnPlenums, true);
        state.dataZonePlenum.CheckSupEquipName.dimension(state.dataZonePlenum.NumZoneSupplyPlenums, true);

        ZonePlenumNum = 0;

        InitUniqueNodeCheck("AirLoopHVAC:ReturnPlenum");
        for (ZonePlenumLoop = 1; ZonePlenumLoop <= state.dataZonePlenum.NumZoneReturnPlenums; ++ZonePlenumLoop) {
            ++ZonePlenumNum;

            CurrentModuleObject = "AirLoopHVAC:ReturnPlenum";

            inputProcessor->getObjectItem(CurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, ErrorsFound);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName = AlphArray(1);

            // Check if this zone is also used in another return plenum
            IOStat = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZonePlenum.ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName, ZonePlenumNum - 1);
            if (IOStat != 0) {
                ShowSevereError(RoutineName + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject + '.');
                ShowContinueError("..Only one " + CurrentModuleObject + " object may be connected to a given zone.");
                ShowContinueError("..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                ErrorsFound = true;
            }
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneName = AlphArray(2);
            // put the X-Ref to the zone heat balance data structure
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum = UtilityRoutines::FindItemInList(AlphArray(2), Zone);
            if (state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum == 0) {
                ShowSevereError("For " + CurrentModuleObject + " = " + AlphArray(1) + ", " + cAlphaFields(2) + " = " + AlphArray(2) + " not found.");
                ErrorsFound = true;
                continue;
            } else {
                Zone(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).IsReturnPlenum = true;
                Zone(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).PlenumCondNum = ZonePlenumNum;
            }
            //  Check if this zone is used as a controlled zone
            ZoneEquipConfigLoop = UtilityRoutines::FindItemInList(AlphArray(2), ZoneEquipConfig, &EquipConfiguration::ZoneName);
            if (ZoneEquipConfigLoop != 0) {
                ShowSevereError(RoutineName + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is a controlled zone. It cannot be used as a " +
                                CurrentModuleObject);
                ShowContinueError("..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneNodeName = AlphArray(3);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum = GetOnlySingleNode(
                AlphArray(3), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_ZoneNode, 1, ObjectIsNotParent);
            // Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
            Zone(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ActualZoneNum).SystemZoneNodeNumber = state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;

            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletNode = GetOnlySingleNode(
                AlphArray(4), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            InducedNodeListName = AlphArray(5);
            NodeListError = false;
            GetNodeNums(InducedNodeListName,
                        NumNodes,
                        NodeNums,
                        NodeListError,
                        NodeType_Air,
                        "AirLoopHVAC:ReturnPlenum",
                        state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName,
                        NodeConnectionType_InducedAir,
                        1,
                        ObjectIsNotParent,
                        _,
                        cAlphaFields(5));

            if (!NodeListError) {
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes = NumNodes;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedNode.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMaxAvail.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMinAvail.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedTemp.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedHumRat.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedPressure.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedCO2.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedGenContam.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes);
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMaxAvail = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMinAvail = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedTemp = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedHumRat = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedPressure = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedCO2 = 0.0;
                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedGenContam = 0.0;
                for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                    state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum) = NodeNums(NodeNum);
                    UniqueNodeError = false;
                    if (!CheckPurchasedAirForReturnPlenum(ZonePlenumNum)) {
                        CheckUniqueNodes("Return Plenum Induced Air Nodes", "NodeNumber", UniqueNodeError, _, NodeNums(NodeNum));
                        if (UniqueNodeError) {
                            ShowContinueError("Occurs for ReturnPlenum = " + AlphArray(1));
                            ErrorsFound = true;
                        }
                        PIUInducesPlenumAir(state, state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum));
                    }
                }
            } else {
                ShowContinueError("Invalid Induced Air Outlet Node or NodeList name in AirLoopHVAC:ReturnPlenum object = " +
                    state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZonePlenumName);
                ErrorsFound = true;
            }

            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes = NumAlphas - 5;

            for (auto &e : state.dataZonePlenum.ZoneRetPlenCond)
                e.InitFlag = true;

            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletTemp.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletHumRat.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletEnthalpy.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletPressure.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneEqNum.allocate(state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes);

            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode = 0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletTemp = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletHumRat = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletEnthalpy = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletPressure = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletTemp = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneTemp = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat = 0.0;
            state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy = 0.0;

            for (NodeNum = 1; NodeNum <= state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++NodeNum) {

                state.dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode(NodeNum) = GetOnlySingleNode(AlphArray(5 + NodeNum),
                                                                                      ErrorsFound,
                                                                                      CurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      NodeType_Air,
                                                                                      NodeConnectionType_Inlet,
                                                                                      1,
                                                                                      ObjectIsNotParent);
            }

        } // end AirLoopHVAC:ReturnPlenum Loop
        EndUniqueNodeCheck("AirLoopHVAC:ReturnPlenum");

        ZonePlenumNum = 0;

        for (ZonePlenumLoop = 1; ZonePlenumLoop <= state.dataZonePlenum.NumZoneSupplyPlenums; ++ZonePlenumLoop) {
            ++ZonePlenumNum;

            CurrentModuleObject = "AirLoopHVAC:SupplyPlenum";

            inputProcessor->getObjectItem(CurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, ErrorsFound);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZonePlenumName = AlphArray(1);

            // Check if this zone is also used in another plenum
            IOStat = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZonePlenum.ZoneSupPlenCond, &ZoneSupplyPlenumConditions::ZoneName, ZonePlenumNum - 1);
            if (IOStat != 0) {
                ShowSevereError(RoutineName + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject + '.');
                ShowContinueError("..Only one " + CurrentModuleObject + " object may be connected to a given zone.");
                ShowContinueError("..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                ErrorsFound = true;
            }
            if (state.dataZonePlenum.NumZoneReturnPlenums > 0) { // Check if this zone is also used in another plenum
                IOStat = UtilityRoutines::FindItemInList(AlphArray(2), state.dataZonePlenum.ZoneRetPlenCond, &ZoneReturnPlenumConditions::ZoneName);
                if (IOStat != 0) {
                    ShowSevereError(RoutineName + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is used more than once as a " + CurrentModuleObject +
                                    " or AirLoopHVAC:ReturnPlenum.");
                    ShowContinueError("..Only one " + CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum object may be connected to a given zone.");
                    ShowContinueError("..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }
            }
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneName = AlphArray(2);
            // put the X-Ref to the zone heat balance data structure
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum = UtilityRoutines::FindItemInList(AlphArray(2), Zone);
            if (state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum == 0) {
                ShowSevereError("For " + CurrentModuleObject + " = " + AlphArray(1) + ", " + cAlphaFields(2) + " = " + AlphArray(2) + " not found.");
                ErrorsFound = true;
                continue;
            } else {
                Zone(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).IsSupplyPlenum = true;
                Zone(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).PlenumCondNum = ZonePlenumNum;
            }
            //  Check if this zone is used as a controlled zone
            if (std::any_of(ZoneEquipConfig.begin(), ZoneEquipConfig.end(), [](EquipConfiguration const &e) { return e.IsControlled; })) {
                ZoneEquipConfigLoop = UtilityRoutines::FindItemInList(AlphArray(2), ZoneEquipConfig, &EquipConfiguration::ZoneName);
                if (ZoneEquipConfigLoop != 0) {
                    ShowSevereError(RoutineName + cAlphaFields(2) + " \"" + AlphArray(2) + "\" is a controlled zone. It cannot be used as a " +
                                    CurrentModuleObject + " or AirLoopHVAC:ReturnPlenum.");
                    ShowContinueError("..occurs in " + CurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }
            }
            // Check if this is also used as a return plenum
            //  *** This next IF loop looks wrong.  Sent e-mail to Peter/Brent 8/14/08 for clarification ****
            //      IF (NumZoneReturnPlenums > 0) THEN
            //        IOSTAT=UtilityRoutines::FindItemInList(AlphArray(1),ZoneRetPlenCond%ZoneName,NumZoneReturnPlenums)
            //        IF (IOStat /= 0) THEN
            //          CALL ShowSevereError(RoutineName//'Plenum "'//TRIM(AlphArray(2))//  &
            //                               '" is a controlled zone.  It cannot be used as a '//  &
            //                               'SUPPLY PLENUM or RETURN PLENUM.')
            //          CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
            //          ErrorsFound=.TRUE.
            //        ENDIF
            //      ENDIF

            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneNodeName = AlphArray(3);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum = GetOnlySingleNode(
                AlphArray(3), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_ZoneNode, 1, ObjectIsNotParent);
            // Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
            Zone(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ActualZoneNum).SystemZoneNodeNumber = state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletNode = GetOnlySingleNode(
                AlphArray(4), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes = NumAlphas - 4;

            for (auto &e : state.dataZonePlenum.ZoneSupPlenCond)
                e.InitFlag = true;

            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletTemp.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletHumRat.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletPressure.allocate(state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes);

            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode = 0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletTemp = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletPressure = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletTemp = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletHumRat = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletEnthalpy = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletPressure = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneTemp = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat = 0.0;
            state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy = 0.0;

            for (NodeNum = 1; NodeNum <= state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeNum) {

                state.dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeNum) = GetOnlySingleNode(AlphArray(4 + NodeNum),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       AlphArray(1),
                                                                                       NodeType_Air,
                                                                                       NodeConnectionType_Outlet,
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
            ShowFatalError(RoutineName + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    void InitAirZoneReturnPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum)
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
        using DataContaminantBalance::Contaminant;
        using DataDefineEquip::AirDistUnit;
        using DataDefineEquip::NumAirDistUnits;
        using DataZoneEquipment::ZoneEquipConfig;

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
        if (dataZonePlenum.InitAirZoneReturnPlenumOneTimeFlag) {

            // For each zone with a return air plenum put the ZoneRetPlenCond number for the return air plenum
            // in the ZoneEquipConfig array for the zone. This allows direct access of the zone's return air
            // plenum conditions, such as plenum temperature and air flow. Also establish and save connections
            // to the Air Distribution Units. This is needed for the simple duct leakage calculation.

            for (ZonePlenumLoop = 1; ZonePlenumLoop <= dataZonePlenum.NumZoneReturnPlenums; ++ZonePlenumLoop) {
                ADUsToPlenIndex = 0;
                NumADUsToPlen = 0;
                if (dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).NumInletNodes > 0) {
                    for (InletNodeLoop = 1; InletNodeLoop <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).NumInletNodes; ++InletNodeLoop) {
                        InletNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).InletNode(InletNodeLoop);
                        // Loop through ZoneEquipConfig's and look for return air node value = InletNode
                        for (ZoneEquipConfigLoop = 1; ZoneEquipConfigLoop <= NumOfZones; ++ZoneEquipConfigLoop) {
                            if (!ZoneEquipConfig(ZoneEquipConfigLoop).IsControlled) continue;
                            for (int retNode = 1; retNode <= ZoneEquipConfig(ZoneEquipConfigLoop).NumReturnNodes; ++retNode) {
                                if (ZoneEquipConfig(ZoneEquipConfigLoop).ReturnNode(retNode) == InletNode) {
                                    ZoneEquipConfig(ZoneEquipConfigLoop).ReturnNodePlenumNum = ZonePlenumLoop;
                                    dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).ZoneEqNum(InletNodeLoop) = ZoneEquipConfigLoop;
                                }
                            }
                        }
                        // count the ADUs that can leak to this plenum
                        for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                            if (AirDistUnit(ADUNum).ZoneEqNum == dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).ZoneEqNum(InletNodeLoop)) {
                                AirDistUnit(ADUNum).RetPlenumNum = ZonePlenumLoop;
                                ++NumADUsToPlen;
                            }
                        }
                    }
                }
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).ADUIndex.allocate(NumADUsToPlen);
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).NumADUs = NumADUsToPlen;
                // fill the list of air distribution units that can leak to this plenum
                if (NumADUsToPlen > 0) {
                    for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                        if (AirDistUnit(ADUNum).RetPlenumNum == ZonePlenumLoop) {
                            ++ADUsToPlenIndex;
                            dataZonePlenum.ZoneRetPlenCond(ZonePlenumLoop).ADUIndex(ADUsToPlenIndex) = ADUNum;
                        }
                    }
                }
            }

            // Check that all ADUs with leakage found a return plenum
            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                auto &thisADU(AirDistUnit(ADUNum));
                if ((thisADU.DownStreamLeak || thisADU.DownStreamLeak) && (thisADU.RetPlenumNum == 0)) {
                    ShowWarningError("No return plenum found for simple duct leakage for ZoneHVAC:AirDistributionUnit=" + thisADU.Name +
                                     " in Zone=" + ZoneEquipConfig(thisADU.ZoneEqNum).ZoneName);
                    ShowContinueError("Leakage will be ignored for this ADU.");
                    thisADU.UpStreamLeak = false;
                    thisADU.DownStreamLeak = false;
                    thisADU.UpStreamLeakFrac = 0.0;
                    thisADU.DownStreamLeakFrac = 0.0;
                }
            }

            dataZonePlenum.InitAirZoneReturnPlenumOneTimeFlag = false;
        }

        // Do the Begin Environment initializations
        if (dataZonePlenum.InitAirZoneReturnPlenumEnvrnFlag && BeginEnvrnFlag) {

            for (PlenumZoneNum = 1; PlenumZoneNum <= dataZonePlenum.NumZoneReturnPlenums; ++PlenumZoneNum) {

                ZoneNodeNum = dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).ZoneNodeNum;
                Node(ZoneNodeNum).Temp = 20.0;
                Node(ZoneNodeNum).MassFlowRate = 0.0;
                Node(ZoneNodeNum).Quality = 1.0;
                Node(ZoneNodeNum).Press = OutBaroPress;
                Node(ZoneNodeNum).HumRat = OutHumRat;
                Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum).Temp, Node(ZoneNodeNum).HumRat);

                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).ZoneTemp = 20.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).ZoneHumRat = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).ZoneEnthalpy = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletTemp = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletHumRat = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletEnthalpy = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletPressure = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRate = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRateMaxAvail = 0.0;
                dataZonePlenum.ZoneRetPlenCond(PlenumZoneNum).InletMassFlowRateMinAvail = 0.0;
            }

            dataZonePlenum.InitAirZoneReturnPlenumEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) {
            dataZonePlenum.InitAirZoneReturnPlenumEnvrnFlag = true;
        }

        // Transfer the node data to ZoneRetPlenCond data structure
        for (NodeNum = 1; NodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++NodeNum) {

            InletNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode(NodeNum);
            // Set all of the inlet mass flow variables from the nodes
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(NodeNum) = Node(InletNode).MassFlowRate;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail(NodeNum) = Node(InletNode).MassFlowRateMaxAvail;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail(NodeNum) = Node(InletNode).MassFlowRateMinAvail;
            //    ! Set all of the inlet state variables from the inlet nodes
            //    ZoneRetPlenCond(ZonePlenumNum)%InletTemp(NodeNum)         = Node(InletNode)%Temp
            //    ZoneRetPlenCond(ZonePlenumNum)%InletHumRat(NodeNum)       = Node(InletNode)%HumRat
            //    ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy(NodeNum)     = Node(InletNode)%Enthalpy
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletPressure(NodeNum) = Node(InletNode).Press;
        }

        ZoneNodeNum = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;
        // Set the induced air flow rates and conditions
        for (NodeNum = 1; NodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++NodeNum) {
            InducedNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedNode(NodeNum);
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate(NodeNum) = Node(InducedNode).MassFlowRate;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMaxAvail(NodeNum) = Node(InducedNode).MassFlowRateMaxAvail;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRateMinAvail(NodeNum) = Node(InducedNode).MassFlowRateMinAvail;

            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedTemp(NodeNum) = Node(ZoneNodeNum).Temp;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedHumRat(NodeNum) = Node(ZoneNodeNum).HumRat;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy(NodeNum) = Node(ZoneNodeNum).Enthalpy;
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedPressure(NodeNum) = Node(ZoneNodeNum).Press;
            if (Contaminant.CO2Simulation) {
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedCO2(NodeNum) = Node(ZoneNodeNum).CO2;
            }
            if (Contaminant.GenericContamSimulation) {
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedGenContam(NodeNum) = Node(ZoneNodeNum).GenContam;
            }
        }

        // Add stuff to calculate conduction inputs to the zone plenum
        // Now load the zone conditions
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneTemp = Node(ZoneNodeNum).Temp;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat = Node(ZoneNodeNum).HumRat;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy = Node(ZoneNodeNum).Enthalpy;
    }

    void InitAirZoneSupplyPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum, bool const FirstHVACIteration, bool const FirstCall)
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

        static bool MyEnvrnFlag(true);

        // Do the Begin Environment initializations
        if (MyEnvrnFlag && BeginEnvrnFlag) {

            for (PlenumZoneNum = 1; PlenumZoneNum <= dataZonePlenum.NumZoneSupplyPlenums; ++PlenumZoneNum) {

                ZoneNodeNum = dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).ZoneNodeNum;
                Node(ZoneNodeNum).Temp = 20.0;
                Node(ZoneNodeNum).MassFlowRate = 0.0;
                Node(ZoneNodeNum).Quality = 1.0;
                Node(ZoneNodeNum).Press = OutBaroPress;
                Node(ZoneNodeNum).HumRat = OutHumRat;
                Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum).Temp, Node(ZoneNodeNum).HumRat);

                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).ZoneTemp = 20.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).ZoneHumRat = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).ZoneEnthalpy = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletTemp = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletHumRat = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletEnthalpy = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletPressure = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRate = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRateMaxAvail = 0.0;
                dataZonePlenum.ZoneSupPlenCond(PlenumZoneNum).InletMassFlowRateMinAvail = 0.0;
            }

            MyEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) {
            MyEnvrnFlag = true;
        }

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.

        InletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletNode;
        ZoneNodeNum = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

        if (FirstHVACIteration && FirstCall) {
            if (Node(InletNode).MassFlowRate > 0.0) {
                Node(ZoneNodeNum).MassFlowRate = Node(InletNode).MassFlowRate;
                for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                    OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                    Node(OutletNode).MassFlowRate = Node(InletNode).MassFlowRate / dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes;
                }
            }
            if (Node(InletNode).MassFlowRateMaxAvail > 0.0) {
                Node(ZoneNodeNum).MassFlowRateMaxAvail = Node(InletNode).MassFlowRateMaxAvail;
                for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                    OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                    Node(OutletNode).MassFlowRateMaxAvail = Node(InletNode).MassFlowRateMaxAvail / dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes;
                }
            }

        } // For FirstHVACIteration and FirstCall

        if (FirstCall) {

            if (Node(InletNode).MassFlowRateMaxAvail == 0.0) { // For Node inlet Max Avail = 0.0

                for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                    OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
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
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneTemp = Node(ZoneNodeNum).Temp;
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat = Node(ZoneNodeNum).HumRat;
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy = Node(ZoneNodeNum).Enthalpy;

            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                Node(OutletNode).Press = Node(InletNode).Press;
                Node(OutletNode).Quality = Node(InletNode).Quality;
            }

            Node(ZoneNodeNum).Press = Node(InletNode).Press;
            Node(ZoneNodeNum).Quality = Node(InletNode).Quality;

        } else { // On the second call from the ZoneEquipManager this is where the flows are passed back to
            // the supply plenum inlet.
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate(NodeIndex) = Node(OutletNode).MassFlowRate;
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail(NodeIndex) = Node(OutletNode).MassFlowRateMaxAvail;
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail(NodeIndex) = Node(OutletNode).MassFlowRateMinAvail;
            }

        } // For FirstCall
    }

    void CalcAirZoneReturnPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   November 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using DataDefineEquip::AirDistUnit;
        using DataDefineEquip::NumAirDistUnits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNodeNum(0);            // inlet node number
        int IndNum(0);                  // induced air index
        int ADUNum(0);                  // air distribution unit number
        int ADUListIndex(0);            // air distribution unit index in zone return plenum data structure
        Real64 TotIndMassFlowRate(0.0); // total induced air mass flow rate [kg/s]

        // Reset the totals to zero before they are summed.
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletTemp = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure = 0.0;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = 0.0;
        TotIndMassFlowRate = 0.0;

        for (InletNodeNum = 1; InletNodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate += dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum);
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail += dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail(InletNodeNum);
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail += dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail(InletNodeNum);
        }

        if (dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {

            // "Momentum balance" to get outlet air pressure
            for (InletNodeNum = 1; InletNodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {

                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure += dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletPressure(InletNodeNum) *
                                                                 dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                                                                 dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
            }

        } else {
            // Mass Flow in air loop is zero and loop is not operating.
            // Arbitrarily set the output to the first inlet leg
            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletPressure(1);
        }

        // add in the leak flow rate, if any. Don't alter the pressure calc (it is not used anyway)
        for (ADUListIndex = 1; ADUListIndex <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumADUs; ++ADUListIndex) {
            ADUNum = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ADUIndex(ADUListIndex);
            if (AirDistUnit(ADUNum).UpStreamLeak || AirDistUnit(ADUNum).DownStreamLeak) {
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate +=
                    AirDistUnit(ADUNum).MassFlowRateUpStrLk + AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail += AirDistUnit(ADUNum).MaxAvailDelta;
                dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail += AirDistUnit(ADUNum).MinAvailDelta;
            }
        }
        // Sum up induced air flow rate
        for (IndNum = 1; IndNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++IndNum) {
            TotIndMassFlowRate += dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedMassFlowRate(IndNum);
        }

        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate -= TotIndMassFlowRate;

        // Set the Plenum Outlet to the Zone Node conditions
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletHumRat = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneHumRat;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneEnthalpy;
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletTemp = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneTemp;
        // make sure the MassFlowMaxAvail >= MassFlowRate
        dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail =
            max(dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail, dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate);
    }

    void CalcAirZoneSupplyPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum, bool const FirstCall)
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
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletHumRat(NodeIndex) = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneHumRat;
            }

            // Energy balance to get outlet air enthalpy
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy(NodeIndex) = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneEnthalpy;
            }

            // Set outlet temperatures equal to inlet temperature
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletTemp(NodeIndex) = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneTemp;
            }

        } else {
            // This is the second time through and this is where the mass flows from the outlets are
            // summed and then assigned upstream to the inlet node.
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate = 0.0;
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail = 0.0;
            dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail = 0.0;
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate += dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRate(NodeIndex);
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail += dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail(NodeIndex);
                dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail += dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail(NodeIndex);
            }
        }
    }

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the ZonePlenum Module
    // *****************************************************************************

    void UpdateAirZoneReturnPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   November 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using DataContaminantBalance::Contaminant;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;
        int InletNode;
        int ZoneNode;
        int InletNodeNum;
        int InducedNode; // the node number of an induced air outlet node
        int IndNum;      // the induced air outlet index in ZoneRetPlenCond

        OutletNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletNode;
        InletNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode(1);
        ZoneNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).ZoneNodeNum;

        // Set the outlet air nodes of the ZonePlenum
        Node(OutletNode).MassFlowRate = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
        Node(OutletNode).MassFlowRateMaxAvail = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail;
        Node(OutletNode).MassFlowRateMinAvail = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail;

        Node(ZoneNode).MassFlowRate = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
        Node(ZoneNode).MassFlowRateMaxAvail = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMaxAvail;
        Node(ZoneNode).MassFlowRateMinAvail = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRateMinAvail;
        Node(ZoneNode).Press = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure;

        Node(OutletNode).Temp = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletTemp;
        Node(OutletNode).HumRat = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletHumRat;
        Node(OutletNode).Enthalpy = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletEnthalpy;
        Node(OutletNode).Press = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletPressure;
        for (IndNum = 1; IndNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInducedNodes; ++IndNum) {
            InducedNode = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedNode(IndNum);
            Node(InducedNode).Temp = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedTemp(IndNum);
            Node(InducedNode).HumRat = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedHumRat(IndNum);
            Node(InducedNode).Enthalpy = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedEnthalpy(IndNum);
            Node(InducedNode).Press = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedPressure(IndNum);
            if (Contaminant.CO2Simulation) {
                Node(InducedNode).CO2 = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedCO2(IndNum);
            }
            if (Contaminant.GenericContamSimulation) {
                Node(InducedNode).GenContam = dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InducedGenContam(IndNum);
            }
            Node(InducedNode).Quality = Node(InletNode).Quality;
        }

        // Set the outlet nodes for properties that are just pass through and not used
        Node(OutletNode).Quality = Node(InletNode).Quality;
        Node(ZoneNode).Quality = Node(InletNode).Quality;

        // Set the outlet node contaminant properties if needed. The zone contaminant conditions are calculated in ZoneContaminantPredictorCorrector
        if (Contaminant.CO2Simulation) {
            if (dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {
                // CO2 balance to get outlet air CO2
                Node(OutletNode).CO2 = 0.0;
                for (InletNodeNum = 1; InletNodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
                    Node(OutletNode).CO2 += Node(dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode(InletNodeNum)).CO2 *
                                            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                                            dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
                }
                Node(ZoneNode).CO2 = Node(OutletNode).CO2;
            } else {
                Node(OutletNode).CO2 = Node(ZoneNode).CO2;
            }
        }
        if (Contaminant.GenericContamSimulation) {
            if (dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate > 0.0) {
                // GenContam balance to get outlet air GenContam
                Node(OutletNode).GenContam = 0.0;
                for (InletNodeNum = 1; InletNodeNum <= dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).NumInletNodes; ++InletNodeNum) {
                    Node(OutletNode).GenContam += Node(dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletNode(InletNodeNum)).GenContam *
                                                  dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).InletMassFlowRate(InletNodeNum) /
                                                  dataZonePlenum.ZoneRetPlenCond(ZonePlenumNum).OutletMassFlowRate;
                }
                Node(ZoneNode).GenContam = Node(OutletNode).GenContam;
            } else {
                Node(OutletNode).GenContam = Node(ZoneNode).GenContam;
            }
        }
    }

    void UpdateAirZoneSupplyPlenum(ZonePlenumData &dataZonePlenum, int const ZonePlenumNum, bool &PlenumInletChanged, bool const FirstCall)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   March 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // METHODOLOGY EMPLOYED:
        // Similar to the Zone Splitter component but with interactions to the plenum zone.

        // Using/Aliasing
        using DataContaminantBalance::Contaminant;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const FlowRateToler(0.01); // Tolerance for mass flow rate convergence (in kg/s)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;
        int InletNode;
        int ZoneNode;
        int NodeIndex;

        OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(1);
        InletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletNode;
        ZoneNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).ZoneNodeNum;

        // On the FirstCall the State properties are passed through and the mass flows are not dealt with
        if (FirstCall) {
            // Set the outlet nodes for properties that just pass through and not used
            for (NodeIndex = 1; NodeIndex <= dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).NumOutletNodes; ++NodeIndex) {
                OutletNode = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletNode(NodeIndex);
                Node(OutletNode).Temp = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletTemp(NodeIndex);
                Node(OutletNode).HumRat = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletHumRat(NodeIndex);
                Node(OutletNode).Enthalpy = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).OutletEnthalpy(NodeIndex);
                if (Contaminant.CO2Simulation) {
                    Node(OutletNode).CO2 = Node(InletNode).CO2;
                }
                if (Contaminant.GenericContamSimulation) {
                    Node(OutletNode).GenContam = Node(InletNode).GenContam;
                }
            }

            if (Contaminant.CO2Simulation) {
                Node(ZoneNode).CO2 = Node(InletNode).CO2;
            }
            if (Contaminant.GenericContamSimulation) {
                Node(ZoneNode).GenContam = Node(InletNode).GenContam;
            }

        } else {
            // The second time through just updates the mass flow conditions back upstream
            // to the inlet.

            if (std::abs(Node(InletNode).MassFlowRate - dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate) > FlowRateToler) {
                PlenumInletChanged = true;
            }

            Node(InletNode).MassFlowRate = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate;
            Node(InletNode).MassFlowRateMaxAvail = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail;
            Node(InletNode).MassFlowRateMinAvail = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail;

            Node(ZoneNode).MassFlowRate = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRate;
            Node(ZoneNode).MassFlowRateMaxAvail = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMaxAvail;
            Node(ZoneNode).MassFlowRateMinAvail = dataZonePlenum.ZoneSupPlenCond(ZonePlenumNum).InletMassFlowRateMinAvail;

        } // For FirstCall
    }

    int GetReturnPlenumIndex(EnergyPlusData &state, int const &ExNodeNum)
    {

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PlenumNum;      // loop counter
        int InducedNodeNum; // loop counter
        int WhichPlenum;    // index to return plenum

        // Obtains and Allocates ZonePlenum related parameters from input file
        if (state.dataZonePlenum.GetInputFlag) { // First time subroutine has been entered
            GetZonePlenumInput(state);
            state.dataZonePlenum.GetInputFlag = false;
        }

        WhichPlenum = 0;
        if (state.dataZonePlenum.NumZoneReturnPlenums > 0) {
            for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum.NumZoneReturnPlenums; ++PlenumNum) {
                if (ExNodeNum != state.dataZonePlenum.ZoneRetPlenCond(PlenumNum).OutletNode) continue;
                WhichPlenum = PlenumNum;
                break;
            }
            if (WhichPlenum == 0) {
                for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum.NumZoneReturnPlenums; ++PlenumNum) {
                    for (InducedNodeNum = 1; InducedNodeNum <= state.dataZonePlenum.ZoneRetPlenCond(PlenumNum).NumInducedNodes; ++InducedNodeNum) {
                        if (ExNodeNum != state.dataZonePlenum.ZoneRetPlenCond(PlenumNum).InducedNode(InducedNodeNum)) continue;
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
        if (state.dataZonePlenum.GetInputFlag) { // First time subroutine has been entered
            GetZonePlenumInput(state);
            state.dataZonePlenum.GetInputFlag = false;
        }

        ReturnPlenumName = " ";
        if (state.dataZonePlenum.NumZoneReturnPlenums > 0) {
            ReturnPlenumName = state.dataZonePlenum.ZoneRetPlenCond(ReturnPlenumIndex).ZonePlenumName;
        }
    }

    int getReturnPlenumIndexFromInletNode(EnergyPlusData &state, int const &InNodeNum)
    {

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PlenumNum; // loop counter
        int InNodeCtr; // loop counter
        int thisPlenum;

        // Obtains and Allocates ZonePlenum related parameters from input file
        if (state.dataZonePlenum.GetInputFlag) { // First time subroutine has been entered
            GetZonePlenumInput(state);
            state.dataZonePlenum.GetInputFlag = false;
        }

        thisPlenum = 0;
        if (state.dataZonePlenum.NumZoneReturnPlenums > 0) {
            for (PlenumNum = 1; PlenumNum <= state.dataZonePlenum.NumZoneReturnPlenums; ++PlenumNum) {
                for (InNodeCtr = 1; InNodeCtr <= state.dataZonePlenum.ZoneRetPlenCond(PlenumNum).NumInletNodes; ++InNodeCtr) {
                    if (InNodeNum != state.dataZonePlenum.ZoneRetPlenCond(PlenumNum).InletNode(InNodeCtr)) continue;
                    thisPlenum = PlenumNum;
                    break;
                }
                if (thisPlenum > 0) break;
            }
        }

        return thisPlenum;
    }

} // namespace ZonePlenum

} // namespace EnergyPlus
