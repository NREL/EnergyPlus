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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::DataZoneEquipment {

// MODULE INFORMATION
//             AUTHOR:  Russ Taylor
//       DATE WRITTEN:  June 1998

// PURPOSE OF THIS MODULE:
// This module contains variable declarations for zone equipment configuration data

Array1D_string const cValidSysAvailManagerCompTypes(NumValidSysAvailZoneComponents,
                                                    {"ZoneHVAC:FourPipeFanCoil",
                                                     "ZoneHVAC:PackagedTerminalHeatPump",
                                                     "ZoneHVAC:PackagedTerminalAirConditioner",
                                                     "ZoneHVAC:WaterToAirHeatPump",
                                                     "ZoneHVAC:WindowAirConditioner",
                                                     "ZoneHVAC:UnitHeater",
                                                     "ZoneHVAC:UnitVentilator",
                                                     "ZoneHVAC:EnergyRecoveryVentilator",
                                                     "ZoneHVAC:VentilatedSlab",
                                                     "ZoneHVAC:OutdoorAirUnit",
                                                     "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
                                                     "ZoneHVAC:IdealLoadsAirSystem",
                                                     "ZoneHVAC:EvaporativeCoolerUnit",
                                                     "ZoneHVAC:HybridUnitaryHVAC"});

void GetZoneEquipmentData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   June 1997
    //       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Get all the system related equipment which may be attached to
    // a zone

    // Using/Aliasing
    using BranchNodeConnections::SetUpCompSets;
    using NodeInputManager::CheckUniqueNodes;
    using NodeInputManager::EndUniqueNodeCheck;
    using NodeInputManager::GetNodeNums;
    using NodeInputManager::GetOnlySingleNode;
    using NodeInputManager::InitUniqueNodeCheck;
    using namespace DataHVACGlobals;
    using namespace DataLoopNode;
    using namespace ScheduleManager;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetZoneEquipmentData: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;
    int NumNums;
    int NodeNum;
    int PathNum;
    int CompNum;
    int ControlledZoneNum;
    int ControlledZoneLoop;
    int ZoneEquipTypeNum;
    int ZoneEquipListNum;
    int IOStat;
    std::string InletNodeListName;
    std::string ExhaustNodeListName;
    std::string ReturnNodeListName;
    std::string ReturnFlowBasisNodeListName;
    Array1D_string AlphArray;
    Array1D<Real64> NumArray;
    int MaxAlphas;
    int MaxNums;
    int NumParams;
    int NumNodes;
    Array1D_int NodeNums;
    int Counter;
    bool IsNotOK; // Flag to verify nam
    bool NodeListError;
    bool UniqueNodeError;
    int NumOfControlledZones;        // The number of Controlled Zone Equip Configuration objects
    std::string CurrentModuleObject; // Object type for getting and error messages
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    int maxEquipCount;
    int numEquipCount;
    int overallEquipCount;
    int Loop1;
    int Loop2;

    auto &TermUnitSizing(state.dataSize->TermUnitSizing);

    struct EquipListAudit
    {
        // Members
        std::string ObjectType;
        std::string ObjectName;
        int OnListNum;

        // Default Constructor
        EquipListAudit() : OnListNum(0)
        {
        }
    };

    // Object Data
    Array1D<EquipListAudit> ZoneEquipListAcct;

    ExhaustNodeListName = "";
    InletNodeListName = "";
    ReturnNodeListName = "";
    ReturnFlowBasisNodeListName = "";

    // Look in the input file for zones with air loop and zone equipment attached

    NumOfControlledZones = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneHVAC:EquipmentConnections");
    state.dataZoneEquip->NumOfZoneEquipLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
        state, "ZoneHVAC:EquipmentList"); // Look for lists of equipment data - there should
    // be as many of these as there are controlled zones
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
    NodeNums.dimension(NumParams, 0);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "ZoneHVAC:EquipmentList", NumParams, NumAlphas, NumNums);
    MaxAlphas = NumAlphas;
    MaxNums = NumNums;
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "ZoneHVAC:EquipmentConnections", NumParams, NumAlphas, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNums = max(MaxNums, NumNums);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC:SupplyPath", NumParams, NumAlphas, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNums = max(MaxNums, NumNums);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC:ReturnPath", NumParams, NumAlphas, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNums = max(MaxNums, NumNums);
    AlphArray.allocate(MaxAlphas);
    NumArray.dimension(MaxNums, 0.0);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNums);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNums, true);

    if (!allocated(state.dataZoneEquip->SupplyAirPath)) {
        // Look for and read in the air supply path
        // component (splitters) information for each zone
        state.dataZoneEquip->NumSupplyAirPaths = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:SupplyPath");
        state.dataZoneEquip->SupplyAirPath.allocate(state.dataZoneEquip->NumSupplyAirPaths);
    }

    if (!allocated(state.dataZoneEquip->ReturnAirPath)) {
        // Look for and read in the air return path
        // component (mixers & plenums) information for each zone
        state.dataZoneEquip->NumReturnAirPaths = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:ReturnPath");
        state.dataZoneEquip->ReturnAirPath.allocate(state.dataZoneEquip->NumReturnAirPaths);
    }

    state.dataZoneEquip->ZoneEquipConfig.allocate(state.dataGlobal->NumOfZones); // Allocate the array containing the configuration
    // data for each zone to the number of controlled zones
    // found in the input file.  This may or may not
    // be the same as the number of zones in the building
    state.dataZoneEquip->ZoneEquipList.allocate(state.dataGlobal->NumOfZones);
    state.dataZoneEquip->ZoneEquipAvail.dimension(state.dataGlobal->NumOfZones, NoAction);
    state.dataZoneEquip->UniqueZoneEquipListNames.reserve(state.dataGlobal->NumOfZones);

    if (state.dataZoneEquip->NumOfZoneEquipLists != NumOfControlledZones) {
        ShowSevereError(state,
                        format("{}Number of Zone Equipment lists [{}] not equal Number of Controlled Zones [{}]",
                               RoutineName,
                               state.dataZoneEquip->NumOfZoneEquipLists,
                               NumOfControlledZones));
        ShowContinueError(state, "..Each Controlled Zone [ZoneHVAC:EquipmentConnections] must have a corresponding (unique) ZoneHVAC:EquipmentList");
        ShowFatalError(state, "GetZoneEquipment: Incorrect number of zone equipment lists");
    }

    if (NumOfControlledZones > state.dataGlobal->NumOfZones) {
        ShowSevereError(state,
                        format("{}Number of Controlled Zone objects [{}] greater than Number of Zones [{}]",
                               RoutineName,
                               NumOfControlledZones,
                               state.dataGlobal->NumOfZones));
        ShowFatalError(state, std::string{RoutineName} + "Too many ZoneHVAC:EquipmentConnections objects.");
    }

    InitUniqueNodeCheck(state, "ZoneHVAC:EquipmentConnections");

    overallEquipCount = 0;
    int locTermUnitSizingCounter = 0; // will increment for every zone inlet node

    auto &Zone(state.dataHeatBal->Zone);

    for (ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfControlledZones; ++ControlledZoneLoop) {

        CurrentModuleObject = "ZoneHVAC:EquipmentConnections";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 ControlledZoneLoop,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields); // Get Equipment | data for one zone

        ControlledZoneNum = UtilityRoutines::FindItemInList(AlphArray(1), Zone);

        if (ControlledZoneNum == 0) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\"");
            ShowContinueError(state, "..Requested Controlled Zone not among Zones, remaining items for this object not processed.");
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            continue;
        } else {
            //    Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%ZoneEquipConfigNum = ControlledZoneNum
            if (Zone(ControlledZoneNum).IsControlled) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\"");
                ShowContinueError(state, "..Duplicate Controlled Zone entered, only one " + CurrentModuleObject + " per zone is allowed.");
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                continue;
            }
            Zone(ControlledZoneNum).IsControlled = true;
            Zone(ControlledZoneNum).ZoneEqNum = ControlledZoneNum;
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled = true;
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum = ControlledZoneNum;
        }
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName = AlphArray(1); // for x-referencing with the geometry data

        IsNotOK = false;
        GlobalNames::IntraObjUniquenessCheck(
            state, AlphArray(2), CurrentModuleObject, cAlphaFields(2), state.dataZoneEquip->UniqueZoneEquipListNames, IsNotOK);
        if (IsNotOK) {
            ShowContinueError(state, "..another Controlled Zone has been assigned that " + cAlphaFields(2) + '.');
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListName = AlphArray(2); // the name of the list containing all the zone eq.
        InletNodeListName = AlphArray(3);
        ExhaustNodeListName = AlphArray(4);
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode = GetOnlySingleNode(state,
                                                                                             AlphArray(5),
                                                                                             state.dataZoneEquip->GetZoneEquipmentDataErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             AlphArray(1),
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::ZoneNode,
                                                                                             1,
                                                                                             ObjectIsNotParent); // all zone air state variables are
        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode == 0) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\", invalid");
            ShowContinueError(state, cAlphaFields(5) + " must be present.");
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        } else {
            UniqueNodeError = false;
            CheckUniqueNodes(state, cAlphaFields(5), "NodeName", UniqueNodeError, AlphArray(5), _, AlphArray(1));
            if (UniqueNodeError) {
                // ShowContinueError(state,  "Occurs for " + trim( cAlphaFields( 1 ) ) + " = " + trim( AlphArray( 1 ) ) );
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            }
        }
        // assigned to this node
        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum > 0) {
            Zone(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).SystemZoneNodeNumber =
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
        } // This error already detected and program will be terminated.

        ReturnNodeListName = AlphArray(6);
        if (lAlphaBlanks(7)) {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum = GetScheduleIndex(state, AlphArray(7));
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + ": invalid " + cAlphaFields(7) + " entered =" + AlphArray(7) + " for " +
                                    cAlphaFields(1) + '=' + AlphArray(1));
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            }
        }
        ReturnFlowBasisNodeListName = AlphArray(8);

        // Read in the equipment type, name and sequence information
        // for each equipment list

        CurrentModuleObject = "ZoneHVAC:EquipmentList";

        ZoneEquipListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(
            state, CurrentModuleObject, state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListName);
        if (ZoneEquipListNum > 0) {

            EquipList &thisZoneEquipList = state.dataZoneEquip->ZoneEquipList(ControlledZoneNum);

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     ZoneEquipListNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields); //  data for one zone
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, state.dataZoneEquip->GetZoneEquipmentDataErrorsFound);
            thisZoneEquipList.Name = AlphArray(1);

            if (!lAlphaBlanks(2)) {
                if (UtilityRoutines::SameString(AlphArray(2), "SequentialLoad")) {
                    thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::SequentialLoading;
                } else if (UtilityRoutines::SameString(AlphArray(2), "UniformLoad")) {
                    thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::UniformLoading;
                } else if (UtilityRoutines::SameString(AlphArray(2), "UniformPLR")) {
                    thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::UniformPLRLoading;
                } else if (UtilityRoutines::SameString(AlphArray(2), "SequentialUniformPLR")) {
                    thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::SequentialUniformPLRLoading;
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\", Invalid choice.");
                    ShowContinueError(state, "..." + cAlphaFields(2) + "=\"" + AlphArray(2) + "\".");
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
            }
            const int nAlphasInExtensible = 4;
            const int nNumsInExtensible = 2;
            const int nAlphasBeforeExtensible = 2;
            const int nNumsBeforeExtensible = 0;
            maxEquipCount = 0;
            numEquipCount = (NumAlphas - nAlphasBeforeExtensible) / nAlphasInExtensible;
            if (numEquipCount * nAlphasInExtensible != (NumAlphas - nAlphasBeforeExtensible)) ++numEquipCount;
            for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= numEquipCount; ++ZoneEquipTypeNum) {
                if (!lAlphaBlanks(nAlphasInExtensible * (ZoneEquipTypeNum - 1) + nAlphasBeforeExtensible + 1) &&
                    !lAlphaBlanks(nAlphasInExtensible * (ZoneEquipTypeNum - 1) + nAlphasBeforeExtensible + 2)) {
                    ++maxEquipCount;
                    continue;
                }
                ShowWarningError(state,
                                 format("{}{}=\"{}\", truncated list at blank field; object count={}",
                                        RoutineName,
                                        CurrentModuleObject,
                                        thisZoneEquipList.Name,
                                        maxEquipCount));
                break;
            }

            overallEquipCount += maxEquipCount;
            thisZoneEquipList.NumOfEquipTypes = maxEquipCount;
            thisZoneEquipList.EquipType.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.EquipType_Num.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.compPointer.resize(thisZoneEquipList.NumOfEquipTypes + 1);
            thisZoneEquipList.EquipName.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.EquipIndex.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.EquipData.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.CoolingPriority.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.HeatingPriority.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.CoolingCapacity.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.HeatingCapacity.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.SequentialCoolingFractionSchedPtr.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.SequentialHeatingFractionSchedPtr.allocate(thisZoneEquipList.NumOfEquipTypes);
            thisZoneEquipList.EquipType = "";
            thisZoneEquipList.EquipType_Num = 0;
            thisZoneEquipList.EquipName = "";
            thisZoneEquipList.EquipIndex = 0;
            thisZoneEquipList.CoolingPriority = 0;
            thisZoneEquipList.HeatingPriority = 0;
            thisZoneEquipList.CoolingCapacity = 0;
            thisZoneEquipList.HeatingCapacity = 0;
            thisZoneEquipList.SequentialCoolingFractionSchedPtr = 0;
            thisZoneEquipList.SequentialHeatingFractionSchedPtr = 0;

            for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= thisZoneEquipList.NumOfEquipTypes; ++ZoneEquipTypeNum) {
                const int ZoneEquipTypeIdx = ZoneEquipTypeNum - 1;
                thisZoneEquipList.EquipType(ZoneEquipTypeNum) = AlphArray(nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 1);
                thisZoneEquipList.EquipName(ZoneEquipTypeNum) = AlphArray(nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 2);
                ValidateComponent(state,
                                  thisZoneEquipList.EquipType(ZoneEquipTypeNum),
                                  thisZoneEquipList.EquipName(ZoneEquipTypeNum),
                                  IsNotOK,
                                  CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "In " + CurrentModuleObject + '=' + thisZoneEquipList.Name);
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
                thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) =
                    nint(NumArray(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 1));
                if ((thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) < 0) ||
                    (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > thisZoneEquipList.NumOfEquipTypes)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ShowContinueError(state,
                                      format("invalid {}=[{}].",
                                             cNumericFields(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 1),
                                             thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum)));
                    ShowContinueError(state, "equipment sequence must be > 0 and <= number of equipments in the list.");
                    if (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > 0)
                        ShowContinueError(state, format("only {} in the list.", thisZoneEquipList.NumOfEquipTypes));
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }

                thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) =
                    nint(NumArray(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 2));
                if ((thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) < 0) ||
                    (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > thisZoneEquipList.NumOfEquipTypes)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ShowContinueError(state,
                                      format("invalid {}=[{}].",
                                             cNumericFields(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 2),
                                             thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum)));
                    ShowContinueError(state, "equipment sequence must be > 0 and <= number of equipments in the list.");
                    if (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > 0)
                        ShowContinueError(state, format("only {} in the list.", thisZoneEquipList.NumOfEquipTypes));
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }

                const int coolingFractionArrayIdx = nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 3;
                if (lAlphaBlanks(coolingFractionArrayIdx)) {
                    thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) =
                        GetScheduleIndex(state, AlphArray(coolingFractionArrayIdx));
                    if (thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) == 0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ShowContinueError(state,
                                          "invalid " + cAlphaFields(coolingFractionArrayIdx) + "=[" + AlphArray(coolingFractionArrayIdx) + "].");
                        ShowContinueError(state, "Schedule does not exist.");
                        state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                    }
                }

                const int heatingFractionArrayIdx = nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 4;
                if (lAlphaBlanks(heatingFractionArrayIdx)) {
                    thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) =
                        GetScheduleIndex(state, AlphArray(heatingFractionArrayIdx));
                    if (thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) == 0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ShowContinueError(state,
                                          "invalid " + cAlphaFields(heatingFractionArrayIdx) + "=[" + AlphArray(heatingFractionArrayIdx) + "].");
                        ShowContinueError(state, "Schedule does not exist.");
                        state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                    }
                }

                // do this here for initial prototype, but later will call all the equipment in a separate function to see who is on - maybe
                if (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > 0) ++thisZoneEquipList.NumAvailHeatEquip;
                if (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > 0) ++thisZoneEquipList.NumAvailCoolEquip;

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(thisZoneEquipList.EquipType(ZoneEquipTypeNum)));

                    if (SELECT_CASE_var == "ZONEHVAC:AIRDISTRIBUTIONUNIT") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = AirDistUnit_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:WINDOWAIRCONDITIONER") { // Window Air Conditioner
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = WindowAC_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALHEATPUMP") { // Packaged Terminal Heat Pump
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermHPAirToAir_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER") { // Packaged Terminal Air Conditioner
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermACAirToAir_Num;

                    } else if (SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM") { // Unitary System
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneUnitarySys_Num;
                        UnitarySystems::UnitarySys thisSys;
                        thisZoneEquipList.compPointer[ZoneEquipTypeNum] =
                            thisSys.factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, thisZoneEquipList.EquipName(ZoneEquipTypeNum), true, 0);

                    } else if (SELECT_CASE_var == "ZONEHVAC:DEHUMIDIFIER:DX") { // Zone dehumidifier
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneDXDehumidifier_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:WATERTOAIRHEATPUMP") { // Zone Water to Air Heat Pump
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermHPWaterToAir_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:FOURPIPEFANCOIL") { // 4-Pipe Fan Coil
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = FanCoil4Pipe_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:UNITVENTILATOR") { // Unit Ventilator
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UnitVentilator_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:UNITHEATER") { // Unit Heater
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UnitHeater_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:IDEALLOADSAIRSYSTEM") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PurchasedAir_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER") { // Hot Water Baseboard
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBWater_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER") { // Baseboard
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBWaterConvective_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC") { // Electric Baseboard
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBElectricConvective_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:COOLINGPANEL:RADIANTCONVECTIVE:WATER") { // Simple Cooling Panel
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = CoolingPanel_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:HIGHTEMPERATURERADIANT") { // High Temperature Radiators
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HiTempRadiant_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW") { // Low temperature radiant system (hydronic)
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                    } else if (SELECT_CASE_var ==
                               "ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW") { // Low temperature radiant system (hydronic, constant flow)
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:ELECTRIC") { // Low temperature radiant system (electric)
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                    } else if (SELECT_CASE_var == "FAN:ZONEEXHAUST") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneExhaustFan_Num;

                    } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HeatXchngr_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:ENERGYRECOVERYVENTILATOR") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ERVStandAlone_Num;

                    } else if (SELECT_CASE_var == "WATERHEATER:HEATPUMP:PUMPEDCONDENSER") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HPWaterHeater_Num;

                    } else if (SELECT_CASE_var == "WATERHEATER:HEATPUMP:WRAPPEDCONDENSER") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HPWaterHeater_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:VENTILATEDSLAB") { // Ventilated Slab
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = VentilatedSlab_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM") { // Steam Baseboard
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBSteam_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:OUTDOORAIRUNIT") { // Outdoor Air Unit
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = OutdoorAirUnit_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC") { // Radiant electric Baseboard
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBElectric_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW") { // VRF AC System
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = VRFTerminalUnit_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:REFRIGERATIONCHILLERSET") { // Refrigeration chiller designed for warehouse applications
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = RefrigerationAirChillerSet_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:FORCEDAIR:USERDEFINED") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UserDefinedZoneHVACForcedAir_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:EVAPORATIVECOOLERUNIT") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneEvaporativeCoolerUnit_Num;

                    } else if (SELECT_CASE_var == "ZONEHVAC:HYBRIDUNITARYHVAC") {
                        thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneHybridEvaporativeCooler_Num;

                    } else {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                        ShowContinueError(state, "..Invalid Equipment Type = " + thisZoneEquipList.EquipType(ZoneEquipTypeNum));
                        state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                    }
                }
            }

            for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= thisZoneEquipList.NumOfEquipTypes; ++ZoneEquipTypeNum) {
                if (count_eq(thisZoneEquipList.CoolingPriority, ZoneEquipTypeNum) > 1) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                    ShowContinueError(state,
                                      format("...multiple assignments for Zone Equipment Cooling Sequence={}, must be 1-1 correspondence between "
                                             "sequence assignments and number of equipments.",
                                             ZoneEquipTypeNum));
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                } else if (count_eq(thisZoneEquipList.CoolingPriority, ZoneEquipTypeNum) == 0) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                    ShowContinueError(state,
                                      format("...zero assigned to Zone Equipment Cooling Sequence={}, apparent gap in sequence assignments in "
                                             "this equipment list.",
                                             ZoneEquipTypeNum));
                }
                if (count_eq(thisZoneEquipList.HeatingPriority, ZoneEquipTypeNum) > 1) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                    ShowContinueError(state,
                                      format("...multiple assignments for Zone Equipment Heating or No-Load Sequence={}, must be 1-1 "
                                             "correspondence between sequence assignments and number of equipments.",
                                             ZoneEquipTypeNum));
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                } else if (count_eq(thisZoneEquipList.HeatingPriority, ZoneEquipTypeNum) == 0) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                    ShowContinueError(state,
                                      format("...zero assigned to Zone Equipment Heating or No-Load Sequence={}, apparent gap in sequence "
                                             "assignments in this equipment list.",
                                             ZoneEquipTypeNum));
                }
            }

        } else {
            ShowSevereError(
                state, std::string{RoutineName} + CurrentModuleObject + " not found = " + state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListName);
            ShowContinueError(
                state, "In ZoneHVAC:EquipmentConnections object, for Zone = " + state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }

        // End ZoneHVAC:EquipmentList

        NodeListError = false;
        GetNodeNums(state,
                    InletNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    "ZoneHVAC:EquipmentConnections",
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName,
                    DataLoopNode::NodeConnectionType::ZoneInlet,
                    1,
                    ObjectIsNotParent);

        if (!NodeListError) {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes = NumNodes;

            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeADUNum.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat.allocate(NumNodes);

            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(NodeNum) = NodeNums(NodeNum);
                UniqueNodeError = false;
                CheckUniqueNodes(state,
                                 "Zone Air Inlet Nodes",
                                 "NodeNumber",
                                 UniqueNodeError,
                                 _,
                                 NodeNums(NodeNum),
                                 state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
                if (UniqueNodeError) {
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(NodeNum) = 0;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeADUNum(NodeNum) = 0;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).InNode = 0;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).InNode = 0;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).OutNode = 0;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).OutNode = 0;
                ++locTermUnitSizingCounter;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).TermUnitSizingIndex = locTermUnitSizingCounter;
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).TermUnitSizingIndex = locTermUnitSizingCounter;
            }
        } else {
            ShowContinueError(state,
                              "Invalid Zone Air Inlet Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone = " +
                                  state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    ExhaustNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    "ZoneHVAC:EquipmentConnections",
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName,
                    DataLoopNode::NodeConnectionType::ZoneExhaust,
                    1,
                    ObjectIsNotParent);

        if (!NodeListError) {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes = NumNodes;

            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode.allocate(NumNodes);

            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(NodeNum) = NodeNums(NodeNum);
                UniqueNodeError = false;
                CheckUniqueNodes(state,
                                 "Zone Air Exhaust Nodes",
                                 "NodeNumber",
                                 UniqueNodeError,
                                 _,
                                 NodeNums(NodeNum),
                                 state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
                if (UniqueNodeError) {
                    // ShowContinueError(state,  "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
            }
        } else {
            ShowContinueError(state,
                              "Invalid Zone Air Exhaust Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                                  state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    ReturnNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    "ZoneHVAC:EquipmentConnections",
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName,
                    DataLoopNode::NodeConnectionType::ZoneReturn,
                    1,
                    ObjectIsNotParent);

        if (!NodeListError) {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes = NumNodes;

            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodeAirLoopNum.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodeInletNum.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).FixedReturnFlow.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodePlenumNum.allocate(NumNodes);
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode = 0;           // initialize to zero here
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodeAirLoopNum = 0; // initialize to zero here
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodeInletNum = 0;   // initialize to zero here
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).FixedReturnFlow = false;  // initialize to false here
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNodePlenumNum = 0;  // initialize to zero here

            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(NodeNum) = NodeNums(NodeNum);
                UniqueNodeError = false;
                CheckUniqueNodes(state,
                                 "Zone Return Air Nodes",
                                 "NodeNumber",
                                 UniqueNodeError,
                                 _,
                                 NodeNums(NodeNum),
                                 state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
                if (UniqueNodeError) {
                    // ShowContinueError(state,  "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
            }
        } else {
            ShowContinueError(state,
                              "Invalid Zone Return Air Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                                  state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    ReturnFlowBasisNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    "ZoneHVAC:EquipmentConnections",
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName,
                    DataLoopNode::NodeConnectionType::Sensor,
                    1,
                    ObjectIsNotParent);

        if (!NodeListError) {
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnFlowBasisNodes = NumNodes;

            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnFlowBasisNode.allocate(NumNodes);

            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnFlowBasisNode(NodeNum) = NodeNums(NodeNum);
            }
        } else {
            ShowContinueError(
                state,
                "Invalid Zone Return Air Node 1 Flow Rate Basis Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
            state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
        }

    } // end loop over controlled zones

    // Allocate TermUnitSizing array and set zone number
    if (locTermUnitSizingCounter > 0) {
        state.dataSize->NumAirTerminalUnits = locTermUnitSizingCounter;
        TermUnitSizing.allocate(state.dataSize->NumAirTerminalUnits);
        for (int loopZoneNum = 1; loopZoneNum <= state.dataGlobal->NumOfZones; ++loopZoneNum) {
            {
                auto &thisZoneEqConfig(state.dataZoneEquip->ZoneEquipConfig(loopZoneNum));
                for (int loopNodeNum = 1; loopNodeNum <= thisZoneEqConfig.NumInletNodes; ++loopNodeNum) {
                    TermUnitSizing(thisZoneEqConfig.AirDistUnitCool(loopNodeNum).TermUnitSizingIndex).CtrlZoneNum = loopZoneNum;
                }
            }
        }
    }
    if (state.dataZoneEquip->GetZoneEquipmentDataErrorsFound) {
        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + ", duplicate items NOT CHECKED due to previous errors.");
        overallEquipCount = 0;
    }
    if (overallEquipCount > 0) {
        ZoneEquipListAcct.allocate(overallEquipCount);
        overallEquipCount = 0;
        for (Loop1 = 1; Loop1 <= NumOfControlledZones; ++Loop1) {
            for (Loop2 = 1; Loop2 <= state.dataZoneEquip->ZoneEquipList(Loop1).NumOfEquipTypes; ++Loop2) {
                ++overallEquipCount;
                ZoneEquipListAcct(overallEquipCount).ObjectType = state.dataZoneEquip->ZoneEquipList(Loop1).EquipType(Loop2);
                ZoneEquipListAcct(overallEquipCount).ObjectName = state.dataZoneEquip->ZoneEquipList(Loop1).EquipName(Loop2);
                ZoneEquipListAcct(overallEquipCount).OnListNum = Loop1;
            }
        }
        // Now check for uniqueness
        for (Loop1 = 1; Loop1 <= overallEquipCount; ++Loop1) {
            for (Loop2 = Loop1 + 1; Loop2 <= overallEquipCount; ++Loop2) {
                if (ZoneEquipListAcct(Loop1).ObjectType != ZoneEquipListAcct(Loop2).ObjectType ||
                    ZoneEquipListAcct(Loop1).ObjectName != ZoneEquipListAcct(Loop2).ObjectName)
                    continue;
                // Duplicated -- not allowed
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ", duplicate items in ZoneHVAC:EquipmentList.");
                ShowContinueError(state, "Equipment: Type=" + ZoneEquipListAcct(Loop1).ObjectType + ", Name=" + ZoneEquipListAcct(Loop1).ObjectName);
                ShowContinueError(state, "Found on List=\"" + state.dataZoneEquip->ZoneEquipList(ZoneEquipListAcct(Loop1).OnListNum).Name + "\".");
                ShowContinueError(
                    state, "Equipment Duplicated on List=\"" + state.dataZoneEquip->ZoneEquipList(ZoneEquipListAcct(Loop2).OnListNum).Name + "\".");
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            }
        }
        ZoneEquipListAcct.deallocate();
    }

    // map ZoneEquipConfig%EquipListIndex to ZoneEquipList%Name

    for (ControlledZoneLoop = 1; ControlledZoneLoop <= state.dataGlobal->NumOfZones; ++ControlledZoneLoop) {
        state.dataZoneEquip->GetZoneEquipmentDataFound = UtilityRoutines::FindItemInList(
            state.dataZoneEquip->ZoneEquipList(ControlledZoneLoop).Name, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::EquipListName);
        if (state.dataZoneEquip->GetZoneEquipmentDataFound > 0)
            state.dataZoneEquip->ZoneEquipConfig(state.dataZoneEquip->GetZoneEquipmentDataFound).EquipListIndex = ControlledZoneLoop;
    } // end loop over controlled zones

    EndUniqueNodeCheck(state, "ZoneHVAC:EquipmentConnections");

    CurrentModuleObject = "AirLoopHVAC:SupplyPath";
    for (PathNum = 1; PathNum <= state.dataZoneEquip->NumSupplyAirPaths; ++PathNum) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 PathNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields); //  data for one zone
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, state.dataZoneEquip->GetZoneEquipmentDataErrorsFound);
        state.dataZoneEquip->SupplyAirPath(PathNum).Name = AlphArray(1);
        state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents = nint((double(NumAlphas) - 2.0) / 2.0);

        state.dataZoneEquip->SupplyAirPath(PathNum).InletNodeNum = GetOnlySingleNode(state,
                                                                                     AlphArray(2),
                                                                                     state.dataZoneEquip->GetZoneEquipmentDataErrorsFound,
                                                                                     CurrentModuleObject,
                                                                                     AlphArray(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     ObjectIsParent);

        state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType_Num.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType_Num = 0;
        state.dataZoneEquip->SupplyAirPath(PathNum).ComponentName.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->SupplyAirPath(PathNum).ComponentIndex.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->SupplyAirPath(PathNum).SplitterIndex.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->SupplyAirPath(PathNum).PlenumIndex.allocate(state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents);

        Counter = 3;

        for (CompNum = 1; CompNum <= state.dataZoneEquip->SupplyAirPath(PathNum).NumOfComponents; ++CompNum) {

            if ((AlphArray(Counter) == "AIRLOOPHVAC:ZONESPLITTER") || (AlphArray(Counter) == "AIRLOOPHVAC:SUPPLYPLENUM")) {

                state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType(CompNum) = AlphArray(Counter);
                state.dataZoneEquip->SupplyAirPath(PathNum).ComponentName(CompNum) = AlphArray(Counter + 1);
                ValidateComponent(state,
                                  state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType(CompNum),
                                  state.dataZoneEquip->SupplyAirPath(PathNum).ComponentName(CompNum),
                                  IsNotOK,
                                  CurrentModuleObject);
                state.dataZoneEquip->SupplyAirPath(PathNum).ComponentIndex(CompNum) = 0;
                state.dataZoneEquip->SupplyAirPath(PathNum).SplitterIndex(CompNum) = 0;
                state.dataZoneEquip->SupplyAirPath(PathNum).PlenumIndex(CompNum) = 0;
                if (AlphArray(Counter) == "AIRLOOPHVAC:ZONESPLITTER")
                    state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType_Num(CompNum) = ZoneSplitter_Type;
                if (AlphArray(Counter) == "AIRLOOPHVAC:SUPPLYPLENUM")
                    state.dataZoneEquip->SupplyAirPath(PathNum).ComponentType_Num(CompNum) = ZoneSupplyPlenum_Type;

            } else {
                ShowSevereError(state, std::string{RoutineName} + cAlphaFields(1) + "=\"" + state.dataZoneEquip->SupplyAirPath(PathNum).Name + "\"");
                ShowContinueError(state, "Unhandled component type =\"" + AlphArray(Counter) + "\".");
                ShowContinueError(state, R"(Must be "AirLoopHVAC:ZoneSplitter" or "AirLoopHVAC:SupplyPlenum")");
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            }

            Counter += 2;
        }

        state.dataZoneEquip->SupplyAirPath(PathNum).NumOutletNodes = 0;
        state.dataZoneEquip->SupplyAirPath(PathNum).NumNodes = 0;

    } // end loop over supply air paths

    CurrentModuleObject = "AirLoopHVAC:ReturnPath";
    for (PathNum = 1; PathNum <= state.dataZoneEquip->NumReturnAirPaths; ++PathNum) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 PathNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields); //  data for one zone
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, state.dataZoneEquip->GetZoneEquipmentDataErrorsFound);
        state.dataZoneEquip->ReturnAirPath(PathNum).Name = AlphArray(1);
        state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents = nint((double(NumAlphas) - 2.0) / 2.0);

        state.dataZoneEquip->ReturnAirPath(PathNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                                      AlphArray(2),
                                                                                      state.dataZoneEquip->GetZoneEquipmentDataErrorsFound,
                                                                                      CurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Outlet,
                                                                                      1,
                                                                                      ObjectIsParent);

        state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType.allocate(state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType_Num.allocate(state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType_Num = 0;
        state.dataZoneEquip->ReturnAirPath(PathNum).ComponentName.allocate(state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents);
        state.dataZoneEquip->ReturnAirPath(PathNum).ComponentIndex.allocate(state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents);

        Counter = 3;

        for (CompNum = 1; CompNum <= state.dataZoneEquip->ReturnAirPath(PathNum).NumOfComponents; ++CompNum) {

            if ((AlphArray(Counter) == "AIRLOOPHVAC:ZONEMIXER") || (AlphArray(Counter) == "AIRLOOPHVAC:RETURNPLENUM")) {

                state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType(CompNum) = AlphArray(Counter);
                state.dataZoneEquip->ReturnAirPath(PathNum).ComponentName(CompNum) = AlphArray(Counter + 1);
                state.dataZoneEquip->ReturnAirPath(PathNum).ComponentIndex(CompNum) = 0;
                ValidateComponent(state,
                                  state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType(CompNum),
                                  state.dataZoneEquip->ReturnAirPath(PathNum).ComponentName(CompNum),
                                  IsNotOK,
                                  CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "In " + CurrentModuleObject + " = " + state.dataZoneEquip->ReturnAirPath(PathNum).Name);
                    state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
                }
                if (AlphArray(Counter) == "AIRLOOPHVAC:ZONEMIXER")
                    state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType_Num(CompNum) = ZoneMixer_Type;
                if (AlphArray(Counter) == "AIRLOOPHVAC:RETURNPLENUM")
                    state.dataZoneEquip->ReturnAirPath(PathNum).ComponentType_Num(CompNum) = ZoneReturnPlenum_Type;
            } else {
                ShowSevereError(state, std::string{RoutineName} + cAlphaFields(1) + "=\"" + state.dataZoneEquip->ReturnAirPath(PathNum).Name + "\"");
                ShowContinueError(state, "Unhandled component type =\"" + AlphArray(Counter) + "\".");
                ShowContinueError(state, R"(Must be "AirLoopHVAC:ZoneMixer" or "AirLoopHVAC:ReturnPlenum")");
                state.dataZoneEquip->GetZoneEquipmentDataErrorsFound = true;
            }

            Counter += 2;
        }

    } // end loop over return air paths

    AlphArray.deallocate();
    NumArray.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    // setup zone equipment info for convection correlations
    SetupZoneEquipmentForConvectionFlowRegime(state);

    if (state.dataZoneEquip->GetZoneEquipmentDataErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting Zone Equipment input.");
    }
}

void SetupZoneEquipmentForConvectionFlowRegime(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Decide a few one-time things for later
    // determination of flow regime for convection

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;

    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
    }
}

bool CheckZoneEquipmentList(EnergyPlusData &state,
                            std::string_view const ComponentType, // Type of component
                            std::string_view const ComponentName, // Name of component
                            Optional_int CtrlZoneNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Provides a way to check if a component name is listed on a zone equipment list.

    // Return value
    bool IsOnList; // True if item is on a list, false if not.

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int ListLoop;
    int CtrlZoneNumLocal;

    CtrlZoneNumLocal = 0;
    IsOnList = false;
    for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {           // NumOfZoneEquipLists
        if (state.dataZoneEquip->ZoneEquipList(Loop).Name.empty()) continue; // dimensioned by NumOfZones.  Only valid ones have names.
        for (ListLoop = 1; ListLoop <= state.dataZoneEquip->ZoneEquipList(Loop).NumOfEquipTypes; ++ListLoop) {
            if (!UtilityRoutines::SameString(state.dataZoneEquip->ZoneEquipList(Loop).EquipType(ListLoop), ComponentType)) continue;
            if (ComponentName == "*") {
                IsOnList = true;
                CtrlZoneNumLocal = Loop;
                goto EquipList_exit;
            }
            if (!UtilityRoutines::SameString(state.dataZoneEquip->ZoneEquipList(Loop).EquipName(ListLoop), ComponentName)) continue;
            IsOnList = true;
            CtrlZoneNumLocal = Loop;
            goto EquipList_exit;
        }
    }
EquipList_exit:;
    if (present(CtrlZoneNum)) {
        CtrlZoneNum = CtrlZoneNumLocal;
    }
    return IsOnList;
}

int GetControlledZoneIndex(EnergyPlusData &state, std::string const &ZoneName) // Zone name to match into Controlled Zone structure
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the index into the Controlled Zone Equipment structure
    // of the indicated zone.

    // Return value
    int ControlledZoneIndex; // Index into Controlled Zone structure

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }

    ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);

    return ControlledZoneIndex;
}

int FindControlledZoneIndexFromSystemNodeNumberForZone(EnergyPlusData &state,
                                                       int const TrialZoneNodeNum) // Node number to match into Controlled Zone structure
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the zone number for the indicated
    // zone node num.  Returns 0 if did not find zone node in any Zone

    // Return value
    int ControlledZoneIndex; // Index into Controlled Zone structure

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool FoundIt;
    int ZoneNum;

    FoundIt = false;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }
    ControlledZoneIndex = 0;
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum > 0) {
            if (TrialZoneNodeNum == state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode) {
                // found it.
                FoundIt = true;
                ControlledZoneIndex = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum;
            }
        }
    }

    return ControlledZoneIndex;
}

int GetSystemNodeNumberForZone(EnergyPlusData &state, std::string const &ZoneName) // Zone name to match into Controlled Zone structure
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the system node number for the indicated
    // zone.  Returns 0 if the Zone is not a controlled zone.

    // Return value
    int SystemZoneNodeNumber; // System node number for controlled zone

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ControlledZoneIndex;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }

    ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
    SystemZoneNodeNumber = 0; // default is not found
    if (ControlledZoneIndex > 0) {
        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex).ActualZoneNum > 0) {
            SystemZoneNodeNumber = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex).ZoneNode;
        }
    }

    return SystemZoneNodeNumber;
}

int GetReturnAirNodeForZone(EnergyPlusData &state,
                            std::string const &ZoneName,             // Zone name to match into Controlled Zone structure
                            std::string const &NodeName,             // Return air node name to match (may be blank)
                            std::string const &calledFromDescription // String identifying the calling function and object
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2008
    //       MODIFIED       Feb 2017 expanded for multiple return nodes in a zone

    // PURPOSE OF THIS FUNCTION:
    // This function returns the return air node number for the indicated
    // zone and node name.  If NodeName is blank, return the first return node number,
    // otherwise return the node number of the matching return node name.
    // Returns 0 if the Zone is not a controlled zone or the node name does not match.

    // Return value
    int ReturnAirNodeNumber; // Return Air node number for controlled zone
    int ControlledZoneIndex;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }

    ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
    ReturnAirNodeNumber = 0; // default is not found
    if (ControlledZoneIndex > 0) {
        {
            auto const &thisZoneEquip(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex));
            if (thisZoneEquip.ActualZoneNum > 0) {
                if (NodeName.empty()) {
                    // If NodeName is blank, return first return node number, but warn if there are multiple return nodes for this zone
                    ReturnAirNodeNumber = thisZoneEquip.ReturnNode(1);
                    if (thisZoneEquip.NumReturnNodes > 1) {
                        ShowWarningError(state, "GetReturnAirNodeForZone: " + calledFromDescription + ", request for zone return node is ambiguous.");
                        ShowContinueError(state,
                                          format("Zone={} has {} return nodes. First return node will be used.",
                                                 thisZoneEquip.ZoneName,
                                                 thisZoneEquip.NumReturnNodes));
                    }
                } else {
                    for (int nodeCount = 1; nodeCount <= thisZoneEquip.NumReturnNodes; ++nodeCount) {
                        int curNodeNum = thisZoneEquip.ReturnNode(nodeCount);
                        if (NodeName == state.dataLoopNodes->NodeID(curNodeNum)) {
                            ReturnAirNodeNumber = curNodeNum;
                        }
                    }
                }
            }
        }
    }

    return ReturnAirNodeNumber;
}

int GetReturnNumForZone(EnergyPlusData &state,
                        std::string const &ZoneName, // Zone name to match into Controlled Zone structure
                        std::string const &NodeName  // Return air node name to match (may be blank)
)
{

    // PURPOSE OF THIS FUNCTION:
    // This function returns the zone return number (not the node number) for the indicated
    // zone and node name.  If NodeName is blank, return 1 (the first return node)
    // otherwise return the index of the matching return node name.
    // Returns 0 if the Zone is not a controlled zone or the node name does not match.

    // Return value
    int ReturnIndex; // Return number for the given zone (not the node number)

    int ControlledZoneIndex;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }

    ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
    ReturnIndex = 0; // default if not found
    if (ControlledZoneIndex > 0) {
        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex).ActualZoneNum > 0) {
            if (NodeName.empty()) {
                // If NodeName is blank, return first return node number
                ReturnIndex = 1;
            } else {
                for (int nodeCount = 1; nodeCount <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex).NumReturnNodes; ++nodeCount) {
                    int curNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneIndex).ReturnNode(nodeCount);
                    if (NodeName == state.dataLoopNodes->NodeID(curNodeNum)) {
                        ReturnIndex = nodeCount;
                    }
                }
            }
        }
    }

    return ReturnIndex;
}

Real64 CalcDesignSpecificationOutdoorAir(EnergyPlusData &state,
                                         int const DSOAPtr,          // Pointer to DesignSpecification:OutdoorAir object
                                         int const ActualZoneNum,    // Zone index
                                         bool const UseOccSchFlag,   // Zone occupancy schedule will be used instead of using total zone occupancy
                                         bool const UseMinOASchFlag, // Use min OA schedule in DesignSpecification:OutdoorAir object
                                         Optional_bool_const PerPersonNotSet, // when calculation should not include occupants (e.g., dual duct)
                                         Optional_bool_const MaxOAVolFlowFlag // TRUE when calculation uses occupancy schedule  (e.g., dual duct)
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   October 2012

    // PURPOSE OF THIS FUNCTION:
    // This function returns the air volume flow rate based on DesignSpecification:OutdoorAir object.

    // METHODOLOGY EMPLOYED:
    // User inputs and zone index allows calculation of outdoor air quantity.
    // Sizing does not use occupancy or min OA schedule and will call with flags set to FALSE
    // Ventilation Rate Procedure uses occupancy schedule based on user input.

    // Using/Aliasing
    using DataSizing::OAFlow;
    using DataSizing::OAFlowACH;
    using DataSizing::OAFlowMax;
    using DataSizing::OAFlowNone;
    using DataSizing::OAFlowPerArea;
    using DataSizing::OAFlowPPer;
    using DataSizing::OAFlowSum;
    using DataSizing::ZOAM_IAQP;
    using DataSizing::ZOAM_ProportionalControlDesOcc;
    using DataSizing::ZOAM_ProportionalControlSchOcc;

    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleMaxValue;

    auto &Zone(state.dataHeatBal->Zone);
    auto &OARequirements(state.dataSize->OARequirements);

    // Return value
    Real64 OAVolumeFlowRate; // Return value for calculated outdoor air volume flow rate [m3/s]

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 DSOAFlowPeople;  // Outdoor air volume flow rate based on occupancy (m3/s)
    Real64 DSOAFlowPerZone; // Outdoor air volume flow rate (m3/s)
    Real64 DSOAFlowPerArea; // Outdoor air volume flow rate based on zone floor area (m3/s)
    Real64 DSOAFlowACH;     // Outdoor air volume flow rate based on air changes per hour (m3/s)
    Real64 PeopleCount;     // total count of people in people objects
    int Loop;               // index counter in LOOP
    bool PerPersonModeNotSet;
    bool MaxOAFlag;
    Real64 ZoneOAPeople;              // Zone OA flow rate based on number of occupants [m3/s]
    Real64 ZoneOAArea;                // Zone OA flow rate based on space floor area [m3/s]
    Real64 ZoneOAMin;                 // Minimum Zone OA flow rate when the zone is unoccupied (i.e. ZoneOAPeople = 0)
                                      // used for "ProportionalControl" System outdoor air method
    Real64 ZoneOAMax;                 // Maximum Zone OA flow rate (ZoneOAPeople + ZoneOAArea)
                                      // used for "ProportionalControl" System outdoor air method
    Real64 ZoneMaxCO2;                // Breathing-zone CO2 concentration
    Real64 ZoneMinCO2;                // Minimum CO2 concentration in zone
    Real64 ZoneContamControllerSched; // Schedule value for ZoneControl:ContaminantController
    Real64 CO2PeopleGeneration;       // CO2 generation from people at design level
    int PeopleNum;

    OAVolumeFlowRate = 0.0;
    if (DSOAPtr == 0) return OAVolumeFlowRate;

    if (state.dataZoneEquip->CalcDesignSpecificationOutdoorAirOneTimeFlag) {
        state.dataZoneEquip->MyEnvrnFlag.allocate(state.dataSize->NumOARequirements);
        state.dataZoneEquip->MyEnvrnFlag = true;
        state.dataZoneEquip->CalcDesignSpecificationOutdoorAirOneTimeFlag = false;
    }

    if (present(PerPersonNotSet)) {
        PerPersonModeNotSet = PerPersonNotSet;
    } else {
        PerPersonModeNotSet = false;
    }

    if (present(MaxOAVolFlowFlag)) {
        MaxOAFlag = MaxOAVolFlowFlag;
    } else {
        MaxOAFlag = false;
    }

    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_IAQP && state.dataZoneEquip->MyEnvrnFlag(DSOAPtr)) {
        if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
            ShowSevereError(state,
                            "DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                R"(" valid Outdoor Air Method =" IndoorAirQualityProcedure" requires CO2 simulation.)");
            ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
            ShowFatalError(state, "CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
        }
        state.dataZoneEquip->MyEnvrnFlag(DSOAPtr) = false;
    }
    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc && state.dataZoneEquip->MyEnvrnFlag(DSOAPtr)) {
        if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
            ShowSevereError(state,
                            "DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                R"(" valid Outdoor Air Method =" ProportionalControlBasedOnDesignOccupancy" requires CO2 simulation.)");
            ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
            ShowFatalError(state, "CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
        }
        state.dataZoneEquip->MyEnvrnFlag(DSOAPtr) = false;
    }
    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc && state.dataZoneEquip->MyEnvrnFlag(DSOAPtr)) {
        if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
            ShowSevereError(state,
                            "DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                R"(" valid Outdoor Air Method =" ProportionalControlBasedOnOccupancySchedule" requires CO2 simulation.)");
            ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
            ShowFatalError(state, "CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
        }
        state.dataZoneEquip->MyEnvrnFlag(DSOAPtr) = false;
    }

    // Calculate people outdoor air flow rate as needed
    {
        auto const SELECT_CASE_var(OARequirements(DSOAPtr).OAFlowMethod);
        if ((SELECT_CASE_var == OAFlowPPer) || (SELECT_CASE_var == OAFlowSum) || (SELECT_CASE_var == OAFlowMax)) {
            if (UseOccSchFlag) {
                if (MaxOAFlag) {
                    // OAPerPersonMode == PerPersonDCVByCurrentLevel (UseOccSchFlag = TRUE)
                    // for dual duct, get max people according to max schedule value when requesting MaxOAFlow
                    PeopleCount = 0.0;
                    for (Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
                        if (ActualZoneNum != state.dataHeatBal->People(Loop).ZonePtr) continue;
                        PeopleCount += state.dataHeatBal->People(Loop).NumberOfPeople *
                                       GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).NumberOfPeoplePtr);
                    }
                    DSOAFlowPeople = PeopleCount * OARequirements(DSOAPtr).OAFlowPerPerson;
                } else {
                    DSOAFlowPeople = state.dataHeatBal->ZoneIntGain(ActualZoneNum).NOFOCC * OARequirements(DSOAPtr).OAFlowPerPerson;
                }
            } else {
                if (MaxOAFlag) {
                    // OAPerPersonMode == PerPersonByDesignLevel (UseOccSchFlag = FALSE)
                    // use total people when requesting MaxOAFlow
                    DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                } else {
                    DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                }
            }
            if (PerPersonModeNotSet) DSOAFlowPeople = 0.0; // for Dual Duct if Per Person Ventilation Rate Mode is not entered
        } else {
            DSOAFlowPeople = 0.0;
        }
    }

    // Calculate minimum outdoor air flow rate
    {
        auto const SELECT_CASE_var(OARequirements(DSOAPtr).OAFlowMethod);
        if (SELECT_CASE_var == OAFlowNone) {
            // Special case for no DesignSpecification:OutdoorAir object in Sizing:Zone object
            // probably won't get to this CASE statement since it will RETURN above (Ptr=0)
            // See SizingManager GetZoneSizingInput for Sizing:Zone input field Design Specification Outdoor Air Object Name
            OAVolumeFlowRate = 0.0;
        } else if (SELECT_CASE_var == OAFlowPPer) {
            // Multiplied by occupancy
            OAVolumeFlowRate = DSOAFlowPeople;
        } else if (SELECT_CASE_var == OAFlow) {
            // User input
            OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowPerZone;
        } else if (SELECT_CASE_var == OAFlowPerArea) {
            // Multiplied by zone floor area
            OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
        } else if (SELECT_CASE_var == OAFlowACH) {
            // Multiplied by zone volume
            OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
        } else if ((SELECT_CASE_var == OAFlowSum) || (SELECT_CASE_var == OAFlowMax)) {
            // Use sum or max of per person and the following
            DSOAFlowPerZone = OARequirements(DSOAPtr).OAFlowPerZone;
            DSOAFlowPerArea = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
            DSOAFlowACH = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
            if (OARequirements(DSOAPtr).OAFlowMethod == OAFlowMax) {
                OAVolumeFlowRate = max(DSOAFlowPeople, DSOAFlowPerZone, DSOAFlowPerArea, DSOAFlowACH);
            } else {
                OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH;
            }
        } else if (SELECT_CASE_var == ZOAM_IAQP) {
            if (state.dataGlobal->DoingSizing) {
                DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                DSOAFlowPerZone = OARequirements(DSOAPtr).OAFlowPerZone;
                DSOAFlowPerArea = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
                DSOAFlowACH = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
                OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH;
            } else {
                OAVolumeFlowRate = state.dataContaminantBalance->ZoneSysContDemand(ActualZoneNum).OutputRequiredToCO2SP / state.dataEnvrn->StdRhoAir;
            }

        } else if (SELECT_CASE_var == ZOAM_ProportionalControlSchOcc || SELECT_CASE_var == ZOAM_ProportionalControlDesOcc) {
            ZoneOAPeople = 0.0;
            if (OARequirements(DSOAPtr).OAFlowMethod != ZOAM_ProportionalControlDesOcc) {
                ZoneOAPeople = state.dataHeatBal->ZoneIntGain(ActualZoneNum).NOFOCC * Zone(ActualZoneNum).Multiplier *
                               Zone(ActualZoneNum).ListMultiplier * OARequirements(DSOAPtr).OAFlowPerPerson;
            } else {
                ZoneOAPeople = Zone(ActualZoneNum).TotOccupants * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier *
                               OARequirements(DSOAPtr).OAFlowPerPerson;
                CO2PeopleGeneration = 0.0;
                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                    // Accumulate CO2 generation from people at design occupancy and current activity level
                    for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                        if (state.dataHeatBal->People(PeopleNum).ZonePtr != ActualZoneNum) continue;
                        CO2PeopleGeneration += state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                                               state.dataHeatBal->People(PeopleNum).CO2RateFactor *
                                               GetCurrentScheduleValue(state, state.dataHeatBal->People(PeopleNum).ActivityLevelPtr);
                    }
                }
            }
            ZoneOAArea = Zone(ActualZoneNum).FloorArea * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier *
                         OARequirements(DSOAPtr).OAFlowPerArea;
            ZoneOAMin = ZoneOAArea;
            ZoneOAMax = (ZoneOAArea + ZoneOAPeople);
            if (Zone(ActualZoneNum).ZoneContamControllerSchedIndex > 0.0) {
                // Check the availability schedule value for ZoneControl:ContaminantController
                ZoneContamControllerSched = GetCurrentScheduleValue(state, Zone(ActualZoneNum).ZoneContamControllerSchedIndex);
                if (ZoneContamControllerSched > 0.0) {
                    if (ZoneOAPeople > 0.0) {
                        if (state.dataContaminantBalance->ZoneCO2GainFromPeople(ActualZoneNum) > 0.0) {
                            if (Zone(ActualZoneNum).ZoneMinCO2SchedIndex > 0.0) {
                                // Take the schedule value of "Minimum Carbon Dioxide Concentration Schedule Name"
                                // in the ZoneControl:ContaminantController
                                ZoneMinCO2 = GetCurrentScheduleValue(state, Zone(ActualZoneNum).ZoneMinCO2SchedIndex);
                            } else {
                                ZoneMinCO2 = state.dataContaminantBalance->OutdoorCO2;
                            }

                            // Calculate zone maximum target CO2 concentration in PPM
                            if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                ZoneMaxCO2 =
                                    state.dataContaminantBalance->OutdoorCO2 +
                                    (CO2PeopleGeneration * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier * 1.0e6) / ZoneOAMax;
                            } else {
                                ZoneMaxCO2 = state.dataContaminantBalance->OutdoorCO2 +
                                             (state.dataContaminantBalance->ZoneCO2GainFromPeople(ActualZoneNum) * Zone(ActualZoneNum).Multiplier *
                                              Zone(ActualZoneNum).ListMultiplier * 1.0e6) /
                                                 ZoneOAMax;
                            }

                            if (ZoneMaxCO2 <= ZoneMinCO2) {
                                ++OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount;
                                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc) {
                                    if (OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount < 2) {
                                        ShowSevereError(state,
                                                        "CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                        ShowContinueError(
                                            state,
                                            format("For System Outdoor Air Method = ProportionalControlBasedOnOccupancySchedule, maximum target "
                                                   "CO2 concentration ({:.2R}), is not greater than minimum target CO2 concentration ({:.2R}).",
                                                   ZoneMaxCO2,
                                                   ZoneMinCO2));
                                        ShowContinueError(state,
                                                          "\"ProportionalControlBasedOnOccupancySchedule\" will not be modeled. Default "
                                                          "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                        ShowContinueErrorTimeStamp(state, "");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                "\", For System Outdoor Air Method = ProportionalControlBasedOnOccupancySchedule, maximum target "
                                                "CO2 concentration is not greater than minimum target CO2 concentration. Error continues...",
                                            OARequirements(DSOAPtr).CO2MaxMinLimitErrorIndex);
                                    }
                                }
                                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                    if (OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount < 2) {
                                        ShowSevereError(state,
                                                        "CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                        ShowContinueError(
                                            state,
                                            format("For System Outdoor Air Method = ProportionalControlBasedOnDesignOccupancy, maximum target "
                                                   "CO2 concentration ({:.2R}), is not greater than minimum target CO2 concentration ({:.2R}).",
                                                   ZoneMaxCO2,
                                                   ZoneMinCO2));
                                        ShowContinueError(state,
                                                          "\"ProportionalControlBasedOnDesignOccupancy\" will not be modeled. Default "
                                                          "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                        ShowContinueErrorTimeStamp(state, "");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                "\", For System Outdoor Air Method = ProportionalControlBasedOnDesignOccupancy, maximum target "
                                                "CO2 concentration is not greater than minimum target CO2 concentration. Error continues...",
                                            OARequirements(DSOAPtr).CO2MaxMinLimitErrorIndex);
                                    }
                                }

                                OAVolumeFlowRate = ZoneOAMax;
                            } else {

                                if (state.dataContaminantBalance->ZoneAirCO2(ActualZoneNum) <= ZoneMinCO2) {
                                    // Zone air CO2 concentration is less than minimum zone CO2 concentration, set the Zone OA flow rate to
                                    // minimum Zone OA flow rate when the zone is unoccupied
                                    OAVolumeFlowRate = ZoneOAMin;
                                } else if (state.dataContaminantBalance->ZoneAirCO2(ActualZoneNum) >= ZoneMaxCO2) {
                                    // Zone air CO2 concentration is greater than maximum zone CO2 concentration, set the Zone OA flow rate to
                                    // maximum Zone OA flow rate (i.e. ZoneOAArea + ZoneOAPeople)
                                    OAVolumeFlowRate = ZoneOAMax;
                                } else {
                                    // Zone air CO2 concentration is between maximum and minimum limits of zone CO2 concentration,
                                    // set Zone OA flow rate by proportionally adjusting between ZoneOAMin and ZoneOAMax
                                    OAVolumeFlowRate = ZoneOAMin + (ZoneOAMax - ZoneOAMin) *
                                                                       ((state.dataContaminantBalance->ZoneAirCO2(ActualZoneNum) - ZoneMinCO2) /
                                                                        (ZoneMaxCO2 - ZoneMinCO2));
                                }
                            }
                        } else {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ++OARequirements(DSOAPtr).CO2GainErrorCount;
                                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc) {
                                    if (OARequirements(DSOAPtr).CO2GainErrorCount < 2) {
                                        ShowSevereError(state,
                                                        "CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                        ShowContinueError(state,
                                                          "For System Outdoor Air Method = ProportionalControlBasedOnOccupancySchedule, CO2 "
                                                          "generation from people is not greater than zero. Occurs in Zone =\"" +
                                                              Zone(ActualZoneNum).Name + "\". ");
                                        ShowContinueError(state,
                                                          "\"ProportionalControlBasedOnOccupancySchedule\" will not be modeled. Default "
                                                          "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                        ShowContinueErrorTimeStamp(state, "");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                                           "\", For System Outdoor Air Method = "
                                                                           "ProportionalControlBasedOnOccupancySchedule, CO2 generation from "
                                                                           "people is not greater than zero. Error continues...",
                                                                       OARequirements(DSOAPtr).CO2GainErrorIndex);
                                    }
                                }
                                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                    if (OARequirements(DSOAPtr).CO2GainErrorCount < 2) {
                                        ShowSevereError(state,
                                                        "CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                        ShowContinueError(state,
                                                          "For System Outdoor Air Method = ProportionalControlBasedOnDesignOccupancy, CO2 "
                                                          "generation from people is not greater than zero. Occurs in Zone =\"" +
                                                              Zone(ActualZoneNum).Name + "\". ");
                                        ShowContinueError(state,
                                                          "\"ProportionalControlBasedOnDesignOccupancy\" will not be modeled. Default "
                                                          "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                        ShowContinueErrorTimeStamp(state, "");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                                           "\", For System Outdoor Air Method = "
                                                                           "ProportionalControlBasedOnDesignOccupancy, CO2 generation from "
                                                                           "people is not greater than zero. Error continues...",
                                                                       OARequirements(DSOAPtr).CO2GainErrorIndex);
                                    }
                                }
                            }
                            OAVolumeFlowRate = ZoneOAMax;
                        }
                    } else {
                        // ZoneOAPeople is less than or equal to zero
                        OAVolumeFlowRate = ZoneOAMax;
                    }
                } else {
                    // ZoneControl:ContaminantController is scheduled off (not available)
                    OAVolumeFlowRate = ZoneOAMax;
                }
            } else {
                // "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController not found
                OAVolumeFlowRate = ZoneOAMax;
            }

        } else {
            // Will never get here
            OAVolumeFlowRate = 0.0;
        }
    }

    // Apply zone multipliers and zone list multipliers
    OAVolumeFlowRate *= Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier;

    // Apply schedule as needed. Sizing does not use schedule.
    if (OARequirements(DSOAPtr).OAFlowFracSchPtr > 0 && UseMinOASchFlag) {
        if (MaxOAFlag) {
            OAVolumeFlowRate *= GetScheduleMaxValue(state, OARequirements(DSOAPtr).OAFlowFracSchPtr);
        } else {
            OAVolumeFlowRate *= GetCurrentScheduleValue(state, OARequirements(DSOAPtr).OAFlowFracSchPtr);
        }
    }

    return OAVolumeFlowRate;
}

void EquipList::getPrioritiesForInletNode(EnergyPlusData &state,
                                          int const inletNodeNum, // Zone inlet node number to match
                                          int &coolingPriority,   // Cooling priority num for matching equipment
                                          int &heatingPriority    // Heating priority num for matching equipment
)
{
    bool equipFound = false;
    for (int equipNum = 1; equipNum <= this->NumOfEquipTypes; ++equipNum) {
        if (this->EquipType_Num(equipNum) == AirDistUnit_Num) {
            if (inletNodeNum == state.dataDefineEquipment->AirDistUnit(this->EquipIndex(equipNum)).OutletNodeNum) {
                equipFound = true;
            }
        }
        if (equipFound) {
            coolingPriority = this->CoolingPriority(equipNum);
            heatingPriority = this->HeatingPriority(equipNum);
            break;
        }
    }
    // Set MinAirLoopIterationsAfterFirst for equipment that uses sequenced loads, based on zone equip load distribution scheme
    int minIterations = state.dataHVACGlobal->MinAirLoopIterationsAfterFirst;
    if (this->LoadDistScheme == DataZoneEquipment::LoadDist::SequentialLoading) {
        // Sequential needs one extra iterations up to the highest airterminal unit equipment number
        minIterations = max(coolingPriority, heatingPriority, minIterations);
    } else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::UniformLoading) {
        // Uniform needs one extra iteration which is the default
    } else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::UniformPLRLoading) {
        // UniformPLR needs two extra iterations, regardless of unit equipment number
        minIterations = max(2, minIterations);
    } else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::SequentialUniformPLRLoading) {
        // SequentialUniformPLR needs one extra iterations up to the highest airterminal unit equipment number plus one more
        minIterations = max((coolingPriority + 1), (heatingPriority + 1), minIterations);
    }
    state.dataHVACGlobal->MinAirLoopIterationsAfterFirst = minIterations;
}

Real64 EquipList::SequentialHeatingFraction(EnergyPlusData &state, const int equipNum)
{
    return ScheduleManager::GetCurrentScheduleValue(state, SequentialHeatingFractionSchedPtr(equipNum));
}

Real64 EquipList::SequentialCoolingFraction(EnergyPlusData &state, const int equipNum)
{
    return ScheduleManager::GetCurrentScheduleValue(state, SequentialCoolingFractionSchedPtr(equipNum));
}

} // namespace EnergyPlus::DataZoneEquipment
