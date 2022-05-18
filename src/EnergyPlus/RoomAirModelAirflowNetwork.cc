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
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ElectricBaseboardRadiator.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HWBaseboardRadiator.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceHAMTManager.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/SteamBaseboardRadiator.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus {

namespace RoomAirModelAirflowNetwork {

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   November 2009
    //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // contains the RoomAir model portions of RoomAirflowNetwork modeling

    // METHODOLOGY EMPLOYED:
    // Interact with Surface HB, internal gain, HVAC system and Airflow Network Domains
    // Do heat and moisture balance calculations on roomair nodes.

    // Using/Aliasing
    using namespace DataRoomAirModel;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace DataHeatBalance;

    // Object Data

    // Functions

    void SimRoomAirModelAirflowNetwork(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   January 2004/Aug 2005
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages RoomAirflowNetwork model simulation

        // METHODOLOGY EMPLOYED:
        // calls subroutines

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ThisRoomAirNode;
        int RAFNNum;

        RAFNNum = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).RAFNNum;

        if (RAFNNum == 0) {
            ShowFatalError(state,
                           "SimRoomAirModelAirflowNetwork: Zone is not defined in the RoomAirModelAirflowNetwork model =" +
                               state.dataHeatBal->Zone(ZoneNum).Name);
        }

        auto &thisRAFN(state.dataRoomAirflowNetModel->RAFN(RAFNNum));
        thisRAFN.ZoneNum = ZoneNum;

        // model control volume for each roomAir:node in the zone.
        for (ThisRoomAirNode = 1; ThisRoomAirNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++ThisRoomAirNode) {

            thisRAFN.RoomAirNode = ThisRoomAirNode;

            thisRAFN.InitRoomAirModelAirflowNetwork(state, ThisRoomAirNode);

            thisRAFN.CalcRoomAirModelAirflowNetwork(state, ThisRoomAirNode);
        }

        thisRAFN.UpdateRoomAirModelAirflowNetwork(state);

    } // SimRoomAirModelAirflowNetwork

    //****************************************************

    void LoadPredictionRoomAirModelAirflowNetwork(EnergyPlusData &state,
                                                  int const ZoneNum,
                                                  int const RoomAirNode) // index number for the specified zone and node
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June, 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Predict zone loads at a controlled node

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool OneTimeFlag_FindFirstLastPtr( true );  // one time setup flag //
        // state.dataRoomAirflowNetModel->LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag
        ////////////////////////////////////////////////////////////////////////////////////
        int RAFNNum;

        if (state.dataRoomAirflowNetModel->LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag) {
            state.dataRoomAirflowNetModel->RAFN.allocate(state.dataRoomAirMod->NumOfRoomAirflowNetControl);
            state.dataRoomAirflowNetModel->LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag = false;
        }

        RAFNNum = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).RAFNNum;

        if (RAFNNum == 0) {
            ShowFatalError(state,
                           "LoadPredictionRoomAirModelAirflowNetwork: Zone is not defined in the RoomAirModelAirflowNetwork model =" +
                               state.dataHeatBal->Zone(ZoneNum).Name);
        }
        auto &thisRAFN(state.dataRoomAirflowNetModel->RAFN(RAFNNum));
        thisRAFN.ZoneNum = ZoneNum;

        thisRAFN.InitRoomAirModelAirflowNetwork(state, RoomAirNode);

    } // LoadPredictionRoomAirModelAirflowNetwork

    //****************************************************

    void RAFNData::InitRoomAirModelAirflowNetwork(EnergyPlusData &state, int const RoomAirNode) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2009
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Perform one-time checking and term calculations

        using InternalHeatGains::SumInternalLatentGainsByTypes;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool MyOneTimeFlag( true );  // one time setup flag // InitRoomAirModelAirflowNetworkOneTimeFlag
        // static bool MyOneTimeFlagConf( true ); // one time setup flag for zone configuration // InitRoomAirModelAirflowNetworkOneTimeFlagConf
        // static bool MyEnvrnFlag( true ); // one time setup flag for zone configuration // InitRoomAirModelAirflowNetworkEnvrnFlag
        ////////////////////////////////////////////////////////////////////////////////////
        Real64 SumLinkMCp;
        Real64 SumLinkMCpT;
        int linkNum;
        Real64 LinkInTemp;
        Real64 CpAir;
        Real64 LinkInHumRat;
        Real64 LinkInMdot;
        Real64 SumLinkM;
        Real64 SumLinkMW;
        int LoopZone;
        int NumSurfs;
        int LoopAirNode;
        int NodeNum;
        int NodeIn;
        int Link;
        int IdZone;
        int IdNode;
        int EquipLoop;
        int MaxNodeNum;
        Array1D_bool NodeFound; // True if a node is found.
        int MaxEquipNum;
        Array1D_bool EquipFound;
        int ISum;
        bool ErrorsFound;
        int I;
        Array1D<Real64> SupplyFrac;
        Array1D<Real64> ReturnFrac;

        if (state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkOneTimeFlag) { // then do one - time setup inits

            // loop over all zones with RoomAirflowNetwork model
            for (LoopZone = 1; LoopZone <= state.dataGlobal->NumOfZones; ++LoopZone) {
                if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).IsUsed) continue;
                NumSurfs = state.dataHeatBal->Zone(LoopZone).HTSurfaceLast - state.dataHeatBal->Zone(LoopZone).HTSurfaceFirst + 1;
                for (LoopAirNode = 1; LoopAirNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes;
                     ++LoopAirNode) { // loop over all the modeled room air nodes
                    // calculate volume of air in node's control volume
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirVolume =
                        state.dataHeatBal->Zone(LoopZone).Volume *
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).ZoneVolumeFraction;

                    SetupOutputVariable(state,
                                        "RoomAirflowNetwork Node NonAirSystemResponse",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).NonAirSystemResponse,
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
                    SetupOutputVariable(state,
                                        "RoomAirflowNetwork Node SysDepZoneLoadsLagged",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SysDepZoneLoadsLagged,
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
                    SetupOutputVariable(state,
                                        "RoomAirflowNetwork Node SumIntSensibleGain",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumIntSensibleGain,
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
                    SetupOutputVariable(state,
                                        "RoomAirflowNetwork Node SumIntLatentGain",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumIntLatentGain,
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
                }
            }
            state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkOneTimeFlag = false;
        }

        if (state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkOneTimeFlagConf) { // then do one - time setup inits
            if (allocated(state.dataZoneEquip->ZoneEquipConfig) && allocated(state.dataZoneEquip->ZoneEquipList)) {
                MaxNodeNum = 0;
                MaxEquipNum = 0;
                ErrorsFound = false;
                for (LoopZone = 1; LoopZone <= state.dataGlobal->NumOfZones; ++LoopZone) {
                    if (!state.dataHeatBal->Zone(LoopZone).IsControlled) continue;
                    MaxEquipNum = max(MaxEquipNum, state.dataZoneEquip->ZoneEquipList(LoopZone).NumOfEquipTypes);
                    MaxNodeNum = max(MaxNodeNum, state.dataZoneEquip->ZoneEquipConfig(LoopZone).NumInletNodes);
                }
                if (MaxNodeNum > 0) {
                    NodeFound.allocate(MaxNodeNum);
                    NodeFound = false;
                }
                if (MaxEquipNum > 0) {
                    EquipFound.allocate(MaxEquipNum);
                    SupplyFrac.allocate(MaxEquipNum);
                    ReturnFrac.allocate(MaxEquipNum);
                    EquipFound = false;
                    SupplyFrac = 0.0;
                    ReturnFrac = 0.0;
                }

                // loop over all zones with RoomAirflowNetwork model
                for (LoopZone = 1; LoopZone <= state.dataGlobal->NumOfZones; ++LoopZone) {
                    if (!state.dataHeatBal->Zone(LoopZone).IsControlled) continue;
                    if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).IsUsed) continue;
                    // find actualZoneID in ZoneEquipConfig
                    for (IdZone = 1; IdZone <= state.dataGlobal->NumOfZones; ++IdZone) {
                        if (state.dataZoneEquip->ZoneEquipConfig(IdZone).ActualZoneNum == LoopZone) {
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).ActualZoneID = IdZone;
                            break;
                        }
                    }
                    SupplyFrac = 0.0;
                    ReturnFrac = 0.0;
                    NodeFound = false;

                    // find supply air node number
                    for (LoopAirNode = 1; LoopAirNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes;
                         ++LoopAirNode) { // loop over all the modeled room air nodes
                        for (EquipLoop = 1; EquipLoop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).NumHVACs;
                             ++EquipLoop) { // loop over all the equip for a single room air node
                            // Check zone equipment name
                            for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { // loop over all equip types
                                if (UtilityRoutines::SameString(
                                        state.dataZoneEquip->ZoneEquipList(LoopZone).EquipName(I),
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name)) {
                                    if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone)
                                            .Node(LoopAirNode)
                                            .HVAC(EquipLoop)
                                            .EquipConfigIndex == 0)
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone)
                                            .Node(LoopAirNode)
                                            .HVAC(EquipLoop)
                                            .EquipConfigIndex = I;
                                    EquipFound(I) = true;
                                    SupplyFrac(I) =
                                        SupplyFrac(I) +
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupplyFraction;
                                    ReturnFrac(I) =
                                        ReturnFrac(I) +
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction;
                                }
                            }
                            for (IdNode = 1; IdNode <= state.dataLoopNodes->NumOfNodes; ++IdNode) { // loop over all nodes to find supply node ID
                                if (UtilityRoutines::SameString(state.dataLoopNodes->NodeID(IdNode),
                                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone)
                                                                    .Node(LoopAirNode)
                                                                    .HVAC(EquipLoop)
                                                                    .SupplyNodeName)) {
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupNodeNum = IdNode;
                                    break;
                                }
                            }
                            // Verify inlet nodes
                            int inletNodeIndex = 0;
                            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(LoopZone).NumInletNodes;
                                 ++NodeNum) { // loop over all supply inlet nodes in a single zone
                                // !Get node conditions
                                if (state.dataZoneEquip->ZoneEquipConfig(LoopZone).InletNode(NodeNum) == IdNode) {
                                    NodeFound(NodeNum) = true;
                                    inletNodeIndex = NodeNum;
                                    break;
                                }
                            }

                            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupNodeNum > 0 &&
                                UtilityRoutines::SameString(
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnNodeName,
                                    "")) {
                                // Find matching return node
                                for (int retNode = 1; retNode <= state.dataZoneEquip->ZoneEquipConfig(LoopZone).NumReturnNodes; ++retNode) {
                                    if ((state.dataZoneEquip->ZoneEquipConfig(LoopZone).ReturnNodeInletNum(retNode) == inletNodeIndex) &&
                                        (state.dataZoneEquip->ZoneEquipConfig(LoopZone).ReturnNode(retNode) > 0)) {
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).RetNodeNum =
                                            state.dataZoneEquip->ZoneEquipConfig(LoopZone).ReturnNode(retNode); // Zone return node
                                        break;
                                    }
                                }
                            }

                            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).RetNodeNum == 0) {
                                for (IdNode = 1; IdNode <= state.dataLoopNodes->NumOfNodes; ++IdNode) { // loop over all nodes to find return node ID
                                    if (UtilityRoutines::SameString(state.dataLoopNodes->NodeID(IdNode),
                                                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone)
                                                                        .Node(LoopAirNode)
                                                                        .HVAC(EquipLoop)
                                                                        .ReturnNodeName)) {
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).RetNodeNum =
                                            IdNode;
                                        break;
                                    }
                                }
                            }
                            SetupOutputVariable(
                                state,
                                "RoomAirflowNetwork Node HVAC Supply Fraction",
                                OutputProcessor::Unit::None,
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupplyFraction,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
                            SetupOutputVariable(
                                state,
                                "RoomAirflowNetwork Node HVAC Return Fraction",
                                OutputProcessor::Unit::None,
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
                        }
                    }
                    // Count node with.TRUE.
                    ISum = 0;
                    for (NodeNum = 1; NodeNum <= MaxNodeNum; ++NodeNum) { // loop over all supply inlet nodes in a single zone
                        if (NodeFound(NodeNum)) ISum = ISum + 1;
                    }
                    // Provide error messages with incorrect supplu node inputs
                    if (ISum != state.dataZoneEquip->ZoneEquipConfig(LoopZone).NumInletNodes) {
                        if (ISum > state.dataZoneEquip->ZoneEquipConfig(LoopZone).NumInletNodes) {
                            ShowSevereError(
                                state, "GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects");
                            ShowContinueError(
                                state, "is greater than the number of zone configuration inlet nodes in " + state.dataHeatBal->Zone(LoopZone).Name);
                            ShowContinueError(state, "Please check inputs of both objects.");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(
                                state, "GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects");
                            ShowContinueError(
                                state, "is less than the number of zone configuration inlet nodes in " + state.dataHeatBal->Zone(LoopZone).Name);
                            ShowContinueError(state, "Please check inputs of both objects.");
                            ErrorsFound = true;
                        }
                    }

                    // Check equipment names to ensure they are used in RoomAirflowNetwork : Node : HVACEquipment objects
                    for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { // loop over all equip types
                        if (!EquipFound(I)) {
                            ShowSevereError(state,
                                            "GetRoomAirflowNetworkData: The equipment listed in ZoneEquipList is not found in the lsit of "
                                            "RoomAir:Node:AirflowNetwork:HVACEquipment objects =");
                            ShowContinueError(state,
                                              state.dataZoneEquip->ZoneEquipList(LoopZone).EquipName(I) + ". Please check inputs of both objects.");
                            ErrorsFound = true;
                        }
                    }

                    // Check fraction to ensure sum = 1.0 for every equipment
                    for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { // loop over all equip types
                        if (std::abs(SupplyFrac(I) - 1.0) > 0.001) {
                            ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, zone supply fractions do not sum to 1.0");
                            ShowContinueError(state,
                                              "Entered in " + state.dataZoneEquip->ZoneEquipList(LoopZone).EquipName(I) +
                                                  " defined in RoomAir:Node:AirflowNetwork:HVACEquipment");
                            ShowContinueError(state,
                                              "The Fraction of supply fraction values across all the roomair nodes in a zone needs to sum to 1.0.");
                            ShowContinueError(state, format("The sum of fractions entered = {:.3R}", SupplyFrac(I)));
                            ErrorsFound = true;
                        }
                        if (std::abs(ReturnFrac(I) - 1.0) > 0.001) {
                            ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, zone return fractions do not sum to 1.0");
                            ShowContinueError(state,
                                              "Entered in " + state.dataZoneEquip->ZoneEquipList(LoopZone).EquipName(I) +
                                                  " defined in RoomAir:Node:AirflowNetwork:HVACEquipment");
                            ShowContinueError(state,
                                              "The Fraction of return fraction values across all the roomair nodes in a zone needs to sum to 1.0.");
                            ShowContinueError(state, format("The sum of fractions entered = {:.3R}", ReturnFrac(I)));
                            ErrorsFound = true;
                        }
                    }
                }
                state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkOneTimeFlagConf = false;
                if (allocated(NodeFound)) NodeFound.deallocate();
                if (ErrorsFound) {
                    ShowFatalError(state, "GetRoomAirflowNetworkData: Errors found getting air model input.  Program terminates.");
                }
            }
        } // End of InitRoomAirModelAirflowNetworkOneTimeFlagConf

        if (state.dataGlobal->BeginEnvrnFlag && state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkEnvrnFlag) {
            for (LoopZone = 1; LoopZone <= state.dataGlobal->NumOfZones; ++LoopZone) {
                if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).IsUsed) continue;
                for (LoopAirNode = 1; LoopAirNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes;
                     ++LoopAirNode) { // loop over all the modeled room air nodes
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTemp = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX1 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX2 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX3 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX4 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX1 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX2 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX3 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX4 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempT1 = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempTMX = 23.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempTM2 = 23.0;

                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRat = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX1 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX2 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX3 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX4 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX1 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX2 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX3 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX4 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatW1 = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatWMX = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatWM2 = 0.0;

                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SysDepZoneLoadsLagged = 0.0;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SysDepZoneLoadsLaggedOld = 0.0;
                }
            }
            state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataRoomAirflowNetModel->InitRoomAirModelAirflowNetworkEnvrnFlag = true;
        }

        // reuse code in ZoneTempPredictorCorrector for sensible components.
        CalcNodeSums(state, RoomAirNode);

        SumNonAirSystemResponseForNode(state, RoomAirNode);

        // latent gains.
        auto &ThisRAFNNode(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode));

        if (allocated(ThisRAFNNode.SurfMask)) {
            CalcSurfaceMoistureSums(state, RoomAirNode, ThisRAFNNode.SumHmAW, ThisRAFNNode.SumHmARa, ThisRAFNNode.SumHmARaW, ThisRAFNNode.SurfMask);
        }

        // prepare AirflowNetwor flow rates and temperatures
        SumLinkMCp = 0.0;
        SumLinkMCpT = 0.0;
        SumLinkM = 0.0;
        SumLinkMW = 0.0;

        NodeNum = ThisRAFNNode.AirflowNetworkNodeID;
        if (NodeNum > 0) {
            for (linkNum = 1; linkNum <= ThisRAFNNode.NumOfAirflowLinks; ++linkNum) {
                Link = ThisRAFNNode.Link(linkNum).AirflowNetworkLinkSimuID;
                if (state.afn->AirflowNetworkLinkageData(Link).NodeNums[0] == NodeNum) { // incoming flow
                    NodeIn = state.afn->AirflowNetworkLinkageData(Link).NodeNums[1];
                    ThisRAFNNode.Link(linkNum).TempIn = state.afn->AirflowNetworkNodeSimu(NodeIn).TZ;
                    ThisRAFNNode.Link(linkNum).HumRatIn = state.afn->AirflowNetworkNodeSimu(NodeIn).WZ;
                    ThisRAFNNode.Link(linkNum).MdotIn = state.afn->AirflowNetworkLinkSimu(Link).FLOW2;
                }
                if (state.afn->AirflowNetworkLinkageData(Link).NodeNums[1] == NodeNum) { // outgoing flow
                    NodeIn = state.afn->AirflowNetworkLinkageData(Link).NodeNums[0];
                    ThisRAFNNode.Link(linkNum).TempIn = state.afn->AirflowNetworkNodeSimu(NodeIn).TZ;
                    ThisRAFNNode.Link(linkNum).HumRatIn = state.afn->AirflowNetworkNodeSimu(NodeIn).WZ;
                    ThisRAFNNode.Link(linkNum).MdotIn = state.afn->AirflowNetworkLinkSimu(Link).FLOW;
                }
            }

            for (linkNum = 1; linkNum <= ThisRAFNNode.NumOfAirflowLinks; ++linkNum) {
                LinkInTemp = ThisRAFNNode.Link(linkNum).TempIn;
                LinkInHumRat = ThisRAFNNode.Link(linkNum).HumRatIn;
                LinkInMdot = ThisRAFNNode.Link(linkNum).MdotIn;
                CpAir = PsyCpAirFnW(LinkInHumRat);
                SumLinkMCp = SumLinkMCp + CpAir * LinkInMdot;
                SumLinkMCpT = SumLinkMCpT + CpAir * LinkInMdot * LinkInTemp;
                SumLinkM = SumLinkM + LinkInMdot;
                SumLinkMW = SumLinkMW + LinkInMdot * LinkInHumRat;
            }
        }

        ThisRAFNNode.SumLinkMCp = SumLinkMCp;
        ThisRAFNNode.SumLinkMCpT = SumLinkMCpT;
        ThisRAFNNode.SumLinkM = SumLinkM;
        ThisRAFNNode.SumLinkMW = SumLinkMW;
        ThisRAFNNode.SysDepZoneLoadsLagged = ThisRAFNNode.SysDepZoneLoadsLaggedOld;

        ThisRAFNNode.RhoAir =
            PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, ThisRAFNNode.AirTemp, ThisRAFNNode.HumRat, "InitRoomAirModelAirflowNetwork");

        ThisRAFNNode.CpAir = PsyCpAirFnW(ThisRAFNNode.HumRat);

    } // InitRoomAirModelAirflowNetwork

    //*****************************************************************************************

    void RAFNData::CalcRoomAirModelAirflowNetwork(EnergyPlusData &state, int const RoomAirNode) // index number for the specified zone and node
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   November 2009
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // calculate new values for temperature and humidity ratio for room air node

        // METHODOLOGY EMPLOYED:
        // take terms(updated in init routine) and use classic air balance equations
        // solved for state variables. Store results in structure.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using Psychrometrics::PsyHgAirFnWTdb;
        using Psychrometrics::PsyRhFnTdbWPb;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 H2OHtOfVap;
        Real64 HumRatTmp;
        Real64 NodeTempX1;
        Real64 NodeTempX2;
        Real64 NodeTempX3;
        Real64 NodeHumRatX1;
        Real64 NodeHumRatX2;
        Real64 NodeHumRatX3;
        Real64 TempDepCoef;
        Real64 TempIndCoef;
        Real64 AirCap;
        Real64 TempTmp;
        Real64 A;
        Real64 B;
        Real64 C;
        Real64 AirTempT1;
        Real64 HumRatW1;

        auto &ThisRAFNNode(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode));

        if (state.dataHVACGlobal->UseZoneTimeStepHistory) {
            NodeTempX1 = ThisRAFNNode.AirTempX1;
            NodeTempX2 = ThisRAFNNode.AirTempX2;
            NodeTempX3 = ThisRAFNNode.AirTempX3;

            NodeHumRatX1 = ThisRAFNNode.HumRatX1;
            NodeHumRatX2 = ThisRAFNNode.HumRatX2;
            NodeHumRatX3 = ThisRAFNNode.HumRatX3;
        } else { // use down - stepped history
            NodeTempX1 = ThisRAFNNode.AirTempDSX1;
            NodeTempX2 = ThisRAFNNode.AirTempDSX2;
            NodeTempX3 = ThisRAFNNode.AirTempDSX3;

            NodeHumRatX1 = ThisRAFNNode.HumRatDSX1;
            NodeHumRatX2 = ThisRAFNNode.HumRatDSX2;
            NodeHumRatX3 = ThisRAFNNode.HumRatDSX3;
        }

        if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
            AirTempT1 = ThisRAFNNode.AirTempT1;
            HumRatW1 = ThisRAFNNode.HumRatW1;
        }
        // solve for node drybulb temperature
        TempDepCoef = ThisRAFNNode.SumHA + ThisRAFNNode.SumLinkMCp + ThisRAFNNode.SumSysMCp;
        TempIndCoef = ThisRAFNNode.SumIntSensibleGain + ThisRAFNNode.SumHATsurf - ThisRAFNNode.SumHATref + ThisRAFNNode.SumLinkMCpT +
                      ThisRAFNNode.SumSysMCpT + ThisRAFNNode.NonAirSystemResponse + ThisRAFNNode.SysDepZoneLoadsLagged;
        AirCap = ThisRAFNNode.AirVolume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens * ThisRAFNNode.RhoAir * ThisRAFNNode.CpAir /
                 (TimeStepSys * DataGlobalConstants::SecInHour);

        if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (TempDepCoef == 0.0) { // B=0
                TempTmp = AirTempT1 + TempIndCoef / AirCap;
            } else {
                TempTmp = (AirTempT1 - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) + TempIndCoef / TempDepCoef;
            }
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            TempTmp = (AirCap * AirTempT1 + TempIndCoef) / (AirCap + TempDepCoef);
        } else {
            TempTmp = (TempIndCoef + AirCap * (3.0 * NodeTempX1 - (3.0 / 2.0) * NodeTempX2 + (1.0 / 3.0) * NodeTempX3)) /
                      ((11.0 / 6.0) * AirCap + TempDepCoef);
        }

        ThisRAFNNode.AirTemp = TempTmp;

        // solve for node humidity ratio using 3 algorithms
        H2OHtOfVap = PsyHgAirFnWTdb(ThisRAFNNode.HumRat, ThisRAFNNode.AirTemp);
        A = ThisRAFNNode.SumLinkM + ThisRAFNNode.SumHmARa + ThisRAFNNode.SumSysM;
        B = (ThisRAFNNode.SumIntLatentGain / H2OHtOfVap) + ThisRAFNNode.SumSysMW + ThisRAFNNode.SumLinkMW + ThisRAFNNode.SumHmARaW;
        C = ThisRAFNNode.RhoAir * ThisRAFNNode.AirVolume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist /
            (DataGlobalConstants::SecInHour * TimeStepSys);

        // Exact solution
        if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (A == 0.0) { // B=0
                HumRatTmp = HumRatW1 + B / C;
            } else {
                HumRatTmp = (HumRatW1 - B / A) * std::exp(min(700., -A / C)) + B / A;
            }
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            HumRatTmp = (C * HumRatW1 + B) / (C + A);
        } else {
            HumRatTmp = (B + C * (3.0 * NodeHumRatX1 - (3.0 / 2.0) * NodeHumRatX2 + (1.0 / 3.0) * NodeHumRatX3)) / ((11.0 / 6.0) * C + A);
        }

        ThisRAFNNode.HumRat = HumRatTmp;

        ThisRAFNNode.AirCap = AirCap;
        ThisRAFNNode.AirHumRat = C;

        ThisRAFNNode.RelHumidity = PsyRhFnTdbWPb(state, TempTmp, HumRatTmp, state.dataEnvrn->OutBaroPress, "CalcRoomAirModelAirflowNetwork") * 100.0;

    } // CalcRoomAirModelAirflowNetwork

    void RAFNData::UpdateRoomAirModelAirflowNetwork(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   November 2009
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update variables

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirNodeNum; // nested node structure index
        int I;
        int LoopAirNode;
        int EquipLoop;
        Real64 NodeMass;
        Real64 SumMass;
        Real64 SumMassT;
        Real64 SumMassW;
        int RetNodeNum;

        auto &ThisRAFNZone(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum));

        if (!ThisRAFNZone.IsUsed) return;

        if (!state.dataGlobal->ZoneSizingCalc) SumSystemDepResponseForNode(state);

        AirNodeNum = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;

        // Update return node conditions
        for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) { // loop over all equip types
            SumMass = 0.0;
            SumMassT = 0.0;
            SumMassW = 0.0;
            for (LoopAirNode = 1; LoopAirNode <= ThisRAFNZone.NumOfAirNodes; ++LoopAirNode) { // loop over all the modeled room air nodes
                for (EquipLoop = 1; EquipLoop <= ThisRAFNZone.Node(LoopAirNode).NumHVACs;
                     ++EquipLoop) { // loop over all the equip for a single room air node
                    if (ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).EquipConfigIndex == I) {
                        if (ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).SupNodeNum > 0 &&
                            ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).RetNodeNum > 0) {
                            NodeMass = state.dataLoopNodes->Node(ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).SupNodeNum).MassFlowRate *
                                       ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction;
                            SumMass += NodeMass;
                            SumMassT += NodeMass * ThisRAFNZone.Node(LoopAirNode).AirTemp;
                            SumMassW += NodeMass * ThisRAFNZone.Node(LoopAirNode).HumRat;
                            RetNodeNum = ThisRAFNZone.Node(LoopAirNode).HVAC(EquipLoop).RetNodeNum;
                        }
                    }
                }
            }
            if (SumMass > 0.0) {
                state.dataLoopNodes->Node(RetNodeNum).Temp = SumMassT / SumMass;
                state.dataLoopNodes->Node(RetNodeNum).HumRat = SumMassW / SumMass;
            }
        }
    } // UpdateRoomAirModelAirflowNetwork

    void RAFNData::CalcNodeSums(EnergyPlusData &state, int const RoomAirNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2009
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE - ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE :
        // This subroutine calculates the various sums that go into the zone heat balance
        // equation.This replaces the SUMC, SUMHA, and SUMHAT calculations that were
        // previously done in various places throughout the program.
        // The SumHAT portion of the code is reproduced in RadiantSystemHighTemp and
        // RadiantSystemLowTemp and should be updated accordingly.
        //
        // A reference temperature(Tref) is specified for use with the ceiling diffuser
        // convection correlation.A bogus value of Tref = -999.9 defaults to using
        // the zone air(i.e.outlet) temperature for the reference temperature.
        // If Tref is applied to all surfaces, SumHA = 0, and SumHATref /= 0.
        // If Tref is not used at all, SumHATref = 0, and SumHA /= 0.
        //

        // USE STATEMENTS:
        using InternalHeatGains::SumInternalConvectionGainsByIndices;
        using InternalHeatGains::SumInternalLatentGainsByIndices;
        using InternalHeatGains::SumReturnAirConvectionGainsByIndices;
        using InternalHeatGains::SumReturnAirConvectionGainsByTypes;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NodeNum;         // System node number
        Real64 NodeTemp;     // System node temperature
        Real64 NodeW;        // System node humidity ratio
        Real64 MassFlowRate; // System node mass flow rate
        int ZoneEquipConfigNum;
        bool ControlledZoneAirFlag;
        int ZoneRetPlenumNum;
        int ZoneSupPlenumNum;
        bool ZoneRetPlenumAirFlag;
        bool ZoneSupPlenumAirFlag;
        Real64 CpAir;      // Specific heat of air
        int SurfNum;       // Surface number
        Real64 HA;         //                     !Hc*Area
        Real64 Area;       //                   !Effective surface area
        Real64 RefAirTemp; //             !Reference air temperature for surface convection calculations
        Real64 ZoneMult;
        int ADUListIndex;
        int ADUNum;
        int ADUInNode;
        int ADUOutNode;
        Real64 SumIntGain; //             !node sum of convective internal gains
        Real64 SumHA;      // Zone sum of Hc*Area
        Real64 SumHATsurf; //             !Zone sum of Hc*Area*Tsurf
        Real64 SumHATref;  //              !Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
        Real64 SumMCp;     //                !Zone sum of MassFlowRate*Cp
        Real64 SumMCpT;    //                !Zone sum of MassFlowRate*Cp*T
        Real64 SumSysMCp;  //              !Zone sum of air system MassFlowRate*Cp
        Real64 SumSysMCpT; //             !Zone sum of air system MassFlowRate*Cp*T
        Real64 SumSysM;    //                !Zone sum of air system MassFlowRate
        Real64 SumSysMW;   //               !Zone sum of air system MassFlowRate*W
        int EquipLoop;     //              !Index of equipment loop
        int Loop;          //                   !Index of RAFN node
        bool Found;        //
        Real64 SumLinkM;   //               !Zone sum of MassFlowRate from the AirflowNetwork model
        Real64 SumLinkMW;  //             !Zone sum of MassFlowRate*W from the AirflowNetwork model

        SumIntGain = 0.0;
        SumHA = 0.0;
        SumHATsurf = 0.0;
        SumHATref = 0.0;
        SumMCp = 0.0;
        SumMCpT = 0.0;
        SumSysMCp = 0.0;
        SumSysMCpT = 0.0;
        SumSysM = 0.0;
        SumSysMW = 0.0;
        SumLinkM = 0.0;
        SumLinkMW = 0.0;

        // Sum all convective internal gains: SumIntGain
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumIntSensibleGain =
            SumInternalConvectionGainsByIndices(state,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).NumIntGains,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).intGainsDeviceSpaces,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsDeviceIndices,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsFractions);

        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumIntLatentGain =
            SumInternalLatentGainsByIndices(state,
                                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).NumIntGains,
                                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).intGainsDeviceSpaces,
                                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsDeviceIndices,
                                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsFractions);
        // Add heat to return air if zonal system(no return air) or cycling system(return air frequently very low or zero)
        if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
            // *******************************************
            SumIntGain = SumReturnAirConvectionGainsByIndices(
                state,
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).NumIntGains,
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).intGainsDeviceSpaces,
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsDeviceIndices,
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsFractions);
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumIntSensibleGain += SumIntGain;
        }

        // Check to see if this is a controlled zone

        ControlledZoneAirFlag = false;
        for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigNum) {
            if (!state.dataHeatBal->Zone(ZoneEquipConfigNum).IsControlled) continue;
            if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != ZoneNum) continue;
            ControlledZoneAirFlag = true;
            break; // sloppy way of finding ZoneEquipConfigNum for later use.
        }          // ZoneEquipConfigNum

        // Check to see if this is a plenum zone
        ZoneRetPlenumAirFlag = false;
        for (ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= state.dataZonePlenum->NumZoneReturnPlenums; ++ZoneRetPlenumNum) {
            if (state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ActualZoneNum != ZoneNum) continue;
            ZoneRetPlenumAirFlag = true;
            break;
        } // ZoneRetPlenumNum
        ZoneSupPlenumAirFlag = false;
        for (ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= state.dataZonePlenum->NumZoneSupplyPlenums; ++ZoneSupPlenumNum) {
            if (state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).ActualZoneNum != ZoneNum) continue;
            ZoneSupPlenumAirFlag = true;
            break;
        } // ZoneSupPlenumNum

        // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
        if (ControlledZoneAirFlag) {
            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                // Get node conditions
                // this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call ?
                // how can we tell ? predict step must be lagged ? correct step, systems have run.
                for (EquipLoop = 1; EquipLoop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).NumHVACs;
                     ++EquipLoop) {
                    if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HVAC(EquipLoop).SupNodeNum ==
                        state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)) {
                        NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                        NodeW = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).HumRat;
                        MassFlowRate =
                            state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HVAC(EquipLoop).SupplyFraction;
                        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                        SumSysMCp += MassFlowRate * CpAir;
                        SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                        SumSysM += MassFlowRate;
                        SumSysMW += MassFlowRate * NodeW;
                    }
                } // EquipLoop
            }     // NodeNum
        } else if (ZoneRetPlenumAirFlag) {
            for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {
                // Get node conditions
                NodeTemp = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).Temp;
                MassFlowRate = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate;
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                SumSysMCp += MassFlowRate * CpAir;
                SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            } // NodeNum
            // add in the leaks
            for (ADUListIndex = 1; ADUListIndex <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum;
                    NodeTemp = state.dataLoopNodes->Node(ADUInNode).Temp;
                    MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                    CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                    SumSysMCp += MassFlowRate * CpAir;
                    SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                }
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                    NodeTemp = state.dataLoopNodes->Node(ADUOutNode).Temp;
                    MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                    CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                    SumSysMCp += MassFlowRate * CpAir;
                    SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                }
            } // ADUListIndex
        } else if (ZoneSupPlenumAirFlag) {
            // Get node conditions
            NodeTemp = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
            MassFlowRate = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            SumSysMCp += MassFlowRate * CpAir;
            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
        }

        ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;

        SumSysMCp = SumSysMCp / ZoneMult;
        SumSysMCpT = SumSysMCpT / ZoneMult;
        SumSysM = SumSysM / ZoneMult;
        SumSysMW = SumSysMW / ZoneMult;

        // Sum all surface convection : SumHA, SumHATsurf, SumHATref(and additional contributions to SumIntGain)
        // Modified by Gu to include assigned surfaces only shown in the surface lsit
        if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HasSurfacesAssigned) return;

        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID == RoomAirNodeNum) {
                Found = false;
                for (Loop = 1; Loop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++Loop) {
                    if (Loop != RoomAirNodeNum) {
                        if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).SurfMask(
                                SurfNum - state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst + 1)) {
                            Found = true;
                            break;
                        }
                    }
                }
                if (Found) continue;
            } else {
                if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                         .Node(RoomAirNodeNum)
                         .SurfMask(SurfNum - state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst + 1))
                    continue;
            }

            HA = 0.0;
            Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area

            if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) {

                // Add to the convective internal gains
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // The shade area covers the area of the glazing plus the area of the dividers.
                    Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                    SumIntGain += state.dataSurface->SurfWinDividerHeatGain(SurfNum);
                }

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)))
                    SumIntGain += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

                // Convective heat gain from airflow window
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                    SumIntGain += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
                        SumIntGain += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                        state.dataSurface->SurfWinHeatGain(SurfNum) += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                        if (state.dataSurface->SurfWinHeatGain(SurfNum) >= 0.0) {
                            state.dataSurface->SurfWinHeatGainRep(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);
                            state.dataSurface->SurfWinHeatGainRepEnergy(SurfNum) =
                                state.dataSurface->SurfWinHeatGainRep(SurfNum) * state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;
                        } else {
                            state.dataSurface->SurfWinHeatLossRep(SurfNum) = -state.dataSurface->SurfWinHeatGain(SurfNum);
                            state.dataSurface->SurfWinHeatLossRepEnergy(SurfNum) =
                                state.dataSurface->SurfWinHeatLossRep(SurfNum) * state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;
                        }
                        state.dataSurface->SurfWinHeatTransferRepEnergy(SurfNum) =
                            state.dataSurface->SurfWinHeatGain(SurfNum) * state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;
                    }
                }

                // Add to the surface convection sums
                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    SumHATsurf += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                  (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) * state.dataSurface->SurfWinFrameTempIn(SurfNum);
                    HA += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                          (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum));
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 &&
                    !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // Window divider contribution(only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                  (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) * state.dataSurface->SurfWinDividerTempIn(SurfNum);
                    HA += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                          (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum));
                }

            } // End of check if window

            HA = HA + state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area;
            SumHATsurf += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area * state.dataHeatBalSurf->SurfTempInTmp(SurfNum);

            if (state.dataSurface->SurfTAirRef(SurfNum) == DataSurfaces::RefAirTemp::ZoneMeanAirTemp) {
                // The zone air is the reference temperature(which is to be solved for in CorrectZoneAirTemp).
                RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                SumHA += HA;
            } else if (state.dataSurface->SurfTAirRef(SurfNum) == DataSurfaces::RefAirTemp::AdjacentAirTemp) {
                RefAirTemp = state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
                SumHATref += HA * RefAirTemp;
            } else if (state.dataSurface->SurfTAirRef(SurfNum) == DataSurfaces::RefAirTemp::ZoneSupplyAirTemp) {
                // check whether this zone is a controlled zone or not
                if (!ControlledZoneAirFlag) {
                    ShowFatalError(state,
                                   "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " +
                                       state.dataHeatBal->Zone(ZoneNum).Name);
                    return;
                }
                // determine supply air temperature as a weighted average of the inlet temperatures.
                RefAirTemp = SumSysMCpT / SumSysMCp;
                SumHATref += HA * RefAirTemp;
            } else {
                RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                SumHA = SumHA + HA;
            }

        } // SurfNum

        // Assemble values
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHA = SumHA;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHATsurf = SumHATsurf;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHATref = SumHATref;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMCp = SumSysMCp;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMCpT = SumSysMCpT;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysM = SumSysM;
        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMW = SumSysMW;

    } // CalcNodeSums

    void RAFNData::CalcSurfaceMoistureSums(EnergyPlusData &state,
                                           int const RoomAirNode,
                                           Real64 &SumHmAW,
                                           Real64 &SumHmARa,
                                           Real64 &SumHmARaW,
                                           [[maybe_unused]] Array1D<bool> const &SurfMask)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //                      derived from P. Biddulph-- HAMT, L. Gu -- EPMD,
        //       DATE WRITTEN   November 2009
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Breakout summation of surface moisture interaction terms

        // Using/Aliasing

        using HeatBalanceHAMTManager::UpdateHeatBalHAMT;
        using MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD;
        using Psychrometrics::PsyRhFnTdbRhov;
        using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdbRhPb;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;
        int Loop;
        Real64 RhoAirZone;
        Real64 Wsurf;
        bool Found;

        SumHmAW = 0.0;
        SumHmARa = 0.0;
        SumHmARaW = 0.0;

        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
            if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) continue;

            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID == RoomAirNode) {
                Found = false;
                for (Loop = 1; Loop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++Loop) {
                    // None - assigned surfaces belong to the zone node
                    if (Loop != RoomAirNode) {
                        if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).SurfMask(
                                SurfNum - state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst + 1)) {
                            Found = true;
                            break;
                        }
                    }
                }
                if (Found) continue;
            } else {
                if (!state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                         .Node(RoomAirNode)
                         .SurfMask(SurfNum - state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst + 1))
                    continue;
            }

            auto &HMassConvInFD = state.dataMstBal->HMassConvInFD;
            auto &RhoVaporSurfIn = state.dataMstBal->RhoVaporSurfIn;
            auto &RhoVaporAirIn = state.dataMstBal->RhoVaporAirIn;
            if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::HAMT) {
                UpdateHeatBalHAMT(state, SurfNum);

                SumHmAW += HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area * (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));

                RhoAirZone = PsyRhoAirFnPbTdbW(
                    state,
                    state.dataEnvrn->OutBaroPress,
                    state.dataHeatBalFanSys->MAT(state.dataSurface->Surface(SurfNum).Zone),
                    PsyRhFnTdbRhov(
                        state, state.dataHeatBalFanSys->MAT(state.dataSurface->Surface(SurfNum).Zone), RhoVaporAirIn(SurfNum), "RhoAirZone"));

                Wsurf = PsyWFnTdbRhPb(state,
                                      state.dataHeatBalSurf->SurfTempInTmp(SurfNum),
                                      PsyRhFnTdbRhov(state, state.dataHeatBalSurf->SurfTempInTmp(SurfNum), RhoVaporSurfIn(SurfNum), "Wsurf"),
                                      state.dataEnvrn->OutBaroPress);

                SumHmARa = SumHmARa + HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area * RhoAirZone;

                SumHmARaW = SumHmARaW + HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area * RhoAirZone * Wsurf;
            }

            if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::EMPD) {

                UpdateMoistureBalanceEMPD(state, SurfNum);
                RhoVaporSurfIn(SurfNum) = state.dataMstBalEMPD->RVSurface(SurfNum);

                SumHmAW =
                    SumHmAW + HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area * (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));
                SumHmARa = SumHmARa +
                           HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area *
                               PsyRhoAirFnPbTdbW(
                                   state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataHeatBalSurf->SurfTempInTmp(SurfNum),
                                   PsyWFnTdbRhPb(state,
                                                 state.dataHeatBalSurf->SurfTempInTmp(SurfNum),
                                                 PsyRhFnTdbRhovLBnd0C(state, state.dataHeatBalSurf->SurfTempInTmp(SurfNum), RhoVaporAirIn(SurfNum)),
                                                 state.dataEnvrn->OutBaroPress));
                SumHmARaW = SumHmARaW + HMassConvInFD(SurfNum) * state.dataSurface->Surface(SurfNum).Area * RhoVaporSurfIn(SurfNum);
            }
        }

    } // CalcSurfaceMoistureSums

    void RAFNData::SumNonAirSystemResponseForNode(EnergyPlusData &state, int const RAFNNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Sum system response from none air systems

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        using BaseboardElectric::SimElectricBaseboard;
        using BaseboardRadiator::SimBaseboard;
        using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveElectric;
        using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveWater;
        using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric;
        using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam;
        using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveWater;
        using DataHVACGlobals::ZoneEquipTypeOf_HighTemperatureRadiant;
        using DataHVACGlobals::ZoneEquipTypeOf_RefrigerationChillerSet;
        using ElectricBaseboardRadiator::SimElecBaseboard;
        using HighTempRadiantSystem::SimHighTempRadiantSystem;
        using HWBaseboardRadiator::SimHWBaseboard;
        using RefrigeratedCase::SimAirChillerSet;
        using SteamBaseboardRadiator::SimSteamBaseboard;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int I;
        Real64 SysOutputProvided;
        Real64 LatOutputProvided;

        // TODO
        auto &ThisRAFNNode(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum));

        ThisRAFNNode.NonAirSystemResponse = 0.0;

        if (!allocated(state.dataZoneEquip->ZoneEquipConfig)) return;

        for (I = 1; I <= ThisRAFNNode.NumHVACs; ++I) {

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveWater) {
                //'ZoneHVAC:Baseboard:RadiantConvective:Water' 13
                SimHWBaseboard(state,
                               ThisRAFNNode.HVAC(I).Name,
                               ZoneNum,
                               state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID,
                               false,
                               SysOutputProvided,
                               ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam) {
                // CASE(BBSteam_Num) !'ZoneHVAC:Baseboard:RadiantConvective:Steam' 14
                SimSteamBaseboard(state,
                                  ThisRAFNNode.HVAC(I).Name,
                                  ZoneNum,
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID,
                                  false,
                                  SysOutputProvided,
                                  ThisRAFNNode.HVAC(I).CompIndex);

                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveWater) {
                // CASE(BBWaterConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Water' 16
                SimBaseboard(state,
                             ThisRAFNNode.HVAC(I).Name,
                             ZoneNum,
                             state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID,
                             false,
                             SysOutputProvided,
                             ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveElectric) {
                // CASE(BBElectricConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Electric' 15
                SimElectricBaseboard(state,
                                     ThisRAFNNode.HVAC(I).Name,
                                     ZoneNum,
                                     state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID,
                                     SysOutputProvided,
                                     ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_RefrigerationChillerSet) {
                // CASE(RefrigerationAirChillerSet_Num)  !'ZoneHVAC:RefrigerationChillerSet' 20
                SimAirChillerSet(
                    state, ThisRAFNNode.HVAC(I).Name, ZoneNum, false, SysOutputProvided, LatOutputProvided, ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric) {
                // CASE(BBElectric_Num)  !'ZoneHVAC:Baseboard:RadiantConvective:Electric' 12
                SimElecBaseboard(state,
                                 ThisRAFNNode.HVAC(I).Name,
                                 ZoneNum,
                                 state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID,
                                 false,
                                 SysOutputProvided,
                                 ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            if (ThisRAFNNode.HVAC(I).TypeOfNum == ZoneEquipTypeOf_HighTemperatureRadiant) {
                // CASE(BBElectric_Num)  !'ZoneHVAC:HighTemperatureRadiant' 17
                SimHighTempRadiantSystem(state, ThisRAFNNode.HVAC(I).Name, false, SysOutputProvided, ThisRAFNNode.HVAC(I).CompIndex);
                ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC(I).SupplyFraction * SysOutputProvided;
                // LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
            }

            // Zone sum of system convective gains, collected via NonAirSystemResponse
        }

    } // SumNonAirSystemResponseForNode

    //*****************************************************************************************

    void RAFNData::SumSystemDepResponseForNode(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith
        //       DATE WRITTEN   aug 2005, Jan2004
        //       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Sum system sensible loads used at the next time step

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        using DataHVACGlobals::ZoneEquipTypeOf_DehumidifierDX;
        using ZoneDehumidifier::SimZoneDehumidifier;

        // Return value
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int I;
        Real64 SysOutputProvided;
        Real64 LatOutputProvided;
        int RoomAirNode;

        // TODO

        auto &ThisRAFNZone(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum));

        // SysDepZoneLoads saved to be added to zone heat balance next
        SysOutputProvided = 0.0;
        for (RoomAirNode = 1; RoomAirNode <= ThisRAFNZone.NumOfAirNodes; ++RoomAirNode) {
            ThisRAFNZone.Node(RoomAirNode).SysDepZoneLoadsLaggedOld = 0.0;
            for (I = 1; I <= ThisRAFNZone.Node(RoomAirNode).NumHVACs; ++I) {
                if (ThisRAFNZone.Node(RoomAirNode).HVAC(I).TypeOfNum == ZoneEquipTypeOf_DehumidifierDX) {
                    if (SysOutputProvided == 0.0)
                        SimZoneDehumidifier(state,
                                            ThisRAFNZone.Node(RoomAirNode).HVAC(I).Name,
                                            ZoneNum,
                                            false,
                                            SysOutputProvided,
                                            LatOutputProvided,
                                            ThisRAFNZone.Node(RoomAirNode).HVAC(I).CompIndex);
                    if (SysOutputProvided > 0.0) break;
                }
            }
        }

        if (SysOutputProvided > 0.0) {
            for (RoomAirNode = 1; RoomAirNode <= ThisRAFNZone.NumOfAirNodes; ++RoomAirNode) {
                for (I = 1; I <= ThisRAFNZone.Node(RoomAirNode).NumHVACs; ++I) {
                    if (ThisRAFNZone.Node(RoomAirNode).HVAC(I).TypeOfNum == ZoneEquipTypeOf_DehumidifierDX) {
                        ThisRAFNZone.Node(RoomAirNode).SysDepZoneLoadsLaggedOld +=
                            ThisRAFNZone.Node(RoomAirNode).HVAC(I).SupplyFraction * SysOutputProvided;
                    }
                }
            }
        }

    } // SumSystemDepResponseForNode

    //*****************************************************************************************

} // namespace RoomAirModelAirflowNetwork

} // namespace EnergyPlus
