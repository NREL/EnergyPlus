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
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MundtSimMgr.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace MundtSimMgr {

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2002
    //       RE-ENGINEERED  June 2003, EnergyPlus Implementation (CC)
    //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

    // PURPOSE OF THIS MODULE:
    // This module is the main module for running the
    // nodal air Mundt model...

    // METHODOLOGY EMPLOYED:
    // This module contains all subroutines required by the mundt model.
    // The following modules from AirToolkit included in this module are:
    // 1) MundtSimMgr Module,
    // 2) MundtInputMgr Module, and
    // 3) DataMundt Module,

    // REFERENCES:
    // AirToolkit source code

    // OTHER NOTES:
    // na

    // Data
    // MODULE PARAMETER DEFINITIONS:
    Real64 const CpAir(1005.0);   // Specific heat of air
    Real64 const MinSlope(0.001); // Bound on result from Mundt model
    Real64 const MaxSlope(5.0);   // Bound on result from Mundt Model

    // MODULE VARIABLE DECLARATIONS:

    void ManageMundtModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chanvit Chantrasrisalai
        //       DATE WRITTEN   July 2003
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   manage the Mundt model

        auto &MundtFirstTimeFlag = state.dataHeatBal->MundtFirstTimeFlag;
        bool ErrorsFound;

        // initialize Mundt model data
        if (MundtFirstTimeFlag) {
            InitMundtModel(state);
            MundtFirstTimeFlag = false;
        }

        // identify the current zone index for zones using Mundt model
        state.dataMundtSimMgr->MundtZoneNum = state.dataMundtSimMgr->ZoneData(ZoneNum).MundtZoneIndex;

        // transfer data from surface domain to air domain for the specified zone
        GetSurfHBDataForMundtModel(state, ZoneNum);

        // use the Mundt model only for cooling case
        if ((state.dataMundtSimMgr->SupplyAirVolumeRate > 0.0001) && (state.dataMundtSimMgr->QsysCoolTot > 0.0001)) {

            // setup Mundt model
            ErrorsFound = false;
            SetupMundtModel(state, ZoneNum, ErrorsFound);
            if (ErrorsFound) ShowFatalError(state, "ManageMundtModel: Errors in setting up Mundt Model. Preceding condition(s) cause termination.");

            // perform Mundt model calculations
            CalcMundtModel(state, ZoneNum);
        }

        // transfer data from air domain back to surface domain for the specified zone
        SetSurfHBDataForMundtModel(state, ZoneNum);
    }

    //*****************************************************************************************

    void InitMundtModel(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chanvit Chantrasrisalai
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     initialize Mundt-model variables

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;            // index for surfaces
        int SurfFirst;          // index number for the first surface in the specified zone
        int NumOfSurfs;         // number of the first surface in the specified zone
        int NodeNum;            // index for air nodes
        int ZoneIndex;          // index for zones
        int NumOfAirNodes;      // total number of nodes in each zone
        int NumOfMundtZones;    // number of zones using the Mundt model
        int MundtZoneIndex;     // index for zones using the Mundt model
        int MaxNumOfSurfs;      // maximum of number of surfaces
        int MaxNumOfFloorSurfs; // maximum of number of surfaces
        int MaxNumOfAirNodes;   // maximum of number of air nodes
        int MaxNumOfRoomNodes;  // maximum of number of nodes connected to walls
        int RoomNodesCount;     // number of nodes connected to walls
        int FloorSurfCount;     // number of nodes connected to walls
        int AirNodeBeginNum;    // index number of the first air node for this zone
        int AirNodeNum;         // index for air nodes
        bool AirNodeFoundFlag;  // flag used for error check
        bool ErrorsFound;       // true if errors found in init

        auto &Zone(state.dataHeatBal->Zone);

        // allocate and initialize zone data
        state.dataMundtSimMgr->ZoneData.allocate(state.dataGlobal->NumOfZones);
        for (auto &e : state.dataMundtSimMgr->ZoneData) {
            e.SurfFirst = 0;
            e.NumOfSurfs = 0;
            e.MundtZoneIndex = 0;
        }

        // get zone data
        NumOfMundtZones = 0;
        MaxNumOfSurfs = 0;
        MaxNumOfFloorSurfs = 0;
        MaxNumOfAirNodes = 0;
        MaxNumOfRoomNodes = 0;
        ErrorsFound = false;
        for (ZoneIndex = 1; ZoneIndex <= state.dataGlobal->NumOfZones; ++ZoneIndex) {
            if (state.dataRoomAirMod->AirModel(ZoneIndex).AirModelType == DataRoomAirModel::RoomAirModel::Mundt) {
                // find number of zones using the Mundt model
                ++NumOfMundtZones;
                // find maximum number of surfaces in zones using the Mundt model
                SurfFirst = Zone(ZoneIndex).HTSurfaceFirst;
                NumOfSurfs = Zone(ZoneIndex).HTSurfaceLast - SurfFirst + 1;
                MaxNumOfSurfs = max(MaxNumOfSurfs, NumOfSurfs);
                // fine maximum number of air nodes in zones using the Mundt model
                NumOfAirNodes = state.dataRoomAirMod->TotNumOfZoneAirNodes(ZoneIndex);
                MaxNumOfAirNodes = max(MaxNumOfAirNodes, NumOfAirNodes);
                // assign zone data
                state.dataMundtSimMgr->ZoneData(ZoneIndex).SurfFirst = SurfFirst;
                state.dataMundtSimMgr->ZoneData(ZoneIndex).NumOfSurfs = NumOfSurfs;
                state.dataMundtSimMgr->ZoneData(ZoneIndex).MundtZoneIndex = NumOfMundtZones;
            }
        }

        // allocate and initialize surface and air-node data
        state.dataMundtSimMgr->ID1dSurf.allocate(MaxNumOfSurfs);
        state.dataMundtSimMgr->TheseSurfIDs.allocate(MaxNumOfSurfs);
        state.dataMundtSimMgr->MundtAirSurf.allocate(MaxNumOfSurfs, NumOfMundtZones);
        state.dataMundtSimMgr->LineNode.allocate(MaxNumOfAirNodes, NumOfMundtZones);
        for (SurfNum = 1; SurfNum <= MaxNumOfSurfs; ++SurfNum)
            state.dataMundtSimMgr->ID1dSurf(SurfNum) = SurfNum;
        for (auto &e : state.dataMundtSimMgr->MundtAirSurf) {
            e.Area = 0.0;
            e.Temp = 25.0;
            e.Hc = 0.0;
            e.TMeanAir = 25.0;
        }
        for (auto &e : state.dataMundtSimMgr->LineNode) {
            e.AirNodeName.clear();
            e.ClassType = DataRoomAirModel::AirNodeType::Unassigned;
            e.Height = 0.0;
            e.Temp = 25.0;
        }

        // get constant data (unchanged over time) for surfaces and air nodes
        for (MundtZoneIndex = 1; MundtZoneIndex <= NumOfMundtZones; ++MundtZoneIndex) {

            for (ZoneIndex = 1; ZoneIndex <= state.dataGlobal->NumOfZones; ++ZoneIndex) {

                if (state.dataMundtSimMgr->ZoneData(ZoneIndex).MundtZoneIndex == MundtZoneIndex) {
                    // get surface data
                    for (SurfNum = 1; SurfNum <= state.dataMundtSimMgr->ZoneData(ZoneIndex).NumOfSurfs; ++SurfNum) {
                        state.dataMundtSimMgr->MundtAirSurf(SurfNum, MundtZoneIndex).Area =
                            state.dataSurface->Surface(state.dataMundtSimMgr->ZoneData(ZoneIndex).SurfFirst + SurfNum - 1).Area;
                    }

                    // get air node data
                    RoomNodesCount = 0;
                    FloorSurfCount = 0;
                    for (NodeNum = 1; NodeNum <= state.dataRoomAirMod->TotNumOfZoneAirNodes(ZoneIndex); ++NodeNum) {

                        state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex)
                            .SurfMask.allocate(state.dataMundtSimMgr->ZoneData(ZoneIndex).NumOfSurfs);

                        if (NodeNum == 1) {
                            AirNodeBeginNum = NodeNum;
                        }

                        // error check for debugging
                        if (AirNodeBeginNum > state.dataRoomAirMod->TotNumOfAirNodes) {
                            ShowFatalError(state, "An array bound exceeded. Error in InitMundtModel subroutine of MundtSimMgr.");
                        }

                        AirNodeFoundFlag = false;
                        for (AirNodeNum = AirNodeBeginNum; AirNodeNum <= state.dataRoomAirMod->TotNumOfAirNodes; ++AirNodeNum) {
                            if (UtilityRoutines::SameString(state.dataRoomAirMod->AirNode(AirNodeNum).ZoneName, Zone(ZoneIndex).Name)) {
                                state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).ClassType =
                                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType;
                                state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).AirNodeName = state.dataRoomAirMod->AirNode(AirNodeNum).Name;
                                state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).Height = state.dataRoomAirMod->AirNode(AirNodeNum).Height;
                                state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).SurfMask =
                                    state.dataRoomAirMod->AirNode(AirNodeNum).SurfMask;
                                SetupOutputVariable(state,
                                                    "Room Air Node Air Temperature",
                                                    OutputProcessor::Unit::C,
                                                    state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).Temp,
                                                    "HVAC",
                                                    "Average",
                                                    state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).AirNodeName);

                                AirNodeBeginNum = AirNodeNum + 1;
                                AirNodeFoundFlag = true;

                                break;
                            }
                        }

                        // error check for debugging
                        if (!AirNodeFoundFlag) {
                            ShowSevereError(state, "InitMundtModel: Air Node in Zone=\"" + Zone(ZoneIndex).Name + "\" is not found.");
                            ErrorsFound = true;
                            continue;
                        }

                        // count air nodes connected to walls in each zone
                        if (state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).ClassType == DataRoomAirModel::AirNodeType::MundtRoomAirNode) {
                            ++RoomNodesCount;
                        }

                        // count floors in each zone
                        if (state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).ClassType == DataRoomAirModel::AirNodeType::FloorAirNode) {
                            FloorSurfCount += count(state.dataMundtSimMgr->LineNode(NodeNum, MundtZoneIndex).SurfMask);
                        }
                    }
                    // got data for this zone so exit the zone loop
                    if (AirNodeFoundFlag) break;
                }
            }

            MaxNumOfRoomNodes = max(MaxNumOfRoomNodes, RoomNodesCount);
            MaxNumOfFloorSurfs = max(MaxNumOfFloorSurfs, FloorSurfCount);
        }

        if (ErrorsFound) ShowFatalError(state, "InitMundtModel: Preceding condition(s) cause termination.");

        // allocate arrays
        state.dataMundtSimMgr->RoomNodeIDs.allocate(MaxNumOfRoomNodes);
        state.dataMundtSimMgr->FloorSurfSetIDs.allocate(MaxNumOfFloorSurfs);
        state.dataMundtSimMgr->FloorSurf.allocate(MaxNumOfFloorSurfs);
    }

    //*****************************************************************************************

    void GetSurfHBDataForMundtModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Weixiu Kong
        //       DATE WRITTEN   April 2003
        //       MODIFIED       July 2003 (CC)
        //                      February 2004, fix allocate-deallocate problem (CC)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     map data from surface domain to air domain for each particular zone

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using InternalHeatGains::SumAllInternalConvectionGains;
        using InternalHeatGains::SumAllReturnAirConvectionGains;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdpPb;

        // Locals
        Real64 CpAir; // specific heat

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;             // index for surfaces
        int NodeNum;             // index for air nodes
        Real64 SumSysMCp;        // zone sum of air system MassFlowRate*Cp
        Real64 SumSysMCpT;       // zone sum of air system MassFlowRate*Cp*T
        Real64 MassFlowRate;     // mass flowrate
        Real64 NodeTemp;         // node temperature
        int ZoneNode;            // index number for specified zone node
        Real64 ZoneMassFlowRate; // zone mass flowrate
        int ZoneEquipConfigNum;  // index number for zone equipment configuration
        Real64 ZoneMult;         // total zone multiplier
        Real64 RetAirConvGain;

        auto &Zone(state.dataHeatBal->Zone);

        // determine ZoneEquipConfigNum for this zone
        ZoneEquipConfigNum = ZoneNum;
        // check whether this zone is a controlled zone or not
        if (!Zone(ZoneNum).IsControlled) {
            ShowFatalError(state, "Zones must be controlled for Mundt air model. No system serves zone " + Zone(ZoneNum).Name);
            return;
        }

        // determine information required by Mundt model
        state.dataMundtSimMgr->ZoneHeight = Zone(ZoneNum).CeilingHeight;
        state.dataMundtSimMgr->ZoneFloorArea = Zone(ZoneNum).FloorArea;
        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        // supply air flowrate is the same as zone air flowrate
        ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
        state.dataMundtSimMgr->ZoneAirDensity =
            PsyRhoAirFnPbTdbW(state,
                              state.dataEnvrn->OutBaroPress,
                              state.dataHeatBalFanSys->MAT(ZoneNum),
                              PsyWFnTdpPb(state, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataEnvrn->OutBaroPress));
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneNode).MassFlowRate;
        state.dataMundtSimMgr->SupplyAirVolumeRate = ZoneMassFlowRate / state.dataMundtSimMgr->ZoneAirDensity;
        if (ZoneMassFlowRate <= 0.0001) {
            // system is off
            state.dataMundtSimMgr->QsysCoolTot = 0.0;
        } else {
            // determine supply air conditions
            SumSysMCp = 0.0;
            SumSysMCpT = 0.0;
            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                MassFlowRate = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                SumSysMCp += MassFlowRate * CpAir;
                SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            }
            // prevent dividing by zero due to zero supply air flow rate
            if (SumSysMCp <= 0.0) {
                state.dataMundtSimMgr->SupplyAirTemp =
                    state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(1)).Temp;
            } else {
                // a weighted average of the inlet temperatures
                state.dataMundtSimMgr->SupplyAirTemp = SumSysMCpT / SumSysMCp;
            }
            // determine cooling load
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            state.dataMundtSimMgr->QsysCoolTot = -(SumSysMCpT - ZoneMassFlowRate * CpAir * state.dataHeatBalFanSys->MAT(ZoneNum));
        }
        // determine heat gains
        SumAllInternalConvectionGains(state, ZoneNum, state.dataMundtSimMgr->ConvIntGain);
        state.dataMundtSimMgr->ConvIntGain += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum) +
                                              state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum) +
                                              state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult;

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(state, ZoneNum, RetAirConvGain, 0);
            state.dataMundtSimMgr->ConvIntGain += RetAirConvGain;
        }

        state.dataMundtSimMgr->QventCool =
            -state.dataHeatBalFanSys->MCPI(ZoneNum) * (Zone(ZoneNum).OutDryBulbTemp - state.dataHeatBalFanSys->MAT(ZoneNum));

        // get surface data
        for (SurfNum = 1; SurfNum <= state.dataMundtSimMgr->ZoneData(ZoneNum).NumOfSurfs; ++SurfNum) {
            state.dataMundtSimMgr->MundtAirSurf(SurfNum, state.dataMundtSimMgr->MundtZoneNum).Temp =
                state.dataHeatBalSurf->TempSurfIn(state.dataMundtSimMgr->ZoneData(ZoneNum).SurfFirst + SurfNum - 1);
            state.dataMundtSimMgr->MundtAirSurf(SurfNum, state.dataMundtSimMgr->MundtZoneNum).Hc =
                state.dataHeatBal->HConvIn(state.dataMundtSimMgr->ZoneData(ZoneNum).SurfFirst + SurfNum - 1);
        }
    }

    //*****************************************************************************************

    void SetupMundtModel(EnergyPlusData &state,
                         int const ZoneNum, // index number for the specified zone
                         bool &ErrorsFound  // true if problems setting up model
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Febraury 2002
        //       RE-ENGINEERED  June 2003, EnergyPlus Implementation (CC)
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

        // PURPOSE OF THIS SUBROUTINE:
        //   Subroutine must be called once before main model calculation
        //   need to pass some zone characteristics only once
        //   initializes module level variables, collect info from Air Data Manager

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataRoomAirModel;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NodeNum; // index for air nodes
        int SurfNum; // index for surfaces

        // set up air node ID
        state.dataMundtSimMgr->NumRoomNodes = 0;
        for (NodeNum = 1; NodeNum <= state.dataRoomAirMod->TotNumOfZoneAirNodes(ZoneNum); ++NodeNum) {
            {
                auto const SELECT_CASE_var(state.dataMundtSimMgr->LineNode(NodeNum, state.dataMundtSimMgr->MundtZoneNum).ClassType);
                if (SELECT_CASE_var == AirNodeType::InletAirNode) { // inlet
                    state.dataMundtSimMgr->SupplyNodeID = NodeNum;
                } else if (SELECT_CASE_var == AirNodeType::FloorAirNode) { // floor
                    state.dataMundtSimMgr->MundtFootAirID = NodeNum;
                } else if (SELECT_CASE_var == AirNodeType::ControlAirNode) { // thermostat
                    state.dataMundtSimMgr->TstatNodeID = NodeNum;
                } else if (SELECT_CASE_var == AirNodeType::CeilingAirNode) { // ceiling
                    state.dataMundtSimMgr->MundtCeilAirID = NodeNum;
                } else if (SELECT_CASE_var == AirNodeType::MundtRoomAirNode) { // wall
                    ++state.dataMundtSimMgr->NumRoomNodes;
                    state.dataMundtSimMgr->RoomNodeIDs(state.dataMundtSimMgr->NumRoomNodes) = NodeNum;
                } else if (SELECT_CASE_var == AirNodeType::ReturnAirNode) { // return
                    state.dataMundtSimMgr->ReturnNodeID = NodeNum;
                } else {
                    ShowSevereError(state, "SetupMundtModel: Non-Standard Type of Air Node for Mundt Model");
                    ErrorsFound = true;
                }
            }
        }

        //  get number of floors in the zone and setup FloorSurfSetIDs
        if (state.dataMundtSimMgr->MundtFootAirID > 0) {
            state.dataMundtSimMgr->NumFloorSurfs =
                count(state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtFootAirID, state.dataMundtSimMgr->MundtZoneNum).SurfMask);
            state.dataMundtSimMgr->FloorSurfSetIDs =
                pack(state.dataMundtSimMgr->ID1dSurf,
                     state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtFootAirID, state.dataMundtSimMgr->MundtZoneNum).SurfMask);
            // initialize floor surface data (a must since NumFloorSurfs is varied among zones)
            for (auto &e : state.dataMundtSimMgr->FloorSurf) {
                e.Temp = 25.0;
                e.Hc = 0.0;
                e.Area = 0.0;
            }
            // get floor surface data
            for (SurfNum = 1; SurfNum <= state.dataMundtSimMgr->NumFloorSurfs; ++SurfNum) {
                state.dataMundtSimMgr->FloorSurf(SurfNum).Temp =
                    state.dataMundtSimMgr->MundtAirSurf(state.dataMundtSimMgr->FloorSurfSetIDs(SurfNum), state.dataMundtSimMgr->MundtZoneNum).Temp;
                state.dataMundtSimMgr->FloorSurf(SurfNum).Hc =
                    state.dataMundtSimMgr->MundtAirSurf(state.dataMundtSimMgr->FloorSurfSetIDs(SurfNum), state.dataMundtSimMgr->MundtZoneNum).Hc;
                state.dataMundtSimMgr->FloorSurf(SurfNum).Area =
                    state.dataMundtSimMgr->MundtAirSurf(state.dataMundtSimMgr->FloorSurfSetIDs(SurfNum), state.dataMundtSimMgr->MundtZoneNum).Area;
            }
        } else {
            ShowSevereError(state, "SetupMundtModel: Mundt model has no FloorAirNode, Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
            ErrorsFound = true;
        }
    }

    //*****************************************************************************************

    void CalcMundtModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   September 2001
        //       RE-ENGINEERED  July 2003, EnergyPlus Implementation (CC)
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

        // PURPOSE OF THIS SUBROUTINE:
        //   Compute the simplified version of Mundt and store results in Air data Manager
        //   argument passing is plentiful but are IN and nothing out.
        //   these variables are scaler conditions at current HB day,timestep, and iteration
        //   This subroutine is USE'ed by heat balance driver (top level module)

        // METHODOLOGY EMPLOYED:
        //   apply Mundt's simple model for delta Temp head-foot and update values in Air data manager.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TAirFoot;        // air temperature at the floor
        Real64 TAirCeil;        // air temperature at the ceiling
        Real64 TLeaving;        // air temperature leaving zone (= return air temp)
        Real64 TControlPoint;   // air temperature at thermostat
        Real64 Slope;           // vertical air temperature gradient (slope) from Mundt equations
        Real64 QequipConvFloor; // convective gain at the floor due to internal heat sources
        Real64 QSensInfilFloor; // convective gain at the floor due to infiltration
        Real64 FloorSumHAT;     // sum of hci*area*temp at the floor
        Real64 FloorSumHA;      // sum of hci*area at the floor
        Real64 TThisNode;       // dummy variable for air node temp
        int NodeNum;            // index for air nodes
        int SurfNum;            // index for surfaces
        int SurfCounted;        // number of surfaces assciated with an air node

        //   apply floor splits
        QequipConvFloor = state.dataRoomAirMod->ConvectiveFloorSplit(ZoneNum) * state.dataMundtSimMgr->ConvIntGain;
        QSensInfilFloor = -state.dataRoomAirMod->InfiltratFloorSplit(ZoneNum) * state.dataMundtSimMgr->QventCool;

        // Begin computations for Mundt model

        // do summations for floor surfaces of this zone
        FloorSumHAT = 0.0;
        FloorSumHA = 0.0;
        for (auto const &s : state.dataMundtSimMgr->FloorSurf) {
            FloorSumHAT += s.Area * s.Hc * s.Temp;
            FloorSumHA += s.Area * s.Hc;
        }

        // Eq 2.2 in ASHRAE RP 1222 Final report
        TAirFoot =
            ((state.dataMundtSimMgr->ZoneAirDensity * CpAir * state.dataMundtSimMgr->SupplyAirVolumeRate * state.dataMundtSimMgr->SupplyAirTemp) +
             (FloorSumHAT) + QequipConvFloor + QSensInfilFloor) /
            ((state.dataMundtSimMgr->ZoneAirDensity * CpAir * state.dataMundtSimMgr->SupplyAirVolumeRate) + (FloorSumHA));

        // prevent dividing by zero due to zero cooling load (or zero supply air flow rate)
        if (state.dataMundtSimMgr->QsysCoolTot <= 0.0) {
            TLeaving = state.dataMundtSimMgr->SupplyAirTemp;
        } else {
            // Eq 2.3 in ASHRAE RP 1222 Final report
            TLeaving =
                (state.dataMundtSimMgr->QsysCoolTot / (state.dataMundtSimMgr->ZoneAirDensity * CpAir * state.dataMundtSimMgr->SupplyAirVolumeRate)) +
                state.dataMundtSimMgr->SupplyAirTemp;
        }

        // Eq 2.4 in ASHRAE RP 1222 Final report
        Slope = (TLeaving - TAirFoot) /
                (state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Height -
                 state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtFootAirID, state.dataMundtSimMgr->MundtZoneNum).Height);
        // check slope
        if (Slope > MaxSlope) {
            Slope = MaxSlope;
            TAirFoot = TLeaving -
                       (Slope * (state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Height -
                                 state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtFootAirID, state.dataMundtSimMgr->MundtZoneNum).Height));
        }
        if (Slope < MinSlope) { // pretty much vertical
            Slope = MinSlope;
            TAirFoot = TLeaving;
        }

        // Eq 2.4 in ASHRAE RP 1222 Final report
        TAirCeil =
            TLeaving - (Slope * (state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Height -
                                 state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtCeilAirID, state.dataMundtSimMgr->MundtZoneNum).Height));

        TControlPoint =
            TLeaving - (Slope * (state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Height -
                                 state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->TstatNodeID, state.dataMundtSimMgr->MundtZoneNum).Height));

        // determine air node temperatures in this zone
        SetNodeResult(state, state.dataMundtSimMgr->SupplyNodeID, state.dataMundtSimMgr->SupplyAirTemp);
        SetNodeResult(state, state.dataMundtSimMgr->ReturnNodeID, TLeaving);
        SetNodeResult(state, state.dataMundtSimMgr->MundtCeilAirID, TAirCeil);
        SetNodeResult(state, state.dataMundtSimMgr->MundtFootAirID, TAirFoot);
        SetNodeResult(state, state.dataMundtSimMgr->TstatNodeID, TControlPoint);

        for (SurfNum = 1; SurfNum <= state.dataMundtSimMgr->NumFloorSurfs; ++SurfNum) {
            SetSurfTmeanAir(state, state.dataMundtSimMgr->FloorSurfSetIDs(SurfNum), TAirFoot);
        }

        SurfCounted = count(state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtCeilAirID, state.dataMundtSimMgr->MundtZoneNum).SurfMask);
        state.dataMundtSimMgr->TheseSurfIDs =
            pack(state.dataMundtSimMgr->ID1dSurf,
                 state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->MundtCeilAirID, state.dataMundtSimMgr->MundtZoneNum).SurfMask);
        for (SurfNum = 1; SurfNum <= SurfCounted; ++SurfNum) {
            SetSurfTmeanAir(state, state.dataMundtSimMgr->TheseSurfIDs(SurfNum), TAirCeil);
        }

        for (NodeNum = 1; NodeNum <= state.dataMundtSimMgr->NumRoomNodes; ++NodeNum) {
            TThisNode =
                TLeaving -
                (Slope * (state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Height -
                          state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->RoomNodeIDs(NodeNum), state.dataMundtSimMgr->MundtZoneNum).Height));
            SetNodeResult(state, state.dataMundtSimMgr->RoomNodeIDs(NodeNum), TThisNode);
            SurfCounted =
                count(state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->RoomNodeIDs(NodeNum), state.dataMundtSimMgr->MundtZoneNum).SurfMask);
            state.dataMundtSimMgr->TheseSurfIDs =
                pack(state.dataMundtSimMgr->ID1dSurf,
                     state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->RoomNodeIDs(NodeNum), state.dataMundtSimMgr->MundtZoneNum).SurfMask);
            for (SurfNum = 1; SurfNum <= SurfCounted; ++SurfNum) {
                SetSurfTmeanAir(state, state.dataMundtSimMgr->TheseSurfIDs(SurfNum), TThisNode);
            }
        }
    }

    //*****************************************************************************************

    void SetNodeResult(EnergyPlusData &state,
                       int const NodeID,       // node ID
                       Real64 const TempResult // temperature for the specified air node
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   September 2002
        //       RE-ENGINEERED  April 2003, Weixiu Kong, EnergyPlus Implementation
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

        // PURPOSE OF THIS SUBROUTINE:
        //   provide set routine for reporting results
        //   to AirDataManager from air model

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

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
        // na

        state.dataMundtSimMgr->LineNode(NodeID, state.dataMundtSimMgr->MundtZoneNum).Temp = TempResult;
    }

    //*****************************************************************************************

    void SetSurfTmeanAir(EnergyPlusData &state,
                         int const SurfID,    // surface ID
                         Real64 const TeffAir // temperature of air node adjacent to the specified surface
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   September 2002
        //       RE-ENGINEERED  April 2003, Wiexiu Kong, EnergyPlus Implementation
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

        // PURPOSE OF THIS SUBROUTINE:
        //   provide set routine for air model prediction of
        //   effective air for single surface

        state.dataMundtSimMgr->MundtAirSurf(SurfID, state.dataMundtSimMgr->MundtZoneNum).TMeanAir = TeffAir;
    }

    //*****************************************************************************************

    void SetSurfHBDataForMundtModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chanvit Chantrasrisalai
        //       DATE WRITTEN   July 2003
        //       MODIFIED       February 2004, fix allocate-deallocate problem (CC)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     map data from air domain back to surface domain for each particular zone

        // Using/Aliasing
        using DataSurfaces::AdjacentAirTemp;
        using DataSurfaces::ZoneMeanAirTemp;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;      // index for surfaces
        int SurfFirst;    // index number of the first surface in the zone
        int NumOfSurfs;   // number of surfaces in the zone
        int ZoneNodeNum;  // index number of the zone node
        Real64 DeltaTemp; // dummy variable for temperature difference

        // get surface info
        SurfFirst = state.dataMundtSimMgr->ZoneData(ZoneNum).SurfFirst;
        NumOfSurfs = state.dataMundtSimMgr->ZoneData(ZoneNum).NumOfSurfs;

        if ((state.dataMundtSimMgr->SupplyAirVolumeRate > 0.0001) &&
            (state.dataMundtSimMgr->QsysCoolTot > 0.0001)) { // Controlled zone when the system is on

            if (state.dataRoomAirMod->AirModel(ZoneNum).TempCoupleScheme == DataRoomAirModel::CouplingScheme::Direct) {
                // Use direct coupling scheme to report air temperatures back to surface/system domains
                // a) Bulk air temperatures -> TempEffBulkAir(SurfNum)
                for (SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfFirst + SurfNum - 1) =
                        state.dataMundtSimMgr->MundtAirSurf(SurfNum, state.dataMundtSimMgr->MundtZoneNum).TMeanAir;
                    // set flag for reference air temperature
                    state.dataSurface->SurfTAirRef(SurfFirst + SurfNum - 1) = AdjacentAirTemp;
                }
                // b) Average zone air temperature -> ZT(ZoneNum)
                // For Mundt model, average room air is the weighted value of floor and ceiling air temps
                // TRoomAverage = ( LineNode( MundtCeilAirID, MundtZoneNum ).Temp + LineNode( MundtFootAirID, MundtZoneNum ).Temp ) / 2;
                // ZT(ZoneNum) = TRoomAverage
                // c) Leaving-zone air temperature -> Node(ZoneNode)%Temp
                ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
                state.dataLoopNodes->Node(ZoneNodeNum).Temp =
                    state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Temp;
                // d) Thermostat air temperature -> TempTstatAir(ZoneNum)
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) =
                    state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->TstatNodeID, state.dataMundtSimMgr->MundtZoneNum).Temp;
            } else {
                // Use indirect coupling scheme to report air temperatures back to surface/system domains
                // a) Bulk air temperatures -> TempEffBulkAir(SurfNum)
                for (SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum) {
                    DeltaTemp = state.dataMundtSimMgr->MundtAirSurf(SurfNum, state.dataMundtSimMgr->MundtZoneNum).TMeanAir -
                                state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->TstatNodeID, state.dataMundtSimMgr->MundtZoneNum).Temp;
                    state.dataHeatBal->SurfTempEffBulkAir(SurfFirst + SurfNum - 1) =
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + DeltaTemp;
                    // set flag for reference air temperature
                    state.dataSurface->SurfTAirRef(SurfFirst + SurfNum - 1) = AdjacentAirTemp;
                }
                // b) Average zone air temperature -> ZT(ZoneNum)
                // For Mundt model, average room air is the weighted value of floor and ceiling air temps
                // TRoomAverage = ( LineNode( MundtCeilAirID, MundtZoneNum ).Temp + LineNode( MundtFootAirID, MundtZoneNum ).Temp ) / 2;
                // DeltaTemp = TRoomAverage - LineNode( TstatNodeID, MundtZoneNum ).Temp;
                // ZT(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum) + DeltaTemp
                // c) Leaving-zone air temperature -> Node(ZoneNode)%Temp
                ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
                DeltaTemp = state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->ReturnNodeID, state.dataMundtSimMgr->MundtZoneNum).Temp -
                            state.dataMundtSimMgr->LineNode(state.dataMundtSimMgr->TstatNodeID, state.dataMundtSimMgr->MundtZoneNum).Temp;
                state.dataLoopNodes->Node(ZoneNodeNum).Temp = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + DeltaTemp;
                // d) Thermostat air temperature -> TempTstatAir(ZoneNum)
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) =
                    state.dataHeatBalFanSys->ZT(ZoneNum); // for indirect coupling, control air temp is equal to mean air temp?
            }
            // set flag to indicate that Mundt model is used for this zone at the present time
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
        } else { // Controlled zone when the system is off --> Use the mixing model instead of the Mundt model
            // Bulk air temperatures -> TempEffBulkAir(SurfNum)
            for (SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfFirst + SurfNum - 1) = state.dataHeatBalFanSys->MAT(ZoneNum);
                // set flag for reference air temperature
                state.dataSurface->SurfTAirRef(SurfFirst + SurfNum - 1) = ZoneMeanAirTemp;
            }
            // set flag to indicate that Mundt model is NOT used for this zone at the present time
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
        }
    }

    //*****************************************************************************************

} // namespace MundtSimMgr

} // namespace EnergyPlus
