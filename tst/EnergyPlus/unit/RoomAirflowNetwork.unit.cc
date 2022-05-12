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

// EnergyPlus::Fans Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

using namespace EnergyPlus;
using namespace DataEnvironment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace DataRoomAirModel;
using namespace DataMoistureBalanceEMPD;
using namespace DataSurfaces;
using namespace DataHeatBalSurface;
using namespace EnergyPlus::RoomAirModelAirflowNetwork;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::Psychrometrics;

class RoomAirflowNetworkTest : public EnergyPlusFixture
{
protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        state->dataSize->CurZoneEqNum = 0;
        state->dataSize->CurSysNum = 0;
        state->dataSize->CurOASysNum = 0;
        state->dataGlobal->NumOfZones = 1;
        state->dataGlobal->numSpaces = 1;
        state->dataLoopNodes->NumOfNodes = 5;
        state->dataGlobal->BeginEnvrnFlag = true;
        int NumOfSurfaces = 2;
        state->dataRoomAirMod->RoomAirflowNetworkZoneInfo.allocate(state->dataGlobal->NumOfZones);
        state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
        state->dataHeatBal->space.allocate(state->dataGlobal->numSpaces);
        state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);
        state->dataZoneEquip->ZoneEquipList.allocate(state->dataGlobal->NumOfZones);
        state->dataHeatBal->ZoneIntGain.allocate(state->dataGlobal->NumOfZones);
        state->dataHeatBal->spaceIntGainDevices.allocate(state->dataGlobal->numSpaces);
        state->dataLoopNodes->NodeID.allocate(state->dataLoopNodes->NumOfNodes);
        state->dataLoopNodes->Node.allocate(state->dataLoopNodes->NumOfNodes);
        state->dataSurface->Surface.allocate(NumOfSurfaces);
        state->dataSurface->SurfTAirRef.allocate(NumOfSurfaces);
        state->dataHeatBalSurf->SurfHConvInt.allocate(NumOfSurfaces);
        state->dataHeatBalSurf->SurfTempInTmp.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVSurface.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVSurfaceOld.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVDeepLayer.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVdeepOld.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVSurfLayerOld.allocate(NumOfSurfaces);
        state->dataMstBalEMPD->RVSurfLayer.allocate(NumOfSurfaces);
        state->dataMstBal->RhoVaporSurfIn.allocate(NumOfSurfaces);
        state->dataMstBal->RhoVaporAirIn.allocate(NumOfSurfaces);
        state->dataMstBal->HMassConvInFD.allocate(NumOfSurfaces);
        state->dataHeatBalFanSys->MAT.allocate(state->dataGlobal->NumOfZones);
        state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
        state->afn->AirflowNetworkLinkageData.allocate(5);
        state->afn->AirflowNetworkNodeSimu.allocate(6);
        state->afn->AirflowNetworkLinkSimu.allocate(5);
        state->dataRoomAirflowNetModel->RAFN.allocate(state->dataGlobal->NumOfZones);
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(RoomAirflowNetworkTest, RAFNTest)
{
    int NumOfAirNodes = 2;
    int ZoneNum = 1;
    int RoomAirNode;
    state->dataHVACGlobal->TimeStepSys = 15.0 / 60.0;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens = 1;

    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID = ZoneNum;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes = NumOfAirNodes;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node.allocate(NumOfAirNodes);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).ZoneVolumeFraction = 0.2;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).ZoneVolumeFraction = 0.8;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).NumHVACs = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).NumHVACs = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).SupplyFraction = 0.4;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).SupplyFraction = 0.6;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).ReturnFraction = 0.4;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).ReturnFraction = 0.6;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).Name = "ZoneHVAC";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).Name = "ZoneHVAC";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).SupplyNodeName = "Supply";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).SupplyNodeName = "Supply";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).ReturnNodeName = "Return";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).ReturnNodeName = "Return";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HVAC(1).Name = "ZoneHVAC";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HVAC(1).Name = "ZoneHVAC";
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).IntGainsDeviceIndices.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).IntGainsDeviceIndices.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).intGainsDeviceSpaces.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).intGainsDeviceSpaces.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).NumIntGains = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).NumIntGains = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).IntGainsDeviceIndices(1) = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).IntGainsDeviceIndices(1) = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).intGainsDeviceSpaces(1) = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).intGainsDeviceSpaces(1) = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).IntGainsFractions.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).IntGainsFractions.allocate(1);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).IntGainsFractions(1) = 0.4;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).IntGainsFractions(1) = 0.6;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HasIntGainsAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HasIntGainsAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HasSurfacesAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HasSurfacesAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).HasHVACAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).HasHVACAssigned = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).SurfMask.allocate(2);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).SurfMask.allocate(2);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).SurfMask(1) = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).SurfMask(2) = false;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).SurfMask(1) = false;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).SurfMask(2) = true;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).NumOfAirflowLinks = 3;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).Link.allocate(3);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).Link(1).AirflowNetworkLinkSimuID = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).Link(2).AirflowNetworkLinkSimuID = 2;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).Link(3).AirflowNetworkLinkSimuID = 3;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(1).AirflowNetworkNodeID = 1;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).NumOfAirflowLinks = 3;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).Link.allocate(3);
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).Link(1).AirflowNetworkLinkSimuID = 3;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).Link(2).AirflowNetworkLinkSimuID = 4;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).Link(3).AirflowNetworkLinkSimuID = 5;
    state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(2).AirflowNetworkNodeID = 2;

    state->afn->AirflowNetworkLinkageData(1).NodeNums[0] = 1;
    state->afn->AirflowNetworkLinkageData(2).NodeNums[0] = 1;
    state->afn->AirflowNetworkLinkageData(3).NodeNums[0] = 1;
    state->afn->AirflowNetworkLinkageData(1).NodeNums[1] = 3;
    state->afn->AirflowNetworkLinkageData(2).NodeNums[1] = 4;
    state->afn->AirflowNetworkLinkageData(3).NodeNums[1] = 2;
    state->afn->AirflowNetworkLinkageData(4).NodeNums[0] = 2;
    state->afn->AirflowNetworkLinkageData(5).NodeNums[0] = 2;
    state->afn->AirflowNetworkLinkageData(4).NodeNums[1] = 5;
    state->afn->AirflowNetworkLinkageData(5).NodeNums[1] = 6;
    state->afn->AirflowNetworkNodeSimu(1).TZ = 25.0;
    state->afn->AirflowNetworkNodeSimu(1).WZ = 0.001;
    state->afn->AirflowNetworkNodeSimu(2).TZ = 20.0;
    state->afn->AirflowNetworkNodeSimu(2).WZ = 0.002;
    state->afn->AirflowNetworkNodeSimu(3).TZ = 30.0;
    state->afn->AirflowNetworkNodeSimu(3).WZ = 0.001;
    state->afn->AirflowNetworkNodeSimu(4).TZ = 22.0;
    state->afn->AirflowNetworkNodeSimu(4).WZ = 0.001;
    state->afn->AirflowNetworkNodeSimu(5).TZ = 27.0;
    state->afn->AirflowNetworkNodeSimu(5).WZ = 0.0015;
    state->afn->AirflowNetworkNodeSimu(6).TZ = 20.0;
    state->afn->AirflowNetworkNodeSimu(6).WZ = 0.002;
    state->afn->AirflowNetworkLinkSimu(1).FLOW = 0.0;
    state->afn->AirflowNetworkLinkSimu(1).FLOW2 = 0.01;
    state->afn->AirflowNetworkLinkSimu(2).FLOW = 0.0;
    state->afn->AirflowNetworkLinkSimu(2).FLOW2 = 0.02;
    state->afn->AirflowNetworkLinkSimu(3).FLOW = 0.01;
    state->afn->AirflowNetworkLinkSimu(3).FLOW2 = 0.0;
    state->afn->AirflowNetworkLinkSimu(4).FLOW = 0.0;
    state->afn->AirflowNetworkLinkSimu(4).FLOW2 = 0.01;
    state->afn->AirflowNetworkLinkSimu(5).FLOW = 0.01;
    state->afn->AirflowNetworkLinkSimu(5).FLOW2 = 0.0;

    state->dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes = 1;
    state->dataZoneEquip->ZoneEquipList(ZoneNum).EquipName.allocate(1);
    state->dataZoneEquip->ZoneEquipList(ZoneNum).EquipName(1) = "ZoneHVAC";

    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum = ZoneNum;
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNode(1) = 1;
    state->dataLoopNodes->NodeID.allocate(state->dataLoopNodes->NumOfNodes);
    state->dataLoopNodes->Node.allocate(state->dataLoopNodes->NumOfNodes);
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode(1) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataHeatBal->Zone(ZoneNum).Volume = 100;
    state->dataHeatBal->Zone(ZoneNum).IsControlled = true;
    state->dataHeatBal->Zone(ZoneNum).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(ZoneNum).HTSurfaceLast = 2;
    state->dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist = 0;
    state->dataHeatBal->Zone(ZoneNum).spaceIndexes.emplace_back(1);

    state->dataHeatBal->spaceIntGainDevices(ZoneNum).numberOfDevices = 1;
    state->dataHeatBal->spaceIntGainDevices(ZoneNum).device.allocate(state->dataHeatBal->spaceIntGainDevices(1).numberOfDevices);
    state->dataHeatBal->spaceIntGainDevices(ZoneNum).device(1).CompObjectName = "PEOPLE";
    state->dataHeatBal->spaceIntGainDevices(ZoneNum).device(1).CompType = DataHeatBalance::IntGainType::People;
    state->dataHeatBal->spaceIntGainDevices(ZoneNum).device(1).ConvectGainRate = 300.0;
    state->dataHeatBal->spaceIntGainDevices(ZoneNum).device(1).LatentGainRate = 200.0;

    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(1).Area = 1.0;
    state->dataSurface->Surface(2).Area = 2.0;

    state->dataSurface->Surface(1).HeatTransferAlgorithm = HeatTransferModel::EMPD;
    state->dataSurface->Surface(2).HeatTransferAlgorithm = HeatTransferModel::EMPD;

    state->dataSurface->SurfTAirRef = 0;

    state->dataMstBalEMPD->RVSurface(1) = 0.0011;
    state->dataMstBalEMPD->RVSurface(2) = 0.0012;

    state->dataLoopNodes->NodeID(1) = "Supply";
    state->dataLoopNodes->NodeID(2) = "Return";

    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(1).HumRat = 0.001;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.01;

    state->dataHeatBalFanSys->MAT(1) = 20.0;
    state->dataHeatBalSurf->SurfHConvInt(1) = 1.0;
    state->dataHeatBalSurf->SurfHConvInt(2) = 1.0;
    state->dataHeatBalSurf->SurfTempInTmp(1) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 30.0;
    state->dataMstBal->RhoVaporAirIn(1) =
        PsyRhovFnTdbWPb(state->dataHeatBalFanSys->MAT(ZoneNum), state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state->dataEnvrn->OutBaroPress);
    state->dataMstBal->RhoVaporAirIn(2) =
        PsyRhovFnTdbWPb(state->dataHeatBalFanSys->MAT(ZoneNum), state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state->dataEnvrn->OutBaroPress);
    state->dataMstBal->HMassConvInFD(1) =
        state->dataHeatBalSurf->SurfHConvInt(1) /
        ((PsyRhoAirFnPbTdbW(
              *state, state->dataEnvrn->OutBaroPress, state->dataHeatBalFanSys->MAT(ZoneNum), state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) +
          state->dataMstBal->RhoVaporAirIn(1)) *
         PsyCpAirFnW(state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)));
    state->dataMstBal->HMassConvInFD(2) =
        state->dataHeatBalSurf->SurfHConvInt(2) /
        ((PsyRhoAirFnPbTdbW(
              *state, state->dataEnvrn->OutBaroPress, state->dataHeatBalFanSys->MAT(ZoneNum), state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) +
          state->dataMstBal->RhoVaporAirIn(2)) *
         PsyCpAirFnW(state->dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)));

    RoomAirNode = 1;
    auto &thisRAFN(state->dataRoomAirflowNetModel->RAFN(ZoneNum));
    thisRAFN.ZoneNum = ZoneNum;

    thisRAFN.InitRoomAirModelAirflowNetwork(*state, RoomAirNode);

    EXPECT_NEAR(120.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain, 0.00001);
    EXPECT_NEAR(80.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain, 0.00001);
    EXPECT_NEAR(1.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA, 0.00001);
    EXPECT_NEAR(25.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf, 0.00001);
    EXPECT_NEAR(0.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref, 0.00001);
    EXPECT_NEAR(4.0268, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCp, 0.0001);
    EXPECT_NEAR(80.536, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCpT, 0.001);
    EXPECT_NEAR(0.004, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysM, 0.00001);
    EXPECT_NEAR(4.0e-6, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMW, 0.00001);
    EXPECT_NEAR(30.200968, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp, 0.0001);
    EXPECT_NEAR(744.95722, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT, 0.001);
    EXPECT_NEAR(0.03, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM, 0.00001);
    EXPECT_NEAR(3.0e-5, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW, 0.00001);
    EXPECT_NEAR(-8.431365e-8, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmAW, 0.0000001);
    EXPECT_NEAR(0.0009756833, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa, 0.0000001);
    EXPECT_NEAR(9.0784549e-7, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW, 0.0000001);

    thisRAFN.CalcRoomAirModelAirflowNetwork(*state, RoomAirNode);

    EXPECT_NEAR(24.907085, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp, 0.00001);
    EXPECT_NEAR(0.00189601, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat, 0.00001);
    EXPECT_NEAR(9.770445, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RelHumidity, 0.00001);

    RoomAirNode = 2;
    thisRAFN.InitRoomAirModelAirflowNetwork(*state, RoomAirNode);

    EXPECT_NEAR(180.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain, 0.00001);
    EXPECT_NEAR(120.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain, 0.00001);
    EXPECT_NEAR(2.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA, 0.00001);
    EXPECT_NEAR(60.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf, 0.00001);
    EXPECT_NEAR(0.0, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref, 0.00001);
    EXPECT_NEAR(6.04019, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCp, 0.0001);
    EXPECT_NEAR(120.803874, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCpT, 0.00001);
    EXPECT_NEAR(0.006, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysM, 0.00001);
    EXPECT_NEAR(6.0e-6, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMW, 0.00001);
    EXPECT_NEAR(20.14327, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp, 0.0001);
    EXPECT_NEAR(523.73441, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT, 0.001);
    EXPECT_NEAR(0.02, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM, 0.00001);
    EXPECT_NEAR(2.5e-5, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW, 0.00001);
    EXPECT_NEAR(-3.5644894e-9, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmAW, 0.0000001);
    EXPECT_NEAR(0.0019191284, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa, 0.0000001);
    EXPECT_NEAR(1.98975381e-6, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW, 0.0000001);

    thisRAFN.CalcRoomAirModelAirflowNetwork(*state, RoomAirNode);

    EXPECT_NEAR(24.057841, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp, 0.00001);
    EXPECT_NEAR(0.0028697086, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat, 0.00001);
    EXPECT_NEAR(15.53486185, state->dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RelHumidity, 0.00001);

    thisRAFN.UpdateRoomAirModelAirflowNetwork(*state);

    EXPECT_NEAR(24.397538, state->dataLoopNodes->Node(2).Temp, 0.00001);
    EXPECT_NEAR(0.0024802305, state->dataLoopNodes->Node(2).HumRat, 0.000001);
}
TEST_F(EnergyPlusFixture, RoomAirInternalGains_InternalHeatGains_Check)
{
    // different names between internal gain objects and room air objects for internal gains result in fatal error from GetInternalGainDeviceIndex.
    bool ErrorsFound(false);
    std::string const idf_objects = delimited_string({

        "Zone,living_unit1;",

        "BuildingSurface:Detailed,",
        "    unit1,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION,               !- Construction Name",
        "    living_unit1,               !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "Construction,",
        "    PARTITION,             !- Name",
        "    GYP BOARD;  !- Outside Layer",

        "Material,",
        "    GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "Schedule:Constant,sch_act,,120.0;",
        "Schedule:Constant,sch,,1.0;",
        "People,",
        "  people_unit1,            !- Name",
        "  living_unit1,            !- Zone or ZoneList Name",
        "  sch,           !- Number of People Schedule Name",
        "  People,                  !- Number of People Calculation Method",
        "  3,                       !- Number of People",
        "  ,                        !- People per Zone Floor Area {person / m2}",
        "  ,                        !- Zone Floor Area per Person {m2 / person}",
        "  0,                       !- Fraction Radiant",
        " autocalculate,           !- Sensible Heat Fraction",
        " sch_act,            !- Activity Level Schedule Name",
        " ;                        !- Carbon Dioxide Generation Rate {m3 / s - W}",

        "Lights,",
        "  Living Hardwired Lighting1,  !- Name",
        "  living_unit1,            !- Zone or ZoneList Name",
        "  sch,  !- Schedule Name",
        "  LightingLevel,           !- Design Level Calculation Method",
        "  1000,                    !- Lighting Level {W}",
        "  ,                        !- Watts per Zone Floor Area {W / m2}",
        "  ,                        !- Watts per Person {W / person}",
        "  0,                       !- Return Air Fraction",
        "  0.6,                     !- Fraction Radiant",
        "  0.2,                     !- Fraction Visible",
        "  0;                       !- Fraction Replaceable",
        " ElectricEquipment,",
        "  Electric Equipment 1,  !- Name",
        "  living_unit1,               !- Zone or ZoneList Name",
        "  sch,               !- Schedule Name",
        "  EquipmentLevel,          !- Design Level Calculation Method",
        "  150.0,                   !- Design Level {W}",
        "  ,                        !- Watts per Zone Floor Area {W/m2}",
        "  ,                        !- Watts per Person {W/person}",
        "  0.0000,                  !- Fraction Latent",
        "  0.5000,                  !- Fraction Radiant",
        "  0.0000;                  !- Fraction Lost",

        "RoomAirModelType,",
        " RoomAirWithAirflowNetwork,  !- Name",
        " living_unit1,            !- Zone Name",
        " AirflowNetwork,          !- Room - Air Modeling Type",
        " DIRECT;                  !- Air Temperature Coupling Strategy",

        "RoomAir:Node:AirflowNetwork,",
        " Node1,                   !- Name",
        " living_unit1,            !- Zone Name",
        " 1,                    !- Fraction of Zone Air Volume",
        " unit1_List,   !- RoomAir : Node : AirflowNetwork : AdjacentSurfaceList Name",
        " Node1_Gain,              !- RoomAir : Node : AirflowNetwork : InternalGains Name",
        " Node1_HVAC;              !- RoomAir:Node:AirflowNetwork:HVACEquipment Name",

        "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList,",
        " unit1_List,   !- Name",
        " unit1;        !- Surface 1 Name",

        "RoomAir:Node:AirflowNetwork:InternalGains,",
        " Node1_Gain,              !- Name",
        " People,                  !- Internal Gain Object 1 Type",
        " living_unit1 People,     !- Internal Gain Object 1 Name",
        " 1,                    !- Fraction of Gains to Node 1",
        " Lights,                  !- Internal Gain Object 2 Type",
        " living_unit1 Lights,     !- Internal Gain Object 2 Name",
        " 1,                    !- Fraction of Gains to Node 2",
        " ElectricEquipment,       !- Internal Gain Object 3 Type",
        " living_unit1 Equip,      !- Internal Gain Object 3 Name",
        " 1;                    !- Fraction of Gains to Node 3",

        "RoomAirSettings:AirflowNetwork,",
        "  living_unit1,            !- Name",
        "  living_unit1,            !- Zone Name",
        "  Node1,            !- Control Point RoomAirflowNetwork : Node Name",
        "  Node1;                   !- RoomAirflowNetwork : Node Name 1",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    ErrorsFound = false;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput(*state);

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ErrorsFound = false;
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ErrorsFound = false;
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ErrorsFound = false;
    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(1);
    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    SurfaceGeometry::GetSurfaceData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    HeatBalanceManager::AllocateHeatBalArrays(*state);
    InternalHeatGains::GetInternalHeatGainsInput(*state);

    ErrorsFound = false;
    state->dataRoomAirMod->AirModel.allocate(1);
    state->dataRoomAirMod->AirModel(1).AirModelType = DataRoomAirModel::RoomAirModel::AirflowNetwork;
    RoomAirModelManager::GetRoomAirflowNetworkData(*state, ErrorsFound);
    EXPECT_TRUE(ErrorsFound);

    std::string const error_string =
        delimited_string({"   ** Severe  ** GetRoomAirflowNetworkData: Invalid Internal Gain Object Name = LIVING_UNIT1 PEOPLE",
                          "   **   ~~~   ** Entered in RoomAir:Node:AirflowNetwork:InternalGains = NODE1_GAIN",
                          "   **   ~~~   ** Internal gain did not match correctly",
                          "   ** Severe  ** GetRoomAirflowNetworkData: Invalid Internal Gain Object Name = LIVING_UNIT1 LIGHTS",
                          "   **   ~~~   ** Entered in RoomAir:Node:AirflowNetwork:InternalGains = NODE1_GAIN",
                          "   **   ~~~   ** Internal gain did not match correctly",
                          "   ** Severe  ** GetRoomAirflowNetworkData: Invalid Internal Gain Object Name = LIVING_UNIT1 EQUIP",
                          "   **   ~~~   ** Entered in RoomAir:Node:AirflowNetwork:InternalGains = NODE1_GAIN",
                          "   **   ~~~   ** Internal gain did not match correctly"});

    EXPECT_TRUE(compare_err_stream(error_string, true));
}
