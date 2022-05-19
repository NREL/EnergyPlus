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

// C++ Headers
#include <algorithm>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>
#include <EnergyPlus/CoolTower.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EarthTube.hh>
#include <EnergyPlus/ElectricBaseboardRadiator.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/ExhaustAirSystemManager.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HWBaseboardRadiator.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ReturnAirPathManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/SteamBaseboardRadiator.hh>
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalChimney.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WindowAC.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::ZoneEquipmentManager {

// Module containing the routines dealing with the Zone Equipment Manager.

// MODULE INFORMATION:
//       AUTHOR         Russ Taylor
//       DATE WRITTEN   Unknown
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module manages the zone equipment.

// Using/Aliasing
using namespace DataSizing;
using namespace DataZoneEquipment;
// Use statements for access to subroutines in other modules
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyHgAirFnWTdb;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyWFnTdbRhPb;
using Psychrometrics::PsyWFnTdpPb;

void ManageZoneEquipment(EnergyPlusData &state,
                         bool const FirstHVACIteration,
                         bool &SimZone, // Set to false at the end of the routine
                         bool &SimAir   // Eventually set to true via SimZoneEquipment if AirLoop must be resimulated
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calls the zone thermal control simulations and the interfaces
    // (water-air, refrigerant-air, steam-air, electric-electric,
    // water-water, etc)

    if (state.dataZoneEquipmentManager->GetZoneEquipmentInputFlag) {
        GetZoneEquipment(state);
        state.dataZoneEquipmentManager->GetZoneEquipmentInputFlag = false;
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }

    InitZoneEquipment(state, FirstHVACIteration);

    if (state.dataGlobal->ZoneSizingCalc) {
        SizeZoneEquipment(state);
    } else {
        SimZoneEquipment(state, FirstHVACIteration, SimAir);
        state.dataZoneEquip->ZoneEquipSimulatedOnce = true;
    }

    UpdateZoneEquipment(state, SimAir);

    SimZone = false;
}

void GetZoneEquipment(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   June 1997
    //       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Get all the system related equipment which may be attached to
    // a zone

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Counter;
    int MaxNumOfEquipTypes;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
    }

    state.dataZoneEquipmentManager->NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;

    MaxNumOfEquipTypes = 0;
    for (Counter = 1; Counter <= state.dataGlobal->NumOfZones; ++Counter) {
        if (!state.dataZoneEquip->ZoneEquipConfig(Counter).IsControlled) continue;
        MaxNumOfEquipTypes = max(MaxNumOfEquipTypes, state.dataZoneEquip->ZoneEquipList(Counter).NumOfEquipTypes);
    }

    state.dataZoneEquipmentManager->PrioritySimOrder.allocate(MaxNumOfEquipTypes);
}

void InitZoneEquipment(EnergyPlusData &state, bool const FirstHVACIteration) // unused 1208
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the zone equipment prior to simulation.

    // Using/Aliasing
    using DataHVACGlobals::NoAction;
    using DataHVACGlobals::NumOfSizingTypes;
    auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNodeNum;
    int InNodeNum;
    int ExhNodeNum;
    int ZoneInNode;
    int ZoneExhNode;
    int ControlledZoneNum;

    int ZoneEquipType; // Type of zone equipment
    int TotalNumComp;  // Total number of zone components of ZoneEquipType
    int ZoneCompNum;   // Number/index of zone equipment component
    int ZoneEquipCount;

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
    auto &Node(state.dataLoopNodes->Node);

    if (state.dataZoneEquipmentManager->InitZoneEquipmentOneTimeFlag) {
        state.dataZoneEquipmentManager->InitZoneEquipmentOneTimeFlag = false;
        ZoneEqSizing.allocate(state.dataGlobal->NumOfZones);
        // setup zone equipment sequenced demand storage
        for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListIndex == 0) continue;
            ZoneEquipCount =
                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListIndex).NumOfEquipTypes;
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).NumZoneEquipment = ZoneEquipCount;
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).SequencedOutputRequired.allocate(ZoneEquipCount);
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).SequencedOutputRequiredToHeatingSP.allocate(ZoneEquipCount);
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).SequencedOutputRequiredToCoolingSP.allocate(ZoneEquipCount);
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum).NumZoneEquipment = ZoneEquipCount;
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum).SequencedOutputRequired.allocate(ZoneEquipCount);
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum).SequencedOutputRequiredToHumidSP.allocate(ZoneEquipCount);
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum).SequencedOutputRequiredToDehumidSP.allocate(ZoneEquipCount);
            ZoneEqSizing(ControlledZoneNum).SizingMethod.allocate(NumOfSizingTypes);
            ZoneEqSizing(ControlledZoneNum).SizingMethod = 0;
        }
    }

    // Do the Begin Environment initializations
    if (state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        state.dataZoneEquip->ZoneEquipAvail = NoAction;

        if (allocated(ZoneComp)) {
            for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) {
                if (allocated(ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)) {
                    TotalNumComp = ZoneComp(ZoneEquipType).TotalNumComp;
                    for (ZoneCompNum = 1; ZoneCompNum <= TotalNumComp; ++ZoneCompNum) {
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus = NoAction;
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).StartTime = 0;
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).StopTime = 0;
                    }
                }
            }
        }
        for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;

            ZoneNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
            Node(ZoneNodeNum).Temp = 20.0;
            Node(ZoneNodeNum).MassFlowRate = 0.0;
            Node(ZoneNodeNum).Quality = 1.0;
            Node(ZoneNodeNum).Press = state.dataEnvrn->OutBaroPress;
            Node(ZoneNodeNum).HumRat = state.dataEnvrn->OutHumRat;
            Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum).Temp, Node(ZoneNodeNum).HumRat);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                Node(ZoneNodeNum).CO2 = state.dataContaminantBalance->OutdoorCO2;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                Node(ZoneNodeNum).GenContam = state.dataContaminantBalance->OutdoorGC;
            }

            for (ZoneInNode = 1; ZoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++ZoneInNode) {

                InNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInNode);
                Node(InNodeNum).Temp = 20.0;
                Node(InNodeNum).MassFlowRate = 0.0;
                Node(InNodeNum).Quality = 1.0;
                Node(InNodeNum).Press = state.dataEnvrn->OutBaroPress;
                Node(InNodeNum).HumRat = state.dataEnvrn->OutHumRat;
                Node(InNodeNum).Enthalpy = PsyHFnTdbW(Node(InNodeNum).Temp, Node(InNodeNum).HumRat);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    Node(InNodeNum).CO2 = state.dataContaminantBalance->OutdoorCO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    Node(InNodeNum).GenContam = state.dataContaminantBalance->OutdoorGC;
                }
            }

            for (ZoneExhNode = 1; ZoneExhNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNode) {

                ExhNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNode);
                Node(ExhNodeNum).Temp = 20.0;
                Node(ExhNodeNum).MassFlowRate = 0.0;
                Node(ExhNodeNum).Quality = 1.0;
                Node(ExhNodeNum).Press = state.dataEnvrn->OutBaroPress;
                Node(ExhNodeNum).HumRat = state.dataEnvrn->OutHumRat;
                Node(ExhNodeNum).Enthalpy = PsyHFnTdbW(Node(ExhNodeNum).Temp, Node(ExhNodeNum).HumRat);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    Node(ExhNodeNum).CO2 = state.dataContaminantBalance->OutdoorCO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    Node(ExhNodeNum).GenContam = state.dataContaminantBalance->OutdoorGC;
                }
            }

            // BG CR 7122 following resets return air node.
            int NumRetNodes = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes;
            if (NumRetNodes > 0) {
                for (int nodeCount = 1; nodeCount <= NumRetNodes; ++nodeCount) {
                    int returnNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(nodeCount);
                    Node(returnNode).Temp = 20.0;
                    Node(returnNode).MassFlowRate = 0.0;
                    Node(returnNode).Quality = 1.0;
                    Node(returnNode).Press = state.dataEnvrn->OutBaroPress;
                    Node(returnNode).HumRat = state.dataEnvrn->OutHumRat;
                    Node(returnNode).Enthalpy = PsyHFnTdbW(Node(returnNode).Temp, Node(returnNode).HumRat);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        Node(returnNode).CO2 = state.dataContaminantBalance->OutdoorCO2;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        Node(returnNode).GenContam = state.dataContaminantBalance->OutdoorGC;
                    }
                }
            }
        }

        state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag = true;
    }

    // do the  HVAC time step initializations

    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        ZoneNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExcessZoneExh = 0.0;

        if (FirstHVACIteration) {
            for (ZoneExhNode = 1; ZoneExhNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNode) {
                ExhNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNode);
                Node(ExhNodeNum).Temp = Node(ZoneNodeNum).Temp;
                Node(ExhNodeNum).HumRat = Node(ZoneNodeNum).HumRat;
                Node(ExhNodeNum).Enthalpy = Node(ZoneNodeNum).Enthalpy;
                Node(ExhNodeNum).Press = Node(ZoneNodeNum).Press;
                Node(ExhNodeNum).Quality = Node(ZoneNodeNum).Quality;
                Node(ExhNodeNum).MassFlowRate = 0.0;
                Node(ExhNodeNum).MassFlowRateMaxAvail = 0.0;
                Node(ExhNodeNum).MassFlowRateMinAvail = 0.0;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    Node(ExhNodeNum).CO2 = Node(ZoneNodeNum).CO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    Node(ExhNodeNum).GenContam = Node(ZoneNodeNum).GenContam;
                }
            }
        }
    }

    for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
        state.dataAirLoop->AirLoopFlow(airLoop).SupFlow = 0.0;
        state.dataAirLoop->AirLoopFlow(airLoop).ZoneRetFlow = 0.0;
        state.dataAirLoop->AirLoopFlow(airLoop).SysRetFlow = 0.0;
        state.dataAirLoop->AirLoopFlow(airLoop).RecircFlow = 0.0;
        state.dataAirLoop->AirLoopFlow(airLoop).LeakFlow = 0.0;
        state.dataAirLoop->AirLoopFlow(airLoop).ExcessZoneExhFlow = 0.0;
    }
}

void SizeZoneEquipment(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Performs the zone sizing calculations and fills the zone sizing
    // data arrays with the results of the calculation.

    // METHODOLOGY EMPLOYED:
    // Using the input from Zone Sizing objects and the Zone Equipment input,
    // for each controlled zone this subroutine performs a "purchased air" calculation
    // and saves the results in the zone sizing data arrays.

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallTempDiff;

    // Parameters
    static constexpr std::string_view RoutineName("SizeZoneEquipment");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ControlledZoneNum;                // controlled zone index
    int ActualZoneNum;                    // index into Zone array (all zones)
    int SupplyAirNode1;                   // node number of 1st zone supply air node
    int SupplyAirNode2;                   // node number of 2nd zone supply air node
    int SupplyAirNode;                    // node number of supply air node for ideal air system
    int ZoneNode;                         // node number of controlled zone
    int ReturnNode;                       // node number of controlled zone return node
    Real64 DeltaTemp;                     // difference between supply air temp and zone temp [C]
    Real64 CpAir;                         // heat capacity of air [J/kg-C]
    Real64 SysOutputProvided;             // system sensible output [W]
    Real64 LatOutputProvided;             // system latent output [kg/s]
    Real64 Temp;                          // inlet temperature [C]
    Real64 HumRat;                        // inlet humidity ratio [kg water/kg dry air]
    Real64 Enthalpy;                      // inlet specific enthalpy [J/kg]
    Real64 MassFlowRate;                  // inlet mass flow rate [kg/s]
    Real64 RetTemp;                       // zone return temperature [C]
    Real64 DOASMassFlowRate(0.0);         // DOAS air mass flow rate for sizing [kg/s]
    Real64 DOASSupplyTemp(0.0);           // DOAS supply air temperature [C]
    Real64 DOASSupplyHumRat(0.0);         // DOAS supply air humidity ratio [kgWater/kgDryAir]
    Real64 DOASCpAir(0.0);                // heat capacity of DOAS air [J/kg-C]
    Real64 DOASSysOutputProvided(0.0);    // heating / cooling provided by DOAS system [W]
    Real64 TotDOASSysOutputProvided(0.0); // total DOAS load on the zone [W]
    Real64 HR90H;                         // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
    Real64 HR90L;                         // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]

    auto &Node(state.dataLoopNodes->Node);

    if (state.dataZoneEquipmentManager->SizeZoneEquipmentOneTimeFlag) {
        SetUpZoneSizingArrays(state);
        state.dataZoneEquipmentManager->SizeZoneEquipmentOneTimeFlag = false;
    }

    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;

        ActualZoneNum = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).ActualZoneNum;
        state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) = 0.0;
        state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) = 0.0;
        SysOutputProvided = 0.0;
        LatOutputProvided = 0.0;
        InitSystemOutputRequired(state, ActualZoneNum, true);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
        SupplyAirNode = 0;
        SupplyAirNode1 = 0;
        SupplyAirNode2 = 0;
        // calculate DOAS heating/cooling effect
        if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).AccountForDOAS) {
            // check for adequate number of supply nodes
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes >= 2) {
                SupplyAirNode1 = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(1);
                SupplyAirNode2 = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(2);
            } else if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes >= 1) {
                SupplyAirNode1 = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(1);
                SupplyAirNode2 = 0;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + ": to account for the effect a Dedicated Outside Air System on zone equipment sizing");
                ShowContinueError(state, "there must be at least one zone air inlet node");
                ShowFatalError(state, "Previous severe error causes abort ");
            }
            // set the DOAS mass flow rate and supply temperature and humidity ratio
            HR90H = PsyWFnTdbRhPb(state,
                                  state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASHighSetpoint,
                                  0.9,
                                  state.dataEnvrn->StdBaroPress);
            HR90L = PsyWFnTdbRhPb(state,
                                  state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASLowSetpoint,
                                  0.9,
                                  state.dataEnvrn->StdBaroPress);
            DOASMassFlowRate = state.dataSize->CalcFinalZoneSizing(ControlledZoneNum).MinOA;
            CalcDOASSupCondsForSizing(state,
                                      state.dataEnvrn->OutDryBulbTemp,
                                      state.dataEnvrn->OutHumRat,
                                      state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASControlStrategy,
                                      state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASLowSetpoint,
                                      state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASHighSetpoint,
                                      HR90H,
                                      HR90L,
                                      DOASSupplyTemp,
                                      DOASSupplyHumRat);
            DOASCpAir = PsyCpAirFnW(DOASSupplyHumRat);
            DOASSysOutputProvided = DOASMassFlowRate * DOASCpAir * (DOASSupplyTemp - Node(ZoneNode).Temp);
            TotDOASSysOutputProvided =
                DOASMassFlowRate * (PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat) - PsyHFnTdbW(Node(ZoneNode).Temp, Node(ZoneNode).HumRat));
            UpdateSystemOutputRequired(state, ActualZoneNum, DOASSysOutputProvided, LatOutputProvided);
            Node(SupplyAirNode1).Temp = DOASSupplyTemp;
            Node(SupplyAirNode1).HumRat = DOASSupplyHumRat;
            Node(SupplyAirNode1).MassFlowRate = DOASMassFlowRate;
            Node(SupplyAirNode1).Enthalpy = PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASHeatAdd = DOASSysOutputProvided;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASLatAdd =
                TotDOASSysOutputProvided - DOASSysOutputProvided;
            SupplyAirNode = SupplyAirNode2;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASSupMassFlow = DOASMassFlowRate;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASSupTemp = DOASSupplyTemp;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASSupHumRat = DOASSupplyHumRat;
            if (DOASSysOutputProvided > 0.0) {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASHeatLoad = DOASSysOutputProvided;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASCoolLoad = 0.0;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASTotCoolLoad = 0.0;
            } else {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASCoolLoad = DOASSysOutputProvided;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASTotCoolLoad = TotDOASSysOutputProvided;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).DOASHeatLoad = 0.0;
            }

        } else {
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes > 0) {
                SupplyAirNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(1);
            } else {
                SupplyAirNode = 0;
            }
        }

        // Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
        //                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
        if (!state.dataZoneEnergyDemand->DeadBandOrSetback(ActualZoneNum) &&
            std::abs(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired) > SmallLoad) {
            // Determine design supply air temperture and design supply air temperature difference
            if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired < 0.0) { // Cooling case
                // If the user specify the design cooling supply air temperature, then
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).ZnCoolDgnSAMethod == SupplyAirTemperature) {
                    Temp = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolDesTemp;
                    HumRat = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolDesHumRat;
                    DeltaTemp = Temp - Node(ZoneNode).Temp;
                    if (state.dataHeatBal->Zone(ActualZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                        DeltaTemp = Temp - state.dataHeatBal->Zone(ActualZoneNum).AdjustedReturnTempByITE;
                    }
                    // If the user specify the design cooling supply air temperature difference, then
                } else {
                    DeltaTemp = -std::abs(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolDesTempDiff);
                    Temp = DeltaTemp + Node(ZoneNode).Temp;
                    if (state.dataHeatBal->Zone(ActualZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                        Temp = DeltaTemp + state.dataHeatBal->Zone(ActualZoneNum).AdjustedReturnTempByITE;
                    }
                    HumRat = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolDesHumRat;
                }
            } else { // Heating Case
                // If the user specify the design heating supply air temperature, then
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).ZnHeatDgnSAMethod == SupplyAirTemperature) {
                    Temp = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatDesTemp;
                    HumRat = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatDesHumRat;
                    DeltaTemp = Temp - Node(ZoneNode).Temp;
                    // If the user specify the design heating supply air temperature difference, then
                } else {
                    DeltaTemp = std::abs(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatDesTempDiff);
                    Temp = DeltaTemp + Node(ZoneNode).Temp;
                    HumRat = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatDesHumRat;
                }
            }

            Enthalpy = PsyHFnTdbW(Temp, HumRat);
            SysOutputProvided = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired;
            CpAir = PsyCpAirFnW(HumRat);
            if (std::abs(DeltaTemp) > SmallTempDiff) {
                //!!PH/WFB/LKL (UCDV model)        MassFlowRate = SysOutputProvided / (CpAir*DeltaTemp)
                MassFlowRate = max(SysOutputProvided / (CpAir * DeltaTemp), 0.0);
            } else {
                MassFlowRate = 0.0;
            }

            if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlowRate *= state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).SupplyAirAdjustFactor;
            }
        } else {

            Temp = Node(ZoneNode).Temp;
            HumRat = Node(ZoneNode).HumRat;
            Enthalpy = Node(ZoneNode).Enthalpy;
            MassFlowRate = 0.0;
        }

        UpdateSystemOutputRequired(state, ActualZoneNum, SysOutputProvided, LatOutputProvided);

        if (SysOutputProvided > 0.0) {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatLoad = SysOutputProvided;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatMassFlow = MassFlowRate;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolLoad = 0.0;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolMassFlow = 0.0;
        } else if (SysOutputProvided < 0.0) {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolLoad = -SysOutputProvided;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolMassFlow = MassFlowRate;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatLoad = 0.0;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatMassFlow = 0.0;
        } else {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolLoad = 0.0;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolMassFlow = 0.0;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatLoad = 0.0;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatMassFlow = 0.0;
        }
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatZoneTemp = Node(ZoneNode).Temp;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatZoneHumRat = Node(ZoneNode).HumRat;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolZoneTemp = Node(ZoneNode).Temp;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolZoneHumRat = Node(ZoneNode).HumRat;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatOutTemp = state.dataEnvrn->OutDryBulbTemp;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatOutHumRat = state.dataEnvrn->OutHumRat;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolOutTemp = state.dataEnvrn->OutDryBulbTemp;
        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolOutHumRat = state.dataEnvrn->OutHumRat;

        if (SupplyAirNode > 0) {
            Node(SupplyAirNode).Temp = Temp;
            Node(SupplyAirNode).HumRat = HumRat;
            Node(SupplyAirNode).Enthalpy = Enthalpy;
            Node(SupplyAirNode).MassFlowRate = MassFlowRate;
        } else {
            state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) = SysOutputProvided;
        }
    }

    CalcZoneMassBalance(state, true);

    CalcZoneLeavingConditions(state, true);

    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        // MJW for now - use first return node, make a separate commit to add a dimension to all of the sizing rettemp variables
        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes > 0) {
            ReturnNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(1);
        } else {
            ReturnNode = 0;
        }
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
        ActualZoneNum = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).ActualZoneNum;
        if (ReturnNode > 0) {
            RetTemp = Node(ReturnNode).Temp;
        } else {
            RetTemp = Node(ZoneNode).Temp;
        }
        if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatLoad > 0.0) {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatZoneRetTemp = RetTemp;
            if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) > 0.0) {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatTstatTemp =
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
            } else {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatTstatTemp =
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
            }
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolTstatTemp =
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
        } else if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolLoad > 0.0) {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolZoneRetTemp = RetTemp;
            if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) > 0.0) {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolTstatTemp =
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
            } else {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolTstatTemp =
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
            }
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatTstatTemp =
                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
        } else {
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolZoneRetTemp = RetTemp;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).HeatTstatTemp =
                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum).CoolTstatTemp =
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
        }
    }
}

void CalcDOASSupCondsForSizing(EnergyPlusData &state,
                               Real64 OutDB,        // outside air temperature [C]
                               Real64 OutHR,        // outside humidity ratio [kg Water / kg Dry Air]
                               int DOASControl,     // dedicated outside air control strategy
                               Real64 DOASLowTemp,  // DOAS low setpoint [C]
                               Real64 DOASHighTemp, // DOAS high setpoint [C]
                               Real64 W90H, // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                               Real64 W90L, // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                               Real64 &DOASSupTemp, // DOAS supply temperature [C]
                               Real64 &DOASSupHR    // DOAS Supply Humidity ratio [kg Water / kg Dry Air]
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2015
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function calculates supply conditions for the direct outside air system
    // (DOAS) sizing calculations

    // METHODOLOGY EMPLOYED:
    // the supply temperature and humidity ratio are set depending on the design control method
    // and the outside air temperature

    // REFERENCES:
    // Consult the "DOAS Effect On Zone Sizing" new feature proposal and design documents

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcDOASSupCondsForSizing");

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    DOASSupTemp = 0.0;
    DOASSupHR = 0.0;
    // neutral supply air
    if (DOASControl == 1) {
        if (OutDB < DOASLowTemp) {
            DOASSupTemp = DOASLowTemp;
            DOASSupHR = OutHR;
        } else if (OutDB > DOASHighTemp) {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = min(OutHR, W90H);
        } else {
            DOASSupTemp = OutDB;
            DOASSupHR = OutHR;
        }
    }

    // neutral dehumidified supply air
    else if (DOASControl == 2) { //
        if (OutDB < DOASLowTemp) {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = OutHR;
        } else {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = min(OutHR, W90L);
        }
    }

    // cold supply air
    else if (DOASControl == 3) {
        if (OutDB < DOASLowTemp) {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = OutHR;
        } else {
            DOASSupTemp = DOASLowTemp;
            DOASSupHR = min(OutHR, W90L);
        }
    } else {
        ShowFatalError(state, std::string{RoutineName} + ":illegal DOAS design control strategy");
    }
}

void SetUpZoneSizingArrays(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Allocate and fill the ZoneSizing data array.

    // METHODOLOGY EMPLOYED:
    // Obtains data from Zone Sizing and Zone Equipment objects already input.

    // Using/Aliasing
    using EMSManager::ManageEMS;
    using ScheduleManager::GetScheduleMaxValue;
    using ZoneTempPredictorCorrector::VerifyThermostatInZone;

    // Locals
    int NumOfTimeStepInDay; // number of zone time steps in a day

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int DesDayNum; // design day index
    // unused  INTEGER :: DesDayEnvrnNum   ! design day index
    int CtrlZoneNum;          // controlled zone index
    int ZoneSizNum;           // zone sizing input index
    Real64 TotPeopleInZone;   // total (maximum) number of people in a zone
    int PeopleNum;            // index of People structure
    Real64 OAFromPeople(0.0); // min OA calculated from zone occupancy [m3/s]
    Real64 OAFromArea(0.0);   // min OA calculated from zone area and OA flow per area [m3/s]
    int ZoneIndex;            // index of Zone Sizing zone name in zone array
    int ZoneSizIndex;         // zone sizing do loop index
    bool ErrorsFound(false);  // Set to true if errors in input, fatal at end of routine
    Real64 SchMax(0.0);       // maximum people multiplier value
    Real64 OAVolumeFlowRate;  // outside air flow rate (m3/s)
    bool UseOccSchFlag;       // flag to use occupancy schedule when calculating OA
    bool UseMinOASchFlag;     // flag to use min OA schedule when calculating OA

    // TODO MJW: Punt for now, sometimes unit test will get here and need these to be allocated, but simulations need them sooner
    if (!state.dataHeatBal->ZoneIntGain.allocated()) {
        state.dataHeatBal->ZoneIntGain.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->spaceIntGain.allocate(state.dataGlobal->numSpaces);
        state.dataHeatBal->spaceIntGainDevices.allocate(state.dataGlobal->numSpaces);
        state.dataDaylightingData->spacePowerReductionFactor.dimension(state.dataGlobal->numSpaces, 1.0);
    }

    for (ZoneSizIndex = 1; ZoneSizIndex <= state.dataSize->NumZoneSizingInput; ++ZoneSizIndex) {
        ZoneIndex = UtilityRoutines::FindItemInList(state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName, state.dataHeatBal->Zone);
        if (ZoneIndex == 0) {
            ShowSevereError(state,
                            "SetUpZoneSizingArrays: Sizing:Zone=\"" + state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName +
                                "\" references unknown zone");
            ErrorsFound = true;
        }
        if (std::any_of(state.dataZoneEquip->ZoneEquipConfig.begin(), state.dataZoneEquip->ZoneEquipConfig.end(), [](EquipConfiguration const &e) {
                return e.IsControlled;
            })) {
            ZoneIndex = UtilityRoutines::FindItemInList(
                state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
            if (ZoneIndex == 0) {
                if (!state.dataGlobal->isPulseZoneSizing) {
                    ShowWarningError(state,
                                     "SetUpZoneSizingArrays: Requested Sizing for Zone=\"" + state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName +
                                         "\", Zone is not found in the Controlled Zones List");
                }
            } else {
                state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneNum = ZoneIndex;
            }
            if (state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolAirDesMethod == FromDDCalc ||
                state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatAirDesMethod == FromDDCalc) {
                if (!VerifyThermostatInZone(state, state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName)) {
                    if (!state.dataGlobal->isPulseZoneSizing) {
                        ShowWarningError(state,
                                         "SetUpZoneSizingArrays: Requested Sizing for Zone=\"" +
                                             state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName +
                                             "\", Zone has no thermostat (ref: ZoneControl:Thermostat, et al)");
                    }
                }
            }
        } else {
            ShowSevereError(state, "SetUpZoneSizingArrays: Zone Sizing is requested but there are no ZoneHVAC:EquipmentConnections statements.");
            ErrorsFound = true;
        }
    }

    // Put Auto Sizing of Sizing:Zone inputs here!
    AutoCalcDOASControlStrategy(state);

    state.dataSize->ZoneSizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays, state.dataGlobal->NumOfZones);
    state.dataSize->FinalZoneSizing.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->CalcZoneSizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays, state.dataGlobal->NumOfZones);
    state.dataSize->CalcFinalZoneSizing.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->TermUnitFinalZoneSizing.allocate(state.dataSize->NumAirTerminalUnits);
    state.dataSize->DesDayWeath.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
    NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;
    state.dataZoneEquipmentManager->AvgData.allocate(NumOfTimeStepInDay);
    state.dataSize->CoolPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->HeatPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->ZoneSizThermSetPtHi.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->ZoneSizThermSetPtLo.allocate(state.dataGlobal->NumOfZones);

    state.dataSize->CoolPeakDateHrMin = "";
    state.dataSize->HeatPeakDateHrMin = "";

    state.dataSize->ZoneSizThermSetPtHi = 0.0;
    state.dataSize->ZoneSizThermSetPtLo = 1000.0;

    for (DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
        state.dataSize->DesDayWeath(DesDayNum).Temp.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        state.dataSize->DesDayWeath(DesDayNum).HumRat.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        state.dataSize->DesDayWeath(DesDayNum).Press.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        state.dataSize->DesDayWeath(DesDayNum).Temp = 0.0;
        state.dataSize->DesDayWeath(DesDayNum).HumRat = 0.0;
        state.dataSize->DesDayWeath(DesDayNum).Press = 0.0;
    }
    // Fill zone sizing arrays from input array
    for (DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ZoneName = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ZoneName = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
            // For each Zone Sizing object, find the corresponding controlled zone
            ZoneSizNum = UtilityRoutines::FindItemInList(
                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
            if (ZoneSizNum > 0) { // move data from zone sizing input
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnCoolDgnSAMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnHeatDgnSAMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTemp;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTemp;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTempDiff;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTempDiff;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesHumRat;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesHumRat;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolAirDesMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatAirDesMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowPerArea;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowFrac =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowFrac;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowPerArea;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowFrac =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowFrac;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatSizingFactor;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolSizingFactor;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(ZoneSizNum).AccountForDOAS;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASControlStrategy =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DOASControlStrategy;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASLowSetpoint;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASHighSetpoint;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ZnCoolDgnSAMethod =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).ZnCoolDgnSAMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ZnHeatDgnSAMethod =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).ZnHeatDgnSAMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTemp;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTemp;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTempDiff;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTempDiff;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesHumRat;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesHumRat;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolAirDesMethod =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).CoolAirDesMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatAirDesMethod =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).HeatAirDesMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowPerArea;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowFrac =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowFrac;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowPerArea;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowFrac =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowFrac;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatSizingFactor =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).HeatSizingFactor;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolSizingFactor =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).CoolSizingFactor;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(ZoneSizNum).AccountForDOAS;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASControlStrategy =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DOASControlStrategy;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASLowSetpoint;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASHighSetpoint =
                    state.dataSize->ZoneSizingInput(ZoneSizNum).DOASHighSetpoint;
            } else { // Every controlled zone must be simulated, so set missing inputs to the first
                // LKL I think this is sufficient for warning -- no need for array
                if (DesDayNum == 1) {
                    if (!state.dataGlobal->isPulseZoneSizing) {
                        ShowWarningError(state,
                                         "SetUpZoneSizingArrays: Sizing for Zone=\"" + state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName +
                                             "\" will use Sizing:Zone specifications listed for Zone=\"" +
                                             state.dataSize->ZoneSizingInput(1).ZoneName + "\".");
                    }
                    // Following needs to be implemented first:
                    //          CALL ShowContinueError(state, '  A better option would be to set up global ZoneList objects for Sizing:Zone objects.')
                }
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnCoolDgnSAMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnHeatDgnSAMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(1).CoolDesTemp;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(1).HeatDesTemp;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(1).CoolDesTempDiff;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(1).HeatDesTempDiff;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(1).CoolDesHumRat;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(1).HeatDesHumRat;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(1).CoolAirDesMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(1).HeatAirDesMethod;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowPerArea;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowFrac;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowPerArea;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlow;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowFrac;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(1).HeatSizingFactor;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(1).CoolSizingFactor;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(1).AccountForDOAS;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(1).DOASControlStrategy;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(1).DOASLowSetpoint;
                state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(1).DOASHighSetpoint;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnCoolDgnSAMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnHeatDgnSAMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(1).CoolDesTemp;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(1).HeatDesTemp;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(1).CoolDesTempDiff;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(1).HeatDesTempDiff;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(1).CoolDesHumRat;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(1).HeatDesHumRat;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(1).CoolAirDesMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(1).HeatAirDesMethod;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowPerArea;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlowFrac =
                    state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowFrac;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                    state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowPerArea;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlow;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlowFrac =
                    state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowFrac;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(1).HeatSizingFactor;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(1).CoolSizingFactor;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(1).AccountForDOAS;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(1).DOASControlStrategy;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(1).DOASLowSetpoint;
                state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(1).DOASHighSetpoint;
            }
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).allocateMemberArrays(NumOfTimeStepInDay);
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).allocateMemberArrays(NumOfTimeStepInDay);
        }
    }

    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
        ZoneSizNum = UtilityRoutines::FindItemInList(
            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
        if (ZoneSizNum > 0) { // move data from zone sizing input
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnCoolDgnSAMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnHeatDgnSAMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTemp;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTemp;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTempDiff;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTempDiff;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesHumRat;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesHumRat;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneAirDistributionIndex =
                state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneAirDistributionIndex;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneDesignSpecOAIndex = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneDesignSpecOAIndex;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolAirDesMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatAirDesMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowPerArea;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowFrac = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowFrac;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowPerArea;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowFrac;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(ZoneSizNum).AccountForDOAS;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASControlStrategy;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASLowSetpoint;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASHighSetpoint;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffCooling = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneADEffCooling;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffHeating = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneADEffHeating;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSecondaryRecirculation =
                state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneSecondaryRecirculation;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneVentilationEff = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneVentilationEff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnCoolDgnSAMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).ZnHeatDgnSAMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTemp;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTemp;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesTempDiff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesTempDiff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolDesHumRat;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatDesHumRat;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneAirDistributionIndex =
                state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneAirDistributionIndex;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneDesignSpecOAIndex =
                state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneDesignSpecOAIndex;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolAirDesMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatAirDesMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowPerArea;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowFrac =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesCoolMinAirFlowFrac;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowPerArea;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac =
                state.dataSize->ZoneSizingInput(ZoneSizNum).DesHeatMaxAirFlowFrac;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).HeatSizingFactor;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(ZoneSizNum).CoolSizingFactor;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(ZoneSizNum).AccountForDOAS;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASControlStrategy;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASLowSetpoint;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(ZoneSizNum).DOASHighSetpoint;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneADEffCooling = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneADEffCooling;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneADEffHeating = state.dataSize->ZoneSizingInput(ZoneSizNum).ZoneADEffHeating;
        } else { // Every controlled zone must be simulated, so set missing inputs to the first
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnCoolDgnSAMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnHeatDgnSAMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(1).CoolDesTemp;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(1).HeatDesTemp;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(1).CoolDesTempDiff;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(1).HeatDesTempDiff;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(1).CoolDesHumRat;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(1).HeatDesHumRat;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneAirDistributionIndex = state.dataSize->ZoneSizingInput(1).ZoneAirDistributionIndex;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneDesignSpecOAIndex = state.dataSize->ZoneSizingInput(1).ZoneDesignSpecOAIndex;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(1).CoolAirDesMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(1).HeatAirDesMethod;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowPerArea;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowFrac;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowPerArea;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowFrac;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(1).HeatSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(1).CoolSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(1).AccountForDOAS;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(1).DOASControlStrategy;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(1).DOASLowSetpoint;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(1).DOASHighSetpoint;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffCooling = state.dataSize->ZoneSizingInput(1).ZoneADEffCooling;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffHeating = state.dataSize->ZoneSizingInput(1).ZoneADEffHeating;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSecondaryRecirculation = state.dataSize->ZoneSizingInput(1).ZoneSecondaryRecirculation;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneVentilationEff = state.dataSize->ZoneSizingInput(1).ZoneVentilationEff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZnCoolDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnCoolDgnSAMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZnHeatDgnSAMethod = state.dataSize->ZoneSizingInput(1).ZnHeatDgnSAMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesTemp = state.dataSize->ZoneSizingInput(1).CoolDesTemp;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesTemp = state.dataSize->ZoneSizingInput(1).HeatDesTemp;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesTempDiff = state.dataSize->ZoneSizingInput(1).CoolDesTempDiff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesTempDiff = state.dataSize->ZoneSizingInput(1).HeatDesTempDiff;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesHumRat = state.dataSize->ZoneSizingInput(1).CoolDesHumRat;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesHumRat = state.dataSize->ZoneSizingInput(1).HeatDesHumRat;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneAirDistributionIndex = state.dataSize->ZoneSizingInput(1).ZoneAirDistributionIndex;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneDesignSpecOAIndex = state.dataSize->ZoneSizingInput(1).ZoneDesignSpecOAIndex;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolAirDesMethod = state.dataSize->ZoneSizingInput(1).CoolAirDesMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatAirDesMethod = state.dataSize->ZoneSizingInput(1).HeatAirDesMethod;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowPerArea;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesCoolMinAirFlowFrac;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowPerArea;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlow;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac = state.dataSize->ZoneSizingInput(1).DesHeatMaxAirFlowFrac;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatSizingFactor = state.dataSize->ZoneSizingInput(1).HeatSizingFactor;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolSizingFactor = state.dataSize->ZoneSizingInput(1).CoolSizingFactor;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).AccountForDOAS = state.dataSize->ZoneSizingInput(1).AccountForDOAS;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASControlStrategy = state.dataSize->ZoneSizingInput(1).DOASControlStrategy;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASLowSetpoint = state.dataSize->ZoneSizingInput(1).DOASLowSetpoint;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DOASHighSetpoint = state.dataSize->ZoneSizingInput(1).DOASHighSetpoint;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneADEffCooling = state.dataSize->ZoneSizingInput(1).ZoneADEffCooling;
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneADEffHeating = state.dataSize->ZoneSizingInput(1).ZoneADEffHeating;
        }
        state.dataSize->FinalZoneSizing(CtrlZoneNum).allocateMemberArrays(NumOfTimeStepInDay);
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).allocateMemberArrays(NumOfTimeStepInDay);

        // setup CalcFinalZoneSizing structure for use with EMS, some as sensors, some as actuators
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {

            // actuate  REAL(r64)             :: DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Heating Air Mass Flow Rate",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/s]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Heating Air Mass Flow Rate",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/s]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Heating Air Mass Flow Rate",
                             "[kg/s]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatMassOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatMassFlow);

            // actuate  REAL(r64)             :: DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Cooling Air Mass Flow Rate",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/s]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Cooling Air Mass Flow Rate",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/s]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Cooling Air Mass Flow Rate",
                             "[kg/s]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolMassOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolMassFlow);

            // actuate  REAL(r64)             :: DesHeatLoad              = 0.0d0   ! zone design heating load [W]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Heating Load",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[W]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Heating Load",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[W]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Heating Load",
                             "[W]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatLoadOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatLoad);

            // actuate  REAL(r64)             :: DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Cooling Load",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[W]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Cooling Load",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[W]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Cooling Load",
                             "[W]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolLoadOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolLoad);

            // sensor?  REAL(r64)             :: DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Heating Air Density",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/m3]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatDens);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Heating Air Density",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/m3]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatDens);
            // sensor?  REAL(r64)             :: DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Cooling Air Density",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/m3]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolDens);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Cooling Air Density",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[kg/m3]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolDens);

            // actuate  REAL(r64)             :: DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Heating Volume Flow",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[m3/s]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Heating Volume Flow",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[m3/s]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Heating Vol Flow",
                             "[m3/s]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatVolOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatVolFlow);

            // actuate  REAL(r64)             :: DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
            SetupEMSInternalVariable(state,
                                     "Final Zone Design Cooling Volume Flow",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[m3/s]",
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Cooling Volume Flow",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[m3/s]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                             "Zone Design Cooling Vol Flow",
                             "[m3/s]",
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolVolOn,
                             state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolVolFlow);

            // actuate  REAL(r64)          :: DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
            // actuate  REAL(r64)          :: DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]

            SetupEMSInternalVariable(state,
                                     "Zone Outdoor Air Design Volume Flow Rate",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName,
                                     "[m3/s]",
                                     state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).MinOA);
        }
    }

    // Populate DesignSpecification:OutdoorAir:SpaceList spaces
    bool dsoaError = false;
    for (int oaIndex = 1; oaIndex <= state.dataSize->NumOARequirements; ++oaIndex) {
        auto &thisOAReq = state.dataSize->OARequirements(oaIndex);
        // If this is a DesignSpecification:OutdoorAir:SpaceList check to make sure spaces are valid and belong to this zone
        if (thisOAReq.numDSOA > 0) {
            for (int spaceCounter = 1; spaceCounter <= thisOAReq.numDSOA; ++spaceCounter) {
                std::string thisSpaceName = thisOAReq.dsoaSpaceNames(spaceCounter);
                int thisSpaceNum = UtilityRoutines::FindItemInList(thisSpaceName, state.dataHeatBal->space);
                if (thisSpaceNum > 0) {
                    thisOAReq.dsoaSpaceIndexes.emplace_back(thisSpaceNum);
                } else {
                    ShowSevereError(state, "SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList=" + thisOAReq.Name);
                    ShowContinueError(state, "Space Name=" + thisSpaceName + " not found.");
                    dsoaError = true;
                    ErrorsFound = true;
                }
                // Check for duplicate spaces
                for (int loop = 1; loop <= int(thisOAReq.dsoaSpaceIndexes.size()) - 1; ++loop) {
                    if (thisSpaceNum == thisOAReq.dsoaSpaceIndexes(loop)) {
                        ShowSevereError(state, "SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList=" + thisOAReq.Name);
                        ShowContinueError(state, "Space Name=" + thisSpaceName + " appears more than once in the list.");
                        dsoaError = true;
                        ErrorsFound = true;
                    }
                }
            }
        }
    }

    // Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
    // Calculate the zone design minimum outside air flow rate from the 3 Zone Sizing OA inputs and
    // from the specified OA method
    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        // Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
        // from the outside air flow per person input
        TotPeopleInZone = 0.0;
        Real64 ZoneMinOccupancy = 0.;
        ZoneIndex = state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum;
        int DSOAPtr = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneDesignSpecOAIndex; // index to DesignSpecification:OutdoorAir object
        if ((DSOAPtr > 0) && !dsoaError) {
            auto &thisOAReq = state.dataSize->OARequirements(DSOAPtr);
            // If this is a DesignSpecification:OutdoorAir:SpaceList check to make sure spaces are valid and belong to this zone
            if (thisOAReq.numDSOA > 0) {
                for (int spaceCounter = 1; spaceCounter <= thisOAReq.numDSOA; ++spaceCounter) {
                    std::string thisSpaceName = thisOAReq.dsoaSpaceNames(spaceCounter);
                    int thisSpaceNum = thisOAReq.dsoaSpaceIndexes(spaceCounter);
                    if (thisSpaceNum > 0) {
                        if (state.dataHeatBal->space(thisSpaceNum).zoneNum != state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum) {
                            ShowSevereError(state, "SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList=" + thisOAReq.Name);
                            ShowContinueError(state, "is invalid for Sizing:Zone=" + state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName);
                            ShowContinueError(state, "All spaces in the list must be part of this zone.");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesOAFlowPPer =
                state.dataSize->OARequirements(DSOAPtr).desFlowPerZonePerson(state, ZoneIndex);
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesOAFlowPerArea =
                state.dataSize->OARequirements(DSOAPtr).desFlowPerZoneArea(state, ZoneIndex);
        }

        for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
            if (state.dataHeatBal->People(PeopleNum).ZonePtr == state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum) {
                TotPeopleInZone += (state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                                    state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).Multiplier *
                                    state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).ListMultiplier);
                SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(PeopleNum).NumberOfPeoplePtr);
                if (SchMax > 0) {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).ZonePeakOccupancy = TotPeopleInZone * SchMax;
                } else {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).ZonePeakOccupancy = TotPeopleInZone;
                }
                ZoneMinOccupancy +=
                    TotPeopleInZone * ScheduleManager::GetScheduleMinValue(state, state.dataHeatBal->People(PeopleNum).NumberOfPeoplePtr);
            }
        }
        state.dataSize->FinalZoneSizing(CtrlZoneNum).TotalZoneFloorArea =
            (state.dataHeatBal->Zone(ZoneIndex).FloorArea *
             state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).Multiplier *
             state.dataHeatBal->Zone(state.dataSize->FinalZoneSizing(CtrlZoneNum).ActualZoneNum).ListMultiplier);
        OAFromPeople = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesOAFlowPPer * TotPeopleInZone;
        OAFromArea = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesOAFlowPerArea * state.dataSize->FinalZoneSizing(CtrlZoneNum).TotalZoneFloorArea;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).TotPeopleInZone = TotPeopleInZone;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).TotalOAFromPeople = OAFromPeople;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).TotalOAFromArea = OAFromArea;

        // save Voz for predefined outdoor air summary report
        Real64 MinEz =
            std::min(state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffCooling, state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffHeating);
        if (MinEz == 0) {
            MinEz = 1.0; // if not calculated assume 1.0 ventilation effectiveness
        }
        state.dataHeatBal->ZonePreDefRep(ZoneIndex).VozMin =
            (ZoneMinOccupancy * state.dataSize->FinalZoneSizing(CtrlZoneNum).DesOAFlowPPer + OAFromArea) / MinEz;

        // Calculate the design min OA flow rate for this zone
        UseOccSchFlag = false;
        UseMinOASchFlag = false;
        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneDesignSpecOAIndex = DSOAPtr; // store for later use
        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneAirDistributionIndex =
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneAirDistributionIndex; // store for later use
        if (!dsoaError) {
            OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(state, DSOAPtr, ZoneIndex, UseOccSchFlag, UseMinOASchFlag);
        } else {
            OAVolumeFlowRate = 0.0;
        }

        // Zone(ZoneIndex)%Multiplier and Zone(ZoneIndex)%ListMultiplier applied in CalcDesignSpecificationOutdoorAir
        state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA = OAVolumeFlowRate;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).MinOA = OAVolumeFlowRate;
        if (state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffCooling > 0.0 ||
            state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffHeating > 0.0) {
            state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA /=
                min(state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffCooling, state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneADEffHeating);
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).MinOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA;
        }
        // calculated zone design flow rates automatically take into account zone multipliers, since the zone
        // loads are multiplied (in ZoneTempPredictorCorrector.cc). Flow rates derived directly from
        // user inputs need to be explicitly multiplied by the zone multipliers.
        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2 =
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea * state.dataHeatBal->Zone(ZoneIndex).FloorArea *
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2 =
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowPerArea * state.dataHeatBal->Zone(ZoneIndex).FloorArea *
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2 =
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea * state.dataHeatBal->Zone(ZoneIndex).FloorArea *
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2 =
            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowPerArea * state.dataHeatBal->Zone(ZoneIndex).FloorArea *
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;
        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow *=
            state.dataHeatBal->Zone(ZoneIndex).Multiplier * state.dataHeatBal->Zone(ZoneIndex).ListMultiplier;

        for (DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).MinOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).MinOA = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).MinOA;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow2 = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow2 =
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow =
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow2 = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow2 =
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2;
            state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow =
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow;
        }
    }
    // Formats
    print(state.files.eio, "! <Load Timesteps in Zone Design Calculation Averaging Window>, Value\n");
    static constexpr std::string_view Format_891(" Load Timesteps in Zone Design Calculation Averaging Window, {:4}\n");
    print(state.files.eio, Format_891, state.dataSize->NumTimeStepsInAvg);
    print(state.files.eio, "! <Heating Sizing Factor Information>, Sizing Factor ID, Value\n");
    static constexpr std::string_view Format_991(" Heating Sizing Factor Information, Global, {:12.5N}\n");
    print(state.files.eio, Format_991, state.dataSize->GlobalHeatSizingFactor);
    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        if (state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor != 1.0) {
            static constexpr std::string_view Format_992(" Heating Sizing Factor Information, Zone {}, {:12.5N}\n");
            print(state.files.eio,
                  Format_992,
                  state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                  state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor);
        }
    }
    print(state.files.eio, "! <Cooling Sizing Factor Information>, Sizing Factor ID, Value\n");
    static constexpr std::string_view Format_994(" Cooling Sizing Factor Information, Global, {:12.5N}\n");
    print(state.files.eio, Format_994, state.dataSize->GlobalCoolSizingFactor);
    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        if (state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor != 1.0) {
            static constexpr std::string_view Format_995(" Cooling Sizing Factor Information, Zone {}, {:12.5N}\n");
            print(state.files.eio,
                  Format_995,
                  state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName,
                  state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor);
        }
    }
    if (ErrorsFound) {
        ShowFatalError(state, "SetUpZoneSizingArrays: Errors found in Sizing:Zone input");
    }
}

void RezeroZoneSizingArrays(EnergyPlusData &state)
{
    // Zero zone sizing arrays between the pulse and normal sizing.
    DisplayString(state, "Re-zeroing zone sizing arrays");

    for (int ctrlZoneNum = 1; ctrlZoneNum <= state.dataGlobal->NumOfZones; ++ctrlZoneNum) {
        for (int desDayNum = 1; desDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++desDayNum) {
            state.dataSize->ZoneSizing(desDayNum, ctrlZoneNum).zeroMemberData();
            state.dataSize->CalcZoneSizing(desDayNum, ctrlZoneNum).zeroMemberData();
        }
        state.dataSize->CalcFinalZoneSizing(ctrlZoneNum).zeroMemberData();
        state.dataSize->FinalZoneSizing(ctrlZoneNum).zeroMemberData();
    }
}

void UpdateZoneSizing(EnergyPlusData &state, DataGlobalConstants::CallIndicator const CallIndicator)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update the result variables of the zone sizing calculation

    // METHODOLOGY EMPLOYED:
    // CallIndicator = 1 (BeginDay) zero the result arrays
    // CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
    // CallIndicator = 3 (EndDay) calculate daily maxima
    // CallIndicator = 4 (EndZoneSizingCalc) write out results

    // Using/Aliasing
    auto &FracTimeStepZone = state.dataHVACGlobal->FracTimeStepZone;
    using DataHVACGlobals::SmallMassFlow;
    using DataHVACGlobals::SmallTempDiff;
    using EMSManager::ManageEMS;
    using General::MovingAvg;

    // SUBROUTINE PARAMETER DEFINITIONS:

    static constexpr std::string_view RoutineName("UpdateZoneSizing");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int DesDayNum;         // design day index
    int TimeStepIndex;     // zone time step index
    int CtrlZoneNum;       // controlled zone index
    int TimeStepInDay;     // zone time step in day
    int I;                 // write statement index
    int HourCounter;       // Hour Counter
    int TimeStepCounter;   // Time Step Counter
    int Minutes;           // Current Minutes Counter
    int HourPrint;         // Hour to print (timestamp)
    Real64 OAFrac;         // outside air fraction
    int TimeStepAtPeak;    // time step number at heat or cool peak
    int TimeStepAtPeakF;   // time step number at heat or cool peak (final)
    int DDNum;             // Design Day index
    int DDNumF;            // Design Day index (final)
    Real64 TotCoolSizMult; // combines user cooling design flow input with zone sizing multiplier
    Real64 TotHeatSizMult; // combines user heating design flow input with zone sizing multiplier
    Real64 MinOAMass;      // zone minimum outside air mass flow rate kg/s
    Real64 MaxHeatVolFlow; // max of user specified design heating max flow [m3/s]
    Real64 SupplyTemp;     // supply air temperature [C]
    Real64 DeltaTemp;      // supply air delta temperature [deltaC]

    switch (CallIndicator) {
    case DataGlobalConstants::CallIndicator::BeginDay: {
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {

            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;

            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDesDay = state.dataEnvrn->EnvironmentName;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDesDay = state.dataEnvrn->EnvironmentName;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatDens = state.dataEnvrn->StdRhoAir;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolDens = state.dataEnvrn->StdRhoAir;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDDNum = state.dataSize->CurOverallSimDay;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDDNum = state.dataSize->CurOverallSimDay;
        }
    } break;
    case DataGlobalConstants::CallIndicator::DuringDay: {
        TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;

        // save the results of the ideal zone component calculation in the CalcZoneSizing sequence variables
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            if (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum) > 0.0 &&
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum) > state.dataSize->ZoneSizThermSetPtHi(CtrlZoneNum)) {
                state.dataSize->ZoneSizThermSetPtHi(CtrlZoneNum) = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum);
            }
            if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum) > 0.0 &&
                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum) < state.dataSize->ZoneSizThermSetPtLo(CtrlZoneNum)) {
                state.dataSize->ZoneSizThermSetPtLo(CtrlZoneNum) = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum);
            }
            state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatSetPtSeq(TimeStepInDay) =
                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum);
            state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatTstatTempSeq(TimeStepInDay) =
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatTstatTemp;
            state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolSetPtSeq(TimeStepInDay) =
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum);
            state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolTstatTempSeq(TimeStepInDay) =
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolTstatTemp;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatMassFlow * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoad * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRat * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRat * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolMassFlow * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoad * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRat * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRat * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatLoadSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatLoad * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASCoolLoadSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASCoolLoad * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatAddSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatAdd * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASLatAddSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASLatAdd * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupMassFlowSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupMassFlow * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupTempSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupTemp * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupHumRatSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASSupHumRat * FracTimeStepZone;
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASTotCoolLoadSeq(TimeStepInDay) +=
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASTotCoolLoad * FracTimeStepZone;
        }
    } break;
    case DataGlobalConstants::CallIndicator::EndDay: {
        // average some of the zone sequences to reduce peakiness
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolFlowSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolFlowSeq = state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq = state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatFlowSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatFlowSeq = state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq = state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq =
                state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq =
                state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatAddSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASHeatAddSeq = state.dataZoneEquipmentManager->AvgData;
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            state.dataZoneEquipmentManager->AvgData = 0.0;
            MovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASLatAddSeq,
                      state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                      state.dataSize->NumTimeStepsInAvg,
                      state.dataZoneEquipmentManager->AvgData);
            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DOASLatAddSeq = state.dataZoneEquipmentManager->AvgData;
        }

        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {

            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // save the sequence values at the heating peak
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepIndex) >
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatLoad) {
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatLoad =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax = TimeStepIndex;
                }
            }
            if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow > 0.0) {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatVolFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow /
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatDens;
                OAFrac = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).MinOA /
                         max(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatVolFlow, SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInTemp =
                    OAFrac * state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).Temp(TimeStepAtPeak) +
                    (1.0 - OAFrac) * state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInHumRat =
                    OAFrac * state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).HumRat(TimeStepAtPeak) +
                    (1.0 - OAFrac) * state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak;
            }
            // save the sequence values at the cooling peak
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepIndex) >
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolLoad) {
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolLoad =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex);
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax = TimeStepIndex;
                }
            }
            if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow > 0.0) {
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow /
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolDens;
                OAFrac = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).MinOA /
                         max(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow, SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInTemp =
                    OAFrac * state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).Temp(TimeStepAtPeak) +
                    (1.0 - OAFrac) * state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
                state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInHumRat =
                    OAFrac * state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).HumRat(TimeStepAtPeak) +
                    (1.0 - OAFrac) * state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak;
            }
            // from all the design periods, choose the one needing the most heating and save all its design variables in CalcFinalZoneSizing
            if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatVolFlow >
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) {
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatVolFlow;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatLoad;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesDay =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDesDay;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatDens =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatDens;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatFlowSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatFlowSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatLoadSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneRetTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneHumRatSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutHumRatSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtHeatPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtHeatPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtHeatPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtHeatPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDDNum;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cHeatDDDate =
                    state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInTemp =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInTemp;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInHumRat =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInHumRat;
            } else {
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatDens = state.dataEnvrn->StdRhoAir;
                // save design heating load when the there is design heating load and the design heating volume flow rate is zero, i.e., when
                // design heating volume flow rate is set to zero due to heating supply air temp less than zone thermostat temperature
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatLoad >
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatLoad;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesDay =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDesDay;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatLoadSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatLoadSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneRetTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDDNum;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cHeatDDDate =
                        state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInTemp;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInHumRat =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInHumRat;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatTstatTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatTstatTemp;
                }
            }
            // from all the design periods, choose the one needing the most Cooling and save all its design variables in CalcFinalZoneSizing
            if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow >
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow) {
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolLoad;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDesDay;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolDens =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolDens;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolFlowSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolFlowSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolLoadSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneRetTempSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneHumRatSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutHumRatSeq =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtCoolPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtCoolPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtCoolPeak =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDDNum;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cCoolDDDate =
                    state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInTemp =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInTemp;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInHumRat =
                    state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInHumRat;
            } else {
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolDens = state.dataEnvrn->StdRhoAir;
                // save design cooling load when the there is design cooling load and the design cooling volume flow rate is zero, i.e., when
                // design cooling volume flow rate is set to zero due to cooling supply air temp greater than zone thermostat temperature
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolLoad >
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolLoad;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDesDay;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolLoadSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolLoadSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneRetTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDDNum;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cCoolDDDate =
                        state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInTemp;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInHumRat =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInHumRat;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolTstatTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolTstatTemp;
                }
            }
            // save heat peak conditions when there is no design heating load or design heating volume flow rate, i.e., when
            // zone temperature is always greater than the zone heating thermostat temperature
            if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if ((state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex) <
                         state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak) ||
                        FirstIteration) {
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtHeatPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax = TimeStepIndex;
                        FirstIteration = false;
                    }
                }
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak <=
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtHeatPeak) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesDay =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDesDay;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneRetTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatOutHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtHeatPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtHeatPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatDDNum;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cHeatDDDate =
                        state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtHeatMax;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInTemp;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatCoilInHumRat =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesHeatCoilInHumRat;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatTstatTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).HeatTstatTemp;
                    FirstIteration = false;
                }
            }
            // save cool peak conditions when there is no design cooling load or design cooling volume flow rate, i.e., when
            // zone temperature is always less than the zone cooling thermostat temperature
            if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if ((state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex) >
                         state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak) ||
                        FirstIteration) {
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak =
                            state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex);
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax = TimeStepIndex;
                        FirstIteration = false;
                    }
                }
                if (state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak >
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtCoolPeak) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDesDay;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneRetTempSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutHumRatSeq =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).OutHumRatAtCoolPeak =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolDDNum;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cCoolDDDate =
                        state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay).DateString;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInTemp;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolCoilInHumRat =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolCoilInHumRat;
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolTstatTemp =
                        state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).CoolTstatTemp;
                }
            }
        }
    } break;
    case DataGlobalConstants::CallIndicator::EndZoneSizingCalc: {
        // candidate EMS calling point to customize CalcFinalZoneSizing
        bool anyEMSRan;
        ManageEMS(state, EMSManager::EMSCallFrom::ZoneSizing, anyEMSRan, ObjexxFCL::Optional_int_const());

        // now apply EMS overrides (if any)

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatMassOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatMassFlow;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolMassOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolMassFlow;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatLoadOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatLoad;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolLoadOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolLoad;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatVolOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatVolFlow;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesCoolVolOn) {
                    if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow > 0.0)
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow =
                            state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesCoolVolFlow;
                }
            }
        }

        if (!state.dataGlobal->isPulseZoneSizing) {

            for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                if (std::abs(state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad) <= 1.e-8) {
                    ShowWarningError(
                        state, "Calculated design cooling load for zone=" + state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName + " is zero.");
                    ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
                }
                if (std::abs(state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad) <= 1.e-8) {
                    ShowWarningError(
                        state, "Calculated design heating load for zone=" + state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneName + " is zero.");
                    ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
                }
            }

            print(state.files.zsz, "Time");
            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;

                static constexpr std::string_view ZSizeFmt11("{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}");
                print(state.files.zsz,
                      ZSizeFmt11,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).ZoneName,
                      state.dataSize->CalcFinalZoneSizing(I).HeatDesDay,
                      ":Des Heat Load [W]",
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).ZoneName,
                      state.dataSize->CalcFinalZoneSizing(I).CoolDesDay,
                      ":Des Sens Cool Load [W]",
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).ZoneName,
                      state.dataSize->CalcFinalZoneSizing(I).HeatDesDay,
                      ":Des Heat Mass Flow [kg/s]",
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).ZoneName,
                      state.dataSize->CalcFinalZoneSizing(I).CoolDesDay,
                      ":Des Cool Mass Flow [kg/s]");

                // Should this be done only if there is a cooling load? Or would this message help determine why there was no load?
                if (std::abs(state.dataSize->CalcFinalZoneSizing(I).DesCoolLoad) > 1.e-8) {
                    // check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
                    if (state.dataSize->CalcFinalZoneSizing(I).ZnCoolDgnSAMethod == SupplyAirTemperature) {
                        SupplyTemp = state.dataSize->CalcFinalZoneSizing(I).CoolDesTemp;
                        DeltaTemp = SupplyTemp - state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak;
                    } else {
                        DeltaTemp = -std::abs(state.dataSize->CalcFinalZoneSizing(I).CoolDesTempDiff);
                        SupplyTemp = DeltaTemp + state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak;
                    }

                    // check for low delta T to avoid very high flow rates
                    if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
                        if (std::abs(DeltaTemp) >= 2.0) {                                   // Vdot exceeds 3000 cfm/ton @ DT=2
                            ShowWarningError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of zone temperature");
                        } else {
                            ShowSevereError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of zone temperature");
                        }
                        ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
                        ShowContinueError(state, "...zone name = " + state.dataSize->CalcFinalZoneSizing(I).ZoneName);
                        ShowContinueError(state,
                                          format("...design sensible cooling load = {:.2R} W", state.dataSize->CalcFinalZoneSizing(I).DesCoolLoad));
                        ShowContinueError(state,
                                          format("...thermostat set point temp    = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).CoolTstatTemp));
                        ShowContinueError(
                            state, format("...zone temperature             = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak));
                        ShowContinueError(state, format("...supply air temperature       = {:.3R} C", SupplyTemp));
                        ShowContinueError(state, format("...temperature difference       = {:.5R} C", DeltaTemp));
                        ShowContinueError(
                            state, format("...calculated volume flow rate  = {:.5R} m3/s", (state.dataSize->CalcFinalZoneSizing(I).DesCoolVolFlow)));
                        ShowContinueError(
                            state, format("...calculated mass flow rate    = {:.5R} kg/s", (state.dataSize->CalcFinalZoneSizing(I).DesCoolMassFlow)));
                        if (SupplyTemp > state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak)
                            ShowContinueError(
                                state, "...Note: supply air temperature should be less than zone temperature during cooling air flow calculations");
                    } else if (std::abs(DeltaTemp) > SmallTempDiff && SupplyTemp > state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak) {
                        ShowSevereError(
                            state, "UpdateZoneSizing: Supply air temperature is greater than zone temperature during cooling air flow calculations");
                        ShowContinueError(
                            state, format("...calculated volume flow rate  = {:.5R} m3/s", (state.dataSize->CalcFinalZoneSizing(I).DesCoolVolFlow)));
                        ShowContinueError(
                            state, format("...calculated mass flow rate    = {:.5R} kg/s", (state.dataSize->CalcFinalZoneSizing(I).DesCoolMassFlow)));
                        ShowContinueError(state,
                                          format("...thermostat set point temp    = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).CoolTstatTemp));
                        ShowContinueError(
                            state, format("...zone temperature            = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak));
                        ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
                        ShowContinueError(state, "...occurs in zone              = " + state.dataSize->CalcFinalZoneSizing(I).ZoneName);
                        ShowContinueError(
                            state, "...Note: supply air temperature should be less than zone temperature during cooling air flow calculations");
                    }
                }
                // Should this be done only if there is a heating load? Or would this message help determine why there was no load?
                if (std::abs(state.dataSize->CalcFinalZoneSizing(I).DesHeatLoad) > 1.e-8) { // ABS() ?
                    // check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
                    if (state.dataSize->CalcFinalZoneSizing(I).ZnHeatDgnSAMethod == SupplyAirTemperature) {
                        SupplyTemp = state.dataSize->CalcFinalZoneSizing(I).HeatDesTemp;
                        DeltaTemp = SupplyTemp - state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak;
                    } else {
                        DeltaTemp = state.dataSize->CalcFinalZoneSizing(I).HeatDesTempDiff;
                        SupplyTemp = DeltaTemp + state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak;
                    }

                    if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
                        if (std::abs(DeltaTemp) >= 2.0) {                                   // Vdot exceeds 3000 cfm/ton @ DT=2
                            ShowWarningError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of zone temperature");
                        } else {
                            ShowSevereError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of zone temperature");
                        }
                        ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
                        ShowContinueError(state, "...zone name = " + state.dataSize->CalcFinalZoneSizing(I).ZoneName);
                        ShowContinueError(state,
                                          format("...design heating load         = {:.2R} W", state.dataSize->CalcFinalZoneSizing(I).DesHeatLoad));
                        ShowContinueError(state,
                                          format("...thermostat set point temp   = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).HeatTstatTemp));
                        ShowContinueError(
                            state, format("...zone temperature            = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak));
                        ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
                        ShowContinueError(state, format("...temperature difference      = {:.5R} C", DeltaTemp));
                        ShowContinueError(
                            state, format("...calculated volume flow rate = {:.5R} m3/s", (state.dataSize->CalcFinalZoneSizing(I).DesHeatVolFlow)));
                        ShowContinueError(
                            state, format("...calculated mass flow rate   = {:.5R} kg/s", (state.dataSize->CalcFinalZoneSizing(I).DesHeatMassFlow)));
                        if (SupplyTemp < state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak)
                            ShowContinueError(state,
                                              "...Note: supply air temperature should be greater than zone temperature during heating air "
                                              "flow calculations");
                    } else if (std::abs(DeltaTemp) > SmallTempDiff && SupplyTemp < state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak) {
                        ShowSevereError(
                            state, "UpdateZoneSizing: Supply air temperature is less than zone temperature during heating air flow calculations");
                        ShowContinueError(state,
                                          format("...calculated design heating volume flow rate = {:.5R} m3/s",
                                                 (state.dataSize->CalcFinalZoneSizing(I).DesHeatVolFlow)));
                        ShowContinueError(state,
                                          format("...calculated design heating mass flow rate   = {:.5R} kg/s",
                                                 (state.dataSize->CalcFinalZoneSizing(I).DesHeatMassFlow)));
                        ShowContinueError(state,
                                          format("...thermostat set piont temp   = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).HeatTstatTemp));
                        ShowContinueError(
                            state, format("...zone temperature            = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak));
                        ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
                        ShowContinueError(state, "...occurs in zone              = " + state.dataSize->CalcFinalZoneSizing(I).ZoneName);
                        ShowContinueError(state,
                                          "...Note: supply air temperature should be greater than zone temperature during heating air "
                                          "flow calculations");
                    }
                }
            }

            print(state.files.zsz, "\n");
            //      HourFrac = 0.0
            Minutes = 0;
            TimeStepIndex = 0;
            for (HourCounter = 1; HourCounter <= 24; ++HourCounter) {
                for (TimeStepCounter = 1; TimeStepCounter <= state.dataGlobal->NumOfTimeStepInHour; ++TimeStepCounter) {
                    ++TimeStepIndex;
                    Minutes += state.dataGlobal->MinutesPerTimeStep;
                    if (Minutes == 60) {
                        Minutes = 0;
                        HourPrint = HourCounter;
                    } else {
                        HourPrint = HourCounter - 1;
                    }
                    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                        if (TimeStepIndex == state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax) {
                            state.dataSize->HeatPeakDateHrMin(CtrlZoneNum) =
                                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cHeatDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                        if (TimeStepIndex == state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax) {
                            state.dataSize->CoolPeakDateHrMin(CtrlZoneNum) =
                                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).cCoolDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                    }

                    static constexpr std::string_view ZSizeFmt20("{:02}:{:02}:00");
                    print(state.files.zsz, ZSizeFmt20, HourPrint, Minutes);
                    for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                        static constexpr std::string_view ZSizeFmt21("{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}");
                        print(state.files.zsz,
                              ZSizeFmt21,
                              state.dataSize->SizingFileColSep,
                              state.dataSize->CalcFinalZoneSizing(I).HeatLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              state.dataSize->CalcFinalZoneSizing(I).CoolLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              state.dataSize->CalcFinalZoneSizing(I).HeatFlowSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              state.dataSize->CalcFinalZoneSizing(I).CoolFlowSeq(TimeStepIndex));
                    }
                    print(state.files.zsz, "\n");
                }
            }
            print(state.files.zsz, "Peak");

            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;

                static constexpr std::string_view ZSizeFmt31("{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}");
                print(state.files.zsz,
                      ZSizeFmt31,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesHeatLoad,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesCoolLoad,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesHeatMassFlow,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesCoolMassFlow);
            }
            print(state.files.zsz, "\n");

            print(state.files.zsz, "\nPeak Vol Flow (m3/s)");
            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                static constexpr std::string_view ZSizeFmt41("{}{}{}{:12.6E}{}{:12.6E}");
                print(state.files.zsz,
                      ZSizeFmt41,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesHeatVolFlow,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->CalcFinalZoneSizing(I).DesCoolVolFlow);
            }
            print(state.files.zsz, "\n");
            state.files.zsz.close();
        }

        // Move data from Calc arrays to user modified arrays

        for (std::size_t i = 0; i < state.dataSize->ZoneSizing.size(); ++i) {
            auto &z(state.dataSize->ZoneSizing[i]);
            auto &c(state.dataSize->CalcZoneSizing[i]);
            z.CoolDesDay = c.CoolDesDay;
            z.HeatDesDay = c.HeatDesDay;
            z.DesHeatDens = c.DesHeatDens;
            z.DesCoolDens = c.DesCoolDens;
            z.HeatDDNum = c.HeatDDNum;
            z.CoolDDNum = c.CoolDDNum;

            z.DesHeatLoad = c.DesHeatLoad;
            z.DesHeatMassFlow = c.DesHeatMassFlow;
            z.ZoneTempAtHeatPeak = c.ZoneTempAtHeatPeak;
            z.OutTempAtHeatPeak = c.OutTempAtHeatPeak;
            z.ZoneRetTempAtHeatPeak = c.ZoneRetTempAtHeatPeak;
            z.ZoneHumRatAtHeatPeak = c.ZoneHumRatAtHeatPeak;
            z.OutHumRatAtHeatPeak = c.OutHumRatAtHeatPeak;
            z.TimeStepNumAtHeatMax = c.TimeStepNumAtHeatMax;
            z.DesHeatVolFlow = c.DesHeatVolFlow;
            z.DesHeatCoilInTemp = c.DesHeatCoilInTemp;
            z.DesHeatCoilInHumRat = c.DesHeatCoilInHumRat;

            z.DesCoolLoad = c.DesCoolLoad;
            z.DesCoolMassFlow = c.DesCoolMassFlow;
            z.ZoneTempAtCoolPeak = c.ZoneTempAtCoolPeak;
            z.OutTempAtCoolPeak = c.OutTempAtCoolPeak;
            z.ZoneRetTempAtCoolPeak = c.ZoneRetTempAtCoolPeak;
            z.ZoneHumRatAtCoolPeak = c.ZoneHumRatAtCoolPeak;
            z.OutHumRatAtCoolPeak = c.OutHumRatAtCoolPeak;
            z.TimeStepNumAtCoolMax = c.TimeStepNumAtCoolMax;
            z.DesCoolVolFlow = c.DesCoolVolFlow;
            z.DesCoolCoilInTemp = c.DesCoolCoilInTemp;
            z.DesCoolCoilInHumRat = c.DesCoolCoilInHumRat;
        }

        for (std::size_t i = 0; i < state.dataSize->FinalZoneSizing.size(); ++i) {
            auto &z(state.dataSize->FinalZoneSizing[i]);
            auto &c(state.dataSize->CalcFinalZoneSizing[i]);
            z.CoolDesDay = c.CoolDesDay;
            z.HeatDesDay = c.HeatDesDay;
            z.DesHeatDens = c.DesHeatDens;
            z.DesCoolDens = c.DesCoolDens;
            z.HeatDDNum = c.HeatDDNum;
            z.CoolDDNum = c.CoolDDNum;

            z.DesHeatLoad = c.DesHeatLoad;
            z.NonAirSysDesHeatLoad = c.DesHeatLoad;
            z.DesHeatMassFlow = c.DesHeatMassFlow;
            z.ZoneTempAtHeatPeak = c.ZoneTempAtHeatPeak;
            z.OutTempAtHeatPeak = c.OutTempAtHeatPeak;
            z.ZoneRetTempAtHeatPeak = c.ZoneRetTempAtHeatPeak;
            z.ZoneHumRatAtHeatPeak = c.ZoneHumRatAtHeatPeak;
            z.OutHumRatAtHeatPeak = c.OutHumRatAtHeatPeak;
            z.TimeStepNumAtHeatMax = c.TimeStepNumAtHeatMax;
            z.DesHeatVolFlow = c.DesHeatVolFlow;
            z.NonAirSysDesHeatVolFlow = c.DesHeatVolFlow;
            z.DesHeatCoilInTemp = c.DesHeatCoilInTemp;
            z.DesHeatCoilInHumRat = c.DesHeatCoilInHumRat;

            z.DesCoolLoad = c.DesCoolLoad;
            z.NonAirSysDesCoolLoad = c.DesCoolLoad;
            z.DesCoolMassFlow = c.DesCoolMassFlow;
            z.ZoneTempAtCoolPeak = c.ZoneTempAtCoolPeak;
            z.OutTempAtCoolPeak = c.OutTempAtCoolPeak;
            z.ZoneRetTempAtCoolPeak = c.ZoneRetTempAtCoolPeak;
            z.ZoneHumRatAtCoolPeak = c.ZoneHumRatAtCoolPeak;
            z.OutHumRatAtCoolPeak = c.OutHumRatAtCoolPeak;
            z.TimeStepNumAtCoolMax = c.TimeStepNumAtCoolMax;
            z.DesCoolVolFlow = c.DesCoolVolFlow;
            z.NonAirSysDesCoolVolFlow = c.DesCoolVolFlow;
            z.DesCoolCoilInTemp = c.DesCoolCoilInTemp;
            z.DesCoolCoilInHumRat = c.DesCoolCoilInHumRat;
        }

        for (DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatFlowSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatFlowSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatLoadSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatLoadSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolLoadSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolLoadSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatOutTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatOutTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolOutTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolOutTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex);
                    state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex) =
                        state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex);
                }
            }
        }

        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatFlowSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatLoadSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatLoadSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolLoadSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolLoadSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatOutTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneRetTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatZoneHumRatSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatOutHumRatSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolOutTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex) =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex);
            }
        }
        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // update non air system design load and air flow to include the sizing factor
            state.dataSize->FinalZoneSizing(CtrlZoneNum).NonAirSysDesCoolLoad *= state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).NonAirSysDesCoolVolFlow *=
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor; // NonAirSysDesCoolVolFlow not currently used
            // Now take into account the user specified sizing factor and user specified cooling design air flow rate
            TotCoolSizMult = 0.0;
            // Calculate a sizing factor from the user specified cooling design air flow rate
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow > 0.0 &&
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolAirDesMethod == InpDesAirFlow &&
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow > 0.0) {
                TotCoolSizMult =
                    (state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow / state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow) *
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor;
                // If no user specified cooling design air flow rate input, use the user specified szing factor
            } else {
                TotCoolSizMult = state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolSizingFactor;
            }
            // If the cooling sizing multiplier is not 1, adjust the cooling design data
            if (std::abs(TotCoolSizMult - 1.0) > 0.00001) {
                if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow > 0.0) {
                    TimeStepAtPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax;
                    DDNum = state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDDNum;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow * TotCoolSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlow =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow * TotCoolSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad * TotCoolSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeq =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolFlowSeq * TotCoolSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolLoadSeq =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolLoadSeq * TotCoolSizMult;
                    OAFrac = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA / state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow;
                    OAFrac = min(1.0, max(0.0, OAFrac));
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolCoilInTemp =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) +
                        (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolCoilInHumRat =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) +
                        (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak;
                } else {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesCoolAirFlow;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlow =
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow * state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolDens;
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    if (state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow > 0.0) {
                        TimeStepAtPeak = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).TimeStepNumAtCoolMax;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow * TotCoolSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow * TotCoolSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolLoad =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesCoolLoad * TotCoolSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq * TotCoolSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolLoadSeq =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).CoolLoadSeq * TotCoolSizMult;
                        OAFrac = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA / state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow;
                        OAFrac = min(1.0, max(0.0, OAFrac));
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolCoilInTemp =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) +
                            (1.0 - OAFrac) * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).ZoneTempAtCoolPeak;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolCoilInHumRat =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) +
                            (1.0 - OAFrac) * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).ZoneHumRatAtCoolPeak;
                    } else {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow =
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).InpDesCoolAirFlow;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow =
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow *
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolDens;
                    }
                    // Save cooling flows without MinOA for use later
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeqNoOA = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlowNoOA = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlowNoOA =
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow;
                }
            }
            // Save a set of design cooling air flow rates greater than or equal to the specified minimums without MinOA
            {
                Real64 MaxOfMinCoolVolFlowNoOA = 0.0; // max of the user specified design cooling minimum flows without min OA flow [m3/s]
                if (state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolAirDesMethod == DesAirFlowWithLim) {
                    MaxOfMinCoolVolFlowNoOA = max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow,
                                                  state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2);
                }
                Real64 MaxOfMinCoolMassFlowNoOA =
                    MaxOfMinCoolVolFlowNoOA * state.dataSize->FinalZoneSizing(CtrlZoneNum)
                                                  .DesCoolDens; // max of the user specified design cooling minimum flows without min OA flow [m3/s]
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlowNoOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlowNoOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlow;
                if (MaxOfMinCoolVolFlowNoOA > state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlowNoOA) {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeqNoOA(TimeStepIndex) =
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
                    if (MaxOfMinCoolMassFlowNoOA > state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeqNoOA(TimeStepIndex)) {
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeqNoOA(TimeStepIndex) = MaxOfMinCoolMassFlowNoOA;
                    }
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlowNoOA = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlowNoOA =
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow;
                    MaxOfMinCoolVolFlowNoOA = max(state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMinAirFlow,
                                                  state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMinAirFlow);
                    MaxOfMinCoolMassFlowNoOA = MaxOfMinCoolVolFlowNoOA * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolDens;
                    if (MaxOfMinCoolVolFlowNoOA > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow) {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
                    }
                    for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeqNoOA(TimeStepIndex) =
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
                        if (MaxOfMinCoolMassFlowNoOA > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex)) {
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeqNoOA(TimeStepIndex) = MaxOfMinCoolMassFlowNoOA;
                        }
                    }
                }
            }

            // Now make sure that the design cooling air flow rates are greater than or equal to the specified minimums including MinOA
            {
                Real64 MaxOfMinCoolVolFlow = 0.0; // max of the user specified design cooling minimum flows and min OA flow [m3/s]
                if (state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolAirDesMethod == DesAirFlowWithLim) {
                    MaxOfMinCoolVolFlow = max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow,
                                              state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2,
                                              state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA);
                } else {
                    MaxOfMinCoolVolFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA;
                }
                Real64 MaxOfMinCoolMassFlow =
                    MaxOfMinCoolVolFlow * state.dataSize->FinalZoneSizing(CtrlZoneNum)
                                              .DesCoolDens; // max of the user specified design cooling minimum flows and min OA flow [kg/s]
                if (MaxOfMinCoolVolFlow > state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow) {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow = MaxOfMinCoolVolFlow;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMassFlow = MaxOfMinCoolMassFlow;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (MaxOfMinCoolMassFlow > state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeq(TimeStepIndex)) {
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
                    }
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    MaxOfMinCoolVolFlow = max(state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMinAirFlow,
                                              state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMinAirFlow,
                                              state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA);
                    MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolDens;
                    if (MaxOfMinCoolVolFlow > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow) {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolVolFlow = MaxOfMinCoolVolFlow;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesCoolMassFlow = MaxOfMinCoolMassFlow;
                    }
                    for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                        if (MaxOfMinCoolMassFlow > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex)) {
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
                        }
                    }
                }
            }
            // IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolLoad == 0) {
                // Check CoolDDNum and TimeStepNumAtCoolMax value and default to 1 if not set, carried over from previous code
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum == 0) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum = 1;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax == 0) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax = 1;
                }
                state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDDNum = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDDNum;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesDay = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay;
                DDNumF = state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDDNum;
                TimeStepAtPeakF = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtCoolMax;

                // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
                // issue 6006, heating coils sizing to 0 when no heating load in zone
                if (state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesCoolSetPtSeq.empty()) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + ":  Thermostat cooling set point temperatures are not initialized for Zone = " +
                                        state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName);
                    ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
                } else {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak =
                        *std::min_element(state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesCoolSetPtSeq.begin(),
                                          state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesCoolSetPtSeq.end());
                }
                state.dataSize->FinalZoneSizing(CtrlZoneNum).OutTempAtCoolPeak =
                    *std::min_element(state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolOutTempSeq.begin(),
                                      state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolOutTempSeq.end());
                state.dataSize->FinalZoneSizing(CtrlZoneNum).OutHumRatAtCoolPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolOutHumRatSeq(TimeStepAtPeakF);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak = state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolDesHumRat;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolZoneTempSeq(TimeStepAtPeakF);
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepAtPeakF);
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtCoolPeak =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolCoilInTemp = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolCoilInHumRat = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtCoolPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneRetTempAtCoolPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak;
            }
            // update non air system design load and air flow to include the sizing factor
            state.dataSize->FinalZoneSizing(CtrlZoneNum).NonAirSysDesHeatLoad *= state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).NonAirSysDesHeatVolFlow *= state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
            // Now take into account the user specified sizing factor or user specified heating design air flow rate (which overrides the
            // sizing factor)
            TotHeatSizMult = 0.0;
            // Calculate a sizing factor from the user specified heating design air flow rate
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow > 0.0 &&
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatAirDesMethod == InpDesAirFlow &&
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow > 0.0) {
                TotHeatSizMult =
                    (state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow / state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) *
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
                // Calculate a sizing factor from the user specified max heating design air flow rates
            } else if (state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatAirDesMethod == DesAirFlowWithLim &&
                       state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow > 0.0) {
                MaxHeatVolFlow = max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow,
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2,
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow *
                                         state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac);
                if (MaxHeatVolFlow < state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) {
                    TotHeatSizMult = (MaxHeatVolFlow / state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) *
                                     state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
                } else {
                    TotHeatSizMult = state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
                }
                // If no user specified heating design air flow rate input, use the user specified sizing factor
            } else {
                TotHeatSizMult = state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatSizingFactor;
            }

            if (std::abs(TotHeatSizMult - 1.0) > 0.00001) {
                if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow > 0.0) {
                    TimeStepAtPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax;
                    DDNum = state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDDNum;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow * TotHeatSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow * TotHeatSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad * TotHeatSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeq =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatFlowSeq * TotHeatSizMult;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatLoadSeq =
                        state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatLoadSeq * TotHeatSizMult;
                    OAFrac = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA / state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow;
                    OAFrac = min(1.0, max(0.0, OAFrac));
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatCoilInTemp =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) +
                        (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatCoilInHumRat =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) +
                        (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak;
                } else {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).InpDesHeatAirFlow;
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow =
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow * state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatDens;
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    if (state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow > 0.0) {
                        TimeStepAtPeak = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).TimeStepNumAtHeatMax;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow * TotHeatSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlow =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlow * TotHeatSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatLoad =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).DesHeatLoad * TotHeatSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeq =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeq * TotHeatSizMult;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatLoadSeq =
                            state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum).HeatLoadSeq * TotHeatSizMult;
                        OAFrac = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA / state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow;
                        OAFrac = min(1.0, max(0.0, OAFrac));
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatCoilInTemp =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) +
                            (1.0 - OAFrac) * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).ZoneTempAtHeatPeak;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatCoilInHumRat =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) +
                            (1.0 - OAFrac) * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).ZoneHumRatAtHeatPeak;
                    } else {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow =
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).InpDesHeatAirFlow;
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlow =
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow *
                            state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatDens;
                    }
                    // Save heating flows without MinOA for use later
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeqNoOA = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeq;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlowNoOA = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlowNoOA =
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlow;
                }
            }

            // Save a set of design heating air flow rates before the MinOA adjustment
            // just in FinalZoneSizing to use for TermUnit sizing adjustments in SizingManager::UpdateTermUnitFinalZoneSizing
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlowNoOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow;
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlowNoOA = state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow;
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeqNoOA(TimeStepIndex) =
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeq(TimeStepIndex);
            }

            // Now make sure that the design heating air flow rates are greater than or equal to MinOA
            MinOAMass = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA * state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatDens;
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA > state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) {
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow = state.dataSize->FinalZoneSizing(CtrlZoneNum).MinOA;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow = MinOAMass;
            }
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (MinOAMass > state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeq(TimeStepIndex)) {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatFlowSeq(TimeStepIndex) = MinOAMass;
                }
            }
            for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                MinOAMass = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA * state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatDens;
                if (state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow) {
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatVolFlow = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).MinOA;
                    state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).DesHeatMassFlow = MinOAMass;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (MinOAMass > state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeq(TimeStepIndex)) {
                        state.dataSize->ZoneSizing(DDNum, CtrlZoneNum).HeatFlowSeq(TimeStepIndex) = MinOAMass;
                    }
                }
            }
            // IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatLoad == 0) {
                // Check HDDNum and TimeStepNumAtHeatMax value and default to 1 if not set, carried over from previous code
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum == 0) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum = 1;
                }
                if (state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax == 0) {
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax = 1;
                }
                state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDDNum = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDDNum;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesDay = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).HeatDesDay;
                DDNumF = state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDDNum;
                TimeStepAtPeakF = state.dataSize->FinalZoneSizing(CtrlZoneNum).TimeStepNumAtHeatMax;

                // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
                // issue 6006, heating coils sizing to 0 when no heating load in zone
                if (state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesHeatSetPtSeq.empty()) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + ":  Thermostat heating set point temperatures not initialized for Zone = " +
                                        state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneName);
                    ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
                } else {
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak =
                        *std::max_element(state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesHeatSetPtSeq.begin(),
                                          state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).DesHeatSetPtSeq.end());
                }
                state.dataSize->FinalZoneSizing(CtrlZoneNum).OutTempAtHeatPeak =
                    *std::min_element(state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatOutTempSeq.begin(),
                                      state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatOutTempSeq.end());
                state.dataSize->FinalZoneSizing(CtrlZoneNum).OutHumRatAtHeatPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatOutHumRatSeq(TimeStepAtPeakF);
                state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak = state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatDesHumRat;
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatZoneTempSeq(TimeStepAtPeakF);
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak =
                    state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepAtPeakF);
                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneRetTempAtHeatPeak =
                    state.dataSize->CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatCoilInTemp = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatCoilInHumRat = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneHumRatAtHeatPeak;
                state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneRetTempAtHeatPeak = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak;
            }

            // set the zone minimum cooling supply air flow rate. This will be used for autosizing VAV terminal unit
            // minimum flow rates (comment seems incorrect, really used as a minimum lower limit for the maximum air flow)
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlowMin =
                max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow,
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlow2,
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow * state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolMinAirFlowFrac);
            // set the zone maximum heating supply air flow rate. This will be used for autosizing VAV terminal unit
            // max heating flow rates
            state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlowMax =
                max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow,
                    state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlow2,
                    max(state.dataSize->FinalZoneSizing(CtrlZoneNum).DesCoolVolFlow, state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow) *
                        state.dataSize->FinalZoneSizing(CtrlZoneNum).DesHeatMaxAirFlowFrac);
            // Determine the design cooling supply air temperature if the supply air temperature difference is specified by user.
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnCoolDgnSAMethod == TemperatureDifference) {
                state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTemp = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtCoolPeak -
                                                                           std::abs(state.dataSize->FinalZoneSizing(CtrlZoneNum).CoolDesTempDiff);
            }
            // Determine the design heating supply air temperature if the supply air temperature difference is specified by user.
            if (state.dataSize->FinalZoneSizing(CtrlZoneNum).ZnHeatDgnSAMethod == TemperatureDifference) {
                state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTemp = state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak +
                                                                           std::abs(state.dataSize->FinalZoneSizing(CtrlZoneNum).HeatDesTempDiff);
            }
        }
    } break;
    default:
        break;
    }
}

void SimZoneEquipment(EnergyPlusData &state, bool const FirstHVACIteration, bool &SimAir)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       Raustad/Shirey, FSEC, June 2003
    //       MODIFIED       Gu, FSEC, Jan. 2004, Don Shirey, Aug 2009 (LatOutputProvided)
    //                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is responsible for determining
    // how much of each type of energy every zone requires.
    // In effect, this subroutine defines and simulates all
    // the system types and in the case of hybrid systems
    // which use more than one type of energy must determine
    // how to apportion the load. An example of a hybrid system
    // is a water loop heat pump with supplemental air.  In
    // this case, a zone will require water from the loop and
    // cooled or heated air from the air system. A simpler
    // example would be a VAV system with baseboard heaters

    // METHODOLOGY EMPLOYED:
    // 1.  Determine zone load - this is zone temperature dependent
    // 2.  Determine balance point - the temperature at which the
    //     zone load is balanced by the system output. The way the
    //     balance point is determined will be different depending on
    //     the type of system being simulated.
    // 3.  Calculate zone energy requirements

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    // Using/Aliasing
    using namespace DataHVACGlobals;
    using BaseboardElectric::SimElectricBaseboard;
    using BaseboardRadiator::SimBaseboard;
    using CoolingPanelSimple::SimCoolingPanel;
    using ElectricBaseboardRadiator::SimElecBaseboard;
    using EvaporativeCoolers::SimZoneEvaporativeCoolerUnit;
    using FanCoilUnits::SimFanCoilUnit;
    using HeatRecovery::SimHeatRecovery;
    using HighTempRadiantSystem::SimHighTempRadiantSystem;
    using HVACStandAloneERV::SimStandAloneERV;
    using HVACVariableRefrigerantFlow::SimulateVRF;
    using HWBaseboardRadiator::SimHWBaseboard;
    using HybridUnitaryAirConditioners::SimZoneHybridUnitaryAirConditioners;
    using LowTempRadiantSystem::SimLowTempRadiantSystem;
    using PackagedTerminalHeatPump::SimPackagedTerminalUnit;
    using PurchasedAirManager::SimPurchasedAir;
    using RefrigeratedCase::SimAirChillerSet;
    using ReturnAirPathManager::SimReturnAirPath;
    using SplitterComponent::SimAirLoopSplitter;
    using SteamBaseboardRadiator::SimSteamBaseboard;
    using SwimmingPool::SimSwimmingPool;
    using SystemAvailabilityManager::GetZoneEqAvailabilityManager;
    using UnitHeater::SimUnitHeater;
    using UnitVentilator::SimUnitVentilator;
    using UserDefinedComponents::SimZoneAirUserDefined;
    using VentilatedSlab::SimVentilatedSlab;
    using WaterThermalTanks::SimHeatPumpWaterHeater;
    using WindowAC::SimWindowAC;
    using ZoneAirLoopEquipmentManager::ManageZoneAirLoopEquipment;
    using ZoneDehumidifier::SimZoneDehumidifier;
    using ZonePlenum::SimAirZonePlenum;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ActualZoneNum;
    int ControlledZoneNum;
    int EquipTypeNum;
    int SupplyAirPathNum;
    int CompNum;
    int EquipPtr;
    int ZoneEquipTypeNum;
    int ZoneCompNum;

    bool SupPathInletChanged(false);
    bool FirstCall; // indicates first call to supply air path components
    bool ErrorFlag;

    Real64 SysOutputProvided; // sensible output delivered by zone equipment (W)
    Real64 LatOutputProvided; // latent output delivered by zone equipment (kg/s)
    Real64 AirSysOutput;
    Real64 NonAirSysOutput;

    // Determine flow rate and temperature of supply air based on type of damper

    FirstCall = true;
    ErrorFlag = false;

    for (SupplyAirPathNum = 1; SupplyAirPathNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SupplyAirPathNum) {

        for (CompNum = 1; CompNum <= state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).NumOfComponents; ++CompNum) {

            switch (state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentTypeEnum(CompNum)) {
            case DataZoneEquipment::AirLoopHVACZone::Splitter: { // 'AirLoopHVAC:ZoneSplitter'

                if (!(state.afn->AirflowNetworkFanActivated && state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
                    SimAirLoopSplitter(state,
                                       state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                       FirstHVACIteration,
                                       FirstCall,
                                       SupPathInletChanged,
                                       state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum));
                }

                break;
            }
            case DataZoneEquipment::AirLoopHVACZone::SupplyPlenum: { // 'AirLoopHVAC:SupplyPlenum'

                SimAirZonePlenum(state,
                                 state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                 DataZoneEquipment::AirLoopHVACZone::SupplyPlenum,
                                 state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum),
                                 FirstHVACIteration,
                                 FirstCall,
                                 SupPathInletChanged);

                break;
            }
            default: {
                ShowSevereError(state, "Error found in Supply Air Path=" + state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).Name);
                ShowContinueError(state,
                                  "Invalid Supply Air Path Component=" + state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentType(CompNum));
                ShowFatalError(state, "Preceding condition causes termination.");

                break;
            }
            }
        }
    }

    FirstCall = false;

    // Simulate all of the pools. These have a potential impact on surface heat balances, zone air heat balances, and moisture balances.
    // These should be simulated first so that any systems or zone equipment devices deal with the effects of the pool properly.
    SimSwimmingPool(state, FirstHVACIteration);

    // Loop over all the primary air loop; simulate their components (equipment)
    // and controllers
    if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
        if (FirstHVACIteration) {
            CalcAirFlowSimple(state, 0);
        } else {
            CalcAirFlowSimple(
                state, 0, state.dataHeatBal->ZoneAirMassFlow.AdjustZoneMixingFlow, state.dataHeatBal->ZoneAirMassFlow.AdjustZoneInfiltrationFlow);
        }
    }

    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {

        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;

        state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) = 0.0;
        state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) = 0.0;
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneExh = 0.0;
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneExhBalanced = 0.0;
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).PlenumMassFlow = 0.0;
        state.dataSize->CurZoneEqNum = ControlledZoneNum;

        InitSystemOutputRequired(state, ActualZoneNum, FirstHVACIteration, true);

        auto &TurnFansOn = state.dataHVACGlobal->TurnFansOn;
        auto &TurnFansOff = state.dataHVACGlobal->TurnFansOff;
        auto &TurnZoneFansOnlyOn = state.dataHVACGlobal->TurnZoneFansOnlyOn;

        // Air loop system availability manager status only applies to PIU and exhaust fans
        // Reset fan SAM operation flags for zone fans.
        TurnFansOn = false;
        TurnZoneFansOnlyOn = false;
        TurnFansOff = false;

        for (EquipTypeNum = 1; EquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).NumOfEquipTypes; ++EquipTypeNum) {

            state.dataHVACGlobal->UnbalExhMassFlow = 0.0;
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
            state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
            EquipPtr = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipPtr;
            SysOutputProvided = 0.0;
            LatOutputProvided = 0.0;
            state.dataSize->DataCoolCoilCap = 0.0; // reset global variable used only for heat pumps (i.e., DX cooling and heating coils)

            // Reset ZoneEqSizing data (because these may change from one equipment type to the next)
            if (state.dataZoneEquipmentManager->FirstPassZoneEquipFlag) {
                ZoneEqSizing(ControlledZoneNum).AirVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).MaxHWVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).MaxCWVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).OAVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).DesCoolingLoad = 0.0;
                ZoneEqSizing(ControlledZoneNum).DesHeatingLoad = 0.0;
                ZoneEqSizing(ControlledZoneNum).CoolingAirVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).HeatingAirVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).SystemAirVolFlow = 0.0;
                ZoneEqSizing(ControlledZoneNum).AirFlow = false;
                ZoneEqSizing(ControlledZoneNum).CoolingAirFlow = false;
                ZoneEqSizing(ControlledZoneNum).HeatingAirFlow = false;
                ZoneEqSizing(ControlledZoneNum).SystemAirFlow = false;
                ZoneEqSizing(ControlledZoneNum).Capacity = false;
                ZoneEqSizing(ControlledZoneNum).CoolingCapacity = false;
                ZoneEqSizing(ControlledZoneNum).HeatingCapacity = false;
                ZoneEqSizing(ControlledZoneNum).SystemCapacity = false;
                ZoneEqSizing(ControlledZoneNum).DesignSizeFromParent = false;
            }

            ZoneEquipTypeNum = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipTypeEnum;

            ZoneCompNum = state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr);

            bool ValidSAMComp = false;

            if (ZoneEquipTypeNum <= NumValidSysAvailZoneComponents) ValidSAMComp = true;

            auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

            if (ZoneCompNum > 0 && ValidSAMComp) {

                GetZoneEqAvailabilityManager(state, ZoneEquipTypeNum, ZoneCompNum, ErrorFlag);

                if (ZoneComp(ZoneEquipTypeNum).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == CycleOn) {
                    state.dataHVACGlobal->ZoneCompTurnFansOn = true;
                    state.dataHVACGlobal->ZoneCompTurnFansOff = false;
                } else if (ZoneComp(ZoneEquipTypeNum).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == ForceOff) {
                    state.dataHVACGlobal->ZoneCompTurnFansOn = false;
                    state.dataHVACGlobal->ZoneCompTurnFansOff = true;
                } else {
                    state.dataHVACGlobal->ZoneCompTurnFansOn = TurnFansOn;
                    state.dataHVACGlobal->ZoneCompTurnFansOff = TurnFansOff;
                }
            } else {
                state.dataHVACGlobal->ZoneCompTurnFansOn = TurnFansOn;
                state.dataHVACGlobal->ZoneCompTurnFansOff = TurnFansOff;
            }

            switch (ZoneEquipTypeNum) {
            case ZoneEquip::AirDistUnit: { // 'ZoneHVAC:AirDistributionUnit'
                // Air loop system availability manager status only applies to PIU and exhaust fans
                // Check to see if System Availability Managers are asking for fans to cycle on or shut off
                // and set fan on/off flags accordingly.
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == CycleOnZoneFansOnly) {
                    TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == CycleOnZoneFansOnly) {
                    // Currently used only by parallel powered induction unit
                    TurnZoneFansOnlyOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == ForceOff) {
                    TurnFansOff = true;
                }

                ManageZoneAirLoopEquipment(state,
                                           state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                           FirstHVACIteration,
                                           AirSysOutput,
                                           NonAirSysOutput,
                                           LatOutputProvided,
                                           ActualZoneNum,
                                           ControlledZoneNum,
                                           state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                //            reset status flags for other zone equipment
                TurnFansOn = false;
                TurnZoneFansOnlyOn = false;
                TurnFansOff = false;

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += NonAirSysOutput;
                SysOutputProvided = NonAirSysOutput + AirSysOutput;
            } break;
            case ZoneEquip::VRFTerminalUnit: { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
                bool HeatingActive = false;
                bool CoolingActive = false;
                int constexpr OAUnitNum = 0;
                Real64 constexpr OAUCoilOutTemp = 0.0;
                bool constexpr ZoneEquipment = true;
                SimulateVRF(state,
                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                            FirstHVACIteration,
                            ControlledZoneNum,
                            state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr),
                            HeatingActive,
                            CoolingActive,
                            OAUnitNum,
                            OAUCoilOutTemp,
                            ZoneEquipment,
                            SysOutputProvided,
                            LatOutputProvided);
            } break;
            case ZoneEquip::WindowAC: { // 'ZoneHVAC:WindowAirConditioner'
                SimWindowAC(state,
                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                            ActualZoneNum,
                            FirstHVACIteration,
                            SysOutputProvided,
                            LatOutputProvided,
                            state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::PkgTermHPAirToAir:
            case ZoneEquip::PkgTermACAirToAir:
            case ZoneEquip::PkgTermHPWaterToAir: { // 'ZoneHVAC:PackagedTerminalHeatPump'
                // 'ZoneHVAC:PackagedTerminalAirConditioner'
                // 'ZoneHVAC:WaterToAirHeatPump'
                SimPackagedTerminalUnit(state,
                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                        ActualZoneNum,
                                        FirstHVACIteration,
                                        SysOutputProvided,
                                        LatOutputProvided,
                                        ZoneEquipTypeNum,
                                        state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::ZoneUnitarySys: { // 'AirloopHVAC:UnitarySystem'
                int AirLoopNum = 0;
                bool HeatingActive = false;
                bool CoolingActive = false;
                int OAUnitNum = 0;
                Real64 OAUCoilOutTemp = 0.0;
                bool ZoneEquipFlag = true;
                state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum)
                    .compPointer[EquipPtr]
                    ->simulate(state,
                               state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                               FirstHVACIteration,
                               AirLoopNum,
                               state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr),
                               HeatingActive,
                               CoolingActive,
                               OAUnitNum,
                               OAUCoilOutTemp,
                               ZoneEquipFlag,
                               SysOutputProvided,
                               LatOutputProvided);
            } break;
            case ZoneEquip::ZoneDXDehumidifier: { // 'ZoneHVAC:Dehumidifier:DX'
                SimZoneDehumidifier(state,
                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                    ActualZoneNum,
                                    FirstHVACIteration,
                                    SysOutputProvided,
                                    LatOutputProvided,
                                    state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) += SysOutputProvided;

                SysOutputProvided = 0.0; // Reset to 0.0 since this equipment is controlled based on zone humidity level (not
                                         // temperature) SysOutputProvided amount was already sent above to
                                         // next Predict-Correct series of calcs via SysDepZoneLoads
            } break;
            case ZoneEquip::FanCoil4Pipe: { // 'ZoneHVAC:FourPipeFanCoil'
                SimFanCoilUnit(state,
                               state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                               ActualZoneNum,
                               ControlledZoneNum,
                               FirstHVACIteration,
                               SysOutputProvided,
                               LatOutputProvided,
                               state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::UnitVentilator: { // 'ZoneHVAC:UnitVentilator'
                SimUnitVentilator(state,
                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                  ActualZoneNum,
                                  FirstHVACIteration,
                                  SysOutputProvided,
                                  LatOutputProvided,
                                  state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::UnitHeater: { // 'ZoneHVAC:UnitHeater'
                SimUnitHeater(state,
                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                              ActualZoneNum,
                              FirstHVACIteration,
                              SysOutputProvided,
                              LatOutputProvided,
                              state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::PurchasedAir: { // 'ZoneHVAC:IdealLoadsAirSystem'
                SimPurchasedAir(state,
                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                SysOutputProvided,
                                LatOutputProvided,
                                FirstHVACIteration,
                                ControlledZoneNum,
                                ActualZoneNum,
                                state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::BBWater: { // 'ZoneHVAC:Baseboard:RadiantConvective:Water'
                SimHWBaseboard(state,
                               state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                               ActualZoneNum,
                               ControlledZoneNum,
                               FirstHVACIteration,
                               SysOutputProvided,
                               state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBSteam: { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
                SimSteamBaseboard(state,
                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                  ActualZoneNum,
                                  ControlledZoneNum,
                                  FirstHVACIteration,
                                  SysOutputProvided,
                                  state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBWaterConvective: { // 'ZoneHVAC:Baseboard:Convective:Water'
                SimBaseboard(state,
                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                             ActualZoneNum,
                             ControlledZoneNum,
                             FirstHVACIteration,
                             SysOutputProvided,
                             state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBElectricConvective: { // 'ZoneHVAC:Baseboard:Convective:Electric'
                SimElectricBaseboard(state,
                                     state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                     ActualZoneNum,
                                     ControlledZoneNum,
                                     SysOutputProvided,
                                     state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::CoolingPanel: { // 'ZoneHVAC:CoolingPanel:RadiantConvective:Water'
                SimCoolingPanel(state,
                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                ActualZoneNum,
                                ControlledZoneNum,
                                FirstHVACIteration,
                                SysOutputProvided,
                                state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This cooling panel does not add/remove any latent heat
            } break;
            case ZoneEquip::HiTempRadiant: { // 'ZoneHVAC:HighTemperatureRadiant'
                SimHighTempRadiantSystem(state,
                                         state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                         FirstHVACIteration,
                                         SysOutputProvided,
                                         state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard currently sends its latent heat gain directly to predictor/corrector
                                         // via SumLatentHTRadSys... so setting LatOutputProvided = 0.0
            } break;
            case ZoneEquip::LoTempRadiant: { // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow',
                                             // 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                // 'ZoneHVAC:LowTemperatureRadiant:Electric'
                SimLowTempRadiantSystem(state,
                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                        FirstHVACIteration,
                                        SysOutputProvided,
                                        state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::ZoneExhaustFan: { // 'Fan:ZoneExhaust'
                // Air loop system availability manager status only applies to PIU and exhaust fans
                // Check to see if System Availability Managers are asking for fans to cycle on or shut off
                // and set fan on/off flags accordingly.
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == CycleOnZoneFansOnly) {
                    TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == ForceOff) {
                    TurnFansOff = true;
                }

                Fans::SimulateFanComponents(state,
                                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                            FirstHVACIteration,
                                            state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                //            reset status flags for other zone equipment
                TurnFansOn = false;
                TurnFansOff = false;
            } break;
            case ZoneEquip::HeatXchngr: { // 'HeatExchanger:AirToAir:FlatPlate'
                SimHeatRecovery(state,
                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                FirstHVACIteration,
                                state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).EquipIndex(EquipPtr),
                                ContFanCycCoil);
            } break;
            case ZoneEquip::ERVStandAlone: { // 'ZoneHVAC:EnergyRecoveryVentilator'
                SimStandAloneERV(state,
                                 state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                 ActualZoneNum,
                                 FirstHVACIteration,
                                 SysOutputProvided,
                                 LatOutputProvided,
                                 state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::HPWaterHeater: { // 'WaterHeater:HeatPump:PumpedCondenser'
                                             //                        auto HPWH =
                //                        WaterThermalTanks::HeatPumpWaterHeaterData::factory(PrioritySimOrder(EquipTypeNum).EquipName);
                //                        PlantLocation A(0, 0, 0, 0);
                //                        Real64 curLoad = 0.0;
                //                        if (dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*> (HPWH)->StandAlone) {
                //                            dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*> (HPWH)->simulate(A, FirstHVACIteration,
                //                            curLoad, true); SysOutputProvided = dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*>
                //                            (HPWH)->HPWaterHeaterSensibleCapacity; LatOutputProvided =
                //                            dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*> (HPWH)->HPWaterHeaterLatentCapacity;
                //                        } else {
                //                            SysOutputProvided = dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*>
                //                            (HPWH)->HPWaterHeaterSensibleCapacity; LatOutputProvided =
                //                            dynamic_cast<WaterThermalTanks::HeatPumpWaterHeaterData*> (HPWH)->HPWaterHeaterLatentCapacity;
                //                        }

                SimHeatPumpWaterHeater(state,
                                       state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                       FirstHVACIteration,
                                       SysOutputProvided,
                                       LatOutputProvided,
                                       state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::VentilatedSlab: { // 'ZoneHVAC:VentilatedSlab'
                SimVentilatedSlab(state,
                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                  ActualZoneNum,
                                  FirstHVACIteration,
                                  SysOutputProvided,
                                  LatOutputProvided,
                                  state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::OutdoorAirUnit: { // 'ZoneHVAC:OutdoorAirUnit'
                OutdoorAirUnit::SimOutdoorAirUnit(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ActualZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::BBElectric: { // 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
                SimElecBaseboard(state,
                                 state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                 ActualZoneNum,
                                 ControlledZoneNum,
                                 FirstHVACIteration,
                                 SysOutputProvided,
                                 state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::RefrigerationAirChillerSet: { // 'ZoneHVAC:RefrigerationChillerSet'
                SimAirChillerSet(state,
                                 state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                 ActualZoneNum,
                                 FirstHVACIteration,
                                 SysOutputProvided,
                                 LatOutputProvided,
                                 state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));

                state.dataHeatBalFanSys->NonAirSystemResponse(ActualZoneNum) += SysOutputProvided;
            } break;
            case ZoneEquip::UserDefinedZoneHVACForcedAir: {
                SimZoneAirUserDefined(state,
                                      state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                      ActualZoneNum,
                                      SysOutputProvided,
                                      LatOutputProvided,
                                      state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::ZoneEvaporativeCoolerUnit: {
                SimZoneEvaporativeCoolerUnit(state,
                                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                             ActualZoneNum,
                                             SysOutputProvided,
                                             LatOutputProvided,
                                             state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::ZoneHybridEvaporativeCooler: {
                SimZoneHybridUnitaryAirConditioners(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ActualZoneNum,
                                                    SysOutputProvided,
                                                    LatOutputProvided,
                                                    state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).EquipIndex(EquipPtr));
            } break;
            default:
                break;
            }

            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneExh +=
                (state.dataHVACGlobal->UnbalExhMassFlow +
                 state.dataHVACGlobal->BalancedExhMassFlow); // This is the total "exhaust" flow from equipment such as a zone exhaust fan
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneExhBalanced += state.dataHVACGlobal->BalancedExhMassFlow;
            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).PlenumMassFlow += state.dataHVACGlobal->PlenumInducedMassFlow;

            // Store available capacities for load distribution calculations
            if (FirstHVACIteration &&
                (state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).LoadDistScheme != DataZoneEquipment::LoadDist::Sequential)) {
                if (SysOutputProvided > 0.0) {
                    state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).HeatingCapacity(EquipPtr) = SysOutputProvided;
                } else {
                    state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum).CoolingCapacity(EquipPtr) = SysOutputProvided;
                }
            }

            UpdateSystemOutputRequired(state, ActualZoneNum, SysOutputProvided, LatOutputProvided, EquipTypeNum);
            state.dataSize->CurTermUnitSizingNum = 0;
        } // zone loop
    }     // End of controlled zone loop
    state.dataSize->CurZoneEqNum = 0;
    state.dataZoneEquipmentManager->FirstPassZoneEquipFlag = false;

    // This is the call to the Supply Air Path after the components are simulated to update
    //  the path inlets

    // Process supply air path components in reverse order
    for (SupplyAirPathNum = 1; SupplyAirPathNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SupplyAirPathNum) {

        SupPathInletChanged = false;

        for (CompNum = state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).NumOfComponents; CompNum >= 1; --CompNum) {
            switch (state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentTypeEnum(CompNum)) {
            case DataZoneEquipment::AirLoopHVACZone::Splitter: { // 'AirLoopHVAC:ZoneSplitter'
                if (!(state.afn->AirflowNetworkFanActivated && state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
                    SimAirLoopSplitter(state,
                                       state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                       FirstHVACIteration,
                                       FirstCall,
                                       SupPathInletChanged,
                                       state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum));
                }
            } break;
            case DataZoneEquipment::AirLoopHVACZone::SupplyPlenum: { // 'AirLoopHVAC:SupplyPlenum'
                SimAirZonePlenum(state,
                                 state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                 DataZoneEquipment::AirLoopHVACZone::SupplyPlenum,
                                 state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum),
                                 FirstHVACIteration,
                                 FirstCall,
                                 SupPathInletChanged);

            } break;
            default: {
                ShowSevereError(state, "Error found in Supply Air Path=" + state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).Name);
                ShowContinueError(state,
                                  "Invalid Supply Air Path Component=" + state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentType(CompNum));
                ShowFatalError(state, "Preceding condition causes termination.");
            } break;
            }
        }

        if (SupPathInletChanged) {
            // If the supply air path inlet conditions have been changed, the Air Loop must be resimulated
            SimAir = true;
        }

    } // end of the Supply Air Path DO Loop

    ExhaustAirSystemManager::SimZoneHVACExhaustControls(state);

    ExhaustAirSystemManager::SimExhaustAirSystem(state, FirstHVACIteration);

    CalcZoneMassBalance(state, FirstHVACIteration);

    CalcZoneLeavingConditions(state, FirstHVACIteration);

    SimReturnAirPath(state);
}

void SetZoneEquipSimOrder(EnergyPlusData &state, int const ControlledZoneNum, int const ActualZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set simulation priorities based on user specified priorities and
    // required conditions (heating or cooling).

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CurEqHeatingPriority; // Used to make sure "optimization features" on compilers don't defeat purpose of this routine
    int CurEqCoolingPriority; // Used to make sure "optimization features" on compilers don't defeat purpose of this routine

    auto &zeq(state.dataZoneEquip->ZoneEquipList(ControlledZoneNum));
    int const NumOfEquipTypes(zeq.NumOfEquipTypes);
    for (int EquipTypeNum = 1; EquipTypeNum <= NumOfEquipTypes; ++EquipTypeNum) {
        auto &pso(state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum));
        pso.EquipType = zeq.EquipType(EquipTypeNum);
        pso.EquipName = zeq.EquipName(EquipTypeNum);
        pso.EquipTypeEnum = zeq.EquipTypeEnum(EquipTypeNum);
        pso.CoolingPriority = zeq.CoolingPriority(EquipTypeNum);
        pso.HeatingPriority = zeq.HeatingPriority(EquipTypeNum);
        pso.EquipPtr = EquipTypeNum;
    }
    for (int EquipTypeNum = NumOfEquipTypes + 1, EquipTypeNum_end = state.dataZoneEquipmentManager->PrioritySimOrder.u();
         EquipTypeNum <= EquipTypeNum_end;
         ++EquipTypeNum) { // Reset unused upper array portion
        auto &pso(state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum));
        pso.EquipType.clear();
        pso.EquipName.clear();
        pso.EquipTypeEnum = 0;
        pso.EquipPtr = 0;
    }

    for (int EquipTypeNum = 1; EquipTypeNum <= NumOfEquipTypes; ++EquipTypeNum) {
        auto &pso(state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum));

        CurEqHeatingPriority = pso.HeatingPriority;
        CurEqCoolingPriority = pso.CoolingPriority;

        for (int ComparedEquipTypeNum = EquipTypeNum; ComparedEquipTypeNum <= NumOfEquipTypes; ++ComparedEquipTypeNum) {
            auto &psc(state.dataZoneEquipmentManager->PrioritySimOrder(ComparedEquipTypeNum));

            if ((CurEqCoolingPriority > psc.CoolingPriority &&
                 state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired < 0.0) ||
                (CurEqHeatingPriority > psc.HeatingPriority &&
                 state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired >= 0.0)) {

                // Tuned C++ string swap avoids copying
                pso.EquipType.swap(psc.EquipType);
                pso.EquipName.swap(psc.EquipName);
                std::swap(pso.EquipPtr, psc.EquipPtr);
                std::swap(pso.EquipTypeEnum, psc.EquipTypeEnum);
                std::swap(pso.CoolingPriority, psc.CoolingPriority);
                std::swap(pso.HeatingPriority, psc.HeatingPriority);

                CurEqCoolingPriority = pso.CoolingPriority;
                CurEqHeatingPriority = pso.HeatingPriority;
            }
        }
    }
}

void InitSystemOutputRequired(EnergyPlusData &state, int const ZoneNum, bool const FirstHVACIteration, bool const ResetSimOrder)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       Don Shirey, Aug 2009 (latent/moisture additions)

    // PURPOSE OF THIS SUBROUTINE:
    // Initialize remaining output required variables

    // METHODOLOGY EMPLOYED:
    // Initialize remaining output variables using predictor calculations

    auto &energy(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum));
    auto &moisture(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum));

    energy.RemainingOutputRequired = energy.TotalOutputRequired;
    energy.UnadjRemainingOutputRequired = energy.TotalOutputRequired;
    energy.RemainingOutputReqToHeatSP = energy.OutputRequiredToHeatingSP;
    energy.UnadjRemainingOutputReqToHeatSP = energy.OutputRequiredToHeatingSP;
    energy.RemainingOutputReqToCoolSP = energy.OutputRequiredToCoolingSP;
    energy.UnadjRemainingOutputReqToCoolSP = energy.OutputRequiredToCoolingSP;

    moisture.RemainingOutputRequired = moisture.TotalOutputRequired;
    moisture.UnadjRemainingOutputRequired = moisture.TotalOutputRequired;
    moisture.RemainingOutputReqToHumidSP = moisture.OutputRequiredToHumidifyingSP;
    moisture.UnadjRemainingOutputReqToHumidSP = moisture.OutputRequiredToHumidifyingSP;
    moisture.RemainingOutputReqToDehumidSP = moisture.OutputRequiredToDehumidifyingSP;
    moisture.UnadjRemainingOutputReqToDehumidSP = moisture.OutputRequiredToDehumidifyingSP;

    if (ResetSimOrder) {
        const int ControlledZoneNum = [&] {
            for (int i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                if (state.dataZoneEquip->ZoneEquipConfig(i).ActualZoneNum == ZoneNum) return i;
            }
            return 0;
        }();
        SetZoneEquipSimOrder(state, ControlledZoneNum, ZoneNum);
    }

    // If one sequenced load is allocated, then all have been allocated in InitZoneEquipment
    if (allocated(energy.SequencedOutputRequired)) {
        // Check if controlled first, because if it's not, there is no zone equipment list
        if (!state.dataHeatBal->Zone(ZoneNum).IsControlled || state.dataGlobal->ZoneSizingCalc) {
            // init each sequenced demand to the full output
            energy.SequencedOutputRequired = energy.TotalOutputRequired;                            // array assignment
            energy.SequencedOutputRequiredToHeatingSP = energy.OutputRequiredToHeatingSP;           // array assignment
            energy.SequencedOutputRequiredToCoolingSP = energy.OutputRequiredToCoolingSP;           // array assignment
                                                                                                    // init each sequenced demand to the full output
            moisture.SequencedOutputRequired = moisture.TotalOutputRequired;                        // array assignment
            moisture.SequencedOutputRequiredToHumidSP = moisture.OutputRequiredToHumidifyingSP;     // array assignment
            moisture.SequencedOutputRequiredToDehumidSP = moisture.OutputRequiredToDehumidifyingSP; // array assignment
        } else if (FirstHVACIteration) {
            auto loadDistType = state.dataZoneEquip->ZoneEquipList(state.dataHeatBal->Zone(ZoneNum).ZoneEqNum).LoadDistScheme;
            if ((loadDistType == DataZoneEquipment::LoadDist::Sequential) || (loadDistType == DataZoneEquipment::LoadDist::Uniform)) {
                // init each sequenced demand to the full output
                energy.SequencedOutputRequired = energy.TotalOutputRequired;                        // array assignment
                energy.SequencedOutputRequiredToHeatingSP = energy.OutputRequiredToHeatingSP;       // array assignment
                energy.SequencedOutputRequiredToCoolingSP = energy.OutputRequiredToCoolingSP;       // array assignment
                                                                                                    // init each sequenced demand to the full output
                moisture.SequencedOutputRequired = moisture.TotalOutputRequired;                    // array assignment
                moisture.SequencedOutputRequiredToHumidSP = moisture.OutputRequiredToHumidifyingSP; // array assignment
                moisture.SequencedOutputRequiredToDehumidSP = moisture.OutputRequiredToDehumidifyingSP; // array assignment
            } else if ((loadDistType == DataZoneEquipment::LoadDist::UniformPLR) ||
                       (loadDistType == DataZoneEquipment::LoadDist::SequentialUniformPLR)) {
                // init each sequenced demand to the zone design load in order to get available capacities from equipment
                if (energy.TotalOutputRequired >= 0.0) {
                    energy.SequencedOutputRequired = state.dataSize->FinalZoneSizing(ZoneNum).DesHeatLoad; // array assignment
                } else {
                    energy.SequencedOutputRequired = -state.dataSize->FinalZoneSizing(ZoneNum).DesCoolLoad; // array assignment
                }
                if (energy.TotalOutputRequired >= 0.0) {
                    energy.SequencedOutputRequiredToHeatingSP = state.dataSize->FinalZoneSizing(ZoneNum).DesHeatLoad; // array assignment
                } else {
                    energy.SequencedOutputRequiredToHeatingSP = -state.dataSize->FinalZoneSizing(ZoneNum).DesCoolLoad; // array assignment
                }
                if (energy.TotalOutputRequired >= 0.0) {
                    energy.SequencedOutputRequiredToCoolingSP = state.dataSize->FinalZoneSizing(ZoneNum).DesHeatLoad; // array assignment
                } else {
                    energy.SequencedOutputRequiredToCoolingSP = -state.dataSize->FinalZoneSizing(ZoneNum).DesCoolLoad; // array assignment
                }
                // init each sequenced moisture demand to the full output
                moisture.SequencedOutputRequired = moisture.TotalOutputRequired;                        // array assignment
                moisture.SequencedOutputRequiredToHumidSP = moisture.OutputRequiredToHumidifyingSP;     // array assignment
                moisture.SequencedOutputRequiredToDehumidSP = moisture.OutputRequiredToDehumidifyingSP; // array assignment
            }
        } else {
            // init first sequenced sensible demand to the full output
            energy.SequencedOutputRequired(1) = energy.TotalOutputRequired;
            energy.SequencedOutputRequiredToHeatingSP(1) = energy.OutputRequiredToHeatingSP;
            energy.SequencedOutputRequiredToCoolingSP(1) = energy.OutputRequiredToCoolingSP;
            // init first sequenced moisture demand to the full output
            moisture.SequencedOutputRequired(1) = moisture.TotalOutputRequired;
            moisture.SequencedOutputRequiredToHumidSP(1) = moisture.OutputRequiredToHumidifyingSP;
            moisture.SequencedOutputRequiredToDehumidSP(1) = moisture.OutputRequiredToDehumidifyingSP;
        }
    }

    state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum);

    DistributeSystemOutputRequired(state, ZoneNum, FirstHVACIteration);
}

void DistributeSystemOutputRequired(EnergyPlusData &state, int const ActualZoneNum, bool const FirstHVACIteration)
{
    // Distribute zone equipment loads according to load distribution scheme

    // Do nothing if this zone is uncontrolled or doing zone sizing
    if (!state.dataHeatBal->Zone(ActualZoneNum).IsControlled) return;
    if (state.dataGlobal->ZoneSizingCalc) return;

    int ctrlZoneNum = state.dataHeatBal->Zone(ActualZoneNum).ZoneEqNum;
    // Do nothing on FirstHVACIteration if not UniformLoading and not SequentialLoading
    if (FirstHVACIteration && (state.dataZoneEquip->ZoneEquipList(ctrlZoneNum).LoadDistScheme != DataZoneEquipment::LoadDist::Uniform) &&
        (state.dataZoneEquip->ZoneEquipList(ctrlZoneNum).LoadDistScheme != DataZoneEquipment::LoadDist::Sequential)) {
        return;
    }

    auto &energy(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum));
    auto &moisture(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum));
    auto &thisZEqList(state.dataZoneEquip->ZoneEquipList(ctrlZoneNum));
    Real64 heatLoadRatio = 1.0;
    Real64 coolLoadRatio = 1.0;
    Real64 availCap = 0.0;
    Real64 plr = 1.0;
    int numOperating = 0;

    switch (thisZEqList.LoadDistScheme) {
    case DataZoneEquipment::LoadDist::Sequential:
        // Nothing to do here for this case
        {
            // Set the load (with load fraction) for the first equipment in priority order
            constexpr int priorityNum = 1;
            const int &equipNum = state.dataZoneEquipmentManager->PrioritySimOrder(priorityNum).EquipPtr;

            // Determine whether we're heating or cooling and choose the appropriate fraction
            const Real64 heatLoadRatio = thisZEqList.SequentialHeatingFraction(state, equipNum);
            const Real64 coolLoadRatio = thisZEqList.SequentialCoolingFraction(state, equipNum);
            const Real64 loadRatio = (energy.TotalOutputRequired >= 0.0) ? heatLoadRatio : coolLoadRatio;

            // Energy loads
            energy.SequencedOutputRequired(priorityNum) = energy.TotalOutputRequired * loadRatio;
            energy.SequencedOutputRequiredToHeatingSP(priorityNum) = energy.OutputRequiredToHeatingSP * loadRatio;
            energy.SequencedOutputRequiredToCoolingSP(priorityNum) = energy.OutputRequiredToCoolingSP * loadRatio;
            energy.RemainingOutputRequired = energy.SequencedOutputRequired(priorityNum);
            energy.RemainingOutputReqToHeatSP = energy.SequencedOutputRequiredToHeatingSP(priorityNum);
            energy.RemainingOutputReqToCoolSP = energy.SequencedOutputRequiredToCoolingSP(priorityNum);

            // Moisture loads
            moisture.SequencedOutputRequired(priorityNum) = moisture.TotalOutputRequired * loadRatio;
            moisture.SequencedOutputRequiredToHumidSP(priorityNum) = moisture.OutputRequiredToHumidifyingSP * loadRatio;
            moisture.SequencedOutputRequiredToDehumidSP(priorityNum) = moisture.OutputRequiredToDehumidifyingSP * loadRatio;
            moisture.RemainingOutputRequired = moisture.SequencedOutputRequired(priorityNum);
            moisture.RemainingOutputReqToHumidSP = moisture.SequencedOutputRequiredToHumidSP(priorityNum);
            moisture.RemainingOutputReqToDehumidSP = moisture.SequencedOutputRequiredToDehumidSP(priorityNum);

            break;
        }
    case DataZoneEquipment::LoadDist::Uniform:
        // Distribute load uniformly across all active equipment
        if (thisZEqList.NumAvailHeatEquip > 0) {
            heatLoadRatio = 1.0 / thisZEqList.NumAvailHeatEquip;
        } else {
            heatLoadRatio = 1.0;
        }
        if (thisZEqList.NumAvailCoolEquip > 0) {
            coolLoadRatio = 1.0 / thisZEqList.NumAvailCoolEquip;
        } else {
            coolLoadRatio = 1.0;
        }
        for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
            if (energy.TotalOutputRequired >= 0.0) {
                if (thisZEqList.HeatingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = energy.TotalOutputRequired * heatLoadRatio;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = energy.OutputRequiredToHeatingSP * heatLoadRatio;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = energy.OutputRequiredToCoolingSP * heatLoadRatio;
                    moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * heatLoadRatio;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = moisture.OutputRequiredToHumidifyingSP * heatLoadRatio;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP * heatLoadRatio;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            } else {
                if (thisZEqList.CoolingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = energy.TotalOutputRequired * coolLoadRatio;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = energy.OutputRequiredToHeatingSP * coolLoadRatio;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = energy.OutputRequiredToCoolingSP * coolLoadRatio;
                    moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * coolLoadRatio;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = moisture.OutputRequiredToHumidifyingSP * coolLoadRatio;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP * coolLoadRatio;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            }
        }
        break;
    case DataZoneEquipment::LoadDist::UniformPLR:
        // Distribute load at uniform PLR across all active equipment
        if (energy.TotalOutputRequired >= 0.0) {
            for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
                if (thisZEqList.HeatingPriority(equipNum) > 0) availCap += thisZEqList.HeatingCapacity(equipNum);
            }
            if (availCap > 0.0) {
                plr = energy.TotalOutputRequired / availCap;
            } else {
                plr = 0.0;
            }
        } else {
            for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
                if (thisZEqList.CoolingPriority(equipNum) > 0) availCap += thisZEqList.CoolingCapacity(equipNum);
            }
            if (availCap < 0.0) {
                plr = energy.TotalOutputRequired / availCap;
            } else {
                plr = 0.0;
            }
        }
        if (plr <= 0.0) break; // Don't change anything
        for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
            if (energy.TotalOutputRequired >= 0.0) {
                if (thisZEqList.HeatingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    if (energy.OutputRequiredToHeatingSP != 0.0) {
                        moisture.SequencedOutputRequired(equipNum) =
                            moisture.TotalOutputRequired * (thisZEqList.HeatingCapacity(equipNum) * plr) / energy.OutputRequiredToHeatingSP;
                        moisture.SequencedOutputRequiredToHumidSP(equipNum) =
                            moisture.OutputRequiredToHumidifyingSP * (thisZEqList.HeatingCapacity(equipNum) * plr) / energy.OutputRequiredToHeatingSP;
                    } else {
                        moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * plr;
                        moisture.SequencedOutputRequiredToHumidSP(equipNum) = moisture.OutputRequiredToHumidifyingSP * plr;
                    }
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            } else {
                if (thisZEqList.CoolingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    if (energy.OutputRequiredToCoolingSP != 0.0) {
                        moisture.SequencedOutputRequired(equipNum) =
                            moisture.TotalOutputRequired * (thisZEqList.CoolingCapacity(equipNum) * plr) / energy.OutputRequiredToCoolingSP;
                        moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP *
                                                                                (thisZEqList.CoolingCapacity(equipNum) * plr) /
                                                                                energy.OutputRequiredToCoolingSP;
                    } else {
                        moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * plr;
                        moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP * plr;
                    }
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            }
        }
        break;
    case DataZoneEquipment::LoadDist::SequentialUniformPLR:
        // Determine how many pieces of equipment are required to meet the current load,
        // then distribute load at uniform PLR across all active equipment
        if (energy.TotalOutputRequired >= 0.0) {
            // For heating capacities and TotalOutputRequired are positive
            for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
                if ((thisZEqList.HeatingCapacity(equipNum) > 0.0) && (availCap < energy.TotalOutputRequired)) {
                    if (thisZEqList.HeatingPriority(equipNum) > 0) availCap += thisZEqList.HeatingCapacity(equipNum);
                    ++numOperating;
                }
            }
            if (availCap > 0.0) {
                plr = energy.TotalOutputRequired / availCap;
            } else {
                plr = 0.0;
                numOperating = 0;
            }
        } else {
            for (int equipNum = 1.0; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
                // For cooling capacities and TotalOutputRequired are negative
                if ((thisZEqList.CoolingCapacity(equipNum) < 0.0) && (availCap > energy.TotalOutputRequired)) {
                    if (thisZEqList.CoolingPriority(equipNum) > 0) availCap += thisZEqList.CoolingCapacity(equipNum);
                    ++numOperating;
                }
            }
            if (availCap < 0.0) {
                plr = energy.TotalOutputRequired / availCap;
            } else {
                plr = 0.0;
                numOperating = 0;
            }
        }
        if (plr <= 0.0) break; // Don't change anything
        // Set loads for operating equipment
        for (int equipNum = 1.0; equipNum <= numOperating; ++equipNum) {
            if (energy.TotalOutputRequired >= 0.0) {
                if (thisZEqList.HeatingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = thisZEqList.HeatingCapacity(equipNum) * plr;
                    if (energy.OutputRequiredToHeatingSP != 0.0) {
                        moisture.SequencedOutputRequired(equipNum) =
                            moisture.TotalOutputRequired * (thisZEqList.HeatingCapacity(equipNum) * plr) / energy.OutputRequiredToHeatingSP;
                        moisture.SequencedOutputRequiredToHumidSP(equipNum) =
                            moisture.OutputRequiredToHumidifyingSP * (thisZEqList.HeatingCapacity(equipNum) * plr) / energy.OutputRequiredToHeatingSP;
                    } else {
                        moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * plr;
                        moisture.SequencedOutputRequiredToHumidSP(equipNum) = moisture.OutputRequiredToHumidifyingSP * plr;
                    }
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            } else {
                if (thisZEqList.CoolingPriority(equipNum) > 0) {
                    energy.SequencedOutputRequired(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = thisZEqList.CoolingCapacity(equipNum) * plr;
                    if (energy.OutputRequiredToCoolingSP != 0.0) {
                        moisture.SequencedOutputRequired(equipNum) =
                            moisture.TotalOutputRequired * (thisZEqList.CoolingCapacity(equipNum) * plr) / energy.OutputRequiredToCoolingSP;
                        moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP *
                                                                                (thisZEqList.CoolingCapacity(equipNum) * plr) /
                                                                                energy.OutputRequiredToCoolingSP;
                    } else {
                        moisture.SequencedOutputRequired(equipNum) = moisture.TotalOutputRequired * plr;
                        moisture.SequencedOutputRequiredToDehumidSP(equipNum) = moisture.OutputRequiredToDehumidifyingSP * plr;
                    }
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                } else {
                    energy.SequencedOutputRequired(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
                    energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequired(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
                    moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
                }
            }
        }
        // Set loads to zero for remaining equipment
        for (int equipNum = numOperating + 1; equipNum <= thisZEqList.NumOfEquipTypes; ++equipNum) {
            energy.SequencedOutputRequired(equipNum) = 0.0;
            energy.SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
            energy.SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
            moisture.SequencedOutputRequired(equipNum) = 0.0;
            moisture.SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
            moisture.SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
        }
        break;
    default:
        ShowFatalError(state, "DistributeSystemOutputRequired: Illegal load distribution scheme type.");
        break;
    }
    // For every load distribution scheme except SequentialLoad
    //  set the remaining loads to the first equipment type's load to support equipment types that don't use the sequenced loads
    if (thisZEqList.LoadDistScheme != DataZoneEquipment::LoadDist::Sequential) {
        energy.RemainingOutputRequired = energy.SequencedOutputRequired(1);
        moisture.RemainingOutputRequired = moisture.SequencedOutputRequired(1);
        energy.RemainingOutputReqToHeatSP = energy.SequencedOutputRequiredToHeatingSP(1);
        moisture.RemainingOutputReqToHumidSP = moisture.SequencedOutputRequiredToHumidSP(1);
        energy.RemainingOutputReqToCoolSP = energy.SequencedOutputRequiredToCoolingSP(1);
        moisture.RemainingOutputReqToDehumidSP = moisture.SequencedOutputRequiredToDehumidSP(1);
    }
}

void UpdateSystemOutputRequired(EnergyPlusData &state,
                                int const ZoneNum,
                                Real64 const SysOutputProvided,     // sensible output provided by zone equipment (W)
                                Real64 const LatOutputProvided,     // latent output provided by zone equipment (kg/s)
                                Optional_int_const EquipPriorityNum // index in PrioritySimOrder for this update
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Unknown
    //       MODIFIED       B. Griffith Sept 2011, add storage of requirements by sequence

    int ctrlZoneNum = state.dataHeatBal->Zone(ZoneNum).ZoneEqNum;
    auto &energy(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum));
    auto &moisture(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum));

    // If zone is uncontrolled use original method for remaining output
    if (!state.dataHeatBal->Zone(ZoneNum).IsControlled) {
        // SequentialLoading, use original method for remaining output
        energy.UnadjRemainingOutputRequired -= SysOutputProvided;
        energy.RemainingOutputRequired = energy.UnadjRemainingOutputRequired;
        energy.UnadjRemainingOutputReqToHeatSP -= SysOutputProvided;
        energy.RemainingOutputReqToHeatSP = energy.UnadjRemainingOutputReqToHeatSP;
        energy.UnadjRemainingOutputReqToCoolSP -= SysOutputProvided;
        energy.RemainingOutputReqToCoolSP = energy.UnadjRemainingOutputReqToCoolSP;
        // Latent output updates
        moisture.UnadjRemainingOutputRequired -= LatOutputProvided;
        moisture.RemainingOutputRequired = moisture.UnadjRemainingOutputRequired;
        moisture.UnadjRemainingOutputReqToHumidSP -= LatOutputProvided;
        moisture.RemainingOutputReqToHumidSP = moisture.UnadjRemainingOutputReqToHumidSP;
        moisture.UnadjRemainingOutputReqToDehumidSP -= LatOutputProvided;
        moisture.RemainingOutputReqToDehumidSP = moisture.UnadjRemainingOutputReqToDehumidSP;

        // re-evaluate if loads are now such that in dead band or set back
        switch (state.dataHeatBalFanSys->TempControlType(ZoneNum)) {
        case DataHVACGlobals::ThermostatType::Uncontrolled:
            // uncontrolled zone; shouldn't ever get here, but who knows
            state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            break;
        case DataHVACGlobals::ThermostatType::SingleHeating:
            if ((energy.RemainingOutputRequired - 1.0) < 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::SingleCooling:
            if ((energy.RemainingOutputRequired + 1.0) > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::SingleHeatCool:
            if (energy.RemainingOutputReqToHeatSP < 0.0 && energy.RemainingOutputReqToCoolSP > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
            if (energy.RemainingOutputReqToHeatSP < 0.0 && energy.RemainingOutputReqToCoolSP > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        default:
            break;
        }

        if (present(EquipPriorityNum)) {
            // now store remaining load at the by sequence level
            if (EquipPriorityNum + 1 <= energy.NumZoneEquipment) {
                energy.SequencedOutputRequired(EquipPriorityNum + 1) = energy.RemainingOutputRequired;
                moisture.SequencedOutputRequired(EquipPriorityNum + 1) = moisture.RemainingOutputRequired;
            }

            if (state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1 <= energy.NumZoneEquipment) {
                energy.SequencedOutputRequiredToHeatingSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1) =
                    energy.RemainingOutputReqToHeatSP;
                moisture.SequencedOutputRequiredToHumidSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1) =
                    moisture.RemainingOutputReqToHumidSP;
            }
            if (state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1 <= energy.NumZoneEquipment) {
                energy.SequencedOutputRequiredToCoolingSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1) =
                    energy.RemainingOutputReqToCoolSP;
                moisture.SequencedOutputRequiredToDehumidSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1) =
                    moisture.RemainingOutputReqToDehumidSP;
            }
        }
        return;
    }

    // Sensible output updates
    auto &thisZEqList(state.dataZoneEquip->ZoneEquipList(ctrlZoneNum));
    switch (thisZEqList.LoadDistScheme) {
    case DataZoneEquipment::LoadDist::Sequential: {
        // Subtract the system output from the unadjusted loads required
        energy.UnadjRemainingOutputRequired -= SysOutputProvided;
        energy.UnadjRemainingOutputReqToHeatSP -= SysOutputProvided;
        energy.UnadjRemainingOutputReqToCoolSP -= SysOutputProvided;
        moisture.UnadjRemainingOutputRequired -= LatOutputProvided;
        moisture.UnadjRemainingOutputReqToHumidSP -= LatOutputProvided;
        moisture.UnadjRemainingOutputReqToDehumidSP -= LatOutputProvided;

        if (present(EquipPriorityNum) && EquipPriorityNum < thisZEqList.NumOfEquipTypes) {

            // Look up the next system in priority order
            int nextEquipPriorityNum = EquipPriorityNum + 1;
            const int &nextSystem = state.dataZoneEquipmentManager->PrioritySimOrder(nextEquipPriorityNum).EquipPtr;

            // Determine the load ratio based on whether we're heating or cooling
            const Real64 loadRatio = (energy.TotalOutputRequired >= 0.0) ? thisZEqList.SequentialHeatingFraction(state, nextSystem)
                                                                         : thisZEqList.SequentialCoolingFraction(state, nextSystem);

            // Update the zone energy demands
            energy.RemainingOutputRequired = loadRatio * energy.UnadjRemainingOutputRequired;
            energy.RemainingOutputReqToHeatSP = loadRatio * energy.UnadjRemainingOutputReqToHeatSP;
            energy.RemainingOutputReqToCoolSP = loadRatio * energy.UnadjRemainingOutputReqToCoolSP;
            moisture.RemainingOutputRequired = loadRatio * moisture.UnadjRemainingOutputRequired;
            moisture.RemainingOutputReqToHumidSP = loadRatio * moisture.UnadjRemainingOutputReqToHumidSP;
            moisture.RemainingOutputReqToDehumidSP = loadRatio * moisture.UnadjRemainingOutputReqToDehumidSP;

            // now store remaining load at the sequence level
            energy.SequencedOutputRequired(nextEquipPriorityNum) = energy.RemainingOutputRequired;
            energy.SequencedOutputRequiredToHeatingSP(nextEquipPriorityNum) = energy.RemainingOutputReqToHeatSP;
            energy.SequencedOutputRequiredToCoolingSP(nextEquipPriorityNum) = energy.RemainingOutputReqToCoolSP;
            moisture.SequencedOutputRequired(nextEquipPriorityNum) = moisture.RemainingOutputRequired;
            moisture.SequencedOutputRequiredToHumidSP(nextEquipPriorityNum) = moisture.RemainingOutputReqToHumidSP;
            moisture.SequencedOutputRequiredToDehumidSP(nextEquipPriorityNum) = moisture.RemainingOutputReqToDehumidSP;
        } else {
            // SequentialLoading, use original method for remaining output
            energy.RemainingOutputRequired = energy.UnadjRemainingOutputRequired;
            energy.RemainingOutputReqToHeatSP = energy.UnadjRemainingOutputReqToHeatSP;
            energy.RemainingOutputReqToCoolSP = energy.UnadjRemainingOutputReqToCoolSP;
            // Latent output updates
            moisture.RemainingOutputRequired = moisture.UnadjRemainingOutputRequired;
            moisture.RemainingOutputReqToHumidSP = moisture.UnadjRemainingOutputReqToHumidSP;
            moisture.RemainingOutputReqToDehumidSP = moisture.UnadjRemainingOutputReqToDehumidSP;
        }

        // re-evaluate if loads are now such that in dead band or set back
        switch (state.dataHeatBalFanSys->TempControlType(ZoneNum)) {
        case DataHVACGlobals::ThermostatType::Uncontrolled:
            // uncontrolled zone; shouldn't ever get here, but who knows
            state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            break;
        case DataHVACGlobals::ThermostatType::SingleHeating:
            if ((energy.RemainingOutputRequired - 1.0) < 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::SingleCooling:
            if ((energy.RemainingOutputRequired + 1.0) > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::SingleHeatCool:
            if (energy.RemainingOutputReqToHeatSP < 0.0 && energy.RemainingOutputReqToCoolSP > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
            if (energy.RemainingOutputReqToHeatSP < 0.0 && energy.RemainingOutputReqToCoolSP > 0.0) {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = true;
            } else {
                state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = false;
            }
            break;
        default:
            break;
        }

    } break;
    case DataZoneEquipment::LoadDist::Uniform:
    case DataZoneEquipment::LoadDist::UniformPLR:
    case DataZoneEquipment::LoadDist::SequentialUniformPLR:
        // For every load distribution scheme except SequentialLoad, do not touch the sequenced loads,
        // but set the remaining loads to the next equipment type's load to support equipment types that don't use the sequenced loads
        if (present(EquipPriorityNum)) {
            if (EquipPriorityNum + 1 <= energy.NumZoneEquipment) {
                energy.RemainingOutputRequired = energy.SequencedOutputRequired(EquipPriorityNum + 1);
                moisture.RemainingOutputRequired = moisture.SequencedOutputRequired(EquipPriorityNum + 1);
            }

            if (state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1 <= energy.NumZoneEquipment) {
                energy.RemainingOutputReqToHeatSP =
                    energy.SequencedOutputRequiredToHeatingSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1);
                moisture.RemainingOutputReqToHumidSP =
                    moisture.SequencedOutputRequiredToHumidSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).HeatingPriority + 1);
            }
            if (state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1 <= energy.NumZoneEquipment) {
                energy.RemainingOutputReqToCoolSP =
                    energy.SequencedOutputRequiredToCoolingSP(state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1);
                moisture.RemainingOutputReqToDehumidSP = moisture.SequencedOutputRequiredToDehumidSP(
                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipPriorityNum).CoolingPriority + 1);
            }
        }
        break;
    default:
        ShowFatalError(state, "UpdateSystemOutputRequired: Illegal load distribution scheme type.");
        break;
    }
}

void CalcZoneMassBalance(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997

    // PURPOSE OF THIS SUBROUTINE:
    // Perform zone mass balance to get outlet air flow conditions.

    // METHODOLOGY EMPLOYED:
    // Mass continuity equation.

    using namespace DataRoomAirModel; // UCSD
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    using DataHVACGlobals::SmallMassFlow;
    auto &ZoneMassBalanceHVACReSim = state.dataHVACGlobal->ZoneMassBalanceHVACReSim;
    using ScheduleManager::GetCurrentScheduleValue;

    int constexpr IterMax(25);
    Real64 constexpr ConvergenceTolerance(0.000010);

    int NodeNum;
    int ZoneNode; // zone air node number
    Real64 TotInletAirMassFlowRateMax;
    Real64 TotInletAirMassFlowRateMaxAvail;
    Real64 TotInletAirMassFlowRateMin;
    Real64 TotInletAirMassFlowRateMinAvail;
    Real64 TotInletAirMassFlowRate;
    Real64 TotExhaustAirMassFlowRate;

    Real64 ZoneMixingAirMassFlowRate;
    Real64 ZoneMixingNetAirMassFlowRate;
    Real64 ZoneMixMassFlowRate;
    Real64 ZoneMixingAirMassFlowRatePrevious;
    Real64 ZoneReturnAirMassFlowRate;
    Real64 ZoneInfiltrationMassFlowRate;
    Real64 BuildingZoneMixingFlowOld;
    Real64 BuildingZoneMixingFlow;
    Real64 StdTotalReturnMassFlow;
    int Iteration;
    int ZoneNum1;

    Real64 BuildingZoneReturnFlow;
    Real64 BuildingZoneReturnFlowOld;

    ZoneMassBalanceHVACReSim = false;
    Iteration = 0;
    BuildingZoneMixingFlow = 0.0;
    BuildingZoneMixingFlowOld = 0.0;
    BuildingZoneReturnFlow = 0.0;
    BuildingZoneReturnFlowOld = 0.0;

    auto &Node(state.dataLoopNodes->Node);

    // Total loop supply and recirc flows (these have been zeroed earlier in InitZoneEquipment
    for (int airDistUnit = 1; airDistUnit <= (int)state.dataDefineEquipment->AirDistUnit.size(); ++airDistUnit) {
        int airLoop = state.dataDefineEquipment->AirDistUnit(airDistUnit).AirLoopNum;
        if (airLoop > 0) {
            state.dataAirLoop->AirLoopFlow(airLoop).SupFlow += state.dataDefineEquipment->AirDistUnit(airDistUnit).MassFlowRateSup;
            state.dataAirLoop->AirLoopFlow(airLoop).RecircFlow += state.dataDefineEquipment->AirDistUnit(airDistUnit).MassFlowRatePlenInd;
            state.dataAirLoop->AirLoopFlow(airLoop).LeakFlow += state.dataDefineEquipment->AirDistUnit(airDistUnit).MassFlowRateDnStrLk +
                                                                state.dataDefineEquipment->AirDistUnit(airDistUnit).MassFlowRateUpStrLk;
        }
    }

    // Set max OA flow and frac for systems which are all OA (no OASys)
    for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
        if (state.dataAirSystemsData->PrimaryAirSystems(airLoop).isAllOA) {
            state.dataAirLoop->AirLoopFlow(airLoop).MaxOutAir = state.dataAirLoop->AirLoopFlow(airLoop).SupFlow;
            state.dataAirLoop->AirLoopFlow(airLoop).OAFlow = state.dataAirLoop->AirLoopFlow(airLoop).SupFlow;
            state.dataAirLoop->AirLoopFlow(airLoop).OAFrac = 1.0;
        }
    }

    do {
        if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
            // These are also reset in ZoneEquipmentManager::InitZoneEquipment, reset again here for each zone mass balance iteration
            for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
                state.dataAirLoop->AirLoopFlow(airLoop).ZoneRetFlow = 0.0;
                state.dataAirLoop->AirLoopFlow(airLoop).SysRetFlow = 0.0;
                state.dataAirLoop->AirLoopFlow(airLoop).ExcessZoneExhFlow = 0.0;
            }
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;
                state.dataHeatBalFanSys->ZoneInfiltrationFlag(ZoneNum) = false;
                state.dataHeatBal->MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 0.0;
                state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate = 0.0;
                state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExcessZoneExh = 0.0;
            }
        }
        BuildingZoneMixingFlowOld = BuildingZoneMixingFlow;
        BuildingZoneMixingFlow = 0.0;

        BuildingZoneReturnFlowOld = BuildingZoneReturnFlow;
        BuildingZoneReturnFlow = 0.0;

        for (ZoneNum1 = 1; ZoneNum1 <= state.dataGlobal->NumOfZones; ++ZoneNum1) {
            int ZoneNum = ZoneNum1;
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) ZoneNum = state.dataHeatBalFanSys->ZoneReOrder(ZoneNum1);

            if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;

            state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotInletAirMassFlowRate = 0.0;
            TotInletAirMassFlowRateMax = 0.0;
            TotInletAirMassFlowRateMaxAvail = 0.0;
            TotInletAirMassFlowRateMin = 0.0;
            TotInletAirMassFlowRateMinAvail = 0.0;
            TotInletAirMassFlowRate = 0.0;
            TotExhaustAirMassFlowRate = 0.0;

            state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotExhaustAirMassFlowRate = 0.0;

            ZoneMixingAirMassFlowRate = 0.0;
            ZoneMixingNetAirMassFlowRate = 0.0;
            ZoneMixMassFlowRate = 0.0;
            ZoneReturnAirMassFlowRate = 0.0;
            ZoneInfiltrationMassFlowRate = 0.0;
            ZoneMixingAirMassFlowRatePrevious = 0.0;

            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
                {
                    auto const &thisNode(Node(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNode(NodeNum)));
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotInletAirMassFlowRate += thisNode.MassFlowRate;
                    TotInletAirMassFlowRateMax += thisNode.MassFlowRateMax;
                    TotInletAirMassFlowRateMaxAvail += thisNode.MassFlowRateMaxAvail;
                    TotInletAirMassFlowRateMin += thisNode.MassFlowRateMin;
                    TotInletAirMassFlowRateMinAvail += thisNode.MassFlowRateMinAvail;
                }
            }

            TotInletAirMassFlowRate = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotInletAirMassFlowRate;

            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumExhaustNodes; ++NodeNum) {

                if (state.afn->AirflowNetworkNumOfExhFan == 0) {
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotExhaustAirMassFlowRate +=
                        Node(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExhaustNode(NodeNum)).MassFlowRate;
                }
            }
            TotExhaustAirMassFlowRate = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotExhaustAirMassFlowRate;

            // Include zone mixing mass flow rate
            if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(ZoneNum)) {
                int NumRetNodes = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes;
                for (int NodeNumHere = 1; NodeNumHere <= NumRetNodes; ++NodeNumHere) {
                    int RetNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode(NodeNumHere);
                    if (RetNode > 0) {
                        ZoneReturnAirMassFlowRate += Node(RetNode).MassFlowRate;
                    }
                }
                // Set zone mixing incoming mass flow rate
                if ((Iteration == 0) || state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnOnly ||
                    state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {
                    ZoneMixingAirMassFlowRate = state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum);
                } else {
                    ZoneMixingAirMassFlowRate = max(0.0,
                                                    ZoneReturnAirMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate +
                                                        state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate);
                }
                CalcZoneMixingFlowRateOfReceivingZone(state, ZoneNum, ZoneMixingAirMassFlowRate);
                ZoneMixingNetAirMassFlowRate = state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate -
                                               state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate;
            }

            ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode;
            Node(ZoneNode).MassFlowRate = TotInletAirMassFlowRate;
            Node(ZoneNode).MassFlowRateMax = TotInletAirMassFlowRateMax;
            Node(ZoneNode).MassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail;
            Node(ZoneNode).MassFlowRateMin = TotInletAirMassFlowRateMin;
            Node(ZoneNode).MassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail;

            // Calculate standard return air flow rate using default method of inlets minus exhausts adjusted for "balanced" exhaust flow
            StdTotalReturnMassFlow = TotInletAirMassFlowRate + ZoneMixingNetAirMassFlowRate -
                                     (TotExhaustAirMassFlowRate - state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneExhBalanced);

            if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                if (StdTotalReturnMassFlow < 0.0) {
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExcessZoneExh = -StdTotalReturnMassFlow;
                    StdTotalReturnMassFlow = 0.0;
                } else {
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExcessZoneExh = 0.0;
                }
            } else {
                state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExcessZoneExh = 0.0;
                StdTotalReturnMassFlow = max(0.0, StdTotalReturnMassFlow);
            }

            Real64 FinalTotalReturnMassFlow = 0;
            CalcZoneReturnFlows(state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                // set mass conservation variables
                state.dataHeatBal->MassConservation(ZoneNum).InMassFlowRate = TotInletAirMassFlowRate;
                state.dataHeatBal->MassConservation(ZoneNum).ExhMassFlowRate = TotExhaustAirMassFlowRate;

                if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingOnly ||
                    state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingThenReturn) {
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    Real64 AdjustedTotalReturnMassFlow = 0;
                    state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate = FinalTotalReturnMassFlow;
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingThenReturn) {

                        // Calculate return air flow rate using mass conservation equation
                        AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                        AdjustedTotalReturnMassFlow =
                            min(AdjustedTotalReturnMassFlow, state.dataZoneEquip->ZoneEquipConfig(ZoneNum).AirLoopDesSupply);
                        // add adjust zone return node air flow calc
                        CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                        state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate = FinalTotalReturnMassFlow;
                        ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    }
                    // Set zone infiltration air flow rate
                    CalcZoneInfiltrationFlows(state, ZoneNum, ZoneReturnAirMassFlowRate);

                } else if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnOnly ||
                           state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {

                    Real64 AdjustedTotalReturnMassFlow = 0;
                    // Calculate return air flow rate using mass conservation equation
                    AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                    AdjustedTotalReturnMassFlow = min(AdjustedTotalReturnMassFlow, state.dataZoneEquip->ZoneEquipConfig(ZoneNum).AirLoopDesSupply);

                    // add adjust zone return node air flow calculation
                    CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                    state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate = FinalTotalReturnMassFlow;
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;

                    if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {
                        ZoneMixingAirMassFlowRate = max(0.0,
                                                        ZoneReturnAirMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate +
                                                            state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate);
                        CalcZoneMixingFlowRateOfReceivingZone(state, ZoneNum, ZoneMixingAirMassFlowRate);
                        ZoneMixingNetAirMassFlowRate = state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate -
                                                       state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate;

                        // Calculate return air flow rate using mass conservation equation
                        AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                        AdjustedTotalReturnMassFlow =
                            min(AdjustedTotalReturnMassFlow, state.dataZoneEquip->ZoneEquipConfig(ZoneNum).AirLoopDesSupply);

                        // add adjust zone return node air flow calc
                        CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                        state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate = FinalTotalReturnMassFlow;
                        ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    }

                    // Set zone infiltration air flow rate
                    CalcZoneInfiltrationFlows(state, ZoneNum, ZoneReturnAirMassFlowRate);
                } else {
                    // if infiltration treatment method is not None
                    // Set zone infiltration air flow rate
                    CalcZoneInfiltrationFlows(state, ZoneNum, ZoneReturnAirMassFlowRate);
                }
            }

            BuildingZoneMixingFlow += state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate;
            BuildingZoneReturnFlow += state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate;

            // Accumulate airloop total return flows and allocate excess exhaust flows
            for (int returnNum = 1; returnNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes; ++returnNum) {
                int retNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode(returnNum);
                int airLoop = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(returnNum);
                if (airLoop > 0) {
                    state.dataAirLoop->AirLoopFlow(airLoop).ZoneRetFlow += Node(retNode).MassFlowRate;
                    if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotAvailAirLoopOA > 0.0) {
                        state.dataAirLoop->AirLoopFlow(airLoop).ExcessZoneExhFlow += state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExcessZoneExh *
                                                                                     state.dataAirLoop->AirLoopFlow(airLoop).MaxOutAir /
                                                                                     state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotAvailAirLoopOA;
                    }
                }
            }
        }

        // adjust the zone return air flow rates to match any excess zone exhaust flows
        for (int airLoopNum = 1; airLoopNum <= NumPrimaryAirSys; ++airLoopNum) {
            auto &thisAirLoopFlow(state.dataAirLoop->AirLoopFlow(airLoopNum));
            Real64 adjZoneRetFlow = max(0.0, thisAirLoopFlow.ZoneRetFlow - thisAirLoopFlow.ExcessZoneExhFlow);
            if (thisAirLoopFlow.ZoneRetFlow > 0.0) {
                thisAirLoopFlow.ZoneRetFlowRatio = adjZoneRetFlow / thisAirLoopFlow.ZoneRetFlow;
            } else {
                thisAirLoopFlow.ZoneRetFlowRatio = 1.0;
            }
            thisAirLoopFlow.ZoneRetFlow = 0.0; // reset to zero and re-accumulate below
        }

        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            auto &thisZoneEquip(state.dataZoneEquip->ZoneEquipConfig(zoneNum));
            if (!thisZoneEquip.IsControlled) continue;
            int numRetNodes = thisZoneEquip.NumReturnNodes;
            Real64 totalZoneReturnMassFlow = 0.0;
            for (int returnNum = 1; returnNum <= numRetNodes; ++returnNum) {
                int retNode = thisZoneEquip.ReturnNode(returnNum);
                int airLoopNum = thisZoneEquip.ReturnNodeAirLoopNum(returnNum);
                if (retNode > 0) {
                    if (airLoopNum > 0) {
                        auto &thisAirLoopFlow(state.dataAirLoop->AirLoopFlow(airLoopNum));
                        Node(retNode).MassFlowRate *= thisAirLoopFlow.ZoneRetFlowRatio;
                        thisAirLoopFlow.ZoneRetFlow += Node(retNode).MassFlowRate;
                    }
                    totalZoneReturnMassFlow += Node(retNode).MassFlowRate;
                }
            }
            // Check zone flow balance but not when zone air mass balance is active
            if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance && !state.dataGlobal->DoingSizing &&
                !state.dataGlobal->DoingHVACSizingSimulations && !state.dataGlobal->WarmupFlag && !FirstHVACIteration) {
                if (!thisZoneEquip.FlowError) {
                    // Net system flows first (sum leaving flows, less entering flows)
                    Real64 sysUnbalExhaust = (thisZoneEquip.TotExhaustAirMassFlowRate - thisZoneEquip.ZoneExhBalanced);
                    Real64 sysUnbalancedFlow = sysUnbalExhaust + totalZoneReturnMassFlow - thisZoneEquip.TotInletAirMassFlowRate;
                    if (sysUnbalancedFlow > SmallMassFlow) {
                        int actualZone = thisZoneEquip.ActualZoneNum;
                        // Now include infiltration, ventilation, and mixing flows (these are all entering the zone, so subtract them)
                        Real64 incomingFlow = state.dataHeatBalFanSys->OAMFL(actualZone) + state.dataHeatBalFanSys->VAMFL(actualZone) +
                                              state.dataHeatBalFanSys->MixingMassFlowZone(actualZone);
                        Real64 unbalancedFlow = max(0.0, sysUnbalancedFlow - incomingFlow);
                        if (unbalancedFlow > SmallMassFlow) {
                            // Re-check on volume basis - use current zone density for incoming, standard density for HVAC sys
                            Real64 zoneTemp = Node(thisZoneEquip.ZoneNode).Temp;
                            Real64 zoneHumRat = Node(thisZoneEquip.ZoneNode).HumRat;
                            Real64 rhoZone = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, zoneTemp, zoneHumRat, "CalcZoneMassBalance");
                            Real64 incomingVolFlow = incomingFlow / rhoZone;
                            Real64 sysUnbalancedVolFlow = sysUnbalancedFlow / state.dataEnvrn->StdRhoAir;
                            Real64 unbalancedVolFlow = max(0.0, sysUnbalancedVolFlow - incomingVolFlow);
                            if (unbalancedVolFlow > DataHVACGlobals::SmallAirVolFlow) {
                                ShowWarningError(state,
                                                 "In zone " + thisZoneEquip.ZoneName +
                                                     " there is unbalanced air flow. Load due to induced outdoor air is neglected.");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("  Flows [m3/s]: Inlets: {:.6R}  Unbalanced exhausts: {:.6R}  Returns: {:.6R}",
                                                         thisZoneEquip.TotInletAirMassFlowRate / state.dataEnvrn->StdRhoAir,
                                                         sysUnbalExhaust / state.dataEnvrn->StdRhoAir,
                                                         totalZoneReturnMassFlow / state.dataEnvrn->StdRhoAir));
                                ShowContinueError(state,
                                                  format("  Infiltration: {:.6R}  Zone Ventilation: {:.6R}  Mixing (incoming): {:.6R}",
                                                         state.dataHeatBalFanSys->OAMFL(actualZone) / rhoZone,
                                                         state.dataHeatBalFanSys->VAMFL(actualZone) / rhoZone,
                                                         state.dataHeatBalFanSys->MixingMassFlowZone(actualZone) / rhoZone));
                                ShowContinueError(
                                    state,
                                    format("  Imbalance (excess outflow): {:.6R}  Total system OA flow (for all airloops serving this zone): {:.6R}",
                                           unbalancedVolFlow,
                                           thisZoneEquip.TotAvailAirLoopOA / state.dataEnvrn->StdRhoAir));
                                ShowContinueError(state, "  This error will only be reported once per zone.");
                                thisZoneEquip.FlowError = true;
                            }
                        }
                    }
                }
            }
        }

        // update the
        if (Iteration > 0) {
            Real64 totalResidual = 0.0;
            totalResidual =
                std::abs(BuildingZoneMixingFlow - BuildingZoneMixingFlowOld) + std::abs(BuildingZoneReturnFlow - BuildingZoneReturnFlowOld);
            if (totalResidual < ConvergenceTolerance) {
                ZoneMassBalanceHVACReSim = false;
                break;
            } else {
                ZoneMassBalanceHVACReSim = true;
            }
        }
        if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) break;
        Iteration += 1;

    } while (Iteration < IterMax);
    // Set system return flows
    for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
        auto &thisAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));
        thisAirLoopFlow.SysRetFlow = thisAirLoopFlow.ZoneRetFlow - thisAirLoopFlow.RecircFlow + thisAirLoopFlow.LeakFlow;
    }
}

void CalcZoneReturnFlows(EnergyPlusData &state,
                         int const ZoneNum,
                         Real64 &ExpTotalReturnMassFlow,  // Expected total return air mass flow rate
                         Real64 &FinalTotalReturnMassFlow // Final total return air mass flow rate
)
{
    auto &thisZoneEquip(state.dataZoneEquip->ZoneEquipConfig(ZoneNum));
    int numRetNodes = thisZoneEquip.NumReturnNodes;
    Real64 totReturnFlow = 0.0; // Total flow to all return nodes in the zone (kg/s)
    Real64 totVarReturnFlow =
        0.0; // Total variable return flow, for return nodes connected to an airloop with an OA system or not with specified flow (kg/s)
    Real64 returnSchedFrac = ScheduleManager::GetCurrentScheduleValue(state, thisZoneEquip.ReturnFlowSchedPtrNum);
    thisZoneEquip.FixedReturnFlow = false;
    FinalTotalReturnMassFlow = 0.0;
    thisZoneEquip.TotAvailAirLoopOA = 0.0;

    // Set initial flow rate for each return node
    for (int returnNum = 1; returnNum <= numRetNodes; ++returnNum) {
        int retNode = thisZoneEquip.ReturnNode(returnNum);

        if (retNode > 0) {
            Real64 returnNodeMassFlow = 0.0;
            auto &retNodeData(state.dataLoopNodes->Node(retNode));

            int inletNum = thisZoneEquip.ReturnNodeInletNum(returnNum); // which inlet node matches this return node (same airloop)
            int ADUNum = 0;
            if (inletNum > 0) ADUNum = thisZoneEquip.InletNodeADUNum(inletNum);
            int airLoop = thisZoneEquip.ReturnNodeAirLoopNum(returnNum);
            Real64 airLoopReturnFrac = 1.0;
            if (airLoop > 0) {
                // Establish corresponding airloop inlet(s) mass flow rate and set return node max/min/maxavail
                Real64 inletMassFlow = 0.0;
                int maxMinNodeNum = 0;
                auto &thisAirLoopFlow(state.dataAirLoop->AirLoopFlow(airLoop));
                if (ADUNum > 0) {
                    // Zone return node could carry supply flow to zone without leaks plus any induced flow from plenum (but don't include other
                    // secondary flows from exhaust nodes)
                    inletMassFlow = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateZSup +
                                    state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRatePlenInd;
                    maxMinNodeNum = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                } else if (inletNum > 0) {
                    // If not connected to an ADU, then use the inlet node flow
                    inletMassFlow = state.dataLoopNodes->Node(thisZoneEquip.InletNode(inletNum)).MassFlowRate;
                    maxMinNodeNum = thisZoneEquip.InletNode(inletNum);
                }
                if (maxMinNodeNum > 0) {
                    auto const &maxMinNodeData(state.dataLoopNodes->Node(maxMinNodeNum));
                    retNodeData.MassFlowRateMax = maxMinNodeData.MassFlowRateMax;
                    retNodeData.MassFlowRateMin = maxMinNodeData.MassFlowRateMin;
                    retNodeData.MassFlowRateMaxAvail = maxMinNodeData.MassFlowRateMaxAvail;
                } else {
                    auto const &zoneNodeData(state.dataLoopNodes->Node(thisZoneEquip.ZoneNode));
                    retNodeData.MassFlowRateMax = zoneNodeData.MassFlowRateMax;
                    retNodeData.MassFlowRateMin = zoneNodeData.MassFlowRateMin;
                    retNodeData.MassFlowRateMaxAvail = zoneNodeData.MassFlowRateMaxAvail;
                }

                airLoopReturnFrac = thisAirLoopFlow.DesReturnFrac;
                if (state.dataAirSystemsData->PrimaryAirSystems(airLoop).OASysExists && (thisAirLoopFlow.MaxOutAir > 0.0)) {
                    // Set return flow as fraction of matching inlet node flow if there is an OA system and available OA flow > 0.0
                    returnNodeMassFlow = airLoopReturnFrac * inletMassFlow;
                    thisZoneEquip.TotAvailAirLoopOA += thisAirLoopFlow.MaxOutAir;
                } else {
                    // Set return flow to matching inlet node flow
                    returnNodeMassFlow = inletMassFlow;
                    thisZoneEquip.FixedReturnFlow(returnNum) = true;
                }
            } else {
                returnNodeMassFlow = 0.0;
            }

            // Return node 1 is special
            if (returnNum == 1) {
                // Make no return air flow adjustments during sizing
                if ((state.dataGlobal->DoingSizing) && numRetNodes == 1) {
                    returnNodeMassFlow = ExpTotalReturnMassFlow;
                    if (airLoop > 0) {
                        if (!state.dataAirSystemsData->PrimaryAirSystems(airLoop).OASysExists ||
                            (state.dataAirLoop->AirLoopFlow(airLoop).MaxOutAir == 0.0)) {
                            ExpTotalReturnMassFlow = max(0.0, ExpTotalReturnMassFlow - thisZoneEquip.ZoneExhBalanced + thisZoneEquip.ZoneExh);
                            returnNodeMassFlow = ExpTotalReturnMassFlow;
                        }
                    }
                } else if (!state.dataGlobal->DoingSizing) {
                    if (thisZoneEquip.NumReturnFlowBasisNodes > 0) {
                        // Set base return air flow rate for node 1 using basis node flow rates
                        Real64 basisNodesMassFlow = 0.0;
                        for (int nodeNum = 1; nodeNum <= thisZoneEquip.NumReturnFlowBasisNodes; ++nodeNum) {
                            basisNodesMassFlow += state.dataLoopNodes->Node(thisZoneEquip.ReturnFlowBasisNode(nodeNum)).MassFlowRate;
                        }
                        returnNodeMassFlow = max(0.0, (basisNodesMassFlow * returnSchedFrac));
                        thisZoneEquip.FixedReturnFlow(returnNum) = true;
                    } else {
                        // If only 1 return node, use the standard return mass flow
                        if ((numRetNodes == 1) && !thisZoneEquip.FixedReturnFlow(returnNum)) {
                            returnNodeMassFlow = max(0.0, (ExpTotalReturnMassFlow * returnSchedFrac * airLoopReturnFrac));
                        }
                    }
                }
            }
            totReturnFlow += returnNodeMassFlow;
            retNodeData.MassFlowRate = returnNodeMassFlow;
            retNodeData.MassFlowRateMinAvail = 0.0;
            if (!thisZoneEquip.FixedReturnFlow(returnNum)) totVarReturnFlow += returnNodeMassFlow;
        }
    }

    // if zone mass balance true, set to expected return flow
    if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing) {
        // applies zone return flow schedule multiplier
        ExpTotalReturnMassFlow = returnSchedFrac * ExpTotalReturnMassFlow;
        // set air flow rate for each return node
        Real64 zoneTotReturnFlow = 0.0;
        Real64 returnNodeMassFlow = 0.0;
        for (int returnNum = 1; returnNum <= numRetNodes; ++returnNum) {
            int retNode = thisZoneEquip.ReturnNode(returnNum);
            if (retNode > 0) {
                if (numRetNodes == 1) {
                    returnNodeMassFlow = ExpTotalReturnMassFlow;
                } else { // multiple return nodes
                    if (ExpTotalReturnMassFlow > 0.0) {
                        Real64 returnAdjFactor = state.dataLoopNodes->Node(retNode).MassFlowRate / ExpTotalReturnMassFlow;
                        returnNodeMassFlow = returnAdjFactor * ExpTotalReturnMassFlow;
                    } else {
                        returnNodeMassFlow = 0.0;
                    }
                }
            }
            zoneTotReturnFlow += returnNodeMassFlow;
        }
        // Adjust return node flows if zone total return flow is > 0
        if (zoneTotReturnFlow > 0.0) {
            for (int returnNum = 1; returnNum <= numRetNodes; ++returnNum) {
                int retNode = thisZoneEquip.ReturnNode(returnNum);
                if (retNode > 0) {
                    if (numRetNodes == 1) {
                        // set it to expected return flows
                        state.dataLoopNodes->Node(retNode).MassFlowRate = ExpTotalReturnMassFlow;
                        FinalTotalReturnMassFlow = ExpTotalReturnMassFlow;
                    } else { // multiple return nodes, adjust nodes flow
                        Real64 newReturnFlow = 0.0;
                        Real64 returnAdjFactor = ExpTotalReturnMassFlow / zoneTotReturnFlow;
                        Real64 curReturnFlow = state.dataLoopNodes->Node(retNode).MassFlowRate;
                        newReturnFlow = curReturnFlow * returnAdjFactor;
                        state.dataLoopNodes->Node(retNode).MassFlowRate = newReturnFlow;
                        FinalTotalReturnMassFlow += newReturnFlow;
                    }
                }
            }
        } else {
            FinalTotalReturnMassFlow = ExpTotalReturnMassFlow;
        }
    } else {
        // Adjust return flows if greater than expected (i.e. there is exhaust or mixing flow reducing the total available for return)
        if ((totReturnFlow > ExpTotalReturnMassFlow) && (totVarReturnFlow > 0.0)) {
            Real64 newReturnFlow = 0.0;
            Real64 returnAdjFactor = (1 - ((totReturnFlow - ExpTotalReturnMassFlow) / totVarReturnFlow)); // Return flow adjustment factor
            for (int returnNum = 1; returnNum <= numRetNodes; ++returnNum) {
                int retNode = thisZoneEquip.ReturnNode(returnNum);
                Real64 curReturnFlow = state.dataLoopNodes->Node(retNode).MassFlowRate;
                if (retNode > 0) {
                    if (!thisZoneEquip.FixedReturnFlow(returnNum)) {
                        newReturnFlow = curReturnFlow * returnAdjFactor;
                        FinalTotalReturnMassFlow += newReturnFlow;
                        state.dataLoopNodes->Node(retNode).MassFlowRate = newReturnFlow;
                    } else {
                        FinalTotalReturnMassFlow += curReturnFlow;
                    }
                }
            }
        } else {
            FinalTotalReturnMassFlow = totReturnFlow;
        }
    }
}

void CalcZoneInfiltrationFlows(EnergyPlusData &state,
                               int const ZoneNum,                // current zone index
                               Real64 &ZoneReturnAirMassFlowRate // zone total zone return air mass flow rate
)
{
    Real64 constexpr ConvergenceTolerance(0.000010);
    Real64 ZoneInfiltrationMassFlowRate = 0.0;

    // Set zone infiltration flow rate
    if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment != DataHeatBalance::InfiltrationFlow::No) {
        if (state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr > 0) {
            if (state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone ||
                (state.dataHeatBal->ZoneAirMassFlow.InfiltrationForZones == DataHeatBalance::InfiltrationZoneType::AllZones)) {
                ZoneInfiltrationMassFlowRate = state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate -
                                               state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate +
                                               state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotExhaustAirMassFlowRate + ZoneReturnAirMassFlowRate -
                                               state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotInletAirMassFlowRate;
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    if (std::abs(ZoneInfiltrationMassFlowRate) > ConvergenceTolerance) {
                        state.dataHeatBalFanSys->ZoneInfiltrationFlag(ZoneNum) = true;
                        state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
                        state.dataHeatBal->MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 1;
                        state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate =
                            ZoneInfiltrationMassFlowRate;
                        state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate =
                            max(0.0, state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate);
                    } else {
                        state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
                        state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate = 0.0;
                    }
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    if (ZoneInfiltrationMassFlowRate > ConvergenceTolerance) {
                        state.dataHeatBalFanSys->ZoneInfiltrationFlag(ZoneNum) = true;
                        state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
                        state.dataHeatBal->MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 1;
                        state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate +=
                            ZoneInfiltrationMassFlowRate;
                    } else {
                        state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
                    }
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::No) {
                    state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
                }
            } else {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate =
                        state.dataHeatBal->Infiltration(state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::No) {
                    state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
                }
            }
        } else {
            // Zone has no infiltration objects
            state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
        }
    }
}

void CalcZoneLeavingConditions(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   January 2001
    //       MODIFIED       June 2003, FCW: add heat from airflow window to return air
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Perform zone upate of the leaving conditions.

    // METHODOLOGY EMPLOYED:
    // Energy Balance.

    // Using/Aliasing
    using DataHVACGlobals::RetTempMax;
    using DataHVACGlobals::RetTempMin;
    using DataSurfaces::AirFlowWindow_Destination_ReturnAir;
    using InternalHeatGains::SumAllReturnAirConvectionGains;
    using InternalHeatGains::SumAllReturnAirLatentGains;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 QRetAir;           // Heat to return air from lights
    Real64 CpAir;             // Air heat capacity [J/kg-K]
    Real64 TempRetAir;        // Return air temperature [C]
    Real64 TempZoneAir;       // Zone air temperature [C]
    int ZoneNum;              // Controlled zone number
    int ActualZoneNum;        // Zone number
    int ZoneNode;             // Node number of controlled zone
    int ReturnNode;           // Node number of controlled zone's return air
    int ReturnNodeExhaustNum; // Asscoaited exhaust node number, corresponding to the return node
    int SurfNum;              // Surface number
    Real64 MassFlowRA;        // Return air mass flow [kg/s]
    Real64 FlowThisTS;        // Window gap air mass flow [kg/s]
    Real64 WinGapFlowToRA;    // Mass flow to return air from all airflow windows in zone [kg/s]
    Real64 WinGapFlowTtoRA;   // Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
    Real64 WinGapTtoRA;       // Temp of outlet flow mixture to return air from all airflow windows in zone [C]
    Real64 H2OHtOfVap;        // Heat of vaporization of water (W/kg)
    Real64 ZoneMult;          // zone multiplier
    Real64 SumRetAirLatentGainRate;

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;
        ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum;
        // A return air system may not exist for certain systems; Therefore when no return node exists
        // there is no update.  Of course if there is no return air system then you cannot update
        // the energy for the return air heat gain from the lights statements.
        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes == 0) continue;
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode;
        ZoneMult = state.dataHeatBal->Zone(ActualZoneNum).Multiplier * state.dataHeatBal->Zone(ActualZoneNum).ListMultiplier;
        for (int nodeCount = 1; nodeCount <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes; ++nodeCount) {
            ReturnNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode(nodeCount);
            ReturnNodeExhaustNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNodeExhaustNodeNum(nodeCount);

            // RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
            // Add sensible heat gain from refrigerated cases with under case returns
            QRetAir = SumAllReturnAirConvectionGains(state, ActualZoneNum, ReturnNode);

            // Need to add the energy to the return air from lights and from airflow windows. Where the heat
            // is added depends on if there is system flow or not.  If there is system flow the heat is added
            // to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
            // Correct step through the SysDepZoneLoads variable.

            MassFlowRA = state.dataLoopNodes->Node(ReturnNode).MassFlowRate / ZoneMult;
            if (ReturnNodeExhaustNum > 0 && state.dataLoopNodes->Node(ReturnNodeExhaustNum).MassFlowRate > 0.0) {
                MassFlowRA += state.dataLoopNodes->Node(ReturnNodeExhaustNum).MassFlowRate;
            }

            // user defined room air model may feed temp that differs from zone node
            if (allocated(state.dataRoomAirMod->AirPatternZoneInfo)) {
                if ((state.dataRoomAirMod->AirPatternZoneInfo(ActualZoneNum).IsUsed) && (!state.dataGlobal->BeginEnvrnFlag)) {
                    TempZoneAir = state.dataRoomAirMod->AirPatternZoneInfo(ActualZoneNum).Tleaving;
                    TempRetAir = TempZoneAir;
                } else {
                    TempZoneAir = state.dataLoopNodes->Node(ZoneNode).Temp;
                    TempRetAir = TempZoneAir;
                }
            } else {
                TempZoneAir = state.dataLoopNodes->Node(ZoneNode).Temp;
                TempRetAir = TempZoneAir;
            }

            WinGapFlowToRA = 0.0;
            WinGapTtoRA = 0.0;
            WinGapFlowTtoRA = 0.0;

            if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneHasAirFlowWindowReturn) {
                for (SurfNum = state.dataHeatBal->Zone(ActualZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ActualZoneNum).HTSurfaceLast;
                     ++SurfNum) {
                    if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0 &&
                        state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_ReturnAir) {
                        FlowThisTS = PsyRhoAirFnPbTdbW(state,
                                                       state.dataEnvrn->OutBaroPress,
                                                       state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum),
                                                       state.dataLoopNodes->Node(ZoneNode).HumRat) *
                                     state.dataSurface->SurfWinAirflowThisTS(SurfNum) * state.dataSurface->Surface(SurfNum).Width;
                        WinGapFlowToRA += FlowThisTS;
                        WinGapFlowTtoRA += FlowThisTS * state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum);
                    }
                }
            }
            if (WinGapFlowToRA > 0.0) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowToRA;
            // the flag NoHeatToReturnAir is TRUE if the system is zonal only or is central with on/off air flow. In these
            // cases the heat to return air is treated as a zone heat gain and dealt with in CalcZoneSums in
            // MODULE ZoneTempPredictorCorrector.
            if (!state.dataHeatBal->Zone(ActualZoneNum).NoHeatToReturnAir) {
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
                if (MassFlowRA > 0.0) {
                    if (WinGapFlowToRA > 0.0) {
                        // Add heat-to-return from window gap airflow
                        if (MassFlowRA >= WinGapFlowToRA) {
                            TempRetAir = (WinGapFlowTtoRA + (MassFlowRA - WinGapFlowToRA) * TempZoneAir) / MassFlowRA;
                        } else {
                            // All of return air comes from flow through airflow windows
                            TempRetAir = WinGapTtoRA;
                            // Put heat from window airflow that exceeds return air flow into zone air
                            state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) +=
                                (WinGapFlowToRA - MassFlowRA) * CpAir * (WinGapTtoRA - TempZoneAir);
                        }
                    }
                    // Add heat-to-return from lights
                    TempRetAir += QRetAir / (MassFlowRA * CpAir);
                    if (TempRetAir > RetTempMax) {
                        state.dataLoopNodes->Node(ReturnNode).Temp = RetTempMax;
                        if (!state.dataGlobal->ZoneSizingCalc) {
                            state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) += CpAir * MassFlowRA * (TempRetAir - RetTempMax);
                        }
                    } else if (TempRetAir < RetTempMin) {
                        state.dataLoopNodes->Node(ReturnNode).Temp = RetTempMin;
                        if (!state.dataGlobal->ZoneSizingCalc) {
                            state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) += CpAir * MassFlowRA * (TempRetAir - RetTempMin);
                        }
                    } else {
                        state.dataLoopNodes->Node(ReturnNode).Temp = TempRetAir;
                    }
                    if (ReturnNodeExhaustNum > 0 && state.dataLoopNodes->Node(ReturnNodeExhaustNum).MassFlowRate > 0.0 && QRetAir > 0.0) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).SharedExhaustNode(nodeCount) != LightReturnExhaustConfig::Shared) {
                            state.dataLoopNodes->Node(ReturnNodeExhaustNum).Temp = TempRetAir;
                        } else {
                            state.dataLoopNodes->Node(ReturnNodeExhaustNum).Temp += QRetAir / (MassFlowRA * CpAir);
                        }
                    }
                } else { // No return air flow
                    // Assign all heat-to-return from window gap airflow to zone air
                    if (WinGapFlowToRA > 0.0)
                        state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) += WinGapFlowToRA * CpAir * (WinGapTtoRA - TempZoneAir);
                    // Assign all heat-to-return from lights to zone air
                    if (QRetAir > 0.0) state.dataHeatBalFanSys->SysDepZoneLoads(ActualZoneNum) += QRetAir;
                    state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
                }
            } else {
                // update the return air node for zonal and central on/off systems
                state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
            }

            // Update the rest of the Return Air Node conditions, if the return air system exists!
            state.dataLoopNodes->Node(ReturnNode).Press = state.dataLoopNodes->Node(ZoneNode).Press;

            // Include impact of under case returns for refrigerated display case when updating the return air node humidity
            if (!state.dataHeatBal->Zone(ActualZoneNum).NoHeatToReturnAir) {
                if (MassFlowRA > 0) {
                    SumRetAirLatentGainRate = SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                    H2OHtOfVap = PsyHgAirFnWTdb(state.dataLoopNodes->Node(ZoneNode).HumRat, state.dataLoopNodes->Node(ReturnNode).Temp);
                    state.dataLoopNodes->Node(ReturnNode).HumRat =
                        state.dataLoopNodes->Node(ZoneNode).HumRat + (SumRetAirLatentGainRate / (H2OHtOfVap * MassFlowRA));
                } else {
                    // If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
                    state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                    state.dataHeatBal->RefrigCaseCredit(ActualZoneNum).LatCaseCreditToZone +=
                        state.dataHeatBal->RefrigCaseCredit(ActualZoneNum).LatCaseCreditToHVAC;
                    // shouldn't the HVAC term be zeroed out then?
                    SumRetAirLatentGainRate = SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                    state.dataHeatBalFanSys->ZoneLatentGain(ActualZoneNum) += SumRetAirLatentGainRate;
                }
            } else {
                state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                state.dataHeatBal->RefrigCaseCredit(ActualZoneNum).LatCaseCreditToZone +=
                    state.dataHeatBal->RefrigCaseCredit(ActualZoneNum).LatCaseCreditToHVAC;
                // shouldn't the HVAC term be zeroed out then?
                SumRetAirLatentGainRate = SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                state.dataHeatBalFanSys->ZoneLatentGain(ActualZoneNum) += SumRetAirLatentGainRate;
            }

            state.dataLoopNodes->Node(ReturnNode).Enthalpy =
                PsyHFnTdbW(state.dataLoopNodes->Node(ReturnNode).Temp, state.dataLoopNodes->Node(ReturnNode).HumRat);

            if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                state.dataLoopNodes->Node(ReturnNode).CO2 = state.dataLoopNodes->Node(ZoneNode).CO2;
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                state.dataLoopNodes->Node(ReturnNode).GenContam = state.dataLoopNodes->Node(ZoneNode).GenContam;

        } // End of check for a return air node, which implies a return air system.

        // Reset current deadband flags, remaining output required, so no impact beyond zone equipment
        InitSystemOutputRequired(state, ActualZoneNum, FirstHVACIteration, true);
    }
}

void UpdateZoneEquipment(EnergyPlusData &state, bool &SimAir)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs the update for Zone Equipment Management.
    // Specifically, it transfers the conditions from the zone equipment return air nodes across
    // to the air loop side, allowing for multiple return air nodes

    // Using/Aliasing
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    using HVACInterfaceManager::UpdateHVACInterface;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneGroupNum;
    int RetAirPathNum;

    // Transfer the conditions from the zone equipment return air nodes across
    // to the air loop side, allowing for multiple return air nodes
    for (ZoneGroupNum = 1; ZoneGroupNum <= NumPrimaryAirSys; ++ZoneGroupNum) {
        for (RetAirPathNum = 1; RetAirPathNum <= state.dataAirLoop->AirToZoneNodeInfo(ZoneGroupNum).NumReturnNodes; ++RetAirPathNum) {
            UpdateHVACInterface(state,
                                ZoneGroupNum,
                                DataConvergParams::CalledFrom::AirSystemDemandSide,
                                state.dataAirLoop->AirToZoneNodeInfo(ZoneGroupNum).ZoneEquipReturnNodeNum(RetAirPathNum),
                                state.dataAirLoop->AirToZoneNodeInfo(ZoneGroupNum).AirLoopReturnNodeNum(RetAirPathNum),
                                SimAir);
        }
    }
}

void CalcAirFlowSimple(EnergyPlusData &state,
                       int const SysTimestepLoop,                // System time step index
                       bool const AdjustZoneMixingFlowFlag,      // holds zone mixing air flow calc status
                       bool const AdjustZoneInfiltrationFlowFlag // holds zone mixing air flow calc status
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Legacy Code
    //       DATE WRITTEN   na
    //       MODIFIED       Shirey, Jan 2008 (MIXING objects, use avg. conditions for Cp, Air Density and Hfg)
    //       MODIFIED       L. Lawrie and L. GU, Jan. 2008 (Allow multiple infiltration and ventilation objects)
    //                      B. Griffith. Jan 2009 add infiltration, residential basic/sherman-grimsrud and enhanced/AIM2
    //                      L. Lawrie - March 2009 - move ventilation electric calculation to this routine (for
    //                        Electricity Net.
    //                      L. Gu - Dec. 2009 - Added a new ventilation object to calculate flow rate based on wind and stack
    //                        effect through an opening.
    //       MODIFIED       Stovall - Aug 2011 (add refrigerator door mixing)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the air component of the heat balance.

    // Using/Aliasing
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using CoolTower::ManageCoolTower;
    using DataHVACGlobals::CycleOn;
    using DataHVACGlobals::CycleOnZoneFansOnly;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using EarthTube::ManageEarthTube;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyTdbFnHW;
    using Psychrometrics::PsyWFnTdbTwbPb;
    using ScheduleManager::GetCurrentScheduleValue;
    using ThermalChimney::ManageThermalChimney;
    using namespace DataLoopNode;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr Real64 StdGravity(9.80665); // The acceleration of gravity at the sea level (m/s2)
    static constexpr std::string_view RoutineNameMixing("CalcAirFlowSimple:Mixing");
    static constexpr std::string_view RoutineNameCrossMixing("CalcAirFlowSimple:CrossMixing");
    static constexpr std::string_view RoutineNameRefrigerationDoorMixing("CalcAirFlowSimple:RefrigerationDoorMixing");
    static constexpr std::string_view RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");
    static constexpr std::string_view RoutineNameZoneAirBalance("CalcAirFlowSimple:ZoneAirBalance");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MCP;
    Real64 MCPxM;
    Real64 MCPxN;
    Real64 TZM;               // Temperature of From Zone
    Real64 TZN;               // Temperature of this zone
    Real64 TD;                // Delta Temp limit of Mixing statement
    Real64 Tavg;              // Average temperature in two zones exchanging air
    Real64 Wavg;              // Average humidity ratio in two zones exchanging air
    int m;                    // Index to From Zone
    int n;                    // Index of this zone
    int j;                    // Loop Counter
    int NZ;                   // A pointer
    int I;                    // Ventilation master object index
    int NH;                   // Hybrid controlled zone number
    Real64 AirDensity;        // Density of air (kg/m^3)
    Real64 CpAir;             // Heat capacity of air (J/kg-C)
    Real64 OutletAirEnthalpy; // Enthlapy of outlet air (VENTILATION objects)
    Real64 TempExt;
    Real64 WindSpeedExt;
    Real64 WindDirExt;
    Real64 HumRatExt;
    Real64 EnthalpyExt;
    bool MixingLimitFlag;
    Real64 MixingTmin;
    Real64 MixingTmax;

    Real64 IVF; // DESIGN INFILTRATION FLOW RATE (M**3/SEC)
    Real64 VVF; // DESIGN VENTILATION FLOW RATE (M**3/SEC)
    Real64 MCpI_temp;
    Real64 VAMFL_temp;

    Real64 Cw;    // Opening effectivenss
    Real64 Cd;    // Discharge coefficent
    Real64 angle; // Angle between wind direction and effective angle
    Real64 Qw;    // Volumetric flow driven by wind
    Real64 Qst;   // Volumetric flow driven by stack effect
    Real64 MassFlowDiff;
    // following variables used for refrigeration door mixing and all defined in EngRef
    int ZoneA;
    int ZoneB;
    Real64 TZoneA;
    Real64 TZoneB;
    Real64 HumRatZoneA;
    Real64 HumRatZoneB;
    Real64 AirDensityZoneA;
    Real64 CpAirZoneA;
    Real64 AirDensityZoneB;
    Real64 CpAirZoneB;
    Real64 AirDensityAvg;
    Real64 MassFlowDryAir;
    Real64 SchedDoorOpen;
    Real64 DoorHeight;
    Real64 DoorArea;
    Real64 DoorProt;
    Real64 FDens;
    Real64 Fb;
    Real64 FFlow;
    Real64 MassFlowToA;
    Real64 MassFlowToB;
    Real64 MassFlowXCpToA;
    Real64 MassFlowXCpToB;
    Real64 MassFlowXCpXTempToA;
    Real64 MassFlowXCpXTempToB;
    Real64 MassFlowXHumRatToA;
    Real64 MassFlowXHumRatToB;
    Real64 MassFlowRate;

    // Allocate the ZMAT and ZHumRat arrays

    if (!allocated(state.dataZoneEquip->ZMAT)) state.dataZoneEquip->ZMAT.allocate(state.dataGlobal->NumOfZones);
    if (!allocated(state.dataZoneEquip->ZHumRat)) state.dataZoneEquip->ZHumRat.allocate(state.dataGlobal->NumOfZones);
    if (!allocated(state.dataZoneEquip->VentMCP)) state.dataZoneEquip->VentMCP.allocate(state.dataHeatBal->TotVentilation);

    // Allocate module level logical arrays for MIXING and CROSS MIXING reporting
    if (!allocated(state.dataZoneEquip->CrossMixingReportFlag))
        state.dataZoneEquip->CrossMixingReportFlag.allocate(state.dataHeatBal->TotCrossMixing);
    if (!allocated(state.dataZoneEquip->MixingReportFlag)) state.dataZoneEquip->MixingReportFlag.allocate(state.dataHeatBal->TotMixing);

    if (!allocated(state.dataHeatBalFanSys->MCPTThermChim)) state.dataHeatBalFanSys->MCPTThermChim.allocate(state.dataGlobal->NumOfZones);
    if (!allocated(state.dataHeatBalFanSys->MCPThermChim)) state.dataHeatBalFanSys->MCPThermChim.allocate(state.dataGlobal->NumOfZones);
    if (!allocated(state.dataHeatBalFanSys->ThermChimAMFL)) state.dataHeatBalFanSys->ThermChimAMFL.allocate(state.dataGlobal->NumOfZones);

    //                                      COMPUTE ZONE AIR MIXINGS
    state.dataHeatBalFanSys->MCPM = 0.0;
    state.dataHeatBalFanSys->MCPTM = 0.0;
    state.dataHeatBalFanSys->MixingMassFlowZone = 0.0;
    state.dataHeatBalFanSys->MixingMassFlowXHumRat = 0.0;
    state.dataZoneEquip->CrossMixingReportFlag = false;
    state.dataZoneEquip->MixingReportFlag = false;
    if (state.dataContaminantBalance->Contaminant.CO2Simulation &&
        state.dataHeatBal->TotMixing + state.dataHeatBal->TotCrossMixing + state.dataHeatBal->TotRefDoorMixing > 0)
        state.dataContaminantBalance->MixingMassFlowCO2 = 0.0;
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation &&
        state.dataHeatBal->TotMixing + state.dataHeatBal->TotCrossMixing + state.dataHeatBal->TotRefDoorMixing > 0)
        state.dataContaminantBalance->MixingMassFlowGC = 0.0;

    IVF = 0.0;
    state.dataHeatBalFanSys->MCPTI = 0.0;
    state.dataHeatBalFanSys->MCPI = 0.0;
    state.dataHeatBalFanSys->OAMFL = 0.0;
    VVF = 0.0;
    state.dataHeatBalFanSys->MCPTV = 0.0;
    state.dataHeatBalFanSys->MCPV = 0.0;
    state.dataHeatBalFanSys->VAMFL = 0.0;
    state.dataZoneEquip->VentMCP = 0.0;
    state.dataHeatBalFanSys->MDotCPOA = 0.0;
    state.dataHeatBalFanSys->MDotOA = 0.0;
    state.dataHeatBalFanSys->MCPThermChim = 0.0;
    state.dataHeatBalFanSys->ThermChimAMFL = 0.0;
    state.dataHeatBalFanSys->MCPTThermChim = 0.0;
    MassFlowRate = 0.0;

    if (state.dataHeatBal->AirFlowFlag != UseSimpleAirFlow) return;
    // AirflowNetwork Multizone field /= SIMPLE
    if (!(state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimple ||
          state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS)) {
        return;
    }

    ManageEarthTube(state);
    ManageCoolTower(state);
    ManageThermalChimney(state);

    // Assign zone air temperature
    for (j = 1; j <= state.dataGlobal->NumOfZones; ++j) {
        state.dataZoneEquip->ZMAT(j) = state.dataHeatBalFanSys->MAT(j);
        state.dataZoneEquip->ZHumRat(j) = state.dataHeatBalFanSys->ZoneAirHumRat(j);
        // This is only temporary fix for CR8867.  (L. Gu 8/12)
        if (SysTimestepLoop == 1) {
            state.dataZoneEquip->ZMAT(j) = state.dataHeatBalFanSys->XMPT(j);
            state.dataZoneEquip->ZHumRat(j) = state.dataHeatBalFanSys->WZoneTimeMinusP(j);
        }
    }

    // Process the scheduled Ventilation for air heat balance
    if (state.dataHeatBal->TotVentilation > 0) {
        for (auto &e : state.dataHeatBal->ZnAirRpt) {
            e.VentilFanElec = 0.0;
        }
    }

    // Initialization of ZoneAirBalance
    if (state.dataHeatBal->TotZoneAirBalance > 0) {
        for (auto &e : state.dataHeatBal->ZoneAirBalance) {
            e.BalMassFlowRate = 0.0;
            e.InfMassFlowRate = 0.0;
            e.NatMassFlowRate = 0.0;
            e.ExhMassFlowRate = 0.0;
            e.IntMassFlowRate = 0.0;
            e.ERVMassFlowRate = 0.0;
        }
    }

    for (j = 1; j <= state.dataHeatBal->TotVentilation; ++j) {
        // Use air node information linked to the zone if defined
        NZ = state.dataHeatBal->Ventilation(j).ZonePtr;
        state.dataHeatBal->Ventilation(j).FanPower = 0.0;
        TempExt = state.dataHeatBal->Zone(NZ).OutDryBulbTemp;
        WindSpeedExt = state.dataHeatBal->Zone(NZ).WindSpeed;
        WindDirExt = state.dataHeatBal->Zone(NZ).WindDir;
        if (state.dataHeatBal->Zone(NZ).HasLinkedOutAirNode) {
            HumRatExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(NZ).LinkedOutAirNode).HumRat;
            EnthalpyExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(NZ).LinkedOutAirNode).Enthalpy;
        } else {
            HumRatExt = state.dataEnvrn->OutHumRat;
            EnthalpyExt = state.dataEnvrn->OutEnthalpy;
        }
        AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempExt, HumRatExt);
        CpAir = PsyCpAirFnW(HumRatExt);
        // Hybrid ventilation global control
        if (state.dataHeatBal->Ventilation(j).HybridControlType == DataHeatBalance::HybridCtrlType::Global &&
            state.dataHeatBal->Ventilation(j).HybridControlMasterNum > 0) {
            I = state.dataHeatBal->Ventilation(j).HybridControlMasterNum;
            NH = state.dataHeatBal->Ventilation(I).ZonePtr;
            if (j == I) {
                state.dataHeatBal->Ventilation(j).HybridControlMasterStatus = false;
            }
        } else {
            I = j;
            NH = NZ;
        }
        // Check scheduled temperatures
        if (state.dataHeatBal->Ventilation(I).MinIndoorTempSchedPtr > 0) {
            state.dataHeatBal->Ventilation(I).MinIndoorTemperature =
                GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(I).MinIndoorTempSchedPtr);
        }
        if (state.dataHeatBal->Ventilation(I).MaxIndoorTempSchedPtr > 0) {
            state.dataHeatBal->Ventilation(I).MaxIndoorTemperature =
                GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(I).MaxIndoorTempSchedPtr);
        }
        // Ensure the minimum indoor temperature <= the maximum indoor temperature
        if (state.dataHeatBal->Ventilation(I).MinIndoorTempSchedPtr > 0 || state.dataHeatBal->Ventilation(I).MaxIndoorTempSchedPtr > 0) {
            if (state.dataHeatBal->Ventilation(I).MinIndoorTemperature > state.dataHeatBal->Ventilation(I).MaxIndoorTemperature) {
                ++state.dataHeatBal->Ventilation(I).IndoorTempErrCount;
                if (state.dataHeatBal->Ventilation(I).IndoorTempErrCount < 2) {
                    ShowWarningError(
                        state,
                        "Ventilation indoor temperature control: The minimum indoor temperature is above the maximum indoor temperature in " +
                            state.dataHeatBal->Ventilation(I).Name);
                    ShowContinueError(state, "The minimum indoor temperature is set to the maximum indoor temperature. Simulation continues.");
                    ShowContinueErrorTimeStamp(state, " Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The minimum indoor temperature is still above the maximum indoor temperature",
                                                   state.dataHeatBal->Ventilation(I).IndoorTempErrIndex,
                                                   state.dataHeatBal->Ventilation(I).MinIndoorTemperature,
                                                   state.dataHeatBal->Ventilation(I).MinIndoorTemperature);
                }
                state.dataHeatBal->Ventilation(I).MinIndoorTemperature = state.dataHeatBal->Ventilation(I).MaxIndoorTemperature;
            }
        }
        if (state.dataHeatBal->Ventilation(I).MinOutdoorTempSchedPtr > 0) {
            state.dataHeatBal->Ventilation(I).MinOutdoorTemperature =
                GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(I).MinOutdoorTempSchedPtr);
        }
        if (state.dataHeatBal->Ventilation(I).MaxOutdoorTempSchedPtr > 0) {
            state.dataHeatBal->Ventilation(I).MaxOutdoorTemperature =
                GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(I).MaxOutdoorTempSchedPtr);
        }
        // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
        if (state.dataHeatBal->Ventilation(I).MinOutdoorTempSchedPtr > 0 || state.dataHeatBal->Ventilation(I).MaxOutdoorTempSchedPtr > 0) {
            if (state.dataHeatBal->Ventilation(I).MinOutdoorTemperature > state.dataHeatBal->Ventilation(I).MaxOutdoorTemperature) {
                ++state.dataHeatBal->Ventilation(I).OutdoorTempErrCount;
                if (state.dataHeatBal->Ventilation(I).OutdoorTempErrCount < 2) {
                    ShowWarningError(
                        state,
                        "Ventilation outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in " +
                            state.dataHeatBal->Ventilation(I).Name);
                    ShowContinueError(state, "The minimum outdoor temperature is set to the maximum outdoor temperature. Simulation continues.");
                    ShowContinueErrorTimeStamp(state, " Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                   state.dataHeatBal->Ventilation(I).OutdoorTempErrIndex,
                                                   state.dataHeatBal->Ventilation(I).MinOutdoorTemperature,
                                                   state.dataHeatBal->Ventilation(I).MinOutdoorTemperature);
                }
                state.dataHeatBal->Ventilation(I).MinIndoorTemperature = state.dataHeatBal->Ventilation(I).MaxIndoorTemperature;
            }
        }
        if (state.dataHeatBal->Ventilation(I).DeltaTempSchedPtr > 0) {
            state.dataHeatBal->Ventilation(I).DelTemperature = GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(I).DeltaTempSchedPtr);
        }
        // Skip this if the zone is below the minimum indoor temperature limit
        if ((state.dataZoneEquip->ZMAT(NH) < state.dataHeatBal->Ventilation(I).MinIndoorTemperature) &&
            (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn))
            continue;
        // Skip this if the zone is above the maximum indoor temperature limit
        if ((state.dataZoneEquip->ZMAT(NH) > state.dataHeatBal->Ventilation(I).MaxIndoorTemperature) &&
            (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn))
            continue;
        // Skip if below the temperature difference limit (3/12/03 Negative DelTemperature allowed now)
        if (((state.dataZoneEquip->ZMAT(NH) - TempExt) < state.dataHeatBal->Ventilation(I).DelTemperature) &&
            (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn))
            continue;
        // Skip this if the outdoor temperature is below the minimum outdoor temperature limit
        if ((TempExt < state.dataHeatBal->Ventilation(I).MinOutdoorTemperature) && (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn)) continue;
        // Skip this if the outdoor temperature is above the maximum outdoor temperature limit
        if ((TempExt > state.dataHeatBal->Ventilation(I).MaxOutdoorTemperature) && (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn)) continue;
        // Skip this if the outdoor wind speed is above the maximum windspeed limit
        if ((WindSpeedExt > state.dataHeatBal->Ventilation(I).MaxWindSpeed) && (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn)) continue;

        // Hybrid ventilation controls
        if ((state.dataHeatBal->Ventilation(j).HybridControlType == DataHeatBalance::HybridCtrlType::Close) &&
            (!state.dataHeatBal->Ventilation(j).EMSSimpleVentOn))
            continue;
        if (state.dataHeatBal->Ventilation(j).HybridControlType == DataHeatBalance::HybridCtrlType::Global &&
            state.dataHeatBal->Ventilation(j).HybridControlMasterNum > 0) {
            if (j == I) state.dataHeatBal->Ventilation(j).HybridControlMasterStatus = true;
        }

        if (state.dataHeatBal->Ventilation(j).ModelType == DataHeatBalance::VentilationModelType::DesignFlowRate) {
            // CR6845 if calculated < 0, don't propagate.
            VVF = state.dataHeatBal->Ventilation(j).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(j).SchedPtr);

            if (state.dataHeatBal->Ventilation(j).EMSSimpleVentOn) VVF = state.dataHeatBal->Ventilation(j).EMSimpleVentFlowRate;

            if (VVF < 0.0) VVF = 0.0;
            state.dataZoneEquip->VentMCP(j) =
                VVF * AirDensity * CpAir *
                (state.dataHeatBal->Ventilation(j).ConstantTermCoef +
                 std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ)) * state.dataHeatBal->Ventilation(j).TemperatureTermCoef +
                 WindSpeedExt *
                     (state.dataHeatBal->Ventilation(j).VelocityTermCoef + WindSpeedExt * state.dataHeatBal->Ventilation(j).VelocitySQTermCoef));
            if (state.dataZoneEquip->VentMCP(j) < 0.0) state.dataZoneEquip->VentMCP(j) = 0.0;
            VAMFL_temp = state.dataZoneEquip->VentMCP(j) / CpAir;
            if (state.dataHeatBal->Ventilation(j).QuadratureSum) {
                switch (state.dataHeatBal->Ventilation(j).FanType) {
                    // ventilation type based calculation
                case DataHeatBalance::VentilationType::Exhaust: {
                    state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Ventilation(j).OABalancePtr).ExhMassFlowRate +=
                        state.dataZoneEquip->VentMCP(j) / CpAir;
                } break;
                case DataHeatBalance::VentilationType::Intake: {
                    state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Ventilation(j).OABalancePtr).IntMassFlowRate +=
                        state.dataZoneEquip->VentMCP(j) / CpAir;
                } break;
                case DataHeatBalance::VentilationType::Natural: {
                    state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Ventilation(j).OABalancePtr).NatMassFlowRate +=
                        state.dataZoneEquip->VentMCP(j) / CpAir;
                } break;
                case DataHeatBalance::VentilationType::Balanced: {
                    state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Ventilation(j).OABalancePtr).BalMassFlowRate +=
                        state.dataZoneEquip->VentMCP(j) / CpAir;
                } break;
                default:
                    break;
                }
            } else {
                state.dataHeatBalFanSys->MCPV(NZ) += state.dataZoneEquip->VentMCP(j);
                state.dataHeatBalFanSys->VAMFL(NZ) += VAMFL_temp;
            }
            if (state.dataHeatBal->Ventilation(j).FanEfficiency > 0.0) {
                state.dataHeatBal->Ventilation(j).FanPower =
                    VAMFL_temp * state.dataHeatBal->Ventilation(j).FanPressure / (state.dataHeatBal->Ventilation(j).FanEfficiency * AirDensity);
                if (state.dataHeatBal->Ventilation(j).FanType == DataHeatBalance::VentilationType::Balanced)
                    state.dataHeatBal->Ventilation(j).FanPower *= 2.0;
                // calc electric
                if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
                    // CR7608 IF (.not. TurnFansOn .or. .not. AirflowNetworkZoneFlag(NZ)) &
                    if (!state.dataGlobal->KickOffSimulation) {
                        if (!(state.dataZoneEquip->ZoneEquipAvail(NZ) == CycleOn || state.dataZoneEquip->ZoneEquipAvail(NZ) == CycleOnZoneFansOnly) ||
                            !state.afn->AirflowNetworkZoneFlag(NZ))
                            state.dataHeatBal->ZnAirRpt(NZ).VentilFanElec +=
                                state.dataHeatBal->Ventilation(j).FanPower * TimeStepSys * DataGlobalConstants::SecInHour;
                    } else if (!state.afn->AirflowNetworkZoneFlag(NZ)) {
                        state.dataHeatBal->ZnAirRpt(NZ).VentilFanElec +=
                            state.dataHeatBal->Ventilation(j).FanPower * TimeStepSys * DataGlobalConstants::SecInHour;
                    }
                } else {
                    state.dataHeatBal->ZnAirRpt(NZ).VentilFanElec +=
                        state.dataHeatBal->Ventilation(j).FanPower * TimeStepSys * DataGlobalConstants::SecInHour;
                }
            }
            // Intake fans will add some heat to the air, raising the temperature for an intake fan...
            if (state.dataHeatBal->Ventilation(j).FanType == DataHeatBalance::VentilationType::Intake ||
                state.dataHeatBal->Ventilation(j).FanType == DataHeatBalance::VentilationType::Balanced) {
                if (VAMFL_temp == 0.0) {
                    OutletAirEnthalpy = EnthalpyExt;
                } else {
                    if (state.dataHeatBal->Ventilation(j).FanPower > 0.0) {
                        if (state.dataHeatBal->Ventilation(j).FanType == DataHeatBalance::VentilationType::Balanced) {
                            OutletAirEnthalpy =
                                EnthalpyExt + state.dataHeatBal->Ventilation(j).FanPower / VAMFL_temp / 2.0; // Half fan power to calculate inlet T
                        } else {
                            OutletAirEnthalpy = EnthalpyExt + state.dataHeatBal->Ventilation(j).FanPower / VAMFL_temp;
                        }
                    } else {
                        OutletAirEnthalpy = EnthalpyExt;
                    }
                }
                state.dataHeatBal->Ventilation(j).AirTemp = PsyTdbFnHW(OutletAirEnthalpy, HumRatExt);
            } else {
                state.dataHeatBal->Ventilation(j).AirTemp = TempExt;
            }
            if (!state.dataHeatBal->Ventilation(j).QuadratureSum)
                state.dataHeatBalFanSys->MCPTV(NZ) += state.dataZoneEquip->VentMCP(j) * state.dataHeatBal->Ventilation(j).AirTemp;
        }

        if (state.dataHeatBal->Ventilation(j).ModelType == DataHeatBalance::VentilationModelType::WindAndStack) {
            if (state.dataHeatBal->Ventilation(j).OpenEff != DataGlobalConstants::AutoCalculate) {
                Cw = state.dataHeatBal->Ventilation(j).OpenEff;
            } else {
                // linear interpolation between effective angle and wind direction
                angle = std::abs(WindDirExt - state.dataHeatBal->Ventilation(j).EffAngle);
                if (angle > 180.0) angle -= 180.0;
                Cw = 0.55 + angle / 180.0 * (0.3 - 0.55);
            }
            if (state.dataHeatBal->Ventilation(j).DiscCoef != DataGlobalConstants::AutoCalculate) {
                Cd = state.dataHeatBal->Ventilation(j).DiscCoef;
            } else {
                Cd = 0.40 + 0.0045 * std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ));
            }
            Qw = Cw * state.dataHeatBal->Ventilation(j).OpenArea *
                 GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(j).OpenAreaSchedPtr) * WindSpeedExt;
            Qst = Cd * state.dataHeatBal->Ventilation(j).OpenArea *
                  GetCurrentScheduleValue(state, state.dataHeatBal->Ventilation(j).OpenAreaSchedPtr) *
                  std::sqrt(2.0 * 9.81 * state.dataHeatBal->Ventilation(j).DH * std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ)) /
                            (state.dataZoneEquip->ZMAT(NZ) + 273.15));
            VVF = std::sqrt(Qw * Qw + Qst * Qst);
            if (state.dataHeatBal->Ventilation(j).EMSSimpleVentOn) VVF = state.dataHeatBal->Ventilation(j).EMSimpleVentFlowRate;
            if (VVF < 0.0) VVF = 0.0;
            state.dataZoneEquip->VentMCP(j) = VVF * AirDensity * CpAir;
            if (state.dataZoneEquip->VentMCP(j) < 0.0) state.dataZoneEquip->VentMCP(j) = 0.0;
            if (state.dataHeatBal->Ventilation(j).QuadratureSum) {
                state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Ventilation(j).OABalancePtr).NatMassFlowRate +=
                    state.dataZoneEquip->VentMCP(j) / CpAir;
            } else {
                state.dataHeatBalFanSys->MCPV(NZ) += state.dataZoneEquip->VentMCP(j);
                VAMFL_temp = state.dataZoneEquip->VentMCP(j) / CpAir;
                state.dataHeatBalFanSys->VAMFL(NZ) += VAMFL_temp;
                state.dataHeatBal->Ventilation(j).AirTemp = TempExt;
                state.dataHeatBalFanSys->MCPTV(NZ) += state.dataZoneEquip->VentMCP(j) * state.dataHeatBal->Ventilation(j).AirTemp;
            }
        }
    }

    // Process Mixing
    for (j = 1; j <= state.dataHeatBal->TotMixing; ++j) {
        n = state.dataHeatBal->Mixing(j).ZonePtr;
        m = state.dataHeatBal->Mixing(j).FromZone;
        TD = state.dataHeatBal->Mixing(j).DeltaTemperature;
        // Get scheduled delta temperature
        if (state.dataHeatBal->Mixing(j).DeltaTempSchedPtr > 0) {
            TD = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).DeltaTempSchedPtr);
        }
        TZN = state.dataZoneEquip->ZMAT(n);
        TZM = state.dataZoneEquip->ZMAT(m);

        // Hybrid ventilation controls
        if (state.dataHeatBal->Mixing(j).HybridControlType == DataHeatBalance::HybridCtrlType::Close) continue;
        // Check temperature limit
        MixingLimitFlag = false;

        // Hybrid ventilation global control
        if (state.dataHeatBal->Mixing(j).HybridControlType == DataHeatBalance::HybridCtrlType::Global &&
            state.dataHeatBal->Mixing(j).HybridControlMasterNum > 0) {
            I = state.dataHeatBal->Mixing(j).HybridControlMasterNum;
            if (!state.dataHeatBal->Ventilation(I).HybridControlMasterStatus) continue;
        } else {
            // Ensure the minimum indoor temperature <= the maximum indoor temperature
            if (state.dataHeatBal->Mixing(j).MinIndoorTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MinIndoorTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MaxIndoorTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MaxIndoorTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MinIndoorTempSchedPtr > 0 && state.dataHeatBal->Mixing(j).MaxIndoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->Mixing(j).IndoorTempErrCount;
                    if (state.dataHeatBal->Mixing(j).IndoorTempErrCount < 2) {
                        ShowWarningError(state,
                                         "Mixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in " +
                                             state.dataHeatBal->Mixing(j).Name);
                        ShowContinueError(state, "The minimum zone temperature is set to the maximum zone temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum zone temperature is still above the maximum zone temperature",
                                                       state.dataHeatBal->Mixing(j).IndoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->Mixing(j).MinIndoorTempSchedPtr > 0) {
                if (TZN < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->Mixing(j).MaxIndoorTempSchedPtr > 0) {
                if (TZN > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum source temperature <= the maximum source temperature
            if (state.dataHeatBal->Mixing(j).MinSourceTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MinSourceTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MaxSourceTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MaxSourceTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MinSourceTempSchedPtr > 0 && state.dataHeatBal->Mixing(j).MaxSourceTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->Mixing(j).SourceTempErrCount;
                    if (state.dataHeatBal->Mixing(j).SourceTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            "Mixing source temperature control: The minimum source temperature is above the maximum source temperature in " +
                                state.dataHeatBal->Mixing(j).Name);
                        ShowContinueError(state, "The minimum source temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum source temperature is still above the maximum source temperature",
                                                       state.dataHeatBal->Mixing(j).SourceTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->Mixing(j).MinSourceTempSchedPtr > 0) {
                if (TZM < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->Mixing(j).MaxSourceTempSchedPtr > 0) {
                if (TZM > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
            TempExt = state.dataHeatBal->Zone(n).OutDryBulbTemp;
            if (state.dataHeatBal->Mixing(j).MinOutdoorTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MinOutdoorTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MaxOutdoorTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(j).MaxOutdoorTempSchedPtr);
            if (state.dataHeatBal->Mixing(j).MinOutdoorTempSchedPtr > 0 && state.dataHeatBal->Mixing(j).MaxOutdoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->Mixing(j).OutdoorTempErrCount;
                    if (state.dataHeatBal->Mixing(j).OutdoorTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            "Mixing outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in " +
                                state.dataHeatBal->Mixing(j).Name);
                        ShowContinueError(state, "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                       state.dataHeatBal->Mixing(j).OutdoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->Mixing(j).MinOutdoorTempSchedPtr > 0) {
                if (TempExt < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->Mixing(j).MaxOutdoorTempSchedPtr > 0) {
                if (TempExt > MixingTmax) MixingLimitFlag = true;
            }
        }

        if (state.dataHeatBal->Mixing(j).HybridControlType != DataHeatBalance::HybridCtrlType::Global && MixingLimitFlag) continue;
        if (state.dataHeatBal->Mixing(j).HybridControlType == DataHeatBalance::HybridCtrlType::Global) TD = 0.0;

        //  If TD equals zero (default) set coefficients for full mixing otherwise test
        //    for mixing conditions if user input delta temp > 0, then from zone temp (TZM)
        //    must be td degrees warmer than zone temp (TZN).  If user input delta temp < 0,
        //    then from zone temp (TZM) must be TD degrees cooler than zone temp (TZN).
        if (TD < 0.0) {
            if (TZM < TZN + TD) {
                //            Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
                //             RhoAirM = PsyRhoAirFnPbTdbW(state, OutBaroPress,tzm,ZHumRat(m))
                //             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnW(ZHumRat(m),tzm) * RhoAirM
                AirDensity = PsyRhoAirFnPbTdbW(state,
                                               state.dataEnvrn->OutBaroPress,
                                               (TZN + TZM) / 2.0,
                                               (state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0);
                CpAir = PsyCpAirFnW((state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0); // Use average conditions

                state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRateSaved;
                if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag) {
                    if (state.dataHeatBal->Mixing(j).MixingMassFlowRate > 0.0) {
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).MixingMassFlowRate / AirDensity;
                    }
                }
                state.dataHeatBal->Mixing(j).MixingMassFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;

                MCP = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * CpAir * AirDensity;
                state.dataHeatBalFanSys->MCPM(n) += MCP;
                state.dataHeatBalFanSys->MCPTM(n) += MCP * TZM;

                // Now to determine the moisture conditions
                state.dataHeatBalFanSys->MixingMassFlowZone(n) += state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(n) +=
                    state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataZoneEquip->ZHumRat(m);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(n) +=
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(m);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(n) +=
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(m);
                }
                state.dataZoneEquip->MixingReportFlag(j) = true;
            }
        }
        if (TD > 0.0) {
            if (TZM > TZN + TD) {
                //             RhoAirM = PsyRhoAirFnPbTdbW(state, OutBaroPress,tzm,ZHumRat(m))
                //             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnW(ZHumRat(m),tzm) * RhoAirM
                AirDensity = PsyRhoAirFnPbTdbW(state,
                                               state.dataEnvrn->OutBaroPress,
                                               (TZN + TZM) / 2.0,
                                               (state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0); // Use avg conditions
                CpAir = PsyCpAirFnW((state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0);            // Use average conditions

                state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRateSaved;
                if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag) {
                    if (state.dataHeatBal->Mixing(j).MixingMassFlowRate > 0.0) {
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).MixingMassFlowRate / AirDensity;
                    }
                }
                state.dataHeatBal->Mixing(j).MixingMassFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;

                MCP = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * CpAir * AirDensity;
                state.dataHeatBalFanSys->MCPM(n) += MCP;
                state.dataHeatBalFanSys->MCPTM(n) += MCP * TZM;
                // Now to determine the moisture conditions
                state.dataHeatBalFanSys->MixingMassFlowZone(n) += state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(n) +=
                    state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataZoneEquip->ZHumRat(m);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(n) +=
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(m);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(n) +=
                        state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(m);
                }
                state.dataZoneEquip->MixingReportFlag(j) = true;
            }
        }
        if (TD == 0.0) {
            //          RhoAirM = PsyRhoAirFnPbTdbW(state, OutBaroPress,tzm,ZHumRat(m))
            //          MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnW(ZHumRat(m),tzm) * RhoAirM
            AirDensity = PsyRhoAirFnPbTdbW(state,
                                           state.dataEnvrn->OutBaroPress,
                                           (TZN + TZM) / 2.0,
                                           (state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0,
                                           RoutineNameMixing);                                              // Use avg conditions
            CpAir = PsyCpAirFnW((state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0); // Use average conditions

            state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRateSaved;
            if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag) {
                if (state.dataHeatBal->Mixing(j).MixingMassFlowRate > 0.0) {
                    state.dataHeatBal->Mixing(j).DesiredAirFlowRate = state.dataHeatBal->Mixing(j).MixingMassFlowRate / AirDensity;
                }
            }
            state.dataHeatBal->Mixing(j).MixingMassFlowRate = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;

            MCP = state.dataHeatBal->Mixing(j).DesiredAirFlowRate * CpAir * AirDensity;
            state.dataHeatBalFanSys->MCPM(n) += MCP;
            state.dataHeatBalFanSys->MCPTM(n) += MCP * TZM;
            // Now to determine the moisture conditions
            state.dataHeatBalFanSys->MixingMassFlowZone(n) += state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity;
            state.dataHeatBalFanSys->MixingMassFlowXHumRat(n) +=
                state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataZoneEquip->ZHumRat(m);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->MixingMassFlowCO2(n) +=
                    state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(m);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->MixingMassFlowGC(n) +=
                    state.dataHeatBal->Mixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(m);
            }
            state.dataZoneEquip->MixingReportFlag(j) = true;
        }
    }

    //                              COMPUTE CROSS ZONE
    //                              AIR MIXING
    for (j = 1; j <= state.dataHeatBal->TotCrossMixing; ++j) {
        n = state.dataHeatBal->CrossMixing(j).ZonePtr;
        m = state.dataHeatBal->CrossMixing(j).FromZone;
        TD = state.dataHeatBal->CrossMixing(j).DeltaTemperature;
        // Get scheduled delta temperature
        if (state.dataHeatBal->CrossMixing(j).DeltaTempSchedPtr > 0) {
            TD = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).DeltaTempSchedPtr);
        }

        if (TD >= 0.0) {
            TZN = state.dataZoneEquip->ZMAT(n);
            TZM = state.dataZoneEquip->ZMAT(m);
            // Check temperature limit
            MixingLimitFlag = false;
            // Ensure the minimum indoor temperature <= the maximum indoor temperature
            if (state.dataHeatBal->CrossMixing(j).MinIndoorTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MinIndoorTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MaxIndoorTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MaxIndoorTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MinIndoorTempSchedPtr > 0 && state.dataHeatBal->CrossMixing(j).MaxIndoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->CrossMixing(j).IndoorTempErrCount;
                    if (state.dataHeatBal->CrossMixing(j).IndoorTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            "CrossMixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in " +
                                state.dataHeatBal->CrossMixing(j).Name);
                        ShowContinueError(state, "The minimum zone temperature is set to the maximum zone temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum zone temperature is still above the maximum zone temperature",
                                                       state.dataHeatBal->CrossMixing(j).IndoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->CrossMixing(j).MinIndoorTempSchedPtr > 0) {
                if (TZN < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->CrossMixing(j).MaxIndoorTempSchedPtr > 0) {
                if (TZN > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum source temperature <= the maximum source temperature
            if (state.dataHeatBal->CrossMixing(j).MinSourceTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MinSourceTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MaxSourceTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MaxSourceTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MinSourceTempSchedPtr > 0 && state.dataHeatBal->CrossMixing(j).MaxSourceTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->CrossMixing(j).SourceTempErrCount;
                    if (state.dataHeatBal->CrossMixing(j).SourceTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            "CrossMixing source temperature control: The minimum source temperature is above the maximum source temperature in " +
                                state.dataHeatBal->CrossMixing(j).Name);
                        ShowContinueError(state, "The minimum source temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum source temperature is still above the maximum source temperature",
                                                       state.dataHeatBal->CrossMixing(j).SourceTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->CrossMixing(j).MinSourceTempSchedPtr > 0) {
                if (TZM < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->CrossMixing(j).MaxSourceTempSchedPtr > 0) {
                if (TZM > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
            TempExt = state.dataHeatBal->Zone(n).OutDryBulbTemp;
            if (state.dataHeatBal->CrossMixing(j).MinOutdoorTempSchedPtr > 0)
                MixingTmin = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MinOutdoorTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MaxOutdoorTempSchedPtr > 0)
                MixingTmax = GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(j).MaxOutdoorTempSchedPtr);
            if (state.dataHeatBal->CrossMixing(j).MinOutdoorTempSchedPtr > 0 && state.dataHeatBal->CrossMixing(j).MaxOutdoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++state.dataHeatBal->CrossMixing(j).OutdoorTempErrCount;
                    if (state.dataHeatBal->CrossMixing(j).OutdoorTempErrCount < 2) {
                        ShowWarningError(state,
                                         "CrossMixing outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor "
                                         "temperature in " +
                                             state.dataHeatBal->Mixing(j).Name);
                        ShowContinueError(state, "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                       state.dataHeatBal->CrossMixing(j).OutdoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (state.dataHeatBal->CrossMixing(j).MinOutdoorTempSchedPtr > 0) {
                if (TempExt < MixingTmin) MixingLimitFlag = true;
            }
            if (state.dataHeatBal->CrossMixing(j).MaxOutdoorTempSchedPtr > 0) {
                if (TempExt > MixingTmax) MixingLimitFlag = true;
            }
            if (MixingLimitFlag) continue;

            if ((TD == 0.0 || (TD > 0.0 && (TZM - TZN) >= TD))) {
                state.dataZoneEquip->CrossMixingReportFlag(j) = true; // set reporting flag
            }

            if ((TD <= 0.0) || ((TD > 0.0) && (TZM - TZN >= TD))) {
                //                                      SET COEFFICIENTS .
                Tavg = (TZN + TZM) / 2.0;
                Wavg = (state.dataZoneEquip->ZHumRat(n) + state.dataZoneEquip->ZHumRat(m)) / 2.0;
                AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tavg, Wavg, RoutineNameCrossMixing);
                CpAir = PsyCpAirFnW(Wavg);
                MCPxN = state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * CpAir * AirDensity;
                state.dataHeatBalFanSys->MCPM(n) += MCPxN;

                MCPxM = state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * CpAir * AirDensity;
                state.dataHeatBalFanSys->MCPM(m) += MCPxM;
                state.dataHeatBalFanSys->MCPTM(n) += MCPxM * TZM;
                state.dataHeatBalFanSys->MCPTM(m) += MCPxN * TZN;

                // Now to determine the moisture conditions
                state.dataHeatBalFanSys->MixingMassFlowZone(m) += state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(m) +=
                    state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataZoneEquip->ZHumRat(n);

                state.dataHeatBalFanSys->MixingMassFlowZone(n) += state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(n) +=
                    state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataZoneEquip->ZHumRat(m);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(m) +=
                        state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(n);
                    state.dataContaminantBalance->MixingMassFlowCO2(n) +=
                        state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(m);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(m) +=
                        state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(n);
                    state.dataContaminantBalance->MixingMassFlowGC(n) +=
                        state.dataHeatBal->CrossMixing(j).DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(m);
                }
            }
        }
    }

    //                              COMPUTE REFRIGERATION DOOR
    //                              AIR MIXING
    if (state.dataHeatBal->TotRefDoorMixing > 0) {
        // Zone loops structured in getinput so only do each pair of zones bounding door once, even if multiple doors in one zone
        for (ZoneA = 1; ZoneA <= (state.dataGlobal->NumOfZones - 1); ++ZoneA) {
            if (!state.dataHeatBal->RefDoorMixing(ZoneA).RefDoorMixFlag) continue;
            for (j = 1; j <= state.dataHeatBal->RefDoorMixing(ZoneA).NumRefDoorConnections; ++j) {
                ZoneB = state.dataHeatBal->RefDoorMixing(ZoneA).MateZonePtr(j);
                TZoneA = state.dataZoneEquip->ZMAT(ZoneA);
                TZoneB = state.dataZoneEquip->ZMAT(ZoneB);
                HumRatZoneA = state.dataZoneEquip->ZHumRat(ZoneA);
                HumRatZoneB = state.dataZoneEquip->ZHumRat(ZoneB);
                AirDensityZoneA = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TZoneA, HumRatZoneA, RoutineNameRefrigerationDoorMixing);
                CpAirZoneA = PsyCpAirFnW(HumRatZoneA);
                AirDensityZoneB = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TZoneB, HumRatZoneB, RoutineNameRefrigerationDoorMixing);
                CpAirZoneB = PsyCpAirFnW(HumRatZoneB);
                Tavg = (TZoneA + TZoneB) / 2.0;
                Wavg = (HumRatZoneA + HumRatZoneB) / 2.0;
                AirDensityAvg = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tavg, Wavg, RoutineNameRefrigerationDoorMixing);

                if (state.dataHeatBal->RefDoorMixing(ZoneA).EMSRefDoorMixingOn(j)) {
                    MassFlowDryAir = state.dataHeatBal->RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensityAvg;
                } else {
                    SchedDoorOpen = GetCurrentScheduleValue(state, state.dataHeatBal->RefDoorMixing(ZoneA).OpenSchedPtr(j));
                    if (SchedDoorOpen == 0.0) continue;
                    DoorHeight = state.dataHeatBal->RefDoorMixing(ZoneA).DoorHeight(j);
                    DoorArea = state.dataHeatBal->RefDoorMixing(ZoneA).DoorArea(j);
                    DoorProt = state.dataHeatBal->RefDoorMixing(ZoneA).Protection(j);
                    if (AirDensityZoneA >= AirDensityZoneB) {
                        // Mass of dry air flow between zones is equal,
                        // but have to calc directionally to avoid sqrt(neg number)
                        FDens = std::pow(2.0 / (1.0 + std::pow(AirDensityZoneA / AirDensityZoneB, 1.0 / 3.0)), 1.5);
                        Fb = 0.221 * DoorArea * AirDensityZoneA * FDens *
                             std::sqrt((1.0 - AirDensityZoneB / AirDensityZoneA) * StdGravity * DoorHeight);
                    } else { // ZoneADens < ZoneBDens
                        FDens = std::pow(2.0 / (1.0 + std::pow(AirDensityZoneB / AirDensityZoneA, 1.0 / 3.0)), 1.5);
                        Fb = 0.221 * DoorArea * AirDensityZoneB * FDens *
                             std::sqrt((1.0 - AirDensityZoneA / AirDensityZoneB) * StdGravity * DoorHeight);
                    } // ZoneADens .GE. ZoneBDens
                    // FFlow = Doorway flow factor, is determined by temperature difference
                    FFlow = 1.1;
                    if (std::abs(TZoneA - TZoneB) > 11.0) FFlow = 0.8;
                    MassFlowDryAir = Fb * SchedDoorOpen * FFlow * (1.0 - DoorProt);
                    state.dataHeatBal->RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) = MassFlowDryAir / AirDensityAvg;
                    // Note - VolRefDoorFlowRate is used ONLY for reporting purposes, where it is
                    //       used with the avg density to generate a reported mass flow
                    //       Considering the small values typical for HumRat, this is not far off.
                } // EMSRefDoorMixingOn

                MassFlowToA = MassFlowDryAir * (1.0 + HumRatZoneB);
                MassFlowToB = MassFlowDryAir * (1.0 + HumRatZoneA);
                MassFlowXCpToA = MassFlowToA * CpAirZoneB;
                MassFlowXCpToB = MassFlowToB * CpAirZoneA;
                MassFlowXCpXTempToA = MassFlowXCpToA * TZoneB;
                MassFlowXCpXTempToB = MassFlowXCpToB * TZoneA;
                MassFlowXHumRatToA = MassFlowToA * HumRatZoneB;
                MassFlowXHumRatToB = MassFlowToB * HumRatZoneA;

                state.dataHeatBalFanSys->MCPM(ZoneA) += MassFlowXCpToA;
                state.dataHeatBalFanSys->MCPM(ZoneB) += MassFlowXCpToB;
                state.dataHeatBalFanSys->MCPTM(ZoneA) += MassFlowXCpXTempToA;
                state.dataHeatBalFanSys->MCPTM(ZoneB) += MassFlowXCpXTempToB;

                // Now to determine the moisture conditions
                state.dataHeatBalFanSys->MixingMassFlowZone(ZoneA) += MassFlowToA;
                state.dataHeatBalFanSys->MixingMassFlowZone(ZoneB) += MassFlowToB;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneA) += MassFlowXHumRatToA;
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneB) += MassFlowXHumRatToB;

                // Now to determine the CO2 and generic contaminant conditions
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(ZoneA) += MassFlowToA * state.dataContaminantBalance->ZoneAirCO2(ZoneB);
                    state.dataContaminantBalance->MixingMassFlowCO2(ZoneB) += MassFlowToB * state.dataContaminantBalance->ZoneAirCO2(ZoneA);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(ZoneA) += MassFlowToA * state.dataContaminantBalance->ZoneAirGC(ZoneB);
                    state.dataContaminantBalance->MixingMassFlowGC(ZoneB) += MassFlowToB * state.dataContaminantBalance->ZoneAirGC(ZoneA);
                }

            } // J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
        }     // ZoneA=1,(NumOfZones - 1)
    }         //(TotRefrigerationDoorMixing > 0) THEN

    // Process the scheduled Infiltration for air heat balance depending on model type
    for (j = 1; j <= state.dataHeatBal->TotInfiltration; ++j) {

        NZ = state.dataHeatBal->Infiltration(j).ZonePtr;

        TempExt = state.dataHeatBal->Zone(NZ).OutDryBulbTemp;
        WindSpeedExt = state.dataHeatBal->Zone(NZ).WindSpeed;

        // Use air node information linked to the zone if defined

        if (state.dataHeatBal->Zone(NZ).HasLinkedOutAirNode) {
            HumRatExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(NZ).LinkedOutAirNode).HumRat;
        } else {
            HumRatExt = state.dataEnvrn->OutHumRat;
        }

        AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempExt, HumRatExt, RoutineNameInfiltration);
        CpAir = PsyCpAirFnW(HumRatExt);

        // CR7751  should maybe use code below, indoor conditions instead of outdoor conditions
        //   AirDensity = PsyRhoAirFnPbTdbW(state, OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
        //   CpAir = PsyCpAirFnW(ZHumRat(NZ),ZMAT(NZ))
        switch (state.dataHeatBal->Infiltration(j).ModelType) {
        case DataHeatBalance::InfiltrationModelType::DesignFlowRate: {
            IVF = state.dataHeatBal->Infiltration(j).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->Infiltration(j).SchedPtr);
            // CR6845 if calculated < 0.0, don't propagate
            if (IVF < 0.0) IVF = 0.0;
            MCpI_temp = IVF * AirDensity * CpAir *
                        (state.dataHeatBal->Infiltration(j).ConstantTermCoef +
                         std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ)) * state.dataHeatBal->Infiltration(j).TemperatureTermCoef +
                         WindSpeedExt * (state.dataHeatBal->Infiltration(j).VelocityTermCoef +
                                         WindSpeedExt * state.dataHeatBal->Infiltration(j).VelocitySQTermCoef));

            if (MCpI_temp < 0.0) MCpI_temp = 0.0;
            state.dataHeatBal->Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            if (AdjustZoneInfiltrationFlowFlag && state.dataHeatBalFanSys->ZoneInfiltrationFlag(NZ)) {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    // if ( Infiltration(j).MassFlowRate > 0.0 ) {
                    state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).MassFlowRate / AirDensity;
                    MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                    //}
                }
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate +
                                                                        state.dataHeatBal->MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
                    MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                }
            }
            state.dataHeatBal->Infiltration(j).MassFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity;
        } break;
        case DataHeatBalance::InfiltrationModelType::ShermanGrimsrud: {
            // Sherman Grimsrud model as formulated in ASHRAE HoF
            WindSpeedExt = state.dataEnvrn->WindSpeed; // formulated to use wind at Meterological Station rather than local
            IVF = GetCurrentScheduleValue(state, state.dataHeatBal->Infiltration(j).SchedPtr) * state.dataHeatBal->Infiltration(j).LeakageArea /
                  1000.0 *
                  std::sqrt(state.dataHeatBal->Infiltration(j).BasicStackCoefficient * std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ)) +
                            state.dataHeatBal->Infiltration(j).BasicWindCoefficient * pow_2(WindSpeedExt));
            if (IVF < 0.0) IVF = 0.0;
            MCpI_temp = IVF * AirDensity * CpAir;
            if (MCpI_temp < 0.0) MCpI_temp = 0.0;
            state.dataHeatBal->Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            if (AdjustZoneInfiltrationFlowFlag && state.dataHeatBalFanSys->ZoneInfiltrationFlag(NZ)) {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    if (state.dataHeatBal->Infiltration(j).MassFlowRate > 0.0) {
                        state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).MassFlowRate / AirDensity;
                        MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                    }
                }
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate +
                                                                        state.dataHeatBal->MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
                    MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                }
            }
            state.dataHeatBal->Infiltration(j).MassFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity;
        } break;
        case DataHeatBalance::InfiltrationModelType::AIM2: {
            // Walker Wilson model as formulated in ASHRAE HoF
            IVF = GetCurrentScheduleValue(state, state.dataHeatBal->Infiltration(j).SchedPtr) *
                  std::sqrt(pow_2(state.dataHeatBal->Infiltration(j).FlowCoefficient * state.dataHeatBal->Infiltration(j).AIM2StackCoefficient *
                                  std::pow(std::abs(TempExt - state.dataZoneEquip->ZMAT(NZ)), state.dataHeatBal->Infiltration(j).PressureExponent)) +
                            pow_2(state.dataHeatBal->Infiltration(j).FlowCoefficient * state.dataHeatBal->Infiltration(j).AIM2WindCoefficient *
                                  std::pow(state.dataHeatBal->Infiltration(j).ShelterFactor * WindSpeedExt,
                                           2.0 * state.dataHeatBal->Infiltration(j).PressureExponent)));
            if (IVF < 0.0) IVF = 0.0;
            MCpI_temp = IVF * AirDensity * CpAir;
            if (MCpI_temp < 0.0) MCpI_temp = 0.0;
            state.dataHeatBal->Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            if (AdjustZoneInfiltrationFlowFlag && state.dataHeatBalFanSys->ZoneInfiltrationFlag(NZ)) {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    if (state.dataHeatBal->Infiltration(j).MassFlowRate > 0.0) {
                        state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).MassFlowRate / AirDensity;
                        MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                    }
                }
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    state.dataHeatBal->Infiltration(j).VolumeFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate +
                                                                        state.dataHeatBal->MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
                    MCpI_temp = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
                }
            }
            state.dataHeatBal->Infiltration(j).MassFlowRate = state.dataHeatBal->Infiltration(j).VolumeFlowRate * AirDensity;
        } break;
        default:
            break;
        }

        if (state.dataHeatBal->Infiltration(j).EMSOverrideOn) {
            IVF = state.dataHeatBal->Infiltration(j).EMSAirFlowRateValue;
            if (IVF < 0.0) IVF = 0.0;
            MCpI_temp = IVF * AirDensity * CpAir;
            if (MCpI_temp < 0.0) MCpI_temp = 0.0;
        }

        if (state.dataHeatBal->Infiltration(j).QuadratureSum) {
            state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Infiltration(j).OABalancePtr).InfMassFlowRate += MCpI_temp / CpAir;
        } else {
            state.dataHeatBal->Infiltration(j).MCpI_temp = MCpI_temp;
            state.dataHeatBalFanSys->MCPI(NZ) += MCpI_temp;
            state.dataHeatBalFanSys->OAMFL(NZ) += MCpI_temp / CpAir;
            state.dataHeatBalFanSys->MCPTI(NZ) += MCpI_temp * TempExt;
        }
    }

    // Add infiltration rate enhanced by the existence of thermal chimney
    for (NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
        state.dataHeatBalFanSys->MCPI(NZ) += state.dataHeatBalFanSys->MCPThermChim(NZ);
        state.dataHeatBalFanSys->OAMFL(NZ) += state.dataHeatBalFanSys->ThermChimAMFL(NZ);
        state.dataHeatBalFanSys->MCPTI(NZ) += state.dataHeatBalFanSys->MCPTThermChim(NZ);
    }

    // Calculate combined outdoor air flows
    for (j = 1; j <= state.dataHeatBal->TotZoneAirBalance; ++j) {
        if (state.dataHeatBal->ZoneAirBalance(j).BalanceMethod == AirBalance::Quadrature) {
            if (!state.dataHeatBal->ZoneAirBalance(j).OneTimeFlag) GetStandAloneERVNodes(state, j);
            if (state.dataHeatBal->ZoneAirBalance(j).NumOfERVs > 0) {
                for (I = 1; I <= state.dataHeatBal->ZoneAirBalance(j).NumOfERVs; ++I) {
                    MassFlowDiff = state.dataLoopNodes->Node(state.dataHeatBal->ZoneAirBalance(j).ERVExhaustNode(I)).MassFlowRate -
                                   state.dataLoopNodes->Node(state.dataHeatBal->ZoneAirBalance(j).ERVInletNode(I)).MassFlowRate;
                    if (MassFlowDiff > 0.0) {
                        state.dataHeatBal->ZoneAirBalance(j).ERVMassFlowRate += MassFlowDiff;
                    }
                }
            }
            NZ = state.dataHeatBal->ZoneAirBalance(j).ZonePtr;
            AirDensity = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataHeatBal->Zone(NZ).OutDryBulbTemp, HumRatExt, RoutineNameZoneAirBalance);
            CpAir = PsyCpAirFnW(HumRatExt);
            state.dataHeatBal->ZoneAirBalance(j).ERVMassFlowRate *= AirDensity;
            state.dataHeatBalFanSys->MDotOA(NZ) =
                std::sqrt(pow_2(state.dataHeatBal->ZoneAirBalance(j).NatMassFlowRate) + pow_2(state.dataHeatBal->ZoneAirBalance(j).IntMassFlowRate) +
                          pow_2(state.dataHeatBal->ZoneAirBalance(j).ExhMassFlowRate) + pow_2(state.dataHeatBal->ZoneAirBalance(j).ERVMassFlowRate) +
                          pow_2(state.dataHeatBal->ZoneAirBalance(j).InfMassFlowRate) +
                          pow_2(AirDensity * state.dataHeatBal->ZoneAirBalance(j).InducedAirRate *
                                GetCurrentScheduleValue(state, state.dataHeatBal->ZoneAirBalance(j).InducedAirSchedPtr))) +
                state.dataHeatBal->ZoneAirBalance(j).BalMassFlowRate;
            state.dataHeatBalFanSys->MDotCPOA(NZ) = state.dataHeatBalFanSys->MDotOA(NZ) * CpAir;
        }
    }
}

void GetStandAloneERVNodes(EnergyPlusData &state, int const OutdoorNum) // Zone Air Balance Outdoor index
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets node numbers of stand alone ERVs to calculate combined outdoor air flows.

    // METHODOLOGY EMPLOYED:
    // Uses program data structures ZoneEquipInfo

    // Using/Aliasing
    using HVACStandAloneERV::GetStandAloneERVOutAirNode;
    using HVACStandAloneERV::GetStandAloneERVReturnAirNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum(0); // zone index
    int j;          // index
    int I;          // index

    if (allocated(state.dataZoneEquip->ZoneEquipList)) {
        ZoneNum = state.dataHeatBal->ZoneAirBalance(OutdoorNum).ZonePtr;
        state.dataHeatBal->ZoneAirBalance(OutdoorNum).OneTimeFlag = true;
        if (state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes > 0) {
            for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
                if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipTypeEnum(I) == DataZoneEquipment::ZoneEquip::ERVStandAlone) {
                    ++state.dataHeatBal->ZoneAirBalance(OutdoorNum).NumOfERVs;
                }
            }
            if (state.dataHeatBal->ZoneAirBalance(OutdoorNum).NumOfERVs > 0) {
                state.dataHeatBal->ZoneAirBalance(OutdoorNum).ERVInletNode.allocate(state.dataHeatBal->ZoneAirBalance(OutdoorNum).NumOfERVs);
                state.dataHeatBal->ZoneAirBalance(OutdoorNum).ERVExhaustNode.allocate(state.dataHeatBal->ZoneAirBalance(OutdoorNum).NumOfERVs);
                j = 1;
                for (I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
                    if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipTypeEnum(I) == DataZoneEquipment::ZoneEquip::ERVStandAlone) {
                        state.dataHeatBal->ZoneAirBalance(OutdoorNum).ERVInletNode(j) =
                            GetStandAloneERVOutAirNode(state, state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipIndex(I));
                        state.dataHeatBal->ZoneAirBalance(OutdoorNum).ERVExhaustNode(j) =
                            GetStandAloneERVReturnAirNode(state, state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipIndex(I));
                        ++j;
                    }
                }
            }
        }
    }
}

void CalcZoneMixingFlowRateOfReceivingZone(EnergyPlusData &state, int const ZoneNum, Real64 &ZoneMixingMassFlowRate)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   February 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the receiving zone mixing flow rate to ensures the zone
    // air mass balance.

    int Loop;
    int MixingNum;
    int NumOfReceivingZoneMixingObjects;
    Real64 MixingMassFlowRate; // current zone mixing mass flow rate, [kg/s]

    MixingMassFlowRate = 0.0;
    NumOfReceivingZoneMixingObjects = state.dataHeatBal->MassConservation(ZoneNum).NumReceivingZonesMixingObject;
    if (NumOfReceivingZoneMixingObjects > 0) {
        // distribute the total zone mixing flow rate to the source zones
        for (Loop = 1; Loop <= NumOfReceivingZoneMixingObjects; ++Loop) {
            MixingNum = state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingPtr(Loop);
            state.dataHeatBal->Mixing(MixingNum).MixingMassFlowRate =
                state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingFr(Loop) * ZoneMixingMassFlowRate;
            MixingMassFlowRate += state.dataHeatBal->Mixing(MixingNum).MixingMassFlowRate;
            CalcZoneMixingFlowRateOfSourceZone(state, state.dataHeatBal->Mixing(MixingNum).FromZone);
        }
    }
    state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate = MixingMassFlowRate;
    ZoneMixingMassFlowRate = MixingMassFlowRate;
}

void CalcZoneMixingFlowRateOfSourceZone(EnergyPlusData &state, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   February 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the zone mixing flow rate such that it ensures the zone
    // air mass balance.

    int Loop;
    int MixingNum;
    int ZoneMixingNum;
    int NumOfSourceZoneMixingObjects;
    Real64 ZoneSourceMassFlowRate; // current zone as a source mass flow rate for zone mixing in other zones, [kg/s]

    ZoneSourceMassFlowRate = 0.0;
    NumOfSourceZoneMixingObjects = state.dataHeatBal->MassConservation(ZoneNum).NumSourceZonesMixingObject;
    if (NumOfSourceZoneMixingObjects > 0) {
        for (ZoneMixingNum = 1; ZoneMixingNum <= NumOfSourceZoneMixingObjects; ++ZoneMixingNum) {
            MixingNum = state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingSourcesPtr(ZoneMixingNum);
            for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                if (Loop == MixingNum) {
                    ZoneSourceMassFlowRate += state.dataHeatBal->Mixing(Loop).MixingMassFlowRate;
                }
            }
        }
    }
    state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate = ZoneSourceMassFlowRate;
}

void AutoCalcDOASControlStrategy(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS Function:
    // This subroutine does the autosizing calculations for the Sizing:Zone
    // DOAS input.

    // REFERENCES:
    // See IO Ref for suggested values

    int ZoneSizIndex;
    bool ErrorsFound;
    bool headerAlreadyPrinted = false;
    ErrorsFound = false;
    for (ZoneSizIndex = 1; ZoneSizIndex <= state.dataSize->NumZoneSizingInput; ++ZoneSizIndex) {
        if (state.dataSize->ZoneSizingInput(ZoneSizIndex).AccountForDOAS) {
            if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy == DOANeutralSup) {
                if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint = 21.1;
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint = 23.9;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint > 0.0) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint =
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint - 2.8;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint > 0.0 &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint =
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint + 2.8;
                }
                ReportZoneSizingDOASInputs(state,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName,
                                           "NeutralSupplyAir",
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint,
                                           headerAlreadyPrinted);
            } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy == DOANeutralDehumSup) {
                if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint = 14.4;
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint = 22.2;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint > 0.0) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint = 14.4;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint > 0.0 &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint = 22.2;
                }
                ReportZoneSizingDOASInputs(state,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName,
                                           "NeutralDehumidifiedSupplyAir",
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint,
                                           headerAlreadyPrinted);
            } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASControlStrategy == DOACoolSup) {
                if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint = 12.2;
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint = 14.4;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint == AutoSize &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint > 0.0) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint =
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint - 2.2;
                } else if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint > 0.0 &&
                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint == AutoSize) {
                    state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint =
                        state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint + 2.2;
                }
                ReportZoneSizingDOASInputs(state,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName,
                                           "ColdSupplyAir",
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint,
                                           state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint,
                                           headerAlreadyPrinted);
            }
            if (state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASLowSetpoint > state.dataSize->ZoneSizingInput(ZoneSizIndex).DOASHighSetpoint) {
                ShowSevereError(state, "For Sizing:Zone = " + state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName);
                ShowContinueError(state, "... Dedicated Outside Air Low Setpoint for Design must be less than the High Setpoint");
                ErrorsFound = true;
            }
        }
    }
    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in DOAS sizing input. Program terminates.");
    }
}

void ReportZoneSizingDOASInputs(EnergyPlusData &state,
                                std::string const &ZoneName,         // the name of the zone
                                std::string const &DOASCtrlStrategy, // DOAS control strategy
                                Real64 const DOASLowTemp,            // DOAS design low setpoint temperature [C]
                                Real64 const DOASHighTemp,           // DOAS design high setpoint temperature [C]
                                bool &headerAlreadyPrinted)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes the DOAS Sizing:Zone input for 1 zone to the eio file

    // Using/Aliasing

    // Formats
    static constexpr std::string_view Format_990(
        "! <Zone Sizing DOAS Inputs>, Zone Name, DOAS Design Control Strategy, DOAS Design Low Setpoint Temperature "
        "{C}, DOAS Design High Setpoint Temperature {C} ");

    if (!headerAlreadyPrinted) {
        print(state.files.eio, "{}\n", Format_990);
        headerAlreadyPrinted = true;
    }

    static constexpr std::string_view Format_991(" Zone Sizing DOAS Inputs, {}, {}, {:.3R}, {:.3R}\n");
    print(state.files.eio, Format_991, ZoneName, DOASCtrlStrategy, DOASLowTemp, DOASHighTemp);

    // BSLLC Start
    // if ( sqlite ) {
    //     state.dataSQLiteProcedures->sqlite->addSQLiteZoneSizingRecord( ZoneName, LoadType, CalcDesLoad, UserDesLoad, CalcDesFlow, UserDesFlow,
    //     DesDayName, PeakHrMin,
    //         PeakTemp, PeakHumRat, MinOAVolFlow, DOASHeatAddRate );
    // }
    // BSLLC Finish
}

} // namespace EnergyPlus::ZoneEquipmentManager
