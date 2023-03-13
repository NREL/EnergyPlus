// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
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

    // PURPOSE OF THIS SUBROUTINE:
    // Calls the zone thermal control simulations and the interfaces (water-air, refrigerant-air, steam-air, electric-electric, water-water, etc)

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

    // PURPOSE OF THIS SUBROUTINE:
    // Get all the system related equipment which may be attached to a zone

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        GetZoneEquipmentData(state);
    }

    state.dataZoneEquipmentManager->NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;

    int MaxNumOfEquipTypes = 0;
    for (int Counter = 1; Counter <= state.dataGlobal->NumOfZones; ++Counter) {
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

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the zone equipment prior to simulation.

    if (state.dataZoneEquipmentManager->InitZoneEquipmentOneTimeFlag) {
        state.dataZoneEquipmentManager->InitZoneEquipmentOneTimeFlag = false;
        state.dataSize->ZoneEqSizing.allocate(state.dataGlobal->NumOfZones);
        // setup zone equipment sequenced demand storage
        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListIndex == 0) continue;
            int ZoneEquipCount =
                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).EquipListIndex).NumOfEquipTypes;
            auto &thisZoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum);
            thisZoneSysEnergyDemand.NumZoneEquipment = ZoneEquipCount;
            thisZoneSysEnergyDemand.SequencedOutputRequired.allocate(ZoneEquipCount);
            thisZoneSysEnergyDemand.SequencedOutputRequiredToHeatingSP.allocate(ZoneEquipCount);
            thisZoneSysEnergyDemand.SequencedOutputRequiredToCoolingSP.allocate(ZoneEquipCount);
            auto &thisZoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum);
            thisZoneSysMoistureDemand.NumZoneEquipment = ZoneEquipCount;
            thisZoneSysMoistureDemand.SequencedOutputRequired.allocate(ZoneEquipCount);
            thisZoneSysMoistureDemand.SequencedOutputRequiredToHumidSP.allocate(ZoneEquipCount);
            thisZoneSysMoistureDemand.SequencedOutputRequiredToDehumidSP.allocate(ZoneEquipCount);
            state.dataSize->ZoneEqSizing(ControlledZoneNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);
            state.dataSize->ZoneEqSizing(ControlledZoneNum).SizingMethod = 0;
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation || state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                    auto &thisSpaceSysEnergyDemand = state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum);
                    thisSpaceSysEnergyDemand.NumZoneEquipment = ZoneEquipCount;
                    thisSpaceSysEnergyDemand.SequencedOutputRequired.allocate(ZoneEquipCount);
                    thisSpaceSysEnergyDemand.SequencedOutputRequiredToHeatingSP.allocate(ZoneEquipCount);
                    thisSpaceSysEnergyDemand.SequencedOutputRequiredToCoolingSP.allocate(ZoneEquipCount);
                    auto &thisSpaceSysMoistureDemand = state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum);
                    thisSpaceSysMoistureDemand.NumZoneEquipment = ZoneEquipCount;
                    thisSpaceSysMoistureDemand.SequencedOutputRequired.allocate(ZoneEquipCount);
                    thisSpaceSysMoistureDemand.SequencedOutputRequiredToHumidSP.allocate(ZoneEquipCount);
                    thisSpaceSysMoistureDemand.SequencedOutputRequiredToDehumidSP.allocate(ZoneEquipCount);
                }
            }
        }
    }

    // Do the Begin Environment initializations
    if (state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        state.dataZoneEquip->ZoneEquipAvail = DataHVACGlobals::NoAction;

        if (allocated(state.dataHVACGlobal->ZoneComp)) {
            for (int ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) {
                if (allocated(state.dataHVACGlobal->ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)) {
                    auto &zoneComp = state.dataHVACGlobal->ZoneComp(ZoneEquipType);
                    for (int ZoneCompNum = 1; ZoneCompNum <= zoneComp.TotalNumComp; ++ZoneCompNum) {
                        zoneComp.ZoneCompAvailMgrs(ZoneCompNum).AvailStatus = DataHVACGlobals::NoAction;
                        zoneComp.ZoneCompAvailMgrs(ZoneCompNum).StartTime = 0;
                        zoneComp.ZoneCompAvailMgrs(ZoneCompNum).StopTime = 0;
                    }
                }
            }
        }
        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;

            auto &zoneNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode);
            zoneNode.Temp = 20.0;
            zoneNode.MassFlowRate = 0.0;
            zoneNode.Quality = 1.0;
            zoneNode.Press = state.dataEnvrn->OutBaroPress;
            zoneNode.HumRat = state.dataEnvrn->OutHumRat;
            zoneNode.Enthalpy = PsyHFnTdbW(zoneNode.Temp, zoneNode.HumRat);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                zoneNode.CO2 = state.dataContaminantBalance->OutdoorCO2;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                zoneNode.GenContam = state.dataContaminantBalance->OutdoorGC;
            }

            for (int ZoneInNode = 1; ZoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++ZoneInNode) {
                auto &inNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInNode));
                inNode.Temp = 20.0;
                inNode.MassFlowRate = 0.0;
                inNode.Quality = 1.0;
                inNode.Press = state.dataEnvrn->OutBaroPress;
                inNode.HumRat = state.dataEnvrn->OutHumRat;
                inNode.Enthalpy = PsyHFnTdbW(inNode.Temp, inNode.HumRat);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    inNode.CO2 = state.dataContaminantBalance->OutdoorCO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    inNode.GenContam = state.dataContaminantBalance->OutdoorGC;
                }
            }

            for (int ZoneExhNode = 1; ZoneExhNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNode) {

                auto &exhNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNode));
                exhNode.Temp = 20.0;
                exhNode.MassFlowRate = 0.0;
                exhNode.Quality = 1.0;
                exhNode.Press = state.dataEnvrn->OutBaroPress;
                exhNode.HumRat = state.dataEnvrn->OutHumRat;
                exhNode.Enthalpy = PsyHFnTdbW(exhNode.Temp, exhNode.HumRat);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    exhNode.CO2 = state.dataContaminantBalance->OutdoorCO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    exhNode.GenContam = state.dataContaminantBalance->OutdoorGC;
                }
            }

            // BG CR 7122 following resets return air node.
            int NumRetNodes = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes;
            if (NumRetNodes > 0) {
                for (int nodeCount = 1; nodeCount <= NumRetNodes; ++nodeCount) {
                    auto &returnNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(nodeCount));
                    returnNode.Temp = 20.0;
                    returnNode.MassFlowRate = 0.0;
                    returnNode.Quality = 1.0;
                    returnNode.Press = state.dataEnvrn->OutBaroPress;
                    returnNode.HumRat = state.dataEnvrn->OutHumRat;
                    returnNode.Enthalpy = PsyHFnTdbW(returnNode.Temp, returnNode.HumRat);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        returnNode.CO2 = state.dataContaminantBalance->OutdoorCO2;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        returnNode.GenContam = state.dataContaminantBalance->OutdoorGC;
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

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        auto &zoneNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode);
        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExcessZoneExh = 0.0;

        if (FirstHVACIteration) {
            for (int ZoneExhNode = 1; ZoneExhNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNode) {
                auto &exhNode = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNode));
                exhNode.Temp = zoneNode.Temp;
                exhNode.HumRat = zoneNode.HumRat;
                exhNode.Enthalpy = zoneNode.Enthalpy;
                exhNode.Press = zoneNode.Press;
                exhNode.Quality = zoneNode.Quality;
                exhNode.MassFlowRate = 0.0;
                exhNode.MassFlowRateMaxAvail = 0.0;
                exhNode.MassFlowRateMinAvail = 0.0;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    exhNode.CO2 = zoneNode.CO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    exhNode.GenContam = zoneNode.GenContam;
                }
            }
        }
    }

    for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
        auto &airLoopFlow = state.dataAirLoop->AirLoopFlow(airLoop);
        airLoopFlow.SupFlow = 0.0;
        airLoopFlow.ZoneRetFlow = 0.0;
        airLoopFlow.SysRetFlow = 0.0;
        airLoopFlow.RecircFlow = 0.0;
        airLoopFlow.LeakFlow = 0.0;
        airLoopFlow.ExcessZoneExhFlow = 0.0;
    }
}

void SizeZoneEquipment(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000

    // PURPOSE OF THIS SUBROUTINE:
    // Performs the zone sizing calculations and fills the zone sizing
    // data arrays with the results of the calculation.

    // METHODOLOGY EMPLOYED:
    // Using the input from Zone Sizing objects and the Zone Equipment input,
    // for each controlled zone this subroutine performs a "purchased air" calculation
    // and saves the results in the zone sizing data arrays.

    static constexpr std::string_view RoutineName("SizeZoneEquipment");

    int SupplyAirNode1;                   // node number of 1st zone supply air node
    int SupplyAirNode2;                   // node number of 2nd zone supply air node
    int SupplyAirNode;                    // node number of supply air node for ideal air system
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

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ControlledZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;

        // use reference to eliminate lots of long lines in this function, after initial commit, so reviewers can see changes
        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum);
        auto &zoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum);
        auto &zoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum);
        auto &zone = state.dataHeatBal->Zone(ControlledZoneNum);

        thisZoneHB.NonAirSystemResponse = 0.0;
        thisZoneHB.SysDepZoneLoads = 0.0;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                // SpaceHB ToDo: For now allocate by space volume frac
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).NonAirSystemResponse = 0.0;
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).SysDepZoneLoads = 0.0;
            }
        }
        SysOutputProvided = 0.0;
        LatOutputProvided = 0.0;
        InitSystemOutputRequired(state, ControlledZoneNum, true);
        int ZoneNode = zoneEquipConfig.ZoneNode;
        SupplyAirNode = 0;
        SupplyAirNode1 = 0;
        SupplyAirNode2 = 0;
        // save raw zone loads without impact of outdoor air
        Real64 LatOutputProvidedNoDOAS = zoneSysMoistureDemand.RemainingOutputRequired;
        Real64 SysOutputProvidedNoDOAS = zoneSysEnergyDemand.RemainingOutputRequired;
        // if Tstat deadband is true then load will be reported as 0
        if (state.dataZoneEnergyDemand->DeadBandOrSetback(ControlledZoneNum)) SysOutputProvidedNoDOAS = 0.0;
        // replicate deadband flag - zone condition is either below the humidistat or above the dehumidistat set point
        // using logic: NOT (!) (there is a load)
        // Pretty sure this could just be if (OutputRequiredToHumidifyingSP < 0 && OutputRequiredToDehumidifyingSP > 0)
        if (!((zoneSysMoistureDemand.OutputRequiredToHumidifyingSP > 0.0 && zoneSysMoistureDemand.OutputRequiredToDehumidifyingSP > 0.0) ||
              (zoneSysMoistureDemand.OutputRequiredToHumidifyingSP < 0.0 && zoneSysMoistureDemand.OutputRequiredToDehumidifyingSP < 0.0))) {
            LatOutputProvidedNoDOAS = 0.0;
        }

        // calculate DOAS heating/cooling effect
        if (calcZoneSizing.AccountForDOAS) {
            // check for adequate number of supply nodes
            if (zoneEquipConfig.NumInletNodes >= 2) {
                SupplyAirNode1 = zoneEquipConfig.InletNode(1);
                SupplyAirNode2 = zoneEquipConfig.InletNode(2);
            } else if (zoneEquipConfig.NumInletNodes >= 1) {
                SupplyAirNode1 = zoneEquipConfig.InletNode(1);
                SupplyAirNode2 = 0;
            } else {
                ShowSevereError(state, format("{}: to account for the effect a Dedicated Outside Air System on zone equipment sizing", RoutineName));
                ShowContinueError(state, "there must be at least one zone air inlet node");
                ShowFatalError(state, "Previous severe error causes abort ");
            }
            // set the DOAS mass flow rate and supply temperature and humidity ratio
            HR90H = PsyWFnTdbRhPb(state, calcZoneSizing.DOASHighSetpoint, 0.9, state.dataEnvrn->StdBaroPress);
            HR90L = PsyWFnTdbRhPb(state, calcZoneSizing.DOASLowSetpoint, 0.9, state.dataEnvrn->StdBaroPress);
            DOASMassFlowRate = state.dataSize->CalcFinalZoneSizing(ControlledZoneNum).MinOA;
            CalcDOASSupCondsForSizing(state,
                                      state.dataEnvrn->OutDryBulbTemp,
                                      state.dataEnvrn->OutHumRat,
                                      calcZoneSizing.DOASControlStrategy,
                                      calcZoneSizing.DOASLowSetpoint,
                                      calcZoneSizing.DOASHighSetpoint,
                                      HR90H,
                                      HR90L,
                                      DOASSupplyTemp,
                                      DOASSupplyHumRat);
            DOASCpAir = PsyCpAirFnW(DOASSupplyHumRat);
            DOASSysOutputProvided = DOASMassFlowRate * DOASCpAir * (DOASSupplyTemp - Node(ZoneNode).Temp);
            TotDOASSysOutputProvided =
                DOASMassFlowRate * (PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat) - PsyHFnTdbW(Node(ZoneNode).Temp, Node(ZoneNode).HumRat));
            Real64 DOASLatOutputProvided = 0.0;
            if (calcZoneSizing.zoneLatentSizing) {
                DOASLatOutputProvided = DOASMassFlowRate * (DOASSupplyHumRat - Node(ZoneNode).HumRat); // kgw/s
            }

            UpdateSystemOutputRequired(state, ControlledZoneNum, DOASSysOutputProvided, DOASLatOutputProvided);
            Node(SupplyAirNode1).Temp = DOASSupplyTemp;
            Node(SupplyAirNode1).HumRat = DOASSupplyHumRat;
            Node(SupplyAirNode1).MassFlowRate = DOASMassFlowRate;
            Node(SupplyAirNode1).Enthalpy = PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat);
            calcZoneSizing.DOASHeatAdd = DOASSysOutputProvided;
            calcZoneSizing.DOASLatAdd = TotDOASSysOutputProvided - DOASSysOutputProvided;
            SupplyAirNode = SupplyAirNode2;
            calcZoneSizing.DOASSupMassFlow = DOASMassFlowRate;
            calcZoneSizing.DOASSupTemp = DOASSupplyTemp;
            calcZoneSizing.DOASSupHumRat = DOASSupplyHumRat;
            if (DOASSysOutputProvided > 0.0) {
                calcZoneSizing.DOASHeatLoad = DOASSysOutputProvided;
                calcZoneSizing.DOASCoolLoad = 0.0;
                calcZoneSizing.DOASTotCoolLoad = 0.0;
            } else {
                calcZoneSizing.DOASCoolLoad = DOASSysOutputProvided;
                calcZoneSizing.DOASTotCoolLoad = TotDOASSysOutputProvided;
                calcZoneSizing.DOASHeatLoad = 0.0;
            }

        } else {
            if (zoneEquipConfig.NumInletNodes > 0) {
                SupplyAirNode = zoneEquipConfig.InletNode(1);
            } else {
                SupplyAirNode = 0;
            }
        }

        // Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
        //                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
        if (!state.dataZoneEnergyDemand->DeadBandOrSetback(ControlledZoneNum) &&
            std::abs(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).RemainingOutputRequired) > DataHVACGlobals::SmallLoad) {
            // Determine design supply air temperture and design supply air temperature difference
            if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).RemainingOutputRequired < 0.0) { // Cooling case
                // If the user specify the design cooling supply air temperature, then
                if (calcZoneSizing.ZnCoolDgnSAMethod == SupplyAirTemperature) {
                    Temp = calcZoneSizing.CoolDesTemp;
                    HumRat = calcZoneSizing.CoolDesHumRat;
                    DeltaTemp = Temp - Node(ZoneNode).Temp;
                    if (zone.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                        DeltaTemp = Temp - zone.AdjustedReturnTempByITE;
                    }
                    // If the user specify the design cooling supply air temperature difference, then
                } else {
                    DeltaTemp = -std::abs(calcZoneSizing.CoolDesTempDiff);
                    Temp = DeltaTemp + Node(ZoneNode).Temp;
                    if (zone.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                        Temp = DeltaTemp + zone.AdjustedReturnTempByITE;
                    }
                    HumRat = calcZoneSizing.CoolDesHumRat;
                }
            } else { // Heating Case
                // If the user specify the design heating supply air temperature, then
                if (calcZoneSizing.ZnHeatDgnSAMethod == SupplyAirTemperature) {
                    Temp = calcZoneSizing.HeatDesTemp;
                    HumRat = calcZoneSizing.HeatDesHumRat;
                    DeltaTemp = Temp - Node(ZoneNode).Temp;
                    // If the user specify the design heating supply air temperature difference, then
                } else {
                    DeltaTemp = std::abs(calcZoneSizing.HeatDesTempDiff);
                    Temp = DeltaTemp + Node(ZoneNode).Temp;
                    HumRat = calcZoneSizing.HeatDesHumRat;
                }
            }

            Enthalpy = PsyHFnTdbW(Temp, HumRat);
            SysOutputProvided = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).RemainingOutputRequired;
            CpAir = PsyCpAirFnW(HumRat);
            if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) {
                //!!PH/WFB/LKL (UCDV model)        MassFlowRate = SysOutputProvided / (CpAir*DeltaTemp)
                MassFlowRate = max(SysOutputProvided / (CpAir * DeltaTemp), 0.0);
            } else {
                MassFlowRate = 0.0;
            }

            if (calcZoneSizing.SupplyAirAdjustFactor > 1.0) {
                MassFlowRate *= calcZoneSizing.SupplyAirAdjustFactor;
            }
        } else {

            Temp = Node(ZoneNode).Temp;
            HumRat = Node(ZoneNode).HumRat;
            Enthalpy = Node(ZoneNode).Enthalpy;
            MassFlowRate = 0.0;
        }

        if (SysOutputProvided > 0.0) {
            calcZoneSizing.HeatLoad = SysOutputProvided;
            calcZoneSizing.HeatMassFlow = MassFlowRate;
            calcZoneSizing.CoolLoad = 0.0;
            calcZoneSizing.CoolMassFlow = 0.0;
        } else if (SysOutputProvided < 0.0) {
            calcZoneSizing.CoolLoad = -SysOutputProvided;
            calcZoneSizing.CoolMassFlow = MassFlowRate;
            calcZoneSizing.HeatLoad = 0.0;
            calcZoneSizing.HeatMassFlow = 0.0;
        } else {
            calcZoneSizing.CoolLoad = 0.0;
            calcZoneSizing.CoolMassFlow = 0.0;
            calcZoneSizing.HeatLoad = 0.0;
            calcZoneSizing.HeatMassFlow = 0.0;
        }
        calcZoneSizing.HeatZoneTemp = Node(ZoneNode).Temp;
        calcZoneSizing.HeatZoneHumRat = Node(ZoneNode).HumRat;
        calcZoneSizing.CoolZoneTemp = Node(ZoneNode).Temp;
        calcZoneSizing.CoolZoneHumRat = Node(ZoneNode).HumRat;
        calcZoneSizing.HeatOutTemp = state.dataEnvrn->OutDryBulbTemp;
        calcZoneSizing.HeatOutHumRat = state.dataEnvrn->OutHumRat;
        calcZoneSizing.CoolOutTemp = state.dataEnvrn->OutDryBulbTemp;
        calcZoneSizing.CoolOutHumRat = state.dataEnvrn->OutHumRat;

        Real64 LatentAirMassFlow = 0.0;
        Real64 MoistureLoad = 0.0;
        Real64 HgAir = PsyHgAirFnWTdb(Node(ZoneNode).HumRat, Node(ZoneNode).Temp);
        if (calcZoneSizing.zoneLatentSizing) {
            // replicate deadband flag - zone condition is either below the humidistat or above the dehumidistat set point
            if ((zoneSysMoistureDemand.OutputRequiredToHumidifyingSP > 0.0 && zoneSysMoistureDemand.OutputRequiredToDehumidifyingSP > 0.0) ||
                (zoneSysMoistureDemand.OutputRequiredToHumidifyingSP < 0.0 && zoneSysMoistureDemand.OutputRequiredToDehumidifyingSP < 0.0)) {
                LatOutputProvided = zoneSysMoistureDemand.RemainingOutputRequired;
            }
            Real64 DeltaHumRat = 0.0;      // positive LatOutputProvided means humidification load
            if (LatOutputProvided < 0.0) { // use SA humrat - zone humrat, or delta humrat based on user choice
                DeltaHumRat = (calcZoneSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio)
                                  ? (calcZoneSizing.LatentCoolDesHumRat - Node(ZoneNode).HumRat)
                                  : -calcZoneSizing.CoolDesHumRatDiff;
            } else if (LatOutputProvided > 0.0) {
                DeltaHumRat = (calcZoneSizing.ZnLatHeatDgnSAMethod == SupplyAirHumidityRatio)
                                  ? (calcZoneSizing.LatentHeatDesHumRat - Node(ZoneNode).HumRat)
                                  : calcZoneSizing.HeatDesHumRatDiff;
            }
            if (std::abs(DeltaHumRat) > DataHVACGlobals::VerySmallMassFlow) LatentAirMassFlow = std::max(0.0, LatOutputProvided / DeltaHumRat);
            MoistureLoad = LatOutputProvided * HgAir;

            if (MassFlowRate > 0.0) {
                HumRat = Node(ZoneNode).HumRat + LatOutputProvided / MassFlowRate;
                CpAir = PsyCpAirFnW(HumRat);
                Temp = (SysOutputProvided / (MassFlowRate * CpAir)) + Node(ZoneNode).Temp;
                Enthalpy = PsyHFnTdbW(Temp, HumRat);
            } else if (LatentAirMassFlow > 0.0) {
                // if there is no sensible load then still need to hold zone RH at set point
                // no need to recalculate T, Sensible load = 0 so T = T,zone
                HumRat = Node(ZoneNode).HumRat + LatOutputProvided / LatentAirMassFlow;
                Enthalpy = PsyHFnTdbW(Temp, HumRat);
                MassFlowRate = (LatentAirMassFlow > DataHVACGlobals::VerySmallMassFlow) ? LatentAirMassFlow : 0.0;
            }

            calcZoneSizing.HeatLatentLoad = (LatOutputProvided > 0.0) ? MoistureLoad : 0.0;
            calcZoneSizing.ZoneHeatLatentMassFlow = (LatOutputProvided > 0.0) ? LatentAirMassFlow : 0.0;
            calcZoneSizing.CoolLatentLoad = (LatOutputProvided < 0.0) ? -MoistureLoad : 0.0;
            calcZoneSizing.ZoneCoolLatentMassFlow = (LatOutputProvided < 0.0) ? LatentAirMassFlow : 0.0;
            calcZoneSizing.HeatLoadNoDOAS = (SysOutputProvidedNoDOAS > 0.0) ? SysOutputProvidedNoDOAS : 0.0;
            calcZoneSizing.CoolLoadNoDOAS = (SysOutputProvidedNoDOAS < 0.0) ? -SysOutputProvidedNoDOAS : 0.0;
            calcZoneSizing.HeatLatentLoadNoDOAS = (LatOutputProvidedNoDOAS > 0.0) ? LatOutputProvidedNoDOAS * HgAir : 0.0;
            calcZoneSizing.CoolLatentLoadNoDOAS = (LatOutputProvidedNoDOAS < 0.0) ? -LatOutputProvidedNoDOAS * HgAir : 0.0;
        }

        if (SupplyAirNode > 0) {
            Node(SupplyAirNode).Temp = Temp;
            Node(SupplyAirNode).HumRat = HumRat;
            Node(SupplyAirNode).Enthalpy = Enthalpy;
            Node(SupplyAirNode).MassFlowRate = MassFlowRate;
        } else {
            thisZoneHB.NonAirSystemResponse = SysOutputProvided;
            if (state.dataHeatBal->doSpaceHeatBalance) {
                for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                    // SpaceHB ToDo: For now allocate by space volume frac
                    state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).NonAirSystemResponse =
                        thisZoneHB.NonAirSystemResponse * state.dataHeatBal->space(spaceNum).fracZoneVolume;
                }
            }
            if (calcZoneSizing.zoneLatentSizing) {
                int ZoneMult = zone.Multiplier * zone.ListMultiplier;
                thisZoneHB.ZoneLatentGain += (LatOutputProvided * HgAir) / ZoneMult;
            }
        }

        UpdateSystemOutputRequired(state, ControlledZoneNum, SysOutputProvided, LatOutputProvided);
    }

    CalcZoneMassBalance(state, true);

    CalcZoneLeavingConditions(state, true);

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {

        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;
        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum);

        // MJW for now - use first return node, make a separate commit to add a dimension to all of the sizing rettemp variables
        int ReturnNode = (zoneEquipConfig.NumReturnNodes > 0) ? zoneEquipConfig.ReturnNode(1) : 0;
        int ZoneNode = zoneEquipConfig.ZoneNode;
        RetTemp = (ReturnNode > 0) ? Node(ReturnNode).Temp : Node(ZoneNode).Temp;
        auto &zoneTstatSP = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ControlledZoneNum);
        if (calcZoneSizing.HeatLoad > 0.0) {
            calcZoneSizing.HeatZoneRetTemp = RetTemp;
            calcZoneSizing.HeatTstatTemp = (zoneTstatSP > 0.0) ? zoneTstatSP : state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ControlledZoneNum);
            calcZoneSizing.CoolTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ControlledZoneNum);
        } else if (calcZoneSizing.CoolLoad > 0.0) {
            calcZoneSizing.CoolZoneRetTemp = RetTemp;
            calcZoneSizing.CoolTstatTemp = (zoneTstatSP > 0.0) ? zoneTstatSP : state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ControlledZoneNum);
            calcZoneSizing.HeatTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ControlledZoneNum);
        } else {
            calcZoneSizing.CoolZoneRetTemp = RetTemp;
            calcZoneSizing.HeatTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ControlledZoneNum);
            calcZoneSizing.CoolTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ControlledZoneNum);
        }
    }
}

void CalcDOASSupCondsForSizing(EnergyPlusData &state,
                               Real64 OutDB,                        // outside air temperature [C]
                               Real64 OutHR,                        // outside humidity ratio [kg Water / kg Dry Air]
                               DataSizing::DOASControl DOASControl, // dedicated outside air control strategy
                               Real64 DOASLowTemp,                  // DOAS low setpoint [C]
                               Real64 DOASHighTemp,                 // DOAS high setpoint [C]
                               Real64 W90H, // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                               Real64 W90L, // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                               Real64 &DOASSupTemp, // DOAS supply temperature [C]
                               Real64 &DOASSupHR    // DOAS Supply Humidity ratio [kg Water / kg Dry Air]
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2015

    // PURPOSE OF THIS FUNCTION:
    // This function calculates supply conditions for the direct outside air system (DOAS) sizing calculations

    // METHODOLOGY EMPLOYED:
    // the supply temperature and humidity ratio are set depending on the design control method and the outside air temperature

    // REFERENCES:
    // Consult the "DOAS Effect On Zone Sizing" new feature proposal and design documents

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcDOASSupCondsForSizing");

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    DOASSupTemp = 0.0;
    DOASSupHR = 0.0;
    // neutral supply air
    if (DOASControl == DataSizing::DOASControl::NeutralSup) {
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
    else if (DOASControl == DataSizing::DOASControl::NeutralDehumSup) { //
        if (OutDB < DOASLowTemp) {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = OutHR;
        } else {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = min(OutHR, W90L);
        }
    }

    // cold supply air
    else if (DOASControl == DataSizing::DOASControl::CoolSup) {
        if (OutDB < DOASLowTemp) {
            DOASSupTemp = DOASHighTemp;
            DOASSupHR = OutHR;
        } else {
            DOASSupTemp = DOASLowTemp;
            DOASSupHR = min(OutHR, W90L);
        }
    } else {
        ShowFatalError(state, format("{}:illegal DOAS design control strategy", RoutineName));
    }
}

void SetUpZoneSizingArrays(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000

    // PURPOSE OF THIS SUBROUTINE:
    // Allocate and fill the ZoneSizing data array.

    // METHODOLOGY EMPLOYED:
    // Obtains data from Zone Sizing and Zone Equipment objects already input.

    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine

    // TODO MJW: Punt for now, sometimes unit test will get here and need these to be allocated, but simulations need them sooner
    if (!state.dataHeatBal->ZoneIntGain.allocated()) {
        DataHeatBalance::AllocateIntGains(state);
    }

    for (int ZoneSizIndex = 1; ZoneSizIndex <= state.dataSize->NumZoneSizingInput; ++ZoneSizIndex) {
        int ZoneIndex = UtilityRoutines::FindItemInList(state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName, state.dataHeatBal->Zone);
        if (ZoneIndex == 0) {
            ShowSevereError(
                state,
                format("SetUpZoneSizingArrays: Sizing:Zone=\"{}\" references unknown zone", state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName));
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
                                     format("SetUpZoneSizingArrays: Requested Sizing for Zone=\"{}\", Zone is not found in the Controlled Zones List",
                                            state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName));
                }
            } else {
                state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneNum = ZoneIndex;
            }
            if (state.dataSize->ZoneSizingInput(ZoneSizIndex).CoolAirDesMethod == AirflowSizingMethod::FromDDCalc ||
                state.dataSize->ZoneSizingInput(ZoneSizIndex).HeatAirDesMethod == AirflowSizingMethod::FromDDCalc) {
                if (!ZoneTempPredictorCorrector::VerifyThermostatInZone(state, state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName)) {
                    if (!state.dataGlobal->isPulseZoneSizing) {
                        ShowWarningError(state,
                                         format("SetUpZoneSizingArrays: Requested Sizing for Zone=\"{}\", Zone has no thermostat (ref: "
                                                "ZoneControl:Thermostat, et al)",
                                                state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName));
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
    int NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;
    state.dataZoneEquipmentManager->AvgData.allocate(NumOfTimeStepInDay);
    state.dataSize->CoolPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->HeatPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->LatCoolPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->LatHeatPeakDateHrMin.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->ZoneSizThermSetPtHi.allocate(state.dataGlobal->NumOfZones);
    state.dataSize->ZoneSizThermSetPtLo.allocate(state.dataGlobal->NumOfZones);

    state.dataSize->CoolPeakDateHrMin = "";
    state.dataSize->HeatPeakDateHrMin = "";
    state.dataSize->LatCoolPeakDateHrMin = "";
    state.dataSize->LatHeatPeakDateHrMin = "";

    state.dataSize->ZoneSizThermSetPtHi = 0.0;
    state.dataSize->ZoneSizThermSetPtLo = 1000.0;

    for (int DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
        auto &thisDesDayWeather = state.dataSize->DesDayWeath(DesDayNum);
        thisDesDayWeather.Temp.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        thisDesDayWeather.HumRat.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        thisDesDayWeather.Press.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        thisDesDayWeather.Temp = 0.0;
        thisDesDayWeather.HumRat = 0.0;
        thisDesDayWeather.Press = 0.0;
    }

    // Fill zone sizing arrays from input array
    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;

        // For each Zone Sizing object, find the corresponding controlled zone
        int ZoneSizNum = UtilityRoutines::FindItemInList(zoneEquipConfig.ZoneName, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
        auto &zoneSizingInput = (ZoneSizNum > 0) ? state.dataSize->ZoneSizingInput(ZoneSizNum) : state.dataSize->ZoneSizingInput(1);

        for (int DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            auto &zoneSizing = state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum);
            auto &calcZoneSizing = state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum);
            zoneSizing.ZoneName = zoneEquipConfig.ZoneName;
            zoneSizing.ZoneNum = CtrlZoneNum;
            calcZoneSizing.ZoneName = zoneEquipConfig.ZoneName;
            calcZoneSizing.ZoneNum = CtrlZoneNum;

            if (DesDayNum == 1 && ZoneSizNum == 0) { // LKL I think this is sufficient for warning -- no need for array
                if (!state.dataGlobal->isPulseZoneSizing) {
                    ShowWarningError(
                        state,
                        format("SetUpZoneSizingArrays: Sizing for Zone=\"{}\" will use Sizing:Zone specifications listed for Zone=\"{}\".",
                               state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName,
                               zoneSizingInput.ZoneName));
                }
            }

            zoneSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
            zoneSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
            zoneSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
            zoneSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
            zoneSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
            zoneSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
            zoneSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
            zoneSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
            zoneSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
            zoneSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
            zoneSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
            zoneSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
            zoneSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
            zoneSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
            zoneSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
            zoneSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
            zoneSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
            zoneSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
            zoneSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
            zoneSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
            zoneSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
            zoneSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
            zoneSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
            zoneSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
            zoneSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
            zoneSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
            zoneSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
            zoneSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
            zoneSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
            zoneSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
            zoneSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
            zoneSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
            calcZoneSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
            calcZoneSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
            calcZoneSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
            calcZoneSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
            calcZoneSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
            calcZoneSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
            calcZoneSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
            calcZoneSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
            calcZoneSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
            calcZoneSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
            calcZoneSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
            calcZoneSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
            calcZoneSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
            calcZoneSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
            calcZoneSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
            calcZoneSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
            calcZoneSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
            calcZoneSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
            calcZoneSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
            calcZoneSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
            calcZoneSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
            calcZoneSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
            calcZoneSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
            calcZoneSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
            calcZoneSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
            calcZoneSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
            calcZoneSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
            calcZoneSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
            calcZoneSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
            calcZoneSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
            calcZoneSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
            calcZoneSizing.LatentCoolDesHumRat = zoneSizingInput.LatentCoolDesHumRat;
            calcZoneSizing.CoolDesHumRatDiff = zoneSizingInput.CoolDesHumRatDiff;
            calcZoneSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
            calcZoneSizing.LatentHeatDesHumRat = zoneSizingInput.LatentHeatDesHumRat;
            calcZoneSizing.HeatDesHumRatDiff = zoneSizingInput.HeatDesHumRatDiff;

            zoneSizing.allocateMemberArrays(NumOfTimeStepInDay);
            calcZoneSizing.allocateMemberArrays(NumOfTimeStepInDay);
        }

        auto &finalZoneSizing = state.dataSize->FinalZoneSizing(CtrlZoneNum);
        auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
        finalZoneSizing.ZoneName = zoneEquipConfig.ZoneName;
        finalZoneSizing.ZoneNum = CtrlZoneNum;
        calcFinalZoneSizing.ZoneName = zoneEquipConfig.ZoneName;
        calcFinalZoneSizing.ZoneNum = CtrlZoneNum;

        finalZoneSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
        finalZoneSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
        finalZoneSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
        finalZoneSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
        finalZoneSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
        finalZoneSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
        finalZoneSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
        finalZoneSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
        finalZoneSizing.ZoneAirDistributionIndex = zoneSizingInput.ZoneAirDistributionIndex;
        finalZoneSizing.ZoneDesignSpecOAIndex = zoneSizingInput.ZoneDesignSpecOAIndex;
        finalZoneSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
        finalZoneSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
        finalZoneSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
        finalZoneSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
        finalZoneSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
        finalZoneSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
        finalZoneSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
        finalZoneSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
        finalZoneSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
        finalZoneSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
        finalZoneSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
        finalZoneSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
        finalZoneSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
        finalZoneSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
        finalZoneSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
        finalZoneSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
        finalZoneSizing.ZoneADEffCooling = zoneSizingInput.ZoneADEffCooling;
        finalZoneSizing.ZoneADEffHeating = zoneSizingInput.ZoneADEffHeating;
        finalZoneSizing.ZoneSecondaryRecirculation = zoneSizingInput.ZoneSecondaryRecirculation;
        finalZoneSizing.ZoneVentilationEff = zoneSizingInput.ZoneVentilationEff;
        finalZoneSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
        finalZoneSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
        finalZoneSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
        finalZoneSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
        finalZoneSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
        finalZoneSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
        finalZoneSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
        finalZoneSizing.LatentCoolDesHumRat = zoneSizingInput.LatentCoolDesHumRat;
        finalZoneSizing.CoolDesHumRatDiff = zoneSizingInput.CoolDesHumRatDiff;
        finalZoneSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
        finalZoneSizing.LatentHeatDesHumRat = zoneSizingInput.LatentHeatDesHumRat;
        finalZoneSizing.HeatDesHumRatDiff = zoneSizingInput.HeatDesHumRatDiff;
        calcFinalZoneSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
        calcFinalZoneSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
        calcFinalZoneSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
        calcFinalZoneSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
        calcFinalZoneSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
        calcFinalZoneSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
        calcFinalZoneSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
        calcFinalZoneSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
        calcFinalZoneSizing.ZoneAirDistributionIndex = zoneSizingInput.ZoneAirDistributionIndex;
        calcFinalZoneSizing.ZoneDesignSpecOAIndex = zoneSizingInput.ZoneDesignSpecOAIndex;
        calcFinalZoneSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
        calcFinalZoneSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
        calcFinalZoneSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
        calcFinalZoneSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
        calcFinalZoneSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
        calcFinalZoneSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
        calcFinalZoneSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
        calcFinalZoneSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
        calcFinalZoneSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
        calcFinalZoneSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
        calcFinalZoneSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
        calcFinalZoneSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
        calcFinalZoneSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
        calcFinalZoneSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
        calcFinalZoneSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
        calcFinalZoneSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
        calcFinalZoneSizing.ZoneADEffCooling = zoneSizingInput.ZoneADEffCooling;
        calcFinalZoneSizing.ZoneADEffHeating = zoneSizingInput.ZoneADEffHeating;
        calcFinalZoneSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
        calcFinalZoneSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
        calcFinalZoneSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
        calcFinalZoneSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
        calcFinalZoneSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
        calcFinalZoneSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
        calcFinalZoneSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
        calcFinalZoneSizing.LatentCoolDesHumRat = zoneSizingInput.LatentCoolDesHumRat;
        calcFinalZoneSizing.CoolDesHumRatDiff = zoneSizingInput.CoolDesHumRatDiff;
        calcFinalZoneSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
        calcFinalZoneSizing.LatentHeatDesHumRat = zoneSizingInput.LatentHeatDesHumRat;
        calcFinalZoneSizing.HeatDesHumRatDiff = zoneSizingInput.HeatDesHumRatDiff;

        finalZoneSizing.allocateMemberArrays(NumOfTimeStepInDay);
        calcFinalZoneSizing.allocateMemberArrays(NumOfTimeStepInDay);

        // setup CalcFinalZoneSizing structure for use with EMS, some as sensors, some as actuators
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(
                state, "Final Zone Design Heating Air Mass Flow Rate", finalZoneSizing.ZoneName, "[kg/s]", finalZoneSizing.DesHeatMassFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Heating Air Mass Flow Rate",
                                     calcFinalZoneSizing.ZoneName,
                                     "[kg/s]",
                                     calcFinalZoneSizing.DesHeatMassFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Heating Air Mass Flow Rate",
                             "[kg/s]",
                             calcFinalZoneSizing.EMSOverrideDesHeatMassOn,
                             calcFinalZoneSizing.EMSValueDesHeatMassFlow);

            SetupEMSInternalVariable(
                state, "Final Zone Design Cooling Air Mass Flow Rate", finalZoneSizing.ZoneName, "[kg/s]", finalZoneSizing.DesCoolMassFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Zone Design Cooling Air Mass Flow Rate",
                                     calcFinalZoneSizing.ZoneName,
                                     "[kg/s]",
                                     calcFinalZoneSizing.DesCoolMassFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Cooling Air Mass Flow Rate",
                             "[kg/s]",
                             calcFinalZoneSizing.EMSOverrideDesCoolMassOn,
                             calcFinalZoneSizing.EMSValueDesCoolMassFlow);

            SetupEMSInternalVariable(state, "Final Zone Design Heating Load", finalZoneSizing.ZoneName, "[W]", finalZoneSizing.DesHeatLoad);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Heating Load", calcFinalZoneSizing.ZoneName, "[W]", calcFinalZoneSizing.DesHeatLoad);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Heating Load",
                             "[W]",
                             calcFinalZoneSizing.EMSOverrideDesHeatLoadOn,
                             calcFinalZoneSizing.EMSValueDesHeatLoad);

            SetupEMSInternalVariable(state, "Final Zone Design Cooling Load", finalZoneSizing.ZoneName, "[W]", finalZoneSizing.DesCoolLoad);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Cooling Load", calcFinalZoneSizing.ZoneName, "[W]", calcFinalZoneSizing.DesCoolLoad);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Cooling Load",
                             "[W]",
                             calcFinalZoneSizing.EMSOverrideDesCoolLoadOn,
                             calcFinalZoneSizing.EMSValueDesCoolLoad);

            SetupEMSInternalVariable(
                state, "Final Zone Design Heating Air Density", finalZoneSizing.ZoneName, "[kg/m3]", finalZoneSizing.DesHeatDens);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Heating Air Density", calcFinalZoneSizing.ZoneName, "[kg/m3]", calcFinalZoneSizing.DesHeatDens);
            SetupEMSInternalVariable(
                state, "Final Zone Design Cooling Air Density", finalZoneSizing.ZoneName, "[kg/m3]", finalZoneSizing.DesCoolDens);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Cooling Air Density", calcFinalZoneSizing.ZoneName, "[kg/m3]", calcFinalZoneSizing.DesCoolDens);

            SetupEMSInternalVariable(
                state, "Final Zone Design Heating Volume Flow", finalZoneSizing.ZoneName, "[m3/s]", finalZoneSizing.DesHeatVolFlow);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Heating Volume Flow", calcFinalZoneSizing.ZoneName, "[m3/s]", calcFinalZoneSizing.DesHeatVolFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Heating Vol Flow",
                             "[m3/s]",
                             calcFinalZoneSizing.EMSOverrideDesHeatVolOn,
                             calcFinalZoneSizing.EMSValueDesHeatVolFlow);

            SetupEMSInternalVariable(
                state, "Final Zone Design Cooling Volume Flow", finalZoneSizing.ZoneName, "[m3/s]", finalZoneSizing.DesCoolVolFlow);
            SetupEMSInternalVariable(
                state, "Intermediate Zone Design Cooling Volume Flow", calcFinalZoneSizing.ZoneName, "[m3/s]", calcFinalZoneSizing.DesCoolVolFlow);
            SetupEMSActuator(state,
                             "Sizing:Zone",
                             calcFinalZoneSizing.ZoneName,
                             "Zone Design Cooling Vol Flow",
                             "[m3/s]",
                             calcFinalZoneSizing.EMSOverrideDesCoolVolOn,
                             calcFinalZoneSizing.EMSValueDesCoolVolFlow);

            SetupEMSInternalVariable(
                state, "Zone Outdoor Air Design Volume Flow Rate", calcFinalZoneSizing.ZoneName, "[m3/s]", calcFinalZoneSizing.MinOA);
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
                    ShowSevereError(state, format("SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList={}", thisOAReq.Name));
                    ShowContinueError(state, format("Space Name={} not found.", thisSpaceName));
                    dsoaError = true;
                    ErrorsFound = true;
                }
                // Check for duplicate spaces
                for (int loop = 1; loop <= int(thisOAReq.dsoaSpaceIndexes.size()) - 1; ++loop) {
                    if (thisSpaceNum == thisOAReq.dsoaSpaceIndexes(loop)) {
                        ShowSevereError(state, format("SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList={}", thisOAReq.Name));
                        ShowContinueError(state, format("Space Name={} appears more than once in the list.", thisSpaceName));
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
    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        auto &thisZone = state.dataHeatBal->Zone(CtrlZoneNum);
        auto &finalZoneSizing = state.dataSize->FinalZoneSizing(CtrlZoneNum);
        auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
        // Use the max occupancy PEOPLE structure to calculate design min OA for each zone from the outside air flow per person input
        Real64 TotPeopleInZone = 0.0;
        Real64 ZoneMinOccupancy = 0.;
        int DSOAPtr = finalZoneSizing.ZoneDesignSpecOAIndex; // index to DesignSpecification:OutdoorAir object
        if ((DSOAPtr > 0) && !dsoaError) {
            auto &thisOAReq = state.dataSize->OARequirements(DSOAPtr);
            // If this is a DesignSpecification:OutdoorAir:SpaceList check to make sure spaces are valid and belong to this zone
            if (thisOAReq.numDSOA > 0) {
                for (int spaceCounter = 1; spaceCounter <= thisOAReq.numDSOA; ++spaceCounter) {
                    std::string thisSpaceName = thisOAReq.dsoaSpaceNames(spaceCounter);
                    int thisSpaceNum = thisOAReq.dsoaSpaceIndexes(spaceCounter);
                    if (thisSpaceNum > 0) {
                        if (state.dataHeatBal->space(thisSpaceNum).zoneNum != CtrlZoneNum) {
                            ShowSevereError(state, format("SetUpZoneSizingArrays: DesignSpecification:OutdoorAir:SpaceList={}", thisOAReq.Name));
                            ShowContinueError(state, format("is invalid for Sizing:Zone={}", finalZoneSizing.ZoneName));
                            ShowContinueError(state, "All spaces in the list must be part of this zone.");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            finalZoneSizing.DesOAFlowPPer = thisOAReq.desFlowPerZonePerson(state, CtrlZoneNum);
            finalZoneSizing.DesOAFlowPerArea = thisOAReq.desFlowPerZoneArea(state, CtrlZoneNum);
        }

        for (int PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
            if (state.dataHeatBal->People(PeopleNum).ZonePtr == CtrlZoneNum) {
                auto &people = state.dataHeatBal->People(PeopleNum);
                TotPeopleInZone += (people.NumberOfPeople * thisZone.Multiplier * thisZone.ListMultiplier);
                Real64 SchMax = ScheduleManager::GetScheduleMaxValue(state, people.NumberOfPeoplePtr);
                if (SchMax > 0) {
                    finalZoneSizing.ZonePeakOccupancy = TotPeopleInZone * SchMax;
                } else {
                    finalZoneSizing.ZonePeakOccupancy = TotPeopleInZone;
                }
                ZoneMinOccupancy += TotPeopleInZone * ScheduleManager::GetScheduleMinValue(state, people.NumberOfPeoplePtr);
            }
        }
        finalZoneSizing.TotalZoneFloorArea = (thisZone.FloorArea * thisZone.Multiplier * thisZone.ListMultiplier);
        Real64 OAFromPeople = finalZoneSizing.DesOAFlowPPer * TotPeopleInZone;
        Real64 OAFromArea = finalZoneSizing.DesOAFlowPerArea * finalZoneSizing.TotalZoneFloorArea;
        finalZoneSizing.TotPeopleInZone = TotPeopleInZone;
        finalZoneSizing.TotalOAFromPeople = OAFromPeople;
        finalZoneSizing.TotalOAFromArea = OAFromArea;

        // save Voz for predefined outdoor air summary report
        Real64 MinEz = std::min(finalZoneSizing.ZoneADEffCooling, finalZoneSizing.ZoneADEffHeating);
        if (MinEz == 0) {
            MinEz = 1.0; // if not calculated assume 1.0 ventilation effectiveness
        }
        state.dataHeatBal->ZonePreDefRep(CtrlZoneNum).VozMin = (ZoneMinOccupancy * finalZoneSizing.DesOAFlowPPer + OAFromArea) / MinEz;

        // Calculate the design min OA flow rate for this zone
        // flag to use occupancy schedule when calculating OA
        bool UseOccSchFlag = false;
        // flag to use min OA schedule when calculating OA
        bool UseMinOASchFlag = false;
        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneDesignSpecOAIndex = DSOAPtr;                                     // store for later use
        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneAirDistributionIndex = finalZoneSizing.ZoneAirDistributionIndex; // store for later use
        Real64 OAVolumeFlowRate = 0.0;
        if (!dsoaError) {
            OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(state, DSOAPtr, CtrlZoneNum, UseOccSchFlag, UseMinOASchFlag);
        }

        // Zone(ZoneIndex)%Multiplier and Zone(ZoneIndex)%ListMultiplier applied in CalcDesignSpecificationOutdoorAir
        finalZoneSizing.MinOA = OAVolumeFlowRate;
        calcFinalZoneSizing.MinOA = OAVolumeFlowRate;
        if (finalZoneSizing.ZoneADEffCooling > 0.0 || finalZoneSizing.ZoneADEffHeating > 0.0) {
            finalZoneSizing.MinOA /= min(finalZoneSizing.ZoneADEffCooling, finalZoneSizing.ZoneADEffHeating);
            calcFinalZoneSizing.MinOA = finalZoneSizing.MinOA;
        }
        // calculated zone design flow rates automatically take into account zone multipliers, since the zone
        // loads are multiplied (in ZoneTempPredictorCorrector.cc). Flow rates derived directly from
        // user inputs need to be explicitly multiplied by the zone multipliers.
        finalZoneSizing.DesCoolMinAirFlow2 =
            finalZoneSizing.DesCoolMinAirFlowPerArea * thisZone.FloorArea * thisZone.Multiplier * thisZone.ListMultiplier;
        calcFinalZoneSizing.DesCoolMinAirFlow2 =
            calcFinalZoneSizing.DesCoolMinAirFlowPerArea * thisZone.FloorArea * thisZone.Multiplier * thisZone.ListMultiplier;
        finalZoneSizing.DesHeatMaxAirFlow2 =
            finalZoneSizing.DesHeatMaxAirFlowPerArea * thisZone.FloorArea * thisZone.Multiplier * thisZone.ListMultiplier;
        calcFinalZoneSizing.DesHeatMaxAirFlow2 =
            calcFinalZoneSizing.DesHeatMaxAirFlowPerArea * thisZone.FloorArea * thisZone.Multiplier * thisZone.ListMultiplier;
        int zoneMultiplier = thisZone.Multiplier * thisZone.ListMultiplier;
        finalZoneSizing.DesCoolMinAirFlow *= zoneMultiplier;
        calcFinalZoneSizing.DesCoolMinAirFlow *= zoneMultiplier;
        finalZoneSizing.DesHeatMaxAirFlow *= zoneMultiplier;
        calcFinalZoneSizing.DesHeatMaxAirFlow *= zoneMultiplier;
        finalZoneSizing.InpDesCoolAirFlow *= zoneMultiplier;
        calcFinalZoneSizing.InpDesCoolAirFlow *= zoneMultiplier;
        finalZoneSizing.InpDesHeatAirFlow *= zoneMultiplier;
        calcFinalZoneSizing.InpDesHeatAirFlow *= zoneMultiplier;

        for (int DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            auto &zoneSizing = state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum);
            zoneSizing.MinOA = finalZoneSizing.MinOA;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).MinOA = calcFinalZoneSizing.MinOA;
            zoneSizing.DesCoolMinAirFlow2 = finalZoneSizing.DesCoolMinAirFlow2;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow2 = calcFinalZoneSizing.DesCoolMinAirFlow2;
            zoneSizing.DesCoolMinAirFlow = finalZoneSizing.DesCoolMinAirFlow;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesCoolMinAirFlow = calcFinalZoneSizing.DesCoolMinAirFlow;
            zoneSizing.DesHeatMaxAirFlow2 = finalZoneSizing.DesHeatMaxAirFlow2;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow2 = calcFinalZoneSizing.DesHeatMaxAirFlow2;
            zoneSizing.DesHeatMaxAirFlow = finalZoneSizing.DesHeatMaxAirFlow;
            state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum).DesHeatMaxAirFlow = calcFinalZoneSizing.DesHeatMaxAirFlow;
        }
    }
    // Formats
    print(state.files.eio, "! <Load Timesteps in Zone Design Calculation Averaging Window>, Value\n");
    static constexpr std::string_view Format_891(" Load Timesteps in Zone Design Calculation Averaging Window, {:4}\n");
    print(state.files.eio, Format_891, state.dataSize->NumTimeStepsInAvg);
    print(state.files.eio, "! <Heating Sizing Factor Information>, Sizing Factor ID, Value\n");
    static constexpr std::string_view Format_991(" Heating Sizing Factor Information, Global, {:12.5N}\n");
    print(state.files.eio, Format_991, state.dataSize->GlobalHeatSizingFactor);
    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
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
    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
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

    // PURPOSE OF THIS SUBROUTINE:
    // Update the result variables of the zone sizing calculation

    // METHODOLOGY EMPLOYED:
    // CallIndicator = 1 (BeginDay) zero the result arrays
    // CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
    // CallIndicator = 3 (EndDay) calculate daily maxima
    // CallIndicator = 4 (EndZoneSizingCalc) write out results

    // SUBROUTINE PARAMETER DEFINITIONS:

    static constexpr std::string_view RoutineName("UpdateZoneSizing");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int DesDayNum;         // design day index
    int TimeStepIndex;     // zone time step index
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
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {

            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);

            calcZoneSizing.CoolDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.HeatDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.DesHeatDens = state.dataEnvrn->StdRhoAir;
            calcZoneSizing.DesCoolDens = state.dataEnvrn->StdRhoAir;
            calcZoneSizing.HeatDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.CoolDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.CoolNoDOASDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.HeatNoDOASDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.LatCoolDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.LatHeatDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.LatCoolNoDOASDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.LatHeatNoDOASDesDay = state.dataEnvrn->EnvironmentName;
            calcZoneSizing.CoolNoDOASDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.HeatNoDOASDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.LatentCoolDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.LatentHeatDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.LatentCoolNoDOASDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.LatentHeatNoDOASDDNum = state.dataSize->CurOverallSimDay;
            calcZoneSizing.CoolSizingType = "Cooling"; // string reported to eio
            calcZoneSizing.HeatSizingType = "Heating"; // string reported to eio
        }
    } break;
    case DataGlobalConstants::CallIndicator::DuringDay: {
        TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;

        Real64 FracTimeStepZone = state.dataHVACGlobal->FracTimeStepZone;
        // save the results of the ideal zone component calculation in the CalcZoneSizing sequence variables
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;

            auto &zoneSizing = state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            auto &zoneThermostatHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum);
            auto &zoneThermostatLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum);
            if (zoneThermostatHi > 0.0 && zoneThermostatHi > state.dataSize->ZoneSizThermSetPtHi(CtrlZoneNum)) {
                state.dataSize->ZoneSizThermSetPtHi(CtrlZoneNum) = zoneThermostatHi;
            }
            if (zoneThermostatLo > 0.0 && zoneThermostatLo < state.dataSize->ZoneSizThermSetPtLo(CtrlZoneNum)) {
                state.dataSize->ZoneSizThermSetPtLo(CtrlZoneNum) = zoneThermostatLo;
            }
            zoneSizing.DesHeatSetPtSeq(TimeStepInDay) = zoneThermostatLo;
            zoneSizing.HeatTstatTempSeq(TimeStepInDay) = calcZoneSizing.HeatTstatTemp;
            zoneSizing.DesCoolSetPtSeq(TimeStepInDay) = zoneThermostatHi;
            zoneSizing.CoolTstatTempSeq(TimeStepInDay) = calcZoneSizing.CoolTstatTemp;
            calcZoneSizing.HeatFlowSeq(TimeStepInDay) += calcZoneSizing.HeatMassFlow * FracTimeStepZone;
            calcZoneSizing.HeatLoadSeq(TimeStepInDay) += calcZoneSizing.HeatLoad * FracTimeStepZone;
            calcZoneSizing.HeatZoneTempSeq(TimeStepInDay) += calcZoneSizing.HeatZoneTemp * FracTimeStepZone;
            calcZoneSizing.HeatOutTempSeq(TimeStepInDay) += calcZoneSizing.HeatOutTemp * FracTimeStepZone;
            calcZoneSizing.HeatZoneRetTempSeq(TimeStepInDay) += calcZoneSizing.HeatZoneRetTemp * FracTimeStepZone;
            calcZoneSizing.HeatZoneHumRatSeq(TimeStepInDay) += calcZoneSizing.HeatZoneHumRat * FracTimeStepZone;
            calcZoneSizing.HeatOutHumRatSeq(TimeStepInDay) += calcZoneSizing.HeatOutHumRat * FracTimeStepZone;
            calcZoneSizing.CoolFlowSeq(TimeStepInDay) += calcZoneSizing.CoolMassFlow * FracTimeStepZone;
            calcZoneSizing.CoolLoadSeq(TimeStepInDay) += calcZoneSizing.CoolLoad * FracTimeStepZone;
            calcZoneSizing.CoolZoneTempSeq(TimeStepInDay) += calcZoneSizing.CoolZoneTemp * FracTimeStepZone;
            calcZoneSizing.CoolOutTempSeq(TimeStepInDay) += calcZoneSizing.CoolOutTemp * FracTimeStepZone;
            calcZoneSizing.CoolZoneRetTempSeq(TimeStepInDay) += calcZoneSizing.CoolZoneRetTemp * FracTimeStepZone;
            calcZoneSizing.CoolZoneHumRatSeq(TimeStepInDay) += calcZoneSizing.CoolZoneHumRat * FracTimeStepZone;
            calcZoneSizing.CoolOutHumRatSeq(TimeStepInDay) += calcZoneSizing.CoolOutHumRat * FracTimeStepZone;
            calcZoneSizing.DOASHeatLoadSeq(TimeStepInDay) += calcZoneSizing.DOASHeatLoad * FracTimeStepZone;
            calcZoneSizing.DOASCoolLoadSeq(TimeStepInDay) += calcZoneSizing.DOASCoolLoad * FracTimeStepZone;
            calcZoneSizing.DOASHeatAddSeq(TimeStepInDay) += calcZoneSizing.DOASHeatAdd * FracTimeStepZone;
            calcZoneSizing.DOASLatAddSeq(TimeStepInDay) += calcZoneSizing.DOASLatAdd * FracTimeStepZone;
            calcZoneSizing.DOASSupMassFlowSeq(TimeStepInDay) += calcZoneSizing.DOASSupMassFlow * FracTimeStepZone;
            calcZoneSizing.DOASSupTempSeq(TimeStepInDay) += calcZoneSizing.DOASSupTemp * FracTimeStepZone;
            calcZoneSizing.DOASSupHumRatSeq(TimeStepInDay) += calcZoneSizing.DOASSupHumRat * FracTimeStepZone;
            calcZoneSizing.DOASTotCoolLoadSeq(TimeStepInDay) += calcZoneSizing.DOASTotCoolLoad * FracTimeStepZone;
            if (calcZoneSizing.zoneLatentSizing) {
                calcZoneSizing.LatentHeatLoadSeq(TimeStepInDay) += calcZoneSizing.HeatLatentLoad * FracTimeStepZone;
                calcZoneSizing.LatentHeatFlowSeq(TimeStepInDay) += calcZoneSizing.ZoneHeatLatentMassFlow * FracTimeStepZone;
                calcZoneSizing.LatentCoolLoadSeq(TimeStepInDay) += calcZoneSizing.CoolLatentLoad * FracTimeStepZone;
                calcZoneSizing.LatentCoolFlowSeq(TimeStepInDay) += calcZoneSizing.ZoneCoolLatentMassFlow * FracTimeStepZone;
                calcZoneSizing.CoolLatentLoadNoDOASSeq(TimeStepInDay) += calcZoneSizing.CoolLatentLoadNoDOAS * FracTimeStepZone;
                calcZoneSizing.HeatLatentLoadNoDOASSeq(TimeStepInDay) += calcZoneSizing.HeatLatentLoadNoDOAS * FracTimeStepZone;
                calcZoneSizing.CoolLoadNoDOASSeq(TimeStepInDay) += calcZoneSizing.CoolLoadNoDOAS * FracTimeStepZone;
                calcZoneSizing.HeatLoadNoDOASSeq(TimeStepInDay) += calcZoneSizing.HeatLoadNoDOAS * FracTimeStepZone;
            }
        }
    } break;
    case DataGlobalConstants::CallIndicator::EndDay: {
        // average some of the zone sequences to reduce peakiness
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            auto &calcZoneSizing(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum));
            General::MovingAvg(calcZoneSizing.CoolFlowSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.CoolLoadSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.HeatFlowSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.HeatLoadSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.CoolZoneRetTempSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.HeatZoneRetTempSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.DOASHeatAddSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.DOASLatAddSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.CoolLatentLoadNoDOASSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.HeatLatentLoadNoDOASSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.CoolLoadNoDOASSeq, state.dataSize->NumTimeStepsInAvg);
            General::MovingAvg(calcZoneSizing.HeatLoadNoDOASSeq, state.dataSize->NumTimeStepsInAvg);

            if (calcZoneSizing.zoneLatentSizing) {
                General::MovingAvg(calcZoneSizing.LatentHeatLoadSeq, state.dataSize->NumTimeStepsInAvg);
                General::MovingAvg(calcZoneSizing.LatentHeatFlowSeq, state.dataSize->NumTimeStepsInAvg);
                General::MovingAvg(calcZoneSizing.LatentCoolLoadSeq, state.dataSize->NumTimeStepsInAvg);
                General::MovingAvg(calcZoneSizing.LatentCoolFlowSeq, state.dataSize->NumTimeStepsInAvg);
            }
        }

        auto &desDayWeath = state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay);
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
            calcFinalZoneSizing.CoolSizingType = calcZoneSizing.CoolSizingType;
            calcFinalZoneSizing.HeatSizingType = calcZoneSizing.HeatSizingType;

            // save the sequence values at the heating peak
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (calcZoneSizing.HeatLoadSeq(TimeStepIndex) > calcZoneSizing.DesHeatLoad) {
                    calcZoneSizing.DesHeatLoad = calcZoneSizing.HeatLoadSeq(TimeStepIndex);
                    calcZoneSizing.DesHeatMassFlow = calcZoneSizing.HeatFlowSeq(TimeStepIndex);
                    calcZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                    calcZoneSizing.OutTempAtHeatPeak = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                    calcZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.HeatZoneRetTempSeq(TimeStepIndex);
                    calcZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.HeatZoneHumRatSeq(TimeStepIndex);
                    calcZoneSizing.OutHumRatAtHeatPeak = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtHeatMax = TimeStepIndex;
                }
            }
            // save the sequence values at the latent heating peak
            if (calcZoneSizing.zoneLatentSizing) {
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (calcZoneSizing.LatentHeatLoadSeq(TimeStepIndex) > calcZoneSizing.DesLatentHeatLoad) {
                        calcZoneSizing.DesLatentHeatLoad = calcZoneSizing.LatentHeatLoadSeq(TimeStepIndex);
                        calcZoneSizing.DesLatentHeatMassFlow = calcZoneSizing.LatentHeatFlowSeq(TimeStepIndex);
                        calcZoneSizing.ZoneHeatLatentMassFlow = calcZoneSizing.LatentHeatFlowSeq(TimeStepIndex);
                        calcZoneSizing.ZoneTempAtLatentHeatPeak = calcZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtLatentHeatPeak = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneHumRatAtLatentHeatPeak = calcZoneSizing.HeatZoneHumRatSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtLatentHeatPeak = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                        calcZoneSizing.ZoneRetTempAtLatentHeatPeak = calcZoneSizing.HeatZoneRetTempSeq(TimeStepIndex);
                        calcZoneSizing.TimeStepNumAtLatentHeatMax = TimeStepIndex;
                    }
                }
            }
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                // select largest load from NoDOAS arrays
                if (calcZoneSizing.HeatLoadNoDOASSeq(TimeStepIndex) > calcZoneSizing.DesHeatLoadNoDOAS) {
                    calcZoneSizing.DesHeatLoadNoDOAS = calcZoneSizing.HeatLoadNoDOASSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtHeatNoDOASMax = TimeStepIndex;
                }
                if (calcZoneSizing.HeatLatentLoadNoDOASSeq(TimeStepIndex) > calcZoneSizing.DesLatentHeatLoadNoDOAS) {
                    calcZoneSizing.DesLatentHeatLoadNoDOAS = calcZoneSizing.HeatLatentLoadNoDOASSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtLatentHeatNoDOASMax = TimeStepIndex;
                }
                if (calcZoneSizing.CoolLoadNoDOASSeq(TimeStepIndex) > calcZoneSizing.DesCoolLoadNoDOAS) {
                    calcZoneSizing.DesCoolLoadNoDOAS = calcZoneSizing.CoolLoadNoDOASSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtCoolNoDOASMax = TimeStepIndex;
                }
                if (calcZoneSizing.CoolLatentLoadNoDOASSeq(TimeStepIndex) > calcZoneSizing.DesLatentCoolLoadNoDOAS) {
                    calcZoneSizing.DesLatentCoolLoadNoDOAS = calcZoneSizing.CoolLatentLoadNoDOASSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtLatentCoolNoDOASMax = TimeStepIndex;
                }
            }
            if (calcZoneSizing.DesHeatMassFlow > 0.0) {
                calcZoneSizing.DesHeatVolFlow = calcZoneSizing.DesHeatMassFlow / calcZoneSizing.DesHeatDens;
                OAFrac = calcZoneSizing.MinOA / max(calcZoneSizing.DesHeatVolFlow, DataHVACGlobals::SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = calcZoneSizing.TimeStepNumAtHeatMax;
                calcZoneSizing.DesHeatCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneTempAtHeatPeak;
                calcZoneSizing.DesHeatCoilInHumRat =
                    OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneHumRatAtHeatPeak;
            }
            if (calcZoneSizing.zoneLatentSizing && calcZoneSizing.DesLatentHeatMassFlow > 0.0) {
                calcZoneSizing.DesLatentHeatVolFlow = calcZoneSizing.DesLatentHeatMassFlow / state.dataEnvrn->StdRhoAir;
                OAFrac = calcZoneSizing.MinOA / max(calcZoneSizing.DesHeatVolFlow, DataHVACGlobals::SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = calcZoneSizing.TimeStepNumAtLatentHeatMax;
                calcZoneSizing.DesLatentHeatCoilInTemp =
                    OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneTempAtHeatPeak;
                calcZoneSizing.DesLatentHeatCoilInHumRat =
                    OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneHumRatAtHeatPeak;
            }
            // save the sequence values at the cooling peak
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (calcZoneSizing.CoolLoadSeq(TimeStepIndex) > calcZoneSizing.DesCoolLoad) {
                    calcZoneSizing.DesCoolLoad = calcZoneSizing.CoolLoadSeq(TimeStepIndex);
                    calcZoneSizing.DesCoolMassFlow = calcZoneSizing.CoolFlowSeq(TimeStepIndex);
                    calcZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                    calcZoneSizing.OutTempAtCoolPeak = calcZoneSizing.CoolOutTempSeq(TimeStepIndex);
                    calcZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.CoolZoneRetTempSeq(TimeStepIndex);
                    calcZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.CoolZoneHumRatSeq(TimeStepIndex);
                    calcZoneSizing.OutHumRatAtCoolPeak = calcZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
                    calcZoneSizing.TimeStepNumAtCoolMax = TimeStepIndex;
                }
            }
            if (calcZoneSizing.zoneLatentSizing) {
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (calcZoneSizing.LatentCoolLoadSeq(TimeStepIndex) > calcZoneSizing.DesLatentCoolLoad) {
                        calcZoneSizing.DesLatentCoolLoad = calcZoneSizing.LatentCoolLoadSeq(TimeStepIndex);
                        calcZoneSizing.DesLatentCoolMassFlow = calcZoneSizing.LatentCoolFlowSeq(TimeStepIndex);
                        calcZoneSizing.ZoneTempAtLatentCoolPeak = calcZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtLatentCoolPeak = calcZoneSizing.CoolOutTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneHumRatAtLatentCoolPeak = calcZoneSizing.CoolZoneHumRatSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtLatentCoolPeak = calcZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
                        calcZoneSizing.ZoneRetTempAtLatentCoolPeak = calcZoneSizing.CoolZoneRetTempSeq(TimeStepIndex);
                        calcZoneSizing.TimeStepNumAtLatentCoolMax = TimeStepIndex;
                    }
                }
            }
            if (calcZoneSizing.DesCoolMassFlow > 0.0) {
                calcZoneSizing.DesCoolVolFlow = calcZoneSizing.DesCoolMassFlow / calcZoneSizing.DesCoolDens;
                OAFrac = calcZoneSizing.MinOA / max(calcZoneSizing.DesCoolVolFlow, DataHVACGlobals::SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = calcZoneSizing.TimeStepNumAtCoolMax;
                calcZoneSizing.DesCoolCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneTempAtCoolPeak;
                calcZoneSizing.DesCoolCoilInHumRat =
                    OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneHumRatAtCoolPeak;
            }
            if (calcZoneSizing.zoneLatentSizing && calcZoneSizing.DesLatentCoolMassFlow > 0.0) {
                calcZoneSizing.DesLatentCoolVolFlow = calcZoneSizing.DesLatentCoolMassFlow / state.dataEnvrn->StdRhoAir;
                OAFrac = calcZoneSizing.MinOA / max(calcZoneSizing.DesCoolVolFlow, DataHVACGlobals::SmallMassFlow);
                OAFrac = min(1.0, max(0.0, OAFrac));
                TimeStepAtPeak = calcZoneSizing.TimeStepNumAtLatentCoolMax;
                calcZoneSizing.DesLatentCoolCoilInTemp =
                    OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneTempAtCoolPeak;
                calcZoneSizing.DesLatentCoolCoilInHumRat =
                    OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * calcZoneSizing.ZoneHumRatAtCoolPeak;
            }
            // from all the design periods, choose the one needing the most heating and save all its design variables in CalcFinalZoneSizing
            if (calcZoneSizing.DesHeatVolFlow > calcFinalZoneSizing.DesHeatVolFlow) {
                calcFinalZoneSizing.DesHeatVolFlow = calcZoneSizing.DesHeatVolFlow;
                calcFinalZoneSizing.DesHeatLoad = calcZoneSizing.DesHeatLoad;
                calcFinalZoneSizing.DesHeatMassFlow = calcZoneSizing.DesHeatMassFlow;
                calcFinalZoneSizing.HeatDesDay = calcZoneSizing.HeatDesDay;
                calcFinalZoneSizing.DesHeatDens = calcZoneSizing.DesHeatDens;
                calcFinalZoneSizing.HeatFlowSeq = calcZoneSizing.HeatFlowSeq;
                calcFinalZoneSizing.HeatLoadSeq = calcZoneSizing.HeatLoadSeq;
                calcFinalZoneSizing.HeatZoneTempSeq = calcZoneSizing.HeatZoneTempSeq;
                calcFinalZoneSizing.HeatOutTempSeq = calcZoneSizing.HeatOutTempSeq;
                calcFinalZoneSizing.HeatZoneRetTempSeq = calcZoneSizing.HeatZoneRetTempSeq;
                calcFinalZoneSizing.HeatZoneHumRatSeq = calcZoneSizing.HeatZoneHumRatSeq;
                calcFinalZoneSizing.HeatOutHumRatSeq = calcZoneSizing.HeatOutHumRatSeq;
                calcFinalZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.ZoneTempAtHeatPeak;
                calcFinalZoneSizing.OutTempAtHeatPeak = calcZoneSizing.OutTempAtHeatPeak;
                calcFinalZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.ZoneRetTempAtHeatPeak;
                calcFinalZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.ZoneHumRatAtHeatPeak;
                calcFinalZoneSizing.OutHumRatAtHeatPeak = calcZoneSizing.OutHumRatAtHeatPeak;
                calcFinalZoneSizing.HeatDDNum = calcZoneSizing.HeatDDNum;
                calcFinalZoneSizing.cHeatDDDate = desDayWeath.DateString;
                calcFinalZoneSizing.TimeStepNumAtHeatMax = calcZoneSizing.TimeStepNumAtHeatMax;
                calcFinalZoneSizing.DesHeatCoilInTemp = calcZoneSizing.DesHeatCoilInTemp;
                calcFinalZoneSizing.DesHeatCoilInHumRat = calcZoneSizing.DesHeatCoilInHumRat;
            } else {
                calcFinalZoneSizing.DesHeatDens = state.dataEnvrn->StdRhoAir;
                // save design heating load when the there is design heating load and the design heating volume flow rate is zero, i.e., when
                // design heating volume flow rate is set to zero due to heating supply air temp less than zone thermostat temperature
                if (calcZoneSizing.DesHeatLoad > calcFinalZoneSizing.DesHeatLoad) {
                    calcFinalZoneSizing.DesHeatLoad = calcZoneSizing.DesHeatLoad;
                    calcFinalZoneSizing.HeatDesDay = calcZoneSizing.HeatDesDay;
                    calcFinalZoneSizing.HeatLoadSeq = calcZoneSizing.HeatLoadSeq;
                    calcFinalZoneSizing.HeatZoneTempSeq = calcZoneSizing.HeatZoneTempSeq;
                    calcFinalZoneSizing.HeatOutTempSeq = calcZoneSizing.HeatOutTempSeq;
                    calcFinalZoneSizing.HeatZoneRetTempSeq = calcZoneSizing.HeatZoneRetTempSeq;
                    calcFinalZoneSizing.HeatZoneHumRatSeq = calcZoneSizing.HeatZoneHumRatSeq;
                    calcFinalZoneSizing.HeatOutHumRatSeq = calcZoneSizing.HeatOutHumRatSeq;
                    calcFinalZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.ZoneTempAtHeatPeak;
                    calcFinalZoneSizing.OutTempAtHeatPeak = calcZoneSizing.OutTempAtHeatPeak;
                    calcFinalZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.ZoneRetTempAtHeatPeak;
                    calcFinalZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.ZoneHumRatAtHeatPeak;
                    calcFinalZoneSizing.OutHumRatAtHeatPeak = calcZoneSizing.OutHumRatAtHeatPeak;
                    calcFinalZoneSizing.HeatDDNum = calcZoneSizing.HeatDDNum;
                    calcFinalZoneSizing.cHeatDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.TimeStepNumAtHeatMax = calcZoneSizing.TimeStepNumAtHeatMax;
                    calcFinalZoneSizing.DesHeatCoilInTemp = calcZoneSizing.DesHeatCoilInTemp;
                    calcFinalZoneSizing.DesHeatCoilInHumRat = calcZoneSizing.DesHeatCoilInHumRat;
                    calcFinalZoneSizing.HeatTstatTemp = calcZoneSizing.HeatTstatTemp;
                }
            }
            if (calcZoneSizing.zoneLatentSizing) {
                // from all the design periods, choose the one needing the most latent heating and save all its design variables in
                // CalcFinalZoneSizing
                if (calcZoneSizing.DesLatentHeatVolFlow > calcFinalZoneSizing.DesLatentHeatVolFlow) {
                    calcFinalZoneSizing.DesLatentHeatVolFlow = calcZoneSizing.DesLatentHeatVolFlow;
                    calcFinalZoneSizing.DesLatentHeatMassFlow = calcZoneSizing.ZoneHeatLatentMassFlow;
                    calcFinalZoneSizing.DesLatentHeatLoad = calcZoneSizing.DesLatentHeatLoad;
                    calcFinalZoneSizing.ZoneTempAtLatentHeatPeak = calcZoneSizing.ZoneTempAtLatentHeatPeak;
                    calcFinalZoneSizing.ZoneHumRatAtLatentHeatPeak = calcZoneSizing.ZoneHumRatAtLatentHeatPeak;
                    calcFinalZoneSizing.ZoneRetTempAtLatentHeatPeak = calcZoneSizing.ZoneRetTempAtLatentHeatPeak;
                    calcFinalZoneSizing.DesLatentHeatCoilInTemp = calcZoneSizing.DesLatentHeatCoilInTemp;
                    calcFinalZoneSizing.DesLatentHeatCoilInHumRat = calcZoneSizing.DesLatentHeatCoilInHumRat;
                    calcFinalZoneSizing.LatHeatDesDay = calcZoneSizing.LatHeatDesDay;
                    calcFinalZoneSizing.cLatentHeatDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.LatentHeatDDNum = calcZoneSizing.LatentHeatDDNum;
                    calcFinalZoneSizing.TimeStepNumAtLatentHeatMax = calcZoneSizing.TimeStepNumAtLatentHeatMax;
                    calcFinalZoneSizing.LatentHeatLoadSeq = calcZoneSizing.LatentHeatLoadSeq;
                    calcFinalZoneSizing.LatentHeatFlowSeq = calcZoneSizing.LatentHeatFlowSeq;
                } else {
                    // save design latent heating load when the there is design load and the design volume flow rate is zero, i.e., when
                    // design latent heating volume flow rate is set to zero due to heating supply air humrat is less than zone humidistat humrat
                    if (calcZoneSizing.DesLatentHeatLoad > calcFinalZoneSizing.DesLatentHeatLoad) {
                        calcFinalZoneSizing.DesLatentHeatLoad = calcZoneSizing.DesLatentHeatLoad;
                        calcFinalZoneSizing.cLatentHeatDDDate = desDayWeath.DateString;
                        calcFinalZoneSizing.LatentHeatDDNum = calcZoneSizing.LatentHeatDDNum;
                        calcFinalZoneSizing.TimeStepNumAtLatentHeatMax = calcZoneSizing.TimeStepNumAtLatentHeatMax;
                        calcFinalZoneSizing.LatentHeatLoadSeq = calcZoneSizing.LatentHeatLoadSeq;
                        calcFinalZoneSizing.LatentHeatFlowSeq = calcZoneSizing.LatentHeatFlowSeq;
                    }
                }
            }
            // select largest load from NoDOAS arrays
            if (calcZoneSizing.DesHeatLoadNoDOAS > calcFinalZoneSizing.DesHeatLoadNoDOAS) {
                calcFinalZoneSizing.DesHeatLoadNoDOAS = calcZoneSizing.DesHeatLoadNoDOAS;
                calcFinalZoneSizing.HeatLoadNoDOASSeq = calcZoneSizing.HeatLoadNoDOASSeq;
                calcFinalZoneSizing.HeatNoDOASDDNum = calcZoneSizing.HeatNoDOASDDNum;
                calcFinalZoneSizing.HeatNoDOASDesDay = calcZoneSizing.HeatNoDOASDesDay;
                calcFinalZoneSizing.TimeStepNumAtHeatNoDOASMax = calcZoneSizing.TimeStepNumAtHeatNoDOASMax;
            }
            if (calcZoneSizing.DesLatentHeatLoadNoDOAS > calcFinalZoneSizing.DesLatentHeatLoadNoDOAS) {
                calcFinalZoneSizing.DesLatentHeatLoadNoDOAS = calcZoneSizing.DesLatentHeatLoadNoDOAS;
                calcFinalZoneSizing.HeatLatentLoadNoDOASSeq = calcZoneSizing.HeatLatentLoadNoDOASSeq;
                calcFinalZoneSizing.LatentHeatNoDOASDDNum = calcZoneSizing.LatentHeatNoDOASDDNum;
                calcFinalZoneSizing.LatHeatNoDOASDesDay = calcZoneSizing.LatHeatNoDOASDesDay;
                calcFinalZoneSizing.TimeStepNumAtLatentHeatNoDOASMax = calcZoneSizing.TimeStepNumAtLatentHeatNoDOASMax;
            }
            // from all the design periods, choose the one needing the most Cooling and save all its design variables in CalcFinalZoneSizing
            if (calcZoneSizing.DesCoolVolFlow > calcFinalZoneSizing.DesCoolVolFlow) {
                calcFinalZoneSizing.DesCoolVolFlow = calcZoneSizing.DesCoolVolFlow;
                calcFinalZoneSizing.DesCoolLoad = calcZoneSizing.DesCoolLoad;
                calcFinalZoneSizing.DesCoolMassFlow = calcZoneSizing.DesCoolMassFlow;
                calcFinalZoneSizing.CoolDesDay = calcZoneSizing.CoolDesDay;
                calcFinalZoneSizing.DesCoolDens = calcZoneSizing.DesCoolDens;
                calcFinalZoneSizing.CoolFlowSeq = calcZoneSizing.CoolFlowSeq;
                calcFinalZoneSizing.CoolLoadSeq = calcZoneSizing.CoolLoadSeq;
                calcFinalZoneSizing.CoolZoneTempSeq = calcZoneSizing.CoolZoneTempSeq;
                calcFinalZoneSizing.CoolOutTempSeq = calcZoneSizing.CoolOutTempSeq;
                calcFinalZoneSizing.CoolZoneRetTempSeq = calcZoneSizing.CoolZoneRetTempSeq;
                calcFinalZoneSizing.CoolZoneHumRatSeq = calcZoneSizing.CoolZoneHumRatSeq;
                calcFinalZoneSizing.CoolOutHumRatSeq = calcZoneSizing.CoolOutHumRatSeq;
                calcFinalZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.ZoneTempAtCoolPeak;
                calcFinalZoneSizing.OutTempAtCoolPeak = calcZoneSizing.OutTempAtCoolPeak;
                calcFinalZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.ZoneRetTempAtCoolPeak;
                calcFinalZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.ZoneHumRatAtCoolPeak;
                calcFinalZoneSizing.OutHumRatAtCoolPeak = calcZoneSizing.OutHumRatAtCoolPeak;
                calcFinalZoneSizing.CoolDDNum = calcZoneSizing.CoolDDNum;
                calcFinalZoneSizing.cCoolDDDate = desDayWeath.DateString;
                calcFinalZoneSizing.TimeStepNumAtCoolMax = calcZoneSizing.TimeStepNumAtCoolMax;
                calcFinalZoneSizing.DesCoolCoilInTemp = calcZoneSizing.DesCoolCoilInTemp;
                calcFinalZoneSizing.DesCoolCoilInHumRat = calcZoneSizing.DesCoolCoilInHumRat;
            } else {
                calcFinalZoneSizing.DesCoolDens = state.dataEnvrn->StdRhoAir;
                // save design cooling load when the there is design cooling load and the design cooling volume flow rate is zero, i.e., when
                // design cooling volume flow rate is set to zero due to cooling supply air temp greater than zone thermostat temperature
                if (calcZoneSizing.DesCoolLoad > calcFinalZoneSizing.DesCoolLoad) {
                    calcFinalZoneSizing.DesCoolLoad = calcZoneSizing.DesCoolLoad;
                    calcFinalZoneSizing.CoolDesDay = calcZoneSizing.CoolDesDay;
                    calcFinalZoneSizing.CoolLoadSeq = calcZoneSizing.CoolLoadSeq;
                    calcFinalZoneSizing.CoolZoneTempSeq = calcZoneSizing.CoolZoneTempSeq;
                    calcFinalZoneSizing.CoolOutTempSeq = calcZoneSizing.CoolOutTempSeq;
                    calcFinalZoneSizing.CoolZoneRetTempSeq = calcZoneSizing.CoolZoneRetTempSeq;
                    calcFinalZoneSizing.CoolZoneHumRatSeq = calcZoneSizing.CoolZoneHumRatSeq;
                    calcFinalZoneSizing.CoolOutHumRatSeq = calcZoneSizing.CoolOutHumRatSeq;
                    calcFinalZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.ZoneTempAtCoolPeak;
                    calcFinalZoneSizing.OutTempAtCoolPeak = calcZoneSizing.OutTempAtCoolPeak;
                    calcFinalZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.ZoneRetTempAtCoolPeak;
                    calcFinalZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.ZoneHumRatAtCoolPeak;
                    calcFinalZoneSizing.OutHumRatAtCoolPeak = calcZoneSizing.OutHumRatAtCoolPeak;
                    calcFinalZoneSizing.CoolDDNum = calcZoneSizing.CoolDDNum;
                    calcFinalZoneSizing.cCoolDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.TimeStepNumAtCoolMax = calcZoneSizing.TimeStepNumAtCoolMax;
                    calcFinalZoneSizing.DesCoolCoilInTemp = calcZoneSizing.DesCoolCoilInTemp;
                    calcFinalZoneSizing.DesCoolCoilInHumRat = calcZoneSizing.DesCoolCoilInHumRat;
                    calcFinalZoneSizing.CoolTstatTemp = calcZoneSizing.CoolTstatTemp;
                }
            }
            if (calcZoneSizing.zoneLatentSizing) {
                // from all the design periods, choose the one needing the most Latent Cooling and save all its design variables in
                // CalcFinalZoneSizing
                if (calcZoneSizing.DesLatentCoolVolFlow > calcFinalZoneSizing.DesLatentCoolVolFlow) {
                    calcFinalZoneSizing.DesLatentCoolVolFlow = calcZoneSizing.DesLatentCoolVolFlow;
                    calcFinalZoneSizing.DesLatentCoolMassFlow = calcZoneSizing.DesLatentCoolMassFlow;
                    calcFinalZoneSizing.DesLatentCoolLoad = calcZoneSizing.DesLatentCoolLoad;
                    calcFinalZoneSizing.ZoneTempAtLatentCoolPeak = calcZoneSizing.ZoneTempAtLatentCoolPeak;
                    calcFinalZoneSizing.ZoneHumRatAtLatentCoolPeak = calcZoneSizing.ZoneHumRatAtLatentCoolPeak;
                    calcFinalZoneSizing.ZoneRetTempAtLatentCoolPeak = calcZoneSizing.ZoneRetTempAtLatentCoolPeak;
                    calcFinalZoneSizing.DesLatentCoolCoilInTemp = calcZoneSizing.DesLatentCoolCoilInTemp;
                    calcFinalZoneSizing.DesLatentCoolCoilInHumRat = calcZoneSizing.DesLatentCoolCoilInHumRat;
                    calcFinalZoneSizing.LatCoolDesDay = calcZoneSizing.LatCoolDesDay;
                    calcFinalZoneSizing.cLatentCoolDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.LatentCoolDDNum = calcZoneSizing.LatentCoolDDNum;
                    calcFinalZoneSizing.TimeStepNumAtLatentCoolMax = calcZoneSizing.TimeStepNumAtLatentCoolMax;
                    calcFinalZoneSizing.LatentCoolLoadSeq = calcZoneSizing.LatentCoolLoadSeq;
                    calcFinalZoneSizing.LatentCoolFlowSeq = calcZoneSizing.LatentCoolFlowSeq;
                } else {
                    // save design latent cooling load when the there is design load and the design volume flow rate is zero, i.e., when
                    // design latent cooling volume flow rate is set to zero due to cooling supply air humrat is greater than zone humidistat humrat
                    if (calcZoneSizing.DesLatentCoolLoad > calcFinalZoneSizing.DesLatentCoolLoad) {
                        calcFinalZoneSizing.DesLatentCoolLoad = calcZoneSizing.DesLatentCoolLoad;
                        calcFinalZoneSizing.cLatentCoolDDDate = desDayWeath.DateString;
                        calcFinalZoneSizing.LatentCoolDDNum = calcZoneSizing.LatentCoolDDNum;
                        calcFinalZoneSizing.LatCoolDesDay = calcZoneSizing.LatCoolDesDay;
                        calcFinalZoneSizing.TimeStepNumAtLatentCoolMax = calcZoneSizing.TimeStepNumAtLatentCoolMax;
                        calcFinalZoneSizing.LatentCoolLoadSeq = calcZoneSizing.LatentCoolLoadSeq;
                    }
                }
            }
            if (calcZoneSizing.DesCoolLoadNoDOAS > calcFinalZoneSizing.DesCoolLoadNoDOAS) {
                calcFinalZoneSizing.DesCoolLoadNoDOAS = calcZoneSizing.DesCoolLoadNoDOAS;
                calcFinalZoneSizing.CoolLoadNoDOASSeq = calcZoneSizing.CoolLoadNoDOASSeq;
                calcFinalZoneSizing.CoolNoDOASDDNum = calcZoneSizing.CoolNoDOASDDNum;
                calcFinalZoneSizing.CoolNoDOASDesDay = calcZoneSizing.CoolNoDOASDesDay;
                calcFinalZoneSizing.TimeStepNumAtCoolNoDOASMax = calcZoneSizing.TimeStepNumAtCoolNoDOASMax;
            }
            if (calcZoneSizing.DesLatentCoolLoadNoDOAS > calcFinalZoneSizing.DesLatentCoolLoadNoDOAS) {
                calcFinalZoneSizing.DesLatentCoolLoadNoDOAS = calcZoneSizing.DesLatentCoolLoadNoDOAS;
                calcFinalZoneSizing.CoolLatentLoadNoDOASSeq = calcZoneSizing.CoolLatentLoadNoDOASSeq;
                calcFinalZoneSizing.LatentCoolNoDOASDDNum = calcZoneSizing.LatentCoolNoDOASDDNum;
                calcFinalZoneSizing.LatCoolNoDOASDesDay = calcZoneSizing.LatCoolNoDOASDesDay;
                calcFinalZoneSizing.TimeStepNumAtLatentCoolNoDOASMax = calcZoneSizing.TimeStepNumAtLatentCoolNoDOASMax;
            }
            // save heat peak conditions when there is no design heating load or design heating volume flow rate, i.e., when
            // zone temperature is always greater than the zone heating thermostat temperature
            if (calcFinalZoneSizing.DesHeatLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if ((calcZoneSizing.HeatZoneTempSeq(TimeStepIndex) < calcZoneSizing.ZoneTempAtHeatPeak) || FirstIteration) {
                        calcZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtHeatPeak = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.HeatZoneRetTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.HeatZoneHumRatSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtHeatPeak = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                        calcZoneSizing.TimeStepNumAtHeatMax = TimeStepIndex;
                        FirstIteration = false;
                    }
                }
                if (calcZoneSizing.OutTempAtHeatPeak <= calcFinalZoneSizing.OutTempAtHeatPeak) {
                    calcFinalZoneSizing.HeatDesDay = calcZoneSizing.HeatDesDay;
                    calcFinalZoneSizing.HeatZoneTempSeq = calcZoneSizing.HeatZoneTempSeq;
                    calcFinalZoneSizing.HeatOutTempSeq = calcZoneSizing.HeatOutTempSeq;
                    calcFinalZoneSizing.HeatZoneRetTempSeq = calcZoneSizing.HeatZoneRetTempSeq;
                    calcFinalZoneSizing.HeatZoneHumRatSeq = calcZoneSizing.HeatZoneHumRatSeq;
                    calcFinalZoneSizing.HeatOutHumRatSeq = calcZoneSizing.HeatOutHumRatSeq;
                    calcFinalZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.ZoneTempAtHeatPeak;
                    calcFinalZoneSizing.OutTempAtHeatPeak = calcZoneSizing.OutTempAtHeatPeak;
                    calcFinalZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.ZoneRetTempAtHeatPeak;
                    calcFinalZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.ZoneHumRatAtHeatPeak;
                    calcFinalZoneSizing.OutHumRatAtHeatPeak = calcZoneSizing.OutHumRatAtHeatPeak;
                    calcFinalZoneSizing.HeatDDNum = calcZoneSizing.HeatDDNum;
                    calcFinalZoneSizing.cHeatDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.TimeStepNumAtHeatMax = calcZoneSizing.TimeStepNumAtHeatMax;
                    calcFinalZoneSizing.DesHeatCoilInTemp = calcZoneSizing.DesHeatCoilInTemp;
                    calcFinalZoneSizing.DesHeatCoilInHumRat = calcZoneSizing.DesHeatCoilInHumRat;
                    calcFinalZoneSizing.HeatTstatTemp = calcZoneSizing.HeatTstatTemp;
                    FirstIteration = false;
                }
            }
            if (calcFinalZoneSizing.zoneLatentSizing && calcFinalZoneSizing.DesLatentHeatLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (calcZoneSizing.HeatZoneTempSeq(TimeStepIndex) < calcZoneSizing.ZoneTempAtLatentHeatPeak || FirstIteration) {
                        calcZoneSizing.ZoneTempAtLatentHeatPeak = calcZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtLatentHeatPeak = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtLatentHeatPeak = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                    }
                    if (calcZoneSizing.HeatOutTempSeq(TimeStepIndex) <= calcFinalZoneSizing.OutTempAtLatentHeatPeak) {
                        calcFinalZoneSizing.OutTempAtLatentHeatPeak = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                        calcFinalZoneSizing.OutHumRatAtLatentHeatPeak = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                        calcFinalZoneSizing.LatHeatDesDay = calcZoneSizing.LatHeatDesDay;
                        calcFinalZoneSizing.LatentHeatDDNum = calcZoneSizing.LatentHeatDDNum;
                        calcFinalZoneSizing.cLatentHeatDDDate = desDayWeath.DateString;
                        calcFinalZoneSizing.TimeStepNumAtLatentHeatMax = calcZoneSizing.TimeStepNumAtLatentHeatMax;
                    }
                    FirstIteration = false;
                }
            }
            // save cool peak conditions when there is no design cooling load or design cooling volume flow rate, i.e., when
            // zone temperature is always less than the zone cooling thermostat temperature
            if (calcFinalZoneSizing.DesCoolLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if ((calcZoneSizing.CoolZoneTempSeq(TimeStepIndex) > calcZoneSizing.ZoneTempAtCoolPeak) || FirstIteration) {
                        calcZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtCoolPeak = calcZoneSizing.CoolOutTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.CoolZoneRetTempSeq(TimeStepIndex);
                        calcZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.CoolZoneHumRatSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtCoolPeak = calcZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
                        calcZoneSizing.TimeStepNumAtCoolMax = TimeStepIndex;
                        FirstIteration = false;
                    }
                }
                if (calcZoneSizing.OutTempAtCoolPeak > calcFinalZoneSizing.OutTempAtCoolPeak) {
                    calcFinalZoneSizing.CoolDesDay = calcZoneSizing.CoolDesDay;
                    calcFinalZoneSizing.CoolZoneTempSeq = calcZoneSizing.CoolZoneTempSeq;
                    calcFinalZoneSizing.CoolOutTempSeq = calcZoneSizing.CoolOutTempSeq;
                    calcFinalZoneSizing.CoolZoneRetTempSeq = calcZoneSizing.CoolZoneRetTempSeq;
                    calcFinalZoneSizing.CoolZoneHumRatSeq = calcZoneSizing.CoolZoneHumRatSeq;
                    calcFinalZoneSizing.CoolOutHumRatSeq = calcZoneSizing.CoolOutHumRatSeq;
                    calcFinalZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.ZoneTempAtCoolPeak;
                    calcFinalZoneSizing.OutTempAtCoolPeak = calcZoneSizing.OutTempAtCoolPeak;
                    calcFinalZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.ZoneRetTempAtCoolPeak;
                    calcFinalZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.ZoneHumRatAtCoolPeak;
                    calcFinalZoneSizing.OutHumRatAtCoolPeak = calcZoneSizing.OutHumRatAtCoolPeak;
                    calcFinalZoneSizing.CoolDDNum = calcZoneSizing.CoolDDNum;
                    calcFinalZoneSizing.cCoolDDDate = desDayWeath.DateString;
                    calcFinalZoneSizing.TimeStepNumAtCoolMax = calcZoneSizing.TimeStepNumAtCoolMax;
                    calcFinalZoneSizing.DesCoolCoilInTemp = calcZoneSizing.DesCoolCoilInTemp;
                    calcFinalZoneSizing.DesCoolCoilInHumRat = calcZoneSizing.DesCoolCoilInHumRat;
                    calcFinalZoneSizing.CoolTstatTemp = calcZoneSizing.CoolTstatTemp;
                }
            }
            if (calcFinalZoneSizing.zoneLatentSizing && calcFinalZoneSizing.DesLatentCoolLoad == 0) {
                bool FirstIteration = true;
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (calcZoneSizing.CoolZoneTempSeq(TimeStepIndex) > calcZoneSizing.ZoneTempAtLatentCoolPeak || FirstIteration) {
                        calcZoneSizing.ZoneTempAtLatentCoolPeak = calcZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                        calcZoneSizing.OutTempAtLatentCoolPeak = calcZoneSizing.CoolOutTempSeq(TimeStepIndex);
                        calcZoneSizing.OutHumRatAtLatentCoolPeak = calcZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
                        FirstIteration = false;
                    }
                    if (calcZoneSizing.OutTempAtLatentCoolPeak >= calcFinalZoneSizing.OutTempAtLatentCoolPeak) {
                        calcFinalZoneSizing.LatCoolDesDay = calcZoneSizing.LatCoolDesDay;
                        calcFinalZoneSizing.LatentCoolDDNum = calcZoneSizing.LatentCoolDDNum;
                        calcFinalZoneSizing.cLatentCoolDDDate = desDayWeath.DateString;
                        calcFinalZoneSizing.TimeStepNumAtLatentCoolMax = calcZoneSizing.TimeStepNumAtLatentCoolMax;
                    }
                }
            }
        }
    } break;
    case DataGlobalConstants::CallIndicator::EndZoneSizingCalc: {
        // candidate EMS calling point to customize CalcFinalZoneSizing
        bool anyEMSRan;
        EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::ZoneSizing, anyEMSRan, ObjexxFCL::Optional_int_const());

        // now apply EMS overrides (if any)
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
                if (calcFinalZoneSizing.EMSOverrideDesHeatMassOn) {
                    if (calcFinalZoneSizing.DesHeatMassFlow > 0.0) calcFinalZoneSizing.DesHeatMassFlow = calcFinalZoneSizing.EMSValueDesHeatMassFlow;
                }
                if (calcFinalZoneSizing.EMSOverrideDesCoolMassOn) {
                    if (calcFinalZoneSizing.DesCoolMassFlow > 0.0) calcFinalZoneSizing.DesCoolMassFlow = calcFinalZoneSizing.EMSValueDesCoolMassFlow;
                }
                if (calcFinalZoneSizing.EMSOverrideDesHeatLoadOn) {
                    if (calcFinalZoneSizing.DesHeatLoad > 0.0) calcFinalZoneSizing.DesHeatLoad = calcFinalZoneSizing.EMSValueDesHeatLoad;
                }
                if (calcFinalZoneSizing.EMSOverrideDesCoolLoadOn) {
                    if (calcFinalZoneSizing.DesCoolLoad > 0.0) calcFinalZoneSizing.DesCoolLoad = calcFinalZoneSizing.EMSValueDesCoolLoad;
                }
                if (calcFinalZoneSizing.EMSOverrideDesHeatVolOn) {
                    if (calcFinalZoneSizing.DesHeatVolFlow > 0.0) calcFinalZoneSizing.DesHeatVolFlow = calcFinalZoneSizing.EMSValueDesHeatVolFlow;
                }
                if (calcFinalZoneSizing.EMSOverrideDesCoolVolOn) {
                    if (calcFinalZoneSizing.DesCoolVolFlow > 0.0) calcFinalZoneSizing.DesCoolVolFlow = calcFinalZoneSizing.EMSValueDesCoolVolFlow;
                }
            }
        }

        if (!state.dataGlobal->isPulseZoneSizing) {

            for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
                if (std::abs(calcFinalZoneSizing.DesCoolLoad) <= 1.e-8) {
                    ShowWarningError(state, format("Calculated design cooling load for zone={} is zero.", calcFinalZoneSizing.ZoneName));
                    ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
                }
                if (std::abs(calcFinalZoneSizing.DesHeatLoad) <= 1.e-8) {
                    ShowWarningError(state, format("Calculated design heating load for zone={} is zero.", calcFinalZoneSizing.ZoneName));
                    ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
                }
            }

            print(state.files.zsz, "Time");
            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(I);

                static constexpr std::string_view ZSizeFmt11("{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{"
                                                             "}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}{}{}:{}{}");
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
                      ":Des Cool Mass Flow [kg/s]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatHeatDesDay,
                      ":Des Latent Heat Load [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatCoolDesDay,
                      ":Des Latent Cool Load [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatHeatDesDay,
                      ":Des Latent Heat Mass Flow [kg/s]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatCoolDesDay,
                      ":Des Latent Cool Mass Flow [kg/s]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.HeatNoDOASDesDay,
                      ":Des Heat Load No DOAS [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.CoolNoDOASDesDay,
                      ":Des Sens Cool Load No DOAS [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatHeatNoDOASDesDay,
                      ":Des Latent Heat Load No DOAS [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.LatCoolNoDOASDesDay,
                      ":Des Latent Cool Load No DOAS [W]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.HeatDesDay,
                      ":Heating Zone Temperature [C]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.HeatDesDay,
                      ":Heating Zone Relative Humidity [%]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.CoolDesDay,
                      ":Cooling Zone Temperature [C]",
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.ZoneName,
                      calcFinalZoneSizing.CoolDesDay,
                      ":Cooling Zone Relative Humidity [%]");

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
                    if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
                        if (std::abs(DeltaTemp) >= 2.0) {                                                    // Vdot exceeds 3000 cfm/ton @ DT=2
                            ShowWarningError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of zone temperature");
                        } else {
                            ShowSevereError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of zone temperature");
                        }
                        ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
                        ShowContinueError(state, format("...zone name = {}", state.dataSize->CalcFinalZoneSizing(I).ZoneName));
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
                    } else if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff &&
                               SupplyTemp > state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtCoolPeak) {
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
                        ShowContinueError(state, format("...occurs in zone              = {}", state.dataSize->CalcFinalZoneSizing(I).ZoneName));
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

                    if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
                        if (std::abs(DeltaTemp) >= 2.0) {                                                    // Vdot exceeds 3000 cfm/ton @ DT=2
                            ShowWarningError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of zone temperature");
                        } else {
                            ShowSevereError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of zone temperature");
                        }
                        ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
                        ShowContinueError(state, format("...zone name = {}", state.dataSize->CalcFinalZoneSizing(I).ZoneName));
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
                    } else if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff &&
                               SupplyTemp < state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak) {
                        ShowSevereError(
                            state, "UpdateZoneSizing: Supply air temperature is less than zone temperature during heating air flow calculations");
                        ShowContinueError(state,
                                          format("...calculated design heating volume flow rate = {:.5R} m3/s",
                                                 (state.dataSize->CalcFinalZoneSizing(I).DesHeatVolFlow)));
                        ShowContinueError(state,
                                          format("...calculated design heating mass flow rate   = {:.5R} kg/s",
                                                 (state.dataSize->CalcFinalZoneSizing(I).DesHeatMassFlow)));
                        ShowContinueError(state,
                                          format("...thermostat set point temp   = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).HeatTstatTemp));
                        ShowContinueError(
                            state, format("...zone temperature            = {:.3R} C", state.dataSize->CalcFinalZoneSizing(I).ZoneTempAtHeatPeak));
                        ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
                        ShowContinueError(state, format("...occurs in zone              = {}", state.dataSize->CalcFinalZoneSizing(I).ZoneName));
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
                    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                        auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
                        if (TimeStepIndex == calcFinalZoneSizing.TimeStepNumAtHeatMax) {
                            state.dataSize->HeatPeakDateHrMin(CtrlZoneNum) =
                                calcFinalZoneSizing.cHeatDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                        if (TimeStepIndex == calcFinalZoneSizing.TimeStepNumAtCoolMax) {
                            state.dataSize->CoolPeakDateHrMin(CtrlZoneNum) =
                                calcFinalZoneSizing.cCoolDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                        if (TimeStepIndex == calcFinalZoneSizing.TimeStepNumAtLatentHeatMax) {
                            state.dataSize->LatHeatPeakDateHrMin(CtrlZoneNum) =
                                calcFinalZoneSizing.cLatentHeatDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                        if (TimeStepIndex == calcFinalZoneSizing.TimeStepNumAtLatentCoolMax) {
                            state.dataSize->LatCoolPeakDateHrMin(CtrlZoneNum) =
                                calcFinalZoneSizing.cLatentCoolDDDate + ' ' + format(PeakHrMinFmt, HourPrint, Minutes);
                        }
                    }

                    static constexpr std::string_view ZSizeFmt20("{:02}:{:02}:00");
                    print(state.files.zsz, ZSizeFmt20, HourPrint, Minutes);
                    for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                        auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(I);
                        static constexpr std::string_view ZSizeFmt21(
                            "{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12."
                            "6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}");
                        Real64 ZoneRHHeat = 0.0;
                        Real64 ZoneRHCool = 0.0;
                        Real64 ZoneTHeat = 0.0;
                        Real64 ZoneTCool = 0.0;
                        if (calcFinalZoneSizing.HeatDDNum > 0) {
                            ZoneTHeat = state.dataSize->CalcZoneSizing(calcFinalZoneSizing.HeatDDNum, I).HeatZoneTempSeq(TimeStepIndex);
                            ZoneRHHeat = Psychrometrics::PsyRhFnTdbWPb(
                                             state,
                                             state.dataSize->CalcZoneSizing(calcFinalZoneSizing.HeatDDNum, I).HeatZoneTempSeq(TimeStepIndex),
                                             state.dataSize->CalcZoneSizing(calcFinalZoneSizing.HeatDDNum, I).HeatZoneHumRatSeq(TimeStepIndex),
                                             state.dataEnvrn->OutBaroPress) *
                                         100.0;
                        }
                        if (calcFinalZoneSizing.CoolDDNum > 0) {
                            ZoneTCool = state.dataSize->CalcZoneSizing(calcFinalZoneSizing.CoolDDNum, I).CoolZoneTempSeq(TimeStepIndex);
                            ZoneRHCool = Psychrometrics::PsyRhFnTdbWPb(
                                             state,
                                             state.dataSize->CalcZoneSizing(calcFinalZoneSizing.CoolDDNum, I).CoolZoneTempSeq(TimeStepIndex),
                                             state.dataSize->CalcZoneSizing(calcFinalZoneSizing.CoolDDNum, I).CoolZoneHumRatSeq(TimeStepIndex),
                                             state.dataEnvrn->OutBaroPress) *
                                         100.0;
                        }
                        print(state.files.zsz,
                              ZSizeFmt21,
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.HeatLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.CoolLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.HeatFlowSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.CoolFlowSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.LatentHeatLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.LatentCoolLoadSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.LatentHeatFlowSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.LatentCoolFlowSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.HeatLoadNoDOASSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.CoolLoadNoDOASSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.HeatLatentLoadNoDOASSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              calcFinalZoneSizing.CoolLatentLoadNoDOASSeq(TimeStepIndex),
                              state.dataSize->SizingFileColSep,
                              ZoneTHeat,
                              state.dataSize->SizingFileColSep,
                              ZoneRHHeat,
                              state.dataSize->SizingFileColSep,
                              ZoneTCool,
                              state.dataSize->SizingFileColSep,
                              ZoneRHCool);
                    }
                    print(state.files.zsz, "\n");
                }
            }
            print(state.files.zsz, "Peak");

            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(I);

                static constexpr std::string_view ZSizeFmt31("{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12."
                                                             "6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{}{}{}");
                print(state.files.zsz,
                      ZSizeFmt31,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesHeatLoad,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesCoolLoad,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesHeatMassFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesCoolMassFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentHeatLoad,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentCoolLoad,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentHeatMassFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentCoolMassFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesHeatLoadNoDOAS,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesCoolLoadNoDOAS,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentHeatLoadNoDOAS,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentCoolLoadNoDOAS,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep);
            }
            print(state.files.zsz, "\n");

            print(state.files.zsz, "\nPeak Vol Flow (m3/s)");
            for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
                if (!state.dataZoneEquip->ZoneEquipConfig(I).IsControlled) continue;
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(I);
                static constexpr std::string_view ZSizeFmt41("{}{}{}{:12.6E}{}{:12.6E}{}{}{}{:12.6E}{}{:12.6E}{}{}{}{}{}{}{}{}");
                print(state.files.zsz,
                      ZSizeFmt41,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesHeatVolFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesCoolVolFlow,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentHeatVolFlow,
                      state.dataSize->SizingFileColSep,
                      calcFinalZoneSizing.DesLatentCoolVolFlow,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep,
                      state.dataSize->SizingFileColSep);
            }
            print(state.files.zsz, "\n");
            state.files.zsz.close();
        }

        if (!state.dataGlobal->isPulseZoneSizing) {
            // Move sizing data into final sizing array according to sizing method
            for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(zoneNum).IsControlled) continue;
                // if this zone does not use latent sizing, skip zone and retain sensible load variables
                if (!state.dataSize->CalcFinalZoneSizing(zoneNum).zoneLatentSizing) continue;
                auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(zoneNum);

                // latent sizing data has the same variables as sensible sizing data
                // if the user has specified latent sizing, move the latent sizing data into the final calc arrays
                // this method allows all upstream sizing functions to use the same data as before (e.g., DesCoolVolFlow)
                // if sensible sizing, use sensible data. if latent sizing, use latent data (if there is latent data).
                // if sensible or latent sizing, use larger of sensible and latent based on volume flow rate

                if ((calcFinalZoneSizing.zoneSizingMethod == ZoneSizing::Latent && calcFinalZoneSizing.DesLatentCoolVolFlow > 0.0) ||
                    (calcFinalZoneSizing.zoneSizingMethod == ZoneSizing::SensibleAndLatent &&
                     calcFinalZoneSizing.DesLatentCoolLoad > calcFinalZoneSizing.DesCoolLoad)) {
                    state.dataHeatBal->isAnyLatentLoad = true;
                    calcFinalZoneSizing.CoolSizingType = "Latent Cooling"; // string reported to eio
                    calcFinalZoneSizing.DesCoolVolFlow = calcFinalZoneSizing.DesLatentCoolVolFlow;
                    calcFinalZoneSizing.DesCoolMassFlow = calcFinalZoneSizing.DesLatentCoolMassFlow;
                    calcFinalZoneSizing.DesCoolLoad = calcFinalZoneSizing.DesLatentCoolLoad;
                    calcFinalZoneSizing.CoolDesDay = calcFinalZoneSizing.LatCoolDesDay;
                    calcFinalZoneSizing.cCoolDDDate = calcFinalZoneSizing.cLatentCoolDDDate;
                    calcFinalZoneSizing.CoolDDNum = calcFinalZoneSizing.LatentCoolDDNum;
                    calcFinalZoneSizing.TimeStepNumAtCoolMax = calcFinalZoneSizing.TimeStepNumAtLatentCoolMax;
                    calcFinalZoneSizing.CoolFlowSeq = calcFinalZoneSizing.LatentCoolFlowSeq;
                    calcFinalZoneSizing.DesCoolCoilInTemp = calcFinalZoneSizing.DesLatentCoolCoilInTemp;
                    calcFinalZoneSizing.DesCoolCoilInHumRat = calcFinalZoneSizing.DesLatentCoolCoilInHumRat;
                    calcFinalZoneSizing.ZoneRetTempAtCoolPeak = calcFinalZoneSizing.ZoneRetTempAtLatentCoolPeak;
                    calcFinalZoneSizing.ZoneTempAtCoolPeak = calcFinalZoneSizing.ZoneTempAtLatentCoolPeak;
                    calcFinalZoneSizing.ZoneHumRatAtCoolPeak = calcFinalZoneSizing.ZoneHumRatAtLatentCoolPeak;
                    state.dataSize->CoolPeakDateHrMin(zoneNum) = state.dataSize->LatCoolPeakDateHrMin(zoneNum);

                    // the zone supply air humrat used for latent sizing is required to adequately size coil capacity
                    if (calcFinalZoneSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio) {
                        calcFinalZoneSizing.CoolDesHumRat = calcFinalZoneSizing.LatentCoolDesHumRat;
                    } else {
                        calcFinalZoneSizing.CoolDesHumRat = calcFinalZoneSizing.ZoneHumRatAtLatentCoolPeak - calcFinalZoneSizing.CoolDesHumRatDiff;
                    }

                    if (calcFinalZoneSizing.LatentCoolDDNum > 0) {
                        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(calcFinalZoneSizing.LatentCoolDDNum, zoneNum);
                        calcZoneSizing.DesCoolVolFlow = calcZoneSizing.DesLatentCoolVolFlow;
                        calcZoneSizing.DesCoolMassFlow = calcZoneSizing.DesLatentCoolMassFlow;
                        calcZoneSizing.DesCoolLoad = calcZoneSizing.DesLatentCoolLoad;
                        calcZoneSizing.CoolDesDay = calcZoneSizing.LatCoolDesDay;
                        calcZoneSizing.cCoolDDDate = calcFinalZoneSizing.cLatentCoolDDDate; // this has correct CoolDDDate
                        calcZoneSizing.CoolDDNum = calcZoneSizing.LatentCoolDDNum;
                        calcZoneSizing.TimeStepNumAtCoolMax = calcZoneSizing.TimeStepNumAtLatentCoolMax;
                        calcZoneSizing.CoolFlowSeq = calcZoneSizing.LatentCoolFlowSeq;
                        calcZoneSizing.DesCoolCoilInTemp = calcZoneSizing.DesLatentCoolCoilInTemp;
                        calcZoneSizing.DesCoolCoilInHumRat = calcZoneSizing.DesLatentCoolCoilInHumRat;
                        calcZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.ZoneRetTempAtLatentCoolPeak;
                        calcZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.ZoneTempAtLatentCoolPeak;
                        calcZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.ZoneHumRatAtLatentCoolPeak;
                        state.dataSize->CoolPeakDateHrMin(zoneNum) = state.dataSize->LatCoolPeakDateHrMin(zoneNum);

                        // the zone supply air humrat used for latent sizing is required to adequately size coil capacity
                        if (calcZoneSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio) {
                            calcZoneSizing.CoolDesHumRat = calcZoneSizing.LatentCoolDesHumRat;
                        } else {
                            calcZoneSizing.CoolDesHumRat = calcZoneSizing.ZoneHumRatAtLatentCoolPeak - calcZoneSizing.CoolDesHumRatDiff;
                        }
                    }
                }
                if ((calcFinalZoneSizing.zoneSizingMethod == ZoneSizing::Latent && calcFinalZoneSizing.DesLatentHeatVolFlow > 0.0) ||
                    (calcFinalZoneSizing.zoneSizingMethod == ZoneSizing::SensibleAndLatent &&
                     calcFinalZoneSizing.DesLatentHeatLoad > calcFinalZoneSizing.DesHeatLoad)) {

                    calcFinalZoneSizing.HeatSizingType = "Latent Heating"; // string reported to eio
                    calcFinalZoneSizing.DesHeatVolFlow = calcFinalZoneSizing.DesLatentHeatVolFlow;
                    calcFinalZoneSizing.DesHeatMassFlow = calcFinalZoneSizing.DesLatentHeatMassFlow;
                    calcFinalZoneSizing.DesHeatLoad = calcFinalZoneSizing.DesLatentHeatLoad;
                    calcFinalZoneSizing.HeatDesDay = calcFinalZoneSizing.LatHeatDesDay;
                    calcFinalZoneSizing.cHeatDDDate = calcFinalZoneSizing.cLatentHeatDDDate;
                    calcFinalZoneSizing.HeatDDNum = calcFinalZoneSizing.LatentHeatDDNum;
                    calcFinalZoneSizing.TimeStepNumAtHeatMax = calcFinalZoneSizing.TimeStepNumAtLatentHeatMax;
                    calcFinalZoneSizing.HeatFlowSeq = calcFinalZoneSizing.LatentHeatFlowSeq;
                    calcFinalZoneSizing.DesHeatCoilInTemp = calcFinalZoneSizing.DesLatentHeatCoilInTemp;
                    calcFinalZoneSizing.DesHeatCoilInHumRat = calcFinalZoneSizing.DesLatentHeatCoilInHumRat;
                    calcFinalZoneSizing.ZoneRetTempAtHeatPeak = calcFinalZoneSizing.ZoneRetTempAtLatentHeatPeak;
                    calcFinalZoneSizing.ZoneTempAtHeatPeak = calcFinalZoneSizing.ZoneTempAtLatentHeatPeak;
                    calcFinalZoneSizing.ZoneHumRatAtHeatPeak = calcFinalZoneSizing.ZoneHumRatAtLatentHeatPeak;
                    state.dataSize->HeatPeakDateHrMin(zoneNum) = state.dataSize->LatHeatPeakDateHrMin(zoneNum);

                    // will this cause sizing issues with heating coils since SA humrat is higher than zone humrat?
                    // use zone humrat instead? this value would size humidifiers well, but what about heating coils?
                    // not sure at this point if heating should reset HeatDesHumRat
                    if (calcFinalZoneSizing.ZnLatHeatDgnSAMethod == SupplyAirHumidityRatio) {
                        calcFinalZoneSizing.HeatDesHumRat = calcFinalZoneSizing.LatentHeatDesHumRat;
                    } else {
                        calcFinalZoneSizing.HeatDesHumRat = calcFinalZoneSizing.ZoneHumRatAtLatentHeatPeak + calcFinalZoneSizing.HeatDesHumRatDiff;
                    }

                    if (calcFinalZoneSizing.LatentHeatDDNum > 0) {
                        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(calcFinalZoneSizing.LatentHeatDDNum, zoneNum);
                        calcZoneSizing.DesHeatVolFlow = calcZoneSizing.DesLatentHeatVolFlow;
                        calcZoneSizing.DesHeatMassFlow = calcZoneSizing.DesLatentHeatMassFlow;
                        calcZoneSizing.DesHeatLoad = calcZoneSizing.DesLatentHeatLoad;
                        calcZoneSizing.HeatDesDay = calcZoneSizing.LatHeatDesDay;
                        calcZoneSizing.cHeatDDDate = calcFinalZoneSizing.cLatentHeatDDDate; // this has correct HeatDDDate
                        calcZoneSizing.HeatDDNum = calcZoneSizing.LatentHeatDDNum;
                        calcZoneSizing.TimeStepNumAtHeatMax = calcZoneSizing.TimeStepNumAtLatentHeatMax;
                        calcZoneSizing.HeatFlowSeq = calcZoneSizing.LatentHeatFlowSeq;
                        calcZoneSizing.DesHeatCoilInTemp = calcZoneSizing.DesLatentHeatCoilInTemp;
                        calcZoneSizing.DesHeatCoilInHumRat = calcZoneSizing.DesLatentHeatCoilInHumRat;
                        calcZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.ZoneRetTempAtLatentHeatPeak;
                        calcZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.ZoneTempAtLatentHeatPeak;
                        calcZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.ZoneHumRatAtLatentHeatPeak;
                        state.dataSize->HeatPeakDateHrMin(zoneNum) = state.dataSize->LatHeatPeakDateHrMin(zoneNum);

                        // the zone supply air humrat used for latent sizing is required to adequately size coil capacity
                        // not sure at this point if heating should reset HeatDesHumRat
                        if (calcZoneSizing.ZnLatHeatDgnSAMethod == SupplyAirHumidityRatio) {
                            calcZoneSizing.HeatDesHumRat = calcZoneSizing.LatentHeatDesHumRat;
                        } else {
                            calcZoneSizing.HeatDesHumRat = calcZoneSizing.ZoneHumRatAtLatentHeatPeak + calcZoneSizing.HeatDesHumRatDiff;
                        }
                    }
                }
            }
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
            z.CoolDesHumRat = c.CoolDesHumRat;

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
            z.CoolDesHumRat = c.CoolDesHumRat;

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
            for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                auto &zoneSizing = state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum);
                auto &calcZoneSizing = state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum);
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    zoneSizing.HeatFlowSeq(TimeStepIndex) = calcZoneSizing.HeatFlowSeq(TimeStepIndex);
                    zoneSizing.HeatLoadSeq(TimeStepIndex) = calcZoneSizing.HeatLoadSeq(TimeStepIndex);
                    zoneSizing.CoolFlowSeq(TimeStepIndex) = calcZoneSizing.CoolFlowSeq(TimeStepIndex);
                    zoneSizing.CoolLoadSeq(TimeStepIndex) = calcZoneSizing.CoolLoadSeq(TimeStepIndex);
                    zoneSizing.HeatZoneTempSeq(TimeStepIndex) = calcZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                    zoneSizing.HeatOutTempSeq(TimeStepIndex) = calcZoneSizing.HeatOutTempSeq(TimeStepIndex);
                    zoneSizing.HeatZoneRetTempSeq(TimeStepIndex) = calcZoneSizing.HeatZoneRetTempSeq(TimeStepIndex);
                    zoneSizing.HeatZoneHumRatSeq(TimeStepIndex) = calcZoneSizing.HeatZoneHumRatSeq(TimeStepIndex);
                    zoneSizing.HeatOutHumRatSeq(TimeStepIndex) = calcZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                    zoneSizing.CoolZoneTempSeq(TimeStepIndex) = calcZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                    zoneSizing.CoolOutTempSeq(TimeStepIndex) = calcZoneSizing.CoolOutTempSeq(TimeStepIndex);
                    zoneSizing.CoolZoneRetTempSeq(TimeStepIndex) = calcZoneSizing.CoolZoneRetTempSeq(TimeStepIndex);
                    zoneSizing.CoolZoneHumRatSeq(TimeStepIndex) = calcZoneSizing.CoolZoneHumRatSeq(TimeStepIndex);
                    zoneSizing.CoolOutHumRatSeq(TimeStepIndex) = calcZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
                }
            }
        }

        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            auto &finalZoneSizing = state.dataSize->FinalZoneSizing(CtrlZoneNum);
            auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                finalZoneSizing.HeatFlowSeq(TimeStepIndex) = calcFinalZoneSizing.HeatFlowSeq(TimeStepIndex);
                finalZoneSizing.HeatLoadSeq(TimeStepIndex) = calcFinalZoneSizing.HeatLoadSeq(TimeStepIndex);
                finalZoneSizing.CoolFlowSeq(TimeStepIndex) = calcFinalZoneSizing.CoolFlowSeq(TimeStepIndex);
                finalZoneSizing.CoolLoadSeq(TimeStepIndex) = calcFinalZoneSizing.CoolLoadSeq(TimeStepIndex);
                finalZoneSizing.HeatZoneTempSeq(TimeStepIndex) = calcFinalZoneSizing.HeatZoneTempSeq(TimeStepIndex);
                finalZoneSizing.HeatOutTempSeq(TimeStepIndex) = calcFinalZoneSizing.HeatOutTempSeq(TimeStepIndex);
                finalZoneSizing.HeatZoneRetTempSeq(TimeStepIndex) = calcFinalZoneSizing.HeatZoneRetTempSeq(TimeStepIndex);
                finalZoneSizing.HeatZoneHumRatSeq(TimeStepIndex) = calcFinalZoneSizing.HeatZoneHumRatSeq(TimeStepIndex);
                finalZoneSizing.HeatOutHumRatSeq(TimeStepIndex) = calcFinalZoneSizing.HeatOutHumRatSeq(TimeStepIndex);
                finalZoneSizing.CoolZoneTempSeq(TimeStepIndex) = calcFinalZoneSizing.CoolZoneTempSeq(TimeStepIndex);
                finalZoneSizing.CoolOutTempSeq(TimeStepIndex) = calcFinalZoneSizing.CoolOutTempSeq(TimeStepIndex);
                finalZoneSizing.CoolZoneRetTempSeq(TimeStepIndex) = calcFinalZoneSizing.CoolZoneRetTempSeq(TimeStepIndex);
                finalZoneSizing.CoolZoneHumRatSeq(TimeStepIndex) = calcFinalZoneSizing.CoolZoneHumRatSeq(TimeStepIndex);
                finalZoneSizing.CoolOutHumRatSeq(TimeStepIndex) = calcFinalZoneSizing.CoolOutHumRatSeq(TimeStepIndex);
            }
        }
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            auto &finalZoneSizing = state.dataSize->FinalZoneSizing(CtrlZoneNum);
            auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
            // update non air system design load and air flow to include the sizing factor
            finalZoneSizing.NonAirSysDesCoolLoad *= finalZoneSizing.CoolSizingFactor;
            finalZoneSizing.NonAirSysDesCoolVolFlow *= finalZoneSizing.CoolSizingFactor; // NonAirSysDesCoolVolFlow not currently used
            // Now take into account the user specified sizing factor and user specified cooling design air flow rate
            TotCoolSizMult = 0.0;
            // Calculate a sizing factor from the user specified cooling design air flow rate
            if (finalZoneSizing.InpDesCoolAirFlow > 0.0 && finalZoneSizing.CoolAirDesMethod == AirflowSizingMethod::InpDesAirFlow &&
                finalZoneSizing.DesCoolVolFlow > 0.0) {
                TotCoolSizMult = (finalZoneSizing.InpDesCoolAirFlow / finalZoneSizing.DesCoolVolFlow) * finalZoneSizing.CoolSizingFactor;
                // If no user specified cooling design air flow rate input, use the user specified szing factor
            } else {
                TotCoolSizMult = finalZoneSizing.CoolSizingFactor;
            }
            // If the cooling sizing multiplier is not 1, adjust the cooling design data
            if (std::abs(TotCoolSizMult - 1.0) > 0.00001) {
                if (finalZoneSizing.DesCoolVolFlow > 0.0) {
                    TimeStepAtPeak = finalZoneSizing.TimeStepNumAtCoolMax;
                    DDNum = finalZoneSizing.CoolDDNum;
                    finalZoneSizing.DesCoolVolFlow = calcFinalZoneSizing.DesCoolVolFlow * TotCoolSizMult;
                    finalZoneSizing.DesCoolMassFlow = calcFinalZoneSizing.DesCoolMassFlow * TotCoolSizMult;
                    finalZoneSizing.DesCoolLoad = calcFinalZoneSizing.DesCoolLoad * TotCoolSizMult;
                    finalZoneSizing.CoolFlowSeq = calcFinalZoneSizing.CoolFlowSeq * TotCoolSizMult;
                    finalZoneSizing.CoolLoadSeq = calcFinalZoneSizing.CoolLoadSeq * TotCoolSizMult;
                    OAFrac = finalZoneSizing.MinOA / finalZoneSizing.DesCoolVolFlow;
                    OAFrac = min(1.0, max(0.0, OAFrac));
                    finalZoneSizing.DesCoolCoilInTemp =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) + (1.0 - OAFrac) * finalZoneSizing.ZoneTempAtCoolPeak;
                    finalZoneSizing.DesCoolCoilInHumRat =
                        OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * finalZoneSizing.ZoneHumRatAtCoolPeak;
                } else {
                    finalZoneSizing.DesCoolVolFlow = finalZoneSizing.InpDesCoolAirFlow;
                    finalZoneSizing.DesCoolMassFlow = finalZoneSizing.DesCoolVolFlow * finalZoneSizing.DesCoolDens;
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    auto &zoneSizing = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum);
                    if (zoneSizing.DesCoolVolFlow > 0.0) {
                        TimeStepAtPeak = zoneSizing.TimeStepNumAtCoolMax;
                        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum);
                        auto &desDayWeath = state.dataSize->DesDayWeath(DDNum);
                        zoneSizing.DesCoolVolFlow = calcZoneSizing.DesCoolVolFlow * TotCoolSizMult;
                        zoneSizing.DesCoolMassFlow = calcZoneSizing.DesCoolMassFlow * TotCoolSizMult;
                        zoneSizing.DesCoolLoad = calcZoneSizing.DesCoolLoad * TotCoolSizMult;
                        zoneSizing.CoolFlowSeq = calcZoneSizing.CoolFlowSeq * TotCoolSizMult;
                        zoneSizing.CoolLoadSeq = calcZoneSizing.CoolLoadSeq * TotCoolSizMult;
                        OAFrac = zoneSizing.MinOA / zoneSizing.DesCoolVolFlow;
                        OAFrac = min(1.0, max(0.0, OAFrac));
                        zoneSizing.DesCoolCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizing.ZoneTempAtCoolPeak;
                        zoneSizing.DesCoolCoilInHumRat =
                            OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizing.ZoneHumRatAtCoolPeak;
                    } else {
                        zoneSizing.DesCoolVolFlow = zoneSizing.InpDesCoolAirFlow;
                        zoneSizing.DesCoolMassFlow = zoneSizing.DesCoolVolFlow * zoneSizing.DesCoolDens;
                    }
                    // Save cooling flows without MinOA for use later
                    zoneSizing.CoolFlowSeqNoOA = zoneSizing.CoolFlowSeq;
                    zoneSizing.DesCoolVolFlowNoOA = zoneSizing.DesCoolVolFlow;
                    zoneSizing.DesCoolMassFlowNoOA = zoneSizing.DesCoolMassFlow;
                }
            }
            // Save a set of design cooling air flow rates greater than or equal to the specified minimums without MinOA
            {
                Real64 MaxOfMinCoolVolFlowNoOA = 0.0; // max of the user specified design cooling minimum flows without min OA flow [m3/s]
                if (finalZoneSizing.CoolAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim) {
                    MaxOfMinCoolVolFlowNoOA = max(finalZoneSizing.DesCoolMinAirFlow, finalZoneSizing.DesCoolMinAirFlow2);
                }
                Real64 MaxOfMinCoolMassFlowNoOA =
                    MaxOfMinCoolVolFlowNoOA *
                    finalZoneSizing.DesCoolDens; // max of the user specified design cooling minimum flows without min OA flow [m3/s]
                finalZoneSizing.DesCoolVolFlowNoOA = finalZoneSizing.DesCoolVolFlow;
                finalZoneSizing.DesCoolMassFlowNoOA = finalZoneSizing.DesCoolMassFlow;
                if (MaxOfMinCoolVolFlowNoOA > finalZoneSizing.DesCoolVolFlowNoOA) {
                    finalZoneSizing.DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
                    finalZoneSizing.DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    finalZoneSizing.CoolFlowSeqNoOA(TimeStepIndex) = finalZoneSizing.CoolFlowSeq(TimeStepIndex);
                    if (MaxOfMinCoolMassFlowNoOA > finalZoneSizing.CoolFlowSeqNoOA(TimeStepIndex)) {
                        finalZoneSizing.CoolFlowSeqNoOA(TimeStepIndex) = MaxOfMinCoolMassFlowNoOA;
                    }
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    auto &zoneSizing = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum);
                    zoneSizing.DesCoolVolFlowNoOA = zoneSizing.DesCoolVolFlow;
                    zoneSizing.DesCoolMassFlowNoOA = zoneSizing.DesCoolMassFlow;
                    MaxOfMinCoolVolFlowNoOA = max(zoneSizing.DesCoolMinAirFlow, zoneSizing.DesCoolMinAirFlow);
                    MaxOfMinCoolMassFlowNoOA = MaxOfMinCoolVolFlowNoOA * zoneSizing.DesCoolDens;
                    if (MaxOfMinCoolVolFlowNoOA > zoneSizing.DesCoolVolFlow) {
                        zoneSizing.DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
                        zoneSizing.DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
                    }
                    for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                        zoneSizing.CoolFlowSeqNoOA(TimeStepIndex) = zoneSizing.CoolFlowSeq(TimeStepIndex);
                        if (MaxOfMinCoolMassFlowNoOA > zoneSizing.CoolFlowSeq(TimeStepIndex)) {
                            zoneSizing.CoolFlowSeqNoOA(TimeStepIndex) = MaxOfMinCoolMassFlowNoOA;
                        }
                    }
                }
            }

            // Now make sure that the design cooling air flow rates are greater than or equal to the specified minimums including MinOA
            {
                Real64 MaxOfMinCoolVolFlow = 0.0; // max of the user specified design cooling minimum flows and min OA flow [m3/s]
                if (finalZoneSizing.CoolAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim) {
                    MaxOfMinCoolVolFlow = max(finalZoneSizing.DesCoolMinAirFlow, finalZoneSizing.DesCoolMinAirFlow2, finalZoneSizing.MinOA);
                } else {
                    MaxOfMinCoolVolFlow = finalZoneSizing.MinOA;
                }
                Real64 MaxOfMinCoolMassFlow =
                    MaxOfMinCoolVolFlow *
                    finalZoneSizing.DesCoolDens; // max of the user specified design cooling minimum flows and min OA flow [kg/s]
                if (MaxOfMinCoolVolFlow > finalZoneSizing.DesCoolVolFlow) {
                    finalZoneSizing.DesCoolVolFlow = MaxOfMinCoolVolFlow;
                    finalZoneSizing.DesCoolMassFlow = MaxOfMinCoolMassFlow;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (MaxOfMinCoolMassFlow > finalZoneSizing.CoolFlowSeq(TimeStepIndex)) {
                        finalZoneSizing.CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
                    }
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    auto &zoneSizing = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum);
                    MaxOfMinCoolVolFlow = max(zoneSizing.DesCoolMinAirFlow, zoneSizing.DesCoolMinAirFlow, zoneSizing.MinOA);
                    MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * zoneSizing.DesCoolDens;
                    if (MaxOfMinCoolVolFlow > zoneSizing.DesCoolVolFlow) {
                        zoneSizing.DesCoolVolFlow = MaxOfMinCoolVolFlow;
                        zoneSizing.DesCoolMassFlow = MaxOfMinCoolMassFlow;
                    }
                    for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                        if (MaxOfMinCoolMassFlow > zoneSizing.CoolFlowSeq(TimeStepIndex)) {
                            zoneSizing.CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
                        }
                    }
                }
            }
            // IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
            if (finalZoneSizing.DesCoolLoad == 0) {
                // Check CoolDDNum and TimeStepNumAtCoolMax value and default to 1 if not set, carried over from previous code
                if (calcFinalZoneSizing.CoolDDNum == 0) {
                    calcFinalZoneSizing.CoolDDNum = 1;
                }
                if (calcFinalZoneSizing.TimeStepNumAtCoolMax == 0) {
                    calcFinalZoneSizing.TimeStepNumAtCoolMax = 1;
                }
                finalZoneSizing.TimeStepNumAtCoolMax = calcFinalZoneSizing.TimeStepNumAtCoolMax;
                finalZoneSizing.CoolDDNum = calcFinalZoneSizing.CoolDDNum;
                finalZoneSizing.CoolDesDay = calcFinalZoneSizing.CoolDesDay;
                DDNumF = finalZoneSizing.CoolDDNum;
                auto &zoneSizingF = state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum);
                TimeStepAtPeakF = finalZoneSizing.TimeStepNumAtCoolMax;

                // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
                // issue 6006, heating coils sizing to 0 when no heating load in zone
                if (zoneSizingF.DesCoolSetPtSeq.empty()) {
                    ShowSevereError(state,
                                    format("{}:  Thermostat cooling set point temperatures are not initialized for Zone = {}",
                                           RoutineName,
                                           finalZoneSizing.ZoneName));
                    ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
                } else {
                    finalZoneSizing.ZoneTempAtCoolPeak = *std::min_element(zoneSizingF.DesCoolSetPtSeq.begin(), zoneSizingF.DesCoolSetPtSeq.end());
                }
                finalZoneSizing.OutTempAtCoolPeak = *std::min_element(zoneSizingF.CoolOutTempSeq.begin(), zoneSizingF.CoolOutTempSeq.end());
                finalZoneSizing.OutHumRatAtCoolPeak = zoneSizingF.CoolOutHumRatSeq(TimeStepAtPeakF);
                finalZoneSizing.ZoneHumRatAtCoolPeak = zoneSizingF.CoolDesHumRat;
                calcFinalZoneSizing.ZoneTempAtCoolPeak = zoneSizingF.CoolZoneTempSeq(TimeStepAtPeakF);
                calcFinalZoneSizing.ZoneHumRatAtCoolPeak = zoneSizingF.CoolZoneHumRatSeq(TimeStepAtPeakF);
                calcFinalZoneSizing.ZoneRetTempAtCoolPeak = calcFinalZoneSizing.ZoneTempAtCoolPeak;
                finalZoneSizing.DesCoolCoilInTemp = finalZoneSizing.ZoneTempAtCoolPeak;
                finalZoneSizing.DesCoolCoilInHumRat = finalZoneSizing.ZoneHumRatAtCoolPeak;
                finalZoneSizing.ZoneRetTempAtCoolPeak = finalZoneSizing.ZoneTempAtCoolPeak;
            }
            // update non air system design load and air flow to include the sizing factor
            finalZoneSizing.NonAirSysDesHeatLoad *= finalZoneSizing.HeatSizingFactor;
            finalZoneSizing.NonAirSysDesHeatVolFlow *= finalZoneSizing.HeatSizingFactor;
            // Now take into account the user specified sizing factor or user specified heating design air flow rate (which overrides the
            // sizing factor)
            TotHeatSizMult = 0.0;
            // Calculate a sizing factor from the user specified heating design air flow rate
            if (finalZoneSizing.InpDesHeatAirFlow > 0.0 && finalZoneSizing.HeatAirDesMethod == AirflowSizingMethod::InpDesAirFlow &&
                finalZoneSizing.DesHeatVolFlow > 0.0) {
                TotHeatSizMult = (finalZoneSizing.InpDesHeatAirFlow / finalZoneSizing.DesHeatVolFlow) * finalZoneSizing.HeatSizingFactor;
                // Calculate a sizing factor from the user specified max heating design air flow rates
            } else if (finalZoneSizing.HeatAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim && finalZoneSizing.DesHeatVolFlow > 0.0) {
                MaxHeatVolFlow = max(finalZoneSizing.DesHeatMaxAirFlow,
                                     finalZoneSizing.DesHeatMaxAirFlow2,
                                     finalZoneSizing.DesCoolVolFlow * finalZoneSizing.DesHeatMaxAirFlowFrac);
                if (MaxHeatVolFlow < finalZoneSizing.DesHeatVolFlow) {
                    TotHeatSizMult = (MaxHeatVolFlow / finalZoneSizing.DesHeatVolFlow) * finalZoneSizing.HeatSizingFactor;
                } else {
                    TotHeatSizMult = finalZoneSizing.HeatSizingFactor;
                }
                // If no user specified heating design air flow rate input, use the user specified sizing factor
            } else {
                TotHeatSizMult = finalZoneSizing.HeatSizingFactor;
            }

            if (std::abs(TotHeatSizMult - 1.0) > 0.00001) {
                if (finalZoneSizing.DesHeatVolFlow > 0.0) {
                    auto &desDayWeath = state.dataSize->DesDayWeath(finalZoneSizing.HeatDDNum);
                    finalZoneSizing.DesHeatVolFlow = calcFinalZoneSizing.DesHeatVolFlow * TotHeatSizMult;
                    finalZoneSizing.DesHeatMassFlow = calcFinalZoneSizing.DesHeatMassFlow * TotHeatSizMult;
                    finalZoneSizing.DesHeatLoad = calcFinalZoneSizing.DesHeatLoad * TotHeatSizMult;
                    finalZoneSizing.HeatFlowSeq = calcFinalZoneSizing.HeatFlowSeq * TotHeatSizMult;
                    finalZoneSizing.HeatLoadSeq = calcFinalZoneSizing.HeatLoadSeq * TotHeatSizMult;
                    OAFrac = finalZoneSizing.MinOA / finalZoneSizing.DesHeatVolFlow;
                    OAFrac = min(1.0, max(0.0, OAFrac));
                    finalZoneSizing.DesHeatCoilInTemp =
                        OAFrac * desDayWeath.Temp(finalZoneSizing.TimeStepNumAtHeatMax) + (1.0 - OAFrac) * finalZoneSizing.ZoneTempAtHeatPeak;
                    finalZoneSizing.DesHeatCoilInHumRat =
                        OAFrac * desDayWeath.HumRat(finalZoneSizing.TimeStepNumAtHeatMax) + (1.0 - OAFrac) * finalZoneSizing.ZoneHumRatAtHeatPeak;
                } else {
                    finalZoneSizing.DesHeatVolFlow = finalZoneSizing.InpDesHeatAirFlow;
                    finalZoneSizing.DesHeatMassFlow = finalZoneSizing.DesHeatVolFlow * finalZoneSizing.DesHeatDens;
                }
                for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                    auto &zoneSizingDD = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum);
                    if (zoneSizingDD.DesHeatVolFlow > 0.0) {
                        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(DDNum, CtrlZoneNum);
                        TimeStepAtPeak = zoneSizingDD.TimeStepNumAtHeatMax;
                        zoneSizingDD.DesHeatVolFlow = calcZoneSizing.DesHeatVolFlow * TotHeatSizMult;
                        zoneSizingDD.DesHeatMassFlow = calcZoneSizing.DesHeatMassFlow * TotHeatSizMult;
                        zoneSizingDD.DesHeatLoad = calcZoneSizing.DesHeatLoad * TotHeatSizMult;
                        zoneSizingDD.HeatFlowSeq = calcZoneSizing.HeatFlowSeq * TotHeatSizMult;
                        zoneSizingDD.HeatLoadSeq = calcZoneSizing.HeatLoadSeq * TotHeatSizMult;
                        OAFrac = zoneSizingDD.MinOA / zoneSizingDD.DesHeatVolFlow;
                        OAFrac = min(1.0, max(0.0, OAFrac));
                        zoneSizingDD.DesHeatCoilInTemp =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizingDD.ZoneTempAtHeatPeak;
                        zoneSizingDD.DesHeatCoilInHumRat =
                            OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizingDD.ZoneHumRatAtHeatPeak;
                    } else {
                        zoneSizingDD.DesHeatVolFlow = zoneSizingDD.InpDesHeatAirFlow;
                        zoneSizingDD.DesHeatMassFlow = zoneSizingDD.DesHeatVolFlow * zoneSizingDD.DesHeatDens;
                    }
                    // Save heating flows without MinOA for use later
                    zoneSizingDD.HeatFlowSeqNoOA = zoneSizingDD.HeatFlowSeq;
                    zoneSizingDD.DesHeatVolFlowNoOA = zoneSizingDD.DesHeatVolFlow;
                    zoneSizingDD.DesHeatMassFlowNoOA = zoneSizingDD.DesHeatMassFlow;
                }
            }

            // Save a set of design heating air flow rates before the MinOA adjustment
            // just in FinalZoneSizing to use for TermUnit sizing adjustments in SizingManager::UpdateTermUnitFinalZoneSizing
            finalZoneSizing.DesHeatVolFlowNoOA = finalZoneSizing.DesHeatVolFlow;
            finalZoneSizing.DesHeatMassFlowNoOA = finalZoneSizing.DesHeatMassFlow;
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                finalZoneSizing.HeatFlowSeqNoOA(TimeStepIndex) = finalZoneSizing.HeatFlowSeq(TimeStepIndex);
            }

            // Now make sure that the design heating air flow rates are greater than or equal to MinOA
            MinOAMass = finalZoneSizing.MinOA * finalZoneSizing.DesHeatDens;
            if (finalZoneSizing.MinOA > finalZoneSizing.DesHeatVolFlow) {
                finalZoneSizing.DesHeatVolFlow = finalZoneSizing.MinOA;
                finalZoneSizing.DesHeatMassFlow = MinOAMass;
            }
            for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (MinOAMass > finalZoneSizing.HeatFlowSeq(TimeStepIndex)) {
                    finalZoneSizing.HeatFlowSeq(TimeStepIndex) = MinOAMass;
                }
            }
            for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
                auto &zoneSizingDD = state.dataSize->ZoneSizing(DDNum, CtrlZoneNum);
                MinOAMass = zoneSizingDD.MinOA * zoneSizingDD.DesHeatDens;
                if (zoneSizingDD.MinOA > zoneSizingDD.DesHeatVolFlow) {
                    zoneSizingDD.DesHeatVolFlow = zoneSizingDD.MinOA;
                    zoneSizingDD.DesHeatMassFlow = MinOAMass;
                }
                for (TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                    if (MinOAMass > zoneSizingDD.HeatFlowSeq(TimeStepIndex)) {
                        zoneSizingDD.HeatFlowSeq(TimeStepIndex) = MinOAMass;
                    }
                }
            }
            // IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data
            if (finalZoneSizing.DesHeatLoad == 0) {
                // Check HDDNum and TimeStepNumAtHeatMax value and default to 1 if not set, carried over from previous code
                if (calcFinalZoneSizing.HeatDDNum == 0) {
                    calcFinalZoneSizing.HeatDDNum = 1;
                }
                if (calcFinalZoneSizing.TimeStepNumAtHeatMax == 0) {
                    calcFinalZoneSizing.TimeStepNumAtHeatMax = 1;
                }
                finalZoneSizing.TimeStepNumAtHeatMax = calcFinalZoneSizing.TimeStepNumAtHeatMax;
                finalZoneSizing.HeatDDNum = calcFinalZoneSizing.HeatDDNum;
                finalZoneSizing.HeatDesDay = calcFinalZoneSizing.HeatDesDay;
                DDNumF = finalZoneSizing.HeatDDNum;
                auto &zoneSizingDDF = state.dataSize->ZoneSizing(DDNumF, CtrlZoneNum);
                TimeStepAtPeakF = finalZoneSizing.TimeStepNumAtHeatMax;

                // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
                // issue 6006, heating coils sizing to 0 when no heating load in zone
                if (zoneSizingDDF.DesHeatSetPtSeq.empty()) {
                    ShowSevereError(state,
                                    format("{}:  Thermostat heating set point temperatures not initialized for Zone = {}",
                                           RoutineName,
                                           finalZoneSizing.ZoneName));
                    ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
                } else {
                    finalZoneSizing.ZoneTempAtHeatPeak =
                        *std::max_element(zoneSizingDDF.DesHeatSetPtSeq.begin(), zoneSizingDDF.DesHeatSetPtSeq.end());
                }
                finalZoneSizing.OutTempAtHeatPeak = *std::min_element(zoneSizingDDF.HeatOutTempSeq.begin(), zoneSizingDDF.HeatOutTempSeq.end());
                finalZoneSizing.OutHumRatAtHeatPeak = zoneSizingDDF.HeatOutHumRatSeq(TimeStepAtPeakF);
                finalZoneSizing.ZoneHumRatAtHeatPeak = zoneSizingDDF.HeatDesHumRat;
                calcFinalZoneSizing.ZoneTempAtHeatPeak = zoneSizingDDF.HeatZoneTempSeq(TimeStepAtPeakF);
                calcFinalZoneSizing.ZoneHumRatAtHeatPeak = zoneSizingDDF.HeatZoneHumRatSeq(TimeStepAtPeakF);
                calcFinalZoneSizing.ZoneRetTempAtHeatPeak = calcFinalZoneSizing.ZoneTempAtHeatPeak;
                finalZoneSizing.DesHeatCoilInTemp = finalZoneSizing.ZoneTempAtHeatPeak;
                finalZoneSizing.DesHeatCoilInHumRat = finalZoneSizing.ZoneHumRatAtHeatPeak;
                finalZoneSizing.ZoneRetTempAtHeatPeak = finalZoneSizing.ZoneTempAtHeatPeak;
            }

            // set the zone minimum cooling supply air flow rate. This will be used for autosizing VAV terminal unit
            // minimum flow rates (comment seems incorrect, really used as a minimum lower limit for the maximum air flow)
            finalZoneSizing.DesCoolVolFlowMin = max(finalZoneSizing.DesCoolMinAirFlow,
                                                    finalZoneSizing.DesCoolMinAirFlow2,
                                                    finalZoneSizing.DesCoolVolFlow * finalZoneSizing.DesCoolMinAirFlowFrac);
            // set the zone maximum heating supply air flow rate. This will be used for autosizing VAV terminal unit
            // max heating flow rates
            finalZoneSizing.DesHeatVolFlowMax =
                max(finalZoneSizing.DesHeatMaxAirFlow,
                    finalZoneSizing.DesHeatMaxAirFlow2,
                    max(finalZoneSizing.DesCoolVolFlow, finalZoneSizing.DesHeatVolFlow) * finalZoneSizing.DesHeatMaxAirFlowFrac);
            // Determine the design cooling supply air temperature if the supply air temperature difference is specified by user.
            if (finalZoneSizing.ZnCoolDgnSAMethod == TemperatureDifference) {
                finalZoneSizing.CoolDesTemp = finalZoneSizing.ZoneTempAtCoolPeak - std::abs(finalZoneSizing.CoolDesTempDiff);
            }
            // Determine the design heating supply air temperature if the supply air temperature difference is specified by user.
            if (finalZoneSizing.ZnHeatDgnSAMethod == TemperatureDifference) {
                finalZoneSizing.HeatDesTemp = finalZoneSizing.ZoneTempAtHeatPeak + std::abs(finalZoneSizing.HeatDesTempDiff);
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

    bool SupPathInletChanged(false);
    Real64 SysOutputProvided; // sensible output delivered by zone equipment (W)
    Real64 LatOutputProvided; // latent output delivered by zone equipment (kg/s)
    Real64 AirSysOutput;
    Real64 NonAirSysOutput;

    // Determine flow rate and temperature of supply air based on type of damper

    bool FirstCall = true; // indicates first call to supply air path components
    bool ErrorFlag = false;

    for (int SupplyAirPathNum = 1; SupplyAirPathNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SupplyAirPathNum) {

        for (int CompNum = 1; CompNum <= state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).NumOfComponents; ++CompNum) {

            switch (state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentTypeEnum(CompNum)) {
            case DataZoneEquipment::AirLoopHVACZone::Splitter: { // 'AirLoopHVAC:ZoneSplitter'

                if (!(state.afn->AirflowNetworkFanActivated && state.afn->distribution_simulated)) {
                    SplitterComponent::SimAirLoopSplitter(state,
                                                          state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                                          FirstHVACIteration,
                                                          FirstCall,
                                                          SupPathInletChanged,
                                                          state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum));
                }

                break;
            }
            case DataZoneEquipment::AirLoopHVACZone::SupplyPlenum: { // 'AirLoopHVAC:SupplyPlenum'

                ZonePlenum::SimAirZonePlenum(state,
                                             state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                             DataZoneEquipment::AirLoopHVACZone::SupplyPlenum,
                                             state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum),
                                             FirstHVACIteration,
                                             FirstCall,
                                             SupPathInletChanged);

                break;
            }
            default: {
                ShowSevereError(state, format("Error found in Supply Air Path={}", state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).Name));
                ShowContinueError(
                    state,
                    format("Invalid Supply Air Path Component={}", state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentType(CompNum)));
                ShowFatalError(state, "Preceding condition causes termination.");

                break;
            }
            }
        }
    }

    FirstCall = false;

    // Simulate all of the pools. These have a potential impact on surface heat balances, zone air heat balances, and moisture balances.
    // These should be simulated first so that any systems or zone equipment devices deal with the effects of the pool properly.
    SwimmingPool::SimSwimmingPool(state, FirstHVACIteration);

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

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {

        if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ControlledZoneNum);

        thisZoneHB.NonAirSystemResponse = 0.0;
        thisZoneHB.SysDepZoneLoads = 0.0;
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        zoneEquipConfig.ZoneExh = 0.0;
        zoneEquipConfig.ZoneExhBalanced = 0.0;
        zoneEquipConfig.PlenumMassFlow = 0.0;
        state.dataSize->CurZoneEqNum = ControlledZoneNum;

        InitSystemOutputRequired(state, ControlledZoneNum, FirstHVACIteration, true);

        auto &TurnFansOn = state.dataHVACGlobal->TurnFansOn;
        auto &TurnFansOff = state.dataHVACGlobal->TurnFansOff;
        auto &TurnZoneFansOnlyOn = state.dataHVACGlobal->TurnZoneFansOnlyOn;

        // Air loop system availability manager status only applies to PIU and exhaust fans
        // Reset fan SAM operation flags for zone fans.
        TurnFansOn = false;
        TurnZoneFansOnlyOn = false;
        TurnFansOff = false;

        for (int EquipTypeNum = 1; EquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).NumOfEquipTypes; ++EquipTypeNum) {

            state.dataHVACGlobal->UnbalExhMassFlow = 0.0;
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
            state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
            const int EquipPtr = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipPtr;
            SysOutputProvided = 0.0;
            LatOutputProvided = 0.0;
            state.dataSize->DataCoolCoilCap = 0.0; // reset global variable used only for heat pumps (i.e., DX cooling and heating coils)

            // Reset ZoneEqSizing data (because these may change from one equipment type to the next)
            if (state.dataZoneEquipmentManager->FirstPassZoneEquipFlag) {
                auto &zoneEqSizing = state.dataSize->ZoneEqSizing(ControlledZoneNum);

                zoneEqSizing.AirVolFlow = 0.0;
                zoneEqSizing.MaxHWVolFlow = 0.0;
                zoneEqSizing.MaxCWVolFlow = 0.0;
                zoneEqSizing.OAVolFlow = 0.0;
                zoneEqSizing.DesCoolingLoad = 0.0;
                zoneEqSizing.DesHeatingLoad = 0.0;
                zoneEqSizing.CoolingAirVolFlow = 0.0;
                zoneEqSizing.HeatingAirVolFlow = 0.0;
                zoneEqSizing.SystemAirVolFlow = 0.0;
                zoneEqSizing.AirFlow = false;
                zoneEqSizing.CoolingAirFlow = false;
                zoneEqSizing.HeatingAirFlow = false;
                zoneEqSizing.SystemAirFlow = false;
                zoneEqSizing.Capacity = false;
                zoneEqSizing.CoolingCapacity = false;
                zoneEqSizing.HeatingCapacity = false;
                zoneEqSizing.SystemCapacity = false;
                zoneEqSizing.DesignSizeFromParent = false;
            }

            const int ZoneEquipTypeNum = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipTypeEnum;

            auto &zoneEquipList = state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum);

            const int ZoneCompNum = zoneEquipList.EquipIndex(EquipPtr);

            bool ValidSAMComp = false;

            if (ZoneEquipTypeNum <= NumValidSysAvailZoneComponents) ValidSAMComp = true;

            auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

            if (ZoneCompNum > 0 && ValidSAMComp) {

                SystemAvailabilityManager::GetZoneEqAvailabilityManager(state, ZoneEquipTypeNum, ZoneCompNum, ErrorFlag);

                if (ZoneComp(ZoneEquipTypeNum).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == DataHVACGlobals::CycleOn) {
                    state.dataHVACGlobal->ZoneCompTurnFansOn = true;
                    state.dataHVACGlobal->ZoneCompTurnFansOff = false;
                } else if (ZoneComp(ZoneEquipTypeNum).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == DataHVACGlobals::ForceOff) {
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
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) {
                    TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) {
                    // Currently used only by parallel powered induction unit
                    TurnZoneFansOnlyOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::ForceOff) {
                    TurnFansOff = true;
                }

                ZoneAirLoopEquipmentManager::ManageZoneAirLoopEquipment(state,
                                                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                        FirstHVACIteration,
                                                                        AirSysOutput,
                                                                        NonAirSysOutput,
                                                                        LatOutputProvided,
                                                                        ControlledZoneNum,
                                                                        zoneEquipList.EquipIndex(EquipPtr));

                //            reset status flags for other zone equipment
                TurnFansOn = false;
                TurnZoneFansOnlyOn = false;
                TurnFansOff = false;

                thisZoneHB.NonAirSystemResponse += NonAirSysOutput;
                SysOutputProvided = NonAirSysOutput + AirSysOutput;
            } break;
            case ZoneEquip::VRFTerminalUnit: { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
                bool HeatingActive = false;
                bool CoolingActive = false;
                int constexpr OAUnitNum = 0;
                Real64 constexpr OAUCoilOutTemp = 0.0;
                bool constexpr ZoneEquipment = true;
                HVACVariableRefrigerantFlow::SimulateVRF(state,
                                                         state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                         FirstHVACIteration,
                                                         ControlledZoneNum,
                                                         zoneEquipList.EquipIndex(EquipPtr),
                                                         HeatingActive,
                                                         CoolingActive,
                                                         OAUnitNum,
                                                         OAUCoilOutTemp,
                                                         ZoneEquipment,
                                                         SysOutputProvided,
                                                         LatOutputProvided);
            } break;
            case ZoneEquip::WindowAC: { // 'ZoneHVAC:WindowAirConditioner'
                WindowAC::SimWindowAC(state,
                                      state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                      ControlledZoneNum,
                                      FirstHVACIteration,
                                      SysOutputProvided,
                                      LatOutputProvided,
                                      zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::PkgTermHPAirToAir:   // 'ZoneHVAC:PackagedTerminalHeatPump'
            case ZoneEquip::PkgTermACAirToAir:   // 'ZoneHVAC:PackagedTerminalAirConditioner'
            case ZoneEquip::PkgTermHPWaterToAir: // 'ZoneHVAC:WaterToAirHeatPump'
            case ZoneEquip::ZoneUnitarySys: {    // 'AirloopHVAC:UnitarySystem'
                int AirLoopNum = 0;
                bool HeatingActive = false;
                bool CoolingActive = false;
                int OAUnitNum = 0;
                Real64 OAUCoilOutTemp = 0.0;
                bool ZoneEquipFlag = true;
                zoneEquipList.compPointer[EquipPtr]->simulate(state,
                                                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                              FirstHVACIteration,
                                                              AirLoopNum,
                                                              zoneEquipList.EquipIndex(EquipPtr),
                                                              HeatingActive,
                                                              CoolingActive,
                                                              OAUnitNum,
                                                              OAUCoilOutTemp,
                                                              ZoneEquipFlag,
                                                              SysOutputProvided,
                                                              LatOutputProvided);
            } break;
            case ZoneEquip::ZoneDXDehumidifier: { // 'ZoneHVAC:Dehumidifier:DX'
                ZoneDehumidifier::SimZoneDehumidifier(state,
                                                      state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                      ControlledZoneNum,
                                                      FirstHVACIteration,
                                                      SysOutputProvided,
                                                      LatOutputProvided,
                                                      zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.SysDepZoneLoads += SysOutputProvided;

                SysOutputProvided = 0.0; // Reset to 0.0 since this equipment is controlled based on zone humidity level (not
                                         // temperature) SysOutputProvided amount was already sent above to
                                         // next Predict-Correct series of calcs via SysDepZoneLoads
            } break;
            case ZoneEquip::FanCoil4Pipe: { // 'ZoneHVAC:FourPipeFanCoil'
                FanCoilUnits::SimFanCoilUnit(state,
                                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                             ControlledZoneNum,
                                             FirstHVACIteration,
                                             SysOutputProvided,
                                             LatOutputProvided,
                                             zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::UnitVentilator: { // 'ZoneHVAC:UnitVentilator'
                UnitVentilator::SimUnitVentilator(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::UnitHeater: { // 'ZoneHVAC:UnitHeater'
                UnitHeater::SimUnitHeater(state,
                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                          ControlledZoneNum,
                                          FirstHVACIteration,
                                          SysOutputProvided,
                                          LatOutputProvided,
                                          zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::PurchasedAir: { // 'ZoneHVAC:IdealLoadsAirSystem'
                PurchasedAirManager::SimPurchasedAir(state,
                                                     state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                     SysOutputProvided,
                                                     LatOutputProvided,
                                                     FirstHVACIteration,
                                                     ControlledZoneNum,
                                                     zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::BBWater: { // 'ZoneHVAC:Baseboard:RadiantConvective:Water'
                HWBaseboardRadiator::SimHWBaseboard(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBSteam: { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
                SteamBaseboardRadiator::SimSteamBaseboard(state,
                                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                          ControlledZoneNum,
                                                          FirstHVACIteration,
                                                          SysOutputProvided,
                                                          zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBWaterConvective: { // 'ZoneHVAC:Baseboard:Convective:Water'
                BaseboardRadiator::SimBaseboard(state,
                                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                ControlledZoneNum,
                                                FirstHVACIteration,
                                                SysOutputProvided,
                                                zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::BBElectricConvective: { // 'ZoneHVAC:Baseboard:Convective:Electric'
                BaseboardElectric::SimElectricBaseboard(state,
                                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                        ControlledZoneNum,
                                                        SysOutputProvided,
                                                        zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::CoolingPanel: { // 'ZoneHVAC:CoolingPanel:RadiantConvective:Water'
                CoolingPanelSimple::SimCoolingPanel(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This cooling panel does not add/remove any latent heat
            } break;
            case ZoneEquip::HiTempRadiant: { // 'ZoneHVAC:HighTemperatureRadiant'
                HighTempRadiantSystem::SimHighTempRadiantSystem(state,
                                                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                FirstHVACIteration,
                                                                SysOutputProvided,
                                                                zoneEquipList.EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard currently sends its latent heat gain directly to predictor/corrector
                                         // via SumLatentHTRadSys... so setting LatOutputProvided = 0.0
            } break;
            case ZoneEquip::LoTempRadiant: { // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow',
                                             // 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                // 'ZoneHVAC:LowTemperatureRadiant:Electric'
                LowTempRadiantSystem::SimLowTempRadiantSystem(state,
                                                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                              FirstHVACIteration,
                                                              SysOutputProvided,
                                                              zoneEquipList.EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::ZoneExhaustFan: { // 'Fan:ZoneExhaust'
                // Air loop system availability manager status only applies to PIU and exhaust fans
                // Check to see if System Availability Managers are asking for fans to cycle on or shut off
                // and set fan on/off flags accordingly.
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) {
                    TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::ForceOff) {
                    TurnFansOff = true;
                }

                Fans::SimulateFanComponents(state,
                                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                            FirstHVACIteration,
                                            zoneEquipList.EquipIndex(EquipPtr));

                //            reset status flags for other zone equipment
                TurnFansOn = false;
                TurnFansOff = false;
            } break;
            case ZoneEquip::HeatXchngr: { // 'HeatExchanger:AirToAir:FlatPlate'
                HeatRecovery::SimHeatRecovery(state,
                                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                              FirstHVACIteration,
                                              zoneEquipList.EquipIndex(EquipPtr),
                                              DataHVACGlobals::ContFanCycCoil);
            } break;
            case ZoneEquip::ERVStandAlone: { // 'ZoneHVAC:EnergyRecoveryVentilator'
                HVACStandAloneERV::SimStandAloneERV(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    LatOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::HPWaterHeater: { // 'WaterHeater:HeatPump:PumpedCondenser'
                WaterThermalTanks::SimHeatPumpWaterHeater(state,
                                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                          FirstHVACIteration,
                                                          SysOutputProvided,
                                                          LatOutputProvided,
                                                          state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::VentilatedSlab: { // 'ZoneHVAC:VentilatedSlab'
                VentilatedSlab::SimVentilatedSlab(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::OutdoorAirUnit: { // 'ZoneHVAC:OutdoorAirUnit'
                OutdoorAirUnit::SimOutdoorAirUnit(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::BBElectric: { // 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
                ElectricBaseboardRadiator::SimElecBaseboard(state,
                                                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                            ControlledZoneNum,
                                                            FirstHVACIteration,
                                                            SysOutputProvided,
                                                            zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;
            case ZoneEquip::RefrigerationAirChillerSet: { // 'ZoneHVAC:RefrigerationChillerSet'
                RefrigeratedCase::SimAirChillerSet(state,
                                                   state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                   ControlledZoneNum,
                                                   FirstHVACIteration,
                                                   SysOutputProvided,
                                                   LatOutputProvided,
                                                   zoneEquipList.EquipIndex(EquipPtr));

                thisZoneHB.NonAirSystemResponse += SysOutputProvided;
            } break;
            case ZoneEquip::UserDefinedZoneHVACForcedAir: {
                UserDefinedComponents::SimZoneAirUserDefined(state,
                                                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                             ControlledZoneNum,
                                                             SysOutputProvided,
                                                             LatOutputProvided,
                                                             zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::ZoneEvaporativeCoolerUnit: {
                EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(state,
                                                                 state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                 ControlledZoneNum,
                                                                 SysOutputProvided,
                                                                 LatOutputProvided,
                                                                 zoneEquipList.EquipIndex(EquipPtr));
            } break;
            case ZoneEquip::ZoneHybridEvaporativeCooler: {
                HybridUnitaryAirConditioners::SimZoneHybridUnitaryAirConditioners(
                    state,
                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                    ControlledZoneNum,
                    SysOutputProvided,
                    LatOutputProvided,
                    zoneEquipList.EquipIndex(EquipPtr));
            } break;
            default:
                break;
            }

            zoneEquipConfig.ZoneExh +=
                (state.dataHVACGlobal->UnbalExhMassFlow +
                 state.dataHVACGlobal->BalancedExhMassFlow); // This is the total "exhaust" flow from equipment such as a zone exhaust fan
            zoneEquipConfig.ZoneExhBalanced += state.dataHVACGlobal->BalancedExhMassFlow;
            zoneEquipConfig.PlenumMassFlow += state.dataHVACGlobal->PlenumInducedMassFlow;

            // Store available capacities for load distribution calculations
            if (FirstHVACIteration && (zoneEquipList.LoadDistScheme != DataZoneEquipment::LoadDist::Sequential)) {
                if (SysOutputProvided > 0.0) {
                    zoneEquipList.HeatingCapacity(EquipPtr) = SysOutputProvided;
                } else {
                    zoneEquipList.CoolingCapacity(EquipPtr) = SysOutputProvided;
                }
            }

            UpdateSystemOutputRequired(state, ControlledZoneNum, SysOutputProvided, LatOutputProvided, EquipTypeNum);
            state.dataSize->CurTermUnitSizingNum = 0;
        } // zone equipment loop
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                // SpaceHB ToDo: For now allocate by space volume frac
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).NonAirSystemResponse =
                    thisZoneHB.NonAirSystemResponse * state.dataHeatBal->space(spaceNum).fracZoneVolume;
            }
        }
    } // End of controlled zone loop
    state.dataSize->CurZoneEqNum = 0;
    state.dataZoneEquipmentManager->FirstPassZoneEquipFlag = false;

    // This is the call to the Supply Air Path after the components are simulated to update
    //  the path inlets

    // Process supply air path components in reverse order
    for (int SupplyAirPathNum = 1; SupplyAirPathNum <= state.dataZoneEquip->NumSupplyAirPaths; ++SupplyAirPathNum) {

        SupPathInletChanged = false;

        for (int CompNum = state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).NumOfComponents; CompNum >= 1; --CompNum) {
            switch (state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentTypeEnum(CompNum)) {
            case DataZoneEquipment::AirLoopHVACZone::Splitter: { // 'AirLoopHVAC:ZoneSplitter'
                if (!(state.afn->AirflowNetworkFanActivated && state.afn->distribution_simulated)) {
                    SplitterComponent::SimAirLoopSplitter(state,
                                                          state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                                          FirstHVACIteration,
                                                          FirstCall,
                                                          SupPathInletChanged,
                                                          state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum));
                }
            } break;
            case DataZoneEquipment::AirLoopHVACZone::SupplyPlenum: { // 'AirLoopHVAC:SupplyPlenum'
                ZonePlenum::SimAirZonePlenum(state,
                                             state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentName(CompNum),
                                             DataZoneEquipment::AirLoopHVACZone::SupplyPlenum,
                                             state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentIndex(CompNum),
                                             FirstHVACIteration,
                                             FirstCall,
                                             SupPathInletChanged);

            } break;
            default: {
                ShowSevereError(state, format("Error found in Supply Air Path={}", state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).Name));
                ShowContinueError(
                    state,
                    format("Invalid Supply Air Path Component={}", state.dataZoneEquip->SupplyAirPath(SupplyAirPathNum).ComponentType(CompNum)));
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

    ReturnAirPathManager::SimReturnAirPath(state);
}

void SetZoneEquipSimOrder(EnergyPlusData &state, int const ControlledZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997

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
                 state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).RemainingOutputRequired < 0.0) ||
                (CurEqHeatingPriority > psc.HeatingPriority &&
                 state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum).RemainingOutputRequired >= 0.0)) {

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
    initOutputRequired(state,
                       ZoneNum,
                       state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum),
                       state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum),
                       FirstHVACIteration,
                       ResetSimOrder);
    // SpaceHB TODO: This may need more work
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            initOutputRequired(state,
                               ZoneNum,
                               state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum),
                               state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum),
                               FirstHVACIteration,
                               ResetSimOrder,
                               spaceNum);
        }
    }

    DistributeSystemOutputRequired(state, ZoneNum, FirstHVACIteration);
}

void initOutputRequired(EnergyPlusData &state,
                        int const ZoneNum,
                        DataZoneEnergyDemands::ZoneSystemSensibleDemand &energy,
                        DataZoneEnergyDemands::ZoneSystemMoistureDemand &moisture,
                        bool const FirstHVACIteration,
                        bool const ResetSimOrder,
                        int spaceNum)
{
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

    if (ResetSimOrder && spaceNum == 0) {
        SetZoneEquipSimOrder(state, ZoneNum);
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
            DataZoneEquipment::LoadDist loadDistType = state.dataZoneEquip->ZoneEquipList(ZoneNum).LoadDistScheme;
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
                // SpaceHB TODO: This may need more work
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
}

void DistributeSystemOutputRequired(EnergyPlusData &state, int const ZoneNum, bool const FirstHVACIteration)
{
    // Distribute zone equipment loads according to load distribution scheme

    // Do nothing if this zone is uncontrolled or doing zone sizing
    if (!state.dataHeatBal->Zone(ZoneNum).IsControlled) return;
    if (state.dataGlobal->ZoneSizingCalc) return;

    // Do nothing on FirstHVACIteration if not UniformLoading and not SequentialLoading
    if (FirstHVACIteration && (state.dataZoneEquip->ZoneEquipList(ZoneNum).LoadDistScheme != DataZoneEquipment::LoadDist::Uniform) &&
        (state.dataZoneEquip->ZoneEquipList(ZoneNum).LoadDistScheme != DataZoneEquipment::LoadDist::Sequential)) {
        return;
    }

    distributeOutputRequired(
        state, ZoneNum, state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum), state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum));
    // SpaceHB TODO: This may need more work
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            distributeOutputRequired(state,
                                     ZoneNum,
                                     state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum),
                                     state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum));
        }
    }
}

void distributeOutputRequired(EnergyPlusData &state,
                              int const ZoneNum,
                              DataZoneEnergyDemands::ZoneSystemSensibleDemand &energy,
                              DataZoneEnergyDemands::ZoneSystemMoistureDemand &moisture)
{
    auto &thisZEqList(state.dataZoneEquip->ZoneEquipList(ZoneNum));
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
                                Real64 const SysOutputProvided,                // sensible output provided by zone equipment (W)
                                Real64 const LatOutputProvided,                // latent output provided by zone equipment (kg/s)
                                ObjexxFCL::Optional_int_const EquipPriorityNum // index in PrioritySimOrder for this update
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Unknown
    //       MODIFIED       B. Griffith Sept 2011, add storage of requirements by sequence

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
    auto &thisZEqList(state.dataZoneEquip->ZoneEquipList(ZoneNum));
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

    int constexpr IterMax(25);
    Real64 constexpr ConvergenceTolerance(0.000010);

    state.dataHVACGlobal->ZoneMassBalanceHVACReSim = false;
    int Iteration = 0;
    Real64 BuildingZoneMixingFlow = 0.0;
    Real64 BuildingZoneMixingFlowOld = 0.0;
    Real64 BuildingZoneReturnFlow = 0.0;
    Real64 BuildingZoneReturnFlowOld = 0.0;

    auto &Node(state.dataLoopNodes->Node);

    // Total loop supply and recirc flows (these have been zeroed earlier in InitZoneEquipment
    for (int airDistUnit = 1; airDistUnit <= (int)state.dataDefineEquipment->AirDistUnit.size(); ++airDistUnit) {
        auto &airDisUnit = state.dataDefineEquipment->AirDistUnit(airDistUnit);
        if (airDisUnit.AirLoopNum > 0) {
            auto &airLoopFlow = state.dataAirLoop->AirLoopFlow(airDisUnit.AirLoopNum);
            airLoopFlow.SupFlow += airDisUnit.MassFlowRateSup;
            airLoopFlow.RecircFlow += airDisUnit.MassFlowRatePlenInd;
            airLoopFlow.LeakFlow += airDisUnit.MassFlowRateDnStrLk + airDisUnit.MassFlowRateUpStrLk;
        }
    }

    // Set max OA flow and frac for systems which are all OA (no OASys)
    for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
        if (state.dataAirSystemsData->PrimaryAirSystems(airLoop).isAllOA) {
            auto &airLoopFlow = state.dataAirLoop->AirLoopFlow(airLoop);
            airLoopFlow.MaxOutAir = airLoopFlow.SupFlow;
            airLoopFlow.OAFlow = airLoopFlow.SupFlow;
            airLoopFlow.OAFrac = 1.0;
        }
    }

    do {
        if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
            // These are also reset in ZoneEquipmentManager::InitZoneEquipment, reset again here for each zone mass balance iteration
            for (int airLoop = 1; airLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoop) {
                auto &airLoopFlow = state.dataAirLoop->AirLoopFlow(airLoop);
                airLoopFlow.ZoneRetFlow = 0.0;
                airLoopFlow.SysRetFlow = 0.0;
                airLoopFlow.ExcessZoneExhFlow = 0.0;
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

        for (int ZoneNum1 = 1; ZoneNum1 <= state.dataGlobal->NumOfZones; ++ZoneNum1) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum1).IsControlled) continue;
            int ZoneNum = ZoneNum1;
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) ZoneNum = state.dataHeatBalFanSys->ZoneReOrder(ZoneNum1);
            auto &massConservation = state.dataHeatBal->MassConservation(ZoneNum);
            auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ZoneNum);
            zoneEquipConfig.TotInletAirMassFlowRate = 0.0;
            Real64 TotInletAirMassFlowRateMax = 0.0;
            Real64 TotInletAirMassFlowRateMaxAvail = 0.0;
            Real64 TotInletAirMassFlowRateMin = 0.0;
            Real64 TotInletAirMassFlowRateMinAvail = 0.0;
            Real64 TotInletAirMassFlowRate = 0.0;
            Real64 TotExhaustAirMassFlowRate = 0.0;

            zoneEquipConfig.TotExhaustAirMassFlowRate = 0.0;

            Real64 ZoneMixingAirMassFlowRate = 0.0;
            Real64 ZoneMixingNetAirMassFlowRate = 0.0;
            Real64 ZoneReturnAirMassFlowRate = 0.0;

            for (int NodeNum = 1; NodeNum <= zoneEquipConfig.NumInletNodes; ++NodeNum) {
                {
                    auto const &thisNode(Node(zoneEquipConfig.InletNode(NodeNum)));
                    zoneEquipConfig.TotInletAirMassFlowRate += thisNode.MassFlowRate;
                    TotInletAirMassFlowRateMax += thisNode.MassFlowRateMax;
                    TotInletAirMassFlowRateMaxAvail += thisNode.MassFlowRateMaxAvail;
                    TotInletAirMassFlowRateMin += thisNode.MassFlowRateMin;
                    TotInletAirMassFlowRateMinAvail += thisNode.MassFlowRateMinAvail;
                }
            }

            TotInletAirMassFlowRate = zoneEquipConfig.TotInletAirMassFlowRate;

            for (int NodeNum = 1; NodeNum <= zoneEquipConfig.NumExhaustNodes; ++NodeNum) {

                if (state.afn->AirflowNetworkNumOfExhFan == 0) {
                    zoneEquipConfig.TotExhaustAirMassFlowRate += Node(zoneEquipConfig.ExhaustNode(NodeNum)).MassFlowRate;
                }
            }
            TotExhaustAirMassFlowRate = zoneEquipConfig.TotExhaustAirMassFlowRate;

            // Include zone mixing mass flow rate
            if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(ZoneNum)) {
                int NumRetNodes = zoneEquipConfig.NumReturnNodes;
                for (int NodeNumHere = 1; NodeNumHere <= NumRetNodes; ++NodeNumHere) {
                    int RetNode = zoneEquipConfig.ReturnNode(NodeNumHere);
                    if (RetNode > 0) {
                        ZoneReturnAirMassFlowRate += Node(RetNode).MassFlowRate;
                    }
                }
                // Set zone mixing incoming mass flow rate
                if ((Iteration == 0) || state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnOnly ||
                    state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {
                    ZoneMixingAirMassFlowRate = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum).MixingMassFlowZone;
                } else {
                    ZoneMixingAirMassFlowRate = max(0.0,
                                                    ZoneReturnAirMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate +
                                                        massConservation.MixingSourceMassFlowRate);
                }
                CalcZoneMixingFlowRateOfReceivingZone(state, ZoneNum, ZoneMixingAirMassFlowRate);
                ZoneMixingNetAirMassFlowRate = massConservation.MixingMassFlowRate - massConservation.MixingSourceMassFlowRate;
            }
            auto &zoneNode = Node(zoneEquipConfig.ZoneNode);
            zoneNode.MassFlowRate = TotInletAirMassFlowRate;
            zoneNode.MassFlowRateMax = TotInletAirMassFlowRateMax;
            zoneNode.MassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail;
            zoneNode.MassFlowRateMin = TotInletAirMassFlowRateMin;
            zoneNode.MassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail;

            // Calculate standard return air flow rate using default method of inlets minus exhausts adjusted for "balanced" exhaust flow
            Real64 StdTotalReturnMassFlow =
                TotInletAirMassFlowRate + ZoneMixingNetAirMassFlowRate - (TotExhaustAirMassFlowRate - zoneEquipConfig.ZoneExhBalanced);

            if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                if (StdTotalReturnMassFlow < 0.0) {
                    zoneEquipConfig.ExcessZoneExh = -StdTotalReturnMassFlow;
                    StdTotalReturnMassFlow = 0.0;
                } else {
                    zoneEquipConfig.ExcessZoneExh = 0.0;
                }
            } else {
                zoneEquipConfig.ExcessZoneExh = 0.0;
                StdTotalReturnMassFlow = max(0.0, StdTotalReturnMassFlow);
            }

            Real64 FinalTotalReturnMassFlow = 0;
            CalcZoneReturnFlows(state, ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow);
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                // set mass conservation variables
                massConservation.InMassFlowRate = TotInletAirMassFlowRate;
                massConservation.ExhMassFlowRate = TotExhaustAirMassFlowRate;

                if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingOnly ||
                    state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingThenReturn) {
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    Real64 AdjustedTotalReturnMassFlow = 0;
                    massConservation.RetMassFlowRate = FinalTotalReturnMassFlow;
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustMixingThenReturn) {

                        // Calculate return air flow rate using mass conservation equation
                        AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                        AdjustedTotalReturnMassFlow = min(AdjustedTotalReturnMassFlow, zoneEquipConfig.AirLoopDesSupply);
                        // add adjust zone return node air flow calc
                        CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                        massConservation.RetMassFlowRate = FinalTotalReturnMassFlow;
                        ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;
                    }
                    // Set zone infiltration air flow rate
                    CalcZoneInfiltrationFlows(state, ZoneNum, ZoneReturnAirMassFlowRate);

                } else if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnOnly ||
                           state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {

                    Real64 AdjustedTotalReturnMassFlow = 0;
                    // Calculate return air flow rate using mass conservation equation
                    AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                    AdjustedTotalReturnMassFlow = min(AdjustedTotalReturnMassFlow, zoneEquipConfig.AirLoopDesSupply);

                    // add adjust zone return node air flow calculation
                    CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                    massConservation.RetMassFlowRate = FinalTotalReturnMassFlow;
                    ZoneReturnAirMassFlowRate = FinalTotalReturnMassFlow;

                    if (state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment == DataHeatBalance::AdjustmentType::AdjustReturnThenMixing) {
                        ZoneMixingAirMassFlowRate = max(0.0,
                                                        ZoneReturnAirMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate +
                                                            massConservation.MixingSourceMassFlowRate);
                        CalcZoneMixingFlowRateOfReceivingZone(state, ZoneNum, ZoneMixingAirMassFlowRate);
                        ZoneMixingNetAirMassFlowRate = massConservation.MixingMassFlowRate - massConservation.MixingSourceMassFlowRate;

                        // Calculate return air flow rate using mass conservation equation
                        AdjustedTotalReturnMassFlow = max(0.0, TotInletAirMassFlowRate - TotExhaustAirMassFlowRate + ZoneMixingNetAirMassFlowRate);
                        AdjustedTotalReturnMassFlow = min(AdjustedTotalReturnMassFlow, zoneEquipConfig.AirLoopDesSupply);

                        // add adjust zone return node air flow calc
                        CalcZoneReturnFlows(state, ZoneNum, AdjustedTotalReturnMassFlow, FinalTotalReturnMassFlow);
                        massConservation.RetMassFlowRate = FinalTotalReturnMassFlow;
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

            BuildingZoneMixingFlow += massConservation.MixingMassFlowRate;
            BuildingZoneReturnFlow += massConservation.RetMassFlowRate;

            // Accumulate airloop total return flows and allocate excess exhaust flows
            for (int returnNum = 1; returnNum <= zoneEquipConfig.NumReturnNodes; ++returnNum) {
                int retNode = zoneEquipConfig.ReturnNode(returnNum);
                int airLoop = zoneEquipConfig.ReturnNodeAirLoopNum(returnNum);
                if (airLoop > 0) {
                    state.dataAirLoop->AirLoopFlow(airLoop).ZoneRetFlow += Node(retNode).MassFlowRate;
                    if (zoneEquipConfig.TotAvailAirLoopOA > 0.0) {
                        state.dataAirLoop->AirLoopFlow(airLoop).ExcessZoneExhFlow +=
                            zoneEquipConfig.ExcessZoneExh * state.dataAirLoop->AirLoopFlow(airLoop).MaxOutAir / zoneEquipConfig.TotAvailAirLoopOA;
                    }
                }
            }
        }

        // adjust the zone return air flow rates to match any excess zone exhaust flows
        for (int airLoopNum = 1; airLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++airLoopNum) {
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
            auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);
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
                    if (sysUnbalancedFlow > DataHVACGlobals::SmallMassFlow) {
                        // Now include infiltration, ventilation, and mixing flows (these are all entering the zone, so subtract them)
                        Real64 incomingFlow = thisZoneHB.OAMFL + thisZoneHB.VAMFL + thisZoneHB.MixingMassFlowZone;
                        Real64 unbalancedFlow = max(0.0, sysUnbalancedFlow - incomingFlow);
                        if (unbalancedFlow > DataHVACGlobals::SmallMassFlow) {
                            // Re-check on volume basis - use current zone density for incoming, standard density for HVAC sys
                            Real64 zoneTemp = Node(thisZoneEquip.ZoneNode).Temp;
                            Real64 zoneHumRat = Node(thisZoneEquip.ZoneNode).HumRat;
                            Real64 rhoZone = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, zoneTemp, zoneHumRat, "CalcZoneMassBalance");
                            Real64 incomingVolFlow = incomingFlow / rhoZone;
                            Real64 sysUnbalancedVolFlow = sysUnbalancedFlow / state.dataEnvrn->StdRhoAir;
                            Real64 unbalancedVolFlow = max(0.0, sysUnbalancedVolFlow - incomingVolFlow);
                            if (unbalancedVolFlow > DataHVACGlobals::SmallAirVolFlow) {
                                ShowWarningError(state,
                                                 format("In zone {} there is unbalanced air flow. Load due to induced outdoor air is neglected.",
                                                        thisZoneEquip.ZoneName));
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("  Flows [m3/s]: Inlets: {:.6R}  Unbalanced exhausts: {:.6R}  Returns: {:.6R}",
                                                         thisZoneEquip.TotInletAirMassFlowRate / state.dataEnvrn->StdRhoAir,
                                                         sysUnbalExhaust / state.dataEnvrn->StdRhoAir,
                                                         totalZoneReturnMassFlow / state.dataEnvrn->StdRhoAir));
                                ShowContinueError(state,
                                                  format("  Infiltration: {:.6R}  Zone Ventilation: {:.6R}  Mixing (incoming): {:.6R}",
                                                         thisZoneHB.OAMFL / rhoZone,
                                                         thisZoneHB.VAMFL / rhoZone,
                                                         thisZoneHB.MixingMassFlowZone / rhoZone));
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
                state.dataHVACGlobal->ZoneMassBalanceHVACReSim = false;
                break;
            } else {
                state.dataHVACGlobal->ZoneMassBalanceHVACReSim = true;
            }
        }
        if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) break;
        Iteration += 1;

    } while (Iteration < IterMax);
    // Set system return flows
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
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
    auto &massConservation = state.dataHeatBal->MassConservation(ZoneNum);

    // Set zone infiltration flow rate
    if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment != DataHeatBalance::InfiltrationFlow::No) {
        if (massConservation.InfiltrationPtr > 0) {
            if (massConservation.IsOnlySourceZone ||
                (state.dataHeatBal->ZoneAirMassFlow.InfiltrationForZones == DataHeatBalance::InfiltrationZoneType::AllZones)) {
                ZoneInfiltrationMassFlowRate = massConservation.MixingSourceMassFlowRate - massConservation.MixingMassFlowRate +
                                               state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotExhaustAirMassFlowRate + ZoneReturnAirMassFlowRate -
                                               state.dataZoneEquip->ZoneEquipConfig(ZoneNum).TotInletAirMassFlowRate;
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    if (std::abs(ZoneInfiltrationMassFlowRate) > ConvergenceTolerance) {
                        state.dataHeatBalFanSys->ZoneInfiltrationFlag(ZoneNum) = true;
                        massConservation.InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
                        massConservation.IncludeInfilToZoneMassBal = 1;
                        state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate = ZoneInfiltrationMassFlowRate;
                        state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate =
                            max(0.0, state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate);
                    } else {
                        massConservation.InfiltrationMassFlowRate = 0.0;
                        state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate = 0.0;
                    }
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    if (ZoneInfiltrationMassFlowRate > ConvergenceTolerance) {
                        state.dataHeatBalFanSys->ZoneInfiltrationFlag(ZoneNum) = true;
                        massConservation.InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
                        massConservation.IncludeInfilToZoneMassBal = 1;
                        state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate += ZoneInfiltrationMassFlowRate;
                    } else {
                        massConservation.InfiltrationMassFlowRate = 0.0;
                    }
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::No) {
                    massConservation.InfiltrationMassFlowRate = 0.0;
                }
            } else {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                    massConservation.InfiltrationMassFlowRate = state.dataHeatBal->Infiltration(massConservation.InfiltrationPtr).MassFlowRate;
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                    massConservation.InfiltrationMassFlowRate = 0.0;
                } else if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::No) {
                    massConservation.InfiltrationMassFlowRate = 0.0;
                }
            }
        } else {
            // Zone has no infiltration objects
            massConservation.InfiltrationMassFlowRate = 0.0;
        }
    }
}

void CalcZoneLeavingConditions(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   January 2001
    //       MODIFIED       June 2003, FCW: add heat from airflow window to return air

    // PURPOSE OF THIS SUBROUTINE:
    // Perform zone upate of the leaving conditions.

    // METHODOLOGY EMPLOYED:
    // Energy Balance.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TempRetAir;  // Return air temperature [C]
    Real64 TempZoneAir; // Zone air temperature [C]
    Real64 SumRetAirLatentGainRate;

    for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;
        // A return air system may not exist for certain systems; Therefore when no return node exists
        // there is no update.  Of course if there is no return air system then you cannot update
        // the energy for the return air heat gain from the lights statements.
        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes == 0) continue;
        int ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode;
        int ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum);
        for (int nodeCount = 1; nodeCount <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes; ++nodeCount) {
            int ReturnNode = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNode(nodeCount);
            int ReturnNodeExhaustNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNodeExhaustNodeNum(nodeCount);

            // RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
            // Add sensible heat gain from refrigerated cases with under case returns
            Real64 QRetAir = InternalHeatGains::zoneSumAllReturnAirConvectionGains(state, ZoneNum, ReturnNode);

            // Need to add the energy to the return air from lights and from airflow windows. Where the heat
            // is added depends on if there is system flow or not.  If there is system flow the heat is added
            // to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
            // Correct step through the SysDepZoneLoads variable.

            Real64 MassFlowRA = state.dataLoopNodes->Node(ReturnNode).MassFlowRate / ZoneMult;
            if (ReturnNodeExhaustNum > 0 && state.dataLoopNodes->Node(ReturnNodeExhaustNum).MassFlowRate > 0.0) {
                MassFlowRA += state.dataLoopNodes->Node(ReturnNodeExhaustNum).MassFlowRate;
            }

            // user defined room air model may feed temp that differs from zone node
            if (allocated(state.dataRoomAirMod->AirPatternZoneInfo)) {
                if ((state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).IsUsed) && (!state.dataGlobal->BeginEnvrnFlag)) {
                    TempZoneAir = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving;
                    TempRetAir = TempZoneAir;
                } else {
                    TempZoneAir = state.dataLoopNodes->Node(ZoneNode).Temp;
                    TempRetAir = TempZoneAir;
                }
            } else {
                TempZoneAir = state.dataLoopNodes->Node(ZoneNode).Temp;
                TempRetAir = TempZoneAir;
            }

            Real64 WinGapFlowToRA = 0.0;  // Mass flow to return air from all airflow windows in zone [kg/s]
            Real64 WinGapTtoRA = 0.0;     // Temp of outlet flow mixture to return air from all airflow windows in zone [C]
            Real64 WinGapFlowTtoRA = 0.0; // Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]

            if (state.dataHeatBal->Zone(ZoneNum).HasAirFlowWindowReturn) {
                for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0 &&
                            state.dataSurface->SurfWinAirflowDestination(SurfNum) == DataSurfaces::WindowAirFlowDestination::Return) {
                            Real64 FlowThisTS = PsyRhoAirFnPbTdbW(state,
                                                                  state.dataEnvrn->OutBaroPress,
                                                                  state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum),
                                                                  state.dataLoopNodes->Node(ZoneNode).HumRat) *
                                                state.dataSurface->SurfWinAirflowThisTS(SurfNum) * state.dataSurface->Surface(SurfNum).Width;
                            WinGapFlowToRA += FlowThisTS;
                            WinGapFlowTtoRA += FlowThisTS * state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum);
                        }
                    }
                }
            }
            if (WinGapFlowToRA > 0.0) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowToRA;
            // the flag NoHeatToReturnAir is TRUE if the system is zonal only or is central with on/off air flow. In these
            // cases the heat to return air is treated as a zone heat gain and dealt with in CalcZoneSums in
            // MODULE ZoneTempPredictorCorrector.
            if (!state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
                Real64 CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
                if (MassFlowRA > 0.0) {
                    if (WinGapFlowToRA > 0.0) {
                        // Add heat-to-return from window gap airflow
                        if (MassFlowRA >= WinGapFlowToRA) {
                            TempRetAir = (WinGapFlowTtoRA + (MassFlowRA - WinGapFlowToRA) * TempZoneAir) / MassFlowRA;
                        } else {
                            // All of return air comes from flow through airflow windows
                            TempRetAir = WinGapTtoRA;
                            // Put heat from window airflow that exceeds return air flow into zone air
                            thisZoneHB.SysDepZoneLoads += (WinGapFlowToRA - MassFlowRA) * CpAir * (WinGapTtoRA - TempZoneAir);
                        }
                    }
                    // Add heat-to-return from lights
                    TempRetAir += QRetAir / (MassFlowRA * CpAir);
                    if (TempRetAir > DataHVACGlobals::RetTempMax) {
                        state.dataLoopNodes->Node(ReturnNode).Temp = DataHVACGlobals::RetTempMax;
                        if (!state.dataGlobal->ZoneSizingCalc) {
                            thisZoneHB.SysDepZoneLoads += CpAir * MassFlowRA * (TempRetAir - DataHVACGlobals::RetTempMax);
                        }
                    } else if (TempRetAir < DataHVACGlobals::RetTempMin) {
                        state.dataLoopNodes->Node(ReturnNode).Temp = DataHVACGlobals::RetTempMin;
                        if (!state.dataGlobal->ZoneSizingCalc) {
                            thisZoneHB.SysDepZoneLoads += CpAir * MassFlowRA * (TempRetAir - DataHVACGlobals::RetTempMin);
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
                    if (WinGapFlowToRA > 0.0) thisZoneHB.SysDepZoneLoads += WinGapFlowToRA * CpAir * (WinGapTtoRA - TempZoneAir);
                    // Assign all heat-to-return from lights to zone air
                    if (QRetAir > 0.0) thisZoneHB.SysDepZoneLoads += QRetAir;
                    state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
                }
            } else {
                // update the return air node for zonal and central on/off systems
                state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
            }

            // Update the rest of the Return Air Node conditions, if the return air system exists!
            state.dataLoopNodes->Node(ReturnNode).Press = state.dataLoopNodes->Node(ZoneNode).Press;

            // Include impact of under case returns for refrigerated display case when updating the return air node humidity
            if (!state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
                if (MassFlowRA > 0) {
                    SumRetAirLatentGainRate = InternalHeatGains::SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                    Real64 H2OHtOfVap = PsyHgAirFnWTdb(state.dataLoopNodes->Node(ZoneNode).HumRat, state.dataLoopNodes->Node(ReturnNode).Temp);
                    state.dataLoopNodes->Node(ReturnNode).HumRat =
                        state.dataLoopNodes->Node(ZoneNode).HumRat + (SumRetAirLatentGainRate / (H2OHtOfVap * MassFlowRA));
                } else {
                    // If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
                    state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                    state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToZone +=
                        state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToHVAC;
                    // shouldn't the HVAC term be zeroed out then?
                    SumRetAirLatentGainRate = InternalHeatGains::SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                    thisZoneHB.ZoneLatentGain += SumRetAirLatentGainRate;
                }
            } else {
                state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToZone += state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToHVAC;
                // shouldn't the HVAC term be zeroed out then?
                SumRetAirLatentGainRate = InternalHeatGains::SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                thisZoneHB.ZoneLatentGain += SumRetAirLatentGainRate;
            }

            state.dataLoopNodes->Node(ReturnNode).Enthalpy =
                PsyHFnTdbW(state.dataLoopNodes->Node(ReturnNode).Temp, state.dataLoopNodes->Node(ReturnNode).HumRat);

            if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                state.dataLoopNodes->Node(ReturnNode).CO2 = state.dataLoopNodes->Node(ZoneNode).CO2;
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                state.dataLoopNodes->Node(ReturnNode).GenContam = state.dataLoopNodes->Node(ZoneNode).GenContam;

        } // End of check for a return air node, which implies a return air system.

        // Reset current deadband flags, remaining output required, so no impact beyond zone equipment
        InitSystemOutputRequired(state, ZoneNum, FirstHVACIteration, true);
    }
}

void UpdateZoneEquipment(EnergyPlusData &state, bool &SimAir)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs the update for Zone Equipment Management.
    // Specifically, it transfers the conditions from the zone equipment return air nodes across
    // to the air loop side, allowing for multiple return air nodes

    // Transfer the conditions from the zone equipment return air nodes across
    // to the air loop side, allowing for multiple return air nodes
    for (int ZoneGroupNum = 1; ZoneGroupNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++ZoneGroupNum) {
        for (int RetAirPathNum = 1; RetAirPathNum <= state.dataAirLoop->AirToZoneNodeInfo(ZoneGroupNum).NumReturnNodes; ++RetAirPathNum) {
            HVACInterfaceManager::UpdateHVACInterface(state,
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
    //       MODIFIED       Shirey, Jan 2008 (MIXING objects, use avg. conditions for Cp, Air Density and Hfg)
    //       MODIFIED       L. Lawrie and L. GU, Jan. 2008 (Allow multiple infiltration and ventilation objects)
    //                      B. Griffith. Jan 2009 add infiltration, residential basic/sherman-grimsrud and enhanced/AIM2
    //                      L. Lawrie - March 2009 - move ventilation electric calculation to this routine (for
    //                        Electricity Net.
    //                      L. Gu - Dec. 2009 - Added a new ventilation object to calculate flow rate based on wind and stack
    //                        effect through an opening.
    //       MODIFIED       Stovall - Aug 2011 (add refrigerator door mixing)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the air component of the heat balance.

    constexpr Real64 StdGravity(9.80665); // The acceleration of gravity at the sea level (m/s2)
    static constexpr std::string_view RoutineNameVentilation("CalcAirFlowSimple:Ventilation");
    static constexpr std::string_view RoutineNameMixing("CalcAirFlowSimple:Mixing");
    static constexpr std::string_view RoutineNameCrossMixing("CalcAirFlowSimple:CrossMixing");
    static constexpr std::string_view RoutineNameRefrigerationDoorMixing("CalcAirFlowSimple:RefrigerationDoorMixing");
    static constexpr std::string_view RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");
    static constexpr std::string_view RoutineNameZoneAirBalance("CalcAirFlowSimple:ZoneAirBalance");

    for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
        thisZoneHB.MCPM = 0.0;
        thisZoneHB.MCPTM = 0.0;
        thisZoneHB.MCPTI = 0.0;
        thisZoneHB.MCPI = 0.0;
        thisZoneHB.OAMFL = 0.0;
        thisZoneHB.MCPTV = 0.0;
        thisZoneHB.MCPV = 0.0;
        thisZoneHB.VAMFL = 0.0;
        thisZoneHB.MDotCPOA = 0.0;
        thisZoneHB.MDotOA = 0.0;
        thisZoneHB.MCPThermChim = 0.0;
        thisZoneHB.ThermChimAMFL = 0.0;
        thisZoneHB.MCPTThermChim = 0.0;
        thisZoneHB.MixingMassFlowZone = 0.0;
        thisZoneHB.MixingMassFlowXHumRat = 0.0;
    }
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
            thisSpaceHB.MCPM = 0.0;
            thisSpaceHB.MCPTM = 0.0;
            thisSpaceHB.MCPTI = 0.0;
            thisSpaceHB.MCPI = 0.0;
            thisSpaceHB.OAMFL = 0.0;
            thisSpaceHB.MCPTV = 0.0;
            thisSpaceHB.MCPV = 0.0;
            thisSpaceHB.VAMFL = 0.0;
            thisSpaceHB.MDotCPOA = 0.0;
            thisSpaceHB.MDotOA = 0.0;
            thisSpaceHB.MCPThermChim = 0.0;
            thisSpaceHB.ThermChimAMFL = 0.0;
            thisSpaceHB.MCPTThermChim = 0.0;
            thisSpaceHB.MixingMassFlowZone = 0.0;
            thisSpaceHB.MixingMassFlowXHumRat = 0.0;
        }
    }
    if (state.dataContaminantBalance->Contaminant.CO2Simulation &&
        state.dataHeatBal->TotMixing + state.dataHeatBal->TotCrossMixing + state.dataHeatBal->TotRefDoorMixing > 0)
        state.dataContaminantBalance->MixingMassFlowCO2 = 0.0;
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation &&
        state.dataHeatBal->TotMixing + state.dataHeatBal->TotCrossMixing + state.dataHeatBal->TotRefDoorMixing > 0)
        state.dataContaminantBalance->MixingMassFlowGC = 0.0;

    Real64 IVF = 0.0; // DESIGN INFILTRATION FLOW RATE (M**3/SEC)
    Real64 VVF = 0.0; // DESIGN VENTILATION FLOW RATE (M**3/SEC)

    if (!state.dataHeatBal->AirFlowFlag) return;
    // AirflowNetwork Multizone field /= SIMPLE
    if (!(state.afn->simulation_control.type == AirflowNetwork::ControlType::NoMultizoneOrDistribution ||
          state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation)) {
        return;
    }

    EarthTube::ManageEarthTube(state);
    CoolTower::ManageCoolTower(state);
    ThermalChimney::ManageThermalChimney(state);

    // Assign zone air temperature
    for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
        thisZoneHB.MixingMAT = thisZoneHB.MAT;
        thisZoneHB.MixingHumRat = thisZoneHB.ZoneAirHumRat;
        // This is only temporary fix for CR8867.  (L. Gu 8/12)
        if (SysTimestepLoop == 1) {
            thisZoneHB.MixingMAT = thisZoneHB.XMPT;
            thisZoneHB.MixingHumRat = thisZoneHB.WZoneTimeMinusP;
        }
    }
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
            thisSpaceHB.MixingMAT = thisSpaceHB.MAT;
            thisSpaceHB.MixingHumRat = thisSpaceHB.ZoneAirHumRat;
            // This is only temporary fix for CR8867.  (L. Gu 8/12)
            if (SysTimestepLoop == 1) {
                thisSpaceHB.MixingMAT = thisSpaceHB.XMPT;
                thisSpaceHB.MixingHumRat = thisSpaceHB.WZoneTimeMinusP;
            }
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

    if (state.dataHeatBal->TotVentilation > 0) {
        for (auto &e : state.dataHeatBal->ZnAirRpt) {
            e.VentilFanElec = 0.0;
        }
    }

    // Process the scheduled Ventilation for air heat balance
    for (int j = 1; j <= state.dataHeatBal->TotVentilation; ++j) {
        auto &thisVentilation = state.dataHeatBal->Ventilation(j);
        int zoneNum = thisVentilation.ZonePtr;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);
        Real64 thisMixingMAT = 0.0;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            thisMixingMAT = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisVentilation.spaceIndex).MixingMAT;
        } else {
            thisMixingMAT = thisZoneHB.MixingMAT;
        }
        thisVentilation.FanPower = 0.0;
        thisVentilation.MCP = 0.0;

        Real64 TempExt = state.dataHeatBal->Zone(zoneNum).OutDryBulbTemp;
        Real64 WindSpeedExt = state.dataHeatBal->Zone(zoneNum).WindSpeed;
        Real64 WindDirExt = state.dataHeatBal->Zone(zoneNum).WindDir;
        Real64 thisMCPV = 0.0;
        Real64 thisVAMFL = 0.0;
        Real64 thisMCPTV = 0.0;

        // Use air node information linked to the zone if defined
        Real64 HumRatExt = 0.0;
        Real64 EnthalpyExt = 0.0;
        if (state.dataHeatBal->Zone(zoneNum).LinkedOutAirNode > 0) {
            HumRatExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(zoneNum).LinkedOutAirNode).HumRat;
            EnthalpyExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(zoneNum).LinkedOutAirNode).Enthalpy;
        } else {
            HumRatExt = state.dataEnvrn->OutHumRat;
            EnthalpyExt = state.dataEnvrn->OutEnthalpy;
        }
        Real64 AirDensity =
            PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempExt, HumRatExt, RoutineNameVentilation); // Density of air (kg/m^3)
        Real64 CpAir = PsyCpAirFnW(HumRatExt);

        // Hybrid ventilation global control
        int I = 0;
        if (thisVentilation.HybridControlType == DataHeatBalance::HybridCtrlType::Global && thisVentilation.HybridControlMasterNum > 0) {
            I = thisVentilation.HybridControlMasterNum;
            if (j == I) {
                thisVentilation.HybridControlMasterStatus = false;
            }
        } else {
            I = j;
        }
        auto &hybridControlVentilation = state.dataHeatBal->Ventilation(I);
        // Hybrid controlled zone MAT
        Real64 hybridControlZoneMAT = state.dataZoneTempPredictorCorrector->zoneHeatBalance(hybridControlVentilation.ZonePtr).MixingMAT;

        // Check scheduled temperatures
        if (hybridControlVentilation.MinIndoorTempSchedPtr > 0) {
            hybridControlVentilation.MinIndoorTemperature =
                ScheduleManager::GetCurrentScheduleValue(state, hybridControlVentilation.MinIndoorTempSchedPtr);
        }
        if (hybridControlVentilation.MaxIndoorTempSchedPtr > 0) {
            hybridControlVentilation.MaxIndoorTemperature =
                ScheduleManager::GetCurrentScheduleValue(state, hybridControlVentilation.MaxIndoorTempSchedPtr);
        }
        // Ensure the minimum indoor temperature <= the maximum indoor temperature
        if (hybridControlVentilation.MinIndoorTempSchedPtr > 0 || hybridControlVentilation.MaxIndoorTempSchedPtr > 0) {
            if (hybridControlVentilation.MinIndoorTemperature > hybridControlVentilation.MaxIndoorTemperature) {
                ++hybridControlVentilation.IndoorTempErrCount;
                if (hybridControlVentilation.IndoorTempErrCount < 2) {
                    ShowWarningError(
                        state,
                        format("Ventilation indoor temperature control: The minimum indoor temperature is above the maximum indoor temperature in {}",
                               hybridControlVentilation.Name));
                    ShowContinueError(state, "The minimum indoor temperature is set to the maximum indoor temperature. Simulation continues.");
                    ShowContinueErrorTimeStamp(state, " Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The minimum indoor temperature is still above the maximum indoor temperature",
                                                   hybridControlVentilation.IndoorTempErrIndex,
                                                   hybridControlVentilation.MinIndoorTemperature,
                                                   hybridControlVentilation.MinIndoorTemperature);
                }
                hybridControlVentilation.MinIndoorTemperature = hybridControlVentilation.MaxIndoorTemperature;
            }
        }
        if (hybridControlVentilation.MinOutdoorTempSchedPtr > 0) {
            hybridControlVentilation.MinOutdoorTemperature =
                ScheduleManager::GetCurrentScheduleValue(state, hybridControlVentilation.MinOutdoorTempSchedPtr);
        }
        if (hybridControlVentilation.MaxOutdoorTempSchedPtr > 0) {
            hybridControlVentilation.MaxOutdoorTemperature =
                ScheduleManager::GetCurrentScheduleValue(state, hybridControlVentilation.MaxOutdoorTempSchedPtr);
        }
        // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
        if (hybridControlVentilation.MinOutdoorTempSchedPtr > 0 || hybridControlVentilation.MaxOutdoorTempSchedPtr > 0) {
            if (hybridControlVentilation.MinOutdoorTemperature > hybridControlVentilation.MaxOutdoorTemperature) {
                ++hybridControlVentilation.OutdoorTempErrCount;
                if (hybridControlVentilation.OutdoorTempErrCount < 2) {
                    ShowWarningError(
                        state,
                        format(
                            "Ventilation outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in {}",
                            hybridControlVentilation.Name));
                    ShowContinueError(state, "The minimum outdoor temperature is set to the maximum outdoor temperature. Simulation continues.");
                    ShowContinueErrorTimeStamp(state, " Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                   hybridControlVentilation.OutdoorTempErrIndex,
                                                   hybridControlVentilation.MinOutdoorTemperature,
                                                   hybridControlVentilation.MinOutdoorTemperature);
                }
                hybridControlVentilation.MinIndoorTemperature = hybridControlVentilation.MaxIndoorTemperature;
            }
        }
        if (hybridControlVentilation.DeltaTempSchedPtr > 0) {
            hybridControlVentilation.DelTemperature = ScheduleManager::GetCurrentScheduleValue(state, hybridControlVentilation.DeltaTempSchedPtr);
        }
        // Skip this if the zone is below the minimum indoor temperature limit
        if ((hybridControlZoneMAT < hybridControlVentilation.MinIndoorTemperature) && (!thisVentilation.EMSSimpleVentOn)) continue;
        // Skip this if the zone is above the maximum indoor temperature limit
        if ((hybridControlZoneMAT > hybridControlVentilation.MaxIndoorTemperature) && (!thisVentilation.EMSSimpleVentOn)) continue;
        // Skip if below the temperature difference limit (3/12/03 Negative DelTemperature allowed now)
        if (((hybridControlZoneMAT - TempExt) < hybridControlVentilation.DelTemperature) && (!thisVentilation.EMSSimpleVentOn)) continue;
        // Skip this if the outdoor temperature is below the minimum outdoor temperature limit
        if ((TempExt < hybridControlVentilation.MinOutdoorTemperature) && (!thisVentilation.EMSSimpleVentOn)) continue;
        // Skip this if the outdoor temperature is above the maximum outdoor temperature limit
        if ((TempExt > hybridControlVentilation.MaxOutdoorTemperature) && (!thisVentilation.EMSSimpleVentOn)) continue;
        // Skip this if the outdoor wind speed is above the maximum windspeed limit
        if ((WindSpeedExt > hybridControlVentilation.MaxWindSpeed) && (!thisVentilation.EMSSimpleVentOn)) continue;

        // Hybrid ventilation controls
        if ((thisVentilation.HybridControlType == DataHeatBalance::HybridCtrlType::Close) && (!thisVentilation.EMSSimpleVentOn)) continue;
        if (thisVentilation.HybridControlType == DataHeatBalance::HybridCtrlType::Global && thisVentilation.HybridControlMasterNum > 0) {
            if (j == I) thisVentilation.HybridControlMasterStatus = true;
        }

        if (thisVentilation.ModelType == DataHeatBalance::VentilationModelType::DesignFlowRate) {
            // CR6845 if calculated < 0, don't propagate.
            VVF = thisVentilation.DesignLevel * ScheduleManager::GetCurrentScheduleValue(state, thisVentilation.SchedPtr);

            if (thisVentilation.EMSSimpleVentOn) VVF = thisVentilation.EMSimpleVentFlowRate;

            if (VVF < 0.0) VVF = 0.0;
            thisVentilation.MCP = VVF * AirDensity * CpAir *
                                  (thisVentilation.ConstantTermCoef + std::abs(TempExt - thisMixingMAT) * thisVentilation.TemperatureTermCoef +
                                   WindSpeedExt * (thisVentilation.VelocityTermCoef + WindSpeedExt * thisVentilation.VelocitySQTermCoef));
            if (thisVentilation.MCP < 0.0) thisVentilation.MCP = 0.0;
            Real64 VAMFL_temp = thisVentilation.MCP / CpAir;
            if (state.dataHeatBal->Zone(zoneNum).zoneOAQuadratureSum) {
                auto &thisZoneAirBalance = state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Zone(zoneNum).zoneOABalanceIndex);
                switch (thisVentilation.FanType) {
                    // ventilation type based calculation
                case DataHeatBalance::VentilationType::Exhaust: {
                    thisZoneAirBalance.ExhMassFlowRate += VAMFL_temp;
                } break;
                case DataHeatBalance::VentilationType::Intake: {
                    thisZoneAirBalance.IntMassFlowRate += VAMFL_temp;
                } break;
                case DataHeatBalance::VentilationType::Natural: {
                    thisZoneAirBalance.NatMassFlowRate += VAMFL_temp;
                } break;
                case DataHeatBalance::VentilationType::Balanced: {
                    thisZoneAirBalance.BalMassFlowRate += VAMFL_temp;
                } break;
                default:
                    break;
                }
            } else {
                thisMCPV = thisVentilation.MCP;
                thisVAMFL = VAMFL_temp;
            }
            if (thisVentilation.FanEfficiency > 0.0) {
                thisVentilation.FanPower = VAMFL_temp * thisVentilation.FanPressure / (thisVentilation.FanEfficiency * AirDensity);
                if (thisVentilation.FanType == DataHeatBalance::VentilationType::Balanced) thisVentilation.FanPower *= 2.0;
                // calc electric
                if (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation) {
                    // CR7608 IF (.not. TurnFansOn .or. .not. AirflowNetworkZoneFlag(zoneNum)) &
                    if (!state.dataGlobal->KickOffSimulation) {
                        if (!(state.dataZoneEquip->ZoneEquipAvail(zoneNum) == DataHVACGlobals::CycleOn ||
                              state.dataZoneEquip->ZoneEquipAvail(zoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) ||
                            !state.afn->AirflowNetworkZoneFlag(zoneNum))
                            state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec +=
                                thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                    } else if (!state.afn->AirflowNetworkZoneFlag(zoneNum)) {
                        state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec +=
                            thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                    }
                } else {
                    state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec +=
                        thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                }
            }
            // Intake fans will add some heat to the air, raising the temperature for an intake fan...
            if (thisVentilation.FanType == DataHeatBalance::VentilationType::Intake ||
                thisVentilation.FanType == DataHeatBalance::VentilationType::Balanced) {
                Real64 OutletAirEnthalpy = 0.0;
                if (VAMFL_temp == 0.0) {
                    OutletAirEnthalpy = EnthalpyExt;
                } else {
                    if (thisVentilation.FanPower > 0.0) {
                        if (thisVentilation.FanType == DataHeatBalance::VentilationType::Balanced) {
                            OutletAirEnthalpy = EnthalpyExt + thisVentilation.FanPower / VAMFL_temp / 2.0; // Half fan power to calculate inlet T
                        } else {
                            OutletAirEnthalpy = EnthalpyExt + thisVentilation.FanPower / VAMFL_temp;
                        }
                    } else {
                        OutletAirEnthalpy = EnthalpyExt;
                    }
                }
                thisVentilation.AirTemp = Psychrometrics::PsyTdbFnHW(OutletAirEnthalpy, HumRatExt);
            } else {
                thisVentilation.AirTemp = TempExt;
            }
            if (!state.dataHeatBal->Zone(zoneNum).zoneOAQuadratureSum) thisMCPTV = thisVentilation.MCP * thisVentilation.AirTemp;
        } else if (thisVentilation.ModelType == DataHeatBalance::VentilationModelType::WindAndStack) {
            Real64 Cw = 0.0;    // Opening effectivenss
            Real64 Cd = 0.0;    // Discharge coefficent
            Real64 angle = 0.0; // Angle between wind direction and effective angle
            Real64 Qw = 0.0;    // Volumetric flow driven by wind
            Real64 Qst = 0.0;   // Volumetric flow driven by stack effect
            if (thisVentilation.OpenEff != DataGlobalConstants::AutoCalculate) {
                Cw = thisVentilation.OpenEff;
            } else {
                //   Wind Dir (β1)                                        90°, min effectiveness
                //           ▲                                     (ϕ3) .  ▲
                //     .   ϕ │    Opening Normal (α)                       │  .  Angle (ϕ1)
                //      . ┌──┼─┐ x    . (β4)                     Wind      │
                //       .▼  │ ▼x   .                  <=>       Blowing   │
                //        .  │ x  .                              Opposite  │         . (ϕ4)
                //         . │x                                  Side      │
                //  ─────────┼─────────► North = 0°          (ϕ2).─────────┴─────────► Opening Normal = 0°, max effectiveness
                //          .│.
                //         . │ .
                //        .  │  .
                //  (β2) .   │   . (β3)
                //
                // This is the absolute angle between opening normal and the wind direction, in the [0, 180] range:
                // * 0 means that it's blowing directly towards the opening (what ASHRAE HoF calls "Perpendicular winds"), so maximum effectiveness
                //       │  │
                //       ▼  ▼
                //    ┌──====──┐
                //    │        │
                //    └────────┘
                //
                // * 90 means that the wind direction is perpendicular to the normal (the wind is blowing parallel to the opening's plane), so
                // effectiveness is very small
                //      ~~~~►
                //      ~~~~►
                //    ┌──====──┐
                //    │        │
                //    └────────┘
                //
                // * Anything >90 means the wind is blowing in the opposite direction (on the other side), so effectiveness is nil
                //    ┌──====──┐
                //    │        │
                //    └────────┘
                //       ▲  ▲
                //       │  │

                angle = 180.0 - std::abs(std::abs(WindDirExt - thisVentilation.EffAngle) - 180);
                if (angle > 90.0) {
                    Cw = 0.0; // blowing on the opposite side of the opening
                } else {
                    // Linear interpolation between effective angle and wind direction
                    // ASHRAE HoF 2009 (Ch 16.14, Equation 37), Q = Cw*A*U, and it describes Cw as :
                    // > Cw = effectiness of openings (Cw is assumed to be 0.5 to 0.6 for perpendicular winds and 0.25 to 0.35 for diagonal winds)
                    //
                    // | ASHRAE description  | min  | max  | mean | Angle* |
                    // |---------------------|------|------|------|--------|
                    // | Perpendicular winds | 0.5  | 0.6  | 0.55 | 0      |
                    // | Diagonal winds      | 0.25 | 0.35 | 0.3  | 45     |
                    //
                    // * Angle is using our convention described above
                    constexpr Real64 slope = (0.3 - 0.55) / (45 - 0.0);
                    constexpr Real64 intercept = 0.55;
                    Cw = intercept + angle * slope;
                }
            }
            if (thisVentilation.DiscCoef != DataGlobalConstants::AutoCalculate) {
                Cd = thisVentilation.DiscCoef;
            } else {
                Cd = 0.40 + 0.0045 * std::abs(TempExt - thisMixingMAT);
            }
            Qw = Cw * thisVentilation.OpenArea * ScheduleManager::GetCurrentScheduleValue(state, thisVentilation.OpenAreaSchedPtr) * WindSpeedExt;
            Qst = Cd * thisVentilation.OpenArea * ScheduleManager::GetCurrentScheduleValue(state, thisVentilation.OpenAreaSchedPtr) *
                  std::sqrt(2.0 * 9.81 * thisVentilation.DH * std::abs(TempExt - thisMixingMAT) / (thisMixingMAT + 273.15));
            VVF = std::sqrt(Qw * Qw + Qst * Qst);
            if (thisVentilation.EMSSimpleVentOn) VVF = thisVentilation.EMSimpleVentFlowRate;
            if (VVF < 0.0) VVF = 0.0;
            thisVentilation.MCP = VVF * AirDensity * CpAir;
            if (thisVentilation.MCP < 0.0) thisVentilation.MCP = 0.0;
            if (state.dataHeatBal->Zone(zoneNum).zoneOAQuadratureSum) {
                state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Zone(zoneNum).zoneOABalanceIndex).NatMassFlowRate += thisVentilation.MCP / CpAir;
            } else {
                thisMCPV = thisVentilation.MCP;
                thisVAMFL = thisVentilation.MCP / CpAir;
                thisVentilation.AirTemp = TempExt;
                thisMCPTV = thisVentilation.MCP * thisVentilation.AirTemp;
            }
        }
        // Accumulate for zone and space
        thisZoneHB.MCPV += thisMCPV;
        thisZoneHB.VAMFL += thisVAMFL;
        thisZoneHB.MCPTV += thisMCPTV;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisVentilation.spaceIndex);
            thisSpaceHB.MCPV += thisMCPV;
            thisSpaceHB.VAMFL += thisVAMFL;
            thisSpaceHB.MCPTV += thisMCPTV;
        }
    }

    // Process Mixing
    for (int j = 1; j <= state.dataHeatBal->TotMixing; ++j) {
        auto &thisMixing = state.dataHeatBal->Mixing(j);
        int thisZoneNum = thisMixing.ZonePtr;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(thisZoneNum);
        int fromZoneNum = thisMixing.FromZone;
        Real64 TD = thisMixing.DeltaTemperature; // Delta Temp limit
        thisMixing.ReportFlag = false;

        // Get scheduled delta temperature
        if (thisMixing.DeltaTempSchedPtr > 0) {
            TD = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.DeltaTempSchedPtr);
        }
        Real64 TZN = 0.0;      // Temperature of this Zone/Space
        Real64 TZM = 0.0;      // Temperature of From Zone/Space
        Real64 HumRatZN = 0.0; // HumRat of this Zone/Space
        Real64 HumRatZM = 0.0; // HumRat of From Zone/Space
        if (state.dataHeatBal->doSpaceHeatBalance) {
            auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisMixing.spaceIndex);
            auto &fromSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisMixing.fromSpaceIndex);
            TZN = thisSpaceHB.MixingMAT;         // Temperature of this Space
            TZM = fromSpaceHB.MixingMAT;         // Temperature of From Space
            HumRatZN = thisSpaceHB.MixingHumRat; // HumRat of this Space
            HumRatZM = fromSpaceHB.MixingHumRat; // HumRat of From Space
        } else {
            auto &fromZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(fromZoneNum);
            TZN = thisZoneHB.MixingMAT;         // Temperature of this zone
            TZM = fromZoneHB.MixingMAT;         // Temperature of From Zone
            HumRatZN = thisZoneHB.MixingHumRat; // HumRat of this zone
            HumRatZM = fromZoneHB.MixingHumRat; // HumRat of From Zone
        }
        Real64 thisMCPM = 0.0;
        Real64 thisMCPTM = 0.0;
        Real64 thisMixingMassFlow = 0.0;
        Real64 thisMixingMassFlowXHumRat = 0.0;

        // Hybrid ventilation controls
        if (thisMixing.HybridControlType == DataHeatBalance::HybridCtrlType::Close) continue;
        // Check temperature limit
        bool MixingLimitFlag = false;

        // Hybrid ventilation global control
        int I = 0;
        if (thisMixing.HybridControlType == DataHeatBalance::HybridCtrlType::Global && thisMixing.HybridControlMasterNum > 0) {
            I = thisMixing.HybridControlMasterNum;
            if (!state.dataHeatBal->Ventilation(I).HybridControlMasterStatus) continue;
        } else {
            // Ensure the minimum indoor temperature <= the maximum indoor temperature
            Real64 MixingTmin = 0.0;
            Real64 MixingTmax = 0.0;
            if (thisMixing.MinIndoorTempSchedPtr > 0) MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MinIndoorTempSchedPtr);
            if (thisMixing.MaxIndoorTempSchedPtr > 0) MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MaxIndoorTempSchedPtr);
            if (thisMixing.MinIndoorTempSchedPtr > 0 && thisMixing.MaxIndoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisMixing.IndoorTempErrCount;
                    if (thisMixing.IndoorTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            format("Mixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in {}",
                                   thisMixing.Name));
                        ShowContinueError(state, "The minimum zone temperature is set to the maximum zone temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum zone temperature is still above the maximum zone temperature",
                                                       thisMixing.IndoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisMixing.MinIndoorTempSchedPtr > 0) {
                if (TZN < MixingTmin) MixingLimitFlag = true;
            }
            if (thisMixing.MaxIndoorTempSchedPtr > 0) {
                if (TZN > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum source temperature <= the maximum source temperature
            if (thisMixing.MinSourceTempSchedPtr > 0) MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MinSourceTempSchedPtr);
            if (thisMixing.MaxSourceTempSchedPtr > 0) MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MaxSourceTempSchedPtr);
            if (thisMixing.MinSourceTempSchedPtr > 0 && thisMixing.MaxSourceTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisMixing.SourceTempErrCount;
                    if (thisMixing.SourceTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            format("Mixing source temperature control: The minimum source temperature is above the maximum source temperature in {}",
                                   thisMixing.Name));
                        ShowContinueError(state, "The minimum source temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum source temperature is still above the maximum source temperature",
                                                       thisMixing.SourceTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisMixing.MinSourceTempSchedPtr > 0) {
                if (TZM < MixingTmin) MixingLimitFlag = true;
            }
            if (thisMixing.MaxSourceTempSchedPtr > 0) {
                if (TZM > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
            Real64 TempExt = state.dataHeatBal->Zone(thisZoneNum).OutDryBulbTemp;
            if (thisMixing.MinOutdoorTempSchedPtr > 0)
                MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MinOutdoorTempSchedPtr);
            if (thisMixing.MaxOutdoorTempSchedPtr > 0)
                MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisMixing.MaxOutdoorTempSchedPtr);
            if (thisMixing.MinOutdoorTempSchedPtr > 0 && thisMixing.MaxOutdoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisMixing.OutdoorTempErrCount;
                    if (thisMixing.OutdoorTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            format(
                                "Mixing outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in {}",
                                thisMixing.Name));
                        ShowContinueError(state, "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                       thisMixing.OutdoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisMixing.MinOutdoorTempSchedPtr > 0) {
                if (TempExt < MixingTmin) MixingLimitFlag = true;
            }
            if (thisMixing.MaxOutdoorTempSchedPtr > 0) {
                if (TempExt > MixingTmax) MixingLimitFlag = true;
            }
        }

        if (thisMixing.HybridControlType != DataHeatBalance::HybridCtrlType::Global && MixingLimitFlag) continue;
        if (thisMixing.HybridControlType == DataHeatBalance::HybridCtrlType::Global) TD = 0.0;

        //            Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
        Real64 AirDensity = PsyRhoAirFnPbTdbW(
            state, state.dataEnvrn->OutBaroPress, (TZN + TZM) / 2.0, (HumRatZN + HumRatZM) / 2.0, RoutineNameMixing); // Density of air (kg/m^3)
        Real64 CpAir = PsyCpAirFnW((HumRatZN + HumRatZM) / 2.0);                                                      // Use average conditions

        //  If TD equals zero (default) set coefficients for full mixing otherwise test
        //    for mixing conditions if user input delta temp > 0, then from zone temp (TZM)
        //    must be td degrees warmer than zone temp (TZN).  If user input delta temp < 0,
        //    then from zone temp (TZM) must be TD degrees cooler than zone temp (TZN).
        if (TD < 0.0) {
            if (TZM < TZN + TD) {

                thisMixing.DesiredAirFlowRate = thisMixing.DesiredAirFlowRateSaved;
                if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(thisZoneNum) && AdjustZoneMixingFlowFlag) {
                    if (thisMixing.MixingMassFlowRate > 0.0) {
                        thisMixing.DesiredAirFlowRate = thisMixing.MixingMassFlowRate / AirDensity;
                    }
                }
                thisMixing.MixingMassFlowRate = thisMixing.DesiredAirFlowRate * AirDensity;

                thisMCPM = thisMixing.MixingMassFlowRate * CpAir;
                thisMCPTM = thisMCPM * TZN;

                // Now to determine the moisture conditions
                thisMixingMassFlow = thisMixing.DesiredAirFlowRate * AirDensity;
                thisMixingMassFlowXHumRat = thisMixing.DesiredAirFlowRate * AirDensity * HumRatZM;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(thisZoneNum) +=
                        thisMixing.DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirCO2(fromZoneNum);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(thisZoneNum) +=
                        thisMixing.DesiredAirFlowRate * AirDensity * state.dataContaminantBalance->ZoneAirGC(fromZoneNum);
                }
                thisMixing.ReportFlag = true;
            }
        } else if (TD > 0.0) {
            if (TZM > TZN + TD) {
                thisMixing.DesiredAirFlowRate = thisMixing.DesiredAirFlowRateSaved;
                if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(thisZoneNum) && AdjustZoneMixingFlowFlag) {
                    if (thisMixing.MixingMassFlowRate > 0.0) {
                        thisMixing.DesiredAirFlowRate = thisMixing.MixingMassFlowRate / AirDensity;
                    }
                }
                thisMixing.MixingMassFlowRate = thisMixing.DesiredAirFlowRate * AirDensity;

                thisMCPM = thisMixing.MixingMassFlowRate * CpAir;
                thisMCPTM = thisMCPM * TZM;
                // Now to determine the moisture conditions
                thisMixingMassFlow = thisMixing.MixingMassFlowRate;
                thisMixingMassFlowXHumRat = thisMixing.MixingMassFlowRate * HumRatZM;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(thisZoneNum) +=
                        thisMixing.MixingMassFlowRate * state.dataContaminantBalance->ZoneAirCO2(fromZoneNum);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(thisZoneNum) +=
                        thisMixing.MixingMassFlowRate * state.dataContaminantBalance->ZoneAirGC(fromZoneNum);
                }
                thisMixing.ReportFlag = true;
            }
        } else if (TD == 0.0) {
            thisMixing.DesiredAirFlowRate = thisMixing.DesiredAirFlowRateSaved;
            if (state.dataHeatBalFanSys->ZoneMassBalanceFlag(thisZoneNum) && AdjustZoneMixingFlowFlag) {
                if (thisMixing.MixingMassFlowRate > 0.0) {
                    thisMixing.DesiredAirFlowRate = thisMixing.MixingMassFlowRate / AirDensity;
                }
            }
            thisMixing.MixingMassFlowRate = thisMixing.DesiredAirFlowRate * AirDensity;

            thisMCPM = thisMixing.MixingMassFlowRate * CpAir;
            thisMCPTM = thisMCPM * TZM;
            // Now to determine the moisture conditions
            thisMixingMassFlow = thisMixing.MixingMassFlowRate;
            thisMixingMassFlowXHumRat = thisMixing.MixingMassFlowRate * HumRatZM;
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->MixingMassFlowCO2(thisZoneNum) +=
                    thisMixing.MixingMassFlowRate * state.dataContaminantBalance->ZoneAirCO2(fromZoneNum);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataContaminantBalance->MixingMassFlowGC(thisZoneNum) +=
                    thisMixing.MixingMassFlowRate * state.dataContaminantBalance->ZoneAirGC(fromZoneNum);
            }
            thisMixing.ReportFlag = true;
        }
        // Accumulate for zone and space
        thisZoneHB.MCPM += thisMCPM;
        thisZoneHB.MCPTM += thisMCPTM;
        thisZoneHB.MixingMassFlowZone += thisMixingMassFlow;
        thisZoneHB.MixingMassFlowXHumRat += thisMixingMassFlowXHumRat;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisMixing.spaceIndex);
            thisSpaceHB.MCPM += thisMCPM;
            thisSpaceHB.MCPTM += thisMCPTM;
            thisSpaceHB.MixingMassFlowZone += thisMixingMassFlow;
            thisSpaceHB.MixingMassFlowXHumRat += thisMixingMassFlowXHumRat;
        }
    }

    //                              COMPUTE CROSS ZONE
    //                              AIR MIXING
    for (int j = 1; j <= state.dataHeatBal->TotCrossMixing; ++j) {
        auto &thisCrossMixing = state.dataHeatBal->CrossMixing(j);
        int thisZoneNum = thisCrossMixing.ZonePtr;
        thisCrossMixing.ReportFlag = false;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(thisZoneNum);
        int fromZoneNum = thisCrossMixing.FromZone;
        auto &fromZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(fromZoneNum);
        Real64 TD = thisCrossMixing.DeltaTemperature; // Delta Temp limit
        // Get scheduled delta temperature
        if (thisCrossMixing.DeltaTempSchedPtr > 0) {
            TD = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.DeltaTempSchedPtr);
        }
        Real64 thisMCPxM = 0.0;
        Real64 thisMCPTxM = 0.0;
        Real64 thisXMixingMassFlow = 0.0;
        Real64 thisXMixingMassFlowXHumRat = 0.0;
        Real64 fromMCPxM = 0.0;
        Real64 fromMCPTxM = 0.0;
        Real64 fromXMixingMassFlowXHumRat = 0.0;

        if (TD >= 0.0) {
            Real64 TZN = 0.0;      // Temperature of this Zone/Space
            Real64 TZM = 0.0;      // Temperature of From Zone/Space
            Real64 HumRatZN = 0.0; // HumRat of this Zone/Space
            Real64 HumRatZM = 0.0; // HumRat of From Zone/Space
            if (state.dataHeatBal->doSpaceHeatBalance) {
                auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisCrossMixing.spaceIndex);
                auto &fromSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisCrossMixing.fromSpaceIndex);
                TZN = thisSpaceHB.MixingMAT;         // Temperature of this Space
                TZM = fromSpaceHB.MixingMAT;         // Temperature of From Space
                HumRatZN = thisSpaceHB.MixingHumRat; // HumRat of this Space
                HumRatZM = fromSpaceHB.MixingHumRat; // HumRat of From Space
            } else {
                TZN = thisZoneHB.MixingMAT;         // Temperature of this zone
                TZM = fromZoneHB.MixingMAT;         // Temperature of From Zone
                HumRatZN = thisZoneHB.MixingHumRat; // HumRat of this zone
                HumRatZM = fromZoneHB.MixingHumRat; // HumRat of From Zone
            }
            // Check temperature limit
            bool MixingLimitFlag = false;
            // Ensure the minimum indoor temperature <= the maximum indoor temperature
            Real64 MixingTmin = 0.0;
            Real64 MixingTmax = 0.0;
            if (thisCrossMixing.MinIndoorTempSchedPtr > 0)
                MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MinIndoorTempSchedPtr);
            if (thisCrossMixing.MaxIndoorTempSchedPtr > 0)
                MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MaxIndoorTempSchedPtr);
            if (thisCrossMixing.MinIndoorTempSchedPtr > 0 && thisCrossMixing.MaxIndoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisCrossMixing.IndoorTempErrCount;
                    if (thisCrossMixing.IndoorTempErrCount < 2) {
                        ShowWarningError(
                            state,
                            format("CrossMixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in {}",
                                   thisCrossMixing.Name));
                        ShowContinueError(state, "The minimum zone temperature is set to the maximum zone temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum zone temperature is still above the maximum zone temperature",
                                                       thisCrossMixing.IndoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisCrossMixing.MinIndoorTempSchedPtr > 0) {
                if (TZN < MixingTmin) MixingLimitFlag = true;
            }
            if (thisCrossMixing.MaxIndoorTempSchedPtr > 0) {
                if (TZN > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum source temperature <= the maximum source temperature
            if (thisCrossMixing.MinSourceTempSchedPtr > 0)
                MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MinSourceTempSchedPtr);
            if (thisCrossMixing.MaxSourceTempSchedPtr > 0)
                MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MaxSourceTempSchedPtr);
            if (thisCrossMixing.MinSourceTempSchedPtr > 0 && thisCrossMixing.MaxSourceTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisCrossMixing.SourceTempErrCount;
                    if (thisCrossMixing.SourceTempErrCount < 2) {
                        ShowWarningError(state,
                                         format("CrossMixing source temperature control: The minimum source temperature is above the maximum source "
                                                "temperature in {}",
                                                thisCrossMixing.Name));
                        ShowContinueError(state, "The minimum source temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum source temperature is still above the maximum source temperature",
                                                       thisCrossMixing.SourceTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisCrossMixing.MinSourceTempSchedPtr > 0) {
                if (TZM < MixingTmin) MixingLimitFlag = true;
            }
            if (thisCrossMixing.MaxSourceTempSchedPtr > 0) {
                if (TZM > MixingTmax) MixingLimitFlag = true;
            }
            // Ensure the minimum outdoor temperature <= the maximum outdoor temperature
            Real64 TempExt = state.dataHeatBal->Zone(thisZoneNum).OutDryBulbTemp;
            if (thisCrossMixing.MinOutdoorTempSchedPtr > 0)
                MixingTmin = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MinOutdoorTempSchedPtr);
            if (thisCrossMixing.MaxOutdoorTempSchedPtr > 0)
                MixingTmax = ScheduleManager::GetCurrentScheduleValue(state, thisCrossMixing.MaxOutdoorTempSchedPtr);
            if (thisCrossMixing.MinOutdoorTempSchedPtr > 0 && thisCrossMixing.MaxOutdoorTempSchedPtr > 0) {
                if (MixingTmin > MixingTmax) {
                    ++thisCrossMixing.OutdoorTempErrCount;
                    if (thisCrossMixing.OutdoorTempErrCount < 2) {
                        ShowWarningError(state,
                                         format("CrossMixing outdoor temperature control: The minimum outdoor temperature is above the maximum "
                                                "outdoor temperature in {}",
                                                state.dataHeatBal->Mixing(j).Name));
                        ShowContinueError(state, "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues.");
                        ShowContinueErrorTimeStamp(state, " Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "The minimum outdoor temperature is still above the maximum outdoor temperature",
                                                       thisCrossMixing.OutdoorTempErrIndex,
                                                       MixingTmin,
                                                       MixingTmin);
                    }
                    MixingTmin = MixingTmax;
                }
            }
            if (thisCrossMixing.MinOutdoorTempSchedPtr > 0) {
                if (TempExt < MixingTmin) MixingLimitFlag = true;
            }
            if (thisCrossMixing.MaxOutdoorTempSchedPtr > 0) {
                if (TempExt > MixingTmax) MixingLimitFlag = true;
            }
            if (MixingLimitFlag) continue;

            if ((TD == 0.0 || (TD > 0.0 && (TZM - TZN) >= TD))) {
                thisCrossMixing.ReportFlag = true;
            }

            if ((TD <= 0.0) || ((TD > 0.0) && (TZM - TZN >= TD))) {
                //                                      SET COEFFICIENTS .
                Real64 Tavg = (TZN + TZM) / 2.0;
                Real64 Wavg = (HumRatZN + HumRatZM) / 2.0;
                Real64 AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tavg, Wavg, RoutineNameCrossMixing);
                Real64 CpAir = PsyCpAirFnW(Wavg);
                thisXMixingMassFlow = thisCrossMixing.DesiredAirFlowRate * AirDensity;
                thisMCPxM = thisXMixingMassFlow * CpAir;

                fromMCPxM = thisMCPxM;
                thisMCPTxM = thisMCPxM * TZM;
                fromMCPTxM = fromMCPxM * TZN;

                // Now to determine the moisture conditions
                fromXMixingMassFlowXHumRat = thisXMixingMassFlow * HumRatZN;
                thisXMixingMassFlowXHumRat = thisXMixingMassFlow * HumRatZM;

                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataContaminantBalance->MixingMassFlowCO2(fromZoneNum) +=
                        thisXMixingMassFlow * state.dataContaminantBalance->ZoneAirCO2(thisZoneNum);
                    state.dataContaminantBalance->MixingMassFlowCO2(thisZoneNum) +=
                        thisXMixingMassFlow * state.dataContaminantBalance->ZoneAirCO2(fromZoneNum);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->MixingMassFlowGC(fromZoneNum) +=
                        thisXMixingMassFlow * state.dataContaminantBalance->ZoneAirGC(thisZoneNum);
                    state.dataContaminantBalance->MixingMassFlowGC(thisZoneNum) +=
                        thisXMixingMassFlow * state.dataContaminantBalance->ZoneAirGC(fromZoneNum);
                }
            }
        }
        // Accumulate for zone and space
        thisZoneHB.MCPM += thisMCPxM;
        thisZoneHB.MCPTM += thisMCPTxM;
        thisZoneHB.MixingMassFlowZone += thisXMixingMassFlow;
        thisZoneHB.MixingMassFlowXHumRat += thisXMixingMassFlowXHumRat;
        fromZoneHB.MCPM += fromMCPxM;
        fromZoneHB.MCPTM += fromMCPTxM;
        fromZoneHB.MixingMassFlowZone += thisXMixingMassFlow;
        fromZoneHB.MixingMassFlowXHumRat += fromXMixingMassFlowXHumRat;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisCrossMixing.spaceIndex);
            auto &fromSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisCrossMixing.fromSpaceIndex);
            thisSpaceHB.MCPM += thisMCPxM;
            thisSpaceHB.MCPTM += thisMCPTxM;
            thisSpaceHB.MixingMassFlowZone += thisXMixingMassFlow;
            thisSpaceHB.MixingMassFlowXHumRat += thisXMixingMassFlowXHumRat;
            fromSpaceHB.MCPM += fromMCPxM;
            fromSpaceHB.MCPTM += fromMCPTxM;
            fromSpaceHB.MixingMassFlowZone += thisXMixingMassFlow;
            fromSpaceHB.MixingMassFlowXHumRat += fromXMixingMassFlowXHumRat;
        }
    }

    //                              COMPUTE REFRIGERATION DOOR
    //                              AIR MIXING
    if (state.dataHeatBal->TotRefDoorMixing > 0) {
        // Zone loops structured in getinput so only do each pair of zones bounding door once, even if multiple doors in one zone
        for (int ZoneA = 1; ZoneA <= (state.dataGlobal->NumOfZones - 1); ++ZoneA) {
            if (!state.dataHeatBal->RefDoorMixing(ZoneA).RefDoorMixFlag) continue;
            auto &zoneAHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneA);
            Real64 TZoneA = zoneAHB.MixingMAT;
            Real64 HumRatZoneA = zoneAHB.MixingHumRat;
            Real64 AirDensityZoneA = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TZoneA, HumRatZoneA, RoutineNameRefrigerationDoorMixing);
            Real64 CpAirZoneA = PsyCpAirFnW(HumRatZoneA);
            for (int j = 1; j <= state.dataHeatBal->RefDoorMixing(ZoneA).NumRefDoorConnections; ++j) {
                int ZoneB = state.dataHeatBal->RefDoorMixing(ZoneA).MateZonePtr(j);
                auto &zoneBHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneB);
                Real64 TZoneB = zoneBHB.MixingMAT;
                Real64 HumRatZoneB = zoneBHB.MixingHumRat;
                Real64 CpAirZoneB = PsyCpAirFnW(HumRatZoneB);
                Real64 Tavg = (TZoneA + TZoneB) / 2.0;
                Real64 Wavg = (HumRatZoneA + HumRatZoneB) / 2.0;
                Real64 AirDensityAvg = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tavg, Wavg, RoutineNameRefrigerationDoorMixing);
                // following variables used for refrigeration door mixing and all defined in EngRef
                Real64 MassFlowDryAir = 0.0;
                Real64 FDens = 0.0;
                Real64 Fb = 0.0;

                if (state.dataHeatBal->RefDoorMixing(ZoneA).EMSRefDoorMixingOn(j)) {
                    MassFlowDryAir = state.dataHeatBal->RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensityAvg;
                } else {
                    Real64 AirDensityZoneB =
                        PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TZoneB, HumRatZoneB, RoutineNameRefrigerationDoorMixing);
                    Real64 SchedDoorOpen = ScheduleManager::GetCurrentScheduleValue(state, state.dataHeatBal->RefDoorMixing(ZoneA).OpenSchedPtr(j));
                    if (SchedDoorOpen == 0.0) continue;
                    Real64 DoorHeight = state.dataHeatBal->RefDoorMixing(ZoneA).DoorHeight(j);
                    Real64 DoorArea = state.dataHeatBal->RefDoorMixing(ZoneA).DoorArea(j);
                    Real64 DoorProt = state.dataHeatBal->RefDoorMixing(ZoneA).Protection(j);
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
                    Real64 FFlow = 1.1;
                    if (std::abs(TZoneA - TZoneB) > 11.0) FFlow = 0.8;
                    MassFlowDryAir = Fb * SchedDoorOpen * FFlow * (1.0 - DoorProt);
                    state.dataHeatBal->RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) = MassFlowDryAir / AirDensityAvg;
                    // Note - VolRefDoorFlowRate is used ONLY for reporting purposes, where it is
                    //       used with the avg density to generate a reported mass flow
                    //       Considering the small values typical for HumRat, this is not far off.
                } // EMSRefDoorMixingOn

                Real64 MassFlowToA = MassFlowDryAir * (1.0 + HumRatZoneB);
                Real64 MassFlowToB = MassFlowDryAir * (1.0 + HumRatZoneA);
                Real64 MassFlowXCpToA = MassFlowToA * CpAirZoneB;
                Real64 MassFlowXCpToB = MassFlowToB * CpAirZoneA;
                Real64 MassFlowXCpXTempToA = MassFlowXCpToA * TZoneB;
                Real64 MassFlowXCpXTempToB = MassFlowXCpToB * TZoneA;
                Real64 MassFlowXHumRatToA = MassFlowToA * HumRatZoneB;
                Real64 MassFlowXHumRatToB = MassFlowToB * HumRatZoneA;

                zoneAHB.MCPM += MassFlowXCpToA;
                zoneBHB.MCPM += MassFlowXCpToB;
                zoneAHB.MCPTM += MassFlowXCpXTempToA;
                zoneBHB.MCPTM += MassFlowXCpXTempToB;
                zoneAHB.MixingMassFlowZone += MassFlowToA;
                zoneBHB.MixingMassFlowZone += MassFlowToB;
                zoneAHB.MixingMassFlowXHumRat += MassFlowXHumRatToA;
                zoneBHB.MixingMassFlowXHumRat += MassFlowXHumRatToB;
                if (state.dataHeatBal->doSpaceHeatBalance) {
                    // ZoneRefrigerationDoorMixing has no space information, just zones
                    // Allocate mixing flows by space volume fraction of zone volume
                    for (int spaceNum : state.dataHeatBal->Zone(ZoneA).spaceIndexes) {
                        Real64 spaceFrac = state.dataHeatBal->space(spaceNum).fracZoneVolume;
                        auto &spaceAHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
                        spaceAHB.MCPM += MassFlowXCpToA * spaceFrac;
                        spaceAHB.MCPTM += MassFlowXCpXTempToA * spaceFrac;
                        spaceAHB.MixingMassFlowZone += MassFlowToA * spaceFrac;
                        spaceAHB.MixingMassFlowXHumRat += MassFlowXHumRatToA * spaceFrac;
                    }
                    for (int spaceNum : state.dataHeatBal->Zone(ZoneB).spaceIndexes) {
                        Real64 spaceFrac = state.dataHeatBal->space(spaceNum).fracZoneVolume;
                        auto &spaceBHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
                        spaceBHB.MCPM += MassFlowXCpToB * spaceFrac;
                        spaceBHB.MCPTM += MassFlowXCpXTempToB * spaceFrac;
                        spaceBHB.MixingMassFlowZone += MassFlowToB * spaceFrac;
                        spaceBHB.MixingMassFlowXHumRat += MassFlowXHumRatToB * spaceFrac;
                    }
                }

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
    for (int j = 1; j <= state.dataHeatBal->TotInfiltration; ++j) {

        auto &thisInfiltration = state.dataHeatBal->Infiltration(j);
        int NZ = state.dataHeatBal->Infiltration(j).ZonePtr;
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(NZ);
        Real64 tempInt = 0.0;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            tempInt = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisInfiltration.spaceIndex).MixingMAT;
        } else {
            tempInt = thisZoneHB.MixingMAT;
        }

        Real64 TempExt = state.dataHeatBal->Zone(NZ).OutDryBulbTemp;
        Real64 WindSpeedExt = state.dataHeatBal->Zone(NZ).WindSpeed;

        // Use air node information linked to the zone if defined
        Real64 HumRatExt = 0.0;
        if (state.dataHeatBal->Zone(NZ).LinkedOutAirNode > 0) {
            HumRatExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(NZ).LinkedOutAirNode).HumRat;
        } else {
            HumRatExt = state.dataEnvrn->OutHumRat;
        }

        Real64 AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempExt, HumRatExt, RoutineNameInfiltration);
        Real64 CpAir = PsyCpAirFnW(HumRatExt);
        Real64 MCpI_temp = 0.0;
        Real64 scheduleFrac = ScheduleManager::GetCurrentScheduleValue(state, thisInfiltration.SchedPtr);
        if (scheduleFrac > 0.0) {
            // CR7751  should maybe use code below, indoor conditions instead of outdoor conditions
            //   AirDensity = PsyRhoAirFnPbTdbW(state, OutBaroPress, MixingMAT(NZ), MixingHumRat(NZ))
            //   CpAir = PsyCpAirFnW(MixingHumRat(NZ),MixingMAT(NZ))
            switch (thisInfiltration.ModelType) {
            case DataHeatBalance::InfiltrationModelType::DesignFlowRate: {
                IVF = thisInfiltration.DesignLevel * scheduleFrac;
                // CR6845 if calculated < 0.0, don't propagate
                if (IVF < 0.0) IVF = 0.0;
                MCpI_temp = IVF * AirDensity * CpAir *
                            (thisInfiltration.ConstantTermCoef + std::abs(TempExt - tempInt) * thisInfiltration.TemperatureTermCoef +
                             WindSpeedExt * (thisInfiltration.VelocityTermCoef + WindSpeedExt * thisInfiltration.VelocitySQTermCoef));

                if (MCpI_temp < 0.0) MCpI_temp = 0.0;
                thisInfiltration.VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            } break;
            case DataHeatBalance::InfiltrationModelType::ShermanGrimsrud: {
                // Sherman Grimsrud model as formulated in ASHRAE HoF
                WindSpeedExt = state.dataEnvrn->WindSpeed; // formulated to use wind at Meterological Station rather than local
                IVF = scheduleFrac * thisInfiltration.LeakageArea / 1000.0 *
                      std::sqrt(thisInfiltration.BasicStackCoefficient * std::abs(TempExt - tempInt) +
                                thisInfiltration.BasicWindCoefficient * pow_2(WindSpeedExt));
                if (IVF < 0.0) IVF = 0.0;
                MCpI_temp = IVF * AirDensity * CpAir;
                if (MCpI_temp < 0.0) MCpI_temp = 0.0;
                thisInfiltration.VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            } break;
            case DataHeatBalance::InfiltrationModelType::AIM2: {
                // Walker Wilson model as formulated in ASHRAE HoF
                IVF =
                    scheduleFrac * std::sqrt(pow_2(thisInfiltration.FlowCoefficient * thisInfiltration.AIM2StackCoefficient *
                                                   std::pow(std::abs(TempExt - tempInt), thisInfiltration.PressureExponent)) +
                                             pow_2(thisInfiltration.FlowCoefficient * thisInfiltration.AIM2WindCoefficient *
                                                   std::pow(thisInfiltration.ShelterFactor * WindSpeedExt, 2.0 * thisInfiltration.PressureExponent)));
                if (IVF < 0.0) IVF = 0.0;
                MCpI_temp = IVF * AirDensity * CpAir;
                if (MCpI_temp < 0.0) MCpI_temp = 0.0;
                thisInfiltration.VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
            } break;
            default:
                break;
            }
        } else {
            thisInfiltration.VolumeFlowRate = 0.0;
            MCpI_temp = 0.0;
        }

        if (AdjustZoneInfiltrationFlowFlag && state.dataHeatBalFanSys->ZoneInfiltrationFlag(NZ)) {
            if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Adjust) {
                if (thisInfiltration.MassFlowRate > 0.0 || thisInfiltration.ModelType == DataHeatBalance::InfiltrationModelType::DesignFlowRate) {
                    // For DesignFlowRate, allow exfiltraion
                    thisInfiltration.VolumeFlowRate = thisInfiltration.MassFlowRate / AirDensity;
                    MCpI_temp = thisInfiltration.VolumeFlowRate * AirDensity * CpAir;
                }
            }
            if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment == DataHeatBalance::InfiltrationFlow::Add) {
                thisInfiltration.VolumeFlowRate =
                    thisInfiltration.VolumeFlowRate + state.dataHeatBal->MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
                MCpI_temp = thisInfiltration.VolumeFlowRate * AirDensity * CpAir;
            }
        }
        thisInfiltration.MassFlowRate = thisInfiltration.VolumeFlowRate * AirDensity;

        if (thisInfiltration.EMSOverrideOn) {
            IVF = thisInfiltration.EMSAirFlowRateValue;
            if (IVF < 0.0) IVF = 0.0;
            MCpI_temp = IVF * AirDensity * CpAir;
            if (MCpI_temp < 0.0) MCpI_temp = 0.0;
        }

        if (state.dataHeatBal->Zone(NZ).zoneOAQuadratureSum) {
            state.dataHeatBal->ZoneAirBalance(state.dataHeatBal->Zone(NZ).zoneOABalanceIndex).InfMassFlowRate += MCpI_temp / CpAir;
        } else {
            thisInfiltration.MCpI_temp = MCpI_temp;
            thisZoneHB.MCPI += MCpI_temp;
            thisZoneHB.OAMFL += MCpI_temp / CpAir;
            thisZoneHB.MCPTI += MCpI_temp * TempExt;
            if (state.dataHeatBal->doSpaceHeatBalance) {
                auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(thisInfiltration.spaceIndex);
                thisSpaceHB.MCPI += MCpI_temp;
                thisSpaceHB.OAMFL += MCpI_temp / CpAir;
                thisSpaceHB.MCPTI += MCpI_temp * TempExt;
            }
        }
    }

    // Add infiltration rate enhanced by the existence of thermal chimney
    for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
        thisZoneHB.MCPI += thisZoneHB.MCPThermChim;
        thisZoneHB.OAMFL += thisZoneHB.ThermChimAMFL;
        thisZoneHB.MCPTI += thisZoneHB.MCPTThermChim;
    }
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
            thisSpaceHB.MCPI += thisSpaceHB.MCPThermChim;
            thisSpaceHB.OAMFL += thisSpaceHB.ThermChimAMFL;
            thisSpaceHB.MCPTI += thisSpaceHB.MCPTThermChim;
        }
    }

    // Calculate combined outdoor air flows
    for (auto &thisZoneAirBalance : state.dataHeatBal->ZoneAirBalance) {
        if (thisZoneAirBalance.BalanceMethod == DataHeatBalance::AirBalance::Quadrature) {
            if (!thisZoneAirBalance.OneTimeFlag) GetStandAloneERVNodes(state, thisZoneAirBalance);
            if (thisZoneAirBalance.NumOfERVs > 0) {
                for (int I = 1; I <= thisZoneAirBalance.NumOfERVs; ++I) {
                    Real64 MassFlowDiff = state.dataLoopNodes->Node(thisZoneAirBalance.ERVExhaustNode(I)).MassFlowRate -
                                          state.dataLoopNodes->Node(thisZoneAirBalance.ERVInletNode(I)).MassFlowRate;
                    if (MassFlowDiff > 0.0) {
                        thisZoneAirBalance.ERVMassFlowRate += MassFlowDiff;
                    }
                }
            }
            int NZ = thisZoneAirBalance.ZonePtr;
            auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(NZ);
            // Use air node information linked to the zone if defined
            Real64 HumRatExt = 0.0;
            if (state.dataHeatBal->Zone(NZ).LinkedOutAirNode > 0) {
                HumRatExt = state.dataLoopNodes->Node(state.dataHeatBal->Zone(NZ).LinkedOutAirNode).HumRat;
            } else {
                HumRatExt = state.dataEnvrn->OutHumRat;
            }
            Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataHeatBal->Zone(NZ).OutDryBulbTemp, HumRatExt, RoutineNameZoneAirBalance);
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(HumRatExt);
            thisZoneAirBalance.ERVMassFlowRate *= AirDensity;
            thisZoneHB.MDotOA = std::sqrt(pow_2(thisZoneAirBalance.NatMassFlowRate) + pow_2(thisZoneAirBalance.IntMassFlowRate) +
                                          pow_2(thisZoneAirBalance.ExhMassFlowRate) + pow_2(thisZoneAirBalance.ERVMassFlowRate) +
                                          pow_2(thisZoneAirBalance.InfMassFlowRate) +
                                          pow_2(AirDensity * thisZoneAirBalance.InducedAirRate *
                                                ScheduleManager::GetCurrentScheduleValue(state, thisZoneAirBalance.InducedAirSchedPtr))) +
                                thisZoneAirBalance.BalMassFlowRate;
            thisZoneHB.MDotCPOA = thisZoneHB.MDotOA * CpAir;
        }
    }
}

void GetStandAloneERVNodes(EnergyPlusData &state, DataHeatBalance::ZoneAirBalanceData &thisZoneAirBalance)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   July 2010

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets node numbers of stand alone ERVs to calculate combined outdoor air flows.

    // METHODOLOGY EMPLOYED:
    // Uses program data structures ZoneEquipInfo

    if (allocated(state.dataZoneEquip->ZoneEquipList)) {
        int ZoneNum = thisZoneAirBalance.ZonePtr;
        thisZoneAirBalance.OneTimeFlag = true;
        if (state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes > 0) {
            for (int I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
                if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipTypeEnum(I) == DataZoneEquipment::ZoneEquip::ERVStandAlone) {
                    ++thisZoneAirBalance.NumOfERVs;
                }
            }
            if (thisZoneAirBalance.NumOfERVs > 0) {
                thisZoneAirBalance.ERVInletNode.allocate(thisZoneAirBalance.NumOfERVs);
                thisZoneAirBalance.ERVExhaustNode.allocate(thisZoneAirBalance.NumOfERVs);
                int j = 1;
                for (int I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
                    if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipTypeEnum(I) == DataZoneEquipment::ZoneEquip::ERVStandAlone) {
                        thisZoneAirBalance.ERVInletNode(j) =
                            HVACStandAloneERV::GetStandAloneERVOutAirNode(state, state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipIndex(I));
                        thisZoneAirBalance.ERVExhaustNode(j) =
                            HVACStandAloneERV::GetStandAloneERVReturnAirNode(state, state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipIndex(I));
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

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the receiving zone mixing flow rate to ensures the zone air mass balance.

    auto &massConservation = state.dataHeatBal->MassConservation(ZoneNum);

    Real64 MixingMassFlowRate = 0.0; // current zone mixing mass flow rate, [kg/s]
    int NumOfReceivingZoneMixingObjects = massConservation.NumReceivingZonesMixingObject;
    if (NumOfReceivingZoneMixingObjects > 0) {
        // distribute the total zone mixing flow rate to the source zones
        for (int Loop = 1; Loop <= NumOfReceivingZoneMixingObjects; ++Loop) {
            int MixingNum = massConservation.ZoneMixingReceivingPtr(Loop);
            state.dataHeatBal->Mixing(MixingNum).MixingMassFlowRate = massConservation.ZoneMixingReceivingFr(Loop) * ZoneMixingMassFlowRate;
            MixingMassFlowRate += state.dataHeatBal->Mixing(MixingNum).MixingMassFlowRate;
            CalcZoneMixingFlowRateOfSourceZone(state, state.dataHeatBal->Mixing(MixingNum).FromZone);
        }
    }
    massConservation.MixingMassFlowRate = MixingMassFlowRate;
    ZoneMixingMassFlowRate = MixingMassFlowRate;
}

void CalcZoneMixingFlowRateOfSourceZone(EnergyPlusData &state, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   February 2014

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the zone mixing flow rate such that it ensures the zone air mass balance.

    auto &massConservation = state.dataHeatBal->MassConservation(ZoneNum);

    Real64 ZoneSourceMassFlowRate = 0.0; // current zone as a source mass flow rate for zone mixing in other zones, [kg/s]
    int NumOfSourceZoneMixingObjects = massConservation.NumSourceZonesMixingObject;
    if (NumOfSourceZoneMixingObjects > 0) {
        for (int ZoneMixingNum = 1; ZoneMixingNum <= NumOfSourceZoneMixingObjects; ++ZoneMixingNum) {
            int MixingNum = massConservation.ZoneMixingSourcesPtr(ZoneMixingNum);
            for (int Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                if (Loop == MixingNum) {
                    ZoneSourceMassFlowRate += state.dataHeatBal->Mixing(Loop).MixingMassFlowRate;
                }
            }
        }
    }
    massConservation.MixingSourceMassFlowRate = ZoneSourceMassFlowRate;
}

void AutoCalcDOASControlStrategy(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2016

    // PURPOSE OF THIS Function:
    // This subroutine does the autosizing calculations for the Sizing:Zone DOAS input.

    // REFERENCES:
    // See IO Ref for suggested values

    bool headerAlreadyPrinted = false;
    bool ErrorsFound = false;
    for (int ZoneSizIndex = 1; ZoneSizIndex <= state.dataSize->NumZoneSizingInput; ++ZoneSizIndex) {
        if (state.dataSize->ZoneSizingInput(ZoneSizIndex).AccountForDOAS) {
            auto &zoneSizingInput = state.dataSize->ZoneSizingInput(ZoneSizIndex);
            if (zoneSizingInput.DOASControlStrategy == DOASControl::NeutralSup) {
                if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASLowSetpoint = 21.1;
                    zoneSizingInput.DOASHighSetpoint = 23.9;
                } else if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint > 0.0) {
                    zoneSizingInput.DOASLowSetpoint = zoneSizingInput.DOASHighSetpoint - 2.8;
                } else if (zoneSizingInput.DOASLowSetpoint > 0.0 && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASHighSetpoint = zoneSizingInput.DOASLowSetpoint + 2.8;
                }
                ReportZoneSizingDOASInputs(state,
                                           zoneSizingInput.ZoneName,
                                           "NeutralSupplyAir",
                                           zoneSizingInput.DOASLowSetpoint,
                                           zoneSizingInput.DOASHighSetpoint,
                                           headerAlreadyPrinted);
            } else if (zoneSizingInput.DOASControlStrategy == DataSizing::DOASControl::NeutralDehumSup) {
                if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASLowSetpoint = 14.4;
                    zoneSizingInput.DOASHighSetpoint = 22.2;
                } else if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint > 0.0) {
                    zoneSizingInput.DOASLowSetpoint = 14.4;
                } else if (zoneSizingInput.DOASLowSetpoint > 0.0 && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASHighSetpoint = 22.2;
                }
                ReportZoneSizingDOASInputs(state,
                                           zoneSizingInput.ZoneName,
                                           "NeutralDehumidifiedSupplyAir",
                                           zoneSizingInput.DOASLowSetpoint,
                                           zoneSizingInput.DOASHighSetpoint,
                                           headerAlreadyPrinted);
            } else if (zoneSizingInput.DOASControlStrategy == DOASControl::CoolSup) {
                if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASLowSetpoint = 12.2;
                    zoneSizingInput.DOASHighSetpoint = 14.4;
                } else if (zoneSizingInput.DOASLowSetpoint == AutoSize && zoneSizingInput.DOASHighSetpoint > 0.0) {
                    zoneSizingInput.DOASLowSetpoint = zoneSizingInput.DOASHighSetpoint - 2.2;
                } else if (zoneSizingInput.DOASLowSetpoint > 0.0 && zoneSizingInput.DOASHighSetpoint == AutoSize) {
                    zoneSizingInput.DOASHighSetpoint = zoneSizingInput.DOASLowSetpoint + 2.2;
                }
                ReportZoneSizingDOASInputs(state,
                                           zoneSizingInput.ZoneName,
                                           "ColdSupplyAir",
                                           zoneSizingInput.DOASLowSetpoint,
                                           zoneSizingInput.DOASHighSetpoint,
                                           headerAlreadyPrinted);
            }
            if (zoneSizingInput.DOASLowSetpoint > zoneSizingInput.DOASHighSetpoint) {
                ShowSevereError(state, format("For Sizing:Zone = {}", zoneSizingInput.ZoneName));
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

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes the DOAS Sizing:Zone input for 1 zone to the eio file

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
    //     state.dataSQLiteProcedures->sqlite->addSQLiteZoneSizingRecord( ZoneName, LoadType, CalcDesLoad, UserDesLoad, CalcDesFlow,
    //     UserDesFlow, DesDayName, PeakHrMin,
    //         PeakTemp, PeakHumRat, MinOAVolFlow, DOASHeatAddRate );
    // }
    // BSLLC Finish
}

} // namespace EnergyPlus::ZoneEquipmentManager
