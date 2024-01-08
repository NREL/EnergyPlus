// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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
        for (auto &thisZoneEquipConfig : state.dataZoneEquip->ZoneEquipConfig) {
            if (!thisZoneEquipConfig.IsControlled) continue;
            thisZoneEquipConfig.beginEnvirnInit(state);
        }
        if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
            for (auto &thisSpaceEquipConfig : state.dataZoneEquip->spaceEquipConfig) {
                if (!thisSpaceEquipConfig.IsControlled) continue;
                thisSpaceEquipConfig.beginEnvirnInit(state);
            }
        }

        state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZoneEquipmentManager->InitZoneEquipmentEnvrnFlag = true;
    }

    // do the  HVAC time step initializations

    for (auto &thisZoneEquipConfig : state.dataZoneEquip->ZoneEquipConfig) {
        if (!thisZoneEquipConfig.IsControlled) continue;
        thisZoneEquipConfig.hvacTimeStepInit(state, FirstHVACIteration);
    }
    if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
        for (auto &thisSpaceEquipConfig : state.dataZoneEquip->spaceEquipConfig) {
            if (!thisSpaceEquipConfig.IsControlled) continue;
            thisSpaceEquipConfig.hvacTimeStepInit(state, FirstHVACIteration);
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
void sizeZoneSpaceEquipmentPart1(EnergyPlusData &state,
                                 DataZoneEquipment::EquipConfiguration &zoneEquipConfig,
                                 DataSizing::ZoneSizingData &zsCalcSizing,
                                 DataZoneEnergyDemands::ZoneSystemSensibleDemand &zsEnergyDemand,
                                 DataZoneEnergyDemands::ZoneSystemMoistureDemand &zsMoistureDemand,
                                 DataHeatBalance::ZoneData &zoneOrSpace,
                                 int zoneNum,
                                 int spaceNum)
{
    static constexpr std::string_view RoutineName("sizeZoneSpaceEquipmentPart1");
    // set up references for space vs zoneHeatBalance
    auto &nonAirSystemResponse = (spaceNum > 0) ? state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).NonAirSystemResponse
                                                : state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).NonAirSystemResponse;
    auto &sysDepZoneLoads = (spaceNum > 0) ? state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).SysDepZoneLoads
                                           : state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).SysDepZoneLoads;
    auto &zoneLatentGain = (spaceNum > 0) ? state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).latentGain
                                          : state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).latentGain;
    auto &zoneNodeNum =
        (spaceNum > 0) ? state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber : state.dataHeatBal->Zone(zoneNum).SystemZoneNodeNumber;
    nonAirSystemResponse = 0.0;
    sysDepZoneLoads = 0.0;
    auto &zoneNode = state.dataLoopNodes->Node(zoneNodeNum);

    // InitSystemOutputRequired(state, zoneNum, true);
    initOutputRequired(state, zoneNum, zsEnergyDemand, zsMoistureDemand, true, false, spaceNum);

    // save raw zone loads without impact of outdoor air
    Real64 LatOutputProvidedNoDOAS = zsMoistureDemand.RemainingOutputRequired;
    Real64 SysOutputProvidedNoDOAS = zsEnergyDemand.RemainingOutputRequired;
    // if Tstat deadband is true then load will be reported as 0
    if (state.dataZoneEnergyDemand->DeadBandOrSetback(zoneNum)) SysOutputProvidedNoDOAS = 0.0;
    // replicate deadband flag - zone condition is either below the humidistat or above the dehumidistat set point
    // using logic: NOT (!) (there is a load)
    // Pretty sure this could just be if (OutputRequiredToHumidifyingSP < 0 && OutputRequiredToDehumidifyingSP > 0)
    if (!((zsMoistureDemand.OutputRequiredToHumidifyingSP > 0.0 && zsMoistureDemand.OutputRequiredToDehumidifyingSP > 0.0) ||
          (zsMoistureDemand.OutputRequiredToHumidifyingSP < 0.0 && zsMoistureDemand.OutputRequiredToDehumidifyingSP < 0.0))) {
        LatOutputProvidedNoDOAS = 0.0;
    }

    // calculate DOAS heating/cooling effect
    int supplyAirNodeNum = 0;
    if (zsCalcSizing.AccountForDOAS) {
        Real64 DOASMassFlowRate = 0.0;         // DOAS air mass flow rate for sizing [kg/s]
        Real64 DOASSupplyTemp = 0.0;           // DOAS supply air temperature [C]
        Real64 DOASSupplyHumRat = 0.0;         // DOAS supply air humidity ratio [kgWater/kgDryAir]
        Real64 DOASCpAir = 0.0;                // heat capacity of DOAS air [J/kg-C]
        Real64 DOASSysOutputProvided = 0.0;    // heating / cooling provided by DOAS system [W]
        Real64 DOASLatOutputProvided = 0.0;    // DOAS system latent output [kg/s]
        Real64 TotDOASSysOutputProvided = 0.0; // total DOAS load on the zone [W]
        Real64 HR90H = 0.0;                    // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
        Real64 HR90L = 0.0;                    // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
        // check for adequate number of supply nodes
        int supplyAirNodeNum1 = 0;
        int supplyAirNodeNum2 = 0;
        if (zoneEquipConfig.NumInletNodes >= 2) {
            supplyAirNodeNum1 = zoneEquipConfig.InletNode(1);
            supplyAirNodeNum2 = zoneEquipConfig.InletNode(2);
        } else if (zoneEquipConfig.NumInletNodes >= 1) {
            supplyAirNodeNum1 = zoneEquipConfig.InletNode(1);
            supplyAirNodeNum2 = 0;
        } else {
            ShowSevereError(state, format("{}: to account for the effect a Dedicated Outside Air System on zone equipment sizing", RoutineName));
            ShowContinueError(state, "there must be at least one zone air inlet node");
            ShowFatalError(state, "Previous severe error causes abort ");
        }
        // set the DOAS mass flow rate and supply temperature and humidity ratio
        HR90H = PsyWFnTdbRhPb(state, zsCalcSizing.DOASHighSetpoint, 0.9, state.dataEnvrn->StdBaroPress);
        HR90L = PsyWFnTdbRhPb(state, zsCalcSizing.DOASLowSetpoint, 0.9, state.dataEnvrn->StdBaroPress);
        DOASMassFlowRate = state.dataSize->CalcFinalZoneSizing(zoneNum).MinOA * state.dataEnvrn->StdRhoAir;
        CalcDOASSupCondsForSizing(state,
                                  state.dataEnvrn->OutDryBulbTemp,
                                  state.dataEnvrn->OutHumRat,
                                  zsCalcSizing.DOASControlStrategy,
                                  zsCalcSizing.DOASLowSetpoint,
                                  zsCalcSizing.DOASHighSetpoint,
                                  HR90H,
                                  HR90L,
                                  DOASSupplyTemp,
                                  DOASSupplyHumRat);
        DOASCpAir = PsyCpAirFnW(DOASSupplyHumRat);
        DOASSysOutputProvided = DOASMassFlowRate * DOASCpAir * (DOASSupplyTemp - zoneNode.Temp);
        TotDOASSysOutputProvided = DOASMassFlowRate * (PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat) - PsyHFnTdbW(zoneNode.Temp, zoneNode.HumRat));
        if (zsCalcSizing.zoneLatentSizing) {
            DOASLatOutputProvided = DOASMassFlowRate * (DOASSupplyHumRat - zoneNode.HumRat); // kgw/s
        }

        updateSystemOutputRequired(state, zoneNum, DOASSysOutputProvided, DOASLatOutputProvided, zsEnergyDemand, zsMoistureDemand);
        auto &supplyAirNode1 = state.dataLoopNodes->Node(supplyAirNodeNum1);
        supplyAirNode1.Temp = DOASSupplyTemp;
        supplyAirNode1.HumRat = DOASSupplyHumRat;
        supplyAirNode1.MassFlowRate = DOASMassFlowRate;
        supplyAirNode1.Enthalpy = PsyHFnTdbW(DOASSupplyTemp, DOASSupplyHumRat);
        zsCalcSizing.DOASHeatAdd = DOASSysOutputProvided;
        zsCalcSizing.DOASLatAdd = TotDOASSysOutputProvided - DOASSysOutputProvided;
        supplyAirNodeNum = supplyAirNodeNum2;
        zsCalcSizing.DOASSupMassFlow = DOASMassFlowRate;
        zsCalcSizing.DOASSupTemp = DOASSupplyTemp;
        zsCalcSizing.DOASSupHumRat = DOASSupplyHumRat;
        if (DOASSysOutputProvided > 0.0) {
            zsCalcSizing.DOASHeatLoad = DOASSysOutputProvided;
            zsCalcSizing.DOASCoolLoad = 0.0;
            zsCalcSizing.DOASTotCoolLoad = 0.0;
        } else {
            zsCalcSizing.DOASCoolLoad = DOASSysOutputProvided;
            zsCalcSizing.DOASTotCoolLoad = TotDOASSysOutputProvided;
            zsCalcSizing.DOASHeatLoad = 0.0;
        }

    } else {
        if (zoneEquipConfig.NumInletNodes > 0) {
            supplyAirNodeNum = zoneEquipConfig.InletNode(1);
        } else {
            supplyAirNodeNum = 0;
        }
    }

    Real64 DeltaTemp = 0.0;         // difference between supply air temp and zone temp [C]
    Real64 CpAir = 0.0;             // heat capacity of air [J/kg-C]
    Real64 SysOutputProvided = 0.0; // system sensible output [W]
    Real64 LatOutputProvided = 0.0; // system latent output [kg/s]
    Real64 Temp = 0.0;              // inlet temperature [C]
    Real64 HumRat = 0.0;            // inlet humidity ratio [kg water/kg dry air]
    Real64 Enthalpy = 0.0;          // inlet specific enthalpy [J/kg]
    Real64 MassFlowRate = 0.0;      // inlet mass flow rate [kg/s]
    // Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
    //                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
    if (!state.dataZoneEnergyDemand->DeadBandOrSetback(zoneNum) && std::abs(zsEnergyDemand.RemainingOutputRequired) > DataHVACGlobals::SmallLoad) {
        // Determine design supply air temperture and design supply air temperature difference
        if (zsEnergyDemand.RemainingOutputRequired < 0.0) { // Cooling case
            // If the user specify the design cooling supply air temperature, then
            if (zsCalcSizing.ZnCoolDgnSAMethod == SupplyAirTemperature) {
                Temp = zsCalcSizing.CoolDesTemp;
                HumRat = zsCalcSizing.CoolDesHumRat;
                DeltaTemp = Temp - zoneNode.Temp;
                if (zoneOrSpace.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                    DeltaTemp = Temp - zoneOrSpace.AdjustedReturnTempByITE;
                }
                // If the user specify the design cooling supply air temperature difference, then
            } else {
                DeltaTemp = -std::abs(zsCalcSizing.CoolDesTempDiff);
                Temp = DeltaTemp + zoneNode.Temp;
                if (zoneOrSpace.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                    Temp = DeltaTemp + zoneOrSpace.AdjustedReturnTempByITE;
                }
                HumRat = zsCalcSizing.CoolDesHumRat;
            }
        } else { // Heating Case
            // If the user specify the design heating supply air temperature, then
            if (zsCalcSizing.ZnHeatDgnSAMethod == SupplyAirTemperature) {
                Temp = zsCalcSizing.HeatDesTemp;
                HumRat = zsCalcSizing.HeatDesHumRat;
                DeltaTemp = Temp - zoneNode.Temp;
                // If the user specify the design heating supply air temperature difference, then
            } else {
                DeltaTemp = std::abs(zsCalcSizing.HeatDesTempDiff);
                Temp = DeltaTemp + zoneNode.Temp;
                HumRat = zsCalcSizing.HeatDesHumRat;
            }
        }

        Enthalpy = PsyHFnTdbW(Temp, HumRat);
        SysOutputProvided = zsEnergyDemand.RemainingOutputRequired;
        CpAir = PsyCpAirFnW(HumRat);
        if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) {
            //!!PH/WFB/LKL (UCDV model)        MassFlowRate = SysOutputProvided / (CpAir*DeltaTemp)
            MassFlowRate = max(SysOutputProvided / (CpAir * DeltaTemp), 0.0);
        } else {
            MassFlowRate = 0.0;
        }

        if (zsCalcSizing.SupplyAirAdjustFactor > 1.0) {
            MassFlowRate *= zsCalcSizing.SupplyAirAdjustFactor;
        }
    } else {

        Temp = zoneNode.Temp;
        HumRat = zoneNode.HumRat;
        Enthalpy = zoneNode.Enthalpy;
        MassFlowRate = 0.0;
    }

    if (SysOutputProvided > 0.0) {
        zsCalcSizing.HeatLoad = SysOutputProvided;
        zsCalcSizing.HeatMassFlow = MassFlowRate;
        zsCalcSizing.CoolLoad = 0.0;
        zsCalcSizing.CoolMassFlow = 0.0;
    } else if (SysOutputProvided < 0.0) {
        zsCalcSizing.CoolLoad = -SysOutputProvided;
        zsCalcSizing.CoolMassFlow = MassFlowRate;
        zsCalcSizing.HeatLoad = 0.0;
        zsCalcSizing.HeatMassFlow = 0.0;
    } else {
        zsCalcSizing.CoolLoad = 0.0;
        zsCalcSizing.CoolMassFlow = 0.0;
        zsCalcSizing.HeatLoad = 0.0;
        zsCalcSizing.HeatMassFlow = 0.0;
    }
    zsCalcSizing.HeatZoneTemp = zoneNode.Temp;
    zsCalcSizing.HeatZoneHumRat = zoneNode.HumRat;
    zsCalcSizing.CoolZoneTemp = zoneNode.Temp;
    zsCalcSizing.CoolZoneHumRat = zoneNode.HumRat;
    zsCalcSizing.HeatOutTemp = state.dataEnvrn->OutDryBulbTemp;
    zsCalcSizing.HeatOutHumRat = state.dataEnvrn->OutHumRat;
    zsCalcSizing.CoolOutTemp = state.dataEnvrn->OutDryBulbTemp;
    zsCalcSizing.CoolOutHumRat = state.dataEnvrn->OutHumRat;

    Real64 LatentAirMassFlow = 0.0;
    Real64 MoistureLoad = 0.0;
    Real64 HgAir = PsyHgAirFnWTdb(zoneNode.HumRat, zoneNode.Temp);
    if (zsCalcSizing.zoneLatentSizing) {
        // replicate deadband flag - zone condition is either below the humidistat or above the dehumidistat set point
        if ((zsMoistureDemand.OutputRequiredToHumidifyingSP > 0.0 && zsMoistureDemand.OutputRequiredToDehumidifyingSP > 0.0) ||
            (zsMoistureDemand.OutputRequiredToHumidifyingSP < 0.0 && zsMoistureDemand.OutputRequiredToDehumidifyingSP < 0.0)) {
            LatOutputProvided = zsMoistureDemand.RemainingOutputRequired;
        }
        Real64 DeltaHumRat = 0.0;      // positive LatOutputProvided means humidification load
        if (LatOutputProvided < 0.0) { // use SA humrat - zone humrat, or delta humrat based on user choice
            DeltaHumRat = (zsCalcSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio) ? (zsCalcSizing.LatentCoolDesHumRat - zoneNode.HumRat)
                                                                                        : -zsCalcSizing.CoolDesHumRatDiff;
        } else if (LatOutputProvided > 0.0) {
            DeltaHumRat = (zsCalcSizing.ZnLatHeatDgnSAMethod == SupplyAirHumidityRatio) ? (zsCalcSizing.LatentHeatDesHumRat - zoneNode.HumRat)
                                                                                        : zsCalcSizing.HeatDesHumRatDiff;
        }
        if (std::abs(DeltaHumRat) > DataHVACGlobals::VerySmallMassFlow) LatentAirMassFlow = std::max(0.0, LatOutputProvided / DeltaHumRat);
        MoistureLoad = LatOutputProvided * HgAir;

        if (MassFlowRate > 0.0) {
            HumRat = zoneNode.HumRat + LatOutputProvided / MassFlowRate;
            CpAir = PsyCpAirFnW(HumRat);
            Temp = (SysOutputProvided / (MassFlowRate * CpAir)) + zoneNode.Temp;
            Enthalpy = PsyHFnTdbW(Temp, HumRat);
        } else if (LatentAirMassFlow > 0.0) {
            // if there is no sensible load then still need to hold zone RH at set point
            // no need to recalculate T, Sensible load = 0 so T = T,zone
            HumRat = zoneNode.HumRat + LatOutputProvided / LatentAirMassFlow;
            Enthalpy = PsyHFnTdbW(Temp, HumRat);
            MassFlowRate = (LatentAirMassFlow > DataHVACGlobals::VerySmallMassFlow) ? LatentAirMassFlow : 0.0;
        }

        zsCalcSizing.HeatLatentLoad = (LatOutputProvided > 0.0) ? MoistureLoad : 0.0;
        zsCalcSizing.ZoneHeatLatentMassFlow = (LatOutputProvided > 0.0) ? LatentAirMassFlow : 0.0;
        zsCalcSizing.CoolLatentLoad = (LatOutputProvided < 0.0) ? -MoistureLoad : 0.0;
        zsCalcSizing.ZoneCoolLatentMassFlow = (LatOutputProvided < 0.0) ? LatentAirMassFlow : 0.0;
        zsCalcSizing.HeatLoadNoDOAS = (SysOutputProvidedNoDOAS > 0.0) ? SysOutputProvidedNoDOAS : 0.0;
        zsCalcSizing.CoolLoadNoDOAS = (SysOutputProvidedNoDOAS < 0.0) ? -SysOutputProvidedNoDOAS : 0.0;
        zsCalcSizing.HeatLatentLoadNoDOAS = (LatOutputProvidedNoDOAS > 0.0) ? LatOutputProvidedNoDOAS * HgAir : 0.0;
        zsCalcSizing.CoolLatentLoadNoDOAS = (LatOutputProvidedNoDOAS < 0.0) ? -LatOutputProvidedNoDOAS * HgAir : 0.0;
    }

    if (supplyAirNodeNum > 0) {
        auto &supplyAirNode = state.dataLoopNodes->Node(supplyAirNodeNum);
        supplyAirNode.Temp = Temp;
        supplyAirNode.HumRat = HumRat;
        supplyAirNode.Enthalpy = Enthalpy;
        supplyAirNode.MassFlowRate = MassFlowRate;
    } else {
        nonAirSystemResponse = SysOutputProvided;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                // SpaceHB ToDo: For now allocate by space volume frac
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).NonAirSystemResponse =
                    nonAirSystemResponse * state.dataHeatBal->space(spaceNum).fracZoneVolume;
            }
        }
        if (zsCalcSizing.zoneLatentSizing) {
            int ZoneMult = zoneOrSpace.Multiplier * zoneOrSpace.ListMultiplier;
            zoneLatentGain += (LatOutputProvided * HgAir) / ZoneMult;
        }
    }

    updateSystemOutputRequired(state, zoneNum, SysOutputProvided, LatOutputProvided, zsEnergyDemand, zsMoistureDemand);
}

void sizeZoneSpaceEquipmentPart2(EnergyPlusData &state,
                                 DataZoneEquipment::EquipConfiguration &zoneEquipConfig,
                                 DataSizing::ZoneSizingData &zsCalcSizing,
                                 int zoneNum,
                                 int spaceNum)
{
    // MJW for now - use first return node, make a separate commit to add a dimension to all of the sizing rettemp variables
    int returnNodeNum = (zoneEquipConfig.NumReturnNodes > 0) ? zoneEquipConfig.ReturnNode(1) : 0;
    int zoneNodeNum =
        (spaceNum > 0) ? state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber : state.dataHeatBal->Zone(zoneNum).SystemZoneNodeNumber;
    Real64 RetTemp = (returnNodeNum > 0) ? state.dataLoopNodes->Node(returnNodeNum).Temp : state.dataLoopNodes->Node(zoneNodeNum).Temp;
    auto &zoneTstatSP = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(zoneNum);
    if (zsCalcSizing.HeatLoad > 0.0) {
        zsCalcSizing.HeatZoneRetTemp = RetTemp;
        zsCalcSizing.HeatTstatTemp = (zoneTstatSP > 0.0) ? zoneTstatSP : state.dataHeatBalFanSys->ZoneThermostatSetPointLo(zoneNum);
        zsCalcSizing.CoolTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(zoneNum);
    } else if (zsCalcSizing.CoolLoad > 0.0) {
        zsCalcSizing.CoolZoneRetTemp = RetTemp;
        zsCalcSizing.CoolTstatTemp = (zoneTstatSP > 0.0) ? zoneTstatSP : state.dataHeatBalFanSys->ZoneThermostatSetPointHi(zoneNum);
        zsCalcSizing.HeatTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(zoneNum);
    } else {
        zsCalcSizing.CoolZoneRetTemp = RetTemp;
        zsCalcSizing.HeatTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(zoneNum);
        zsCalcSizing.CoolTstatTemp = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(zoneNum);
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

    if (state.dataZoneEquipmentManager->SizeZoneEquipmentOneTimeFlag) {
        SetUpZoneSizingArrays(state);
        state.dataZoneEquipmentManager->SizeZoneEquipmentOneTimeFlag = false;
    }

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;

        // use reference to eliminate lots of long lines in this function, after initial commit, so reviewers can see changes
        auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum);
        auto &zoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum);
        auto &zoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum);
        auto &zone = state.dataHeatBal->Zone(ControlledZoneNum);

        sizeZoneSpaceEquipmentPart1(state, zoneEquipConfig, calcZoneSizing, zoneSysEnergyDemand, zoneSysMoistureDemand, zone, ControlledZoneNum);
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                sizeZoneSpaceEquipmentPart1(state,
                                            state.dataZoneEquip->spaceEquipConfig(spaceNum),
                                            state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum),
                                            state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum),
                                            state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum),
                                            zone,
                                            ControlledZoneNum,
                                            spaceNum);
            }
        }
    }

    CalcZoneMassBalance(state, true);

    CalcZoneLeavingConditions(state, true);

    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {

        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;
        sizeZoneSpaceEquipmentPart2(
            state, zoneEquipConfig, state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, ControlledZoneNum), ControlledZoneNum);
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                sizeZoneSpaceEquipmentPart2(
                    state, zoneEquipConfig, state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum), ControlledZoneNum, spaceNum);
            }
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
        int ZoneIndex = Util::FindItemInList(state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName, state.dataHeatBal->Zone);
        if (ZoneIndex == 0) {
            ShowSevereError(
                state,
                format("SetUpZoneSizingArrays: Sizing:Zone=\"{}\" references unknown zone", state.dataSize->ZoneSizingInput(ZoneSizIndex).ZoneName));
            ErrorsFound = true;
        }
        if (std::any_of(state.dataZoneEquip->ZoneEquipConfig.begin(), state.dataZoneEquip->ZoneEquipConfig.end(), [](EquipConfiguration const &e) {
                return e.IsControlled;
            })) {
            ZoneIndex = Util::FindItemInList(
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
    if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
        state.dataSize->SpaceSizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays, state.dataGlobal->numSpaces);
        state.dataSize->FinalSpaceSizing.allocate(state.dataGlobal->numSpaces);
        state.dataSize->CalcSpaceSizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays, state.dataGlobal->numSpaces);
        state.dataSize->CalcFinalSpaceSizing.allocate(state.dataGlobal->numSpaces);
    }
    state.dataSize->TermUnitFinalZoneSizing.allocate(state.dataSize->NumAirTerminalUnits);
    for (auto &tufzs : state.dataSize->TermUnitFinalZoneSizing) {
        tufzs.allocateMemberArrays(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
    }
    state.dataSize->DesDayWeath.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
    state.dataZoneEquipmentManager->AvgData.allocate(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
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
        int ZoneSizNum = Util::FindItemInList(zoneEquipConfig.ZoneName, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
        auto &zoneSizingInput = (ZoneSizNum > 0) ? state.dataSize->ZoneSizingInput(ZoneSizNum) : state.dataSize->ZoneSizingInput(1);
        if (ZoneSizNum == 0) { // LKL I think this is sufficient for warning -- no need for array
            if (!state.dataGlobal->isPulseZoneSizing) {
                ShowWarningError(state,
                                 format("SetUpZoneSizingArrays: Sizing for Zone=\"{}\" will use Sizing:Zone specifications listed for Zone=\"{}\".",
                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneName,
                                        zoneSizingInput.ZoneName));
            }
        }

        fillZoneSizingFromInput(state,
                                zoneSizingInput,
                                state.dataSize->ZoneSizing,
                                state.dataSize->CalcZoneSizing,
                                state.dataSize->FinalZoneSizing(CtrlZoneNum),
                                state.dataSize->CalcFinalZoneSizing(CtrlZoneNum),
                                state.dataHeatBal->Zone(CtrlZoneNum).Name,
                                CtrlZoneNum);
        if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
            for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                fillZoneSizingFromInput(state,
                                        zoneSizingInput,
                                        state.dataSize->SpaceSizing,
                                        state.dataSize->CalcSpaceSizing,
                                        state.dataSize->FinalSpaceSizing(spaceNum),
                                        state.dataSize->CalcFinalSpaceSizing(spaceNum),
                                        state.dataHeatBal->space(spaceNum).Name,
                                        spaceNum);
            }
        }

        // setup CalcFinalZoneSizing structure for use with EMS, some as sensors, some as actuators
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            auto &finalZoneSizing = state.dataSize->FinalZoneSizing(CtrlZoneNum);
            auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);

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
                int thisSpaceNum = Util::FindItemInList(thisSpaceName, state.dataHeatBal->space);
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

void fillZoneSizingFromInput(EnergyPlusData &state,
                             DataSizing::ZoneSizingInputData const &zoneSizingInput,
                             Array2D<DataSizing::ZoneSizingData> &zsSizing,
                             Array2D<DataSizing::ZoneSizingData> &zsCalcSizing,
                             DataSizing::ZoneSizingData &zsFinalSizing,
                             DataSizing::ZoneSizingData &zsCalcFinalSizing,
                             std::string_view const zoneOrSpaceName,
                             int const zoneOrSpaceNum)
{
    for (int DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
        auto &zoneSizing = zsSizing(DesDayNum, zoneOrSpaceNum);
        auto &calcZoneSizing = zsCalcSizing(DesDayNum, zoneOrSpaceNum);
        zoneSizing.ZoneName = zoneOrSpaceName;
        zoneSizing.ZoneNum = zoneOrSpaceNum;
        calcZoneSizing.ZoneName = zoneOrSpaceName;
        calcZoneSizing.ZoneNum = zoneOrSpaceNum;

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

        zoneSizing.allocateMemberArrays(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
        calcZoneSizing.allocateMemberArrays(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
    }

    zsFinalSizing.ZoneName = zoneOrSpaceName;
    zsFinalSizing.ZoneNum = zoneOrSpaceNum;
    zsCalcFinalSizing.ZoneName = zoneOrSpaceName;
    zsCalcFinalSizing.ZoneNum = zoneOrSpaceNum;

    zsFinalSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
    zsFinalSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
    zsFinalSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
    zsFinalSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
    zsFinalSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
    zsFinalSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
    zsFinalSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
    zsFinalSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
    zsFinalSizing.ZoneAirDistributionIndex = zoneSizingInput.ZoneAirDistributionIndex;
    zsFinalSizing.ZoneDesignSpecOAIndex = zoneSizingInput.ZoneDesignSpecOAIndex;
    zsFinalSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
    zsFinalSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
    zsFinalSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
    zsFinalSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
    zsFinalSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
    zsFinalSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
    zsFinalSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
    zsFinalSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
    zsFinalSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
    zsFinalSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
    zsFinalSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
    zsFinalSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
    zsFinalSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
    zsFinalSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
    zsFinalSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
    zsFinalSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
    zsFinalSizing.ZoneADEffCooling = zoneSizingInput.ZoneADEffCooling;
    zsFinalSizing.ZoneADEffHeating = zoneSizingInput.ZoneADEffHeating;
    zsFinalSizing.ZoneSecondaryRecirculation = zoneSizingInput.ZoneSecondaryRecirculation;
    zsFinalSizing.ZoneVentilationEff = zoneSizingInput.ZoneVentilationEff;
    zsFinalSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
    zsFinalSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
    zsFinalSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
    zsFinalSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
    zsFinalSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
    zsFinalSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
    zsFinalSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
    zsFinalSizing.LatentCoolDesHumRat = zoneSizingInput.LatentCoolDesHumRat;
    zsFinalSizing.CoolDesHumRatDiff = zoneSizingInput.CoolDesHumRatDiff;
    zsFinalSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
    zsFinalSizing.LatentHeatDesHumRat = zoneSizingInput.LatentHeatDesHumRat;
    zsFinalSizing.HeatDesHumRatDiff = zoneSizingInput.HeatDesHumRatDiff;
    zsCalcFinalSizing.ZnCoolDgnSAMethod = zoneSizingInput.ZnCoolDgnSAMethod;
    zsCalcFinalSizing.ZnHeatDgnSAMethod = zoneSizingInput.ZnHeatDgnSAMethod;
    zsCalcFinalSizing.CoolDesTemp = zoneSizingInput.CoolDesTemp;
    zsCalcFinalSizing.HeatDesTemp = zoneSizingInput.HeatDesTemp;
    zsCalcFinalSizing.CoolDesTempDiff = zoneSizingInput.CoolDesTempDiff;
    zsCalcFinalSizing.HeatDesTempDiff = zoneSizingInput.HeatDesTempDiff;
    zsCalcFinalSizing.CoolDesHumRat = zoneSizingInput.CoolDesHumRat;
    zsCalcFinalSizing.HeatDesHumRat = zoneSizingInput.HeatDesHumRat;
    zsCalcFinalSizing.ZoneAirDistributionIndex = zoneSizingInput.ZoneAirDistributionIndex;
    zsCalcFinalSizing.ZoneDesignSpecOAIndex = zoneSizingInput.ZoneDesignSpecOAIndex;
    zsCalcFinalSizing.CoolAirDesMethod = zoneSizingInput.CoolAirDesMethod;
    zsCalcFinalSizing.HeatAirDesMethod = zoneSizingInput.HeatAirDesMethod;
    zsCalcFinalSizing.InpDesCoolAirFlow = zoneSizingInput.DesCoolAirFlow;
    zsCalcFinalSizing.DesCoolMinAirFlowPerArea = zoneSizingInput.DesCoolMinAirFlowPerArea;
    zsCalcFinalSizing.DesCoolMinAirFlow = zoneSizingInput.DesCoolMinAirFlow;
    zsCalcFinalSizing.DesCoolMinAirFlowFrac = zoneSizingInput.DesCoolMinAirFlowFrac;
    zsCalcFinalSizing.InpDesHeatAirFlow = zoneSizingInput.DesHeatAirFlow;
    zsCalcFinalSizing.DesHeatMaxAirFlowPerArea = zoneSizingInput.DesHeatMaxAirFlowPerArea;
    zsCalcFinalSizing.DesHeatMaxAirFlow = zoneSizingInput.DesHeatMaxAirFlow;
    zsCalcFinalSizing.DesHeatMaxAirFlowFrac = zoneSizingInput.DesHeatMaxAirFlowFrac;
    zsCalcFinalSizing.HeatSizingFactor = zoneSizingInput.HeatSizingFactor;
    zsCalcFinalSizing.CoolSizingFactor = zoneSizingInput.CoolSizingFactor;
    zsCalcFinalSizing.AccountForDOAS = zoneSizingInput.AccountForDOAS;
    zsCalcFinalSizing.DOASControlStrategy = zoneSizingInput.DOASControlStrategy;
    zsCalcFinalSizing.DOASLowSetpoint = zoneSizingInput.DOASLowSetpoint;
    zsCalcFinalSizing.DOASHighSetpoint = zoneSizingInput.DOASHighSetpoint;
    zsCalcFinalSizing.ZoneADEffCooling = zoneSizingInput.ZoneADEffCooling;
    zsCalcFinalSizing.ZoneADEffHeating = zoneSizingInput.ZoneADEffHeating;
    zsCalcFinalSizing.zoneSizingMethod = zoneSizingInput.zoneSizingMethod;
    zsCalcFinalSizing.zoneLatentSizing = zoneSizingInput.zoneLatentSizing;
    zsCalcFinalSizing.zoneRHDehumidifySetPoint = zoneSizingInput.zoneRHDehumidifySetPoint;
    zsCalcFinalSizing.zoneRHHumidifySetPoint = zoneSizingInput.zoneRHHumidifySetPoint;
    zsCalcFinalSizing.zoneRHDehumidifySchIndex = zoneSizingInput.zoneRHDehumidifySchIndex;
    zsCalcFinalSizing.zoneRHHumidifySchIndex = zoneSizingInput.zoneRHHumidifySchIndex;
    zsCalcFinalSizing.ZnLatCoolDgnSAMethod = zoneSizingInput.ZnLatCoolDgnSAMethod;
    zsCalcFinalSizing.LatentCoolDesHumRat = zoneSizingInput.LatentCoolDesHumRat;
    zsCalcFinalSizing.CoolDesHumRatDiff = zoneSizingInput.CoolDesHumRatDiff;
    zsCalcFinalSizing.ZnLatHeatDgnSAMethod = zoneSizingInput.ZnLatHeatDgnSAMethod;
    zsCalcFinalSizing.LatentHeatDesHumRat = zoneSizingInput.LatentHeatDesHumRat;
    zsCalcFinalSizing.HeatDesHumRatDiff = zoneSizingInput.HeatDesHumRatDiff;

    zsFinalSizing.allocateMemberArrays(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
    zsCalcFinalSizing.allocateMemberArrays(state.dataZoneEquipmentManager->NumOfTimeStepInDay);
}
void RezeroZoneSizingArrays(EnergyPlusData &state)
{
    // Zero zone sizing arrays between the pulse and normal sizing.
    DisplayString(state, "Re-zeroing zone sizing arrays");

    for (int ctrlZoneNum = 1; ctrlZoneNum <= state.dataGlobal->NumOfZones; ++ctrlZoneNum) {
        if (!state.dataZoneEquip->ZoneEquipConfig(ctrlZoneNum).IsControlled) continue;
        for (int desDayNum = 1; desDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++desDayNum) {
            state.dataSize->ZoneSizing(desDayNum, ctrlZoneNum).zeroMemberData();
            state.dataSize->CalcZoneSizing(desDayNum, ctrlZoneNum).zeroMemberData();
        }
        state.dataSize->CalcFinalZoneSizing(ctrlZoneNum).zeroMemberData();
        state.dataSize->FinalZoneSizing(ctrlZoneNum).zeroMemberData();
    }
    if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(state.dataHeatBal->space(spaceNum).zoneNum).IsControlled) continue;
            for (int desDayNum = 1; desDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++desDayNum) {
                state.dataSize->SpaceSizing(desDayNum, spaceNum).zeroMemberData();
                state.dataSize->CalcSpaceSizing(desDayNum, spaceNum).zeroMemberData();
            }
            state.dataSize->CalcFinalSpaceSizing(spaceNum).zeroMemberData();
            state.dataSize->FinalSpaceSizing(spaceNum).zeroMemberData();
        }
    }
}
void updateZoneSizingBeginDay(EnergyPlusData &state, DataSizing::ZoneSizingData &zsCalcSizing)
{
    zsCalcSizing.CoolDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.HeatDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.DesHeatDens = state.dataEnvrn->StdRhoAir;
    zsCalcSizing.DesCoolDens = state.dataEnvrn->StdRhoAir;
    zsCalcSizing.HeatDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.CoolDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.CoolNoDOASDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.HeatNoDOASDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.LatCoolDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.LatHeatDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.LatCoolNoDOASDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.LatHeatNoDOASDesDay = state.dataEnvrn->EnvironmentName;
    zsCalcSizing.CoolNoDOASDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.HeatNoDOASDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.LatentCoolDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.LatentHeatDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.LatentCoolNoDOASDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.LatentHeatNoDOASDDNum = state.dataSize->CurOverallSimDay;
    zsCalcSizing.CoolSizingType = "Cooling"; // string reported to eio
    zsCalcSizing.HeatSizingType = "Heating"; // string reported to eio
}

void updateZoneSizingDuringDay(DataSizing::ZoneSizingData &zsSizing,
                               DataSizing::ZoneSizingData &zsCalcSizing,
                               Real64 const tstatHi,
                               Real64 const tstatLo,
                               Real64 &sizTstatHi,
                               Real64 &sizTstatLo,
                               int const timeStepInDay,
                               Real64 const fracTimeStepZone)
{
    if (tstatHi > 0.0 && tstatHi > sizTstatHi) {
        sizTstatHi = tstatHi;
    }
    if (tstatLo > 0.0 && tstatLo < sizTstatHi) {
        sizTstatLo = tstatLo;
    }
    zsSizing.DesHeatSetPtSeq(timeStepInDay) = tstatLo;
    zsSizing.HeatTstatTempSeq(timeStepInDay) = zsCalcSizing.HeatTstatTemp;
    zsSizing.DesCoolSetPtSeq(timeStepInDay) = tstatHi;
    zsSizing.CoolTstatTempSeq(timeStepInDay) = zsCalcSizing.CoolTstatTemp;
    zsCalcSizing.HeatFlowSeq(timeStepInDay) += zsCalcSizing.HeatMassFlow * fracTimeStepZone;
    zsCalcSizing.HeatLoadSeq(timeStepInDay) += zsCalcSizing.HeatLoad * fracTimeStepZone;
    zsCalcSizing.HeatZoneTempSeq(timeStepInDay) += zsCalcSizing.HeatZoneTemp * fracTimeStepZone;
    zsCalcSizing.HeatOutTempSeq(timeStepInDay) += zsCalcSizing.HeatOutTemp * fracTimeStepZone;
    zsCalcSizing.HeatZoneRetTempSeq(timeStepInDay) += zsCalcSizing.HeatZoneRetTemp * fracTimeStepZone;
    zsCalcSizing.HeatZoneHumRatSeq(timeStepInDay) += zsCalcSizing.HeatZoneHumRat * fracTimeStepZone;
    zsCalcSizing.HeatOutHumRatSeq(timeStepInDay) += zsCalcSizing.HeatOutHumRat * fracTimeStepZone;
    zsCalcSizing.CoolFlowSeq(timeStepInDay) += zsCalcSizing.CoolMassFlow * fracTimeStepZone;
    zsCalcSizing.CoolLoadSeq(timeStepInDay) += zsCalcSizing.CoolLoad * fracTimeStepZone;
    zsCalcSizing.CoolZoneTempSeq(timeStepInDay) += zsCalcSizing.CoolZoneTemp * fracTimeStepZone;
    zsCalcSizing.CoolOutTempSeq(timeStepInDay) += zsCalcSizing.CoolOutTemp * fracTimeStepZone;
    zsCalcSizing.CoolZoneRetTempSeq(timeStepInDay) += zsCalcSizing.CoolZoneRetTemp * fracTimeStepZone;
    zsCalcSizing.CoolZoneHumRatSeq(timeStepInDay) += zsCalcSizing.CoolZoneHumRat * fracTimeStepZone;
    zsCalcSizing.CoolOutHumRatSeq(timeStepInDay) += zsCalcSizing.CoolOutHumRat * fracTimeStepZone;
    zsCalcSizing.DOASHeatLoadSeq(timeStepInDay) += zsCalcSizing.DOASHeatLoad * fracTimeStepZone;
    zsCalcSizing.DOASCoolLoadSeq(timeStepInDay) += zsCalcSizing.DOASCoolLoad * fracTimeStepZone;
    zsCalcSizing.DOASHeatAddSeq(timeStepInDay) += zsCalcSizing.DOASHeatAdd * fracTimeStepZone;
    zsCalcSizing.DOASLatAddSeq(timeStepInDay) += zsCalcSizing.DOASLatAdd * fracTimeStepZone;
    zsCalcSizing.DOASSupMassFlowSeq(timeStepInDay) += zsCalcSizing.DOASSupMassFlow * fracTimeStepZone;
    zsCalcSizing.DOASSupTempSeq(timeStepInDay) += zsCalcSizing.DOASSupTemp * fracTimeStepZone;
    zsCalcSizing.DOASSupHumRatSeq(timeStepInDay) += zsCalcSizing.DOASSupHumRat * fracTimeStepZone;
    zsCalcSizing.DOASTotCoolLoadSeq(timeStepInDay) += zsCalcSizing.DOASTotCoolLoad * fracTimeStepZone;
    if (zsCalcSizing.zoneLatentSizing) {
        zsCalcSizing.LatentHeatLoadSeq(timeStepInDay) += zsCalcSizing.HeatLatentLoad * fracTimeStepZone;
        zsCalcSizing.LatentHeatFlowSeq(timeStepInDay) += zsCalcSizing.ZoneHeatLatentMassFlow * fracTimeStepZone;
        zsCalcSizing.LatentCoolLoadSeq(timeStepInDay) += zsCalcSizing.CoolLatentLoad * fracTimeStepZone;
        zsCalcSizing.LatentCoolFlowSeq(timeStepInDay) += zsCalcSizing.ZoneCoolLatentMassFlow * fracTimeStepZone;
        zsCalcSizing.CoolLatentLoadNoDOASSeq(timeStepInDay) += zsCalcSizing.CoolLatentLoadNoDOAS * fracTimeStepZone;
        zsCalcSizing.HeatLatentLoadNoDOASSeq(timeStepInDay) += zsCalcSizing.HeatLatentLoadNoDOAS * fracTimeStepZone;
        zsCalcSizing.CoolLoadNoDOASSeq(timeStepInDay) += zsCalcSizing.CoolLoadNoDOAS * fracTimeStepZone;
        zsCalcSizing.HeatLoadNoDOASSeq(timeStepInDay) += zsCalcSizing.HeatLoadNoDOAS * fracTimeStepZone;
    }
}

void updateZoneSizingEndDayMovingAvg(DataSizing::ZoneSizingData &zsCalcSizing, int const numTimeStepsInAvg)
{
    General::MovingAvg(zsCalcSizing.CoolFlowSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.CoolLoadSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.HeatFlowSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.HeatLoadSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.CoolZoneRetTempSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.HeatZoneRetTempSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.DOASHeatAddSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.DOASLatAddSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.CoolLatentLoadNoDOASSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.HeatLatentLoadNoDOASSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.CoolLoadNoDOASSeq, numTimeStepsInAvg);
    General::MovingAvg(zsCalcSizing.HeatLoadNoDOASSeq, numTimeStepsInAvg);

    if (zsCalcSizing.zoneLatentSizing) {
        General::MovingAvg(zsCalcSizing.LatentHeatLoadSeq, numTimeStepsInAvg);
        General::MovingAvg(zsCalcSizing.LatentHeatFlowSeq, numTimeStepsInAvg);
        General::MovingAvg(zsCalcSizing.LatentCoolLoadSeq, numTimeStepsInAvg);
        General::MovingAvg(zsCalcSizing.LatentCoolFlowSeq, numTimeStepsInAvg);
    }
}

void updateZoneSizingEndDay(DataSizing::ZoneSizingData &zsCalcSizing,
                            DataSizing::ZoneSizingData &zsCalcFinalSizing,
                            int const numTimeStepInDay,
                            DataSizing::DesDayWeathData const &desDayWeath,
                            Real64 const stdRhoAir)
{
    zsCalcFinalSizing.CoolSizingType = zsCalcSizing.CoolSizingType;
    zsCalcFinalSizing.HeatSizingType = zsCalcSizing.HeatSizingType;

    // save the sequence values at the heating peak
    for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
        if (zsCalcSizing.HeatLoadSeq(TimeStepIndex) > zsCalcSizing.DesHeatLoad) {
            zsCalcSizing.DesHeatLoad = zsCalcSizing.HeatLoadSeq(TimeStepIndex);
            zsCalcSizing.DesHeatMassFlow = zsCalcSizing.HeatFlowSeq(TimeStepIndex);
            zsCalcSizing.ZoneTempAtHeatPeak = zsCalcSizing.HeatZoneTempSeq(TimeStepIndex);
            zsCalcSizing.OutTempAtHeatPeak = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
            zsCalcSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.HeatZoneRetTempSeq(TimeStepIndex);
            zsCalcSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.HeatZoneHumRatSeq(TimeStepIndex);
            zsCalcSizing.OutHumRatAtHeatPeak = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtHeatMax = TimeStepIndex;
        }
    }
    // save the sequence values at the latent heating peak
    if (zsCalcSizing.zoneLatentSizing) {
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if (zsCalcSizing.LatentHeatLoadSeq(TimeStepIndex) > zsCalcSizing.DesLatentHeatLoad) {
                zsCalcSizing.DesLatentHeatLoad = zsCalcSizing.LatentHeatLoadSeq(TimeStepIndex);
                zsCalcSizing.DesLatentHeatMassFlow = zsCalcSizing.LatentHeatFlowSeq(TimeStepIndex);
                zsCalcSizing.ZoneHeatLatentMassFlow = zsCalcSizing.LatentHeatFlowSeq(TimeStepIndex);
                zsCalcSizing.ZoneTempAtLatentHeatPeak = zsCalcSizing.HeatZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtLatentHeatPeak = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneHumRatAtLatentHeatPeak = zsCalcSizing.HeatZoneHumRatSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtLatentHeatPeak = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
                zsCalcSizing.ZoneRetTempAtLatentHeatPeak = zsCalcSizing.HeatZoneRetTempSeq(TimeStepIndex);
                zsCalcSizing.TimeStepNumAtLatentHeatMax = TimeStepIndex;
            }
        }
    }
    for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
        // select largest load from NoDOAS arrays
        if (zsCalcSizing.HeatLoadNoDOASSeq(TimeStepIndex) > zsCalcSizing.DesHeatLoadNoDOAS) {
            zsCalcSizing.DesHeatLoadNoDOAS = zsCalcSizing.HeatLoadNoDOASSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtHeatNoDOASMax = TimeStepIndex;
        }
        if (zsCalcSizing.HeatLatentLoadNoDOASSeq(TimeStepIndex) > zsCalcSizing.DesLatentHeatLoadNoDOAS) {
            zsCalcSizing.DesLatentHeatLoadNoDOAS = zsCalcSizing.HeatLatentLoadNoDOASSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtLatentHeatNoDOASMax = TimeStepIndex;
        }
        if (zsCalcSizing.CoolLoadNoDOASSeq(TimeStepIndex) > zsCalcSizing.DesCoolLoadNoDOAS) {
            zsCalcSizing.DesCoolLoadNoDOAS = zsCalcSizing.CoolLoadNoDOASSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtCoolNoDOASMax = TimeStepIndex;
        }
        if (zsCalcSizing.CoolLatentLoadNoDOASSeq(TimeStepIndex) > zsCalcSizing.DesLatentCoolLoadNoDOAS) {
            zsCalcSizing.DesLatentCoolLoadNoDOAS = zsCalcSizing.CoolLatentLoadNoDOASSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtLatentCoolNoDOASMax = TimeStepIndex;
        }
    }
    if (zsCalcSizing.DesHeatMassFlow > 0.0) {
        zsCalcSizing.DesHeatVolFlow = zsCalcSizing.DesHeatMassFlow / zsCalcSizing.DesHeatDens;
        Real64 OAFrac = zsCalcSizing.MinOA / max(zsCalcSizing.DesHeatVolFlow, DataHVACGlobals::SmallMassFlow);
        OAFrac = min(1.0, max(0.0, OAFrac));
        int TimeStepAtPeak = zsCalcSizing.TimeStepNumAtHeatMax;
        zsCalcSizing.DesHeatCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneTempAtHeatPeak;
        zsCalcSizing.DesHeatCoilInHumRat = OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneHumRatAtHeatPeak;
    }
    if (zsCalcSizing.zoneLatentSizing && zsCalcSizing.DesLatentHeatMassFlow > 0.0) {
        zsCalcSizing.DesLatentHeatVolFlow = zsCalcSizing.DesLatentHeatMassFlow / stdRhoAir;
        Real64 OAFrac = zsCalcSizing.MinOA / max(zsCalcSizing.DesHeatVolFlow, DataHVACGlobals::SmallMassFlow);
        OAFrac = min(1.0, max(0.0, OAFrac));
        int TimeStepAtPeak = zsCalcSizing.TimeStepNumAtLatentHeatMax;
        zsCalcSizing.DesLatentHeatCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneTempAtHeatPeak;
        zsCalcSizing.DesLatentHeatCoilInHumRat = OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneHumRatAtHeatPeak;
    }
    // save the sequence values at the cooling peak
    for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
        if (zsCalcSizing.CoolLoadSeq(TimeStepIndex) > zsCalcSizing.DesCoolLoad) {
            zsCalcSizing.DesCoolLoad = zsCalcSizing.CoolLoadSeq(TimeStepIndex);
            zsCalcSizing.DesCoolMassFlow = zsCalcSizing.CoolFlowSeq(TimeStepIndex);
            zsCalcSizing.ZoneTempAtCoolPeak = zsCalcSizing.CoolZoneTempSeq(TimeStepIndex);
            zsCalcSizing.OutTempAtCoolPeak = zsCalcSizing.CoolOutTempSeq(TimeStepIndex);
            zsCalcSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.CoolZoneRetTempSeq(TimeStepIndex);
            zsCalcSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.CoolZoneHumRatSeq(TimeStepIndex);
            zsCalcSizing.OutHumRatAtCoolPeak = zsCalcSizing.CoolOutHumRatSeq(TimeStepIndex);
            zsCalcSizing.TimeStepNumAtCoolMax = TimeStepIndex;
        }
    }
    if (zsCalcSizing.zoneLatentSizing) {
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if (zsCalcSizing.LatentCoolLoadSeq(TimeStepIndex) > zsCalcSizing.DesLatentCoolLoad) {
                zsCalcSizing.DesLatentCoolLoad = zsCalcSizing.LatentCoolLoadSeq(TimeStepIndex);
                zsCalcSizing.DesLatentCoolMassFlow = zsCalcSizing.LatentCoolFlowSeq(TimeStepIndex);
                zsCalcSizing.ZoneTempAtLatentCoolPeak = zsCalcSizing.CoolZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtLatentCoolPeak = zsCalcSizing.CoolOutTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneHumRatAtLatentCoolPeak = zsCalcSizing.CoolZoneHumRatSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtLatentCoolPeak = zsCalcSizing.CoolOutHumRatSeq(TimeStepIndex);
                zsCalcSizing.ZoneRetTempAtLatentCoolPeak = zsCalcSizing.CoolZoneRetTempSeq(TimeStepIndex);
                zsCalcSizing.TimeStepNumAtLatentCoolMax = TimeStepIndex;
            }
        }
    }
    if (zsCalcSizing.DesCoolMassFlow > 0.0) {
        zsCalcSizing.DesCoolVolFlow = zsCalcSizing.DesCoolMassFlow / zsCalcSizing.DesCoolDens;
        Real64 OAFrac = zsCalcSizing.MinOA / max(zsCalcSizing.DesCoolVolFlow, DataHVACGlobals::SmallMassFlow);
        OAFrac = min(1.0, max(0.0, OAFrac));
        int TimeStepAtPeak = zsCalcSizing.TimeStepNumAtCoolMax;
        zsCalcSizing.DesCoolCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneTempAtCoolPeak;
        zsCalcSizing.DesCoolCoilInHumRat = OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneHumRatAtCoolPeak;
    }
    if (zsCalcSizing.zoneLatentSizing && zsCalcSizing.DesLatentCoolMassFlow > 0.0) {
        zsCalcSizing.DesLatentCoolVolFlow = zsCalcSizing.DesLatentCoolMassFlow / stdRhoAir;
        Real64 OAFrac = zsCalcSizing.MinOA / max(zsCalcSizing.DesCoolVolFlow, DataHVACGlobals::SmallMassFlow);
        OAFrac = min(1.0, max(0.0, OAFrac));
        int TimeStepAtPeak = zsCalcSizing.TimeStepNumAtLatentCoolMax;
        zsCalcSizing.DesLatentCoolCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneTempAtCoolPeak;
        zsCalcSizing.DesLatentCoolCoilInHumRat = OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zsCalcSizing.ZoneHumRatAtCoolPeak;
    }
    // from all the design periods, choose the one needing the most heating and save all its design variables in zsCalcFinalSizing
    if (zsCalcSizing.DesHeatVolFlow > zsCalcFinalSizing.DesHeatVolFlow) {
        zsCalcFinalSizing.DesHeatVolFlow = zsCalcSizing.DesHeatVolFlow;
        zsCalcFinalSizing.DesHeatLoad = zsCalcSizing.DesHeatLoad;
        zsCalcFinalSizing.DesHeatMassFlow = zsCalcSizing.DesHeatMassFlow;
        zsCalcFinalSizing.HeatDesDay = zsCalcSizing.HeatDesDay;
        zsCalcFinalSizing.DesHeatDens = zsCalcSizing.DesHeatDens;
        zsCalcFinalSizing.HeatFlowSeq = zsCalcSizing.HeatFlowSeq;
        zsCalcFinalSizing.HeatLoadSeq = zsCalcSizing.HeatLoadSeq;
        zsCalcFinalSizing.HeatZoneTempSeq = zsCalcSizing.HeatZoneTempSeq;
        zsCalcFinalSizing.HeatOutTempSeq = zsCalcSizing.HeatOutTempSeq;
        zsCalcFinalSizing.HeatZoneRetTempSeq = zsCalcSizing.HeatZoneRetTempSeq;
        zsCalcFinalSizing.HeatZoneHumRatSeq = zsCalcSizing.HeatZoneHumRatSeq;
        zsCalcFinalSizing.HeatOutHumRatSeq = zsCalcSizing.HeatOutHumRatSeq;
        zsCalcFinalSizing.ZoneTempAtHeatPeak = zsCalcSizing.ZoneTempAtHeatPeak;
        zsCalcFinalSizing.OutTempAtHeatPeak = zsCalcSizing.OutTempAtHeatPeak;
        zsCalcFinalSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.ZoneRetTempAtHeatPeak;
        zsCalcFinalSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.ZoneHumRatAtHeatPeak;
        zsCalcFinalSizing.OutHumRatAtHeatPeak = zsCalcSizing.OutHumRatAtHeatPeak;
        zsCalcFinalSizing.HeatDDNum = zsCalcSizing.HeatDDNum;
        zsCalcFinalSizing.cHeatDDDate = desDayWeath.DateString;
        zsCalcFinalSizing.TimeStepNumAtHeatMax = zsCalcSizing.TimeStepNumAtHeatMax;
        zsCalcFinalSizing.DesHeatCoilInTemp = zsCalcSizing.DesHeatCoilInTemp;
        zsCalcFinalSizing.DesHeatCoilInHumRat = zsCalcSizing.DesHeatCoilInHumRat;
    } else {
        zsCalcFinalSizing.DesHeatDens = stdRhoAir;
        // save design heating load when the there is design heating load and the design heating volume flow rate is zero, i.e., when
        // design heating volume flow rate is set to zero due to heating supply air temp less than zone thermostat temperature
        if (zsCalcSizing.DesHeatLoad > zsCalcFinalSizing.DesHeatLoad) {
            zsCalcFinalSizing.DesHeatLoad = zsCalcSizing.DesHeatLoad;
            zsCalcFinalSizing.HeatDesDay = zsCalcSizing.HeatDesDay;
            zsCalcFinalSizing.HeatLoadSeq = zsCalcSizing.HeatLoadSeq;
            zsCalcFinalSizing.HeatZoneTempSeq = zsCalcSizing.HeatZoneTempSeq;
            zsCalcFinalSizing.HeatOutTempSeq = zsCalcSizing.HeatOutTempSeq;
            zsCalcFinalSizing.HeatZoneRetTempSeq = zsCalcSizing.HeatZoneRetTempSeq;
            zsCalcFinalSizing.HeatZoneHumRatSeq = zsCalcSizing.HeatZoneHumRatSeq;
            zsCalcFinalSizing.HeatOutHumRatSeq = zsCalcSizing.HeatOutHumRatSeq;
            zsCalcFinalSizing.ZoneTempAtHeatPeak = zsCalcSizing.ZoneTempAtHeatPeak;
            zsCalcFinalSizing.OutTempAtHeatPeak = zsCalcSizing.OutTempAtHeatPeak;
            zsCalcFinalSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.ZoneRetTempAtHeatPeak;
            zsCalcFinalSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.ZoneHumRatAtHeatPeak;
            zsCalcFinalSizing.OutHumRatAtHeatPeak = zsCalcSizing.OutHumRatAtHeatPeak;
            zsCalcFinalSizing.HeatDDNum = zsCalcSizing.HeatDDNum;
            zsCalcFinalSizing.cHeatDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.TimeStepNumAtHeatMax = zsCalcSizing.TimeStepNumAtHeatMax;
            zsCalcFinalSizing.DesHeatCoilInTemp = zsCalcSizing.DesHeatCoilInTemp;
            zsCalcFinalSizing.DesHeatCoilInHumRat = zsCalcSizing.DesHeatCoilInHumRat;
            zsCalcFinalSizing.HeatTstatTemp = zsCalcSizing.HeatTstatTemp;
        }
    }
    if (zsCalcSizing.zoneLatentSizing) {
        // from all the design periods, choose the one needing the most latent heating and save all its design variables in
        // zsCalcFinalSizing
        if (zsCalcSizing.DesLatentHeatVolFlow > zsCalcFinalSizing.DesLatentHeatVolFlow) {
            zsCalcFinalSizing.DesLatentHeatVolFlow = zsCalcSizing.DesLatentHeatVolFlow;
            zsCalcFinalSizing.DesLatentHeatMassFlow = zsCalcSizing.ZoneHeatLatentMassFlow;
            zsCalcFinalSizing.DesLatentHeatLoad = zsCalcSizing.DesLatentHeatLoad;
            zsCalcFinalSizing.ZoneTempAtLatentHeatPeak = zsCalcSizing.ZoneTempAtLatentHeatPeak;
            zsCalcFinalSizing.ZoneHumRatAtLatentHeatPeak = zsCalcSizing.ZoneHumRatAtLatentHeatPeak;
            zsCalcFinalSizing.ZoneRetTempAtLatentHeatPeak = zsCalcSizing.ZoneRetTempAtLatentHeatPeak;
            zsCalcFinalSizing.DesLatentHeatCoilInTemp = zsCalcSizing.DesLatentHeatCoilInTemp;
            zsCalcFinalSizing.DesLatentHeatCoilInHumRat = zsCalcSizing.DesLatentHeatCoilInHumRat;
            zsCalcFinalSizing.LatHeatDesDay = zsCalcSizing.LatHeatDesDay;
            zsCalcFinalSizing.cLatentHeatDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.LatentHeatDDNum = zsCalcSizing.LatentHeatDDNum;
            zsCalcFinalSizing.TimeStepNumAtLatentHeatMax = zsCalcSizing.TimeStepNumAtLatentHeatMax;
            zsCalcFinalSizing.LatentHeatLoadSeq = zsCalcSizing.LatentHeatLoadSeq;
            zsCalcFinalSizing.LatentHeatFlowSeq = zsCalcSizing.LatentHeatFlowSeq;
        } else {
            // save design latent heating load when the there is design load and the design volume flow rate is zero, i.e., when
            // design latent heating volume flow rate is set to zero due to heating supply air humrat is less than zone humidistat humrat
            if (zsCalcSizing.DesLatentHeatLoad > zsCalcFinalSizing.DesLatentHeatLoad) {
                zsCalcFinalSizing.DesLatentHeatLoad = zsCalcSizing.DesLatentHeatLoad;
                zsCalcFinalSizing.cLatentHeatDDDate = desDayWeath.DateString;
                zsCalcFinalSizing.LatentHeatDDNum = zsCalcSizing.LatentHeatDDNum;
                zsCalcFinalSizing.TimeStepNumAtLatentHeatMax = zsCalcSizing.TimeStepNumAtLatentHeatMax;
                zsCalcFinalSizing.LatentHeatLoadSeq = zsCalcSizing.LatentHeatLoadSeq;
                zsCalcFinalSizing.LatentHeatFlowSeq = zsCalcSizing.LatentHeatFlowSeq;
            }
        }
    }
    // select largest load from NoDOAS arrays
    if (zsCalcSizing.DesHeatLoadNoDOAS > zsCalcFinalSizing.DesHeatLoadNoDOAS) {
        zsCalcFinalSizing.DesHeatLoadNoDOAS = zsCalcSizing.DesHeatLoadNoDOAS;
        zsCalcFinalSizing.HeatLoadNoDOASSeq = zsCalcSizing.HeatLoadNoDOASSeq;
        zsCalcFinalSizing.HeatNoDOASDDNum = zsCalcSizing.HeatNoDOASDDNum;
        zsCalcFinalSizing.HeatNoDOASDesDay = zsCalcSizing.HeatNoDOASDesDay;
        zsCalcFinalSizing.TimeStepNumAtHeatNoDOASMax = zsCalcSizing.TimeStepNumAtHeatNoDOASMax;
    }
    if (zsCalcSizing.DesLatentHeatLoadNoDOAS > zsCalcFinalSizing.DesLatentHeatLoadNoDOAS) {
        zsCalcFinalSizing.DesLatentHeatLoadNoDOAS = zsCalcSizing.DesLatentHeatLoadNoDOAS;
        zsCalcFinalSizing.HeatLatentLoadNoDOASSeq = zsCalcSizing.HeatLatentLoadNoDOASSeq;
        zsCalcFinalSizing.LatentHeatNoDOASDDNum = zsCalcSizing.LatentHeatNoDOASDDNum;
        zsCalcFinalSizing.LatHeatNoDOASDesDay = zsCalcSizing.LatHeatNoDOASDesDay;
        zsCalcFinalSizing.TimeStepNumAtLatentHeatNoDOASMax = zsCalcSizing.TimeStepNumAtLatentHeatNoDOASMax;
    }
    // from all the design periods, choose the one needing the most Cooling and save all its design variables in zsCalcFinalSizing
    if (zsCalcSizing.DesCoolVolFlow > zsCalcFinalSizing.DesCoolVolFlow) {
        zsCalcFinalSizing.DesCoolVolFlow = zsCalcSizing.DesCoolVolFlow;
        zsCalcFinalSizing.DesCoolLoad = zsCalcSizing.DesCoolLoad;
        zsCalcFinalSizing.DesCoolMassFlow = zsCalcSizing.DesCoolMassFlow;
        zsCalcFinalSizing.CoolDesDay = zsCalcSizing.CoolDesDay;
        zsCalcFinalSizing.DesCoolDens = zsCalcSizing.DesCoolDens;
        zsCalcFinalSizing.CoolFlowSeq = zsCalcSizing.CoolFlowSeq;
        zsCalcFinalSizing.CoolLoadSeq = zsCalcSizing.CoolLoadSeq;
        zsCalcFinalSizing.CoolZoneTempSeq = zsCalcSizing.CoolZoneTempSeq;
        zsCalcFinalSizing.CoolOutTempSeq = zsCalcSizing.CoolOutTempSeq;
        zsCalcFinalSizing.CoolZoneRetTempSeq = zsCalcSizing.CoolZoneRetTempSeq;
        zsCalcFinalSizing.CoolZoneHumRatSeq = zsCalcSizing.CoolZoneHumRatSeq;
        zsCalcFinalSizing.CoolOutHumRatSeq = zsCalcSizing.CoolOutHumRatSeq;
        zsCalcFinalSizing.ZoneTempAtCoolPeak = zsCalcSizing.ZoneTempAtCoolPeak;
        zsCalcFinalSizing.OutTempAtCoolPeak = zsCalcSizing.OutTempAtCoolPeak;
        zsCalcFinalSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.ZoneRetTempAtCoolPeak;
        zsCalcFinalSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.ZoneHumRatAtCoolPeak;
        zsCalcFinalSizing.OutHumRatAtCoolPeak = zsCalcSizing.OutHumRatAtCoolPeak;
        zsCalcFinalSizing.CoolDDNum = zsCalcSizing.CoolDDNum;
        zsCalcFinalSizing.cCoolDDDate = desDayWeath.DateString;
        zsCalcFinalSizing.TimeStepNumAtCoolMax = zsCalcSizing.TimeStepNumAtCoolMax;
        zsCalcFinalSizing.DesCoolCoilInTemp = zsCalcSizing.DesCoolCoilInTemp;
        zsCalcFinalSizing.DesCoolCoilInHumRat = zsCalcSizing.DesCoolCoilInHumRat;
    } else {
        zsCalcFinalSizing.DesCoolDens = stdRhoAir;
        // save design cooling load when the there is design cooling load and the design cooling volume flow rate is zero, i.e., when
        // design cooling volume flow rate is set to zero due to cooling supply air temp greater than zone thermostat temperature
        if (zsCalcSizing.DesCoolLoad > zsCalcFinalSizing.DesCoolLoad) {
            zsCalcFinalSizing.DesCoolLoad = zsCalcSizing.DesCoolLoad;
            zsCalcFinalSizing.CoolDesDay = zsCalcSizing.CoolDesDay;
            zsCalcFinalSizing.CoolLoadSeq = zsCalcSizing.CoolLoadSeq;
            zsCalcFinalSizing.CoolZoneTempSeq = zsCalcSizing.CoolZoneTempSeq;
            zsCalcFinalSizing.CoolOutTempSeq = zsCalcSizing.CoolOutTempSeq;
            zsCalcFinalSizing.CoolZoneRetTempSeq = zsCalcSizing.CoolZoneRetTempSeq;
            zsCalcFinalSizing.CoolZoneHumRatSeq = zsCalcSizing.CoolZoneHumRatSeq;
            zsCalcFinalSizing.CoolOutHumRatSeq = zsCalcSizing.CoolOutHumRatSeq;
            zsCalcFinalSizing.ZoneTempAtCoolPeak = zsCalcSizing.ZoneTempAtCoolPeak;
            zsCalcFinalSizing.OutTempAtCoolPeak = zsCalcSizing.OutTempAtCoolPeak;
            zsCalcFinalSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.ZoneRetTempAtCoolPeak;
            zsCalcFinalSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.ZoneHumRatAtCoolPeak;
            zsCalcFinalSizing.OutHumRatAtCoolPeak = zsCalcSizing.OutHumRatAtCoolPeak;
            zsCalcFinalSizing.CoolDDNum = zsCalcSizing.CoolDDNum;
            zsCalcFinalSizing.cCoolDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.TimeStepNumAtCoolMax = zsCalcSizing.TimeStepNumAtCoolMax;
            zsCalcFinalSizing.DesCoolCoilInTemp = zsCalcSizing.DesCoolCoilInTemp;
            zsCalcFinalSizing.DesCoolCoilInHumRat = zsCalcSizing.DesCoolCoilInHumRat;
            zsCalcFinalSizing.CoolTstatTemp = zsCalcSizing.CoolTstatTemp;
        }
    }
    if (zsCalcSizing.zoneLatentSizing) {
        // from all the design periods, choose the one needing the most Latent Cooling and save all its design variables in
        // zsCalcFinalSizing
        if (zsCalcSizing.DesLatentCoolVolFlow > zsCalcFinalSizing.DesLatentCoolVolFlow) {
            zsCalcFinalSizing.DesLatentCoolVolFlow = zsCalcSizing.DesLatentCoolVolFlow;
            zsCalcFinalSizing.DesLatentCoolMassFlow = zsCalcSizing.DesLatentCoolMassFlow;
            zsCalcFinalSizing.DesLatentCoolLoad = zsCalcSizing.DesLatentCoolLoad;
            zsCalcFinalSizing.ZoneTempAtLatentCoolPeak = zsCalcSizing.ZoneTempAtLatentCoolPeak;
            zsCalcFinalSizing.ZoneHumRatAtLatentCoolPeak = zsCalcSizing.ZoneHumRatAtLatentCoolPeak;
            zsCalcFinalSizing.ZoneRetTempAtLatentCoolPeak = zsCalcSizing.ZoneRetTempAtLatentCoolPeak;
            zsCalcFinalSizing.DesLatentCoolCoilInTemp = zsCalcSizing.DesLatentCoolCoilInTemp;
            zsCalcFinalSizing.DesLatentCoolCoilInHumRat = zsCalcSizing.DesLatentCoolCoilInHumRat;
            zsCalcFinalSizing.LatCoolDesDay = zsCalcSizing.LatCoolDesDay;
            zsCalcFinalSizing.cLatentCoolDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.LatentCoolDDNum = zsCalcSizing.LatentCoolDDNum;
            zsCalcFinalSizing.TimeStepNumAtLatentCoolMax = zsCalcSizing.TimeStepNumAtLatentCoolMax;
            zsCalcFinalSizing.LatentCoolLoadSeq = zsCalcSizing.LatentCoolLoadSeq;
            zsCalcFinalSizing.LatentCoolFlowSeq = zsCalcSizing.LatentCoolFlowSeq;
        } else {
            // save design latent cooling load when the there is design load and the design volume flow rate is zero, i.e., when
            // design latent cooling volume flow rate is set to zero due to cooling supply air humrat is greater than zone humidistat humrat
            if (zsCalcSizing.DesLatentCoolLoad > zsCalcFinalSizing.DesLatentCoolLoad) {
                zsCalcFinalSizing.DesLatentCoolLoad = zsCalcSizing.DesLatentCoolLoad;
                zsCalcFinalSizing.cLatentCoolDDDate = desDayWeath.DateString;
                zsCalcFinalSizing.LatentCoolDDNum = zsCalcSizing.LatentCoolDDNum;
                zsCalcFinalSizing.LatCoolDesDay = zsCalcSizing.LatCoolDesDay;
                zsCalcFinalSizing.TimeStepNumAtLatentCoolMax = zsCalcSizing.TimeStepNumAtLatentCoolMax;
                zsCalcFinalSizing.LatentCoolLoadSeq = zsCalcSizing.LatentCoolLoadSeq;
            }
        }
    }
    if (zsCalcSizing.DesCoolLoadNoDOAS > zsCalcFinalSizing.DesCoolLoadNoDOAS) {
        zsCalcFinalSizing.DesCoolLoadNoDOAS = zsCalcSizing.DesCoolLoadNoDOAS;
        zsCalcFinalSizing.CoolLoadNoDOASSeq = zsCalcSizing.CoolLoadNoDOASSeq;
        zsCalcFinalSizing.CoolNoDOASDDNum = zsCalcSizing.CoolNoDOASDDNum;
        zsCalcFinalSizing.CoolNoDOASDesDay = zsCalcSizing.CoolNoDOASDesDay;
        zsCalcFinalSizing.TimeStepNumAtCoolNoDOASMax = zsCalcSizing.TimeStepNumAtCoolNoDOASMax;
    }
    if (zsCalcSizing.DesLatentCoolLoadNoDOAS > zsCalcFinalSizing.DesLatentCoolLoadNoDOAS) {
        zsCalcFinalSizing.DesLatentCoolLoadNoDOAS = zsCalcSizing.DesLatentCoolLoadNoDOAS;
        zsCalcFinalSizing.CoolLatentLoadNoDOASSeq = zsCalcSizing.CoolLatentLoadNoDOASSeq;
        zsCalcFinalSizing.LatentCoolNoDOASDDNum = zsCalcSizing.LatentCoolNoDOASDDNum;
        zsCalcFinalSizing.LatCoolNoDOASDesDay = zsCalcSizing.LatCoolNoDOASDesDay;
        zsCalcFinalSizing.TimeStepNumAtLatentCoolNoDOASMax = zsCalcSizing.TimeStepNumAtLatentCoolNoDOASMax;
    }
    // save heat peak conditions when there is no design heating load or design heating volume flow rate, i.e., when
    // zone temperature is always greater than the zone heating thermostat temperature
    if (zsCalcFinalSizing.DesHeatLoad == 0) {
        bool FirstIteration = true;
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if ((zsCalcSizing.HeatZoneTempSeq(TimeStepIndex) < zsCalcSizing.ZoneTempAtHeatPeak) || FirstIteration) {
                zsCalcSizing.ZoneTempAtHeatPeak = zsCalcSizing.HeatZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtHeatPeak = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.HeatZoneRetTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.HeatZoneHumRatSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtHeatPeak = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
                zsCalcSizing.TimeStepNumAtHeatMax = TimeStepIndex;
                FirstIteration = false;
            }
        }
        if (zsCalcSizing.OutTempAtHeatPeak <= zsCalcFinalSizing.OutTempAtHeatPeak) {
            zsCalcFinalSizing.HeatDesDay = zsCalcSizing.HeatDesDay;
            zsCalcFinalSizing.HeatZoneTempSeq = zsCalcSizing.HeatZoneTempSeq;
            zsCalcFinalSizing.HeatOutTempSeq = zsCalcSizing.HeatOutTempSeq;
            zsCalcFinalSizing.HeatZoneRetTempSeq = zsCalcSizing.HeatZoneRetTempSeq;
            zsCalcFinalSizing.HeatZoneHumRatSeq = zsCalcSizing.HeatZoneHumRatSeq;
            zsCalcFinalSizing.HeatOutHumRatSeq = zsCalcSizing.HeatOutHumRatSeq;
            zsCalcFinalSizing.ZoneTempAtHeatPeak = zsCalcSizing.ZoneTempAtHeatPeak;
            zsCalcFinalSizing.OutTempAtHeatPeak = zsCalcSizing.OutTempAtHeatPeak;
            zsCalcFinalSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.ZoneRetTempAtHeatPeak;
            zsCalcFinalSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.ZoneHumRatAtHeatPeak;
            zsCalcFinalSizing.OutHumRatAtHeatPeak = zsCalcSizing.OutHumRatAtHeatPeak;
            zsCalcFinalSizing.HeatDDNum = zsCalcSizing.HeatDDNum;
            zsCalcFinalSizing.cHeatDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.TimeStepNumAtHeatMax = zsCalcSizing.TimeStepNumAtHeatMax;
            zsCalcFinalSizing.DesHeatCoilInTemp = zsCalcSizing.DesHeatCoilInTemp;
            zsCalcFinalSizing.DesHeatCoilInHumRat = zsCalcSizing.DesHeatCoilInHumRat;
            zsCalcFinalSizing.HeatTstatTemp = zsCalcSizing.HeatTstatTemp;
            FirstIteration = false;
        }
    }
    if (zsCalcFinalSizing.zoneLatentSizing && zsCalcFinalSizing.DesLatentHeatLoad == 0) {
        bool FirstIteration = true;
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if (zsCalcSizing.HeatZoneTempSeq(TimeStepIndex) < zsCalcSizing.ZoneTempAtLatentHeatPeak || FirstIteration) {
                zsCalcSizing.ZoneTempAtLatentHeatPeak = zsCalcSizing.HeatZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtLatentHeatPeak = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtLatentHeatPeak = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
            }
            if (zsCalcSizing.HeatOutTempSeq(TimeStepIndex) <= zsCalcFinalSizing.OutTempAtLatentHeatPeak) {
                zsCalcFinalSizing.OutTempAtLatentHeatPeak = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
                zsCalcFinalSizing.OutHumRatAtLatentHeatPeak = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
                zsCalcFinalSizing.LatHeatDesDay = zsCalcSizing.LatHeatDesDay;
                zsCalcFinalSizing.LatentHeatDDNum = zsCalcSizing.LatentHeatDDNum;
                zsCalcFinalSizing.cLatentHeatDDDate = desDayWeath.DateString;
                zsCalcFinalSizing.TimeStepNumAtLatentHeatMax = zsCalcSizing.TimeStepNumAtLatentHeatMax;
            }
            FirstIteration = false;
        }
    }
    // save cool peak conditions when there is no design cooling load or design cooling volume flow rate, i.e., when
    // zone temperature is always less than the zone cooling thermostat temperature
    if (zsCalcFinalSizing.DesCoolLoad == 0) {
        bool FirstIteration = true;
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if ((zsCalcSizing.CoolZoneTempSeq(TimeStepIndex) > zsCalcSizing.ZoneTempAtCoolPeak) || FirstIteration) {
                zsCalcSizing.ZoneTempAtCoolPeak = zsCalcSizing.CoolZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtCoolPeak = zsCalcSizing.CoolOutTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.CoolZoneRetTempSeq(TimeStepIndex);
                zsCalcSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.CoolZoneHumRatSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtCoolPeak = zsCalcSizing.CoolOutHumRatSeq(TimeStepIndex);
                zsCalcSizing.TimeStepNumAtCoolMax = TimeStepIndex;
                FirstIteration = false;
            }
        }
        if (zsCalcSizing.OutTempAtCoolPeak > zsCalcFinalSizing.OutTempAtCoolPeak) {
            zsCalcFinalSizing.CoolDesDay = zsCalcSizing.CoolDesDay;
            zsCalcFinalSizing.CoolZoneTempSeq = zsCalcSizing.CoolZoneTempSeq;
            zsCalcFinalSizing.CoolOutTempSeq = zsCalcSizing.CoolOutTempSeq;
            zsCalcFinalSizing.CoolZoneRetTempSeq = zsCalcSizing.CoolZoneRetTempSeq;
            zsCalcFinalSizing.CoolZoneHumRatSeq = zsCalcSizing.CoolZoneHumRatSeq;
            zsCalcFinalSizing.CoolOutHumRatSeq = zsCalcSizing.CoolOutHumRatSeq;
            zsCalcFinalSizing.ZoneTempAtCoolPeak = zsCalcSizing.ZoneTempAtCoolPeak;
            zsCalcFinalSizing.OutTempAtCoolPeak = zsCalcSizing.OutTempAtCoolPeak;
            zsCalcFinalSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.ZoneRetTempAtCoolPeak;
            zsCalcFinalSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.ZoneHumRatAtCoolPeak;
            zsCalcFinalSizing.OutHumRatAtCoolPeak = zsCalcSizing.OutHumRatAtCoolPeak;
            zsCalcFinalSizing.CoolDDNum = zsCalcSizing.CoolDDNum;
            zsCalcFinalSizing.cCoolDDDate = desDayWeath.DateString;
            zsCalcFinalSizing.TimeStepNumAtCoolMax = zsCalcSizing.TimeStepNumAtCoolMax;
            zsCalcFinalSizing.DesCoolCoilInTemp = zsCalcSizing.DesCoolCoilInTemp;
            zsCalcFinalSizing.DesCoolCoilInHumRat = zsCalcSizing.DesCoolCoilInHumRat;
            zsCalcFinalSizing.CoolTstatTemp = zsCalcSizing.CoolTstatTemp;
        }
    }
    if (zsCalcFinalSizing.zoneLatentSizing && zsCalcFinalSizing.DesLatentCoolLoad == 0) {
        bool FirstIteration = true;
        for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepInDay; ++TimeStepIndex) {
            if (zsCalcSizing.CoolZoneTempSeq(TimeStepIndex) > zsCalcSizing.ZoneTempAtLatentCoolPeak || FirstIteration) {
                zsCalcSizing.ZoneTempAtLatentCoolPeak = zsCalcSizing.CoolZoneTempSeq(TimeStepIndex);
                zsCalcSizing.OutTempAtLatentCoolPeak = zsCalcSizing.CoolOutTempSeq(TimeStepIndex);
                zsCalcSizing.OutHumRatAtLatentCoolPeak = zsCalcSizing.CoolOutHumRatSeq(TimeStepIndex);
                FirstIteration = false;
            }
            if (zsCalcSizing.OutTempAtLatentCoolPeak >= zsCalcFinalSizing.OutTempAtLatentCoolPeak) {
                zsCalcFinalSizing.LatCoolDesDay = zsCalcSizing.LatCoolDesDay;
                zsCalcFinalSizing.LatentCoolDDNum = zsCalcSizing.LatentCoolDDNum;
                zsCalcFinalSizing.cLatentCoolDDDate = desDayWeath.DateString;
                zsCalcFinalSizing.TimeStepNumAtLatentCoolMax = zsCalcSizing.TimeStepNumAtLatentCoolMax;
            }
        }
    }
}

void updateZoneSizingEndZoneSizingCalc1(EnergyPlusData &state, DataSizing::ZoneSizingData const &zsCalcSizing)
{
    if (std::abs(zsCalcSizing.DesCoolLoad) <= 1.e-8) {
        ShowWarningError(state, format("Calculated design cooling load for zone={} is zero.", zsCalcSizing.ZoneName));
        ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
    }
    if (std::abs(zsCalcSizing.DesHeatLoad) <= 1.e-8) {
        ShowWarningError(state, format("Calculated design heating load for zone={} is zero.", zsCalcSizing.ZoneName));
        ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
    }

    Real64 SupplyTemp = 0.0;
    Real64 DeltaTemp = 0.0;
    // Should this be done only if there is a cooling load? Or would this message help determine why there was no load?
    if (std::abs(zsCalcSizing.DesCoolLoad) > 1.e-8) {
        // check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
        if (zsCalcSizing.ZnCoolDgnSAMethod == SupplyAirTemperature) {
            SupplyTemp = zsCalcSizing.CoolDesTemp;
            DeltaTemp = SupplyTemp - zsCalcSizing.ZoneTempAtCoolPeak;
        } else {
            DeltaTemp = -std::abs(zsCalcSizing.CoolDesTempDiff);
            SupplyTemp = DeltaTemp + zsCalcSizing.ZoneTempAtCoolPeak;
        }

        // check for low delta T to avoid very high flow rates
        if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
            if (std::abs(DeltaTemp) >= 2.0) {                                                    // Vdot exceeds 3000 cfm/ton @ DT=2
                ShowWarningError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of zone temperature");
            } else {
                ShowSevereError(state, "UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of zone temperature");
            }
            ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
            ShowContinueError(state, format("...zone name = {}", zsCalcSizing.ZoneName));
            ShowContinueError(state, format("...design sensible cooling load = {:.2R} W", zsCalcSizing.DesCoolLoad));
            ShowContinueError(state, format("...thermostat set point temp    = {:.3R} C", zsCalcSizing.CoolTstatTemp));
            ShowContinueError(state, format("...zone temperature             = {:.3R} C", zsCalcSizing.ZoneTempAtCoolPeak));
            ShowContinueError(state, format("...supply air temperature       = {:.3R} C", SupplyTemp));
            ShowContinueError(state, format("...temperature difference       = {:.5R} C", DeltaTemp));
            ShowContinueError(state, format("...calculated volume flow rate  = {:.5R} m3/s", (zsCalcSizing.DesCoolVolFlow)));
            ShowContinueError(state, format("...calculated mass flow rate    = {:.5R} kg/s", (zsCalcSizing.DesCoolMassFlow)));
            if (SupplyTemp > zsCalcSizing.ZoneTempAtCoolPeak)
                ShowContinueError(state, "...Note: supply air temperature should be less than zone temperature during cooling air flow calculations");
        } else if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff && SupplyTemp > zsCalcSizing.ZoneTempAtCoolPeak) {
            ShowSevereError(state, "UpdateZoneSizing: Supply air temperature is greater than zone temperature during cooling air flow calculations");
            ShowContinueError(state, format("...calculated volume flow rate  = {:.5R} m3/s", (zsCalcSizing.DesCoolVolFlow)));
            ShowContinueError(state, format("...calculated mass flow rate    = {:.5R} kg/s", (zsCalcSizing.DesCoolMassFlow)));
            ShowContinueError(state, format("...thermostat set point temp    = {:.3R} C", zsCalcSizing.CoolTstatTemp));
            ShowContinueError(state, format("...zone temperature            = {:.3R} C", zsCalcSizing.ZoneTempAtCoolPeak));
            ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
            ShowContinueError(state, format("...occurs in zone              = {}", zsCalcSizing.ZoneName));
            ShowContinueError(state, "...Note: supply air temperature should be less than zone temperature during cooling air flow calculations");
        }
    }
    // Should this be done only if there is a heating load? Or would this message help determine why there was no load?
    if (std::abs(zsCalcSizing.DesHeatLoad) > 1.e-8) { // ABS() ?
        // check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
        if (zsCalcSizing.ZnHeatDgnSAMethod == SupplyAirTemperature) {
            SupplyTemp = zsCalcSizing.HeatDesTemp;
            DeltaTemp = SupplyTemp - zsCalcSizing.ZoneTempAtHeatPeak;
        } else {
            DeltaTemp = zsCalcSizing.HeatDesTempDiff;
            SupplyTemp = DeltaTemp + zsCalcSizing.ZoneTempAtHeatPeak;
        }

        if (std::abs(DeltaTemp) < 5.0 && std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) { // Vdot exceeds 1200 cfm/ton @ DT=5
            if (std::abs(DeltaTemp) >= 2.0) {                                                    // Vdot exceeds 3000 cfm/ton @ DT=2
                ShowWarningError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of zone temperature");
            } else {
                ShowSevereError(state, "UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of zone temperature");
            }
            ShowContinueError(state, "...check zone thermostat set point and design supply air temperatures");
            ShowContinueError(state, format("...zone name = {}", zsCalcSizing.ZoneName));
            ShowContinueError(state, format("...design heating load         = {:.2R} W", zsCalcSizing.DesHeatLoad));
            ShowContinueError(state, format("...thermostat set point temp   = {:.3R} C", zsCalcSizing.HeatTstatTemp));
            ShowContinueError(state, format("...zone temperature            = {:.3R} C", zsCalcSizing.ZoneTempAtHeatPeak));
            ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
            ShowContinueError(state, format("...temperature difference      = {:.5R} C", DeltaTemp));
            ShowContinueError(state, format("...calculated volume flow rate = {:.5R} m3/s", (zsCalcSizing.DesHeatVolFlow)));
            ShowContinueError(state, format("...calculated mass flow rate   = {:.5R} kg/s", (zsCalcSizing.DesHeatMassFlow)));
            if (SupplyTemp < zsCalcSizing.ZoneTempAtHeatPeak)
                ShowContinueError(state,
                                  "...Note: supply air temperature should be greater than zone temperature during heating air "
                                  "flow calculations");
        } else if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff && SupplyTemp < zsCalcSizing.ZoneTempAtHeatPeak) {
            ShowSevereError(state, "UpdateZoneSizing: Supply air temperature is less than zone temperature during heating air flow calculations");
            ShowContinueError(state, format("...calculated design heating volume flow rate = {:.5R} m3/s", (zsCalcSizing.DesHeatVolFlow)));
            ShowContinueError(state, format("...calculated design heating mass flow rate   = {:.5R} kg/s", (zsCalcSizing.DesHeatMassFlow)));
            ShowContinueError(state, format("...thermostat set point temp   = {:.3R} C", zsCalcSizing.HeatTstatTemp));
            ShowContinueError(state, format("...zone temperature            = {:.3R} C", zsCalcSizing.ZoneTempAtHeatPeak));
            ShowContinueError(state, format("...supply air temperature      = {:.3R} C", SupplyTemp));
            ShowContinueError(state, format("...occurs in zone              = {}", zsCalcSizing.ZoneName));
            ShowContinueError(state,
                              "...Note: supply air temperature should be greater than zone temperature during heating air "
                              "flow calculations");
        }
    }
}

void updateZoneSizingEndZoneSizingCalc2(DataSizing::ZoneSizingData &zsCalcSizing, int const timeStepIndex, int const hourPrint, int const minutes)
{
    // SpaceSizing TODO: There's gotta be a better place to set these timestamps
    if (timeStepIndex == zsCalcSizing.TimeStepNumAtHeatMax) {
        zsCalcSizing.HeatPeakDateHrMin = zsCalcSizing.cHeatDDDate + ' ' + format(PeakHrMinFmt, hourPrint, minutes);
    }
    if (timeStepIndex == zsCalcSizing.TimeStepNumAtCoolMax) {
        zsCalcSizing.CoolPeakDateHrMin = zsCalcSizing.cCoolDDDate + ' ' + format(PeakHrMinFmt, hourPrint, minutes);
    }
    if (timeStepIndex == zsCalcSizing.TimeStepNumAtLatentHeatMax) {
        zsCalcSizing.LatHeatPeakDateHrMin = zsCalcSizing.cLatentHeatDDDate + ' ' + format(PeakHrMinFmt, hourPrint, minutes);
    }
    if (timeStepIndex == zsCalcSizing.TimeStepNumAtLatentCoolMax) {
        zsCalcSizing.LatCoolPeakDateHrMin = zsCalcSizing.cLatentCoolDDDate + ' ' + format(PeakHrMinFmt, hourPrint, minutes);
    }
}

void updateZoneSizingEndZoneSizingCalc3(DataSizing::ZoneSizingData &zsCalcFinalSizing,
                                        Array2D<DataSizing::ZoneSizingData> &zsCalcSizing,
                                        bool &anyLatentLoad,
                                        int const zoneOrSpaceNum)
{
    // latent sizing data has the same variables as sensible sizing data
    // if the user has specified latent sizing, move the latent sizing data into the final calc arrays
    // this method allows all upstream sizing functions to use the same data as before (e.g., DesCoolVolFlow)
    // if sensible sizing, use sensible data. if latent sizing, use latent data (if there is latent data).
    // if sensible or latent sizing, use larger of sensible and latent based on volume flow rate

    if ((zsCalcFinalSizing.zoneSizingMethod == ZoneSizing::Latent && zsCalcFinalSizing.DesLatentCoolVolFlow > 0.0) ||
        (zsCalcFinalSizing.zoneSizingMethod == ZoneSizing::SensibleAndLatent &&
         zsCalcFinalSizing.DesLatentCoolLoad > zsCalcFinalSizing.DesCoolLoad)) {
        anyLatentLoad = true;
        zsCalcFinalSizing.CoolSizingType = "Latent Cooling"; // string reported to eio
        zsCalcFinalSizing.DesCoolVolFlow = zsCalcFinalSizing.DesLatentCoolVolFlow;
        zsCalcFinalSizing.DesCoolMassFlow = zsCalcFinalSizing.DesLatentCoolMassFlow;
        zsCalcFinalSizing.DesCoolLoad = zsCalcFinalSizing.DesLatentCoolLoad;
        zsCalcFinalSizing.CoolDesDay = zsCalcFinalSizing.LatCoolDesDay;
        zsCalcFinalSizing.cCoolDDDate = zsCalcFinalSizing.cLatentCoolDDDate;
        zsCalcFinalSizing.CoolDDNum = zsCalcFinalSizing.LatentCoolDDNum;
        zsCalcFinalSizing.TimeStepNumAtCoolMax = zsCalcFinalSizing.TimeStepNumAtLatentCoolMax;
        zsCalcFinalSizing.CoolFlowSeq = zsCalcFinalSizing.LatentCoolFlowSeq;
        zsCalcFinalSizing.DesCoolCoilInTemp = zsCalcFinalSizing.DesLatentCoolCoilInTemp;
        zsCalcFinalSizing.DesCoolCoilInHumRat = zsCalcFinalSizing.DesLatentCoolCoilInHumRat;
        zsCalcFinalSizing.ZoneRetTempAtCoolPeak = zsCalcFinalSizing.ZoneRetTempAtLatentCoolPeak;
        zsCalcFinalSizing.ZoneTempAtCoolPeak = zsCalcFinalSizing.ZoneTempAtLatentCoolPeak;
        zsCalcFinalSizing.ZoneHumRatAtCoolPeak = zsCalcFinalSizing.ZoneHumRatAtLatentCoolPeak;
        zsCalcFinalSizing.CoolPeakDateHrMin = zsCalcFinalSizing.LatCoolPeakDateHrMin;

        // the zone supply air humrat used for latent sizing is required to adequately size coil capacity
        if (zsCalcFinalSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio) {
            zsCalcFinalSizing.CoolDesHumRat = zsCalcFinalSizing.LatentCoolDesHumRat;
        } else {
            zsCalcFinalSizing.CoolDesHumRat = zsCalcFinalSizing.ZoneHumRatAtLatentCoolPeak - zsCalcFinalSizing.CoolDesHumRatDiff;
        }

        if (zsCalcFinalSizing.LatentCoolDDNum > 0) {
            auto &calcZoneSizing = zsCalcSizing(zsCalcFinalSizing.LatentCoolDDNum, zoneOrSpaceNum);
            calcZoneSizing.DesCoolVolFlow = calcZoneSizing.DesLatentCoolVolFlow;
            calcZoneSizing.DesCoolMassFlow = calcZoneSizing.DesLatentCoolMassFlow;
            calcZoneSizing.DesCoolLoad = calcZoneSizing.DesLatentCoolLoad;
            calcZoneSizing.CoolDesDay = calcZoneSizing.LatCoolDesDay;
            calcZoneSizing.cCoolDDDate = zsCalcFinalSizing.cLatentCoolDDDate; // this has correct CoolDDDate
            calcZoneSizing.CoolDDNum = calcZoneSizing.LatentCoolDDNum;
            calcZoneSizing.TimeStepNumAtCoolMax = calcZoneSizing.TimeStepNumAtLatentCoolMax;
            calcZoneSizing.CoolFlowSeq = calcZoneSizing.LatentCoolFlowSeq;
            calcZoneSizing.DesCoolCoilInTemp = calcZoneSizing.DesLatentCoolCoilInTemp;
            calcZoneSizing.DesCoolCoilInHumRat = calcZoneSizing.DesLatentCoolCoilInHumRat;
            calcZoneSizing.ZoneRetTempAtCoolPeak = calcZoneSizing.ZoneRetTempAtLatentCoolPeak;
            calcZoneSizing.ZoneTempAtCoolPeak = calcZoneSizing.ZoneTempAtLatentCoolPeak;
            calcZoneSizing.ZoneHumRatAtCoolPeak = calcZoneSizing.ZoneHumRatAtLatentCoolPeak;
            calcZoneSizing.CoolPeakDateHrMin = zsCalcFinalSizing.LatCoolPeakDateHrMin;

            // the zone supply air humrat used for latent sizing is required to adequately size coil capacity
            if (calcZoneSizing.ZnLatCoolDgnSAMethod == SupplyAirHumidityRatio) {
                calcZoneSizing.CoolDesHumRat = calcZoneSizing.LatentCoolDesHumRat;
            } else {
                calcZoneSizing.CoolDesHumRat = calcZoneSizing.ZoneHumRatAtLatentCoolPeak - calcZoneSizing.CoolDesHumRatDiff;
            }
        }
    }
    if ((zsCalcFinalSizing.zoneSizingMethod == ZoneSizing::Latent && zsCalcFinalSizing.DesLatentHeatVolFlow > 0.0) ||
        (zsCalcFinalSizing.zoneSizingMethod == ZoneSizing::SensibleAndLatent &&
         zsCalcFinalSizing.DesLatentHeatLoad > zsCalcFinalSizing.DesHeatLoad)) {

        zsCalcFinalSizing.HeatSizingType = "Latent Heating"; // string reported to eio
        zsCalcFinalSizing.DesHeatVolFlow = zsCalcFinalSizing.DesLatentHeatVolFlow;
        zsCalcFinalSizing.DesHeatMassFlow = zsCalcFinalSizing.DesLatentHeatMassFlow;
        zsCalcFinalSizing.DesHeatLoad = zsCalcFinalSizing.DesLatentHeatLoad;
        zsCalcFinalSizing.HeatDesDay = zsCalcFinalSizing.LatHeatDesDay;
        zsCalcFinalSizing.cHeatDDDate = zsCalcFinalSizing.cLatentHeatDDDate;
        zsCalcFinalSizing.HeatDDNum = zsCalcFinalSizing.LatentHeatDDNum;
        zsCalcFinalSizing.TimeStepNumAtHeatMax = zsCalcFinalSizing.TimeStepNumAtLatentHeatMax;
        zsCalcFinalSizing.HeatFlowSeq = zsCalcFinalSizing.LatentHeatFlowSeq;
        zsCalcFinalSizing.DesHeatCoilInTemp = zsCalcFinalSizing.DesLatentHeatCoilInTemp;
        zsCalcFinalSizing.DesHeatCoilInHumRat = zsCalcFinalSizing.DesLatentHeatCoilInHumRat;
        zsCalcFinalSizing.ZoneRetTempAtHeatPeak = zsCalcFinalSizing.ZoneRetTempAtLatentHeatPeak;
        zsCalcFinalSizing.ZoneTempAtHeatPeak = zsCalcFinalSizing.ZoneTempAtLatentHeatPeak;
        zsCalcFinalSizing.ZoneHumRatAtHeatPeak = zsCalcFinalSizing.ZoneHumRatAtLatentHeatPeak;
        zsCalcFinalSizing.HeatPeakDateHrMin = zsCalcFinalSizing.LatHeatPeakDateHrMin;

        // will this cause sizing issues with heating coils since SA humrat is higher than zone humrat?
        // use zone humrat instead? this value would size humidifiers well, but what about heating coils?
        // not sure at this point if heating should reset HeatDesHumRat
        if (zsCalcFinalSizing.ZnLatHeatDgnSAMethod == SupplyAirHumidityRatio) {
            zsCalcFinalSizing.HeatDesHumRat = zsCalcFinalSizing.LatentHeatDesHumRat;
        } else {
            zsCalcFinalSizing.HeatDesHumRat = zsCalcFinalSizing.ZoneHumRatAtLatentHeatPeak + zsCalcFinalSizing.HeatDesHumRatDiff;
        }

        if (zsCalcFinalSizing.LatentHeatDDNum > 0) {
            auto &calcZoneSizing = zsCalcSizing(zsCalcFinalSizing.LatentHeatDDNum, zoneOrSpaceNum);
            calcZoneSizing.DesHeatVolFlow = calcZoneSizing.DesLatentHeatVolFlow;
            calcZoneSizing.DesHeatMassFlow = calcZoneSizing.DesLatentHeatMassFlow;
            calcZoneSizing.DesHeatLoad = calcZoneSizing.DesLatentHeatLoad;
            calcZoneSizing.HeatDesDay = calcZoneSizing.LatHeatDesDay;
            calcZoneSizing.cHeatDDDate = zsCalcFinalSizing.cLatentHeatDDDate; // this has correct HeatDDDate
            calcZoneSizing.HeatDDNum = calcZoneSizing.LatentHeatDDNum;
            calcZoneSizing.TimeStepNumAtHeatMax = calcZoneSizing.TimeStepNumAtLatentHeatMax;
            calcZoneSizing.HeatFlowSeq = calcZoneSizing.LatentHeatFlowSeq;
            calcZoneSizing.DesHeatCoilInTemp = calcZoneSizing.DesLatentHeatCoilInTemp;
            calcZoneSizing.DesHeatCoilInHumRat = calcZoneSizing.DesLatentHeatCoilInHumRat;
            calcZoneSizing.ZoneRetTempAtHeatPeak = calcZoneSizing.ZoneRetTempAtLatentHeatPeak;
            calcZoneSizing.ZoneTempAtHeatPeak = calcZoneSizing.ZoneTempAtLatentHeatPeak;
            calcZoneSizing.ZoneHumRatAtHeatPeak = calcZoneSizing.ZoneHumRatAtLatentHeatPeak;
            calcZoneSizing.HeatPeakDateHrMin = zsCalcFinalSizing.LatHeatPeakDateHrMin;

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
void updateZoneSizingEndZoneSizingCalc4(DataSizing::ZoneSizingData &zsSizing, DataSizing::ZoneSizingData const &zsCalcSizing)
{
    zsSizing.CoolDesDay = zsCalcSizing.CoolDesDay;
    zsSizing.HeatDesDay = zsCalcSizing.HeatDesDay;
    zsSizing.DesHeatDens = zsCalcSizing.DesHeatDens;
    zsSizing.DesCoolDens = zsCalcSizing.DesCoolDens;
    zsSizing.HeatDDNum = zsCalcSizing.HeatDDNum;
    zsSizing.CoolDDNum = zsCalcSizing.CoolDDNum;

    zsSizing.DesHeatLoad = zsCalcSizing.DesHeatLoad;
    zsSizing.DesHeatMassFlow = zsCalcSizing.DesHeatMassFlow;
    zsSizing.ZoneTempAtHeatPeak = zsCalcSizing.ZoneTempAtHeatPeak;
    zsSizing.OutTempAtHeatPeak = zsCalcSizing.OutTempAtHeatPeak;
    zsSizing.ZoneRetTempAtHeatPeak = zsCalcSizing.ZoneRetTempAtHeatPeak;
    zsSizing.ZoneHumRatAtHeatPeak = zsCalcSizing.ZoneHumRatAtHeatPeak;
    zsSizing.OutHumRatAtHeatPeak = zsCalcSizing.OutHumRatAtHeatPeak;
    zsSizing.TimeStepNumAtHeatMax = zsCalcSizing.TimeStepNumAtHeatMax;
    zsSizing.DesHeatVolFlow = zsCalcSizing.DesHeatVolFlow;
    zsSizing.DesHeatCoilInTemp = zsCalcSizing.DesHeatCoilInTemp;
    zsSizing.DesHeatCoilInHumRat = zsCalcSizing.DesHeatCoilInHumRat;
    zsSizing.CoolDesHumRat = zsCalcSizing.CoolDesHumRat;

    zsSizing.DesCoolLoad = zsCalcSizing.DesCoolLoad;
    zsSizing.DesCoolMassFlow = zsCalcSizing.DesCoolMassFlow;
    zsSizing.ZoneTempAtCoolPeak = zsCalcSizing.ZoneTempAtCoolPeak;
    zsSizing.OutTempAtCoolPeak = zsCalcSizing.OutTempAtCoolPeak;
    zsSizing.ZoneRetTempAtCoolPeak = zsCalcSizing.ZoneRetTempAtCoolPeak;
    zsSizing.ZoneHumRatAtCoolPeak = zsCalcSizing.ZoneHumRatAtCoolPeak;
    zsSizing.OutHumRatAtCoolPeak = zsCalcSizing.OutHumRatAtCoolPeak;
    zsSizing.TimeStepNumAtCoolMax = zsCalcSizing.TimeStepNumAtCoolMax;
    zsSizing.DesCoolVolFlow = zsCalcSizing.DesCoolVolFlow;
    zsSizing.DesCoolCoilInTemp = zsCalcSizing.DesCoolCoilInTemp;
    zsSizing.DesCoolCoilInHumRat = zsCalcSizing.DesCoolCoilInHumRat;
}

void updateZoneSizingEndZoneSizingCalc5(DataSizing::ZoneSizingData &zsFinalSizing, DataSizing::ZoneSizingData const &zsCalcFinalSizing)
{
    // SpaceSizing TODO: This is essentially the same as updateZoneSizingEndZoneSizingCalc4, except there are two extra fields copied here
    zsFinalSizing.CoolDesDay = zsCalcFinalSizing.CoolDesDay;
    zsFinalSizing.HeatDesDay = zsCalcFinalSizing.HeatDesDay;
    zsFinalSizing.DesHeatDens = zsCalcFinalSizing.DesHeatDens;
    zsFinalSizing.DesCoolDens = zsCalcFinalSizing.DesCoolDens;
    zsFinalSizing.HeatDDNum = zsCalcFinalSizing.HeatDDNum;
    zsFinalSizing.CoolDDNum = zsCalcFinalSizing.CoolDDNum;

    zsFinalSizing.DesHeatLoad = zsCalcFinalSizing.DesHeatLoad;
    zsFinalSizing.NonAirSysDesHeatLoad = zsCalcFinalSizing.DesHeatLoad;
    zsFinalSizing.DesHeatMassFlow = zsCalcFinalSizing.DesHeatMassFlow;
    zsFinalSizing.ZoneTempAtHeatPeak = zsCalcFinalSizing.ZoneTempAtHeatPeak;
    zsFinalSizing.OutTempAtHeatPeak = zsCalcFinalSizing.OutTempAtHeatPeak;
    zsFinalSizing.ZoneRetTempAtHeatPeak = zsCalcFinalSizing.ZoneRetTempAtHeatPeak;
    zsFinalSizing.ZoneHumRatAtHeatPeak = zsCalcFinalSizing.ZoneHumRatAtHeatPeak;
    zsFinalSizing.OutHumRatAtHeatPeak = zsCalcFinalSizing.OutHumRatAtHeatPeak;
    zsFinalSizing.TimeStepNumAtHeatMax = zsCalcFinalSizing.TimeStepNumAtHeatMax;
    zsFinalSizing.DesHeatVolFlow = zsCalcFinalSizing.DesHeatVolFlow;
    zsFinalSizing.NonAirSysDesHeatVolFlow = zsCalcFinalSizing.DesHeatVolFlow; // SpaceSizing TODO: Suspicious
    zsFinalSizing.DesHeatCoilInTemp = zsCalcFinalSizing.DesHeatCoilInTemp;
    zsFinalSizing.DesHeatCoilInHumRat = zsCalcFinalSizing.DesHeatCoilInHumRat;
    zsFinalSizing.CoolDesHumRat = zsCalcFinalSizing.CoolDesHumRat;

    zsFinalSizing.DesCoolLoad = zsCalcFinalSizing.DesCoolLoad;
    zsFinalSizing.NonAirSysDesCoolLoad = zsCalcFinalSizing.DesCoolLoad;
    zsFinalSizing.DesCoolMassFlow = zsCalcFinalSizing.DesCoolMassFlow;
    zsFinalSizing.ZoneTempAtCoolPeak = zsCalcFinalSizing.ZoneTempAtCoolPeak;
    zsFinalSizing.OutTempAtCoolPeak = zsCalcFinalSizing.OutTempAtCoolPeak;
    zsFinalSizing.ZoneRetTempAtCoolPeak = zsCalcFinalSizing.ZoneRetTempAtCoolPeak;
    zsFinalSizing.ZoneHumRatAtCoolPeak = zsCalcFinalSizing.ZoneHumRatAtCoolPeak;
    zsFinalSizing.OutHumRatAtCoolPeak = zsCalcFinalSizing.OutHumRatAtCoolPeak;
    zsFinalSizing.TimeStepNumAtCoolMax = zsCalcFinalSizing.TimeStepNumAtCoolMax;
    zsFinalSizing.DesCoolVolFlow = zsCalcFinalSizing.DesCoolVolFlow;
    zsFinalSizing.NonAirSysDesCoolVolFlow = zsCalcFinalSizing.DesCoolVolFlow; // SpaceSizing TODO: Suspicious
    zsFinalSizing.DesCoolCoilInTemp = zsCalcFinalSizing.DesCoolCoilInTemp;
    zsFinalSizing.DesCoolCoilInHumRat = zsCalcFinalSizing.DesCoolCoilInHumRat;
}

void updateZoneSizingEndZoneSizingCalc6(DataSizing::ZoneSizingData &zsSizing,
                                        DataSizing::ZoneSizingData const &zsCalcSizing,
                                        int const numTimeStepsInDay)
{
    // This is called for all zsSizing/zsCalcSizing (2D arrays) and FinalZoneSizing/CalcFinalZoneSizing (1D arrays) for both zones and spaces
    for (int TimeStepIndex = 1; TimeStepIndex <= numTimeStepsInDay; ++TimeStepIndex) {
        zsSizing.HeatFlowSeq(TimeStepIndex) = zsCalcSizing.HeatFlowSeq(TimeStepIndex);
        zsSizing.HeatLoadSeq(TimeStepIndex) = zsCalcSizing.HeatLoadSeq(TimeStepIndex);
        zsSizing.CoolFlowSeq(TimeStepIndex) = zsCalcSizing.CoolFlowSeq(TimeStepIndex);
        zsSizing.CoolLoadSeq(TimeStepIndex) = zsCalcSizing.CoolLoadSeq(TimeStepIndex);
        zsSizing.HeatZoneTempSeq(TimeStepIndex) = zsCalcSizing.HeatZoneTempSeq(TimeStepIndex);
        zsSizing.HeatOutTempSeq(TimeStepIndex) = zsCalcSizing.HeatOutTempSeq(TimeStepIndex);
        zsSizing.HeatZoneRetTempSeq(TimeStepIndex) = zsCalcSizing.HeatZoneRetTempSeq(TimeStepIndex);
        zsSizing.HeatZoneHumRatSeq(TimeStepIndex) = zsCalcSizing.HeatZoneHumRatSeq(TimeStepIndex);
        zsSizing.HeatOutHumRatSeq(TimeStepIndex) = zsCalcSizing.HeatOutHumRatSeq(TimeStepIndex);
        zsSizing.CoolZoneTempSeq(TimeStepIndex) = zsCalcSizing.CoolZoneTempSeq(TimeStepIndex);
        zsSizing.CoolOutTempSeq(TimeStepIndex) = zsCalcSizing.CoolOutTempSeq(TimeStepIndex);
        zsSizing.CoolZoneRetTempSeq(TimeStepIndex) = zsCalcSizing.CoolZoneRetTempSeq(TimeStepIndex);
        zsSizing.CoolZoneHumRatSeq(TimeStepIndex) = zsCalcSizing.CoolZoneHumRatSeq(TimeStepIndex);
        zsSizing.CoolOutHumRatSeq(TimeStepIndex) = zsCalcSizing.CoolOutHumRatSeq(TimeStepIndex);
    }
}

void updateZoneSizingEndZoneSizingCalc7(EnergyPlusData &state,
                                        DataSizing::ZoneSizingData &zsFinalSizing,
                                        DataSizing::ZoneSizingData &zsCalcFinalSizing,
                                        Array2D<DataSizing::ZoneSizingData> &zsSizing,
                                        Array2D<DataSizing::ZoneSizingData> &zsCalcSizing,
                                        int const zoneOrSpaceNum)
{
    static constexpr std::string_view RoutineName("updateZoneSizingEndZoneSizingCalc7");
    // update non air system design load and air flow to include the sizing factor
    zsFinalSizing.NonAirSysDesCoolLoad *= zsFinalSizing.CoolSizingFactor;
    zsFinalSizing.NonAirSysDesCoolVolFlow *= zsFinalSizing.CoolSizingFactor; // NonAirSysDesCoolVolFlow not currently used
    // Now take into account the user specified sizing factor and user specified cooling design air flow rate
    Real64 TotCoolSizMult = 0.0;
    // Calculate a sizing factor from the user specified cooling design air flow rate
    if (zsFinalSizing.InpDesCoolAirFlow > 0.0 && zsFinalSizing.CoolAirDesMethod == AirflowSizingMethod::InpDesAirFlow &&
        zsFinalSizing.DesCoolVolFlow > 0.0) {
        TotCoolSizMult = (zsFinalSizing.InpDesCoolAirFlow / zsFinalSizing.DesCoolVolFlow) * zsFinalSizing.CoolSizingFactor;
        // If no user specified cooling design air flow rate input, use the user specified szing factor
    } else {
        TotCoolSizMult = zsFinalSizing.CoolSizingFactor;
    }
    // If the cooling sizing multiplier is not 1, adjust the cooling design data
    if (std::abs(TotCoolSizMult - 1.0) > 0.00001) {
        if (zsFinalSizing.DesCoolVolFlow > 0.0) {
            int TimeStepAtPeak = zsFinalSizing.TimeStepNumAtCoolMax;
            int DDNum = zsFinalSizing.CoolDDNum;
            zsFinalSizing.DesCoolVolFlow = zsCalcFinalSizing.DesCoolVolFlow * TotCoolSizMult;
            zsFinalSizing.DesCoolMassFlow = zsCalcFinalSizing.DesCoolMassFlow * TotCoolSizMult;
            zsFinalSizing.DesCoolLoad = zsCalcFinalSizing.DesCoolLoad * TotCoolSizMult;
            for (int i = 0; i < (int)zsFinalSizing.CoolFlowSeq.size(); ++i) {
                zsFinalSizing.CoolFlowSeq[i] = zsCalcFinalSizing.CoolFlowSeq[i] * TotCoolSizMult;
                zsFinalSizing.CoolLoadSeq[i] = zsCalcFinalSizing.CoolLoadSeq[i] * TotCoolSizMult;
            }
            Real64 OAFrac = zsFinalSizing.MinOA / zsFinalSizing.DesCoolVolFlow;
            OAFrac = min(1.0, max(0.0, OAFrac));
            zsFinalSizing.DesCoolCoilInTemp =
                OAFrac * state.dataSize->DesDayWeath(DDNum).Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zsFinalSizing.ZoneTempAtCoolPeak;
            zsFinalSizing.DesCoolCoilInHumRat =
                OAFrac * state.dataSize->DesDayWeath(DDNum).HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zsFinalSizing.ZoneHumRatAtCoolPeak;
        } else {
            zsFinalSizing.DesCoolVolFlow = zsFinalSizing.InpDesCoolAirFlow;
            zsFinalSizing.DesCoolMassFlow = zsFinalSizing.DesCoolVolFlow * zsFinalSizing.DesCoolDens;
        }
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            auto &zoneSizing = zsSizing(DDNum, zoneOrSpaceNum);
            if (zoneSizing.DesCoolVolFlow > 0.0) {
                int TimeStepAtPeak = zoneSizing.TimeStepNumAtCoolMax;
                auto &calcZoneSizing = zsCalcSizing(DDNum, zoneOrSpaceNum);
                auto &desDayWeath = state.dataSize->DesDayWeath(DDNum);
                zoneSizing.DesCoolVolFlow = calcZoneSizing.DesCoolVolFlow * TotCoolSizMult;
                zoneSizing.DesCoolMassFlow = calcZoneSizing.DesCoolMassFlow * TotCoolSizMult;
                zoneSizing.DesCoolLoad = calcZoneSizing.DesCoolLoad * TotCoolSizMult;
                for (int i = 0; i < (int)zoneSizing.CoolFlowSeq.size(); ++i) {
                    zoneSizing.CoolFlowSeq[i] = calcZoneSizing.CoolFlowSeq[i] * TotCoolSizMult;
                    zoneSizing.CoolLoadSeq[i] = calcZoneSizing.CoolLoadSeq[i] * TotCoolSizMult;
                }
                Real64 OAFrac = zoneSizing.MinOA / zoneSizing.DesCoolVolFlow;
                OAFrac = min(1.0, max(0.0, OAFrac));
                zoneSizing.DesCoolCoilInTemp = OAFrac * desDayWeath.Temp(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizing.ZoneTempAtCoolPeak;
                zoneSizing.DesCoolCoilInHumRat = OAFrac * desDayWeath.HumRat(TimeStepAtPeak) + (1.0 - OAFrac) * zoneSizing.ZoneHumRatAtCoolPeak;
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
        if (zsFinalSizing.CoolAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim) {
            MaxOfMinCoolVolFlowNoOA = max(zsFinalSizing.DesCoolMinAirFlow, zsFinalSizing.DesCoolMinAirFlow2);
        }
        Real64 MaxOfMinCoolMassFlowNoOA =
            MaxOfMinCoolVolFlowNoOA * zsFinalSizing.DesCoolDens; // max of the user specified design cooling minimum flows without min OA flow [m3/s]
        zsFinalSizing.DesCoolVolFlowNoOA = zsFinalSizing.DesCoolVolFlow;
        zsFinalSizing.DesCoolMassFlowNoOA = zsFinalSizing.DesCoolMassFlow;
        if (MaxOfMinCoolVolFlowNoOA > zsFinalSizing.DesCoolVolFlowNoOA) {
            zsFinalSizing.DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
            zsFinalSizing.DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
        }
        for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
            zsFinalSizing.CoolFlowSeqNoOA(TimeStepIndex) = zsFinalSizing.CoolFlowSeq(TimeStepIndex);
            if (MaxOfMinCoolMassFlowNoOA > zsFinalSizing.CoolFlowSeqNoOA(TimeStepIndex)) {
                zsFinalSizing.CoolFlowSeqNoOA(TimeStepIndex) = MaxOfMinCoolMassFlowNoOA;
            }
        }
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            auto &zoneSizing = zsSizing(DDNum, zoneOrSpaceNum);
            zoneSizing.DesCoolVolFlowNoOA = zoneSizing.DesCoolVolFlow;
            zoneSizing.DesCoolMassFlowNoOA = zoneSizing.DesCoolMassFlow;
            MaxOfMinCoolVolFlowNoOA = max(zoneSizing.DesCoolMinAirFlow, zoneSizing.DesCoolMinAirFlow);
            MaxOfMinCoolMassFlowNoOA = MaxOfMinCoolVolFlowNoOA * zoneSizing.DesCoolDens;
            if (MaxOfMinCoolVolFlowNoOA > zoneSizing.DesCoolVolFlow) {
                zoneSizing.DesCoolVolFlowNoOA = MaxOfMinCoolVolFlowNoOA;
                zoneSizing.DesCoolMassFlowNoOA = MaxOfMinCoolMassFlowNoOA;
            }
            for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
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
        if (zsFinalSizing.CoolAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim) {
            MaxOfMinCoolVolFlow = max(zsFinalSizing.DesCoolMinAirFlow, zsFinalSizing.DesCoolMinAirFlow2, zsFinalSizing.MinOA);
        } else {
            MaxOfMinCoolVolFlow = zsFinalSizing.MinOA;
        }
        Real64 MaxOfMinCoolMassFlow =
            MaxOfMinCoolVolFlow * zsFinalSizing.DesCoolDens; // max of the user specified design cooling minimum flows and min OA flow [kg/s]
        if (MaxOfMinCoolVolFlow > zsFinalSizing.DesCoolVolFlow) {
            zsFinalSizing.DesCoolVolFlow = MaxOfMinCoolVolFlow;
            zsFinalSizing.DesCoolMassFlow = MaxOfMinCoolMassFlow;
        }
        for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
            if (MaxOfMinCoolMassFlow > zsFinalSizing.CoolFlowSeq(TimeStepIndex)) {
                zsFinalSizing.CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
            }
        }
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            auto &zoneSizing = zsSizing(DDNum, zoneOrSpaceNum);
            MaxOfMinCoolVolFlow = max(zoneSizing.DesCoolMinAirFlow, zoneSizing.DesCoolMinAirFlow, zoneSizing.MinOA);
            MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * zoneSizing.DesCoolDens;
            if (MaxOfMinCoolVolFlow > zoneSizing.DesCoolVolFlow) {
                zoneSizing.DesCoolVolFlow = MaxOfMinCoolVolFlow;
                zoneSizing.DesCoolMassFlow = MaxOfMinCoolMassFlow;
            }
            for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
                if (MaxOfMinCoolMassFlow > zoneSizing.CoolFlowSeq(TimeStepIndex)) {
                    zoneSizing.CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow;
                }
            }
        }
    }
    // IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
    if (zsFinalSizing.DesCoolLoad == 0) {
        // Check CoolDDNum and TimeStepNumAtCoolMax value and default to 1 if not set, carried over from previous code
        if (zsCalcFinalSizing.CoolDDNum == 0) {
            zsCalcFinalSizing.CoolDDNum = 1;
        }
        if (zsCalcFinalSizing.TimeStepNumAtCoolMax == 0) {
            zsCalcFinalSizing.TimeStepNumAtCoolMax = 1;
        }
        zsFinalSizing.TimeStepNumAtCoolMax = zsCalcFinalSizing.TimeStepNumAtCoolMax;
        zsFinalSizing.CoolDDNum = zsCalcFinalSizing.CoolDDNum;
        zsFinalSizing.CoolDesDay = zsCalcFinalSizing.CoolDesDay;
        int DDNumF = zsFinalSizing.CoolDDNum;
        auto &zoneSizingF = zsSizing(DDNumF, zoneOrSpaceNum);
        int TimeStepAtPeakF = zsFinalSizing.TimeStepNumAtCoolMax;

        // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
        // issue 6006, heating coils sizing to 0 when no heating load in zone
        if (zoneSizingF.DesCoolSetPtSeq.empty()) {
            ShowSevereError(
                state,
                format("{}:  Thermostat cooling set point temperatures are not initialized for Zone = {}", RoutineName, zsFinalSizing.ZoneName));
            ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
        } else {
            zsFinalSizing.ZoneTempAtCoolPeak = *std::min_element(zoneSizingF.DesCoolSetPtSeq.begin(), zoneSizingF.DesCoolSetPtSeq.end());
        }
        zsFinalSizing.OutTempAtCoolPeak = *std::min_element(zoneSizingF.CoolOutTempSeq.begin(), zoneSizingF.CoolOutTempSeq.end());
        zsFinalSizing.OutHumRatAtCoolPeak = zoneSizingF.CoolOutHumRatSeq(TimeStepAtPeakF);
        zsFinalSizing.ZoneHumRatAtCoolPeak = zoneSizingF.CoolDesHumRat;
        zsCalcFinalSizing.ZoneTempAtCoolPeak = zoneSizingF.CoolZoneTempSeq(TimeStepAtPeakF);
        zsCalcFinalSizing.ZoneHumRatAtCoolPeak = zoneSizingF.CoolZoneHumRatSeq(TimeStepAtPeakF);
        zsCalcFinalSizing.ZoneRetTempAtCoolPeak = zsCalcFinalSizing.ZoneTempAtCoolPeak;
        zsFinalSizing.DesCoolCoilInTemp = zsFinalSizing.ZoneTempAtCoolPeak;
        zsFinalSizing.DesCoolCoilInHumRat = zsFinalSizing.ZoneHumRatAtCoolPeak;
        zsFinalSizing.ZoneRetTempAtCoolPeak = zsFinalSizing.ZoneTempAtCoolPeak;
    }
    // update non air system design load and air flow to include the sizing factor
    zsFinalSizing.NonAirSysDesHeatLoad *= zsFinalSizing.HeatSizingFactor;
    zsFinalSizing.NonAirSysDesHeatVolFlow *= zsFinalSizing.HeatSizingFactor;
    // Now take into account the user specified sizing factor or user specified heating design air flow rate (which overrides the
    // sizing factor)
    Real64 TotHeatSizMult = 0.0;
    // Calculate a sizing factor from the user specified heating design air flow rate
    if (zsFinalSizing.InpDesHeatAirFlow > 0.0 && zsFinalSizing.HeatAirDesMethod == AirflowSizingMethod::InpDesAirFlow &&
        zsFinalSizing.DesHeatVolFlow > 0.0) {
        TotHeatSizMult = (zsFinalSizing.InpDesHeatAirFlow / zsFinalSizing.DesHeatVolFlow) * zsFinalSizing.HeatSizingFactor;
        // Calculate a sizing factor from the user specified max heating design air flow rates
    } else if (zsFinalSizing.HeatAirDesMethod == AirflowSizingMethod::DesAirFlowWithLim && zsFinalSizing.DesHeatVolFlow > 0.0) {
        Real64 MaxHeatVolFlow = max(
            zsFinalSizing.DesHeatMaxAirFlow, zsFinalSizing.DesHeatMaxAirFlow2, zsFinalSizing.DesCoolVolFlow * zsFinalSizing.DesHeatMaxAirFlowFrac);
        if (MaxHeatVolFlow < zsFinalSizing.DesHeatVolFlow) {
            TotHeatSizMult = (MaxHeatVolFlow / zsFinalSizing.DesHeatVolFlow) * zsFinalSizing.HeatSizingFactor;
        } else {
            TotHeatSizMult = zsFinalSizing.HeatSizingFactor;
        }
        // If no user specified heating design air flow rate input, use the user specified sizing factor
    } else {
        TotHeatSizMult = zsFinalSizing.HeatSizingFactor;
    }

    if (std::abs(TotHeatSizMult - 1.0) > 0.00001) {
        if (zsFinalSizing.DesHeatVolFlow > 0.0) {
            auto &desDayWeath = state.dataSize->DesDayWeath(zsFinalSizing.HeatDDNum);
            zsFinalSizing.DesHeatVolFlow = zsCalcFinalSizing.DesHeatVolFlow * TotHeatSizMult;
            zsFinalSizing.DesHeatMassFlow = zsCalcFinalSizing.DesHeatMassFlow * TotHeatSizMult;
            zsFinalSizing.DesHeatLoad = zsCalcFinalSizing.DesHeatLoad * TotHeatSizMult;
            for (int i = 0; i < (int)zsFinalSizing.HeatFlowSeq.size(); ++i) {
                zsFinalSizing.HeatFlowSeq[i] = zsCalcFinalSizing.HeatFlowSeq[i] * TotHeatSizMult;
                zsFinalSizing.HeatLoadSeq[i] = zsCalcFinalSizing.HeatLoadSeq[i] * TotHeatSizMult;
            }
            Real64 OAFrac = zsFinalSizing.MinOA / zsFinalSizing.DesHeatVolFlow;
            OAFrac = min(1.0, max(0.0, OAFrac));
            zsFinalSizing.DesHeatCoilInTemp =
                OAFrac * desDayWeath.Temp(zsFinalSizing.TimeStepNumAtHeatMax) + (1.0 - OAFrac) * zsFinalSizing.ZoneTempAtHeatPeak;
            zsFinalSizing.DesHeatCoilInHumRat =
                OAFrac * desDayWeath.HumRat(zsFinalSizing.TimeStepNumAtHeatMax) + (1.0 - OAFrac) * zsFinalSizing.ZoneHumRatAtHeatPeak;
        } else {
            zsFinalSizing.DesHeatVolFlow = zsFinalSizing.InpDesHeatAirFlow;
            zsFinalSizing.DesHeatMassFlow = zsFinalSizing.DesHeatVolFlow * zsFinalSizing.DesHeatDens;
        }
        for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
            auto &zoneSizingDD = zsSizing(DDNum, zoneOrSpaceNum);
            if (zoneSizingDD.DesHeatVolFlow > 0.0) {
                auto &calcZoneSizing = zsCalcSizing(DDNum, zoneOrSpaceNum);
                int TimeStepAtPeak = zoneSizingDD.TimeStepNumAtHeatMax;
                zoneSizingDD.DesHeatVolFlow = calcZoneSizing.DesHeatVolFlow * TotHeatSizMult;
                zoneSizingDD.DesHeatMassFlow = calcZoneSizing.DesHeatMassFlow * TotHeatSizMult;
                zoneSizingDD.DesHeatLoad = calcZoneSizing.DesHeatLoad * TotHeatSizMult;
                for (int i = 0; i < (int)zoneSizingDD.HeatFlowSeq.size(); ++i) {
                    zoneSizingDD.HeatFlowSeq[i] = calcZoneSizing.HeatFlowSeq[i] * TotHeatSizMult;
                    zoneSizingDD.HeatLoadSeq[i] = calcZoneSizing.HeatLoadSeq[i] * TotHeatSizMult;
                }
                Real64 OAFrac = zoneSizingDD.MinOA / zoneSizingDD.DesHeatVolFlow;
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
    // just in zsFinalSizing to use for TermUnit sizing adjustments in SizingManager::UpdateTermUnitFinalZoneSizing
    zsFinalSizing.DesHeatVolFlowNoOA = zsFinalSizing.DesHeatVolFlow;
    zsFinalSizing.DesHeatMassFlowNoOA = zsFinalSizing.DesHeatMassFlow;
    for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
        zsFinalSizing.HeatFlowSeqNoOA(TimeStepIndex) = zsFinalSizing.HeatFlowSeq(TimeStepIndex);
    }

    // Now make sure that the design heating air flow rates are greater than or equal to MinOA
    Real64 MinOAMass = zsFinalSizing.MinOA * zsFinalSizing.DesHeatDens;
    if (zsFinalSizing.MinOA > zsFinalSizing.DesHeatVolFlow) {
        zsFinalSizing.DesHeatVolFlow = zsFinalSizing.MinOA;
        zsFinalSizing.DesHeatMassFlow = MinOAMass;
    }
    for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
        if (MinOAMass > zsFinalSizing.HeatFlowSeq(TimeStepIndex)) {
            zsFinalSizing.HeatFlowSeq(TimeStepIndex) = MinOAMass;
        }
    }
    for (int DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {
        auto &zoneSizingDD = zsSizing(DDNum, zoneOrSpaceNum);
        MinOAMass = zoneSizingDD.MinOA * zoneSizingDD.DesHeatDens;
        if (zoneSizingDD.MinOA > zoneSizingDD.DesHeatVolFlow) {
            zoneSizingDD.DesHeatVolFlow = zoneSizingDD.MinOA;
            zoneSizingDD.DesHeatMassFlow = MinOAMass;
        }
        for (int TimeStepIndex = 1; TimeStepIndex <= state.dataZoneEquipmentManager->NumOfTimeStepInDay; ++TimeStepIndex) {
            if (MinOAMass > zoneSizingDD.HeatFlowSeq(TimeStepIndex)) {
                zoneSizingDD.HeatFlowSeq(TimeStepIndex) = MinOAMass;
            }
        }
    }
    // IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data
    if (zsFinalSizing.DesHeatLoad == 0) {
        // Check HDDNum and TimeStepNumAtHeatMax value and default to 1 if not set, carried over from previous code
        if (zsCalcFinalSizing.HeatDDNum == 0) {
            zsCalcFinalSizing.HeatDDNum = 1;
        }
        if (zsCalcFinalSizing.TimeStepNumAtHeatMax == 0) {
            zsCalcFinalSizing.TimeStepNumAtHeatMax = 1;
        }
        zsFinalSizing.TimeStepNumAtHeatMax = zsCalcFinalSizing.TimeStepNumAtHeatMax;
        zsFinalSizing.HeatDDNum = zsCalcFinalSizing.HeatDDNum;
        zsFinalSizing.HeatDesDay = zsCalcFinalSizing.HeatDesDay;
        int DDNumF = zsFinalSizing.HeatDDNum;
        auto &zoneSizingDDF = zsSizing(DDNumF, zoneOrSpaceNum);
        int TimeStepAtPeakF = zsFinalSizing.TimeStepNumAtHeatMax;

        // initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
        // issue 6006, heating coils sizing to 0 when no heating load in zone
        if (zoneSizingDDF.DesHeatSetPtSeq.empty()) {
            ShowSevereError(
                state, format("{}:  Thermostat heating set point temperatures not initialized for Zone = {}", RoutineName, zsFinalSizing.ZoneName));
            ShowFatalError(state, "Please send your input file to the EnergyPlus support/development team for further investigation.");
        } else {
            zsFinalSizing.ZoneTempAtHeatPeak = *std::max_element(zoneSizingDDF.DesHeatSetPtSeq.begin(), zoneSizingDDF.DesHeatSetPtSeq.end());
        }
        zsFinalSizing.OutTempAtHeatPeak = *std::min_element(zoneSizingDDF.HeatOutTempSeq.begin(), zoneSizingDDF.HeatOutTempSeq.end());
        zsFinalSizing.OutHumRatAtHeatPeak = zoneSizingDDF.HeatOutHumRatSeq(TimeStepAtPeakF);
        zsFinalSizing.ZoneHumRatAtHeatPeak = zoneSizingDDF.HeatDesHumRat;
        zsCalcFinalSizing.ZoneTempAtHeatPeak = zoneSizingDDF.HeatZoneTempSeq(TimeStepAtPeakF);
        zsCalcFinalSizing.ZoneHumRatAtHeatPeak = zoneSizingDDF.HeatZoneHumRatSeq(TimeStepAtPeakF);
        zsCalcFinalSizing.ZoneRetTempAtHeatPeak = zsCalcFinalSizing.ZoneTempAtHeatPeak;
        zsFinalSizing.DesHeatCoilInTemp = zsFinalSizing.ZoneTempAtHeatPeak;
        zsFinalSizing.DesHeatCoilInHumRat = zsFinalSizing.ZoneHumRatAtHeatPeak;
        zsFinalSizing.ZoneRetTempAtHeatPeak = zsFinalSizing.ZoneTempAtHeatPeak;
    }

    // set the zone minimum cooling supply air flow rate. This will be used for autosizing VAV terminal unit
    // minimum flow rates (comment seems incorrect, really used as a minimum lower limit for the maximum air flow)
    zsFinalSizing.DesCoolVolFlowMin =
        max(zsFinalSizing.DesCoolMinAirFlow, zsFinalSizing.DesCoolMinAirFlow2, zsFinalSizing.DesCoolVolFlow * zsFinalSizing.DesCoolMinAirFlowFrac);
    // set the zone maximum heating supply air flow rate. This will be used for autosizing VAV terminal unit
    // max heating flow rates
    zsFinalSizing.DesHeatVolFlowMax = max(zsFinalSizing.DesHeatMaxAirFlow,
                                          zsFinalSizing.DesHeatMaxAirFlow2,
                                          max(zsFinalSizing.DesCoolVolFlow, zsFinalSizing.DesHeatVolFlow) * zsFinalSizing.DesHeatMaxAirFlowFrac);
    // Determine the design cooling supply air temperature if the supply air temperature difference is specified by user.
    if (zsFinalSizing.ZnCoolDgnSAMethod == TemperatureDifference) {
        zsFinalSizing.CoolDesTemp = zsFinalSizing.ZoneTempAtCoolPeak - std::abs(zsFinalSizing.CoolDesTempDiff);
    }
    // Determine the design heating supply air temperature if the supply air temperature difference is specified by user.
    if (zsFinalSizing.ZnHeatDgnSAMethod == TemperatureDifference) {
        zsFinalSizing.HeatDesTemp = zsFinalSizing.ZoneTempAtHeatPeak + std::abs(zsFinalSizing.HeatDesTempDiff);
    }
}

void UpdateZoneSizing(EnergyPlusData &state, Constant::CallIndicator const CallIndicator)
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

    switch (CallIndicator) {
    case Constant::CallIndicator::BeginDay: {
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {

            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;

            // auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            updateZoneSizingBeginDay(state, state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum));
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingBeginDay(state, state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum));
                }
            }
        }
    } break;
    case Constant::CallIndicator::DuringDay: {
        int timeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
        Real64 fracTimeStepZone = state.dataHVACGlobal->FracTimeStepZone;

        // save the results of the ideal zone component calculation in the CalcZoneSizing sequence variables
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;

            // auto &zoneSizing = state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            // auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            // auto const &zoneThermostatHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum);
            // auto const &zoneThermostatLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum);
            updateZoneSizingDuringDay(state.dataSize->ZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum),
                                      state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum),
                                      state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum),
                                      state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum),
                                      state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSizThermSetPtHi,
                                      state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSizThermSetPtLo,
                                      timeStepInDay,
                                      fracTimeStepZone);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingDuringDay(state.dataSize->SpaceSizing(state.dataSize->CurOverallSimDay, spaceNum),
                                              state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum),
                                              state.dataHeatBalFanSys->ZoneThermostatSetPointHi(CtrlZoneNum),
                                              state.dataHeatBalFanSys->ZoneThermostatSetPointLo(CtrlZoneNum),
                                              state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSizThermSetPtHi,
                                              state.dataSize->FinalZoneSizing(CtrlZoneNum).ZoneSizThermSetPtLo,
                                              timeStepInDay,
                                              fracTimeStepZone);
                }
            }
        }
    } break;
    case Constant::CallIndicator::EndDay: {
        // average some of the zone sequences to reduce peakiness
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // auto &calcZoneSizing(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum));
            updateZoneSizingEndDayMovingAvg(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum),
                                            state.dataSize->NumTimeStepsInAvg);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingEndDayMovingAvg(state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum),
                                                    state.dataSize->NumTimeStepsInAvg);
                }
            }
        }

        // auto &desDayWeath = state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay);
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // auto &calcZoneSizing = state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum);
            // auto &calcFinalZoneSizing = state.dataSize->CalcFinalZoneSizing(CtrlZoneNum);
            updateZoneSizingEndDay(state.dataSize->CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum),
                                   state.dataSize->CalcFinalZoneSizing(CtrlZoneNum),
                                   state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                                   state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay),
                                   state.dataEnvrn->StdRhoAir);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingEndDay(state.dataSize->CalcSpaceSizing(state.dataSize->CurOverallSimDay, spaceNum),
                                           state.dataSize->CalcFinalSpaceSizing(spaceNum),
                                           state.dataZoneEquipmentManager->NumOfTimeStepInDay,
                                           state.dataSize->DesDayWeath(state.dataSize->CurOverallSimDay),
                                           state.dataEnvrn->StdRhoAir);
                }
            }
        }
    } break;
    case Constant::CallIndicator::EndZoneSizingCalc: {
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
                updateZoneSizingEndZoneSizingCalc1(state, state.dataSize->CalcFinalZoneSizing(CtrlZoneNum));
                if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                    for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                        updateZoneSizingEndZoneSizingCalc1(state, state.dataSize->CalcFinalSpaceSizing(spaceNum));
                    }
                }
            }

            // SpaceSizing TODO: For now only write zone-level zsz
            print(state.files.zsz, "Time");
            for (int I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
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
            }

            print(state.files.zsz, "\n");
            //      HourFrac = 0.0
            int Minutes = 0;
            int TimeStepIndex = 0;
            for (int HourCounter = 1; HourCounter <= 24; ++HourCounter) {
                for (int TimeStepCounter = 1; TimeStepCounter <= state.dataGlobal->NumOfTimeStepInHour; ++TimeStepCounter) {
                    ++TimeStepIndex;
                    Minutes += state.dataGlobal->MinutesPerTimeStep;
                    int HourPrint = HourCounter - 1;
                    if (Minutes == 60) {
                        Minutes = 0;
                        HourPrint = HourCounter;
                    }
                    for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                        updateZoneSizingEndZoneSizingCalc2(state.dataSize->CalcFinalZoneSizing(CtrlZoneNum), TimeStepIndex, HourPrint, Minutes);
                        if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                            for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                                updateZoneSizingEndZoneSizingCalc2(state.dataSize->CalcFinalSpaceSizing(spaceNum), TimeStepIndex, HourPrint, Minutes);
                            }
                        }
                    }

                    static constexpr std::string_view ZSizeFmt20("{:02}:{:02}:00");
                    print(state.files.zsz, ZSizeFmt20, HourPrint, Minutes);
                    for (int I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
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

            for (int I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
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
            for (int I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
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
                updateZoneSizingEndZoneSizingCalc3(
                    state.dataSize->CalcFinalZoneSizing(zoneNum), state.dataSize->CalcZoneSizing, state.dataHeatBal->isAnyLatentLoad, zoneNum);
                if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                    for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                        updateZoneSizingEndZoneSizingCalc3(state.dataSize->CalcFinalSpaceSizing(spaceNum),
                                                           state.dataSize->CalcSpaceSizing,
                                                           state.dataHeatBal->isAnyLatentLoad,
                                                           spaceNum);
                    }
                }
            }
        }

        // Move data from Calc arrays to user modified arrays

        for (std::size_t i = 0; i < state.dataSize->ZoneSizing.size(); ++i) {
            updateZoneSizingEndZoneSizingCalc4(state.dataSize->ZoneSizing[i], state.dataSize->CalcZoneSizing[i]);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (std::size_t i = 0; i < state.dataSize->SpaceSizing.size(); ++i) {
                    updateZoneSizingEndZoneSizingCalc4(state.dataSize->SpaceSizing[i], state.dataSize->CalcSpaceSizing[i]);
                }
            }
        }

        for (std::size_t i = 0; i < state.dataSize->FinalZoneSizing.size(); ++i) {
            updateZoneSizingEndZoneSizingCalc5(state.dataSize->FinalZoneSizing[i], state.dataSize->CalcFinalZoneSizing[i]);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (std::size_t i = 0; i < state.dataSize->FinalSpaceSizing.size(); ++i) {
                    updateZoneSizingEndZoneSizingCalc5(state.dataSize->FinalSpaceSizing[i], state.dataSize->CalcFinalSpaceSizing[i]);
                }
            }
        }

        for (int DesDayNum = 1; DesDayNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayNum) {
            for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                updateZoneSizingEndZoneSizingCalc6(state.dataSize->ZoneSizing(DesDayNum, CtrlZoneNum),
                                                   state.dataSize->CalcZoneSizing(DesDayNum, CtrlZoneNum),
                                                   state.dataZoneEquipmentManager->NumOfTimeStepInDay);
                if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                    for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                        updateZoneSizingEndZoneSizingCalc6(state.dataSize->SpaceSizing(DesDayNum, spaceNum),
                                                           state.dataSize->CalcSpaceSizing(DesDayNum, spaceNum),
                                                           state.dataZoneEquipmentManager->NumOfTimeStepInDay);
                    }
                }
            }
        }

        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // Yes, call updateZoneSizingEndZoneSizingCalc6 again here to copyd the same fields
            updateZoneSizingEndZoneSizingCalc6(state.dataSize->FinalZoneSizing(CtrlZoneNum),
                                               state.dataSize->CalcFinalZoneSizing(CtrlZoneNum),
                                               state.dataZoneEquipmentManager->NumOfTimeStepInDay);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingEndZoneSizingCalc6(state.dataSize->FinalSpaceSizing(spaceNum),
                                                       state.dataSize->CalcFinalSpaceSizing(spaceNum),
                                                       state.dataZoneEquipmentManager->NumOfTimeStepInDay);
                }
            }
        }
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            updateZoneSizingEndZoneSizingCalc7(state,
                                               state.dataSize->FinalZoneSizing(CtrlZoneNum),
                                               state.dataSize->CalcFinalZoneSizing(CtrlZoneNum),
                                               state.dataSize->ZoneSizing,
                                               state.dataSize->CalcZoneSizing,
                                               CtrlZoneNum);
            if (state.dataHeatBal->doSpaceHeatBalanceSizing) {
                for (int spaceNum : state.dataHeatBal->Zone(CtrlZoneNum).spaceIndexes) {
                    updateZoneSizingEndZoneSizingCalc7(state,
                                                       state.dataSize->FinalSpaceSizing(spaceNum),
                                                       state.dataSize->CalcFinalSpaceSizing(spaceNum),
                                                       state.dataSize->SpaceSizing,
                                                       state.dataSize->CalcSpaceSizing,
                                                       spaceNum);
                }
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

    bool SupPathInletChanged = false;
    Real64 SysOutputProvided = 0.0; // sensible output delivered by zone equipment (W)
    Real64 LatOutputProvided = 0.0; // latent output delivered by zone equipment (kg/s)
    Real64 AirSysOutput = 0.0;
    Real64 NonAirSysOutput = 0.0;

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

    // If SpaceHVAC is active calculate SpaceHVAC:EquipmentMixer outlet conditions before simulating zone equipment
    if (state.dataHeatBal->doSpaceHeatBalanceSimulation && !state.dataGlobal->DoingSizing) {
        for (auto &thisSpaceHVACMixer : state.dataZoneEquip->zoneEquipMixer) {
            thisSpaceHVACMixer.setOutletConditions(state);
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
        if (state.dataHeatBal->doSpaceHeatBalanceSimulation && !state.dataGlobal->DoingSizing) {
            for (int spaceNum : state.dataHeatBal->Zone(ControlledZoneNum).spaceIndexes) {
                auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
                thisSpaceHB.NonAirSystemResponse = 0.0;
                thisSpaceHB.SysDepZoneLoads = 0.0;
                auto &thisSpaceEquipConfig = state.dataZoneEquip->spaceEquipConfig(spaceNum);
                if (!thisSpaceEquipConfig.IsControlled) continue;
                thisSpaceEquipConfig.ZoneExh = 0.0;
                thisSpaceEquipConfig.ZoneExhBalanced = 0.0;
                thisSpaceEquipConfig.PlenumMassFlow = 0.0;
            }
        }

        InitSystemOutputRequired(state, ControlledZoneNum, FirstHVACIteration, true);

        for (int EquipTypeNum = 1; EquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).NumOfEquipTypes; ++EquipTypeNum) {

            // Air loop system availability manager status only applies to PIU and exhaust fans
            // Reset fan SAM operation flags for zone fans.
            state.dataHVACGlobal->TurnFansOn = false;
            state.dataHVACGlobal->TurnFansOff = false;

            state.dataHVACGlobal->UnbalExhMassFlow = 0.0;
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
            state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
            const int EquipPtr = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipPtr;
            SysOutputProvided = 0.0;
            LatOutputProvided = 0.0;
            NonAirSysOutput = 0.0;
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

            DataZoneEquipment::ZoneEquipType zoneEquipType = state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).equipType;

            auto &zoneEquipList = state.dataZoneEquip->ZoneEquipList(state.dataSize->CurZoneEqNum);

            const int ZoneCompNum = zoneEquipList.EquipIndex(EquipPtr);

            bool ValidSAMComp = false;

            if ((int)zoneEquipType <= NumValidSysAvailZoneComponents) ValidSAMComp = true;

            auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

            if (ZoneCompNum > 0 && ValidSAMComp) {

                SystemAvailabilityManager::GetZoneEqAvailabilityManager(state, zoneEquipType, ZoneCompNum, ErrorFlag);

                if (ZoneComp((int)zoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == DataHVACGlobals::CycleOn) {
                    state.dataHVACGlobal->TurnFansOn = true;
                    state.dataHVACGlobal->TurnFansOff = false;
                } else if (ZoneComp((int)zoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == DataHVACGlobals::ForceOff) {
                    state.dataHVACGlobal->TurnFansOn = false;
                    state.dataHVACGlobal->TurnFansOff = true;
                }
            }

            // If SpaceHVAC is active and this equipment has a space splitter, scale the zone load if needed
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation && !state.dataGlobal->DoingSizing &&
                zoneEquipList.zoneEquipSplitterIndex(EquipPtr) > -1) {
                state.dataZoneEquip->zoneEquipSplitter[zoneEquipList.zoneEquipSplitterIndex(EquipPtr)].adjustLoads(
                    state, ControlledZoneNum, EquipTypeNum);
            }

            switch (zoneEquipType) {
            case ZoneEquipType::AirDistributionUnit: { // 'ZoneHVAC:AirDistributionUnit'
                // Air loop system availability manager status only applies to PIU and exhaust fans
                // Check to see if System Availability Managers are asking for fans to cycle on or shut off
                // and set fan on/off flags accordingly.
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) {
                    state.dataHVACGlobal->TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::ForceOff) {
                    state.dataHVACGlobal->TurnFansOff = true;
                }

                ZoneAirLoopEquipmentManager::ManageZoneAirLoopEquipment(state,
                                                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                        FirstHVACIteration,
                                                                        AirSysOutput,
                                                                        NonAirSysOutput,
                                                                        LatOutputProvided,
                                                                        ControlledZoneNum,
                                                                        zoneEquipList.EquipIndex(EquipPtr));

                SysOutputProvided = NonAirSysOutput + AirSysOutput;
            } break;

            case ZoneEquipType::VariableRefrigerantFlowTerminal: { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
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

            case ZoneEquipType::WindowAirConditioner: { // 'ZoneHVAC:WindowAirConditioner'
                WindowAC::SimWindowAC(state,
                                      state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                      ControlledZoneNum,
                                      FirstHVACIteration,
                                      SysOutputProvided,
                                      LatOutputProvided,
                                      zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::PackagedTerminalHeatPump:           // 'ZoneHVAC:PackagedTerminalHeatPump'
            case ZoneEquipType::PackagedTerminalAirConditioner:     // 'ZoneHVAC:PackagedTerminalAirConditioner'
            case ZoneEquipType::PackagedTerminalHeatPumpWaterToAir: // 'ZoneHVAC:WaterToAirHeatPump'
            case ZoneEquipType::UnitarySystem: {                    // 'AirloopHVAC:UnitarySystem'
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
            case ZoneEquipType::DehumidifierDX: { // 'ZoneHVAC:Dehumidifier:DX'
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

            case ZoneEquipType::FourPipeFanCoil: { // 'ZoneHVAC:FourPipeFanCoil'
                FanCoilUnits::SimFanCoilUnit(state,
                                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                             ControlledZoneNum,
                                             FirstHVACIteration,
                                             SysOutputProvided,
                                             LatOutputProvided,
                                             zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::UnitVentilator: { // 'ZoneHVAC:UnitVentilator'
                UnitVentilator::SimUnitVentilator(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::UnitHeater: { // 'ZoneHVAC:UnitHeater'
                UnitHeater::SimUnitHeater(state,
                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                          ControlledZoneNum,
                                          FirstHVACIteration,
                                          SysOutputProvided,
                                          LatOutputProvided,
                                          zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::PurchasedAir: { // 'ZoneHVAC:IdealLoadsAirSystem'
                PurchasedAirManager::SimPurchasedAir(state,
                                                     state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                     SysOutputProvided,
                                                     LatOutputProvided,
                                                     FirstHVACIteration,
                                                     ControlledZoneNum,
                                                     zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::BaseboardWater: { // 'ZoneHVAC:Baseboard:RadiantConvective:Water'
                HWBaseboardRadiator::SimHWBaseboard(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::BaseboardSteam: { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
                SteamBaseboardRadiator::SimSteamBaseboard(state,
                                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                          ControlledZoneNum,
                                                          FirstHVACIteration,
                                                          SysOutputProvided,
                                                          zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::BaseboardConvectiveWater: { // 'ZoneHVAC:Baseboard:Convective:Water'
                BaseboardRadiator::SimBaseboard(state,
                                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                ControlledZoneNum,
                                                FirstHVACIteration,
                                                SysOutputProvided,
                                                zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::BaseboardConvectiveElectric: { // 'ZoneHVAC:Baseboard:Convective:Electric'
                BaseboardElectric::SimElectricBaseboard(state,
                                                        state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                        ControlledZoneNum,
                                                        SysOutputProvided,
                                                        zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::CoolingPanel: { // 'ZoneHVAC:CoolingPanel:RadiantConvective:Water'
                CoolingPanelSimple::SimCoolingPanel(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This cooling panel does not add/remove any latent heat
            } break;

            case ZoneEquipType::HighTemperatureRadiant: { // 'ZoneHVAC:HighTemperatureRadiant'
                HighTempRadiantSystem::SimHighTempRadiantSystem(state,
                                                                state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                FirstHVACIteration,
                                                                SysOutputProvided,
                                                                zoneEquipList.EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard currently sends its latent heat gain directly to predictor/corrector
                                         // via SumLatentHTRadSys... so setting LatOutputProvided = 0.0
            } break;

            case ZoneEquipType::LowTemperatureRadiant: { // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow',
                                                         // 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                // 'ZoneHVAC:LowTemperatureRadiant:Electric'
                LowTempRadiantSystem::SimLowTempRadiantSystem(state,
                                                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                              FirstHVACIteration,
                                                              SysOutputProvided,
                                                              zoneEquipList.EquipIndex(EquipPtr));
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::ExhaustFan: { // 'Fan:ZoneExhaust'
                // Air loop system availability manager status only applies to PIU and exhaust fans
                // Check to see if System Availability Managers are asking for fans to cycle on or shut off
                // and set fan on/off flags accordingly.
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOn ||
                    state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::CycleOnZoneFansOnly) {
                    state.dataHVACGlobal->TurnFansOn = true;
                }
                if (state.dataZoneEquip->ZoneEquipAvail(ControlledZoneNum) == DataHVACGlobals::ForceOff) {
                    state.dataHVACGlobal->TurnFansOff = true;
                }

                Fans::SimulateFanComponents(state,
                                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                            FirstHVACIteration,
                                            zoneEquipList.EquipIndex(EquipPtr));

            } break;

            case ZoneEquipType::HeatExchanger: { // 'HeatExchanger:AirToAir:FlatPlate'
                HeatRecovery::SimHeatRecovery(state,
                                              state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                              FirstHVACIteration,
                                              zoneEquipList.EquipIndex(EquipPtr),
                                              DataHVACGlobals::ContFanCycCoil);
            } break;

            case ZoneEquipType::EnergyRecoveryVentilator: { // 'ZoneHVAC:EnergyRecoveryVentilator'
                HVACStandAloneERV::SimStandAloneERV(state,
                                                    state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                    ControlledZoneNum,
                                                    FirstHVACIteration,
                                                    SysOutputProvided,
                                                    LatOutputProvided,
                                                    zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::HeatPumpWaterHeater: { // 'WaterHeater:HeatPump:PumpedCondenser'
                WaterThermalTanks::SimHeatPumpWaterHeater(state,
                                                          state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                          FirstHVACIteration,
                                                          SysOutputProvided,
                                                          LatOutputProvided,
                                                          state.dataZoneEquip->ZoneEquipList(ControlledZoneNum).EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::VentilatedSlab: { // 'ZoneHVAC:VentilatedSlab'
                VentilatedSlab::SimVentilatedSlab(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::OutdoorAirUnit: { // 'ZoneHVAC:OutdoorAirUnit'
                OutdoorAirUnit::SimOutdoorAirUnit(state,
                                                  state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                  ControlledZoneNum,
                                                  FirstHVACIteration,
                                                  SysOutputProvided,
                                                  LatOutputProvided,
                                                  zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::BaseboardElectric: { // 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
                ElectricBaseboardRadiator::SimElecBaseboard(state,
                                                            state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                            ControlledZoneNum,
                                                            FirstHVACIteration,
                                                            SysOutputProvided,
                                                            zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
                LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat
            } break;

            case ZoneEquipType::RefrigerationChillerSet: { // 'ZoneHVAC:RefrigerationChillerSet'
                RefrigeratedCase::SimAirChillerSet(state,
                                                   state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                   ControlledZoneNum,
                                                   FirstHVACIteration,
                                                   SysOutputProvided,
                                                   LatOutputProvided,
                                                   zoneEquipList.EquipIndex(EquipPtr));

                NonAirSysOutput = SysOutputProvided;
            } break;

            case ZoneEquipType::UserDefinedHVACForcedAir: {
                UserDefinedComponents::SimZoneAirUserDefined(state,
                                                             state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                             ControlledZoneNum,
                                                             SysOutputProvided,
                                                             LatOutputProvided,
                                                             zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::EvaporativeCooler: {
                EvaporativeCoolers::SimZoneEvaporativeCoolerUnit(state,
                                                                 state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum).EquipName,
                                                                 ControlledZoneNum,
                                                                 SysOutputProvided,
                                                                 LatOutputProvided,
                                                                 zoneEquipList.EquipIndex(EquipPtr));
            } break;

            case ZoneEquipType::HybridEvaporativeCooler: {
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

            // If SpaceHVAC is active and this equipment has a space splitter, distribute the equipment output and update the spaces
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation && !state.dataGlobal->DoingSizing &&
                zoneEquipList.zoneEquipSplitterIndex(EquipPtr) > -1) {
                state.dataZoneEquip->zoneEquipSplitter[zoneEquipList.zoneEquipSplitterIndex(EquipPtr)].distributeOutput(
                    state, ControlledZoneNum, SysOutputProvided, LatOutputProvided, NonAirSysOutput, EquipTypeNum);
            } else {
                thisZoneHB.NonAirSystemResponse += NonAirSysOutput;
            }
            // Space HVAC TODO: For now, update both spaces and zone, but maybe ultimately update one or the other
            updateSystemOutputRequired(state,
                                       ControlledZoneNum,
                                       SysOutputProvided,
                                       LatOutputProvided,
                                       state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ControlledZoneNum),
                                       state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ControlledZoneNum),
                                       EquipTypeNum);

            state.dataSize->CurTermUnitSizingNum = 0;
        } // zone equipment loop
    }     // End of controlled zone loop

    // If SpaceHVAC is active calculate SpaceHVAC:EquipmentMixer inlet flow rates after simulating zone equipment
    if (state.dataHeatBal->doSpaceHeatBalanceSimulation && !state.dataGlobal->DoingSizing) {
        for (auto &thisSpaceHVACMixer : state.dataZoneEquip->zoneEquipMixer) {
            thisSpaceHVACMixer.setInletFlows(state);
        }
    }

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
        pso.EquipTypeName = zeq.EquipTypeName(EquipTypeNum);
        pso.EquipName = zeq.EquipName(EquipTypeNum);
        pso.equipType = zeq.EquipType(EquipTypeNum);
        pso.CoolingPriority = zeq.CoolingPriority(EquipTypeNum);
        pso.HeatingPriority = zeq.HeatingPriority(EquipTypeNum);
        pso.EquipPtr = EquipTypeNum;
    }
    for (int EquipTypeNum = NumOfEquipTypes + 1, EquipTypeNum_end = state.dataZoneEquipmentManager->PrioritySimOrder.u();
         EquipTypeNum <= EquipTypeNum_end;
         ++EquipTypeNum) { // Reset unused upper array portion
        auto &pso(state.dataZoneEquipmentManager->PrioritySimOrder(EquipTypeNum));
        pso.EquipTypeName.clear();
        pso.EquipName.clear();
        pso.equipType = DataZoneEquipment::ZoneEquipType::Invalid;
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
                pso.EquipTypeName.swap(psc.EquipTypeName);
                pso.EquipName.swap(psc.EquipName);
                std::swap(pso.EquipPtr, psc.EquipPtr);
                std::swap(pso.equipType, psc.equipType);
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

void updateSystemOutputRequired(EnergyPlusData &state,
                                int const ZoneNum,
                                Real64 const SysOutputProvided, // sensible output provided by zone equipment (W)
                                Real64 const LatOutputProvided, // latent output provided by zone equipment (kg/s)
                                DataZoneEnergyDemands::ZoneSystemSensibleDemand &energy,
                                DataZoneEnergyDemands::ZoneSystemMoistureDemand &moisture,
                                int const EquipPriorityNum // optional index in PrioritySimOrder for this update
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       MODIFIED       B. Griffith Sept 2011, add storage of requirements by sequence

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

        if (EquipPriorityNum > -1) {
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

        if ((EquipPriorityNum > -1) && (EquipPriorityNum < thisZEqList.NumOfEquipTypes)) {

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
        if (EquipPriorityNum > -1) {
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

void adjustSystemOutputRequired(Real64 const sensibleRatio, // sensible load adjustment
                                Real64 const latentRatio,   // latent load adjustment
                                DataZoneEnergyDemands::ZoneSystemSensibleDemand &energy,
                                DataZoneEnergyDemands::ZoneSystemMoistureDemand &moisture,
                                int const equipPriorityNum // index in PrioritySimOrder
)
{
    // Adjust the zone energy demands for space thermostat control
    energy.RemainingOutputRequired *= sensibleRatio;
    energy.RemainingOutputReqToHeatSP *= sensibleRatio;
    energy.RemainingOutputReqToCoolSP *= sensibleRatio;
    moisture.RemainingOutputRequired *= latentRatio;
    moisture.RemainingOutputReqToHumidSP *= latentRatio;
    moisture.RemainingOutputReqToDehumidSP *= latentRatio;

    energy.SequencedOutputRequired(equipPriorityNum) *= sensibleRatio;
    energy.SequencedOutputRequiredToHeatingSP(equipPriorityNum) *= sensibleRatio;
    energy.SequencedOutputRequiredToCoolingSP(equipPriorityNum) *= sensibleRatio;
    moisture.SequencedOutputRequired(equipPriorityNum) *= latentRatio;
    moisture.SequencedOutputRequiredToHumidSP(equipPriorityNum) *= latentRatio;
    moisture.SequencedOutputRequiredToDehumidSP(equipPriorityNum) *= latentRatio;
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
                if (state.dataHeatBal->doSpaceHeatBalance) {
                    for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                        if (!state.dataZoneEquip->spaceEquipConfig(spaceNum).IsControlled) continue;
                        state.dataZoneEquip->spaceEquipConfig(spaceNum).ExcessZoneExh = 0.0;
                    }
                }
            }
        }
        BuildingZoneMixingFlowOld = BuildingZoneMixingFlow;
        BuildingZoneMixingFlow = 0.0;

        BuildingZoneReturnFlowOld = BuildingZoneReturnFlow;
        BuildingZoneReturnFlow = 0.0;

        for (int ZoneNum1 = 1; ZoneNum1 <= state.dataGlobal->NumOfZones; ++ZoneNum1) {
            int ZoneNum = ZoneNum1;
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) ZoneNum = state.dataHeatBalFanSys->ZoneReOrder(ZoneNum1);
            if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;
            auto &massConservation = state.dataHeatBal->MassConservation(ZoneNum);
            auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ZoneNum);
            Real64 TotExhaustAirMassFlowRate = 0.0;

            zoneEquipConfig.TotExhaustAirMassFlowRate = 0.0;

            Real64 ZoneMixingAirMassFlowRate = 0.0;
            Real64 ZoneMixingNetAirMassFlowRate = 0.0;
            Real64 ZoneReturnAirMassFlowRate = 0.0;

            // SpaceHVAC TODO: For now, ZoneMassBalance happens at the zone level, not space
            zoneEquipConfig.setTotalInletFlows(state);
            Real64 TotInletAirMassFlowRate = zoneEquipConfig.TotInletAirMassFlowRate;
            if (state.dataHeatBal->doSpaceHeatBalance) {
                for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                    auto &spaceEquipConfig = state.dataZoneEquip->spaceEquipConfig(spaceNum);
                    if (spaceEquipConfig.IsControlled) {
                        spaceEquipConfig.setTotalInletFlows(state);
                    } else {
                        // If space is not controlled, allocate zone-level airflow by volume
                        auto &thisSpace = state.dataHeatBal->space(spaceNum);
                        Real64 spaceFrac = thisSpace.fracZoneVolume;
                        DataZoneEquipment::scaleInletFlows(state, zoneEquipConfig.ZoneNode, thisSpace.SystemZoneNodeNumber, spaceFrac);
                    }
                }
            }

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
            if (allocated(state.dataRoomAir->AirPatternZoneInfo)) {
                if ((state.dataRoomAir->AirPatternZoneInfo(ZoneNum).IsUsed) && (!state.dataGlobal->BeginEnvrnFlag)) {
                    TempZoneAir = state.dataRoomAir->AirPatternZoneInfo(ZoneNum).Tleaving;
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
                    thisZoneHB.latentGain += SumRetAirLatentGainRate;
                }
            } else {
                state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToZone += state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToHVAC;
                // shouldn't the HVAC term be zeroed out then?
                SumRetAirLatentGainRate = InternalHeatGains::SumAllReturnAirLatentGains(state, ZoneNum, ReturnNode);
                thisZoneHB.latentGain += SumRetAirLatentGainRate;
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
        thisZoneHB.MixingHumRat = thisZoneHB.airHumRat;
        // This is only temporary fix for CR8867.  (L. Gu 8/12)
        if (SysTimestepLoop == 1) {
            thisZoneHB.MixingMAT = thisZoneHB.XMPT;
            thisZoneHB.MixingHumRat = thisZoneHB.WTimeMinusP;
        }
    }
    if (state.dataHeatBal->doSpaceHeatBalance) {
        for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
            thisSpaceHB.MixingMAT = thisSpaceHB.MAT;
            thisSpaceHB.MixingHumRat = thisSpaceHB.airHumRat;
            // This is only temporary fix for CR8867.  (L. Gu 8/12)
            if (SysTimestepLoop == 1) {
                thisSpaceHB.MixingMAT = thisSpaceHB.XMPT;
                thisSpaceHB.MixingHumRat = thisSpaceHB.WTimeMinusP;
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
                    ShowWarningError(state,
                                     format("Ventilation indoor temperature control: The minimum indoor temperature is above the maximum indoor "
                                            "temperature in {}",
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
                    ShowWarningError(state,
                                     format("Ventilation outdoor temperature control: The minimum outdoor temperature is above the maximum "
                                            "outdoor temperature in {}",
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
                            state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec += thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSysSec;
                    } else if (!state.afn->AirflowNetworkZoneFlag(zoneNum)) {
                        state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec += thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSysSec;
                    }
                } else {
                    state.dataHeatBal->ZnAirRpt(zoneNum).VentilFanElec += thisVentilation.FanPower * state.dataHVACGlobal->TimeStepSysSec;
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
            if (thisVentilation.OpenEff != Constant::AutoCalculate) {
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
                // * 0 means that it's blowing directly towards the opening (what ASHRAE HoF calls "Perpendicular winds"), so maximum
                // effectiveness
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
            if (thisVentilation.DiscCoef != Constant::AutoCalculate) {
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
                        ShowWarningError(state,
                                         format("Mixing outdoor temperature control: The minimum outdoor temperature is above the maximum "
                                                "outdoor temperature in {}",
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
                if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipType(I) == DataZoneEquipment::ZoneEquipType::EnergyRecoveryVentilator) {
                    ++thisZoneAirBalance.NumOfERVs;
                }
            }
            if (thisZoneAirBalance.NumOfERVs > 0) {
                thisZoneAirBalance.ERVInletNode.allocate(thisZoneAirBalance.NumOfERVs);
                thisZoneAirBalance.ERVExhaustNode.allocate(thisZoneAirBalance.NumOfERVs);
                int j = 1;
                for (int I = 1; I <= state.dataZoneEquip->ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
                    if (state.dataZoneEquip->ZoneEquipList(ZoneNum).EquipType(I) == DataZoneEquipment::ZoneEquipType::EnergyRecoveryVentilator) {
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
