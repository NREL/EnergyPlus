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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SZVAVModel.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WaterToAirHeatPump.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus::PackagedTerminalHeatPump {

// Module containing the routines for modeling packaged terminal air conditioners and heat pumps

// MODULE INFORMATION:
//       AUTHOR         Richard Raustad
//       DATE WRITTEN   July 2005
//       MODIFIED       B. Griffith Dec. 2006 added Function call for OA node and moved get input flag up to Module
//                      B. Griffith, Sept 2010, plant upgrades, fluid properties, for heating coils
//                      B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
//                      Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms needed to simulate packaged
// terminal units, which are considered "Zone Equipment" in EnergyPlus

// METHODOLOGY EMPLOYED:
// Units are modeled as a collection of components: outside air mixer, supply air fan, DX cooling coils,
// DX heating coil (PTHP) or gas/elec/water/steam heating coil (PTAC), and supplemental heater if necessary.
// Control is by means of cycling: either continuous air flow with the DX compressor
// cycling on/off or the entire unit - fan and compressor cycling on/off. Cycling behavior
// is not explicitly modeled - instead cycling inefficiencies must be included in
// the efficiency curves of the DX coil module.

// Using/Aliasing
using namespace DataLoopNode;
using namespace DataSizing;
using namespace DataHVACGlobals;

// Use statements for access to subroutines in other modules
using namespace ScheduleManager;

constexpr const char *fluidNameSteam("STEAM");

void SimPackagedTerminalUnit(EnergyPlusData &state,
                             std::string_view CompName,   // name of the packaged terminal heat pump
                             int const ZoneNum,             // number of zone being served
                             bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                             Real64 &QUnitOut,              // sensible capacity delivered to zone
                             Real64 &LatOutputProvided,     // Latent add/removal by packaged terminal unit (kg/s), dehumid = negative
                             int const PTUnitType,          // indicates whether PTAC, PTHP or PTWSHP
                             int &CompIndex                 // index to Packaged Terminal Heat Pump
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       D. Shirey, Aug 2009 (LatOutputProvided)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Manages the simulation of a packaged terminal heat pump. Called from SimZoneEquipment.

    // Using/Aliasing

    using namespace DataZoneEnergyDemands;
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum(0); // index of packaged terminal heat pump being simulated

    Real64 OnOffAirFlowRatio;          // ratio of compressor ON airflow to average airflow over timestep
    Real64 QZnReq;                     // load to be met by zone equipment
    Real64 RemainingOutputToHeatingSP; // remaining load to heating setpoint
    Real64 RemainingOutputToCoolingSP; // remaining load to cooling setpoint

    // First time SimPackagedTerminalHeatPump is called, get the input for all the PTUnits
    if (state.dataPTHP->GetPTUnitInputFlag) {
        GetPTUnit(state);
        state.dataPTHP->GetPTUnitInputFlag = false;
    }

    // Find the correct packaged terminal heat pump
    if (CompIndex == 0) {
        PTUnitNum = UtilityRoutines::FindItemInList(CompName, state.dataPTHP->PTUnit);
        if (PTUnitNum == 0) {
            ShowFatalError(state, "SimPackagedTerminalUnit: Unit not found=" + std::string{CompName});
        }
        CompIndex = state.dataPTHP->PTUnit(PTUnitNum).PTObjectIndex;
    } else {
        {
            auto const SELECT_CASE_var(PTUnitType);
            if (SELECT_CASE_var == PkgTermHPAirToAir_Num) {
                PTUnitNum = CompIndex;
            } else if (SELECT_CASE_var == PkgTermACAirToAir_Num) {
                PTUnitNum = CompIndex + state.dataPTHP->NumPTHP;
            } else if (SELECT_CASE_var == PkgTermHPWaterToAir_Num) {
                PTUnitNum = CompIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
            } else {
                assert(false);
            }
        }
        if (PTUnitNum > state.dataPTHP->NumPTUs || PTUnitNum < 1) {
            ShowFatalError(state,
                           format("SimPackagedTerminalUnit:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  PTUnitNum,
                                  state.dataPTHP->NumPTUs,
                                  CompName));
        }
        if (state.dataPTHP->CheckEquipName(PTUnitNum)) {
            if (CompName != state.dataPTHP->PTUnit(PTUnitNum).Name) {
                ShowFatalError(state,
                               format("SimPackagedTerminalUnit: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      PTUnitNum,
                                      CompName,
                                      state.dataPTHP->PTUnit(PTUnitNum).Name));
            }
            state.dataPTHP->CheckEquipName(PTUnitNum) = false;
        }
    }

    OnOffAirFlowRatio = 0.0;

    RemainingOutputToHeatingSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
    RemainingOutputToCoolingSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;

    if (RemainingOutputToCoolingSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
        QZnReq = RemainingOutputToCoolingSP;
    } else if (RemainingOutputToHeatingSP > 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
        QZnReq = RemainingOutputToHeatingSP;
    } else {
        QZnReq = 0.0;
    }

    state.dataSize->ZoneEqDXCoil = true;

    // Initialize the packaged terminal heat pump
    InitPTUnit(state, PTUnitNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq);

    SimPTUnit(state, PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);

    // Report the result of the simulation
    ReportPTUnit(state, PTUnitNum);

    state.dataSize->ZoneEqDXCoil = false;
}

void SimPTUnit(EnergyPlusData &state,
               int const PTUnitNum,           // number of the current Packaged Terminal Heat Pump being simulated
               int const ZoneNum,             // number of zone being served
               bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
               Real64 &QSensUnitOut,          // sensible delivered capacity [W]
               Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
               Real64 const QZnReq,           // cooling/heating needed by zone [W]
               Real64 &QLatUnitOut            // Latent delivered capacity [kg/s], dehumidification = negative
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       D. Shirey, Aug 2009 (QLatUnitOut)
    //       MODIFIED       Bo Shen, March 2012, added switch to variable-speed water-source heat pump
    //       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulate a packaged terminal heat pump; adjust its output to match the
    // remaining zone load.

    // METHODOLOGY EMPLOYED:
    // Calls ControlPTUnitOutput to obtain the desired unit output

    // Using/Aliasing
    using Psychrometrics::PsyHFnTdbW;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PartLoadFrac;      // compressor part load fraction
    bool UnitOn;              // TRUE if unit is on
    int OutletNode;           // PTUnit air outlet node
    int InletNode;            // PTUnit air inlet node
    Real64 QTotUnitOut;       // total delivered capacity [W]
    Real64 AirMassFlow;       // air mass flow rate [kg/s]
    Real64 SpecHumOut;        // Specific humidity ratio of outlet air (kg moisture / kg moist air)
    Real64 SpecHumIn;         // Specific humidity ratio of inlet air (kg moisture / kg moist air)
    int OpMode;               // operating mode (fan cycling or continious; DX coil always cycles)
    bool HXUnitOn;            // flag to enable heat exchanger
    Real64 QLatReq;           // latent cooling output needed by zone [W], now is zero
    Real64 QSensUnitOutMul;   // sensible output for the variable speed HP
    Real64 QLatUnitOutMul;    // latent output for the variable speed HP
    Real64 MinHumRat;         // min humidity for calculating sensible capacity of VS WSHP
    Real64 QSensUnitOutNoATM; // sensible unit output excluding air added by supply side air terminal mixer

    // zero the fan, DX coils, and supplemental electric heater electricity consumption
    state.dataHVACGlobal->DXElecCoolingPower = 0.0;
    state.dataHVACGlobal->DXElecHeatingPower = 0.0;
    state.dataHVACGlobal->ElecHeatingCoilPower = 0.0;
    state.dataHVACGlobal->SuppHeatingCoilPower = 0.0;
    state.dataPTHP->SaveCompressorPLR = 0.0;
    QLatReq = 0.0;

    // initialize local variables
    UnitOn = true;
    HXUnitOn = true;
    QSensUnitOut = 0.0;
    QLatUnitOut = 0.0;
    OutletNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
    OpMode = state.dataPTHP->PTUnit(PTUnitNum).OpMode;

    // reset operation flag if unit is off
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == CycFanCycCoil) {
        // cycling unit: only runs if there is a cooling or heating load.
        if ((!state.dataPTHP->CoolingLoad && !state.dataPTHP->HeatingLoad) || AirMassFlow < SmallMassFlow) {
            UnitOn = false;
        }
    } else if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        // continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
        if (AirMassFlow < SmallMassFlow) {
            UnitOn = false;
        }
    }

    state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;

    if (UnitOn) {
        if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
            SimVariableSpeedHP(state, PTUnitNum, ZoneNum, FirstHVACIteration, QZnReq, QLatReq, OnOffAirFlowRatio, OpMode, HXUnitOn);
        } else {
            ControlPTUnitOutput(state,
                                PTUnitNum,
                                FirstHVACIteration,
                                OpMode,
                                QZnReq,
                                ZoneNum,
                                PartLoadFrac,
                                OnOffAirFlowRatio,
                                state.dataPTHP->SupHeaterLoad,
                                HXUnitOn);
        }
    } else {
        PartLoadFrac = 0.0;
        state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 0.0; // SZVAV will operate at minimum fan speed
        OnOffAirFlowRatio = 1.0;
        state.dataPTHP->SupHeaterLoad = 0.0;
        if (state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling > 0) {
            CalcVarSpeedHeatPump(state,
                                 PTUnitNum,
                                 ZoneNum,
                                 FirstHVACIteration,
                                 0,
                                 1,
                                 0.0,
                                 PartLoadFrac,
                                 QSensUnitOutMul,
                                 QLatUnitOutMul,
                                 0.0,
                                 0.0,
                                 OnOffAirFlowRatio,
                                 state.dataPTHP->SupHeaterLoad,
                                 HXUnitOn);
        }
    }

    // calculate delivered capacity
    AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

    if (!state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
        CalcPTUnit(
            state, PTUnitNum, FirstHVACIteration, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, state.dataPTHP->SupHeaterLoad, HXUnitOn);
    } else {
        // calculate delivered capacity
        MinHumRat = min(state.dataLoopNodes->Node(InletNode).HumRat, state.dataLoopNodes->Node(OutletNode).HumRat);
        QSensUnitOut = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                      PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));
    }

    // CR9155 Remove specific humidity calculations
    SpecHumOut = state.dataLoopNodes->Node(OutletNode).HumRat;
    SpecHumIn = state.dataLoopNodes->Node(InletNode).HumRat;
    QLatUnitOut = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate, kg/s (dehumid = negative)
    QSensUnitOutNoATM = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                       PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));
    QTotUnitOut = AirMassFlow * (state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);

    if (!state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
        // report variables
        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = PartLoadFrac;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = state.dataPTHP->SaveCompressorPLR;
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == CycFanCycCoil) {
            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = PartLoadFrac;
        } else {
            if (UnitOn) {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0;
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 0.0;
            }
        }
    }

    state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergyRate = std::abs(min(0.0, QTotUnitOut));
    state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergyRate = std::abs(max(0.0, QTotUnitOut));
    state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergyRate = std::abs(min(0.0, QSensUnitOutNoATM));
    state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergyRate = std::abs(max(0.0, QSensUnitOutNoATM));
    state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOutNoATM)));
    state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOutNoATM)));

    Real64 locFanElecPower = 0.0;
    if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
        locFanElecPower = Fans::GetFanPower(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
    } else {
        locFanElecPower = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->fanPower();
    }
    state.dataPTHP->PTUnit(PTUnitNum).ElecPower = locFanElecPower + state.dataHVACGlobal->DXElecCoolingPower +
                                                  state.dataHVACGlobal->DXElecHeatingPower + state.dataHVACGlobal->ElecHeatingCoilPower +
                                                  state.dataHVACGlobal->SuppHeatingCoilPower;
}

void GetPTUnit(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
    //                      Bereket Nigusse, FSEC, April 2011: added OA Mixer object type
    //       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
    //       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for packaged terminal units and stores it in PTUnit data structures

    // METHODOLOGY EMPLOYED:
    // Uses "Get" routines to read in data.

    // Using/Aliasing
    using DXCoils::GetDXCoilIndex;
    using Fans::GetFanAvailSchPtr;
    using Fans::GetFanIndex;
    using Fans::GetFanInletNode;
    using Fans::GetFanOutletNode;
    using Fans::GetFanType;
    using Fans::GetFanVolFlow;

    using MixedAir::GetOAMixerNodeNumbers;
    auto &GetDXCoilInletNode(DXCoils::GetCoilInletNode);
    auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
    using DXCoils::GetCoilCondenserInletNode;
    using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
    auto &GetHXDXCoilInletNode(HVACHXAssistedCoolingCoil::GetCoilInletNode);
    auto &GetHXDXCoilOutletNode(HVACHXAssistedCoolingCoil::GetCoilOutletNode);
    auto &GetHeatingCoilIndex(HeatingCoils::GetCoilIndex);
    auto &GetHeatingCoilInletNode(HeatingCoils::GetCoilInletNode);
    auto &GetHeatingCoilOutletNode(HeatingCoils::GetCoilOutletNode);
    auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
    using HeatingCoils::GetHeatingCoilTypeNum;
    auto &GetSteamCoilAirInletNode(SteamCoils::GetCoilAirInletNode);
    using SteamCoils::GetCoilAirOutletNode;
    using SteamCoils::GetCoilSteamInletNode;
    using SteamCoils::GetSteamCoilIndex;
    auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
    using SteamCoils::GetTypeOfCoil;
    using WaterCoils::GetCoilMaxWaterFlowRate;
    using WaterCoils::GetCoilWaterInletNode;
    auto &GetWaterCoilInletNode(WaterCoils::GetCoilInletNode);
    auto &GetWaterCoilOutletNode(WaterCoils::GetCoilOutletNode);
    using BranchNodeConnections::SetUpCompSets;
    using FluidProperties::GetSatDensityRefrig;
    using NodeInputManager::GetOnlySingleNode;
    auto &GetWtoAHPCoilCapacity(WaterToAirHeatPump::GetCoilCapacity);
    auto &GetWtoAHPSimpleCoilCapacity(WaterToAirHeatPumpSimple::GetCoilCapacity);
    auto &GetWtoAHPSimpleCoilInletNode(WaterToAirHeatPumpSimple::GetCoilInletNode);
    auto &GetWtoAHPSimpleCoilOutletNode(WaterToAirHeatPumpSimple::GetCoilOutletNode);
    auto &GetWtoAHPSimpleCoilIndex(WaterToAirHeatPumpSimple::GetCoilIndex);
    using DataHVACGlobals::WaterConstant;
    using DataHVACGlobals::WaterConstantOnDemand;
    using DataHVACGlobals::WaterCycling;
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;
    using OutAirNodeManager::CheckOutAirNodeNumber;
    using SingleDuct::GetATMixer;
    using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
    using VariableSpeedCoils::GetCoilIndexVariableSpeed;
    using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
    using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;
    using VariableSpeedCoils::GetVSCoilCondenserInletNode;
    using VariableSpeedCoils::SetVarSpeedCoilData;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetPTUnit: "); // include trailing blank space
    static constexpr std::string_view RoutineNameFull("GetPackagedTerminalHeatPumpInput");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PTUnitIndex;             // loop index
    int PTUnitNum;               // current packaged terminal unit number
    Array1D_string Alphas;       // Alpha items for object
    Array1D<Real64> Numbers;     // Numeric items for object
    Array1D_int OANodeNums(4);   // Node numbers of OA mixer (OA, EA, RA, MA)
    int FanInletNodeNum;         // Fan inlet node number
    int FanOutletNodeNum;        // Fan outlet node number
    int SuppHeatInletNodeNum;    // Supplemental heating coil inlet node number
    int SuppHeatOutletNodeNum;   // Supplemental heating coil outlet node number
    int CoolCoilInletNodeNum;    // cooling coil inlet node number
    int CoolCoilOutletNodeNum;   // cooling coil outlet node number
    std::string ACHeatCoilName;  // name of heating coil
    int HeatCoilInletNodeNum;    // heating coil inlet node number
    int HeatCoilOutletNodeNum;   // heating coil outlet node number
    int SuppHeatHWInletNodeNum;  // Supplemental heating coil Hot Water inlet node number
    int SuppHeatHWOutletNodeNum; // Supplemental heating coil Hot Water outlet node number
    std::string CompSetFanInlet;
    std::string CompSetCoolInlet;
    std::string CompSetFanOutlet;
    std::string CompSetCoolOutlet;
    std::string CompSetHeatInlet;
    std::string CompSetHeatOutlet;
    std::string CompSetSupHeatInlet;
    std::string CompSetSupHeatOutlet;
    int NumAlphas;                   // Number of Alphas for each GetObjectItem call
    int NumNumbers;                  // Number of Numbers for each GetObjectItem call
    int MaxAlphas;                   // Maximum number of alpha fields in all objects
    int MaxNumbers;                  // Maximum number of numeric fields in all objects
    int NumFields;                   // Total number of fields in object
    int IOStatus;                    // Used in GetObjectItem
    bool ErrorsFound(false);         // Set to true if errors in input, fatal at end of routine
    bool IsNotOK;                    // Flag to verify name
    std::string CurrentModuleObject; // Object type for getting and error messages
    bool errFlag(false);             // Error flag returned during CALL to mining functions
    Real64 FanVolFlow;               // maximum supply air volumetric flow rate of fan
    int TempNodeNum;                 // dummy variable to set up HW coil water inlet node
    int SteamIndex;                  // dummy variable to set up steam coil steam inlet density
    std::string SuppHeatCoilType;    // type of supplemental heating coil
    std::string SuppHeatCoilName;    // name of supplemental heating coil
    int CtrlZone;                    // index to loop counter
    int NodeNum;                     // index to loop counter
    bool ZoneNodeNotFound;           // used in error checking

    Array1D_string cAlphaFields;   // Alpha field names
    Array1D_string cNumericFields; // Numeric field names
    Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.

    MaxNumbers = 0;
    MaxAlphas = 0;

    // find the number of each type of packaged terminal unit
    CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
    state.dataPTHP->NumPTHP = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
    MaxNumbers = max(MaxNumbers, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    CurrentModuleObject = "ZoneHVAC:PackagedTerminalAirConditioner";
    state.dataPTHP->NumPTAC = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
    MaxNumbers = max(MaxNumbers, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    CurrentModuleObject = "ZoneHVAC:WaterToAirHeatPump";
    state.dataPTHP->NumPTWSHP = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
    MaxNumbers = max(MaxNumbers, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    Alphas.allocate(MaxAlphas);
    Numbers.dimension(MaxNumbers, 0.0);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNumbers);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNumbers, true);
    state.dataPTHP->NumPTUs = state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC + state.dataPTHP->NumPTWSHP;

    // allocate the data structures
    if (state.dataPTHP->NumPTUs > 0) {
        state.dataPTHP->PTUnit.allocate(state.dataPTHP->NumPTUs);
        state.dataPTHP->PTUnitUniqueNames.reserve(static_cast<unsigned>(state.dataPTHP->NumPTUs));
        state.dataPTHP->CheckEquipName.allocate(state.dataPTHP->NumPTUs);
        state.dataPTHP->PTUnitUNumericFields.allocate(state.dataPTHP->NumPTUs);
    }
    state.dataPTHP->CheckEquipName = true;

    // loop over PTHP units; get and load the input data
    for (PTUnitIndex = 1; PTUnitIndex <= state.dataPTHP->NumPTHP; ++PTUnitIndex) {

        FanInletNodeNum = 0;
        FanOutletNodeNum = 0;
        SuppHeatInletNodeNum = 0;
        SuppHeatOutletNodeNum = 0;
        CoolCoilInletNodeNum = 0;
        CoolCoilOutletNodeNum = 0;
        HeatCoilInletNodeNum = 0;
        HeatCoilOutletNodeNum = 0;
        SuppHeatHWInletNodeNum = 0;
        SuppHeatHWOutletNodeNum = 0;
        OANodeNums = 0;

        CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 PTUnitIndex,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        PTUnitNum = PTUnitIndex;
        state.dataPTHP->PTUnit(PTUnitNum).PTObjectIndex = PTUnitIndex;

        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames.allocate(NumNumbers);
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = "";
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = cNumericFields;

        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataPTHP->PTUnitUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataPTHP->PTUnit(PTUnitNum).Name = Alphas(1);
        state.dataPTHP->PTUnit(PTUnitNum).UnitType = CurrentModuleObject;
        state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num = iPTHPType::PTHPUnit;
        state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType = PkgTermHPAirToAir_Num;
        if (lAlphaBlanks(2)) {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer (index number)
            if (state.dataPTHP->PTUnit(PTUnitNum).SchedPtr == 0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" invalid data.");
                ShowContinueError(state, "invalid-not found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).AirInNode = GetOnlySingleNode(state,
                                                                        Alphas(3),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject,
                                                                        Alphas(1),
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                        1,
                                                                        ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).AirOutNode = GetOnlySingleNode(state,
                                                                         Alphas(4),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                         1,
                                                                         ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).OAMixType = Alphas(5);
        state.dataPTHP->PTUnit(PTUnitNum).OAMixName = Alphas(6);

        // check to see if local OA mixer specified
        if (!lAlphaBlanks(6)) {
            errFlag = false;
            ValidateComponent(
                state, state.dataPTHP->PTUnit(PTUnitNum).OAMixType, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag, CurrentModuleObject);
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                ErrorsFound = true;
            } else {
                // OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
                OANodeNums = GetOAMixerNodeNumbers(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "that was specified in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.");
                    ErrorsFound = true;
                } else {
                    //  Set connection type to 'Inlet', because this is not necessarily directly come from
                    //  outside air.  Outside Air Inlet Node List will set the connection to outside air
                    state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode = OANodeNums(1);
                    state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode = OANodeNums(2);
                }
            }
        }
        state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = Numbers(1);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(1), Numbers(1)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = Numbers(2);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(2), Numbers(2)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = Numbers(3);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(3), Numbers(3)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = Numbers(4);
        if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(4), Numbers(4)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        //   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
        if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow &&
            state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
            ShowSevereError(state, CurrentModuleObject + ' ' + cNumericFields(4) + " cannot be greater than " + cNumericFields(1));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = Numbers(5);
        if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(5), Numbers(5)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        //   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
        if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow &&
            state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, CurrentModuleObject + ' ' + cNumericFields(5) + " cannot be greater than " + cNumericFields(2));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = Numbers(6);
        if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(6), Numbers(6)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }
        //   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

        state.dataPTHP->PTUnit(PTUnitNum).FanType = Alphas(7);
        state.dataPTHP->PTUnit(PTUnitNum).FanName = Alphas(8);

        if (UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:SystemModel")) {
            state.dataPTHP->PTUnit(PTUnitNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, state.dataPTHP->PTUnit(PTUnitNum).FanName)); // call constructor
            state.dataPTHP->PTUnit(PTUnitNum).FanIndex = HVACFan::getFanObjectVectorIndex(state, state.dataPTHP->PTUnit(PTUnitNum).FanName);
            FanInletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->inletNodeNum;
            FanOutletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->outletNodeNum;
            FanVolFlow = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
            state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
            state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->availSchedIndex;
        } else {
            errFlag = false;
            GetFanType(state,
                       state.dataPTHP->PTUnit(PTUnitNum).FanName,
                       state.dataPTHP->PTUnit(PTUnitNum).FanType_Num,
                       errFlag,
                       CurrentModuleObject,
                       state.dataPTHP->PTUnit(PTUnitNum).Name);
            FanVolFlow = 0.0;
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            } else {
                GetFanIndex(
                    state, state.dataPTHP->PTUnit(PTUnitNum).FanName, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, errFlag, CurrentModuleObject);
                FanInletNodeNum =
                    GetFanInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                FanOutletNodeNum =
                    GetFanOutletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, FanVolFlow);
                state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
                // Get the fan's availability schedule
                state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                    GetFanAvailSchPtr(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
            }
        }
        if (FanVolFlow != AutoSize) {
            if (FanVolFlow < max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow,
                                 state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow,
                                 state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow)) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\", invalid air flow rate");
                ShowContinueError(state,
                                  format("air flow rate = {:.7T} in fan object {} is less than the maximum PTHP supply air flow rate.",
                                         FanVolFlow,
                                         state.dataPTHP->PTUnit(PTUnitNum).FanName));
                ShowContinueError(state, " The fan flow rate must be greater than the PTHP maximum supply air flow rate.");
                ErrorsFound = true;
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName = Alphas(10);
        if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:DX:SingleSpeed")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType = Alphas(9);
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num = CoilDX_HeatingEmpirical;
            errFlag = false;
            GetDXCoilIndex(state,
                           state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                           state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                           errFlag,
                           state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                           ObjexxFCL::Optional_bool_const());
            HeatCoilInletNodeNum = GetDXCoilInletNode(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            HeatCoilOutletNodeNum = GetDXCoilOutletNode(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            if (errFlag)
                ShowContinueError(
                    state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
        } else if (UtilityRoutines::SameString(Alphas(9), "COIL:HEATING:DX:VARIABLESPEED")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType = Alphas(9);
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num = Coil_HeatingAirToAirVariableSpeed;
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName = Alphas(10);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum = GetCoilIndexVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                HeatCoilInletNodeNum = GetCoilInletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                HeatCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            }
        } else {
            ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\", invalid field");
            ShowContinueError(state, " illegal " + cAlphaFields(9) + " = " + Alphas(9));
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilInletNodeNum = HeatCoilInletNodeNum;
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilOutletNodeNum = HeatCoilOutletNodeNum;

        state.dataPTHP->PTUnit(PTUnitNum).HeatConvergenceTol = Numbers(7);
        state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);
        state.dataPTHP->PTUnit(PTUnitNum).CoolConvergenceTol = Numbers(8);

        if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:DX:SingleSpeed") ||
            UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:DX:SingleSpeed")) {
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed;
                errFlag = false;
                GetDXCoilIndex(state,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                               errFlag,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                               ObjexxFCL::Optional_bool_const());
                CoolCoilInletNodeNum = GetDXCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetDXCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = GetCoilCondenserInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag)
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            } else if (UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = CoilDX_CoolingHXAssisted;
                errFlag = false;
                GetDXCoilIndex(
                    state,
                    GetHXDXCoilName(
                        state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag),
                    state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                    errFlag,
                    "Coil:Cooling:DX:SingleSpeed",
                    ObjexxFCL::Optional_bool_const());
                CoolCoilInletNodeNum = GetHXDXCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetHXDXCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = GetCoilCondenserInletNode(
                    state,
                    "Coil:Cooling:DX:SingleSpeed",
                    GetHXDXCoilName(
                        state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag),
                    errFlag);
                if (errFlag)
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            }
        } else if (UtilityRoutines::SameString(Alphas(11), "COIL:COOLING:DX:VARIABLESPEED")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum =
                    GetVSCoilCondenserInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);

                if (errFlag)
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            }
        } else {
            ShowWarningError(state, CurrentModuleObject + " illegal " + cAlphaFields(11) + " = " + Alphas(11));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }
        state.dataPTHP->PTUnit(PTUnitNum).CoolCoilInletNodeNum = CoolCoilInletNodeNum;
        state.dataPTHP->PTUnit(PTUnitNum).CoolCoilOutletNodeNum = CoolCoilOutletNodeNum;

        if (Alphas(9) == "COIL:HEATING:DX:VARIABLESPEED" && Alphas(11) == "COIL:COOLING:DX:VARIABLESPEED") {
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum > 0 && state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum > 0) {
                SetVarSpeedCoilData(state,
                                    state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                    ErrorsFound,
                                    _,
                                    state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum);
                state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel = true;
            }
        }

        // set minimum outdoor temperature for compressor operation
        SetMinOATCompressor(state,
                            PTUnitNum,
                            state.dataPTHP->PTUnit(PTUnitNum).Name,
                            CurrentModuleObject,
                            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                            ErrorsFound);

        SuppHeatCoilType = Alphas(13);
        SuppHeatCoilName = Alphas(14);
        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName = SuppHeatCoilName;
        if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric") ||
            UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Water") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Steam")) {
            state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType = SuppHeatCoilType;
            if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric")) {
                if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel")) {
                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingGasOrOtherFuel;
                } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric")) {
                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingElectric;
                }
                errFlag = false;
                ValidateComponent(state, SuppHeatCoilType, SuppHeatCoilName, errFlag, CurrentModuleObject);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    GetHeatingCoilIndex(state, SuppHeatCoilName, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag);
                    // Get the Supplemental Heating Coil Node Numbers
                    SuppHeatInletNodeNum = GetHeatingCoilInletNode(state, SuppHeatCoilType, SuppHeatCoilName, errFlag);
                    SuppHeatOutletNodeNum = GetHeatingCoilOutletNode(state, SuppHeatCoilType, SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Water")) {
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingWater;
                errFlag = false;
                SuppHeatHWInletNodeNum = GetCoilWaterInletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode = SuppHeatHWInletNodeNum;
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                }
                errFlag = false;
                SuppHeatInletNodeNum =
                    GetWaterCoilInletNode(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode = SuppHeatInletNodeNum;
                SuppHeatOutletNodeNum =
                    GetWaterCoilOutletNode(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Steam")) {
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingSteam;
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex =
                    GetSteamCoilIndex(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex == 0) {
                    ShowSevereError(
                        state, CurrentModuleObject + " illegal " + cAlphaFields(14) + " = " + state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName);
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                // IF (ErrFlag) CALL ShowContinueError(state, 'Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
                errFlag = false;
                SuppHeatHWInletNodeNum = GetCoilSteamInletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode = SuppHeatHWInletNodeNum;
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    state.dataPTHP->SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineNameFull);
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag) * state.dataPTHP->SteamDensity;
                }
                errFlag = false;
                SuppHeatInletNodeNum = GetSteamCoilAirInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode = SuppHeatInletNodeNum;
                SuppHeatOutletNodeNum = GetCoilAirOutletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
            }

        } else {
            ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(13) + " = " + Alphas(13));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat = Numbers(9);
        state.dataPTHP->PTUnit(PTUnitNum).MaxOATSupHeat = Numbers(10);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxOATSupHeat > 21.0) {
            ShowWarningError(
                state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name + ": " + cNumericFields(10) + " should be <= to 21.");
            ShowContinueError(state, format("...{} = {:.1T}", cNumericFields(10), Numbers(10)));
        }

        if (UtilityRoutines::SameString(Alphas(15), "BlowThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = BlowThru;
        if (UtilityRoutines::SameString(Alphas(15), "DrawThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = DrawThru;
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == 0) {
            ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(15) + " = " + Alphas(15));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        // Get AirTerminal mixer data
        GetATMixer(state,
                   state.dataPTHP->PTUnit(PTUnitNum).Name,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerName,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerType,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode,
                   state.dataPTHP->PTUnit(PTUnitNum).AirOutNode);
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide ||
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists = true;
        }
        // check that heat pump doesn' have local outside air and DOA
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
            ShowSevereError(state,
                            CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                "\". heat pump unit has local as well as central outdoor air specified");
            ErrorsFound = true;
        }
        // check that PTUnit inlet node is a zone exhaust node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heat Pumps air inlet node name must be the same as a zone exhaust node name.");
                ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(state,
                                  "..Heat pumps inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        // check that PTUnit outlet node is a zone inlet node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                        state.dataPTHP->PTUnit(PTUnitNum).ZonePtr = CtrlZone;
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heat Pumps air outlet node name must be the same as a zone inlet node name.");
                ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(state,
                                  "..Heat pumps outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            // check that the air teminal mixer out node is the heat pump inlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\". heat pump unit air inlet node name must be the same as the air terminal mixer outlet node name.");
                ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(
                    state, "..heat pump unit air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            // check that the air teminal mixer secondary air node is the heat pump outlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\". heat pump unit air outlet node name must be the same as the air terminal mixer secondary node name.");
                ShowContinueError(state, "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:SupplySideMixer object.");
                ShowContinueError(
                    state, "..heat pump unit air outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
            // check that the air teminal mixer secondary node is the supplemental heat coil air outlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode != SuppHeatOutletNodeNum) {
                ShowSevereError(
                    state,
                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                        "\". supplemental heating coil air outlet node name must be the same as an air terminal mixer secondary air node name.");
                ShowContinueError(state, "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(state, "..heat pump unit supp heater outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum));
                ErrorsFound = true;
            }
        }
        // Check component placement
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {
            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // check OA Mixer return node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..PTUnit air inlet node name            = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
                // Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
                if (OANodeNums(4) != FanInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" Fan inlet node name must be the same as the heat pumps");
                    ShowContinueError(state, "OutdoorAir:Mixer mixed air node name when blow through " + cAlphaFields(15) + " is specified.");
                    ShowContinueError(state, "..Fan inlet node name                   = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air node name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ErrorsFound = true;
                }
            }
            if (CoolCoilInletNodeNum != FanOutletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Fan outlet node name must be the same as the cooling coil");
                ShowContinueError(state, " inlet node name when blow through " + cAlphaFields(15) + " is specified.");
                ShowContinueError(state, "..Fan outlet node name         = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (CoolCoilOutletNodeNum != HeatCoilInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Cooling coil outlet node name must be the same as the heating coil inlet node name.");
                ShowContinueError(state, "..Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));
                ShowContinueError(state, "..Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (HeatCoilOutletNodeNum != SuppHeatInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Heating coil outlet node name must be the same as the supplemental heating coil inlet");
                ShowContinueError(state, " node name when blow through " + cAlphaFields(14) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name              = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Supplemental heating coil inlet node name  = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (SuppHeatOutletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.");
                ShowContinueError(state, "..Supplemental heating coil outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum));
                ShowContinueError(state,
                                  "..Heat pumps outlet node name                   = " +
                                      state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air teminal mixer out node is the fan inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode != FanInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". fan inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..fan inlet node name = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ErrorsFound = true;
                }
            }
        } else { // draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
            // check that PTUnit inlet node is a zone exhaust node.
            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // check OA Mixer return node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..PTUnit air inlet node name            = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
                // Fan outlet node name must be the same as the supplemental heating coil inlet node name
                if (CoolCoilInletNodeNum != OANodeNums(4)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil");
                    ShowContinueError(state, " inlet node name when draw through " + cAlphaFields(15) + " is specified.");
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ShowContinueError(state, "..Cooling coil inlet node name     = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
            if (CoolCoilOutletNodeNum != HeatCoilInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Cooling coil outlet node name must be the same as the heating coil inlet node name.");
                ShowContinueError(state, "..Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));
                ShowContinueError(state, "..Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (HeatCoilOutletNodeNum != FanInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Heating coil outlet node name must be the same as the fan inlet node name");
                ShowContinueError(state, " when draw through " + cAlphaFields(15) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Fan inlet node name           = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                ErrorsFound = true;
            }
            if (SuppHeatInletNodeNum != FanOutletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" Fan outlet node name must be the same");
                ShowContinueError(state, "as the supplemental heating coil inlet node name when draw through " + cAlphaFields(15) + " is specified.");
                ShowContinueError(state, "..Fan outlet node = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Supplemental heating coil inlet node = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (SuppHeatOutletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.");
                ShowContinueError(state, "..Supplemental heating coil outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum));
                ShowContinueError(state,
                                  "..Heat pumps outlet node name                = " +
                                      state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air teminal mixer out node is the cooling coil inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != CoolCoilInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". cooling coil inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
        } // IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

        state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr = GetScheduleIndex(state, Alphas(16));
        if (!lAlphaBlanks(16) && state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr == 0) {
            ShowSevereError(
                state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" " + cAlphaFields(16) + " not found: " + Alphas(16));
            ErrorsFound = true;
        } else if (lAlphaBlanks(16)) {
            //     default to cycling fan if not specified in input
            state.dataPTHP->PTUnit(PTUnitNum).OpMode = CycFanCycCoil;
        }

        if (!lAlphaBlanks(17)) {
            state.dataPTHP->PTUnit(PTUnitNum).AvailManagerListName = Alphas(17);
        }

        state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = 0;
        if (!lAlphaBlanks(18)) {
            state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(18), state.dataSize->ZoneHVACSizing);
            if (state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex == 0) {
                ShowSevereError(state, cAlphaFields(18) + " = " + Alphas(18) + " not found.");
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }
        }

        if (NumAlphas < 19) {
            state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
        } else if (!lAlphaBlanks(19)) {
            if (UtilityRoutines::SameString(Alphas(19), "SingleZoneVAV")) {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::CCM_ASHRAE;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = true;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = true;
            } else if (UtilityRoutines::SameString(Alphas(19), "None")) {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            }
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
        }

        if (NumNumbers > 10) {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp = Numbers(11);
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp = AutoSize; // what should happen here
        }
        if (NumNumbers > 11) {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp = Numbers(12);
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp = AutoSize; // what should happen here
        }

        //            if ( PTUnit( PTUnitNum ).MaxOATSupHeat > 21.0 ) {
        //                ShowWarningError(state,  CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name + ": " + cNumericFields( 11 ) +
        //"
        // should  be
        //<=  to 21."
        //);                 ShowContinueError(state,  format("...{} = {:.1T}", cNumericFields( 11 ),  Numbers( 11 )) );
        //            }

        //   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
        //                              UseCompressorOffFlow = operate at value specified by user
        //   AirFlowControl only valid if fan opmode = ContFanCycCoil
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == 0.0) {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOnFlow;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOffFlow;
        }

        //   Initialize last mode of compressor operation
        state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::HeatingMode;

        if (UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:OnOff") ||
            UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:ConstantVolume") ||
            UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:SystemModel")) {
            if (state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr > 0 &&
                UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:ConstantVolume")) {
                if (!CheckScheduleValueMinMax(state, state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                    ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "Fan operating mode must be continuous (fan operating mode schedule values > 0) for supply fan type Fan:ConstantVolume.");
                    ShowContinueError(state, "Error found in " + cAlphaFields(16) + " = " + Alphas(16));
                    ShowContinueError(state, "schedule values must be (>0., <=1.)");
                    ErrorsFound = true;
                } else if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow &&
                           state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != AutoSize &&
                           state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != AutoSize &&
                           state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != 0.0) {
                    ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(
                        state, "Outdoor air flow rate when compressor is off cannot be greater than supply air flow rate when compressor is off");
                    ErrorsFound = true;
                }
            }
        } else {
            ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(state,
                              cAlphaFields(8) + " \"" + state.dataPTHP->PTUnit(PTUnitNum).FanName +
                                  "\" must be type Fan:SystemModel, Fan:OnOff, or Fan:ConstantVolume.");
            ErrorsFound = true;
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignHeatingCapacity = GetCoilCapacityVariableSpeed(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
            //   check that OA flow in cooling must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(4) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(4) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = 0;
            }
            //   check that OA flow in heating must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(5) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(5) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = 0;
            }
            //   check that OA flow in no cooling and no heating must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(6) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(6) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = 0;
            }
        }

        // check for specific input requirements for SZVAV model
        if (state.dataPTHP->PTUnit(PTUnitNum).ControlType == iCtrlType::CCM_ASHRAE) {

            // MaxNoCoolHeatAirVolFlow should be greater than 0
            if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == 0) {
                ShowWarningError(state, format("{} illegal {} = {:.3T}", CurrentModuleObject, cNumericFields(3), Numbers(3)));
                ShowContinueError(
                    state, "... when " + cAlphaFields(19) + " = " + Alphas(19) + " the minimum operating air flow rate should be autosized or > 0.");
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                //                    ErrorsFound = true;
            }

            // only allowed for DX cooling coils at this time
            if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num != CoilDX_CoolingSingleSpeed) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, CurrentModuleObject + ": " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "Single Zone VAV control method requires specific cooling coil types.");
                    ShowContinueError(state, "Valid cooling coil type is Coil:Cooling:DX:SingleSpeed.");
                    ShowContinueError(state,
                                      "The input cooling coil type = " + state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType +
                                          ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                }
                // mark this coil as non-SZVAV type
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            }
            // only allow for DX heating coils at this time
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num != CoilDX_HeatingEmpirical) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, CurrentModuleObject + ": " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "Single Zone VAV control method requires specific heating coil types.");
                    ShowContinueError(state, "Valid heating coil type is Coil:Heating:DX:SingleSpeed.");
                    ShowContinueError(state,
                                      "The input heating coil type = " + state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType +
                                          ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                }
                // mark this coil as non-SZVAV type
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            }
        }

        CompSetFanInlet = state.dataLoopNodes->NodeID(FanInletNodeNum);
        CompSetFanOutlet = state.dataLoopNodes->NodeID(FanOutletNodeNum);
        CompSetCoolInlet = state.dataLoopNodes->NodeID(CoolCoilInletNodeNum);
        CompSetCoolOutlet = state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum);
        CompSetHeatInlet = state.dataLoopNodes->NodeID(HeatCoilInletNodeNum);
        CompSetHeatOutlet = state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum);
        CompSetSupHeatInlet = state.dataLoopNodes->NodeID(SuppHeatInletNodeNum);
        CompSetSupHeatOutlet = state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum);

        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).FanType,
                      state.dataPTHP->PTUnit(PTUnitNum).FanName,
                      CompSetFanInlet,
                      CompSetFanOutlet);

        // Add cooling coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                      CompSetCoolInlet,
                      CompSetCoolOutlet);

        // Add heating coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                      CompSetHeatInlet,
                      CompSetHeatOutlet);

        // Add supplemental heating coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      SuppHeatCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                      CompSetSupHeatInlet,
                      CompSetSupHeatOutlet);

        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTHPUnit) {
            if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                // Add heating coil water inlet node as actuator node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                // Add heating coil steam inlet node as actualtor node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Steam,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            }
        }
        if (OANodeNums(1) > 0) {
            // Set up component set for OA mixer - use OA node and Mixed air node
            SetUpCompSets(state,
                          state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                          state.dataPTHP->PTUnit(PTUnitNum).Name,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixType,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixName,
                          state.dataLoopNodes->NodeID(OANodeNums(1)),
                          state.dataLoopNodes->NodeID(OANodeNums(4)));
        }
    }

    // loop over PTAC units; get and load the input data
    for (PTUnitIndex = 1; PTUnitIndex <= state.dataPTHP->NumPTAC; ++PTUnitIndex) {

        FanInletNodeNum = 0;
        FanOutletNodeNum = 0;
        CoolCoilInletNodeNum = 0;
        CoolCoilOutletNodeNum = 0;
        HeatCoilInletNodeNum = 0;
        HeatCoilOutletNodeNum = 0;
        SuppHeatInletNodeNum = 0;
        OANodeNums = 0;

        CurrentModuleObject = "ZoneHVAC:PackagedTerminalAirConditioner";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 PTUnitIndex,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        PTUnitNum = PTUnitIndex + state.dataPTHP->NumPTHP;
        state.dataPTHP->PTUnit(PTUnitNum).PTObjectIndex = PTUnitIndex;

        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames.allocate(NumNumbers);
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = "";
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = cNumericFields;

        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataPTHP->PTUnitUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataPTHP->PTUnit(PTUnitNum).Name = Alphas(1);
        state.dataPTHP->PTUnit(PTUnitNum).UnitType = CurrentModuleObject;
        state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num = iPTHPType::PTACUnit;
        state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType = PkgTermACAirToAir_Num;
        if (lAlphaBlanks(2)) {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer (index number)
            if (state.dataPTHP->PTUnit(PTUnitNum).SchedPtr == 0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" invalid data.");
                ShowContinueError(state, "invalid-not found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).AirInNode = GetOnlySingleNode(state,
                                                                        Alphas(3),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject,
                                                                        Alphas(1),
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                        1,
                                                                        ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).AirOutNode = GetOnlySingleNode(state,
                                                                         Alphas(4),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                         1,
                                                                         ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).OAMixType = Alphas(5);
        state.dataPTHP->PTUnit(PTUnitNum).OAMixName = Alphas(6);

        // check to see if local OA mixer specified
        if (!lAlphaBlanks(6)) {
            errFlag = false;
            ValidateComponent(
                state, state.dataPTHP->PTUnit(PTUnitNum).OAMixType, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag, CurrentModuleObject);
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                ErrorsFound = true;
            } else {
                // OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
                OANodeNums = GetOAMixerNodeNumbers(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.");
                    ErrorsFound = true;
                } else {
                    //  Set connection type to 'Inlet', because this is not necessarily directly come from
                    //  outside air.  Outside Air Inlet Node List will set the connection to outside air
                    state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode = OANodeNums(1);
                    state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode = OANodeNums(2);
                }
            }
        }
        state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = Numbers(1);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(1), Numbers(1)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = Numbers(2);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(2), Numbers(2)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = Numbers(3);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(3), Numbers(3)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = Numbers(4);
        if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(4), Numbers(4)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        //   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
        if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow &&
            state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
            ShowSevereError(state, CurrentModuleObject + ' ' + cNumericFields(4) + " cannot be greater than " + cNumericFields(1));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = Numbers(5);
        if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(5), Numbers(5)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        //   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
        if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow &&
            state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, CurrentModuleObject + ' ' + cNumericFields(5) + " cannot be greater than " + cNumericFields(2));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = Numbers(6);
        if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != AutoSize) {
            ShowSevereError(state, format("{} illegal {} = {:.7T}", CurrentModuleObject, cNumericFields(6), Numbers(6)));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        //   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

        state.dataPTHP->PTUnit(PTUnitNum).FanType = Alphas(7);
        state.dataPTHP->PTUnit(PTUnitNum).FanName = Alphas(8);
        ValidateComponent(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "In " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }
        if (UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:SystemModel")) {
            state.dataPTHP->PTUnit(PTUnitNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, state.dataPTHP->PTUnit(PTUnitNum).FanName)); // call constructor
            state.dataPTHP->PTUnit(PTUnitNum).FanIndex = HVACFan::getFanObjectVectorIndex(state, state.dataPTHP->PTUnit(PTUnitNum).FanName);
            FanInletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->inletNodeNum;
            FanOutletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->outletNodeNum;
            FanVolFlow = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
            state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
            state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->availSchedIndex;
        } else {

            // Get the fan's availability schedule
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                GetFanAvailSchPtr(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }

            errFlag = false;
            GetFanType(state,
                       state.dataPTHP->PTUnit(PTUnitNum).FanName,
                       state.dataPTHP->PTUnit(PTUnitNum).FanType_Num,
                       errFlag,
                       CurrentModuleObject,
                       state.dataPTHP->PTUnit(PTUnitNum).Name);
            FanVolFlow = 0.0;
            if (errFlag) {
                ShowContinueError(
                    state, "...specified in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ErrorsFound = true;
            } else {
                GetFanIndex(
                    state, state.dataPTHP->PTUnit(PTUnitNum).FanName, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, errFlag, CurrentModuleObject);
                FanInletNodeNum =
                    GetFanInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                FanOutletNodeNum =
                    GetFanOutletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, FanVolFlow);
                state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
            }
        }
        if (FanVolFlow != AutoSize) {
            if (FanVolFlow < max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow,
                                 state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow,
                                 state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow)) {
                ShowWarningError(state,
                                 format("{} - air flow rate = {:.7T} in fan object {} is less than the maximum PTHP supply air flow rate.",
                                        CurrentModuleObject,
                                        FanVolFlow,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanName));
                ShowContinueError(state, " The fan flow rate must be greater than the PTHP maximum supply air flow rate.");
                ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }
        }

        //   Name is currently used in CALL to Sim routines, can't get rid of the character string at this time.
        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName = Alphas(10);
        ACHeatCoilName = Alphas(10);

        if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Electric") ||
            UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Water") || UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Steam")) {
            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType = Alphas(9);
            if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Electric")) {
                if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Fuel"))
                    state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num = Coil_HeatingGasOrOtherFuel;
                if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Electric"))
                    state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num = Coil_HeatingElectric;
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap =
                    GetHeatingCoilCapacity(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, ACHeatCoilName, ErrorsFound);
                errFlag = false;
                HeatCoilInletNodeNum = GetHeatingCoilInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, ACHeatCoilName, errFlag);
                HeatCoilOutletNodeNum = GetHeatingCoilOutletNode(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, ACHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Water")) {
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num = Coil_HeatingWater;
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode =
                    GetCoilWaterInletNode(state, "Coil:Heating:Water", ACHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", ACHeatCoilName, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow > 0.0) {
                    state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", ACHeatCoilName, errFlag);
                }
                HeatCoilInletNodeNum = GetWaterCoilInletNode(state, "Coil:Heating:Water", ACHeatCoilName, errFlag);
                HeatCoilOutletNodeNum =
                    GetWaterCoilOutletNode(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(9), "Coil:Heating:Steam")) {
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num = Coil_HeatingSteam;
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex = GetSteamCoilIndex(state, Alphas(9), ACHeatCoilName, errFlag);
                HeatCoilInletNodeNum = GetSteamCoilAirInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ACHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode =
                    GetCoilSteamInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ACHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, errFlag);
                SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                state.dataPTHP->SteamDensity =
                    GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineNameFull);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow > 0.0) {
                    state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                        GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, errFlag) * state.dataPTHP->SteamDensity;
                }
                HeatCoilOutletNodeNum = GetCoilAirOutletNode(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ACHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ErrorsFound = true;
                }
                if (GetTypeOfCoil(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ACHeatCoilName, errFlag) !=
                    state.dataSteamCoils->ZoneLoadControl) {
                    if (errFlag) {
                        ShowContinueError(state,
                                          "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" +
                                              state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                        ErrorsFound = true;
                    }
                    ShowSevereError(state,
                                    CurrentModuleObject + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" Steam coil type of control must be set to ZoneLoadControl in the heating coil = Coil:Heating:Steam \"" +
                                        ACHeatCoilName + "\"");
                    ErrorsFound = true;
                }
            }
            state.dataPTHP->PTUnit(PTUnitNum).HeatCoilInletNodeNum = HeatCoilInletNodeNum;
            state.dataPTHP->PTUnit(PTUnitNum).HeatCoilOutletNodeNum = HeatCoilOutletNodeNum;
        } else {
            ShowWarningError(state, CurrentModuleObject + " illegal " + cAlphaFields(9) + " = " + Alphas(9));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).HeatConvergenceTol = 0.001;
        state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);

        if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:DX:SingleSpeed") ||
            UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:DX:SingleSpeed")) {
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed;
                errFlag = false;
                GetDXCoilIndex(state,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                               errFlag,
                               state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                               ObjexxFCL::Optional_bool_const());
                CoolCoilInletNodeNum = GetDXCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetDXCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = GetCoilCondenserInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = CoilDX_CoolingHXAssisted;
                errFlag = false;
                GetDXCoilIndex(
                    state,
                    GetHXDXCoilName(
                        state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag),
                    state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                    errFlag,
                    "Coil:Cooling:DX:SingleSpeed",
                    ObjexxFCL::Optional_bool_const());
                CoolCoilInletNodeNum = GetHXDXCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetHXDXCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = GetCoilCondenserInletNode(
                    state,
                    "Coil:Cooling:DX:SingleSpeed",
                    GetHXDXCoilName(
                        state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag),
                    errFlag);
                if (errFlag) {
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ErrorsFound = true;
                }
            }
        } else if (UtilityRoutines::SameString(Alphas(11), "COIL:COOLING:DX:VARIABLESPEED")) {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum =
                    GetVSCoilCondenserInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);

                if (errFlag)
                    ShowContinueError(
                        state, "...occurs in " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            }
        } else {
            ShowWarningError(state, CurrentModuleObject + " illegal " + cAlphaFields(11) + " = " + Alphas(11));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }
        state.dataPTHP->PTUnit(PTUnitNum).CoolCoilInletNodeNum = CoolCoilInletNodeNum;
        state.dataPTHP->PTUnit(PTUnitNum).CoolCoilOutletNodeNum = CoolCoilOutletNodeNum;

        if (UtilityRoutines::SameString(Alphas(13), "BlowThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = BlowThru;
        if (UtilityRoutines::SameString(Alphas(13), "DrawThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = DrawThru;
        //   default to draw through if not specified in input
        if (lAlphaBlanks(13)) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = DrawThru;
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == 0) {
            ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(13) + " = " + Alphas(13));
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ErrorsFound = true;
        }

        // set minimum outdoor temperature for compressor operation
        SetMinOATCompressor(state,
                            PTUnitNum,
                            state.dataPTHP->PTUnit(PTUnitNum).Name,
                            CurrentModuleObject,
                            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                            ErrorsFound);

        // Get AirTerminal mixer data
        GetATMixer(state,
                   state.dataPTHP->PTUnit(PTUnitNum).Name,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerName,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerType,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode,
                   state.dataPTHP->PTUnit(PTUnitNum).AirOutNode);
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide ||
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists = true;
        }
        // check that air-conditioner doesn't have local outside air and DOA
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
            ShowSevereError(state,
                            CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                "\". Air-conditioners has local as well as central outdoor air specified");
            ErrorsFound = true;
        }
        // check that Air-conditioners inlet node is a zone exhaust node or the OA Mixer return node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Air-conditioners air inlet node name must be the same as a zone exhaust node name.");
                ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(state,
                                  "..Air-conditioners inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        // check that Air-conditioners outlet node is a zone inlet node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                        state.dataPTHP->PTUnit(PTUnitNum).ZonePtr = CtrlZone;
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Air-conditioners air outlet node name must be the same as a zone inlet node name.");
                ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(
                    state, "..Air-conditioners outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            // check that the air teminal mixer out node is the air-conditioner inlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode) {
                ShowSevereError(state, CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Air-conditioners air inlet node name must be the same as the air terminal mixer outlet node name.");
                ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(
                    state, "..Air-conditioners air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            // check that the air teminal mixer secondary air node is the air-conditioner outlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode) {
                ShowSevereError(state, CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Air-conditioners air outlet node name must be the same as the air terminal mixer secondary node name.");
                ShowContinueError(state, "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(
                    state, "..Air-conditioners air outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
        }

        // Check component placement
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {

            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // Fan inlet node name must be the same as the air-conditioner's OA mixer mixed air node name
                if (OANodeNums(4) != FanInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" Fan inlet node name must be the same as the air conditioners");
                    ShowContinueError(state, "OutdoorAir:Mixer mixed air node name when blow through " + cAlphaFields(13) + " is specified.");
                    ShowContinueError(state, "..Fan inlet node name                   = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air node name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ErrorsFound = true;
                }

                // OA mixer return node must equal air-conditioner air inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..Heat Pump air inlet node name         = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
            }

            if (CoolCoilInletNodeNum != FanOutletNodeNum) { // check that fan outlet equals cooling coil inlet
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Fan outlet node name must be the same as the cooling coil");
                ShowContinueError(state, " inlet node name when blow through " + cAlphaFields(12) + " is specified.");
                ShowContinueError(state, "..Fan outlet node name         = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (CoolCoilOutletNodeNum != HeatCoilInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Cooling coil outlet node name must be the same as the heating coil inlet node name.");
                ShowContinueError(state, "..Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));
                ShowContinueError(state, "..Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (HeatCoilOutletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Heating coil outlet node name must be the same as the air conditioners outlet");
                ShowContinueError(state, " node name when blow through " + cAlphaFields(12) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name      = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Air conditioners outlet node name  = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) == 0) {
                // For no OA Mixer fan inlet node name must be the same as the Air-conditioner's inlet air node name
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != FanInletNodeNum) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Fan inlet node name must be the same as the Air-conditioners inlet air node name");
                    ShowContinueError(state, "..when blow through " + cAlphaFields(16) + " is specified and an outdoor air mixer is not used.");
                    ShowContinueError(state, "..Fan inlet node name           = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ShowContinueError(
                        state, "..Heat pump air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ErrorsFound = true;
                }
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air teminal mixer outlet node is the fan inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode != FanInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". fan inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..fan inlet node name = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ErrorsFound = true;
                }
            }

        } else { // draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // check OA Mixer return node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" Air Conditioners air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..Air Conditioner air inlet node name   = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
                // cooling coil inlet node name must be the same as the OA mixers mixed air node name
                if (CoolCoilInletNodeNum != OANodeNums(4)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil");
                    ShowContinueError(state, " inlet node name when draw through " + cAlphaFields(13) + " is specified.");
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ShowContinueError(state, "..Cooling coil inlet node name     = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
            if (CoolCoilOutletNodeNum != HeatCoilInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Cooling coil outlet node name must be the same as the heating coil inlet node name.");
                ShowContinueError(state, "..Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));
                ShowContinueError(state, "..Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (HeatCoilOutletNodeNum != FanInletNodeNum) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\" Heating coil outlet node name must be the same as the fan inlet node name");
                ShowContinueError(state, " when blow through " + cAlphaFields(13) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Fan inlet node name           = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                ErrorsFound = true;
            }
            if (FanOutletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" Fan outlet node name must be the same");
                ShowContinueError(state, "as the air conditioners outlet node name when draw through " + cAlphaFields(13) + " is specified.");
                ShowContinueError(state, "..Fan outlet node  name             = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Air conditioners outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air teminal mixer out node is the cooling coil inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != CoolCoilInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". cooling coil inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
        } // IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

        state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr = GetScheduleIndex(state, Alphas(14));
        if (!lAlphaBlanks(14) && state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr == 0) {
            ShowSevereError(
                state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" " + cAlphaFields(14) + " not found: " + Alphas(14));
            ErrorsFound = true;
        } else if (lAlphaBlanks(14)) {
            //     default to cycling fan if not specified in input
            state.dataPTHP->PTUnit(PTUnitNum).OpMode = CycFanCycCoil;
        }

        if (!lAlphaBlanks(15)) {
            state.dataPTHP->PTUnit(PTUnitNum).AvailManagerListName = Alphas(15);
        }

        state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = 0;
        if (!lAlphaBlanks(16)) {
            state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(16), state.dataSize->ZoneHVACSizing);
            if (state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex == 0) {
                ShowSevereError(state, cAlphaFields(16) + " = " + Alphas(16) + " not found.");
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }
        }

        if (NumAlphas < 17) {
            state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
        } else if (!lAlphaBlanks(17)) {
            if (UtilityRoutines::SameString(Alphas(17), "SingleZoneVAV")) {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::CCM_ASHRAE;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = true;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = true;
            } else if (UtilityRoutines::SameString(Alphas(17), "None")) {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            }
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
        }

        if (NumNumbers > 6) {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp = Numbers(7);
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp = AutoSize; // what should happen here
        }
        if (NumNumbers > 7) {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp = Numbers(8);
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp = AutoSize; // what should happen here
        }

        //   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
        //                              UseCompressorOffFlow = operate at value specified by user
        //   AirFlowControl only valid if fan opmode = ContFanCycCoil
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == 0.0) {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOnFlow;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOffFlow;
        }

        //   Initialize last mode of compressor operation
        state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::HeatingMode;

        if (UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:OnOff") ||
            UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:ConstantVolume") ||
            UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:SystemModel")) {
            if (state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr > 0 &&
                UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:ConstantVolume")) {
                if (!CheckScheduleValueMinMax(state, state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                    ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "Fan operating mode must be continuous (fan operating mode schedule values > 0) for supply fan type Fan:ConstantVolume.");
                    ShowContinueError(state, "Error found in " + cAlphaFields(14) + " = " + Alphas(14));
                    ShowContinueError(state, "schedule values must be (>0., <=1.)");
                    ErrorsFound = true;
                } else if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow &&
                           state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != AutoSize &&
                           state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != AutoSize &&
                           state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != 0.0) {
                    ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(
                        state, "Outdoor air flow rate when compressor is off cannot be greater than supply air flow rate when compressor is off");
                    ErrorsFound = true;
                }
            }
        } else {
            ShowSevereError(state, CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(
                state, cAlphaFields(8) + " \"" + state.dataPTHP->PTUnit(PTUnitNum).FanName + "\" must be type Fan:OnOff or Fan:ConstantVolume.");
            ErrorsFound = true;
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel = true;
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
            //   check that OA flow in cooling must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(4) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(4) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = 0;
            }
            //   check that OA flow in heating must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(5) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(5) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = 0;
            }
            //  check that OA flow in no cooling and no heating must be set to zero when connected to DOAS
            if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != 0.0) {
                ShowWarningError(state, CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ShowContinueError(state, ".. " + cNumericFields(6) + " must be zero when " + CurrentModuleObject);
                ShowContinueError(state, "..object is connected to central dedicated outdoor air system via AirTerminal:SingleDuct:Mixer");
                ShowContinueError(state, ".. " + cNumericFields(6) + " is set to 0 and simulation continues.");
                state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = 0;
            }
        }

        // check for specific input requirements for ASHRAE90.1 model
        if (state.dataPTHP->PTUnit(PTUnitNum).ControlType == iCtrlType::CCM_ASHRAE) {

            // MaxNoCoolHeatAirVolFlow should be greater than 0
            if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == 0) {
                ShowWarningError(state, format("{} illegal {} = {:.3T}", CurrentModuleObject, cNumericFields(3), Numbers(3)));
                ShowContinueError(
                    state, "... when " + cAlphaFields(17) + " = " + Alphas(17) + " the minimum operating air flow rate should be autosized or > 0.");
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                //                    ErrorsFound = true;
            }

            // only allowed for DX cooling coils at this time
            if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num != CoilDX_CoolingSingleSpeed) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, CurrentModuleObject + ": " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "ASHRAE90.1 control method requires specific cooling coil types.");
                    ShowContinueError(state, "Valid cooling coil type is Coil:Cooling:DX:SingleSpeed.");
                    ShowContinueError(state,
                                      "The input cooling coil type = " + state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType +
                                          ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                }
                // mark this coil as non-ASHRAE90 type
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
            }
            // only allow for water, fuel, or electric at this time
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num != Coil_HeatingWater &&
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num != Coil_HeatingGasOrOtherFuel &&
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num != Coil_HeatingElectric) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, CurrentModuleObject + ": " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "ASHRAE90.1 control method requires specific heating coil types.");
                    ShowContinueError(state, "Valid heating coil type is Coil:Heating:DX:SingleSpeed.");
                    ShowContinueError(state,
                                      "The input heating coil type = " + state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType +
                                          ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                }
                // mark this coil as non-ASHRAE90 type
                state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;
            }
        }

        CompSetFanInlet = state.dataLoopNodes->NodeID(FanInletNodeNum);
        CompSetFanOutlet = state.dataLoopNodes->NodeID(FanOutletNodeNum);
        CompSetCoolInlet = state.dataLoopNodes->NodeID(CoolCoilInletNodeNum);
        CompSetCoolOutlet = state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum);
        CompSetHeatInlet = state.dataLoopNodes->NodeID(HeatCoilInletNodeNum);
        CompSetHeatOutlet = state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum);

        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).FanType,
                      state.dataPTHP->PTUnit(PTUnitNum).FanName,
                      state.dataLoopNodes->NodeID(FanInletNodeNum),
                      state.dataLoopNodes->NodeID(FanOutletNodeNum));

        // Add cooling coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                      state.dataLoopNodes->NodeID(CoolCoilInletNodeNum),
                      state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));

        // Add heating coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType,
                      ACHeatCoilName,
                      state.dataLoopNodes->NodeID(HeatCoilInletNodeNum),
                      state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));

        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                // Add heating coil water inlet node as actuator node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                // Add heating coil steam inlet node as actualtor node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Steam,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            }
        }

        if (OANodeNums(1) > 0) {
            // Set up component set for OA mixer - use OA node and Mixed air node
            SetUpCompSets(state,
                          state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                          state.dataPTHP->PTUnit(PTUnitNum).Name,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixType,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixName,
                          state.dataLoopNodes->NodeID(OANodeNums(1)),
                          state.dataLoopNodes->NodeID(OANodeNums(4)));
        }
    }

    //***********************************************************************************

    for (PTUnitIndex = 1; PTUnitIndex <= state.dataPTHP->NumPTWSHP; ++PTUnitIndex) {

        FanInletNodeNum = 0;
        FanOutletNodeNum = 0;
        CoolCoilInletNodeNum = 0;
        CoolCoilOutletNodeNum = 0;
        HeatCoilInletNodeNum = 0;
        HeatCoilOutletNodeNum = 0;
        SuppHeatInletNodeNum = 0;
        SuppHeatOutletNodeNum = 0;
        SuppHeatHWInletNodeNum = 0;
        SuppHeatHWOutletNodeNum = 0;
        OANodeNums = 0;

        CurrentModuleObject = "ZoneHVAC:WaterToAirHeatPump";
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 PTUnitIndex,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        PTUnitNum = PTUnitIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
        state.dataPTHP->PTUnit(PTUnitNum).PTObjectIndex = PTUnitIndex;

        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames.allocate(NumNumbers);
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = "";
        state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames = cNumericFields;

        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataPTHP->PTUnitUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataPTHP->PTUnit(PTUnitNum).Name = Alphas(1);
        state.dataPTHP->PTUnit(PTUnitNum).UnitType = CurrentModuleObject;
        state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num = iPTHPType::PTWSHPUnit;
        state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType = PkgTermHPWaterToAir_Num;
        if (lAlphaBlanks(2)) {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataPTHP->PTUnit(PTUnitNum).SchedPtr == 0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\" invalid data.");
                ShowContinueError(state, "invalid-not found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).AirInNode = GetOnlySingleNode(state,
                                                                        Alphas(3),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject,
                                                                        Alphas(1),
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                        1,
                                                                        ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).AirOutNode = GetOnlySingleNode(state,
                                                                         Alphas(4),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                         1,
                                                                         ObjectIsParent);

        state.dataPTHP->PTUnit(PTUnitNum).OAMixType = Alphas(5);
        state.dataPTHP->PTUnit(PTUnitNum).OAMixName = Alphas(6);

        // check to see if local OA mixer specified
        if (!lAlphaBlanks(6)) {
            errFlag = false;
            ValidateComponent(
                state, state.dataPTHP->PTUnit(PTUnitNum).OAMixType, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag, CurrentModuleObject);
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                ErrorsFound = true;
            } else {
                // OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
                OANodeNums = GetOAMixerNodeNumbers(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "that was specified in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ShowContinueError(state, "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.");
                    ErrorsFound = true;
                } else {
                    //  Set connection type to 'Inlet', because this is not necessarily directly come from
                    //  outside air.  Outside Air Inlet Node List will set the connection to outside air
                    state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode = OANodeNums(1);
                    state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode = OANodeNums(2);
                }
            }
        }

        // Get fan data
        state.dataPTHP->PTUnit(PTUnitNum).FanType = Alphas(7);
        state.dataPTHP->PTUnit(PTUnitNum).FanName = Alphas(8);
        state.dataPTHP->PTUnit(PTUnitNum).FanName = Alphas(8);
        ValidateComponent(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
            ErrorsFound = true;
        } else {

            if (UtilityRoutines::SameString(state.dataPTHP->PTUnit(PTUnitNum).FanType, "Fan:SystemModel")) {
                state.dataPTHP->PTUnit(PTUnitNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, state.dataPTHP->PTUnit(PTUnitNum).FanName)); // call constructor
                state.dataPTHP->PTUnit(PTUnitNum).FanIndex = HVACFan::getFanObjectVectorIndex(state, state.dataPTHP->PTUnit(PTUnitNum).FanName);
                FanInletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->inletNodeNum;
                FanOutletNodeNum = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->outletNodeNum;
                FanVolFlow = state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
                state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
                state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                    state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->availSchedIndex;
            } else {
                errFlag = false;
                GetFanType(state,
                           state.dataPTHP->PTUnit(PTUnitNum).FanName,
                           state.dataPTHP->PTUnit(PTUnitNum).FanType_Num,
                           errFlag,
                           CurrentModuleObject,
                           Alphas(1));
                FanVolFlow = 0.0;
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                    ErrorsFound = true;
                }

                if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == FanType_SimpleOnOff) {

                    errFlag = false;
                    GetFanIndex(state,
                                state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                state.dataPTHP->PTUnit(PTUnitNum).FanIndex,
                                errFlag,
                                ObjexxFCL::Optional_string_const());
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    FanInletNodeNum =
                        GetFanInletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    FanOutletNodeNum =
                        GetFanOutletNode(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, FanVolFlow);
                        state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
                    }
                    errFlag = false;
                    state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr =
                        GetFanAvailSchPtr(state, state.dataPTHP->PTUnit(PTUnitNum).FanType, state.dataPTHP->PTUnit(PTUnitNum).FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
        }

        // Get heating coil type and name data
        if (Alphas(9) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType = Alphas(9);
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num = Coil_HeatingWaterToAirHPSimple;
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName = Alphas(10);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum = GetWtoAHPSimpleCoilIndex(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                HeatCoilInletNodeNum = GetWtoAHPSimpleCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                HeatCoilOutletNodeNum = GetWtoAHPSimpleCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            }
        } else if (Alphas(9) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType = Alphas(9);
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit;
            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName = Alphas(10);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum = GetCoilIndexVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                HeatCoilInletNodeNum = GetCoilInletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
                HeatCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            }
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\"");
            ShowContinueError(state, "Illegal " + cAlphaFields(9) + " = " + Alphas(9));
            ErrorsFound = true;
        }

        // Get Cooling Coil Information if available
        if (Alphas(11) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = Coil_CoolingWaterToAirHPSimple;
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum = GetWtoAHPSimpleCoilIndex(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                CoolCoilInletNodeNum = GetWtoAHPSimpleCoilInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetWtoAHPSimpleCoilOutletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            }
        } else if (Alphas(11) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType = Alphas(11);
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit;
            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName = Alphas(12);
            ValidateComponent(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                }
                CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
                CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            }
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\"");
            ShowContinueError(state, "Illegal " + cAlphaFields(11) + "=\"" + Alphas(11) + "\".");
            ErrorsFound = true;
        }

        if (NumAlphas >= 19) {
            // get water flow mode info before calling SetSimpleWSHPData
            if (UtilityRoutines::SameString(Alphas(19), "Constant")) state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode = WaterConstant;
            if (UtilityRoutines::SameString(Alphas(19), "Cycling")) state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode = WaterCycling;
            if (UtilityRoutines::SameString(Alphas(19), "ConstantOnDemand"))
                state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode = WaterConstantOnDemand;
            // default to draw through if not specified in input
            if (lAlphaBlanks(19)) state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode = WaterCycling;
            if (state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode == 0) {
                ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(19) + " = " + Alphas(19));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode = WaterCycling;
        }

        // end get water flow mode info
        if (Alphas(9) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT" && Alphas(11) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum > 0 && state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum > 0) {
                WaterToAirHeatPumpSimple::SetSimpleWSHPData(state,
                                                            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                                            ErrorsFound,
                                                            state.dataPTHP->PTUnit(PTUnitNum).WaterCyclingMode,
                                                            _,
                                                            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum);
            }
        } else if (Alphas(9) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" &&
                   Alphas(11) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum > 0 && state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum > 0) {
                SetVarSpeedCoilData(state,
                                    state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                    ErrorsFound,
                                    _,
                                    state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum);
                state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel = true;
            }
        } else {
            ShowContinueError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\"");
            ShowContinueError(state, "Cooling coil and heating coil should use the equation fit model and be of same general type");
            ErrorsFound = true;
        }

        // set minimum outdoor temperature for compressor operation
        SetMinOATCompressor(state,
                            PTUnitNum,
                            state.dataPTHP->PTUnit(PTUnitNum).Name,
                            CurrentModuleObject,
                            state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                            state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                            ErrorsFound);

        // Get supplemental heating coil information

        SuppHeatCoilType = Alphas(13);
        SuppHeatCoilName = Alphas(14);
        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName = SuppHeatCoilName;
        if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric") ||
            UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Water") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Steam")) {
            state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType = SuppHeatCoilType;
            if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel") || UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric")) {
                if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Fuel")) {
                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingGasOrOtherFuel;
                } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric")) {
                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingElectric;
                }
                errFlag = false;
                ValidateComponent(state, SuppHeatCoilType, SuppHeatCoilName, errFlag, CurrentModuleObject);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    GetHeatingCoilIndex(state, SuppHeatCoilName, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag);
                    // Get the Supplemental Heating Coil Node Numbers
                    SuppHeatInletNodeNum = GetHeatingCoilInletNode(state, SuppHeatCoilType, SuppHeatCoilName, errFlag);
                    SuppHeatOutletNodeNum = GetHeatingCoilOutletNode(state, SuppHeatCoilType, SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Water")) {
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingWater;
                errFlag = false;
                SuppHeatHWInletNodeNum = GetCoilWaterInletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode = SuppHeatHWInletNodeNum;
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                }
                errFlag = false;
                SuppHeatInletNodeNum =
                    GetWaterCoilInletNode(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode = SuppHeatInletNodeNum;
                SuppHeatOutletNodeNum =
                    GetWaterCoilOutletNode(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Steam")) {
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num = Coil_HeatingSteam;
                errFlag = false;
                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex =
                    GetSteamCoilIndex(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex == 0) {
                    ShowSevereError(
                        state, CurrentModuleObject + " illegal " + cAlphaFields(14) + " = " + state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName);
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                errFlag = false;
                SuppHeatHWInletNodeNum = GetCoilSteamInletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode = SuppHeatHWInletNodeNum;
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    state.dataPTHP->SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineNameFull);
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, errFlag) * state.dataPTHP->SteamDensity;
                }
                errFlag = false;
                SuppHeatInletNodeNum = GetSteamCoilAirInletNode(
                    state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode = SuppHeatInletNodeNum;
                SuppHeatOutletNodeNum = GetCoilAirOutletNode(state, SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    ErrorsFound = true;
                }
            }
        } else {
            ShowSevereError(state, CurrentModuleObject + " = " + Alphas(1));
            ShowContinueError(state, "Illegal " + cAlphaFields(13) + " = " + Alphas(13));
            ErrorsFound = true;
        }

        if (lAlphaBlanks(15)) {
            state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = 0;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum = GetOnlySingleNode(state,
                                                                                   Alphas(15),
                                                                                   ErrorsFound,
                                                                                   CurrentModuleObject,
                                                                                   Alphas(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                   1,
                                                                                   ObjectIsNotParent);
            // need better verification.
            if (!CheckOutAirNodeNumber(state, state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum)) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\"");
                ShowContinueError(state, " Node name of outdoor dry-bulb temperature sensor not valid outdoor air node=\"" + Alphas(15) + "\"");
                ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }
        }

        if (UtilityRoutines::SameString(Alphas(16), "BlowThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = BlowThru;
        if (UtilityRoutines::SameString(Alphas(16), "DrawThrough")) state.dataPTHP->PTUnit(PTUnitNum).FanPlace = DrawThru;
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == 0) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\"");
            ShowContinueError(state, "Illegal " + cAlphaFields(16) + "=\"" + Alphas(16) + "\".");
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr = GetScheduleIndex(state, Alphas(17));
        if (!lAlphaBlanks(17) && state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr == 0) {
            ShowSevereError(state, CurrentModuleObject + " = " + Alphas(1));
            ShowContinueError(state, "Illegal " + cAlphaFields(17) + " = " + Alphas(17));
            ErrorsFound = true;
        } else if (lAlphaBlanks(17)) {
            state.dataPTHP->PTUnit(PTUnitNum).OpMode = CycFanCycCoil;
        }

        if (!lAlphaBlanks(18)) {
            state.dataPTHP->PTUnit(PTUnitNum).AvailManagerListName = Alphas(18);
        }

        state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = 0;
        if (!lAlphaBlanks(20)) {
            state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(20), state.dataSize->ZoneHVACSizing);
            if (state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex == 0) {
                ShowSevereError(state, cAlphaFields(20) + " = " + Alphas(20) + " not found.");
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                ErrorsFound = true;
            }
        }

        // Get AirTerminal mixer data
        GetATMixer(state,
                   state.dataPTHP->PTUnit(PTUnitNum).Name,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerName,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerType,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode,
                   state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode,
                   state.dataPTHP->PTUnit(PTUnitNum).AirOutNode);
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide ||
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists = true;
        }

        // check that heat pump doesn' have local outside air and DOA
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
            ShowSevereError(state,
                            CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                "\". heat pump unit has local as well as central outdoor air specified");
            ErrorsFound = true;
        }

        // Check component placement and connectivity

        // Placement checks good for both blow-thru and draw-thru fan
        if (CoolCoilOutletNodeNum != HeatCoilInletNodeNum) { // cooling coil outlet must equal heating coil inlet
            ShowSevereError(state,
                            CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                "\" Cooling coil outlet node name must be the same as the heating coil inlet node name.");
            ShowContinueError(state, "..Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum));
            ShowContinueError(state, "..Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatCoilInletNodeNum));
            ErrorsFound = true;
        }
        if (SuppHeatOutletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirOutNode) { // check that supp HC out = heat pump air outlet
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(state, "..Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.");
            ShowContinueError(state, "..Supplemental heating coil outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum));
            ShowContinueError(state,
                              "..Heat pumps outlet node name                   = " +
                                  state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
            ErrorsFound = true;
        }
        // check that PTUnit inlet node is a zone exhaust node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heat Pumps air inlet node name must be the same as a zone exhaust node name.");
                ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(state,
                                  "..Heat pumps inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        // check that PTUnit outlet node is a zone inlet node.
        if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists || state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            ZoneNodeNotFound = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                        state.dataPTHP->PTUnit(PTUnitNum).ZonePtr = CtrlZone;
                        ZoneNodeNotFound = false;
                        break;
                    }
                }
            }
            if (ZoneNodeNotFound) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heat Pumps air outlet node name must be the same as a zone inlet node name.");
                ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                ShowContinueError(state,
                                  "..Heat pumps outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
            // check that the air teminal mixer out node is the heat pump inlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\". heat pump unit air inlet node name must be the same as the air terminal mixer outlet node name.");
                ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(
                    state, "..heat pump unit air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                ErrorsFound = true;
            }
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            // check that the air teminal mixer secondary air node is the heat pump outlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode != state.dataPTHP->PTUnit(PTUnitNum).ATMixerSecNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                    "\". heat pump unit air outlet node name must be the same as the air terminal mixer secondary node name.");
                ShowContinueError(state, "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(
                    state, "..heat pump unit air outlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode));
                ErrorsFound = true;
            }
            // check that the air terminal mixer secondary node is the supplemental heat coil air outlet node
            if (state.dataPTHP->PTUnit(PTUnitNum).AirOutNode != SuppHeatOutletNodeNum) {
                ShowSevereError(
                    state,
                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                        "\". supplemental heating coil air outlet node name must be the same as an air terminal mixer secondary air node name.");
                ShowContinueError(state, "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:Mixer object.");
                ShowContinueError(state, "..heat pump unit supp heater outlet node name = " + state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum));
                ErrorsFound = true;
            }
        }

        // check connectivity for blow through fan
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {
            if (CoolCoilInletNodeNum != FanOutletNodeNum) { // check that fan outlet equals cooling coil inlet
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Fan outlet node name must be the same as the cooling coil inlet node name");
                ShowContinueError(state, "..when blow through " + cAlphaFields(16) + " is specified.");
                ShowContinueError(state, "..Fan outlet node name         = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                ErrorsFound = true;
            }
            if (HeatCoilOutletNodeNum != SuppHeatInletNodeNum) { // check that heating coil outlet equals supp heating coil inlet
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heating coil outlet node name must be the same as the supplemental heating coil inlet node name");
                ShowContinueError(state, "..when blow through " + cAlphaFields(16) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name              = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Supplemental heating coil inlet node name  = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
                if (OANodeNums(4) != FanInletNodeNum) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Fan inlet node name must be the same as the heat pumps OutdoorAir:Mixer mixed air node name");
                    ShowContinueError(state, "..when blow through " + cAlphaFields(16) + " is specified.");
                    ShowContinueError(state, "..Fan inlet node name                   = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air node name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ErrorsFound = true;
                }
                // OA mixer return node must equal heat pump air inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..Heat Pump air inlet node name         = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
            }
            if (OANodeNums(4) == 0) {
                // For no OA Mixer fan inlet node name must be the same as the heat pump's inlet air node name
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != FanInletNodeNum) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Fan inlet node name must be the same as the heat pumps inlet air node name");
                    ShowContinueError(state, "..when blow through " + cAlphaFields(16) + " is specified and an outdoor air mixer is not used.");
                    ShowContinueError(state, "..Fan inlet node name           = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ShowContinueError(
                        state, "..Heat pump air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ErrorsFound = true;
                }
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air terminal mixer out node is the fan inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != FanInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". fan inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..fan inlet node name = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                    ErrorsFound = true;
                }
            }
        } // end blow through fan IF block

        // check connectivity for draw through fan
        if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == DrawThru) {
            if (HeatCoilOutletNodeNum != FanInletNodeNum) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Heating coil outlet node name must be the same as the fan inlet node name");
                ShowContinueError(state, "..when draw through " + cAlphaFields(16) + " is specified.");
                ShowContinueError(state, "..Heating coil outlet node name = " + state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum));
                ShowContinueError(state, "..Fan inlet node name           = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                ErrorsFound = true;
            }
            if (SuppHeatInletNodeNum != FanOutletNodeNum) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, "..Fan outlet node name must be the same as the supplemental heating coil inlet node name ");
                ShowContinueError(state, "..when draw through " + cAlphaFields(16) + " is specified.");
                ShowContinueError(state, "..Fan outlet node = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
                ShowContinueError(state, "..Supplemental heating coil inlet node = " + state.dataLoopNodes->NodeID(SuppHeatInletNodeNum));
                ErrorsFound = true;
            }
            if (OANodeNums(4) == 0) {
                // For no OA mixer, cooling coil inlet node name must be the same as the heat pump's inlet air node name
                if (CoolCoilInletNodeNum != state.dataPTHP->PTUnit(PTUnitNum).AirInNode) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Heat pump air inlet node name must be the same as the cooling coil inlet node name");
                    ShowContinueError(state, "..when draw through " + cAlphaFields(16) + " is specified and an outdoor air mixer is not used.");
                    ShowContinueError(
                        state, "..Heat pump air inlet node name = " + state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..Cooling coil inlet node name  = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
            if (!state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists && OANodeNums(4) > 0) {
                // Cooling coil inlet node name must be the same as the OA mixers mixed air node name
                if (CoolCoilInletNodeNum != OANodeNums(4)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil");
                    ShowContinueError(state, " inlet node name when draw through " + cAlphaFields(16) + " is specified.");
                    ShowContinueError(state, "..OutdoorAir:Mixer mixed air name = " + state.dataLoopNodes->NodeID(OANodeNums(4)));
                    ShowContinueError(state, "..Cooling coil inlet node name    = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
                // check OA Mixer return node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != OANodeNums(3)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name.");
                    ShowContinueError(state,
                                      "..Heat Pump air inlet node name         = " +
                                          state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).AirInNode));
                    ShowContinueError(state, "..OutdoorAir:Mixer return air node name = " + state.dataLoopNodes->NodeID(OANodeNums(3)));
                    ErrorsFound = true;
                }
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) {
                // check that the air teminal mixer out node is the cooling coil inlet node
                if (state.dataPTHP->PTUnit(PTUnitNum).AirInNode != CoolCoilInletNodeNum) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                        "\". cooling coil inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                    ShowContinueError(state, "..cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolCoilInletNodeNum));
                    ErrorsFound = true;
                }
            }
        } // end draw through IF block

        CompSetFanInlet = state.dataLoopNodes->NodeID(FanInletNodeNum);
        CompSetFanOutlet = state.dataLoopNodes->NodeID(FanOutletNodeNum);
        CompSetCoolInlet = state.dataLoopNodes->NodeID(CoolCoilInletNodeNum);
        CompSetCoolOutlet = state.dataLoopNodes->NodeID(CoolCoilOutletNodeNum);
        CompSetHeatInlet = state.dataLoopNodes->NodeID(HeatCoilInletNodeNum);
        CompSetHeatOutlet = state.dataLoopNodes->NodeID(HeatCoilOutletNodeNum);
        CompSetSupHeatInlet = state.dataLoopNodes->NodeID(SuppHeatInletNodeNum);
        CompSetSupHeatOutlet = state.dataLoopNodes->NodeID(SuppHeatOutletNodeNum);

        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).FanType,
                      state.dataPTHP->PTUnit(PTUnitNum).FanName,
                      CompSetFanInlet,
                      CompSetFanOutlet);

        // Add cooling coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                      CompSetCoolInlet,
                      CompSetCoolOutlet);

        // Add heating coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                      CompSetHeatInlet,
                      CompSetHeatOutlet);

        // Add supplemental heating coil to component sets array
        SetUpCompSets(state,
                      state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                      state.dataPTHP->PTUnit(PTUnitNum).Name,
                      SuppHeatCoilType,
                      SuppHeatCoilName,
                      CompSetSupHeatInlet,
                      CompSetSupHeatOutlet);

        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTWSHPUnit) {
            if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                // Add heating coil water inlet node as actuator node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                // Add heating coil steam inlet node as actualtor node for coil
                TempNodeNum = GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode),
                                                ErrorsFound,
                                                state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                DataLoopNode::NodeFluidType::Steam,
                                                DataLoopNode::NodeConnectionType::Actuator,
                                                1,
                                                ObjectIsParent);
            }
        }
        if (OANodeNums(1) > 0) {
            // Set up component set for OA mixer - use OA node and Mixed air node
            SetUpCompSets(state,
                          state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                          state.dataPTHP->PTUnit(PTUnitNum).Name,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixType,
                          state.dataPTHP->PTUnit(PTUnitNum).OAMixName,
                          state.dataLoopNodes->NodeID(OANodeNums(1)),
                          state.dataLoopNodes->NodeID(OANodeNums(4)));
        }

        // Set the Design Fan Volume Flow Rate
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate =
                state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
        } else {
            errFlag = false;
            GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, FanVolFlow);
            state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate = FanVolFlow;
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }
        //     PTUnit(PTUnitNum)%ActualFanVolFlowRate = MAX(Numbers(1),Numbers(2),Numbers(3))
        if (FanVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate != AutoSize) {
            if (state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate > FanVolFlow) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError(state, "... has a Design Fan Flow Rate > Max Fan Volume Flow Rate, should be <=.");
                ShowContinueError(state,
                                  format("... Entered value={:.2R}... Fan [{}:{}] Max Value={:.2R}",
                                         state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate,
                                         state.dataPTHP->PTUnit(PTUnitNum).FanType,
                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                         FanVolFlow));
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate <= 0.0) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError(state, "... has a Design Fan Flow Rate <= 0.0, it must be >0.0");
                ShowContinueError(state, format("... Entered value={:.2R}", state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate));
                ErrorsFound = true;
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = Numbers(1);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(state, format(" illegal value {} = {:.7T}", cNumericFields(1), Numbers(1)));
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = Numbers(2);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow <= 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(state, format(" illegal {} = {:.7T}", cNumericFields(2), Numbers(2)));
            ErrorsFound = true;
        }

        state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = Numbers(3);
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow != AutoSize) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
            ShowContinueError(state, format(" illegal {} = {:.7T}", cNumericFields(3), Numbers(3)));
            ErrorsFound = true;
        }

        //   AirFlowControl only valid if fan opmode = ContFanCycCoil
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == 0.0) {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOnFlow;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl = iAirflowCtrlMode::UseCompressorOffFlow;
        }

        if (OANodeNums(1) > 0) {
            state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = Numbers(4);
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, format(" illegal {} = {:.7T}", cNumericFields(4), Numbers(4)));
                ErrorsFound = true;
            }

            //     only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow &&
                state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow != AutoSize) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, ".." + cNumericFields(4) + " cannot be greater than " + cNumericFields(1));
                ShowContinueError(state, format("..{} = {:.7T}", cNumericFields(1), Numbers(1)));
                ShowContinueError(state, format("..{} = {:.7T}", cNumericFields(4), Numbers(4)));
                ErrorsFound = true;
            }

            state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = Numbers(5);
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow < 0 && state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, format(" illegal {} = {:.7T}", cNumericFields(5), Numbers(5)));
                ErrorsFound = true;
            }

            //     only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow > state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow &&
                state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize && state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow != AutoSize) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, ".." + cNumericFields(5) + " cannot be greater than " + cNumericFields(2));
                ShowContinueError(state, format("..{} = {:.7T}", cNumericFields(2), Numbers(2)));
                ShowContinueError(state, format("..{} = {:.7T}", cNumericFields(5), Numbers(5)));
                ErrorsFound = true;
            }

            state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = Numbers(6);
            if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow < 0 &&
                state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow != AutoSize) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                ShowContinueError(state, format(" illegal {} = {:.7T}", cNumericFields(6), Numbers(6)));
                ErrorsFound = true;
            }
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = 0.0;
            state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = 0.0;
            state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = 0.0;
            if (!lNumericBlanks(4) || !lNumericBlanks(5) || !lNumericBlanks(6)) {
                // user entered values for OA with no outdoor air mixer name specified
                state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = 0.0;
            }
        }

        // Set the heat pump heating coil capacity
        //  Get from coil module.
        if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingWaterToAirHP) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignHeatingCapacity = GetWtoAHPCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);

            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        } else if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPSimple) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignHeatingCapacity = GetWtoAHPSimpleCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        } else if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignHeatingCapacity = GetCoilCapacityVariableSpeed(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }
        // Set the heat pump heating coil convergence
        state.dataPTHP->PTUnit(PTUnitNum).HeatConvergenceTol = 0.001;
        // Set the heat pump cooling coil capacity (Total capacity)
        //  Get from coil module.
        if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == Coil_CoolingWaterToAirHP) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity = GetWtoAHPCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        } else if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == Coil_CoolingWaterToAirHPSimple) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity = GetWtoAHPSimpleCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        } else if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(
                state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }
        // Set the heat pump cooling coil convergence
        state.dataPTHP->PTUnit(PTUnitNum).CoolConvergenceTol = 0.001;
        // Set the heatpump cycling rate
        state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour = Numbers(7);

        // Set the heat pump time constant
        state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant = Numbers(8);

        // Set the heat pump on-cycle power use fraction
        state.dataPTHP->PTUnit(PTUnitNum).OnCyclePowerFraction = Numbers(9);

        // Set the heat pump fan delay time
        state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime = Numbers(10);

        // Set the heatpump design supplemental heating capacity
        //  Get from coil module.
        if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingGasOrOtherFuel ||
            state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingElectric) {
            errFlag = false;
            state.dataPTHP->PTUnit(PTUnitNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(state, SuppHeatCoilType, SuppHeatCoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }

        // Set the max outlet temperature for supplemental heating coil
        state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat = Numbers(11);

        // Set maximum supply air temperature for supplemental heating coil
        state.dataPTHP->PTUnit(PTUnitNum).MaxOATSupHeat = Numbers(12);

        // WSHP not yet included in ASHRAE90.1 model
        state.dataPTHP->PTUnit(PTUnitNum).ControlType = iCtrlType::None;
        state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = false;
        state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil = false;
        state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil = false;

    } // End of the WaterToAirHeatPump Loop

    //***********************************************************************************

    Alphas.deallocate();
    Numbers.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting input.");
        ShowContinueError(state, "... Preceding condition causes termination.");
    }

    for (PTUnitNum = 1; PTUnitNum <= state.dataPTHP->NumPTHP; ++PTUnitNum) {
        // Setup Report variables for the Packaged Terminal Heat Psmps,   CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Total Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Total Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecPower,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecConsumption,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Fan Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Compressor Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Heat Pump Fan Availability Status",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).AvailStatus,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
    }

    for (PTUnitNum = 1 + state.dataPTHP->NumPTHP; PTUnitNum <= state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC; ++PTUnitNum) {
        // Setup Report variables for the Packaged Terminal Air Conditioners,
        // CurrentModuleObject = 'ZoneHVAC:PackagedTerminalAirConditioner'
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Total Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Total Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecPower,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecConsumption,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Fan Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Compressor Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Packaged Terminal Air Conditioner Fan Availability Status",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).AvailStatus,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
    }

    for (PTUnitNum = 1 + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC; PTUnitNum <= state.dataPTHP->NumPTUs; ++PTUnitNum) {
        // Setup Report variables for the Zone Water Source Heat Pumps, CurrentModuleObject='ZoneHVAC:WaterToAirHeatPump'
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Total Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Total Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergyRate,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecPower,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataPTHP->PTUnit(PTUnitNum).ElecConsumption,
                            "System",
                            "Sum",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Fan Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Compressor Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
        SetupOutputVariable(state,
                            "Zone Water to Air Heat Pump Fan Availability Status",
                            OutputProcessor::Unit::None,
                            state.dataPTHP->PTUnit(PTUnitNum).AvailStatus,
                            "System",
                            "Average",
                            state.dataPTHP->PTUnit(PTUnitNum).Name);
    }
    for (PTUnitNum = 1; PTUnitNum <= state.dataPTHP->NumPTUs; ++PTUnitNum) {
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
        } else {
            if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num > 0) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         state.dataPTHP->PTUnit(PTUnitNum).FanIndex);
            }
        }
    }
}

void InitPTUnit(EnergyPlusData &state,
                int const PTUnitNum,           // number of the current PTHP unit being simulated
                int const ZoneNum,             // zone number where the current PTHP unit is located
                bool const FirstHVACIteration, // TRUE on first HVAC iteration
                Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to average airflow over timestep
                Real64 &ZoneLoad               // cooling or heating needed by zone [watts]
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
    //       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
    //       MODIFIED       Bo Shen, ORNL, July 2012, added variable-speed air-source heat pump
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the packaged terminal heat pump components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;
    using DataZoneEquipment::CheckZoneEquipmentList;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
    using SteamCoils::SimulateSteamCoilComponents;
    auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
    auto &GetSteamCoilCapacity(SteamCoils::GetCoilCapacity);
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using DataPlant::TypeOf_CoilWaterSimpleHeating;
    using Fans::GetFanVolFlow;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;

    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;
    using PlantUtilities::SetComponentFlowRate;
    using VariableSpeedCoils::SimVariableSpeedCoils;
    using WaterCoils::GetCoilMaxWaterFlowRate;
    using WaterCoils::SimulateWaterCoilComponents;

    // Locals
    Real64 SupHeaterLoad;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InitPTUnit");
    static constexpr std::string_view RoutineNameSpace(" InitPTUnit");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InNode;                // inlet node number in PTHP loop
    int OutNode;               // outlet node number in PTHP loop
    int OutsideAirNode;        // outside air node number in PTHP loop
    Real64 QZnReq;             // cooling or heating needed by zone [watts]
    Real64 RhoAir;             // air density at InNode
    Real64 PartLoadFrac;       // compressor part load fraction
    Real64 CoilMaxVolFlowRate; // water or steam max volumetric water flow rate
    int Loop;
    auto &MyEnvrnFlag = state.dataPTHP->MyEnvrnFlag;
    auto &MySizeFlag = state.dataPTHP->MySizeFlag;
    auto &MyFanFlag = state.dataPTHP->MyFanFlag;
    auto &MyPlantScanFlag = state.dataPTHP->MyPlantScanFlag;
    auto &MyZoneEqFlag = state.dataPTHP->MyZoneEqFlag;
    Real64 QActual;   // actual PTAC steam heating coil load met (W)
    bool ErrorsFound; // flag returned from mining call
    Real64 QToCoolSetPt;
    Real64 QToHeatSetPt;
    Real64 NoCompOutput;
    Real64 mdot; // local temporary for mass flow rate (kg/s)
    Real64 rho;  // local for fluid density
    int SteamIndex;
    bool errFlag;
    Real64 LatentOutput;             // no load latent output (coils off) (W)
    int NumOfSpeedCooling;           // Number of speeds for cooling
    int NumOfSpeedHeating;           // Number of speeds for heating
    std::string CurrentModuleObject; // Object type for getting and error messages
    int i;                           // Loop index
    int Iter;                        // speed iteration count
    int PTObjectIndex;
    int CtrlZoneNum; // the controlled zone index (index of ZoneEquipConfig)

    InNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    OutNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    CtrlZoneNum = 0;
    // Do the one time initializations
    if (state.dataPTHP->MyOneTimeFlag) {

        MyEnvrnFlag.allocate(state.dataPTHP->NumPTUs);
        MySizeFlag.allocate(state.dataPTHP->NumPTUs);
        MyFanFlag.allocate(state.dataPTHP->NumPTUs);
        MyPlantScanFlag.allocate(state.dataPTHP->NumPTUs);
        MyZoneEqFlag.allocate(state.dataPTHP->NumPTUs);
        MyEnvrnFlag = true;
        MySizeFlag = true;
        MyFanFlag = true;
        MyPlantScanFlag = true;
        MyZoneEqFlag = true;
        state.dataPTHP->MyOneTimeFlag = false;
        state.dataPTHP->ZoneEquipmentListNotChecked = true;
    }

    if (allocated(state.dataHVACGlobal->ZoneComp)) {
        PTObjectIndex = state.dataPTHP->PTUnit(PTUnitNum).PTObjectIndex;
        if (MyZoneEqFlag(PTUnitNum)) { // initialize the name of each availability manager list and zone number
            state.dataHVACGlobal->ZoneComp(state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType).ZoneCompAvailMgrs(PTObjectIndex).AvailManagerListName =
                state.dataPTHP->PTUnit(PTUnitNum).AvailManagerListName;
            state.dataHVACGlobal->ZoneComp(state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType).ZoneCompAvailMgrs(PTObjectIndex).ZoneNum = ZoneNum;
            MyZoneEqFlag(PTUnitNum) = false;
        }
        state.dataPTHP->PTUnit(PTUnitNum).AvailStatus =
            state.dataHVACGlobal->ZoneComp(state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType).ZoneCompAvailMgrs(PTObjectIndex).AvailStatus;
    }

    if (MyPlantScanFlag(PTUnitNum) && allocated(state.dataPlnt->PlantLoop)) {
        if ((state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) ||
            (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam)) {
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {

                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                        TypeOf_CoilWaterSimpleHeating,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError(state,
                                      "Reference Unit=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                          "\", type=" + state.dataPTHP->PTUnit(PTUnitNum).UnitType);
                    ShowFatalError(state, "InitPTUnit: Program terminated for previous conditions.");
                }

                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound);

                if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow > 0.0) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum).FluidIndex,
                                           RoutineName);

                    state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound) * rho;
                }

            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {

                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                        TypeOf_CoilSteamAirHeating,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError(state,
                                      "Reference Unit=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                          "\", type=" + state.dataPTHP->PTUnit(PTUnitNum).UnitType);
                    ShowFatalError(state, "InitPTUnit: Program terminated for previous conditions.");
                }

                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ErrorsFound);

                if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow > 0.0) {
                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    state.dataPTHP->SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                    state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow =
                        GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ErrorsFound) * state.dataPTHP->SteamDensity;
                }
            }

            // fill outlet node for coil
            state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode = state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum)
                                                                        .LoopSide(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide)
                                                                        .Branch(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum)
                                                                        .Comp(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum)
                                                                        .NodeNumOut;
            state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidOutletNodeNum = state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode;
            MyPlantScanFlag(PTUnitNum) = false;

        } else if ((state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) ||
                   (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam)) {
            if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                        TypeOf_CoilWaterSimpleHeating,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitPTUnit: Program terminated for previous conditions.");
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, ErrorsFound);

                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum).FluidIndex,
                                           RoutineName);
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, ErrorsFound) * rho;
                }
            } else if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                        TypeOf_CoilSteamAirHeating,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitPTUnit: Program terminated for previous conditions.");
                }
                state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, ErrorsFound);
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow > 0.0) {
                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    state.dataPTHP->SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                    state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, ErrorsFound) *
                        state.dataPTHP->SteamDensity;
                }
            }
            // fill outlet node for coil
            state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode = state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum)
                                                                        .LoopSide(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide)
                                                                        .Branch(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum)
                                                                        .Comp(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum)
                                                                        .NodeNumOut;
            MyPlantScanFlag(PTUnitNum) = false;
        } else { // pthp not connected to plant
            MyPlantScanFlag(PTUnitNum) = false;
        }
    } else if (MyPlantScanFlag(PTUnitNum) && !state.dataGlobal->AnyPlantInModel) {
        MyPlantScanFlag(PTUnitNum) = false;
    }

    if (state.dataPTHP->ZoneEquipmentListNotChecked) {
        if (state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataPTHP->ZoneEquipmentListNotChecked = false;
            for (Loop = 1; Loop <= state.dataPTHP->NumPTUs; ++Loop) {
                if (CheckZoneEquipmentList(state, state.dataPTHP->PTUnit(Loop).UnitType, state.dataPTHP->PTUnit(Loop).Name, CtrlZoneNum)) {
                    // save the ZoneEquipConfig index for this unit
                    state.dataPTHP->PTUnit(Loop).ControlZoneNum = CtrlZoneNum;
                    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                        for (int ZoneExhNum = 1; ZoneExhNum <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                             ++ZoneExhNum) {
                            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) !=
                                state.dataPTHP->PTUnit(Loop).AirInNode)
                                continue;
                            // Find the controlled zone number for the specified thermostat location
                            state.dataPTHP->PTUnit(Loop).NodeNumOfControlledZone = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                            break;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    "InitPTHP: Packaged Terminal Unit=[" + state.dataPTHP->PTUnit(Loop).UnitType + ',' +
                                        state.dataPTHP->PTUnit(Loop).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
                }
            }
        }
    }

    if (!state.dataGlobal->SysSizingCalc && MySizeFlag(PTUnitNum)) {
        SizePTUnit(state, PTUnitNum);
        MySizeFlag(PTUnitNum) = false;

        RhoAir = state.dataEnvrn->StdRhoAir;
        state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;

        if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel && state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling == 0) {

            SimVariableSpeedCoils(state,
                                  "",
                                  state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                  0,
                                  state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                  state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                  state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                  0,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  0.0); // conduct the sizing operation in the VS WSHP
            state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling =
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum).NumOfSpeeds;

            for (Iter = 1; Iter <= state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling; ++Iter) {
                state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(Iter) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum).MSRatedAirVolFlowRate(Iter) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum)
                        .MSRatedAirVolFlowRate(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling);
                state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(Iter) =
                    state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow * state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(Iter);
                state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(Iter) =
                    state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow * state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(Iter);
            }

            if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit ||
                state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {

                SimVariableSpeedCoils(state,
                                      "",
                                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                      0,
                                      state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                      state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                      state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                      0,
                                      0.0,
                                      1,
                                      0.0,
                                      0.0,
                                      0.0,
                                      0.0); // conduct the sizing operation in the VS WSHP

                state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating =
                    state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum).NumOfSpeeds;

                for (Iter = 1; Iter <= state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating; ++Iter) {
                    state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(Iter) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum).MSRatedAirVolFlowRate(Iter) /
                        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum)
                            .MSRatedAirVolFlowRate(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating);
                    state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(Iter) =
                        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow * state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(Iter);
                    state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(Iter) =
                        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow * state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(Iter);
                }
            }
            // intialize idle flow

            if (state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating > 0) {
                state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate =
                    min(state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(1), state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1));
                state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio =
                    min(state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(1), state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(1));
                state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate =
                    min(state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(1), state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(1));
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1);
                state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(1);
                state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate = state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(1);
            }

            if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
                state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate;
                state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate;
                state.dataPTHP->PTUnit(PTUnitNum).NoHeatCoolSpeedRatio = state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio;
            }
        }
    }

    if (MyFanFlag(PTUnitNum)) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate != AutoSize) {
            if (state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate > 0.0) {
                state.dataPTHP->PTUnit(PTUnitNum).HeatingSpeedRatio =
                    state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate;
                state.dataPTHP->PTUnit(PTUnitNum).CoolingSpeedRatio =
                    state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate;
                state.dataPTHP->PTUnit(PTUnitNum).NoHeatCoolSpeedRatio =
                    state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate;
            }
            MyFanFlag(PTUnitNum) = false;
        } else {
            if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate =
                    state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
            } else {
                GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, state.dataPTHP->PTUnit(PTUnitNum).ActualFanVolFlowRate);
            }
        }
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr > 0) {
        if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).FanSchedPtr) == 0.0) {
            state.dataPTHP->PTUnit(PTUnitNum).OpMode = CycFanCycCoil;
        } else {
            state.dataPTHP->PTUnit(PTUnitNum).OpMode = ContFanCycCoil;
        }
    }

    QZnReq = ZoneLoad;

    // Original thermostat control logic
    // Sets initial control based on load - works only for cycling fan systems
    // Constant fan systems will further test the load including the impacts of OA
    // OA can change the load to be met by the PTUnit (this is done later in Init)
    if (QZnReq > SmallLoad) {
        state.dataPTHP->HeatingLoad = true;
        state.dataPTHP->CoolingLoad = false;
    } else if (std::abs(QZnReq) > SmallLoad) {
        state.dataPTHP->HeatingLoad = false;
        state.dataPTHP->CoolingLoad = true;
    } else {
        state.dataPTHP->HeatingLoad = false;
        state.dataPTHP->CoolingLoad = false;
    }

    // Initialize the operating PLR (turn coils on if needed, otherwise turn coils off)
    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) > 0.0) {
        if (state.dataPTHP->HeatingLoad || state.dataPTHP->CoolingLoad) {
            PartLoadFrac = 1.0;
        } else {
            PartLoadFrac = 0.0;
        }
    } else {
        PartLoadFrac = 0.0;
    }
    state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = PartLoadFrac; // also set fan PLR variable for SZVAV models

    if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel && state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling > 0 &&
        !MySizeFlag(PTUnitNum)) { // BoS, variable-speed water source hp
        // PTUnit(PTUnitNum)%IdleMassFlowRate = RhoAir*PTUnit(PTUnitNum)%IdleVolumeAirRate
        NumOfSpeedCooling = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling;
        NumOfSpeedHeating = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating;
        // IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
        if (state.dataPTHP->PTUnit(PTUnitNum).CheckFanFlow) {
            CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
            if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow =
                    state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->designAirVolFlowRate;
            } else {
                GetFanVolFlow(state, state.dataPTHP->PTUnit(PTUnitNum).FanIndex, state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow);
            }

            if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow != AutoSize) {
                //     Check fan versus system supply air flow rates
                if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow + 1e-10 < state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(NumOfSpeedCooling)) {
                    ShowWarningError(state,
                                     format("{} - air flow rate = {:.7T} in fan object is less than the MSHP system air flow rate when cooling "
                                            "is required ({:.7T}).",
                                            CurrentModuleObject,
                                            state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow,
                                            state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(NumOfSpeedCooling)));
                    ShowContinueError(
                        state, " The MSHP system flow rate when cooling is required is reset to the fan flow rate and the simulation continues.");
                    ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(NumOfSpeedCooling) = state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                    // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                    for (i = NumOfSpeedCooling - 1; i >= 1; --i) {
                        if (state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i) > state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i + 1)) {
                            ShowContinueError(state,
                                              format(" The MSHP system flow rate when cooling is required is reset to the flow rate at higher "
                                                     "speed and the simulation continues at Speed{}.",
                                                     i));
                            ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                            state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i) = state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i + 1);
                        }
                    }
                }

                if (state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating > 0) {
                    if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow + 1e-10 <
                        state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(NumOfSpeedHeating)) {
                        ShowWarningError(state,
                                         format("{} - air flow rate = {:.7T} in fan object is less than the MSHP system air flow rate when "
                                                "heating is required ({:.7T}).",
                                                CurrentModuleObject,
                                                state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow,
                                                state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(NumOfSpeedHeating)));
                        ShowContinueError(
                            state, " The MSHP system flow rate when heating is required is reset to the fan flow rate and the simulation continues.");
                        ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                        state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(NumOfSpeedHeating) = state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                        for (i = NumOfSpeedHeating - 1; i >= 1; --i) {
                            if (state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i) >
                                state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i + 1)) {
                                ShowContinueError(state,
                                                  format(" The MSHP system flow rate when heating is required is reset to the flow rate at "
                                                         "higher speed and the simulation continues at Speed{}.",
                                                         i));
                                ShowContinueError(state, " Occurs in " + CurrentModuleObject + " system = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                                state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i) = state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i + 1);
                            }
                        }
                    }
                }

                if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow < state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate &&
                    state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate != 0.0) {
                    ShowWarningError(state,
                                     format("{} - air flow rate = {:.7T} in fan object is less than the MSHP system air flow rate when no "
                                            "heating or cooling is needed ({:.7T}).",
                                            CurrentModuleObject,
                                            state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow,
                                            state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate));
                    ShowContinueError(state,
                                      " The MSHP system flow rate when no heating or cooling is needed is reset to the fan flow rate and the "
                                      "simulation continues.");
                    ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
                    state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate = state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                }
                // set the mass flow rates from the reset volume flow rates
                for (i = 1; i <= NumOfSpeedCooling; ++i) {
                    state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(i) = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i);
                    if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow > 0.0) {
                        state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(i) =
                            state.dataPTHP->PTUnit(PTUnitNum).CoolVolumeFlowRate(i) / state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                    }
                }
                for (i = 1; i <= NumOfSpeedHeating; ++i) {
                    state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(i) = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i);
                    if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow > 0.0) {
                        state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(i) =
                            state.dataPTHP->PTUnit(PTUnitNum).HeatVolumeFlowRate(i) / state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                    }
                }
                state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate;
                if (state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow > 0.0) {
                    state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio =
                        state.dataPTHP->PTUnit(PTUnitNum).IdleVolumeAirRate / state.dataPTHP->PTUnit(PTUnitNum).FanVolFlow;
                }
                // set the node max and min mass flow rates based on reset volume flow rates
                if (state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating > 0) {
                    state.dataLoopNodes->Node(InNode).MassFlowRateMax = max(state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(NumOfSpeedCooling),
                                                                            state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(NumOfSpeedHeating));
                    state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail =
                        max(state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(NumOfSpeedCooling),
                            state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(NumOfSpeedHeating));
                } else {
                    state.dataLoopNodes->Node(InNode).MassFlowRateMax = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(NumOfSpeedCooling);
                    state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(NumOfSpeedCooling);
                }

                state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(OutNode) = state.dataLoopNodes->Node(InNode);
            }
        }

        state.dataPTHP->PTUnit(PTUnitNum).CheckFanFlow = false;

        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);

    } else {
        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(PTUnitNum)) {
        InNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
        OutNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
        OutsideAirNode = state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode;
        RhoAir = state.dataEnvrn->StdRhoAir;
        // set the mass flow rates from the input volume flow rates
        state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow;
        state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirMassFlow = RhoAir * state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow;
        // set the node max and min mass flow rates
        // outside air mixer is optional, check that node num > 0
        if (OutsideAirNode > 0) {
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax =
                max(state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirMassFlow, state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirMassFlow);
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
        }
        state.dataLoopNodes->Node(OutNode).MassFlowRateMax =
            max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow);
        state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMinAvail = 0.0;
        state.dataLoopNodes->Node(InNode).MassFlowRateMax =
            max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow);
        state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
        state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = 0.0;
        if (state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode > 0) {
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode).MassFlowRateMinAvail = 0.0;
        }
        MyEnvrnFlag(PTUnitNum) = false;
        state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::HeatingMode;

        //   set fluid-side hardware limits
        if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode > 0) {
            // If coil max fluid flow rate is autosized, simulate once in order to mine max flow rate
            if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow == AutoSize) {
                if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex);
                    CoilMaxVolFlowRate =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound);
                    if (CoilMaxVolFlowRate != AutoSize) {
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum).FluidName,
                                               DataGlobalConstants::HWInitConvTemp,
                                               state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum).FluidIndex,
                                               RoutineNameSpace);
                        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                    }
                } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                                1.0,
                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                    CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex, ErrorsFound);
                    if (CoilMaxVolFlowRate != AutoSize) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        state.dataPTHP->SteamDensity =
                            GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                        state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * state.dataPTHP->SteamDensity;
                    }
                }
            }
            InitComponentNodes(state,
                               0.0,
                               state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow,
                               state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                               state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                               state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                               state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                               state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                               state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode > 0) {
            // If coil max fluid flow rate is autosized, simulate once in order to mine max flow rate
            if (state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow == AutoSize) {
                if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex);
                    CoilMaxVolFlowRate =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, ErrorsFound);
                    if (CoilMaxVolFlowRate != AutoSize) {
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum).FluidName,
                                               DataGlobalConstants::HWInitConvTemp,
                                               state.dataPlnt->PlantLoop(state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum).FluidIndex,
                                               RoutineNameSpace);
                        state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                    }
                } else if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                1.0,
                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                    CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex, ErrorsFound);

                    if (CoilMaxVolFlowRate != AutoSize) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        state.dataPTHP->SteamDensity =
                            GetSatDensityRefrig(state, fluidNameSteam, state.dataPTHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                        state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * state.dataPTHP->SteamDensity;
                    }
                }
            }
            InitComponentNodes(state,
                               0.0,
                               state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow,
                               state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                               state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                               state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                               state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                               state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                               state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
        }
    } // end one time inits

    if (!state.dataGlobal->BeginEnvrnFlag) {
        MyEnvrnFlag(PTUnitNum) = true;
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap == AutoSize) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingGasOrOtherFuel) {
            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap = GetHeatingCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound);
        } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingElectric) {
            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap = GetHeatingCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound);
        }
    }

    state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = false; // flag used to envoke ASHRAE 90.1 model calculations
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        if (state.dataPTHP->CoolingLoad) {
            if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil) state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
        } else if (state.dataPTHP->HeatingLoad) {
            if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil) state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
        } else if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil || state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil) {
            state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
        }
    }

    // Constant fan systems are tested for ventilation load to determine if load to be met changes.

    if ((state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil || state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) &&
        GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) > 0.0 &&
        ((GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr) > 0.0 || state.dataHVACGlobal->ZoneCompTurnFansOn) &&
         !state.dataHVACGlobal->ZoneCompTurnFansOff)) {

        if (state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel)
            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 0.0; // check unit output at low fan speed
        SupHeaterLoad = 0.0;
        if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
            CalcVarSpeedHeatPump(state,
                                 PTUnitNum,
                                 ZoneNum,
                                 FirstHVACIteration,
                                 Off,
                                 1,
                                 0.0,
                                 0.0,
                                 NoCompOutput,
                                 LatentOutput,
                                 QZnReq,
                                 0.0,
                                 OnOffAirFlowRatio,
                                 SupHeaterLoad,
                                 false);
        } else {
            CalcPTUnit(state, PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false);
        }

        QToCoolSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
        QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;

        //   If the PTUnit has a net cooling capacity (NoCompOutput < 0) and
        //   the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
        if (NoCompOutput < 0.0 && QToHeatSetPt <= 0.0) {
            if (NoCompOutput - QToHeatSetPt < -SmallLoad) {
                //  If the net cooling capacity overshoots the heating setpoint, change mode
                QZnReq = QToHeatSetPt;
                state.dataPTHP->CoolingLoad = false;
                //       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
                if (state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleCoolingSetPoint ||
                    state.dataHeatBalFanSys->TempControlType(ZoneNum) == 0) {
                    state.dataPTHP->HeatingLoad = false;
                } else {
                    state.dataPTHP->HeatingLoad = true;
                }
                PartLoadFrac = 1.0;
                if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    CalcVarSpeedHeatPump(state,
                                         PTUnitNum,
                                         ZoneNum,
                                         FirstHVACIteration,
                                         Off,
                                         1,
                                         0.0,
                                         0.0,
                                         NoCompOutput,
                                         LatentOutput,
                                         QZnReq,
                                         0.0,
                                         OnOffAirFlowRatio,
                                         SupHeaterLoad,
                                         false);
                } else {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false);
                }
                if (NoCompOutput > QToHeatSetPt) {
                    //         If changing operating mode (flow rates) does not overshoot heating setpoint, turn off coil
                    QZnReq = 0.0;
                    state.dataPTHP->HeatingLoad = false;
                    PartLoadFrac = 0.0;
                    if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    } else {
                        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    }
                }
            } else if (NoCompOutput < QZnReq) {
                //       If the net cooling capacity meets the zone cooling load but does not overshoot heating set point, turn off coil
                QZnReq = 0.0;
                state.dataPTHP->CoolingLoad = false;
                PartLoadFrac = 0.0;
                if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                } else {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                }
            }
        }
        //   If the furnace has a net heating capacity and the zone temp is below the Tstat cooling setpoint
        if (NoCompOutput > 0.0 && QToCoolSetPt > 0.0) {
            if (NoCompOutput > QToCoolSetPt) {
                QZnReq = QToCoolSetPt;
                //       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
                if (state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleHeatingSetPoint ||
                    state.dataHeatBalFanSys->TempControlType(ZoneNum) == 0) {
                    state.dataPTHP->CoolingLoad = false;
                } else {
                    state.dataPTHP->CoolingLoad = true;
                }
                state.dataPTHP->HeatingLoad = false;
                PartLoadFrac = 1.0;
                if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    CalcVarSpeedHeatPump(state,
                                         PTUnitNum,
                                         ZoneNum,
                                         FirstHVACIteration,
                                         Off,
                                         1,
                                         0.0,
                                         0.0,
                                         NoCompOutput,
                                         LatentOutput,
                                         QZnReq,
                                         0.0,
                                         OnOffAirFlowRatio,
                                         SupHeaterLoad,
                                         false);
                } else {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false);
                }

                if (NoCompOutput < QToCoolSetPt) {
                    //         If changing operating mode (flow rates) does not overshoot cooling setpoint, turn off coil
                    QZnReq = 0.0;
                    state.dataPTHP->CoolingLoad = false;
                    PartLoadFrac = 0.0;
                    if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    } else {
                        SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                    }
                }
            } else if (NoCompOutput > QZnReq) {
                //       If the net heating capacity meets the zone heating load but does not overshoot, turn off coil
                QZnReq = 0.0;
                state.dataPTHP->HeatingLoad = false;
                PartLoadFrac = 0.0;
                if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                } else {
                    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
                }
            }
        }
        ZoneLoad = QZnReq;
        // Check SZVAV model after fan operation is tested. Constant fan operating mode is required.
        state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = false; // flag used to envoke ASHRAE 90.1 model calculations
        if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
            if (state.dataPTHP->CoolingLoad) {
                if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil) state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
            } else if (state.dataPTHP->HeatingLoad) {
                if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil) state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
            } else if (state.dataPTHP->PTUnit(PTUnitNum).validASHRAECoolCoil || state.dataPTHP->PTUnit(PTUnitNum).validASHRAEHeatCoil) {
                state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel = true;
            }
        }
    }

    // get operating capacity of water and steam coil (dependent on entering water/steam temperature)
    if (FirstHVACIteration && PartLoadFrac > 0.0) {

        if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {

            //     set water-side mass flow rates
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilInletNodeNum).MassFlowRate = state.dataPTHP->CompOnMassFlow;

            mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow;

            SetComponentFlowRate(state,
                                 mdot,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

            //     simulate water coil to find operating capacity
            SimulateWaterCoilComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                        QActual);
            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap = QActual;

        } // from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN

        if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {

            //     set air-side and steam-side mass flow rates
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).HeatCoilInletNodeNum).MassFlowRate = state.dataPTHP->CompOnMassFlow;

            mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow;
            SetComponentFlowRate(state,
                                 mdot,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

            //     simulate steam coil to find operating capacity
            SimulateSteamCoilComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                        1.0,
                                        QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap = GetSteamCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, ErrorsFound);

        } // from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN

        if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingWater) {

            //     set air-side and steam-side mass flow rates
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode).MassFlowRate = state.dataPTHP->CompOnMassFlow;
            mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
            SetComponentFlowRate(state,
                                 mdot,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);

            //     simulate water coil to find operating capacity
            SimulateWaterCoilComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                        QActual);
            state.dataPTHP->PTUnit(PTUnitNum).SupHeatCoilCap = QActual;

        } // from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {

            //     set air-side and steam-side mass flow rates
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).SupCoilAirInletNode).MassFlowRate = state.dataPTHP->CompOnMassFlow;
            mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
            SetComponentFlowRate(state,
                                 mdot,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                 state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);

            //     simulate steam coil to find operating capacity
            SimulateSteamCoilComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                        1.0,
                                        QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
            state.dataPTHP->PTUnit(PTUnitNum).SupHeatCoilCap = GetSteamCoilCapacity(
                state, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName, ErrorsFound);

        } // from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
    }     // from IF(FirstHVACIteration .AND. PartLoadFrac > 0.0) THEN

    // Initialize each time step
    //        PTUnit( PTUnitNum ).CoolCoilWaterFlowRatio = 0.0; // water cooling coils are not allowed in PTUnit model
    state.dataPTHP->PTUnit(PTUnitNum).HeatCoilWaterFlowRatio = 0.0;

    SetAverageAirFlow(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
}

void SetOnOffMassFlowRate(EnergyPlusData &state,
                          int const PTUnitNum,       // number of the current PTHP unit being simulated
                          Real64 const PartLoadFrac, // coil operating part-load ratio
                          Real64 &OnOffAirFlowRatio  // ratio of coil on to coil off air flow rate
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   November 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the operating flow rates.

    // METHODOLOGY EMPLOYED:
    // Set cooling or heating and no cooling or heating flow rate.
    // Set mass flow rate using PLR and call to Subroutine SetAverageAirFlow.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // na

    // Set the operating air mass flow rate
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        // constant fan mode
        if (state.dataPTHP->HeatingLoad) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).HeatingSpeedRatio;
            state.dataPTHP->OACompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirMassFlow;
            state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::HeatingMode;
        } else if (state.dataPTHP->CoolingLoad) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).CoolingSpeedRatio;
            state.dataPTHP->OACompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirMassFlow;
            state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::CoolingMode;
        } else {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).NoHeatCoolSpeedRatio;
            state.dataPTHP->OACompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirMassFlow;
        }

        if (state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl == iAirflowCtrlMode::UseCompressorOnFlow) {
            if (state.dataPTHP->PTUnit(PTUnitNum).LastMode == iCompMode::HeatingMode) {
                state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow;
                state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).HeatingSpeedRatio;
                state.dataPTHP->OACompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirMassFlow;
            } else {
                state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow;
                state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).CoolingSpeedRatio;
                state.dataPTHP->OACompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirMassFlow;
            }
        } else {
            state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow;
            state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).NoHeatCoolSpeedRatio;
            state.dataPTHP->OACompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirMassFlow;
        }
    } else {
        // cycling fan mode
        if (state.dataPTHP->HeatingLoad) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).HeatingSpeedRatio;
            state.dataPTHP->OACompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirMassFlow;
        } else if (state.dataPTHP->CoolingLoad) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).CoolingSpeedRatio;
            state.dataPTHP->OACompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirMassFlow;
        } else {
            state.dataPTHP->CompOnMassFlow = 0.0;
            state.dataPTHP->CompOnFlowRatio = 0.0;
            state.dataPTHP->OACompOnMassFlow = 0.0;
        }
        state.dataPTHP->CompOffMassFlow = 0.0;
        state.dataPTHP->CompOffFlowRatio = 0.0;
        state.dataPTHP->OACompOffMassFlow = 0.0;
    }

    SetAverageAirFlow(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);
}

void SizePTUnit(EnergyPlusData &state, int const PTUnitNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
    //                      August 2013 Daeho Kang, add component sizing table entries
    //                      July 2014, B. Nigusse, added scalable sizing
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing packaged terminal heat pump components for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone sizing arrays. ParentCoolAirFlowSizing and ParentHeatAirFlowSizing
    // arrays are used to pass volumetric flow rates to child objects when zone sizing array values are overridden.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataSizing;
    using DataHVACGlobals::CoolingCapacitySizing;
    using DataHVACGlobals::HeatingAirflowSizing;
    using DataHVACGlobals::HeatingCapacitySizing;
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;

    using VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed;
    using VariableSpeedCoils::SimVariableSpeedCoils;
    using WaterCoils::SetCoilDesFlow;

    // Locals
    bool IsAutoSize;                    // Indicator to autosize
    Real64 MaxCoolAirVolFlowDes;        // Autosized cooling air flow for reporting
    Real64 MaxCoolAirVolFlowUser;       // Hardsizedcooling air flow for reporting
    Real64 MaxHeatAirVolFlowDes;        // Autosized heating air flow for reporting
    Real64 MaxHeatAirVolFlowUser;       // Hardsized heating air flow for reporting
    Real64 MaxNoCoolHeatAirVolFlowDes;  // Autosized maximum air flow when unconditioned for reporting
    Real64 MaxNoCoolHeatAirVolFlowUser; // Hardsized maximum air flow when unconditioned for reporting
    Real64 CoolOutAirVolFlowDes;        // Autosized cooling outdoor air flow for reporting
    Real64 CoolOutAirVolFlowUser;       // Hardsized cooling outdoor air flow for reporting
    Real64 HeatOutAirVolFlowDes;        // Autosized heating outdoor air flow for reporting
    Real64 HeatOutAirVolFlowUser;       // Hardsized heating outdoor air flow for reporting
    Real64 NoCoolHeatOutAirVolFlowDes;  // Autosized outdoor air flow when unconditioned for reporting
    Real64 NoCoolHeatOutAirVolFlowUser; // Hardsized outdoor air flow when unconditioned for reporting
    Real64 MaxSATSupHeatDes;            // Autosized supply air temperature of supplemental heater for reporting
    Real64 MaxSATSupHeatUser;           // Hardsized supply air temperature of supplemental heater for reporting

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizePTUnit: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound;
    bool SizingDesRunThisZone; // true if a particular zone had a Sizing:Zone object and zone sizing was done
    std::string CompName;      // component name
    std::string CompType;      // component type
    std::string SizingString;  // input field sizing description (e.g., Nominal Capacity)
    Real64 TempSize;           // autosized value of coil input field
    int FieldNum = 2;          // IDD numeric field number where input field description is found
    int SizingMethod;        // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                             // HeatingCapacitySizing, etc.)
    bool PrintFlag;          // TRUE when sizing information is reported in the eio file
    int zoneHVACIndex;       // index of zoneHVAC equipment sizing specification
    int SAFMethod(0);        // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                             // FractionOfAutosizedHeatingAirflow ...)
    int CapSizingMethod(0);  // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                             // FractionOfAutosizedHeatingCapacity )
    Real64 minNoLoadFlow(0); // used for sizing MaxNoCoolHeatVolFlow for SingleZoneVAV method

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    ErrorsFound = false;
    IsAutoSize = false;
    MaxCoolAirVolFlowDes = 0.0;
    MaxCoolAirVolFlowUser = 0.0;
    MaxHeatAirVolFlowDes = 0.0;
    MaxHeatAirVolFlowUser = 0.0;
    MaxNoCoolHeatAirVolFlowDes = 0.0;
    MaxNoCoolHeatAirVolFlowUser = 0.0;
    CoolOutAirVolFlowDes = 0.0;
    CoolOutAirVolFlowUser = 0.0;
    HeatOutAirVolFlowDes = 0.0;
    HeatOutAirVolFlowUser = 0.0;
    NoCoolHeatOutAirVolFlowDes = 0.0;
    NoCoolHeatOutAirVolFlowUser = 0.0;
    MaxSATSupHeatDes = 0.0;
    MaxSATSupHeatUser = 0.0;

    state.dataSize->DataFracOfAutosizedCoolingAirflow = 1.0;
    state.dataSize->DataFracOfAutosizedHeatingAirflow = 1.0;
    state.dataSize->DataFracOfAutosizedCoolingCapacity = 1.0;
    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;

    state.dataSize->DataScalableSizingON = false;
    state.dataSize->DataScalableCapSizingON = false;
    CompType = state.dataPTHP->PTUnit(PTUnitNum).UnitType;
    CompName = state.dataPTHP->PTUnit(PTUnitNum).Name;
    state.dataSize->DataZoneNumber = state.dataPTHP->PTUnit(PTUnitNum).ZonePtr;
    if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataSize->DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
    } else {
        state.dataSize->DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
    }
    state.dataSize->DataFanIndex = state.dataPTHP->PTUnit(PTUnitNum).FanIndex;
    if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {
        state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;
    } else if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == DrawThru) {
        state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneDrawThru;
    }

    if (state.dataSize->CurZoneEqNum > 0) {
        if (state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex > 0) {
            // initialize OA flow for sizing other inputs (e.g., capacity)
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow == AutoSize) {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
            } else {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize) {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow =
                    max(ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow, state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow);
            }

            if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {          // set up ATMixer conditions for scalable capacity sizing
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
                SingleDuct::setATMixerSizingProperties(state,
                                                       state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex,
                                                       state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum,
                                                       state.dataSize->CurZoneEqNum);
            }

            zoneHVACIndex = state.dataPTHP->PTUnit(PTUnitNum).HVACSizingIndex;
            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            PrintFlag = true;
            SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
            if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                SAFMethod == FractionOfAutosizedCoolingAirflow) {
                if (SAFMethod == SupplyAirFlowRate) {
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    }
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                } else if (SAFMethod == FlowPerFloorArea) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    state.dataSize->DataScalableSizingON = true;
                } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                    state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    TempSize = AutoSize;
                    state.dataSize->DataScalableSizingON = true;
                } else {
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                }
                bool errorsFound = false;
                CoolingAirFlowSizer sizingCoolingAirFlow;
                std::string stringOverride = "Cooling Supply Air Flow Rate [m3/s]";
                if (state.dataGlobal->isEpJSON) stringOverride = "cooling_supply_air_flow_rate [m3/s]";
                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
            } else if (SAFMethod == FlowPerCoolingCapacity) {
                SizingMethod = CoolingCapacitySizing;
                TempSize = AutoSize;
                PrintFlag = false;
                state.dataSize->DataScalableSizingON = true;
                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod == FractionOfAutosizedCoolingCapacity) {
                    state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                }
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.overrideSizingString(SizingString);
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataSize->DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                PrintFlag = true;
                TempSize = AutoSize;
                bool errorsFound = false;
                CoolingAirFlowSizer sizingCoolingAirFlow;
                std::string stringOverride = "Cooling Supply Air Flow Rate [m3/s]";
                if (state.dataGlobal->isEpJSON) stringOverride = "cooling_supply_air_flow_rate [m3/s]";
                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
            }

            SizingMethod = HeatingAirflowSizing;
            FieldNum = 2; // N2, \field Supply Air Flow Rate During Heating Operation
            PrintFlag = true;
            SizingString = state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames(FieldNum) + " [m3/s]";
            SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
            if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                SAFMethod == FractionOfAutosizedHeatingAirflow) {
                if (SAFMethod == SupplyAirFlowRate) {
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    }
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                } else if (SAFMethod == FlowPerFloorArea) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow *
                                                                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    state.dataSize->DataScalableSizingON = true;
                } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                    state.dataSize->DataFracOfAutosizedHeatingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                    TempSize = AutoSize;
                    state.dataSize->DataScalableSizingON = true;
                } else {
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                }
                bool errorsFound = false;
                HeatingAirFlowSizer sizingHeatingAirFlow;
                sizingHeatingAirFlow.overrideSizingString(SizingString);
                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
            } else if (SAFMethod == FlowPerHeatingCapacity) {
                SizingMethod = HeatingCapacitySizing;
                TempSize = AutoSize;
                PrintFlag = false;
                state.dataSize->DataScalableSizingON = true;
                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow;
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                TempSize = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                }
                state.dataSize->DataAutosizedHeatingCapacity = TempSize;
                state.dataSize->DataFlowPerHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                SizingMethod = HeatingAirflowSizing;
                PrintFlag = true;
                TempSize = AutoSize;
                errorsFound = false;
                HeatingAirFlowSizer sizingHeatingAirFlow;
                sizingHeatingAirFlow.overrideSizingString(SizingString);
                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
            }

            FieldNum = 3; // N3, \field Supply Air Flow Rate When No Cooling or Heating is Needed
            PrintFlag = true;
            SizingString = state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames(FieldNum) + " [m3/s]";
            SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).NoCoolHeatSAFMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
            if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                SAFMethod == FractionOfAutosizedCoolingAirflow || SAFMethod == FractionOfAutosizedHeatingAirflow) {
                if (SAFMethod == SupplyAirFlowRate) {
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow > 0.0) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    }
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow;
                } else if (SAFMethod == FlowPerFloorArea) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow *
                                                                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    state.dataSize->DataScalableSizingON = true;
                } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                    state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow;
                    TempSize = AutoSize;
                    state.dataSize->DataScalableSizingON = true;
                } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                    state.dataSize->DataFracOfAutosizedHeatingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow;
                    TempSize = AutoSize;
                    state.dataSize->DataScalableSizingON = true;
                } else {
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxNoCoolHeatAirVolFlow;
                }
                bool errorsFound = false;
                SystemAirFlowSizer sizerSystemAirFlow;
                sizerSystemAirFlow.overrideSizingString(SizingString);
                // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = sizerSystemAirFlow.size(state, TempSize, errorsFound);
            }

            // initialize capacity sizing variables: cooling
            SizingMethod = CoolingCapacitySizing;
            CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                if (CapSizingMethod == CoolingDesignCapacity) {
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                    }
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity *
                                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    state.dataSize->DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                    state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                    state.dataSize->DataScalableCapSizingON = true;
                }
            }

            // initialize capacity sizing variables: heating
            SizingMethod = HeatingCapacitySizing;
            CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                if (CapSizingMethod == HeatingDesignCapacity) {
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                    }
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity *
                                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                }
            }
        } else {
            // no scalable sizing method has been specified. Sizing proceeds using the method
            // specified in the zoneHVAC object

            // initialize OA flow for sizing other inputs (e.g., capacity)
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow == AutoSize) {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
            } else {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow != AutoSize) {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow =
                    max(ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow, state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow);
            }

            PrintFlag = false;
            TempSize = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
            if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel) {
                SimVariableSpeedCoils(state,
                                      std::string(),
                                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                      state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                      state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                      state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                      state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                      1,
                                      0.0,
                                      1,
                                      0.0,
                                      0.0,
                                      0.0,
                                      1.0);
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirFlow = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow = GetCoilAirFlowRateVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName, ErrorsFound);
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow;
                TempSize = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
            }
            PrintFlag = true;
            bool errorsFound = false;
            CoolingAirFlowSizer sizingCoolingAirFlow;
            std::string stringOverride = "Cooling Supply Air Flow Rate [m3/s]";
            if (state.dataGlobal->isEpJSON) stringOverride = "cooling_supply_air_flow_rate [m3/s]";
            sizingCoolingAirFlow.overrideSizingString(stringOverride);
            // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow = sizingCoolingAirFlow.size(state, TempSize, errorsFound);

            ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirFlow = false;

            PrintFlag = false;
            SizingMethod = HeatingAirflowSizing;
            FieldNum = 2; // N2, \field Supply Air Flow Rate During Heating Operation
            SizingString = state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;
            if (state.dataPTHP->PTUnit(PTUnitNum).useVSCoilModel && state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum > 0) {
                SimVariableSpeedCoils(state,
                                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                                      state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                      state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                      state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                      state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                      state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                      1,
                                      0.0,
                                      1,
                                      0.0,
                                      0.0,
                                      0.0,
                                      1.0);
                ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirFlow = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirVolFlow = GetCoilAirFlowRateVariableSpeed(
                    state, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType, state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName, ErrorsFound);
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow =
                    max(ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow, ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirVolFlow);
                TempSize = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;
            }
            PrintFlag = true;
            errorsFound = false;
            HeatingAirFlowSizer sizingHeatingAirFlow;
            sizingHeatingAirFlow.overrideSizingString(SizingString);
            // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirFlow = false;

            if (state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType == PkgTermHPAirToAir_Num ||
                state.dataPTHP->PTUnit(PTUnitNum).ZoneEquipType == PkgTermACAirToAir_Num) {
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow == AutoSize &&
                    state.dataPTHP->PTUnit(PTUnitNum).ControlType == iCtrlType::CCM_ASHRAE) {
                    SizingMethod = AutoCalculateSizing;
                    state.dataSize->DataConstantUsedForSizing =
                        max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow);
                    minNoLoadFlow = 0.6667;
                    if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow >= state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow) {
                        state.dataSize->DataFractionUsedForSizing =
                            min(minNoLoadFlow,
                                (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow) - 0.01);
                    } else {
                        state.dataSize->DataFractionUsedForSizing =
                            min(minNoLoadFlow,
                                (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow) - 0.01);
                    }
                }
            }
            FieldNum = 3; // N3, \field Supply Air Flow Rate When No Cooling or Heating is Needed
            SizingString = state.dataPTHP->PTUnitUNumericFields(PTUnitNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow;
            errorsFound = false;
            SystemAirFlowSizer sizerSystemAirFlow;
            sizerSystemAirFlow.overrideSizingString(SizingString);
            // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow = sizerSystemAirFlow.size(state, TempSize, errorsFound);
            state.dataSize->DataConstantUsedForSizing = 0.0;
            state.dataSize->DataFractionUsedForSizing = 0.0;
        }
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {          // set up ATMixer conditions for use in component sizing
        ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
        SingleDuct::setATMixerSizingProperties(
            state, state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex, state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum, state.dataSize->CurZoneEqNum);
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow > 0.0) {
        state.dataPTHP->PTUnit(PTUnitNum).LowSpeedCoolFanRatio =
            state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
    }
    if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow > 0.0) {
        state.dataPTHP->PTUnit(PTUnitNum).LowSpeedHeatFanRatio =
            state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow / state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;
    }

    IsAutoSize = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
        if (!IsAutoSize && !SizingDesRunThisZone) { // Simulation continue
            if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]",
                                             state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType, state.dataPTHP->PTUnit(PTUnitNum).Name);
            CoolOutAirVolFlowDes =
                min(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA, state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow);
            if (CoolOutAirVolFlowDes < SmallAirVolFlow) {
                CoolOutAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow = CoolOutAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]",
                                             CoolOutAirVolFlowDes);
            } else {
                if (state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow > 0.0 && CoolOutAirVolFlowDes > 0.0 && SizingDesRunThisZone) {
                    CoolOutAirVolFlowUser = state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                 state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                 "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]",
                                                 CoolOutAirVolFlowDes,
                                                 "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]",
                                                 CoolOutAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(CoolOutAirVolFlowDes - CoolOutAirVolFlowUser) / CoolOutAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePTUnit: Potential issue with equipment sizing for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + ' ' +
                                            state.dataPTHP->PTUnit(PTUnitNum).Name);
                            ShowContinueError(
                                state,
                                format("User-Specified Outdoor Air Flow Rate During Cooling Operation of {:.5R} [m3/s]", CoolOutAirVolFlowUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Outdoor Air Flow Rate During Cooling Operation of {:.5R} [m3/s]",
                                                     CoolOutAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
        if (!IsAutoSize && !SizingDesRunThisZone) { // Simulation continue
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "User-Specified Supply Air Flow Rate During Heating Operation [m3/s]",
                                             state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType, state.dataPTHP->PTUnit(PTUnitNum).Name);
            HeatOutAirVolFlowDes =
                min(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow);
            if (HeatOutAirVolFlowDes < SmallAirVolFlow) {
                HeatOutAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow = HeatOutAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]",
                                             HeatOutAirVolFlowDes);
            } else {
                if (state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow > 0.0 && HeatOutAirVolFlowDes > 0.0 && SizingDesRunThisZone) {
                    HeatOutAirVolFlowUser = state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                 state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                 "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]",
                                                 HeatOutAirVolFlowDes,
                                                 "User-Specified Outdoor Air Flow Rate During Heating Operation [m3/s]",
                                                 HeatOutAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(HeatOutAirVolFlowDes - HeatOutAirVolFlowUser) / HeatOutAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePTUnit: Potential issue with equipment sizing for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + ' ' +
                                            state.dataPTHP->PTUnit(PTUnitNum).Name);
                            ShowContinueError(
                                state,
                                format("User-Specified Outdoor Air Flow Rate During Heating Operation of {:.5R} [m3/s]", HeatOutAirVolFlowUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Outdoor Air Flow Rate During Heating Operation of {:.5R} [m3/s]",
                                                     HeatOutAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
        if (!IsAutoSize && !SizingDesRunThisZone) { // Simulation continue
            if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                             state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType, state.dataPTHP->PTUnit(PTUnitNum).Name);
            NoCoolHeatOutAirVolFlowDes =
                min(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA, state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow);
            if (NoCoolHeatOutAirVolFlowDes < SmallAirVolFlow) {
                NoCoolHeatOutAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow = NoCoolHeatOutAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                             NoCoolHeatOutAirVolFlowDes);
            } else {
                if (state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow > 0.0 && NoCoolHeatOutAirVolFlowDes > 0.0 && SizingDesRunThisZone) {
                    NoCoolHeatOutAirVolFlowUser = state.dataPTHP->PTUnit(PTUnitNum).NoCoolHeatOutAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                 state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                 "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                                 NoCoolHeatOutAirVolFlowDes,
                                                 "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                                 NoCoolHeatOutAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(NoCoolHeatOutAirVolFlowDes - NoCoolHeatOutAirVolFlowUser) / NoCoolHeatOutAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePTUnit: Potential issue with equipment sizing for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + ' ' +
                                            state.dataPTHP->PTUnit(PTUnitNum).Name);
                            ShowContinueError(state,
                                              format("User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed of {:.5R} [m3/s]",
                                                     NoCoolHeatOutAirVolFlowUser));
                            ShowContinueError(
                                state,
                                format("differs from Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed of {:.5R} [m3/s]",
                                       NoCoolHeatOutAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
        if (!IsAutoSize && !SizingDesRunThisZone) { // Simulation continue
            if (state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]",
                                             state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat);
            }
        } else {
            CheckZoneSizing(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType, state.dataPTHP->PTUnit(PTUnitNum).Name);
            MaxSATSupHeatDes = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).HeatDesTemp;
            if (IsAutoSize) {
                state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat = MaxSATSupHeatDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                             state.dataPTHP->PTUnit(PTUnitNum).Name,
                                             "Design Size Maximum Supply Air Temperature from Supplemental Heater [C]",
                                             MaxSATSupHeatDes);
            } else {
                if (state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat > 0.0 && MaxSATSupHeatDes > 0.0 && SizingDesRunThisZone) {
                    MaxSATSupHeatUser = state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                 state.dataPTHP->PTUnit(PTUnitNum).Name,
                                                 "Design Size Maximum Supply Air Temperature from Supplemental Heater [C]",
                                                 MaxSATSupHeatDes,
                                                 "User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]",
                                                 MaxSATSupHeatUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if (std::abs(MaxSATSupHeatDes - MaxSATSupHeatUser) > (4.0 * state.dataSize->AutoVsHardSizingDeltaTempThreshold)) {
                            ShowMessage(state,
                                        "SizePTUnit: Potential issue with equipment sizing for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType + ' ' +
                                            state.dataPTHP->PTUnit(PTUnitNum).Name);
                            ShowContinueError(
                                state,
                                format("User-Specified Maximum Supply Air Temperature from Supplemental Heater of {:.2R} [C]", MaxSATSupHeatUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Maximum Supply Air Temperature from Supplemental Heater of {:.2R} [C]",
                                                     MaxSATSupHeatDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    SetCoilDesFlow(state,
                   state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType,
                   state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                   state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow,
                   ErrorsFound);

    if (state.dataSize->CurZoneEqNum > 0) {
        ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow =
            max(state.dataPTHP->PTUnit(PTUnitNum).CoolOutAirVolFlow, state.dataPTHP->PTUnit(PTUnitNum).HeatOutAirVolFlow);
        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow =
            max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow);
        // save air flow rates for child object sizing
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow > 0.0) {
            ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirFlow = true;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow;
        }
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow > 0.0) {
            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirFlow = true;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirVolFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow;
        }
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).ControlType == iCtrlType::CCM_ASHRAE) {

        SizingDesRunThisZone = false;
        state.dataSize->DataZoneUsedForSizing = state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum;
        CheckThisZoneForSizing(state, state.dataSize->DataZoneUsedForSizing, SizingDesRunThisZone);

        Real64 capacityMultiplier = 0.5; // one-half of design zone load
        if (SizingDesRunThisZone) {
            state.dataSize->DataCapacityUsedForSizing =
                state.dataSize->FinalZoneSizing(state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum).DesCoolLoad * capacityMultiplier;
        } else {
            state.dataSize->DataCapacityUsedForSizing = state.dataPTHP->PTUnit(PTUnitNum).DesignCoolingCapacity * capacityMultiplier;
        }
        state.dataSize->DataCapacityUsedForSizing /= state.dataPTHP->PTUnit(PTUnitNum).ControlZoneMassFlowFrac;
        state.dataSize->DataFlowUsedForSizing = state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow;
        PrintFlag = true;
        ASHRAEMinSATCoolingSizer sizerASHRAEMinSATCooling;
        sizerASHRAEMinSATCooling.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
        state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp =
            sizerASHRAEMinSATCooling.size(state, state.dataPTHP->PTUnit(PTUnitNum).DesignMinOutletTemp, ErrorsFound);

        if (SizingDesRunThisZone) {
            state.dataSize->DataCapacityUsedForSizing =
                state.dataSize->FinalZoneSizing(state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum).DesHeatLoad * capacityMultiplier;
        } else {
            state.dataSize->DataCapacityUsedForSizing = state.dataPTHP->PTUnit(PTUnitNum).DesignHeatingCapacity * capacityMultiplier;
        }
        state.dataSize->DataCapacityUsedForSizing /= state.dataPTHP->PTUnit(PTUnitNum).ControlZoneMassFlowFrac;
        state.dataSize->DataFlowUsedForSizing = state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow;
        ASHRAEMaxSATHeatingSizer sizerASHRAEMaxSATHeating;
        sizerASHRAEMaxSATHeating.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
        state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp =
            sizerASHRAEMaxSATHeating.size(state, state.dataPTHP->PTUnit(PTUnitNum).DesignMaxOutletTemp, ErrorsFound);

        state.dataSize->DataCapacityUsedForSizing = 0.0; // reset so other routines don't use this inadvertently
        state.dataSize->DataFlowUsedForSizing = 0.0;
        state.dataSize->DataZoneUsedForSizing = 0;

        // check that MaxNoCoolHeatAirVolFlow is less than both MaxCoolAirVolFlow and MaxHeatAirVolFlow
        if (state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow >= state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow ||
            state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow >= state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow) {
            ShowSevereError(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType + " = " + state.dataPTHP->PTUnit(PTUnitNum).Name);
            ShowContinueError(state,
                              " For SingleZoneVAV control the No Load Supply Air Flow Rate must be less than both the cooling and heating supply "
                              "air flow rates.");
            state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow =
                min(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirVolFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirVolFlow) - 0.01;
            ShowContinueError(state,
                              format(" The SingleZoneVAV control No Load Supply Air Flow Rate is reset to {:.5R} and the simulation continues.",
                                     state.dataPTHP->PTUnit(PTUnitNum).MaxNoCoolHeatAirVolFlow));
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }

    state.dataSize->DataScalableCapSizingON = false;
}

void ControlPTUnitOutput(EnergyPlusData &state,
                         int const PTUnitNum,           // Unit index in fan coil array
                         bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                         int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                         Real64 const QZnReq,           // cooling or heating output needed by zone [W]
                         int const ZoneNum,             // Index to zone number
                         Real64 &PartLoadFrac,          // unit part load fraction
                         Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                         Real64 &SupHeaterLoad,         // Supplemental heater load [W]
                         bool &HXUnitOn                 // flag to enable heat exchanger
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determine the part load fraction of the heat pump for this time step.

    // METHODOLOGY EMPLOYED:
    // Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

    // Using/Aliasing
    using General::SolveRoot;

    using HeatingCoils::SimulateHeatingCoilComponents;
    using PlantUtilities::SetComponentFlowRate;
    using Psychrometrics::PsyCpAirFnW;
    using SteamCoils::SimulateSteamCoilComponents;
    using WaterCoils::SimulateWaterCoilComponents;

    // SUBROUTINE ARGUMENT DEFINITIONS:
    Real64 mdot; // coil fluid mass flow rate (kg/s)

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIte(500);    // maximum number of iterations
    Real64 const MinPLF(0.0); // minimum part load factor allowed

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FullOutput;   // unit full output when compressor is operating [W]
    Real64 TempOutput;   // unit output when iteration limit exceeded [W]
    Real64 NoCompOutput; // output when no active compressor [W]
    Real64 ErrorToler;   // error tolerance
    int SolFla;          // Flag of RegulaFalsi solver
    auto &ControlPTUnitOutputPar = state.dataPTHP->ControlPTUnitOutputPar;
    Real64 CpAir;              // air specific heat
    Real64 OutsideDryBulbTemp; // Outside air temperature at external node height
    // unused1208  REAL(r64)          :: UpperLimitPLR ! used when RegulaFalsi exceeds iteration limit
    Real64 TempMinPLR;
    Real64 TempMaxPLR;
    bool ContinueIter;

    SupHeaterLoad = 0.0;
    PartLoadFrac = 0.0;

    if (state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum == 0) {
        OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
    } else {
        OutsideDryBulbTemp = state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum).Temp;
    }

    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) == 0.0) return;

    // If no heating or cooling required the coils needs to be off
    if (!state.dataPTHP->HeatingLoad && !state.dataPTHP->CoolingLoad) {
        return;
    }

    // Get result when DX coil is off
    state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 0.0; // set SZVAV model variable
    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, PartLoadFrac, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);

    // Get full load result
    PartLoadFrac = 1.0;
    state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0; // set SZVAV model variable
    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);

    if (state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel) {

        if ((state.dataPTHP->CoolingLoad && FullOutput < QZnReq) || (state.dataPTHP->HeatingLoad && FullOutput > QZnReq)) {
            if ((state.dataPTHP->CoolingLoad && NoCompOutput < QZnReq) || (state.dataPTHP->HeatingLoad && NoCompOutput > QZnReq)) {
                PartLoadFrac = 0.0;
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 0.0; // set SZVAV model variable
            } else {
                int AirLoopNum = 0;
                int CompressorOnFlag = 0;
                auto &SZVAVModel(state.dataPTHP->PTUnit(PTUnitNum));
                // seems like passing these (arguments 2-n) as an array (similar to ControlPTUnitOutputPar) would make this more uniform across
                // different models
                SZVAVModel::calcSZVAVModel(state,
                                           SZVAVModel,
                                           PTUnitNum,
                                           FirstHVACIteration,
                                           state.dataPTHP->CoolingLoad,
                                           state.dataPTHP->HeatingLoad,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           HXUnitOn,
                                           AirLoopNum,
                                           PartLoadFrac,
                                           CompressorOnFlag);
            }
        }

    } else {

        if (state.dataPTHP->CoolingLoad) {
            // Since we are cooling, we expect FullOutput < NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
            if (FullOutput >= NoCompOutput) {
                PartLoadFrac = 0.0;
                return;
            }
            // If the QZnReq <= FullOutput the unit needs to run full out
            if (QZnReq <= FullOutput) {
                PartLoadFrac = 1.0;
                return;
            }
            if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
                ErrorToler = 0.001;
            } else {
                ErrorToler = state.dataPTHP->PTUnit(PTUnitNum).CoolConvergenceTol; // Error tolerance for convergence from input deck
            }
        } else {
            // Since we are heating, we expect FullOutput > NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off)
            if (FullOutput <= NoCompOutput) {
                PartLoadFrac = 0.0;
                // may need supplemental heating so don't return in heating mode
                //    RETURN
            }
            // If the QZnReq >= FullOutput the unit needs to run full out
            if (QZnReq >= FullOutput && state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
                PartLoadFrac = 1.0;
                // may need supplemental heating so don't return in heating mode
                //    RETURN
            }
            ErrorToler = state.dataPTHP->PTUnit(PTUnitNum).HeatConvergenceTol; // Error tolerance for convergence from input deck
        }

        // Calculate the part load fraction

        if ((state.dataPTHP->HeatingLoad && QZnReq < FullOutput) || (state.dataPTHP->CoolingLoad && QZnReq > FullOutput)) {

            ControlPTUnitOutputPar(1) = PTUnitNum;
            ControlPTUnitOutputPar(2) = ZoneNum;
            if (FirstHVACIteration) {
                ControlPTUnitOutputPar(3) = 1.0;
            } else {
                ControlPTUnitOutputPar(3) = 0.0;
            }
            ControlPTUnitOutputPar(4) = OpMode;
            ControlPTUnitOutputPar(5) = QZnReq;
            ControlPTUnitOutputPar(6) = OnOffAirFlowRatio;
            ControlPTUnitOutputPar(7) = SupHeaterLoad;
            if (HXUnitOn) {
                ControlPTUnitOutputPar(8) = 1.0;
            } else {
                ControlPTUnitOutputPar(8) = 0.0;
            }
            SolveRoot(state, ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual, 0.0, 1.0, ControlPTUnitOutputPar);
            if (SolFla == -1) {
                //     Very low loads may not converge quickly. Tighten PLR boundary and try again.
                TempMaxPLR = -0.1;
                ContinueIter = true;
                while (ContinueIter && TempMaxPLR < 1.0) {
                    TempMaxPLR += 0.1;
                    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, TempMaxPLR, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);
                    if (state.dataPTHP->HeatingLoad && TempOutput > QZnReq) ContinueIter = false;
                    if (state.dataPTHP->CoolingLoad && TempOutput < QZnReq) ContinueIter = false;
                }
                TempMinPLR = TempMaxPLR;
                ContinueIter = true;
                while (ContinueIter && TempMinPLR > 0.0) {
                    TempMinPLR -= 0.01;
                    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, TempMinPLR, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);
                    if (state.dataPTHP->HeatingLoad && TempOutput < QZnReq) ContinueIter = false;
                    if (state.dataPTHP->CoolingLoad && TempOutput > QZnReq) ContinueIter = false;
                }
                SolveRoot(state, ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual, TempMinPLR, TempMaxPLR, ControlPTUnitOutputPar);
                if (SolFla == -1) {
                    if (!FirstHVACIteration && !state.dataGlobal->WarmupFlag) {
                        CalcPTUnit(
                            state, PTUnitNum, FirstHVACIteration, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);
                        if (state.dataPTHP->PTUnit(PTUnitNum).IterErrIndex == 0) {
                            ShowWarningError(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                            ShowContinueError(
                                state,
                                format(" Iteration limit exceeded calculating packaged terminal unit part-load ratio, maximum iterations = {}",
                                       MaxIte));
                            ShowContinueErrorTimeStamp(state, format(" Part-load ratio returned = {:.3R}", PartLoadFrac));
                            ShowContinueError(state, format(" Load requested = {:.5T}, Load delivered = {:.5T}", QZnReq, TempOutput));
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                           "\" - Iteration limit exceeded error continues...",
                                                       state.dataPTHP->PTUnit(PTUnitNum).IterErrIndex,
                                                       TempOutput,
                                                       TempOutput,
                                                       _,
                                                       "{W}",
                                                       "{W}");
                    }
                } else if (SolFla == -2) {
                    if (!FirstHVACIteration) {
                        ShowWarningError(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                        ShowContinueError(state, "Packaged terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded");
                        ShowContinueError(state, "Please fill out a bug report and forward to the EnergyPlus support group.");
                        ShowContinueErrorTimeStamp(state, "");
                        if (state.dataGlobal->WarmupFlag) ShowContinueError(state, "Error occurred during warmup days.");
                    }
                    PartLoadFrac = max(MinPLF, std::abs(QZnReq - NoCompOutput) / std::abs(FullOutput - NoCompOutput));
                }
            } else if (SolFla == -2) {
                if (!FirstHVACIteration) {
                    ShowWarningError(state, state.dataPTHP->PTUnit(PTUnitNum).UnitType + " \"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                    ShowContinueError(state, "Packaged terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded");
                    ShowContinueError(state, "Please fill out a bug report and forward to the EnergyPlus support group.");
                    ShowContinueErrorTimeStamp(state, "");
                    if (state.dataGlobal->WarmupFlag) ShowContinueError(state, "Error occurred during warmup days.");
                }
                PartLoadFrac = max(MinPLF, std::abs(QZnReq - NoCompOutput) / std::abs(FullOutput - NoCompOutput));
            }
        }
    }
    // if the DX heating coil cannot meet the load, trim with supplemental heater
    // occurs with constant fan mode when compressor is on or off
    // occurs with cycling fan mode when compressor PLR is equal to 1
    if (state.dataPTHP->HeatingLoad && QZnReq > FullOutput && state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        PartLoadFrac = 1.0;
        if (OutsideDryBulbTemp <= state.dataPTHP->PTUnit(PTUnitNum).MaxOATSupHeat) {
            SupHeaterLoad = QZnReq - FullOutput;
        } else {
            SupHeaterLoad = 0.0;
        }
        CalcPTUnit(state, PTUnitNum, FirstHVACIteration, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);
    }

    // check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
    if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        if (state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp > state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat &&
            SupHeaterLoad > 0.0) {

            // If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
            SupHeaterLoad = 0.0;
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                                PartLoadFrac);
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad);
                }
            }

            //     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
            //     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
            //     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
            //     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
            if (state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp < state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat) {
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).HumRat);
                SupHeaterLoad =
                    state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirInNode).MassFlowRate * CpAir *
                    (state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat - state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp);

            } else {
                SupHeaterLoad = 0.0;
            }
        }
    }
}

void CalcPTUnit(EnergyPlusData &state,
                int const PTUnitNum,           // Unit index in fan coil array
                bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                Real64 const PartLoadFrac,     // compressor part load fraction
                Real64 &LoadMet,               // load met by unit (W)
                Real64 const QZnReq,           // Zone load (W) unused1208
                Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                Real64 &SupHeaterLoad,         // supplemental heater load (W)
                bool const HXUnitOn            // flag to enable heat exchanger
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED        B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
    //                       Chandan Sharma, July 2012 : Added zone sys avail managers
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulate the components making up the packaged terminal heat pump.

    // METHODOLOGY EMPLOYED:
    // Simulates the unit components sequentially in the air flow direction.

    // Using/Aliasing
    using DXCoils::SimDXCoil;

    using General::SolveRoot;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
    using MixedAir::SimOAMixer;
    using PlantUtilities::SetComponentFlowRate;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using SingleDuct::SimATMixer;
    using SteamCoils::SimulateSteamCoilComponents;
    using WaterCoils::SimulateWaterCoilComponents;
    using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

    // Locals
    Real64 MinWaterFlow; // minimum water mass flow rate

    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const ErrTolerance(0.001); // convergence limit for hotwater coil
    int const SolveMaxIter(50);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;            // PTHP air outlet node
    int InletNode;             // PTHP air inlet node
    int ZoneNode;              // Zone node of zone PTHP is serving
    int ControlledZoneNum;     // Index of ZoneEquipConfig that uses this heat pump
    Real64 AirMassFlow;        // total supply air mass flow through the PTHP [m3/s]
    Real64 MinHumRat;          // minimum humidity ratio for sensible capacity calculation (kg/kg)
    Real64 OutsideDryBulbTemp; // Outdoor air temperature at external node height
    Real64 QCoilReq;           // load passed to heating coil (W)
    Real64 QActual;            // actual heating coil output (W)
    int OpMode;                // fan operating mode, CycFanCycCoil or ContFanCycCoil
    bool errFlag;              // subroutine error flag
    Real64 WSHPRuntimeFrac;    // RTF variable for WSHP's
    Real64 mdot;               // local temporary for mass flow rate
    Real64 MaxHotWaterFlow;    // coil maximum hot water mass flow rate, kg/s
    Real64 HotWaterMdot;       // actual hot water mass flow rate
    auto &CalcPTUnitPar = state.dataPTHP->CalcPTUnitPar;
    int SolFlag;
    int ATMixOutNode(0); // outlet node of ATM Mixer

    // Tuned Named constants to avoid heap allocation when passed to Optional args
    bool const True(true);
    bool const False(false);
    int const iZero(0);
    int const iOne(1);
    Real64 const dZero(0.0);
    Real64 const dOne(1.0);

    OutletNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    ControlledZoneNum = state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum;
    ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
    OpMode = state.dataPTHP->PTUnit(PTUnitNum).OpMode;
    if (state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum == 0) {
        OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
    } else {
        OutsideDryBulbTemp = state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).CondenserNodeNum).Temp;
    }

    state.dataPTHP->SaveCompressorPLR = 0.0;
    // Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
    SetAverageAirFlow(state, PTUnitNum, PartLoadFrac, OnOffAirFlowRatio);

    AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        // There is an air terminal mixer
        ATMixOutNode = state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode;
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) { // if there is an inlet side air terminal mixer
            // set the primary air inlet mass flow rate
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode).MassFlowRate =
                min(state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode).MassFlowRateMaxAvail,
                    state.dataLoopNodes->Node(InletNode).MassFlowRate);
            // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
            // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
            SimATMixer(state, state.dataPTHP->PTUnit(PTUnitNum).ATMixerName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex);
        }
    } else {
        // No air terminal mixer; simulate the outside air mixer
        ATMixOutNode = 0;
        if (state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode > 0)
            SimOAMixer(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex);
    }

    // if blow through, simulate fan then coils
    if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        } else {
            Fans::SimulateFanComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanIndex,
                                        state.dataPTHP->FanSpeedRatio,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        }
    }

    if (state.dataPTHP->CoolingLoad && OutsideDryBulbTemp > state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling) {
        {
            auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num);
            if ((SELECT_CASE_var == iPTHPType::PTACUnit) || (SELECT_CASE_var == iPTHPType::PTHPUnit)) {
                if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) {
                    SimHXAssistedCoolingCoil(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                                             FirstHVACIteration,
                                             On,
                                             PartLoadFrac,
                                             state.dataPTHP->PTUnit(PTUnitNum).CoolCoilCompIndex,
                                             state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                             HXUnitOn);
                } else {
                    SimDXCoil(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              On,
                              FirstHVACIteration,
                              state.dataPTHP->PTUnit(PTUnitNum).CoolCoilCompIndex,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              PartLoadFrac,
                              OnOffAirFlowRatio);
                }
                state.dataPTHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum);
            } else if (SELECT_CASE_var == iPTHPType::PTWSHPUnit) {
                HeatPumpRunFrac(state, PTUnitNum, PartLoadFrac, errFlag, WSHPRuntimeFrac);
                SimWatertoAirHPSimple(state,
                                      std::string(),
                                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                      QZnReq,
                                      dOne,
                                      OpMode,
                                      WSHPRuntimeFrac,
                                      state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                      state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                      state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                      iOne,
                                      PartLoadFrac,
                                      FirstHVACIteration,
                                      OnOffAirFlowRatio);
                state.dataPTHP->SaveCompressorPLR = PartLoadFrac;
            } else {
            }
        }
    } else { // cooling coil is off
        {
            auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num);
            if ((SELECT_CASE_var == iPTHPType::PTACUnit) || (SELECT_CASE_var == iPTHPType::PTHPUnit)) {
                if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) {
                    SimHXAssistedCoolingCoil(state,
                                             state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                                             FirstHVACIteration,
                                             Off,
                                             dZero,
                                             state.dataPTHP->PTUnit(PTUnitNum).CoolCoilCompIndex,
                                             state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                             HXUnitOn);
                } else {
                    SimDXCoil(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilName,
                              Off,
                              FirstHVACIteration,
                              state.dataPTHP->PTUnit(PTUnitNum).CoolCoilCompIndex,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              dZero,
                              OnOffAirFlowRatio);
                }
            } else if (SELECT_CASE_var == iPTHPType::PTWSHPUnit) {
                SimWatertoAirHPSimple(state,
                                      std::string(),
                                      state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                                      dZero,
                                      dZero,
                                      OpMode,
                                      dZero,
                                      state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                      state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                      state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                      iZero,
                                      dZero,
                                      FirstHVACIteration);
            } else {
            }
        }
    }
    if (state.dataPTHP->HeatingLoad && OutsideDryBulbTemp > state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorHeating) {
        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
            QCoilReq = state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap * PartLoadFrac;
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingGasOrOtherFuel ||
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingElectric) {
                SimulateHeatingCoilComponents(state,
                                              state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                              FirstHVACIteration,
                                              QCoilReq,
                                              state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                              QActual,
                                              False,
                                              OpMode,
                                              PartLoadFrac);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                //       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
                if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoilWaterFlowRatio == 0.0) {
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow * PartLoadFrac;
                } else {
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).HeatCoilWaterFlowRatio * state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow;
                }

                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                SimulateWaterCoilComponents(state,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                            FirstHVACIteration,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                            QActual,
                                            state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                            PartLoadFrac);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                //       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
                mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow * PartLoadFrac;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                SimulateSteamCoilComponents(state,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                            FirstHVACIteration,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                            QCoilReq,
                                            QActual,
                                            state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                            PartLoadFrac);
            }
        } else {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num);
                if (SELECT_CASE_var == iPTHPType::PTHPUnit) {
                    SimDXCoil(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                              On,
                              FirstHVACIteration,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              PartLoadFrac,
                              OnOffAirFlowRatio);
                    state.dataPTHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum);
                } else if (SELECT_CASE_var == iPTHPType::PTWSHPUnit) {
                    HeatPumpRunFrac(state, PTUnitNum, PartLoadFrac, errFlag, WSHPRuntimeFrac);
                    SimWatertoAirHPSimple(state,
                                          std::string(),
                                          state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                          QZnReq,
                                          dZero,
                                          OpMode,
                                          WSHPRuntimeFrac,
                                          state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                          state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                          state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                          iOne,
                                          PartLoadFrac,
                                          FirstHVACIteration,
                                          OnOffAirFlowRatio);
                    state.dataPTHP->SaveCompressorPLR = PartLoadFrac;
                } else {
                }
            }
        }
    } else {
        //   heating coil is off
        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
            QCoilReq = 0.0;
            if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingGasOrOtherFuel ||
                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingElectric) {
                SimulateHeatingCoilComponents(state,
                                              state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                              FirstHVACIteration,
                                              QCoilReq,
                                              state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                SimulateWaterCoilComponents(
                    state, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex);
            } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                     state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                SimulateSteamCoilComponents(state,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                            FirstHVACIteration,
                                            state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                            QCoilReq,
                                            QActual,
                                            state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                            PartLoadFrac);
            }
        } else {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num);
                if (SELECT_CASE_var == iPTHPType::PTHPUnit) {
                    SimDXCoil(state,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilName,
                              Off,
                              FirstHVACIteration,
                              state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              dZero,
                              OnOffAirFlowRatio);
                } else if (SELECT_CASE_var == iPTHPType::PTWSHPUnit) {
                    SimWatertoAirHPSimple(state,
                                          std::string(),
                                          state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                          dZero,
                                          dZero,
                                          OpMode,
                                          dZero,
                                          state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                          state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                          state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                          iZero,
                                          dZero,
                                          FirstHVACIteration);

                } else {
                }
            }
        }
    }

    // if draw through, simulate coils then fan
    if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == DrawThru) {
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        } else {
            Fans::SimulateFanComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanIndex,
                                        state.dataPTHP->FanSpeedRatio,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        }
    }
    if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        if (SupHeaterLoad < SmallLoad) {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                  QActual,
                                                  True,
                                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                }
            }
        } else {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                  QActual,
                                                  True,
                                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    MaxHotWaterFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         MaxHotWaterFlow,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    QActual = SupHeaterLoad;
                    // simulate the hot water supplemental heating coil
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                    if (QActual > (SupHeaterLoad + SmallLoad)) {
                        // control water flow to obtain output matching SupHeaterLoad
                        SolFlag = 0;
                        MinWaterFlow = 0.0;
                        CalcPTUnitPar(1) = double(PTUnitNum);
                        if (FirstHVACIteration) {
                            CalcPTUnitPar(2) = 1.0;
                        } else {
                            CalcPTUnitPar(2) = 0.0;
                        }
                        CalcPTUnitPar(3) = SupHeaterLoad;
                        MaxHotWaterFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                        SolveRoot(state,
                                  ErrTolerance,
                                  SolveMaxIter,
                                  SolFlag,
                                  HotWaterMdot,
                                  HotWaterCoilResidual,
                                  MinWaterFlow,
                                  MaxHotWaterFlow,
                                  CalcPTUnitPar);
                        if (SolFlag == -1) {
                            if (state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex == 0) {
                                ShowWarningMessage(state,
                                                   "CalcPTUnit: Hot water coil control failed for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType +
                                                       "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name + "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("  Iteration limit [{}] exceeded in calculating hot water mass flow rate", SolveMaxIter));
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           format("CalcPTUnit: Hot water coil control failed (iteration limit [{}]) for {}=\"{}",
                                                                  SolveMaxIter,
                                                                  state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                                                  state.dataPTHP->PTUnit(PTUnitNum).Name),
                                                           state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex);
                        } else if (SolFlag == -2) {
                            if (state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex2 == 0) {
                                ShowWarningMessage(state,
                                                   "CalcPTUnit: Hot water coil control failed (maximum flow limits) for " +
                                                       state.dataPTHP->PTUnit(PTUnitNum).UnitType + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                       "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state, "...Bad hot water maximum flow rate limits");
                                ShowContinueError(state, format("...Given minimum water flow rate={:.3R} kg/s", MinWaterFlow));
                                ShowContinueError(state, format("...Given maximum water flow rate={:.3R} kg/s", MaxHotWaterFlow));
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "CalcPTUnit: Hot water coil control failed (flow limits) for " +
                                                               state.dataPTHP->PTUnit(PTUnitNum).UnitType + "=\"" +
                                                               state.dataPTHP->PTUnit(PTUnitNum).Name + "\"",
                                                           state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex2,
                                                           MaxHotWaterFlow,
                                                           MinWaterFlow,
                                                           _,
                                                           "[kg/s]",
                                                           "[kg/s]");
                        }
                        QActual = SupHeaterLoad;
                        // simulate the hot water supplemental heating coil
                        SimulateWaterCoilComponents(state,
                                                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                    FirstHVACIteration,
                                                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                    QActual,
                                                    state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                    }
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);

                    // simulate the steam supplemental heating coil
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                }
            }
        }
    }

    // If there is a supply side air terminal mixer, calculate its output
    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            SimATMixer(state, state.dataPTHP->PTUnit(PTUnitNum).ATMixerName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex);
        }
    }

    // calculate sensible load met
    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            // Air terminal supply side mixer
            LoadMet = state.dataLoopNodes->Node(ATMixOutNode).MassFlowRate *
                      (PsyHFnTdbW(state.dataLoopNodes->Node(ATMixOutNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                       PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
        } else {
            // Air terminal inlet side mixer
            LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                                     PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
        }
    } else {
        MinHumRat = min(state.dataLoopNodes->Node(InletNode).HumRat, state.dataLoopNodes->Node(OutletNode).HumRat);
        LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                 PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));
    }
}

void HeatPumpRunFrac(EnergyPlusData &state,
                     int const PTUnitNum, // PTAC Index Number
                     Real64 const PLR,    // part load ratio
                     bool &errFlag,       // part load factor out of range flag
                     Real64 &RuntimeFrac  // the required run time fraction to meet part load
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad (based on subroutine by Kenneth Tang)
    //       DATE WRITTEN   June 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the PLF based on the PLR. Parameters required are
    // thermostat cycling rate (Nmax), heat pump time constant (tau), and the fraction
    // of on-cycle power use (pr)

    // METHODOLOGY EMPLOYED:
    // NA

    // REFERENCES:
    // (1) Henderson, H. I., K. Rengarajan.1996. A Model to predict the latent capacity
    // of air conditioners and heat pumps at part-load conditions with constant fan
    // operation. ASHRAE Transactions 102 (1): 266-274

    // (2) Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment
    // Part Load Curves for Use in DOE-2.  Environmental Energy Technologies Division,
    // Ernest Orlando Lawrence Berkeley National Laboratory.

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PartLoadFactor; // Part load factor
    Real64 Nmax;           // Maximum cycling rate [cycles/hr]
    Real64 tau;            // Heat pump time constant [s]
    Real64 pr;             // On-cycle power use fraction [~]
    Real64 error;          // Calculation error
    Real64 PLF1;           // ith term of part load factor
    Real64 PLF2;           // (i+1)th term of part load factor
    Real64 A;              // Variable for simplify equation
    int NumIteration;      // Iteration Counter

    Nmax = state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour;
    tau = state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant;
    pr = state.dataPTHP->PTUnit(PTUnitNum).OnCyclePowerFraction;

    // Initialize
    errFlag = false;
    error = 1.0;
    NumIteration = 0;

    // Initial guess for part load fraction
    PLF1 = 1.0;

    // Calculate PLF using successive substitution until convergence is achieved
    while (true) {
        ++NumIteration;

        if (PLR == 1) {
            // Set part load fraction, PLF1=1.0 if PLR=1.0 and exit loop
            PLF1 = 1.0;
            goto LOOPPLF_exit;
        }

        if (NumIteration > 100) {
            // Exit loop if interation exceed 100
            errFlag = true;
            PLF1 = 1.0;
            goto LOOPPLF_exit;
        }

        if (error < 0.00001) {
            // Exit loop if convergence is achieved
            goto LOOPPLF_exit;

        } else {
            // Calculate PLF
            A = 4.0 * tau * (Nmax / 3600.0) * (1 - PLR / PLF1);
            if (A < 1.5e-3) {
                // A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
                // from "float underflow error". Occurs when PLR is very close to 1.0,
                // small A value, thus Exp(-1/A) = 0
                PLF2 = 1 - A;
            } else {
                PLF2 = 1.0 - A * (1 - std::exp(-1.0 / A));
            }
            error = std::abs((PLF2 - PLF1) / PLF1);
            PLF1 = PLF2;
        }
    }
LOOPPLF_exit:;

    // Adjust PLF for the off cycle power consumption if
    // on-cycle power use is specified by the user
    if (pr > 0.0) {
        PartLoadFactor = PLR / ((PLR / PLF1) + (1 - PLR / PLF1) * pr);
    } else {
        PartLoadFactor = PLF1;
    }

    if (PartLoadFactor <= 0.0) {
        PartLoadFactor = 0.0;
        RuntimeFrac = 0.0;
        errFlag = true;
    } else {
        RuntimeFrac = PLR / PartLoadFactor;
    }

    if (RuntimeFrac > 1.0) {
        RuntimeFrac = 1.0;
    }
}

Real64 HotWaterCoilResidual(EnergyPlusData &state,
                            Real64 const HWFlow,       // hot water flow rate in kg/s
                            Array1D<Real64> const &Par // Par(5) is the requested coil load
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   January 2012
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (Actual Coil Output - SupHeaterLoad) / SupHeaterLoad
    // the actual coil output depends on the hot water flow rate which is being varied to
    // minimize the residual to the tolerance limit specified.

    // METHODOLOGY EMPLOYED:
    // Calls HotWaterCoilResidual, and calculates the residual as defined above.

    // REFERENCES:

    // Using/Aliasing
    using PlantUtilities::SetComponentFlowRate;
    using WaterCoils::SimulateWaterCoilComponents;

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // Argument array dimensioning

    // Locals
    Real64 SupHeaterLoad; // requested coild load, W

    // SUBROUTINE ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum;
    bool FirstHVACSoln;
    Real64 QCoilActual; // delivered coild load, W
    Real64 mdot;

    PTUnitNum = int(Par(1));
    FirstHVACSoln = (Par(2) > 0.0);
    SupHeaterLoad = Par(3);
    QCoilActual = SupHeaterLoad;
    mdot = HWFlow;

    SetComponentFlowRate(state,
                         mdot,
                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
    // simulate the hot water supplemental heating coil
    SimulateWaterCoilComponents(state,
                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                FirstHVACSoln,
                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                QCoilActual,
                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);

    if (SupHeaterLoad != 0.0) {
        Residuum = (QCoilActual - SupHeaterLoad) / SupHeaterLoad;
    } else { // Autodesk:Return ELSE added to assure return value is set
        Residuum = 0.0;
    }
    return Residuum;
}

Real64 SupSATResidual(EnergyPlusData &state,
                      Real64 &TempSupHeater,     // supplemental heater load at maximum SAT
                      Array1D<Real64> const &Par // par(1) = PTUnitNum
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function (outlet temp - maximum supplemental heater SAT)
    //  Outlet temperature depends on the supplemental heater load which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls SimulateHeatingCoilComponents to get outlet temperature minus the maximum supplemental heater SAT
    //  at the given supplemental heater load and calculates the residual as defined above

    // REFERENCES:

    // Using/Aliasing
    using HeatingCoils::SimulateHeatingCoilComponents;

    // Return value
    Real64 SupSATResidual;

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = FirstHVACIteration

    // FUNCTION PARAMETER DEFINITIONS:
    //  na

    // INTERFACE BLOCK SPECIFICATIONS
    //  na

    // DERIVED TYPE DEFINITIONS
    //  na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum;           // PTHP index
    bool FirstHVACIteration; // FirstHVACIteration flag

    PTUnitNum = int(Par(1));
    // FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
    FirstHVACIteration = (Par(2) == 1.0);
    auto &thisUnit = state.dataPTHP->PTUnit(PTUnitNum);
    SimulateHeatingCoilComponents(state, thisUnit.SuppHeatCoilName, FirstHVACIteration, TempSupHeater, thisUnit.SuppHeatCoilIndex, _, true);
    SupSATResidual = state.dataLoopNodes->Node(thisUnit.AirOutNode).Temp - thisUnit.MaxSATSupHeat;

    return SupSATResidual;
}

Real64 PLRResidual(EnergyPlusData &state,
                   Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                   Array1D<Real64> const &Par // par(1) = PTUnitNum
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
    //  PTHP output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls CalcPTHP to get ActualOutput at the given part load ratio
    //  and calculates the residual as defined above

    // REFERENCES:

    // USE STATEMENTS:
    // na

    // Return value
    Real64 PLRResidual;

    // Argument array dimensioning

    // Locals
    Real64 SupHeaterLoad; // load passed to supplemental heater (W)

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = Zone Num
    // par(3) = FirstHVACIteration
    // par(4) = OpMode
    // par(5) = QZnReq
    // par(6) = OnOffAirFlowRatio
    // par(7) = SupHeaterLoad

    // FUNCTION PARAMETER DEFINITIONS:
    //  na

    // INTERFACE BLOCK SPECIFICATIONS
    //  na

    // DERIVED TYPE DEFINITIONS
    //  na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum;            // PTHP index
    int ZoneNum;              // Zone index
    bool FirstHVACIteration;  // FirstHVACIteration flag
    int OpMode;               // Compressor operating mode
    Real64 QZnReq;            // zone load (W)
    Real64 QZnReqTemp;        // denominator representing zone load (W)
    Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
    Real64 ActualOutput;      // delivered capacity of PTHP
    bool HXUnitOn;            // flag to enable heat exchanger

    PTUnitNum = int(Par(1));
    ZoneNum = int(Par(2));
    // FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
    FirstHVACIteration = (Par(3) == 1.0);
    OpMode = int(Par(4));
    QZnReq = Par(5);
    QZnReqTemp = QZnReq;
    if (std::abs(QZnReq) < 100.0) QZnReqTemp = sign(100.0, QZnReq);
    OnOffAirFlowRatio = Par(6);
    SupHeaterLoad = Par(7) * PartLoadFrac;
    HXUnitOn = (Par(8) == 1.0);

    CalcPTUnit(state, PTUnitNum, FirstHVACIteration, PartLoadFrac, ActualOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn);
    PLRResidual = (ActualOutput - QZnReq) / QZnReqTemp;

    return PLRResidual;
}

void SetAverageAirFlow(EnergyPlusData &state,
                       int const PTUnitNum,        // Unit index
                       Real64 const PartLoadRatio, // unit part load ratio
                       Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to average airflow over timestep
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the average air mass flow rates using the part load fraction of the heat pump for this time step
    // Set OnOffAirFlowRatio to be used by DX coils

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;              // inlet node number for PTUnitNum
    int OutsideAirNode;         // outside air node number in PTHP loop
    int AirRelNode;             // relief air node number in PTHP loop
    Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step
    Real64 AverageOAMassFlow;   // average outdoor air mass flow rate over time step
    Real64 fanPartLoadRatio;    // local variable for PLR, used to disconnect fan speed from coil capacity when using SZVAV

    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    OutsideAirNode = state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode;
    AirRelNode = state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode;

    fanPartLoadRatio = PartLoadRatio;
    if (state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel) fanPartLoadRatio = state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio;

    AverageUnitMassFlow = (fanPartLoadRatio * state.dataPTHP->CompOnMassFlow) + ((1 - fanPartLoadRatio) * state.dataPTHP->CompOffMassFlow);
    AverageOAMassFlow = (fanPartLoadRatio * state.dataPTHP->OACompOnMassFlow) + ((1 - fanPartLoadRatio) * state.dataPTHP->OACompOffMassFlow);
    if (state.dataPTHP->CompOffFlowRatio > 0.0) {
        state.dataPTHP->FanSpeedRatio =
            (fanPartLoadRatio * state.dataPTHP->CompOnFlowRatio) + ((1 - fanPartLoadRatio) * state.dataPTHP->CompOffFlowRatio);
    } else {
        state.dataPTHP->FanSpeedRatio = state.dataPTHP->CompOnFlowRatio;
    }

    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) > 0.0 &&
        ((GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr) > 0.0 || state.dataHVACGlobal->ZoneCompTurnFansOn) &&
         !state.dataHVACGlobal->ZoneCompTurnFansOff)) {

        state.dataLoopNodes->Node(InletNode).MassFlowRate = AverageUnitMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AverageUnitMassFlow;
        if (OutsideAirNode > 0) {
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = AverageOAMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = AverageOAMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = AverageOAMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = AverageOAMassFlow;
        }
        if (AverageUnitMassFlow > 0.0) {
            OnOffAirFlowRatio = state.dataPTHP->CompOnMassFlow / AverageUnitMassFlow;
        } else {
            OnOffAirFlowRatio = 0.0;
        }

    } else {

        state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
        if (OutsideAirNode > 0) {
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = 0.0;
        }
        OnOffAirFlowRatio = 0.0;
    }
}

void ReportPTUnit(EnergyPlusData &state, int const PTUnitNum) // number of the current AC unit being simulated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Fills some of the report variables for the packaged terminal heat pump

    // METHODOLOGY EMPLOYED:
    // NA

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // NA

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ReportingConstant;

    ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergy = state.dataPTHP->PTUnit(PTUnitNum).TotCoolEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergy = state.dataPTHP->PTUnit(PTUnitNum).TotHeatEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergy = state.dataPTHP->PTUnit(PTUnitNum).SensCoolEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergy = state.dataPTHP->PTUnit(PTUnitNum).SensHeatEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergy = state.dataPTHP->PTUnit(PTUnitNum).LatCoolEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergy = state.dataPTHP->PTUnit(PTUnitNum).LatHeatEnergyRate * ReportingConstant;
    state.dataPTHP->PTUnit(PTUnitNum).ElecConsumption = state.dataPTHP->PTUnit(PTUnitNum).ElecPower * ReportingConstant;

    // adjust fan report variable to represent how the SZVAV model performs
    if (state.dataPTHP->PTUnit(PTUnitNum).simASHRAEModel) {
        state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio =
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirInNode).MassFlowRate /
            max(state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow, state.dataPTHP->PTUnit(PTUnitNum).MaxHeatAirMassFlow);
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
        if (!state.dataGlobal->SysSizingCalc) {
            DataSizing::resetHVACSizingGlobals(state, state.dataSize->CurZoneEqNum, 0, state.dataPTHP->PTUnit(PTUnitNum).FirstPass);
        }
    }

    state.dataHVACGlobal->OnOffFanPartLoadFraction =
        1.0; // reset to 1 in case blow through fan configuration (fan resets to 1, but for blow thru fans coil sets back down < 1)
}

int GetPTUnitZoneInletAirNode(EnergyPlusData &state, int const PTUnitCompIndex, int const PTUnitType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for zone air inlet node

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // Using/Aliasing
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;

    // Return value
    int GetPTUnitZoneInletAirNode;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum(0);

    if (state.dataPTHP->GetPTUnitInputFlag) {
        GetPTUnit(state);
        state.dataPTHP->GetPTUnitInputFlag = false;
    }

    GetPTUnitZoneInletAirNode = 0;

    // PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
    // objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
    // is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
    // objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
    // So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
    // which was recalculated for total number of all three object type for use in PT data structure.

    {
        auto const SELECT_CASE_var(PTUnitType);
        if (SELECT_CASE_var == PkgTermHPAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex;
        } else if (SELECT_CASE_var == PkgTermACAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP;
        } else if (SELECT_CASE_var == PkgTermHPWaterToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
        } else {
            assert(false);
        }
    }

    if (PTUnitNum > 0 && PTUnitNum <= state.dataPTHP->NumPTUs) {
        GetPTUnitZoneInletAirNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    }

    return GetPTUnitZoneInletAirNode;
}

int GetPTUnitOutAirNode(EnergyPlusData &state, int const PTUnitCompIndex, int const PTUnitType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for OA inlet node

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // Using/Aliasing
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;

    // Return value
    int GetPTUnitOutAirNode;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum(0);

    if (state.dataPTHP->GetPTUnitInputFlag) {
        GetPTUnit(state);
        state.dataPTHP->GetPTUnitInputFlag = false;
    }

    GetPTUnitOutAirNode = 0;

    // PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
    // objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
    // is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
    // objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
    // So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
    // which was recalculated for total number of all three object type for use in PT data structure.

    {
        auto const SELECT_CASE_var(PTUnitType);
        if (SELECT_CASE_var == PkgTermHPAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex;
        } else if (SELECT_CASE_var == PkgTermACAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP;
        } else if (SELECT_CASE_var == PkgTermHPWaterToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
        } else {
            assert(false);
        }
    }

    if (PTUnitNum > 0 && PTUnitNum <= state.dataPTHP->NumPTUs) {

        GetPTUnitOutAirNode = state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode;
    }

    return GetPTUnitOutAirNode;
}

int GetPTUnitReturnAirNode(EnergyPlusData &state, int const PTUnitCompIndex, int const PTUnitType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for mixer return air node

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // Using/Aliasing
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;
    using MixedAir::GetOAMixerReturnNodeNumber;

    // Return value
    int GetPTUnitReturnAirNode;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum(0);

    if (state.dataPTHP->GetPTUnitInputFlag) {
        GetPTUnit(state);
        state.dataPTHP->GetPTUnitInputFlag = false;
    }

    // PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
    // objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
    // is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
    // objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
    // So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
    // which was recalculated for total number of all three object type for use in PT data structure.

    {
        auto const SELECT_CASE_var(PTUnitType);
        if (SELECT_CASE_var == PkgTermHPAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex;
        } else if (SELECT_CASE_var == PkgTermACAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP;
        } else if (SELECT_CASE_var == PkgTermHPWaterToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
        } else {
            assert(false);
        }
    }

    if (PTUnitNum > 0 && PTUnitNum <= state.dataPTHP->NumPTUs) {
        if (state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex > 0) {
            GetPTUnitReturnAirNode = GetOAMixerReturnNodeNumber(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex);
        } else {
            GetPTUnitReturnAirNode = 0;
        }
    } else {
        GetPTUnitReturnAirNode = 0;
    }

    return GetPTUnitReturnAirNode;
}

int GetPTUnitMixedAirNode(EnergyPlusData &state, int const PTUnitCompIndex, int const PTUnitType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for mixed air node

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // Using/Aliasing
    using DataZoneEquipment::PkgTermACAirToAir_Num;
    using DataZoneEquipment::PkgTermHPAirToAir_Num;
    using DataZoneEquipment::PkgTermHPWaterToAir_Num;
    using MixedAir::GetOAMixerMixedNodeNumber;

    // Return value
    int GetPTUnitMixedAirNode;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PTUnitNum(0);

    if (state.dataPTHP->GetPTUnitInputFlag) {
        GetPTUnit(state);
        state.dataPTHP->GetPTUnitInputFlag = false;
    }

    // PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
    // objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
    // is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
    // objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
    // So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
    // which was recalculated for total number of all three object type for use in PT data structure.

    {
        auto const SELECT_CASE_var(PTUnitType);
        if (SELECT_CASE_var == PkgTermHPAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex;
        } else if (SELECT_CASE_var == PkgTermACAirToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP;
        } else if (SELECT_CASE_var == PkgTermHPWaterToAir_Num) {
            PTUnitNum = PTUnitCompIndex + state.dataPTHP->NumPTHP + state.dataPTHP->NumPTAC;
        } else {
            assert(false);
        }
    }

    if (PTUnitNum > 0 && PTUnitNum <= state.dataPTHP->NumPTUs) {
        if (state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex > 0) {
            GetPTUnitMixedAirNode = GetOAMixerMixedNodeNumber(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex);
        } else {
            GetPTUnitMixedAirNode = 0;
        }
    } else {
        GetPTUnitMixedAirNode = 0;
    }

    return GetPTUnitMixedAirNode;
}

//******************************************************************************

void SimVariableSpeedHP(EnergyPlusData &state,
                        int const PTUnitNum,           // number of the current engine driven Heat Pump being simulated
                        int const ZoneNum,             // Controlled zone number
                        bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                        Real64 const QZnReq,           // required zone load
                        Real64 const QLatReq,          // required latent load
                        Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                        int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                        bool const HXUnitOn            // flag to enable heat exchanger
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
    //       DATE WRITTEN   March, 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulate a multispeed heat pump; adjust its output to match the
    // required system load.

    // METHODOLOGY EMPLOYED:
    // Calls ControlMSHPOutput to obtain the desired unit output

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;

    // Locals
    Real64 SupHeaterLoad; // supplement heater load

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PartLoadFrac(0.0);   // compressor part load fraction
    Real64 SpeedRatio(0.0);     // compressor speed ratio
    bool UnitOn;                // TRUE if unit is on
    int OutletNode;             // MSHP air outlet node
    int InletNode;              // MSHP air inlet node
    Real64 AirMassFlow;         // air mass flow rate [kg/s]
    Real64 QTotUnitOut;         // capacity output
    int SpeedNum(1);            // Speed number
    Real64 QSensUnitOut;        // sensible capacity output
    Real64 QLatUnitOut;         // latent capacity output
    int CompOp;                 // compressor operation; 1=on, 0=off
    Real64 TotalZoneLatentLoad; // Total ZONE heating load (not including outside air)

    // zero the fan, DX coils, and supplemental electric heater electricity consumption

    state.dataHVACGlobal->DXElecHeatingPower = 0.0;
    state.dataHVACGlobal->DXElecCoolingPower = 0.0;
    state.dataPTHP->SaveCompressorPLR = 0.0;
    state.dataHVACGlobal->ElecHeatingCoilPower = 0.0;
    state.dataHVACGlobal->SuppHeatingCoilPower = 0.0;

    // initialize local variables
    UnitOn = true;
    CompOp = 1;
    OutletNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    AirMassFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxCoolAirMassFlow;

    // Set latent load for heating
    if (state.dataPTHP->HeatingLoad) {
        TotalZoneLatentLoad = 0.0;
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::HeatingMode;
        // Set latent load for cooling and no sensible load condition
    } else {
        TotalZoneLatentLoad = QLatReq;
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::CoolingMode;
    }

    if (state.dataPTHP->HeatingLoad) {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::HeatingMode;
    } else if (state.dataPTHP->CoolingLoad) {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::CoolingMode;
    } else {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::Unassigned;
    }

    // set the on/off flags
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == CycFanCycCoil) {
        // cycling unit only runs if there is a cooling or heating load.
        if (std::abs(QZnReq) < SmallLoad || AirMassFlow < SmallMassFlow || state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            UnitOn = false;
        }
    } else if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        // continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
        if (AirMassFlow < SmallMassFlow) {
            UnitOn = false;
        }
    }

    state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;

    // compressor on
    CompOp = On;
    ControlVSHPOutput(state,
                      PTUnitNum,
                      FirstHVACIteration,
                      CompOp,
                      OpMode,
                      QZnReq,
                      TotalZoneLatentLoad,
                      ZoneNum,
                      SpeedNum,
                      SpeedRatio,
                      PartLoadFrac,
                      OnOffAirFlowRatio,
                      SupHeaterLoad,
                      HXUnitOn);

    if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
        state.dataPTHP->SaveCompressorPLR = PartLoadFrac;
    } else {
        if (SpeedNum > 1) {
            state.dataPTHP->SaveCompressorPLR = 1.0;
        }

        if (PartLoadFrac == 1.0 && state.dataPTHP->SaveCompressorPLR < 1.0) {
            PartLoadFrac = state.dataPTHP->SaveCompressorPLR;
        }
    }

    CalcVarSpeedHeatPump(state,
                         PTUnitNum,
                         ZoneNum,
                         FirstHVACIteration,
                         CompOp,
                         SpeedNum,
                         SpeedRatio,
                         PartLoadFrac,
                         QSensUnitOut,
                         QLatUnitOut,
                         QZnReq,
                         TotalZoneLatentLoad,
                         OnOffAirFlowRatio,
                         SupHeaterLoad,
                         HXUnitOn);

    // calculate delivered capacity
    AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

    QTotUnitOut = AirMassFlow * (state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);

    state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = AirMassFlow;

    // report variables
    state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = state.dataPTHP->SaveCompressorPLR;
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == CycFanCycCoil) {
        if (SupHeaterLoad > 0.0) {
            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0;
        } else {
            if (SpeedNum < 2) {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = PartLoadFrac;
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0;
            }
        }
    } else {
        if (UnitOn) {
            state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0;
        } else {
            if (SpeedNum < 2) {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = PartLoadFrac;
            } else {
                state.dataPTHP->PTUnit(PTUnitNum).FanPartLoadRatio = 1.0;
            }
        }
    }
}

//******************************************************************************
//******************************************************************************

void ControlVSHPOutput(EnergyPlusData &state,
                       int const PTUnitNum,           // Unit index in fan coil array
                       bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                       int const CompOp,              // compressor operation; 1=on, 0=off
                       int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                       Real64 const QZnReq,           // cooling or heating output needed by zone [W]
                       Real64 const QLatReq,          // latent cooling output needed by zone [W]
                       int const ZoneNum,             // Index to zone number
                       int &SpeedNum,                 // Speed number
                       Real64 &SpeedRatio,            // unit speed ratio for DX coils
                       Real64 &PartLoadFrac,          // unit part load fraction
                       Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                       Real64 &SupHeaterLoad,         // Supplemental heater load [W]
                       bool const HXUnitOn            // flag to enable heat exchanger
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:ControlMSHPOutput
    //       DATE WRITTEN   March,  2012
    //       MODIFIED       na
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // Determine the part load fraction at low speed, and speed ratio at high speed for this time step.

    // METHODOLOGY EMPLOYED:
    // Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

    using General::SolveRoot;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using PlantUtilities::SetComponentFlowRate;
    using Psychrometrics::PsyCpAirFnW;
    using SteamCoils::SimulateSteamCoilComponents;
    using WaterCoils::SimulateWaterCoilComponents;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const MaxIte(500); // maximum number of iterations

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FullOutput;                                                 // unit full output when compressor is operating [W]
    Real64 LowOutput;                                                  // unit full output at low speed [W]
    Real64 TempOutput;                                                 // unit output when iteration limit exceeded [W]
    Real64 NoCompOutput;                                               // output when no active compressor [W]
    Real64 LatOutput;                                                  // latent capacity output
    Real64 ErrorToler;                                                 // error tolerance
    int SolFla;                                                        // Flag of RegulaFalsi solver
    auto &ControlVSHPOutputPar = state.dataPTHP->ControlVSHPOutputPar; // Parameters passed to RegulaFalsi
    Real64 CpAir;                                                      // air specific heat
    int i;                                                             // Speed index
    auto &ErrCountCyc = state.dataPTHP->ErrCountCyc;                   // Counter used to minimize the occurrence of output warnings
    auto &ErrCountVar = state.dataPTHP->ErrCountVar;                   // Counter used to minimize the occurrence of output warnings
    Real64 mdot;                                                       // coil fluid mass flow rate (kg/s)

    SupHeaterLoad = 0.0;
    PartLoadFrac = 0.0;
    SpeedRatio = 0.0;
    SpeedNum = 1;
    LatOutput = 0.0;
    ErrorToler = 0.001; // Error tolerance for convergence from input deck

    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) == 0.0) return;

    // Get result when DX coil is off
    CalcVarSpeedHeatPump(state,
                         PTUnitNum,
                         ZoneNum,
                         FirstHVACIteration,
                         CompOp,
                         SpeedNum,
                         SpeedRatio,
                         PartLoadFrac,
                         NoCompOutput,
                         LatOutput,
                         QZnReq,
                         QLatReq,
                         OnOffAirFlowRatio,
                         SupHeaterLoad,
                         HXUnitOn);

    // If cooling and NoCompOutput < QZnReq, the coil needs to be off
    // If heating and NoCompOutput > QZnReq, the coil needs to be off
    if ((QZnReq < (-1.0 * SmallLoad) && NoCompOutput < QZnReq) || (QZnReq > SmallLoad && NoCompOutput > QZnReq) ||
        (std::abs(QZnReq) <= SmallLoad && std::abs(QLatReq) <= SmallLoad) || state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
        return;
    }

    // Get full load result
    PartLoadFrac = 1.0;
    SpeedRatio = 1.0;
    if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::HeatingMode) {
        if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
            SpeedNum = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling;
        } else {
            SpeedNum = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating;
        }
    } else if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::CoolingMode) {
        SpeedNum = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling;
    } else {
        SpeedNum = 1;
        PartLoadFrac = 0.0;
    }

    CalcVarSpeedHeatPump(state,
                         PTUnitNum,
                         ZoneNum,
                         FirstHVACIteration,
                         CompOp,
                         SpeedNum,
                         SpeedRatio,
                         PartLoadFrac,
                         FullOutput,
                         LatOutput,
                         QZnReq,
                         QLatReq,
                         OnOffAirFlowRatio,
                         SupHeaterLoad,
                         HXUnitOn);

    if (QLatReq < 0.0) { // dehumidification mode
        //  ! If the QLatReq <= LatOutput the unit needs to run full out
        if (QLatReq <= LatOutput) {
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = PartLoadFrac;
            state.dataPTHP->PTUnit(PTUnitNum).CompSpeedRatio = SpeedRatio;
            state.dataPTHP->PTUnit(PTUnitNum).CompSpeedNum = SpeedNum;
            return;
        }
        ErrorToler = 0.001; // Error tolerance for convergence from input deck
    } else if (QZnReq < (-1.0 * SmallLoad) && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
        // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
        // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
        if (FullOutput >= 0.0 || FullOutput >= NoCompOutput) {
            PartLoadFrac = 0.0;
            SpeedRatio = 0.0;
            SpeedNum = 1;
            return;
        }
        //  ! If the QZnReq <= FullOutput the unit needs to run full out
        if (QZnReq <= FullOutput) {
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = PartLoadFrac;
            state.dataPTHP->PTUnit(PTUnitNum).CompSpeedRatio = SpeedRatio;
            state.dataPTHP->PTUnit(PTUnitNum).CompSpeedNum = SpeedNum;
            return;
        }
        ErrorToler = 0.001; // Error tolerance for convergence from input deck
    } else if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
        // Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
        // Check that this is the case; if not set PartLoadFrac = 0.0 (off)
        if (FullOutput <= 0.0 || FullOutput <= NoCompOutput) {
            PartLoadFrac = 0.0;
            SpeedRatio = 0.0;
            SpeedNum = 1;
            // may need supplemental heating so don't return in heating mode
        }
        if (QZnReq >= FullOutput) {
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            // may need supplemental heating so don't return in heating mode
        }
        ErrorToler = 0.001; // Error tolerance for convergence from input deck
    } else {                // no load
        PartLoadFrac = 0.0;
        SpeedRatio = 0.0;
        SpeedNum = 1;
    }

    // Calculate the part load fraction
    if (((QZnReq > SmallLoad && QZnReq < FullOutput) || (QZnReq < (-1.0 * SmallLoad) && QZnReq > FullOutput) || (QLatReq < (-1.0 * SmallLoad))) &&
        !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {

        ControlVSHPOutputPar(1) = PTUnitNum;
        ControlVSHPOutputPar(2) = ZoneNum;
        if (FirstHVACIteration) {
            ControlVSHPOutputPar(3) = 1.0;
        } else {
            ControlVSHPOutputPar(3) = 0.0;
        }
        ControlVSHPOutputPar(4) = OpMode;
        ControlVSHPOutputPar(5) = QZnReq;
        ControlVSHPOutputPar(6) = OnOffAirFlowRatio;
        ControlVSHPOutputPar(7) = SupHeaterLoad;
        ControlVSHPOutputPar(9) = CompOp;
        ControlVSHPOutputPar(10) = 1.0;
        if (HXUnitOn) {
            ControlVSHPOutputPar(11) = 1.0;
        } else {
            ControlVSHPOutputPar(11) = 0.0;
        }
        // Check whether the low speed coil can meet the load or not
        CalcVarSpeedHeatPump(state,
                             PTUnitNum,
                             ZoneNum,
                             FirstHVACIteration,
                             CompOp,
                             1,
                             0.0,
                             1.0,
                             LowOutput,
                             LatOutput,
                             QZnReq,
                             QLatReq,
                             OnOffAirFlowRatio,
                             SupHeaterLoad,
                             HXUnitOn);
        if (((QZnReq > SmallLoad && QZnReq <= LowOutput) || (QZnReq < (-1.0 * SmallLoad) && QZnReq >= LowOutput) ||
             (QLatReq < (-1.0 * SmallLoad) && QLatReq > LatOutput)) &&
            !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            SpeedRatio = 0.0;
            SpeedNum = 1;

            if (QLatReq < 0.0) { // calculate latent heat residual
                ControlVSHPOutputPar(10) = 0.0;
                ControlVSHPOutputPar(5) = QLatReq;
            }

            SolveRoot(state, ErrorToler, MaxIte, SolFla, PartLoadFrac, VSHPCyclingResidual, 0.0, 1.0, ControlVSHPOutputPar);
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (ErrCountCyc == 0) {
                        ++ErrCountCyc;
                        ShowWarningError(state,
                                         "Iteration limit exceeded calculating VS WSHP unit cycling ratio, for unit=" +
                                             state.dataPTHP->PTUnit(PTUnitNum).Name);
                        ShowContinueErrorTimeStamp(state, format("Cycling ratio returned={:.2R}", PartLoadFrac));
                    } else {
                        ++ErrCountCyc;
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                           "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...",
                                                       state.dataPTHP->PTUnit(PTUnitNum).ErrIndexCyc,
                                                       PartLoadFrac,
                                                       PartLoadFrac);
                    }
                }
            } else if (SolFla == -2) {
                ShowFatalError(state,
                               "VS WSHP unit cycling ratio calculation failed: cycling limits exceeded, for unit=" +
                                   state.dataPTHP->PTUnit(PTUnitNum).Name);
            }
        } else {
            // Check to see which speed to meet the load
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            // Cooling
            if (((QZnReq < (-1.0 * SmallLoad)) || (QLatReq < (-1.0 * SmallLoad))) && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                for (i = 2; i <= state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling; ++i) {
                    CalcVarSpeedHeatPump(state,
                                         PTUnitNum,
                                         ZoneNum,
                                         FirstHVACIteration,
                                         CompOp,
                                         i,
                                         SpeedRatio,
                                         PartLoadFrac,
                                         TempOutput,
                                         LatOutput,
                                         QZnReq,
                                         QLatReq,
                                         OnOffAirFlowRatio,
                                         SupHeaterLoad,
                                         HXUnitOn);

                    if (QLatReq < 0.0) {
                        if (QLatReq > LatOutput) {
                            SpeedNum = i;
                            break;
                        }
                    } else if (QZnReq >= TempOutput) {
                        SpeedNum = i;
                        break;
                    }
                }
            } else {
                for (i = 2; i <= state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating; ++i) {
                    CalcVarSpeedHeatPump(state,
                                         PTUnitNum,
                                         ZoneNum,
                                         FirstHVACIteration,
                                         CompOp,
                                         i,
                                         SpeedRatio,
                                         PartLoadFrac,
                                         TempOutput,
                                         LatOutput,
                                         QZnReq,
                                         QLatReq,
                                         OnOffAirFlowRatio,
                                         SupHeaterLoad,
                                         HXUnitOn);
                    if (QZnReq <= TempOutput) {
                        SpeedNum = i;
                        break;
                    }
                }
            }
            ControlVSHPOutputPar(8) = SpeedNum;

            if (QLatReq < (-1.0 * SmallLoad)) { // calculate latent heat residual
                ControlVSHPOutputPar(10) = 0.0;
                ControlVSHPOutputPar(5) = QLatReq;
            }

            SolveRoot(state, ErrorToler, MaxIte, SolFla, SpeedRatio, VSHPSpeedResidual, 1.0e-10, 1.0, ControlVSHPOutputPar);
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (ErrCountVar == 0) {
                        ++ErrCountVar;
                        ShowWarningError(state,
                                         "Iteration limit exceeded calculating VS WSHP unit speed ratio, for unit=" +
                                             state.dataPTHP->PTUnit(PTUnitNum).Name);
                        ShowContinueErrorTimeStamp(state, format("Speed ratio returned=[{:.2R}], Speed number ={}", SpeedRatio, SpeedNum));
                    } else {
                        ++ErrCountVar;
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                           "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...",
                                                       state.dataPTHP->PTUnit(PTUnitNum).ErrIndexVar,
                                                       SpeedRatio,
                                                       SpeedRatio);
                    }
                }
            } else if (SolFla == -2) {
                ShowFatalError(state,
                               "VS WSHP unit compressor speed calculation failed: speed limits exceeded, for unit=" +
                                   state.dataPTHP->PTUnit(PTUnitNum).Name);
            }
        }
    }

    // if the DX heating coil cannot meet the load, trim with supplemental heater
    // occurs with constant fan mode when compressor is on or off
    // occurs with cycling fan mode when compressor PLR is equal to 1
    if (state.dataPTHP->HeatingLoad && QZnReq > FullOutput && state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        PartLoadFrac = 1.0;
        SpeedRatio = 1.0;

        if (state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating > 0)
            SpeedNum = state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedHeating; // maximum heating speed, avoid zero

        if (state.dataEnvrn->OutDryBulbTemp <= state.dataPTHP->PTUnit(PTUnitNum).MaxOATSupHeat) {
            SupHeaterLoad = QZnReq - FullOutput;
        } else {
            SupHeaterLoad = 0.0;
        }
        CalcVarSpeedHeatPump(state,
                             PTUnitNum,
                             ZoneNum,
                             FirstHVACIteration,
                             CompOp,
                             SpeedNum,
                             SpeedRatio,
                             PartLoadFrac,
                             TempOutput,
                             LatOutput,
                             QZnReq,
                             QLatReq,
                             OnOffAirFlowRatio,
                             SupHeaterLoad,
                             HXUnitOn);
    }

    // check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
    if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        if (state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp > state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat &&
            SupHeaterLoad > 0.0) {

            // If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
            SupHeaterLoad = 0.0;
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                  _,
                                                  true);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                                PartLoadFrac);
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad);
                }
            }

            //     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
            //     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
            //     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
            //     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
            if (state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp < state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat) {
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).HumRat);
                SupHeaterLoad =
                    state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirInNode).MassFlowRate * CpAir *
                    (state.dataPTHP->PTUnit(PTUnitNum).MaxSATSupHeat - state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).AirOutNode).Temp);

            } else {
                SupHeaterLoad = 0.0;
            }
        }
    }

    state.dataPTHP->PTUnit(PTUnitNum).CompPartLoadRatio = PartLoadFrac;
    state.dataPTHP->PTUnit(PTUnitNum).CompSpeedRatio = SpeedRatio;
    state.dataPTHP->PTUnit(PTUnitNum).CompSpeedNum = SpeedNum;
}

//******************************************************************************

//******************************************************************************

Real64 VSHPCyclingResidual(EnergyPlusData &state,
                           Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                           Array1D<Real64> const &Par // par(1) = FurnaceNum
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:MSHPCyclingResidual
    //       DATE WRITTEN   March, 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
    //  MSHP output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls CalcMSHeatPump to get ActualOutput at the given part load ratio
    //  and calculates the residual as defined above

    // REFERENCES:

    // USE STATEMENTS:
    // na

    // Return value
    Real64 VSHPCyclingResidual;

    // Argument array dimensioning

    // Locals
    Real64 SupHeaterLoad; // Supplemental heater load

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = Zone Num
    // par(3) = FirstHVACIteration
    // par(4) = OpMode
    // par(5) = QZnReq, load to be met
    // par(6) = OnOffAirFlowRatio
    // par(7) = SupHeaterLoad

    // par(9) = CompOp
    // par(10) = 1.0 to meet sensible load

    // FUNCTION PARAMETER DEFINITIONS:
    //  na

    // INTERFACE BLOCK SPECIFICATIONS
    //  na

    // DERIVED TYPE DEFINITIONS
    //  na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool HXUnitOn;            // flag to enable heat exchanger
    int PTUnitNum;            // MSHP index
    int ZoneNum;              // Zone index
    bool FirstHVACIteration;  // FirstHVACIteration flag
    int OpMode;               // Compressor operating mode
    Real64 QZnReq;            // zone sensible load (W)
    Real64 QZnLat;            // zone latent load (W)
    Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
    Real64 ZoneSensLoadMet;   // delivered sensible capacity of MSHP
    Real64 ZoneLatLoadMet;    // delivered latent capacity of MSHP
    Real64 LoadToBeMet;       // sensible or latent load to be met
    Real64 ResScale;          // Residual scale
    int CompOp;               // compressor operation; 1=on, 0=off

    PTUnitNum = int(Par(1));
    ZoneNum = int(Par(2));
    // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
    FirstHVACIteration = (Par(3) == 1.0);
    OpMode = int(Par(4));

    QZnReq = 0.0;
    QZnLat = 0.0;

    LoadToBeMet = Par(5);
    if (Par(10) == 1.0) {
        QZnReq = Par(5);
    } else {
        QZnLat = Par(5);
    }

    OnOffAirFlowRatio = Par(6);
    SupHeaterLoad = Par(7);
    CompOp = int(Par(9));

    HXUnitOn = (Par(11) > 0.0);

    CalcVarSpeedHeatPump(state,
                         PTUnitNum,
                         ZoneNum,
                         FirstHVACIteration,
                         CompOp,
                         1,
                         0.0,
                         PartLoadFrac,
                         ZoneSensLoadMet,
                         ZoneLatLoadMet,
                         QZnReq,
                         QZnLat,
                         OnOffAirFlowRatio,
                         SupHeaterLoad,
                         HXUnitOn);

    ResScale = std::abs(LoadToBeMet);
    if (ResScale < 100.0) {
        ResScale = 100.0;
    }

    // Calculate residual based on output calculation flag
    if (Par(10) == 1.0) {
        VSHPCyclingResidual = (ZoneSensLoadMet - LoadToBeMet) / ResScale;
    } else {
        VSHPCyclingResidual = (ZoneLatLoadMet - LoadToBeMet) / ResScale;
    }

    return VSHPCyclingResidual;
}

//******************************************************************************

Real64 VSHPSpeedResidual(EnergyPlusData &state,
                         Real64 const SpeedRatio,   // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                         Array1D<Real64> const &Par // par(1) = MSHPNum
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen, , based on HVACMultiSpeedHeatPump:MSHPVarSpeedgResidual
    //       DATE WRITTEN   March, 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
    //  MSHP output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls CalcMSHeatPump to get ActualOutput at the given speed ratio (partload ratio for high speed)
    //  and calculates the residual as defined above

    // REFERENCES:

    // USE STATEMENTS:
    // na

    // Return value
    Real64 VSHPSpeedResidual;

    // Argument array dimensioning

    // Locals
    Real64 SupHeaterLoad; // Supplemental heater load

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = Zone Num
    // par(3) = FirstHVACIteration
    // par(4) = OpMode
    // par(5) = QZnReq
    // par(6) = OnOffAirFlowRatio
    // par(7) = SupHeaterLoad
    // par(8) = SpeedNum
    // par(9) = CompOp
    // par(10) = 1.0 to meet sensible load

    // FUNCTION PARAMETER DEFINITIONS:
    //  na

    // INTERFACE BLOCK SPECIFICATIONS
    //  na

    // DERIVED TYPE DEFINITIONS
    //  na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool HXUnitOn;            // flag to enable heat exchanger
    int PTUnitNum;            // MSHP index
    int ZoneNum;              // Zone index
    bool FirstHVACIteration;  // FirstHVACIteration flag
    int OpMode;               // Compressor operating mode
    Real64 QZnReq;            // zone load (W)
    Real64 QZnLat;            // zone latent load (W)
    Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
    Real64 ZoneSensLoadMet;   // delivered sensible capacity of MSHP
    Real64 ZoneLatLoadMet;    // delivered latent capacity of MSHP
    Real64 LoadToBeMet;       // sensible or latent load to be met
    Real64 ResScale;          // Residual scale
    int SpeedNum;             // Speed number
    int CompOp;               // compressor operation; 1=on, 0=off

    PTUnitNum = int(Par(1));
    ZoneNum = int(Par(2));
    // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
    FirstHVACIteration = (Par(3) == 1.0);
    OpMode = int(Par(4));

    QZnReq = 0.0;
    QZnLat = 0.0;

    LoadToBeMet = Par(5);
    if (Par(10) == 1.0) {
        QZnReq = Par(5);
    } else {
        QZnLat = Par(5);
    }

    OnOffAirFlowRatio = Par(6);
    SupHeaterLoad = Par(7);
    SpeedNum = int(Par(8));
    CompOp = int(Par(9));

    HXUnitOn = (Par(11) > 0.0);

    CalcVarSpeedHeatPump(state,
                         PTUnitNum,
                         ZoneNum,
                         FirstHVACIteration,
                         CompOp,
                         SpeedNum,
                         SpeedRatio,
                         1.0,
                         ZoneSensLoadMet,
                         ZoneLatLoadMet,
                         QZnReq,
                         QZnLat,
                         OnOffAirFlowRatio,
                         SupHeaterLoad,
                         HXUnitOn);

    ResScale = std::abs(LoadToBeMet);
    if (ResScale < 100.0) {
        ResScale = 100.0;
    }

    // Calculate residual based on output calculation flag
    if (Par(10) == 1.0) {
        VSHPSpeedResidual = (ZoneSensLoadMet - LoadToBeMet) / ResScale;
    } else {
        VSHPSpeedResidual = (ZoneLatLoadMet - LoadToBeMet) / ResScale;
    }

    return VSHPSpeedResidual;
}

//******************************************************************************

void CalcVarSpeedHeatPump(EnergyPlusData &state,
                          int const PTUnitNum,                 // Unit index in fan coil array
                          int const ZoneNum,                   // Zone index
                          bool const FirstHVACIteration,       // flag for 1st HVAC iteration in the time step
                          int const CompOp,                    // Compressor on/off; 1=on, 0=off
                          int const SpeedNum,                  // Speed number
                          Real64 const SpeedRatio,             // Compressor speed ratio
                          Real64 const PartLoadFrac,           // compressor part load fraction
                          Real64 &LoadMet,                     // load met by unit (W)
                          Real64 &LatentLoadMet,               // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
                          Real64 const QZnReq,                 // Zone load (W) unused1208
                          Real64 const QLatReq,                // Zone latent load []
                          Real64 &OnOffAirFlowRatio,           // ratio of compressor ON airflow to AVERAGE airflow over timestep
                          Real64 &SupHeaterLoad,               // supplemental heater load (W)
                          [[maybe_unused]] bool const HXUnitOn // flag to enable heat exchanger
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
    //       DATE WRITTEN:    March 2012
    //       MODIFIED         July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    //  This routine will calcultes MSHP performance based on given system load

    // Using/Aliasing
    using DXCoils::SimDXCoil;

    using General::SolveRoot;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
    using MixedAir::SimOAMixer;
    using PlantUtilities::SetComponentFlowRate;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using SingleDuct::SimATMixer;
    using SteamCoils::SimulateSteamCoilComponents;
    using VariableSpeedCoils::SimVariableSpeedCoils;
    using WaterCoils::SimulateWaterCoilComponents;
    using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcVarSpeedHeatPump: "); // for error messages
    Real64 const ErrTolerance(0.001);                               // convergence limit for hotwater coil
    int const SolveMaxIter(50);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;            // PTHP air outlet node
    int InletNode;             // PTHP air inlet node
    int ZoneNode;              // Zone node of zone PTHP is serving
    int ControlledZoneNum;     // Index of ZoneEquipConfig that uses this heat pump
    Real64 AirMassFlow;        // total supply air mass flow through the PTHP [m3/s]
    Real64 MinHumRat;          // minimum humidity ratio for sensible capacity calculation (kg/kg)
    Real64 OutsideDryBulbTemp; // Outdoor air temperature at external node height
    Real64 QCoilReq;           // load passed to heating coil (W)
    Real64 QActual;            // actual heating coil output (W)
    int OpMode;                // fan operating mode, CycFanCycCoil or ContFanCycCoil
    Real64 mdot;               // local temporary for mass flow rate
    Real64 MaxHotWaterFlow;    // coil maximum hot water mass flow rate, kg/s
    Real64 HotWaterMdot;       // actual hot water mass flow rate
    auto &CalcVarSpeedHeatPumpPar = state.dataPTHP->CalcVarSpeedHeatPumpPar;
    int SolFlag;
    int ATMixOutNode(0); // outlet node of ATM Mixer

    OutletNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;
    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    ControlledZoneNum = state.dataPTHP->PTUnit(PTUnitNum).ControlZoneNum;
    ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
    OpMode = state.dataPTHP->PTUnit(PTUnitNum).OpMode;

    OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;

    state.dataPTHP->SaveCompressorPLR = 0.0;
    // Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
    SetVSHPAirFlow(state, PTUnitNum, ZoneNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio);

    AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        // There is an air terminal mixer
        ATMixOutNode = state.dataPTHP->PTUnit(PTUnitNum).ATMixerOutNode;
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_InletSide) { // if there is an inlet side air terminal mixer
            // set the primary air inlet mass flow rate
            state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode).MassFlowRate =
                min(state.dataLoopNodes->Node(state.dataPTHP->PTUnit(PTUnitNum).ATMixerPriNode).MassFlowRateMaxAvail,
                    state.dataLoopNodes->Node(InletNode).MassFlowRate);
            // now calculate the mixer outlet conditions (and the secondary air inlet flow rate)
            // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
            SimATMixer(state, state.dataPTHP->PTUnit(PTUnitNum).ATMixerName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex);
        }
    } else {
        // No air terminal mixer; simulate the outside air mixer
        ATMixOutNode = 0;
        if (state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode > 0)
            SimOAMixer(state, state.dataPTHP->PTUnit(PTUnitNum).OAMixName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).OAMixIndex);
    }

    // if blow through, simulate fan then coils
    if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == BlowThru) {
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        } else {
            Fans::SimulateFanComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanIndex,
                                        state.dataPTHP->FanSpeedRatio,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        }
    }

    if (state.dataPTHP->CoolingLoad && OutsideDryBulbTemp > state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling) {

        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                              state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                              state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                              CompOp,
                              PartLoadFrac,
                              SpeedNum,
                              SpeedRatio,
                              QZnReq,
                              QLatReq,
                              OnOffAirFlowRatio);

        state.dataPTHP->SaveCompressorPLR = PartLoadFrac;
    } else { // cooling coil is off
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum,
                              state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                              state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                              state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                              state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRatio);
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num != iPTHPType::PTACUnit) { // PTHP
        if (state.dataPTHP->HeatingLoad) {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                  state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                  state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                  state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  QZnReq,
                                  QLatReq,
                                  OnOffAirFlowRatio);

            state.dataPTHP->SaveCompressorPLR = PartLoadFrac;
        } else {
            //   heating coil is off
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilIndexNum,
                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                  state.dataPTHP->PTUnit(PTUnitNum).MaxONOFFCyclesperHour,
                                  state.dataPTHP->PTUnit(PTUnitNum).HPTimeConstant,
                                  state.dataPTHP->PTUnit(PTUnitNum).FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRatio);
        }
    } else { // PTAC
        if (state.dataPTHP->HeatingLoad) {
            if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
                QCoilReq = state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilCap * PartLoadFrac;
                if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingGasOrOtherFuel ||
                    state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingElectric) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                  FirstHVACIteration,
                                                  QCoilReq,
                                                  state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                                  QActual,
                                                  false,
                                                  OpMode,
                                                  PartLoadFrac);
                } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                    //       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow * PartLoadFrac;

                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                                PartLoadFrac);
                } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                    //       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxHeatCoilFluidFlow * PartLoadFrac;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                                QCoilReq,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                                PartLoadFrac);
                }
            }
        } else {
            //   heating coil is off
            if (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit) {
                QCoilReq = 0.0;
                if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingGasOrOtherFuel ||
                    state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingElectric) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                  FirstHVACIteration,
                                                  QCoilReq,
                                                  state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex);
                } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex);
                } else if (state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilType_Num == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).HeatCoilCompNum);

                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).ACHeatCoilIndex,
                                                QCoilReq,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode,
                                                PartLoadFrac);
                }
            }
        }
    }

    // if draw through, simulate coils then fan
    if (state.dataPTHP->PTUnit(PTUnitNum).FanPlace == DrawThru) {
        if (state.dataPTHP->PTUnit(PTUnitNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[state.dataPTHP->PTUnit(PTUnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        } else {
            Fans::SimulateFanComponents(state,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanName,
                                        FirstHVACIteration,
                                        state.dataPTHP->PTUnit(PTUnitNum).FanIndex,
                                        state.dataPTHP->FanSpeedRatio,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        }
    }

    if (state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex > 0) {
        if (SupHeaterLoad < SmallLoad) {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                  QActual,
                                                  true,
                                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                }
            }
        } else {
            {
                auto const SELECT_CASE_var(state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                  FirstHVACIteration,
                                                  SupHeaterLoad,
                                                  state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                  QActual,
                                                  true,
                                                  state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    MaxHotWaterFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         MaxHotWaterFlow,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);
                    QActual = SupHeaterLoad;
                    // simulate the hot water supplemental heating coil
                    SimulateWaterCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                    if (QActual > (SupHeaterLoad + SmallLoad)) {
                        // control water flow to obtain output matching SupHeaterLoad
                        SolFlag = 0;
                        state.dataPTHP->MinWaterFlow = 0.0;
                        CalcVarSpeedHeatPumpPar(1) = double(PTUnitNum);
                        if (FirstHVACIteration) {
                            CalcVarSpeedHeatPumpPar(2) = 1.0;
                        } else {
                            CalcVarSpeedHeatPumpPar(2) = 0.0;
                        }
                        CalcVarSpeedHeatPumpPar(3) = SupHeaterLoad;
                        MaxHotWaterFlow = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                        SolveRoot(state,
                                  ErrTolerance,
                                  SolveMaxIter,
                                  SolFlag,
                                  HotWaterMdot,
                                  HotWaterCoilResidual,
                                  state.dataPTHP->MinWaterFlow,
                                  MaxHotWaterFlow,
                                  CalcVarSpeedHeatPumpPar);
                        if (SolFlag == -1) {
                            if (state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex == 0) {
                                ShowWarningMessage(state,
                                                   "RoutineName//Hot water coil control failed for " + state.dataPTHP->PTUnit(PTUnitNum).UnitType +
                                                       "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                       "\""); // Autodesk:Bug? Meant std::string{RoutineName} + "Hot water...
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("  Iteration limit [{}] exceeded in calculating hot water mass flow rate", SolveMaxIter));
                            }
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("RoutineName//Hot water coil control failed (iteration limit [{}]) for {}=\"{}",
                                       SolveMaxIter,
                                       state.dataPTHP->PTUnit(PTUnitNum).UnitType,
                                       state.dataPTHP->PTUnit(PTUnitNum).Name),
                                state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex); // Autodesk:Bug? Meant std::string{RoutineName} + "Hot water...
                        } else if (SolFlag == -2) {
                            if (state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex2 == 0) {
                                ShowWarningMessage(state,
                                                   "RoutineName//Hot water coil control failed (maximum flow limits) for " +
                                                       state.dataPTHP->PTUnit(PTUnitNum).UnitType + "=\"" + state.dataPTHP->PTUnit(PTUnitNum).Name +
                                                       "\""); // Autodesk:Bug? Meant std::string{RoutineName} + "Hot water...
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state, "...Bad hot water maximum flow rate limits");
                                ShowContinueError(state, format("...Given minimum water flow rate={:.3R} kg/s", state.dataPTHP->MinWaterFlow));
                                ShowContinueError(state, format("...Given maximum water flow rate={:.3R} kg/s", MaxHotWaterFlow));
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "RoutineName//Hot water coil control failed (flow limits) for " +
                                                               state.dataPTHP->PTUnit(PTUnitNum).UnitType + "=\"" +
                                                               state.dataPTHP->PTUnit(PTUnitNum).Name + "\"",
                                                           state.dataPTHP->PTUnit(PTUnitNum).HotWaterCoilMaxIterIndex2,
                                                           MaxHotWaterFlow,
                                                           state.dataPTHP->MinWaterFlow,
                                                           _,
                                                           "[kg/s]",
                                                           "[kg/s]"); // Autodesk:Bug? Meant std::string{RoutineName} + "Hot water...
                        }
                        QActual = SupHeaterLoad;
                        // simulate the hot water supplemental heating coil
                        SimulateWaterCoilComponents(state,
                                                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                    FirstHVACIteration,
                                                    state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                    QActual,
                                                    state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                    }
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = state.dataPTHP->PTUnit(PTUnitNum).MaxSuppCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilFluidInletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).PlantCoilOutletNode,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilLoopSide,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilBranchNum,
                                         state.dataPTHP->PTUnit(PTUnitNum).SuppCoilCompNum);

                    // simulate the steam supplemental heating coil
                    SimulateSteamCoilComponents(state,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilName,
                                                FirstHVACIteration,
                                                state.dataPTHP->PTUnit(PTUnitNum).SuppHeatCoilIndex,
                                                SupHeaterLoad,
                                                QActual,
                                                state.dataPTHP->PTUnit(PTUnitNum).OpMode);
                }
            }
        }
    }

    // If there is a supply side air terminal mixer, calculate its output
    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            SimATMixer(state, state.dataPTHP->PTUnit(PTUnitNum).ATMixerName, FirstHVACIteration, state.dataPTHP->PTUnit(PTUnitNum).ATMixerIndex);
        }
    }

    // calculate sensible load met
    if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerExists) {
        if (state.dataPTHP->PTUnit(PTUnitNum).ATMixerType == ATMixer_SupplySide) {
            // Air terminal supply side mixer
            LoadMet = state.dataLoopNodes->Node(ATMixOutNode).MassFlowRate *
                      (PsyHFnTdbW(state.dataLoopNodes->Node(ATMixOutNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                       PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
        } else {
            // Air terminal inlet side mixer
            LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                                     PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
        }
    } else {
        MinHumRat = min(state.dataLoopNodes->Node(InletNode).HumRat, state.dataLoopNodes->Node(OutletNode).HumRat);
        LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                 PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));
    }

    LatentLoadMet = 0.0;
}

void SetVSHPAirFlow(EnergyPlusData &state,
                    int const PTUnitNum,                // Unit index
                    [[maybe_unused]] int const ZoneNum, // Zone index
                    Real64 const PartLoadRatio,         // unit part load ratio
                    Real64 &OnOffAirFlowRatio,          // ratio of compressor ON airflow to average airflow over timestep
                    Optional_int_const SpeedNum,        // Speed number
                    Optional<Real64 const> SpeedRatio   // Speed ratio
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:SetAverageAirFlow
    //       DATE WRITTEN   March, 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na
    // PURPOSE OF THIS SUBROUTINE:
    // Set the average air mass flow rates using the part load fraction of the heat pump for this time step
    // Set OnOffAirFlowRatio to be used by DX coils

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;                   // inlet node number for PTUnitNum
    int OutsideAirNode;              // outside air node number in PTHP loop
    int AirRelNode;                  // relief air node number in PTHP loop
    Real64 AverageUnitMassFlow(0.0); // average supply air mass flow rate over time step
    Real64 AverageOAMassFlow(0.0);   // average outdoor air mass flow rate over time step

    auto &MSHPMassFlowRateHigh = state.dataHVACGlobal->MSHPMassFlowRateHigh;
    auto &MSHPMassFlowRateLow = state.dataHVACGlobal->MSHPMassFlowRateLow;

    MSHPMassFlowRateLow = 0.0;  // Mass flow rate at low speed
    MSHPMassFlowRateHigh = 0.0; // Mass flow rate at high speed

    InletNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    OutsideAirNode = state.dataPTHP->PTUnit(PTUnitNum).OutsideAirNode;
    AirRelNode = state.dataPTHP->PTUnit(PTUnitNum).AirReliefNode;

    AverageOAMassFlow = (PartLoadRatio * state.dataPTHP->OACompOnMassFlow) + ((1 - PartLoadRatio) * state.dataPTHP->OACompOffMassFlow);

    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate;
        state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio;
    } else {
        state.dataPTHP->CompOffMassFlow = 0.0;
        state.dataPTHP->CompOffFlowRatio = 0.0;
    }

    if (state.dataPTHP->HeatingLoad && (state.dataPTHP->PTUnit(PTUnitNum).UnitType_Num == iPTHPType::PTACUnit)) {
        state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling);
        state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling);
        MSHPMassFlowRateLow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling);
        MSHPMassFlowRateHigh = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(state.dataPTHP->PTUnit(PTUnitNum).NumOfSpeedCooling);
        AverageUnitMassFlow = (PartLoadRatio * state.dataPTHP->CompOnMassFlow) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffMassFlow);
        if (state.dataPTHP->CompOffFlowRatio > 0.0) {
            state.dataPTHP->FanSpeedRatio =
                (PartLoadRatio * state.dataPTHP->CompOnFlowRatio) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffFlowRatio);
        } else {
            state.dataPTHP->FanSpeedRatio = state.dataPTHP->CompOnFlowRatio;
        }
    } else {
        if (present(SpeedNum)) {
            if (state.dataPTHP->HeatingLoad) {
                if (SpeedNum == 1) {
                    state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum);
                    state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(SpeedNum);
                    MSHPMassFlowRateLow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(1);
                    MSHPMassFlowRateHigh = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(1);
                } else if (SpeedNum > 1) {
                    state.dataPTHP->CompOnMassFlow = SpeedRatio * state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum) +
                                                     (1.0 - SpeedRatio) * state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum - 1);
                    state.dataPTHP->CompOnFlowRatio = SpeedRatio * state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(SpeedNum) +
                                                      (1.0 - SpeedRatio) * state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(SpeedNum - 1);
                    MSHPMassFlowRateLow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum - 1);
                    MSHPMassFlowRateHigh = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum);
                }
            } else if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::CoolingMode) {
                if (SpeedNum == 1) {
                    state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum);
                    state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(SpeedNum);
                    MSHPMassFlowRateLow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1);
                    MSHPMassFlowRateHigh = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1);
                } else if (SpeedNum > 1) {
                    state.dataPTHP->CompOnMassFlow = SpeedRatio * state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum) +
                                                     (1.0 - SpeedRatio) * state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum - 1);
                    state.dataPTHP->CompOnFlowRatio = SpeedRatio * state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(SpeedNum) +
                                                      (1.0 - SpeedRatio) * state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(SpeedNum - 1);
                    MSHPMassFlowRateLow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum - 1);
                    MSHPMassFlowRateHigh = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum);
                }
            }
        }

        // Set up fan flow rate during compressor off time
        if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil && present(SpeedNum)) {
            if (state.dataPTHP->PTUnit(PTUnitNum).AirFlowControl == iAirflowCtrlMode::UseCompressorOnFlow && state.dataPTHP->CompOnMassFlow > 0.0) {
                if (SpeedNum == 1) { // LOWEST SPEED USE IDLE FLOW
                    state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate;
                    state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio;
                } else if (state.dataPTHP->PTUnit(PTUnitNum).LastMode == iCompMode::HeatingMode) {
                    state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(SpeedNum);
                    state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(SpeedNum);
                } else {
                    state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(SpeedNum);
                    state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(SpeedNum);
                }
            }
        }

        if (present(SpeedNum)) {
            if (SpeedNum > 1) {
                AverageUnitMassFlow = state.dataPTHP->CompOnMassFlow;
                state.dataPTHP->FanSpeedRatio = state.dataPTHP->CompOnFlowRatio;
            } else {
                AverageUnitMassFlow = (PartLoadRatio * state.dataPTHP->CompOnMassFlow) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffMassFlow);
                if (state.dataPTHP->CompOffFlowRatio > 0.0) {
                    state.dataPTHP->FanSpeedRatio =
                        (PartLoadRatio * state.dataPTHP->CompOnFlowRatio) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffFlowRatio);
                } else {
                    state.dataPTHP->FanSpeedRatio = state.dataPTHP->CompOnFlowRatio;
                }
            }
        } else {
            AverageUnitMassFlow = (PartLoadRatio * state.dataPTHP->CompOnMassFlow) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffMassFlow);
            if (state.dataPTHP->CompOffFlowRatio > 0.0) {
                state.dataPTHP->FanSpeedRatio =
                    (PartLoadRatio * state.dataPTHP->CompOnFlowRatio) + ((1 - PartLoadRatio) * state.dataPTHP->CompOffFlowRatio);
            } else {
                state.dataPTHP->FanSpeedRatio = state.dataPTHP->CompOnFlowRatio;
            }
        }
    }

    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).SchedPtr) > 0.0 &&
        ((GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr) > 0.0 || state.dataHVACGlobal->ZoneCompTurnFansOn) &&
         !state.dataHVACGlobal->ZoneCompTurnFansOff)) {

        state.dataLoopNodes->Node(InletNode).MassFlowRate = AverageUnitMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AverageUnitMassFlow;
        if (OutsideAirNode > 0) {
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = AverageOAMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = AverageOAMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = AverageOAMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = AverageOAMassFlow;
        }
        if (AverageUnitMassFlow > 0.0) {
            OnOffAirFlowRatio = state.dataPTHP->CompOnMassFlow / AverageUnitMassFlow;
        } else {
            OnOffAirFlowRatio = 0.0;
        }

    } else {

        state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
        if (OutsideAirNode > 0) {
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = 0.0;
        }
        OnOffAirFlowRatio = 0.0;
    }
}

void SetOnOffMassFlowRateVSCoil(EnergyPlusData &state,
                                int const PTUnitNum,                        // index to furnace
                                int const ZoneNum,                          // index to zone
                                bool const FirstHVACIteration,              // Flag for 1st HVAC iteration
                                [[maybe_unused]] int const AirLoopNum,      // index to air loop !unused1208
                                Real64 &OnOffAirFlowRatio,                  // ratio of coil on to coil off air flow rate
                                [[maybe_unused]] int const OpMode,          // fan operating mode
                                [[maybe_unused]] Real64 const QZnReq,       // sensible load to be met (W) !unused1208
                                [[maybe_unused]] Real64 const MoistureLoad, // moisture load to be met (W)
                                Real64 &PartLoadRatio                       // coil part-load ratio
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the Furnace Components.

    // METHODOLOGY EMPLOYED:
    // The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
    // in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
    // air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
    // based on PLR.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InNode;  // Inlet node number in MSHP loop
    int OutNode; // Outlet node number in MSHP loop

    InNode = state.dataPTHP->PTUnit(PTUnitNum).AirInNode;
    OutNode = state.dataPTHP->PTUnit(PTUnitNum).AirOutNode;

    SetOnOffMassFlowRate(state, PTUnitNum, PartLoadRatio, OnOffAirFlowRatio);
    // INITIALIZE FIXED SPEED FIRST, AND OVER-WRITE USING MUL-SPEED

    if (state.dataPTHP->CoolingLoad) {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::CoolingMode;
    } else if (state.dataPTHP->HeatingLoad) {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::HeatingMode;
    } else {
        state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode = iCompMode::Unassigned;
    }

    // Set the inlet node mass flow rate
    if (state.dataPTHP->PTUnit(PTUnitNum).OpMode == ContFanCycCoil) {
        // constant fan mode
        if ((state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::HeatingMode) &&
            !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(1);
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(1);
            state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::HeatingMode;
        } else if ((state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::CoolingMode) &&
                   !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1);
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(1);
            state.dataPTHP->PTUnit(PTUnitNum).LastMode = iCompMode::CoolingMode;
        } else {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate;
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio;
        }
        state.dataPTHP->CompOffMassFlow = state.dataPTHP->PTUnit(PTUnitNum).IdleMassFlowRate;
        state.dataPTHP->CompOffFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).IdleSpeedRatio;
    } else {
        // cycling fan mode
        if ((state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::HeatingMode) &&
            !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).HeatMassFlowRate(1);
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSHeatingSpeedRatio(1);
        } else if ((state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode == iCompMode::CoolingMode) &&
                   !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
            state.dataPTHP->CompOnMassFlow = state.dataPTHP->PTUnit(PTUnitNum).CoolMassFlowRate(1);
            state.dataPTHP->CompOnFlowRatio = state.dataPTHP->PTUnit(PTUnitNum).MSCoolingSpeedRatio(1);
        } else {
            state.dataPTHP->CompOnMassFlow = 0.0;
            state.dataPTHP->CompOnFlowRatio = 0.0;
        }
        state.dataPTHP->CompOffMassFlow = 0.0;
        state.dataPTHP->CompOffFlowRatio = 0.0;
    }

    // Set the inlet node mass flow rate
    if (GetCurrentScheduleValue(state, state.dataPTHP->PTUnit(PTUnitNum).FanAvailSchedPtr) > 0.0 && state.dataPTHP->CompOnMassFlow != 0.0) {
        OnOffAirFlowRatio = 1.0;
        if (FirstHVACIteration) {
            state.dataLoopNodes->Node(InNode).MassFlowRate = state.dataPTHP->CompOnMassFlow;
            PartLoadRatio = 0.0;
        } else {
            if (state.dataPTHP->PTUnit(PTUnitNum).HeatCoolMode != iCompMode::Unassigned) {
                PartLoadRatio = 1.0;
            } else {
                PartLoadRatio = 0.0;
            }
        }
    } else {
        PartLoadRatio = 0.0;
        state.dataLoopNodes->Node(InNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(OutNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMaxAvail = 0.0;
        OnOffAirFlowRatio = 1.0;
    }

    // Set the system mass flow rates
    SetVSHPAirFlow(state, PTUnitNum, ZoneNum, PartLoadRatio, OnOffAirFlowRatio);
}

void SetMinOATCompressor(EnergyPlusData &state,
                         int const PTUnitNum,                     // index to furnace
                         std::string const &PTUnitName,           // name of furnace
                         std::string const &cCurrentModuleObject, // type of furnace
                         int const CoolingCoilIndex,              // index of cooling coil
                         int const HeatingCoilIndex,              // index of heating coil
                         bool &ErrorsFound                        // GetInput logical that errors were found
)
{

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool errFlag;

    // Set minimum OAT for heat pump compressor operation in cooling mode
    errFlag = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == CoilDX_CoolingSingleSpeed) {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling = DXCoils::GetMinOATCompressorUsingIndex(state, CoolingCoilIndex, errFlag);
    } else if (state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling =
            DXCoils::GetMinOATCompressorUsingIndex(state, state.dataPTHP->PTUnit(PTUnitNum).DXCoolCoilIndexNum, errFlag);
    } else if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling =
            VariableSpeedCoils::GetVSCoilMinOATCompressorUsingIndex(state, CoolingCoilIndex, errFlag);
    } else {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorCooling = -1000.0;
    }
    if (errFlag) {
        ShowContinueError(state, "...occurs in " + cCurrentModuleObject + " = " + PTUnitName);
        ErrorsFound = true;
    }

    // Set minimum OAT for heat pump compressor operation in heating mode
    errFlag = false;
    if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorHeating =
            VariableSpeedCoils::GetVSCoilMinOATCompressorUsingIndex(state, HeatingCoilIndex, errFlag);
    } else if (state.dataPTHP->PTUnit(PTUnitNum).DXHeatCoilType_Num == CoilDX_HeatingEmpirical) {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorHeating = DXCoils::GetMinOATCompressorUsingIndex(state, HeatingCoilIndex, errFlag);
    } else {
        state.dataPTHP->PTUnit(PTUnitNum).MinOATCompressorHeating = -1000.0;
    }
    if (errFlag) {
        ShowContinueError(state, "...occurs in " + cCurrentModuleObject + " = " + PTUnitName);
        ErrorsFound = true;
    }
}

Real64 CalcPTUnitWaterFlowResidual(EnergyPlusData &state,
                                   Real64 const PartLoadRatio, // coil part load ratio
                                   Array1D<Real64> const &Par  // Function parameters
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   December 2017
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // To calculate the part-load ratio for the PTUnit coil with varying part load ratio

    // METHODOLOGY EMPLOYED:
    // Use SolveRoot to CALL this Function to converge on a solution

    // USE STATEMENTS:
    using General::SolveRoot;
    using Psychrometrics::PsyHFnTdbW;

    // Return value
    Real64 Residuum; // Result (forces solution to be within tolerance)

    // Argument array dimensioning
    //   Parameter description example:
    //       Par(1)  = double(UnitarySysNum)    ! Index to unitary system
    //       Par(2)  = 0.0                      ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
    //       Par(3)  = double(ControlledZoneNum) ! zone index
    //       Par(4)  = QZnReq                   ! zone load [W]
    //       Par(5)  = double(AirControlNode)   ! UnitarySystem air inlet node number
    //       Par(6)  = OnOffAirFlowRatio        ! ratio of coil on air flow rate to coil off air flow rate
    //       Par(7)  = double(AirLoopNum)       ! index to air loop
    //       Par(8)  = double(WaterControlNode) ! CW or HW control node number
    //       Par(9)  = lowWaterMdot             ! water flow rate at low speed fan that meets outlet air set point temperature
    //       Par(10) = highWaterMdot            ! water flow rate at high speed fan that meets outlet air set point temperature
    //       Par(11) = lowSpeedRatio            ! ratio of low speed fan flow rate to high speed fan flow rate
    //       Par(12) = airMdot                  ! air flow rate used for function calculations
    //       Par(13) = SATempTarget             ! SA temperature target [C], 0 if target is load [W]
    //       Par(14) = systemMaxAirFlowRate     ! UnitarySystem maximum air flow rate [kg/s]
    //       Par(15) = LoadType                 ! 1.0 for CoolingLoad otherwise don't care
    //       Par(16) = iteration method         ! 1 = iteration on coil capacity, 2 = iterate on air flow rate at constant coil capacity

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool FirstHVACIteration;     // FirstHVACIteration flag
    bool HXUnitOn;               // flag for HX operation
    bool LoadIsTarget;           // flag to determine iteration mode
    bool coolingLoad;            // denotes cooling operation
    bool iterateOnAirOnly;       // flag to determine which iteration method is used
    int AirLoopNum;              // index to air loop
    int OutletNode;              // UnitarySystem air outlet node
    int UnitarySysNum;           // Index to this fan coil unit
    int ControlledZoneNum;       // zone index
    int AirControlNode;          // UnitarySystem inlet air node
    int WaterControlNode;        // water node to control
    Real64 SensOutput;           // delivered sensible capacity [W]
    Real64 LatOutput;            // delivered latent capacity [W]
    Real64 OnOffAirFlowRat;      // ratio of coil on to coil off air flow rate
    Real64 QZnReq;               // Sensible load to be met [W]
    Real64 systemMaxAirFlowRate; // UnitarySystem maximum air flow rate [kg/s]
    Real64 lowWaterMdot;         // water flow rate at low air flow rate [kg/s]
    Real64 highWaterMdot;        // water flow rate at high air flow rate [kg/s]
    Real64 lowSpeedRatio;        // low speed air flow ratio []
    Real64 airMdot;              // air flow rate [kg/s]
    Real64 waterMdot;            // water flow rate [kg/s]
    Real64 SATempTarget;         // coil outelt air target temperature [C]
    Real64 coolingPLR;           // part-load ratio passed to cooling coil
    Real64 heatingPLR;           // part-load ratio passed to heating coil
    Real64 SupHeaterLoad;        // new variable used in PTHP

    // Convert parameters to usable variables
    UnitarySysNum = int(Par(1));
    if (Par(2) == 1.0) {
        FirstHVACIteration = true;
    } else {
        FirstHVACIteration = false;
    }
    ControlledZoneNum = int(Par(3));
    QZnReq = Par(4);
    AirControlNode = int(Par(5));
    OnOffAirFlowRat = Par(6);
    AirLoopNum = int(Par(7));
    WaterControlNode = int(Par(8));
    lowWaterMdot = Par(9);
    highWaterMdot = Par(10);
    lowSpeedRatio = Par(11);
    airMdot = Par(12);
    SATempTarget = 0.0;
    LoadIsTarget = false;
    if (Par(13) == 0.0) {
        LoadIsTarget = true;
    } else {
        SATempTarget = Par(13);
    }
    systemMaxAirFlowRate = Par(14);
    coolingLoad = false;
    if (Par(15) == 1.0) coolingLoad = true;
    iterateOnAirOnly = false;
    if (Par(16) == 2.0) iterateOnAirOnly = true;

    HXUnitOn = true;

    if (iterateOnAirOnly) {

        // set air flow rate bounded by low speed and high speed air flow rates
        state.dataLoopNodes->Node(AirControlNode).MassFlowRate = airMdot * (lowSpeedRatio + (PartLoadRatio * (1.0 - lowSpeedRatio)));
        // FanPartLoadRatio is used to pass info over to function SetAverageAirFlow since air and coil PLR are disassociated in the SZVAV model
        // FanPartLoadRatio is a report variable that is updated (overwritten) in ReportUnitarySystem
        state.dataPTHP->PTUnit(UnitarySysNum).FanPartLoadRatio = PartLoadRatio;
        //            if( WaterControlNode > 0 ) state.dataLoopNodes->Node( WaterControlNode ).MassFlowRate = highWaterMdot;

    } else {

        state.dataLoopNodes->Node(AirControlNode).MassFlowRate = airMdot;
        state.dataPTHP->PTUnit(UnitarySysNum).FanPartLoadRatio =
            max(0.0, ((airMdot - (systemMaxAirFlowRate * lowSpeedRatio)) / ((1.0 - lowSpeedRatio) * systemMaxAirFlowRate)));

        if (WaterControlNode > 0) {
            waterMdot = highWaterMdot * PartLoadRatio;
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = waterMdot;
        }
    }

    coolingPLR = 0.0;
    heatingPLR = 0.0;

    if (WaterControlNode > 0 && WaterControlNode == state.dataPTHP->PTUnit(UnitarySysNum).CoolCoilFluidInletNode) {
        // cooling load using water cooling coil
        coolingPLR = PartLoadRatio;
        //            PTUnit( UnitarySysNum ).CoolingPartLoadFrac = PartLoadRatio;
        state.dataPTHP->PTUnit(UnitarySysNum).CoolCoilWaterFlowRatio =
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate / state.dataPTHP->PTUnit(UnitarySysNum).MaxCoolCoilFluidFlow;
    } else if (WaterControlNode > 0 && WaterControlNode == state.dataPTHP->PTUnit(UnitarySysNum).HeatCoilFluidInletNode) {
        // heating load using water heating coil
        heatingPLR = PartLoadRatio;
        //            PTUnit( UnitarySysNum ).HeatingPartLoadFrac = PartLoadRatio;
        state.dataPTHP->PTUnit(UnitarySysNum).HeatCoilWaterFlowRatio =
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate / state.dataPTHP->PTUnit(UnitarySysNum).MaxHeatCoilFluidFlow;
    } else if (coolingLoad) { // non-water coil with cooling load
        coolingPLR = PartLoadRatio;
        //            PTUnit( UnitarySysNum ).CoolingPartLoadFrac = coolingPLR;
    } else { // must be non-water coil with heating load
        heatingPLR = PartLoadRatio;
        //            PTUnit( UnitarySysNum ).HeatingPartLoadFrac = heatingPLR;
    }

    SensOutput = 0.0;
    LatOutput = 0.0;
    CalcPTUnit(state, UnitarySysNum, FirstHVACIteration, PartLoadRatio, SensOutput, QZnReq, OnOffAirFlowRat, SupHeaterLoad, HXUnitOn);

    if (LoadIsTarget) {
        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (SensOutput - QZnReq) / 100.0;
        } else {
            Residuum = (SensOutput - QZnReq) / QZnReq;
        }
    } else {
        // Calculate residual based on outlet temperature
        OutletNode = state.dataPTHP->PTUnit(UnitarySysNum).AirOutNode;
        Residuum = (state.dataLoopNodes->Node(OutletNode).Temp - SATempTarget) * 10.0;
    }

    return Residuum;
}

Real64 CalcPTUnitAirAndWaterFlowResidual(EnergyPlusData &state,
                                         Real64 const PartLoadRatio, // water and air part load ratio
                                         Array1D<Real64> const &Par  // Function parameters
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   December 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // To calculate the part-load ratio for the FCU with varying water flow rate

    // METHODOLOGY EMPLOYED:
    // Use SolveRoot to CALL this Function to converge on a solution

    // Return value
    Real64 Residuum; // Result (forces solution to be within tolerance)

    // SUBROUTINE ARGUMENT DEFINITIONS:

    //   Parameter description example:
    //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
    //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
    //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
    //       Par(4)  = QZnReq               ! zone load [W]
    //       Par(5)  = WaterControlNode     ! CW or HW control node number

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool FirstHVACIteration; // FirstHVACIteration flag
    Real64 QUnitOut;         // delivered capacity [W]

    // Convert parameters to usable variables
    int UnitarySystemNum = int(Par(1));
    if (Par(2) == 1.0) {
        FirstHVACIteration = true;
    } else {
        FirstHVACIteration = false;
    }
    // int ControlledZoneNum = int(Par(3));
    Real64 QZnReq = Par(4);
    int WaterControlNode = int(Par(5));
    Real64 OnOffAirFlowRat = Par(6);
    Real64 MinAirFlow = Par(7);
    Real64 MinWaterFlow = 0.0;
    Real64 coolingPLR = 0.0;
    Real64 heatingPLR = 0.0;
    Real64 lowSpeedRatio = Par(11);
    Real64 maxAirFlow = Par(14);
    bool coolingLoad = false;
    if (Par(15) == 1.0) coolingLoad = true;
    bool HXUnitOn = false;
    if (Par(18) == 1.0) HXUnitOn = true;

    // set air flow rate
    state.dataLoopNodes->Node(state.dataPTHP->PTUnit(UnitarySystemNum).AirInNode).MassFlowRate =
        MinAirFlow + (PartLoadRatio * (maxAirFlow - MinAirFlow));
    state.dataPTHP->PTUnit(UnitarySystemNum).FanPartLoadRatio =
        max(0.0,
            ((state.dataLoopNodes->Node(state.dataPTHP->PTUnit(UnitarySystemNum).AirInNode).MassFlowRate - (maxAirFlow * lowSpeedRatio)) /
             ((1.0 - lowSpeedRatio) * maxAirFlow)));
    // set water flow rate
    if (WaterControlNode > 0 && WaterControlNode == state.dataPTHP->PTUnit(UnitarySystemNum).CoolCoilFluidInletNode) {
        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
            MinWaterFlow + (PartLoadRatio * (state.dataPTHP->PTUnit(UnitarySystemNum).MaxCoolCoilFluidFlow - MinWaterFlow));
    } else if (WaterControlNode > 0 && WaterControlNode == state.dataPTHP->PTUnit(UnitarySystemNum).HeatCoilFluidInletNode) {
        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
            MinWaterFlow + (PartLoadRatio * (state.dataPTHP->PTUnit(UnitarySystemNum).MaxHeatCoilFluidFlow - MinWaterFlow));
    } else if (coolingLoad) {
        coolingPLR = PartLoadRatio;
        // PTUnit( UnitarySysNum ).CoolingPartLoadFrac = coolingPLR;
    } else { // must be non-water coil with heating load
        heatingPLR = PartLoadRatio;
        // PTUnit( UnitarySysNum ).HeatingPartLoadFrac = heatingPLR;
    }
    CalcPTUnit(
        state, UnitarySystemNum, FirstHVACIteration, PartLoadRatio, QUnitOut, QZnReq, OnOffAirFlowRat, state.dataPTHP->SupHeaterLoad, HXUnitOn);

    // Calculate residual based on output magnitude
    if (std::abs(QZnReq) <= 100.0) {
        Residuum = (QUnitOut - QZnReq) / 100.0;
    } else {
        Residuum = (QUnitOut - QZnReq) / QZnReq;
    }

    return Residuum;
}
} // namespace EnergyPlus::PackagedTerminalHeatPump
