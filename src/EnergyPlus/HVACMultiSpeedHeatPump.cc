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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACMultiSpeedHeatPump.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace HVACMultiSpeedHeatPump {

    // Module containing the Multi Speed Heat Pump simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu, Florida Solar Energy Center
    //       DATE WRITTEN   June 2007
    //       MODIFIED       Bereket Nigusse, FSEC, June 2010 - deprecated supply air flow fraction through controlled
    //                      zone from the furnace object input field. Now, the flow fraction is calculated internally
    //                      Brent Griffith, NREL, Dec 2010 -- upgrade to new plant for heat recovery, general fluid props.
    //                      Bereket Nigusse, FSEC, Jan. 2012 -- added hot water and steam heating coil

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to simulate Multi Speed Heat Pump in
    // EnergyPlus.

    // Module currently models air-cooled or evap-cooled direct expansion systems
    // (split or packaged) with mulptiple speeds. Air-side performance is modeled to determine
    // coil discharge air conditions. The module also determines the DX unit's energy
    // usage. Neither the air-side performance nor the energy usage includes the effect
    // of supply air fan heat/energy usage. The supply air fan is modeled by other modules.

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::BlowThru;
    using DataHVACGlobals::Coil_HeatingElectric;
    using DataHVACGlobals::Coil_HeatingElectric_MultiStage;
    using DataHVACGlobals::Coil_HeatingGas_MultiStage;
    using DataHVACGlobals::Coil_HeatingGasOrOtherFuel;
    using DataHVACGlobals::Coil_HeatingSteam;
    using DataHVACGlobals::Coil_HeatingWater;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::DrawThru;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using namespace Psychrometrics;
    using namespace ScheduleManager;

    // Curve Types
    enum class CurveType
    {
        Invalid = -1,
        Linear,      // Linear curve type
        BiLinear,    // Bi-linear curve type
        Quadratic,   // Quadratic curve type
        BiQuadratic, // Bi-quadratic curve type
        Cubic,       // Cubic curve type
        Num
    };

    static constexpr std::string_view fluidNameSteam("STEAM");

    void SimMSHeatPump(EnergyPlusData &state,
                       std::string_view CompName,     // Name of the unitary engine driven heat pump system
                       bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
                       int const AirLoopNum,          // air loop index
                       int &CompIndex                 // Index to changeover-bypass VAV system
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, Florida Solar Energy Center
        //       DATE WRITTEN   June. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the simulation of multispeed heat pump.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int MSHeatPumpNum;        // index of fan coil unit being simulated
        Real64 OnOffAirFlowRatio; // Ratio of compressor ON airflow to average airflow over timestep
        Real64 QZnLoad;           // Zone load required by all zones served by this air loop system
        Real64 QSensUnitOut;      // MSHP sensible capacity output [W]

        // First time SimMSHeatPump is called, get the input
        if (state.dataHVACMultiSpdHP->GetInputFlag) {
            GetMSHeatPumpInput(state);
            state.dataHVACMultiSpdHP->GetInputFlag = false; // Set GetInputFlag false so you don't get coil inputs again
        }

        if (CompIndex == 0) {
            MSHeatPumpNum = UtilityRoutines::FindItemInList(CompName, state.dataHVACMultiSpdHP->MSHeatPump);
            if (MSHeatPumpNum == 0) {
                ShowFatalError(state, "MultiSpeed Heat Pump is not found=" + std::string{CompName});
            }
            CompIndex = MSHeatPumpNum;
        } else {
            MSHeatPumpNum = CompIndex;
            if (MSHeatPumpNum > state.dataHVACMultiSpdHP->NumMSHeatPumps || MSHeatPumpNum < 1) {
                ShowFatalError(state,
                               format("SimMSHeatPump: Invalid CompIndex passed={}, Number of MultiSpeed Heat Pumps={}, Heat Pump name={}",
                                      MSHeatPumpNum,
                                      state.dataHVACMultiSpdHP->NumMSHeatPumps,
                                      CompName));
            }
            if (state.dataHVACMultiSpdHP->CheckEquipName(MSHeatPumpNum)) {
                if (CompName != state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).Name) {
                    ShowFatalError(state,
                                   format("SimMSHeatPump: Invalid CompIndex passed={}, Heat Pump name={}{}",
                                          MSHeatPumpNum,
                                          CompName,
                                          state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).Name));
                }
                state.dataHVACMultiSpdHP->CheckEquipName(MSHeatPumpNum) = false;
            }
        }

        OnOffAirFlowRatio = 0.0;

        // Initialize the engine driven heat pump
        InitMSHeatPump(state, MSHeatPumpNum, FirstHVACIteration, AirLoopNum, QZnLoad, OnOffAirFlowRatio);

        SimMSHP(state, MSHeatPumpNum, FirstHVACIteration, AirLoopNum, QSensUnitOut, QZnLoad, OnOffAirFlowRatio);

        // Update the unit outlet nodes
        UpdateMSHeatPump(state, MSHeatPumpNum);

        // Report the result of the simulation
        ReportMSHeatPump(state, MSHeatPumpNum);
    }

    //******************************************************************************

    void SimMSHP(EnergyPlusData &state,
                 int const MSHeatPumpNum,       // number of the current engine driven Heat Pump being simulated
                 bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                 int const AirLoopNum,          // air loop index
                 Real64 &QSensUnitOut,          // cooling/heating deliveded to zones [W]
                 Real64 const QZnReq,           // required zone load
                 Real64 &OnOffAirFlowRatio      // ratio of compressor ON airflow to AVERAGE airflow over timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on SimPTHP

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a multispeed heat pump; adjust its output to match the
        // required system load.

        // METHODOLOGY EMPLOYED:
        // Calls ControlMSHPOutput to obtain the desired unit output

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataHVACGlobals::SmallLoad;
        using DataHVACGlobals::SmallMassFlow;

        // Locals
        Real64 SupHeaterLoad;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadFrac; // compressor part load fraction
        Real64 SpeedRatio;   // compressor speed ratio
        bool UnitOn;         // TRUE if unit is on
        int OutletNode;      // MSHP air outlet node
        int InletNode;       // MSHP air inlet node
        Real64 AirMassFlow;  // air mass flow rate [kg/s]
        int OpMode;          // operating mode (fan cycling or continious; DX coil always cycles)
        int ZoneNum;         // Controlled zone number
        Real64 QTotUnitOut;
        int SpeedNum;                                      // Speed number
        DataHVACGlobals::CompressorOperation CompressorOp; // compressor operation; 1=on, 0=off
        Real64 SaveMassFlowRate;                           // saved inlet air mass flow rate [kg/s]

        // zero the fan, DX coils, and supplemental electric heater electricity consumption
        state.dataHVACGlobal->DXElecHeatingPower = 0.0;
        state.dataHVACGlobal->DXElecCoolingPower = 0.0;
        state.dataHVACMultiSpdHP->SaveCompressorPLR = 0.0;
        state.dataHVACGlobal->ElecHeatingCoilPower = 0.0;
        state.dataHVACGlobal->SuppHeatingCoilPower = 0.0;

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        // initialize local variables
        UnitOn = true;
        OutletNode = MSHeatPump(MSHeatPumpNum).AirOutletNodeNum;
        InletNode = MSHeatPump(MSHeatPumpNum).AirInletNodeNum;
        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        OpMode = MSHeatPump(MSHeatPumpNum).OpMode;
        ZoneNum = MSHeatPump(MSHeatPumpNum).ControlZoneNum;
        CompressorOp = DataHVACGlobals::CompressorOperation::On;

        // set the on/off flags
        if (MSHeatPump(MSHeatPumpNum).OpMode == CycFanCycCoil) {
            // cycling unit only runs if there is a cooling or heating load.
            if (std::abs(QZnReq) < SmallLoad || AirMassFlow < SmallMassFlow || state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                UnitOn = false;
            }
        } else if (MSHeatPump(MSHeatPumpNum).OpMode == ContFanCycCoil) {
            // continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
            if (AirMassFlow < SmallMassFlow) {
                UnitOn = false;
            }
        }

        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;

        SaveMassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        if (MSHeatPump(MSHeatPumpNum).EMSOverrideCoilSpeedNumOn) {
            Real64 SpeedVal = MSHeatPump(MSHeatPumpNum).EMSOverrideCoilSpeedNumValue;

            if (!FirstHVACIteration && MSHeatPump(MSHeatPumpNum).OpMode == CycFanCycCoil && QZnReq < 0.0 &&
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoActive) {
                CompressorOp = DataHVACGlobals::CompressorOperation::Off;
                ControlMSHPOutputEMS(state,
                                     MSHeatPumpNum,
                                     FirstHVACIteration,
                                     CompressorOp,
                                     OpMode,
                                     QZnReq,
                                     SpeedVal,
                                     SpeedNum,
                                     SpeedRatio,
                                     PartLoadFrac,
                                     OnOffAirFlowRatio,
                                     SupHeaterLoad);
                if (ceil(SpeedVal) == MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling && SpeedRatio == 1.0) {
                    state.dataLoopNodes->Node(InletNode).MassFlowRate = SaveMassFlowRate;
                    CompressorOp = DataHVACGlobals::CompressorOperation::On;
                    ControlMSHPOutputEMS(state,
                                         MSHeatPumpNum,
                                         FirstHVACIteration,
                                         CompressorOp,
                                         OpMode,
                                         QZnReq,
                                         SpeedVal,
                                         SpeedNum,
                                         SpeedRatio,
                                         PartLoadFrac,
                                         OnOffAirFlowRatio,
                                         SupHeaterLoad);
                }
            } else {
                ControlMSHPOutputEMS(state,
                                     MSHeatPumpNum,
                                     FirstHVACIteration,
                                     CompressorOp,
                                     OpMode,
                                     QZnReq,
                                     SpeedVal,
                                     SpeedNum,
                                     SpeedRatio,
                                     PartLoadFrac,
                                     OnOffAirFlowRatio,
                                     SupHeaterLoad);
            }
        } else {
            if (!FirstHVACIteration && MSHeatPump(MSHeatPumpNum).OpMode == CycFanCycCoil && QZnReq < 0.0 &&
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoActive) {
                // for cycling fan, cooling load, check whether furnace can meet load with compressor off
                CompressorOp = DataHVACGlobals::CompressorOperation::Off;
                ControlMSHPOutput(state,
                                  MSHeatPumpNum,
                                  FirstHVACIteration,
                                  CompressorOp,
                                  OpMode,
                                  QZnReq,
                                  ZoneNum,
                                  SpeedNum,
                                  SpeedRatio,
                                  PartLoadFrac,
                                  OnOffAirFlowRatio,
                                  SupHeaterLoad);
                if (SpeedNum == MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling && SpeedRatio == 1.0) {
                    // compressor on (reset inlet air mass flow rate to starting value)
                    state.dataLoopNodes->Node(InletNode).MassFlowRate = SaveMassFlowRate;
                    CompressorOp = DataHVACGlobals::CompressorOperation::On;
                    ControlMSHPOutput(state,
                                      MSHeatPumpNum,
                                      FirstHVACIteration,
                                      CompressorOp,
                                      OpMode,
                                      QZnReq,
                                      ZoneNum,
                                      SpeedNum,
                                      SpeedRatio,
                                      PartLoadFrac,
                                      OnOffAirFlowRatio,
                                      SupHeaterLoad);
                }
            } else {
                // compressor on
                ControlMSHPOutput(state,
                                  MSHeatPumpNum,
                                  FirstHVACIteration,
                                  CompressorOp,
                                  OpMode,
                                  QZnReq,
                                  ZoneNum,
                                  SpeedNum,
                                  SpeedRatio,
                                  PartLoadFrac,
                                  OnOffAirFlowRatio,
                                  SupHeaterLoad);
            }
        }

        if (MSHeatPump(MSHeatPumpNum).HeatCoilType != MultiSpeedHeatingCoil) {
            state.dataHVACMultiSpdHP->SaveCompressorPLR = PartLoadFrac;
        } else {
            if (SpeedNum > 1) {
                state.dataHVACMultiSpdHP->SaveCompressorPLR = 1.0;
            }

            if (PartLoadFrac == 1.0 && state.dataHVACMultiSpdHP->SaveCompressorPLR < 1.0 && (!MSHeatPump(MSHeatPumpNum).Staged)) {
                PartLoadFrac = state.dataHVACMultiSpdHP->SaveCompressorPLR;
            }
        }

        CalcMSHeatPump(state,
                       MSHeatPumpNum,
                       FirstHVACIteration,
                       CompressorOp,
                       SpeedNum,
                       SpeedRatio,
                       PartLoadFrac,
                       QSensUnitOut,
                       QZnReq,
                       OnOffAirFlowRatio,
                       SupHeaterLoad);

        // calculate delivered capacity
        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

        QTotUnitOut = AirMassFlow * (state.dataLoopNodes->Node(OutletNode).Enthalpy -
                                     state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).Enthalpy);

        // report variables
        MSHeatPump(MSHeatPumpNum).CompPartLoadRatio = state.dataHVACMultiSpdHP->SaveCompressorPLR;
        if (MSHeatPump(MSHeatPumpNum).OpMode == CycFanCycCoil) {
            if (SupHeaterLoad > 0.0) {
                MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = 1.0;
            } else {
                if (SpeedNum < 2) {
                    MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = PartLoadFrac;
                } else {
                    MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = 1.0;
                }
            }
        } else {
            if (UnitOn) {
                MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = 1.0;
            } else {
                if (SpeedNum < 2) {
                    MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = PartLoadFrac;
                } else {
                    MSHeatPump(MSHeatPumpNum).FanPartLoadRatio = 1.0;
                }
            }
        }

        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode) {
            MSHeatPump(MSHeatPumpNum).TotHeatEnergyRate = std::abs(max(0.0, QTotUnitOut));
            MSHeatPump(MSHeatPumpNum).SensHeatEnergyRate = std::abs(max(0.0, QSensUnitOut));
            MSHeatPump(MSHeatPumpNum).LatHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
            MSHeatPump(MSHeatPumpNum).TotCoolEnergyRate = 0.0;
            MSHeatPump(MSHeatPumpNum).SensCoolEnergyRate = 0.0;
            MSHeatPump(MSHeatPumpNum).LatCoolEnergyRate = 0.0;
        }
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
            MSHeatPump(MSHeatPumpNum).TotCoolEnergyRate = std::abs(min(0.0, QTotUnitOut));
            MSHeatPump(MSHeatPumpNum).SensCoolEnergyRate = std::abs(min(0.0, QSensUnitOut));
            MSHeatPump(MSHeatPumpNum).LatCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
            MSHeatPump(MSHeatPumpNum).TotHeatEnergyRate = 0.0;
            MSHeatPump(MSHeatPumpNum).SensHeatEnergyRate = 0.0;
            MSHeatPump(MSHeatPumpNum).LatHeatEnergyRate = 0.0;
        }

        MSHeatPump(MSHeatPumpNum).AuxElecPower = MSHeatPump(MSHeatPumpNum).AuxOnCyclePower * state.dataHVACMultiSpdHP->SaveCompressorPLR +
                                                 MSHeatPump(MSHeatPumpNum).AuxOffCyclePower * (1.0 - state.dataHVACMultiSpdHP->SaveCompressorPLR);
        Real64 locFanElecPower = 0.0;
        locFanElecPower = Fans::GetFanPower(state, MSHeatPump(MSHeatPumpNum).FanNum);
        MSHeatPump(MSHeatPumpNum).ElecPower = locFanElecPower + state.dataHVACGlobal->DXElecCoolingPower + state.dataHVACGlobal->DXElecHeatingPower +
                                              state.dataHVACGlobal->ElecHeatingCoilPower + state.dataHVACGlobal->SuppHeatingCoilPower +
                                              MSHeatPump(MSHeatPumpNum).AuxElecPower;
    }

    //******************************************************************************

    void GetMSHeatPumpInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    July 2007
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will get the input required by the multispeed heat pump model

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataHVACGlobals::FanType_SimpleConstVolume;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataSizing::AutoSize;
        using Fans::GetFanIndex;
        using Fans::GetFanInletNode;
        using Fans::GetFanOutletNode;
        using Fans::GetFanType;
        using Fans::GetFanVolFlow;
        using FluidProperties::FindGlycol;

        using BranchNodeConnections::SetUpCompSets;
        using DXCoils::GetDXCoilIndex;
        using NodeInputManager::GetOnlySingleNode;
        auto &GetDXCoilInletNode(DXCoils::GetCoilInletNode);
        auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
        using DXCoils::GetDXCoilNumberOfSpeeds;
        auto &GetHeatingCoilInletNode(HeatingCoils::GetCoilInletNode);
        auto &GetHeatingCoilOutletNode(HeatingCoils::GetCoilOutletNode);
        auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
        using HeatingCoils::GetCoilIndex;
        using HeatingCoils::GetCoilInletNode;
        using HeatingCoils::GetCoilOutletNode;
        using HeatingCoils::GetHeatingCoilIndex;
        using HeatingCoils::GetHeatingCoilNumberOfStages;
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetWaterCoilInletNode(WaterCoils::GetCoilInletNode);
        auto &GetWaterCoilOutletNode(WaterCoils::GetCoilOutletNode);
        auto &GetSteamCoilAirInletNode(SteamCoils::GetCoilAirInletNode);
        using SteamCoils::GetCoilAirOutletNode;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetSteamCoilIndex;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        using DXCoils::SetMSHPDXCoilHeatRecoveryFlag;
        using FluidProperties::GetSatDensityRefrig;

        // Locals
        // PARAMETERS
        static constexpr std::string_view RoutineName("GetMSHeatPumpInput: "); // include trailing blank space
        static constexpr std::string_view RoutineNameNoColon("GetMSHeatPumpInput");

        // LOCAL VARIABLES
        int MSHPNum;                   // Engine driven heat pump count
        int NumAlphas;                 // Number of elements in the alpha array
        int NumNumbers;                // Number of Numbers for each GetObjectItem call
        int IOStatus;                  // Used in GetObjectItem
        bool ErrorsFound(false);       // True when input errors found
        bool IsNotOK;                  // Flag to verify name
        bool AirNodeFound;             // True when an air node is found
        bool AirLoopFound;             // True when an air loop is found
        int ControlledZoneNum;         // Controlled zone number
        int FanType;                   // Fan type
        int BranchNum;                 // Index to branch
        int CompNum;                   // Index to component
        int TstatZoneNum;              // Used to determine if control zone has a thermostat object
        int i;                         // Index to speeds
        int j;                         // Index to speeds
        bool Found;                    // Flag to find autosize
        int HeatingCoilInletNode;      // Heating coil inlet node number
        int HeatingCoilOutletNode;     // Heating coil outlet node number
        int CoolingCoilInletNode;      // Cooling coil inlet node number
        int CoolingCoilOutletNode;     // Cooling coil outlet node number
        int SuppHeatCoilInletNode;     // Supplemental heating coil inlet node number
        int SuppHeatCoilOutletNode;    // Supplemental heating coil outlet node number
        bool LocalError;               // Local error flag
        Array1D_string Alphas;         // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> Numbers;       // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        int MaxNums(0);                // Maximum number of numeric input fields
        int MaxAlphas(0);              // Maximum number of alpha input fields
        int TotalArgs(0);              // Total number of alpha and numeric arguments (max) for a
        //  certain object in the input file
        bool errFlag;
        int SteamIndex;      // steam coil steam inlet density
        Real64 SteamDensity; // density of steam at 100C

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        if (MSHeatPump.allocated()) return;

        state.dataHVACMultiSpdHP->CurrentModuleObject =
            "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed"; // Object type for getting and error messages

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, state.dataHVACMultiSpdHP->CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        Numbers.dimension(MaxNums, 0.0);
        cNumericFields.allocate(MaxNums);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        state.dataHVACMultiSpdHP->NumMSHeatPumps =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataHVACMultiSpdHP->CurrentModuleObject);

        if (state.dataHVACMultiSpdHP->NumMSHeatPumps <= 0) {
            ShowSevereError(state, "No " + state.dataHVACMultiSpdHP->CurrentModuleObject + " objects specified in input file.");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        MSHeatPump.allocate(state.dataHVACMultiSpdHP->NumMSHeatPumps);
        state.dataHVACMultiSpdHP->MSHeatPumpReport.allocate(state.dataHVACMultiSpdHP->NumMSHeatPumps);
        state.dataHVACMultiSpdHP->CheckEquipName.dimension(state.dataHVACMultiSpdHP->NumMSHeatPumps, true);

        // Load arrays with reformulated electric EIR chiller data
        for (MSHPNum = 1; MSHPNum <= state.dataHVACMultiSpdHP->NumMSHeatPumps; ++MSHPNum) {

            HeatingCoilInletNode = 0;
            HeatingCoilOutletNode = 0;
            CoolingCoilInletNode = 0;
            CoolingCoilOutletNode = 0;
            SuppHeatCoilInletNode = 0;
            SuppHeatCoilOutletNode = 0;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataHVACMultiSpdHP->CurrentModuleObject,
                                                                     MSHPNum,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), state.dataHVACMultiSpdHP->CurrentModuleObject, ErrorsFound);

            MSHeatPump(MSHPNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                MSHeatPump(MSHPNum).AvaiSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                MSHeatPump(MSHPNum).AvaiSchedPtr = GetScheduleIndex(state, Alphas(2));
                if (MSHeatPump(MSHPNum).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\" " + cAlphaFields(2) +
                                        " not found: " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            MSHeatPump(MSHPNum).AirInletNodeName = Alphas(3);
            MSHeatPump(MSHPNum).AirOutletNodeName = Alphas(4);
            MSHeatPump(MSHPNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                    Alphas(3),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed,
                                                                    Alphas(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Inlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    ObjectIsParent);

            MSHeatPump(MSHPNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                     Alphas(4),
                                                                     ErrorsFound,
                                                                     DataLoopNode::ConnectionObjectType::AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed,
                                                                     Alphas(1),
                                                                     DataLoopNode::NodeFluidType::Air,
                                                                     DataLoopNode::ConnectionType::Outlet,
                                                                     NodeInputManager::CompFluidStream::Primary,
                                                                     ObjectIsParent);

            TestCompSet(state, state.dataHVACMultiSpdHP->CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            // Get the Controlling Zone or Location of the engine driven heat pump Thermostat
            MSHeatPump(MSHPNum).ControlZoneNum = UtilityRoutines::FindItemInList(Alphas(5), state.dataHeatBal->Zone);
            MSHeatPump(MSHPNum).ControlZoneName = Alphas(5);
            if (MSHeatPump(MSHPNum).ControlZoneNum == 0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\" " + cAlphaFields(5) +
                                    " not found: " + MSHeatPump(MSHPNum).ControlZoneName);
                ErrorsFound = true;
            }

            // Get the node number for the zone with the thermostat
            if (MSHeatPump(MSHPNum).ControlZoneNum > 0) {
                AirNodeFound = false;
                AirLoopFound = false;
                for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != MSHeatPump(MSHPNum).ControlZoneNum) continue;
                    // Find the controlled zone number for the specified thermostat location
                    MSHeatPump(MSHPNum).NodeNumOfControlledZone = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                    // Determine if furnace is on air loop served by the thermostat location specified
                    for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                        int AirLoopNumber = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                        if (AirLoopNumber > 0) {
                            for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).NumBranches; ++BranchNum) {
                                for (CompNum = 1;
                                     CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).TotalComponents;
                                     ++CompNum) {
                                    if (!UtilityRoutines::SameString(
                                            state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                            MSHeatPump(MSHPNum).Name) ||
                                        !UtilityRoutines::SameString(
                                            state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                            state.dataHVACMultiSpdHP->CurrentModuleObject))
                                        continue;
                                    AirLoopFound = true;
                                    MSHeatPump(MSHPNum).AirLoopNumber = AirLoopNumber;
                                    break;
                                }
                                MSHeatPump(MSHPNum).ZoneInletNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                if (AirLoopFound) break;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TstatZoneNum) {
                                if (state.dataZoneCtrls->TempControlledZone(TstatZoneNum).ActualZoneNum != MSHeatPump(MSHPNum).ControlZoneNum)
                                    continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++TstatZoneNum) {
                                if (state.dataZoneCtrls->ComfortControlledZone(TstatZoneNum).ActualZoneNum != MSHeatPump(MSHPNum).ControlZoneNum)
                                    continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= state.dataZoneTempPredictorCorrector->NumStageCtrZone; ++TstatZoneNum) {
                                if (state.dataZoneCtrls->StageControlledZone(TstatZoneNum).ActualZoneNum != MSHeatPump(MSHPNum).ControlZoneNum)
                                    continue;
                                AirNodeFound = true;
                            }
                        }
                        if (AirLoopFound) break;
                    }
                    if (AirLoopFound) break;
                }
                if (!AirNodeFound) {
                    ShowSevereError(state,
                                    "Did not find Air Node (" + cAlphaFields(5) + "), " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = \"\"" +
                                        MSHeatPump(MSHPNum).Name);
                    ShowContinueError(state, "Specified " + cAlphaFields(5) + " = " + Alphas(5));
                    ErrorsFound = true;
                }
                if (!AirLoopFound) {
                    ShowSevereError(state,
                                    "Did not find correct AirLoopHVAC for " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " +
                                        MSHeatPump(MSHPNum).Name);
                    ShowContinueError(state, "The " + cAlphaFields(5) + " = " + Alphas(5) + " is not served by this Primary Air Loop equipment.");
                    ErrorsFound = true;
                }
            }

            // Get supply fan data
            MSHeatPump(MSHPNum).FanName = Alphas(7);
            if (UtilityRoutines::SameString(Alphas(6), "Fan:OnOff") || UtilityRoutines::SameString(Alphas(6), "Fan:ConstantVolume")) {
                if (UtilityRoutines::SameString(Alphas(6), "Fan:OnOff")) {
                    MSHeatPump(MSHPNum).FanType = FanType_SimpleOnOff;
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Fan:OnOff",
                                  MSHeatPump(MSHPNum).FanName,
                                  "UNDEFINED",
                                  "UNDEFINED");
                    MSHeatPump(MSHPNum).FanInletNode = GetFanInletNode(state, "Fan:OnOff", MSHeatPump(MSHPNum).FanName, ErrorsFound);
                    MSHeatPump(MSHPNum).FanOutletNode = GetFanOutletNode(state, "Fan:OnOff", MSHeatPump(MSHPNum).FanName, ErrorsFound);
                } else {
                    MSHeatPump(MSHPNum).FanType = FanType_SimpleConstVolume;
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Fan:ConstantVolume",
                                  MSHeatPump(MSHPNum).FanName,
                                  "UNDEFINED",
                                  "UNDEFINED");
                    MSHeatPump(MSHPNum).FanInletNode = GetFanInletNode(state, "Fan:ConstantVolume", MSHeatPump(MSHPNum).FanName, ErrorsFound);
                    MSHeatPump(MSHPNum).FanOutletNode = GetFanOutletNode(state, "Fan:ConstantVolume", MSHeatPump(MSHPNum).FanName, ErrorsFound);
                }
                GetFanIndex(state, Alphas(7), MSHeatPump(MSHPNum).FanNum, ErrorsFound, state.dataHVACMultiSpdHP->CurrentModuleObject);
                GetFanType(state, Alphas(7), FanType, ErrorsFound);
                if (FanType != MSHeatPump(MSHPNum).FanType) {
                    ShowSevereError(state,
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cAlphaFields(6) +
                                        " and " + cAlphaFields(7) + " do not match in Fan objects.");
                    ShowContinueError(state, "The entered " + cAlphaFields(7) + " = " + Alphas(7) + " and " + cAlphaFields(6) + " = " + Alphas(6));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cAlphaFields(6) +
                                    " is not allowed = " + Alphas(6));
                ShowContinueError(state, "Valid choices are Fan:OnOff or Fan:ConstantVolume");
                ErrorsFound = true;
            }

            // Get supply fan placement data
            if (UtilityRoutines::SameString(Alphas(8), "BlowThrough") || UtilityRoutines::SameString(Alphas(8), "DrawThrough")) {
                if (UtilityRoutines::SameString(Alphas(8), "BlowThrough")) {
                    MSHeatPump(MSHPNum).FanPlaceType = BlowThru;
                } else {
                    MSHeatPump(MSHPNum).FanPlaceType = DrawThru;
                }
            } else {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cAlphaFields(8) +
                                    " is not allowed = " + Alphas(8));
                ShowContinueError(state, "Valid choices are BlowThrough or DrawThrough");
                ErrorsFound = true;
            }

            MSHeatPump(MSHPNum).FanSchedule = Alphas(9);
            MSHeatPump(MSHPNum).FanSchedPtr = GetScheduleIndex(state, Alphas(9));
            if (MSHeatPump(MSHPNum).FanSchedPtr == 0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\" " + cAlphaFields(9) +
                                    " not found: " + Alphas(9));
                ErrorsFound = true;
            }

            if (MSHeatPump(MSHPNum).FanSchedPtr > 0 && MSHeatPump(MSHPNum).FanType == FanType_SimpleConstVolume) {
                if (!CheckScheduleValueMinMax(state, MSHeatPump(MSHPNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                    ShowSevereError(state, state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state,
                                      cAlphaFields(9) + " must be continuous (fan operating mode schedule values > 0) for " + cAlphaFields(6) +
                                          " = Fan:ConstantVolume.");
                    ShowContinueError(state, "Error found in " + cAlphaFields(9) + " = " + Alphas(9));
                    ShowContinueError(state, "schedule values must be (>0., <=1.)");
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:DX:MultiSpeed")) {
                MSHeatPump(MSHPNum).HeatCoilType = MultiSpeedHeatingCoil;
                MSHeatPump(MSHPNum).HeatCoilNum =
                    state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Coil:Heating:DX:MultiSpeed", Alphas(11));
                MSHeatPump(MSHPNum).DXHeatCoilName = Alphas(11);
                if (MSHeatPump(MSHPNum).HeatCoilNum <= 0) {
                    ShowSevereError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(state, cAlphaFields(11) + " \"" + Alphas(11) + "\" not found.");
                    ShowContinueError(state, cAlphaFields(10) + " must be Coil:Heating:DX:MultiSpeed ");
                    ShowFatalError(state,
                                   std::string{RoutineName} + "Errors found in getting " + state.dataHVACMultiSpdHP->CurrentModuleObject +
                                       " input. Preceding condition(s) causes termination.");
                    ErrorsFound = true;
                }
                LocalError = false;
                GetDXCoilIndex(
                    state, MSHeatPump(MSHPNum).DXHeatCoilName, MSHeatPump(MSHPNum).DXHeatCoilIndex, LocalError, "Coil:Heating:DX:MultiSpeed");
                if (LocalError) {
                    ShowSevereError(state, "The index of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                HeatingCoilInletNode = GetDXCoilInletNode(state, Alphas(10), Alphas(11), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The inlet node number of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                HeatingCoilOutletNode = GetDXCoilOutletNode(state, Alphas(10), Alphas(11), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The outlet node number of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                MSHeatPump(MSHPNum).MinOATCompressorHeating =
                    DXCoils::GetMinOATCompressorUsingIndex(state, MSHeatPump(MSHPNum).DXHeatCoilIndex, LocalError);
                if (LocalError) {
                    ShowContinueError(state,
                                      "...for heating coil. Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    LocalError = false;
                }
                SetUpCompSets(state,
                              state.dataHVACMultiSpdHP->CurrentModuleObject,
                              MSHeatPump(MSHPNum).Name,
                              "Coil:Heating:DX:MultiSpeed",
                              MSHeatPump(MSHPNum).DXHeatCoilName,
                              "UNDEFINED",
                              "UNDEFINED");
            } else if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Electric:MultiStage") ||
                       UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Gas:MultiStage")) {

                if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Electric:MultiStage")) {
                    MSHeatPump(MSHPNum).HeatCoilType = Coil_HeatingElectric_MultiStage;
                    MSHeatPump(MSHPNum).HeatCoilNum =
                        state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Coil:Heating:Electric:MultiStage", Alphas(11));
                    if (MSHeatPump(MSHPNum).HeatCoilNum <= 0) {
                        ShowSevereError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError(state, cAlphaFields(11) + " \"" + Alphas(11) + "\" not found.");
                        ShowContinueError(state, cAlphaFields(10) + " must be Coil:Heating:Electric:MultiStage ");
                        ShowFatalError(state,
                                       std::string{RoutineName} + "Errors found in getting " + state.dataHVACMultiSpdHP->CurrentModuleObject +
                                           " input. Preceding condition(s) causes termination.");
                        ErrorsFound = true;
                    }
                } else {
                    MSHeatPump(MSHPNum).HeatCoilType = Coil_HeatingGas_MultiStage;
                    MSHeatPump(MSHPNum).HeatCoilNum =
                        state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Coil:Heating:Gas:MultiStage", Alphas(11));
                    if (MSHeatPump(MSHPNum).HeatCoilNum <= 0) {
                        ShowSevereError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError(state, cAlphaFields(11) + " \"" + Alphas(11) + "\" not found.");
                        ShowContinueError(state, cAlphaFields(10) + " must be Coil:Heating:Gas:MultiStage ");
                        ShowFatalError(state,
                                       std::string{RoutineName} + "Errors found in getting " + state.dataHVACMultiSpdHP->CurrentModuleObject +
                                           " input. Preceding condition(s) causes termination.");
                        ErrorsFound = true;
                    }
                }
                MSHeatPump(MSHPNum).HeatCoilName = Alphas(11);
                LocalError = false;
                if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Electric:MultiStage")) {
                    GetCoilIndex(state, MSHeatPump(MSHPNum).HeatCoilName, MSHeatPump(MSHPNum).HeatCoilIndex, LocalError);
                } else {
                    GetCoilIndex(state, MSHeatPump(MSHPNum).HeatCoilName, MSHeatPump(MSHPNum).HeatCoilIndex, LocalError);
                }
                if (LocalError) {
                    ShowSevereError(state, "The index of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                HeatingCoilInletNode = GetCoilInletNode(state, Alphas(10), Alphas(11), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The inlet node number of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                HeatingCoilOutletNode = GetCoilOutletNode(state, Alphas(10), Alphas(11), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The outlet node number of " + cAlphaFields(11) + " is not found \"" + Alphas(11) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Electric:MultiStage")) {
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Electric:MultiStage",
                                  MSHeatPump(MSHPNum).HeatCoilName,
                                  "UNDEFINED",
                                  "UNDEFINED");
                } else {
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Gas:MultiStage",
                                  MSHeatPump(MSHPNum).HeatCoilName,
                                  "UNDEFINED",
                                  "UNDEFINED");
                }
            } else if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Water")) {
                MSHeatPump(MSHPNum).HeatCoilType = Coil_HeatingWater;
                ValidateComponent(state, Alphas(10), Alphas(11), IsNotOK, state.dataHVACMultiSpdHP->CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    MSHeatPump(MSHPNum).HeatCoilName = Alphas(11);
                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    MSHeatPump(MSHPNum).CoilControlNode =
                        GetCoilWaterInletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil hot water max volume flow rate
                    errFlag = false;
                    MSHeatPump(MSHPNum).MaxCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the lemental Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode = GetWaterCoilInletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).CoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the lemental Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetWaterCoilOutletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Water",
                                  MSHeatPump(MSHPNum).HeatCoilName,
                                  state.dataLoopNodes->NodeID(HeatingCoilInletNode),
                                  state.dataLoopNodes->NodeID(HeatingCoilOutletNode));
                }
            } else if (UtilityRoutines::SameString(Alphas(10), "Coil:Heating:Steam")) {
                MSHeatPump(MSHPNum).HeatCoilType = Coil_HeatingSteam;
                ValidateComponent(state, Alphas(10), Alphas(11), IsNotOK, state.dataHVACMultiSpdHP->CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    MSHeatPump(MSHPNum).HeatCoilName = Alphas(11);
                    errFlag = false;
                    MSHeatPump(MSHPNum).HeatCoilNum = GetSteamCoilIndex(state, Alphas(10), MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (MSHeatPump(MSHPNum).HeatCoilNum == 0) {
                        ShowSevereError(state,
                                        state.dataHVACMultiSpdHP->CurrentModuleObject + " illegal " + cAlphaFields(10) + " = " +
                                            MSHeatPump(MSHPNum).HeatCoilName);
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the lemental Heating Coil steam inlet node number
                    errFlag = false;
                    MSHeatPump(MSHPNum).CoilControlNode =
                        GetCoilAirOutletNode(state, "Coil:Heating:Steam", MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the lemental Heating Coil steam max volume flow rate
                    MSHeatPump(MSHPNum).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHPNum).HeatCoilNum, errFlag);
                    if (MSHeatPump(MSHPNum).MaxCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity =
                            GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineNameNoColon);
                        MSHeatPump(MSHPNum).MaxCoilFluidFlow *= SteamDensity;
                    }

                    // Get the lemental Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode =
                        GetSteamCoilAirInletNode(state, MSHeatPump(MSHPNum).HeatCoilNum, MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).CoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the lemental Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetCoilAirOutletNode(state, MSHeatPump(MSHPNum).HeatCoilNum, MSHeatPump(MSHPNum).HeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Steam",
                                  MSHeatPump(MSHPNum).HeatCoilName,
                                  state.dataLoopNodes->NodeID(HeatingCoilInletNode),
                                  state.dataLoopNodes->NodeID(HeatingCoilOutletNode));
                }
            } else {
                ShowSevereError(state,
                                "The allowed " + cAlphaFields(10) +
                                    " are Coil:Heating:DX:MultiSpeed, Coil:Heating:Electric:MultiStage, and Coil:Heating:Gas:MultiStage  in " +
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                ShowContinueError(state, "The entered " + cAlphaFields(10) + " = \"" + Alphas(10) + "\".");
                ErrorsFound = true;
            }

            // MSHeatPump(MSHPNum).MinOATCompressor = Numbers(1); // deprecated, now uses coil MinOAT inputs

            if (UtilityRoutines::SameString(Alphas(12), "Coil:Cooling:DX:MultiSpeed")) {
                MSHeatPump(MSHPNum).CoolCoilType = MultiSpeedCoolingCoil;
                MSHeatPump(MSHPNum).DXCoolCoilName = Alphas(13);
                if (state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Coil:Cooling:DX:MultiSpeed", Alphas(13)) <= 0) {
                    ShowSevereError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(state, cAlphaFields(13) + " \"" + Alphas(13) + "\" not found.");
                    ShowContinueError(state, cAlphaFields(12) + " must be Coil:Cooling:DX:MultiSpeed ");
                    ShowFatalError(state,
                                   std::string{RoutineName} + "Errors found in getting " + state.dataHVACMultiSpdHP->CurrentModuleObject +
                                       " input. Preceding condition(s) causes termination.");
                    ErrorsFound = true;
                }
                LocalError = false;
                GetDXCoilIndex(
                    state, MSHeatPump(MSHPNum).DXCoolCoilName, MSHeatPump(MSHPNum).DXCoolCoilIndex, LocalError, "Coil:Cooling:DX:MultiSpeed");
                if (LocalError) {
                    ShowSevereError(state, "The index of " + cAlphaFields(13) + " is not found \"" + Alphas(13) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                CoolingCoilInletNode = GetDXCoilInletNode(state, Alphas(12), Alphas(13), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The inlet node number of " + cAlphaFields(13) + " is not found \"" + Alphas(13) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                CoolingCoilOutletNode = GetDXCoilOutletNode(state, Alphas(12), Alphas(13), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The outlet node number of " + cAlphaFields(13) + " is not found \"" + Alphas(13) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                MSHeatPump(MSHPNum).MinOATCompressorCooling =
                    DXCoils::GetMinOATCompressorUsingIndex(state, MSHeatPump(MSHPNum).DXCoolCoilIndex, LocalError);
                if (LocalError) {
                    ShowContinueError(state,
                                      "...for cooling coil. Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    LocalError = false;
                }
            } else {
                ShowSevereError(state,
                                "The allowed " + cAlphaFields(12) + " is Coil:Cooling:DX:MultiSpeed in " +
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                ShowContinueError(state, "The entered " + cAlphaFields(12) + " = \"" + Alphas(12) + "\".");
                ErrorsFound = true;
            }
            SetUpCompSets(state,
                          state.dataHVACMultiSpdHP->CurrentModuleObject,
                          MSHeatPump(MSHPNum).Name,
                          "Coil:Cooling:DX:MultiSpeed",
                          MSHeatPump(MSHPNum).DXCoolCoilName,
                          "UNDEFINED",
                          "UNDEFINED");

            // Get supplemental heating coil data
            MSHeatPump(MSHPNum).SuppHeatCoilName = Alphas(15);
            if (UtilityRoutines::SameString(Alphas(14), "Coil:Heating:Fuel")) {
                MSHeatPump(MSHPNum).SuppHeatCoilType = SuppHeatingCoilGas;
                errFlag = false;
                MSHeatPump(MSHPNum).SuppHeatCoilNum = GetHeatingCoilIndex(state, "Coil:Heating:Fuel", Alphas(15), errFlag);
                if (MSHeatPump(MSHPNum).SuppHeatCoilNum <= 0 || errFlag) {
                    ShowContinueError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(state, cAlphaFields(15) + " of type Coil:Heating:Fuel \"" + Alphas(15) + "\" not found.");
                    ErrorsFound = true;
                }

                // Get the Supplemental Heating Coil Node Numbers
                LocalError = false;
                SuppHeatCoilInletNode = GetHeatingCoilInletNode(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The inlet node number of " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                SuppHeatCoilOutletNode = GetHeatingCoilOutletNode(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The outlet node number of " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }

                // Get supplemental heating coil capacity to see if it is autosize
                MSHeatPump(MSHPNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The capacity " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                SetUpCompSets(state,
                              state.dataHVACMultiSpdHP->CurrentModuleObject,
                              MSHeatPump(MSHPNum).Name,
                              "Coil:Heating:Fuel",
                              MSHeatPump(MSHPNum).SuppHeatCoilName,
                              "UNDEFINED",
                              "UNDEFINED");
            }
            if (UtilityRoutines::SameString(Alphas(14), "Coil:Heating:Electric")) {
                MSHeatPump(MSHPNum).SuppHeatCoilType = SuppHeatingCoilElec;
                errFlag = false;
                MSHeatPump(MSHPNum).SuppHeatCoilNum = GetHeatingCoilIndex(state, "Coil:Heating:Electric", Alphas(15), errFlag);
                if (MSHeatPump(MSHPNum).SuppHeatCoilNum <= 0 || errFlag) {
                    ShowContinueError(state, "Configuration error in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(state, cAlphaFields(15) + " of type Coil:Heating:Electric \"" + Alphas(15) + "\" not found.");
                    ErrorsFound = true;
                }

                // Get the Supplemental Heating Coil Node Numbers
                LocalError = false;
                SuppHeatCoilInletNode = GetHeatingCoilInletNode(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The inlet node number of " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }
                SuppHeatCoilOutletNode = GetHeatingCoilOutletNode(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The outlet node number of " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }

                // Get supplemental heating coil capacity to see if it is autosize
                MSHeatPump(MSHPNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(state, Alphas(14), Alphas(15), LocalError);
                if (LocalError) {
                    ShowSevereError(state, "The capacity " + cAlphaFields(15) + " is not found \"" + Alphas(15) + "\"");
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ErrorsFound = true;
                    LocalError = false;
                }

                SetUpCompSets(state,
                              state.dataHVACMultiSpdHP->CurrentModuleObject,
                              MSHeatPump(MSHPNum).Name,
                              "Coil:Heating:Electric",
                              MSHeatPump(MSHPNum).SuppHeatCoilName,
                              "UNDEFINED",
                              "UNDEFINED");
            }

            if (UtilityRoutines::SameString(Alphas(14), "Coil:Heating:Water")) {
                MSHeatPump(MSHPNum).SuppHeatCoilType = Coil_HeatingWater;
                ValidateComponent(state, Alphas(14), MSHeatPump(MSHPNum).SuppHeatCoilName, IsNotOK, state.dataHVACMultiSpdHP->CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    MSHeatPump(MSHPNum).SuppCoilControlNode =
                        GetCoilWaterInletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil hot water max volume flow rate
                    errFlag = false;
                    MSHeatPump(MSHPNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Supplemental Heating Coil Inlet Node
                    errFlag = false;
                    SuppHeatCoilInletNode = GetWaterCoilInletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).SuppCoilAirInletNode = SuppHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Supplemental Heating Coil Outlet Node
                    errFlag = false;
                    SuppHeatCoilOutletNode = GetWaterCoilOutletNode(state, "Coil:Heating:Water", MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).SuppCoilAirOutletNode = SuppHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }
                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Water",
                                  MSHeatPump(MSHPNum).SuppHeatCoilName,
                                  state.dataLoopNodes->NodeID(SuppHeatCoilInletNode),
                                  state.dataLoopNodes->NodeID(SuppHeatCoilOutletNode));
                }
            }
            if (UtilityRoutines::SameString(Alphas(14), "Coil:Heating:Steam")) {
                MSHeatPump(MSHPNum).SuppHeatCoilType = Coil_HeatingSteam;
                ValidateComponent(state, Alphas(14), MSHeatPump(MSHPNum).SuppHeatCoilName, IsNotOK, state.dataHVACMultiSpdHP->CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    errFlag = false;
                    MSHeatPump(MSHPNum).SuppHeatCoilNum = GetSteamCoilIndex(state, Alphas(14), MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    if (MSHeatPump(MSHPNum).SuppHeatCoilNum == 0) {
                        ShowSevereError(state,
                                        state.dataHVACMultiSpdHP->CurrentModuleObject + " illegal " + cAlphaFields(14) + " = " +
                                            MSHeatPump(MSHPNum).SuppHeatCoilName);
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Supplemental Heating Coil steam inlet node number
                    errFlag = false;
                    MSHeatPump(MSHPNum).SuppCoilControlNode =
                        GetCoilAirOutletNode(state, "Coil:Heating:Steam", MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Supplemental Heating Coil steam max volume flow rate
                    MSHeatPump(MSHPNum).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHPNum).SuppHeatCoilNum, errFlag);
                    if (MSHeatPump(MSHPNum).MaxSuppCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity =
                            GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineNameNoColon);
                        MSHeatPump(MSHPNum).MaxSuppCoilFluidFlow *= SteamDensity;
                    }

                    // Get the Supplemental Heating Coil Inlet Node
                    errFlag = false;
                    SuppHeatCoilInletNode =
                        GetSteamCoilAirInletNode(state, MSHeatPump(MSHPNum).SuppHeatCoilNum, MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).SuppCoilAirInletNode = SuppHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Supplemental Heating Coil Outlet Node
                    errFlag = false;
                    SuppHeatCoilOutletNode =
                        GetCoilAirOutletNode(state, MSHeatPump(MSHPNum).SuppHeatCoilNum, MSHeatPump(MSHPNum).SuppHeatCoilName, errFlag);
                    MSHeatPump(MSHPNum).SuppCoilAirOutletNode = SuppHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHPNum).Name);
                        ErrorsFound = true;
                    }

                    SetUpCompSets(state,
                                  state.dataHVACMultiSpdHP->CurrentModuleObject,
                                  MSHeatPump(MSHPNum).Name,
                                  "Coil:Heating:Steam",
                                  MSHeatPump(MSHPNum).SuppHeatCoilName,
                                  state.dataLoopNodes->NodeID(SuppHeatCoilInletNode),
                                  state.dataLoopNodes->NodeID(SuppHeatCoilOutletNode));
                }
            }

            if (MSHeatPump(MSHPNum).SuppHeatCoilType == 0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cAlphaFields(14) +
                                    " is not allowed = " + Alphas(14));
                ShowContinueError(state, "Valid choices are Coil:Heating:Fuel,Coil:Heating:Electric,Coil:Heating:Steam,or Coil:Heating:Water");
                ErrorsFound = true;
            }

            MSHeatPump(MSHPNum).SuppMaxAirTemp = Numbers(2);
            MSHeatPump(MSHPNum).SuppMaxOATemp = Numbers(3);
            if (MSHeatPump(MSHPNum).SuppMaxOATemp > 21.0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cNumericFields(3) +
                                    " is greater than 21.0");
                ShowContinueError(state, format("The input value is {:.2R}", Numbers(3)));
                ErrorsFound = true;
            }

            MSHeatPump(MSHPNum).AuxOnCyclePower = Numbers(4);
            MSHeatPump(MSHPNum).AuxOffCyclePower = Numbers(5);
            if (MSHeatPump(MSHPNum).AuxOnCyclePower < 0.0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", A negative value for " +
                                    cNumericFields(4) + " is not allowed ");
                ErrorsFound = true;
            }
            if (MSHeatPump(MSHPNum).AuxOffCyclePower < 0.0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", A negative value for " +
                                    cNumericFields(5) + " is not allowed ");
                ErrorsFound = true;
            }

            // Heat recovery
            MSHeatPump(MSHPNum).DesignHeatRecFlowRate = Numbers(6);
            if (MSHeatPump(MSHPNum).DesignHeatRecFlowRate > 0.0) {
                MSHeatPump(MSHPNum).HeatRecActive = true;
                MSHeatPump(MSHPNum).DesignHeatRecMassFlowRate =
                    RhoH2O(DataGlobalConstants::HWInitConvTemp) * MSHeatPump(MSHPNum).DesignHeatRecFlowRate;
                MSHeatPump(MSHPNum).HeatRecInletNodeNum =
                    GetOnlySingleNode(state,
                                      Alphas(16),
                                      ErrorsFound,
                                      DataLoopNode::ConnectionObjectType::AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed,
                                      Alphas(1),
                                      DataLoopNode::NodeFluidType::Water,
                                      DataLoopNode::ConnectionType::Inlet,
                                      NodeInputManager::CompFluidStream::Tertiary,
                                      ObjectIsNotParent);
                if (MSHeatPump(MSHPNum).HeatRecInletNodeNum == 0) {
                    ShowSevereError(state,
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", Missing " +
                                        cAlphaFields(16) + '.');
                    ErrorsFound = true;
                }
                MSHeatPump(MSHPNum).HeatRecOutletNodeNum =
                    GetOnlySingleNode(state,
                                      Alphas(17),
                                      ErrorsFound,
                                      DataLoopNode::ConnectionObjectType::AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed,
                                      Alphas(1),
                                      DataLoopNode::NodeFluidType::Water,
                                      DataLoopNode::ConnectionType::Outlet,
                                      NodeInputManager::CompFluidStream::Tertiary,
                                      ObjectIsNotParent);
                if (MSHeatPump(MSHPNum).HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state,
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", Missing " +
                                        cAlphaFields(17) + '.');
                    ErrorsFound = true;
                }
                TestCompSet(state, state.dataHVACMultiSpdHP->CurrentModuleObject, Alphas(1), Alphas(16), Alphas(17), "MSHP Heat receovery Nodes");
                SetMSHPDXCoilHeatRecoveryFlag(state, MSHeatPump(MSHPNum).DXCoolCoilIndex);
                if (MSHeatPump(MSHPNum).DXHeatCoilIndex > 0) {
                    SetMSHPDXCoilHeatRecoveryFlag(state, MSHeatPump(MSHPNum).DXHeatCoilIndex);
                }
            } else {
                MSHeatPump(MSHPNum).HeatRecActive = false;
                MSHeatPump(MSHPNum).DesignHeatRecMassFlowRate = 0.0;
                MSHeatPump(MSHPNum).HeatRecInletNodeNum = 0;
                MSHeatPump(MSHPNum).HeatRecOutletNodeNum = 0;
                if (!lAlphaBlanks(16) || !lAlphaBlanks(17)) {
                    ShowWarningError(state,
                                     "Since " + cNumericFields(6) + " = 0.0, heat recovery is inactive for " +
                                         state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state, "However, " + cAlphaFields(16) + " or " + cAlphaFields(17) + " was specified.");
                }
            }
            MSHeatPump(MSHPNum).MaxHeatRecOutletTemp = Numbers(7);
            if (MSHeatPump(MSHPNum).MaxHeatRecOutletTemp < 0.0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", The value for " +
                                    cNumericFields(7) + " is below 0.0");
                ErrorsFound = true;
            }
            if (MSHeatPump(MSHPNum).MaxHeatRecOutletTemp > 100.0) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", The value for " +
                                    cNumericFields(7) + " is above 100.0");
                ErrorsFound = true;
            }

            MSHeatPump(MSHPNum).IdleVolumeAirRate = Numbers(8);
            if (MSHeatPump(MSHPNum).IdleVolumeAirRate < 0.0 && MSHeatPump(MSHPNum).IdleVolumeAirRate != AutoSize) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " + cNumericFields(8) +
                                    " cannot be less than zero.");
                ErrorsFound = true;
            }

            //     AirFlowControl only valid if fan opmode = ContFanCycCoil
            if (MSHeatPump(MSHPNum).IdleVolumeAirRate == 0.0) {
                MSHeatPump(MSHPNum).AirFlowControl = AirflowControl::UseCompressorOnFlow;
            } else {
                MSHeatPump(MSHPNum).AirFlowControl = AirflowControl::UseCompressorOffFlow;
            }

            //   Initialize last mode of compressor operation
            MSHeatPump(MSHPNum).LastMode = ModeOfOperation::HeatingMode;

            MSHeatPump(MSHPNum).NumOfSpeedHeating = Numbers(9);
            if (MSHeatPump(MSHPNum).NumOfSpeedHeating < 2 || MSHeatPump(MSHPNum).NumOfSpeedHeating > 4) {
                if (MSHeatPump(MSHPNum).HeatCoilType == MultiSpeedHeatingCoil) {
                    ShowSevereError(state,
                                    state.dataHVACMultiSpdHP->CurrentModuleObject + ", The maximum " + cNumericFields(9) +
                                        " is 4, and the minimum number is 2");
                    ShowContinueError(state, format("The input value is {:.0R}", Numbers(9)));
                    ErrorsFound = true;
                }
            }
            MSHeatPump(MSHPNum).NumOfSpeedCooling = Numbers(10);
            if (MSHeatPump(MSHPNum).NumOfSpeedCooling < 2 || MSHeatPump(MSHPNum).NumOfSpeedCooling > 4) {
                ShowSevereError(state,
                                state.dataHVACMultiSpdHP->CurrentModuleObject + ", The maximum " + cNumericFields(10) +
                                    " is 4, and the minimum number is 2");
                ShowContinueError(state, format("The input value is {:.0R}", Numbers(10)));
                ErrorsFound = true;
            }

            // Generate a dynamic array for heating
            if (MSHeatPump(MSHPNum).NumOfSpeedHeating > 0) {
                MSHeatPump(MSHPNum).HeatMassFlowRate.allocate(MSHeatPump(MSHPNum).NumOfSpeedHeating);
                MSHeatPump(MSHPNum).HeatVolumeFlowRate.allocate(MSHeatPump(MSHPNum).NumOfSpeedHeating);
                MSHeatPump(MSHPNum).HeatingSpeedRatio.allocate(MSHeatPump(MSHPNum).NumOfSpeedHeating);
                MSHeatPump(MSHPNum).HeatingSpeedRatio = 1.0;
                for (i = 1; i <= MSHeatPump(MSHPNum).NumOfSpeedHeating; ++i) {
                    MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) = Numbers(10 + i);
                    if (MSHeatPump(MSHPNum).HeatCoilType == MultiSpeedHeatingCoil) {
                        if (MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) <= 0.0 && MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) != AutoSize) {
                            ShowSevereError(state,
                                            state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " +
                                                cNumericFields(10 + i) + " must be greater than zero.");
                            ErrorsFound = true;
                        }
                    }
                }
                // Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
                for (i = 2; i <= MSHeatPump(MSHPNum).NumOfSpeedHeating; ++i) {
                    if (MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) == AutoSize) continue;
                    Found = false;
                    for (j = i - 1; j >= 1; --j) {
                        if (MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) != AutoSize) {
                            Found = true;
                            break;
                        }
                    }
                    if (Found) {
                        if (MSHeatPump(MSHPNum).HeatVolumeFlowRate(i) < MSHeatPump(MSHPNum).HeatVolumeFlowRate(j)) {
                            ShowSevereError(state,
                                            state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " +
                                                cNumericFields(10 + i));
                            ShowContinueError(state, " cannot be less than " + cNumericFields(10 + j));
                            ErrorsFound = true;
                        }
                    }
                }
            }

            if (state.dataGlobal->DoCoilDirectSolutions) {
                int MaxNumber = std::max(MSHeatPump(MSHPNum).NumOfSpeedCooling, MSHeatPump(MSHPNum).NumOfSpeedHeating);
                MSHeatPump(MSHPNum).FullOutput.allocate(MaxNumber);
                DXCoils::DisableLatentDegradation(state, MSHeatPump(MSHPNum).DXCoolCoilIndex);
            }
            // Generate a dynamic array for cooling
            if (MSHeatPump(MSHPNum).NumOfSpeedCooling > 0) {
                MSHeatPump(MSHPNum).CoolMassFlowRate.allocate(MSHeatPump(MSHPNum).NumOfSpeedCooling);
                MSHeatPump(MSHPNum).CoolVolumeFlowRate.allocate(MSHeatPump(MSHPNum).NumOfSpeedCooling);
                MSHeatPump(MSHPNum).CoolingSpeedRatio.allocate(MSHeatPump(MSHPNum).NumOfSpeedCooling);
                MSHeatPump(MSHPNum).CoolingSpeedRatio = 1.0;
                for (i = 1; i <= MSHeatPump(MSHPNum).NumOfSpeedCooling; ++i) {
                    MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) = Numbers(14 + i);
                    if (MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) <= 0.0 && MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) != AutoSize) {
                        ShowSevereError(state,
                                        state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " +
                                            cNumericFields(14 + i) + " must be greater than zero.");
                        ErrorsFound = true;
                    }
                }
                // Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
                for (i = 2; i <= MSHeatPump(MSHPNum).NumOfSpeedCooling; ++i) {
                    if (MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) == AutoSize) continue;
                    Found = false;
                    for (j = i - 1; j >= 1; --j) {
                        if (MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) != AutoSize) {
                            Found = true;
                            break;
                        }
                    }
                    if (Found) {
                        if (MSHeatPump(MSHPNum).CoolVolumeFlowRate(i) < MSHeatPump(MSHPNum).CoolVolumeFlowRate(j)) {
                            ShowSevereError(state,
                                            state.dataHVACMultiSpdHP->CurrentModuleObject + ", \"" + MSHeatPump(MSHPNum).Name + "\", " +
                                                cNumericFields(14 + i));
                            ShowContinueError(state, " cannot be less than " + cNumericFields(14 + j));
                            ErrorsFound = true;
                        }
                    }
                }
            }

            // Check node integrity
            if (MSHeatPump(MSHPNum).FanPlaceType == BlowThru) {
                if (MSHeatPump(MSHPNum).FanInletNode != MSHeatPump(MSHPNum).AirInletNodeNum) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state,
                                      "When a blow through fan is specified, the fan inlet node name must be the same as the " + cAlphaFields(3));
                    ShowContinueError(state, "...Fan inlet node name           = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).FanInletNode));
                    ShowContinueError(state, "..." + cAlphaFields(3) + " = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).AirInletNodeNum));
                    ErrorsFound = true;
                }
                if (MSHeatPump(MSHPNum).FanOutletNode != CoolingCoilInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil inlet node name.");
                    ShowContinueError(state, "...Fan outlet node name         = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).FanOutletNode));
                    ShowContinueError(state, "...Cooling coil inlet node name = " + state.dataLoopNodes->NodeID(CoolingCoilInletNode));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state, "The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                    ShowContinueError(state, "...Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolingCoilOutletNode));
                    ShowContinueError(state, "...Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != SuppHeatCoilInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state,
                                      "When a blow through fan is specified, the heating coil outlet node name must be the same as the reheat coil "
                                      "inlet node name.");
                    ShowContinueError(state, "...Heating coil outlet node name = " + state.dataLoopNodes->NodeID(HeatingCoilOutletNode));
                    ShowContinueError(state, "...Reheat coil inlet node name   = " + state.dataLoopNodes->NodeID(SuppHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SuppHeatCoilOutletNode != MSHeatPump(MSHPNum).AirOutletNodeNum) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state, "The supplemental heating coil outlet node name must be the same as the " + cAlphaFields(4));
                    ShowContinueError(state,
                                      "...Supplemental heating coil outlet node name   = " + state.dataLoopNodes->NodeID(SuppHeatCoilOutletNode));
                    ShowContinueError(state, "..." + cAlphaFields(4) + " = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).AirOutletNodeNum));
                    ErrorsFound = true;
                }
            } else {
                if (CoolingCoilInletNode != MSHeatPump(MSHPNum).AirInletNodeNum) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(
                        state, "When a draw through fan is specified, the cooling coil inlet node name must be the same as the " + cAlphaFields(3));
                    ShowContinueError(state, "...Cooling coil inlet node name  = " + state.dataLoopNodes->NodeID(CoolingCoilInletNode));
                    ShowContinueError(state, "..." + cAlphaFields(3) + " = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).AirInletNodeNum));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state, "The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                    ShowContinueError(state, "...Cooling coil outlet node name = " + state.dataLoopNodes->NodeID(CoolingCoilOutletNode));
                    ShowContinueError(state, "...Heating coil inlet node name  = " + state.dataLoopNodes->NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != MSHeatPump(MSHPNum).FanInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name.");
                    ShowContinueError(state, "...Heating coil outlet node name = " + state.dataLoopNodes->NodeID(HeatingCoilOutletNode));
                    ShowContinueError(state, "...Fan inlet node name           = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).FanInletNode));
                    ErrorsFound = true;
                }
                if (MSHeatPump(MSHPNum).FanOutletNode != SuppHeatCoilInletNode) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(
                        state, "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil inlet node name.");
                    ShowContinueError(state, "...Fan outlet node name        = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).FanOutletNode));
                    ShowContinueError(state, "...Reheat coil inlet node name = " + state.dataLoopNodes->NodeID(SuppHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SuppHeatCoilOutletNode != MSHeatPump(MSHPNum).AirOutletNodeNum) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state, "The reheat coil outlet node name must be the same as the " + cAlphaFields(4));
                    ShowContinueError(state, "...Reheat coil outlet node name   = " + state.dataLoopNodes->NodeID(SuppHeatCoilOutletNode));
                    ShowContinueError(state, "..." + cAlphaFields(4) + " = " + state.dataLoopNodes->NodeID(MSHeatPump(MSHPNum).AirOutletNodeNum));
                    ErrorsFound = true;
                }
            }

            // Ensure the numbers of speeds defined in the parent object are equal to the numbers defined in coil objects
            if (MSHeatPump(MSHPNum).HeatCoilType == MultiSpeedHeatingCoil) {
                i = GetDXCoilNumberOfSpeeds(state, Alphas(10), Alphas(11), ErrorsFound);
                if (MSHeatPump(MSHPNum).NumOfSpeedHeating != i) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state,
                                      "The " + cNumericFields(9) + " is not equal to the number defined in " + cAlphaFields(11) + " = " + Alphas(11));
                    ErrorsFound = true;
                }
            } else if (MSHeatPump(MSHPNum).HeatCoilType == Coil_HeatingElectric_MultiStage ||
                       MSHeatPump(MSHPNum).HeatCoilType == Coil_HeatingGas_MultiStage) {
                i = GetHeatingCoilNumberOfStages(state, Alphas(10), Alphas(11), ErrorsFound);
                if (MSHeatPump(MSHPNum).NumOfSpeedHeating != i) {
                    ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                    ShowContinueError(state,
                                      "The " + cNumericFields(9) + " is not equal to the number defined in " + cAlphaFields(11) + " = " + Alphas(11));
                    ErrorsFound = true;
                }
            }
            i = GetDXCoilNumberOfSpeeds(state, Alphas(12), Alphas(13), ErrorsFound);
            if (MSHeatPump(MSHPNum).NumOfSpeedCooling != i) {
                ShowSevereError(state, "For " + state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHPNum).Name + "\"");
                ShowContinueError(state,
                                  "The " + cNumericFields(10) + " is not equal to the number defined in " + cAlphaFields(13) + " = " + Alphas(13));
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state,
                           std::string{RoutineName} + "Errors found in getting " + state.dataHVACMultiSpdHP->CurrentModuleObject +
                               " input.  Preceding condition(s) causes termination.");
        }
        // End of multispeed heat pump

        for (MSHPNum = 1; MSHPNum <= state.dataHVACMultiSpdHP->NumMSHeatPumps; ++MSHPNum) {
            // Setup Report Variables for MSHP Equipment
            SetupOutputVariable(state,
                                "Unitary System Ancillary Electricity Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).AuxElecPower,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Cooling Ancillary Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).AuxElecCoolConsumption,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                MSHeatPump(MSHPNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Unitary System Heating Ancillary Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).AuxElecHeatConsumption,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                MSHeatPump(MSHPNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                MSHeatPump(MSHPNum).FanPartLoadRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                MSHeatPump(MSHPNum).CompPartLoadRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Electricity Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).ElecPower,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).ElecPowerConsumption,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System DX Coil Cycling Ratio",
                                OutputProcessor::Unit::None,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).CycRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System DX Coil Speed Ratio",
                                OutputProcessor::Unit::None,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).SpeedRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System DX Coil Speed Level",
                                OutputProcessor::Unit::None,
                                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).SpeedNum,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).TotCoolEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Total Heating Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).TotHeatEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).SensCoolEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).SensHeatEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).LatCoolEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            SetupOutputVariable(state,
                                "Unitary System Latent Heating Rate",
                                OutputProcessor::Unit::W,
                                MSHeatPump(MSHPNum).LatHeatEnergyRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                MSHeatPump(MSHPNum).Name);
            if (MSHeatPump(MSHPNum).HeatRecActive) {
                SetupOutputVariable(state,
                                    "Unitary System Heat Recovery Rate",
                                    OutputProcessor::Unit::W,
                                    MSHeatPump(MSHPNum).HeatRecoveryRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    MSHeatPump(MSHPNum).Name);
                SetupOutputVariable(state,
                                    "Unitary System Heat Recovery Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    MSHeatPump(MSHPNum).HeatRecoveryInletTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    MSHeatPump(MSHPNum).Name);
                SetupOutputVariable(state,
                                    "Unitary System Heat Recovery Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    MSHeatPump(MSHPNum).HeatRecoveryOutletTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    MSHeatPump(MSHPNum).Name);
                SetupOutputVariable(state,
                                    "Unitary System Heat Recovery Fluid Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    MSHeatPump(MSHPNum).HeatRecoveryMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    MSHeatPump(MSHPNum).Name);
                SetupOutputVariable(state,
                                    "Unitary System Heat Recovery Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHPNum).HeatRecoveryEnergy,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    MSHeatPump(MSHPNum).Name);
            }
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            for (int coilNUM = 1; coilNUM <= int(state.dataHVACMultiSpdHP->MSHeatPump.size()); ++coilNUM) {
                SetupEMSActuator(state,
                                 "Coil Speed Control",
                                 state.dataHVACMultiSpdHP->MSHeatPump(coilNUM).Name,
                                 "Unitary System DX Coil Speed Value",
                                 "[]",
                                 state.dataHVACMultiSpdHP->MSHeatPump(coilNUM).EMSOverrideCoilSpeedNumOn,
                                 state.dataHVACMultiSpdHP->MSHeatPump(coilNUM).EMSOverrideCoilSpeedNumValue);
            }
        }
    }

    //******************************************************************************

    void InitMSHeatPump(EnergyPlusData &state,
                        int const MSHeatPumpNum,       // Engine driven heat pump number
                        bool const FirstHVACIteration, // TRUE if first HVAC iteration
                        int const AirLoopNum,          // air loop index
                        Real64 &QZnReq,                // Heating/Cooling load for all served zones
                        Real64 &OnOffAirFlowRatio      // Ratio of compressor ON airflow to average airflow over timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    July 2007
        //       MODIFIED         Bereket Nigusse, June 2010 - added a procedure to calculate supply air flow fraction
        //                        through controlled zone
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the multispeed heat pump (MSHP) components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations. The MSHP system is simulated with no load (coils off) to
        // determine the outlet temperature. A setpoint temperature is calculated on FirstHVACIteration = TRUE.
        // Once the setpoint is calculated, the inlet mass flow rate on FirstHVACIteration = FALSE is used to
        // determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
        // temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

        // Using/Aliasing
        using DataSizing::AutoSize;
        using Fans::GetFanIndex;
        using Fans::GetFanVolFlow;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSatDensityRefrig;

        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using ScheduleManager::GetCurrentScheduleValue;
        using SteamCoils::SimulateSteamCoilComponents;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        auto &GetSteamCoilCapacity(SteamCoils::GetCoilCapacity);
        using DXCoils::GetDXCoilAvailSchPtr;
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static constexpr std::string_view RoutineName("InitMSHeatPump");
        int InNode;     // Inlet node number in MSHP loop
        int OutNode;    // Outlet node number in MSHP loop
        int ZoneInNode; // Zone inlet node number in the controlled zone for MSHP
        Real64 RhoAir;  // Air density at InNode

        Real64 QSensUnitOut; // Output of MSHP system with coils off
        Real64 PartLoadFrac; // Part-load ratio
        int ZoneNum;
        int i;                 // Index to speed
        int NumOfSpeedCooling; // Number of speeds for cooling
        int NumOfSpeedHeating; // Number of speeds for heating
        Real64 DeltaMassRate;  // Difference of mass flow rate between inlet node and system outlet node

        int ZoneInSysIndex(0);                            // number of zone inlet nodes counter in an airloop
        int NumAirLoopZones(0);                           // number of zone inlet nodes in an air loop
        int ZoneInletNodeNum(0);                          // zone inlet nodes node number
        Real64 SumOfMassFlowRateMax(0.0);                 // the sum of mass flow rates at inlet to zones in an airloop
        Real64 CntrlZoneTerminalUnitMassFlowRateMax(0.0); // Maximum mass flow rate through controlled zone terminal unit
        bool errFlag;
        Real64 rho;    // local fluid density
        Real64 MdotHR; // local temporary for heat recovery fluid mass flow rate (kg/s)
        Real64 ZoneLoadToCoolSPSequenced;
        Real64 ZoneLoadToHeatSPSequenced;

        bool ErrorsFound(false);        // flag returned from mining call
        int SteamIndex(0);              // index of steam quality for steam heating coil
        Real64 mdot(0.0);               // local temporary for mass flow rate (kg/s)
        Real64 SteamDensity(0.0);       // density of steam at 100C, used for steam heating coils
        Real64 CoilMaxVolFlowRate(0.0); // coil fluid maximum volume flow rate
        Real64 QActual(0.0);            // coil actual capacity
        int CoilAvailSchPtr(0);         // DX coil availability schedule pointer

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        InNode = MSHeatPump(MSHeatPumpNum).AirInletNodeNum;
        OutNode = MSHeatPump(MSHeatPumpNum).AirOutletNodeNum;
        NumOfSpeedCooling = MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling;
        NumOfSpeedHeating = MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating;

        ++state.dataHVACMultiSpdHP->AirLoopPass;
        if (state.dataHVACMultiSpdHP->AirLoopPass > 2) state.dataHVACMultiSpdHP->AirLoopPass = 1;

        if (MSHeatPump(MSHeatPumpNum).MyPlantScantFlag && allocated(state.dataPlnt->PlantLoop)) {
            if (MSHeatPump(MSHeatPumpNum).HeatRecActive) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        MSHeatPump(MSHeatPumpNum).Name,
                                        DataPlant::PlantEquipmentType::MultiSpeedHeatPumpRecovery,
                                        MSHeatPump(MSHeatPumpNum).HRPlantLoc,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitMSHeatPump: Program terminated for previous conditions.");
                }

                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;
            } else {
                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;
            }
            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingWater) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                        DataPlant::PlantEquipmentType::CoilWaterSimpleHeating,
                                        MSHeatPump(MSHeatPumpNum).plantLoc,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitMSHeatPump: Program terminated for previous conditions.");
                }
                MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).HeatCoilName, ErrorsFound);

                if (MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow > 0.0) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).plantLoc.loopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).plantLoc.loopNum).FluidIndex,
                                           RoutineName);
                    MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).HeatCoilName, ErrorsFound) * rho;
                }
                // fill outlet node for coil
                MSHeatPump(MSHeatPumpNum).CoilOutletNode =
                    DataPlant::CompData::getPlantComponent(state, MSHeatPump(MSHeatPumpNum).plantLoc).NodeNumOut;
                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;

            } else if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingSteam) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                        DataPlant::PlantEquipmentType::CoilSteamAirHeating,
                                        MSHeatPump(MSHeatPumpNum).plantLoc,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitMSHeatPump: Program terminated for previous conditions.");
                }
                MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHeatPumpNum).HeatCoilNum, ErrorsFound);
                if (MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow > 0.0) {
                    SteamIndex =
                        0; // Function GetSatDensityRefrig will look up steam index if 0 is passed // TODO: Why do you want to re-look this up?
                    SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                    MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow *= SteamDensity;
                }

                // fill outlet node for coil
                MSHeatPump(MSHeatPumpNum).CoilOutletNode =
                    DataPlant::CompData::getPlantComponent(state, MSHeatPump(MSHeatPumpNum).plantLoc).NodeNumOut;
                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;
            }
            if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingWater) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        MSHeatPump(MSHeatPumpNum).SuppHeatCoilName,
                                        DataPlant::PlantEquipmentType::CoilWaterSimpleHeating,
                                        MSHeatPump(MSHeatPumpNum).SuppPlantLoc,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitMSHeatPump: Program terminated for previous conditions.");
                }
                MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, ErrorsFound);

                if (MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow > 0.0) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).SuppPlantLoc.loopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).SuppPlantLoc.loopNum).FluidIndex,
                                           RoutineName);
                    MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, ErrorsFound) * rho;
                }
                // fill outlet node for coil
                MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode =
                    DataPlant::CompData::getPlantComponent(state, MSHeatPump(MSHeatPumpNum).SuppPlantLoc).NodeNumOut;
                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;

            } else if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingSteam) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        MSHeatPump(MSHeatPumpNum).SuppHeatCoilName,
                                        DataPlant::PlantEquipmentType::CoilSteamAirHeating,
                                        MSHeatPump(MSHeatPumpNum).SuppPlantLoc,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitMSHeatPump: Program terminated for previous conditions.");
                }
                MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow =
                    GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum, ErrorsFound);
                if (MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow > 0.0) {
                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                    MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow *= SteamDensity;
                }

                // fill outlet node for coil
                MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode =
                    DataPlant::CompData::getPlantComponent(state, MSHeatPump(MSHeatPumpNum).SuppPlantLoc).NodeNumOut;
                MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;
            }
        } else if (MSHeatPump(MSHeatPumpNum).MyPlantScantFlag && !state.dataGlobal->AnyPlantInModel) {
            MSHeatPump(MSHeatPumpNum).MyPlantScantFlag = false;
        }

        if (!state.dataGlobal->SysSizingCalc && MSHeatPump(MSHeatPumpNum).MySizeFlag) {
            GetFanVolFlow(state, MSHeatPump(MSHeatPumpNum).FanNum, MSHeatPump(MSHeatPumpNum).FanVolFlow);
            SizeMSHeatPump(state, MSHeatPumpNum);
            MSHeatPump(MSHeatPumpNum).FlowFraction = 1.0;
            MSHeatPump(MSHeatPumpNum).MySizeFlag = false;
            // Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CycFanSchedPtr = MSHeatPump(MSHeatPumpNum).FanSchedPtr;
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).UnitarySys = true;
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                false; // affects child coil sizing by allowing coil to size itself instead of parent telling coil what size to use
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).FanOpMode = MSHeatPump(MSHeatPumpNum).OpMode;
        }

        if (allocated(state.dataZoneEquip->ZoneEquipConfig) && MSHeatPump(MSHeatPumpNum).MyCheckFlag) {
            int zoneNum = state.dataHeatBal->Zone(MSHeatPump(MSHeatPumpNum).ControlZoneNum).ZoneEqNum;
            int zoneInlet = MSHeatPump(MSHeatPumpNum).ZoneInletNode;
            int coolingPriority = 0;
            int heatingPriority = 0;
            // setup furnace zone equipment sequence information based on finding matching air terminal
            if (state.dataZoneEquip->ZoneEquipConfig(zoneNum).EquipListIndex > 0) {
                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(zoneNum).EquipListIndex)
                    .getPrioritiesForInletNode(state, zoneInlet, coolingPriority, heatingPriority);
                MSHeatPump(MSHeatPumpNum).ZoneSequenceCoolingNum = coolingPriority;
                MSHeatPump(MSHeatPumpNum).ZoneSequenceHeatingNum = heatingPriority;
            }
            MSHeatPump(MSHeatPumpNum).MyCheckFlag = false;
            if (MSHeatPump(MSHeatPumpNum).ZoneSequenceCoolingNum == 0 || MSHeatPump(MSHeatPumpNum).ZoneSequenceHeatingNum == 0) {
                ShowSevereError(state,
                                "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, \"" + MSHeatPump(MSHeatPumpNum).Name +
                                    "\": Airloop air terminal in the zone equipment list for zone = " + MSHeatPump(MSHeatPumpNum).ControlZoneName +
                                    " not found or is not allowed Zone Equipment Cooling or Heating Sequence = 0.");
                ShowFatalError(state,
                               "Subroutine InitMSHeatPump: Errors found in getting AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed input.  Preceding "
                               "condition(s) causes termination.");
            }
        }

        // Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
        NumAirLoopZones =
            state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled + state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
        if (allocated(state.dataAirLoop->AirToZoneNodeInfo) && MSHeatPump(MSHeatPumpNum).MyFlowFracFlag) {
            state.dataHVACMultiSpdHP->FlowFracFlagReady = true;
            for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                // zone inlet nodes for cooling
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled > 0) {
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex) == -999) {
                        // the data structure for the zones inlet nodes has not been filled
                        state.dataHVACMultiSpdHP->FlowFracFlagReady = false;
                    }
                }
                // zone inlet nodes for heating
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesHeated > 0) {
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).TermUnitHeatInletNodes(ZoneInSysIndex) == -999) {
                        // the data structure for the zones inlet nodes has not been filled
                        state.dataHVACMultiSpdHP->FlowFracFlagReady = false;
                    }
                }
            }
        }
        if (allocated(state.dataAirLoop->AirToZoneNodeInfo) && state.dataHVACMultiSpdHP->FlowFracFlagReady) {
            SumOfMassFlowRateMax = 0.0; // initialize the sum of the maximum flows
            for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                ZoneInletNodeNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex);
                SumOfMassFlowRateMax += state.dataLoopNodes->Node(ZoneInletNodeNum).MassFlowRateMax;
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZoneInSysIndex) == MSHeatPump(MSHeatPumpNum).ControlZoneNum) {
                    CntrlZoneTerminalUnitMassFlowRateMax = state.dataLoopNodes->Node(ZoneInletNodeNum).MassFlowRateMax;
                }
            }
            if (SumOfMassFlowRateMax != 0.0 && MSHeatPump(MSHeatPumpNum).MyFlowFracFlag) {
                if (CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow) {
                    MSHeatPump(MSHeatPumpNum).FlowFraction = CntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
                } else {
                    ShowSevereError(state, state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHeatPumpNum).Name);
                    ShowContinueError(state, " The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.");
                }
                BaseSizer::reportSizerOutput(state,
                                             state.dataHVACMultiSpdHP->CurrentModuleObject,
                                             MSHeatPump(MSHeatPumpNum).Name,
                                             "Fraction of Supply Air Flow That Goes Through the Controlling Zone",
                                             MSHeatPump(MSHeatPumpNum).FlowFraction);
                MSHeatPump(MSHeatPumpNum).MyFlowFracFlag = false;
            }
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MSHeatPump(MSHeatPumpNum).MyEnvrnFlag) {
            RhoAir = state.dataEnvrn->StdRhoAir;
            // set the mass flow rates from the input volume flow rates
            for (i = 1; i <= NumOfSpeedCooling; ++i) {
                MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(i) = RhoAir * MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i);
            }
            for (i = 1; i <= NumOfSpeedHeating; ++i) {
                MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(i) = RhoAir * MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i);
            }
            MSHeatPump(MSHeatPumpNum).IdleMassFlowRate = RhoAir * MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate;
            // set the node max and min mass flow rates
            state.dataLoopNodes->Node(InNode).MassFlowRateMax =
                max(MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(NumOfSpeedCooling), MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(NumOfSpeedHeating));
            state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail =
                max(MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(NumOfSpeedCooling), MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(NumOfSpeedHeating));
            state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutNode) = state.dataLoopNodes->Node(InNode);
            MSHeatPump(MSHeatPumpNum).LoadLoss = 0.0;

            if ((MSHeatPump(MSHeatPumpNum).HeatRecActive) && (!MSHeatPump(MSHeatPumpNum).MyPlantScantFlag)) {

                rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).HRPlantLoc.loopNum).FluidName,
                                       DataGlobalConstants::HWInitConvTemp,
                                       state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).HRPlantLoc.loopNum).FluidIndex,
                                       RoutineName);

                MSHeatPump(MSHeatPumpNum).DesignHeatRecMassFlowRate = MSHeatPump(MSHeatPumpNum).DesignHeatRecFlowRate * rho;

                InitComponentNodes(state,
                                   0.0,
                                   MSHeatPump(MSHeatPumpNum).DesignHeatRecMassFlowRate,
                                   MSHeatPump(MSHeatPumpNum).HeatRecInletNodeNum,
                                   MSHeatPump(MSHeatPumpNum).HeatRecOutletNodeNum);
            }
            if (MSHeatPump(MSHeatPumpNum).CoilControlNode > 0) {
                if (MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow == AutoSize) {
                    if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingWater) {
                        SimulateWaterCoilComponents(
                            state, MSHeatPump(MSHeatPumpNum).HeatCoilName, FirstHVACIteration, MSHeatPump(MSHeatPumpNum).HeatCoilNum);

                        CoilMaxVolFlowRate =
                            GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).HeatCoilName, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).plantLoc.loopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).plantLoc.loopNum).FluidIndex,
                                                   RoutineName);
                            MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                        InitComponentNodes(state,
                                           0.0,
                                           MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow,
                                           MSHeatPump(MSHeatPumpNum).CoilControlNode,
                                           MSHeatPump(MSHeatPumpNum).CoilOutletNode);
                    }
                    if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingSteam) {

                        SimulateSteamCoilComponents(state,
                                                    MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                    FirstHVACIteration,
                                                    MSHeatPump(MSHeatPumpNum).HeatCoilNum,
                                                    1.0,
                                                    QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                        CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHeatPumpNum).HeatCoilNum, ErrorsFound);

                        if (CoilMaxVolFlowRate != AutoSize) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity =
                                GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                            MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                        InitComponentNodes(state,
                                           0.0,
                                           MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow,
                                           MSHeatPump(MSHeatPumpNum).CoilControlNode,
                                           MSHeatPump(MSHeatPumpNum).CoilOutletNode);
                    }
                }
            }
            if (MSHeatPump(MSHeatPumpNum).SuppCoilControlNode > 0) {
                if (MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow == AutoSize) {
                    if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingWater) {
                        SimulateWaterCoilComponents(
                            state, MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, FirstHVACIteration, MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum);

                        CoilMaxVolFlowRate =
                            GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).SuppPlantLoc.loopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(MSHeatPump(MSHeatPumpNum).SuppPlantLoc.loopNum).FluidIndex,
                                                   RoutineName);
                            MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                        InitComponentNodes(state,
                                           0.0,
                                           MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow,
                                           MSHeatPump(MSHeatPumpNum).SuppCoilControlNode,
                                           MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode);
                    }
                    if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingSteam) {

                        SimulateSteamCoilComponents(state,
                                                    MSHeatPump(MSHeatPumpNum).SuppHeatCoilName,
                                                    FirstHVACIteration,
                                                    MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum,
                                                    1.0,
                                                    QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                        CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(state, MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum, ErrorsFound);

                        if (CoilMaxVolFlowRate != AutoSize) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity =
                                GetSatDensityRefrig(state, fluidNameSteam, state.dataHVACMultiSpdHP->TempSteamIn, 1.0, SteamIndex, RoutineName);
                            MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                        InitComponentNodes(state,
                                           0.0,
                                           MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow,
                                           MSHeatPump(MSHeatPumpNum).SuppCoilControlNode,
                                           MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode);
                    }
                }
            }
            MSHeatPump(MSHeatPumpNum).MyEnvrnFlag = false;
        } // end one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MSHeatPump(MSHeatPumpNum).MyEnvrnFlag = true;
        }

        // IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
        if (!state.dataGlobal->DoingSizing && MSHeatPump(MSHeatPumpNum).CheckFanFlow) {
            state.dataHVACMultiSpdHP->CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed";
            GetFanVolFlow(state, MSHeatPump(MSHeatPumpNum).FanNum, MSHeatPump(MSHeatPumpNum).FanVolFlow);
            if (MSHeatPump(MSHeatPumpNum).FanVolFlow != AutoSize) {
                //     Check fan versus system supply air flow rates
                if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(NumOfSpeedCooling)) {
                    ShowWarningError(state,
                                     format("{} - air flow rate = {:.7T} in fan object {} is less than the MSHP system air flow rate when cooling is "
                                            "required ({:.7T}).",
                                            state.dataHVACMultiSpdHP->CurrentModuleObject,
                                            MSHeatPump(MSHeatPumpNum).FanVolFlow,
                                            MSHeatPump(MSHeatPumpNum).FanName,
                                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(NumOfSpeedCooling)));
                    ShowContinueError(
                        state, " The MSHP system flow rate when cooling is required is reset to the fan flow rate and the simulation continues.");
                    ShowContinueError(state, " Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHeatPumpNum).Name);
                    MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(NumOfSpeedCooling) = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                    // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                    for (i = NumOfSpeedCooling - 1; i >= 1; --i) {
                        if (MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i + 1)) {
                            ShowContinueError(state,
                                              format(" The MSHP system flow rate when cooling is required is reset to the flow rate at higher speed "
                                                     "and the simulation continues at Speed{}.",
                                                     i));
                            ShowContinueError(state,
                                              " Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHeatPumpNum).Name);
                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i + 1);
                        }
                    }
                }
                if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(NumOfSpeedHeating)) {
                    ShowWarningError(state,
                                     format("{} - air flow rate = {:.7T} in fan object {} is less than the MSHP system air flow rate when heating is "
                                            "required ({:.7T}).",
                                            state.dataHVACMultiSpdHP->CurrentModuleObject,
                                            MSHeatPump(MSHeatPumpNum).FanVolFlow,
                                            MSHeatPump(MSHeatPumpNum).FanName,
                                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(NumOfSpeedHeating)));
                    ShowContinueError(
                        state, " The MSHP system flow rate when heating is required is reset to the fan flow rate and the simulation continues.");
                    ShowContinueError(state, " Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHeatPumpNum).Name);
                    MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(NumOfSpeedHeating) = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                    for (i = NumOfSpeedHeating - 1; i >= 1; --i) {
                        if (MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i + 1)) {
                            ShowContinueError(state,
                                              format(" The MSHP system flow rate when heating is required is reset to the flow rate at higher speed "
                                                     "and the simulation continues at Speed{}.",
                                                     i));
                            ShowContinueError(
                                state, " Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " system = " + MSHeatPump(MSHeatPumpNum).Name);
                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i + 1);
                        }
                    }
                }
                if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate &&
                    MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate != 0.0) {
                    ShowWarningError(state,
                                     format("{} - air flow rate = {:.7T} in fan object {} is less than the MSHP system air flow rate when no heating "
                                            "or cooling is needed ({:.7T}).",
                                            state.dataHVACMultiSpdHP->CurrentModuleObject,
                                            MSHeatPump(MSHeatPumpNum).FanVolFlow,
                                            MSHeatPump(MSHeatPumpNum).FanName,
                                            MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate));
                    ShowContinueError(state,
                                      " The MSHP system flow rate when no heating or cooling is needed is reset to the fan flow rate and the "
                                      "simulation continues.");
                    ShowContinueError(state, " Occurs in " + state.dataHVACMultiSpdHP->CurrentModuleObject + " = " + MSHeatPump(MSHeatPumpNum).Name);
                    MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                }
                RhoAir = state.dataEnvrn->StdRhoAir;
                // set the mass flow rates from the reset volume flow rates
                for (i = 1; i <= NumOfSpeedCooling; ++i) {
                    MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(i) = RhoAir * MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i);
                    if (MSHeatPump(MSHeatPumpNum).FanVolFlow > 0.0) {
                        MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(i) =
                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) / MSHeatPump(MSHeatPumpNum).FanVolFlow;
                    }
                }
                for (i = 1; i <= NumOfSpeedHeating; ++i) {
                    MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(i) = RhoAir * MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i);
                    if (MSHeatPump(MSHeatPumpNum).FanVolFlow > 0.0) {
                        MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(i) =
                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) / MSHeatPump(MSHeatPumpNum).FanVolFlow;
                    }
                }
                MSHeatPump(MSHeatPumpNum).IdleMassFlowRate = RhoAir * MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate;
                if (MSHeatPump(MSHeatPumpNum).FanVolFlow > 0.0) {
                    MSHeatPump(MSHeatPumpNum).IdleSpeedRatio = MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate / MSHeatPump(MSHeatPumpNum).FanVolFlow;
                }
                // set the node max and min mass flow rates based on reset volume flow rates
                state.dataLoopNodes->Node(InNode).MassFlowRateMax =
                    max(MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(NumOfSpeedCooling), MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(NumOfSpeedHeating));
                state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail =
                    max(MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(NumOfSpeedCooling), MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(NumOfSpeedHeating));
                state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(OutNode) = state.dataLoopNodes->Node(InNode);
                MSHeatPump(MSHeatPumpNum).CheckFanFlow = false;
            }
        }

        if (MSHeatPump(MSHeatPumpNum).FanSchedPtr > 0) {
            if (GetCurrentScheduleValue(state, MSHeatPump(MSHeatPumpNum).FanSchedPtr) == 0.0) {
                MSHeatPump(MSHeatPumpNum).OpMode = CycFanCycCoil;
            } else {
                MSHeatPump(MSHeatPumpNum).OpMode = ContFanCycCoil;
            }
        }

        // Calcuate air distribution losses
        if (!FirstHVACIteration && state.dataHVACMultiSpdHP->AirLoopPass == 1) {
            ZoneInNode = MSHeatPump(MSHeatPumpNum).ZoneInletNode;
            DeltaMassRate = state.dataLoopNodes->Node(OutNode).MassFlowRate -
                            state.dataLoopNodes->Node(ZoneInNode).MassFlowRate / MSHeatPump(MSHeatPumpNum).FlowFraction;
            if (DeltaMassRate < 0.0) DeltaMassRate = 0.0;
            Real64 MassFlowRate(0.0);        // parent mass flow rate
            Real64 LatentOutput(0.0);        // latent output rate
            Real64 TotalOutput(0.0);         // total output rate
            Real64 SensibleOutputDelta(0.0); // delta sensible output rate
            Real64 LatentOutputDelta(0.0);   // delta latent output rate
            Real64 TotalOutputDelta(0.0);    // delta total output rate
            MassFlowRate = state.dataLoopNodes->Node(ZoneInNode).MassFlowRate / MSHeatPump(MSHeatPumpNum).FlowFraction;
            Real64 MinHumRat = state.dataLoopNodes->Node(ZoneInNode).HumRat;
            if (state.dataLoopNodes->Node(OutNode).Temp < state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).Temp)
                MinHumRat = state.dataLoopNodes->Node(OutNode).HumRat;
            CalcZoneSensibleLatentOutput(MassFlowRate,
                                         state.dataLoopNodes->Node(OutNode).Temp,
                                         MinHumRat,
                                         state.dataLoopNodes->Node(ZoneInNode).Temp,
                                         MinHumRat,
                                         MSHeatPump(MSHeatPumpNum).LoadLoss,
                                         LatentOutput,
                                         TotalOutput);
            CalcZoneSensibleLatentOutput(DeltaMassRate,
                                         state.dataLoopNodes->Node(OutNode).Temp,
                                         MinHumRat,
                                         state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).Temp,
                                         MinHumRat,
                                         SensibleOutputDelta,
                                         LatentOutputDelta,
                                         TotalOutputDelta);
            MSHeatPump(MSHeatPumpNum).LoadLoss = MSHeatPump(MSHeatPumpNum).LoadLoss + SensibleOutputDelta;
            if (std::abs(MSHeatPump(MSHeatPumpNum).LoadLoss) < 1.0e-6) MSHeatPump(MSHeatPumpNum).LoadLoss = 0.0;
        }

        // Returns load only for zones requesting cooling (heating). If in deadband, Qzoneload = 0.
        ZoneNum = MSHeatPump(MSHeatPumpNum).ControlZoneNum;
        if ((MSHeatPump(MSHeatPumpNum).ZoneSequenceCoolingNum > 0) && (MSHeatPump(MSHeatPumpNum).ZoneSequenceHeatingNum > 0)) {
            ZoneLoadToCoolSPSequenced = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(MSHeatPump(MSHeatPumpNum).ControlZoneNum)
                                            .SequencedOutputRequiredToCoolingSP(MSHeatPump(MSHeatPumpNum).ZoneSequenceCoolingNum);
            ZoneLoadToHeatSPSequenced = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(MSHeatPump(MSHeatPumpNum).ControlZoneNum)
                                            .SequencedOutputRequiredToHeatingSP(MSHeatPump(MSHeatPumpNum).ZoneSequenceHeatingNum);
            if (ZoneLoadToHeatSPSequenced > SmallLoad && ZoneLoadToCoolSPSequenced > SmallLoad) {
                QZnReq = ZoneLoadToHeatSPSequenced;
            } else if (ZoneLoadToHeatSPSequenced < (-1.0 * SmallLoad) && ZoneLoadToCoolSPSequenced < (-1.0 * SmallLoad)) {
                QZnReq = ZoneLoadToCoolSPSequenced;
            } else if (ZoneLoadToHeatSPSequenced <= (-1.0 * SmallLoad) && ZoneLoadToCoolSPSequenced >= SmallLoad) {
                QZnReq = 0.0;
            } else {
                QZnReq = 0.0; // Autodesk:Init Case added to prevent use of uninitialized value (occurred in MultiSpeedACFurnace example)
            }
            QZnReq /= MSHeatPump(MSHeatPumpNum).FlowFraction;
        } else {
            QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired / MSHeatPump(MSHeatPumpNum).FlowFraction;
        }
        if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) QZnReq = 0.0;

        if (QZnReq > SmallLoad) {
            MSHeatPump(MSHeatPumpNum).HeatCoolMode = ModeOfOperation::HeatingMode;
        } else if (QZnReq < (-1.0 * SmallLoad)) {
            MSHeatPump(MSHeatPumpNum).HeatCoolMode = ModeOfOperation::CoolingMode;
        } else {
            MSHeatPump(MSHeatPumpNum).HeatCoolMode = ModeOfOperation::Invalid;
        }

        // Determine the staged status
        if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
            if (state.dataZoneCtrls->StageZoneLogic(ZoneNum)) {
                MSHeatPump(MSHeatPumpNum).Staged = true;
                MSHeatPump(MSHeatPumpNum).StageNum = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).StageNum;
            } else {
                if (MSHeatPump(MSHeatPumpNum).MyStagedFlag) {
                    ShowWarningError(state,
                                     "ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to this "
                                     "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed object = ");
                    ShowContinueError(state, MSHeatPump(MSHeatPumpNum).Name + ". Please make correction. Simulation continues...");
                    MSHeatPump(MSHeatPumpNum).MyStagedFlag = false;
                }
            }
        }
        // Set the inlet node mass flow rate
        if (MSHeatPump(MSHeatPumpNum).OpMode == ContFanCycCoil) {
            // constant fan mode
            if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                state.dataHVACMultiSpdHP->CompOnMassFlow = MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(1);
                state.dataHVACMultiSpdHP->CompOnFlowRatio = MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(1);
                MSHeatPump(MSHeatPumpNum).LastMode = ModeOfOperation::HeatingMode;
            } else if (QZnReq < (-1.0 * SmallLoad) && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                state.dataHVACMultiSpdHP->CompOnMassFlow = MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(1);
                state.dataHVACMultiSpdHP->CompOnFlowRatio = MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(1);
                MSHeatPump(MSHeatPumpNum).LastMode = ModeOfOperation::CoolingMode;
            } else {
                state.dataHVACMultiSpdHP->CompOnMassFlow = MSHeatPump(MSHeatPumpNum).IdleMassFlowRate;
                state.dataHVACMultiSpdHP->CompOnFlowRatio = MSHeatPump(MSHeatPumpNum).IdleSpeedRatio;
            }
            state.dataHVACMultiSpdHP->CompOffMassFlow = MSHeatPump(MSHeatPumpNum).IdleMassFlowRate;
            state.dataHVACMultiSpdHP->CompOffFlowRatio = MSHeatPump(MSHeatPumpNum).IdleSpeedRatio;
        } else {
            // cycling fan mode
            if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                state.dataHVACMultiSpdHP->CompOnMassFlow = MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(1);
                state.dataHVACMultiSpdHP->CompOnFlowRatio = MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(1);
            } else if (QZnReq < (-1.0 * SmallLoad) && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                state.dataHVACMultiSpdHP->CompOnMassFlow = MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(1);
                state.dataHVACMultiSpdHP->CompOnFlowRatio = MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(1);
            } else {
                state.dataHVACMultiSpdHP->CompOnMassFlow = 0.0;
                state.dataHVACMultiSpdHP->CompOnFlowRatio = 0.0;
            }
            state.dataHVACMultiSpdHP->CompOffMassFlow = 0.0;
            state.dataHVACMultiSpdHP->CompOffFlowRatio = 0.0;
        }

        // Set the inlet node mass flow rate
        if (GetCurrentScheduleValue(state, MSHeatPump(MSHeatPumpNum).AvaiSchedPtr) > 0.0 && state.dataHVACMultiSpdHP->CompOnMassFlow != 0.0) {
            OnOffAirFlowRatio = 1.0;
            if (FirstHVACIteration) {
                state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirInletNodeNum).MassFlowRate = state.dataHVACMultiSpdHP->CompOnMassFlow;
                PartLoadFrac = 0.0;
            } else {
                if (MSHeatPump(MSHeatPumpNum).HeatCoolMode != ModeOfOperation::Invalid) {
                    PartLoadFrac = 1.0;
                } else {
                    PartLoadFrac = 0.0;
                }
            }
        } else {
            PartLoadFrac = 0.0;
            state.dataLoopNodes->Node(InNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMaxAvail = 0.0;
            OnOffAirFlowRatio = 1.0;
        }

        // Check availability of DX coils
        if (GetCurrentScheduleValue(state, MSHeatPump(MSHeatPumpNum).AvaiSchedPtr) > 0.0) {
            if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
                CoilAvailSchPtr = GetDXCoilAvailSchPtr( // TODO: Why isn't this stored on the struct?
                    state,
                    "Coil:Cooling:DX:MultiSpeed",
                    MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                    ErrorsFound,
                    MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex);
                if (ErrorsFound) {
                    ShowFatalError(state, "InitMSHeatPump, The previous error causes termination.");
                }
                if (GetCurrentScheduleValue(state, CoilAvailSchPtr) == 0.0) {
                    if (MSHeatPump(MSHeatPumpNum).CoolCountAvail == 0) {
                        ++MSHeatPump(MSHeatPumpNum).CoolCountAvail;
                        ShowWarningError(state,
                                         MSHeatPump(MSHeatPumpNum).Name +
                                             " is ready to perform cooling, but its DX cooling coil = " + MSHeatPump(MSHeatPumpNum).DXCoolCoilName +
                                             " is not available at Available Schedule = " + GetScheduleName(state, CoilAvailSchPtr) + '.');
                        ShowContinueErrorTimeStamp(state,
                                                   format("Availability schedule returned={:.1R}", GetCurrentScheduleValue(state, CoilAvailSchPtr)));
                    } else {
                        ++MSHeatPump(MSHeatPumpNum).CoolCountAvail;
                        ShowRecurringWarningErrorAtEnd(state,
                                                       MSHeatPump(MSHeatPumpNum).Name + ": Cooling coil is still not available ...",
                                                       MSHeatPump(MSHeatPumpNum).CoolIndexAvail,
                                                       GetCurrentScheduleValue(state, CoilAvailSchPtr),
                                                       GetCurrentScheduleValue(state, CoilAvailSchPtr));
                    }
                }
            }
            if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode &&
                MSHeatPump(MSHeatPumpNum).HeatCoilType == MultiSpeedHeatingCoil) {
                CoilAvailSchPtr = GetDXCoilAvailSchPtr(state,
                                                       "Coil:Heating:DX:MultiSpeed",
                                                       MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                                       ErrorsFound,
                                                       MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex);
                if (ErrorsFound) {
                    ShowFatalError(state, "InitMSHeatPump, The previous error causes termination.");
                }
                if (GetCurrentScheduleValue(state, CoilAvailSchPtr) == 0.0) {
                    if (MSHeatPump(MSHeatPumpNum).HeatCountAvail == 0) {
                        ++MSHeatPump(MSHeatPumpNum).HeatCountAvail;
                        ShowWarningError(state,
                                         MSHeatPump(MSHeatPumpNum).Name +
                                             " is ready to perform heating, but its DX heating coil = " + MSHeatPump(MSHeatPumpNum).DXCoolCoilName +
                                             " is not available at Available Schedule = " + GetScheduleName(state, CoilAvailSchPtr) + '.');
                        ShowContinueErrorTimeStamp(state,
                                                   format("Availability schedule returned={:.1R}", GetCurrentScheduleValue(state, CoilAvailSchPtr)));
                    } else {
                        ++MSHeatPump(MSHeatPumpNum).HeatCountAvail;
                        ShowRecurringWarningErrorAtEnd(state,
                                                       MSHeatPump(MSHeatPumpNum).Name + ": Heating coil is still not available ...",
                                                       MSHeatPump(MSHeatPumpNum).HeatIndexAvail,
                                                       GetCurrentScheduleValue(state, CoilAvailSchPtr),
                                                       GetCurrentScheduleValue(state, CoilAvailSchPtr));
                    }
                }
            }
        }

        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).CycRatio = 0.0;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedRatio = 0.0;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedNum = 0;

        CalcMSHeatPump(state,
                       MSHeatPumpNum,
                       FirstHVACIteration,
                       DataHVACGlobals::CompressorOperation::On,
                       1,
                       0.0,
                       PartLoadFrac,
                       QSensUnitOut,
                       QZnReq,
                       OnOffAirFlowRatio,
                       state.dataHVACMultiSpdHP->SupHeaterLoad);

        auto &e = MSHeatPump(MSHeatPumpNum);
        {
            e.TotHeatEnergyRate = 0.0;
            e.SensHeatEnergyRate = 0.0;
            e.LatHeatEnergyRate = 0.0;
            e.TotCoolEnergyRate = 0.0;
            e.SensCoolEnergyRate = 0.0;
            e.LatCoolEnergyRate = 0.0;
        }
        // If unit is scheduled OFF, setpoint is equal to inlet node temperature.
        //!!LKL Discrepancy with < 0
        if (GetCurrentScheduleValue(state, MSHeatPump(MSHeatPumpNum).AvaiSchedPtr) == 0.0) {
            state.dataLoopNodes->Node(OutNode).Temp = state.dataLoopNodes->Node(InNode).Temp;
            return;
        }

        if ((MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::Invalid && MSHeatPump(MSHeatPumpNum).OpMode == CycFanCycCoil) ||
            state.dataHVACMultiSpdHP->CompOnMassFlow == 0.0) {
            QZnReq = 0.0;
            PartLoadFrac = 0.0;
            state.dataLoopNodes->Node(InNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMaxAvail = 0.0;
        }
        MSHeatPump(MSHeatPumpNum).LoadMet = 0.0;
        SetAverageAirFlow(state, MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio);

        // Init maximum available Heat Recovery flow rate
        if ((MSHeatPump(MSHeatPumpNum).HeatRecActive) && (!MSHeatPump(MSHeatPumpNum).MyPlantScantFlag)) {
            if (PartLoadFrac > 0.0) {
                if (FirstHVACIteration) {
                    MdotHR = MSHeatPump(MSHeatPumpNum).DesignHeatRecMassFlowRate;
                } else {
                    if (MSHeatPump(MSHeatPumpNum).HeatRecoveryMassFlowRate > 0.0) {
                        MdotHR = MSHeatPump(MSHeatPumpNum).HeatRecoveryMassFlowRate;
                    } else {
                        MdotHR = MSHeatPump(MSHeatPumpNum).DesignHeatRecMassFlowRate;
                    }
                }
            } else {
                MdotHR = 0.0;
            }

            SetComponentFlowRate(state,
                                 MdotHR,
                                 MSHeatPump(MSHeatPumpNum).HeatRecInletNodeNum,
                                 MSHeatPump(MSHeatPumpNum).HeatRecOutletNodeNum,
                                 MSHeatPump(MSHeatPumpNum).HRPlantLoc);
        }

        // get operating capacity of water and steam coil
        if (FirstHVACIteration) {
            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingWater) {
                //     set air-side and steam-side mass flow rates
                state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).CoilAirInletNode).MassFlowRate = state.dataHVACMultiSpdHP->CompOnMassFlow;
                mdot = MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow;
                SetComponentFlowRate(state,
                                     mdot,
                                     MSHeatPump(MSHeatPumpNum).CoilControlNode,
                                     MSHeatPump(MSHeatPumpNum).CoilOutletNode,
                                     MSHeatPump(MSHeatPumpNum).plantLoc);
                //     simulate water coil to find operating capacity
                SimulateWaterCoilComponents(
                    state, MSHeatPump(MSHeatPumpNum).HeatCoilName, FirstHVACIteration, MSHeatPump(MSHeatPumpNum).HeatCoilNum, QActual);
            } // from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN

            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingSteam) {

                //     set air-side and steam-side mass flow rates
                state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).CoilAirInletNode).MassFlowRate = state.dataHVACMultiSpdHP->CompOnMassFlow;
                mdot = MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow;
                SetComponentFlowRate(state,
                                     mdot,
                                     MSHeatPump(MSHeatPumpNum).CoilControlNode,
                                     MSHeatPump(MSHeatPumpNum).CoilOutletNode,
                                     MSHeatPump(MSHeatPumpNum).plantLoc);

                //     simulate steam coil to find operating capacity
                SimulateSteamCoilComponents(state,
                                            MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                            FirstHVACIteration,
                                            MSHeatPump(MSHeatPumpNum).HeatCoilNum,
                                            1.0,
                                            QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil

            } // from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN
            if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingWater) {
                //     set air-side and steam-side mass flow rates
                state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).SuppCoilAirInletNode).MassFlowRate = state.dataHVACMultiSpdHP->CompOnMassFlow;
                mdot = MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow;
                SetComponentFlowRate(state,
                                     mdot,
                                     MSHeatPump(MSHeatPumpNum).SuppCoilControlNode,
                                     MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode,
                                     MSHeatPump(MSHeatPumpNum).SuppPlantLoc);
                //     simulate water coil to find operating capacity
                SimulateWaterCoilComponents(
                    state, MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, FirstHVACIteration, MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum, QActual);
                MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity = QActual;

            } // from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN

            if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == Coil_HeatingSteam) {

                //     set air-side and steam-side mass flow rates
                state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).SuppCoilAirInletNode).MassFlowRate = state.dataHVACMultiSpdHP->CompOnMassFlow;
                mdot = MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow;
                SetComponentFlowRate(state,
                                     mdot,
                                     MSHeatPump(MSHeatPumpNum).SuppCoilControlNode,
                                     MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode,
                                     MSHeatPump(MSHeatPumpNum).SuppPlantLoc);

                //     simulate steam coil to find operating capacity
                SimulateSteamCoilComponents(state,
                                            MSHeatPump(MSHeatPumpNum).SuppHeatCoilName,
                                            FirstHVACIteration,
                                            MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum,
                                            1.0,
                                            QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity =
                    GetSteamCoilCapacity(state, "Coil:Heating:Steam", MSHeatPump(MSHeatPumpNum).SuppHeatCoilName, ErrorsFound);

            } // from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN
        }     // from IF( FirstHVACIteration ) THEN
    }

    //******************************************************************************

    void SizeMSHeatPump(EnergyPlusData &state, int const MSHeatPumpNum) // Engine driven heat pump number
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    June 2007
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing multispeed heat pump airflow rates and flow fraction.

        // Using/Aliasing
        using namespace DataSizing;

        using PlantUtilities::RegisterPlantCompDesignFlow;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumOfSpeedCooling; // Number of speeds for cooling
        int NumOfSpeedHeating; // Number of speeds for heating
        int i;                 // Index to speed

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);
        if (state.dataSize->CurSysNum > 0 && state.dataSize->CurOASysNum == 0) {
            if (MSHeatPump(MSHeatPumpNum).FanType == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanVecIndex = MSHeatPump(MSHeatPumpNum).FanNum;
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanModelType = DataAirSystems::ObjectVectorOOFanSystemModel;
            } else {
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).SupFanNum = MSHeatPump(MSHeatPumpNum).FanNum;
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanModelType = DataAirSystems::StructArrayLegacyFanModels;
            }
            if (MSHeatPump(MSHeatPumpNum).FanPlaceType == BlowThru) {
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation = DataAirSystems::FanPlacement::BlowThru;
            } else if (MSHeatPump(MSHeatPumpNum).FanPlaceType == DrawThru) {
                state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation = DataAirSystems::FanPlacement::DrawThru;
            }
        }

        NumOfSpeedCooling = MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling;
        NumOfSpeedHeating = MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating;

        for (i = NumOfSpeedCooling; i >= 1; --i) {

            if (MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) == AutoSize) {
                if (state.dataSize->CurSysNum > 0) {
                    if (i == NumOfSpeedCooling) {
                        CheckSysSizing(state, state.dataHVACMultiSpdHP->CurrentModuleObject, MSHeatPump(MSHeatPumpNum).Name);
                        MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) &&
                            MSHeatPump(MSHeatPumpNum).FanVolFlow != AutoSize) {
                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                            ShowWarningError(state, state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHeatPumpNum).Name + "\"");
                            ShowContinueError(state,
                                              "The supply air flow rate at high speed is less than the autosized value for the supply air flow rate "
                                              "in cooling mode. Consider autosizing the fan for this simulation.");
                            ShowContinueError(
                                state,
                                "The air flow rate at high speed in cooling mode is reset to the supply air flow rate and the simulation continues.");
                        }
                    } else {
                        MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) =
                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(NumOfSpeedCooling) * i / NumOfSpeedCooling;
                    }
                    if (MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) < SmallAirVolFlow) {
                        MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate = 0.0;
                    }
                    // Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
                    if (i != NumOfSpeedCooling) {
                        if (MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i + 1)) {
                            MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i + 1);
                        }
                    }
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataHVACMultiSpdHP->CurrentModuleObject,
                                                 MSHeatPump(MSHeatPumpNum).Name,
                                                 format("Speed {} Supply Air Flow Rate During Cooling Operation [m3/s]", i),
                                                 MSHeatPump(MSHeatPumpNum).CoolVolumeFlowRate(i));
                }
            }
        }

        for (i = NumOfSpeedHeating; i >= 1; --i) {
            if (MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) == AutoSize) {
                if (state.dataSize->CurSysNum > 0) {
                    if (i == NumOfSpeedHeating) {
                        CheckSysSizing(state, state.dataHVACMultiSpdHP->CurrentModuleObject, MSHeatPump(MSHeatPumpNum).Name);
                        MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) &&
                            MSHeatPump(MSHeatPumpNum).FanVolFlow != AutoSize) {
                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                            ShowWarningError(state, state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHeatPumpNum).Name + "\"");
                            ShowContinueError(state,
                                              "The supply air flow rate at high speed is less than the autosized value for the maximum air flow rate "
                                              "in heating mode. Consider autosizing the fan for this simulation.");
                            ShowContinueError(state,
                                              "The maximum air flow rate at high speed in heating mode is reset to the supply air flow rate and the "
                                              "simulation continues.");
                        }
                    } else {
                        MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) =
                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(NumOfSpeedHeating) * i / NumOfSpeedHeating;
                    }
                    if (MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) < SmallAirVolFlow) {
                        MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) = 0.0;
                    }
                    // Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
                    if (i != NumOfSpeedHeating) {
                        if (MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i + 1)) {
                            MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i + 1);
                        }
                    }
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataHVACMultiSpdHP->CurrentModuleObject,
                                                 MSHeatPump(MSHeatPumpNum).Name,
                                                 format("Speed{}Supply Air Flow Rate During Heating Operation [m3/s]", i),
                                                 MSHeatPump(MSHeatPumpNum).HeatVolumeFlowRate(i));
                }
            }
        }

        if (MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate == AutoSize) {
            if (state.dataSize->CurSysNum > 0) {
                CheckSysSizing(state, state.dataHVACMultiSpdHP->CurrentModuleObject, MSHeatPump(MSHeatPumpNum).Name);
                MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                if (MSHeatPump(MSHeatPumpNum).FanVolFlow < MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate &&
                    MSHeatPump(MSHeatPumpNum).FanVolFlow != AutoSize) {
                    MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate = MSHeatPump(MSHeatPumpNum).FanVolFlow;
                    ShowWarningError(state, state.dataHVACMultiSpdHP->CurrentModuleObject + " \"" + MSHeatPump(MSHeatPumpNum).Name + "\"");
                    ShowContinueError(state,
                                      "The supply air flow rate is less than the autosized value for the maximum air flow rate when no heating or "
                                      "cooling is needed. Consider autosizing the fan for this simulation.");
                    ShowContinueError(state,
                                      "The maximum air flow rate when no heating or cooling is needed is reset to the supply air flow rate and the "
                                      "simulation continues.");
                }
                if (MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate < SmallAirVolFlow) {
                    MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate = 0.0;
                }

                BaseSizer::reportSizerOutput(state,
                                             state.dataHVACMultiSpdHP->CurrentModuleObject,
                                             MSHeatPump(MSHeatPumpNum).Name,
                                             "Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                             MSHeatPump(MSHeatPumpNum).IdleVolumeAirRate);
            }
        }

        if (MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp == AutoSize) {
            if (state.dataSize->CurSysNum > 0) {
                if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == 1) { // Gas
                    CheckZoneSizing(state, "Coil:Heating:Fuel", MSHeatPump(MSHeatPumpNum).Name);
                } else {
                    CheckZoneSizing(state, "Coil:Heating:Electric", MSHeatPump(MSHeatPumpNum).Name);
                }
                MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatSupTemp;
                BaseSizer::reportSizerOutput(state,
                                             state.dataHVACMultiSpdHP->CurrentModuleObject,
                                             MSHeatPump(MSHeatPumpNum).Name,
                                             "Maximum Supply Air Temperature from Supplemental Heater [C]",
                                             MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp);
            }
        }

        if (MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity == AutoSize) {
            if (state.dataSize->CurSysNum > 0) {
                if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilType == 1) { // Gas
                    CheckSysSizing(state, "Coil:Heating:Fuel", MSHeatPump(MSHeatPumpNum).Name);
                } else {
                    CheckSysSizing(state, "Coil:Heating:Electric", MSHeatPump(MSHeatPumpNum).Name);
                }
                MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatCap;
            } else {
                MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity = 0.0;
            }
            BaseSizer::reportSizerOutput(state,
                                         state.dataHVACMultiSpdHP->CurrentModuleObject,
                                         MSHeatPump(MSHeatPumpNum).Name,
                                         "Supplemental Heating Coil Nominal Capacity [W]",
                                         MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity);
        }
        state.dataSize->SuppHeatCap = MSHeatPump(MSHeatPumpNum).DesignSuppHeatingCapacity;

        if (MSHeatPump(MSHeatPumpNum).HeatRecActive) {
            RegisterPlantCompDesignFlow(state, MSHeatPump(MSHeatPumpNum).HeatRecInletNodeNum, MSHeatPump(MSHeatPumpNum).DesignHeatRecFlowRate);
        }
    }

    //******************************************************************************

    void ControlMSHPOutputEMS(EnergyPlusData &state,
                              int const MSHeatPumpNum,                                 // Unit index of engine driven heat pump
                              bool const FirstHVACIteration,                           // flag for 1st HVAC iteration in the time step
                              DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation; 1=on, 0=off
                              int const OpMode,                                        // operating mode: CycFanCycCoil | ContFanCycCoil
                              Real64 const QZnReq,                                     // cooling or heating output needed by zone [W]
                              Real64 const SpeedVal,                                   // continuous speed value
                              int &SpeedNum,                                           // discrete speed level
                              Real64 &SpeedRatio,                                      // unit speed ratio for DX coils
                              Real64 &PartLoadFrac,                                    // unit part load fraction
                              Real64 &OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
                              Real64 &SupHeaterLoad      // Supplemental heater load [W]

    )
    {
        OnOffAirFlowRatio = 0.0;
        SupHeaterLoad = 0.0;

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        // Get EMS output
        SpeedNum = ceil(SpeedVal);
        bool useMaxedSpeed = false;
        std::string useMaxedSpeedCoilName;
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode) {
            if (SpeedNum > MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating) {
                SpeedNum = MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating;
                useMaxedSpeed = true;
                useMaxedSpeedCoilName = MSHeatPump(MSHeatPumpNum).DXHeatCoilName;
            }
        } else if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
            if (SpeedNum > MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling) {
                SpeedNum = MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling;
                useMaxedSpeed = true;
                useMaxedSpeedCoilName = MSHeatPump(MSHeatPumpNum).DXCoolCoilName;
            }
        }
        if (useMaxedSpeed) {
            MSHeatPump(MSHeatPumpNum).CoilSpeedErrIndex++;
            ShowRecurringWarningErrorAtEnd(state,
                                           "Wrong coil speed EMS override value, for unit=\"" + useMaxedSpeedCoilName +
                                               "\". Exceeding maximum coil speed level. Speed level is set to the maximum coil speed level allowed.",
                                           MSHeatPump(MSHeatPumpNum).CoilSpeedErrIndex,
                                           SpeedVal,
                                           SpeedVal,
                                           _,
                                           "",
                                           "");
        }
        // Calculate TempOutput
        Real64 TempOutput = 0.0; // unit output when iteration limit exceeded [W]

        if (SpeedNum == 1) {
            SpeedRatio = 0.0;
            if (useMaxedSpeed || floor(SpeedVal) == SpeedVal) {
                PartLoadFrac = 1;
            } else {
                PartLoadFrac = SpeedVal - floor(SpeedVal);
            }
            CalcMSHeatPump(state,
                           MSHeatPumpNum,
                           FirstHVACIteration,
                           CompressorOp,
                           SpeedNum,
                           SpeedRatio,
                           PartLoadFrac,
                           TempOutput,
                           QZnReq,
                           OnOffAirFlowRatio,
                           SupHeaterLoad);
        } else {
            PartLoadFrac = 0.0;
            if (useMaxedSpeed || floor(SpeedVal) == SpeedVal) {
                SpeedRatio = 1;
            } else {
                SpeedRatio = SpeedVal - floor(SpeedVal);
            }
            CalcMSHeatPump(state,
                           MSHeatPumpNum,
                           FirstHVACIteration,
                           CompressorOp,
                           SpeedNum,
                           SpeedRatio,
                           PartLoadFrac,
                           TempOutput,
                           QZnReq,
                           OnOffAirFlowRatio,
                           SupHeaterLoad);
        }

        ControlMSHPSupHeater(state,
                             MSHeatPumpNum,
                             FirstHVACIteration,
                             CompressorOp,
                             OpMode,
                             QZnReq,
                             TempOutput,
                             SpeedNum,
                             SpeedRatio,
                             PartLoadFrac,
                             OnOffAirFlowRatio,
                             SupHeaterLoad);
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).CycRatio = PartLoadFrac;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedRatio = SpeedRatio;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedNum = SpeedNum;
    }

    void ControlMSHPSupHeater(EnergyPlusData &state,
                              int const MSHeatPumpNum,                                 // Unit index of engine driven heat pump
                              bool const FirstHVACIteration,                           // flag for 1st HVAC iteration in the time step
                              DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation; 1=on, 0=off
                              int const OpMode,                                        // operating mode: CycFanCycCoil | ContFanCycCoil
                              Real64 const QZnReq,                                     // cooling or heating output needed by zone [W]
                              int const EMSOutput,                                     // unit full output when compressor is operating [W]vvvv
                              int const SpeedNum,                                      // Speed number
                              Real64 SpeedRatio,                                       // unit speed ratio for DX coils
                              Real64 PartLoadFrac,                                     // unit part load fraction
                              Real64 OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
                              Real64 &SupHeaterLoad     // Supplemental heater load [W]

    )
    {
        // if the DX heating coil cannot meet the load, trim with supplemental heater
        // occurs with constant fan mode when compressor is on or off
        // occurs with cycling fan mode when compressor PLR is equal to 1
        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        if ((QZnReq > SmallLoad && QZnReq > EMSOutput)) {
            Real64 TempOutput;
            if (state.dataEnvrn->OutDryBulbTemp <= MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp) {
                SupHeaterLoad = QZnReq - EMSOutput;
            } else {
                SupHeaterLoad = 0.0;
            }
            CalcMSHeatPump(state,
                           MSHeatPumpNum,
                           FirstHVACIteration,
                           CompressorOp,
                           SpeedNum,
                           SpeedRatio,
                           PartLoadFrac,
                           TempOutput,
                           QZnReq,
                           OnOffAirFlowRatio,
                           SupHeaterLoad);
        }

        // check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
        if (state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp > MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp &&
            SupHeaterLoad > 0.0) {

            //   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
            SupHeaterLoad = 0.0;
            Real64 QCoilActual; // coil load actually delivered returned to calling component
            CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, OpMode, QCoilActual);

            //   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
            //   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
            //   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
            //   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
            if (state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp < MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp) {
                Real64 CpAir = Psychrometrics::PsyCpAirFnW(state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).HumRat);
                SupHeaterLoad =
                    state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirInletNodeNum).MassFlowRate * CpAir *
                    (MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp - state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp);

            } else {
                SupHeaterLoad = 0.0;
            }
        }
    }

    void ControlMSHPOutput(EnergyPlusData &state,
                           int const MSHeatPumpNum,                                 // Unit index of engine driven heat pump
                           bool const FirstHVACIteration,                           // flag for 1st HVAC iteration in the time step
                           DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation; 1=on, 0=off
                           int const OpMode,                                        // operating mode: CycFanCycCoil | ContFanCycCoil
                           Real64 const QZnReq,                                     // cooling or heating output needed by zone [W]
                           int const ZoneNum,                                       // Index to zone number
                           int &SpeedNum,                                           // Speed number
                           Real64 &SpeedRatio,                                      // unit speed ratio for DX coils
                           Real64 &PartLoadFrac,                                    // unit part load fraction
                           Real64 &OnOffAirFlowRatio,                               // ratio of compressor ON airflow to AVERAGE airflow over timestep
                           Real64 &SupHeaterLoad                                    // Supplemental heater load [W]
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised for multispeed heat pump use based on ControlPTHPOutput

        // PURPOSE OF THIS SUBROUTINE:
        // Determine the part load fraction at low speed, and speed ratio at high speed for this time step.

        // METHODOLOGY EMPLOYED:
        // Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

        using General::SolveRoot;
        using Psychrometrics::PsyCpAirFnW;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int constexpr MaxIte(500); // maximum number of iterations

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FullOutput;         // unit full output when compressor is operating [W]
        Real64 LowOutput;          // unit full output at low speed [W]
        Real64 TempOutput;         // unit output when iteration limit exceeded [W]
        Real64 NoCompOutput;       // output when no active compressor [W]
        Real64 ErrorToler;         // error tolerance
        int SolFla;                // Flag of RegulaFalsi solver
        Array1D<Real64> Par(9);    // Parameters passed to RegulaFalsi
        Real64 CpAir;              // air specific heat
        Real64 OutsideDryBulbTemp; // Outside air temperature at external node height
        Real64 QCoilActual;        // coil load actually delivered returned to calling component
        int i;                     // Speed index

        SupHeaterLoad = 0.0;
        PartLoadFrac = 0.0;
        SpeedRatio = 0.0;
        SpeedNum = 1;

        OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        //!!LKL Discrepancy with < 0
        if (GetCurrentScheduleValue(state, MSHeatPump(MSHeatPumpNum).AvaiSchedPtr) == 0.0) return;

        // Get result when DX coil is off
        CalcMSHeatPump(state,
                       MSHeatPumpNum,
                       FirstHVACIteration,
                       CompressorOp,
                       SpeedNum,
                       SpeedRatio,
                       PartLoadFrac,
                       NoCompOutput,
                       QZnReq,
                       OnOffAirFlowRatio,
                       SupHeaterLoad);

        // If cooling and NoCompOutput < QZnReq, the coil needs to be off
        // If heating and NoCompOutput > QZnReq, the coil needs to be off
        if ((QZnReq < (-1.0 * SmallLoad) && NoCompOutput < QZnReq) || (QZnReq > SmallLoad && NoCompOutput > QZnReq) ||
            std::abs(QZnReq) <= SmallLoad) {
            return;
        }

        // Get full load result
        PartLoadFrac = 1.0;
        SpeedRatio = 1.0;
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode) {
            SpeedNum = MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating;
            if (MSHeatPump(MSHeatPumpNum).Staged && std::abs(MSHeatPump(MSHeatPumpNum).StageNum) < SpeedNum) {
                SpeedNum = std::abs(MSHeatPump(MSHeatPumpNum).StageNum);
                if (SpeedNum == 1) SpeedRatio = 0.0;
            }
        }
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
            SpeedNum = MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling;
            if (MSHeatPump(MSHeatPumpNum).Staged && std::abs(MSHeatPump(MSHeatPumpNum).StageNum) < SpeedNum) {
                SpeedNum = std::abs(MSHeatPump(MSHeatPumpNum).StageNum);
                if (SpeedNum == 1) SpeedRatio = 0.0;
            }
        }

        CalcMSHeatPump(state,
                       MSHeatPumpNum,
                       FirstHVACIteration,
                       CompressorOp,
                       SpeedNum,
                       SpeedRatio,
                       PartLoadFrac,
                       FullOutput,
                       QZnReq,
                       OnOffAirFlowRatio,
                       SupHeaterLoad);

        if (QZnReq < (-1.0 * SmallLoad)) {
            // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
            if (FullOutput >= 0.0 || FullOutput >= NoCompOutput) {
                PartLoadFrac = 0.0;
                SpeedRatio = 0.0;
                SpeedNum = 0;
                return;
            }
            //  ! If the QZnReq <= FullOutput the unit needs to run full out
            if (QZnReq <= FullOutput) {
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                if (MSHeatPump(MSHeatPumpNum).Staged && SpeedNum == 1) SpeedRatio = 0.0;
                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).CycRatio = PartLoadFrac;
                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedRatio = SpeedRatio;
                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedNum = SpeedNum;
                return;
            }
            ErrorToler = 0.001; // Error tolerance for convergence from input deck
        } else {
            // Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off)
            if (FullOutput <= 0.0 || FullOutput <= NoCompOutput) {
                PartLoadFrac = 0.0;
                SpeedRatio = 0.0;
                // may need supplemental heating so don't return in heating mode
            }
            if (QZnReq >= FullOutput) {
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                // may need supplemental heating so don't return in heating mode
            }
            ErrorToler = 0.001; // Error tolerance for convergence from input deck
        }

        // Direct solution
        if (state.dataGlobal->DoCoilDirectSolutions && !MSHeatPump(MSHeatPumpNum).Staged) {
            Real64 TempOutput0 = 0.0;
            MSHeatPump(MSHeatPumpNum).FullOutput = 0.0;

            // heating
            if (QZnReq > SmallLoad && QZnReq < FullOutput) {
                CalcMSHeatPump(
                    state, MSHeatPumpNum, FirstHVACIteration, CompressorOp, 1, 0.0, 0.0, TempOutput0, QZnReq, OnOffAirFlowRatio, SupHeaterLoad);

                for (int i = 1; i <= MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating; ++i) {
                    if (i == 1) {
                        CalcMSHeatPump(state,
                                       MSHeatPumpNum,
                                       FirstHVACIteration,
                                       CompressorOp,
                                       i,
                                       0.0,
                                       1.0,
                                       MSHeatPump(MSHeatPumpNum).FullOutput(i),
                                       QZnReq,
                                       OnOffAirFlowRatio,
                                       SupHeaterLoad);
                        if (QZnReq <= MSHeatPump(MSHeatPumpNum).FullOutput(i)) {
                            SpeedNum = i;
                            PartLoadFrac = (QZnReq - TempOutput0) / (MSHeatPump(MSHeatPumpNum).FullOutput(i) - TempOutput0);
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           0.0,
                                           PartLoadFrac,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            break;
                        }
                    } else {
                        CalcMSHeatPump(state,
                                       MSHeatPumpNum,
                                       FirstHVACIteration,
                                       CompressorOp,
                                       i,
                                       1.0,
                                       1.0,
                                       MSHeatPump(MSHeatPumpNum).FullOutput(i),
                                       QZnReq,
                                       OnOffAirFlowRatio,
                                       SupHeaterLoad);
                        if (QZnReq <= MSHeatPump(MSHeatPumpNum).FullOutput(i)) {
                            SpeedNum = i;
                            PartLoadFrac = 1.0;
                            SpeedRatio = (QZnReq - MSHeatPump(MSHeatPumpNum).FullOutput(i - 1)) /
                                         (MSHeatPump(MSHeatPumpNum).FullOutput(i) - MSHeatPump(MSHeatPumpNum).FullOutput(i - 1));
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           SpeedRatio,
                                           1.0,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            break;
                        }
                    }
                }
            }

            // Coolling
            if (QZnReq < (-1.0 * SmallLoad) && QZnReq > FullOutput) {
                CalcMSHeatPump(
                    state, MSHeatPumpNum, FirstHVACIteration, CompressorOp, 1, 0.0, 0.0, TempOutput0, QZnReq, OnOffAirFlowRatio, SupHeaterLoad);
                for (int i = 1; i <= MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling; ++i) {
                    if (i == 1) {
                        CalcMSHeatPump(state,
                                       MSHeatPumpNum,
                                       FirstHVACIteration,
                                       CompressorOp,
                                       i,
                                       0.0,
                                       1.0,
                                       MSHeatPump(MSHeatPumpNum).FullOutput(i),
                                       QZnReq,
                                       OnOffAirFlowRatio,
                                       SupHeaterLoad);
                        if (QZnReq >= MSHeatPump(MSHeatPumpNum).FullOutput(i)) {
                            SpeedNum = i;
                            PartLoadFrac = (QZnReq - TempOutput0) / (MSHeatPump(MSHeatPumpNum).FullOutput(i) - TempOutput0);
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           0.0,
                                           PartLoadFrac,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            break;
                        }
                    } else {
                        CalcMSHeatPump(state,
                                       MSHeatPumpNum,
                                       FirstHVACIteration,
                                       CompressorOp,
                                       i,
                                       1.0,
                                       1.0,
                                       MSHeatPump(MSHeatPumpNum).FullOutput(i),
                                       QZnReq,
                                       OnOffAirFlowRatio,
                                       SupHeaterLoad);
                        if (QZnReq >= MSHeatPump(MSHeatPumpNum).FullOutput(i)) {
                            SpeedNum = i;
                            PartLoadFrac = 1.0;
                            SpeedRatio = (QZnReq - MSHeatPump(MSHeatPumpNum).FullOutput(i - 1)) /
                                         (MSHeatPump(MSHeatPumpNum).FullOutput(i) - MSHeatPump(MSHeatPumpNum).FullOutput(i - 1));
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           SpeedRatio,
                                           1.0,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            break;
                        }
                    }
                }
            }
        } else {
            // Calculate the part load fraction
            if (((QZnReq > SmallLoad && QZnReq < FullOutput) || (QZnReq < (-1.0 * SmallLoad) && QZnReq > FullOutput)) &&
                (!MSHeatPump(MSHeatPumpNum).Staged)) {

                Par(1) = MSHeatPumpNum;
                Par(2) = ZoneNum;
                if (FirstHVACIteration) {
                    Par(3) = 1.0;
                } else {
                    Par(3) = 0.0;
                }
                Par(4) = OpMode;
                Par(5) = QZnReq;
                Par(6) = OnOffAirFlowRatio;
                Par(7) = SupHeaterLoad;
                Par(9) = static_cast<int>(CompressorOp);
                // Check whether the low speed coil can meet the load or not
                CalcMSHeatPump(
                    state, MSHeatPumpNum, FirstHVACIteration, CompressorOp, 1, 0.0, 1.0, LowOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad);
                if ((QZnReq > 0.0 && QZnReq <= LowOutput) || (QZnReq < 0.0 && QZnReq >= LowOutput)) {
                    SpeedRatio = 0.0;
                    SpeedNum = 1;
                    SolveRoot(state, ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0, 1.0, Par);
                    if (SolFla == -1) {
                        if (!state.dataGlobal->WarmupFlag) {
                            if (state.dataHVACMultiSpdHP->ErrCountCyc == 0) {
                                ++state.dataHVACMultiSpdHP->ErrCountCyc; // TODO: Why is the error count shared among all heat pump units?
                                ShowWarningError(
                                    state, "Iteration limit exceeded calculating DX unit cycling ratio, for unit=" + MSHeatPump(MSHeatPumpNum).Name);
                                ShowContinueErrorTimeStamp(state, format("Cycling ratio returned={:.2R}", PartLoadFrac));
                            } else {
                                ++state.dataHVACMultiSpdHP->ErrCountCyc;
                                ShowRecurringWarningErrorAtEnd(
                                    state,
                                    MSHeatPump(MSHeatPumpNum).Name +
                                        "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...",
                                    MSHeatPump(MSHeatPumpNum).ErrIndexCyc,
                                    PartLoadFrac,
                                    PartLoadFrac);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError(state,
                                       "DX unit cycling ratio calculation failed: cycling limits exceeded, for unit=" +
                                           MSHeatPump(MSHeatPumpNum).DXCoolCoilName);
                    }
                } else {
                    // Check to see which speed to meet the load
                    PartLoadFrac = 1.0;
                    SpeedRatio = 1.0;
                    if (QZnReq < (-1.0 * SmallLoad)) { // Cooling
                        for (i = 2; i <= MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling; ++i) {
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           SpeedRatio,
                                           PartLoadFrac,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            if (QZnReq >= TempOutput) {
                                SpeedNum = i;
                                break;
                            }
                        }
                    } else {
                        for (i = 2; i <= MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating; ++i) {
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           i,
                                           SpeedRatio,
                                           PartLoadFrac,
                                           TempOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            if (QZnReq <= TempOutput) {
                                SpeedNum = i;
                                break;
                            }
                        }
                    }
                    Par(8) = SpeedNum;
                    SolveRoot(state, ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0, 1.0, Par);
                    if (SolFla == -1) {
                        if (!state.dataGlobal->WarmupFlag) {
                            if (state.dataHVACMultiSpdHP->ErrCountVar == 0) {
                                ++state.dataHVACMultiSpdHP->ErrCountVar;
                                ShowWarningError(
                                    state, "Iteration limit exceeded calculating DX unit speed ratio, for unit=" + MSHeatPump(MSHeatPumpNum).Name);
                                ShowContinueErrorTimeStamp(state, format("Speed ratio returned=[{:.2R}], Speed number ={}", SpeedRatio, SpeedNum));
                            } else {
                                ++state.dataHVACMultiSpdHP->ErrCountVar;
                                ShowRecurringWarningErrorAtEnd(
                                    state,
                                    MSHeatPump(MSHeatPumpNum).Name +
                                        "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...",
                                    MSHeatPump(MSHeatPumpNum).ErrIndexVar,
                                    SpeedRatio,
                                    SpeedRatio);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError(state,
                                       "DX unit compressor speed calculation failed: speed limits exceeded, for unit=" +
                                           MSHeatPump(MSHeatPumpNum).DXCoolCoilName);
                    }
                }
            } else {
                // Staged thermostat performance
                if (MSHeatPump(MSHeatPumpNum).StageNum != 0) {
                    Par(1) = MSHeatPumpNum;
                    Par(2) = ZoneNum;
                    if (FirstHVACIteration) {
                        Par(3) = 1.0;
                    } else {
                        Par(3) = 0.0;
                    }
                    Par(4) = OpMode;
                    Par(5) = QZnReq;
                    Par(6) = OnOffAirFlowRatio;
                    Par(7) = SupHeaterLoad;
                    Par(9) = static_cast<int>(CompressorOp);
                    SpeedNum = std::abs(MSHeatPump(MSHeatPumpNum).StageNum);
                    Par(8) = SpeedNum;
                    if (SpeedNum == 1) {
                        CalcMSHeatPump(
                            state, MSHeatPumpNum, FirstHVACIteration, CompressorOp, 1, 0.0, 1.0, LowOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad);
                        SpeedRatio = 0.0;
                        if ((QZnReq > 0.0 && QZnReq <= LowOutput) || (QZnReq < 0.0 && QZnReq >= LowOutput)) {
                            SolveRoot(state, ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0, 1.0, Par);
                            if (SolFla == -1) {
                                if (!state.dataGlobal->WarmupFlag) {
                                    if (state.dataHVACMultiSpdHP->ErrCountCyc == 0) {
                                        ++state.dataHVACMultiSpdHP->ErrCountCyc;
                                        ShowWarningError(state,
                                                         "Iteration limit exceeded calculating DX unit cycling ratio, for unit=" +
                                                             MSHeatPump(MSHeatPumpNum).Name);
                                        ShowContinueErrorTimeStamp(state, format("Cycling ratio returned={:.2R}", PartLoadFrac));
                                    } else {
                                        ++state.dataHVACMultiSpdHP->ErrCountCyc;
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            MSHeatPump(MSHeatPumpNum).Name +
                                                "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...",
                                            MSHeatPump(MSHeatPumpNum).ErrIndexCyc,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            } else if (SolFla == -2) {
                                ShowFatalError(state,
                                               "DX unit cycling ratio calculation failed: cycling limits exceeded, for unit=" +
                                                   MSHeatPump(MSHeatPumpNum).DXCoolCoilName);
                            }
                        } else {
                            FullOutput = LowOutput;
                            PartLoadFrac = 1.0;
                        }
                    } else {
                        if (MSHeatPump(MSHeatPumpNum).StageNum < 0) {
                            SpeedNum = min(MSHeatPump(MSHeatPumpNum).NumOfSpeedCooling, std::abs(MSHeatPump(MSHeatPumpNum).StageNum));
                        } else {
                            SpeedNum = min(MSHeatPump(MSHeatPumpNum).NumOfSpeedHeating, std::abs(MSHeatPump(MSHeatPumpNum).StageNum));
                        }
                        CalcMSHeatPump(state,
                                       MSHeatPumpNum,
                                       FirstHVACIteration,
                                       CompressorOp,
                                       SpeedNum,
                                       0.0,
                                       1.0,
                                       LowOutput,
                                       QZnReq,
                                       OnOffAirFlowRatio,
                                       SupHeaterLoad);
                        if ((QZnReq > 0.0 && QZnReq >= LowOutput) || (QZnReq < 0.0 && QZnReq <= LowOutput)) {
                            CalcMSHeatPump(state,
                                           MSHeatPumpNum,
                                           FirstHVACIteration,
                                           CompressorOp,
                                           SpeedNum,
                                           1.0,
                                           1.0,
                                           FullOutput,
                                           QZnReq,
                                           OnOffAirFlowRatio,
                                           SupHeaterLoad);
                            if ((QZnReq > 0.0 && QZnReq <= FullOutput) || (QZnReq < 0.0 && QZnReq >= FullOutput)) {
                                Par(8) = SpeedNum;
                                SolveRoot(state, ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACMultiSpdHP->ErrCountVar == 0) {
                                            ++state.dataHVACMultiSpdHP->ErrCountVar;
                                            ShowWarningError(state,
                                                             "Iteration limit exceeded calculating DX unit speed ratio, for unit=" +
                                                                 MSHeatPump(MSHeatPumpNum).Name);
                                            ShowContinueErrorTimeStamp(
                                                state, format("Speed ratio returned=[{:.2R}], Speed number ={}", SpeedRatio, SpeedNum));
                                        } else {
                                            ++state.dataHVACMultiSpdHP->ErrCountVar;
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                MSHeatPump(MSHeatPumpNum).Name +
                                                    "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...",
                                                MSHeatPump(MSHeatPumpNum).ErrIndexVar,
                                                SpeedRatio,
                                                SpeedRatio);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    ShowFatalError(state,
                                                   "DX unit compressor speed calculation failed: speed limits exceeded, for unit=" +
                                                       MSHeatPump(MSHeatPumpNum).DXCoolCoilName);
                                }
                            } else {
                                SpeedRatio = 1.0;
                            }
                        } else { // lowOutput provides a larger capacity than needed
                            SpeedRatio = 0.0;
                        }
                    }
                }
            }
        }

        // if the DX heating coil cannot meet the load, trim with supplemental heater
        // occurs with constant fan mode when compressor is on or off
        // occurs with cycling fan mode when compressor PLR is equal to 1
        if ((QZnReq > SmallLoad && QZnReq > FullOutput)) {
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            if (MSHeatPump(MSHeatPumpNum).Staged && SpeedNum == 1) SpeedRatio = 0.0;
            if (OutsideDryBulbTemp <= MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp) {
                SupHeaterLoad = QZnReq - FullOutput;
            } else {
                SupHeaterLoad = 0.0;
            }
            CalcMSHeatPump(state,
                           MSHeatPumpNum,
                           FirstHVACIteration,
                           CompressorOp,
                           SpeedNum,
                           SpeedRatio,
                           PartLoadFrac,
                           TempOutput,
                           QZnReq,
                           OnOffAirFlowRatio,
                           SupHeaterLoad);
        }

        // check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
        if (state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp > MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp &&
            SupHeaterLoad > 0.0) {

            //   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
            SupHeaterLoad = 0.0;
            CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, OpMode, QCoilActual);

            //   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
            //   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
            //   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
            //   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
            if (state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp < MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp) {
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).HumRat);
                SupHeaterLoad =
                    state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirInletNodeNum).MassFlowRate * CpAir *
                    (MSHeatPump(MSHeatPumpNum).SuppMaxAirTemp - state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).AirOutletNodeNum).Temp);

            } else {
                SupHeaterLoad = 0.0;
            }
        }

        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).CycRatio = PartLoadFrac;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedRatio = SpeedRatio;
        state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).SpeedNum = SpeedNum;
    }

    //******************************************************************************

    void CalcMSHeatPump(EnergyPlusData &state,
                        int const MSHeatPumpNum,                                 // Engine driven heat pump number
                        bool const FirstHVACIteration,                           // Flag for 1st HVAC iteration
                        DataHVACGlobals::CompressorOperation const CompressorOp, // Compressor on/off; 1=on, 0=off
                        int const SpeedNum,                                      // Speed number
                        Real64 const SpeedRatio,                                 // Compressor speed ratio
                        Real64 const PartLoadFrac,                               // Compressor part load fraction
                        Real64 &LoadMet,                                         // Load met by unit (W)
                        Real64 const QZnReq,                                     // Zone load (W)
                        Real64 &OnOffAirFlowRatio,                               // Ratio of compressor ON airflow to AVERAGE airflow over timestep
                        Real64 &SupHeaterLoad                                    // supplemental heater load (W)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    June 2007
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will calcultes MSHP performance based on given system load

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES: na

        // Using/Aliasing
        using DXCoils::SimDXCoilMultiSpeed;
        using Fans::SimulateFanComponents;
        using HeatingCoils::SimulateHeatingCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  INTEGER, PARAMETER  ::   On  = 1           ! Compressor on flag
        //  INTEGER, PARAMETER  ::   Off = 2           ! Compressor off flag

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVMS TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;            // MSHP air outlet node
        int InletNode;             // MSHP air inlet node
        Real64 OutsideDryBulbTemp; // Outdoor dry bulb temperature [C]
        Real64 AirMassFlow;        // Air mass flow rate [kg/s]
        int FanInletNode;          // MSHP air outlet node
        int FanOutletNode;         // MSHP air inlet node
        Real64 SavePartloadRatio;
        Real64 SaveSpeedRatio;
        Real64 QCoilActual;  // coil load actually delivered returned to calling component
        Real64 MinWaterFlow; // minimum water flow rate
        Real64 ErrorToler;   // supplemental heating coil convergence tollerance

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        OutletNode = MSHeatPump(MSHeatPumpNum).AirOutletNodeNum;
        InletNode = MSHeatPump(MSHeatPumpNum).AirInletNodeNum;
        if (MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex > 0) {
            if (state.dataDXCoils->DXCoil(MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex).IsSecondaryDXCoilInZone) {
                OutsideDryBulbTemp = state.dataHeatBalFanSys->ZT(state.dataDXCoils->DXCoil(MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex).SecZonePtr);
            } else {
                OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
            }
        } else if (MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex > 0) {
            if (state.dataDXCoils->DXCoil(MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex).IsSecondaryDXCoilInZone) {
                OutsideDryBulbTemp = state.dataHeatBalFanSys->ZT(state.dataDXCoils->DXCoil(MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex).SecZonePtr);
            } else {
                OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
            }
        } else {
            OutsideDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
        }
        FanOutletNode = MSHeatPump(MSHeatPumpNum).FanOutletNode;
        FanInletNode = MSHeatPump(MSHeatPumpNum).FanInletNode;

        state.dataHVACMultiSpdHP->SaveCompressorPLR = 0.0;
        SavePartloadRatio = 0.0;
        MinWaterFlow = 0.0;
        ErrorToler = 0.001;
        // Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
        SetAverageAirFlow(state, MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio);

        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        // if blow through, simulate fan then coils
        if (MSHeatPump(MSHeatPumpNum).FanPlaceType == BlowThru) {
            SimulateFanComponents(state,
                                  MSHeatPump(MSHeatPumpNum).FanName,
                                  FirstHVACIteration,
                                  MSHeatPump(MSHeatPumpNum).FanNum,
                                  state.dataHVACMultiSpdHP->FanSpeedRatio);
            if (QZnReq < (-1.0 * SmallLoad)) {
                if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorCooling) {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        SpeedRatio,
                                        PartLoadFrac,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
                state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex);
            } else {
                SimDXCoilMultiSpeed(state,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                    0.0,
                                    0.0,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                    SpeedNum,
                                    MSHeatPump(MSHeatPumpNum).OpMode,
                                    CompressorOp);
            }
            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == MultiSpeedHeatingCoil) {
                if (QZnReq > SmallLoad) {
                    if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorHeating) {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            SpeedRatio,
                                            PartLoadFrac,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                        SavePartloadRatio = PartLoadFrac;
                        SaveSpeedRatio = SpeedRatio;
                    } else {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            0.0,
                                            0.0,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                    }
                    state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex);
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
            } else if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingElectric_MultiStage ||
                       MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingGas_MultiStage) {
                if (QZnReq > SmallLoad) {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  PartLoadFrac,
                                                  SpeedNum,
                                                  SpeedRatio);
                } else {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  0.0,
                                                  SpeedNum,
                                                  0.0);
                }
            } else {
                CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump(MSHeatPumpNum).OpMode, QCoilActual, PartLoadFrac);
            }
            // Call twice to ensure the fan outlet conditions are updated
            SimulateFanComponents(state,
                                  MSHeatPump(MSHeatPumpNum).FanName,
                                  FirstHVACIteration,
                                  MSHeatPump(MSHeatPumpNum).FanNum,
                                  state.dataHVACMultiSpdHP->FanSpeedRatio);
            if (QZnReq < (-1.0 * SmallLoad)) {
                if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorCooling) {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        SpeedRatio,
                                        PartLoadFrac,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
                state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex);
            } else {
                SimDXCoilMultiSpeed(state,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                    0.0,
                                    0.0,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                    SpeedNum,
                                    MSHeatPump(MSHeatPumpNum).OpMode,
                                    CompressorOp);
            }
            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == MultiSpeedHeatingCoil) {
                if (QZnReq > SmallLoad) {
                    if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorHeating) {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            SpeedRatio,
                                            PartLoadFrac,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                        SavePartloadRatio = PartLoadFrac;
                        SaveSpeedRatio = SpeedRatio;
                    } else {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            0.0,
                                            0.0,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                    }
                    state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex);
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
            } else if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingElectric_MultiStage ||
                       MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingGas_MultiStage) {
                if (QZnReq > SmallLoad) {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  PartLoadFrac,
                                                  SpeedNum,
                                                  SpeedRatio);
                } else {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  0.0,
                                                  SpeedNum,
                                                  0.0);
                }
            } else {
                CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump(MSHeatPumpNum).OpMode, QCoilActual, PartLoadFrac);
            }
            //  Simulate supplemental heating coil for blow through fan
            if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum > 0) {
                CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, MSHeatPump(MSHeatPumpNum).OpMode, QCoilActual);
            }
        } else { // otherwise simulate DX coils then fan then supplemental heater
            if (QZnReq < (-1.0 * SmallLoad)) {
                if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorCooling) {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        SpeedRatio,
                                        PartLoadFrac,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
                state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex);
            } else {
                SimDXCoilMultiSpeed(state,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilName,
                                    0.0,
                                    0.0,
                                    MSHeatPump(MSHeatPumpNum).DXCoolCoilIndex,
                                    SpeedNum,
                                    MSHeatPump(MSHeatPumpNum).OpMode,
                                    CompressorOp);
            }
            if (MSHeatPump(MSHeatPumpNum).HeatCoilType == MultiSpeedHeatingCoil) {
                if (QZnReq > SmallLoad) {
                    if (OutsideDryBulbTemp > MSHeatPump(MSHeatPumpNum).MinOATCompressorHeating) {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            SpeedRatio,
                                            PartLoadFrac,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                        SavePartloadRatio = PartLoadFrac;
                        SaveSpeedRatio = SpeedRatio;
                    } else {
                        SimDXCoilMultiSpeed(state,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                            0.0,
                                            0.0,
                                            MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                            SpeedNum,
                                            MSHeatPump(MSHeatPumpNum).OpMode,
                                            CompressorOp);
                    }
                    state.dataHVACMultiSpdHP->SaveCompressorPLR = state.dataDXCoils->DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex);
                } else {
                    SimDXCoilMultiSpeed(state,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilName,
                                        0.0,
                                        0.0,
                                        MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex,
                                        SpeedNum,
                                        MSHeatPump(MSHeatPumpNum).OpMode,
                                        CompressorOp);
                }
            } else if (MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingElectric_MultiStage ||
                       MSHeatPump(MSHeatPumpNum).HeatCoilType == Coil_HeatingGas_MultiStage) {
                if (QZnReq > SmallLoad) {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  PartLoadFrac,
                                                  SpeedNum,
                                                  SpeedRatio);
                } else {
                    SimulateHeatingCoilComponents(state,
                                                  MSHeatPump(MSHeatPumpNum).HeatCoilName,
                                                  FirstHVACIteration,
                                                  _,
                                                  0,
                                                  _,
                                                  _,
                                                  MSHeatPump(MSHeatPumpNum).OpMode,
                                                  0.0,
                                                  SpeedNum,
                                                  0.0);
                }
            } else {
                CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump(MSHeatPumpNum).OpMode, QCoilActual, PartLoadFrac);
            }
            SimulateFanComponents(state,
                                  MSHeatPump(MSHeatPumpNum).FanName,
                                  FirstHVACIteration,
                                  MSHeatPump(MSHeatPumpNum).FanNum,
                                  state.dataHVACMultiSpdHP->FanSpeedRatio);
            //  Simulate supplemental heating coil for draw through fan
            if (MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum > 0) {
                CalcNonDXHeatingCoils(state, MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, MSHeatPump(MSHeatPumpNum).OpMode, QCoilActual);
            }
        }

        // calculate sensible load met
        Real64 SensibleOutput(0.0); // sensible output rate
        // calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
        Real64 MinHumRat = state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).HumRat;
        if (state.dataLoopNodes->Node(OutletNode).Temp < state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).Temp)
            MinHumRat = state.dataLoopNodes->Node(OutletNode).HumRat;
        SensibleOutput = AirMassFlow *
                         Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(state.dataLoopNodes->Node(OutletNode).Temp,
                                                                    MinHumRat,
                                                                    state.dataLoopNodes->Node(MSHeatPump(MSHeatPumpNum).NodeNumOfControlledZone).Temp,
                                                                    MinHumRat);
        LoadMet = SensibleOutput - MSHeatPump(MSHeatPumpNum).LoadLoss;

        MSHeatPump(MSHeatPumpNum).LoadMet = LoadMet;
    }

    //******************************************************************************

    Real64 MSHPCyclingResidual(EnergyPlusData &state,
                               Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                               Array1D<Real64> const &Par // par(1) = MSHPNum
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilCyclingResidual

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
        Real64 MSHPCyclingResidual;

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
        // par(9) = CompressorOp

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int MSHeatPumpNum;                                 // MSHP index
        int ZoneNum;                                       // Zone index
        bool FirstHVACIteration;                           // FirstHVACIteration flag
        int OpMode;                                        // Compressor operating mode
        Real64 QZnReq;                                     // zone load (W)
        Real64 OnOffAirFlowRatio;                          // ratio of compressor ON airflow to average airflow over timestep
        Real64 ActualOutput;                               // delivered capacity of MSHP
        DataHVACGlobals::CompressorOperation CompressorOp; // compressor operation; 1=on, 0=off

        MSHeatPumpNum = int(Par(1));
        ZoneNum = int(Par(2));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        OpMode = int(Par(4));
        QZnReq = Par(5);
        OnOffAirFlowRatio = Par(6);
        SupHeaterLoad = Par(7);
        CompressorOp = static_cast<DataHVACGlobals::CompressorOperation>(Par(9));

        CalcMSHeatPump(
            state, MSHeatPumpNum, FirstHVACIteration, CompressorOp, 1, 0.0, PartLoadFrac, ActualOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad);

        MSHPCyclingResidual = (ActualOutput - QZnReq) / QZnReq;
        return MSHPCyclingResidual;
    }

    //******************************************************************************

    Real64 MSHPVarSpeedResidual(EnergyPlusData &state,
                                Real64 const SpeedRatio,   // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                Array1D<Real64> const &Par // par(1) = MSHPNum
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na L. Gu, Oct. 2006, revised for multispeed heat pump use
        //       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilVarSpeedResidual

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
        Real64 MSHPVarSpeedResidual;

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
        // par(9) = CompressorOp

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int MSHeatPumpNum;                                 // MSHP index
        int ZoneNum;                                       // Zone index
        bool FirstHVACIteration;                           // FirstHVACIteration flag
        int OpMode;                                        // Compressor operating mode
        Real64 QZnReq;                                     // zone load (W)
        Real64 OnOffAirFlowRatio;                          // ratio of compressor ON airflow to average airflow over timestep
        Real64 ActualOutput;                               // delivered capacity of MSHP
        int SpeedNum;                                      // Speed number
        DataHVACGlobals::CompressorOperation CompressorOp; // compressor operation; 1=on, 0=off

        MSHeatPumpNum = int(Par(1));
        ZoneNum = int(Par(2));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        OpMode = int(Par(4));
        QZnReq = Par(5);
        OnOffAirFlowRatio = Par(6);
        SupHeaterLoad = Par(7);
        SpeedNum = int(Par(8));
        CompressorOp = static_cast<DataHVACGlobals::CompressorOperation>(Par(9));

        CalcMSHeatPump(state,
                       MSHeatPumpNum,
                       FirstHVACIteration,
                       CompressorOp,
                       SpeedNum,
                       SpeedRatio,
                       1.0,
                       ActualOutput,
                       QZnReq,
                       OnOffAirFlowRatio,
                       SupHeaterLoad);

        MSHPVarSpeedResidual = (ActualOutput - QZnReq) / QZnReq;
        return MSHPVarSpeedResidual;
    }

    //******************************************************************************

    void UpdateMSHeatPump(EnergyPlusData &state, int const MSHeatPumpNum) // Engine driven heat pump number
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    June 2007
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will update MSHP performance and calculate heat recovery rate and crankcase heater power

        // METHODOLOGY EMPLOYED:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // Calculate heat recovery
        if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecActive) {
            MSHPHeatRecovery(state, MSHeatPumpNum);
        }

        if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
            state.dataAirLoop->AirLoopAFNInfo(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirLoopNumber).LoopSystemOnMassFlowrate =
                state.dataHVACMultiSpdHP->CompOnMassFlow;
            state.dataAirLoop->AirLoopAFNInfo(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirLoopNumber).LoopSystemOffMassFlowrate =
                state.dataHVACMultiSpdHP->CompOffMassFlow;
            state.dataAirLoop->AirLoopAFNInfo(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirLoopNumber).LoopFanOperationMode =
                state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).OpMode;
            state.dataAirLoop->AirLoopAFNInfo(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirLoopNumber).LoopOnOffFanPartLoadRatio =
                state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).FanPartLoadRatio;
            state.dataAirLoop->AirLoopAFNInfo(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirLoopNumber).LoopCompCycRatio =
                state.dataHVACMultiSpdHP->MSHeatPumpReport(MSHeatPumpNum).CycRatio;
        }
    }

    //******************************************************************************

    void ReportMSHeatPump(EnergyPlusData &state, int const MSHeatPumpNum) // Engine driven heat pump number
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    June 2007
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will write values to output variables in MSHP

        // METHODOLOGY EMPLOYED:

        // REFERENCES: na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant;

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);
        auto &MSHeatPumpReport(state.dataHVACMultiSpdHP->MSHeatPumpReport);

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        MSHeatPumpReport(MSHeatPumpNum).ElecPowerConsumption = MSHeatPump(MSHeatPumpNum).ElecPower * ReportingConstant; // + &
        MSHeatPumpReport(MSHeatPumpNum).HeatRecoveryEnergy = MSHeatPump(MSHeatPumpNum).HeatRecoveryRate * ReportingConstant;

        MSHeatPumpReport(MSHeatPumpNum).AuxElecHeatConsumption = 0.0;
        MSHeatPumpReport(MSHeatPumpNum).AuxElecCoolConsumption = 0.0;

        MSHeatPump(MSHeatPumpNum).AuxElecPower = MSHeatPump(MSHeatPumpNum).AuxOnCyclePower * state.dataHVACMultiSpdHP->SaveCompressorPLR +
                                                 MSHeatPump(MSHeatPumpNum).AuxOffCyclePower * (1.0 - state.dataHVACMultiSpdHP->SaveCompressorPLR);
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
            MSHeatPumpReport(MSHeatPumpNum).AuxElecCoolConsumption =
                MSHeatPump(MSHeatPumpNum).AuxOnCyclePower * state.dataHVACMultiSpdHP->SaveCompressorPLR * ReportingConstant;
        }
        if (MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode) {
            MSHeatPumpReport(MSHeatPumpNum).AuxElecHeatConsumption =
                MSHeatPump(MSHeatPumpNum).AuxOnCyclePower * state.dataHVACMultiSpdHP->SaveCompressorPLR * ReportingConstant;
        }
        if (MSHeatPump(MSHeatPumpNum).LastMode == ModeOfOperation::HeatingMode) {
            MSHeatPumpReport(MSHeatPumpNum).AuxElecHeatConsumption +=
                MSHeatPump(MSHeatPumpNum).AuxOffCyclePower * (1.0 - state.dataHVACMultiSpdHP->SaveCompressorPLR) * ReportingConstant;
        } else {
            MSHeatPumpReport(MSHeatPumpNum).AuxElecCoolConsumption +=
                MSHeatPump(MSHeatPumpNum).AuxOffCyclePower * (1.0 - state.dataHVACMultiSpdHP->SaveCompressorPLR) * ReportingConstant;
        }

        if (MSHeatPump(MSHeatPumpNum).FirstPass) {
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(
                    state, state.dataSize->CurZoneEqNum, state.dataSize->CurSysNum, MSHeatPump(MSHeatPumpNum).FirstPass);
            }
        }

        // reset to 1 in case blow through fan configuration (fan resets to 1, but for blow thru fans coil sets back down < 1)
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
    }

    void MSHPHeatRecovery(EnergyPlusData &state, int const MSHeatPumpNum) // Number of the current electric MSHP being simulated
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu
        //       DATE WRITTEN:    June 2007
        //       MODIFIED:        na
        //       RE-ENGINEERED    Revised to calculate MSHP heat recovery rate based on EIR Chiller heat recovery subroutine
        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the heat recovered from MSHP

        // Using/Aliasing
        auto &MSHPWasteHeat = state.dataHVACGlobal->MSHPWasteHeat;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("MSHPHeatRecovery");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HeatRecInNode;          // Node number of heat recovery water inlet node
        int HeatRecOutNode;         // Node number of heat recovery water outlet node
        Real64 QHeatRec;            // Total heat recovered [W]
        Real64 HeatRecInletTemp;    // Heat reclaim inlet temp [C]
        Real64 HeatRecOutletTemp;   // Heat reclaim outlet temp [C]
        Real64 HeatRecMassFlowRate; // Heat reclaim mass flow rate [m3/s]
        Real64 CpHeatRec;           // Heat reclaim water inlet specific heat [J/kg-K]

        // Begin routine
        HeatRecInNode = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecInletNodeNum;
        HeatRecOutNode = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecOutletNodeNum;

        // Inlet node to the heat recovery heat exchanger
        HeatRecInletTemp = state.dataLoopNodes->Node(HeatRecInNode).Temp;

        // Set heat recovery mass flow rates
        HeatRecMassFlowRate = state.dataLoopNodes->Node(HeatRecInNode).MassFlowRate;

        QHeatRec = MSHPWasteHeat;

        if (HeatRecMassFlowRate > 0.0) {

            CpHeatRec =
                GetSpecificHeatGlycol(state,
                                      state.dataPlnt->PlantLoop(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HRPlantLoc.loopNum).FluidName,
                                      HeatRecInletTemp,
                                      state.dataPlnt->PlantLoop(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HRPlantLoc.loopNum).FluidIndex,
                                      RoutineName);

            HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + HeatRecInletTemp;
            if (HeatRecOutletTemp > state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).MaxHeatRecOutletTemp) {
                HeatRecOutletTemp = max(HeatRecInletTemp, state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).MaxHeatRecOutletTemp);
                QHeatRec = HeatRecMassFlowRate * CpHeatRec * (HeatRecOutletTemp - HeatRecInletTemp);
            }
        } else {
            HeatRecOutletTemp = HeatRecInletTemp;
            QHeatRec = 0.0;
        }

        SafeCopyPlantNode(state, HeatRecInNode, HeatRecOutNode);
        // changed outputs
        state.dataLoopNodes->Node(HeatRecOutNode).Temp = HeatRecOutletTemp;

        state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecoveryRate = QHeatRec;
        state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecoveryInletTemp = HeatRecInletTemp;
        state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecoveryOutletTemp = HeatRecOutletTemp;
        state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatRecoveryMassFlowRate = HeatRecMassFlowRate;
    }

    void SetAverageAirFlow(EnergyPlusData &state,
                           int const MSHeatPumpNum,          // Unit index
                           Real64 const PartLoadRatio,       // unit part load ratio
                           Real64 &OnOffAirFlowRatio,        // ratio of compressor ON airflow to average airflow over timestep
                           Optional_int_const SpeedNum,      // Speed number
                           Optional<Real64 const> SpeedRatio // Speed ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing
        //       DATE WRITTEN   June 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  Resived to meet requirements of multispeed heat pump based on the same subroutine
        //                      in PTHP module

        // PURPOSE OF THIS SUBROUTINE:
        // Set the average air mass flow rates using the part load fraction of the heat pump for this time step
        // Set OnOffAirFlowRatio to be used by DX coils

        // Using/Aliasing
        auto &MSHPMassFlowRateHigh = state.dataHVACGlobal->MSHPMassFlowRateHigh;
        auto &MSHPMassFlowRateLow = state.dataHVACGlobal->MSHPMassFlowRateLow;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;              // inlet node number for PTHPNum
        Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step

        MSHPMassFlowRateLow = 0.0;  // Mass flow rate at low speed
        MSHPMassFlowRateHigh = 0.0; // Mass flow rate at high speed

        if (!state.dataZoneEnergyDemand->CurDeadBandOrSetback(state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).ControlZoneNum) &&
            present(SpeedNum)) {
            if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::HeatingMode) {
                if (SpeedNum == 1) {
                    state.dataHVACMultiSpdHP->CompOnMassFlow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum);
                    state.dataHVACMultiSpdHP->CompOnFlowRatio = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(SpeedNum);
                    MSHPMassFlowRateLow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(1);
                    MSHPMassFlowRateHigh = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(1);
                } else if (SpeedNum > 1) {
                    state.dataHVACMultiSpdHP->CompOnMassFlow =
                        SpeedRatio * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum) +
                        (1.0 - SpeedRatio) * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum - 1);
                    state.dataHVACMultiSpdHP->CompOnFlowRatio =
                        SpeedRatio * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(SpeedNum) +
                        (1.0 - SpeedRatio) * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(SpeedNum - 1);
                    MSHPMassFlowRateLow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum - 1);
                    MSHPMassFlowRateHigh = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum);
                }
            } else if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatCoolMode == ModeOfOperation::CoolingMode) {
                if (SpeedNum == 1) {
                    state.dataHVACMultiSpdHP->CompOnMassFlow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum);
                    state.dataHVACMultiSpdHP->CompOnFlowRatio = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(SpeedNum);
                    MSHPMassFlowRateLow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(1);
                    MSHPMassFlowRateHigh = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(1);
                } else if (SpeedNum > 1) {
                    state.dataHVACMultiSpdHP->CompOnMassFlow =
                        SpeedRatio * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum) +
                        (1.0 - SpeedRatio) * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum - 1);
                    state.dataHVACMultiSpdHP->CompOnFlowRatio =
                        SpeedRatio * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(SpeedNum) +
                        (1.0 - SpeedRatio) * state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(SpeedNum - 1);
                    MSHPMassFlowRateLow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum - 1);
                    MSHPMassFlowRateHigh = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum);
                }
            }
        }
        InletNode = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirInletNodeNum;

        // Set up fan flow rate during compressor off time
        if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).OpMode == ContFanCycCoil && present(SpeedNum)) {
            if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AirFlowControl == AirflowControl::UseCompressorOnFlow &&
                state.dataHVACMultiSpdHP->CompOnMassFlow > 0.0) {
                if (state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).LastMode == ModeOfOperation::HeatingMode) {
                    state.dataHVACMultiSpdHP->CompOffMassFlow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatMassFlowRate(SpeedNum);
                    state.dataHVACMultiSpdHP->CompOffFlowRatio = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HeatingSpeedRatio(SpeedNum);
                } else {
                    state.dataHVACMultiSpdHP->CompOffMassFlow = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolMassFlowRate(SpeedNum);
                    state.dataHVACMultiSpdHP->CompOffFlowRatio = state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).CoolingSpeedRatio(SpeedNum);
                }
            }
        }

        if (present(SpeedNum)) {
            if (SpeedNum > 1) {
                AverageUnitMassFlow = state.dataHVACMultiSpdHP->CompOnMassFlow;
                state.dataHVACMultiSpdHP->FanSpeedRatio = state.dataHVACMultiSpdHP->CompOnFlowRatio;
            } else {
                AverageUnitMassFlow =
                    (PartLoadRatio * state.dataHVACMultiSpdHP->CompOnMassFlow) + ((1 - PartLoadRatio) * state.dataHVACMultiSpdHP->CompOffMassFlow);
                if (state.dataHVACMultiSpdHP->CompOffFlowRatio > 0.0) {
                    state.dataHVACMultiSpdHP->FanSpeedRatio = (PartLoadRatio * state.dataHVACMultiSpdHP->CompOnFlowRatio) +
                                                              ((1 - PartLoadRatio) * state.dataHVACMultiSpdHP->CompOffFlowRatio);
                } else {
                    state.dataHVACMultiSpdHP->FanSpeedRatio = state.dataHVACMultiSpdHP->CompOnFlowRatio;
                }
            }
        } else {
            AverageUnitMassFlow =
                (PartLoadRatio * state.dataHVACMultiSpdHP->CompOnMassFlow) + ((1 - PartLoadRatio) * state.dataHVACMultiSpdHP->CompOffMassFlow);
            if (state.dataHVACMultiSpdHP->CompOffFlowRatio > 0.0) {
                state.dataHVACMultiSpdHP->FanSpeedRatio =
                    (PartLoadRatio * state.dataHVACMultiSpdHP->CompOnFlowRatio) + ((1 - PartLoadRatio) * state.dataHVACMultiSpdHP->CompOffFlowRatio);
            } else {
                state.dataHVACMultiSpdHP->FanSpeedRatio = state.dataHVACMultiSpdHP->CompOnFlowRatio;
            }
        }

        //!!LKL Discrepancy with > 0
        if (GetCurrentScheduleValue(state, state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).AvaiSchedPtr) == 0.0) {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
            OnOffAirFlowRatio = 0.0;
        } else {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = AverageUnitMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AverageUnitMassFlow;
            if (AverageUnitMassFlow > 0.0) {
                OnOffAirFlowRatio = state.dataHVACMultiSpdHP->CompOnMassFlow / AverageUnitMassFlow;
            } else {
                OnOffAirFlowRatio = 0.0;
            }
        }
    }

    void CalcNonDXHeatingCoils(EnergyPlusData &state,
                               int const MSHeatPumpNum,       // multispeed heatpump index
                               bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
                               Real64 const HeatingLoad,      // supplemental coil load to be met by unit (watts)
                               int const FanMode,             // fan operation mode
                               Real64 &HeatCoilLoadmet,       // Heating Load Met
                               Optional<Real64 const> PartLoadFrac)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

        // METHODOLOGY EMPLOYED:
        // Simply calls the different heating coil component.  The hot water flow rate matching the coil load
        // is calculated iteratively.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;

        using General::SolveRoot;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using PlantUtilities::SetComponentFlowRate;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        static constexpr std::string_view CurrentModuleObject("AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed");

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr ErrTolerance(0.001); // convergence limit for hotwater coil
        int constexpr SolveMaxIter(50);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QCoilActual;     // actual heating load met
        Real64 mdot;            // heating coil steam or hot water mass flow rate
        Real64 MinWaterFlow;    // coil minimum hot water mass flow rate, kg/s
        Real64 MaxHotWaterFlow; // coil maximum hot water mass flow rate, kg/s
        Real64 HotWaterMdot;    // actual hot water mass flow rate
        Array1D<Real64> Par(3);
        int SolFlag;

        int HeatCoilType;
        int HeatCoilNum;
        Real64 MaxCoilFluidFlow;
        Real64 SteamCoilHeatingLoad;
        int CoilControlNode;
        int CoilOutletNode;
        PlantLocation plantLoc{};

        QCoilActual = 0.0;

        auto &MSHeatPump(state.dataHVACMultiSpdHP->MSHeatPump);

        if (present(PartLoadFrac)) {
            HeatCoilType = MSHeatPump(MSHeatPumpNum).HeatCoilType;
            state.dataHVACMultiSpdHP->HeatCoilName = MSHeatPump(MSHeatPumpNum).HeatCoilName;
            HeatCoilNum = MSHeatPump(MSHeatPumpNum).HeatCoilNum;
            MaxCoilFluidFlow = MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow;
            CoilControlNode = MSHeatPump(MSHeatPumpNum).CoilControlNode;
            CoilOutletNode = MSHeatPump(MSHeatPumpNum).CoilOutletNode;
            plantLoc = MSHeatPump(MSHeatPumpNum).plantLoc;
        } else {
            HeatCoilType = MSHeatPump(MSHeatPumpNum).SuppHeatCoilType;
            state.dataHVACMultiSpdHP->HeatCoilName = MSHeatPump(MSHeatPumpNum).SuppHeatCoilName;
            HeatCoilNum = MSHeatPump(MSHeatPumpNum).SuppHeatCoilNum;
            MaxCoilFluidFlow = MSHeatPump(MSHeatPumpNum).MaxSuppCoilFluidFlow;
            CoilControlNode = MSHeatPump(MSHeatPumpNum).SuppCoilControlNode;
            CoilOutletNode = MSHeatPump(MSHeatPumpNum).SuppCoilOutletNode;
            plantLoc = MSHeatPump(MSHeatPumpNum).SuppPlantLoc;
        }

        MSHeatPump(MSHeatPumpNum).HotWaterPlantLoc = plantLoc;
        MSHeatPump(MSHeatPumpNum).HotWaterCoilControlNode = CoilControlNode;
        MSHeatPump(MSHeatPumpNum).HotWaterCoilOutletNode = CoilOutletNode;
        MSHeatPump(MSHeatPumpNum).HotWaterCoilName = state.dataHVACMultiSpdHP->HeatCoilName;
        MSHeatPump(MSHeatPumpNum).HotWaterCoilNum = HeatCoilNum;

        if (HeatingLoad > SmallLoad) {

            switch (HeatCoilType) {
            case SuppHeatingCoilGas:
            case SuppHeatingCoilElec: {
                SimulateHeatingCoilComponents(
                    state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatingLoad, HeatCoilNum, QCoilActual, true, FanMode);
            } break;
            case Coil_HeatingWater: {
                if (present(PartLoadFrac)) {
                    MaxHotWaterFlow = MaxCoilFluidFlow * PartLoadFrac;
                    SetComponentFlowRate(state, MaxHotWaterFlow, CoilControlNode, CoilOutletNode, plantLoc);
                    SimulateWaterCoilComponents(state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode);
                } else {
                    MaxHotWaterFlow = MaxCoilFluidFlow;
                    SetComponentFlowRate(state, MaxHotWaterFlow, CoilControlNode, CoilOutletNode, plantLoc);
                    SimulateWaterCoilComponents(state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode);
                    if (QCoilActual > (HeatingLoad + SmallLoad)) {
                        // control water flow to obtain output matching HeatingLoad
                        SolFlag = 0;
                        MinWaterFlow = 0.0;
                        Par(1) = double(MSHeatPumpNum);
                        if (FirstHVACIteration) {
                            Par(2) = 1.0;
                        } else {
                            Par(2) = 0.0;
                        }
                        Par(3) = HeatingLoad;
                        SolveRoot(state, ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par);
                        if (SolFlag == -1) {
                            if (MSHeatPump(MSHeatPumpNum).HotWaterCoilMaxIterIndex == 0) {
                                ShowWarningMessage(state,
                                                   "CalcNonDXHeatingCoils: Hot water coil control failed for " + std::string{CurrentModuleObject} +
                                                       "=\"" + MSHeatPump(MSHeatPumpNum).Name + "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("  Iteration limit [{}] exceeded in calculating hot water mass flow rate", SolveMaxIter));
                            }
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [{}]) for {}=\"{}",
                                       SolveMaxIter,
                                       CurrentModuleObject,
                                       MSHeatPump(MSHeatPumpNum).Name),
                                MSHeatPump(MSHeatPumpNum).HotWaterCoilMaxIterIndex);
                        } else if (SolFlag == -2) {
                            if (MSHeatPump(MSHeatPumpNum).HotWaterCoilMaxIterIndex2 == 0) {
                                ShowWarningMessage(state,
                                                   "CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " +
                                                       std::string{CurrentModuleObject} + "=\"" + MSHeatPump(MSHeatPumpNum).Name + "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state, "...Bad hot water maximum flow rate limits");
                                ShowContinueError(state, format("...Given minimum water flow rate={:.3R} kg/s", MinWaterFlow));
                                ShowContinueError(state, format("...Given maximum water flow rate={:.3R} kg/s", MaxHotWaterFlow));
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " +
                                                               std::string{CurrentModuleObject} + "=\"" + MSHeatPump(MSHeatPumpNum).Name + "\"",
                                                           MSHeatPump(MSHeatPumpNum).HotWaterCoilMaxIterIndex2,
                                                           MaxHotWaterFlow,
                                                           MinWaterFlow,
                                                           _,
                                                           "[kg/s]",
                                                           "[kg/s]");
                        }
                        // simulate hot water supplemental heating coil
                        SimulateWaterCoilComponents(
                            state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode);
                    }
                }
            } break;
            case Coil_HeatingSteam: {
                if (present(PartLoadFrac)) {
                    mdot = MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow * PartLoadFrac;
                    SteamCoilHeatingLoad = HeatingLoad * PartLoadFrac;
                } else {
                    mdot = MSHeatPump(MSHeatPumpNum).MaxCoilFluidFlow;
                    SteamCoilHeatingLoad = HeatingLoad;
                }
                SetComponentFlowRate(state, mdot, CoilControlNode, CoilOutletNode, plantLoc);
                // simulate steam supplemental heating coil
                SimulateSteamCoilComponents(
                    state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, SteamCoilHeatingLoad, QCoilActual, FanMode);
            } break;
            default:
                break;
            }

        } else { // end of IF (HeatingLoad > SmallLoad) THEN

            switch (HeatCoilType) {
            case SuppHeatingCoilGas:
            case SuppHeatingCoilElec: {
                SimulateHeatingCoilComponents(
                    state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatingLoad, HeatCoilNum, QCoilActual, true, FanMode);
            } break;
            case Coil_HeatingWater: {
                mdot = 0.0;
                SetComponentFlowRate(state, mdot, CoilControlNode, CoilOutletNode, plantLoc);
                SimulateWaterCoilComponents(state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode);
            } break;
            case Coil_HeatingSteam: {
                mdot = 0.0;
                SetComponentFlowRate(state, mdot, CoilControlNode, CoilOutletNode, plantLoc);
                // simulate the steam supplemental heating coil
                SimulateSteamCoilComponents(
                    state, state.dataHVACMultiSpdHP->HeatCoilName, FirstHVACIteration, HeatCoilNum, HeatingLoad, QCoilActual, FanMode);
            } break;
            default:
                break;
            }
        }
        HeatCoilLoadmet = QCoilActual;
    }

    Real64 HotWaterCoilResidual(EnergyPlusData &state,
                                Real64 const HWFlow,       // hot water flow rate in kg/s
                                Array1D<Real64> const &Par // Par(5) is the requested coil load
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   November 2011
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (QCoilActual - SupHeatCoilLoad) / SupHeatCoilLoad
        // coil actual output depends on the hot water flow rate which is varied to minimize the
        // residual.

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
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int MSHeatPumpNum;
        bool FirstHVACSoln;
        Real64 QCoilActual;  // delivered coild load, W
        Real64 HeatCoilLoad; // requested coild load, W
        Real64 mdot;

        MSHeatPumpNum = int(Par(1));
        FirstHVACSoln = (Par(2) > 0.0);
        HeatCoilLoad = Par(3);
        QCoilActual = HeatCoilLoad;
        mdot = HWFlow;
        SetComponentFlowRate(state,
                             mdot,
                             state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HotWaterCoilControlNode,
                             state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HotWaterCoilOutletNode,
                             state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HotWaterPlantLoc);
        // simulate the hot water supplemental heating coil
        SimulateWaterCoilComponents(state,
                                    state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HotWaterCoilName,
                                    FirstHVACSoln,
                                    state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).HotWaterCoilNum,
                                    QCoilActual,
                                    state.dataHVACMultiSpdHP->MSHeatPump(MSHeatPumpNum).OpMode);
        if (HeatCoilLoad != 0.0) {
            Residuum = (QCoilActual - HeatCoilLoad) / HeatCoilLoad;
        } else { // Autodesk:Return Condition added to assure return value is set
            Residuum = 0.0;
        }
        return Residuum;
    }

} // namespace HVACMultiSpeedHeatPump

} // namespace EnergyPlus
