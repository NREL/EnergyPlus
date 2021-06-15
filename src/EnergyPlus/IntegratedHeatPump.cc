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
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterThermalTanks.hh>

namespace EnergyPlus::IntegratedHeatPump {

// Using/Aliasing
using namespace DataLoopNode;

void SimIHP(EnergyPlusData &state,
            std::string_view CompName,   // Coil Name
            int &CompIndex,                // Index for Component name
            int const CyclingScheme,       // Continuous fan OR cycling compressor
            Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
            Real64 &HPTimeConstant,        // Heat pump time constant [s]
            Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
            int const CompOp,              // compressor on/off. 0 = off; 1= on
            Real64 const PartLoadFrac,     // part load fraction
            int const SpeedNum,            // compressor speed number
            Real64 const SpeedRatio,       // compressor speed ratio
            Real64 const SensLoad,         // Sensible demand load [W]
            Real64 const LatentLoad,       // Latent demand load [W]
            bool const IsCallbyWH,         // whether the call from the water heating loop or air loop, true = from water heating loop
            [[maybe_unused]] bool const FirstHVACIteration, // TRUE if First iteration of simulation
            Optional<Real64 const> OnOffAirFlowRat          // ratio of comp on to comp off air flow rate
)
{

    //       AUTHOR         Bo Shen, ORNL
    //       DATE WRITTEN   March 2016
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages variable-speed integrated Air source heat pump simulation.

    // Using/Aliasing
    using VariableSpeedCoils::InitVarSpeedCoil;
    using VariableSpeedCoils::SimVariableSpeedCoils;
    using VariableSpeedCoils::UpdateVarSpeedCoil;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int DXCoilNum(0); // The IHP No that you are currently dealing with
    Real64 waterMassFlowRate(0);
    Real64 airMassFlowRate(0);

    // Obtains and Allocates ASIHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (CompIndex == 0) {
        DXCoilNum = UtilityRoutines::FindItemInList(CompName, state.dataIntegratedHP->IntegratedHeatPumps);
        if (DXCoilNum == 0) {
            ShowFatalError(state, "Integrated Heat Pump not found=" + std::string{CompName});
        }
        CompIndex = DXCoilNum;
    } else {
        DXCoilNum = CompIndex;
        if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("SimIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name={}",
                                  DXCoilNum,
                                  state.dataIntegratedHP->IntegratedHeatPumps.size(),
                                  CompName));
        }
        if (!CompName.empty() && CompName != state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name) {
            ShowFatalError(state,
                           format("SimIHP: Invalid CompIndex passed={}, Integrated HP name={}, stored Integrated HP Name for that index={}",
                                  DXCoilNum,
                                  CompName,
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name));
        }
    };

    if (!state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

    InitializeIHP(state, DXCoilNum);

    airMassFlowRate = state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRate;
    waterMassFlowRate = state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = airMassFlowRate;

    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::SCMode:
        if (!IsCallbyWH) // process when called from air loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
        break;
    case IHPOperationMode::SHMode:
        if (!IsCallbyWH) // process when called from air loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
        }
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
        break;
    case IHPOperationMode::DWHMode:
        if (IsCallbyWH) // process when called from water loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);
            // IntegratedHeatPumps(DXCoilNum).TotalHeatingEnergyRate =
            // VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).TotalHeatingEnergyRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        break;
    case IHPOperationMode::SCWHMatchSCMode:
        if (!IsCallbyWH) // process when called from air loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;

        break;
    case IHPOperationMode::SCWHMatchWHMode:
        if (IsCallbyWH) // process when called from water loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = airMassFlowRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        break;
    case IHPOperationMode::SCDWHMode:
        if (!IsCallbyWH) // process when called from air loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        if (!IsCallbyWH) // process when called from air loop
        {
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  0.0,
                                  1,
                                  0.0,
                                  0.0,
                                  0.0,
                                  OnOffAirFlowRat);

            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);
            SimVariableSpeedCoils(state,
                                  std::string(),
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                                  CyclingScheme,
                                  MaxONOFFCyclesperHour,
                                  HPTimeConstant,
                                  FanDelayTime,
                                  CompOp,
                                  PartLoadFrac,
                                  SpeedNum,
                                  SpeedRatio,
                                  SensLoad,
                                  LatentLoad,
                                  OnOffAirFlowRat);

            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        break;
    case IHPOperationMode::IdleMode:
    default: // clear up
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        SimVariableSpeedCoils(state,
                              std::string(),
                              state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                              CyclingScheme,
                              MaxONOFFCyclesperHour,
                              HPTimeConstant,
                              FanDelayTime,
                              CompOp,
                              0.0,
                              1,
                              0.0,
                              0.0,
                              0.0,
                              OnOffAirFlowRat);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = 0.0;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = 0.0;
        break;
    }

    UpdateIHP(state, DXCoilNum);
}

void GetIHPInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   December, 2015
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for Integrated HPs and stores it in IHP data structures

    // METHODOLOGY EMPLOYED:
    // Uses "Get" routines to read in data.

    // Using/Aliasing
    using namespace NodeInputManager;
    using BranchNodeConnections::OverrideNodeConnectionType;
    using BranchNodeConnections::RegisterNodeConnection;
    using BranchNodeConnections::SetUpCompSets;
    using BranchNodeConnections::TestCompSet;
    using GlobalNames::VerifyUniqueCoilName;
    using VariableSpeedCoils::GetCoilIndexVariableSpeed;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetIHPInput: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int DXCoilNum; // No of IHP DX system
    int NumASIHPs; // Counter for air-source integrated heat pumps

    int NumAlphas;           // Number of variables in String format
    int NumNums;             // Number of variables in Numeric format
    int NumParams;           // Total number of input fields
    int MaxNums(0);          // Maximum number of numeric input fields
    int MaxAlphas(0);        // Maximum number of alpha input fields
    std::string CoilName;    // Name of the  Coil
    std::string Coiltype;    // type of coil
    std::string InNodeName;  // Name of coil inlet node
    std::string OutNodeName; // Name of coil outlet node

    std::string CurrentModuleObject; // for ease in getting objects
    std::string sIHPType;            // specify IHP type
    Array1D_string AlphArray;        // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D<Real64> NumArray;        // Numeric input items for object
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

    bool ErrorsFound(false); // If errors detected in input
    int CoilCounter;         // Counter

    int IOStat;
    int AlfaFieldIncre; // increment number of Alfa field

    bool IsNotOK; // Flag to verify name
    bool errFlag;
    int InNode(0);         // inlet air or water node
    int OutNode(0);        // outlet air or water node
    int ChildCoilIndex(0); // refer to a child coil

    NumASIHPs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE");
    DXCoilNum = 0;

    if (NumASIHPs <= 0) return;

    // Allocate Arrays
    state.dataIntegratedHP->IntegratedHeatPumps.allocate(NumASIHPs);

    // air-source integrated heat pump
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", NumParams, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    AlphArray.allocate(MaxAlphas);
    cAlphaFields.allocate(MaxAlphas);
    lAlphaBlanks.dimension(MaxAlphas, true);
    cNumericFields.allocate(MaxNums);
    lNumericBlanks.dimension(MaxNums, true);
    NumArray.dimension(MaxNums, 0.0);

    // Get the data for air-source IHPs
    CurrentModuleObject = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE"; // for reporting
    sIHPType = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE";            // for checking

    for (CoilCounter = 1; CoilCounter <= NumASIHPs; ++CoilCounter) {

        ++DXCoilNum;
        AlfaFieldIncre = 1;

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 CoilCounter,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name = AlphArray(1);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPtype = "AIRSOURCE_IHP";

        // AlphArray( 2 ) is the water sensor node

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName = AlphArray(3);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName = AlphArray(4);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName = AlphArray(5);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName = AlphArray(6);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType = "COIL:COOLING:DX:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName = AlphArray(7);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName = AlphArray(8);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).bIsDesuperheater =
                    true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType = "COIL:HEATING:DX:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName = AlphArray(9);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName = AlphArray(10);
        Coiltype = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType;
        CoilName = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName;

        ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
        if (IsNotOK) {
            ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
            ErrorsFound = true;
        } else {
            errFlag = false;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).bIsDesuperheater =
                    true;
            }
        }

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow = NumArray(1);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow = NumArray(2);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TindoorWHHighPriority = NumArray(3);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TambientWHHighPriority = NumArray(4);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ModeMatchSCWH = int(NumArray(5));
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSCWH = int(NumArray(6));
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterVolSCDWH = NumArray(7);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSCDWH = int(NumArray(8));
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TimeLimitSHDWH = NumArray(9);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSHDWH = int(NumArray(10));

        // Due to the overlapping coil objects, compsets and node registrations are handled as follows:
        //  1. The ASIHP coil object is registered as four different coils, Name+" Cooling Coil", Name+" Heating Coil",
        //     Name+" Outdoor Coil", and Name+" Water Coil"
        //  2. For each of these four coils, TestCompSet is called once to register it as a child object
        //  3. For each of these four coils, RegisterNodeConnection is called twice to register the inlet and outlet nodes
        //     RegisterNodeConnection is used instead of GetOnlySingleNode because the node names are not inputs here
        //  4. The parent objects that reference the ASIHP coil must use the appropriate name suffixes when calling SetUpCompSets
        //  5. The ASIHP calls SetUpCompSets to register the various child coils.  This is important so that the system energy
        //     use is collected in SystemReports::CalcSystemEnergyUse
        //  6. The child coil inlet/outlet node connections are reset to connection type "Internal" to avoid duplicate node problems
        //     using OverrideNodeConnectionType

        // cooling coil air node connections
        ChildCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
        OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
        InNodeName = state.dataLoopNodes->NodeID(InNode);
        OutNodeName = state.dataLoopNodes->NodeID(OutNode);

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = InNode;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = OutNode;

        TestCompSet(state,
                    CurrentModuleObject,
                    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                    InNodeName,
                    OutNodeName,
                    "Cooling Air Nodes");
        RegisterNodeConnection(state,
                               InNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                               "Inlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);
        RegisterNodeConnection(state,
                               OutNode,
                               state.dataLoopNodes->NodeID(OutNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                               "Outlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        // heating coil air node connections
        ChildCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex;

        InNode = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum;
        OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum = OutNode;
        InNodeName = state.dataLoopNodes->NodeID(InNode);
        OutNodeName = state.dataLoopNodes->NodeID(OutNode);
        if (state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum != InNode) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject + "- cooling coil outlet mismatches heating coil inlet" + ".");
            ErrorsFound = true;
        }
        TestCompSet(state,
                    CurrentModuleObject,
                    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                    InNodeName,
                    OutNodeName,
                    "Heating Air Nodes");
        RegisterNodeConnection(state,
                               InNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                               "Inlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);
        RegisterNodeConnection(state,
                               OutNode,
                               state.dataLoopNodes->NodeID(OutNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                               "Outlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        // water node connections
        ChildCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;

        InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterInletNodeNum;
        OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterOutletNodeNum;
        InNodeName = state.dataLoopNodes->NodeID(InNode);
        OutNodeName = state.dataLoopNodes->NodeID(OutNode);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum = InNode;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum = OutNode;
        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken water node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }

        TestCompSet(state,
                    CurrentModuleObject,
                    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                    InNodeName,
                    OutNodeName,
                    "Water Nodes");
        RegisterNodeConnection(state,
                               InNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                               "Inlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);
        RegisterNodeConnection(state,
                               OutNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                               "Outlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken water node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken water node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                   "Internal",
                                   2,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterTankoutNod = GetOnlySingleNode(state,
                                                                                                   AlphArray(2),
                                                                                                   ErrorsFound,
                                                                                                   CurrentModuleObject,
                                                                                                   AlphArray(1),
                                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                                   DataLoopNode::NodeConnectionType::Sensor,
                                                                                                   2,
                                                                                                   ObjectIsNotParent);

        // outdoor air node connections for water heating coils
        // DWH, SCDWH, SHDWH coils have the same outdoor air nodes
        ChildCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
        InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
        OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
        InNodeName = state.dataLoopNodes->NodeID(InNode);
        OutNodeName = state.dataLoopNodes->NodeID(OutNode);
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ODAirInletNodeNum = InNode;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ODAirOutletNodeNum = OutNode;
        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }

        TestCompSet(state,
                    CurrentModuleObject,
                    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                    InNodeName,
                    OutNodeName,
                    "Outdoor Air Nodes");
        RegisterNodeConnection(state,
                               InNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                               "Inlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);
        RegisterNodeConnection(state,
                               OutNode,
                               state.dataLoopNodes->NodeID(InNode),
                               CurrentModuleObject,
                               state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                               "Outlet",
                               1,
                               ObjectIsNotParent,
                               ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum = InNode;
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum =
            OutNode;
        if ((state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum !=
             InNode) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum !=
             OutNode)) {
            ShowContinueError(state,
                              "Mistaken air node connection: " + CurrentModuleObject +
                                  state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName + "-wrong coil node names.");
            ErrorsFound = true;
        }
        SetUpCompSets(state,
                      CurrentModuleObject,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                      state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                      InNodeName,
                      OutNodeName);
        OverrideNodeConnectionType(state,
                                   InNode,
                                   InNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
        OverrideNodeConnectionType(state,
                                   OutNode,
                                   OutNodeName,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                   state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                   "Internal",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = false;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow = 1e10;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow = 1e10;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow = 1e10;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow = 1e10;
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination.");
    } else {
        // set up output variables, not reported in the individual coil models

        //                TODO: Figure out how to get enum class to work with SetupOutputVariable
        //                Setup Output Variable( "Operation Mode []",
        //                                     static_cast< int >( IntegratedHeatPumps( DXCoilNum ).CurMode ),
        //                                     "System", "Average",
        //                                     IntegratedHeatPumps( DXCoilNum ).Name );
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Loop Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Condenser Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Water Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Source Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump COP",
                            OutputProcessor::Unit::None,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCOP,
                            "System",
                            "Average",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Energy,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Water Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Air Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLatent,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
        SetupOutputVariable(state,
                            "Integrated Heat Pump Source Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergySource,
                            "System",
                            "Summed",
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Name);
    }
}

void SizeIHP(EnergyPlusData &state, int const DXCoilNum)
{
    using DataSizing::AutoSize;
    using VariableSpeedCoils::SetVarSpeedCoilData;
    using VariableSpeedCoils::SimVariableSpeedCoils;
    using VariableSpeedCoils::SizeVarSpeedCoil;

    bool ErrorsFound(false);   // If errors detected in input
    Real64 RatedCapacity(0.0); // rated building cooling load

    // Obtains and Allocates AS-IHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    };

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("SizeIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) {
        return;
    }

    // associate SC coil with SH coil
    SetVarSpeedCoilData(state,
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                        ErrorsFound,
                        _,
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(state,
                        "SizeIHP: Could not match cooling coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName +
                            "\" with heating coil=\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
        ErrorsFound = false;
    };

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex); // size cooling coil
    if (ErrorsFound) {
        ShowFatalError(state, "SizeIHP: failed to size SC coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilName + "\"");
        ErrorsFound = false;
    } else {
        RatedCapacity =
            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex).RatedCapCoolTotal;
    };

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex); // size heating coil
    if (ErrorsFound) {
        ShowSevereError(state, "SizeIHP: failed to size SH coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
        ErrorsFound = false;
    };

    // pass SC coil capacity to SCDWH cool coil
    if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal ==
        AutoSize) {
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal =
            RatedCapacity;
    };

    // associate SCDWH air coil to SHDWH air coil
    SetVarSpeedCoilData(state,
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                        ErrorsFound,
                        _,
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex);

    // size SCDWH air coil
    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(
            state, "SizeIHP: failed to size SCDWH cooling coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName + "\"");
        ErrorsFound = false;
    };

    // size SHDWH air coil
    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(
            state, "SizeIHP: failed to size SHDWH heating coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName + "\"");
        ErrorsFound = false;
    };

    // size the water coils below
    // size SCWH water coil
    if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH == AutoSize) {
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH =
            RatedCapacity /
            (1.0 -
             1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCOPHeat);
    }

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(state, "SizeIHP: failed to size SCWH coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilName + "\"");
        ErrorsFound = false;
    };

    // size DWH water coil
    if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH == AutoSize) {
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH = RatedCapacity;
    }

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(state, "SizeIHP: failed to size DWH coil\"" + state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilName + "\"");
        ErrorsFound = false;
    };

    // size SCDWH water coil
    if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH == AutoSize) {
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH =
            RatedCapacity * 0.13;
    }

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex);
    if (ErrorsFound) {
        ShowSevereError(state,
                        "SizeIHP: failed to size SCDWH water heating coil\"" +
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName + "\"");
        ErrorsFound = false;
    };

    // size SHDWH water coil
    if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH == AutoSize) {
        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH =
            RatedCapacity * 0.1;
    }

    SizeVarSpeedCoil(state, state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex);

    if (ErrorsFound) {
        ShowSevereError(state,
                        "SizeIHP: failed to size SHDWH water heating coil\"" +
                            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName + "\"");
        ErrorsFound = false;
    };

    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = true;
}

void InitializeIHP(EnergyPlusData &state, int const DXCoilNum)
{
    // Obtains and Allocates AS-IHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("InitializeIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = 0.0;             // air loop mass flow rate [kg/s]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0; // water loop mass flow rate [kg/s]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;            // total cooling rate [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;       // total water heating rate [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;       // total space heating rate [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower = 0.0;                  // total power consumption  [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;             // total latent cooling rate [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource = 0.0;                     // source energy rate, [w]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Energy = 0.0;                      // total electric energy consumption [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling = 0.0;      // total cooling energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating = 0.0;      // total heating energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating = 0.0; // total heating energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLatent = 0.0;                // total latent energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergySource = 0.0;                // total source energy
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCOP = 0.0;
}

void UpdateIHP(EnergyPlusData &state, int const DXCoilNum)
{
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    int VSCoilIndex(0);
    Real64 ReportingConstant(0.0);
    Real64 TotalDelivery(0.0);

    // Obtains and Allocates AS-IHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("UpdateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::SCMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal;             // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0; // total water heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power; // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent; // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // source energy rate, [w]
        break;
    case IHPOperationMode::SHMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;      // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0; // total water heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0; // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // source energy rate, [w]
        break;
    case IHPOperationMode::DWHMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0; // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;                // total water heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0; // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // source energy rate, [w]
        break;
    case IHPOperationMode::SCWHMatchSCMode:
    case IHPOperationMode::SCWHMatchWHMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;                // total water heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power; // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent;  // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource = 0.0; // source energy rate, [w]
        break;
    case IHPOperationMode::SCDWHMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal;             // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power; // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent; // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // source energy rate, [w]

        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0; // total cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0; // total latent cooling rate [w]
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // source energy rate, [w]

        VSCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

        break;
    case IHPOperationMode::IdleMode:
    default:
        break;
    }

    ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Energy =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower * ReportingConstant; // total electric energy consumption
                                                                                               // [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate * ReportingConstant; // total cooling energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate * ReportingConstant; // total heating energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate * ReportingConstant; // total heating energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergyLatent =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalLatentLoad * ReportingConstant; // total latent energy [J]
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).EnergySource =
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).Qsource * ReportingConstant; // total source energy

    if (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower > 0.0) {
        TotalDelivery = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCoolingRate +
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate +
                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate;
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalCOP =
            TotalDelivery / state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TotalPower;
    }
}

void DecideWorkMode(EnergyPlusData &state,
                    int const DXCoilNum,
                    Real64 const SensLoad,  // Sensible demand load [W]
                    Real64 const LatentLoad // Latent demand load [W]
                    )                       // shall be called from a air loop parent
{
    //       AUTHOR         Bo Shen, ORNL
    //       DATE WRITTEN   March 2016
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine determine the IHP working mode in the next time step,
    // it should be called by an air loop parent object, when FirstHVACIteration == true

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using WaterThermalTanks::GetWaterThermalTankInput;

    Real64 MyLoad(0.0);
    Real64 WHHeatTimeSav(0.0); // time accumulation for water heating
    Real64 WHHeatVolSave(0.0); // volume accumulation for water heating

    // Obtains and Allocates AS-IHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("DecideWorkMode: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(state, DXCoilNum);

    // decide working mode at the first moment
    // check if there is a water heating call
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CheckWHCall = true; // set checking flag
    if (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankID == 0)  // not initialized yet
    {
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
    } else {
        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
            GetWaterVolFlowRateIHP(state, DXCoilNum, 1.0, 1.0, true) * 987.0; // 987.0 water density at 60 C.
        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum).Temp =
            state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp;

        int tankType = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankType;

        if ((tankType == DataPlant::TypeOf_WtrHeaterMixed) || (tankType == DataPlant::TypeOf_WtrHeaterStratified) ||
            (tankType == DataPlant::TypeOf_ChilledWaterTankMixed) || (tankType == DataPlant::TypeOf_ChilledWaterTankStratified)) {

            int tankIDX = WaterThermalTanks::getTankIDX(state,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankName,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankID);
            auto &tank = state.dataWaterThermalTanks->WaterThermalTank(tankIDX);
            tank.callerLoopNum = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).LoopNum;

            PlantLocation A(0, 0, 0, 0);
            tank.simulate(state, A, true, MyLoad, true);

            tank.callerLoopNum = 0;

        } else if (tankType == DataPlant::TypeOf_HeatPumpWtrHeaterPumped || tankType == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {

            int hpIDX = WaterThermalTanks::getHPTankIDX(state,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankName,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankID);
            auto &HPWH = state.dataWaterThermalTanks->HPWaterHeater(hpIDX);
            int tankIDX = HPWH.WaterHeaterTankNum;
            auto &tank = state.dataWaterThermalTanks->WaterThermalTank(tankIDX);
            tank.callerLoopNum = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).LoopNum;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WHtankType = tankType;

            PlantLocation A(0, 0, 0, 0);
            HPWH.simulate(state, A, true, MyLoad, true);

            tank.callerLoopNum = 0;
        }
    }
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CheckWHCall = false; // clear checking flag

    // keep the water heating time and volume history
    WHHeatTimeSav = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHRunTime;
    if (IHPOperationMode::SCDWHMode == state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
        WHHeatVolSave = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol +
                        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterTankoutNod).MassFlowRate / 983.0 *
                            TimeStepSys * DataGlobalConstants::SecInHour; // 983 - water density at 60 C
    } else {
        WHHeatVolSave = 0.0;
    }

    // clear the accumulation amount for other modes
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = 0.0;
    state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = 0.0;

    if (!state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IsWHCallAvail) // no water heating call
    {
        if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // space cooling mode
        {
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCMode;
        } else if (SensLoad > SmallLoad) {
            if ((state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp >
                 state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow) &&
                (state.dataEnvrn->OutDryBulbTemp > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum)
                                                       .TambientOverCoolAllow)) // used for cooling season, avoid heating after SCWH mode
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            else
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHMode;
        } else {
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
        }
    }
    // below has water heating calls
    else if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // simultaneous SC and WH calls
    {
        if (WHHeatVolSave < state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterVolSCDWH) // small water heating amount
        {
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCDWHMode;
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = WHHeatVolSave;
        } else {
            if (1 == state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ModeMatchSCWH) // water heating priority
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
            else // space cooling priority
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchSCMode;
        };

    } else if ((state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp >
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow) &&
               (state.dataEnvrn->OutDryBulbTemp >
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow)) // over-cooling allowed, water heating priority
    {
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
    } else if ((state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp >
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TindoorWHHighPriority) &&
               (state.dataEnvrn->OutDryBulbTemp >
                state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TambientWHHighPriority)) // ignore space heating request
    {
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
    } else if (SensLoad > SmallLoad) {
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = WHHeatTimeSav + TimeStepSys * DataGlobalConstants::SecInHour;

        if (WHHeatTimeSav > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).TimeLimitSHDWH) {
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOnMode;
        } else {
            state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOffMode;
        };
    } else {
        state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
    }

    // clear up, important
    ClearCoils(state, DXCoilNum);
}

void ClearCoils(EnergyPlusData &state, int const DXCoilNum)
{
    using VariableSpeedCoils::SimVariableSpeedCoils;

    Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling clear up function
    int CycFanCycCoil(1);                   // fan cycl manner place holder

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("ClearCoils: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    // clear up
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);
    SimVariableSpeedCoils(state,
                          std::string(),
                          state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
                          CycFanCycCoil,
                          EMP1,
                          EMP2,
                          EMP3,
                          1,
                          0.0,
                          1.0,
                          0.0,
                          0.0,
                          0.0,
                          1.0);

    return;
}

IHPOperationMode GetCurWorkMode(EnergyPlusData &state, int const DXCoilNum)
{
    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetCurWorkMode: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(state, DXCoilNum);

    return (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode);
}

bool IHPInModel(EnergyPlusData &state)
{
    if (state.dataIntegratedHP->GetCoilsInputFlag) {
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }
    return !state.dataIntegratedHP->IntegratedHeatPumps.empty();
}

int GetCoilIndexIHP(EnergyPlusData &state,
                    std::string const &CoilType, // must match coil types in this module
                    std::string const &CoilName, // must match coil names for the coil type
                    bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   March 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the coil index for the given coil and returns it.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
    // as zero.

    // Return value
    int IndexNum; // returned index of matched coil

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);

    if (IndexNum == 0) {
        ShowSevereError(state, "GetCoilIndexIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
    }

    return IndexNum;
}

int GetCoilInletNodeIHP(EnergyPlusData &state,
                        std::string const &CoilType, // must match coil types in this module
                        std::string const &CoilName, // must match coil names for the coil type
                        bool &ErrorsFound            // set to true if problem
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   March 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the inlet node.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int NodeNumber(0); // returned outlet node of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);
    if (WhichCoil != 0) {
        NodeNumber = state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).AirCoolInletNodeNum;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetDWHCoilInletNodeIHP(EnergyPlusData &state,
                           std::string const &CoilType, // must match coil types in this module
                           std::string const &CoilName, // must match coil names for the coil type
                           bool &ErrorsFound            // set to true if problem
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   July 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the inlet node.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int NodeNumber(0); // returned outlet node of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);
    if (WhichCoil != 0) {
        NodeNumber = state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).ODAirInletNodeNum;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetDWHCoilOutletNodeIHP(EnergyPlusData &state,
                            std::string const &CoilType, // must match coil types in this module
                            std::string const &CoilName, // must match coil names for the coil type
                            bool &ErrorsFound            // set to true if problem
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   July 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the outlet node.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Return value
    int NodeNumber(0); // returned outlet node of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);
    if (WhichCoil != 0) {
        NodeNumber = state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).ODAirOutletNodeNum;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetIHPDWHCoilPLFFPLR(EnergyPlusData &state,
                         std::string const &CoilType,                  // must match coil types in this module
                         std::string const &CoilName,                  // must match coil names for the coil type
                         [[maybe_unused]] IHPOperationMode const Mode, // mode coil type
                         bool &ErrorsFound                             // set to true if problem
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   March, 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns PLR curve index.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
    // as zero.

    // Using/Aliasing
    using VariableSpeedCoils::GetVSCoilPLFFPLR;

    // Return value
    int PLRNumber(0); // returned outlet node of matched coil

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    int WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);
    if (WhichCoil != 0) {
        // this will be called by HPWH parent
        if (state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0)
            PLRNumber = GetVSCoilPLFFPLR(state,
                                         state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilType,
                                         state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilName,
                                         ErrorsFound);
        else
            PLRNumber = GetVSCoilPLFFPLR(state,
                                         state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).SCWHCoilType,
                                         state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).SCWHCoilName,
                                         ErrorsFound);
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetIHPDWHCoilPLFFPLR: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        PLRNumber = 0;
    }

    return PLRNumber;
}

Real64 GetDWHCoilCapacityIHP(EnergyPlusData &state,
                             std::string const &CoilType,                  // must match coil types in this module
                             std::string const &CoilName,                  // must match coil names for the coil type
                             [[maybe_unused]] IHPOperationMode const Mode, // mode coil type
                             bool &ErrorsFound                             // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bo Shen
    //       DATE WRITTEN   Jan 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
    // as negative.

    // Using/Aliasing
    using VariableSpeedCoils::GetCoilCapacityVariableSpeed;

    // Return value
    Real64 CoilCapacity; // returned capacity of matched coil

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    int WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataIntegratedHP->IntegratedHeatPumps);
    if (WhichCoil != 0) {

        if (state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).IHPCoilsSized == false) SizeIHP(state, WhichCoil);

        if (state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0) {
            CoilCapacity = GetCoilCapacityVariableSpeed(state,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilType,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).DWHCoilName,
                                                        ErrorsFound);
        } else {
            CoilCapacity = GetCoilCapacityVariableSpeed(state,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).SCWHCoilType,
                                                        state.dataIntegratedHP->IntegratedHeatPumps(WhichCoil).SCWHCoilName,
                                                        ErrorsFound);
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilCapacityVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        CoilCapacity = -1000.0;
    }

    return CoilCapacity;
}

int GetLowSpeedNumIHP(EnergyPlusData &state, int const DXCoilNum)
{
    int SpeedNum(0);

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetLowSpeedNumIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::IdleMode:
        SpeedNum = 1;
        break;
    case IHPOperationMode::SCMode:
        SpeedNum = 1;
        break;
    case IHPOperationMode::SHMode:
        SpeedNum = 1;
        break;
    case IHPOperationMode::DWHMode:
        SpeedNum = 1;
        break;
    case IHPOperationMode::SCWHMatchSCMode:
    case IHPOperationMode::SCWHMatchWHMode:
        SpeedNum = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSCWH;
        break;
    case IHPOperationMode::SCDWHMode:
        SpeedNum = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSCDWH;
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        SpeedNum = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MinSpedSHDWH;
        break;
    default:
        SpeedNum = 1;
        break;
    }

    return (SpeedNum);
}

int GetMaxSpeedNumIHP(EnergyPlusData &state, int const DXCoilNum)
{
    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetMaxSpeedNumIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    int SpeedNum(0);

    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::IdleMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::SCMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::SHMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::DWHMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::SCWHMatchSCMode:
    case IHPOperationMode::SCWHMatchWHMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::SCDWHMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).NumOfSpeeds;
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).NumOfSpeeds;
        break;
    default:
        SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
        break;
    }

    return (SpeedNum);
}

Real64 GetAirVolFlowRateIHP(EnergyPlusData &state,
                            int const DXCoilNum,
                            int const SpeedNum,
                            Real64 const SpeedRatio,
                            bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
)
{
    int IHPCoilIndex(0);
    Real64 AirVolFlowRate(0.0);
    Real64 FlowScale(1.0);
    bool IsResultFlow(false); // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetAirVolFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (!state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

    FlowScale = 0.0;
    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::IdleMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        break;
    case IHPOperationMode::SCMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        if (!IsCallbyWH) // call from air loop
        {
            FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        }

        break;
    case IHPOperationMode::SHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
        if (!IsCallbyWH) // call from air loop
        {
            FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
        }
        break;
    case IHPOperationMode::DWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
        FlowScale = 1.0;
        break;
    case IHPOperationMode::SCWHMatchSCMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        if (IsCallbyWH) // call from water loop
        {
            IsResultFlow = true;
            AirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
        }
        break;
    case IHPOperationMode::SCWHMatchWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        if (!IsCallbyWH) {
            IsResultFlow = true;
            AirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
        }
        break;
    case IHPOperationMode::SCDWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        if (IsCallbyWH) {
            IsResultFlow = true;
            AirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirVolFlowRate;
        }
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
        if (IsCallbyWH) {
            IsResultFlow = true;
            AirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirVolFlowRate;
        }
        break;
    default:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        FlowScale = 0.0;
        break;
    }

    if (!IsResultFlow) {
        if (1 == SpeedNum)
            AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum);
        else
            AirVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum) +
                             (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum - 1);

        AirVolFlowRate = AirVolFlowRate * FlowScale;
    }

    if (AirVolFlowRate > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow)
        AirVolFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow;
    if (AirVolFlowRate > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow)
        AirVolFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow;

    return (AirVolFlowRate);
}

Real64 GetWaterVolFlowRateIHP(
    EnergyPlusData &state,
    int const DXCoilNum,
    int const SpeedNum,
    Real64 const SpeedRatio,
    [[maybe_unused]] bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
)
{
    int IHPCoilIndex(0);
    Real64 WaterVolFlowRate(0.0);

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetWaterVolFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (!state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::IdleMode:
        WaterVolFlowRate = 0.0;
        break;
    case IHPOperationMode::SCMode:
        WaterVolFlowRate = 0.0;
        break;
    case IHPOperationMode::SHMode:
        WaterVolFlowRate = 0.0;
        break;
    case IHPOperationMode::DWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
        if (1 == SpeedNum)
            WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
        else
            WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                               (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
        break;
    case IHPOperationMode::SCWHMatchSCMode:
    case IHPOperationMode::SCWHMatchWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        if (1 == SpeedNum)
            WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
        else
            WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                               (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
        break;
    case IHPOperationMode::SCDWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
        if (1 == SpeedNum)
            WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
        else
            WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                               (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
        if (1 == SpeedNum)
            WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
        else
            WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                               (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
        break;
    default:
        WaterVolFlowRate = 0.0;
        break;
    }

    return (WaterVolFlowRate);
}

Real64 GetAirMassFlowRateIHP(EnergyPlusData &state,
                             int const DXCoilNum,
                             int const SpeedNum,
                             Real64 const SpeedRatio,
                             bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
)
{
    int IHPCoilIndex(0);
    Real64 AirMassFlowRate(0.0);
    Real64 FlowScale(1.0);
    bool IsResultFlow(false);   // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated
    Real64 WaterDensity(986.0); // standard water density at 60 C

    // Obtains and Allocates WatertoAirHP related parameters from input file
    if (state.dataIntegratedHP->GetCoilsInputFlag) { // First time subroutine has been entered
        GetIHPInput(state);
        state.dataIntegratedHP->GetCoilsInputFlag = false;
    }

    if (DXCoilNum > static_cast<int>(state.dataIntegratedHP->IntegratedHeatPumps.size()) || DXCoilNum < 1) {
        ShowFatalError(state,
                       format("GetAirMassFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                              DXCoilNum,
                              state.dataIntegratedHP->IntegratedHeatPumps.size()));
    }

    if (!state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

    FlowScale = 0.0;
    switch (state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CurMode) {
    case IHPOperationMode::IdleMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        AirMassFlowRate = 0.0;
        break;
    case IHPOperationMode::SCMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        if (!IsCallbyWH) {
            FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        } else {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
        }
        break;
    case IHPOperationMode::SHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
        if (!IsCallbyWH) {
            FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
        } else {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
        }
        break;
    case IHPOperationMode::DWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
        FlowScale = 1.0;
        break;
    case IHPOperationMode::SCWHMatchSCMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
            GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
        if (IsCallbyWH) {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
        }
        break;
    case IHPOperationMode::SCWHMatchWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        if (!IsCallbyWH) {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop;
        }
        break;
    case IHPOperationMode::SCDWHMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
            GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
        if (IsCallbyWH) {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
        }
        break;
    case IHPOperationMode::SHDWHElecHeatOffMode:
    case IHPOperationMode::SHDWHElecHeatOnMode:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
        FlowScale = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
        state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
            GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
        if (IsCallbyWH) {
            IsResultFlow = true;
            AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
        }
        break;
    default:
        IHPCoilIndex = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
        FlowScale = 0.0;
        break;
    }

    if (!IsResultFlow) {
        if (SpeedNum == 1) {
            AirMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum);
        } else {
            AirMassFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum) +
                              (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum - 1);
        }

        AirMassFlowRate = AirMassFlowRate * FlowScale;
    }

    if (AirMassFlowRate > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow) {
        AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow;
    }
    if (AirMassFlowRate > state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow) {
        AirMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow;
    }

    // set max air flow rate
    state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRateMax = AirMassFlowRate;
    state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRateMax = AirMassFlowRate;
    state.dataLoopNodes->Node(state.dataIntegratedHP->IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum).MassFlowRateMax = AirMassFlowRate;

    return AirMassFlowRate;
}

} // namespace EnergyPlus::IntegratedHeatPump
