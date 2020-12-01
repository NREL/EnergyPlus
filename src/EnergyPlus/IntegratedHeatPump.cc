// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/IceThermalStorage.hh>
#include <EnergyPlus/CurveManager.hh>

namespace EnergyPlus {

namespace IntegratedHeatPump {

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;

    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    bool GetCoilsInputFlag(true);

    // Object Data
    Array1D<IntegratedHeatPumpData> IntegratedHeatPumps;

    void clear_state()
    {
        GetCoilsInputFlag = true;
        IntegratedHeatPumps.deallocate();
    }

    void SimIHP(EnergyPlusData &state, std::string const &CompName,              // Coil Name
                int &CompIndex,                           // Index for Component name
                int const CyclingScheme,                  // Continuous fan OR cycling compressor
                Real64 &MaxONOFFCyclesperHour,            // Maximum cycling rate of heat pump [cycles/hr]
                Real64 &HPTimeConstant,                   // Heat pump time constant [s]
                Real64 &FanDelayTime,                     // Fan delay time, time delay for the HP's fan to
                int const CompOp,                         // compressor on/off. 0 = off; 1= on
                Real64 const PartLoadFrac,                // part load fraction
                int const SpeedNum,                       // compressor speed number
                Real64 const SpeedRatio,                  // compressor speed ratio
                Real64 const SensLoad,                    // Sensible demand load [W]
                Real64 const LatentLoad,                  // Latent demand load [W]
                bool const IsCallbyWH,                    // whether the call from the water heating loop or air loop, true = from water heating loop
                bool const FirstHVACIteration, // TRUE if First iteration of simulation
                Optional<Real64 const> OnOffAirFlowRat,    // ratio of comp on to comp off air flow rate
                bool const bEnhancedDehum         // whether it requires enhanced dehumidification
    )
    {

        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   March 2016
        //       RE-ENGINEERED  Sept 2020

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages variable-speed integrated Air source heat pump simulation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum(0); // The IHP No that you are currently dealing with

        // Obtains and Allocates ASIHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (CompIndex == 0) {
            DXCoilNum = UtilityRoutines::FindItemInList(CompName, IntegratedHeatPumps);
            if (DXCoilNum == 0) {
                ShowFatalError(state, "Integrated Heat Pump not found=" + CompName);
            }
            CompIndex = DXCoilNum;
        } else {
            DXCoilNum = CompIndex;
            if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
                ShowFatalError(state,
                               format("SimIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name={}",
                                      DXCoilNum,
                                      IntegratedHeatPumps.size(),
                                      CompName));
            }
            if (!CompName.empty() && CompName != IntegratedHeatPumps(DXCoilNum).Name) {
                ShowFatalError(state,
                               format("SimIHP: Invalid CompIndex passed={}, Integrated HP name={}, stored Integrated HP Name for that index={}",
                                      DXCoilNum,
                                      CompName,
                                      IntegratedHeatPumps(DXCoilNum).Name));
            }
        };

        if (!IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

        if (IHPStorageType::LIQUIDDESICCANT == IntegratedHeatPumps(DXCoilNum).StorageType) {
            
             SimIHPLiquidDesiccantStorage(state,
                                DXCoilNum,
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
                                IsCallbyWH,
                                FirstHVACIteration,
                                OnOffAirFlowRat,
                                bEnhancedDehum 
            );
        } else {
            SimIHPWaterIceStorage(state,
                                DXCoilNum,
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
                                IsCallbyWH,
                                FirstHVACIteration,
                                OnOffAirFlowRat,
                                bEnhancedDehum);
        
        }
    }

    void SimIHPWaterIceStorage(EnergyPlusData &state,
                                 int &CompIndex,                // Index for Component name
                                 int const CyclingScheme,       // Continuous fan OR cycling compressor
                                 Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                                 Real64 &HPTimeConstant,        // Heat pump time constant [s]
                                 Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                                 int const CompOp,              // compressor on/off. 0 = off; 1= on
                                 Real64 const PartLoadFrac,
                                 int const SpeedNum,      // compressor speed number
                                 Real64 const SpeedRatio, // compressor speed ratio
                                 Real64 const SensLoad,   // Sensible demand load [W]
                                 Real64 const LatentLoad, // Latent demand load [W]
                                 bool const IsCallbyWH,   // whether the call from the water heating loop or air loop, true = from water heating loop
                                 bool const FirstHVACIteration,          // TRUE if First iteration of simulation
                                 Optional<Real64 const> OnOffAirFlowRat, // ratio of comp on to comp off air flow rate
                                 bool const bEnhancedDehum               // whether it requires enhanced dehumidification
                                 )
    {
        // Using/Aliasing
        using VariableSpeedCoils::InitVarSpeedCoil;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::UpdateVarSpeedCoil;
        using VariableSpeedCoils::IsGridResponsiveMode;

        int DXCoilNum(0); // The IHP No that you are currently dealing with
        Real64 airMassFlowRate(0); //loop air flow rate
        
        DXCoilNum = CompIndex; 

        if (IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex > 0)
            state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex).ExtOn = false;

        IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment = false;
        airMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRate;
        IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = airMassFlowRate;

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::SCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)) {
                    if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                    // turn on the supplemental cooling coil downstream
                    if (IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex > 0)
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex).ExtOn = true;
                } else if (true == bEnhancedDehum) { // run enhanced DH or grid reponsive mode
                    if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                    if (true == bEnhancedDehum) IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment = true;
                } else {
                    if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                }

                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::SHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex)) {
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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
                } else if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0) {
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                }

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }
            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::DWHMode:
            if (IsCallbyWH) // process when called from water loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                // state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).TotalHeatingEnergyRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;

            break;
        case IHPOperationMode::SCWHMatchWHMode:
            if (IsCallbyWH) // process when called from water loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SCDWHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::IdleMode:
        default: // clear up
            InitializeIHP(state, DXCoilNum);
            if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = 0.0;
            IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 0.0;
            UpdateIHP(state, DXCoilNum);
            break;
        }
    
    }

    void SimIHPLiquidDesiccantStorage(EnergyPlusData &state,
                             int &CompIndex,                // Index for Component name
                             int const CyclingScheme,       // Continuous fan OR cycling compressor
                             Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                             Real64 &HPTimeConstant,        // Heat pump time constant [s]
                             Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                             int const CompOp,              // compressor on/off. 0 = off; 1= on
                             Real64 const PartLoadFrac,
                             int const SpeedNum,      // compressor speed number
                             Real64 const SpeedRatio, // compressor speed ratio
                             Real64 const SensLoad,   // Sensible demand load [W]
                             Real64 const LatentLoad, // Latent demand load [W]
                             bool const IsCallbyWH,   // whether the call from the water heating loop or air loop, true = from water heating loop
                             bool const FirstHVACIteration,              // TRUE if First iteration of simulation
                             Optional<Real64 const> OnOffAirFlowRat, // ratio of comp on to comp off air flow rate
                             bool const bEnhancedDehum          // whether it requires enhanced dehumidification
    )
    {

        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   March 2016
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages variable-speed integrated Air source heat pump simulation.

        // Using/Aliasing
        using VariableSpeedCoils::InitVarSpeedCoil;
        using VariableSpeedCoils::IsGridResponsiveMode;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::UpdateVarSpeedCoil;
        using EvaporativeCoolers::EvapCond; 
        using EvaporativeCoolers::SimEvapCooler; 
        using WaterCoils::SimulateWaterCoilComponents;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using DataEnvironment::OutDryBulbTemp;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum(0); // The IHP No that you are currently dealing with
        Real64 waterMassFlowRate(0);
        Real64 airMassFlowRate(0);

        // local variable to check loop connections
        Real64 DehumLDTin(0.0);
        Real64 DehumLDTout(0.0);
        Real64 DehumTin(0.0);
        Real64 DehumWin(0.0);
        Real64 DeHumTout(0.0);
        Real64 DehumWout(0.0);
        Real64 EvapCSecTin(0.0);
        Real64 EvapSecWin(0.0);
        Real64 EvapCMainTin(0.0);
        Real64 EvapCMainTout(0.0);
        Real64 AirCoolInT(0.0); 
        Real64 EvapSecInWB(0.0);
        Real64 DehumInWB(0.0);

        int iWNod = 0;
        int iANod = 0;

        DXCoilNum = CompIndex;
        IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment = false;
        airMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRate;
        AirCoolInT = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).Temp; 
        waterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = airMassFlowRate;
        Node(IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum).MassFlowRate = waterMassFlowRate; 
        if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex > 0)
            state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).MaxWaterMassFlowRate = waterMassFlowRate;

        //assume outdoor tank with no insulation
        //Node(state.dataWaterCoils->state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum).Temp = OutDryBulbTemp; 

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::SCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                 if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).MassFlowRate = 0.0;
                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).MassFlowRate = waterMassFlowRate; 
                }

                if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)) {
                    if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                        (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                        
                        iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                        iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;

                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate =
                            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).MaxWaterMassFlowRate =
                            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;
                        Node(iWNod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;
                        Node(iWNod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;

                        //use loop air flow rate
                        //Node(iANod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate;
                        //Node(iANod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate;

                        DehumLDTin = Node(iWNod).Temp;
                        DehumTin = Node(iANod).Temp;
                        DehumWin = Node(iANod).HumRat;

                        SimulateWaterCoilComponents(state,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                    FirstHVACIteration,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                    0.0,
                                                    CyclingScheme,
                                                    PartLoadFrac);
                        iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirOutletNodeNum;
                        DeHumTout = Node(iANod).Temp;
                        DehumWout = Node(iANod).HumRat;
                        iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterOutletNodeNum;
                        DehumLDTout = Node(iWNod).Temp;
                    }

                    if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                } else if (true == bEnhancedDehum) { // run enhanced DH or grid reponsive mode
                    
                    if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                    if (true == bEnhancedDehum) IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment = true;
                } else {
                    if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                }

                if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)) 
                    //only apply desiccant and evaporative cool during grid response
                {
                    if ((AirCoolInT > 28.0) && (PartLoadFrac > 0.90)) {//debug breaking point
                        iWNod = 0;
                    }

                    if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                        (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                        iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                        iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;

                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate =
                            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;    
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).MaxWaterMassFlowRate =
                            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate; 
                        Node(iWNod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate; 
                        Node(iWNod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate; 

                        Node(iANod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate;                        
                        Node(iANod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate; 

                        DehumLDTin = Node(iWNod).Temp;
                        DehumTin = Node(iANod).Temp;
                        DehumWin = Node(iANod).HumRat;
                        
                        SimulateWaterCoilComponents(state,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                    FirstHVACIteration,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                    0.0,
                                                    CyclingScheme,
                                                    PartLoadFrac);
                        iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirOutletNodeNum;
                        DeHumTout = Node(iANod).Temp;
                        DehumWout = Node(iANod).HumRat;
                        iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterOutletNodeNum;
                        DehumLDTout = Node(iWNod).Temp;
                    }

                    if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                        EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).InletMassFlowRate = airMassFlowRate;

                        SimEvapCooler(
                            state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, 
                            IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, PartLoadFrac);
                        EvapCSecTin = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).SecInletTemp; 
                        EvapSecWin = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).SecInletHumRat; 

                        EvapCMainTin = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).InletTemp;
                        EvapCMainTout = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).OutletTemp;
                        EvapSecInWB = PsyTwbFnTdbWPb(state, EvapCSecTin, EvapSecWin, 1.013e5);  
                        DehumInWB = PsyTwbFnTdbWPb(state, DehumTin, DehumWin, 1.013e5);  
                    }
                } else {
                    if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                        (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                        iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                        iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                        Node(iWNod).MassFlowRate = 0.0;
                        Node(iWNod).MassFlowRateMax = 0.0;
                        Node(iANod).MassFlowRate = 0.0;
                        Node(iANod).MassFlowRateMax = 0.0;

                        SimulateWaterCoilComponents(state,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                    FirstHVACIteration,
                                                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                    0.0,
                                                    CyclingScheme,
                                                    0.0);
                    }

                    if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                        Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0; 
                        SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, 
                            IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                        Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                        Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
                    }                
                }
                
                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                 if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                 IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 
                     state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).PartLoadRatio; 
                 UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::SHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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

                 if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }
                if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
                    SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, 
                        IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                    Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
                }

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).MassFlowRate = 0.0;
                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).MassFlowRate = waterMassFlowRate; 
                }

                 if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex)) {
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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
                }
                else if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0) {
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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
                 }


                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                 IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                     state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).PartLoadRatio; 
                 UpdateIHP(state, DXCoilNum);
            }
            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::DWHMode:
            if (IsCallbyWH) // process when called from water loop
            {
                InitializeIHP(state, DXCoilNum);
                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                
                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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

                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }
                if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
                    SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                    Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
                }

                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                 if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
                    if (Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TlimitDWH) {
                        Node(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).AirInletNodeNum).MassFlowRate
                            = GetAirMassFlowRateIHP(state, CompIndex, SpeedNum, SpeedRatio, true); 
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                    }                     

                    else
                        SimVariableSpeedCoils(state,
                                              BlankString,
                                              IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                }                  

                if ((Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TregenTarget) 
                    &&(PartLoadFrac > 0.0)) {
                    IntegratedHeatPumps(DXCoilNum).SupHeatRate =
                        (IntegratedHeatPumps(DXCoilNum).TregenTarget - Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp) *
                        waterMassFlowRate * 4182.0; //supplmental heat rate
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp = IntegratedHeatPumps(DXCoilNum).TregenTarget; 
                }

                if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).AirInletNodeNum;                  
                    Node(iANod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate;
                    Node(iANod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate; 

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                PartLoadFrac);
                }

                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).PartLoadRatio; 
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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
                
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                if ((IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) &&
                    (Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TlimitSCWH))
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                else if ((IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) &&
                         (Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TlimitDWH) 
                         && (true == IntegratedHeatPumps(DXCoilNum).bIsDWHSeparateunit)) {
                    Node(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).AirInletNodeNum).MassFlowRate =
                        GetAirMassFlowRateIHP(state, CompIndex, SpeedNum, SpeedRatio, true); //borrow the indoor air flow rate to avoid zero

                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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
                }
                    
                if ((Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TregenTarget) 
                    && (PartLoadFrac > 0.0)) {
                    IntegratedHeatPumps(DXCoilNum).SupHeatRate =
                        (IntegratedHeatPumps(DXCoilNum).TregenTarget - Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp) *
                        waterMassFlowRate * 4182.0;
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp = IntegratedHeatPumps(DXCoilNum).TregenTarget;
                }

                if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0)  {
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).AirInletNodeNum;
                    Node(iANod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate;
                    Node(iANod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate; 
                    
                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                PartLoadFrac);
                }

                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
                    SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                    Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
                }

                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).PartLoadRatio;
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;

            break;
        case IHPOperationMode::SCDWHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                InitializeIHP(state, DXCoilNum);
                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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
                if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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

                if ((Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TregenTarget) 
                    && (PartLoadFrac > 0.0)) {
                    IntegratedHeatPumps(DXCoilNum).SupHeatRate =
                        (IntegratedHeatPumps(DXCoilNum).TregenTarget - Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp) *
                        waterMassFlowRate * 4182.0;
                    Node(IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum).Temp = IntegratedHeatPumps(DXCoilNum).TregenTarget;
                }

                if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).AirInletNodeNum;
                    Node(iANod).MassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate;
                    Node(iANod).MassFlowRateMax = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate; 

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                PartLoadFrac);
                }

                if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                    (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                    int iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                    iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                    Node(iWNod).MassFlowRate = 0.0;
                    Node(iWNod).MassFlowRateMax = 0.0;
                    //Node(iANod).MassFlowRate = 0.0;
                    //Node(iANod).MassFlowRateMax = 0.0;

                    SimulateWaterCoilComponents(state,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                                FirstHVACIteration,
                                                IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                                0.0,
                                                CyclingScheme,
                                                0.0);
                }

                if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
                    SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                    Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                    Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
                }

                if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

                if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                    SimVariableSpeedCoils(state,
                                          BlankString,
                                          IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
                IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio =
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).PartLoadRatio; 
                UpdateIHP(state, DXCoilNum);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::IdleMode:
        default: // clear up
            InitializeIHP(state, DXCoilNum);
            if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) &&
                (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                Node(iWNod).MassFlowRate = 0.0;
                Node(iWNod).MassFlowRateMax = 0.0;
                //Node(iANod).MassFlowRate = 0.0;
                //Node(iANod).MassFlowRateMax = 0.0;

                SimulateWaterCoilComponents(state,
                                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                            FirstHVACIteration,
                                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                            0.0,
                                            CyclingScheme,
                                            0.0);
            }

            if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SCCoilIndex,
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
            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex,
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

            if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) && 
                (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
                int iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
                iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                Node(iWNod).MassFlowRate = 0.0;
                Node(iWNod).MassFlowRateMax = 0.0;
                //Node(iANod).MassFlowRate = 0.0;
                //Node(iANod).MassFlowRateMax = 0.0;

                SimulateWaterCoilComponents(state,
                                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                            FirstHVACIteration,
                                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                            0.0,
                                            CyclingScheme,
                                            0.0);
            }

            if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
                Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
                SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
                Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = airMassFlowRate;
                Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRate = airMassFlowRate;
            }

            if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).SHCoilIndex,
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

             if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                SimVariableSpeedCoils(state,
                                      BlankString,
                                      IntegratedHeatPumps(DXCoilNum).DWHCoilIndex,
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

            if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).AirInletNodeNum;
                Node(iANod).MassFlowRate = 0.0;
                Node(iANod).MassFlowRateMax = 0.0; 

                SimulateWaterCoilComponents(state,
                                            IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                            FirstHVACIteration,
                                            IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex,
                                            0.0,
                                            CyclingScheme,
                                            0.0);
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = 0.0;
            IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 0.0; 
            UpdateIHP(state, DXCoilNum);
            break;
        }
                
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
        using WaterCoils::GetWaterCoilInput;
        using WaterCoils::GetWaterCoilIndex;
        using EvaporativeCoolers::EvapCond; 
        using EvaporativeCoolers::GetEvapInput;
        using EvaporativeCoolers::GetEvapCoolerIndex; 
        using IceThermalStorage::GetTankIndex; 
        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIHPInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum; // No of IHP DX system
        int NumASIHPs; // Counter for air-source integrated heat pumps
        int NumDesIHPs;//liquid dessicant storage integrated heat pumps

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

        NumASIHPs = inputProcessor->getNumObjectsFound(state, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE");
        NumDesIHPs = inputProcessor->getNumObjectsFound(state, "COILSYSTEM:DESICCANTSTORAGEHEATPUMP:AIRSOURCE");
        DXCoilNum = 0;

        if ((NumASIHPs + NumDesIHPs) <= 0) return;

        // Allocate Arrays
        IntegratedHeatPumps.allocate(NumASIHPs + NumDesIHPs);

        // air-source integrated heat pump
        inputProcessor->getObjectDefMaxArgs(state, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        inputProcessor->getObjectDefMaxArgs(state, "COILSYSTEM:DESICCANTSTORAGEHEATPUMP:AIRSOURCE", 
            NumParams, NumAlphas, NumNums);
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

            inputProcessor->getObjectItem(state, CurrentModuleObject,
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

            IntegratedHeatPumps(DXCoilNum).Name = AlphArray(1);
            IntegratedHeatPumps(DXCoilNum).IHPtype = "AIRSOURCE_IHP";

            // AlphArray( 2 ) is the water sensor node

            IntegratedHeatPumps(DXCoilNum).SCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCCoilName = AlphArray(3);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;

            ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SHCoilName = AlphArray(4);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SHCoilName;

            ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            if (!lAlphaBlanks(5)) {
                IntegratedHeatPumps(DXCoilNum).DWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).DWHCoilName = AlphArray(5);
                Coiltype = IntegratedHeatPumps(DXCoilNum).DWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).DWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).DWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }            
            }
            
            if (!lAlphaBlanks(6)) {
                IntegratedHeatPumps(DXCoilNum).SCWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCWHCoilName = AlphArray(6);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
            
            if (!lAlphaBlanks(7)) {
                IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName = AlphArray(7);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
                       
            if (!lAlphaBlanks(8)) {
                IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName = AlphArray(8);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).bIsDesuperheater = true;
                    }
                }
            }
            
            if (!lAlphaBlanks(9)) {
                IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType = "COIL:HEATING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName = AlphArray(9);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
            
            if (!lAlphaBlanks(10)) {
                IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName = AlphArray(10);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).bIsDesuperheater = true;
                    }
                }
            }
            
            if (!lAlphaBlanks(11)) {
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilName = AlphArray(11);
                Coiltype = IntegratedHeatPumps(DXCoilNum).EnDehumCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).EnDehumCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } 
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).EnDehumCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex; 
            }

            if (!lAlphaBlanks(12)) {
                IntegratedHeatPumps(DXCoilNum).GridSCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSCCoilName = AlphArray(12);
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSCCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSCCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).GridSCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSCCoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSCCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSCCoilName;
                IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            }

            if (!lAlphaBlanks(13)) {
                IntegratedHeatPumps(DXCoilNum).GridSHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSHCoilName = AlphArray(13);
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).GridSHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSHCoilName = IntegratedHeatPumps(DXCoilNum).SHCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSHCoilName;
                IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            }

            if (!lAlphaBlanks(14)) {
                IntegratedHeatPumps(DXCoilNum).ChillerCoilType = "COIL:CHILLER:AIRSOURCE:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).ChillerCoilName = AlphArray(14);
                Coiltype = IntegratedHeatPumps(DXCoilNum).ChillerCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).ChillerCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }
                        
            if (UtilityRoutines::SameString(AlphArray(15), "SINGLE")) {
                IntegratedHeatPumps(DXCoilNum).bIsChillerSeparateunit = false;
            } else {
                IntegratedHeatPumps(DXCoilNum).bIsChillerSeparateunit = true;
            }

            if (!lAlphaBlanks(16)) IntegratedHeatPumps(DXCoilNum).SupWaterCoilType = AlphArray(16);

            if (!lAlphaBlanks(17)) IntegratedHeatPumps(DXCoilNum).SupWaterCoilName = AlphArray(17);

            if ((!lAlphaBlanks(16)) && (!lAlphaBlanks(17))) {
                Coiltype = IntegratedHeatPumps(DXCoilNum).SupWaterCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SupWaterCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex = GetWaterCoilIndex(
                        state, IntegratedHeatPumps(DXCoilNum).SupWaterCoilType, IntegratedHeatPumps(DXCoilNum).SupWaterCoilName, errFlag);

                    if (0 == IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            };

            if (!lAlphaBlanks(18)) IntegratedHeatPumps(DXCoilNum).ChillTankType = AlphArray(18);
            if (!lAlphaBlanks(19)) IntegratedHeatPumps(DXCoilNum).ChillTankName = AlphArray(19);

            if ((!lAlphaBlanks(18)) && (!lAlphaBlanks(19))) {
                Coiltype = IntegratedHeatPumps(DXCoilNum).ChillTankType;
                CoilName = IntegratedHeatPumps(DXCoilNum).ChillTankName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } 
                else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).ChillTankIndex = GetTankIndex(state, IntegratedHeatPumps(DXCoilNum).ChillTankType, 
                        IntegratedHeatPumps(DXCoilNum).ChillTankName); 

                    if (0 == IntegratedHeatPumps(DXCoilNum).ChillTankIndex) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        IceThermalStorage::SetIHPID(
                            state, IntegratedHeatPumps(DXCoilNum).ChillTankType, IntegratedHeatPumps(DXCoilNum).ChillTankIndex, DXCoilNum); 
                    }
                }
            };

            IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow = NumArray(1);
            IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow = NumArray(2);
            IntegratedHeatPumps(DXCoilNum).TindoorWHHighPriority = NumArray(3);
            IntegratedHeatPumps(DXCoilNum).TambientWHHighPriority = NumArray(4);
            IntegratedHeatPumps(DXCoilNum).ModeMatchSCWH = int(NumArray(5));
            IntegratedHeatPumps(DXCoilNum).MinSpedSCWH = int(NumArray(6));
            IntegratedHeatPumps(DXCoilNum).WaterVolSCDWH = NumArray(7);
            IntegratedHeatPumps(DXCoilNum).MinSpedSCDWH = int(NumArray(8));
            IntegratedHeatPumps(DXCoilNum).TimeLimitSHDWH = NumArray(9);
            IntegratedHeatPumps(DXCoilNum).MinSpedSHDWH = int(NumArray(10));

            if (!lNumericBlanks(11)) IntegratedHeatPumps(DXCoilNum).SHCoilSize = NumArray(11); // 1.0
            if (!lNumericBlanks(12)) IntegratedHeatPumps(DXCoilNum).DWHCoilSize = NumArray(12); // 1.0
            if (!lNumericBlanks(13)) IntegratedHeatPumps(DXCoilNum).SCWHCoilSize = NumArray(13); // 1.0
            if (!lNumericBlanks(14)) IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilSize = NumArray(14); // 1.0
            if (!lNumericBlanks(15)) IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilSize = NumArray(15);   // 0.13
            if (!lNumericBlanks(16)) IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilSize = NumArray(16); // 0.9
            if (!lNumericBlanks(17)) IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilSize = NumArray(17);   // 0.1
            if (!lNumericBlanks(18)) IntegratedHeatPumps(DXCoilNum).EnDehumCoilSize = NumArray(18);   // 0.5
            if (!lNumericBlanks(19)) IntegratedHeatPumps(DXCoilNum).GridSCCoilSize = NumArray(19);      // 0.9
            if (!lNumericBlanks(20)) IntegratedHeatPumps(DXCoilNum).GridSHCoilSize = NumArray(20);    // 0.9
            if (!lNumericBlanks(21)) IntegratedHeatPumps(DXCoilNum).ChillerRunSpeed = int(NumArray(21)); // -5.0
            if (!lNumericBlanks(22)) IntegratedHeatPumps(DXCoilNum).ChillerSize = NumArray(22);       // 1.0
            if (!lNumericBlanks(23)) IntegratedHeatPumps(DXCoilNum).SupWaterCoilAirFRatio = NumArray(23); // 1.0
            if (!lNumericBlanks(24)) IntegratedHeatPumps(DXCoilNum).SupWaterCoilWaterFRatio = NumArray(24); // 1.0
            if (!lNumericBlanks(25)) IntegratedHeatPumps(DXCoilNum).ChargFracLow = NumArray(25);            // 0.9
            if (!lNumericBlanks(26)) IntegratedHeatPumps(DXCoilNum).TchargeZeroFrac = NumArray(26);         // 0.9

            if (!lAlphaBlanks(20)) {
                IntegratedHeatPumps(DXCoilNum).CurveChargeT = GetCurveIndex(state, AlphArray(20)); // convert curve name to number
                if (IntegratedHeatPumps(DXCoilNum).CurveChargeT == 0) {
                       ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + IntegratedHeatPumps(DXCoilNum).Name +
                                            "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(20) + "=\"" + AlphArray(20) + "\".");
                    ErrorsFound = true;
                } else {
                    Real64 CurveVal = CurveValue(state, IntegratedHeatPumps(DXCoilNum).CurveChargeT, 0.0);
                    if (CurveVal > 2.0 || CurveVal < -2.0) {
                        ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + IntegratedHeatPumps(DXCoilNum).Name +
                                             "\", curve value");
                        ShowContinueError(state, "..." + cAlphaFields(20) + " output is not equal to 0.0 (+ 2.0 or - 2.0 C) at 0 fraction.");
                        ShowContinueError(state, format("...Curve output at 0 fraction = {:.3T}", CurveVal));
                    }
                }
            }
                      
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
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
            OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);

            IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = InNode;
            IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum = OutNode;
            IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = OutNode;

            TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", InNodeName, OutNodeName, "Cooling Air Nodes");
            RegisterNodeConnection(state, InNode,
                                   NodeID(InNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                                   "Inlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
            RegisterNodeConnection(state, OutNode,
                                   NodeID(OutNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                                   "Outlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

            SetUpCompSets(state, CurrentModuleObject,
                          IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                          IntegratedHeatPumps(DXCoilNum).SCCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCCoilName,
                          InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(state, InNode,
                                       InNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
            OverrideNodeConnectionType(state, OutNode,
                                       OutNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            
            if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            

            if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex) {

                 if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).EnDehumCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex) {

                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).GridSCCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                              IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            
            // heating coil air node connections
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;

            InNode = IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum;
            OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum = OutNode;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum != InNode) {
                ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + "- cooling coil outlet mismatches heating coil inlet" +
                                  ".");
                ErrorsFound = true;
            }
            TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", InNodeName, OutNodeName, "Heating Air Nodes");
            RegisterNodeConnection(state, InNode,
                                   NodeID(InNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                                   "Inlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
            RegisterNodeConnection(state, OutNode,
                                   NodeID(OutNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                                   "Outlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

            SetUpCompSets(state, CurrentModuleObject,
                          IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                          IntegratedHeatPumps(DXCoilNum).SHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SHCoilName,
                          InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(state, InNode,
                                       InNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
            OverrideNodeConnectionType(state, OutNode,
                                       OutNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

            if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).GridSHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                              IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                              IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                              IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                              IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            
            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
                // water node connections
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;

                InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);
                IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum = InNode;
                IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum = OutNode;
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }

                TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", InNodeName, OutNodeName, "Water Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                                       "Inlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                                       "Outlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            
            if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

           
            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).DWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            
            if (!lAlphaBlanks(2)) IntegratedHeatPumps(DXCoilNum).WaterTankoutNod = GetOnlySingleNode(state,
                AlphArray(2), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Sensor, 2, ObjectIsNotParent);

            // outdoor air node connections for water heating coils
            // DWH, SCDWH, SHDWH coils have the same outdoor air nodes
            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
                InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);
                IntegratedHeatPumps(DXCoilNum).ODAirInletNodeNum = InNode;
                IntegratedHeatPumps(DXCoilNum).ODAirOutletNodeNum = OutNode;
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }

                TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", InNodeName, OutNodeName, "Outdoor Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                              IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            
            if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum = InNode;
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum = OutNode;
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                              IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex != 0) {
                // water node connections
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex;

                InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Chiller Coil",
                              IntegratedHeatPumps(DXCoilNum).ChillerCoilType,
                              IntegratedHeatPumps(DXCoilNum).ChillerCoilName,
                              InNodeName,
                              OutNodeName,
                              "Water Nodes" );
             
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).ChillerCoilType,
                                           IntegratedHeatPumps(DXCoilNum).ChillerCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).ChillerCoilType,
                                           IntegratedHeatPumps(DXCoilNum).ChillerCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }
            
            IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = false;
            IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow = 1e10;
        }

         // Get the data for Liquid desiccant storage IHPs
        CurrentModuleObject = "COILSYSTEM:DESICCANTSTORAGEHEATPUMP:AIRSOURCE"; // for reporting
        sIHPType = "COILSYSTEM:DESICCANTSTORAGEHEATPUMP:AIRSOURCE";            // for checking

        for (CoilCounter = 1; CoilCounter <= NumDesIHPs; ++CoilCounter) {

            ++DXCoilNum;
            AlfaFieldIncre = 1;

            inputProcessor->getObjectItem(state, CurrentModuleObject,
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

            IntegratedHeatPumps(DXCoilNum).Name = AlphArray(1);
            IntegratedHeatPumps(DXCoilNum).IHPtype = "AIRSOURCE_IHP_DESICCANTSTORAGE";
            IntegratedHeatPumps(DXCoilNum).StorageType = IHPStorageType::LIQUIDDESICCANT;

            // AlphArray( 2 ) is the water sensor node

            IntegratedHeatPumps(DXCoilNum).SCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCCoilName = AlphArray(2);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;

            ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SHCoilName = AlphArray(3);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SHCoilName;

            ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            if (!lAlphaBlanks(4)) {
                IntegratedHeatPumps(DXCoilNum).DWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).DWHCoilName = AlphArray(4);
                Coiltype = IntegratedHeatPumps(DXCoilNum).DWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).DWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).DWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }

            if ((UtilityRoutines::SameString(AlphArray(5), "SINGLE")) || lAlphaBlanks(5)) {
                IntegratedHeatPumps(DXCoilNum).bIsDWHSeparateunit = false; 
            } else {
                IntegratedHeatPumps(DXCoilNum).bIsDWHSeparateunit = true;
            }

            if (!lAlphaBlanks(6)) {
                IntegratedHeatPumps(DXCoilNum).SCWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCWHCoilName = AlphArray(6);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }

            if (!lAlphaBlanks(7)) {
                IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName = AlphArray(7);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }

            if (!lAlphaBlanks(8)) {
                IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName = AlphArray(8);
                Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).bIsDesuperheater = true;
                    }
                }
            }

            if (!lAlphaBlanks(9)) {
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilName = AlphArray(9);
                Coiltype = IntegratedHeatPumps(DXCoilNum).EnDehumCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).EnDehumCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).EnDehumCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            }

            if (!lAlphaBlanks(10)) {
                IntegratedHeatPumps(DXCoilNum).GridSCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSCCoilName = AlphArray(10);
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSCCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSCCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).GridSCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSCCoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSCCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSCCoilName;
                IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            }

            if ((!lAlphaBlanks(11)) &&(!lAlphaBlanks(12))) {
                IntegratedHeatPumps(DXCoilNum).LDDehumCoilType = AlphArray(11);
                IntegratedHeatPumps(DXCoilNum).LDDehumCoilName = AlphArray(12);
                Coiltype = IntegratedHeatPumps(DXCoilNum).LDDehumCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).LDDehumCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex =
                        GetWaterCoilIndex(state,
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilType, 
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName, errFlag); 
                    if (0 == IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else { // not in plant loop
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).IsInPlantLoop = false;
                    }
                }
            }

            if (UtilityRoutines::SameString(AlphArray(13), "UPSTREAM"))
            {
                IntegratedHeatPumps(DXCoilNum).DehumPlace = DehumPlacement::UPSTRREAM;
            } else if (UtilityRoutines::SameString(AlphArray(13), "DOWNSTREAM")) {
                IntegratedHeatPumps(DXCoilNum).DehumPlace = DehumPlacement::DOWNSTREAM;
            } else {
                IntegratedHeatPumps(DXCoilNum).DehumPlace = DehumPlacement::OUTDOOR;
            }

            if ((!lAlphaBlanks(14)) && (!lAlphaBlanks(15))) {
                IntegratedHeatPumps(DXCoilNum).LDRegenCoilType = AlphArray(14);
                IntegratedHeatPumps(DXCoilNum).LDRegenCoilName = AlphArray(15);
                Coiltype = IntegratedHeatPumps(DXCoilNum).LDRegenCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).LDRegenCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;

                    IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex = 
                        GetWaterCoilIndex(state, IntegratedHeatPumps(DXCoilNum).LDRegenCoilType, 
                            IntegratedHeatPumps(DXCoilNum).LDRegenCoilName, errFlag); 
                    if (0 == IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    } else {//not in plant loop
                        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).IsInPlantLoop = false; 
                    }
                }
            }

            if ((!lAlphaBlanks(16)) && (!lAlphaBlanks(17))) {
                IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType = AlphArray(16);
                IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName = AlphArray(17);
                Coiltype = IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex =
                        GetEvapCoolerIndex(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, errFlag); 

                    if (0 == IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            }

            if (!lAlphaBlanks(18)) {
                IntegratedHeatPumps(DXCoilNum).GridSHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSHCoilName = AlphArray(18);
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSHCoilName;

                ValidateComponent(state, Coiltype, CoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex = GetCoilIndexVariableSpeed(state, Coiltype, CoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                IntegratedHeatPumps(DXCoilNum).GridSHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
                IntegratedHeatPumps(DXCoilNum).GridSHCoilName = IntegratedHeatPumps(DXCoilNum).SHCoilName;
                Coiltype = IntegratedHeatPumps(DXCoilNum).GridSHCoilType;
                CoilName = IntegratedHeatPumps(DXCoilNum).GridSHCoilName;
                IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            }

            //supplemental heater type
            if (!lAlphaBlanks(19)) IntegratedHeatPumps(DXCoilNum).SUPHEATTYPE = AlphArray(19);
            else IntegratedHeatPumps(DXCoilNum).SUPHEATTYPE = "NONE";

            IntegratedHeatPumps(DXCoilNum).WHHPType = AlphArray(20);
            IntegratedHeatPumps(DXCoilNum).WHtankName = AlphArray(21);

            IntegratedHeatPumps(DXCoilNum).WHtankType = DataPlant::TypeOf_HeatPumpWtrHeaterPumped; 

            IntegratedHeatPumps(DXCoilNum).MinSpedSCWH = int(NumArray(1));
            IntegratedHeatPumps(DXCoilNum).MinSpedSCDWH = int(NumArray(2));

            IntegratedHeatPumps(DXCoilNum).TlimitSCWH = NumArray(3);
            IntegratedHeatPumps(DXCoilNum).TlimitSCDWH = NumArray(4);
            IntegratedHeatPumps(DXCoilNum).TlimitDWH = NumArray(5);
            IntegratedHeatPumps(DXCoilNum).TregenTarget = NumArray(6);
            IntegratedHeatPumps(DXCoilNum).Concen_Set = NumArray(7);
            IntegratedHeatPumps(DXCoilNum).Concen_band = NumArray(8);
 
            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowSize = NumArray(9);
            IntegratedHeatPumps(DXCoilNum).DehumAirMasslowSize = NumArray(10);
            IntegratedHeatPumps(DXCoilNum).RegenAirMasslowSize = NumArray(11);
            

            if (UtilityRoutines::SameString(IntegratedHeatPumps(DXCoilNum).SUPHEATTYPE, "NONE"))
                IntegratedHeatPumps(DXCoilNum).TregenTarget =  -10000.0; 

            if (!lNumericBlanks(12)) IntegratedHeatPumps(DXCoilNum).SHCoilSize = NumArray(12);        // 1.0
            if (!lNumericBlanks(13)) IntegratedHeatPumps(DXCoilNum).DWHCoilSize = NumArray(13);       // 1.0
            if (!lNumericBlanks(14)) IntegratedHeatPumps(DXCoilNum).SCWHCoilSize = NumArray(14);      // 1.0
            if (!lNumericBlanks(15)) IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilSize = NumArray(15); // 1.0
            if (!lNumericBlanks(16)) IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilSize = NumArray(16);   // 0.13
            if (!lNumericBlanks(17)) IntegratedHeatPumps(DXCoilNum).EnDehumCoilSize = NumArray(17);   // 0.5
            if (!lNumericBlanks(18)) IntegratedHeatPumps(DXCoilNum).GridSCCoilSize = NumArray(18);      // 0.9
            if (!lNumericBlanks(19)) IntegratedHeatPumps(DXCoilNum).GridSCCoilSize = NumArray(19);      // 0.9

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

            if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) && 
                (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex;
                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                            InNodeName,
                            OutNodeName,
                            "Dehumidification Coil Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                              InNodeName,
                              OutNodeName);
               
                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterOutletNodeNum;

                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                            InNodeName,
                            OutNodeName,
                            "Dehumidification Coil Water Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Inlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Outlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                              InNodeName,
                              OutNodeName);
            }

            // cooling coil air node connections
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
            if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex > 0)
                OutNode = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).InletNode; 
            else
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;

            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);

            if (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::OUTDOOR) {
                IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
                IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum = 
                    EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).InletNode;
                IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = 
                    EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).OutletNode; 
            } else if ((IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM) &&
                       (IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex > 0)) {
                IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = 
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
                IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            } else if ((IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::DOWNSTREAM) &&
                       (IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex > 0)) {
                IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = 
                    state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
                IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum =
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
                IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = 
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirOutletNodeNum;
            }
            
            TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", InNodeName, OutNodeName, "Cooling Air Nodes");
            RegisterNodeConnection(state, InNode,
                                   NodeID(InNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                                   "Inlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
            RegisterNodeConnection(state, OutNode,
                                   NodeID(OutNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                                   "Outlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

            SetUpCompSets(state, CurrentModuleObject,
                          IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                          IntegratedHeatPumps(DXCoilNum).SCCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCCoilName,
                          InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(state, InNode,
                                       InNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
            OverrideNodeConnectionType(state, OutNode,
                                       OutNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex) {

                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).EnDehumCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EnDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex) {

                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).GridSCCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                              IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                              IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSCCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            // heating coil air node connections
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;

            InNode = IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum;
            OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum = OutNode;

            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum != InNode) {
                ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + "- cooling coil outlet mismatches heating coil inlet" +
                                  ".");
                ErrorsFound = true;
            }
            TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", InNodeName, OutNodeName, "Heating Air Nodes");
            RegisterNodeConnection(state, InNode,
                                   NodeID(InNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                                   "Inlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);
            RegisterNodeConnection(state, OutNode,
                                   NodeID(OutNode),
                                   CurrentModuleObject,
                                   IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                                   "Outlet",
                                   1,
                                   ObjectIsNotParent,
                                   ErrorsFound);

            SetUpCompSets(state, CurrentModuleObject,
                          IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                          IntegratedHeatPumps(DXCoilNum).SHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SHCoilName,
                          InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(state, InNode,
                                       InNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
            OverrideNodeConnectionType(state, OutNode,
                                       OutNodeName,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

             if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).GridSHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                              IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                              IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).GridSHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
                // water node connections
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;

                InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).WaterOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);
                IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum = InNode;

                if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0)
                    IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum = 
                    state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterOutletNodeNum;
                else
                    IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum = OutNode;

                IntegratedHeatPumps(DXCoilNum).WaterMiddleNodeNum = OutNode;
                
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }

                //TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", InNodeName, OutNodeName, "Water Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                                       "Inlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                                       "Outlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            
            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).DWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }
                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Water Coil",
                              IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           2,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

            
            // outdoor air node connections for water heating coils
            // DWH, SCDWH, SHDWH coils have the same outdoor air nodes
            if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
                InNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
                OutNode = state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);
                IntegratedHeatPumps(DXCoilNum).ODAirInletNodeNum = InNode;
                IntegratedHeatPumps(DXCoilNum).ODAirOutletNodeNum = OutNode;
                if ((state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirInletNodeNum != InNode) ||
                    (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirOutletNodeNum != OutNode)) {
                    ShowContinueError(state, "Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                      "-wrong coil node names.");
                    ErrorsFound = true;
                }

                TestCompSet(state, CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", InNodeName, OutNodeName, "Outdoor Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                              IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil",
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                              IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                           IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            }

             // evaportive cooling coil node connection
            if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0 ) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex;
                InNode = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).InletNode;
                OutNode = EvapCond(IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex).OutletNode;
                // state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType,
                            IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName,
                            InNodeName,
                            OutNodeName, 
                    "Evaporative Cooling Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Evaporative Cooling Coil",
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       CurrentModuleObject,
                                       IntegratedHeatPumps(DXCoilNum).Name + " Evaporative Cooling Coil",
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType,
                              IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).EvapCoolCoilType,
                                           IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
            
            }

            if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex;
                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirOutletNodeNum;
                // state.dataVariableSpeedCoils->VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                            InNodeName,
                            OutNodeName,
                            "Regeneration Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                           IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                           IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterOutletNodeNum;
                
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                 TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                            InNodeName,
                            OutNodeName,
                            "Regeneration Water Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                       "Inlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                                       "Outlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDRegenCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDRegenCoilName,
                              InNodeName,
                              OutNodeName);
            }
            

            if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) && 
                (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM)) {
                ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex;
                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).AirOutletNodeNum;
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                            InNodeName,
                            OutNodeName,
                            "Dehumidification Coil Air Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Inlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Outlet",
                                       1,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                              InNodeName,
                              OutNodeName);
                OverrideNodeConnectionType(state, InNode,
                                           InNodeName,
                                           IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);
                OverrideNodeConnectionType(state, OutNode,
                                           OutNodeName,
                                           IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                           IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                           "Internal",
                                           1,
                                           ObjectIsNotParent,
                                           ErrorsFound);

                InNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterInletNodeNum;
                OutNode = state.dataWaterCoils->WaterCoil(ChildCoilIndex).WaterOutletNodeNum;
                
                InNodeName = NodeID(InNode);
                OutNodeName = NodeID(OutNode);

                TestCompSet(state, IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                            IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                            InNodeName,
                            OutNodeName,
                            "Dehumidification Coil Water Nodes");
                RegisterNodeConnection(state, InNode,
                                       NodeID(InNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Inlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);
                RegisterNodeConnection(state, OutNode,
                                       NodeID(OutNode),
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                                       IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                       "Outlet",
                                       2,
                                       ObjectIsNotParent,
                                       ErrorsFound);

                SetUpCompSets(state, CurrentModuleObject,
                              IntegratedHeatPumps(DXCoilNum).Name,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilType,
                              IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                              InNodeName,
                              OutNodeName);
            }

            IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = false;
            IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow = 1e10;

            SetupOutputVariable(state,
                    "Desiccant Storage Heat Pump Supplemental Heat Rate",
                    OutputProcessor::Unit::W,
                    IntegratedHeatPumps(DXCoilNum).SupHeatRate,
                    "System",
                    "Average",
                    IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Desiccant Storage Heat Pump Salt Concentration",
                                OutputProcessor::Unit::None,
                                IntegratedHeatPumps(DXCoilNum).SaltConcentration,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state,
                    "Desiccant Storage Heat Pump Supplemental Heat Energy",
                    OutputProcessor::Unit::J,
                    IntegratedHeatPumps(DXCoilNum).SupHeatEnergy,
                    "System",
                    "Summed",
                    IntegratedHeatPumps(DXCoilNum).Name);
        }

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination.");
        } else {
            // set up output variables, not reported in the individual coil models

            //				TODO: Figure out how to get enum class to work with SetupOutputVariable
            //				Setup Output Variable( "Operation Mode []",
            //				                     static_cast< int >( IntegratedHeatPumps( DXCoilNum ).CurMode ),
            //				                     "System", "Average",
            //				                     IntegratedHeatPumps( DXCoilNum ).Name );
            SetupOutputVariable(state, "Integrated Heat Pump Air Loop Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Condenser Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalCoolingRate,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Heating Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Water Heating Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalPower,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalLatentLoad,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Source Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).Qsource,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump COP",
                                OutputProcessor::Unit::None,
                                IntegratedHeatPumps(DXCoilNum).TotalCOP,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Integrated Heat Pump SHR",
                                OutputProcessor::Unit::None,
                                IntegratedHeatPumps(DXCoilNum).SHR,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).Energy,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Heating Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Water Heating Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Air Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLatent,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state, "Integrated Heat Pump Source Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergySource,
                                "System",
                                "Summed",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Integrated Heat Pump Work Mode",
                                OutputProcessor::Unit::None,
                                IntegratedHeatPumps(DXCoilNum).iWorkMode,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Integrated Heat Pump Supplemental Cooling Capacity",
                                OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).DischargeCapacity,
                                "System",
                                "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
        }
    }

    void SizeIHP(EnergyPlusData &state, int const DXCoilNum)
    {
        using DataSizing::AutoSize;
        using VariableSpeedCoils::SetVarSpeedCoilData;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::SizeVarSpeedCoil;
        using WaterCoils::SizeWaterCoil_NotInPlant;
        using WaterCoils::SizeWaterCoil;
        using EvaporativeCoolers::EvapCond; 
        using EvaporativeCoolers::SizeEvapCooler;

        static bool ErrorsFound(false); // If errors detected in input
        Real64 RatedCapacity(0.0);      // rated building cooling load

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        };

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(
                state,
                format("SizeIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP", DXCoilNum, IntegratedHeatPumps.size()));
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) {
            return;
        }

        // associate SC coil with SH coil
        SetVarSpeedCoilData(state, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, ErrorsFound, _, IntegratedHeatPumps(DXCoilNum).SHCoilIndex);
        if (ErrorsFound) {
            ShowSevereError(state, "SizeIHP: Could not match cooling coil\"" + IntegratedHeatPumps(DXCoilNum).SCCoilName + "\" with heating coil=\"" +
                            IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
            ErrorsFound = false;
        };

        SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SCCoilIndex); // size cooling coil
        if (ErrorsFound) {
            ShowFatalError(state, "SizeIHP: failed to size SC coil\"" + IntegratedHeatPumps(DXCoilNum).SCCoilName + "\"");
            ErrorsFound = false;
        } else {
            RatedCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).RatedCapCoolTotal;
        };

        if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).RatedCapHeat == AutoSize) {
            state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).RatedCapHeat = RatedCapacity * IntegratedHeatPumps(DXCoilNum).SHCoilSize; //1.0
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SHCoilIndex); // size heating coil
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size SH coil\"" + IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
                ErrorsFound = false;
            };       
        }

        // pass SC coil capacity to enhanced dehumidification coil
        if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).RatedCapCoolTotal == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).RatedCapCoolTotal =
                    RatedCapacity * IntegratedHeatPumps(DXCoilNum).EnDehumCoilSize; // 0.9
            };

            // size SCDWH air coil
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size enhanced dehumidification cooling coil\"" + IntegratedHeatPumps(DXCoilNum).EnDehumCoilName + "\"");
                ErrorsFound = false;
            };
        }

        // pass SC coil capacity to grid responsive cooling coil
        if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex){
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).RatedCapCoolTotal == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).RatedCapCoolTotal =
                    RatedCapacity * IntegratedHeatPumps(DXCoilNum).GridSCCoilSize; // 0.9
            };

            // size SCDWH air coil
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size grid responsive cooling coil\"" + IntegratedHeatPumps(DXCoilNum).GridSCCoilName +
                                "\"");
                ErrorsFound = false;
            };
        }

        // pass SC coil capacity to SCDWH cool coil
        if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
        {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal =
                    RatedCapacity * IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilSize;//1.0
            };

            // size SCDWH air coil
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size SCDWH cooling coil\"" + IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName + "\"");
                ErrorsFound = false;
            };

            if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0) {

                SetVarSpeedCoilData(
                    state, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, ErrorsFound, _, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex); 

                if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).RatedCapHeat == AutoSize) 
                    state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).RatedCapHeat =
                        RatedCapacity * IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilSize; // 1.0
                 // size SHDWH air coil
                SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex); 
                
                if (ErrorsFound) {
                        ShowSevereError(state, "SizeIHP: failed to size SHDWH heating coil\"" + IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName + "\"");
                        ErrorsFound = false;
                    };
            }
        }

        // size the water coils below
        // size SCWH water coil
        if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
              if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH = IntegratedHeatPumps(DXCoilNum).SCWHCoilSize * 
                    RatedCapacity / (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCOPHeat);//1.0
            }      

            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size SCWH coil\"" + IntegratedHeatPumps(DXCoilNum).SCWHCoilName + "\"");
                ErrorsFound = false;
            };
        }

        if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
            // size DWH water coil
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH = RatedCapacity * IntegratedHeatPumps(DXCoilNum).DWHCoilSize;
            }

            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size DWH coil\"" + IntegratedHeatPumps(DXCoilNum).DWHCoilName + "\"");
                ErrorsFound = false;
            };        
        }

        // size SCDWH water coil
        if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH = RatedCapacity *
                    IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilSize; // 0.13
            }        

            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size SCDWH water heating coil\"" + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName + "\"");
                ErrorsFound = false;
            };       
        }

        if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0) {
             // size SHDWH water coil
            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH = RatedCapacity * 
                    IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilSize; // 0.1
            }

            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex);      

            
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size SHDWH water heating coil\"" + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName + "\"");
                ErrorsFound = false;
            };
        }

        // pass SH coil capacity to grid responsive heating coil
        if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex) {

             // associate SC coil with SH coil
            SetVarSpeedCoilData(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex, ErrorsFound, _, 
                IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: Could not match cooling coil\"" + IntegratedHeatPumps(DXCoilNum).GridSCCoilName + "\" with heating coil=\"" +
                                IntegratedHeatPumps(DXCoilNum).GridSHCoilName + "\"");
                ErrorsFound = false;
            };

            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).RatedCapHeat == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).RatedCapHeat =
                    RatedCapacity * IntegratedHeatPumps(DXCoilNum).GridSHCoilSize; // 0.9
            };

            // size SCDWH air coil
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size grid responsive cooling coil\"" + IntegratedHeatPumps(DXCoilNum).GridSHCoilName + "\"");
                ErrorsFound = false;
            };
        }

        // size chiller coil 
        if (IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex > 0) {

            if (state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex).RatedCapCoolTotal == AutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex).RatedCapCoolTotal =
                    RatedCapacity * IntegratedHeatPumps(DXCoilNum).ChillerSize; // 0.9
            };

            // size SCDWH air coil
            SizeVarSpeedCoil(state, IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex);
            if (ErrorsFound) {
                ShowSevereError(state, "SizeIHP: failed to size chiller coil\"" + IntegratedHeatPumps(DXCoilNum).ChillerCoilName + "\"");
                ErrorsFound = false;
            };
        }

        if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0) {
            IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex)
                    .MSRatedWaterMassFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).NormSpedLevel); 
            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowSize 
                * IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate;
        } else if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0) {
            IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex)
                    .MSRatedWaterMassFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).NormSpedLevel); 
            IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowSize 
                * IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate;
        }

        IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate =
            IntegratedHeatPumps(DXCoilNum).RegenAirMasslowSize *
            state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                .MSRatedAirMassFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel);

        int iCoilID = IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex;
        if (iCoilID != 0) {
            state.dataWaterCoils->WaterCoil(iCoilID).DesAirMassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowRate; 
            state.dataWaterCoils->WaterCoil(iCoilID).DesAirVolFlowRate = IntegratedHeatPumps(DXCoilNum).RegenAirMasslowSize *
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                    .MSRatedAirVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel);

            state.dataWaterCoils->WaterCoil(iCoilID).InletWaterMassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate;
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletSolnConcentration = IntegratedHeatPumps(DXCoilNum).Concen_Set; 
            state.dataWaterCoils->WaterCoil(iCoilID).MaxWaterMassFlowRate = IntegratedHeatPumps(DXCoilNum).RegenLDMassFlowRate;
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletAirTemp = 13.8; 
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletAirHumRat = 0.00788862; // 
            state.dataWaterCoils->WaterCoil(iCoilID).DesOutletAirTemp = 19.0;
            state.dataWaterCoils->WaterCoil(iCoilID).DesOutletAirHumRat = 0.00668435; // 
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletWaterTemp = 20.0;

            SizeWaterCoil_NotInPlant(state, iCoilID);
        }

        IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate =
            IntegratedHeatPumps(DXCoilNum).DehumAirMasslowSize *
            state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                .MSRatedAirMassFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel);

        
        iCoilID = IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex;
        if (iCoilID != 0) {
            state.dataWaterCoils->WaterCoil(iCoilID).DesAirMassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowRate;
            state.dataWaterCoils->WaterCoil(iCoilID).DesAirVolFlowRate = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowSize *
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                                                       .MSRatedAirVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel);

            state.dataWaterCoils->WaterCoil(iCoilID).InletWaterMassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletSolnConcentration = IntegratedHeatPumps(DXCoilNum).Concen_Set;
            state.dataWaterCoils->WaterCoil(iCoilID).MaxWaterMassFlowRate = IntegratedHeatPumps(DXCoilNum).DehumLDMassFlowRate;
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletAirTemp = 13.8;
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletAirHumRat = 0.00788862; // 
            state.dataWaterCoils->WaterCoil(iCoilID).DesOutletAirTemp = 19.0;
            state.dataWaterCoils->WaterCoil(iCoilID).DesOutletAirHumRat = 0.00668435; // 
            state.dataWaterCoils->WaterCoil(iCoilID).DesInletWaterTemp = 20.0;

            SizeWaterCoil_NotInPlant(state, iCoilID);
        }

        iCoilID = IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex;
        if (iCoilID != 0) {
            EvapCond(iCoilID).IndirectVolFlowRate = IntegratedHeatPumps(DXCoilNum).DehumAirMasslowSize *
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                    .MSRatedAirVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel);

            SizeEvapCooler(state, iCoilID);
        }

        iCoilID = IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex;
        double dDesignAir = 0.0; 
        double dDesignWater = 0.0; 
        if (iCoilID != 0) {
            state.dataWaterCoils->WaterCoil(iCoilID).DesAirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
                    .MSRatedAirVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NormSpedLevel) 
                 *IntegratedHeatPumps(DXCoilNum).SupWaterCoilAirFRatio;
            state.dataWaterCoils->WaterCoil(iCoilID).MaxWaterVolFlowRate = 
                state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex)
                    .MSRatedWaterVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex).NumOfSpeeds) *
                IntegratedHeatPumps(DXCoilNum).SupWaterCoilWaterFRatio;

            dDesignAir = state.dataWaterCoils->WaterCoil(iCoilID).DesAirVolFlowRate; 
            dDesignWater = state.dataWaterCoils->WaterCoil(iCoilID).MaxWaterVolFlowRate;
            SizeWaterCoil(state, iCoilID);
        }

        IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = true;
    }

    void InitializeIHP(EnergyPlusData &state, int const DXCoilNum)
    {
        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("InitializeIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = 0.0;             // air loop mass flow rate [kg/s]
        IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0; // water loop mass flow rate [kg/s]
        IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;            // total cooling rate [w]
        IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;       // total water heating rate [w]
        IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;       // total space heating rate [w]
        IntegratedHeatPumps(DXCoilNum).TotalPower = 0.0;                  // total power consumption  [w]
        IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;             // total latent cooling rate [w]
        IntegratedHeatPumps(DXCoilNum).Qsource = 0.0;                     // source energy rate, [w]
        IntegratedHeatPumps(DXCoilNum).Energy = 0.0;                      // total electric energy consumption [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling = 0.0;      // total cooling energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating = 0.0;      // total heating energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating = 0.0; // total heating energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLatent = 0.0;                // total latent energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergySource = 0.0;                // total source energy
        IntegratedHeatPumps(DXCoilNum).TotalCOP = 0.0;
        IntegratedHeatPumps(DXCoilNum).SupHeatRate = 0.0; 

        ClearCoils(state, DXCoilNum); 
    }

    void UpdateIHP(EnergyPlusData &state, int const DXCoilNum)
    {
        using DataHVACGlobals::TimeStepSys;

        int VSCoilIndex(0);
        Real64 ReportingConstant(0.0);
        Real64 TotalDelivery(0.0);

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("UpdateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::SCMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            if (VSCoilIndex == 0) break; 
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;                             // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                             // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent;     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;             // source energy rate, [w]
            break;
        case IHPOperationMode::SHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            if (VSCoilIndex == 0) break; 
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                       // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;                                  // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;                 // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                        // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;                  // source energy rate, [w]
            break;
        case IHPOperationMode::DWHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            if (VSCoilIndex == 0) break; 
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                    // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                               // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;              // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal;            // source energy rate, [w]
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            if ((IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
                 && (true == IntegratedHeatPumps(DXCoilNum).bIsDWHSeparateunit)) {
                VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
                
                IntegratedHeatPumps(DXCoilNum).TotalCoolingRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
                
                IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;          // total space heating rate [w]
                IntegratedHeatPumps(DXCoilNum).TotalPower =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power; // total power consumption  [w]
                IntegratedHeatPumps(DXCoilNum).TotalLatentLoad =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent; // total latent cooling rate [w]
                IntegratedHeatPumps(DXCoilNum).Qsource = 0.0;                        // source energy rate, [w]     

                VSCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
                IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

                IntegratedHeatPumps(DXCoilNum).TotalPower = IntegratedHeatPumps(DXCoilNum).TotalPower + 
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;
                
            } else {
                VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
                if (VSCoilIndex == 0) break;
                IntegratedHeatPumps(DXCoilNum).TotalCoolingRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
                IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]
                IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;          // total space heating rate [w]
                IntegratedHeatPumps(DXCoilNum).TotalPower =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power; // total power consumption  [w]
                IntegratedHeatPumps(DXCoilNum).TotalLatentLoad =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent; // total latent cooling rate [w]
                IntegratedHeatPumps(DXCoilNum).Qsource = 0.0;                        // source energy rate, [w]            
            }            

            break;
        case IHPOperationMode::SCDWHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            if (VSCoilIndex == 0) break; 
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                             // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLatent;     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;             // source energy rate, [w]

            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
            if (VSCoilIndex == 0) break; 
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                       // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).Power;                 // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                        // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource;                  // source energy rate, [w]

            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

            break;
        case IHPOperationMode::IdleMode:
        default:
            break;
        }

        IntegratedHeatPumps(DXCoilNum).iWorkMode = int(IntegratedHeatPumps(DXCoilNum).CurMode); 
        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour();

        IntegratedHeatPumps(DXCoilNum).Energy = IntegratedHeatPumps(DXCoilNum).TotalPower * ReportingConstant; // total electric energy consumption
                                                                                                               // [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling =
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate * ReportingConstant; // total cooling energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating =
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate * ReportingConstant; // total heating energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating =
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate * ReportingConstant;                                     // total heating energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergyLatent = IntegratedHeatPumps(DXCoilNum).TotalLatentLoad * ReportingConstant; // total latent energy [J]
        IntegratedHeatPumps(DXCoilNum).EnergySource = IntegratedHeatPumps(DXCoilNum).Qsource * ReportingConstant;         // total source energy
        IntegratedHeatPumps(DXCoilNum).SupHeatEnergy = IntegratedHeatPumps(DXCoilNum).SupHeatRate * ReportingConstant; //supplemental heat energy

        if (IntegratedHeatPumps(DXCoilNum).TotalPower > 0.0) {
            TotalDelivery = IntegratedHeatPumps(DXCoilNum).TotalCoolingRate + IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate +
                            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate;
            IntegratedHeatPumps(DXCoilNum).TotalCOP = TotalDelivery / IntegratedHeatPumps(DXCoilNum).TotalPower;
        }

        Real64 Win = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).HumRat; 
        Real64 Tin = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).Temp; 
        Real64 Hin = 0.0; 
        Real64 Cpa = 1.0; 

        Real64 Wout = Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).HumRat;
        Real64 Tout = Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).Temp; 
        Real64 Hout = 0.0 ; 

        if (fabs(Win - Wout) < 1e-6) {
            IntegratedHeatPumps(DXCoilNum).SHR = 1.0;
        }
        else {
            Hin = Psychrometrics::PsyHFnTdbW(Tin, Win);     
            Cpa = Psychrometrics::PsyCpAirFnW((Win + Wout)/2.0);   
            Hout = Psychrometrics::PsyHFnTdbW(Tout, Wout);    
            IntegratedHeatPumps(DXCoilNum).SHR = Cpa * (Tin - Tout) / (Hin - Hout); 
        }
    }

    bool CheckLDWHCall(EnergyPlusData &state, int const DXCoilNum,
                       Real64 const SensLoad,  Real64 const LatentLoad )
    {
        using DataHVACGlobals::SmallLoad;

        bool bWHCall = true; 
        const int HeatOff= 0; 
        const int HeatOn = 1; 

        if (IntegratedHeatPumps(DXCoilNum).LDLoopChecked == false) {
            IntegratedHeatPumps(DXCoilNum).SaltConcentration = IntegratedHeatPumps(DXCoilNum).Concen_Set;
            IntegratedHeatPumps(DXCoilNum).TankSaltMass =
                IntegratedHeatPumps(DXCoilNum).SaltConcentration * IntegratedHeatPumps(DXCoilNum).TankLDMass;
            IntegratedHeatPumps(DXCoilNum).LDLoopChecked = true;             
        }
      
        double dRegenMass = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).DesiccantWaterLoss; 
        double dDeHumMass = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex)
                                .DesiccantWaterLoss; 
                
        if ((SensLoad > SmallLoad) || (IntegratedHeatPumps(DXCoilNum).TankLDMass == 0.0)) { // heating mode no change
            bWHCall = false;
        } else {

            double dMassLoss = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).DesiccantWaterLoss +
                state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).DesiccantWaterLoss; 
            IntegratedHeatPumps(DXCoilNum).TankLDMass = IntegratedHeatPumps(DXCoilNum).TankLDMass - dDeHumMass - dRegenMass;
            IntegratedHeatPumps(DXCoilNum).SaltConcentration =
                IntegratedHeatPumps(DXCoilNum).TankSaltMass / IntegratedHeatPumps(DXCoilNum).TankLDMass;

            double dMass = IntegratedHeatPumps(DXCoilNum).TankLDMass;
            double dConcen = IntegratedHeatPumps(DXCoilNum).SaltConcentration; 

            if (IntegratedHeatPumps(DXCoilNum).SaltConcentration >= 
                IntegratedHeatPumps(DXCoilNum).Concen_Set) {
                IntegratedHeatPumps(DXCoilNum).LDHeatMode = HeatOff; 
            }
            else if (IntegratedHeatPumps(DXCoilNum).SaltConcentration <= 
                (IntegratedHeatPumps(DXCoilNum).Concen_Set - IntegratedHeatPumps(DXCoilNum).Concen_band)) {
                IntegratedHeatPumps(DXCoilNum).LDHeatMode = HeatOn;
            } else {
                if (IntegratedHeatPumps(DXCoilNum).LDHeatMode == HeatOn)
                    IntegratedHeatPumps(DXCoilNum).LDHeatMode = HeatOn;
                else
                    IntegratedHeatPumps(DXCoilNum).LDHeatMode = HeatOff;
            }

            if (IntegratedHeatPumps(DXCoilNum).LDHeatMode == HeatOn) bWHCall = true;  
            else bWHCall = false;  
        }

        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).DesInletSolnConcentration = 
            IntegratedHeatPumps(DXCoilNum).SaltConcentration;

        state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).DesInletSolnConcentration = 
            IntegratedHeatPumps(DXCoilNum).SaltConcentration; 

        return (bWHCall); 
    }

    void UpdateIceStorage(EnergyPlusData &state, int const DXCoilNum)
    {
        using IceThermalStorage::IceTankReference; 
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using CurveManager::CurveValue;

        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling clear up function
        int CycFanCycCoil(1);                   // fan cycl manner place holder
        Real64 TankFraction = 0.0;
        Real64 ChillCapacity = 0.0; 
        Real64 dTinChiller = IntegratedHeatPumps(DXCoilNum).TchargeZeroFrac; 
        Real64 CurveVal = 0.0; 

        int iChillInNode = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex).WaterInletNodeNum;
        Real64 ChillerWFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex).DesignWaterMassFlowRate;
        Real64 DischargeCapacity = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).SupWaterCoilIndex).TotWaterCoolingCoilRate;
        IntegratedHeatPumps(DXCoilNum).DischargeCapacity = DischargeCapacity; 
                          
        if  (!state.dataGlobal->WarmupFlag) {
            //initialization
            SimVariableSpeedCoils(state, 
                BlankString, IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 
                1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
            // charge the storage tank
            
            TankFraction = IceThermalStorage::
                GetIceFraction(state, IntegratedHeatPumps(DXCoilNum).ChillTankType, IntegratedHeatPumps(DXCoilNum).ChillTankIndex);

            if (IntegratedHeatPumps(DXCoilNum).CurveChargeT != 0) {
                CurveVal = CurveValue(state, IntegratedHeatPumps(DXCoilNum).CurveChargeT, TankFraction);
                dTinChiller = dTinChiller + CurveVal ; 
            }

            ChillCapacity = 0.0; 

            int iIceMode = IntegratedHeatPumps(DXCoilNum).IceStoreMode;  

            if ((((TankFraction < IntegratedHeatPumps(DXCoilNum).ChargFracLow) 
                && (0 == iIceMode)) || ((TankFraction < 0.99) && (1 == iIceMode)))   
                && ((IntegratedHeatPumps(DXCoilNum).bIsChillerSeparateunit == true) || 
                    (IntegratedHeatPumps(DXCoilNum).CurMode == IHPOperationMode::IdleMode))) {
                iIceMode = 1; 
            } else {
                iIceMode = 0; 
            }
             
             if (iIceMode == 1)
             {                                
                Node(iChillInNode).MassFlowRate = ChillerWFlowRate; 
                Node(iChillInNode).Temp = dTinChiller;

                SimVariableSpeedCoils(state,
                                        BlankString,
                                        IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex,
                                        CycFanCycCoil,
                                        EMP1,
                                        EMP2,
                                        EMP3,
                                        1,
                                        1.0,
                                        IntegratedHeatPumps(DXCoilNum).ChillerRunSpeed,
                                        1.0,
                                        0.0,
                                        0.0,
                                        1.0);
                ChillCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).ChillerCoilIndex)
                                    .QLoadTotal;                                        
    
                IceThermalStorage::UpdateIceFractionIHP(state,
                                                        IntegratedHeatPumps(DXCoilNum).ChillTankType,
                                                        IntegratedHeatPumps(DXCoilNum).ChillTankIndex,
                                                        ChillCapacity,
                                                        DischargeCapacity);                
                IntegratedHeatPumps(DXCoilNum).IceStoreMode = 1;
            } else {
                IceThermalStorage::UpdateIceFractionIHP(state,
                                                        IntegratedHeatPumps(DXCoilNum).ChillTankType,
                                                        IntegratedHeatPumps(DXCoilNum).ChillTankIndex,
                                                        ChillCapacity,
                                                        DischargeCapacity);
                IntegratedHeatPumps(DXCoilNum).IceStoreMode = 0;
            }                        
        } 

        // Real64 CurTime =
       //(state.dataGlobal->HourOfDay - 1) + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone
        //+ DataHVACGlobals::SysTimeElapsed;
        //IntegratedHeatPumps(DXCoilNum).LastTime = CurTime; 
    }

    void DecideWorkMode(EnergyPlusData &state,
                        int const DXCoilNum,
                        Real64 const SensLoad,  // Sensible demand load [W]
                        Real64 const LatentLoad // Latent demand load [W]
                        )                       // shall be called from a air loop parent
    {
        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   March 2016
        //       RE-ENGINEERED  Sept 2020

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determine the IHP working mode in the next time step,
        // it should be called by an air loop parent object, when FirstHVACIteration == true

        // Using/Aliasing
        using DataEnvironment::OutDryBulbTemp;
        using DataHVACGlobals::SmallLoad;
        using DataHVACGlobals::TimeStepSys;
        using VariableSpeedCoils::IsGridResponsiveMode;

        Real64 MyLoad(0.0);
        Real64 WHHeatTimeSav(0.0); // time accumulation for water heating
        Real64 WHHeatVolSave(0.0); // volume accumulation for water heating

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("DecideWorkMode: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(state, DXCoilNum);

        //no water heating elements
        if ((IntegratedHeatPumps(DXCoilNum).DWHCoilIndex == 0) && (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex == 0)) {
            if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // space cooling mode
            {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCMode;
            } else if (SensLoad > SmallLoad) {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHMode;
            } else {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            }

            return; 
        }

        // decide working mode at the first moment
        // check if there is a water heating call
        IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
        IntegratedHeatPumps(DXCoilNum).CheckWHCall = true; // set checking flag
        if (IntegratedHeatPumps(DXCoilNum).WHtankID == 0)  // not initialized yet
        {
            IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
            if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) {
                int hpIDX =
                    WaterThermalTanks::getHPTankIDX(state, IntegratedHeatPumps(DXCoilNum).WHtankName, IntegratedHeatPumps(DXCoilNum).WHtankID);
                auto &HPWH = state.dataWaterThermalTanks->HPWaterHeater(hpIDX);
                int tankIDX = HPWH.WaterHeaterTankNum;
                auto &tank = state.dataWaterThermalTanks->WaterThermalTank(tankIDX);
                
                tank.scanPlantLoopsFlag = false; 
                tank.SetLoopIndexFlag = false; 
                tank.IsLiquidDesiccantHP = true; 

                tank.callerLoopNum = IntegratedHeatPumps(DXCoilNum).LoopNum;

                IntegratedHeatPumps(DXCoilNum).TankLDMass = tank.Volume * 1000.0; 
            }
        } else  {

            if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT)
                IntegratedHeatPumps(DXCoilNum).CheckWHCall = false; // clear checking for desiccant HP

            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(state, DXCoilNum, 1.0, 1.0, true) * 987.0; // 987.0 water density at 60 C.
            Node(IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum).Temp = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp;

            int tankType = IntegratedHeatPumps(DXCoilNum).WHtankType;

            if ((tankType == DataPlant::TypeOf_WtrHeaterMixed) || (tankType == DataPlant::TypeOf_WtrHeaterStratified) ||
                (tankType == DataPlant::TypeOf_ChilledWaterTankMixed) || (tankType == DataPlant::TypeOf_ChilledWaterTankStratified)) {
                
                int tankIDX = WaterThermalTanks::getTankIDX(state, IntegratedHeatPumps(DXCoilNum).WHtankName, IntegratedHeatPumps(DXCoilNum).WHtankID);
                auto &tank = state.dataWaterThermalTanks->WaterThermalTank(tankIDX);
                tank.callerLoopNum = IntegratedHeatPumps(DXCoilNum).LoopNum;

                PlantLocation A(0, 0, 0, 0);

                if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) {
                    tank.UseMassFlowRate =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum).MassFlowRate;
                    tank.UseInletTemp =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterOutletNodeNum).Temp;
                    tank.SourceInletTemp =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterOutletNodeNum).Temp;
                    if (Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterInletNodeNum).MassFlowRate > 0.0) {
                        tank.SourceMassFlowRate =
                            Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterInletNodeNum).MassFlowRate;
                    } else {
                        tank.SourceMassFlowRate = 0.0;
                    }
                }

                tank.simulate(state, A, true, MyLoad, true);

                tank.callerLoopNum = 0;

            } else if (tankType == DataPlant::TypeOf_HeatPumpWtrHeaterPumped || tankType == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {

                int hpIDX = WaterThermalTanks::getHPTankIDX(state, IntegratedHeatPumps(DXCoilNum).WHtankName, IntegratedHeatPumps(DXCoilNum).WHtankID);
                auto &HPWH = state.dataWaterThermalTanks->HPWaterHeater(hpIDX);
                int tankIDX = HPWH.WaterHeaterTankNum;
                auto &tank = state.dataWaterThermalTanks->WaterThermalTank(tankIDX);
                tank.callerLoopNum = IntegratedHeatPumps(DXCoilNum).LoopNum;
                IntegratedHeatPump::IntegratedHeatPumps(DXCoilNum).WHtankType = tankType;

                PlantLocation A(0, 0, 0, 0);

                int bakHPNum = tank.HeatPumpNum; 
                Real64 HPSet = HPWH.SetPointTemp; 

                if ((IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) && 
                (IntegratedHeatPumps(DXCoilNum).CurMode == IHPOperationMode::SCMode)) {
                    tank.UseMassFlowRate =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum).MassFlowRate;
                    tank.UseInletTemp =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterOutletNodeNum).Temp;
                    tank.SourceInletTemp =
                        Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterOutletNodeNum).Temp;
                    if (Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterInletNodeNum).MassFlowRate > 0.0) {
                        tank.SourceMassFlowRate =
                            Node(state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).WaterInletNodeNum).MassFlowRate;
                        tank.HeatPumpNum = 0; 
                    } else {
                        tank.SourceMassFlowRate = 0.0; 
                    }
                    
                    //calculate tank eneryg balance
                    tank.simulate(state, A, true, MyLoad, true);

                    tank.HeatPumpNum = bakHPNum; 

                } else {

                    if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) {
                        HPWH.SetPointTemp = IntegratedHeatPumps(DXCoilNum).TregenTarget; 

                    }

                    HPWH.simulate(state, A, true, MyLoad, true);

                    HPWH.SetPointTemp = HPSet; 
                }
                           
                tank.callerLoopNum = 0;
            }
        }

        IntegratedHeatPumps(DXCoilNum).CheckWHCall = false; // clear checking flag

        if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) {
            IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = CheckLDWHCall(state, DXCoilNum, SensLoad, LatentLoad); 
        } else {
            // keep the water heating time and volume history
            WHHeatTimeSav = IntegratedHeatPumps(DXCoilNum).SHDWHRunTime;
            if (IHPOperationMode::SCDWHMode == IntegratedHeatPumps(DXCoilNum).CurMode) {
                WHHeatVolSave = IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol + Node(IntegratedHeatPumps(DXCoilNum).WaterTankoutNod).MassFlowRate /
                                                                                       983.0 * TimeStepSys *
                                                                                       DataGlobalConstants::SecInHour(); // 983 - water density at 60 C
            } else {
                WHHeatVolSave = 0.0;
            }        
        }

        // clear the accumulation amount for other modes
        IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = 0.0;
        IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = 0.0;

        if ((!IntegratedHeatPumps(DXCoilNum).IsWHCallAvail) || 
            (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex))) // no water heating call
        {
            if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // space cooling mode
            {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCMode;
            } else if (SensLoad > SmallLoad) {
                if ((IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow) &&
                    (OutDryBulbTemp > IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow)) // used for cooling season, avoid heating after SCWH mode
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
                else
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHMode;
            } else {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            }
        }
        // below has water heating calls
        else {
            if (IntegratedHeatPumps(DXCoilNum).StorageType == IHPStorageType::LIQUIDDESICCANT) {//liquid desiccant storage
                
                if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // simultaneous SC and WH calls
                {
                    if ((Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TlimitSCWH) && 
                        (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)) 
                    {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchSCMode;
                    } 
                    else if ((Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp < IntegratedHeatPumps(DXCoilNum).TlimitSCDWH) &&
                        (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)) 
                    {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCDWHMode;
                    }                    
                    else 
                    {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchSCMode;
                    } 
                } 
                else {
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                }     
            } else {//hot water storage
                if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // simultaneous SC and WH calls
                {
                    if ((WHHeatVolSave < IntegratedHeatPumps(DXCoilNum).WaterVolSCDWH) &&
                        (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)) // small water heating amount
                    {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCDWHMode;
                        IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = WHHeatVolSave;
                    } else {
                        if (1 == IntegratedHeatPumps(DXCoilNum).ModeMatchSCWH) // water heating priority
                            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
                        else // space cooling priority
                            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchSCMode;

                        if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex == 0) IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                    };

                } else if ((IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow) &&
                           (OutDryBulbTemp > IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow)) // over-cooling allowed, water heating priority
                {
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
                    if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex == 0) IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                } else if ((IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumps(DXCoilNum).TindoorWHHighPriority) &&
                           (OutDryBulbTemp > IntegratedHeatPumps(DXCoilNum).TambientWHHighPriority)) // ignore space heating request
                {
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                } else if ((SensLoad > SmallLoad) && (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)) {
                    IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = WHHeatTimeSav + TimeStepSys * DataGlobalConstants::SecInHour();

                    if (IntegratedHeatPumps(DXCoilNum).SHDWHRunTime > IntegratedHeatPumps(DXCoilNum).TimeLimitSHDWH) {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOnMode;
                    } else {
                        IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOffMode;
                    };
                } else {
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                }       
            }
        }
        
        // clear up, important
        //ClearCoils(state, DXCoilNum);
    }

    void ClearCoils(EnergyPlusData &state, int const DXCoilNum)
    {
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using EvaporativeCoolers::EvapCond; 
        using EvaporativeCoolers::SimEvapCooler; 
        using WaterCoils::SimulateWaterCoilComponents;

        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling clear up function
        int CycFanCycCoil(1);                   // fan cycl manner place holder

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("ClearCoils: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        // clear up
        if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) && 
            (IntegratedHeatPumps(DXCoilNum).DehumPlace == DehumPlacement::UPSTRREAM)) {
            /*state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
            int iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
            int iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
            Node(iWNod).MassFlowRate = 0.0;
            Node(iWNod).MassFlowRateMax = 0.0;
            Node(iANod).MassFlowRate = 0.0;
            Node(iANod).MassFlowRateMax = 0.0;*/

            SimulateWaterCoilComponents(state,
                                        IntegratedHeatPumps(DXCoilNum).LDDehumCoilName,
                                        false,
                                        IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                        0.0,
                                        CycFanCycCoil,
                                        0.0);
        }

        if (IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

        if (IntegratedHeatPumps(DXCoilNum).SCCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
            SimVariableSpeedCoils(
                state, BlankString, IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        if (IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex != IntegratedHeatPumps(DXCoilNum).SCCoilIndex)
            SimVariableSpeedCoils(
                state, BlankString, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
        
        if (IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex != 0) {
            /*int iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).AirInletNodeNum;
            Node(iANod).MassFlowRate = 0.0;
            Node(iANod).MassFlowRateMax = 0.0;*/ 

            SimulateWaterCoilComponents(
                state, IntegratedHeatPumps(DXCoilNum).LDRegenCoilName, false, IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex, 
                0.0, CycFanCycCoil, 0.0); 

            Node(IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum).MassFlowRate = 0.0;
            state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDRegenCoilIndex).MaxWaterMassFlowRate = 0.0;
        }

        if ((IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex != 0) 
            && (IntegratedHeatPumps(DXCoilNum).DehumPlace != DehumPlacement::UPSTRREAM))
        {
            /*state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).InletWaterMassFlowRate = 0.0;
            int iWNod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).WaterInletNodeNum;
            int iANod = state.dataWaterCoils->WaterCoil(IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex).AirInletNodeNum;
            Node(iWNod).MassFlowRate = 0.0;
            Node(iWNod).MassFlowRateMax = 0.0;
            Node(iANod).MassFlowRate = 0.0;
            Node(iANod).MassFlowRateMax = 0.0;*/

            SimulateWaterCoilComponents(
                state, IntegratedHeatPumps(DXCoilNum).LDDehumCoilName, false, IntegratedHeatPumps(DXCoilNum).LDDehumCoilIndex,
                                        0.0, CycFanCycCoil, 0.0);
        }

        Real64 MassFlowBack = 0.0; 
        if (IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex != 0) {
            MassFlowBack = Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate; 
            Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = 0.0;
            SimEvapCooler(state, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilName, IntegratedHeatPumps(DXCoilNum).EvapCoolCoilIndex, 0.0);
            Node(IntegratedHeatPumps(DXCoilNum).AirCoolOutletNodeNum).MassFlowRate = MassFlowBack; 
        }
                           
        if (IntegratedHeatPumps(DXCoilNum).SHCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

        if (IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex != IntegratedHeatPumps(DXCoilNum).SHCoilIndex)
            SimVariableSpeedCoils(
                state, BlankString, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

        if (IntegratedHeatPumps(DXCoilNum).DWHCoilIndex != 0)
        SimVariableSpeedCoils(state,
            BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

        IntegratedHeatPumps(DXCoilNum).CompressorPartLoadRatio = 0.0;

        return;
    }

    IHPOperationMode GetCurWorkMode(EnergyPlusData &state, int const DXCoilNum)
    {
        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetCurWorkMode: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(state, DXCoilNum);

        return (IntegratedHeatPumps(DXCoilNum).CurMode);
    }

    bool IHPInModel(EnergyPlusData &state)
    {
        if (GetCoilsInputFlag) {
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }
        return !IntegratedHeatPumps.empty();
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        IndexNum = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);

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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).AirCoolInletNodeNum;
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).ODAirInletNodeNum;
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).ODAirOutletNodeNum;
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        int WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            // this will be called by HPWH parent
            if (IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0)
                PLRNumber = GetVSCoilPLFFPLR(state, IntegratedHeatPumps(WhichCoil).DWHCoilType, IntegratedHeatPumps(WhichCoil).DWHCoilName, ErrorsFound);
            else
                PLRNumber = GetVSCoilPLFFPLR(state, IntegratedHeatPumps(WhichCoil).SCWHCoilType, IntegratedHeatPumps(WhichCoil).SCWHCoilName, ErrorsFound);
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        int WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {

            if (IntegratedHeatPumps(WhichCoil).IHPCoilsSized == false) SizeIHP(state, WhichCoil);

            if (IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0) {
                CoilCapacity =
                    GetCoilCapacityVariableSpeed(state, IntegratedHeatPumps(WhichCoil).DWHCoilType, IntegratedHeatPumps(WhichCoil).DWHCoilName, ErrorsFound);
            } else {
                CoilCapacity = GetCoilCapacityVariableSpeed(state,
                    IntegratedHeatPumps(WhichCoil).SCWHCoilType, IntegratedHeatPumps(WhichCoil).SCWHCoilName, ErrorsFound);
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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetLowSpeedNumIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
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
            SpeedNum = IntegratedHeatPumps(DXCoilNum).MinSpedSCWH;
            break;
        case IHPOperationMode::SCDWHMode:
            SpeedNum = IntegratedHeatPumps(DXCoilNum).MinSpedSCDWH;
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            SpeedNum = IntegratedHeatPumps(DXCoilNum).MinSpedSHDWH;
            break;
        default:
            SpeedNum = 1;
            break;
        }

        return (SpeedNum);
    }

    int GetMaxSpeedNumIHP(EnergyPlusData &state, int const DXCoilNum)
    {
        using VariableSpeedCoils::IsGridResponsiveMode;
        using VariableSpeedCoils::CompareGridSpeed; 

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetMaxSpeedNumIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        int SpeedNum(0);

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCMode:
            if(true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex)) {
                SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex).NumOfSpeeds;
                SpeedNum = CompareGridSpeed(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex, SpeedNum); 
            }                
            else if (IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment == true)
                SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex).NumOfSpeeds;
            else SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SHMode:
            if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex)) {
                SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex).NumOfSpeeds;
                SpeedNum = CompareGridSpeed(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex, SpeedNum);            
            }
            else SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).NumOfSpeeds;

            break;
        case IHPOperationMode::DWHMode:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCDWHMode:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).NumOfSpeeds;
            break;
        default:
            SpeedNum = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        }

        return (SpeedNum);
    }

    Real64 GetAirVolFlowRateIHP(EnergyPlusData &state, int const DXCoilNum,
                                int const SpeedNum,
                                Real64 const SpeedRatio,
                                bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    )
    {
        using VariableSpeedCoils::IsGridResponsiveMode;

        int IHPCoilIndex(0);
        Real64 AirVolFlowRate(0.0);
        Real64 FlowScale(1.0);
        bool IsResultFlow(false); // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetAirVolFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        if (!IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

        FlowScale = 0.0;
        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            break;
        case IHPOperationMode::SCMode:
            if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex))
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex;
            else if (IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment == true)
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex;
            else IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            if (!IsCallbyWH) // call from air loop
            {
                FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            }

            break;
        case IHPOperationMode::SHMode:
            if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex))
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex;
            else IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            if (!IsCallbyWH) // call from air loop
            {
                FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            }
            break;
        case IHPOperationMode::DWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            FlowScale = 1.0;
            if (!IsCallbyWH) {
                FlowScale = 0.0;
            };
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (IsCallbyWH) // call from water loop
            {
                IsResultFlow = true;
                AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (!IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirVolFlowRate;
            }
            break;
        default:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            FlowScale = 0.0;
            break;
        }

        if (!IsResultFlow) {
            if (SpeedNum == 0) {
                if (true == IsGridResponsiveMode(state, IHPCoilIndex)) {
                    AirVolFlowRate = 
                        state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).NormSpedLevel);
                } else {
                    AirVolFlowRate = 0.0;
                }
            }
            else if (1 == SpeedNum)
                AirVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum);
            else
                AirVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum) +
                                 (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum - 1);

            AirVolFlowRate = AirVolFlowRate * FlowScale;
        }

        if (AirVolFlowRate > IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow) AirVolFlowRate = IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow;
        if (AirVolFlowRate > IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow) AirVolFlowRate = IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow;

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
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetWaterVolFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        if (!IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
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
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
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

    Real64 GetAirMassFlowRateIHP(EnergyPlusData &state, int const DXCoilNum,
                                 int const SpeedNum,
                                 Real64 const SpeedRatio,
                                 bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    )
    {
        using VariableSpeedCoils::IsGridResponsiveMode;

        int IHPCoilIndex(0);
        Real64 AirMassFlowRate(0.0);
        Real64 FlowScale(1.0);
        bool IsResultFlow(false);   // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated
        Real64 WaterDensity(986.0); // standard water density at 60 C

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput(state);
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError(state,
                           format("GetAirMassFlowRateIHP: Invalid CompIndex passed={}, Number of Integrated HPs={}, IHP name=AS-IHP",
                                  DXCoilNum,
                                  IntegratedHeatPumps.size()));
        }

        if (!IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) SizeIHP(state, DXCoilNum);

        FlowScale = 0.0;
        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            AirMassFlowRate = 0.0;
            break;
        case IHPOperationMode::SCMode:
            if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex))
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex;
            else if (IntegratedHeatPumps(DXCoilNum).bIsEnhanchedDumLastMoment == true)
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).EnDehumCoilIndex;
            else IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            if (!IsCallbyWH) {
                FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            } else {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::SHMode:
            if (true == IsGridResponsiveMode(state, IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex))
                IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).GridSHCoilIndex;
            else IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;

            if (!IsCallbyWH) {
                FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            } else {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::DWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            FlowScale = 1.0;
            if (!IsCallbyWH) {
                FlowScale = 0.0;
            };
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (!IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop;
            }
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(state, DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        default:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            FlowScale = 0.0;
            break;
        }

        if (!IsResultFlow) {

            if (SpeedNum == 0) {
                if (true == IsGridResponsiveMode(state, IHPCoilIndex)) 
                {
                    AirMassFlowRate = 
                        state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).NormSpedLevel);
                } 
                else
                {
                    AirMassFlowRate = 0.0; 
                }
            }
            else if (SpeedNum == 1) {
                AirMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum);
            } else {
                AirMassFlowRate = SpeedRatio * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum) +
                                  (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum - 1);
            }

            AirMassFlowRate = AirMassFlowRate * FlowScale;
        }

        if (AirMassFlowRate > IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow) {
            AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow;
        }
        if (AirMassFlowRate > IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow) {
            AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow;
        }

        // set max air flow rate
        Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRateMax = AirMassFlowRate;
        Node(IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum).MassFlowRateMax = AirMassFlowRate;
        Node(IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum).MassFlowRateMax = AirMassFlowRate;

        return AirMassFlowRate;
    }

     int GetGridLoadCtrlModeIHP(EnergyPlusData &state, const int DXCoilNum // coil index No
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   09/2020
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        //      PURPOSE OF THIS FUNCTION:
        //      check grid responsive mode
         using VariableSpeedCoils::GetGridLoadCtrlMode; 

         int iMode = GetGridLoadCtrlMode(state, IntegratedHeatPumps(DXCoilNum).GridSCCoilIndex); 

        return (iMode);
    }

} // namespace IntegratedHeatPump

} // namespace EnergyPlus
