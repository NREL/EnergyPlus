// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <IntegratedHeatPump.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>
#include <WaterThermalTanks.hh>

namespace EnergyPlus {

namespace IntegratedHeatPump {

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using namespace DataGlobals;
    using General::RoundSigDigits;

    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    bool GetCoilsInputFlag(true);

    // Object Data
    Array1D<IntegratedHeatPumpData> IntegratedHeatPumps;

    void clear_state()
    {
        IntegratedHeatPumps.deallocate();
    }

    void SimIHP(std::string const &CompName,              // Coil Name
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
                bool const EP_UNUSED(FirstHVACIteration), // TRUE if First iteration of simulation
                Optional<Real64 const> OnOffAirFlowRat    // ratio of comp on to comp off air flow rate
    )
    {

        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   March 2016
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages variable-speed integrated Air source heat pump simulation.

        // Using/Aliasing
        using General::TrimSigDigits;
        using VariableSpeedCoils::InitVarSpeedCoil;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::UpdateVarSpeedCoil;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum(0); // The IHP No that you are currently dealing with
        Real64 waterMassFlowRate(0);
        Real64 airMassFlowRate(0);

        // Obtains and Allocates ASIHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (CompIndex == 0) {
            DXCoilNum = UtilityRoutines::FindItemInList(CompName, IntegratedHeatPumps);
            if (DXCoilNum == 0) {
                ShowFatalError("Integrated Heat Pump not found=" + CompName);
            }
            CompIndex = DXCoilNum;
        } else {
            DXCoilNum = CompIndex;
            if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
                ShowFatalError("SimIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                               ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + CompName);
            }
            if (!CompName.empty() && CompName != IntegratedHeatPumps(DXCoilNum).Name) {
                ShowFatalError("SimIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) + ", Integrated HP name=" + CompName +
                               ", stored Integrated HP Name for that index=" + IntegratedHeatPumps(DXCoilNum).Name);
            }
        };

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

        InitializeIHP(DXCoilNum);

        airMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum).MassFlowRate;
        waterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
        IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate = airMassFlowRate;

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::SCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::SHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
            }
            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            break;
        case IHPOperationMode::DWHMode:
            if (IsCallbyWH) // process when called from water loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
                // IntegratedHeatPumps(DXCoilNum).TotalHeatingEnergyRate =
                // VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).TotalHeatingEnergyRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;

            break;
        case IHPOperationMode::SCWHMatchWHMode:
            if (IsCallbyWH) // process when called from water loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = airMassFlowRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SCDWHMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad,
                                      OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad,
                                      OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            if (!IsCallbyWH) // process when called from air loop
            {
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                      FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad,
                                      OnOffAirFlowRat);
                SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                      HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, SensLoad, LatentLoad,
                                      OnOffAirFlowRat);

                IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = airMassFlowRate;
            }

            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate;
            break;
        case IHPOperationMode::IdleMode:
        default: // clear up
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                  HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CyclingScheme, MaxONOFFCyclesperHour,
                                  HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant,
                                  FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
            IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop = 0.0;
            IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop = 0.0;
            break;
        }

        UpdateIHP(DXCoilNum);
    }

    void GetIHPInput()
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
        using namespace OutputReportPredefined;
        using General::TrimSigDigits;
        using VariableSpeedCoils::GetCoilIndexVariableSpeed;
        using VariableSpeedCoils::VarSpeedCoil;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIHPInput: "); // include trailing blank space

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

        NumASIHPs = inputProcessor->getNumObjectsFound("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE");
        DXCoilNum = 0;

        if (NumASIHPs <= 0) return;

        // Allocate Arrays
        IntegratedHeatPumps.allocate(NumASIHPs);

        // air-source integrated heat pump
        inputProcessor->getObjectDefMaxArgs("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", NumParams, NumAlphas, NumNums);
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

            inputProcessor->getObjectItem(CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks,
                                          lAlphaBlanks, cAlphaFields, cNumericFields);
            VerifyUniqueCoilName(CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

            IntegratedHeatPumps(DXCoilNum).Name = AlphArray(1);
            IntegratedHeatPumps(DXCoilNum).IHPtype = "AIRSOURCE_IHP";

            // AlphArray( 2 ) is the water sensor node

            IntegratedHeatPumps(DXCoilNum).SCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCCoilName = AlphArray(3);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SHCoilName = AlphArray(4);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SHCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).DWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).DWHCoilName = AlphArray(5);
            Coiltype = IntegratedHeatPumps(DXCoilNum).DWHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).DWHCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).DWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SCWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCWHCoilName = AlphArray(6);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCWHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCWHCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType = "COIL:COOLING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName = AlphArray(7);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName = AlphArray(8);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).bIsDesuperheater = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType = "COIL:HEATING:DX:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName = AlphArray(9);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                }
            }

            IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
            IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName = AlphArray(10);
            Coiltype = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType;
            CoilName = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName;

            ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                ErrorsFound = true;
            } else {
                errFlag = false;
                IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                    ErrorsFound = true;
                } else {
                    VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).bIsDesuperheater = true;
                }
            }

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
            InNode = VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
            OutNode = VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);

            IntegratedHeatPumps(DXCoilNum).AirCoolInletNodeNum = InNode;
            IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum = OutNode;

            TestCompSet(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", InNodeName, OutNodeName, "Cooling Air Nodes");
            RegisterNodeConnection(InNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", "Inlet", 1,
                                   ObjectIsNotParent, ErrorsFound);
            RegisterNodeConnection(OutNode, NodeID(OutNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", "Outlet", 1,
                                   ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", IntegratedHeatPumps(DXCoilNum).SCCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCCoilType, IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCCoilType, IntegratedHeatPumps(DXCoilNum).SCCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);

            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil", IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCWHCoilType, IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCWHCoilType, IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);

            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Cooling Coil",
                          IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName, InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);

            // heating coil air node connections
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;

            InNode = IntegratedHeatPumps(DXCoilNum).AirHeatInletNodeNum;
            OutNode = VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            IntegratedHeatPumps(DXCoilNum).AirOutletNodeNum = OutNode;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);
            if (VarSpeedCoil(ChildCoilIndex).AirInletNodeNum != InNode) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + "- cooling coil outlet mismatches heating coil inlet" +
                                  ".");
                ErrorsFound = true;
            }
            TestCompSet(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", InNodeName, OutNodeName, "Heating Air Nodes");
            RegisterNodeConnection(InNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", "Inlet", 1,
                                   ObjectIsNotParent, ErrorsFound);
            RegisterNodeConnection(OutNode, NodeID(OutNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", "Outlet", 1,
                                   ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil", IntegratedHeatPumps(DXCoilNum).SHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SHCoilType, IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SHCoilType, IntegratedHeatPumps(DXCoilNum).SHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);

            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Heating Coil",
                          IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName, InNodeName,
                          OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);

            // water node connections
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;

            InNode = VarSpeedCoil(ChildCoilIndex).WaterInletNodeNum;
            OutNode = VarSpeedCoil(ChildCoilIndex).WaterOutletNodeNum;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);
            IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum = InNode;
            IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum = OutNode;
            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }

            TestCompSet(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", InNodeName, OutNodeName, "Water Nodes");
            RegisterNodeConnection(InNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", "Inlet", 1,
                                   ObjectIsNotParent, ErrorsFound);
            RegisterNodeConnection(OutNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", "Outlet", 1,
                                   ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", IntegratedHeatPumps(DXCoilNum).SCWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCWHCoilType, IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                       "Internal", 2, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCWHCoilType, IntegratedHeatPumps(DXCoilNum).SCWHCoilName,
                                       "Internal", 2, ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, "Internal", 2, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, "Internal", 2, ObjectIsNotParent, ErrorsFound);

            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, "Internal", 2, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, "Internal", 2, ObjectIsNotParent, ErrorsFound);

            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).WaterOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken water node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).DWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Water Coil", IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).DWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).DWHCoilType, IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                       "Internal", 2, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).DWHCoilType, IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                       "Internal", 2, ObjectIsNotParent, ErrorsFound);

            IntegratedHeatPumps(DXCoilNum).WaterTankoutNod = GetOnlySingleNode(AlphArray(2), ErrorsFound, CurrentModuleObject, AlphArray(1),
                                                                               NodeType_Water, NodeConnectionType_Sensor, 2, ObjectIsNotParent);

            // outdoor air node connections for water heating coils
            // DWH, SCDWH, SHDWH coils have the same outdoor air nodes
            ChildCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            InNode = VarSpeedCoil(ChildCoilIndex).AirInletNodeNum;
            OutNode = VarSpeedCoil(ChildCoilIndex).AirOutletNodeNum;
            InNodeName = NodeID(InNode);
            OutNodeName = NodeID(OutNode);
            IntegratedHeatPumps(DXCoilNum).ODAirInletNodeNum = InNode;
            IntegratedHeatPumps(DXCoilNum).ODAirOutletNodeNum = OutNode;
            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).AirOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }

            TestCompSet(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", InNodeName, OutNodeName, "Outdoor Air Nodes");
            RegisterNodeConnection(InNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", "Inlet", 1,
                                   ObjectIsNotParent, ErrorsFound);
            RegisterNodeConnection(OutNode, NodeID(InNode), CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", "Outlet", 1,
                                   ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", IntegratedHeatPumps(DXCoilNum).DWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).DWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).DWHCoilType, IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).DWHCoilType, IntegratedHeatPumps(DXCoilNum).DWHCoilName,
                                       "Internal", 1, ObjectIsNotParent, ErrorsFound);

            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);

            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum = InNode;
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum = OutNode;
            if ((VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirInletNodeNum != InNode) ||
                (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).AirOutletNodeNum != OutNode)) {
                ShowContinueError("Mistaken air node connection: " + CurrentModuleObject + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName +
                                  "-wrong coil node names.");
                ErrorsFound = true;
            }
            SetUpCompSets(CurrentModuleObject, IntegratedHeatPumps(DXCoilNum).Name + " Outdoor Coil", IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                          IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, InNodeName, OutNodeName);
            OverrideNodeConnectionType(InNode, InNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);
            OverrideNodeConnectionType(OutNode, OutNodeName, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilType,
                                       IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName, "Internal", 1, ObjectIsNotParent, ErrorsFound);

            IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = false;
            IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale = 1.0; // scale coil flow rates to match the parent fan object
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::IdleMode;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirMassFlow = 1e10;
            IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow = 1e10;
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination.");
        } else {
            // set up output variables, not reported in the individual coil models

            //				TODO: Figure out how to get enum class to work with SetupOutputVariable
            //				Setup Output Variable( "Operation Mode []",
            //				                     static_cast< int >( IntegratedHeatPumps( DXCoilNum ).CurMode ),
            //				                     "System", "Average",
            //				                     IntegratedHeatPumps( DXCoilNum ).Name );
            SetupOutputVariable("Integrated Heat Pump Air Loop Mass Flow Rate", OutputProcessor::Unit::kg_s,
                                IntegratedHeatPumps(DXCoilNum).AirLoopFlowRate, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Condenser Water Mass Flow Rate", OutputProcessor::Unit::kg_s,
                                IntegratedHeatPumps(DXCoilNum).TankSourceWaterMassFlowRate, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Total Cooling Rate", OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalCoolingRate, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Heating Rate", OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Water Heating Rate", OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Electric Power", OutputProcessor::Unit::W, IntegratedHeatPumps(DXCoilNum).TotalPower, "System",
                                "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Latent Cooling Rate", OutputProcessor::Unit::W,
                                IntegratedHeatPumps(DXCoilNum).TotalLatentLoad, "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Source Heat Transfer Rate", OutputProcessor::Unit::W, IntegratedHeatPumps(DXCoilNum).Qsource,
                                "System", "Average", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump COP", OutputProcessor::Unit::None, IntegratedHeatPumps(DXCoilNum).TotalCOP, "System", "Average",
                                IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Electric Energy", OutputProcessor::Unit::J, IntegratedHeatPumps(DXCoilNum).Energy, "System",
                                "Summed", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Total Cooling Energy", OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalCooling, "System", "Summed", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Heating Energy", OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalHeating, "System", "Summed", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Water Heating Energy", OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLoadTotalWaterHeating, "System", "Summed", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Air Latent Cooling Energy", OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergyLatent, "System", "Summed", IntegratedHeatPumps(DXCoilNum).Name);
            SetupOutputVariable("Integrated Heat Pump Source Heat Transfer Energy", OutputProcessor::Unit::J,
                                IntegratedHeatPumps(DXCoilNum).EnergySource, "System", "Summed", IntegratedHeatPumps(DXCoilNum).Name);
        }
    }

    void SizeIHP(int const DXCoilNum)
    {
        using DataSizing::AutoSize;
        using General::TrimSigDigits;
        using VariableSpeedCoils::SetVarSpeedCoilData;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::SizeVarSpeedCoil;
        using VariableSpeedCoils::VarSpeedCoil;

        static bool ErrorsFound(false); // If errors detected in input
        Real64 RatedCapacity(0.0);      // rated building cooling load

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        };

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("SizeIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized) {
            return;
        }

        // associate SC coil with SH coil
        SetVarSpeedCoilData(IntegratedHeatPumps(DXCoilNum).SCCoilIndex, ErrorsFound, _, IntegratedHeatPumps(DXCoilNum).SHCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: Could not match cooling coil\"" + IntegratedHeatPumps(DXCoilNum).SCCoilName + "\" with heating coil=\"" +
                            IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
            ErrorsFound = false;
        };

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex); // size cooling coil
        if (ErrorsFound) {
            ShowFatalError("SizeIHP: failed to size SC coil\"" + IntegratedHeatPumps(DXCoilNum).SCCoilName + "\"");
            ErrorsFound = false;
        } else {
            RatedCapacity = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).RatedCapCoolTotal;
        };

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex); // size heating coil
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SH coil\"" + IntegratedHeatPumps(DXCoilNum).SHCoilName + "\"");
            ErrorsFound = false;
        };

        // pass SC coil capacity to SCDWH cool coil
        if (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal == AutoSize) {
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal = RatedCapacity;
        };

        // associate SCDWH air coil to SHDWH air coil
        SetVarSpeedCoilData(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, ErrorsFound, _, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex);

        // size SCDWH air coil
        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SCDWH cooling coil\"" + IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilName + "\"");
            ErrorsFound = false;
        };

        // size SHDWH air coil
        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SHDWH heating coil\"" + IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilName + "\"");
            ErrorsFound = false;
        };

        // size the water coils below
        // size SCWH water coil
        if (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH == AutoSize) {
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCapWH =
                RatedCapacity / (1.0 - 1.0 / VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).RatedCOPHeat);
        }

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SCWH coil\"" + IntegratedHeatPumps(DXCoilNum).SCWHCoilName + "\"");
            ErrorsFound = false;
        };

        // size DWH water coil
        if (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH == AutoSize) {
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).RatedCapWH = RatedCapacity;
        }

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size DWH coil\"" + IntegratedHeatPumps(DXCoilNum).DWHCoilName + "\"");
            ErrorsFound = false;
        };

        // size SCDWH water coil
        if (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH == AutoSize) {
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH = RatedCapacity * 0.13;
        }

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex);
        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SCDWH water heating coil\"" + IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilName + "\"");
            ErrorsFound = false;
        };

        // size SHDWH water coil
        if (VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH == AutoSize) {
            VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH = RatedCapacity * 0.1;
        }

        SizeVarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex);

        if (ErrorsFound) {
            ShowSevereError("SizeIHP: failed to size SHDWH water heating coil\"" + IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilName + "\"");
            ErrorsFound = false;
        };

        IntegratedHeatPumps(DXCoilNum).IHPCoilsSized = true;
    }

    void InitializeIHP(int const DXCoilNum)
    {
        using General::TrimSigDigits;

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("InitializeIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
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
    }

    void UpdateIHP(int const DXCoilNum)
    {
        using DataHVACGlobals::TimeStepSys;
        using General::TrimSigDigits;
        using VariableSpeedCoils::VarSpeedCoil;

        int VSCoilIndex(0);
        Real64 ReportingConstant(0.0);
        Real64 TotalDelivery(0.0);

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("UpdateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::SCMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;                             // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                             // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = VarSpeedCoil(VSCoilIndex).QLatent;     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = VarSpeedCoil(VSCoilIndex).QSource;             // source energy rate, [w]
            break;
        case IHPOperationMode::SHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                       // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = 0.0;                                  // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;                 // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                        // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = VarSpeedCoil(VSCoilIndex).QSource;                  // source energy rate, [w]
            break;
        case IHPOperationMode::DWHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                    // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                               // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;              // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = VarSpeedCoil(VSCoilIndex).QLoadTotal;            // source energy rate, [w]
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = VarSpeedCoil(VSCoilIndex).QLoadTotal;   // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                               // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;              // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = VarSpeedCoil(VSCoilIndex).QLatent;       // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = 0.0;                                             // source energy rate, [w]
            break;
        case IHPOperationMode::SCDWHMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = VarSpeedCoil(VSCoilIndex).QLoadTotal; // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = 0.0;                             // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;            // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = VarSpeedCoil(VSCoilIndex).QLatent;     // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = VarSpeedCoil(VSCoilIndex).QSource;             // source energy rate, [w]

            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalCoolingRate = 0.0;                                       // total cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate = VarSpeedCoil(VSCoilIndex).QLoadTotal; // total space heating rate [w]
            IntegratedHeatPumps(DXCoilNum).TotalPower = VarSpeedCoil(VSCoilIndex).Power;                 // total power consumption  [w]
            IntegratedHeatPumps(DXCoilNum).TotalLatentLoad = 0.0;                                        // total latent cooling rate [w]
            IntegratedHeatPumps(DXCoilNum).Qsource = VarSpeedCoil(VSCoilIndex).QSource;                  // source energy rate, [w]

            VSCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate = VarSpeedCoil(VSCoilIndex).QSource; // total water heating rate [w]

            break;
        case IHPOperationMode::IdleMode:
        default:
            break;
        }

        ReportingConstant = TimeStepSys * SecInHour;

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

        if (IntegratedHeatPumps(DXCoilNum).TotalPower > 0.0) {
            TotalDelivery = IntegratedHeatPumps(DXCoilNum).TotalCoolingRate + IntegratedHeatPumps(DXCoilNum).TotalSpaceHeatingRate +
                            IntegratedHeatPumps(DXCoilNum).TotalWaterHeatingRate;
            IntegratedHeatPumps(DXCoilNum).TotalCOP = TotalDelivery / IntegratedHeatPumps(DXCoilNum).TotalPower;
        }
    }

    void DecideWorkMode(int const DXCoilNum,
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
        using DataEnvironment::OutDryBulbTemp;
        using DataHVACGlobals::SmallLoad;
        using DataHVACGlobals::TimeStepSys;
        using General::TrimSigDigits;
        using WaterThermalTanks::GetWaterThermalTankInput;
        using WaterThermalTanks::SimWaterThermalTank;

        Real64 MyLoad(0.0);
        Real64 MaxCap(0.0);
        Real64 MinCap(0.0);
        Real64 OptCap(0.0);
        Real64 WHHeatTimeSav(0.0); // time accumulation for water heating
        Real64 WHHeatVolSave(0.0); // volume accumulation for water heating

        // Obtains and Allocates AS-IHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("DecideWorkMode: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

        // decide working mode at the first moment
        // check if there is a water heating call
        IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
        IntegratedHeatPumps(DXCoilNum).CheckWHCall = true; // set checking flag
        if (IntegratedHeatPumps(DXCoilNum).WHtankID == 0)  // not initialized yet
        {
            IntegratedHeatPumps(DXCoilNum).IsWHCallAvail = false;
        } else {
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(DXCoilNum, 1.0, 1.0, true) * 987.0; // 987.0 water density at 60 C.
            Node(IntegratedHeatPumps(DXCoilNum).WaterOutletNodeNum).Temp = Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).Temp;
            SimWaterThermalTank(IntegratedHeatPumps(DXCoilNum).WHtankType, IntegratedHeatPumps(DXCoilNum).WHtankName,
                                IntegratedHeatPumps(DXCoilNum).WHtankID, false, false, MyLoad, MaxCap, MinCap, OptCap, true,
                                IntegratedHeatPumps(DXCoilNum).LoopNum, IntegratedHeatPumps(DXCoilNum).LoopSideNum);
        }
        IntegratedHeatPumps(DXCoilNum).CheckWHCall = false; // clear checking flag

        // keep the water heating time and volume history
        WHHeatTimeSav = IntegratedHeatPumps(DXCoilNum).SHDWHRunTime;
        if (IHPOperationMode::SCDWHMode == IntegratedHeatPumps(DXCoilNum).CurMode) {
            WHHeatVolSave = IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol + Node(IntegratedHeatPumps(DXCoilNum).WaterTankoutNod).MassFlowRate /
                                                                                   983.0 * TimeStepSys * SecInHour; // 983 - water density at 60 C
        } else {
            WHHeatVolSave = 0.0;
        }

        // clear the accumulation amount for other modes
        IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = 0.0;
        IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = 0.0;

        if (!IntegratedHeatPumps(DXCoilNum).IsWHCallAvail) // no water heating call
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
        else if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad))) // simultaneous SC and WH calls
        {
            if (WHHeatVolSave < IntegratedHeatPumps(DXCoilNum).WaterVolSCDWH) // small water heating amount
            {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCDWHMode;
                IntegratedHeatPumps(DXCoilNum).WaterFlowAccumVol = WHHeatVolSave;
            } else {
                if (1 == IntegratedHeatPumps(DXCoilNum).ModeMatchSCWH) // water heating priority
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
                else // space cooling piority
                    IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchSCMode;
            };

        } else if ((IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumps(DXCoilNum).TindoorOverCoolAllow) &&
                   (OutDryBulbTemp > IntegratedHeatPumps(DXCoilNum).TambientOverCoolAllow)) // over-cooling allowed, water heating priority
        {
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
        } else if ((IntegratedHeatPumps(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumps(DXCoilNum).TindoorWHHighPriority) &&
                   (OutDryBulbTemp > IntegratedHeatPumps(DXCoilNum).TambientWHHighPriority)) // ignore space heating request
        {
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
        } else if (SensLoad > SmallLoad) {
            IntegratedHeatPumps(DXCoilNum).SHDWHRunTime = WHHeatTimeSav + TimeStepSys * SecInHour;

            if (WHHeatTimeSav > IntegratedHeatPumps(DXCoilNum).TimeLimitSHDWH) {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOnMode;
            } else {
                IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::SHDWHElecHeatOffMode;
            };
        } else {
            IntegratedHeatPumps(DXCoilNum).CurMode = IHPOperationMode::DWHMode;
        }

        // clear up, important
        ClearCoils(DXCoilNum);
    }

    void ClearCoils(int const DXCoilNum)
    {
        using General::TrimSigDigits;
        using VariableSpeedCoils::SimVariableSpeedCoils;

        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling clear up function
        int CycFanCycCoil(1);                   // fan cycl manner place holder

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("ClearCoils: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        // clear up
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0,
                              0.0, 1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0,
                              0.0, 1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0,
                              0.0, 1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0,
                              0.0, 1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0,
                              1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SCCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0,
                              1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).SHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0,
                              1.0);
        SimVariableSpeedCoils(BlankString, IntegratedHeatPumps(DXCoilNum).DWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0,
                              1.0);

        return;
    }

    IHPOperationMode GetCurWorkMode(int const DXCoilNum)
    {
        using General::TrimSigDigits;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetCurWorkMode: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

        return (IntegratedHeatPumps(DXCoilNum).CurMode);
    }

    bool IHPInModel()
    {
        if (GetCoilsInputFlag) {
            GetIHPInput();
            GetCoilsInputFlag = false;
        }
        return !IntegratedHeatPumps.empty();
    }

    int GetCoilIndexIHP(std::string const &CoilType, // must match coil types in this module
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        IndexNum = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);

        if (IndexNum == 0) {
            ShowSevereError("GetCoilIndexIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }

        return IndexNum;
    }

    int GetCoilInletNodeIHP(std::string const &CoilType, // must match coil types in this module
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).AirCoolInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError("GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetDWHCoilInletNodeIHP(std::string const &CoilType, // must match coil types in this module
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).ODAirInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError("GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetDWHCoilOutletNodeIHP(std::string const &CoilType, // must match coil types in this module
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            NodeNumber = IntegratedHeatPumps(WhichCoil).ODAirOutletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError("GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetIHPDWHCoilPLFFPLR(std::string const &CoilType,            // must match coil types in this module
                             std::string const &CoilName,            // must match coil names for the coil type
                             IHPOperationMode const EP_UNUSED(Mode), // mode coil type
                             bool &ErrorsFound                       // set to true if problem
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        int WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {
            // this will be called by HPWH parent
            if (IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0)
                PLRNumber = GetVSCoilPLFFPLR(IntegratedHeatPumps(WhichCoil).DWHCoilType, IntegratedHeatPumps(WhichCoil).DWHCoilName, ErrorsFound);
            else
                PLRNumber = GetVSCoilPLFFPLR(IntegratedHeatPumps(WhichCoil).SCWHCoilType, IntegratedHeatPumps(WhichCoil).SCWHCoilName, ErrorsFound);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError("GetIHPDWHCoilPLFFPLR: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            PLRNumber = 0;
        }

        return PLRNumber;
    }

    Real64 GetDWHCoilCapacityIHP(std::string const &CoilType,            // must match coil types in this module
                                 std::string const &CoilName,            // must match coil names for the coil type
                                 IHPOperationMode const EP_UNUSED(Mode), // mode coil type
                                 bool &ErrorsFound                       // set to true if problem
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
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        int WhichCoil = UtilityRoutines::FindItemInList(CoilName, IntegratedHeatPumps);
        if (WhichCoil != 0) {

            if (IntegratedHeatPumps(WhichCoil).IHPCoilsSized == false) SizeIHP(WhichCoil);

            if (IntegratedHeatPumps(WhichCoil).DWHCoilIndex > 0) {
                CoilCapacity =
                    GetCoilCapacityVariableSpeed(IntegratedHeatPumps(WhichCoil).DWHCoilType, IntegratedHeatPumps(WhichCoil).DWHCoilName, ErrorsFound);
            } else {
                CoilCapacity = GetCoilCapacityVariableSpeed(IntegratedHeatPumps(WhichCoil).SCWHCoilType, IntegratedHeatPumps(WhichCoil).SCWHCoilName,
                                                            ErrorsFound);
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError("GetCoilCapacityVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CoilCapacity = -1000.0;
        }

        return CoilCapacity;
    }

    int GetLowSpeedNumIHP(int const DXCoilNum)
    {
        using General::TrimSigDigits;

        int SpeedNum(0);

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetLowSpeedNumIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
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

    int GetMaxSpeedNumIHP(int const DXCoilNum)
    {
        using General::TrimSigDigits;
        using VariableSpeedCoils::VarSpeedCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetMaxSpeedNumIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        int SpeedNum(0);

        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SHMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::DWHMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).DWHCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SCDWHMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).NumOfSpeeds;
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).NumOfSpeeds;
            break;
        default:
            SpeedNum = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCCoilIndex).NumOfSpeeds;
            break;
        }

        return (SpeedNum);
    }

    Real64 GetAirVolFlowRateIHP(int const DXCoilNum,
                                int const SpeedNum,
                                Real64 const SpeedRatio,
                                bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    )
    {
        using General::TrimSigDigits;
        using VariableSpeedCoils::VarSpeedCoil;

        int IHPCoilIndex(0);
        Real64 AirVolFlowRate(0.0);
        Real64 FlowScale(1.0);
        bool IsResultFlow(false); // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetAirVolFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

        FlowScale = 0.0;
        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            break;
        case IHPOperationMode::SCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            if (false == IsCallbyWH) // call from air loop
            {
                FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            }

            break;
        case IHPOperationMode::SHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            if (false == IsCallbyWH) // call from air loop
            {
                FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            }
            break;
        case IHPOperationMode::DWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            FlowScale = 1.0;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (IsCallbyWH) // call from water loop
            {
                IsResultFlow = true;
                AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (false == IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex).AirVolFlowRate;
            }
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumps(DXCoilNum).SHDWHHeatCoilIndex).AirVolFlowRate;
            }
            break;
        default:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            FlowScale = 0.0;
            break;
        }

        if (false == IsResultFlow) {
            if (1 == SpeedNum)
                AirVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum);
            else
                AirVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum) +
                                 (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum - 1);

            AirVolFlowRate = AirVolFlowRate * FlowScale;
        }

        if (AirVolFlowRate > IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow) AirVolFlowRate = IntegratedHeatPumps(DXCoilNum).MaxCoolAirVolFlow;
        if (AirVolFlowRate > IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow) AirVolFlowRate = IntegratedHeatPumps(DXCoilNum).MaxHeatAirVolFlow;

        return (AirVolFlowRate);
    }

    Real64 GetWaterVolFlowRateIHP(
        int const DXCoilNum,
        int const SpeedNum,
        Real64 const SpeedRatio,
        bool const EP_UNUSED(IsCallbyWH) // whether the call from the water heating loop or air loop, true = from water heating loop
    )
    {
        using General::TrimSigDigits;
        using VariableSpeedCoils::VarSpeedCoil;

        int IHPCoilIndex(0);
        Real64 WaterVolFlowRate(0.0);

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetWaterVolFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

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
                WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SCWHMatchSCMode:
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        case IHPOperationMode::SHDWHElecHeatOffMode:
        case IHPOperationMode::SHDWHElecHeatOnMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHDWHWHCoilIndex;
            if (1 == SpeedNum)
                WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
            else
                WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
                                   (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
            break;
        default:
            WaterVolFlowRate = 0.0;
            break;
        }

        return (WaterVolFlowRate);
    }

    Real64 GetAirMassFlowRateIHP(int const DXCoilNum,
                                 int const SpeedNum,
                                 Real64 const SpeedRatio,
                                 bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    )
    {
        using General::TrimSigDigits;
        using VariableSpeedCoils::VarSpeedCoil;

        int IHPCoilIndex(0);
        Real64 AirMassFlowRate(0.0);
        Real64 FlowScale(1.0);
        bool IsResultFlow(false);   // IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated
        Real64 WaterDensity(986.0); // standard water density at 60 C

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (GetCoilsInputFlag) { // First time subroutine has been entered
            GetIHPInput();
            GetCoilsInputFlag = false;
        }

        if (DXCoilNum > static_cast<int>(IntegratedHeatPumps.size()) || DXCoilNum < 1) {
            ShowFatalError("GetAirMassFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
                           ", Number of Integrated HPs=" + TrimSigDigits(IntegratedHeatPumps.size()) + ", IHP name=" + "AS-IHP");
        }

        if (IntegratedHeatPumps(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

        FlowScale = 0.0;
        switch (IntegratedHeatPumps(DXCoilNum).CurMode) {
        case IHPOperationMode::IdleMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            AirMassFlowRate = 0.0;
            break;
        case IHPOperationMode::SCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCCoilIndex;
            if (false == IsCallbyWH) {
                FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            } else {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::SHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SHCoilIndex;
            if (false == IsCallbyWH) {
                FlowScale = IntegratedHeatPumps(DXCoilNum).HeatVolFlowScale;
            } else {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::DWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).DWHCoilIndex;
            FlowScale = 1.0;
            break;
        case IHPOperationMode::SCWHMatchSCMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
            if (IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInAirLoop;
            }
            break;
        case IHPOperationMode::SCWHMatchWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCWHCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            if (false == IsCallbyWH) {
                IsResultFlow = true;
                AirMassFlowRate = IntegratedHeatPumps(DXCoilNum).AirFlowSavInWaterLoop;
            }
            break;
        case IHPOperationMode::SCDWHMode:
            IHPCoilIndex = IntegratedHeatPumps(DXCoilNum).SCDWHCoolCoilIndex;
            FlowScale = IntegratedHeatPumps(DXCoilNum).CoolVolFlowScale;
            Node(IntegratedHeatPumps(DXCoilNum).WaterInletNodeNum).MassFlowRate =
                GetWaterVolFlowRateIHP(DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
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
                GetWaterVolFlowRateIHP(DXCoilNum, SpeedNum, SpeedRatio, true) * WaterDensity;
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
            if (SpeedNum == 1) {
                AirMassFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum);
            } else {
                AirMassFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum) +
                                  (1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum - 1);
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

} // namespace IntegratedHeatPump

} // namespace EnergyPlus
