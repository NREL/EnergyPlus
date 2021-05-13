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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/HWBaseboardRadiator.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/SteamBaseboardRadiator.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus {

// Integer constants for different system types handled by the routines in this file
enum GeneralRoutinesEquipNums
{
    ParallelPIUReheatNum = 1,
    SeriesPIUReheatNum = 2,
    HeatingCoilWaterNum = 3,
    BBWaterConvOnlyNum = 4,
    BBSteamRadConvNum = 5,
    BBWaterRadConvNum = 6,
    FourPipeFanCoilNum = 7,
    OutdoorAirUnitNum = 8,
    UnitHeaterNum = 9,
    UnitVentilatorNum = 10,
    VentilatedSlabNum = 11
};

void ControlCompOutput(EnergyPlusData &state,
                       std::string const &CompName,           // the component Name
                       std::string const &CompType,           // Type of component
                       int &CompNum,                          // Index of component in component array
                       bool const FirstHVACIteration,         // flag for 1st HVAV iteration in the time step
                       Real64 const QZnReq,                   // zone load to be met
                       int const ActuatedNode,                // node that controls unit output
                       Real64 const MaxFlow,                  // maximum water flow
                       Real64 const MinFlow,                  // minimum water flow
                       Real64 const ControlOffset,            // really the tolerance
                       int &ControlCompTypeNum,               // Internal type num for CompType
                       int &CompErrIndex,                     // for Recurring error call
                       Optional_int_const TempInNode,         // inlet node for output calculation
                       Optional_int_const TempOutNode,        // outlet node for output calculation
                       Optional<Real64 const> AirMassFlow,    // air mass flow rate
                       Optional_int_const Action,             // 1=reverse; 2=normal
                       Optional_int_const EquipIndex,         // Identifier for equipment of Outdoor Air Unit "ONLY"
                       Optional_int_const LoopNum,            // for plant components, plant loop index
                       Optional_int_const LoopSide,           // for plant components, plant loop side index
                       Optional_int_const BranchIndex,        // for plant components, plant branch index
                       Optional_int_const ControlledZoneIndex // controlled zone index for the zone containing the component
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   April 2000
    //       MODIFIED       Brent Griffith, Sept 2010 update plant interactions
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // The purpose of this subroutine is to control the output of heating or cooling
    // meet the zone load.

    // METHODOLOGY EMPLOYED:
    // Currently this is using an intervasl halving scheme to a control tolerance

    // Using/Aliasing
    using namespace DataLoopNode;
    using BaseboardRadiator::SimHWConvective;
    using FanCoilUnits::Calc4PipeFanCoil;

    using HWBaseboardRadiator::CalcHWBaseboard;
    using PlantUtilities::SetActuatedBranchFlowRate;
    using Psychrometrics::PsyCpAirFnW;
    using SteamBaseboardRadiator::CalcSteamBaseboard;
    using UnitHeater::CalcUnitHeaterComponents;
    using UnitVentilator::CalcUnitVentilatorComponents;
    using VentilatedSlab::CalcVentilatedSlabComps;
    using WaterCoils::SimulateWaterCoilComponents;

    // SUBROUTINE PARAMETER DEFINITIONS:
    // Iteration maximum for reheat control
    static int const MaxIter(25);
    static Real64 const iter_fac(1.0 / std::pow(2, MaxIter - 3));
    int const iReverseAction(1);
    int const iNormalAction(2);

    // Note - order in routine must match order below
    //  Plus -- order in ListOfComponents array must be in sorted order.
    int const NumComponents(11);
    static Array1D_string const ListOfComponents(NumComponents,
                                                 {"AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT",
                                                  "AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT",
                                                  "COIL:HEATING:WATER",
                                                  "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER",
                                                  "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM",
                                                  "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER",
                                                  "ZONEHVAC:FOURPIPEFANCOIL",
                                                  "ZONEHVAC:OUTDOORAIRUNIT",
                                                  "ZONEHVAC:UNITHEATER",
                                                  "ZONEHVAC:UNITVENTILATOR",
                                                  "ZONEHVAC:VENTILATEDSLAB"});

    // DERIVED TYPE DEFINITIONS
    // Interval Half Type used for Controller

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Iter(0);  // Iteration limit for the interval halving process
    Real64 CpAir; // specific heat of air (J/kg-C)
    bool Converged;
    Real64 Denom;   // the denominator of the control signal
    Real64 LoadMet; // Actual output of unit (watts)
    // INTEGER, SAVE    :: ErrCount=0  ! Number of times that the maximum iterations was exceeded
    // INTEGER, SAVE    :: ErrCount1=0 ! for recurring error
    bool WaterCoilAirFlowControl; // True if controlling air flow through water coil, water flow fixed
    int SimCompNum;               // internal number for case statement
    Real64 HalvingPrec(0.0);      // precision of halving algorithm
    bool BBConvergeCheckFlag;     // additional check on convergence specifically for radiant/convective baseboard units

    // Object Data
    auto &ZoneInterHalf = state.dataGeneralRoutines->ZoneInterHalf;
    auto &ZoneController = state.dataGeneralRoutines->ZoneController;

    if (ControlCompTypeNum != 0) {
        SimCompNum = ControlCompTypeNum;
    } else {
        SimCompNum = UtilityRoutines::FindItemInSortedList(CompType, ListOfComponents, NumComponents);
        ControlCompTypeNum = SimCompNum;
    }

    Iter = 0;
    Converged = false;
    WaterCoilAirFlowControl = false;
    LoadMet = 0.0;
    HalvingPrec = 0.0;

    // At the beginning of every time step the value is reset to the User Input
    ZoneController.SetPoint = 0.0;

    // Set to converged controller
    ZoneInterHalf.MaxFlowCalc = true;
    ZoneInterHalf.MinFlowCalc = false;
    ZoneInterHalf.NormFlowCalc = false;
    ZoneInterHalf.MinFlowResult = false;
    ZoneInterHalf.MaxResult = 1.0;
    ZoneInterHalf.MinResult = 0.0;

    // Start the Solution Iteration
    while (!Converged) {

        if (FirstHVACIteration) {
            state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail = MaxFlow;
            state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail = MinFlow;
            // Check to make sure that the Minimum Flow rate is less than the max.
            if (MinFlow > MaxFlow) {
                ShowSevereError(state, "ControlCompOutput:" + CompType + ':' + CompName + ", Min Control Flow is > Max Control Flow");
                ShowContinueError(
                    state, format("Acuated Node={} MinFlow=[{:.3T}], Max Flow={:.3T}", state.dataLoopNodes->NodeID(ActuatedNode), MinFlow, MaxFlow));
                ShowContinueErrorTimeStamp(state, "");
                ShowFatalError(state, "Program terminates due to preceding condition.");
            }
        } // End of FirstHVACIteration Conditional If
        // The interface managers can reset the Max or Min to available values during the time step
        // and these will then be the new setpoint limits for the controller to work within.
        if ((SimCompNum == 3) && (!present(AirMassFlow))) {
            ZoneController.MaxSetPoint = state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail;
            ZoneController.MinSetPoint = state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail;
        } else {
            ZoneController.MaxSetPoint =
                min(state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMaxAvail, state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMax);
            ZoneController.MinSetPoint =
                max(state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMinAvail, state.dataLoopNodes->Node(ActuatedNode).MassFlowRateMin);
        }
        // The first time through run at maximum flow rate and find results
        if (ZoneInterHalf.MaxFlowCalc) {
            ZoneController.CalculatedSetPoint = ZoneController.MaxSetPoint;
            ZoneInterHalf.MaxFlow = ZoneController.MaxSetPoint;
            ZoneInterHalf.MaxFlowCalc = false;
            ZoneInterHalf.MinFlowCalc = true;
            // Record the maximum flow rates and set the flow to the minimum and find results
        } else if (ZoneInterHalf.MinFlowCalc) {
            ZoneInterHalf.MaxResult = ZoneController.SensedValue;
            ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
            ZoneInterHalf.MinFlow = ZoneController.MinSetPoint;
            ZoneInterHalf.MinFlowCalc = false;
            ZoneInterHalf.MinFlowResult = true;
            // Record the minimum results and set flow to half way between the max and min and find results
        } else if (ZoneInterHalf.MinFlowResult) {
            ZoneInterHalf.MinResult = ZoneController.SensedValue;
            HalvingPrec = (ZoneInterHalf.MaxResult - ZoneInterHalf.MinResult) * iter_fac;
            ZoneInterHalf.MidFlow = (ZoneInterHalf.MaxFlow + ZoneInterHalf.MinFlow) / 2.0;
            ZoneController.CalculatedSetPoint = (ZoneInterHalf.MaxFlow + ZoneInterHalf.MinFlow) / 2.0;
            ZoneInterHalf.MinFlowResult = false;
            ZoneInterHalf.NormFlowCalc = true;
            // Record the Mid results and check all possibilities and start interval halving procedure
        } else if (ZoneInterHalf.NormFlowCalc) {
            ZoneInterHalf.MidResult = ZoneController.SensedValue;

            // First check to see if the component is running; if not converge and return
            if (ZoneInterHalf.MaxResult == ZoneInterHalf.MinResult) {
                // Set to converged controller
                Converged = true;
                ZoneInterHalf.MaxFlowCalc = true;
                ZoneInterHalf.MinFlowCalc = false;
                ZoneInterHalf.NormFlowCalc = false;
                ZoneInterHalf.MinFlowResult = false;
                ZoneInterHalf.MaxResult = 1.0;
                ZoneInterHalf.MinResult = 0.0;
                if ((SimCompNum >= 4) && (SimCompNum <= 6)) { // hot water baseboards use min flow
                    ZoneController.CalculatedSetPoint = 0.0;  // CR7253
                } else {
                    ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow; // CR7253
                }
                // Set the Actuated node MassFlowRate with zero value
                if (present(LoopNum)) { // this is a plant component
                    SetActuatedBranchFlowRate(state,
                                              ZoneController.CalculatedSetPoint,
                                              ActuatedNode,
                                              LoopNum,
                                              LoopSide,
                                              BranchIndex,
                                              false); // Autodesk:OPTIONAL LoopSide, BranchIndex used without PRESENT check
                } else {                              // assume not a plant component
                    state.dataLoopNodes->Node(ActuatedNode).MassFlowRate = ZoneController.CalculatedSetPoint;
                }
                return;
            }

            // The next series of checks is to determine what interval the current solution is in
            //   comparison to the setpoint and then respond appropriately.

            // Normal controller assumes that MaxResult will be greater than MinResult. First check
            // to make sure that this is the case
            if (ZoneInterHalf.MaxResult <= ZoneInterHalf.MinResult) {
                if (WaterCoilAirFlowControl) {
                    ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow;
                } else {
                    ZoneController.CalculatedSetPoint = ZoneInterHalf.MinFlow;
                }
                // set to converged controller
                Converged = true;
                ZoneInterHalf.MaxFlowCalc = true;
                ZoneInterHalf.MinFlowCalc = false;
                ZoneInterHalf.NormFlowCalc = false;
                ZoneInterHalf.MinFlowResult = false;
                ZoneInterHalf.MaxResult = 1.0;
                ZoneInterHalf.MinResult = 0.0;
                // MaxResult is greater than MinResult so simulation control algorithm may proceed normally
            } else if (ZoneInterHalf.MaxResult > ZoneInterHalf.MinResult) {
                // Now check to see if the setpoint is outside the endpoints of the control range
                // First check to see if the water is too cold and if so set to the minimum flow.
                if (ZoneController.SetPoint <= ZoneInterHalf.MinResult) {
                    ZoneController.CalculatedSetPoint = ZoneInterHalf.MinFlow;
                    // Set to Converged Controller
                    Converged = true;
                    ZoneInterHalf.MaxFlowCalc = true;
                    ZoneInterHalf.MinFlowCalc = false;
                    ZoneInterHalf.NormFlowCalc = false;
                    ZoneInterHalf.MinFlowResult = false;
                    ZoneInterHalf.MaxResult = 1.0;
                    ZoneInterHalf.MinResult = 0.0;
                    // Then check if too hot and if so set it to the maximum flow
                } else if (ZoneController.SetPoint >= ZoneInterHalf.MaxResult) {
                    ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow;
                    // Set to Converged Controller
                    Converged = true;
                    ZoneInterHalf.MaxFlowCalc = true;
                    ZoneInterHalf.MinFlowCalc = false;
                    ZoneInterHalf.NormFlowCalc = false;
                    ZoneInterHalf.MinFlowResult = false;
                    ZoneInterHalf.MaxResult = 1.0;
                    ZoneInterHalf.MinResult = 0.0;
                    // If between the max and mid set to new flow and raise min to mid
                } else if ((ZoneController.SetPoint < ZoneInterHalf.MaxResult) && (ZoneController.SetPoint >= ZoneInterHalf.MidResult)) {
                    ZoneController.CalculatedSetPoint = (ZoneInterHalf.MaxFlow + ZoneInterHalf.MidFlow) / 2.0;
                    ZoneInterHalf.MinFlow = ZoneInterHalf.MidFlow;
                    ZoneInterHalf.MinResult = ZoneInterHalf.MidResult;
                    ZoneInterHalf.MidFlow = (ZoneInterHalf.MaxFlow + ZoneInterHalf.MidFlow) / 2.0;
                    // If between the min and mid set to new flow and lower Max to mid
                } else if ((ZoneController.SetPoint < ZoneInterHalf.MidResult) && (ZoneController.SetPoint > ZoneInterHalf.MinResult)) {
                    ZoneController.CalculatedSetPoint = (ZoneInterHalf.MinFlow + ZoneInterHalf.MidFlow) / 2.0;
                    ZoneInterHalf.MaxFlow = ZoneInterHalf.MidFlow;
                    ZoneInterHalf.MaxResult = ZoneInterHalf.MidResult;
                    ZoneInterHalf.MidFlow = (ZoneInterHalf.MinFlow + ZoneInterHalf.MidFlow) / 2.0;

                } // End of the Conditional for the actual interval halving scheme itself
            }     // end of max > min check

        } // End of the Conditinal for the first 3 iterations for the interval halving

        // Make sure that the Calculated setpoint falls between the minimum and maximum allowed
        if (ZoneController.CalculatedSetPoint > ZoneController.MaxSetPoint) {
            ZoneController.CalculatedSetPoint = ZoneController.MaxSetPoint;
            Converged = true;
            ZoneInterHalf.MaxFlowCalc = true;
            ZoneInterHalf.MinFlowCalc = false;
            ZoneInterHalf.NormFlowCalc = false;
            ZoneInterHalf.MinFlowResult = false;
            ZoneInterHalf.MaxResult = 1.0;
            ZoneInterHalf.MinResult = 0.0;
        } else if (ZoneController.CalculatedSetPoint < ZoneController.MinSetPoint) {
            ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
            Converged = true;
            ZoneInterHalf.MaxFlowCalc = true;
            ZoneInterHalf.MinFlowCalc = false;
            ZoneInterHalf.NormFlowCalc = false;
            ZoneInterHalf.MinFlowResult = false;
            ZoneInterHalf.MaxResult = 1.0;
            ZoneInterHalf.MinResult = 0.0;
        }

        // check if hunting down around the limit of a significant mass flow in systems.
        if ((Iter > MaxIter / 2) && (ZoneController.CalculatedSetPoint < DataBranchAirLoopPlant::MassFlowTolerance)) {
            ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
            Converged = true;
            ZoneInterHalf.MaxFlowCalc = true;
            ZoneInterHalf.MinFlowCalc = false;
            ZoneInterHalf.NormFlowCalc = false;
            ZoneInterHalf.MinFlowResult = false;
            ZoneInterHalf.MaxResult = 1.0;
            ZoneInterHalf.MinResult = 0.0;
        }

        // Set the Actuated node MassFlowRate with the new value
        if (present(LoopNum)) { // this is a plant component
            SetActuatedBranchFlowRate(state,
                                      ZoneController.CalculatedSetPoint,
                                      ActuatedNode,
                                      LoopNum,
                                      LoopSide,
                                      BranchIndex,
                                      false); // Autodesk:OPTIONAL LoopSide, BranchIndex used without PRESENT check
        } else {                              // assume not a plant component, leave alone
            state.dataLoopNodes->Node(ActuatedNode).MassFlowRate = ZoneController.CalculatedSetPoint;
        }

        // The denominator of the control signal should be no less than 100 watts
        Denom = sign(max(std::abs(QZnReq), 100.0), QZnReq);
        if (present(Action)) {
            if (Action == iNormalAction) {
                Denom = max(std::abs(QZnReq), 100.0);
            } else if (Action == iReverseAction) {
                Denom = -max(std::abs(QZnReq), 100.0);
            } else {
                ShowFatalError(state, format("ControlCompOutput: Illegal Action argument =[{}]", Action));
            }
        }

        switch (SimCompNum) {      // Tuned If block changed to switch
        case ParallelPIUReheatNum: // 'AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT'
            // simulate series piu reheat coil
            SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompNum);
            // Calculate the control signal (the variable we are forcing to zero)
            CpAir =
                PsyCpAirFnW(state.dataLoopNodes->Node(TempOutNode).HumRat); // Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
            LoadMet = CpAir * state.dataLoopNodes->Node(TempOutNode).MassFlowRate *
                      (state.dataLoopNodes->Node(TempOutNode).Temp -
                       state.dataLoopNodes->Node(TempInNode).Temp); // Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case SeriesPIUReheatNum: // 'AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT'
            // simulate series piu reheat coil
            SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompNum);
            // Calculate the control signal (the variable we are forcing to zero)
            CpAir =
                PsyCpAirFnW(state.dataLoopNodes->Node(TempOutNode).HumRat); // Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
            LoadMet = CpAir * state.dataLoopNodes->Node(TempOutNode).MassFlowRate *
                      (state.dataLoopNodes->Node(TempOutNode).Temp -
                       state.dataLoopNodes->Node(TempInNode).Temp); // Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case HeatingCoilWaterNum: // 'COIL:HEATING:WATER'
            // Simulate reheat coil for the VAV system
            SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompNum);
            // Calculate the control signal (the variable we are forcing to zero)
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(TempOutNode).HumRat);
            if (present(AirMassFlow)) {
                LoadMet = AirMassFlow * CpAir * state.dataLoopNodes->Node(TempOutNode).Temp;
                ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            } else {
                WaterCoilAirFlowControl = true;
                LoadMet = state.dataLoopNodes->Node(TempOutNode).MassFlowRate * CpAir *
                          (state.dataLoopNodes->Node(TempOutNode).Temp -
                           state.dataLoopNodes->Node(TempInNode).Temp); // Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
                ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            }
            break;

        case BBWaterConvOnlyNum: // 'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER'
            // Simulate baseboard
            SimHWConvective(state, CompNum, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case BBSteamRadConvNum: // 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM'
            // Simulate baseboard
            CalcSteamBaseboard(state, CompNum, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case BBWaterRadConvNum: // 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER'
            // Simulate baseboard
            CalcHWBaseboard(state, CompNum, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case FourPipeFanCoilNum: // 'ZONEHVAC:FOURPIPEFANCOIL'
            // Simulate fancoil unit
            Calc4PipeFanCoil(state, CompNum, ControlledZoneIndex, FirstHVACIteration, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case OutdoorAirUnitNum: //'ZONEHVAC:OUTDOORAIRUNIT'
            // Simulate outdoor air unit components
            OutdoorAirUnit::CalcOAUnitCoilComps(
                state, CompNum, FirstHVACIteration, EquipIndex, LoadMet); // Autodesk:OPTIONAL EquipIndex used without PRESENT check
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case UnitHeaterNum: // 'ZONEHVAC:UNITHEATER'
            // Simulate unit heater components
            CalcUnitHeaterComponents(state, CompNum, FirstHVACIteration, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case UnitVentilatorNum: // 'ZONEHVAC:UNITVENTILATOR'
            // Simulate unit ventilator components
            CalcUnitVentilatorComponents(state, CompNum, FirstHVACIteration, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        case VentilatedSlabNum: // 'ZONEHVAC:VENTILATEDSLAB'
            // Simulate unit ventilator components
            CalcVentilatedSlabComps(state, CompNum, FirstHVACIteration, LoadMet);
            // Calculate the control signal (the variable we are forcing to zero)
            ZoneController.SensedValue = (LoadMet - QZnReq) / Denom;
            break;

        default:
            ShowFatalError(state, format("ControlCompOutput: Illegal Component Number argument =[{}]", SimCompNum));
            break;
        }

        // Check for Controller convergence to see if within the offset
        if (std::abs(ZoneController.SensedValue) <= ControlOffset || std::abs(ZoneController.SensedValue) <= HalvingPrec) {
            // Set to converged controller
            Converged = true;
            ZoneInterHalf.MaxFlowCalc = true;
            ZoneInterHalf.MinFlowCalc = false;
            ZoneInterHalf.NormFlowCalc = false;
            ZoneInterHalf.MinFlowResult = false;
            ZoneInterHalf.MaxResult = 1.0;
            ZoneInterHalf.MinResult = 0.0;
            break;
        }
        if (!Converged) {
            BBConvergeCheckFlag = BBConvergeCheck(SimCompNum, ZoneInterHalf.MaxFlow, ZoneInterHalf.MinFlow);
            if (BBConvergeCheckFlag) {
                // Set to converged controller
                Converged = true;
                ZoneInterHalf.MaxFlowCalc = true;
                ZoneInterHalf.MinFlowCalc = false;
                ZoneInterHalf.NormFlowCalc = false;
                ZoneInterHalf.MinFlowResult = false;
                ZoneInterHalf.MaxResult = 1.0;
                ZoneInterHalf.MinResult = 0.0;
                break;
            }
        }

        ++Iter;
        if ((Iter > MaxIter) && (!state.dataGlobal->WarmupFlag)) {
            // if ( CompErrIndex == 0 ) {
            ShowWarningMessage(state, "ControlCompOutput: Maximum iterations exceeded for " + CompType + " = " + CompName);
            ShowContinueError(state, format("... Load met       = {:.5T} W.", LoadMet));
            ShowContinueError(state, format("... Load requested = {:.5T} W.", QZnReq));
            ShowContinueError(state, format("... Error          = {:.8T} %.", std::abs((LoadMet - QZnReq) * 100.0 / Denom)));
            ShowContinueError(state, format("... Tolerance      = {:.8T} %.", ControlOffset * 100.0));
            ShowContinueError(state, "... Error          = (Load met - Load requested) / MAXIMUM(Load requested, 100)");
            ShowContinueError(state, format("... Actuated Node Mass Flow Rate ={:.9R} kg/s", state.dataLoopNodes->Node(ActuatedNode).MassFlowRate));
            ShowContinueErrorTimeStamp(state, "");
            ShowRecurringWarningErrorAtEnd(state,
                                           "ControlCompOutput: Maximum iterations error for " + CompType + " = " + CompName,
                                           CompErrIndex,
                                           std::abs((LoadMet - QZnReq) * 100.0 / Denom),
                                           std::abs((LoadMet - QZnReq) * 100.0 / Denom),
                                           _,
                                           "%",
                                           "%");
            //}
            ShowRecurringWarningErrorAtEnd(state,
                                           "ControlCompOutput: Maximum iterations error for " + CompType + " = " + CompName,
                                           CompErrIndex,
                                           std::abs((LoadMet - QZnReq) * 100.0 / Denom),
                                           std::abs((LoadMet - QZnReq) * 100.0 / Denom),
                                           _,
                                           "%",
                                           "%");
            break; // It will not converge this time
        } else if (Iter > MaxIter * 2) {
            break;
        }

    } // End of the Convergence Iteration
}

bool BBConvergeCheck(int const SimCompNum, Real64 const MaxFlow, Real64 const MinFlow)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   November 2017

    // PURPOSE OF THIS SUBROUTINE:
    // This is an additional check for the radiant/convective baseboard units
    // to see if they are converged or the flow is sufficiently converged to
    // procede with the simulation.  With the radiant component to these systems,
    // the impact on the load met is more difficult to calculate and the impact
    // on the actual system output is not as well behaved as for convective
    // systems.  This additional check avoids excessive iterations and max
    // iteration warnings and provides sufficiently converged results.  It is
    // only called from ControlCompOutput.

    // Return Value
    bool BBConvergeCheck;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Real64 const BBIterLimit(0.00001);

    if (SimCompNum != BBSteamRadConvNum && SimCompNum != BBWaterRadConvNum) {
        // For all zone equipment except radiant/convective baseboard (steam and water) units:
        BBConvergeCheck = false;
    } else {
        // For steam and water radiant/convective baseboard units:
        if ((MaxFlow - MinFlow) > BBIterLimit) {
            BBConvergeCheck = false;
        } else {
            BBConvergeCheck = true;
        }
    }

    return BBConvergeCheck;
}

void CheckSysSizing(EnergyPlusData &state,
                    std::string const &CompType, // Component Type (e.g. Chiller:Electric)
                    std::string const &CompName  // Component Name (e.g. Big Chiller)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   October 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine is called when an "autosize" input is encountered in a component
    // sizing routine to check that the system sizing calculations have been done.

    // METHODOLOGY EMPLOYED:
    // Checks SysSizingRunDone flag. If false throws a fatal error.

    if (!state.dataSize->SysSizingRunDone) {
        ShowSevereError(state, "For autosizing of " + CompType + ' ' + CompName + ", a system sizing run must be done.");
        if (state.dataSize->NumSysSizInput == 0) {
            ShowContinueError(state, "No \"Sizing:System\" objects were entered.");
        }
        if (!state.dataGlobal->DoSystemSizing) {
            ShowContinueError(state, R"(The "SimulationControl" object did not have the field "Do System Sizing Calculation" set to Yes.)");
        }
        ShowFatalError(state, "Program terminates due to previously shown condition(s).");
    }
}

void CheckThisAirSystemForSizing(EnergyPlusData &state, int const AirLoopNum, bool &AirLoopWasSized)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   October 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ThisAirSysSizineInputLoop;

    AirLoopWasSized = false;
    if (state.dataSize->SysSizingRunDone) {
        for (ThisAirSysSizineInputLoop = 1; ThisAirSysSizineInputLoop <= state.dataSize->NumSysSizInput; ++ThisAirSysSizineInputLoop) {
            if (state.dataSize->SysSizInput(ThisAirSysSizineInputLoop).AirLoopNum == AirLoopNum) {
                AirLoopWasSized = true;
                break;
            }
        }
    }
}

void CheckZoneSizing(EnergyPlusData &state,
                     std::string const &CompType, // Component Type (e.g. Chiller:Electric)
                     std::string const &CompName  // Component Name (e.g. Big Chiller)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   October 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine is called when an "autosize" input is encountered in a component
    // sizing routine to check that the zone sizing calculations have been done.

    // METHODOLOGY EMPLOYED:
    // Checks ZoneSizingRunDone flag. If false throws a fatal error.

    if (!state.dataSize->ZoneSizingRunDone) {
        ShowSevereError(state, "For autosizing of " + CompType + ' ' + CompName + ", a zone sizing run must be done.");
        if (state.dataSize->NumZoneSizingInput == 0) {
            ShowContinueError(state, "No \"Sizing:Zone\" objects were entered.");
        }
        if (!state.dataGlobal->DoZoneSizing) {
            ShowContinueError(state, R"(The "SimulationControl" object did not have the field "Do Zone Sizing Calculation" set to Yes.)");
        }
        ShowFatalError(state, "Program terminates due to previously shown condition(s).");
    }
}

void CheckThisZoneForSizing(EnergyPlusData &state,
                            int const ZoneNum, // zone index to be checked
                            bool &ZoneWasSized)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Oct 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // utility routine to see if a particular zone has a Sizing:Zone object for it
    // and that sizing was done.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ThisSizingInput;

    ZoneWasSized = false;
    if (state.dataSize->ZoneSizingRunDone) {
        for (ThisSizingInput = 1; ThisSizingInput <= state.dataSize->NumZoneSizingInput; ++ThisSizingInput) {
            if (state.dataSize->ZoneSizingInput(ThisSizingInput).ZoneNum == ZoneNum) {
                ZoneWasSized = true;
                break;
            }
        }
    }
}

void ValidateComponent(EnergyPlusData &state,
                       std::string const &CompType,  // Component Type (e.g. Chiller:Electric)
                       std::string const &CompName,  // Component Name (e.g. Big Chiller)
                       bool &IsNotOK,                // .TRUE. if this component pair is invalid
                       std::string const &CallString // Context of this pair -- for error message
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine can be called to validate the component type-name pairs that
    // are so much a part of the EnergyPlus input.  The main drawback to this validation
    // has been that the "GetInput" routine may not have been called and/or exists in
    // another module from the one with the list.  This means that validation must be
    // done later, perhaps after simulation has already started or perhaps raises an
    // array bound error instead.

    // METHODOLOGY EMPLOYED:
    // Uses existing routines in InputProcessor.  GetObjectItemNum uses the "standard"
    // convention of the Name of the item/object being the first Alpha Argument.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ItemNum;

    IsNotOK = false;

    ItemNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, CompType, CompName);

    if (ItemNum < 0) {
        ShowSevereError(state, "During " + CallString + " Input, Invalid Component Type input=" + CompType);
        ShowContinueError(state, "Component name=" + CompName);
        IsNotOK = true;
    } else if (ItemNum == 0) {
        ShowSevereError(state, "During " + CallString + " Input, Invalid Component Name input=" + CompName);
        ShowContinueError(state, "Component type=" + CompType);
        IsNotOK = true;
    }
}

void ValidateComponent(EnergyPlusData &state,
                       std::string const &CompType,    // Component Type (e.g. Chiller:Electric)
                       std::string const &CompValType, // Component "name" field type
                       std::string const &CompName,    // Component Name (e.g. Big Chiller)
                       bool &IsNotOK,                  // .TRUE. if this component pair is invalid
                       std::string const &CallString   // Context of this pair -- for error message
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine can be called to validate the component type-name pairs that
    // are so much a part of the EnergyPlus input.  The main drawback to this validation
    // has been that the "GetInput" routine may not have been called and/or exists in
    // another module from the one with the list.  This means that validation must be
    // done later, perhaps after simulation has already started or perhaps raises an
    // array bound error instead.

    // METHODOLOGY EMPLOYED:
    // Uses existing routines in InputProcessor.  GetObjectItemNum uses the "standard"
    // convention of the Name of the item/object being the first Alpha Argument.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ItemNum;

    IsNotOK = false;

    ItemNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, CompType, CompValType, CompName);

    if (ItemNum < 0) {
        ShowSevereError(state, "During " + CallString + " Input, Invalid Component Type input=" + CompType);
        ShowContinueError(state, "Component name=" + CompName);
        IsNotOK = true;
    } else if (ItemNum == 0) {
        ShowSevereError(state, "During " + CallString + " Input, Invalid Component Name input=" + CompName);
        ShowContinueError(state, "Component type=" + CompType);
        IsNotOK = true;
    }
}

void CalcPassiveExteriorBaffleGap(EnergyPlusData &state,
                                  const Array1D_int &SurfPtrARR, // Array of indexes pointing to Surface structure in DataSurfaces
                                  Real64 const VentArea,         // Area available for venting the gap [m2]
                                  Real64 const Cv,               // Orifice coefficient for volume-based discharge, wind-driven [--]
                                  Real64 const Cd,               // Orifice coefficient for discharge,  bouyancy-driven [--]
                                  Real64 const HdeltaNPL,        // Height difference from neutral pressure level [m]
                                  Real64 const SolAbs,           // solar absorptivity of baffle [--]
                                  Real64 const AbsExt,           // thermal absorptance/emittance of baffle material [--]
                                  Real64 const Tilt,             // Tilt of gap [Degrees]
                                  Real64 const AspRat,           // aspect ratio of gap  Height/gap [--]
                                  Real64 const GapThick,         // Thickness of air space between baffle and underlying heat transfer surface
                                  int const Roughness,           // Roughness index (1-6), see DataHeatBalance parameters
                                  Real64 const QdotSource,       // Source/sink term, e.g. electricity exported from solar cell [W]
                                  Real64 &TsBaffle,              // Temperature of baffle (both sides) use lagged value on input [C]
                                  Real64 &TaGap,                 // Temperature of air gap (assumed mixed) use lagged value on input [C]
                                  Optional<Real64> HcGapRpt,
                                  Optional<Real64> HrGapRpt,
                                  Optional<Real64> IscRpt,
                                  Optional<Real64> MdotVentRpt,
                                  Optional<Real64> VdotWindRpt,
                                  Optional<Real64> VdotBouyRpt)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B.T. Griffith
    //       DATE WRITTEN   November 2004
    //       MODIFIED       BG March 2007 outdoor conditions from surface for height-dependent conditions
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // model the effect of the a ventilated baffle covering the outside of a heat transfer surface.
    // return calculated temperatures and certain intermediate values for reporting

    // METHODOLOGY EMPLOYED:
    // Heat balances on baffle and air space.
    // Natural ventilation calculations use bouyancy and wind.

    // REFERENCES:
    // Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

    // Using/Aliasing
    using ConvectionCoefficients::InitExteriorConvectionCoeff;
    using DataSurfaces::SurfaceData;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdbTwbPb;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const g(9.807);          // gravitational constant (m/s**2)
    Real64 const nu(15.66e-6);      // kinematic viscosity (m**2/s) for air at 300 K (Mills 1999 Heat Transfer)
    Real64 const k(0.0267);         // thermal conductivity (W/m K) for air at 300 K (Mills 1999 Heat Transfer)
    Real64 const Sigma(5.6697e-08); // Stefan-Boltzmann constant
    static std::string const RoutineName("CalcPassiveExteriorBaffleGap");
    // INTERFACE BLOCK SPECIFICATIONS:

    // DERIVED TYPE DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    // following arrays are used to temporarily hold results from multiple underlying surfaces
    Array1D<Real64> HSkyARR;
    Array1D<Real64> HGroundARR;
    Array1D<Real64> HAirARR;
    Array1D<Real64> HPlenARR;
    Array1D<Real64> HExtARR;
    Array1D<Real64> LocalWindArr;

    // local working variables
    Real64 RhoAir;                // density of air
    Real64 CpAir;                 // specific heat of air
    Real64 Tamb;                  // outdoor drybulb
    Real64 A;                     // projected area of baffle from sum of underlying surfaces
    Real64 HcPlen;                // surface convection heat transfer coefficient for plenum surfaces
    int ThisSurf;                 // do loop counter
    int NumSurfs;                 // number of underlying HT surfaces associated with UTSC
    Real64 TmpTsBaf;              // baffle temperature
    int SurfPtr;                  // index of surface in main surface structure
    Real64 HMovInsul;             // dummy for call to InitExteriorConvectionCoeff
    Real64 HExt;                  // dummy for call to InitExteriorConvectionCoeff
    int ConstrNum;                // index of construction in main construction structure
    Real64 AbsThermSurf;          // thermal emmittance of underlying wall.
    Real64 TsoK;                  // underlying surface temperature in Kelvin
    Real64 TsBaffK;               // baffle temperature in Kelvin  (lagged)
    Real64 Vwind;                 // localized, and area-weighted average for wind speed
    Real64 HrSky;                 // radiation coeff for sky, area-weighted average
    Real64 HrGround;              // radiation coeff for ground, area-weighted average
    Real64 HrAtm;                 // radiation coeff for air (bulk atmosphere), area-weighted average
    Real64 Isc;                   // Incoming combined solar radiation, area-weighted average
    Real64 HrPlen;                // radiation coeff for plenum surfaces, area-weighted average
    Real64 Tso;                   // temperature of underlying surface, area-weighted average
    Real64 TmeanK;                // average of surface temps , for Beta in Grashoff no.
    Real64 Gr;                    // Grasshof number for natural convection calc
    Real64 VdotWind;              // volume flow rate of nat. vent due to wind
    Real64 VdotThermal;           // Volume flow rate of nat. vent due to bouyancy
    Real64 VdotVent;              // total volume flow rate of nat vent
    Real64 MdotVent;              // total mass flow rate of nat vent
    Real64 NuPlen;                // Nusselt No. for plenum Gap
    Real64 LocalOutDryBulbTemp;   // OutDryBulbTemp for here
    Real64 LocalWetBulbTemp;      // OutWetBulbTemp for here
    Real64 LocalOutHumRat;        // OutHumRat for here
    bool ICSCollectorIsOn(false); // ICS collector has OSCM on
    int CollectorNum;             // current solar collector index
    Real64 ICSWaterTemp;          // ICS solar collector water temp
    Real64 ICSULossbottom;        // ICS solar collector bottom loss Conductance
    Real64 sum_area = 0.0;
    Real64 sum_produc_area_drybulb = 0.0;
    Real64 sum_produc_area_wetbulb = 0.0;
    for (int SurfNum : SurfPtrARR) {
        sum_area += state.dataSurface->Surface(SurfNum).Area;
        sum_produc_area_drybulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutDryBulbTemp(SurfNum);
        sum_produc_area_wetbulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutWetBulbTemp(SurfNum);
    }
    //    LocalOutDryBulbTemp = sum( Surface( SurfPtrARR ).Area * Surface( SurfPtrARR ).OutDryBulbTemp ) / sum( Surface( SurfPtrARR ).Area );
    LocalOutDryBulbTemp = sum_produc_area_drybulb / sum_area; // Autodesk:F2C++ Functions handle array subscript usage
    //    LocalWetBulbTemp = sum( Surface( SurfPtrARR ).Area * Surface( SurfPtrARR ).OutWetBulbTemp ) / sum( Surface( SurfPtrARR ).Area );
    LocalWetBulbTemp = sum_produc_area_wetbulb / sum_area;

    LocalOutHumRat = PsyWFnTdbTwbPb(state, LocalOutDryBulbTemp, LocalWetBulbTemp, state.dataEnvrn->OutBaroPress, RoutineName);

    RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, LocalOutDryBulbTemp, LocalOutHumRat, RoutineName);
    CpAir = PsyCpAirFnW(LocalOutHumRat);
    if (!state.dataEnvrn->IsRain) {
        Tamb = LocalOutDryBulbTemp;
    } else { // when raining we use wetbulb not drybulb
        Tamb = LocalWetBulbTemp;
    }
    //    A = sum( Surface( SurfPtrARR ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
    A = sum_area;
    TmpTsBaf = TsBaffle;

    // loop through underlying surfaces and collect needed data
    NumSurfs = size(SurfPtrARR);
    HSkyARR.dimension(NumSurfs, 0.0);
    HGroundARR.dimension(NumSurfs, 0.0);
    HAirARR.dimension(NumSurfs, 0.0);
    LocalWindArr.dimension(NumSurfs, 0.0);
    HPlenARR.dimension(NumSurfs, 0.0);
    HExtARR.dimension(NumSurfs, 0.0);

    for (ThisSurf = 1; ThisSurf <= NumSurfs; ++ThisSurf) {
        SurfPtr = SurfPtrARR(ThisSurf);
        // Initializations for this surface
        HMovInsul = 0.0;
        LocalWindArr(ThisSurf) = state.dataSurface->SurfOutWindSpeed(SurfPtr);
        InitExteriorConvectionCoeff(
            state, SurfPtr, HMovInsul, Roughness, AbsExt, TmpTsBaf, HExtARR(ThisSurf), HSkyARR(ThisSurf), HGroundARR(ThisSurf), HAirARR(ThisSurf));
        ConstrNum = state.dataSurface->Surface(SurfPtr).Construction;
        AbsThermSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;
        TsoK = state.dataHeatBalSurf->TH(1, 1, SurfPtr) + DataGlobalConstants::KelvinConv;
        TsBaffK = TmpTsBaf + DataGlobalConstants::KelvinConv;
        if (TsBaffK == TsoK) {        // avoid divide by zero
            HPlenARR(ThisSurf) = 0.0; // no net heat transfer if same temperature
        } else {
            HPlenARR(ThisSurf) = Sigma * AbsExt * AbsThermSurf * (pow_4(TsBaffK) - pow_4(TsoK)) / (TsBaffK - TsoK);
        }
        // Added for ICS collector OSCM
        if (state.dataSurface->SurfIsICS(SurfPtr)) {
            ICSCollectorIsOn = true;
            CollectorNum = state.dataSurface->SurfICSPtr(SurfPtr);
        }
    }

    if (ICSCollectorIsOn) {
        if (state.dataGlobal->BeginEnvrnFlag && state.dataGeneralRoutines->MyICSEnvrnFlag) {
            ICSULossbottom = 0.40;
            ICSWaterTemp = 20.0;
        } else {
            if (!state.dataSolarCollectors->Collector.allocated()) {
                ICSULossbottom = 0.40;
                ICSWaterTemp = 20.0;
            } else {
                ICSULossbottom = state.dataSolarCollectors->Collector(CollectorNum).UbLoss;
                ICSWaterTemp = state.dataSolarCollectors->Collector(CollectorNum).TempOfWater;
                state.dataGeneralRoutines->MyICSEnvrnFlag = false;
            }
        }
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataGeneralRoutines->MyICSEnvrnFlag = true;
    }
    if (A == 0.0) { // should have been caught earlier
    }
    auto Area(array_sub(state.dataSurface->Surface,
                        &SurfaceData::Area,
                        SurfPtrARR)); // Autodesk:F2C++ Copy of subscripted Area array for use below: This makes a copy so review wrt performance
    // now figure area-weighted averages from underlying surfaces.
    //    Vwind = sum( LocalWindArr * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    Vwind = sum(LocalWindArr * Area) / A;
    LocalWindArr.deallocate();
    //    HrSky = sum( HSkyARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    HrSky = sum(HSkyARR * Area) / A;
    HSkyARR.deallocate();
    //    HrGround = sum( HGroundARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    HrGround = sum(HGroundARR * Area) / A;
    HGroundARR.deallocate();
    //    HrAtm = sum( HAirARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    HrAtm = sum(HAirARR * Area) / A;
    HAirARR.deallocate();
    //    HrPlen = sum( HPlenARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    HrPlen = sum(HPlenARR * Area) / A;
    HPlenARR.deallocate();
    //    HExt = sum( HExtARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    HExt = sum(HExtARR * Area) / A;
    HExtARR.deallocate();

    if (state.dataEnvrn->IsRain) HExt = 1000.0;

    //    Tso = sum( TH( 1, 1, SurfPtrARR ) * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    Tso = sum_product_sub(state.dataHeatBalSurf->TH(1, 1, _), state.dataSurface->Surface, &SurfaceData::Area, SurfPtrARR) /
          A; // Autodesk:F2C++ Functions handle array subscript usage
    //    Isc = sum( QRadSWOutIncident( SurfPtrARR ) * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
    Isc = sum_product_sub(state.dataHeatBal->SurfQRadSWOutIncident, state.dataSurface->Surface, &SurfaceData::Area, SurfPtrARR) /
          A; // Autodesk:F2C++ Functions handle array subscript usage

    TmeanK = 0.5 * (TmpTsBaf + Tso) + DataGlobalConstants::KelvinConv;

    Gr = g * pow_3(GapThick) * std::abs(Tso - TmpTsBaf) * pow_2(RhoAir) / (TmeanK * pow_2(nu));

    PassiveGapNusseltNumber(AspRat, Tilt, TmpTsBaf, Tso, Gr, NuPlen); // intentionally switch Tso to Tsi

    HcPlen = NuPlen * (k / GapThick);

    // now model natural ventilation of plenum gap.
    VdotWind = Cv * (VentArea / 2.0) * Vwind;

    if (TaGap > Tamb) {
        VdotThermal = Cd * (VentArea / 2.0) * std::sqrt(2.0 * g * HdeltaNPL * (TaGap - Tamb) / (TaGap + DataGlobalConstants::KelvinConv));
    } else if (TaGap == Tamb) {
        VdotThermal = 0.0;
    } else {
        if ((std::abs(Tilt) < 5.0) || (std::abs(Tilt - 180.0) < 5.0)) {
            VdotThermal = 0.0; // stable bouyancy situation
        } else {
            VdotThermal = Cd * (VentArea / 2.0) * std::sqrt(2.0 * g * HdeltaNPL * (Tamb - TaGap) / (Tamb + DataGlobalConstants::KelvinConv));
        }
    }

    VdotVent = VdotWind + VdotThermal;
    MdotVent = VdotVent * RhoAir;

    // now calculate baffle temperature
    if (!ICSCollectorIsOn) {
        TsBaffle = (Isc * SolAbs + HExt * Tamb + HrAtm * Tamb + HrSky * state.dataEnvrn->SkyTemp + HrGround * Tamb + HrPlen * Tso + HcPlen * TaGap +
                    QdotSource) /
                   (HExt + HrAtm + HrSky + HrGround + HrPlen + HcPlen);
    } else {

        TsBaffle = (ICSULossbottom * ICSWaterTemp + HrPlen * Tso + HcPlen * TaGap + QdotSource) / (ICSULossbottom + HrPlen + HcPlen);
    }
    // now calculate gap air temperature

    TaGap = (HcPlen * A * Tso + MdotVent * CpAir * Tamb + HcPlen * A * TsBaffle) / (HcPlen * A + MdotVent * CpAir + HcPlen * A);

    if (present(HcGapRpt)) HcGapRpt = HcPlen;
    if (present(HrGapRpt)) HrGapRpt = HrPlen;
    if (present(IscRpt)) IscRpt = Isc;
    if (present(MdotVentRpt)) MdotVentRpt = MdotVent;
    if (present(VdotWindRpt)) VdotWindRpt = VdotWind;
    if (present(VdotBouyRpt)) VdotBouyRpt = VdotThermal;
}

//****************************************************************************

void PassiveGapNusseltNumber(Real64 const AspRat, // Aspect Ratio of Gap height to gap width
                             Real64 const Tilt,   // Tilt of gap, degrees
                             Real64 const Tso,    // Temperature of gap surface closest to outside (K)
                             Real64 const Tsi,    // Temperature of gap surface closest to zone (K)
                             Real64 const Gr,     // Gap gas Grashof number
                             Real64 &gNu          // Gap gas Nusselt number
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Adapted by B. Griffith from Fred Winkelmann's from NusseltNumber in WindowManager.cc
    //       DATE WRITTEN   September 2001
    //       MODIFIED       B. Griffith November 2004  (same models but slightly different for general use)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Finds the Nusselt number for air-filled gaps between isothermal solid layers.

    // METHODOLOGY EMPLOYED:
    // Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
    // "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
    // The equation numbers below correspond to those in the standard.

    // REFERENCES:
    // Window5 source code; ISO 15099

    // Using/Aliasing
    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const Pr(0.71); // Prandtl number for air

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS
    Real64 Ra;     // Rayleigh number
    Real64 gnu901; // Nusselt number temporary variables for
    Real64 gnu902;
    Real64 gnu90;
    Real64 gnu601;
    Real64 gnu602; // different tilt and Ra ranges
    Real64 gnu60;
    Real64 gnu601a;
    Real64 gnua;
    Real64 gnub;
    Real64 cra; // Temporary variables
    Real64 a;
    Real64 b;
    Real64 g;
    Real64 ang;
    Real64 tiltr;

    tiltr = Tilt * DataGlobalConstants::DegToRadians;
    Ra = Gr * Pr;

    if (Ra > 2.0e6) {

        // write(*,*)' error, outside range of Rayleigh number'
    }

    if (Ra <= 1.0e4) {
        gnu901 = 1.0 + 1.7596678e-10 * std::pow(Ra, 2.2984755); // eq. 51
    }
    if (Ra > 1.0e4 && Ra <= 5.0e4) gnu901 = 0.028154 * std::pow(Ra, 0.4134); // eq. 50
    if (Ra > 5.0e4) gnu901 = 0.0673838 * std::pow(Ra, 1.0 / 3.0);            // eq. 49

    gnu902 = 0.242 * std::pow(Ra / AspRat, 0.272); // eq. 52
    gnu90 = max(gnu901, gnu902);

    if (Tso > Tsi) {                                 // window heated from above
        gNu = 1.0 + (gnu90 - 1.0) * std::sin(tiltr); // eq. 53
    } else {                                         // window heated from below
        if (Tilt >= 60.0) {
            g = 0.5 * std::pow(1.0 + std::pow(Ra / 3160.0, 20.6), -0.1);     // eq. 47
            gnu601a = 1.0 + pow_7(0.0936 * std::pow(Ra, 0.314) / (1.0 + g)); // eq. 45
            gnu601 = std::pow(gnu601a, 0.142857);

            // For any aspect ratio
            gnu602 = (0.104 + 0.175 / AspRat) * std::pow(Ra, 0.283); // eq. 46
            gnu60 = max(gnu601, gnu602);

            // linear interpolation for layers inclined at angles between 60 and 90 deg
            gNu = ((90.0 - Tilt) * gnu60 + (Tilt - 60.0) * gnu90) / 30.0;
        }
        if (Tilt < 60.0) { // eq. 42
            cra = Ra * std::cos(tiltr);
            a = 1.0 - 1708.0 / cra;
            b = std::pow(cra / 5830.0, 0.33333) - 1.0;
            gnua = (std::abs(a) + a) / 2.0;
            gnub = (std::abs(b) + b) / 2.0;
            ang = 1708.0 * std::pow(std::sin(1.8 * tiltr), 1.6);
            gNu = 1.0 + 1.44 * gnua * (1.0 - ang / cra) + gnub;
        }
    }
}

void CalcBasinHeaterPower(EnergyPlusData &state,
                          Real64 const Capacity,     // Basin heater capacity per degree C below setpoint (W/C)
                          int const SchedulePtr,     // Pointer to basin heater schedule
                          Real64 const SetPointTemp, // setpoint temperature for basin heater operation (C)
                          Real64 &Power              // Basin heater power (W)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma, FSEC
    //       DATE WRITTEN   Feb 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // To calculate basin heater power when the evaporative cooled equipment is not operating
    // and outdoor air dry-bulb temperature is below the set-point

    // METHODOLOGY EMPLOYED:
    // Checks to see whether schedule for basin heater exists or not. If the schedule exists,
    // the basin heater is operated for the schedule specified otherwise the heater runs
    // for the entire simulation timestep whenever the outdoor temperature is below setpoint
    // and water is not flowing through the evaporative cooled equipment.

    // REFERENCES:
    // na

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 BasinHeaterSch; // Schedule for basin heater operation

    Power = 0.0;
    // Operate basin heater anytime outdoor temperature is below setpoint and water is not flowing through the equipment
    // IF schedule exists, basin heater performance can be scheduled OFF
    if (SchedulePtr > 0) {
        BasinHeaterSch = GetCurrentScheduleValue(state, SchedulePtr);
        if (Capacity > 0.0 && BasinHeaterSch > 0.0) {
            Power = max(0.0, Capacity * (SetPointTemp - state.dataEnvrn->OutDryBulbTemp));
        }
    } else {
        // IF schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
        if (Capacity > 0.0) {
            Power = max(0.0, Capacity * (SetPointTemp - state.dataEnvrn->OutDryBulbTemp));
        }
    }
}

void TestAirPathIntegrity(EnergyPlusData &state, bool &ErrFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests supply, return and overall air path integrity.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataLoopNode;
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // COMPILER-GENERATED INTERFACE MODULE: Thu Sep 29 07:54:46 2011

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int Loop1;
    int Loop2;
    int Loop3;
    int Count;
    int TestNode;
    bool errFlag;
    Array2D_int ValRetAPaths;
    Array2D_int NumRAPNodes;
    Array2D_int ValSupAPaths;
    Array2D_int NumSAPNodes;

    NumSAPNodes.allocate(state.dataLoopNodes->NumOfNodes, NumPrimaryAirSys);
    NumRAPNodes.allocate(state.dataLoopNodes->NumOfNodes, NumPrimaryAirSys);
    ValRetAPaths.allocate(state.dataLoopNodes->NumOfNodes, NumPrimaryAirSys);
    ValSupAPaths.allocate(state.dataLoopNodes->NumOfNodes, NumPrimaryAirSys);
    NumSAPNodes = 0;
    NumRAPNodes = 0;
    ValRetAPaths = 0;
    ValSupAPaths = 0;

    TestSupplyAirPathIntegrity(state, errFlag);
    if (errFlag) ErrFound = true;
    TestReturnAirPathIntegrity(state, errFlag, ValRetAPaths);
    if (errFlag) ErrFound = true;

    // Final tests, look for duplicate nodes
    for (Loop = 1; Loop <= NumPrimaryAirSys; ++Loop) {
        if (ValRetAPaths(1, Loop) != 0) continue;
        if (state.dataAirLoop->AirToZoneNodeInfo(Loop).NumReturnNodes <= 0) continue;
        ValRetAPaths(1, Loop) = state.dataAirLoop->AirToZoneNodeInfo(Loop).ZoneEquipReturnNodeNum(1);
    }

    for (Loop = 1; Loop <= NumPrimaryAirSys; ++Loop) {
        for (Loop1 = 1; Loop1 <= state.dataLoopNodes->NumOfNodes; ++Loop1) {
            TestNode = ValRetAPaths(Loop1, Loop);
            Count = 0;
            for (Loop2 = 1; Loop2 <= NumPrimaryAirSys; ++Loop2) {
                for (Loop3 = 1; Loop3 <= state.dataLoopNodes->NumOfNodes; ++Loop3) {
                    if (Loop2 == Loop && Loop1 == Loop3) continue; // Don't count test node
                    if (ValRetAPaths(Loop3, Loop2) == 0) break;
                    if (ValRetAPaths(Loop3, Loop2) == TestNode) ++Count;
                }
            }
            if (Count > 0) {
                ShowSevereError(state, "Duplicate Node detected in Return Air Paths");
                ShowContinueError(state, "Test Node=" + state.dataLoopNodes->NodeID(TestNode));
                ShowContinueError(state, "In Air Path=" + state.dataAirLoop->AirToZoneNodeInfo(Loop).AirLoopName);
                ErrFound = true;
            }
        }
    }

    NumSAPNodes.deallocate();
    NumRAPNodes.deallocate();
    ValRetAPaths.deallocate();
    ValSupAPaths.deallocate();
}

void TestSupplyAirPathIntegrity(EnergyPlusData &state, bool &ErrFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests supply air path integrity and displays the loop for each branch.
    // Also, input and output nodes.

    // Using/Aliasing
    using namespace DataLoopNode;
    auto &GetZoneSplitterInput(SplitterComponent::GetSplitterInput);
    using namespace DataZoneEquipment;
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Count;
    std::string AirPathNodeName;    // Air Path Inlet Node Name
    std::string PrimaryAirLoopName; // Air Loop to which this supply air path is connected
    Array1D_bool FoundSupplyPlenum;
    Array1D_bool FoundZoneSplitter;
    Array1D_string FoundNames;
    int NumErr(0); // Error Counter //Autodesk:Init Initialization added
    int BCount;
    int Found;
    int Count1;
    int Count2;

    // Do by Paths
    ShowMessage(state, "Testing Individual Supply Air Path Integrity");
    ErrFound = false;

    print(state.files.bnd, "{}\n", "! ===============================================================");
    static constexpr auto Format_700("! <#Supply Air Paths>,<Number of Supply Air Paths>");
    print(state.files.bnd, "{}\n", Format_700);
    print(state.files.bnd, " #Supply Air Paths,{}\n", state.dataZoneEquip->NumSupplyAirPaths);
    static constexpr auto Format_702("! <Supply Air Path>,<Supply Air Path Count>,<Supply Air Path Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_702);
    static constexpr auto Format_703("! <#Components on Supply Air Path>,<Number of Components>");
    print(state.files.bnd, "{}\n", Format_703);
    static constexpr auto Format_704("! <Supply Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_704);
    static constexpr auto Format_707("! <#Outlet Nodes on Supply Air Path Component>,<Number of Nodes>");
    print(state.files.bnd, "{}\n", Format_707);
    static constexpr auto Format_708("! <Supply Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,<Inlet Node Name>,<Outlet "
                                     "Node Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_708);

    for (BCount = 1; BCount <= state.dataZoneEquip->NumSupplyAirPaths; ++BCount) {

        // Determine which air loop this supply air path is connected to
        Found = 0;
        for (Count1 = 1; Count1 <= NumPrimaryAirSys; ++Count1) {
            PrimaryAirLoopName = state.dataAirLoop->AirToZoneNodeInfo(Count1).AirLoopName;
            Found = 0;
            for (Count2 = 1; Count2 <= state.dataAirLoop->AirToZoneNodeInfo(Count1).NumSupplyNodes; ++Count2) {
                if (state.dataZoneEquip->SupplyAirPath(BCount).InletNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(Count1).ZoneEquipSupplyNodeNum(Count2))
                    Found = Count2;
            }
            if (Found != 0) break;
        }
        if (Found == 0) PrimaryAirLoopName = "**Unknown**";

        print(state.files.bnd, " Supply Air Path,{},{},{}\n", BCount, state.dataZoneEquip->SupplyAirPath(BCount).Name, PrimaryAirLoopName);
        print(state.files.bnd, "   #Components on Supply Air Path,{}\n", state.dataZoneEquip->SupplyAirPath(BCount).NumOfComponents);

        AirPathNodeName = state.dataLoopNodes->NodeID(state.dataZoneEquip->SupplyAirPath(BCount).InletNodeNum);

        for (Count = 1; Count <= state.dataZoneEquip->SupplyAirPath(BCount).NumOfComponents; ++Count) {

            print(state.files.bnd,
                  "   Supply Air Path Component,{},{},{},{}\n",
                  Count,
                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count),
                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count),
                  PrimaryAirLoopName);

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count)));

                if (SELECT_CASE_var == "AIRLOOPHVAC:SUPPLYPLENUM") {
                    for (Count2 = 1; Count2 <= state.dataZonePlenum->NumZoneSupplyPlenums; ++Count2) {
                        if (state.dataZonePlenum->ZoneSupPlenCond(Count2).ZonePlenumName !=
                            state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count))
                            continue;
                        if (Count == 1 && AirPathNodeName != state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneSupPlenCond(Count2).InletNode)) {
                            ShowSevereError(state, "Error in AirLoopHVAC:SupplyPath=" + state.dataZoneEquip->SupplyAirPath(BCount).Name);
                            ShowContinueError(state, "For AirLoopHVAC:SupplyPlenum=" + state.dataZonePlenum->ZoneSupPlenCond(Count2).ZonePlenumName);
                            ShowContinueError(state, "Expected inlet node (supply air path)=" + AirPathNodeName);
                            ShowContinueError(state,
                                              "Encountered node name (supply plenum)=" +
                                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneSupPlenCond(Count2).OutletNode(1)));
                            ErrFound = true;
                            ++NumErr;
                        }
                        print(state.files.bnd,
                              "     #Outlet Nodes on Supply Air Path Component,{}\n",
                              state.dataZonePlenum->ZoneSupPlenCond(Count2).NumOutletNodes);
                        for (Count1 = 1; Count1 <= state.dataZonePlenum->ZoneSupPlenCond(Count2).NumOutletNodes; ++Count1) {
                            print(state.files.bnd,
                                  "     Supply Air Path Component Nodes,{},{},{},{},{},{}\n",
                                  Count1,
                                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count),
                                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count),
                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneSupPlenCond(Count2).InletNode),
                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneSupPlenCond(Count2).OutletNode(Count1)),
                                  PrimaryAirLoopName);
                        }
                    }

                } else if (SELECT_CASE_var == "AIRLOOPHVAC:ZONESPLITTER") {
                    for (Count2 = 1; Count2 <= state.dataSplitterComponent->NumSplitters; ++Count2) {
                        if (state.dataSplitterComponent->SplitterCond(Count2).SplitterName !=
                            state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count))
                            continue;
                        if (Count == 1 &&
                            AirPathNodeName != state.dataLoopNodes->NodeID(state.dataSplitterComponent->SplitterCond(Count2).InletNode)) {
                            ShowSevereError(state, "Error in AirLoopHVAC:SupplyPath=" + state.dataZoneEquip->SupplyAirPath(BCount).Name);
                            ShowContinueError(state,
                                              "For AirLoopHVAC:ZoneSplitter=" + state.dataSplitterComponent->SplitterCond(Count2).SplitterName);
                            ShowContinueError(state, "Expected inlet node (supply air path)=" + AirPathNodeName);
                            ShowContinueError(state,
                                              "Encountered node name (zone splitter)=" +
                                                  state.dataLoopNodes->NodeID(state.dataSplitterComponent->SplitterCond(Count2).InletNode));
                            ErrFound = true;
                            ++NumErr;
                        }
                        print(state.files.bnd,
                              "     #Outlet Nodes on Supply Air Path Component,{}\n",
                              state.dataSplitterComponent->SplitterCond(Count2).NumOutletNodes);
                        for (Count1 = 1; Count1 <= state.dataSplitterComponent->SplitterCond(Count2).NumOutletNodes; ++Count1) {
                            print(state.files.bnd,
                                  "     Supply Air Path Component Nodes,{},{},{},{},{},{}\n",
                                  Count1,
                                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count),
                                  state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count),
                                  state.dataLoopNodes->NodeID(state.dataSplitterComponent->SplitterCond(Count2).InletNode),
                                  state.dataLoopNodes->NodeID(state.dataSplitterComponent->SplitterCond(Count2).OutletNode(Count1)),
                                  PrimaryAirLoopName);
                        }
                    }

                } else {
                    ShowSevereError(state,
                                    "Invalid Component Type in Supply Air Path=" + state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count));
                    ErrFound = true;
                    ++NumErr;
                }
            }
        }

        if (state.dataZoneEquip->SupplyAirPath(BCount).NumNodes > 0) {
            static constexpr auto Format_705("! <#Nodes on Supply Air Path>,<Number of Nodes>");
            print(state.files.bnd, "{}\n", Format_705);
            static constexpr auto Format_706("! <Supply Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>");
            print(state.files.bnd, "{}\n", Format_706);
            print(state.files.bnd, "#Nodes on Supply Air Path,{}\n", state.dataZoneEquip->SupplyAirPath(BCount).NumNodes);
            for (Count2 = 1; Count2 <= state.dataZoneEquip->SupplyAirPath(BCount).NumNodes; ++Count2) {
                if (state.dataZoneEquip->SupplyAirPath(BCount).NodeType(Count2) == PathInlet) {
                    print(state.files.bnd,
                          "   Supply Air Path Node,Inlet Node,{},{},{}\n",
                          Count2,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->SupplyAirPath(BCount).Node(Count2)),
                          PrimaryAirLoopName);
                } else if (state.dataZoneEquip->SupplyAirPath(BCount).NodeType(Count2) == Intermediate) {
                    print(state.files.bnd,
                          "   Supply Air Path Node,Through Node,{},{},{}\n",
                          Count2,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->SupplyAirPath(BCount).Node(Count2)),
                          PrimaryAirLoopName);
                } else if (state.dataZoneEquip->SupplyAirPath(BCount).NodeType(Count2) == Outlet) {
                    print(state.files.bnd,
                          "   Supply Air Path Node,Outlet Node,{},{},{}\n",
                          Count2,
                          state.dataLoopNodes->NodeID(state.dataZoneEquip->SupplyAirPath(BCount).Node(Count2)),
                          PrimaryAirLoopName);
                }
            }
        }
    }

    if (state.dataSplitterComponent->NumSplitters == 0) {
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:ZoneSplitter") > 0) {
            GetZoneSplitterInput(state);
        }
    }
    if (state.dataZonePlenum->NumZoneSupplyPlenums == 0 && state.dataZonePlenum->NumZoneReturnPlenums == 0) {
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:SupplyPlenum") > 0) {
            ZonePlenum::GetZonePlenumInput(state);
        }
    }

    // now the reverse.  is every zone splitter and supply plenum on supply air path
    FoundSupplyPlenum.dimension(state.dataZonePlenum->NumZoneSupplyPlenums, false);
    FoundZoneSplitter.dimension(state.dataSplitterComponent->NumSplitters, false);
    FoundNames.allocate(state.dataZonePlenum->NumZoneSupplyPlenums);
    for (Count1 = 1; Count1 <= state.dataZonePlenum->NumZoneSupplyPlenums; ++Count1) {
        for (BCount = 1; BCount <= state.dataZoneEquip->NumSupplyAirPaths; ++BCount) {
            for (Count = 1; Count <= state.dataZoneEquip->SupplyAirPath(BCount).NumOfComponents; ++Count) {
                if (state.dataZonePlenum->ZoneSupPlenCond(Count1).ZonePlenumName != state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count) ||
                    state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count) != "AIRLOOPHVAC:SUPPLYPLENUM")
                    continue;
                if (FoundSupplyPlenum(Count1)) {
                    ShowSevereError(
                        state, "AirLoopHVAC:SupplyPlenum=\"" + state.dataZonePlenum->ZoneSupPlenCond(Count1).ZonePlenumName + "\", duplicate entry.");
                    ShowContinueError(state, "already exists on AirLoopHVAC:SupplyPath=\"" + FoundNames(Count1) + "\".");
                    ErrFound = true;
                } else {
                    // record use
                    FoundSupplyPlenum(Count1) = true;
                    FoundNames(Count1) = state.dataZoneEquip->SupplyAirPath(BCount).Name;
                }
            }
        }
    }
    FoundNames.deallocate();
    FoundNames.allocate(state.dataSplitterComponent->NumSplitters);
    for (Count1 = 1; Count1 <= state.dataSplitterComponent->NumSplitters; ++Count1) {
        for (BCount = 1; BCount <= state.dataZoneEquip->NumSupplyAirPaths; ++BCount) {
            for (Count = 1; Count <= state.dataZoneEquip->SupplyAirPath(BCount).NumOfComponents; ++Count) {
                if (state.dataSplitterComponent->SplitterCond(Count1).SplitterName !=
                        state.dataZoneEquip->SupplyAirPath(BCount).ComponentName(Count) ||
                    state.dataZoneEquip->SupplyAirPath(BCount).ComponentType(Count) != "AIRLOOPHVAC:ZONESPLITTER")
                    continue;
                if (FoundZoneSplitter(Count1)) {
                    ShowSevereError(state,
                                    "AirLoopHVAC:ZoneSplitter=\"" + state.dataSplitterComponent->SplitterCond(Count1).SplitterName +
                                        "\", duplicate entry.");
                    ShowContinueError(state, "already exists on AirLoopHVAC:SupplyPath=\"" + FoundNames(Count1) + "\".");
                    ErrFound = true;
                } else {
                    // record use
                    FoundZoneSplitter(Count1) = true;
                    FoundNames(Count1) = state.dataZoneEquip->SupplyAirPath(BCount).Name;
                }
            }
        }
    }
    FoundNames.deallocate();

    if (!all(FoundSupplyPlenum)) {
        for (Count1 = 1; Count1 <= state.dataZonePlenum->NumZoneSupplyPlenums; ++Count1) {
            if (FoundSupplyPlenum(Count1)) continue;
            ShowSevereError(state,
                            "AirLoopHVAC:SupplyPlenum=\"" + state.dataZonePlenum->ZoneSupPlenCond(Count1).ZonePlenumName +
                                "\", not found on any AirLoopHVAC:SupplyPath.");
            //      ErrFound=.TRUE.
        }
    }

    if (!all(FoundZoneSplitter)) {
        for (Count1 = 1; Count1 <= state.dataSplitterComponent->NumSplitters; ++Count1) {
            if (FoundZoneSplitter(Count1)) continue;
            ShowSevereError(state,
                            "AirLoopHVAC:ZoneSplitter=\"" + state.dataSplitterComponent->SplitterCond(Count1).SplitterName +
                                "\", not found on any AirLoopHVAC:SupplyPath.");
            //      ErrFound=.TRUE.
        }
    }

    FoundSupplyPlenum.deallocate();
    FoundZoneSplitter.deallocate();

    if (ErrFound) {
        ShowSevereError(state, "Supply Air Path(s) did not pass integrity testing");
    } else {
        ShowMessage(state, "All Supply Air Paths passed integrity testing");
    }
}

void TestReturnAirPathIntegrity(EnergyPlusData &state, bool &ErrFound, Array2S_int ValRetAPaths)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2003

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests return air path integrity and displays the loop for each branch.
    // Also, input and output nodes.

    // REFERENCES:
    // Return Air Path Validity Rules:
    //  Last component (zone mixer or zone return plenum) must resolve to
    //  be the outlet node for the return air path.  Inlets to this component must be outlets from
    //  previous components or "controlled zone outlets"?.
    //  (though converse not true -- each outlet in previous components do not
    //  have to be inlets on this item -- though they must be inputs somewhere in the stream).
    //  If multiple components and no mixer, then a zone return plenums "outlet" must
    //  be represented as an inlet on a later plenum.  i.e. some zone return plenums are
    //  really acting as "mixers" in a sense.  These do not need to be stepwise in succession.
    //  Same caveat for inlets from previous item.
    //  If multiple components and mixer, then prior condition (nested plenums) is allowed as long as
    //  those aren't duplicated as mixer inlets.  (i.e. zone rp 1 => zone rp 2 => zone mixer but
    //  zone rp 1 outlet should not also be inlet to mixer.
    //  Can have (nzrp -- nested zone return plenum, pzrp -- parallel zone return plenum):
    //  nzrp 1 => nzrp 2 & pzrp 3 => zm (inlets from nzrp 2 and pzrp 3).  Or, likewise:
    //  pzrp 1 & pzrp 2 => zm => pzrp 3 (outlets from pzrp 1/2 are inlets to zm whose outlet is an
    //  inlet to pzrp 3 whose outlet is the outlet for the return air path.

    //  Cannot have duplicate nodes in the "inlet" stream?  (i.e. cannot have same zone feeding two independent
    //  plenums, for example).  Similarly, Same return plenum can't be in two air loops nor as two independent
    //  return plenums in one return air path.

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataZoneEquipment;
    using namespace ZonePlenum;
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    auto &GetZoneMixerInput(MixerComponent::GetMixerInput);
    using HVACSingleDuctInduc::FourPipeInductionUnitHasMixer;
    using PoweredInductionUnits::PIUnitHasMixer;
    using PurchasedAirManager::CheckPurchasedAirForReturnPlenum;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int Count;
    std::string AirPathNodeName;    // Air Path Inlet Node Name
    std::string PrimaryAirLoopName; // Air Loop to which this return air path is connected
    Array1D_bool FoundReturnPlenum;
    Array1D_bool FoundZoneMixer;
    Array1D_string FoundNames;
    int NumErr; // Error Counter
    int BCount;
    int Found;
    int Count1;
    int Count2;
    Array1D_int AllNodes;
    int MixerCount;
    int Count3;
    int NumComp;
    int CountNodes;

    // Formats

    // Do by Paths
    ShowMessage(state, "Testing Individual Return Air Path Integrity");
    ErrFound = false;
    NumErr = 0;

    print(state.files.bnd, "{}\n", "! ===============================================================");
    static constexpr auto Format_700("! <#Return Air Paths>,<Number of Return Air Paths>");
    print(state.files.bnd, "{}\n", Format_700);
    print(state.files.bnd, " #Return Air Paths,{}\n", state.dataZoneEquip->NumReturnAirPaths);
    static constexpr auto Format_702("! <Return Air Path>,<Return Air Path Count>,<Return Air Path Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_702);
    static constexpr auto Format_703("! <#Components on Return Air Path>,<Number of Components>");
    print(state.files.bnd, "{}\n", Format_703);
    static constexpr auto Format_704("! <Return Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_704);
    static constexpr auto Format_707("! <#Inlet Nodes on Return Air Path Component>,<Number of Nodes>");
    print(state.files.bnd, "{}\n", Format_707);
    static constexpr auto Format_708("! <Return Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,<Inlet Node Name>,<Outlet "
                                     "Node Name>,<AirLoopHVAC Name>");
    print(state.files.bnd, "{}\n", Format_708);

    AllNodes.allocate(state.dataLoopNodes->NumOfNodes);

    for (BCount = 1; BCount <= state.dataZoneEquip->NumReturnAirPaths; ++BCount) {
        //             Determine which air loop this supply air path is connected to
        Found = 0;
        for (Count1 = 1; Count1 <= NumPrimaryAirSys; ++Count1) {
            PrimaryAirLoopName = state.dataAirLoop->AirToZoneNodeInfo(Count1).AirLoopName;
            Found = 0;
            for (Count2 = 1; Count2 <= state.dataAirLoop->AirToZoneNodeInfo(Count1).NumReturnNodes; ++Count2) {
                if (state.dataZoneEquip->ReturnAirPath(BCount).OutletNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(Count1).ZoneEquipReturnNodeNum(Count2))
                    Found = Count2;
            }
            if (Found != 0) break;
        }
        if (Found == 0) PrimaryAirLoopName = "**Unknown**";

        print(state.files.bnd, " Return Air Path,{},{},{}\n", BCount, state.dataZoneEquip->ReturnAirPath(BCount).Name, PrimaryAirLoopName);

        NumComp = state.dataZoneEquip->ReturnAirPath(BCount).NumOfComponents;
        print(state.files.bnd, "   #Components on Return Air Path,{}\n", NumComp);

        AirPathNodeName = state.dataLoopNodes->NodeID(state.dataZoneEquip->ReturnAirPath(BCount).OutletNodeNum);

        MixerCount = 0;
        for (Count = 1; Count <= NumComp; ++Count) {
            print(state.files.bnd,
                  "   Return Air Path Component,{},{},{},{}\n",
                  Count,
                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(Count),
                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(Count),
                  PrimaryAirLoopName);

            if (UtilityRoutines::SameString(state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(Count), "AirLoopHVAC:ZoneMixer")) {
                ++MixerCount;
            }
        }

        if (MixerCount > 1) {
            ShowSevereError(state, "Too many zone mixers in Return Air Path=" + state.dataZoneEquip->ReturnAirPath(BCount).Name);
            ErrFound = true;
            ++NumErr;
            continue;
        }

        AllNodes = 0;
        CountNodes = 0;

        if (NumComp > 0) {

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(NumComp)));

                if (SELECT_CASE_var == "AIRLOOPHVAC:ZONEMIXER") {
                    for (Count2 = 1; Count2 <= state.dataMixerComponent->NumMixers; ++Count2) {
                        if (state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp) !=
                            state.dataMixerComponent->MixerCond(Count2).MixerName)
                            continue;
                        // Found correct Mixer (by name), check outlet node vs. return air path outlet node
                        if (AirPathNodeName != state.dataLoopNodes->NodeID(state.dataMixerComponent->MixerCond(Count2).OutletNode)) {
                            ShowSevereError(state, "Error in Return Air Path=" + state.dataZoneEquip->ReturnAirPath(BCount).Name);
                            ShowContinueError(state, "For Connector:Mixer=" + state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp));
                            ShowContinueError(state, "Expected outlet node (return air path)=" + AirPathNodeName);
                            ShowContinueError(state,
                                              "Encountered node name (mixer)=" +
                                                  state.dataLoopNodes->NodeID(state.dataMixerComponent->MixerCond(Count2).OutletNode));
                            ErrFound = true;
                            ++NumErr;
                        } else {
                            ++CountNodes;
                            AllNodes(CountNodes) = state.dataMixerComponent->MixerCond(Count2).OutletNode;
                            for (Loop = 1; Loop <= state.dataMixerComponent->MixerCond(Count2).NumInletNodes; ++Loop) {
                                ++CountNodes;
                                AllNodes(CountNodes) = state.dataMixerComponent->MixerCond(Count2).InletNode(Loop);
                            }
                        }
                        print(state.files.bnd,
                              "     #Inlet Nodes on Return Air Path Component,{}\n",
                              state.dataMixerComponent->MixerCond(Count2).NumInletNodes);
                        for (Count1 = 1; Count1 <= state.dataMixerComponent->MixerCond(Count2).NumInletNodes; ++Count1) {
                            print(state.files.bnd,
                                  "     Return Air Path Component Nodes,{},{},{},{},{},{}\n",
                                  Count1,
                                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(NumComp),
                                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp),
                                  state.dataLoopNodes->NodeID(state.dataMixerComponent->MixerCond(Count2).InletNode(Count1)),
                                  state.dataLoopNodes->NodeID(state.dataMixerComponent->MixerCond(Count2).OutletNode),
                                  PrimaryAirLoopName);
                        }
                    }

                } else if (SELECT_CASE_var == "AIRLOOPHVAC:RETURNPLENUM") {
                    for (Count2 = 1; Count2 <= state.dataZonePlenum->NumZoneReturnPlenums; ++Count2) {
                        if (state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp) !=
                            state.dataZonePlenum->ZoneRetPlenCond(Count2).ZonePlenumName)
                            continue;
                        if (AirPathNodeName != state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneRetPlenCond(Count2).OutletNode)) {
                            ShowSevereError(state, "Error in Return Air Path=" + state.dataZoneEquip->ReturnAirPath(BCount).Name);
                            ShowContinueError(state,
                                              "For AirLoopHVAC:ReturnPlenum=" + state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp));
                            ShowContinueError(state, "Expected outlet node (return air path)=" + AirPathNodeName);
                            ShowContinueError(state,
                                              "Encountered node name (zone return plenum)=" +
                                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneRetPlenCond(Count2).OutletNode));
                            ErrFound = true;
                            ++NumErr;
                        } else {
                            ++CountNodes;
                            AllNodes(CountNodes) = state.dataZonePlenum->ZoneRetPlenCond(Count2).OutletNode;
                            for (Loop = 1; Loop <= state.dataZonePlenum->ZoneRetPlenCond(Count2).NumInletNodes; ++Loop) {
                                ++CountNodes;
                                AllNodes(CountNodes) = state.dataZonePlenum->ZoneRetPlenCond(Count2).InletNode(Loop);
                            }
                        }
                        print(state.files.bnd,
                              "     #Inlet Nodes on Return Air Path Component,{}\n",
                              state.dataZonePlenum->ZoneRetPlenCond(Count2).NumInletNodes);
                        for (Count1 = 1; Count1 <= state.dataZonePlenum->ZoneRetPlenCond(Count2).NumInletNodes; ++Count1) {
                            print(state.files.bnd,
                                  "     Return Air Path Component Nodes,{},{},{},{},{},{}\n",
                                  Count1,
                                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(NumComp),
                                  state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(NumComp),
                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneRetPlenCond(Count2).InletNode(Count1)),
                                  state.dataLoopNodes->NodeID(state.dataZonePlenum->ZoneRetPlenCond(Count2).OutletNode),
                                  PrimaryAirLoopName);
                        }
                    }

                } else {
                    // This already validated in GetReturnAirPath
                }
            }
        }

        if (NumComp > 1) {
            for (Count3 = 1; Count3 <= NumComp - 1; ++Count3) {
                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(Count3)));

                    if (SELECT_CASE_var == "AIRLOOPHVAC:ZONEMIXER") {
                        for (Count2 = 1; Count2 <= state.dataMixerComponent->NumMixers; ++Count2) {
                            if (state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(Count3) !=
                                state.dataMixerComponent->MixerCond(Count2).MixerName)
                                continue;
                            for (Loop = 1; Loop <= state.dataMixerComponent->MixerCond(Count2).NumInletNodes; ++Loop) {
                                ++CountNodes;
                                AllNodes(CountNodes) = state.dataMixerComponent->MixerCond(Count2).InletNode(Loop);
                            }
                        }

                    } else if (SELECT_CASE_var == "AIRLOOPHVAC:RETURNPLENUM") {
                        for (Count2 = 1; Count2 <= state.dataZonePlenum->NumZoneReturnPlenums; ++Count2) {
                            if (state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(Count3) !=
                                state.dataZonePlenum->ZoneRetPlenCond(Count2).ZonePlenumName)
                                continue;
                            for (Loop = 1; Loop <= state.dataZonePlenum->ZoneRetPlenCond(Count2).NumInletNodes; ++Loop) {
                                ++CountNodes;
                                AllNodes(CountNodes) = state.dataZonePlenum->ZoneRetPlenCond(Count2).InletNode(Loop);
                            }
                        }

                    } else {
                        // This already validated in GetReturnAirPath
                    }
                }
            }
        }
        if (CountNodes > 0) {
            static constexpr auto Format_705("! <#Nodes on Return Air Path>,<Number of Nodes>");
            print(state.files.bnd, "{}\n", Format_705);
            static constexpr auto Format_706("! <Return Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>");
            print(state.files.bnd, "{}\n", Format_706);
            print(state.files.bnd, "   #Nodes on Return Air Path,{}\n", CountNodes);
            for (Count2 = 1; Count2 <= CountNodes; ++Count2) {
                if (Count2 == 1) {
                    print(state.files.bnd,
                          "   Return Air Path Node,Outlet Node,{},{},{}\n",
                          Count2,
                          state.dataLoopNodes->NodeID(AllNodes(Count2)),
                          PrimaryAirLoopName);
                } else {
                    print(state.files.bnd,
                          "   Return Air Path Node,Inlet Node,{},{},{}\n",
                          Count2,
                          state.dataLoopNodes->NodeID(AllNodes(Count2)),
                          PrimaryAirLoopName);
                }
            }
        }
        // Determine Air Loop this Return Air Path is on
        for (Count2 = 1; Count2 <= NumPrimaryAirSys; ++Count2) {
            if (state.dataAirLoop->AirToZoneNodeInfo(Count2).NumReturnNodes > 0) {
                if (AllNodes(1) == state.dataAirLoop->AirToZoneNodeInfo(Count2).ZoneEquipReturnNodeNum(1)) {
                    const auto WAirLoop = Count2;
                    ValRetAPaths(_, WAirLoop) = 0;
                    ValRetAPaths({1, CountNodes}, WAirLoop) = AllNodes({1, CountNodes});
                    break;
                }
            } else {
                ShowWarningError(state,
                                 "TestReturnAirPathIntegrity: Air Loop has no Zone Equipment Return Node=" +
                                     state.dataAirLoop->AirToZoneNodeInfo(Count2).AirLoopName);
            }
        }
    }

    AllNodes.deallocate();

    if (state.dataMixerComponent->NumMixers == 0) {
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:ZoneMixer") > 0) {
            GetZoneMixerInput(state);
        }
    }
    if (state.dataZonePlenum->NumZoneSupplyPlenums == 0 && state.dataZonePlenum->NumZoneReturnPlenums == 0) {
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:ReturnPlenum") > 0) {
            GetZonePlenumInput(state);
        }
    }

    // now the reverse.  is every zone Mixer and Return plenum on Return air path
    FoundReturnPlenum.dimension(state.dataZonePlenum->NumZoneReturnPlenums, false);
    FoundZoneMixer.dimension(state.dataMixerComponent->NumMixers, false);
    FoundNames.allocate(state.dataZonePlenum->NumZoneReturnPlenums);
    for (Count1 = 1; Count1 <= state.dataZonePlenum->NumZoneReturnPlenums; ++Count1) {
        for (BCount = 1; BCount <= state.dataZoneEquip->NumReturnAirPaths; ++BCount) {
            for (Count = 1; Count <= state.dataZoneEquip->ReturnAirPath(BCount).NumOfComponents; ++Count) {
                if (state.dataZonePlenum->ZoneRetPlenCond(Count1).ZonePlenumName != state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(Count) ||
                    state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(Count) != "AIRLOOPHVAC:RETURNPLENUM")
                    continue;
                if (FoundReturnPlenum(Count1)) {
                    ShowSevereError(
                        state, "AirLoopHVAC:ReturnPlenum=\"" + state.dataZonePlenum->ZoneRetPlenCond(Count1).ZonePlenumName + "\", duplicate entry.");
                    ShowContinueError(state, "already exists on AirLoopHVAC:ReturnPath=\"" + FoundNames(Count1) + "\".");
                    ErrFound = true;
                } else {
                    // record use
                    FoundReturnPlenum(Count1) = true;
                    FoundNames(Count1) = state.dataZoneEquip->ReturnAirPath(BCount).Name;
                }
            }
        }
        if (CheckPurchasedAirForReturnPlenum(state, Count1)) FoundReturnPlenum(Count1) = true;
    }
    FoundNames.deallocate();
    FoundNames.allocate(state.dataMixerComponent->NumMixers);
    for (Count1 = 1; Count1 <= state.dataMixerComponent->NumMixers; ++Count1) {
        for (BCount = 1; BCount <= state.dataZoneEquip->NumReturnAirPaths; ++BCount) {
            for (Count = 1; Count <= state.dataZoneEquip->ReturnAirPath(BCount).NumOfComponents; ++Count) {
                if (state.dataMixerComponent->MixerCond(Count1).MixerName != state.dataZoneEquip->ReturnAirPath(BCount).ComponentName(Count) ||
                    state.dataZoneEquip->ReturnAirPath(BCount).ComponentType(Count) != "AIRLOOPHVAC:ZONEMIXER")
                    continue;
                if (FoundZoneMixer(Count1)) {
                    ShowSevereError(state,
                                    "AirLoopHVAC:ZoneMixer=\"" + state.dataMixerComponent->MixerCond(Count1).MixerName + "\", duplicate entry.");
                    ShowContinueError(state, "already exists on AirLoopHVAC:ReturnPath=\"" + FoundNames(Count1) + "\".");
                    ErrFound = true;
                } else {
                    // record use
                    FoundZoneMixer(Count1) = true;
                    FoundNames(Count1) = state.dataZoneEquip->ReturnAirPath(BCount).Name;
                }
            }
        }
        if (!FoundZoneMixer(Count1)) { // could be as child on other items
            // PIU Units
            if (PIUnitHasMixer(state, state.dataMixerComponent->MixerCond(Count1).MixerName)) FoundZoneMixer(Count1) = true;
        }
        if (!FoundZoneMixer(Count1)) { // could be as child on other items
            // fourPipeInduction units
            if (FourPipeInductionUnitHasMixer(state, state.dataMixerComponent->MixerCond(Count1).MixerName)) FoundZoneMixer(Count1) = true;
        }
    }
    FoundNames.deallocate();

    if (!all(FoundReturnPlenum)) {
        for (Count1 = 1; Count1 <= state.dataZonePlenum->NumZoneReturnPlenums; ++Count1) {
            if (FoundReturnPlenum(Count1)) continue;
            ShowSevereError(state,
                            "AirLoopHVAC:ReturnPlenum=\"" + state.dataZonePlenum->ZoneRetPlenCond(Count1).ZonePlenumName +
                                "\", not found on any AirLoopHVAC:ReturnPath.");
            //      ErrFound=.TRUE.
        }
    }

    if (!all(FoundZoneMixer)) {
        for (Count1 = 1; Count1 <= state.dataMixerComponent->NumMixers; ++Count1) {
            if (FoundZoneMixer(Count1)) continue;
            ShowSevereError(state,
                            "AirLoopHVAC:ZoneMixer=\"" + state.dataMixerComponent->MixerCond(Count1).MixerName +
                                "\", not found on any AirLoopHVAC:ReturnPath, AirTerminal:SingleDuct:SeriesPIU:Reheat,");
            ShowContinueError(state, "AirTerminal:SingleDuct:ParallelPIU:Reheat or AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction.");
            //      ErrFound=.TRUE.
        }
    }

    FoundReturnPlenum.deallocate();
    FoundZoneMixer.deallocate();

    if (ErrFound) {
        ShowSevereError(state, "Return Air Path(s) did not pass integrity testing");
    } else {
        ShowMessage(state, "All Return Air Paths passed integrity testing");
    }
}

void CalcComponentSensibleLatentOutput(Real64 const MassFlow,  // air mass flow rate, {kg/s}
                                       Real64 const TDB2,      // dry-bulb temperature at state 2 {C}
                                       Real64 const W2,        // humidity ratio at state 2
                                       Real64 const TDB1,      // dry-bulb temperature at  at state 1 {C}
                                       Real64 const W1,        // humidity ratio at state 1
                                       Real64 &SensibleOutput, // sensible output rate (state 2 -> State 1), {W}
                                       Real64 &LatentOutput,   // latent output rate (state 2 -> State 1), {W}
                                       Real64 &TotalOutput     // total = sensible + latent putput rate (state 2 -> State 1), {W}
)
{

    // Purpose:
    // returns total, sensible and latent heat rate of change of moist air transitioning
    // between two states. The moist air energy transfer can be cooling or heating process
    // across a cooling, a heating coil, or an HVAC component.

    // Methodology:
    // Q_total = m_dot * (h2 - h1)
    // Q_sensible = m_dot * Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(TDB2, W2, TDB1, W1);
    // or Q_sensible = m_dot * cp_moistair_MinHumRat * (TDB2 - TDB1)
    //    cp_moistair_MinHumRat = Psychrometrics::PsyCpAirFnW(min(W2, W1));
    // Q_latent = Q_total - Q_latent;

    TotalOutput = 0.0;
    LatentOutput = 0.0;
    SensibleOutput = 0.0;
    if (MassFlow > 0.0) {
        TotalOutput = MassFlow * (Psychrometrics::PsyHFnTdbW(TDB2, W2) - Psychrometrics::PsyHFnTdbW(TDB1, W1)); // total addition/removal rate, {W};
        SensibleOutput = MassFlow * Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(TDB2, W2, TDB1, W1); // sensible addition/removal rate, {W};
        LatentOutput = TotalOutput - SensibleOutput;                                                // latent addition/removal rate, {W}
    }
}

void CalcZoneSensibleLatentOutput(Real64 const MassFlow,  // air mass flow rate, {kg/s}
                                  Real64 const TDBEquip,  // dry-bulb temperature at equipment outlet {C}
                                  Real64 const WEquip,    // humidity ratio at equipment outlet
                                  Real64 const TDBZone,   // dry-bulb temperature at zone air node {C}
                                  Real64 const WZone,     // humidity ratio at zone air node
                                  Real64 &SensibleOutput, // sensible output rate (state 2 -> State 1), {W}
                                  Real64 &LatentOutput,   // latent output rate (state 2 -> State 1), {W}
                                  Real64 &TotalOutput     // total = sensible + latent putput rate (state 2 -> State 1), {W}
)
{

    // Purpose:
    // returns total, sensible and latent heat rate of transfer between the supply air zone inlet
    // node and zone air node. The moist air energy transfer can be cooling or heating depending
    // on the supply air zone inlet node and zone air node conditions.

    // Methodology:
    // Q_total = m_dot * (hEquip - hZone)
    // Q_sensible = m_dot * Psychrometrics::PsyDeltaHSenFnTdbEquipTdbWZone(TDBEquip, TDBZone, WZone);
    // or Q_sensible = m_dot * cp_moistair_zoneHumRat * (TDBEquip - TDBZone)
    //    cp_moistair_zoneHumRat = Psychrometrics::PsyCpAirFnW(WZone);
    // Q_latent = Q_total - Q_latent;

    TotalOutput = 0.0;
    LatentOutput = 0.0;
    SensibleOutput = 0.0;
    if (MassFlow > 0.0) {
        TotalOutput = MassFlow * (Psychrometrics::PsyHFnTdbW(TDBEquip, WEquip) -
                                  Psychrometrics::PsyHFnTdbW(TDBZone, WZone));                         // total addition/removal rate, {W};
        SensibleOutput = MassFlow * Psychrometrics::PsyDeltaHSenFnTdb2Tdb1W(TDBEquip, TDBZone, WZone); // sensible addition/removal rate, {W};
        LatentOutput = TotalOutput - SensibleOutput;                                                   // latent addition/removal rate, {W}
    }
}

void CalcZoneSensibleOutput(Real64 const MassFlow, // air mass flow rate, {kg/s}
                            Real64 const TDBEquip, // dry-bulb temperature at equipment outlet {C}
                            Real64 const TDBZone,  // dry-bulb temperature at zone air node {C}
                            Real64 const WZone,    // humidity ratio at zone air node
                            Real64 &SensibleOutput // sensible output rate (state 2 -> State 1), {W}
)
{

    // Purpose:
    // returns sensible heat rate of transfer between the supply air zone inlet node and
    // zone air node. The moist air energy transfer can be cooling or heating depending
    // on the supply air zone inlet node and zone air node conditions.

    // Methodology:
    // Q_sensible = m_dot * Psychrometrics::PsyDeltaHSenFnTdbEquipTdbWZone(TDBEquip, TDBZone, WZone);
    // or Q_sensible = m_dot * cp_moistair_zoneHumRat * (TDBEquip - TDBZone)
    //    cp_moistair_zoneHumRat = Psychrometrics::PsyCpAirFnW(WZone);

    SensibleOutput = 0.0;
    if (MassFlow > 0.0) {
        SensibleOutput = MassFlow * Psychrometrics::PsyDeltaHSenFnTdb2Tdb1W(TDBEquip, TDBZone, WZone); // sensible addition/removal rate, {W};
    }
}
} // namespace EnergyPlus
