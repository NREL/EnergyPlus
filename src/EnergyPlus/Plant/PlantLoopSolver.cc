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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/Plant/PlantLoopSolver.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantPressureSystem.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Pumps.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

    namespace PlantLoopSolver {

        // MODULE INFORMATION:
        //       AUTHOR         B. Griffith,  Dan Fisher, Sankaranarayanan K P, Rich Liesen, Edwin Lee
        //       DATE WRITTEN   Feb 2010
        //         This file developed from PlantSupplySideSolvers.cc by Sankaranarayanan K P, Rich Liesen, Dan Fisher
        //       MODIFIED       na
        //       RE-ENGINEERED  Aug 2010 Edwin Lee

        // PURPOSE OF THIS MODULE:
        // This module contains subroutines to solve plant half loops of various configurations.

        // METHODOLOGY EMPLOYED:
        // Main worker calling driver for plant loop system model
        // Calls various worker routines to model flow rates around a plant half loop
        // The procedural flow depends on the pump(s), loop side, and operation scheme at the time (and current flow lock?)

        // MODULE VARIABLE DEFINITIONS
        int RefrigIndex(0); // Index denoting refrigerant used (possibly steam)

        static std::string const fluidNameSteam("STEAM");

        // Functions
        void clear_state() {
            RefrigIndex = 0; // Index denoting refrigerant used (possibly steam)
        }

        void PlantHalfLoopSolver(bool const FirstHVACIteration, // TRUE if First HVAC iteration of Time step
                                 int const LoopSideNum,
                                 int const LoopNum,
                                 bool &ReSimOtherSideNeeded) {

            // SUBROUTINE INFORMATION:
            //       AUTHORS:         Dan Fisher, Sankaranarayanan K P, Edwin Lee
            //       DATE WRITTEN:    April 1998
            //       MODIFIED         June 2005(Work in the Plant Super Manager Module)
            //                        July 2006
            //       RE-ENGINEERED    July 2010

            // PURPOSE OF THIS SUBROUTINE:
            // SimSupplyFlowSolution is the driver routine for plant loops.  It performs
            //  the following tasks for each half loop (supply or demand side):
            // 1. Calculate flow request for half loop
            // 2. Predict Loop Flow
            // 3. Simulate the inlet branch
            // 4. Simulate the parallel branches, distributing load if necessary
            // 5. Set flow rates on parallel branches
            // 6. Simulate outlet branch and update node and report variables

            // METHODOLOGY EMPLOYED:
            // The algorithm operates on a predictor/corrector flow setting method by simulating all available loop components
            // based on component requested flow rates, then enforcing continuity on all loop branch flows by calling
            // the flow resolver and locking those flows down.  Available components are then re-simulated using the
            // corrected flow rates.

            // Initialize variables
            int ThisSide = LoopSideNum;
            int OtherSide = 3 - ThisSide; // will give us 1 if thisside is 2, or 2 if thisside is 1

            auto &thisPlantLoop = DataPlant::PlantLoop(LoopNum);
            auto &thisLoopSide = thisPlantLoop.LoopSide(ThisSide);
            int ThisSideInletNode = thisLoopSide.NodeNumIn;

            thisLoopSide.InitialDemandToLoopSetPoint = 0.0;
            thisLoopSide.CurrentAlterationsToDemand = 0.0;
            thisLoopSide.UpdatedDemandToLoopSetPoint = 0.0;

            // The following block is related to validating the flow control paths of the loop side
            // Since the control types are scheduled, I think BeginTimeStep should be a decent check frequency
            if (DataGlobals::BeginTimeStepFlag && thisLoopSide.OncePerTimeStepOperations) {

                // Initialize loop side controls -- could just be done for one loop since this routine inherently
                //  loops over all plant/condenser loops.  Not sure if the penalty is worth investigating.
                PlantCondLoopOperation::InitLoadDistribution(FirstHVACIteration);

                // Now that the op scheme types are updated, do LoopSide validation
                thisLoopSide.ValidateFlowControlPaths();

                // Set the flag to false so we won't do these again this time step
                thisLoopSide.OncePerTimeStepOperations = false;

            } else {

                // Set the flag to true so that it is activated for the next time step
                thisLoopSide.OncePerTimeStepOperations = true;
            }

            // Do pressure system initialize if this is the demand side (therefore once per whole loop)
            if (ThisSide == DataPlant::DemandSide)
                PlantPressureSystem::SimPressureDropSystem(LoopNum, FirstHVACIteration, DataPlant::PressureCall_Init);

            // Turn on any previously disabled branches due to constant speed branch pump issue
            thisLoopSide.TurnOnAllLoopSideBranches();

            // Do the actual simulation here every time
            thisLoopSide.DoFlowAndLoadSolutionPass(OtherSide, ThisSideInletNode, FirstHVACIteration);

            // On constant speed branch pump loop sides we need to resimulate
            if (thisLoopSide.hasConstSpeedBranchPumps) {
                // turn off any pumps connected to unloaded equipment and re-do the flow/load solution pass
                thisLoopSide.DisableAnyBranchPumpsConnectedToUnloadedEquipment();
                thisLoopSide.DoFlowAndLoadSolutionPass(OtherSide, ThisSideInletNode, FirstHVACIteration);
            }

            // A couple things are specific to which LoopSide we are on
            if (LoopSideNum == DataPlant::DemandSide) {

                // Pass the loop information via the HVAC interface manager
                HVACInterfaceManager::UpdatePlantLoopInterface(LoopNum,
                                                               LoopSideNum,
                                                               thisPlantLoop.LoopSide(DataPlant::DemandSide).NodeNumOut,
                                                               thisPlantLoop.LoopSide(DataPlant::SupplySide).NodeNumIn,
                                                               ReSimOtherSideNeeded,
                                                               thisPlantLoop.CommonPipeType);

            } else { // LoopSide == SupplySide

                // Update pressure drop reporting, calculate total loop pressure drop for use elsewhere
                PlantPressureSystem::SimPressureDropSystem(LoopNum, FirstHVACIteration, DataPlant::PressureCall_Update);

                // Pass the loop information via the HVAC interface manager (only the flow)
                HVACInterfaceManager::UpdatePlantLoopInterface(LoopNum,
                                                               LoopSideNum,
                                                               thisPlantLoop.LoopSide(DataPlant::SupplySide).NodeNumOut,
                                                               thisPlantLoop.LoopSide(DataPlant::DemandSide).NodeNumIn,
                                                               ReSimOtherSideNeeded,
                                                               thisPlantLoop.CommonPipeType);

                // Update the loop outlet node conditions
                thisPlantLoop.loopSolver.CheckLoopExitNode(LoopNum, FirstHVACIteration);
            }

            // Update some reporting information at Plant half loop level
            thisPlantLoop.loopSolver.UpdateLoopSideReportVars(LoopNum, LoopSideNum, thisLoopSide.InitialDemandToLoopSetPointSAVED,
                                                              thisLoopSide.LoadToLoopSetPointThatWasntMet);
        }

        void PlantLoopSolverClass::UpdateLoopSideReportVars(
                int const LoopNum,
                int const LoopSide,
                Real64 const OtherSideDemand,   // This is the 'other side' demand, based on other side flow
                Real64 const LocalRemLoopDemand // Unmet Demand after equipment has been simulated (report variable)
        ) {

            // SUBROUTINE INFORMATION:
            //       AUTHOR         Dan Fisher
            //       DATE WRITTEN   July 1998
            //       MODIFIED       Aug 2010 Edwin Lee -- add per LoopSide variable support
            //       RE-ENGINEERED  na

            // PURPOSE OF THIS SUBROUTINE:
            // Update the report variables

            // Using/Aliasing
            using DataLoopNode::Node;
            using DataPlant::PlantLoop;
            using DataPlant::PlantReport;
            using DataPlant::SupplySide;

            // Locals
            // SUBROUTINE ARGUMENT DEFINITIONS:
            // and delta T (inlet to SetPt)
            // This is evaluated once at the beginning of the loop side solver, before
            //  any of this side equipment alters it

            // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
            auto &this_supplyside(PlantLoop(LoopNum).LoopSide(SupplySide));
            auto &this_loop_report(PlantReport(LoopNum));

            if (LoopSide == SupplySide) {
                this_loop_report.InletNodeFlowrate = Node(this_supplyside.NodeNumIn).MassFlowRate;
                this_loop_report.InletNodeTemperature = Node(this_supplyside.NodeNumIn).Temp;
                this_loop_report.OutletNodeFlowrate = Node(this_supplyside.NodeNumOut).MassFlowRate;
                this_loop_report.OutletNodeTemperature = Node(this_supplyside.NodeNumOut).Temp;

                // In the baseline code, only reported supply side demand. so putting in "SupplySide" IF block for now but might expand later
                if (OtherSideDemand < 0.0) {
                    this_loop_report.CoolingDemand = std::abs(OtherSideDemand);
                    this_loop_report.HeatingDemand = 0.0;
                    this_loop_report.DemandNotDispatched = -LocalRemLoopDemand; //  Setting sign based on old logic for now
                } else {
                    this_loop_report.HeatingDemand = OtherSideDemand;
                    this_loop_report.CoolingDemand = 0.0;
                    this_loop_report.DemandNotDispatched = LocalRemLoopDemand; //  Setting sign based on old logic for now
                }

                DataPlant::PlantLoop(LoopNum).loopSolver.CalcUnmetPlantDemand(LoopNum, LoopSide);
            }
        }

        void PlantLoopSolverClass::CalcUnmetPlantDemand(int const LoopNum, int const LoopSideNum) {

            // SUBROUTINE INFORMATION:
            //       AUTHOR         Brent Griffith
            //       DATE WRITTEN   June 2011
            //       MODIFIED       na
            //       RE-ENGINEERED  na

            // PURPOSE OF THIS SUBROUTINE:
            // determine the magnitude of unmet plant loads after the half loop simulation is done

            // METHODOLOGY EMPLOYED:
            // using the loop setpoint node, look at target vs current and
            // calculate a demand based on mass flow times specific heat times delta T

            // Using/Aliasing
            using DataLoopNode::Node;
            using DataLoopNode::NodeType_Steam;
            using DataLoopNode::NodeType_Water;
            using DataPlant::DualSetPointDeadBand;
            using DataPlant::LoopDemandTol;
            using DataPlant::PlantLoop;
            using DataPlant::PlantReport;
            using DataPlant::SingleSetPoint;
            using FluidProperties::GetSatEnthalpyRefrig;
            using FluidProperties::GetSpecificHeatGlycol;

            // SUBROUTINE PARAMETER DEFINITIONS:
            static std::string const RoutineName("PlantLoopSolver::EvaluateLoopSetPointLoad");
            static std::string const RoutineNameAlt("PlantSupplySide:EvaluateLoopSetPointLoad");

            //~ General variables
            Real64 MassFlowRate;
            Real64 TargetTemp;
            Real64 LoopSetPointTemperature;
            Real64 LoopSetPointTemperatureHi;
            Real64 LoopSetPointTemperatureLo;
            Real64 LoadToHeatingSetPoint;
            Real64 LoadToCoolingSetPoint;
            Real64 DeltaTemp;
            Real64 Cp;
            Real64 EnthalpySteamSatVapor;  // Enthalpy of saturated vapor
            Real64 EnthalpySteamSatLiquid; // Enthalpy of saturated liquid
            Real64 LatentHeatSteam;        // Latent heat of steam
            Real64 LoadToLoopSetPoint;

            // Initialize
            LoadToLoopSetPoint = 0.0;
            auto &this_loop(PlantLoop(LoopNum));

            // Get temperature at loop setpoint node.
            TargetTemp = Node(this_loop.TempSetPointNodeNum).Temp;
            MassFlowRate = Node(this_loop.TempSetPointNodeNum).MassFlowRate;

            if (this_loop.FluidType == NodeType_Water) {

                Cp = GetSpecificHeatGlycol(this_loop.FluidName, TargetTemp, this_loop.FluidIndex, RoutineName);

                {
                    auto const SELECT_CASE_var(this_loop.LoopDemandCalcScheme);

                    if (SELECT_CASE_var == SingleSetPoint) {

                        // Pick up the loop setpoint temperature
                        LoopSetPointTemperature = this_loop.LoopSide(LoopSideNum).TempSetPoint;
                        // Calculate the delta temperature
                        DeltaTemp = LoopSetPointTemperature - TargetTemp;

                        // Calculate the demand on the loop
                        LoadToLoopSetPoint = MassFlowRate * Cp * DeltaTemp;

                    } else if (SELECT_CASE_var == DualSetPointDeadBand) {

                        // Get the range of setpoints
                        LoopSetPointTemperatureHi = Node(this_loop.TempSetPointNodeNum).TempSetPointHi;
                        LoopSetPointTemperatureLo = Node(this_loop.TempSetPointNodeNum).TempSetPointLo;

                        // Calculate the demand on the loop
                        if (MassFlowRate > 0.0) {
                            LoadToHeatingSetPoint = MassFlowRate * Cp * (LoopSetPointTemperatureLo - TargetTemp);
                            LoadToCoolingSetPoint = MassFlowRate * Cp * (LoopSetPointTemperatureHi - TargetTemp);
                            // Possible combinations:
                            // 1  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
                            // 2  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
                            // 3  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
                            // 4  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Not Feasible if LoopSetPointHi >= LoopSetPointLo
                            if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
                                LoadToLoopSetPoint = LoadToHeatingSetPoint;
                            } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
                                LoadToLoopSetPoint = LoadToCoolingSetPoint;
                            } else if (LoadToHeatingSetPoint <= 0.0 &&
                                       LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
                                LoadToLoopSetPoint = 0.0;
                            }
                        } else {
                            LoadToLoopSetPoint = 0.0;
                        }
                    }
                }

            } else if (this_loop.FluidType == NodeType_Steam) {

                Cp = GetSpecificHeatGlycol(this_loop.FluidName, TargetTemp, this_loop.FluidIndex, RoutineName);

                {
                    auto const SELECT_CASE_var(this_loop.LoopDemandCalcScheme);

                    if (SELECT_CASE_var == SingleSetPoint) {

                        // Pick up the loop setpoint temperature
                        LoopSetPointTemperature = this_loop.LoopSide(LoopSideNum).TempSetPoint;

                        // Calculate the delta temperature
                        DeltaTemp = LoopSetPointTemperature - TargetTemp;

                        EnthalpySteamSatVapor = GetSatEnthalpyRefrig(fluidNameSteam, LoopSetPointTemperature, 1.0,
                                                                     RefrigIndex, RoutineNameAlt);
                        EnthalpySteamSatLiquid = GetSatEnthalpyRefrig(fluidNameSteam, LoopSetPointTemperature, 0.0,
                                                                      RefrigIndex, RoutineNameAlt);

                        LatentHeatSteam = EnthalpySteamSatVapor - EnthalpySteamSatLiquid;

                        // Calculate the demand on the loop
                        LoadToLoopSetPoint = MassFlowRate * (Cp * DeltaTemp + LatentHeatSteam);
                    }
                }

            } else { // only have two types, water serves for glycol.
            }

            // Trim the demand to zero if it is very small
            if (std::abs(LoadToLoopSetPoint) < LoopDemandTol) LoadToLoopSetPoint = 0.0;

            PlantReport(LoopNum).UnmetDemand = LoadToLoopSetPoint;
        }

        void PlantLoopSolverClass::CheckLoopExitNode(int const LoopNum,            // plant loop counter
                                                     bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
        ) {

            // SUBROUTINE INFORMATION:
            //       AUTHOR         Dan Fisher
            //       DATE WRITTEN   October 1998
            //       MODIFIED       na
            //       RE-ENGINEERED  na

            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine sets the temperature
            // and mass flow rate of the plant loop supply side exit
            // node.  As written, the routine calculates the exit
            // temperature based on the fraction of loop demand met
            // by the plant equipment.  This assumes that each piece
            // of operating plant equipment produced chilled/hot water
            // at the loop setpoint temperature.

            // Using/Aliasing
            using DataBranchAirLoopPlant::MassFlowTolerance;
            using DataGlobals::WarmupFlag;
            using DataLoopNode::Node;
            using DataLoopNode::NodeID;
            using DataPlant::DemandSide;
            using DataPlant::PlantLoop;
            using DataPlant::SupplySide;
            using General::RoundSigDigits;

            // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
            int LoopInlet;  // plant loop inlet node num.
            int LoopOutlet; // plant loop outlet node num.

            // set local variables: loop inlet and outlet nodes
            LoopInlet = PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn;
            LoopOutlet = PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumOut;
            // Check continuity invalid...loop pumps now turned on and off
            if (!FirstHVACIteration && !WarmupFlag) {
                if (std::abs(Node(LoopOutlet).MassFlowRate - Node(LoopInlet).MassFlowRate) > MassFlowTolerance) {
                    if (PlantLoop(LoopNum).MFErrIndex == 0) {
                        ShowWarningError("PlantSupplySide: PlantLoop=\"" + PlantLoop(LoopNum).Name +
                                         "\", Error (CheckLoopExitNode) -- Mass Flow Rate Calculation. Outlet and Inlet differ by more than tolerance.");
                        ShowContinueErrorTimeStamp("");
                        ShowContinueError("Loop inlet node=" + NodeID(LoopInlet) + ", flowrate=" +
                                          RoundSigDigits(Node(LoopInlet).MassFlowRate, 4) +
                                          " kg/s");
                        ShowContinueError("Loop outlet node=" + NodeID(LoopOutlet) + ", flowrate=" +
                                          RoundSigDigits(Node(LoopOutlet).MassFlowRate, 4) +
                                          " kg/s");
                        ShowContinueError("This loop might be helped by a bypass.");
                    }
                    ShowRecurringWarningErrorAtEnd("PlantSupplySide: PlantLoop=\"" + PlantLoop(LoopNum).Name +
                                                   "\", Error -- Mass Flow Rate Calculation -- continues ** ",
                                                   PlantLoop(LoopNum).MFErrIndex);
                }
            }
            // Reset Max loop flow rate based on pump performance
            Node(LoopOutlet).MassFlowRateMax = Node(LoopInlet).MassFlowRateMax;
        }
    } // namespace PlantLoopSolver

} // namespace EnergyPlus
