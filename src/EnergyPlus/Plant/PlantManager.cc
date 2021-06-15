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
#include <algorithm>
#include <cassert>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/ChillerExhaustAbsorption.hh>
#include <EnergyPlus/ChillerGasAbsorption.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/ChillerReformulatedEIR.hh>
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EvaporativeFluidCoolers.hh>
#include <EnergyPlus/FluidCoolers.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/FuelCellElectricGenerator.hh>
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatPumpWaterToWaterCOOLING.hh>
#include <EnergyPlus/HeatPumpWaterToWaterHEATING.hh>
#include <EnergyPlus/HeatPumpWaterToWaterSimple.hh>
#include <EnergyPlus/ICEngineElectricGenerator.hh>
#include <EnergyPlus/IceThermalStorage.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/MicroturbineElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/PipeHeatTransfer.hh>
#include <EnergyPlus/Pipes.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/PlantComponentTemperatureSources.hh>
#include <EnergyPlus/PlantHeatExchangerFluidToFluid.hh>
#include <EnergyPlus/PlantLoadProfile.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/PlantPipingSystemsManager.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PlantValves.hh>
#include <EnergyPlus/PondGroundHeatExchanger.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/SurfaceGroundHeatExchanger.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterUse.hh>

namespace EnergyPlus::PlantManager {

// MODULE INFORMATION:
//       AUTHOR         Sankaranarayanan K P, Rich Liesen
//       DATE WRITTEN   May 2005
//       MODIFIED
//       RE-ENGINEERED  Sept. 2010 D. Fisher, Edwin Lee, Brent Griffith
//                      major plant upgrades:
//                         Single half loop solver
//                         Automated branch control types
//                         new loop sequencing structure
//                         Temperature out range checks

// PURPOSE OF THIS MODULE:
// This module serves as the driver for the plant simulation. All necessary iterations and update related to plant
// connections are performed in this module.

// Using/Aliasing
using namespace DataHVACGlobals;
using namespace DataPlant;
using namespace DataBranchAirLoopPlant;
using namespace DataLoopNode;
using namespace FluidProperties;

static constexpr std::string_view fluidNameSteam("STEAM");

void ManagePlantLoops(EnergyPlusData &state,
                      bool const FirstHVACIteration,
                      bool &SimAirLoops,                          // True when the air loops need to be (re)simulated
                      bool &SimZoneEquipment,                     // True when zone equipment components need to be (re)simulated
                      [[maybe_unused]] bool &SimNonZoneEquipment, // True when non-zone equipment components need to be (re)simulated
                      bool &SimPlantLoops,                        // True when some part of Plant needs to be (re)simulated
                      bool &SimElecCircuits                       // True when electic circuits need to be (re)simulated
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   Apr 2005
    //       MODIFIED
    //       RE-ENGINEERED  B. Griffith, Feb. 2010

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the plant loop simulation

    // METHODOLOGY EMPLOYED:
    // Set up the while iteration block for the plant loop simulation.
    // Calls half loop sides to be simulated in predetermined order.
    // Reset the flags as necessary

    // Using/Aliasing
    using PlantUtilities::LogPlantConvergencePoints;

    // SUBROUTINE VARIABLE DEFINITIONS
    int IterPlant;
    int LoopNum;
    int LoopSide;
    int LoopSideNum;
    int OtherSide;
    bool SimHalfLoopFlag;
    int HalfLoopNum;
    int CurntMinPlantSubIterations;

    if (std::any_of(state.dataPlnt->PlantLoop.begin(), state.dataPlnt->PlantLoop.end(), [](DataPlant::PlantLoopData const &e) {
            return (e.CommonPipeType == DataPlant::iCommonPipeType::Single) || (e.CommonPipeType == DataPlant::iCommonPipeType::TwoWay);
        })) {
        CurntMinPlantSubIterations = max(7, state.dataConvergeParams->MinPlantSubIterations);
    } else {
        CurntMinPlantSubIterations = state.dataConvergeParams->MinPlantSubIterations;
    }

    if (state.dataPlnt->TotNumLoops <= 0) { // quick return if no plant in model
        SimPlantLoops = false;
        return;
    }

    IterPlant = 0;
    InitializeLoops(state, FirstHVACIteration);

    while ((SimPlantLoops) && (IterPlant <= state.dataConvergeParams->MaxPlantSubIterations)) {
        // go through half loops in predetermined calling order
        for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {

            LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
            LoopSide = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
            OtherSide = 3 - LoopSide; // will give us 1 if LoopSide is 2, or 2 if LoopSide is 1

            auto &this_loop(state.dataPlnt->PlantLoop(LoopNum));
            auto &this_loop_side(this_loop.LoopSide(LoopSide));
            auto &other_loop_side(this_loop.LoopSide(OtherSide));

            SimHalfLoopFlag = this_loop_side.SimLoopSideNeeded; // set half loop sim flag

            if (SimHalfLoopFlag || IterPlant <= CurntMinPlantSubIterations) {

                this_loop_side.solve(state, FirstHVACIteration, other_loop_side.SimLoopSideNeeded);

                // Always set this side to false,  so that it won't keep being turned on just because of first hvac
                this_loop_side.SimLoopSideNeeded = false;

                // If we did the demand side, turn on the supply side (only if we need to do it last)
                if (LoopSide == DemandSide) {
                    if (this_loop.HasPressureComponents) {
                        other_loop_side.SimLoopSideNeeded = false;
                    }
                }

                // Update the report variable
                this_loop.LastLoopSideSimulated = LoopSide;

                ++state.dataPlnt->PlantManageHalfLoopCalls;
            }

        } // half loop based calling order...

        // decide new status for SimPlantLoops flag
        SimPlantLoops = false;
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (LoopSideNum = 1; LoopSideNum <= 2; ++LoopSideNum) {
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).SimLoopSideNeeded) {
                    SimPlantLoops = true;
                    goto LoopLevel_exit;
                }
            }
        }
    LoopLevel_exit:;

        ++IterPlant; // Increment the iteration counter
        if (IterPlant < CurntMinPlantSubIterations) SimPlantLoops = true;
        ++state.dataPlnt->PlantManageSubIterations; // these are summed across all half loops for reporting
    }                                               // while

    // add check for non-plant system sim flag updates
    //  could set SimAirLoops, SimElecCircuits, SimZoneEquipment flags for now
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (LoopSide = DemandSide; LoopSide <= SupplySide; ++LoopSide) {
            auto &this_loop_side(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide));
            if (this_loop_side.SimAirLoopsNeeded) SimAirLoops = true;
            if (this_loop_side.SimZoneEquipNeeded) SimZoneEquipment = true;
            //  IF (this_loop_side.SimNonZoneEquipNeeded) SimNonZoneEquipment = .TRUE.
            if (this_loop_side.SimElectLoadCentrNeeded) SimElecCircuits = true;
        }
    }

    // Also log the convergence history of all loopsides once complete
    LogPlantConvergencePoints(state, FirstHVACIteration);
}

void GetPlantLoopData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   April 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine reads the primary plant loop
    // attributes from the input file

    // METHODOLOGY EMPLOYED:
    // calls the Input Processor to retrieve data from input file.

    // Using/Aliasing
    using ScheduleManager::GetScheduleIndex;
    using SetPointManager::IsNodeOnSetPtManager;
    auto localTempSetPt = SetPointManager::iCtrlVarType::Temp;
    using NodeInputManager::GetOnlySingleNode;
    using namespace BranchInputManager;
    using DataSizing::AutoSize;
    using FluidProperties::CheckFluidPropertyName;
    using FluidProperties::FindGlycol;
    ;
    using SystemAvailabilityManager::GetPlantAvailabilityManager;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetPlant/CondenserLoopData: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum;   // DO loop counter for loops
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    int NumFluids; // number of fluids in sim
    int PlantLoopNum;
    int CondLoopNum;
    Array1D_string Alpha(18); // dimension to num of alpha fields in input
    Array1D<Real64> Num(30);  // dimension to num of numeric data fields in input
    bool ErrorsFound(false);
    std::string LoadingScheme;
    bool ErrFound;
    std::string CurrentModuleObject; // for ease in renaming.
    bool MatchedPressureString;
    int PressSimAlphaIndex;

    CurrentModuleObject = "PlantLoop";
    state.dataHVACGlobal->NumPlantLoops =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state,
                                                                      CurrentModuleObject); // Get the number of primary plant loops
    CurrentModuleObject = "CondenserLoop";
    state.dataHVACGlobal->NumCondLoops =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Get the number of Condenser loops
    state.dataPlnt->TotNumLoops = state.dataHVACGlobal->NumPlantLoops + state.dataHVACGlobal->NumCondLoops;

    if (state.dataPlnt->TotNumLoops > 0) {
        state.dataPlnt->PlantLoop.allocate(state.dataPlnt->TotNumLoops);
        state.dataConvergeParams->PlantConvergence.allocate(state.dataPlnt->TotNumLoops);
        if (!allocated(state.dataPlnt->PlantAvailMgr)) {
            state.dataPlnt->PlantAvailMgr.allocate(state.dataPlnt->TotNumLoops);
        }
    } else {
        return;
    }

    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        Alpha = "";
        Num = 0.0;

        // set up some references
        auto &this_loop(state.dataPlnt->PlantLoop(LoopNum));
        this_loop.LoopSide.allocate(2);
        auto &this_demand_side(this_loop.LoopSide(1));
        auto &this_supply_side(this_loop.LoopSide(2));
        if (LoopNum <= state.dataHVACGlobal->NumPlantLoops) {
            PlantLoopNum = LoopNum;
            this_loop.TypeOfLoop = LoopType::Plant;
            CurrentModuleObject = "PlantLoop";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     PlantLoopNum,
                                                                     Alpha,
                                                                     NumAlphas,
                                                                     Num,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
        } else {
            CondLoopNum = LoopNum - state.dataHVACGlobal->NumPlantLoops;
            this_loop.TypeOfLoop = LoopType::Condenser;
            CurrentModuleObject = "CondenserLoop";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CondLoopNum,
                                                                     Alpha,
                                                                     NumAlphas,
                                                                     Num,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
        }
        UtilityRoutines::IsNameEmpty(state, Alpha(1), CurrentModuleObject, ErrorsFound);
        this_loop.Name = Alpha(1); // Load the Plant Loop Name

        if (UtilityRoutines::SameString(Alpha(2), "STEAM")) {
            this_loop.FluidType = DataLoopNode::NodeFluidType::Steam;
            this_loop.FluidName = Alpha(2);
        } else if (UtilityRoutines::SameString(Alpha(2), "WATER")) {
            this_loop.FluidType = DataLoopNode::NodeFluidType::Water;
            this_loop.FluidName = Alpha(2);
            this_loop.FluidIndex = FindGlycol(state, Alpha(2));
        } else if (UtilityRoutines::SameString(Alpha(2), "USERDEFINEDFLUIDTYPE")) {
            this_loop.FluidType = DataLoopNode::NodeFluidType::Water;
            this_loop.FluidName = Alpha(3);
            // check for valid fluid name
            NumFluids = CheckFluidPropertyName(state, Alpha(3));
            if (NumFluids == 0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + Alpha(1) + "\", missing fluid data for Plant loop.");
                ErrorsFound = true;
            } else {
                this_loop.FluidIndex = FindGlycol(state, Alpha(3));
                if (this_loop.FluidIndex == 0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + Alpha(1) + "\", invalid glycol fluid data for Plant loop.");
                    ErrorsFound = true;
                }
            }
        } else {
            ShowWarningError(state,
                             "Input error: " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + Alpha(2) + " entered, in " + CurrentModuleObject +
                                 '=' + Alpha(1));
            ShowContinueError(state, "Will default to Water.");

            this_loop.FluidType = DataLoopNode::NodeFluidType::Water;
            this_loop.FluidName = "WATER";
            this_loop.FluidIndex = FindGlycol(state, "WATER");
        }

        this_loop.OperationScheme = Alpha(4); // Load the Plant Control Scheme Priority List

        // Load the temperature and flow rate maximum and minimum limits
        this_loop.MaxTemp = Num(1);
        this_loop.MinTemp = Num(2);
        this_loop.MaxVolFlowRate = Num(3);
        if (this_loop.MaxVolFlowRate == AutoSize) {
            this_loop.MaxVolFlowRateWasAutoSized = true;
        }
        this_loop.MinVolFlowRate = Num(4);

        // The Plant loop volume for both halves of the loop is read in and used in this module for the
        // correct loop temperature step.  Loop data is read in supply side, but the volume is not used in
        // a calculation there.
        this_loop.Volume = Num(5);
        if (state.dataIPShortCut->lNumericFieldBlanks(5)) this_loop.Volume = DataGlobalConstants::AutoCalculate;
        if (this_loop.Volume == DataGlobalConstants::AutoCalculate) {
            this_loop.VolumeWasAutoSized = true;
        }
        // circulation time used to autocalculate loop volume
        if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
            this_loop.CirculationTime = 2.0; // default
        } else {
            this_loop.CirculationTime = Num(6);
        }

        // Load the Loop Inlet and Outlet Nodes and Connection Info (Alpha(7-10) are related to the supply side)
        this_supply_side.NodeNameIn = Alpha(6);
        this_supply_side.NodeNameOut = Alpha(7);
        this_supply_side.BranchList = Alpha(8);
        this_supply_side.ConnectList = Alpha(9);
        this_demand_side.NodeNameIn = Alpha(10);
        this_demand_side.NodeNameOut = Alpha(11);
        this_demand_side.BranchList = Alpha(12);
        this_demand_side.ConnectList = Alpha(13);

        this_supply_side.NodeNumIn = GetOnlySingleNode(state,
                                                       Alpha(6),
                                                       ErrorsFound,
                                                       CurrentModuleObject,
                                                       Alpha(1),
                                                       this_loop.FluidType,
                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                       1,
                                                       ObjectIsParent);
        this_supply_side.NodeNumOut = GetOnlySingleNode(state,
                                                        Alpha(7),
                                                        ErrorsFound,
                                                        CurrentModuleObject,
                                                        Alpha(1),
                                                        this_loop.FluidType,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        ObjectIsParent);
        this_demand_side.NodeNumIn = GetOnlySingleNode(state,
                                                       Alpha(10),
                                                       ErrorsFound,
                                                       CurrentModuleObject,
                                                       Alpha(1),
                                                       this_loop.FluidType,
                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                       1,
                                                       ObjectIsParent);
        this_demand_side.NodeNumOut = GetOnlySingleNode(state,
                                                        Alpha(11),
                                                        ErrorsFound,
                                                        CurrentModuleObject,
                                                        Alpha(1),
                                                        this_loop.FluidType,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        ObjectIsParent);

        this_demand_side.InletNodeSetPt = IsNodeOnSetPtManager(state, this_demand_side.NodeNumIn, localTempSetPt);
        this_demand_side.OutletNodeSetPt = IsNodeOnSetPtManager(state, this_demand_side.NodeNumOut, localTempSetPt);
        this_supply_side.InletNodeSetPt = IsNodeOnSetPtManager(state, this_supply_side.NodeNumIn, localTempSetPt);
        this_supply_side.OutletNodeSetPt = IsNodeOnSetPtManager(state, this_supply_side.NodeNumOut, localTempSetPt);
        this_loop.TempSetPointNodeNum = GetOnlySingleNode(state,
                                                          Alpha(5),
                                                          ErrorsFound,
                                                          CurrentModuleObject,
                                                          Alpha(1),
                                                          this_loop.FluidType,
                                                          DataLoopNode::NodeConnectionType::Sensor,
                                                          1,
                                                          ObjectIsParent);

        // Load the load distribution scheme.
        LoadingScheme = Alpha(14);
        if (UtilityRoutines::SameString(LoadingScheme, "Optimal")) {
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::Optimal;
        } else if (UtilityRoutines::SameString(LoadingScheme, "SequentialLoad")) {
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::Sequential;
        } else if (UtilityRoutines::SameString(LoadingScheme, "UniformLoad")) {
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::Uniform;
        } else if (UtilityRoutines::SameString(LoadingScheme, "UniformPLR")) {
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::UniformPLR;
        } else if (UtilityRoutines::SameString(LoadingScheme, "SequentialUniformPLR")) {
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::SequentialUniformPLR;
        } else {
            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
            ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(14) + "=\"" + Alpha(14) + "\".");
            ShowContinueError(state, "Will default to SequentialLoad."); // TODO rename point
            this_loop.LoadDistribution = DataPlant::iLoadingScheme::Sequential;
        }

        // When dual setpoint is allowed in condenser loop modify this code.
        if (this_loop.TypeOfLoop == LoopType::Plant) {
            // Get the Loop Demand Calculation Scheme
            if (UtilityRoutines::SameString(Alpha(16), "SingleSetpoint")) {
                this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
            } else if (UtilityRoutines::SameString(Alpha(16), "DualSetpointDeadband")) {
                if (this_loop.FluidType == DataLoopNode::NodeFluidType::Steam) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                    ShowContinueError(state,
                                      state.dataIPShortCut->cAlphaFieldNames(16) + "=\"" + Alpha(16) + "\" not valid for " +
                                          state.dataIPShortCut->cAlphaFieldNames(2) + "= Steam");
                    ShowContinueError(state,
                                      "Will reset " + state.dataIPShortCut->cAlphaFieldNames(16) + " = SingleSetPoint and simulation will continue.");
                    this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
                } else {
                    this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand;
                }
            } else if (UtilityRoutines::SameString(Alpha(16), "")) {
                this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
            } else {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(16) + "=\"" + Alpha(16) + "\".");
                ShowContinueError(state, "Will default to SingleSetPoint."); // TODO rename point
                this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
            }
        } else if (this_loop.TypeOfLoop == LoopType::Condenser) {
            this_loop.LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
        }

        // When Commonpipe is allowed in condenser loop modify this code. Sankar 06/29/2009
        if (this_loop.TypeOfLoop == LoopType::Plant) {
            if (UtilityRoutines::SameString(Alpha(17), "CommonPipe")) {
                this_loop.CommonPipeType = DataPlant::iCommonPipeType::Single;
            } else if (UtilityRoutines::SameString(Alpha(17), "TwoWayCommonPipe")) {
                this_loop.CommonPipeType = DataPlant::iCommonPipeType::TwoWay;
            } else if (UtilityRoutines::SameString(Alpha(17), "None") || state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                this_loop.CommonPipeType = DataPlant::iCommonPipeType::No;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(17) + "=\"" + Alpha(17) + "\".");
                ShowContinueError(state, "Refer to I/O reference document for more details.");
                ErrorsFound = true;
            }
        } else if (this_loop.TypeOfLoop == LoopType::Condenser) {
            this_loop.CommonPipeType = DataPlant::iCommonPipeType::No;
        }

        if (this_loop.CommonPipeType == DataPlant::iCommonPipeType::TwoWay) {
            if (this_demand_side.InletNodeSetPt && this_supply_side.InletNodeSetPt) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
                ShowContinueError(state,
                                  "While using a two way common pipe there can be setpoint on only one node other than Plant Supply Outlet node.");
                ShowContinueError(state, "Currently both Plant Demand inlet and plant supply inlet have setpoints.");
                ShowContinueError(state, "Select one of the two nodes and rerun the simulation.");
                ErrorsFound = true;
            }
            if (!this_demand_side.InletNodeSetPt && !this_supply_side.InletNodeSetPt) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
                ShowContinueError(state, "While using a two way common pipe there must be a setpoint in addition to the Plant Supply Outlet node.");
                ShowContinueError(state, "Currently neither plant demand inlet nor plant supply inlet have setpoints.");
                ShowContinueError(state, "Select one of the two nodes and rerun the simulation.");
                ErrorsFound = true;
            }
        }

        // Pressure Simulation Type Input
        // First set the alpha index in the object as it is different for plant/condenser
        // When CommonPipe, etc., is allowed in condenser loop, modify this code.  Edwin/Sankar 08/12/2009
        if (this_loop.TypeOfLoop == LoopType::Plant) {
            PressSimAlphaIndex = 18;
        } else {
            PressSimAlphaIndex = 15;
        }

        if (NumAlphas >= PressSimAlphaIndex) {
            MatchedPressureString = false;

            // Check all types
            if (UtilityRoutines::SameString(Alpha(PressSimAlphaIndex), format("{}", cPressureSimType(DataPlant::iPressSimType::NoPressure)))) {
                this_loop.PressureSimType = DataPlant::iPressSimType::NoPressure;
                MatchedPressureString = true;
            } else if (UtilityRoutines::SameString(Alpha(PressSimAlphaIndex),
                                                   format("{}", cPressureSimType(DataPlant::iPressSimType::PumpPowerCorrection)))) {
                this_loop.PressureSimType = DataPlant::iPressSimType::PumpPowerCorrection;
                MatchedPressureString = true;
            } else if (UtilityRoutines::SameString(Alpha(PressSimAlphaIndex),
                                                   format("{}", cPressureSimType(DataPlant::iPressSimType::FlowSimulation)))) {
                this_loop.PressureSimType = DataPlant::iPressSimType::FlowSimulation;
                MatchedPressureString = true;
            } else if (UtilityRoutines::SameString(Alpha(PressSimAlphaIndex),
                                                   format("{}", cPressureSimType(DataPlant::iPressSimType::FlowCorrection)))) {
                this_loop.PressureSimType = DataPlant::iPressSimType::FlowCorrection;
                MatchedPressureString = true;
            }

            // If we found a match, check to make sure it is one of the valid
            // ones for this phase of pressure implementation
            if (MatchedPressureString) {
                if ((this_loop.PressureSimType == DataPlant::iPressSimType::NoPressure) ||
                    (this_loop.PressureSimType == DataPlant::iPressSimType::PumpPowerCorrection) ||
                    (this_loop.PressureSimType == DataPlant::iPressSimType::FlowCorrection)) {
                    // We are OK here, move on
                } else {
                    // We have an erroneous input, alert user
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                    ShowContinueError(
                        state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(PressSimAlphaIndex) + "=\"" + Alpha(PressSimAlphaIndex) + "\".");
                    ShowContinueError(state, "Currently only options are: ");
                    ShowContinueError(state, "  - " + format("{}", cPressureSimType(DataPlant::iPressSimType::NoPressure)));
                    ShowContinueError(state, "  - " + format("{}", cPressureSimType(DataPlant::iPressSimType::PumpPowerCorrection)));
                    ShowContinueError(state, "  - " + format("{}", cPressureSimType(DataPlant::iPressSimType::FlowCorrection)));
                    ErrorsFound = true;
                }
            }

            // if we made it this far and didn't get a match, check for blank
            if (!MatchedPressureString) {
                if (Alpha(PressSimAlphaIndex).empty()) {
                    this_loop.PressureSimType = DataPlant::iPressSimType::NoPressure;
                    break;
                }
            }

            // if we made it this far, there was no match, and it wasn't blank
            if (!MatchedPressureString) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
                ShowContinueError(
                    state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(PressSimAlphaIndex) + "=\"" + Alpha(PressSimAlphaIndex) + "\".");
                ErrorsFound = true;
            }
        }

        ErrFound = false;

        if (this_loop.TypeOfLoop == LoopType::Plant) {
            GetPlantAvailabilityManager(state, Alpha(15), LoopNum, state.dataPlnt->TotNumLoops, ErrFound);
        }

        if (ErrFound) {
            ShowContinueError(state, "Input errors in  " + CurrentModuleObject + '=' + Alpha(1));
            ErrorsFound = true;
        }

        if (GetFirstBranchInletNodeName(state, this_demand_side.BranchList) != this_demand_side.NodeNameIn) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
            ShowContinueError(state,
                              "The inlet node of the first branch in the " + state.dataIPShortCut->cAlphaFieldNames(12) + '=' +
                                  Alpha(12)); //"Plant Demand Side Branch List"
            ShowContinueError(state,
                              "is not the same as the " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' +
                                  Alpha(10)); // "Plant Demand Side Inlet Node Name"
            ShowContinueError(state,
                              "Branch List Inlet Node Name=" + GetFirstBranchInletNodeName(state, this_demand_side.BranchList)); // TODO rename point
            ShowContinueError(
                state,
                "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch."); // TODO
            // rename
            // point
            ErrorsFound = true;
        }

        if (GetLastBranchOutletNodeName(state, this_demand_side.BranchList) != this_demand_side.NodeNameOut) {
            //"Plant Demand Side Branch List"
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
            ShowContinueError(state, "The outlet node of the last branch in the " + state.dataIPShortCut->cAlphaFieldNames(12) + '=' + Alpha(12));
            //"Plant Demand Side Outlet Node Name"
            ShowContinueError(state, "is not the same as the " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + Alpha(11));
            ShowContinueError(state,
                              "Branch List Outlet Node Name=" + GetLastBranchOutletNodeName(state, this_demand_side.BranchList)); // TODO rename point
            // TODO rename point
            ShowContinueError(state,
                              "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch.");
            ErrorsFound = true;
        }

        if (GetFirstBranchInletNodeName(state, this_supply_side.BranchList) != this_supply_side.NodeNameIn) {
            //"Plant Supply Side Branch List"
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
            ShowContinueError(state, "The inlet node of the first branch in the " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + Alpha(8));
            //"Plant Supply Side Inlet Node Name
            ShowContinueError(state, "is not the same as the " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + Alpha(6));
            ShowContinueError(state,
                              "Branch List Inlet Node Name=" + GetFirstBranchInletNodeName(state, this_supply_side.BranchList)); // TODO rename point
            // TODO rename point
            ShowContinueError(state,
                              "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch.");
            ErrorsFound = true;
        }

        if (GetLastBranchOutletNodeName(state, this_supply_side.BranchList) != this_supply_side.NodeNameOut) {
            //"Plant Supply Side Branch List"
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
            ShowContinueError(state, "The outlet node of the last branch in the " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + Alpha(8));
            //"Plant Supply Side Outlet Node Name"
            ShowContinueError(state, "is not the same as the " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + Alpha(7));
            ShowContinueError(state,
                              "Branch List Outlet Node Name=" + GetLastBranchOutletNodeName(state, this_supply_side.BranchList)); // TODO rename point
            // TODO rename point
            ShowContinueError(state,
                              "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch.");
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in processing input. Preceding conditions cause termination.");
    }

    // set up loop status (set by system availability managers) report variables
    // Condenser loop does not have availability manager yet. Once implemented, move the setup output variable to
    // outside the IF statement.
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {

        SetupOutputVariable(state,
                            "Plant System Cycle On Off Status",
                            OutputProcessor::Unit::None,
                            state.dataPlnt->PlantAvailMgr(LoopNum).AvailStatus,
                            "Plant",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
    }
}

void GetPlantInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   April 2005
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets input either through the Plant Loop derived type
    // or by calls out to the branch manager to obtain data.  By the end of
    // the routine the module level derived type Loop should be fully allocated
    // and fully populated.

    // Using/Aliasing
    using namespace NodeInputManager;
    using namespace BranchInputManager;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum; // DO loop counter for loops
    int HalfLoopNum;
    int NumOfPipesInLoop;
    int LoopSideNum;
    int BranchNum; // DO loop counter for branches
    int CompNum;   // DO loop counter for components
    int NodeNum;   // DO loop counter for nodes
    int Outlet;
    int Inlet;
    int NumParams;
    int NumAlphas;
    int NumNumbers;
    int SplitNum;
    int MixNum;
    int NumConnectorsInLoop;
    int ConnNum;
    int TotCompsOnBranch;
    int MaxNumAlphas;

    bool SplitInBranch;
    bool MixerOutBranch;
    bool ErrorsFound(false);
    bool ASeriesBranchHasPump;
    bool AParallelBranchHasPump;

    std::string LoopIdentifier;

    Array1D_string BranchNames;     // Branch names from GetBranchList call
    Array1D_string CompTypes;       // Branch names from GetBranchList call
    Array1D_string CompNames;       // Branch names from GetBranchList call
    Array1D_int CompCtrls;          // Branch names from GetBranchList call
    Array1D_string InletNodeNames;  // Node names from GetBranchData call
    Array1D_string OutletNodeNames; // Node names from GetBranchData call
    Array1D_int InletNodeNumbers;   // Node numbers from GetBranchData call
    Array1D_int OutletNodeNumbers;  // Node numbers from GetBranchData call
    Array1D_bool SplitOutBranch;
    Array1D_bool MixerInBranch;
    bool errFlag;
    int LoopNumInArray;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Connector:Splitter", NumParams, NumAlphas, NumNumbers);
    MaxNumAlphas = NumAlphas;
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Connector:Mixer", NumParams, NumAlphas, NumNumbers);
    MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
    HalfLoopNum = 0;

    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        auto &plantLoop = state.dataPlnt->PlantLoop(LoopNum);
        plantLoop.LoopHasConnectionComp = false;

        for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
            auto &loopSide = plantLoop.LoopSide(LoopSideNum);
            ASeriesBranchHasPump = false;
            AParallelBranchHasPump = false;
            NumOfPipesInLoop = 0; // Initialization
            ++HalfLoopNum;
            loopSide.BypassExists = false;
            if (plantLoop.TypeOfLoop == LoopType::Plant && LoopSideNum == DemandSide) {
                LoopIdentifier = "Plant Demand";
            } else if (plantLoop.TypeOfLoop == LoopType::Plant && LoopSideNum == SupplySide) {
                LoopIdentifier = "Plant Supply";
            } else if (plantLoop.TypeOfLoop == LoopType::Condenser && LoopSideNum == DemandSide) {
                LoopIdentifier = "Condenser Demand";
            } else if (plantLoop.TypeOfLoop == LoopType::Condenser && LoopSideNum == SupplySide) {
                LoopIdentifier = "Condenser Supply";
            }

            // Get the branch list and size the Branch portion of the Loop derived type
            loopSide.TotalBranches = NumBranchesInBranchList(state, loopSide.BranchList);
            BranchNames.allocate(loopSide.TotalBranches);
            BranchNames = "";
            GetBranchList(state, plantLoop.Name, loopSide.BranchList, loopSide.TotalBranches, BranchNames, LoopIdentifier);
            loopSide.Branch.allocate(loopSide.TotalBranches);

            // Cycle through all of the branches and set up the node data
            for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                auto &branch = loopSide.Branch(BranchNum);
                branch.Name = BranchNames(BranchNum);
                branch.TotalComponents = NumCompsInBranch(state, BranchNames(BranchNum));
                branch.IsBypass = false;

                CompTypes.allocate(branch.TotalComponents);
                CompNames.allocate(branch.TotalComponents);
                CompCtrls.dimension(branch.TotalComponents, 0);
                InletNodeNames.allocate(branch.TotalComponents);
                InletNodeNumbers.dimension(branch.TotalComponents, 0);
                OutletNodeNames.allocate(branch.TotalComponents);
                OutletNodeNumbers.dimension(branch.TotalComponents, 0);

                GetBranchData(state,
                              plantLoop.Name,
                              BranchNames(BranchNum),
                              branch.PressureCurveType,
                              branch.PressureCurveIndex,
                              branch.TotalComponents,
                              CompTypes,
                              CompNames,
                              InletNodeNames,
                              InletNodeNumbers,
                              OutletNodeNames,
                              OutletNodeNumbers,
                              ErrorsFound);

                branch.Comp.allocate(branch.TotalComponents);

                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    // set up some references
                    auto &this_comp_type(CompTypes(CompNum));
                    auto &this_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum));

                    this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                    this_comp.TypeOf = this_comp_type;
                    this_comp.location = EnergyPlus::PlantLocation(LoopNum, LoopSideNum, BranchNum, CompNum);

                    if (UtilityRoutines::SameString(this_comp_type, "Pipe:Adiabatic")) {
                        this_comp.TypeOf_Num = TypeOf_Pipe;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = Pipes::LocalPipeData::factory(state, TypeOf_Pipe, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Pipe:Adiabatic:Steam")) {
                        this_comp.TypeOf_Num = TypeOf_PipeSteam;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = Pipes::LocalPipeData::factory(state, TypeOf_PipeSteam, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Pipe:Outdoor")) {
                        this_comp.TypeOf_Num = TypeOf_PipeExterior;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, TypeOf_PipeExterior, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Pipe:Indoor")) {
                        this_comp.TypeOf_Num = TypeOf_PipeInterior;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, TypeOf_PipeInterior, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Pipe:Underground")) {
                        this_comp.TypeOf_Num = TypeOf_PipeUnderground;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, TypeOf_PipeUnderground, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "PipingSystem:Underground:PipeCircuit")) {
                        this_comp.TypeOf_Num = TypeOf_PipingSystemPipeCircuit;
                        this_comp.CurOpSchemeType = NoControlOpSchemeType;
                        this_comp.compPtr = PlantPipingSystemsManager::Circuit::factory(state, TypeOf_PipingSystemPipeCircuit, CompNames(CompNum));
                    } else if (has_prefixi(this_comp_type, "Pump") || has_prefixi(this_comp_type, "HeaderedPumps")) {
                        if (has_prefixi(this_comp_type, "Pump:VariableSpeed:Condensate")) {
                            this_comp.TypeOf_Num = TypeOf_PumpCondensate;
                            this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        } else if (has_prefixi(this_comp_type, "Pump:ConstantSpeed")) {
                            this_comp.TypeOf_Num = TypeOf_PumpConstantSpeed;
                            this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        } else if (has_prefixi(this_comp_type, "Pump:VariableSpeed")) {
                            this_comp.TypeOf_Num = TypeOf_PumpVariableSpeed;
                            this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        } else if (has_prefixi(this_comp_type, "HeaderedPumps:ConstantSpeed")) {
                            this_comp.TypeOf_Num = TypeOf_PumpBankConstantSpeed;
                            this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        } else if (has_prefixi(this_comp_type, "HeaderedPumps:VariableSpeed")) {
                            this_comp.TypeOf_Num = TypeOf_PumpBankVariableSpeed;
                            this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        } else {
                            // discover unsupported equipment on branches.
                            ShowSevereError(state, "GetPlantInput: trying to process a pump type that is not supported, dev note");
                            ShowContinueError(state, "Component Type =" + this_comp_type);
                        }
                        this_comp.CurOpSchemeType = PumpOpSchemeType;
                        if (BranchNum == 1 || BranchNum == state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches) {
                            ASeriesBranchHasPump = true;
                        } else {
                            AParallelBranchHasPump = true;
                        }
                        LoopSidePumpInformation p;
                        p.PumpName = CompNames(CompNum);
                        p.BranchNum = BranchNum;
                        p.CompNum = CompNum;
                        p.PumpOutletNode = OutletNodeNumbers(CompNum);
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).BranchPumpsExist = AParallelBranchHasPump;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Pumps.push_back(p);
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalPumps++;
                    } else if (UtilityRoutines::SameString(this_comp_type, "WaterHeater:Mixed")) {
                        this_comp.TypeOf_Num = TypeOf_WtrHeaterMixed;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "WaterHeater:Stratified")) {
                        this_comp.TypeOf_Num = TypeOf_WtrHeaterStratified;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ChillerHeater:Absorption:Directfired")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_DFAbsorption;
                        this_comp.compPtr = ChillerGasAbsorption::GasAbsorberSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ChillerHeater:Absorption:DoubleEffect")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_ExhFiredAbsorption;
                        this_comp.compPtr = ChillerExhaustAbsorption::ExhaustAbsorberSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ThermalStorage:ChilledWater:Mixed")) {
                        this_comp.TypeOf_Num = TypeOf_ChilledWaterTankMixed;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ThermalStorage:ChilledWater:Stratified")) {
                        this_comp.TypeOf_Num = TypeOf_ChilledWaterTankStratified;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "WaterUse:Connections")) {
                        this_comp.TypeOf_Num = TypeOf_WaterUseConnection;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = WaterUse::WaterConnectionsType::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:Water")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWaterCooling;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:Water:DetailedGeometry")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWaterDetailedFlatCooling;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Heating:Water")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWaterSimpleHeating;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Heating:Steam")) {
                        this_comp.TypeOf_Num = TypeOf_CoilSteamAirHeating;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "SolarCollector:FlatPlate:Water")) {
                        this_comp.TypeOf_Num = TypeOf_SolarCollectorFlatPlate;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        }
                        this_comp.compPtr = SolarCollectors::CollectorData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "SolarCollector:IntegralCollectorStorage")) {
                        this_comp.TypeOf_Num = TypeOf_SolarCollectorICS;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        }
                        this_comp.compPtr = SolarCollectors::CollectorData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "LoadProfile:Plant")) {
                        this_comp.TypeOf_Num = TypeOf_PlantLoadProfile;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = PlantLoadProfile::PlantProfileData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "GroundHeatExchanger:System")) {
                        this_comp.TypeOf_Num = TypeOf_GrndHtExchgSystem;
                        this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory(state, TypeOf_GrndHtExchgSystem, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "GroundHeatExchanger:Surface")) {
                        this_comp.TypeOf_Num = TypeOf_GrndHtExchgSurface;
                        this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        this_comp.compPtr =
                            SurfaceGroundHeatExchanger::SurfaceGroundHeatExchangerData::factory(state, TypeOf_GrndHtExchgSurface, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "GroundHeatExchanger:Pond")) {
                        this_comp.TypeOf_Num = TypeOf_GrndHtExchgPond;
                        this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        this_comp.compPtr = PondGroundHeatExchanger::PondGroundHeatExchangerData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "GroundHeatExchanger:Slinky")) {
                        this_comp.TypeOf_Num = TypeOf_GrndHtExchgSlinky;
                        this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory(state, TypeOf_GrndHtExchgSlinky, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:Electric:EIR")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_ElectricEIR;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = ChillerElectricEIR::ElectricEIRChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:Electric:ReformulatedEIR")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_ElectricReformEIR;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = ChillerReformulatedEIR::ReformulatedEIRChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:Electric")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_Electric;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = PlantChillers::ElectricChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:EngineDriven")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_EngineDriven;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = PlantChillers::EngineDrivenChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:CombustionTurbine")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_CombTurbine;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = PlantChillers::GTChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:ConstantCOP")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_ConstCOP;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = PlantChillers::ConstCOPChillerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Boiler:HotWater")) {
                        this_comp.TypeOf_Num = TypeOf_Boiler_Simple;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = Boilers::BoilerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Boiler:Steam")) {
                        this_comp.TypeOf_Num = TypeOf_Boiler_Steam;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = BoilerSteam::BoilerSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:Absorption:Indirect")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_Indirect_Absorption;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = ChillerIndirectAbsorption::IndirectAbsorberSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Chiller:Absorption")) {
                        this_comp.TypeOf_Num = TypeOf_Chiller_Absorption;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = ChillerAbsorption::BLASTAbsorberSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "CoolingTower:SingleSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_CoolingTower_SingleSpd;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "CoolingTower:TwoSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_CoolingTower_TwoSpd;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "CoolingTower:VariableSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_CoolingTower_VarSpd;
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "CoolingTower:VariableSpeed:Merkel")) {
                        this_comp.TypeOf_Num = TypeOf_CoolingTower_VarSpdMerkel;
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:FuelCell:ExhaustGasToWaterHeatExchanger")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_FCExhaust;
                        this_comp.compPtr = FuelCellElectricGenerator::FCDataStruct::factory_exhaust(state, CompNames(CompNum));
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "WaterHeater:HeatPump:PumpedCondenser")) {
                        this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterPumped;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = WaterThermalTanks::HeatPumpWaterHeaterData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "WaterHeater:HeatPump:WrappedCondenser")) {
                        this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterWrapped;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = WaterThermalTanks::HeatPumpWaterHeaterData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:WatertoWater:EquationFit:Cooling")) {
                        this_comp.compPtr = HeatPumpWaterToWaterSimple::GshpSpecs::factory(state, TypeOf_HPWaterEFCooling, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HPWaterEFCooling;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:WatertoWater:EquationFit:Heating")) {
                        this_comp.compPtr = HeatPumpWaterToWaterSimple::GshpSpecs::factory(state, TypeOf_HPWaterEFHeating, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HPWaterEFHeating;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:WaterToWater:ParameterEstimation:Heating")) {
                        this_comp.compPtr = HeatPumpWaterToWaterHEATING::GshpPeHeatingSpecs::factory(state, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HPWaterPEHeating;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:WaterToWater:ParameterEstimation:Cooling")) {
                        this_comp.compPtr = HeatPumpWaterToWaterCOOLING::GshpPeCoolingSpecs::factory(state, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HPWaterPECooling;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:PlantLoop:EIR:Heating")) {
                        this_comp.compPtr =
                            EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::factory(state, TypeOf_HeatPumpEIRHeating, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HeatPumpEIRHeating;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatPump:PlantLoop:EIR:Cooling")) {
                        this_comp.compPtr =
                            EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::factory(state, TypeOf_HeatPumpEIRCooling, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_HeatPumpEIRCooling;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirConditioner:VariableRefrigerantFlow")) {
                        this_comp.TypeOf_Num = TypeOf_HeatPumpVRF;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = HVACVariableRefrigerantFlow::VRFCondenserEquipment::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "DistrictCooling")) {
                        this_comp.TypeOf_Num = TypeOf_PurchChilledWater;
                        this_comp.compPtr =
                            OutsideEnergySources::OutsideEnergySourceSpecs::factory(state, TypeOf_PurchChilledWater, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "DistrictHeating")) {
                        this_comp.TypeOf_Num = TypeOf_PurchHotWater;
                        this_comp.compPtr = OutsideEnergySources::OutsideEnergySourceSpecs::factory(state, TypeOf_PurchHotWater, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ThermalStorage:Ice:Simple")) {
                        this_comp.TypeOf_Num = TypeOf_TS_IceSimple;
                        this_comp.compPtr = IceThermalStorage::SimpleIceStorageData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "ThermalStorage:Ice:Detailed")) {
                        this_comp.TypeOf_Num = TypeOf_TS_IceDetailed;
                        this_comp.compPtr = IceThermalStorage::DetailedIceStorageData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "TemperingValve")) {
                        this_comp.compPtr = PlantValves::TemperValveData::factory(state, CompNames(CompNum));
                        this_comp.TypeOf_Num = TypeOf_ValveTempering;
                    } else if (UtilityRoutines::SameString(this_comp_type, "HeatExchanger:FluidToFluid")) {
                        this_comp.TypeOf_Num = TypeOf_FluidToFluidPlantHtExchg;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = FreeRejectionOpSchemeType;
                        }
                        this_comp.compPtr = PlantHeatExchangerFluidToFluid::HeatExchangerStruct::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:MicroTurbine")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_MicroTurbine;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = MicroturbineElectricGenerator::MTGeneratorSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:InternalCombustionEngine")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_ICEngine;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = ICEngineElectricGenerator::ICEngineGeneratorSpecs::factory(state, CompNames(CompNum));

                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:CombustionTurbine")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_CTurbine;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = CTElectricGenerator::CTGeneratorData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:MicroCHP")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_MicroCHP;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = MicroCHPElectricGenerator::MicroCHPDataStruct::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Generator:FuelCell:StackCooler")) {
                        this_comp.TypeOf_Num = TypeOf_Generator_FCStackCooler;
                        this_comp.compPtr = FuelCellElectricGenerator::FCDataStruct::factory(state, CompNames(CompNum));
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                    } else if (UtilityRoutines::SameString(this_comp_type, "FluidCooler:SingleSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_FluidCooler_SingleSpd;
                        this_comp.compPtr = FluidCoolers::FluidCoolerspecs::factory(state, TypeOf_FluidCooler_SingleSpd, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "FluidCooler:TwoSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_FluidCooler_TwoSpd;
                        this_comp.compPtr = FluidCoolers::FluidCoolerspecs::factory(state, TypeOf_FluidCooler_TwoSpd, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "EvaporativeFluidCooler:SingleSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_EvapFluidCooler_SingleSpd;
                        this_comp.compPtr =
                            EvaporativeFluidCoolers::EvapFluidCoolerSpecs::factory(state, TypeOf_EvapFluidCooler_SingleSpd, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "EvaporativeFluidCooler:TwoSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_EvapFluidCooler_TwoSpd;
                        this_comp.compPtr =
                            EvaporativeFluidCoolers::EvapFluidCoolerSpecs::factory(state, TypeOf_EvapFluidCooler_TwoSpd, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "SolarCollector:FlatPlate:PhotovoltaicThermal")) {
                        this_comp.TypeOf_Num = TypeOf_PVTSolarCollectorFlatPlate;
                        if (LoopSideNum == DemandSide) {
                            this_comp.CurOpSchemeType = DemandOpSchemeType;
                        } else if (LoopSideNum == SupplySide) {
                            this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        }
                        this_comp.compPtr = PhotovoltaicThermalCollectors::PVTCollectorStruct::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "CentralHeatPumpSystem")) {
                        this_comp.TypeOf_Num = TypeOf_CentralGroundSourceHeatPump;
                        this_comp.compPtr = PlantCentralGSHP::WrapperSpecs::factory(state, CompNames(CompNum));
                        // now deal with demand components of the ZoneHVAC type served by ControlCompOutput
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:Baseboard:RadiantConvective:Water")) {
                        this_comp.TypeOf_Num = TypeOf_Baseboard_Rad_Conv_Water;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:Baseboard:Convective:Water")) {
                        this_comp.TypeOf_Num = TypeOf_Baseboard_Conv_Water;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:Baseboard:RadiantConvective:Steam")) {
                        this_comp.TypeOf_Num = TypeOf_Baseboard_Rad_Conv_Steam;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:CoolingPanel:RadiantConvective:Water")) {
                        this_comp.TypeOf_Num = TypeOf_CoolingPanel_Simple;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:LowTemperatureRadiant:VariableFlow")) {
                        this_comp.TypeOf_Num = TypeOf_LowTempRadiant_VarFlow;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:LowTemperatureRadiant:ConstantFlow")) {
                        this_comp.TypeOf_Num = TypeOf_LowTempRadiant_ConstFlow;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirTerminal:SingleDuct:ConstantVolume:CooledBeam")) {
                        this_comp.TypeOf_Num = TypeOf_CooledBeamAirTerminal;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam")) {
                        this_comp.TypeOf_Num = TypeOf_FourPipeBeamAirTerminal;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed")) {
                        this_comp.TypeOf_Num = TypeOf_MultiSpeedHeatPumpRecovery;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirLoopHVAC:UnitarySystem")) {
                        this_comp.TypeOf_Num = TypeOf_UnitarySysRecovery;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Heating:WaterToAirHeatPump:EquationFit")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWAHPHeatingEquationFit;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWAHPCoolingEquationFit;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                        this_comp.TypeOf_Num = TypeOf_CoilVSWAHPHeatingEquationFit;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                        this_comp.TypeOf_Num = TypeOf_CoilVSWAHPCoolingEquationFit;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWAHPHeatingParamEst;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation")) {
                        this_comp.TypeOf_Num = TypeOf_CoilWAHPCoolingParamEst;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "Refrigeration:Condenser:WaterCooled")) {
                        this_comp.TypeOf_Num = TypeOf_RefrigSystemWaterCondenser;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = RefrigeratedCase::RefrigCondenserData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Refrigeration:CompressorRack")) {
                        this_comp.TypeOf_Num = TypeOf_RefrigerationWaterCoolRack;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = RefrigeratedCase::RefrigRackData::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "PlantComponent:UserDefined")) {
                        this_comp.TypeOf_Num = TypeOf_PlantComponentUserDefined;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = UserDefinedComponents::UserPlantComponentStruct::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:UserDefined")) {
                        this_comp.TypeOf_Num = TypeOf_CoilUserDefined;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "ZoneHVAC:ForcedAir:UserDefined")) {
                        this_comp.TypeOf_Num = TypeOf_ZoneHVACAirUserDefined;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "AirTerminal:SingleDuct:UserDefined")) {
                        this_comp.TypeOf_Num = TypeOf_AirTerminalUserDefined;
                        this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "PlantComponent:TemperatureSource")) {
                        this_comp.TypeOf_Num = TypeOf_WaterSource;
                        this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
                        this_comp.compPtr = PlantComponentTemperatureSources::WaterSourceSpecs::factory(state, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "GroundHeatExchanger:HorizontalTrench")) {
                        this_comp.TypeOf_Num = TypeOf_GrndHtExchgHorizTrench;
                        this_comp.CurOpSchemeType = TypeOf_GrndHtExchgHorizTrench;
                        this_comp.compPtr = PlantPipingSystemsManager::Circuit::factory(state, TypeOf_GrndHtExchgHorizTrench, CompNames(CompNum));
                    } else if (UtilityRoutines::SameString(this_comp_type, "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {
                        this_comp.TypeOf_Num = TypeOf_PackagedTESCoolingCoil;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else if (UtilityRoutines::SameString(this_comp_type, "SwimmingPool:Indoor")) {
                        this_comp.TypeOf_Num = TypeOf_SwimmingPool_Indoor;
                        this_comp.CurOpSchemeType = DemandOpSchemeType;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                    } else {
                        // discover unsupported equipment on branches.
                        ShowSevereError(state, "GetPlantInput: Branch=\"" + BranchNames(BranchNum) + "\", invalid component on branch.");
                        ShowContinueError(state, "...invalid component type=\"" + this_comp_type + "\", name=\"" + CompNames(CompNum) + "\".");
                        //            ErrorsFound=.TRUE.
                    }

                    if (!this_comp.compPtr) ShowFatalError(state, format(" Plant component \"{}\" was not assigned a pointer.", this_comp_type));

                    this_comp.Name = CompNames(CompNum);
                    this_comp.NodeNameIn = InletNodeNames(CompNum);
                    this_comp.NodeNumIn = InletNodeNumbers(CompNum);
                    this_comp.NodeNameOut = OutletNodeNames(CompNum);
                    this_comp.NodeNumOut = OutletNodeNumbers(CompNum);
                }

                // set branch inlet/outlet nodes
                branch.NodeNumIn = branch.Comp(1).NodeNumIn;
                branch.NodeNumOut = branch.Comp(branch.TotalComponents).NodeNumOut;

                CompTypes.deallocate();
                CompNames.deallocate();
                CompCtrls.deallocate();
                InletNodeNames.deallocate();
                InletNodeNumbers.deallocate();
                OutletNodeNames.deallocate();
                OutletNodeNumbers.deallocate();
            }

            BranchNames.deallocate();

            if (ASeriesBranchHasPump && AParallelBranchHasPump) {
                ShowSevereError(state, "Current version does not support Loop pumps and branch pumps together");
                ShowContinueError(state, "Occurs in loop " + state.dataPlnt->PlantLoop(LoopNum).Name);
                ErrorsFound = true;
            }

            // Obtain the Splitter and Mixer information
            if (loopSide.ConnectList.empty()) {
                state.dataLoopNodes->NumofSplitters = 0;
                state.dataLoopNodes->NumofMixers = 0;
            } else {
                errFlag = false;
                GetNumSplitterMixerInConntrList(
                    state, plantLoop.Name, loopSide.ConnectList, state.dataLoopNodes->NumofSplitters, state.dataLoopNodes->NumofMixers, errFlag);
                if (errFlag) {
                    ErrorsFound = true;
                }
                if (state.dataLoopNodes->NumofSplitters != state.dataLoopNodes->NumofMixers) {
                    ShowSevereError(state,
                                    "GetPlantInput: Loop Name=" + plantLoop.Name + ", ConnectorList=" + loopSide.ConnectList +
                                        ", unequal number of splitters and mixers");
                    ErrorsFound = true;
                }
            }

            loopSide.Splitter.Exists = state.dataLoopNodes->NumofSplitters > 0;
            loopSide.Mixer.Exists = state.dataLoopNodes->NumofMixers > 0;

            if (ErrorsFound) {
                ShowFatalError(state, "GetPlantInput: Previous Severe errors cause termination.");
            }

            NumConnectorsInLoop = state.dataLoopNodes->NumofSplitters + state.dataLoopNodes->NumofMixers;
            SplitNum = 1;
            for (ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum) {

                if (SplitNum > state.dataLoopNodes->NumofSplitters) break;
                OutletNodeNames.allocate(MaxNumAlphas);
                OutletNodeNumbers.allocate(MaxNumAlphas);
                GetLoopSplitter(state,
                                plantLoop.Name,
                                loopSide.ConnectList,
                                loopSide.Splitter.Name,
                                loopSide.Splitter.Exists,
                                loopSide.Splitter.NodeNameIn,
                                loopSide.Splitter.NodeNumIn,
                                loopSide.Splitter.TotalOutletNodes,
                                OutletNodeNames,
                                OutletNodeNumbers,
                                ErrorsFound,
                                ConnNum,
                                SplitNum);

                if (SplitNum == 1) {
                    OutletNodeNames.deallocate();
                    OutletNodeNumbers.deallocate();
                    continue;
                }

                // Map the inlet node to the splitter to a branch number
                if (loopSide.Splitter.Exists) {
                    // Map the inlet node to the splitter to a branch number
                    SplitInBranch = false;
                    for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                        auto &branch = loopSide.Branch(BranchNum);
                        CompNum = branch.TotalComponents;
                        if (loopSide.Splitter.NodeNumIn == branch.Comp(CompNum).NodeNumOut) {
                            loopSide.Splitter.BranchNumIn = BranchNum;
                            SplitInBranch = true;
                            break; // BranchNum DO loop
                        }
                    }
                    if (!SplitInBranch) {
                        ShowSevereError(state, "Splitter Inlet Branch not found, Splitter=" + loopSide.Splitter.Name);
                        ShowContinueError(state, "Splitter Branch Inlet name=" + loopSide.Splitter.NodeNameIn);
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ErrorsFound = true;
                    }

                    loopSide.Splitter.NodeNameOut.allocate(loopSide.Splitter.TotalOutletNodes);
                    loopSide.Splitter.NodeNumOut.dimension(loopSide.Splitter.TotalOutletNodes, 0);
                    loopSide.Splitter.BranchNumOut.dimension(loopSide.Splitter.TotalOutletNodes, 0);

                    SplitOutBranch.allocate(loopSide.Splitter.TotalOutletNodes);
                    SplitOutBranch = false;
                    for (NodeNum = 1; NodeNum <= loopSide.Splitter.TotalOutletNodes; ++NodeNum) {
                        loopSide.Splitter.NodeNameOut(NodeNum) = OutletNodeNames(NodeNum);
                        loopSide.Splitter.NodeNumOut(NodeNum) = OutletNodeNumbers(NodeNum);
                        // The following DO loop series is intended to store the branch number for each outlet
                        // branch of the splitter
                        for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                            if (loopSide.Splitter.NodeNumOut(NodeNum) == loopSide.Branch(BranchNum).Comp(1).NodeNumIn) {
                                loopSide.Splitter.BranchNumOut(NodeNum) = BranchNum;
                                SplitOutBranch(NodeNum) = true;
                                break; // BranchNum DO loop
                            }
                        }
                    }

                    for (Outlet = 1; Outlet <= loopSide.Splitter.TotalOutletNodes; ++Outlet) {
                        if (SplitOutBranch(Outlet)) continue;
                        ShowSevereError(state, "Splitter Outlet Branch not found, Splitter=" + loopSide.Splitter.Name);
                        ShowContinueError(state, "Splitter Branch Outlet node name=" + loopSide.Splitter.NodeNameOut(Outlet));
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ShowContinueError(state, "Loop BranchList=" + loopSide.BranchList);
                        ShowContinueError(state, "Loop ConnectorList=" + loopSide.ConnectList);
                        ErrorsFound = true;
                    }

                    SplitOutBranch.deallocate();

                } // Splitter exists
                OutletNodeNames.deallocate();
                OutletNodeNumbers.deallocate();
            }

            MixNum = 1;
            for (ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum) {

                if (MixNum > state.dataLoopNodes->NumofMixers) break;
                InletNodeNames.allocate(MaxNumAlphas);
                InletNodeNumbers.allocate(MaxNumAlphas);
                GetLoopMixer(state,
                             plantLoop.Name,
                             loopSide.ConnectList,
                             loopSide.Mixer.Name,
                             loopSide.Mixer.Exists,
                             loopSide.Mixer.NodeNameOut,
                             loopSide.Mixer.NodeNumOut,
                             loopSide.Mixer.TotalInletNodes,
                             InletNodeNames,
                             InletNodeNumbers,
                             ErrorsFound,
                             ConnNum,
                             MixNum);

                if (MixNum == 1) {
                    InletNodeNames.deallocate();
                    InletNodeNumbers.deallocate();
                    continue;
                }
                // Map the outlet node of the mixer to a branch number
                if (loopSide.Mixer.Exists) {
                    // Map the outlet node of the mixer to a branch number
                    MixerOutBranch = false;
                    for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                        if (loopSide.Mixer.NodeNumOut == loopSide.Branch(BranchNum).Comp(1).NodeNumIn) {
                            loopSide.Mixer.BranchNumOut = BranchNum;
                            MixerOutBranch = true;
                            break; // BranchNum DO loop
                        }
                    }
                    if (!MixerOutBranch) {
                        ShowSevereError(state, "Mixer Outlet Branch not found, Mixer=" + loopSide.Mixer.Name);
                        ErrorsFound = true;
                    }

                    loopSide.Mixer.NodeNameIn.allocate(loopSide.Mixer.TotalInletNodes);
                    loopSide.Mixer.NodeNumIn.dimension(loopSide.Mixer.TotalInletNodes, 0);
                    loopSide.Mixer.BranchNumIn.dimension(loopSide.Mixer.TotalInletNodes, 0);

                    MixerInBranch.allocate(loopSide.Mixer.TotalInletNodes);
                    MixerInBranch = false;
                    for (NodeNum = 1; NodeNum <= loopSide.Mixer.TotalInletNodes; ++NodeNum) {
                        loopSide.Mixer.NodeNameIn(NodeNum) = InletNodeNames(NodeNum);
                        loopSide.Mixer.NodeNumIn(NodeNum) = InletNodeNumbers(NodeNum);
                        // The following DO loop series is intended to store the branch number for each inlet branch of the mixer
                        for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                            auto &branch = loopSide.Branch(BranchNum);
                            CompNum = branch.TotalComponents;
                            if (loopSide.Mixer.NodeNumIn(NodeNum) == branch.Comp(CompNum).NodeNumOut) {
                                loopSide.Mixer.BranchNumIn(NodeNum) = BranchNum;
                                MixerInBranch(NodeNum) = true;
                                break; // BranchNum DO loop
                            }
                        }
                    }

                    for (Inlet = 1; Inlet <= loopSide.Mixer.TotalInletNodes; ++Inlet) {
                        if (MixerInBranch(Inlet)) continue;
                        ShowSevereError(state, "Mixer Inlet Branch not found, Mixer=" + loopSide.Mixer.Name);
                        ShowContinueError(state, "Mixer Branch Inlet name=" + loopSide.Mixer.NodeNameIn(Inlet));
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ShowContinueError(state, "Loop BranchList=" + loopSide.BranchList);
                        ShowContinueError(state, "Loop ConnectorList=" + loopSide.ConnectList);
                        ErrorsFound = true;
                    }

                    MixerInBranch.deallocate();
                } // Mixer exists
                InletNodeNames.deallocate();
                InletNodeNumbers.deallocate();
            }

            loopSide.noLoadConstantSpeedBranchFlowRateSteps.allocate(loopSide.TotalBranches - 2);

            // TODO: this is just intended to be temporary
            loopSide.myLoopNum = LoopNum;
            loopSide.myLoopSideNum = LoopSideNum;
            loopSide.myOtherLoopSideNum = 3 - LoopSideNum;

        } // ... end LoopSideNum=DemandSide,SupplySide

        plantLoop.LoopSide(1).loopSideDescription = plantLoop.Name + " - Demand Side";
        plantLoop.LoopSide(2).loopSideDescription = plantLoop.Name + " - Supply Side";

        // a nice little spot to report out bad pump/common-pipe configurations
        bool const ThisSideHasPumps = (plantLoop.LoopSide(1).TotalPumps > 0);
        bool const OtherSideHasPumps = (plantLoop.LoopSide(2).TotalPumps > 0);
        if ((plantLoop.CommonPipeType != DataPlant::iCommonPipeType::No) && (!ThisSideHasPumps || !OtherSideHasPumps)) {
            ShowSevereError(state, "Input Error: Common Pipe configurations must have pumps on both sides of loop");
            ShowContinueError(state, "Occurs on plant loop name =\"" + plantLoop.Name + "\"");
            ShowContinueError(state, "Make sure both demand and supply sides have a pump");
            ErrorsFound = true;
        } else if ((plantLoop.CommonPipeType == DataPlant::iCommonPipeType::No) && ThisSideHasPumps && OtherSideHasPumps) {
            ShowSevereError(state, "Input Error: Pumps on both loop sides must utilize a common pipe");
            ShowContinueError(state, "Occurs on plant loop name =\"" + plantLoop.Name + "\"");
            ShowContinueError(state, "Add common pipe or remove one loop side pump");
            ErrorsFound = true;
        } else if (!ThisSideHasPumps && !OtherSideHasPumps) {
            ShowSevereError(state, "SetupLoopFlowRequest: Problem in plant topology, no pumps specified on the loop");
            ShowContinueError(state, "Occurs on plant loop name =\"" + plantLoop.Name + "\"");
            ShowContinueError(state, "All plant loops require at least one pump");
            ErrorsFound = true;
        }

        // set up some pump indexing for convenience later
        for (int LoopSideCounter = 1; LoopSideCounter <= 2; ++LoopSideCounter) {
            for (int PumpCounter = 1; PumpCounter <= plantLoop.LoopSide(LoopSideCounter).TotalPumps; ++PumpCounter) {
                int const PumpBranchNum = plantLoop.LoopSide(LoopSideCounter).Pumps(PumpCounter).BranchNum;
                int const PumpCompNum = plantLoop.LoopSide(LoopSideCounter).Pumps(PumpCounter).CompNum;
                plantLoop.LoopSide(LoopSideCounter).Branch(PumpBranchNum).Comp(PumpCompNum).IndexInLoopSidePumps = PumpCounter;
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "GetPlantInput: Errors in getting PlantLoop Input");
    }

    if (state.dataHVACGlobal->NumPlantLoops > 0) state.dataPlnt->VentRepPlantSupplySide.allocate(state.dataHVACGlobal->NumPlantLoops);
    if (state.dataHVACGlobal->NumPlantLoops > 0) state.dataPlnt->VentRepPlantDemandSide.allocate(state.dataHVACGlobal->NumPlantLoops);

    for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumPlantLoops; ++LoopNum) {

        // set up references for this loop
        auto &this_plant_loop(state.dataPlnt->PlantLoop(LoopNum));
        auto &this_plant_supply(this_plant_loop.LoopSide(SupplySide));
        auto &this_vent_plant_supply(state.dataPlnt->VentRepPlantSupplySide(LoopNum));
        auto &this_plant_demand(this_plant_loop.LoopSide(DemandSide));
        auto &this_vent_plant_demand(state.dataPlnt->VentRepPlantDemandSide(LoopNum));

        this_vent_plant_supply.Name = this_plant_loop.Name;
        this_vent_plant_supply.NodeNumIn = this_plant_supply.NodeNumIn;
        this_vent_plant_supply.NodeNameIn = this_plant_supply.NodeNameIn;
        this_vent_plant_supply.NodeNumOut = this_plant_supply.NodeNumOut;
        this_vent_plant_supply.NodeNameOut = this_plant_supply.NodeNameOut;
        this_vent_plant_supply.TotalBranches = this_plant_supply.TotalBranches;

        if (this_vent_plant_supply.TotalBranches > 0) this_vent_plant_supply.Branch.allocate(this_vent_plant_supply.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_plant_supply.TotalBranches; ++BranchNum) {

            auto &this_plant_supply_branch(state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum));
            auto &this_vent_plant_supply_branch(state.dataPlnt->VentRepPlantSupplySide(LoopNum).Branch(BranchNum));

            this_vent_plant_supply_branch.Name = this_plant_supply_branch.Name;
            this_vent_plant_supply_branch.NodeNumIn = this_plant_supply_branch.NodeNumIn;
            this_vent_plant_supply_branch.NodeNumOut = this_plant_supply_branch.NodeNumOut;
            this_vent_plant_supply_branch.TotalComponents = this_plant_supply_branch.TotalComponents;
            if (this_vent_plant_supply_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_plant_supply_branch.TotalComponents;
                this_vent_plant_supply_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= state.dataPlnt->VentRepPlantSupplySide(LoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {

                auto &this_plant_supply_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_plant_supply_comp(state.dataPlnt->VentRepPlantSupplySide(LoopNum).Branch(BranchNum).Comp(CompNum));

                this_vent_plant_supply_comp.Name = this_plant_supply_comp.Name;
                this_vent_plant_supply_comp.TypeOf = this_plant_supply_comp.TypeOf;
                this_vent_plant_supply_comp.NodeNameIn = this_plant_supply_comp.NodeNameIn;
                this_vent_plant_supply_comp.NodeNameOut = this_plant_supply_comp.NodeNameOut;
                this_vent_plant_supply_comp.NodeNumIn = this_plant_supply_comp.NodeNumIn;
                this_vent_plant_supply_comp.NodeNumOut = this_plant_supply_comp.NodeNumOut;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

        this_vent_plant_demand.Name = this_plant_loop.Name;
        this_vent_plant_demand.NodeNumIn = this_plant_demand.NodeNumIn;
        this_vent_plant_demand.NodeNameIn = this_plant_demand.NodeNameIn;
        this_vent_plant_demand.NodeNumOut = this_plant_demand.NodeNumOut;
        this_vent_plant_demand.NodeNameOut = this_plant_demand.NodeNameOut;
        this_vent_plant_demand.TotalBranches = this_plant_demand.TotalBranches;

        if (this_vent_plant_demand.TotalBranches > 0) this_vent_plant_demand.Branch.allocate(this_vent_plant_demand.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_plant_demand.TotalBranches; ++BranchNum) {

            auto &this_plant_demand_branch(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum));
            auto &this_vent_plant_demand_branch(state.dataPlnt->VentRepPlantDemandSide(LoopNum).Branch(BranchNum));

            this_vent_plant_demand_branch.Name = this_plant_demand_branch.Name;
            this_vent_plant_demand_branch.NodeNumIn = this_plant_demand_branch.NodeNumIn;
            this_vent_plant_demand_branch.NodeNumOut = this_plant_demand_branch.NodeNumOut;
            this_vent_plant_demand_branch.TotalComponents = this_plant_demand_branch.TotalComponents;
            if (this_vent_plant_demand_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_plant_demand_branch.TotalComponents;
                this_vent_plant_demand_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= this_vent_plant_demand_branch.TotalComponents; ++CompNum) {

                auto &this_plant_demand_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_plant_demand_comp(state.dataPlnt->VentRepPlantDemandSide(LoopNum).Branch(BranchNum).Comp(CompNum));

                this_vent_plant_demand_comp.Name = this_plant_demand_comp.Name;
                this_vent_plant_demand_comp.TypeOf = this_plant_demand_comp.TypeOf;
                this_vent_plant_demand_comp.NodeNameIn = this_plant_demand_comp.NodeNameIn;
                this_vent_plant_demand_comp.NodeNameOut = this_plant_demand_comp.NodeNameOut;
                this_vent_plant_demand_comp.NodeNumIn = this_plant_demand_comp.NodeNumIn;
                this_vent_plant_demand_comp.NodeNumOut = this_plant_demand_comp.NodeNumOut;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

    } // loop over plant supply loops (ventilation report data)

    if (state.dataHVACGlobal->NumCondLoops > 0) state.dataPlnt->VentRepCondSupplySide.allocate(state.dataHVACGlobal->NumCondLoops);
    if (state.dataHVACGlobal->NumCondLoops > 0) state.dataPlnt->VentRepCondDemandSide.allocate(state.dataHVACGlobal->NumCondLoops);

    for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumCondLoops; ++LoopNum) {

        LoopNumInArray = LoopNum + state.dataHVACGlobal->NumPlantLoops;

        // set up references for this loop
        auto &this_cond_loop(state.dataPlnt->PlantLoop(LoopNumInArray));
        auto &this_cond_supply(this_cond_loop.LoopSide(SupplySide));
        auto &this_vent_cond_supply(state.dataPlnt->VentRepCondSupplySide(LoopNum));
        auto &this_cond_demand(this_cond_loop.LoopSide(DemandSide));
        auto &this_vent_cond_demand(state.dataPlnt->VentRepCondDemandSide(LoopNum));

        this_vent_cond_supply.Name = this_cond_loop.Name;
        this_vent_cond_supply.NodeNumIn = this_cond_supply.NodeNumIn;
        this_vent_cond_supply.NodeNameIn = this_cond_supply.NodeNameIn;
        this_vent_cond_supply.NodeNumOut = this_cond_supply.NodeNumOut;
        this_vent_cond_supply.NodeNameOut = this_cond_supply.NodeNameOut;
        this_vent_cond_supply.TotalBranches = this_cond_supply.TotalBranches;
        if (this_vent_cond_supply.TotalBranches > 0) this_vent_cond_supply.Branch.allocate(this_vent_cond_supply.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_cond_supply.TotalBranches; ++BranchNum) {

            auto &this_cond_supply_branch(this_cond_supply.Branch(BranchNum));
            auto &this_vent_cond_supply_branch(this_vent_cond_supply.Branch(BranchNum));

            this_vent_cond_supply_branch.Name = this_cond_supply_branch.Name;
            this_vent_cond_supply_branch.NodeNumIn = this_cond_supply_branch.NodeNumIn;
            this_vent_cond_supply_branch.NodeNumOut = this_cond_supply_branch.NodeNumOut;
            this_vent_cond_supply_branch.TotalComponents = this_cond_supply_branch.TotalComponents;
            if (this_vent_cond_supply_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_cond_supply_branch.TotalComponents;
                this_vent_cond_supply_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= this_vent_cond_supply_branch.TotalComponents; ++CompNum) {

                auto &this_cond_supply_comp(this_cond_loop.LoopSide(SupplySide).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_cond_supply_comp(this_vent_cond_supply.Branch(BranchNum).Comp(CompNum));

                this_vent_cond_supply_comp.Name = this_cond_supply_comp.Name;
                this_vent_cond_supply_comp.TypeOf = this_cond_supply_comp.TypeOf;
                this_vent_cond_supply_comp.NodeNameIn = this_cond_supply_comp.NodeNameIn;
                this_vent_cond_supply_comp.NodeNameOut = this_cond_supply_comp.NodeNameOut;
                this_vent_cond_supply_comp.NodeNumIn = this_cond_supply_comp.NodeNumIn;
                this_vent_cond_supply_comp.NodeNumOut = this_cond_supply_comp.NodeNumOut;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

        this_vent_cond_demand.Name = this_cond_loop.Name;
        this_vent_cond_demand.NodeNumIn = this_cond_demand.NodeNumIn;
        this_vent_cond_demand.NodeNameIn = this_cond_demand.NodeNameIn;
        this_vent_cond_demand.NodeNumOut = this_cond_demand.NodeNumOut;
        this_vent_cond_demand.NodeNameOut = this_cond_demand.NodeNameOut;
        this_vent_cond_demand.TotalBranches = this_cond_demand.TotalBranches;
        if (this_vent_cond_demand.TotalBranches > 0) this_vent_cond_demand.Branch.allocate(this_vent_cond_demand.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_cond_demand.TotalBranches; ++BranchNum) {

            auto &this_cond_demand_branch(this_cond_demand.Branch(BranchNum));
            auto &this_vent_cond_demand_branch(this_vent_cond_demand.Branch(BranchNum));

            this_vent_cond_demand_branch.Name = this_cond_demand_branch.Name;
            this_vent_cond_demand_branch.NodeNumIn = this_cond_demand_branch.NodeNumIn;
            this_vent_cond_demand_branch.NodeNumOut = this_cond_demand_branch.NodeNumOut;
            this_vent_cond_demand_branch.TotalComponents = this_cond_demand_branch.TotalComponents;
            if (this_vent_cond_demand_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_cond_demand_branch.TotalComponents;
                this_vent_cond_demand_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= this_vent_cond_demand_branch.TotalComponents; ++CompNum) {

                auto &this_cond_demand_comp(this_cond_demand_branch.Comp(CompNum));
                auto &this_vent_cond_demand_comp(this_vent_cond_demand_branch.Comp(CompNum));

                this_vent_cond_demand_comp.Name = this_cond_demand_comp.Name;
                this_vent_cond_demand_comp.TypeOf = this_cond_demand_comp.TypeOf;
                this_vent_cond_demand_comp.NodeNameIn = this_cond_demand_comp.NodeNameIn;
                this_vent_cond_demand_comp.NodeNameOut = this_cond_demand_comp.NodeNameOut;
                this_vent_cond_demand_comp.NodeNumIn = this_cond_demand_comp.NodeNumIn;
                this_vent_cond_demand_comp.NodeNumOut = this_cond_demand_comp.NodeNumOut;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

    } // loop over plant supply loops (ventilation report data)
}

void SetupReports(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   July 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the plant supply side reports.
    // It was created during the splitting of supply and demand side functions.

    // Using/Aliasing
    using DataPlant::DemandOpSchemeType;
    using DataPlant::DemandSide;
    using DataPlant::SupplySide;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum; // DO loop counter (plant supply sides)
    int LoopSideNum;
    int BranchNum;
    int CompNum;
    int MaxBranches;                 // Maximum number of branches on any plant loop (used for allocating arrays)
    std::string CurrentModuleObject; // for ease in renaming.
    int FluidIndex;

    MaxBranches = 0;
    for (auto &loop : state.dataPlnt->PlantLoop) {
        MaxBranches = max(MaxBranches, loop.LoopSide(DemandSide).TotalBranches);
        MaxBranches = max(MaxBranches, loop.LoopSide(SupplySide).TotalBranches);
        loop.MaxBranch = MaxBranches;
        loop.CoolingDemand = 0.0;
        loop.HeatingDemand = 0.0;
        loop.DemandNotDispatched = 0.0;
        loop.UnmetDemand = 0.0;
        loop.InletNodeTemperature = 0.0;
        loop.OutletNodeTemperature = 0.0;
        loop.InletNodeFlowrate = 0.0;
        loop.BypassFrac = 0.0;
        loop.OutletNodeFlowrate = 0.0;
    }

    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        auto &loop = state.dataPlnt->PlantLoop(LoopNum);
        if (LoopNum <= state.dataHVACGlobal->NumPlantLoops) {
            CurrentModuleObject = "Plant Loop";
        } else {
            CurrentModuleObject = "Cond Loop";
        }
        // CurrentModuleObject='Plant/Condenser Loop'
        SetupOutputVariable(state,
                            "Plant Supply Side Cooling Demand Rate",
                            OutputProcessor::Unit::W,
                            loop.CoolingDemand,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Heating Demand Rate",
                            OutputProcessor::Unit::W,
                            loop.HeatingDemand,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Inlet Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            loop.InletNodeFlowrate,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);

        SetupOutputVariable(state,
                            "Plant Supply Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            loop.InletNodeTemperature,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            loop.OutletNodeTemperature,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);

        SetupOutputVariable(state,
                            "Plant Supply Side Not Distributed Demand Rate",
                            OutputProcessor::Unit::W,
                            loop.DemandNotDispatched,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Unmet Demand Rate",
                            OutputProcessor::Unit::W,
                            loop.UnmetDemand,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Debug Plant Loop Bypass Fraction",
                            OutputProcessor::Unit::None,
                            loop.BypassFrac,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Debug Plant Last Simulated Loop Side",
                            OutputProcessor::Unit::None,
                            loop.LastLoopSideSimulated,
                            "System",
                            "Average",
                            state.dataPlnt->PlantLoop(LoopNum).Name);
    }

    // setup more variables inside plant data structure
    // CurrentModuleObject='Plant/Condenser Loop(Advanced)'
    if (state.dataGlobal->DisplayAdvancedReportVariables) {
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TankTemp,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Temperature",
                                OutputProcessor::Unit::C,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_TankTemp,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Heat Transport Rate",
                                OutputProcessor::Unit::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_MdotCpDeltaT,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Heat Transport Rate",
                                OutputProcessor::Unit::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_MdotCpDeltaT,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Heat Storage Rate",
                                OutputProcessor::Unit::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_McpDTdt,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Heat Storage Rate",
                                OutputProcessor::Unit::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_McpDTdt,
                                "System",
                                "Average",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Excessive Storage Time",
                                OutputProcessor::Unit::hr,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_CapExcessStorageTimeReport,
                                "System",
                                "Sum",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Excessive Storage Time",
                                OutputProcessor::Unit::hr,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_CapExcessStorageTimeReport,
                                "System",
                                "Sum",
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType !=
                            DemandOpSchemeType) {
                            SetupOutputVariable(state,
                                                "Plant Component Distributed Demand Rate",
                                                OutputProcessor::Unit::W,
                                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).MyLoad,
                                                "System",
                                                "Average",
                                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).Name);
                        }
                    }
                }
            }
        }
    }

    // now traverse plant loops and set fluid type index in all nodes on the loop
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        FluidIndex = state.dataPlnt->PlantLoop(LoopNum).FluidIndex;
        for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumIn).FluidIndex = FluidIndex;
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).FluidIndex = FluidIndex;
            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumIn)
                        .FluidIndex = FluidIndex;
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumOut)
                        .FluidIndex = FluidIndex;
                }
            }
        }
    } // plant loops
}

void InitializeLoops(EnergyPlusData &state, bool const FirstHVACIteration) // true if first iteration of the simulation
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   May 2005
    //       MODIFIED       Dan Fisher Aug. 2008
    //                      Brent Griffith May 2009 EMS setpoint check
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the
    // Plant loop nodes one time at the beginning of the simulation.
    // It also reinitializes loop temperatures if loop setpoint
    // temperature changes. Branch levels for all branches are also set.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;
    using namespace DataSizing;
    using EMSManager::CheckIfNodeSetPointManagedByEMS;

    using PlantUtilities::SetAllFlowLocks;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum; // plant loop counter
    int LoopSideNum;
    int BranchNum; // branch loop counter
    int CompNum;   // plant side component counter
    int SensedNode;

    bool ErrorsFound(false);
    bool FinishSizingFlag;

    int HalfLoopNum;
    int passNum;

    if (!allocated(state.dataPlantMgr->PlantLoopSetPointInitFlag)) {
        state.dataPlantMgr->PlantLoopSetPointInitFlag.allocate(state.dataPlnt->TotNumLoops);
    }

    // Initialize the setpoints  for Load range based schemes only as determined by the init flag
    // The input already requires a loop setpoint.  The plantloop object requires
    // specification of a loop node and corresponding setpoint manager.  Using a 'component setpoint'
    // control scheme does NOT eliminate the requirement for a plant loop setpoint.  So there is
    // already the possibility that a component setpoint controlled object on the loop outlet
    // branch would have the same setpoint node as the loop.  I don't think setpoint manager traps
    // for this user input error, but it might.  Since both loop and component setpoints already
    // peacefully coexist on the loop, we can allow the user to intentionally specify and use both.
    // The only change required is to NOT smear the loop setpoint over all the loop nodes.  Just
    // read it from the setpoint node and use it.  In the short term it will remain up to the user
    // to specify the location of the loop setpoint control node and avoid conflicts with component
    // setpoint nodes.  Operationally, we will ignore the user specified placement of the loop setpoint
    // node and assume that it is physically located at each half loop outlet for purposes of calculating loop
    // demand.  Long term, I recommend that we:
    //     1. specify the setpointmanager:plant object name (not the node name) in the plantloop/condloop objects
    //     2. write a new setpoint manager (setpointmanager:plant) that is more suitable for plant use and
    //        accomodates AIR and GROUND setpoints...with offsets.

    //*****************************************************************
    // ONE TIME LOOP NODE SETPOINT CHECK
    //*****************************************************************
    if (state.dataPlantMgr->MySetPointCheckFlag && state.dataHVACGlobal->DoSetPointTest) {

        // check for missing setpoints
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {

            SensedNode = state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum;
            if (SensedNode > 0) {
                if (state.dataLoopNodes->Node(SensedNode).TempSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state,
                                        "PlantManager: No Setpoint Manager Defined for Node=" + state.dataLoopNodes->NodeID(SensedNode) +
                                            " in PlantLoop=" + state.dataPlnt->PlantLoop(LoopNum).Name);
                        ShowContinueError(state, "Add Temperature Setpoint Manager with Control Variable = \"Temperature\" for this PlantLoop.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // need call to EMS to check node
                        CheckIfNodeSetPointManagedByEMS(
                            state, SensedNode, EMSManager::SPControlType::iTemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            "PlantManager: No Setpoint Manager Defined for Node=" + state.dataLoopNodes->NodeID(SensedNode) +
                                                " in PlantLoop=" + state.dataPlnt->PlantLoop(LoopNum).Name);
                            ShowContinueError(state, "Add Temperature Setpoint Manager with Control Variable = \"Temperature\" for this PlantLoop.");
                            ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                        }
                    }
                }
            }
        }
        state.dataPlantMgr->MySetPointCheckFlag = false;
    }
    //*****************************************************************
    // END ONE TIME LOOP NODE SETPOINT CHECK

    //*****************************************************************
    // First Pass PUMP AND SIZING INIT
    //*****************************************************************
    if (!state.dataPlnt->PlantFirstSizeCompleted) {

        SetAllFlowLocks(state, DataPlant::iFlowLock::Unlocked);
        FinishSizingFlag = false;
        state.dataPlnt->PlantFirstSizesOkayToFinalize = false; // set global flag for when it ready to store final sizes
        state.dataPlnt->PlantFirstSizesOkayToReport = false;
        state.dataPlnt->PlantFinalSizesOkayToReport = false;
        state.dataPlantMgr->GetCompSizFac = true;
        for (passNum = 1; passNum <= 4; ++passNum) { // begin while loop to iterate over the next calls sequentially
            state.dataPlantMgr->InitLoopEquip = true;

            // Step 2, call component models it  using PlantCallingOrderInfo for sizing
            for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {
                LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
                LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
                state.dataSize->CurLoopNum = LoopNum;

                for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        state.dataPlnt->PlantLoop(LoopNum)
                            .LoopSide(LoopSideNum)
                            .Branch(BranchNum)
                            .Comp(CompNum)
                            .simulate(state, FirstHVACIteration, state.dataPlantMgr->InitLoopEquip, state.dataPlantMgr->GetCompSizFac);
                    } //-CompNum
                }     //-BranchNum
            }

            // step 3, revise calling order
            // have now called each plant component model at least once with InitLoopEquip = .TRUE.
            //  this means the calls to InterConnectTwoPlantLoopSides have now been made, so rework calling order
            RevisePlantCallingOrder(state);

            // Step 4: Simulate plant loop components so their design flows are included

            for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {

                LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
                LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
                state.dataSize->CurLoopNum = LoopNum;
                if (LoopSideNum == SupplySide) {
                    SizePlantLoop(state, LoopNum, FinishSizingFlag);
                }
            }
            state.dataPlantMgr->GetCompSizFac = false;
        } // iterative passes thru sizing related routines.  end while?

        // Step 5 now one more time for the final
        for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {
            if (state.dataGlobal->DoHVACSizingSimulation) {
                state.dataPlnt->PlantFirstSizesOkayToFinalize = true;
                FinishSizingFlag = true;
                state.dataPlnt->PlantFirstSizesOkayToReport = true;
                state.dataPlnt->PlantFinalSizesOkayToReport = false;
            } else {
                state.dataPlnt->PlantFirstSizesOkayToFinalize = true;
                FinishSizingFlag = true;
                state.dataPlnt->PlantFirstSizesOkayToReport = false;
                state.dataPlnt->PlantFinalSizesOkayToReport = true;
            }
            LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
            LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
            state.dataSize->CurLoopNum = LoopNum;
            if (LoopSideNum == SupplySide) {
                SizePlantLoop(state, LoopNum, FinishSizingFlag);
            }
            // pumps are special so call them directly
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).SimulateAllLoopSidePumps(state);
            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    state.dataPlnt->PlantLoop(LoopNum)
                        .LoopSide(LoopSideNum)
                        .Branch(BranchNum)
                        .Comp(CompNum)
                        .simulate(state, FirstHVACIteration, state.dataPlantMgr->InitLoopEquip, state.dataPlantMgr->GetCompSizFac);
                } //-CompNum
            }     //-BranchNum
            //                if ( PlantLoop( LoopNum ).PlantSizNum > 0 ) PlantSizData( PlantLoop( LoopNum ).PlantSizNum
            //).VolFlowSizingDone = true;
        }

        state.dataPlnt->PlantFirstSizeCompleted = true;
        state.dataPlnt->PlantFirstSizesOkayToReport = false;
    }
    //*****************************************************************
    // END First Pass SIZING INIT
    //*****************************************************************
    //*****************************************************************
    // BEGIN Resizing Pass for HVAC Sizing Simultion Adjustments
    //*****************************************************************
    if (state.dataGlobal->RedoSizesHVACSimulation && !state.dataPlnt->PlantReSizingCompleted) {

        // cycle through plant equipment calling with InitLoopEquip true
        state.dataPlantMgr->InitLoopEquip = true;
        state.dataPlantMgr->GetCompSizFac = false;
        for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {
            LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
            LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
            state.dataSize->CurLoopNum = LoopNum;

            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    state.dataPlnt->PlantLoop(LoopNum)
                        .LoopSide(LoopSideNum)
                        .Branch(BranchNum)
                        .Comp(CompNum)
                        .simulate(state, FirstHVACIteration, state.dataPlantMgr->InitLoopEquip, state.dataPlantMgr->GetCompSizFac);
                } //-CompNum
            }     //-BranchNum
        }

        // reset loop level
        state.dataPlnt->PlantFinalSizesOkayToReport = true;
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            ResizePlantLoopLevelSizes(state, LoopNum);
        }

        state.dataPlantMgr->InitLoopEquip = true;

        // now call everything again to reporting turned on
        for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {
            LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
            LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;
            state.dataSize->CurLoopNum = LoopNum;

            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    state.dataPlnt->PlantLoop(LoopNum)
                        .LoopSide(LoopSideNum)
                        .Branch(BranchNum)
                        .Comp(CompNum)
                        .simulate(state, FirstHVACIteration, state.dataPlantMgr->InitLoopEquip, state.dataPlantMgr->GetCompSizFac);
                } //-CompNum
            }     //-BranchNum
            // pumps are special so call them directly
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).SimulateAllLoopSidePumps(state);
        }

        state.dataPlnt->PlantReSizingCompleted = true;
        state.dataPlnt->PlantFinalSizesOkayToReport = false;
    }
    //*****************************************************************
    // END Resizing Pass for HVAC Sizing Simulation Adjustments
    //*****************************************************************
    //*****************************************************************
    // BEGIN ONE TIME ENVIRONMENT INITS
    //*****************************************************************
    if (state.dataPlantMgr->SupplyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                // check if setpoints being placed on node properly
                if (state.dataPlnt->PlantLoop(LoopNum).LoopDemandCalcScheme == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointHi == SensedNodeFlagValue) {
                        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                            ShowSevereError(state, "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme");
                            ShowContinueError(
                                state, "Node Referenced =" + state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum));
                            ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                            state.dataHVACGlobal->SetPointErrorFlag = true;
                        } else {
                            CheckIfNodeSetPointManagedByEMS(state,
                                                            state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum,
                                                            EMSManager::SPControlType::iTemperatureMaxSetPoint,
                                                            state.dataHVACGlobal->SetPointErrorFlag);
                            if (state.dataHVACGlobal->SetPointErrorFlag) {
                                ShowSevereError(state, "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme");
                                ShowContinueError(
                                    state, "Node Referenced =" + state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum));
                                ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                                ShowContinueError(state, "Or add EMS Actuator for Temperature Maximum Setpoint");

                            } // SetPointErrorFlag
                        }     // Not EMS
                    }         // Node TSPhi = Sensed
                    if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointLo == SensedNodeFlagValue) {
                        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                            ShowSevereError(state, "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme");
                            ShowContinueError(
                                state, "Node Referenced =" + state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum));
                            ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                            state.dataHVACGlobal->SetPointErrorFlag = true;
                        } else {
                            CheckIfNodeSetPointManagedByEMS(state,
                                                            state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum,
                                                            EMSManager::SPControlType::iTemperatureMinSetPoint,
                                                            state.dataHVACGlobal->SetPointErrorFlag);
                            if (state.dataHVACGlobal->SetPointErrorFlag) {
                                ShowSevereError(state, "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme");
                                ShowContinueError(
                                    state, "Node Referenced =" + state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum));
                                ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                                ShowContinueError(state, "Or add EMS Actuator for Temperature Minimum Setpoint");

                            } // SetPointErrorFlag
                        }     // NOT EMS
                    }         // Node TSPtLo = Sensed...
                }             // LoopDemandScheme = DualSPDB
            }                 // LOOPSIDE
        }                     // PLANT LOOP

        // Any per-environment load distribution init should be OK here
        // Just clear away any trailing MyLoad for now...
        // This could likely be moved into InitLoadDistribution also...
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
                for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).MyLoad = 0.0;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).FreeCoolCntrlShutDown = false;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).Available = false;
                    }
                }
            }
        }

        state.dataPlantMgr->SupplyEnvrnFlag = false;
        //!*****************************************************************
        // !END OF ONE TIME ENVIRONMENT INITS
        //!*****************************************************************
    } //
    if (!state.dataGlobal->BeginEnvrnFlag) state.dataPlantMgr->SupplyEnvrnFlag = true;

    if (ErrorsFound) ShowFatalError(state, "Preceding errors caused termination");
}

void ReInitPlantLoopsAtFirstHVACIteration(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // initialize node mass flow requests

    // METHODOLOGY EMPLOYED:
    // called from SimHVAC to reset mass flow rate requests
    // this contains all the initializations

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const StartQuality(1.0);
    Real64 const StartHumRat(0.0);
    static constexpr std::string_view RoutineNameAlt("InitializeLoops");
    static constexpr std::string_view RoutineName("PlantManager:InitializeLoop");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum;                      // plant loop counter
    Real64 LoopMaxMassFlowRate;       // maximum allowable loop mass flow rate
    Real64 LoopSetPointTemp;          // the loop control or setpoint temperature
    Real64 LoopMaxTemp;               // maximum allowable loop temperature
    Real64 LoopMinTemp;               // minimum allowable loop temperature
    Real64 LoopSetPointTempLo;        // the loop control or setpoint temperature
    Real64 LoopSetPointTempHi;        // the loop control or setpoint temperature
    Real64 SecondaryLoopSetPointTemp; // loop setpoint temperature for common pipes with different secondary setpt
    int LoopSideNum;
    int BranchNum;       // branch loop counter
    int OpNum;           // operation scheme counter
    int CompNum;         // plant side component counter
    int BranchInlet;     // branch inlet node number
    int ComponentInlet;  // component inlet node number
    int ComponentOutlet; // component outlet node number

    Real64 LoopMinMassFlowRate; // minimum allowable loop mass flow rate
    Real64 SteamDensity;
    Real64 SteamTemp;
    Real64 StartEnthalpy;
    Real64 Cp;
    Real64 rho;
    Real64 LoopSetPointTemperatureHi;
    Real64 LoopSetPointTemperatureLo;

    //*****************************************************************
    // BEGIN ENVIRONMENT INITS
    //*****************************************************************

    if (state.dataPlantMgr->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {

                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(LoopNum).LoopDemandCalcScheme);

                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        LoopSetPointTemp = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;

                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        // Get the range of setpoints
                        LoopSetPointTemperatureHi = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointHi;
                        LoopSetPointTemperatureLo = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointLo;
                        LoopSetPointTemp = (LoopSetPointTemperatureLo + LoopSetPointTemperatureHi) / 2.0;
                    }
                }

                if ((state.dataPlnt->PlantLoop(LoopNum).CommonPipeType == DataPlant::iCommonPipeType::TwoWay) && (LoopSideNum == DemandSide) &&
                    (state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt)) { // get a second setpoint for secondaryLoop
                    // if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
                    // we want to initialize the demand side of the loop using that setpoint
                    LoopSetPointTemp = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn).TempSetPoint;
                }

                // Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
                LoopMaxTemp = state.dataPlnt->PlantLoop(LoopNum).MaxTemp;
                LoopMinTemp = state.dataPlnt->PlantLoop(LoopNum).MinTemp;

                // trap for -999 and set to average of limits if so
                if (LoopSetPointTemp == SensedNodeFlagValue) {
                    LoopSetPointTemp = (LoopMinTemp + LoopMaxTemp) / 2.0;
                }
                // Check it against the loop temperature limits
                LoopSetPointTemp = min(LoopMaxTemp, LoopSetPointTemp);
                LoopSetPointTemp = max(LoopMinTemp, LoopSetPointTemp);

                // Initialize the capacitance model at the tank interface, and other loop side values
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TempInterfaceTankOutlet = LoopSetPointTemp;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).LastTempInterfaceTankOutlet = LoopSetPointTemp;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).LoopSideInlet_TankTemp = LoopSetPointTemp;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalPumpHeat = 0.0;
                if (allocated(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Pumps))
                    for (auto &e : state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Pumps)
                        e.PumpHeatToFluid = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowRequest = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TimeElapsed = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock = DataPlant::iFlowLock::Unlocked;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).InletNode.TemperatureHistory = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).InletNode.MassFlowRateHistory = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).OutletNode.TemperatureHistory = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).OutletNode.MassFlowRateHistory = 0.0;

                if (state.dataPlnt->PlantLoop(LoopNum).FluidType != DataLoopNode::NodeFluidType::Steam) {
                    Cp = GetSpecificHeatGlycol(state,
                                               state.dataPlnt->PlantLoop(LoopNum).FluidName,
                                               LoopSetPointTemp,
                                               state.dataPlnt->PlantLoop(LoopNum).FluidIndex,
                                               RoutineNameAlt);
                    StartEnthalpy = Cp * LoopSetPointTemp;
                }
                // Use Min/Max flow rates to initialize loop
                if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Water) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(LoopNum).FluidName,
                                           LoopSetPointTemp,
                                           state.dataPlnt->PlantLoop(LoopNum).FluidIndex,
                                           RoutineNameAlt);

                    LoopMaxMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * rho;
                    LoopMinMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate * rho;
                }
                // use saturated liquid of steam at the loop setpoint temp as the starting enthalpy for a water loop
                if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Steam) {
                    SteamTemp = 100.0;
                    SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, SteamTemp, 1.0, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
                    LoopMaxMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * SteamDensity;
                    StartEnthalpy = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, LoopSetPointTemp, 0.0, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
                    LoopMinMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate * SteamDensity;
                }

                LoopMaxMassFlowRate = max(0.0, LoopMaxMassFlowRate);
                LoopMinMassFlowRate = max(0.0, LoopMinMassFlowRate);

                // Initial all loop nodes by initializing all component inlet and outlet nodes
                for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        ComponentInlet = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                        ComponentOutlet = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumOut;
                        BranchInlet = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).NodeNumIn;

                        state.dataLoopNodes->Node(ComponentInlet).Temp = LoopSetPointTemp;
                        state.dataLoopNodes->Node(ComponentInlet).TempMin = LoopMinTemp;
                        state.dataLoopNodes->Node(ComponentInlet).TempMax = LoopMaxTemp;
                        state.dataLoopNodes->Node(ComponentInlet).TempLastTimestep = LoopSetPointTemp;

                        state.dataLoopNodes->Node(ComponentInlet).MassFlowRate = 0.0;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).MyLoad = 0.0;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).Available = false;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).FreeCoolCntrlShutDown = false;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).RequestedMassFlow = 0.0;

                        if (state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMin > 0.0) {
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMinAvail =
                                state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMin;
                        } else {
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMin = LoopMinMassFlowRate;
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMinAvail = LoopMinMassFlowRate;
                        }

                        if (state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMax > 0.0) {
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMaxAvail =
                                state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMax;
                        } else {
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMax = LoopMaxMassFlowRate;
                            state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMaxAvail = LoopMaxMassFlowRate;
                        }

                        state.dataLoopNodes->Node(ComponentInlet).MassFlowRateRequest = 0.0;
                        state.dataLoopNodes->Node(ComponentInlet).Quality = StartQuality;
                        state.dataLoopNodes->Node(ComponentInlet).Press = state.dataEnvrn->StdBaroPress;
                        state.dataLoopNodes->Node(ComponentInlet).Enthalpy = StartEnthalpy;
                        state.dataLoopNodes->Node(ComponentInlet).HumRat = StartHumRat;

                        state.dataLoopNodes->Node(ComponentOutlet).FluidType = state.dataLoopNodes->Node(BranchInlet).FluidType;
                        state.dataLoopNodes->Node(ComponentOutlet).Temp = state.dataLoopNodes->Node(BranchInlet).Temp;
                        state.dataLoopNodes->Node(ComponentOutlet).TempMin = state.dataLoopNodes->Node(BranchInlet).TempMin;
                        state.dataLoopNodes->Node(ComponentOutlet).TempMax = state.dataLoopNodes->Node(BranchInlet).TempMax;
                        state.dataLoopNodes->Node(ComponentOutlet).TempLastTimestep = state.dataLoopNodes->Node(BranchInlet).TempLastTimestep;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRate = state.dataLoopNodes->Node(BranchInlet).MassFlowRate;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMin = state.dataLoopNodes->Node(BranchInlet).MassFlowRateMin;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMax = state.dataLoopNodes->Node(BranchInlet).MassFlowRateMax;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMinAvail = state.dataLoopNodes->Node(BranchInlet).MassFlowRateMinAvail;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMaxAvail = state.dataLoopNodes->Node(BranchInlet).MassFlowRateMaxAvail;
                        state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateRequest = 0.0;
                        state.dataLoopNodes->Node(ComponentOutlet).Quality = StartQuality;
                        state.dataLoopNodes->Node(ComponentOutlet).Press = state.dataEnvrn->StdBaroPress;
                        state.dataLoopNodes->Node(ComponentOutlet).Enthalpy = StartEnthalpy;
                        state.dataLoopNodes->Node(ComponentOutlet).HumRat = StartHumRat;
                    } // COMPONENT LOOP
                }     // BRANCH LOOP
            }         // LOOPSIDE
        }             // PLANT LOOP
        for (auto &loop : state.dataPlnt->PlantLoop) {
            loop.CoolingDemand = 0.0;
            loop.HeatingDemand = 0.0;
            loop.DemandNotDispatched = 0.0;
            loop.UnmetDemand = 0.0;
            loop.LastLoopSideSimulated = 0;
            loop.InletNodeFlowrate = 0.0;
            loop.InletNodeTemperature = 0.0;
            loop.OutletNodeFlowrate = 0.0;
            loop.OutletNodeTemperature = 0.0;
        }

        state.dataPlantMgr->MyEnvrnFlag = false;
        //*****************************************************************
        // END OF ENVIRONMENT INITS
        //*****************************************************************
    }

    if (!state.dataGlobal->BeginEnvrnFlag) state.dataPlantMgr->MyEnvrnFlag = true;

    // FirstHVACiteration inits
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        LoopSetPointTemp = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;

        // Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
        LoopMaxTemp = state.dataPlnt->PlantLoop(LoopNum).MaxTemp;
        LoopMinTemp = state.dataPlnt->PlantLoop(LoopNum).MinTemp;
        // Check it against the loop temperature limits
        LoopSetPointTemp = min(LoopMaxTemp, LoopSetPointTemp);
        LoopSetPointTemp = max(LoopMinTemp, LoopSetPointTemp);

        // Update supply side loop setpoint in plant data structure
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TempSetPoint = LoopSetPointTemp;
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPoint = LoopSetPointTemp;

        // Update supply side hi-lo setpoints for dual SP control
        if (state.dataPlnt->PlantLoop(LoopNum).LoopDemandCalcScheme == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
            LoopSetPointTempHi = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointHi;
            LoopSetPointTempLo = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointLo;
            LoopSetPointTempHi = min(LoopMaxTemp, LoopSetPointTempHi);
            LoopSetPointTempHi = max(LoopMinTemp, LoopSetPointTempHi);
            LoopSetPointTempLo = min(LoopMaxTemp, LoopSetPointTempLo);
            LoopSetPointTempLo = max(LoopMinTemp, LoopSetPointTempLo);
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TempSetPointHi = LoopSetPointTempHi;
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TempSetPointLo = LoopSetPointTempLo;
        }

        // update demand side loop setpoint in plant data structure
        if (state.dataPlnt->PlantLoop(LoopNum).CommonPipeType == DataPlant::iCommonPipeType::TwoWay) { // get a second setpoint for secondaryLoop
            // if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
            // we want to initialize the demand side of the loop using that setpoint
            if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
                SecondaryLoopSetPointTemp = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn).TempSetPoint;
                SecondaryLoopSetPointTemp = min(LoopMaxTemp, SecondaryLoopSetPointTemp);
                SecondaryLoopSetPointTemp = max(LoopMinTemp, SecondaryLoopSetPointTemp);
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPoint = SecondaryLoopSetPointTemp;
                // Since Dual setpoint not explicitly available for demand side, we can't do the
                // bounding check on hi/lo setpoint.  IF we did we would over-write
                // the SensedNodeFlagValue of -999 for no dual setpoint case.
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPointHi =
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn).TempSetPointHi;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPointLo =
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn).TempSetPointLo;
            }

            // initialize common pipe flows to zero.
            if (allocated(state.dataHVACInterfaceMgr->PlantCommonPipe)) {
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).PriToSecFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).SecToPriFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).PriCPLegFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).SecCPLegFlow = 0.0;
            }
        } else { // no secondary loop, so use supply side loop SP on demand side too.
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPoint = LoopSetPointTemp;
            if (state.dataPlnt->PlantLoop(LoopNum).LoopDemandCalcScheme == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPointHi = LoopSetPointTempHi;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TempSetPointLo = LoopSetPointTempLo;
            }
        }

        for (LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum) {
            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    ComponentInlet = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                    ComponentOutlet = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).NodeNumOut;

                    // reinit to node hardware limits
                    state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMinAvail = state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMin;
                    state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMinAvail = state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMin;
                    state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMaxAvail = state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMax;
                    state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateMaxAvail = state.dataLoopNodes->Node(ComponentInlet).MassFlowRateMax;

                    state.dataLoopNodes->Node(ComponentInlet).MassFlowRateRequest = 0.0;
                    state.dataLoopNodes->Node(ComponentOutlet).MassFlowRateRequest = 0.0;
                }
            }
        }

        for (OpNum = 1; OpNum <= state.dataPlnt->PlantLoop(LoopNum).NumOpSchemes; ++OpNum) {
            // If the operating scheme is scheduled "OFF", go to next scheme
            state.dataPlnt->PlantLoop(LoopNum).OpScheme(OpNum).Available =
                GetCurrentScheduleValue(state, state.dataPlnt->PlantLoop(LoopNum).OpScheme(OpNum).SchedPtr) > 0.0;
        }
    }
}

void UpdateNodeThermalHistory(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010

    // PURPOSE OF THIS SUBROUTINE:
    // update temperature history for plant capacitance model and other

    // METHODOLOGY EMPLOYED:
    // copy current values into "LastTimestep" values

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // na

    // array assignment
    if (state.dataLoopNodes->NumOfNodes > 0) {
        for (auto &e : state.dataLoopNodes->Node) { // MA
            e.TempLastTimestep = e.Temp;
            e.EnthalpyLastTimestep = e.Enthalpy;
        }
    }
    if (state.dataPlnt->TotNumLoops > 0 && !state.dataGlobal->WarmupFlag) {
        for (auto &loop : state.dataPlnt->PlantLoop) {
            for (auto &side : loop.LoopSide) {
                if (loop.OutletNodeFlowrate > DataHVACGlobals::SmallMassFlow) {
                    // Accumulate total time loop is active
                    side.LoopSideInlet_TotalTime += state.dataHVACGlobal->TimeStepSys;
                    // Determine excessive storage - if both are moving in the same direction and McpDTdt is larger than MdotCpDeltaT
                    if ((abs(side.LoopSideInlet_MdotCpDeltaT) > DataHVACGlobals::SmallLoad) &&
                        ((side.LoopSideInlet_McpDTdt / side.LoopSideInlet_MdotCpDeltaT) > 1.1)) {
                        side.LoopSideInlet_CapExcessStorageTimeReport = state.dataHVACGlobal->TimeStepSys;
                        side.LoopSideInlet_CapExcessStorageTime += state.dataHVACGlobal->TimeStepSys;
                    } else {
                        side.LoopSideInlet_CapExcessStorageTimeReport = 0;
                    }
                } else {
                    side.LoopSideInlet_CapExcessStorageTimeReport = 0;
                }
            }
        }
    }
}

void CheckPlantOnAbort(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Septemeber 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Called once E+ is in the process of aborting because of fatal error
    //  check for plant input problems to help users find problems in input files

    // METHODOLOGY EMPLOYED:
    //  search plant data structures for issues that may help solve problems in input files
    //  1.   if loop side has a splitter/mixer and one branch in there is control type bypass,
    //       then another branch in the s/m needs to be active
    //  other checks could/should be added!

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum;           // DO loop counter for loops
    bool ActiveCntrlfound; // used to search for active control branches in parallel with bypass branches
    int ParalBranchNum;    // used to search for active control branches in parallel with bypass branches
    int ParalBranchNum2;   // used to search for active control branches in parallel with bypass branches
    int BranchNum2;        // used to search for active control branches in parallel with bypass branches
    int SideNum;
    int numLoopSides;
    int BranchNum; // DO loop counter for branches
    int CompNum;   // do loop for multiple components on a branch
    bool ShouldBeACTIVE;

    if (!(state.dataErrTracking->AskForPlantCheckOnAbort)) {
        return;
    }

    if (state.dataPlnt->TotNumLoops <= 0) return;
    if (!(allocated(state.dataPlnt->PlantLoop))) return;

    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        numLoopSides = 2;
        for (SideNum = 1; SideNum <= numLoopSides; ++SideNum) {
            if (!(state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Splitter.Exists)) continue;

            for (ParalBranchNum = 1; ParalBranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Splitter.TotalOutletNodes;
                 ++ParalBranchNum) {
                BranchNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Splitter.BranchNumOut(ParalBranchNum);
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).IsBypass) { // we know there is a bypass
                    // check that there is at least one 'Active' control type in parallel with bypass branch
                    ActiveCntrlfound = false;
                    for (ParalBranchNum2 = 1; ParalBranchNum2 <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Splitter.TotalOutletNodes;
                         ++ParalBranchNum2) {
                        BranchNum2 = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Splitter.BranchNumOut(ParalBranchNum2);
                        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum2).ControlType ==
                            DataBranchAirLoopPlant::ControlTypeEnum::Active) {
                            ActiveCntrlfound = true;
                        }
                    }
                    if (!(ActiveCntrlfound)) {
                        ShowWarningError(state,
                                         "Check control types on branches between splitter and mixer in PlantLoop=" +
                                             state.dataPlnt->PlantLoop(LoopNum).Name);
                        ShowContinueError(state, "Found a BYPASS branch with no ACTIVE branch in parallel with it");
                        ShowContinueError(state, "In certain (but not all) situations, this can cause problems; please verify your inputs");
                        ShowContinueError(state,
                                          "Bypass branch named: " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).Name);
                    }
                } // bypass present

                // check for possible components on demand side that should be ACTIVE but are not
                if (SideNum == DemandSide) {
                    // check for presences of the following components whose branch control type should be active
                    // WATER HEATER:MIXED
                    // WATER HEATER:STRATIFIED
                    // WATER USE CONNECTIONS
                    // COIL:WATER:COOLING
                    // COIL:WATER:SIMPLEHEATING
                    // COIL:STEAM:AIRHEATING
                    // SOLAR COLLECTOR:FLAT PLATE
                    // PLANT LOAD PROFILE
                    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                        ShouldBeACTIVE = false;
                        {
                            auto const SELECT_CASE_var(
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).Comp(CompNum).TypeOf_Num);

                            if (SELECT_CASE_var == TypeOf_WtrHeaterMixed) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_WtrHeaterStratified) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_WaterUseConnection) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_CoilWaterCooling) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_CoilWaterDetailedFlatCooling) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_CoilWaterSimpleHeating) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_CoilSteamAirHeating) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_SolarCollectorFlatPlate) {
                                ShouldBeACTIVE = true;
                            } else if (SELECT_CASE_var == TypeOf_PlantLoadProfile) {
                                ShouldBeACTIVE = true;
                            } else {
                                // not a demand side component that we know needs to be active, do nothing
                            }
                        }

                        if (ShouldBeACTIVE) {
                            {
                                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).ControlType);

                                if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Unknown) {
                                    ShowWarningError(state,
                                                     "Found potential problem with Control Type for Branch named: " +
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).Name);
                                    ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type unknown");
                                } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Active) {
                                    // do nothing, this is correct control type.
                                } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Passive) {
                                    ShowWarningError(state,
                                                     "Found potential problem with Control Type for Branch named: " +
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).Name);
                                    ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type PASSIVE");
                                } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                                    // do nothing, should be okay. (? don't really understand SeriesActive though)
                                } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Bypass) {
                                    ShowWarningError(state,
                                                     "Found potential problem with Control Type for Branch named: " +
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).Branch(BranchNum).Name);
                                    ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type Bypass");
                                }
                            }
                        } // should be active
                    }     // comp num loop
                }         // demand side

            } // splitter outlet nodes

            // check to see if bypass exists in demand side. If not warn error of possible flow problems
            if (!state.dataPlnt->PlantLoop(LoopNum).LoopSide(SideNum).BypassExists) {
                if (SideNum == DemandSide) {
                    ShowWarningError(state,
                                     "There is no BYPASS component in the demand-side of PlantLoop =" + state.dataPlnt->PlantLoop(LoopNum).Name);
                    ShowContinueError(state, "You may be able to fix the fatal error above by adding a demand-side BYPASS PIPE.");
                }
            }
        } // loop sides
    }     // plant loops
}

void InitOneTimePlantSizingInfo(EnergyPlusData &state, int const LoopNum) // loop being initialized for sizing
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // one time init what can be set up related to plant sizing data structure.

    // Using/Aliasing
    using DataSizing::PlantSizingData;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlantSizNum(0); // index of Plant Sizing data for this loop

    if (state.dataPlnt->PlantLoop(LoopNum).PlantSizNum == 0) {
        if (state.dataSize->NumPltSizInput > 0) {
            PlantSizNum = UtilityRoutines::FindItemInList(
                state.dataPlnt->PlantLoop(LoopNum).Name, state.dataSize->PlantSizData, &PlantSizingData::PlantLoopName);
            if (PlantSizNum > 0) {
                state.dataPlnt->PlantLoop(LoopNum).PlantSizNum = PlantSizNum;
            }
        }
    }
}

void SizePlantLoop(EnergyPlusData &state,
                   int const LoopNum, // Supply side loop being simulated
                   bool const OkayToFinish)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing the supply side of Plant Loops for which loop flow rates
    // have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains volumetric flow rate data from the PlantSizData array..

    // Using/Aliasing
    using namespace DataSizing;
    using FluidProperties::GetDensityGlycol;
    ;

    // Locals
    bool localInitLoopEquip(true);

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizePlantLoop");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlantSizNum(0);      // index of Plant Sizing data for this loop
    int BranchNum;           // DO loop counter for cycling through branches on a demand side loop
    int CompNum;             // DO loop counter for cycling through components on a demand side loop
    int SupNodeNum;          // component inlet water node number
    int WaterCompNum;        // DO loop counter for cycling through all the components that demand water
    bool ErrorsFound(false); // If errors detected in input
    Real64 LoopSizFac(0.0);
    Real64 AvLoopSizFac;
    Real64 PlantSizFac(1.0);
    Real64 MaxSizFac(0.0);
    Real64 BranchSizFac;
    Real64 NumBrSizFac(0.0);
    Real64 FluidDensity(0.0); // local value from glycol routine
    bool Finalize(OkayToFinish);

    if (state.dataPlnt->PlantLoop(LoopNum).PlantSizNum > 0) {
        PlantSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
        // PlantSizData(PlantSizNum)%DesVolFlowRate = 0.0D0 ! DSU2
    } else {
        if (state.dataSize->NumPltSizInput > 0) {
            PlantSizNum = UtilityRoutines::FindItemInList(
                state.dataPlnt->PlantLoop(LoopNum).Name, state.dataSize->PlantSizData, &PlantSizingData::PlantLoopName);
        }
    }
    state.dataPlnt->PlantLoop(LoopNum).PlantSizNum = PlantSizNum;
    // calculate a loop sizing factor and a branch sizing factor. Note that components without a sizing factor
    // are assigned sizing factors of zero in this calculation
    if (PlantSizNum > 0) {
        if (state.dataPlantMgr->GetCompSizFac) {
            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TotalBranches; ++BranchNum) {
                BranchSizFac = 0.0;
                state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).PumpSizFac = 1.0;
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn ==
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumIn)
                    continue;
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumOut ==
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumOut)
                    continue;
                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).TotalComponents; ++CompNum) {
                    state.dataPlnt->PlantLoop(LoopNum)
                        .LoopSide(SupplySide)
                        .Branch(BranchNum)
                        .Comp(CompNum)
                        .simulate(state, true, localInitLoopEquip, state.dataPlantMgr->GetCompSizFac);
                    BranchSizFac = max(BranchSizFac, state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).Comp(CompNum).SizFac);
                }
                LoopSizFac += BranchSizFac;
                MaxSizFac = max(MaxSizFac, BranchSizFac);
                if (BranchSizFac > 0.0) {
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).PumpSizFac = BranchSizFac;
                    ++NumBrSizFac;
                }
            }
            AvLoopSizFac = LoopSizFac / max(1.0, NumBrSizFac);

            if (AvLoopSizFac > 0.0 && AvLoopSizFac < 1.0) {
                PlantSizFac = LoopSizFac;
            } else if (AvLoopSizFac > 1.0) {
                PlantSizFac = MaxSizFac;
            } else {
                PlantSizFac = 1.0;
            }
            // store the sizing factor now, for later reuse,
            state.dataSize->PlantSizData(PlantSizNum).PlantSizFac = PlantSizFac;
            // might deprecate this next bit in favor of simpler storage in PlantSizData structure
            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TotalBranches; ++BranchNum) {
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn ==
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumIn) {
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).PumpSizFac = PlantSizFac;
                }
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumOut ==
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumOut) {
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).PumpSizFac = PlantSizFac;
                }
            }

        } else {
            // fill PlantSizFac from data structure
            //                    for (BranchNum = 1;
            //                         BranchNum <= PlantLoop(LoopNum).LoopSide(SupplySide).TotalBranches; ++BranchNum) {
            //                        if (PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn ==
            //                            PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumIn) {
            //                            break;
            //                        }
            //                    }
        }

        // sum up contributions from CompDesWaterFlow, demand side size request (non-coincident)
        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate = 0.0; // init for summation
        for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TotalBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum).TotalComponents; ++CompNum) {
                SupNodeNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                for (WaterCompNum = 1; WaterCompNum <= state.dataSize->SaveNumPlantComps; ++WaterCompNum) {
                    if (SupNodeNum == state.dataSize->CompDesWaterFlow(WaterCompNum).SupNode) {
                        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate += state.dataSize->CompDesWaterFlow(WaterCompNum).DesVolFlowRate;
                    }
                }
            }
        }

        if (!state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRateWasAutoSized && (state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate > 0.0)) {
            // if the user puts in a large throwaway value for hard max plant loop size, they may not want this affecting anything else.
            //  but if they put in a smaller value, then it should cap the design size, so use hard value if it is smaller than non-coincident
            //  result
            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate =
                std::min(state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate, state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
        }
    }

    if (state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRateWasAutoSized) {

        if ((PlantSizNum > 0)) {

            if (state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate =
                    state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * state.dataSize->PlantSizData(PlantSizNum).PlantSizFac;
            } else {
                state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate = 0.0;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    ShowWarningError(state,
                                     format("SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=[{:.2R}] is too small. Set to 0.0",
                                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate));
                    ShowContinueError(state, "..occurs for PlantLoop=" + state.dataPlnt->PlantLoop(LoopNum).Name);
                }
            }
            if (Finalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
                        BaseSizer::reportSizerOutput(state,
                                                     "PlantLoop",
                                                     state.dataPlnt->PlantLoop(LoopNum).Name,
                                                     "Maximum Loop Flow Rate [m3/s]",
                                                     state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                    } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
                        BaseSizer::reportSizerOutput(state,
                                                     "CondenserLoop",
                                                     state.dataPlnt->PlantLoop(LoopNum).Name,
                                                     "Maximum Loop Flow Rate [m3/s]",
                                                     state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                    }
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
                        BaseSizer::reportSizerOutput(state,
                                                     "PlantLoop",
                                                     state.dataPlnt->PlantLoop(LoopNum).Name,
                                                     "Initial Maximum Loop Flow Rate [m3/s]",
                                                     state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                    } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
                        BaseSizer::reportSizerOutput(state,
                                                     "CondenserLoop",
                                                     state.dataPlnt->PlantLoop(LoopNum).Name,
                                                     "Initial Maximum Loop Flow Rate [m3/s]",
                                                     state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                    }
                }
            }

        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowFatalError(state, "Autosizing of plant loop requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in PlantLoop object=" + state.dataPlnt->PlantLoop(LoopNum).Name);
                ErrorsFound = true;
            }
        }
    }

    // Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
    if (state.dataPlnt->PlantLoop(LoopNum).VolumeWasAutoSized) {
        // There is no stability requirement (mass can be zero), autosizing is based on loop circulation time.
        // Note this calculation also appears in PlantManager::ResizePlantLoopLevelSizes and SizingAnalysisObjects::ResolveDesignFlowRate
        state.dataPlnt->PlantLoop(LoopNum).Volume =
            state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * state.dataPlnt->PlantLoop(LoopNum).CirculationTime * 60.0;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
                // condenser loop vs plant loop breakout needed.
                BaseSizer::reportSizerOutput(
                    state, "PlantLoop", state.dataPlnt->PlantLoop(LoopNum).Name, "Plant Loop Volume [m3]", state.dataPlnt->PlantLoop(LoopNum).Volume);
            } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             state.dataPlnt->PlantLoop(LoopNum).Name,
                                             "Condenser Loop Volume [m3]",
                                             state.dataPlnt->PlantLoop(LoopNum).Volume);
            }
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
                // condenser loop vs plant loop breakout needed.
                BaseSizer::reportSizerOutput(state,
                                             "PlantLoop",
                                             state.dataPlnt->PlantLoop(LoopNum).Name,
                                             "Initial Plant Loop Volume [m3]",
                                             state.dataPlnt->PlantLoop(LoopNum).Volume);
            } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             state.dataPlnt->PlantLoop(LoopNum).Name,
                                             "Initial Condenser Loop Volume [m3]",
                                             state.dataPlnt->PlantLoop(LoopNum).Volume);
            }
        }
    }

    // should now have plant volume, calculate plant volume's mass for fluid type
    if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Water) {
        FluidDensity = GetDensityGlycol(state,
                                        state.dataPlnt->PlantLoop(LoopNum).FluidName,
                                        DataGlobalConstants::InitConvTemp,
                                        state.dataPlnt->PlantLoop(LoopNum).FluidIndex,
                                        RoutineName);
    } else if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Steam) {
        FluidDensity = GetSatDensityRefrig(state, fluidNameSteam, 100.0, 1.0, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
    } else {
        assert(false);
    }

    state.dataPlnt->PlantLoop(LoopNum).Mass = state.dataPlnt->PlantLoop(LoopNum).Volume * FluidDensity;

    state.dataPlnt->PlantLoop(LoopNum).MaxMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * FluidDensity;
    state.dataPlnt->PlantLoop(LoopNum).MinMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate * FluidDensity;

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void ResizePlantLoopLevelSizes(EnergyPlusData &state, int const LoopNum // Supply side loop being simulated
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jan 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for redon the sizing of plant loops to support HVAC Sizing Simulation

    // METHODOLOGY EMPLOYED:
    // Obtains volumetric flow rate data from the PlantSizData array..

    // Using/Aliasing
    using namespace DataSizing;
    using FluidProperties::GetDensityGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("ResizePlantLoop");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PlantSizNum(0);      // index of Plant Sizing data for this loop
    int BranchNum;           // DO loop counter for cycling through branches on a demand side loop
    int CompNum;             // DO loop counter for cycling through components on a demand side loop
    int SupNodeNum;          // component inlet water node number
    int WaterCompNum;        // DO loop counter for cycling through all the components that demand water
    bool ErrorsFound(false); // If errors detected in input

    Real64 FluidDensity(0.0); // local value from glycol routine

    Real64 PlantSizeFac = 0.0;

    PlantSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;

    // fill PlantSizFac from data structure
    for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TotalBranches; ++BranchNum) {
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn ==
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).NodeNumIn) {
            PlantSizeFac = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).Branch(BranchNum).PumpSizFac;
            break;
        }
    }
    if (state.dataSize->PlantSizData(PlantSizNum).ConcurrenceOption == NonCoincident) {
        // we can have plant loops that are non-coincident along with some that are coincident
        // so refresh sum of registered flows (they may have changed)

        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate = 0.0; // init for summation
        for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TotalBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum).TotalComponents; ++CompNum) {
                SupNodeNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                for (WaterCompNum = 1; WaterCompNum <= state.dataSize->SaveNumPlantComps; ++WaterCompNum) {
                    if (SupNodeNum == state.dataSize->CompDesWaterFlow(WaterCompNum).SupNode) {
                        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate += state.dataSize->CompDesWaterFlow(WaterCompNum).DesVolFlowRate;
                    }
                }
            }
        }
    }

    if (state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRateWasAutoSized) {

        if ((PlantSizNum > 0)) {

            if (state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate = state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * PlantSizeFac;
            } else {
                state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate = 0.0;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    ShowWarningError(state,
                                     format("SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=[{:.2R}] is too small. Set to 0.0",
                                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate));
                    ShowContinueError(state, "..occurs for PlantLoop=" + state.dataPlnt->PlantLoop(LoopNum).Name);
                }
            }
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
                    BaseSizer::reportSizerOutput(state,
                                                 "PlantLoop",
                                                 state.dataPlnt->PlantLoop(LoopNum).Name,
                                                 "Maximum Loop Flow Rate [m3/s]",
                                                 state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
                    BaseSizer::reportSizerOutput(state,
                                                 "CondenserLoop",
                                                 state.dataPlnt->PlantLoop(LoopNum).Name,
                                                 "Maximum Loop Flow Rate [m3/s]",
                                                 state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate);
                }
            }
        }
    }

    // Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
    if (state.dataPlnt->PlantLoop(LoopNum).VolumeWasAutoSized) {
        // There is no stability requirement (mass can be zero), autosizing is based on loop circulation time.
        // Note this calculation also appears in PlantManager::SizePlantLoop and SizingAnalysisObjects::ResolveDesignFlowRate
        state.dataPlnt->PlantLoop(LoopNum).Volume =
            state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * state.dataPlnt->PlantLoop(LoopNum).CirculationTime * 60.0;
        if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
            // condenser loop vs plant loop breakout needed.
            BaseSizer::reportSizerOutput(
                state, "PlantLoop", state.dataPlnt->PlantLoop(LoopNum).Name, "Plant Loop Volume [m3]", state.dataPlnt->PlantLoop(LoopNum).Volume);
        } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
            BaseSizer::reportSizerOutput(state,
                                         "CondenserLoop",
                                         state.dataPlnt->PlantLoop(LoopNum).Name,
                                         "Condenser Loop Volume [m3]",
                                         state.dataPlnt->PlantLoop(LoopNum).Volume);
        }
    }

    // should now have plant volume, calculate plant volume's mass for fluid type
    if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Water) {
        FluidDensity = GetDensityGlycol(state,
                                        state.dataPlnt->PlantLoop(LoopNum).FluidName,
                                        DataGlobalConstants::InitConvTemp,
                                        state.dataPlnt->PlantLoop(LoopNum).FluidIndex,
                                        RoutineName);
    } else if (state.dataPlnt->PlantLoop(LoopNum).FluidType == DataLoopNode::NodeFluidType::Steam) {
        FluidDensity = GetSatDensityRefrig(state, fluidNameSteam, 100.0, 1.0, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
    } else {
        assert(false);
    }

    state.dataPlnt->PlantLoop(LoopNum).Mass = state.dataPlnt->PlantLoop(LoopNum).Volume * FluidDensity;

    state.dataPlnt->PlantLoop(LoopNum).MaxMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate * FluidDensity;
    state.dataPlnt->PlantLoop(LoopNum).MinMassFlowRate = state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate * FluidDensity;

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void SetupInitialPlantCallingOrder(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Feb 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // setup the order that plant loops are to be called

    // METHODOLOGY EMPLOYED:
    // simple rule-based allocation of which order to call the half loops
    //  initially just mimicing historical practice until a better set of rules is
    // developed
    // 1.  first call all plant demand sides
    // 2.  second call all plant supply sides
    // 3.  third call all condenser demand sides
    // 4.  fourth call all condenser supply sides

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OrderIndex; // local
    int I;          // local loop

    state.dataPlnt->TotNumHalfLoops = 2 * state.dataPlnt->TotNumLoops;

    if (state.dataPlnt->TotNumHalfLoops <= 0) return;

    // first allocate to total number of plant half loops

    if (!allocated(state.dataPlnt->PlantCallingOrderInfo)) state.dataPlnt->PlantCallingOrderInfo.allocate(state.dataPlnt->TotNumHalfLoops);

    // set plant loop demand sides
    for (I = 1; I <= state.dataHVACGlobal->NumPlantLoops; ++I) {
        state.dataPlnt->PlantCallingOrderInfo(I).LoopIndex = I;
        state.dataPlnt->PlantCallingOrderInfo(I).LoopSide = DemandSide;
    }

    // set plant loop supply sides
    for (I = 1; I <= state.dataHVACGlobal->NumPlantLoops; ++I) {
        OrderIndex = I + state.dataHVACGlobal->NumPlantLoops;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = SupplySide;
    }

    // set condenser Loop demand sides
    for (I = 1; I <= state.dataHVACGlobal->NumCondLoops; ++I) {
        OrderIndex = 2 * state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = DemandSide;
    }

    // set condenser Loop supply sides
    for (I = 1; I <= state.dataHVACGlobal->NumCondLoops; ++I) {
        OrderIndex = 2 * state.dataHVACGlobal->NumPlantLoops + state.dataHVACGlobal->NumCondLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = SupplySide;
    }
}

void RevisePlantCallingOrder(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   april 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // setup the order that plant loops are to be called

    // METHODOLOGY EMPLOYED:
    // simple rule-based allocation of which order to call the half loops
    // Examine for interconnected components and rearrange to impose the following rules

    // Using/Aliasing
    using PlantUtilities::ShiftPlantLoopSideCallingOrder;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int HalfLoopNum;
    int LoopNum;
    int LoopSideNum;
    int OtherLoopNum;
    int OtherLoopSideNum;

    bool thisLoopPutsDemandOnAnother;
    int ConnctNum;

    for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {

        LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
        LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;

        if (allocated(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Connected)) {
            for (ConnctNum = 1; ConnctNum <= isize(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Connected); ++ConnctNum) {
                OtherLoopNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Connected(ConnctNum).LoopNum;
                OtherLoopSideNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Connected(ConnctNum).LoopSideNum;
                state.dataPlantMgr->OtherLoopCallingIndex = FindLoopSideInCallingOrder(state, OtherLoopNum, OtherLoopSideNum);

                thisLoopPutsDemandOnAnother = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Connected(ConnctNum).LoopDemandsOnRemote;
                if (thisLoopPutsDemandOnAnother) {                                 // make sure this loop side is called before the other loop side
                    if (state.dataPlantMgr->OtherLoopCallingIndex < HalfLoopNum) { // rearrange
                        state.dataPlantMgr->newCallingIndex = min(HalfLoopNum + 1, state.dataPlnt->TotNumHalfLoops);
                        ShiftPlantLoopSideCallingOrder(state, state.dataPlantMgr->OtherLoopCallingIndex, state.dataPlantMgr->newCallingIndex);
                    }

                } else {                                                           // make sure the other is called before this one
                    if (state.dataPlantMgr->OtherLoopCallingIndex > HalfLoopNum) { // rearrange
                        state.dataPlantMgr->newCallingIndex = max(HalfLoopNum, 1);

                        if (OtherLoopSideNum == SupplySide) { // if this is a supplyside, don't push it before its own demand side
                            state.dataPlantMgr->OtherLoopDemandSideCallingIndex = FindLoopSideInCallingOrder(state, OtherLoopNum, DemandSide);
                            if (state.dataPlantMgr->OtherLoopDemandSideCallingIndex < HalfLoopNum) { // good to go
                                state.dataPlantMgr->newCallingIndex = min(state.dataPlantMgr->OtherLoopDemandSideCallingIndex + 1,
                                                                          state.dataPlnt->TotNumHalfLoops); // put it right after its demand side
                                ShiftPlantLoopSideCallingOrder(state, state.dataPlantMgr->OtherLoopCallingIndex, state.dataPlantMgr->newCallingIndex);
                            } else { // move both sides of other loop before this, keeping demand side in front
                                state.dataPlantMgr->NewOtherDemandSideCallingIndex = max(HalfLoopNum, 1);
                                ShiftPlantLoopSideCallingOrder(
                                    state, state.dataPlantMgr->OtherLoopDemandSideCallingIndex, state.dataPlantMgr->NewOtherDemandSideCallingIndex);
                                // get fresh pointer after it has changed in previous call
                                state.dataPlantMgr->OtherLoopCallingIndex = FindLoopSideInCallingOrder(state, OtherLoopNum, OtherLoopSideNum);
                                state.dataPlantMgr->newCallingIndex = state.dataPlantMgr->NewOtherDemandSideCallingIndex + 1;
                                ShiftPlantLoopSideCallingOrder(state, state.dataPlantMgr->OtherLoopCallingIndex, state.dataPlantMgr->newCallingIndex);
                            }
                        } else {
                            ShiftPlantLoopSideCallingOrder(state, state.dataPlantMgr->OtherLoopCallingIndex, state.dataPlantMgr->newCallingIndex);
                        }
                    }
                }
            }
        }
    }
}

int FindLoopSideInCallingOrder(EnergyPlusData &state, int const LoopNum, int const LoopSide)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // locate loop and loop side in calling order structure

    // METHODOLOGY EMPLOYED:
    // returns integer "pointer" index to calling order structure

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int CallingIndex;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int HalfLoopNum;

    CallingIndex = 0;

    for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {
        if ((LoopNum == state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex) &&
            (LoopSide == state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide)) {

            CallingIndex = HalfLoopNum;
        }
    }
    return CallingIndex;
}

void SetupBranchControlTypes(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   March 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // set the control types on plant branches using heuristics.
    //  Trying to obsolete branch control type  input

    // METHODOLOGY EMPLOYED:
    // set component control types based on component type
    //  process branches and set branch level control types based on the type of components on them
    //  Rules applied
    //   - Most component models are active
    //   - Pipes are passive unless located between splitter/mixers when assumed to be bypass
    //   - A branch with multiple active components becomes SeriesActive and so do its components

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopCtr;
    int LoopSideCtr;
    int BranchCtr;
    int CompCtr;
    bool BranchIsInSplitterMixer;
    DataBranchAirLoopPlant::ControlTypeEnum ComponentFlowCtrl;
    int ActiveCount;
    int BypassCount;
    int NumComponentsOnBranch;
    int NumCount;

    // first set component level control type (obsoletes one input in field set for Branch )
    if (allocated(state.dataPlnt->PlantLoop)) {
        NumCount = size(state.dataPlnt->PlantLoop);
    } else {
        NumCount = 0;
    }
    for (LoopCtr = 1; LoopCtr <= NumCount; ++LoopCtr) {
        for (LoopSideCtr = DemandSide; LoopSideCtr <= SupplySide; ++LoopSideCtr) {
            for (BranchCtr = 1; BranchCtr <= state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).TotalBranches; ++BranchCtr) {
                BranchIsInSplitterMixer = false;
                // test if this branch is inside a splitter/mixer
                if (state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Splitter.Exists) {
                    if ((BranchCtr > 1) && (BranchCtr < state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).TotalBranches)) {
                        BranchIsInSplitterMixer = true;
                    }
                }

                NumComponentsOnBranch = state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).TotalComponents;

                for (CompCtr = 1; CompCtr <= isize(state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Comp); ++CompCtr) {

                    auto &this_component(state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Comp(CompCtr));

                    {
                        auto const SELECT_CASE_var(this_component.TypeOf_Num);

                        if (SELECT_CASE_var == TypeOf_Other) { //                             = -1
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Unknown;
                            this_component.FlowPriority = LoopFlowStatus_Unknown;
                            this_component.HowLoadServed = HowMet_Unknown;
                        } else if (SELECT_CASE_var == TypeOf_Boiler_Simple) { //         =  1
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCapHiOutLimit;
                        } else if (SELECT_CASE_var == TypeOf_Boiler_Steam) { //                      =  2
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Chiller_Absorption) { // = 3 ! older BLAST absorption chiller
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_Indirect_Absorption) { // = 4 ! revised absorption chiller
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_CombTurbine) { //           =  5
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_ConstCOP) { //                 =  6
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;

                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_DFAbsorption) { //             =  7
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_ExhFiredAbsorption) { //             =  76
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_Electric) { //                 =  8
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_ElectricEIR) { //              =  9
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR) { //        = 10
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Chiller_EngineDriven) { //             = 11
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                            }
                        } else if (SELECT_CASE_var == TypeOf_CoolingTower_SingleSpd) { //           = 12
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_CoolingTower_TwoSpd) { //              = 13
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_CoolingTower_VarSpd) { //              = 14
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_CoolingTower_VarSpdMerkel) { //              = 89
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_FCExhaust) { //              = 15
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;

                        } else if (SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterPumped ||
                                   SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterWrapped) { //                = 16, 92
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_HPWaterEFCooling) { //                 = 17
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }

                        } else if (SELECT_CASE_var == TypeOf_HPWaterEFHeating) { //                 = 18
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_HPWaterPECooling) { //                 = 19
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_HPWaterPEHeating) { //                 = 20
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_Pipe) { //                             = 21
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                            if (BranchIsInSplitterMixer) {
                                if (NumComponentsOnBranch == 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                } else if (NumComponentsOnBranch > 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                } else {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                }
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PipeSteam) { //                        = 22
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                            if (BranchIsInSplitterMixer) {
                                if (NumComponentsOnBranch == 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                } else if (NumComponentsOnBranch > 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                } else {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                }
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PipeExterior) { //                     = 23
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                            if (BranchIsInSplitterMixer) {
                                if (NumComponentsOnBranch == 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                } else if (NumComponentsOnBranch > 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                } else {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                }
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PipeInterior) { //                     = 24
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                            if (BranchIsInSplitterMixer) {
                                if (NumComponentsOnBranch == 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                } else if (NumComponentsOnBranch > 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                } else {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                }
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PipeUnderground) { //                  = 25
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                            if (BranchIsInSplitterMixer) {
                                if (NumComponentsOnBranch == 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                } else if (NumComponentsOnBranch > 1) {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                } else {
                                    this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                                }
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PurchChilledWater) { //                = 26
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                        } else if (SELECT_CASE_var == TypeOf_PurchHotWater) { //                    = 27
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCapHiOutLimit;
                        } else if (SELECT_CASE_var == TypeOf_TS_IceDetailed) { //                   = 28
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_TS_IceSimple) { //                    = 29
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_ValveTempering) { //                  = 30
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_WtrHeaterMixed) { //                   = 31
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_WtrHeaterStratified) { //              = 32
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PumpVariableSpeed) { //                 = 33
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_PumpConstantSpeed) { //                 = 34
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_PumpCondensate) { //                    = 35
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_PumpBankVariableSpeed) { //             = 36
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_PumpBankConstantSpeed) { //             = 37
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_WaterUseConnection) { //              = 38
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWaterCooling) { //               = 39  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWaterDetailedFlatCooling) { //      = 40  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWaterSimpleHeating) { //           = 41  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilSteamAirHeating) { //         = 42  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_SolarCollectorFlatPlate) { //         = 43  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_PlantLoadProfile) { //            = 44  ! demand side component
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_GrndHtExchgSystem) { //            = 45
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_GrndHtExchgSurface) { //            = 46
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_GrndHtExchgPond) { //            = 47
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_MicroTurbine) { //          = 48  !newer FSEC turbine
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_ICEngine) { //             = 49
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_CTurbine) { //             = 50  !older BLAST turbine
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_MicroCHP) { //              = 51
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_Generator_FCStackCooler) { //         = 52
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_ByNominalCap;
                        } else if (SELECT_CASE_var == TypeOf_FluidCooler_SingleSpd) { //           = 53
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_FluidCooler_TwoSpd) { //            = 54
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_EvapFluidCooler_SingleSpd) { //       = 55
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_EvapFluidCooler_TwoSpd) { //         = 56
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_ChilledWaterTankMixed) { //         = 57
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_ChilledWaterTankStratified) { //      = 58
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PVTSolarCollectorFlatPlate) { //      = 59
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                            // next batch for ZoneHVAC
                        } else if (SELECT_CASE_var == TypeOf_Baseboard_Conv_Water) { //        = 60
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Steam) { //      = 61
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Water) { //      = 62
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoolingPanel_Simple) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_LowTempRadiant_VarFlow) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_LowTempRadiant_ConstFlow) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CooledBeamAirTerminal) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_FourPipeBeamAirTerminal) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWAHPHeatingEquationFit) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWAHPCoolingEquationFit) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilVSWAHPHeatingEquationFit) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilVSWAHPCoolingEquationFit) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWAHPHeatingParamEst) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_CoilWAHPCoolingParamEst) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_MultiSpeedHeatPumpRecovery) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_UnitarySysRecovery) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_PipingSystemPipeCircuit) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_SolarCollectorICS) { //         = 75
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_PlantComponentUserDefined) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_Unknown;
                            this_component.HowLoadServed = HowMet_Unknown;
                        } else if (SELECT_CASE_var == TypeOf_CoilUserDefined) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_Unknown;
                            this_component.HowLoadServed = HowMet_Unknown;
                        } else if (SELECT_CASE_var == TypeOf_ZoneHVACAirUserDefined) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_Unknown;
                            this_component.HowLoadServed = HowMet_Unknown;
                        } else if (SELECT_CASE_var == TypeOf_AirTerminalUserDefined) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_Unknown;
                            this_component.HowLoadServed = HowMet_Unknown;
                        } else if (SELECT_CASE_var == TypeOf_HeatPumpVRF) { //       =  82  ! AirConditioner:VariableRefrigerantFlow
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;

                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else { // should never happen
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_WaterSource) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
                        } else if (SELECT_CASE_var == TypeOf_GrndHtExchgHorizTrench) { // = 83  GroundHeatExchanger:HorizontalTrench
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_FluidToFluidPlantHtExchg) { //          = 84
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                                this_component.HowLoadServed = HowMet_PassiveCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_CentralGroundSourceHeatPump) { // 86
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else if (SELECT_CASE_var == TypeOf_PackagedTESCoolingCoil) { // 88
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_SwimmingPool_Indoor) { // 90
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = HowMet_NoneDemand;
                        } else if (SELECT_CASE_var == TypeOf_GrndHtExchgSlinky) { //            = 91
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
                            this_component.HowLoadServed = HowMet_PassiveCap;
                        } else if (SELECT_CASE_var == TypeOf_HeatPumpEIRCooling || SELECT_CASE_var == TypeOf_HeatPumpEIRHeating) { // 95, 96
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            if (LoopSideCtr == DemandSide) {
                                this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
                                this_component.HowLoadServed = HowMet_NoneDemand;
                            } else {
                                this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
                                this_component.HowLoadServed = HowMet_ByNominalCap;
                            }
                        } else {
                            ShowSevereError(state, "SetBranchControlTypes: Caught unexpected equipment type of number");
                        }
                    }
                }
            }
        }
    }

    // now set up branch control types based on components.

    if (allocated(state.dataPlnt->PlantLoop)) {
        NumCount = size(state.dataPlnt->PlantLoop);
    } else {
        NumCount = 0;
    }
    for (LoopCtr = 1; LoopCtr <= NumCount; ++LoopCtr) { // SIZE(PlantLoop)
        for (LoopSideCtr = DemandSide; LoopSideCtr <= SupplySide; ++LoopSideCtr) {
            for (BranchCtr = 1; BranchCtr <= state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).TotalBranches; ++BranchCtr) {
                ActiveCount = 0;
                BypassCount = 0;
                for (CompCtr = 1; CompCtr <= isize(state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Comp); ++CompCtr) {
                    ComponentFlowCtrl = state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Comp(CompCtr).FlowCtrl;

                    {
                        auto const SELECT_CASE_var(ComponentFlowCtrl);

                        if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Unknown) {
                            state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).ControlType =
                                DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                        } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Active) {
                            ++ActiveCount;
                            if (ActiveCount > 1) {
                                //  assume multiple active components in series means branch is SeriesActive
                                state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).ControlType =
                                    DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive;
                                // assume all components on branch are to be SeriesActive as well
                                for (auto &e : state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Comp)
                                    e.FlowCtrl = DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive;
                            } else {
                                state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).ControlType =
                                    DataBranchAirLoopPlant::ControlTypeEnum::Active;
                            }

                            if (BypassCount > 0) {
                                ShowSevereError(state, "An active component is on the same branch as a pipe situated between splitter/mixer");
                                ShowContinueError(
                                    state, "Occurs in Branch=" + state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Name);
                                ShowContinueError(state, "Occurs in Plant Loop=" + state.dataPlnt->PlantLoop(LoopCtr).Name);
                                ShowContinueError(state, "SetupBranchControlTypes: and the simulation continues");
                                //  note not sure why this is so bad.  heat transfer pipe might be a good reason to allow this?
                                //   this used to fatal in older PlantFlowResolver.
                            }

                            // test for active component in series with bypass
                        } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Bypass) {

                            ++BypassCount;
                            state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).ControlType =
                                DataBranchAirLoopPlant::ControlTypeEnum::Bypass;
                            state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).IsBypass = true;
                            state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).BypassExists = true;

                            if (CompCtr > 1) {
                                ShowSevereError(state, "A pipe used as a bypass should not be in series with another component");
                                ShowContinueError(
                                    state, "Occurs in Branch = " + state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).Name);
                                ShowContinueError(state, "Occurs in PlantLoop = " + state.dataPlnt->PlantLoop(LoopCtr).Name);
                                ShowFatalError(state, "SetupBranchControlTypes: preceding condition causes termination.");
                            }

                        } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::Passive) {
                            if (ActiveCount > 0) {
                                // do nothing, branch set before)
                            } else {
                                if (BypassCount > 0) {

                                } else {
                                    state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).Branch(BranchCtr).ControlType =
                                        DataBranchAirLoopPlant::ControlTypeEnum::Passive;
                                }
                            }
                        } else if (SELECT_CASE_var == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                            // do nothing, already set when more than one active component found on a branch
                        }
                    }
                }
            }
        }
    }
}

void CheckIfAnyPlant(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // determine if any plant loops will be ever be set up

    // METHODOLOGY EMPLOYED:
    // use input processor ot find number of plant loops

    // Using/Aliasing
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int numPlantLoopsCheck;
    int numCondenserLoopsCheck;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "PlantLoop";
    numPlantLoopsCheck = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    cCurrentModuleObject = "CondenserLoop";
    numCondenserLoopsCheck = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if ((numPlantLoopsCheck + numCondenserLoopsCheck) > 0) {
        state.dataGlobal->AnyPlantInModel = true;
    } else {
        state.dataGlobal->AnyPlantInModel = false;
        state.dataPlnt->PlantLoop.allocate(0);
    }
}

void CheckOngoingPlantWarnings(EnergyPlusData &state)
{
    int LoopNum;
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        // Warning if the excess storage time is more than half of the total time
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_CapExcessStorageTime >
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TotalTime / 2) {
            ShowWarningError(
                state, "Plant Loop: " + state.dataPlnt->PlantLoop(LoopNum).Name + " Demand Side is storing excess heat the majority of the time.");
            ShowContinueError(state,
                              format("Excesss Storage Time={:.2R}[hr], Total Loop Active Time={:.2R}[hr]",
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_CapExcessStorageTime,
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TotalTime));
        }
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_CapExcessStorageTime >
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_TotalTime / 2) {
            ShowWarningError(
                state, "Plant Loop: " + state.dataPlnt->PlantLoop(LoopNum).Name + " Supply Side is storing excess heat the majority of the time.");
            ShowContinueError(state,
                              format("Excesss Storage Time={:.2R}[hr], Total Loop Active Time={:.2R}[hr]",
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_CapExcessStorageTime,
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TotalTime));
        }
    }
}

} // namespace EnergyPlus::PlantManager
