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
#include <EnergyPlus/ChillerElectricASHRAE205.hh>
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
#include <EnergyPlus/OutputReportPredefined.hh>
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
#include <EnergyPlus/SwimmingPool.hh>
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
using namespace DataPlant;
using namespace DataBranchAirLoopPlant;
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
    DataPlant::LoopSideLocation LoopSide;
    DataPlant::LoopSideLocation OtherSide;
    bool SimHalfLoopFlag;
    int HalfLoopNum;
    int CurntMinPlantSubIterations;

    if (std::any_of(state.dataPlnt->PlantLoop.begin(), state.dataPlnt->PlantLoop.end(), [](DataPlant::PlantLoopData const &e) {
            return (e.CommonPipeType == DataPlant::CommonPipeType::Single) || (e.CommonPipeType == DataPlant::CommonPipeType::TwoWay);
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
            OtherSide = LoopSideOther[static_cast<int>(LoopSide)]; // will give us 1 if LoopSide is 2, or 2 if LoopSide is 1

            auto &this_loop(state.dataPlnt->PlantLoop(LoopNum));
            auto &this_loop_side(this_loop.LoopSide(LoopSide));
            auto &other_loop_side(this_loop.LoopSide(OtherSide));

            SimHalfLoopFlag = this_loop_side.SimLoopSideNeeded; // set half loop sim flag

            if (SimHalfLoopFlag || IterPlant <= CurntMinPlantSubIterations) {

                this_loop_side.solve(state, FirstHVACIteration, other_loop_side.SimLoopSideNeeded);

                // Always set this side to false,  so that it won't keep being turned on just because of first hvac
                this_loop_side.SimLoopSideNeeded = false;

                // If we did the demand side, turn on the supply side (only if we need to do it last)
                if (LoopSide == LoopSideLocation::Demand) {
                    if (this_loop.HasPressureComponents) {
                        other_loop_side.SimLoopSideNeeded = false;
                    }
                }

                // Update the report variable
                this_loop.LastLoopSideSimulated = static_cast<int>(LoopSide);

                ++state.dataPlnt->PlantManageHalfLoopCalls;
            }

        } // half loop based calling order...

        // decide new status for SimPlantLoops flag
        SimPlantLoops = false;
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
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
        for (DataPlant::LoopSideLocation LoopSide : DataPlant::LoopSideKeys) {
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
    HVAC::CtrlVarType localTempSetPt = HVAC::CtrlVarType::Temp;
    using namespace BranchInputManager;
    using DataSizing::AutoSize;
    using FluidProperties::CheckFluidPropertyName;
    using FluidProperties::FindGlycol;

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
        if (!allocated(state.dataAvail->PlantAvailMgr)) {
            state.dataAvail->PlantAvailMgr.allocate(state.dataPlnt->TotNumLoops);
        }
    } else {
        return;
    }

    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        Alpha = "";
        Num = 0.0;

        // set up some references
        auto &this_loop(state.dataPlnt->PlantLoop(LoopNum));
        auto &this_demand_side(this_loop.LoopSide(DataPlant::LoopSideLocation::Demand));
        auto &this_supply_side(this_loop.LoopSide(DataPlant::LoopSideLocation::Supply));
        Node::ConnObjType objType;
        if (LoopNum <= state.dataHVACGlobal->NumPlantLoops) {
            PlantLoopNum = LoopNum;
            this_loop.TypeOfLoop = LoopType::Plant;
            CurrentModuleObject = "PlantLoop";
            objType = Node::ConnObjType::PlantLoop;
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
            objType = Node::ConnObjType::CondenserLoop;
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
        Util::IsNameEmpty(state, Alpha(1), CurrentModuleObject, ErrorsFound);
        this_loop.Name = Alpha(1); // Load the Plant Loop Name

        if (Util::SameString(Alpha(2), "STEAM")) {
            this_loop.fluidType = Node::FluidType::Steam;
            this_loop.FluidName = Alpha(2);
        } else if (Util::SameString(Alpha(2), "WATER")) {
            this_loop.fluidType = Node::FluidType::Water;
            this_loop.FluidName = Alpha(2);
            this_loop.FluidIndex = FindGlycol(state, Alpha(2));
        } else if (Util::SameString(Alpha(2), "USERDEFINEDFLUIDTYPE")) {
            this_loop.fluidType = Node::FluidType::Water;
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

            this_loop.fluidType = Node::FluidType::Water;
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
        if (state.dataIPShortCut->lNumericFieldBlanks(5)) this_loop.Volume = Constant::AutoCalculate;
        if (this_loop.Volume == Constant::AutoCalculate) {
            this_loop.VolumeWasAutoSized = true;
        }
        // circulation time used to autocalculate loop volume
        if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
            this_loop.CirculationTime = 2.0; // default
        } else {
            this_loop.CirculationTime = Num(6);
        }

        // Load the Loop Inlet and Outlet Nodes and Connection Info (Alpha(7-10) are related to the supply side)
        this_supply_side.InNodeName = Alpha(6);
        this_supply_side.OutNodeName = Alpha(7);
        this_supply_side.BranchList = Alpha(8);
        this_supply_side.ConnectList = Alpha(9);
        this_demand_side.InNodeName = Alpha(10);
        this_demand_side.OutNodeName = Alpha(11);
        this_demand_side.BranchList = Alpha(12);
        this_demand_side.ConnectList = Alpha(13);

        this_supply_side.InNodeNum = GetSingleNode(state,
                                                       Alpha(6),
                                                       ErrorsFound,
                                                       objType,
                                                       Alpha(1),
                                                       this_loop.fluidType,
                                                       Node::ConnType::Inlet,
                                                       Node::CompFluidStream::Primary,
                                                       Node::ObjectIsParent);
        this_supply_side.OutNodeNum = GetSingleNode(state,
                                                        Alpha(7),
                                                        ErrorsFound,
                                                        objType,
                                                        Alpha(1),
                                                        this_loop.fluidType,
                                                        Node::ConnType::Outlet,
                                                        Node::CompFluidStream::Primary,
                                                        Node::ObjectIsParent);
        this_demand_side.InNodeNum = GetSingleNode(state,
                                                       Alpha(10),
                                                       ErrorsFound,
                                                       objType,
                                                       Alpha(1),
                                                       this_loop.fluidType,
                                                       Node::ConnType::Inlet,
                                                       Node::CompFluidStream::Primary,
                                                       Node::ObjectIsParent);
        this_demand_side.OutNodeNum = GetSingleNode(state,
                                                        Alpha(11),
                                                        ErrorsFound,
                                                        objType,
                                                        Alpha(1),
                                                        this_loop.fluidType,
                                                        Node::ConnType::Outlet,
                                                        Node::CompFluidStream::Primary,
                                                        Node::ObjectIsParent);

        this_demand_side.InNodeSetPt = IsNodeOnSetPtManager(state, this_demand_side.InNodeNum, localTempSetPt);
        this_demand_side.OutNodeSetPt = IsNodeOnSetPtManager(state, this_demand_side.OutNodeNum, localTempSetPt);
        this_supply_side.InNodeSetPt = IsNodeOnSetPtManager(state, this_supply_side.InNodeNum, localTempSetPt);
        this_supply_side.OutNodeSetPt = IsNodeOnSetPtManager(state, this_supply_side.OutNodeNum, localTempSetPt);
        this_loop.TempSetPointNodeNum = GetSingleNode(state,
                                                          Alpha(5),
                                                          ErrorsFound,
                                                          objType,
                                                          Alpha(1),
                                                          this_loop.fluidType,
                                                          Node::ConnType::Sensor,
                                                          Node::CompFluidStream::Primary,
                                                          Node::ObjectIsParent);

        // Load the load distribution scheme.
        LoadingScheme = Alpha(14);
        if (Util::SameString(LoadingScheme, "Optimal")) {
            this_loop.LoadDistribution = DataPlant::LoadingScheme::Optimal;
        } else if (Util::SameString(LoadingScheme, "SequentialLoad")) {
            this_loop.LoadDistribution = DataPlant::LoadingScheme::Sequential;
        } else if (Util::SameString(LoadingScheme, "UniformLoad")) {
            this_loop.LoadDistribution = DataPlant::LoadingScheme::Uniform;
        } else if (Util::SameString(LoadingScheme, "UniformPLR")) {
            this_loop.LoadDistribution = DataPlant::LoadingScheme::UniformPLR;
        } else if (Util::SameString(LoadingScheme, "SequentialUniformPLR")) {
            this_loop.LoadDistribution = DataPlant::LoadingScheme::SequentialUniformPLR;
        } else {
            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
            ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(14) + "=\"" + Alpha(14) + "\".");
            ShowContinueError(state, "Will default to SequentialLoad."); // TODO rename point
            this_loop.LoadDistribution = DataPlant::LoadingScheme::Sequential;
        }

        // When dual setpoint is allowed in condenser loop modify this code.
        if (this_loop.TypeOfLoop == LoopType::Plant) {
            // Get the Loop Demand Calculation Scheme
            if (Util::SameString(Alpha(16), "SingleSetpoint")) {
                this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
            } else if (Util::SameString(Alpha(16), "DualSetpointDeadband")) {
                if (this_loop.fluidType == Node::FluidType::Steam) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                    ShowContinueError(state,
                                      state.dataIPShortCut->cAlphaFieldNames(16) + "=\"" + Alpha(16) + "\" not valid for " +
                                          state.dataIPShortCut->cAlphaFieldNames(2) + "= Steam");
                    ShowContinueError(state,
                                      "Will reset " + state.dataIPShortCut->cAlphaFieldNames(16) + " = SingleSetPoint and simulation will continue.");
                    this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
                } else {
                    this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand;
                }
            } else if (Util::SameString(Alpha(16), "")) {
                this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
            } else {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                ShowContinueError(state, "..." + state.dataIPShortCut->cAlphaFieldNames(16) + "=\"" + Alpha(16) + "\".");
                ShowContinueError(state, "Will default to SingleSetPoint."); // TODO rename point
                this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
            }
        } else if (this_loop.TypeOfLoop == LoopType::Condenser) {
            this_loop.LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
        }

        // When Commonpipe is allowed in condenser loop modify this code. Sankar 06/29/2009
        if (this_loop.TypeOfLoop == LoopType::Plant) {
            if (Util::SameString(Alpha(17), "CommonPipe")) {
                this_loop.CommonPipeType = DataPlant::CommonPipeType::Single;
            } else if (Util::SameString(Alpha(17), "TwoWayCommonPipe")) {
                this_loop.CommonPipeType = DataPlant::CommonPipeType::TwoWay;
            } else if (Util::SameString(Alpha(17), "None") || state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                this_loop.CommonPipeType = DataPlant::CommonPipeType::No;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(17) + "=\"" + Alpha(17) + "\".");
                ShowContinueError(state, "Refer to I/O reference document for more details.");
                ErrorsFound = true;
            }
        } else if (this_loop.TypeOfLoop == LoopType::Condenser) {
            this_loop.CommonPipeType = DataPlant::CommonPipeType::No;
        }

        if (this_loop.CommonPipeType == DataPlant::CommonPipeType::TwoWay) {
            if (this_demand_side.InNodeSetPt && this_supply_side.InNodeSetPt) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid condition.");
                ShowContinueError(state,
                                  "While using a two way common pipe there can be setpoint on only one node other than Plant Supply Outlet node.");
                ShowContinueError(state, "Currently both Plant Demand inlet and plant supply inlet have setpoints.");
                ShowContinueError(state, "Select one of the two nodes and rerun the simulation.");
                ErrorsFound = true;
            }
            if (!this_demand_side.InNodeSetPt && !this_supply_side.InNodeSetPt) {
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

            this_loop.PressureSimType =
                static_cast<DataPlant::PressSimType>(getEnumValue(PressureSimTypeNamesUC, Util::makeUPPER(Alpha(PressSimAlphaIndex))));

            switch (this_loop.PressureSimType) {
                // Check all types
            case DataPlant::PressSimType::NoPressure:
            case DataPlant::PressSimType::FlowCorrection:
            case DataPlant::PressSimType::PumpPowerCorrection:
            case DataPlant::PressSimType::FlowSimulation: {
                MatchedPressureString = true;
                break;
            }
            default:
                break;
            }

            // If we found a match, check to make sure it is one of the valid
            // ones for this phase of pressure implementation
            if (MatchedPressureString) {
                if ((this_loop.PressureSimType == DataPlant::PressSimType::NoPressure) ||
                    (this_loop.PressureSimType == DataPlant::PressSimType::PumpPowerCorrection) ||
                    (this_loop.PressureSimType == DataPlant::PressSimType::FlowCorrection)) {
                    // We are OK here, move on
                } else {
                    // We have an erroneous input, alert user
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alpha(1) + "\", Invalid choice.");
                    ShowContinueError(
                        state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(PressSimAlphaIndex) + "=\"" + Alpha(PressSimAlphaIndex) + "\".");
                    ShowContinueError(state, "Currently only options are: ");
                    ShowContinueError(state, "  - " + format("{}", PressureSimTypeNamesUC[static_cast<int>(DataPlant::PressSimType::NoPressure)]));
                    ShowContinueError(state,
                                      "  - " + format("{}", PressureSimTypeNamesUC[static_cast<int>(DataPlant::PressSimType::PumpPowerCorrection)]));
                    ShowContinueError(state,
                                      "  - " + format("{}", PressureSimTypeNamesUC[static_cast<int>(DataPlant::PressSimType::FlowCorrection)]));
                    ErrorsFound = true;
                }
            }

            // if we made it this far and didn't get a match, check for blank
            if (!MatchedPressureString) {
                if (Alpha(PressSimAlphaIndex).empty()) {
                    this_loop.PressureSimType = DataPlant::PressSimType::NoPressure;
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
            Avail::GetPlantAvailabilityManager(state, Alpha(15), LoopNum, state.dataPlnt->TotNumLoops, ErrFound);
        }

        if (ErrFound) {
            ShowContinueError(state, "Input errors in  " + CurrentModuleObject + '=' + Alpha(1));
            ErrorsFound = true;
        }

        if (GetFirstBranchInletNodeName(state, this_demand_side.BranchList) != this_demand_side.InNodeName) {
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

        if (GetLastBranchOutletNodeName(state, this_demand_side.BranchList) != this_demand_side.OutNodeName) {
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

        if (GetFirstBranchInletNodeName(state, this_supply_side.BranchList) != this_supply_side.InNodeName) {
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

        if (GetLastBranchOutletNodeName(state, this_supply_side.BranchList) != this_supply_side.OutNodeName) {
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
                            Constant::Units::None,
                            (int &)state.dataAvail->PlantAvailMgr(LoopNum).availStatus,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
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
    using namespace BranchInputManager;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopNum; // DO loop counter for loops
    int HalfLoopNum;
    int NumOfPipesInLoop;
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
    Array1D_string InNodeNames;  // Node names from GetBranchData call
    Array1D_string OutNodeNames; // Node names from GetBranchData call
    Array1D_int InNodeNums;   // Node numbers from GetBranchData call
    Array1D_int OutNodeNums;  // Node numbers from GetBranchData call
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

        for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
            auto &loopSide = plantLoop.LoopSide(LoopSideNum);
            ASeriesBranchHasPump = false;
            AParallelBranchHasPump = false;
            NumOfPipesInLoop = 0; // Initialization
            ++HalfLoopNum;
            loopSide.BypassExists = false;
            if (plantLoop.TypeOfLoop == LoopType::Plant && LoopSideNum == LoopSideLocation::Demand) {
                LoopIdentifier = "Plant Demand";
            } else if (plantLoop.TypeOfLoop == LoopType::Plant && LoopSideNum == LoopSideLocation::Supply) {
                LoopIdentifier = "Plant Supply";
            } else if (plantLoop.TypeOfLoop == LoopType::Condenser && LoopSideNum == LoopSideLocation::Demand) {
                LoopIdentifier = "Condenser Demand";
            } else if (plantLoop.TypeOfLoop == LoopType::Condenser && LoopSideNum == LoopSideLocation::Supply) {
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
                InNodeNames.allocate(branch.TotalComponents);
                InNodeNums.dimension(branch.TotalComponents, 0);
                OutNodeNames.allocate(branch.TotalComponents);
                OutNodeNums.dimension(branch.TotalComponents, 0);

                GetBranchData(state,
                              plantLoop.Name,
                              BranchNames(BranchNum),
                              branch.PressureCurveType,
                              branch.PressureCurveIndex,
                              branch.TotalComponents,
                              CompTypes,
                              CompNames,
                              InNodeNames,
                              InNodeNums,
                              OutNodeNames,
                              OutNodeNums,
                              ErrorsFound);

                branch.Comp.allocate(branch.TotalComponents);

                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    // set up some references
                    auto &this_comp_type(CompTypes(CompNum));
                    auto &this_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum));

                    this_comp.CurOpSchemeType = OpScheme::Invalid;
                    this_comp.TypeOf = this_comp_type;
                    this_comp.location = EnergyPlus::PlantLocation(LoopNum, LoopSideNum, BranchNum, CompNum);

                    this_comp.Type = static_cast<PlantEquipmentType>(getEnumValue(PlantEquipTypeNamesUC, Util::makeUPPER(this_comp_type)));

                    switch (this_comp.Type) {
                    case PlantEquipmentType::Pipe: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr = Pipes::LocalPipeData::factory(state, PlantEquipmentType::Pipe, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PipeSteam: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr = Pipes::LocalPipeData::factory(state, PlantEquipmentType::PipeSteam, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PipeExterior: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, PlantEquipmentType::PipeExterior, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PipeInterior: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, PlantEquipmentType::PipeInterior, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PipeUnderground: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory(state, PlantEquipmentType::PipeUnderground, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PipingSystemPipeCircuit: {
                        this_comp.CurOpSchemeType = OpScheme::NoControl;
                        this_comp.compPtr =
                            PlantPipingSystemsManager::Circuit::factory(state, PlantEquipmentType::PipingSystemPipeCircuit, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PumpCondensate:
                    case PlantEquipmentType::PumpConstantSpeed:
                    case PlantEquipmentType::PumpVariableSpeed:
                    case PlantEquipmentType::PumpBankConstantSpeed:
                    case PlantEquipmentType::PumpBankVariableSpeed: {
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        this_comp.CurOpSchemeType = OpScheme::Pump;
                        if (BranchNum == 1 || BranchNum == state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches) {
                            ASeriesBranchHasPump = true;
                        } else {
                            AParallelBranchHasPump = true;
                        }
                        LoopSidePumpInformation p;
                        p.PumpName = CompNames(CompNum);
                        p.BranchNum = BranchNum;
                        p.CompNum = CompNum;
                        p.PumpOutNodeNum = OutNodeNums(CompNum);
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).BranchPumpsExist = AParallelBranchHasPump;
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Pumps.push_back(p);
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalPumps++;
                        break;
                    }
                    case PlantEquipmentType::WtrHeaterMixed:
                    case PlantEquipmentType::WtrHeaterStratified: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_DFAbsorption: {
                        this_comp.compPtr = ChillerGasAbsorption::GasAbsorberSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_ExhFiredAbsorption: {
                        this_comp.compPtr = ChillerExhaustAbsorption::ExhaustAbsorberSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::ChilledWaterTankMixed:
                    case PlantEquipmentType::ChilledWaterTankStratified: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = WaterThermalTanks::WaterThermalTankData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::WaterUseConnection: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = WaterUse::WaterConnectionsType::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::SolarCollectorFlatPlate:
                    case PlantEquipmentType::SolarCollectorICS: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        }
                        this_comp.compPtr = SolarCollectors::CollectorData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PlantLoadProfile: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = PlantLoadProfile::PlantProfileData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::GrndHtExchgSystem: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory(state, PlantEquipmentType::GrndHtExchgSystem, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::GrndHtExchgSurface: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr = SurfaceGroundHeatExchanger::SurfaceGroundHeatExchangerData::factory(
                            state, PlantEquipmentType::GrndHtExchgSurface, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::GrndHtExchgPond: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr = PondGroundHeatExchanger::PondGroundHeatExchangerData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::GrndHtExchgSlinky: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory(state, PlantEquipmentType::GrndHtExchgSlinky, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_ElectricEIR: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ChillerElectricEIR::ElectricEIRChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_ElectricReformEIR: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ChillerReformulatedEIR::ReformulatedEIRChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_Electric: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = PlantChillers::ElectricChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_ElectricASHRAE205: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ChillerElectricASHRAE205::ASHRAE205ChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_EngineDriven: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = PlantChillers::EngineDrivenChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_CombTurbine: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = PlantChillers::GTChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_ConstCOP: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = PlantChillers::ConstCOPChillerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Boiler_Simple: {
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        this_comp.compPtr = Boilers::BoilerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Boiler_Steam: {
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        this_comp.compPtr = BoilerSteam::BoilerSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_Indirect_Absorption: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ChillerIndirectAbsorption::IndirectAbsorberSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Chiller_Absorption: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ChillerAbsorption::BLASTAbsorberSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::CoolingTower_SingleSpd:
                    case PlantEquipmentType::CoolingTower_TwoSpd: {
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::CoolingTower_VarSpd:
                    case PlantEquipmentType::CoolingTower_VarSpdMerkel: {
                        this_comp.compPtr = CondenserLoopTowers::CoolingTower::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Generator_FCExhaust: {
                        this_comp.compPtr = FuelCellElectricGenerator::FCDataStruct::factory_exhaust(state, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HeatPumpWtrHeaterPumped:
                    case PlantEquipmentType::HeatPumpWtrHeaterWrapped: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = WaterThermalTanks::HeatPumpWaterHeaterData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::HPWaterEFCooling: {
                        this_comp.compPtr =
                            HeatPumpWaterToWaterSimple::GshpSpecs::factory(state, PlantEquipmentType::HPWaterEFCooling, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HPWaterEFHeating: {
                        this_comp.compPtr =
                            HeatPumpWaterToWaterSimple::GshpSpecs::factory(state, PlantEquipmentType::HPWaterEFHeating, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HPWaterPEHeating: {
                        this_comp.compPtr = HeatPumpWaterToWaterHEATING::GshpPeHeatingSpecs::factory(state, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HPWaterPECooling: {
                        this_comp.compPtr = HeatPumpWaterToWaterCOOLING::GshpPeCoolingSpecs::factory(state, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HeatPumpEIRHeating: {
                        this_comp.compPtr =
                            EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::factory(state, PlantEquipmentType::HeatPumpEIRHeating, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HeatPumpEIRCooling: {
                        this_comp.compPtr =
                            EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::factory(state, PlantEquipmentType::HeatPumpEIRCooling, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::HeatPumpFuelFiredHeating: {
                        this_comp.compPtr = EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::factory(
                            state, PlantEquipmentType::HeatPumpFuelFiredHeating, CompNames(CompNum));
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        break;
                    }
                    case PlantEquipmentType::HeatPumpFuelFiredCooling: {
                        this_comp.compPtr = EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::factory(
                            state, PlantEquipmentType::HeatPumpFuelFiredCooling, CompNames(CompNum));
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        break;
                    }
                    case PlantEquipmentType::HeatPumpVRF: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = HVACVariableRefrigerantFlow::VRFCondenserEquipment::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PurchChilledWater: {
                        this_comp.compPtr =
                            OutsideEnergySources::OutsideEnergySourceSpecs::factory(state, PlantEquipmentType::PurchChilledWater, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PurchHotWater: {
                        this_comp.compPtr =
                            OutsideEnergySources::OutsideEnergySourceSpecs::factory(state, PlantEquipmentType::PurchHotWater, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PurchSteam: {
                        this_comp.compPtr =
                            OutsideEnergySources::OutsideEnergySourceSpecs::factory(state, PlantEquipmentType::PurchSteam, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::TS_IceSimple: {
                        this_comp.compPtr = IceThermalStorage::SimpleIceStorageData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::TS_IceDetailed: {
                        this_comp.compPtr = IceThermalStorage::DetailedIceStorageData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::ValveTempering: {
                        this_comp.compPtr = PlantValves::TemperValveData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::FluidToFluidPlantHtExchg: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::FreeRejection;
                        }
                        this_comp.compPtr = PlantHeatExchangerFluidToFluid::HeatExchangerStruct::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Generator_MicroTurbine: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = MicroturbineElectricGenerator::MTGeneratorSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Generator_ICEngine: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = ICEngineElectricGenerator::ICEngineGeneratorSpecs::factory(state, CompNames(CompNum));

                        break;
                    }
                    case PlantEquipmentType::Generator_CTurbine: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = CTElectricGenerator::CTGeneratorData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Generator_MicroCHP: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = MicroCHPElectricGenerator::MicroCHPDataStruct::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::Generator_FCStackCooler: {
                        this_comp.compPtr = FuelCellElectricGenerator::FCDataStruct::factory(state, CompNames(CompNum));
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        break;
                    }
                    case PlantEquipmentType::FluidCooler_SingleSpd: {
                        this_comp.compPtr =
                            FluidCoolers::FluidCoolerspecs::factory(state, PlantEquipmentType::FluidCooler_SingleSpd, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::FluidCooler_TwoSpd: {
                        this_comp.compPtr =
                            FluidCoolers::FluidCoolerspecs::factory(state, PlantEquipmentType::FluidCooler_TwoSpd, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::EvapFluidCooler_SingleSpd: {
                        this_comp.compPtr = EvaporativeFluidCoolers::EvapFluidCoolerSpecs::factory(
                            state, PlantEquipmentType::EvapFluidCooler_SingleSpd, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::EvapFluidCooler_TwoSpd: {
                        this_comp.compPtr = EvaporativeFluidCoolers::EvapFluidCoolerSpecs::factory(
                            state, PlantEquipmentType::EvapFluidCooler_TwoSpd, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PVTSolarCollectorFlatPlate: {
                        if (LoopSideNum == LoopSideLocation::Demand) {
                            this_comp.CurOpSchemeType = OpScheme::Demand;
                        } else if (LoopSideNum == LoopSideLocation::Supply) {
                            this_comp.CurOpSchemeType = OpScheme::Invalid;
                        }
                        this_comp.compPtr = PhotovoltaicThermalCollectors::PVTCollectorStruct::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::CentralGroundSourceHeatPump: {
                        this_comp.compPtr = PlantCentralGSHP::WrapperSpecs::factory(state, CompNames(CompNum));
                        // now deal with demand components of the ZoneHVAC type served by ControlCompOutput
                        break;
                    }
                    case PlantEquipmentType::SwimmingPool_Indoor: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = SwimmingPool::SwimmingPoolData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PackagedTESCoolingCoil:
                    case PlantEquipmentType::CoilWaterCooling:
                    case PlantEquipmentType::CoilWaterDetailedFlatCooling:
                    case PlantEquipmentType::CoilWaterSimpleHeating:
                    case PlantEquipmentType::CoilSteamAirHeating:
                    case PlantEquipmentType::Baseboard_Rad_Conv_Water:
                    case PlantEquipmentType::Baseboard_Conv_Water:
                    case PlantEquipmentType::Baseboard_Rad_Conv_Steam:
                    case PlantEquipmentType::CoolingPanel_Simple:
                    case PlantEquipmentType::LowTempRadiant_VarFlow:
                    case PlantEquipmentType::LowTempRadiant_ConstFlow:
                    case PlantEquipmentType::CooledBeamAirTerminal:
                    case PlantEquipmentType::FourPipeBeamAirTerminal:
                    case PlantEquipmentType::MultiSpeedHeatPumpRecovery:
                    case PlantEquipmentType::UnitarySysRecovery:
                    case PlantEquipmentType::CoilWAHPHeatingEquationFit:
                    case PlantEquipmentType::CoilWAHPCoolingEquationFit:
                    case PlantEquipmentType::CoilVSWAHPHeatingEquationFit:
                    case PlantEquipmentType::CoilVSWAHPCoolingEquationFit:
                    case PlantEquipmentType::CoilWAHPHeatingParamEst:
                    case PlantEquipmentType::CoilWAHPCoolingParamEst: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        break;
                    }
                    case PlantEquipmentType::RefrigSystemWaterCondenser: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = RefrigeratedCase::RefrigCondenserData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::RefrigerationWaterCoolRack: {
                        this_comp.CurOpSchemeType = OpScheme::Demand;
                        this_comp.compPtr = RefrigeratedCase::RefrigRackData::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::PlantComponentUserDefined: {
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        this_comp.compPtr = UserDefinedComponents::UserPlantComponentStruct::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::CoilUserDefined:
                    case PlantEquipmentType::ZoneHVACAirUserDefined:
                    case PlantEquipmentType::AirTerminalUserDefined: {
                        this_comp.CurOpSchemeType = OpScheme::Invalid;
                        this_comp.compPtr = &state.dataPlantMgr->dummyPlantComponent;
                        break;
                    }
                    case PlantEquipmentType::WaterSource: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr = PlantComponentTemperatureSources::WaterSourceSpecs::factory(state, CompNames(CompNum));
                        break;
                    }
                    case PlantEquipmentType::GrndHtExchgHorizTrench: {
                        this_comp.CurOpSchemeType = OpScheme::Uncontrolled;
                        this_comp.compPtr =
                            PlantPipingSystemsManager::Circuit::factory(state, PlantEquipmentType::GrndHtExchgHorizTrench, CompNames(CompNum));
                        break;
                    }
                    default: {
                        if (has_prefixi(this_comp_type, "Pump") || has_prefixi(this_comp_type, "HeaderedPumps")) {
                            // discover unsupported equipment on branches.
                            ShowSevereError(state, "GetPlantInput: trying to process a pump type that is not supported, dev note");
                            ShowContinueError(state, "Component Type =" + this_comp_type);
                        } else {
                            // discover unsupported equipment on branches.
                            ShowSevereError(state, "GetPlantInput: Branch=\"" + BranchNames(BranchNum) + "\", invalid component on branch.");
                            ShowContinueError(state, "...invalid component type=\"" + this_comp_type + "\", name=\"" + CompNames(CompNum) + "\".");
                            //            ErrorsFound=.TRUE.
                        }
                    }
                    }

                    if (!this_comp.compPtr) ShowFatalError(state, format(" Plant component \"{}\" was not assigned a pointer.", this_comp_type));

                    this_comp.Name = CompNames(CompNum);
                    this_comp.InNodeName = InNodeNames(CompNum);
                    this_comp.InNodeNum = InNodeNums(CompNum);
                    this_comp.OutNodeName = OutNodeNames(CompNum);
                    this_comp.OutNodeNum = OutNodeNums(CompNum);
                }

                // set branch inlet/outlet nodes
                branch.InNodeNum = branch.Comp(1).InNodeNum;
                branch.OutNodeNum = branch.Comp(branch.TotalComponents).OutNodeNum;

                CompTypes.deallocate();
                CompNames.deallocate();
                CompCtrls.deallocate();
                InNodeNames.deallocate();
                InNodeNums.deallocate();
                OutNodeNames.deallocate();
                OutNodeNums.deallocate();
            }

            BranchNames.deallocate();

            if (ASeriesBranchHasPump && AParallelBranchHasPump) {
                ShowSevereError(state, "Current version does not support Loop pumps and branch pumps together");
                ShowContinueError(state, "Occurs in loop " + state.dataPlnt->PlantLoop(LoopNum).Name);
                ErrorsFound = true;
            }

            // Obtain the Splitter and Mixer information
            auto &dln = state.dataLoopNodes;
            if (loopSide.ConnectList.empty()) {
                dln->NumSplitters = 0;
                dln->NumMixers = 0;
            } else {
                errFlag = false;
                GetNumSplitterMixerInConntrList(
                    state, plantLoop.Name, loopSide.ConnectList, dln->NumSplitters, dln->NumMixers, errFlag);
                if (errFlag) {
                    ErrorsFound = true;
                }
                if (dln->NumSplitters != dln->NumMixers) {
                    ShowSevereError(state,
                                    "GetPlantInput: Loop Name=" + plantLoop.Name + ", ConnectorList=" + loopSide.ConnectList +
                                        ", unequal number of splitters and mixers");
                    ErrorsFound = true;
                }
            }

            loopSide.Splitter.Exists = dln->NumSplitters > 0;
            loopSide.Mixer.Exists = dln->NumMixers > 0;

            if (ErrorsFound) {
                ShowFatalError(state, "GetPlantInput: Previous Severe errors cause termination.");
            }

            NumConnectorsInLoop = dln->NumSplitters + dln->NumMixers;
            SplitNum = 1;
            for (ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum) {

                if (SplitNum > dln->NumSplitters) break;
                OutNodeNames.allocate(MaxNumAlphas);
                OutNodeNums.allocate(MaxNumAlphas);
                GetLoopSplitter(state,
                                plantLoop.Name,
                                loopSide.ConnectList,
                                loopSide.Splitter.Name,
                                loopSide.Splitter.Exists,
                                loopSide.Splitter.InNodeName,
                                loopSide.Splitter.InNodeNum,
                                loopSide.Splitter.NumOutNodes,
                                OutNodeNames,
                                OutNodeNums,
                                ErrorsFound,
                                ConnNum,
                                SplitNum);

                if (SplitNum == 1) {
                    OutNodeNames.deallocate();
                    OutNodeNums.deallocate();
                    continue;
                }

                // Map the inlet node to the splitter to a branch number
                if (loopSide.Splitter.Exists) {
                    // Map the inlet node to the splitter to a branch number
                    SplitInBranch = false;
                    for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                        auto &branch = loopSide.Branch(BranchNum);
                        CompNum = branch.TotalComponents;
                        if (loopSide.Splitter.InNodeNum == branch.Comp(CompNum).OutNodeNum) {
                            loopSide.Splitter.BranchNumIn = BranchNum;
                            SplitInBranch = true;
                            break; // BranchNum DO loop
                        }
                    }
                    if (!SplitInBranch) {
                        ShowSevereError(state, "Splitter Inlet Branch not found, Splitter=" + loopSide.Splitter.Name);
                        ShowContinueError(state, "Splitter Branch Inlet name=" + loopSide.Splitter.InNodeName);
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ErrorsFound = true;
                    }

                    loopSide.Splitter.OutNodeNames.allocate(loopSide.Splitter.NumOutNodes);
                    loopSide.Splitter.OutNodeNums.dimension(loopSide.Splitter.NumOutNodes, 0);
                    loopSide.Splitter.BranchNumOut.dimension(loopSide.Splitter.NumOutNodes, 0);

                    SplitOutBranch.allocate(loopSide.Splitter.NumOutNodes);
                    SplitOutBranch = false;
                    for (NodeNum = 1; NodeNum <= loopSide.Splitter.NumOutNodes; ++NodeNum) {
                        loopSide.Splitter.OutNodeNames(NodeNum) = OutNodeNames(NodeNum);
                        loopSide.Splitter.OutNodeNums(NodeNum) = OutNodeNums(NodeNum);
                        // The following DO loop series is intended to store the branch number for each outlet
                        // branch of the splitter
                        for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                            if (loopSide.Splitter.OutNodeNums(NodeNum) == loopSide.Branch(BranchNum).Comp(1).InNodeNum) {
                                loopSide.Splitter.BranchNumOut(NodeNum) = BranchNum;
                                SplitOutBranch(NodeNum) = true;
                                break; // BranchNum DO loop
                            }
                        }
                    }

                    for (Outlet = 1; Outlet <= loopSide.Splitter.NumOutNodes; ++Outlet) {
                        if (SplitOutBranch(Outlet)) continue;
                        ShowSevereError(state, "Splitter Outlet Branch not found, Splitter=" + loopSide.Splitter.Name);
                        ShowContinueError(state, "Splitter Branch Outlet node name=" + loopSide.Splitter.OutNodeNames(Outlet));
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ShowContinueError(state, "Loop BranchList=" + loopSide.BranchList);
                        ShowContinueError(state, "Loop ConnectorList=" + loopSide.ConnectList);
                        ErrorsFound = true;
                    }

                    SplitOutBranch.deallocate();

                } // Splitter exists
                OutNodeNames.deallocate();
                OutNodeNums.deallocate();
            }

            MixNum = 1;
            for (ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum) {

                if (MixNum > dln->NumMixers) break;
                InNodeNames.allocate(MaxNumAlphas);
                InNodeNums.allocate(MaxNumAlphas);
                GetLoopMixer(state,
                             plantLoop.Name,
                             loopSide.ConnectList,
                             loopSide.Mixer.Name,
                             loopSide.Mixer.Exists,
                             loopSide.Mixer.OutNodeName,
                             loopSide.Mixer.OutNodeNum,
                             loopSide.Mixer.NumInNodes,
                             InNodeNames,
                             InNodeNums,
                             ErrorsFound,
                             ConnNum,
                             MixNum);

                if (MixNum == 1) {
                    InNodeNames.deallocate();
                    InNodeNums.deallocate();
                    continue;
                }
                // Map the outlet node of the mixer to a branch number
                if (loopSide.Mixer.Exists) {
                    // Map the outlet node of the mixer to a branch number
                    MixerOutBranch = false;
                    for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                        if (loopSide.Mixer.OutNodeNum == loopSide.Branch(BranchNum).Comp(1).InNodeNum) {
                            loopSide.Mixer.BranchNumOut = BranchNum;
                            MixerOutBranch = true;
                            break; // BranchNum DO loop
                        }
                    }
                    if (!MixerOutBranch) {
                        ShowSevereError(state, "Mixer Outlet Branch not found, Mixer=" + loopSide.Mixer.Name);
                        ErrorsFound = true;
                    }

                    loopSide.Mixer.InNodeNames.allocate(loopSide.Mixer.NumInNodes);
                    loopSide.Mixer.InNodeNums.dimension(loopSide.Mixer.NumInNodes, 0);
                    loopSide.Mixer.BranchNumIn.dimension(loopSide.Mixer.NumInNodes, 0);

                    MixerInBranch.allocate(loopSide.Mixer.NumInNodes);
                    MixerInBranch = false;
                    for (NodeNum = 1; NodeNum <= loopSide.Mixer.NumInNodes; ++NodeNum) {
                        loopSide.Mixer.InNodeNames(NodeNum) = InNodeNames(NodeNum);
                        loopSide.Mixer.InNodeNums(NodeNum) = InNodeNums(NodeNum);
                        // The following DO loop series is intended to store the branch number for each inlet branch of the mixer
                        for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                            auto &branch = loopSide.Branch(BranchNum);
                            CompNum = branch.TotalComponents;
                            if (loopSide.Mixer.InNodeNums(NodeNum) == branch.Comp(CompNum).OutNodeNum) {
                                loopSide.Mixer.BranchNumIn(NodeNum) = BranchNum;
                                MixerInBranch(NodeNum) = true;
                                break; // BranchNum DO loop
                            }
                        }
                    }

                    for (Inlet = 1; Inlet <= loopSide.Mixer.NumInNodes; ++Inlet) {
                        if (MixerInBranch(Inlet)) continue;
                        ShowSevereError(state, "Mixer Inlet Branch not found, Mixer=" + loopSide.Mixer.Name);
                        ShowContinueError(state, "Mixer Branch Inlet name=" + loopSide.Mixer.InNodeNames(Inlet));
                        ShowContinueError(state, "In Loop=" + plantLoop.Name);
                        ShowContinueError(state, "Loop BranchList=" + loopSide.BranchList);
                        ShowContinueError(state, "Loop ConnectorList=" + loopSide.ConnectList);
                        ErrorsFound = true;
                    }

                    MixerInBranch.deallocate();
                } // Mixer exists
                InNodeNames.deallocate();
                InNodeNums.deallocate();
            }

            loopSide.noLoadConstantSpeedBranchFlowRateSteps.allocate(loopSide.TotalBranches - 2);

            // TODO: this is just intended to be temporary
            loopSide.plantLoc.loopNum = LoopNum;
            loopSide.plantLoc.loopSideNum = LoopSideNum;

        } // ... end LoopSideNum=LoopSideLocation::Demand,LoopSideLocation::Supply

        plantLoop.LoopSide(DataPlant::LoopSideLocation::Demand).loopSideDescription = plantLoop.Name + " - Demand Side";
        plantLoop.LoopSide(DataPlant::LoopSideLocation::Supply).loopSideDescription = plantLoop.Name + " - Supply Side";

        // a nice little spot to report out bad pump/common-pipe configurations
        bool const ThisSideHasPumps = (plantLoop.LoopSide(DataPlant::LoopSideLocation::Demand).TotalPumps > 0);
        bool const OtherSideHasPumps = (plantLoop.LoopSide(DataPlant::LoopSideLocation::Supply).TotalPumps > 0);
        if ((plantLoop.CommonPipeType != DataPlant::CommonPipeType::No) && (!ThisSideHasPumps || !OtherSideHasPumps)) {
            ShowSevereError(state, "Input Error: Common Pipe configurations must have pumps on both sides of loop");
            ShowContinueError(state, "Occurs on plant loop name =\"" + plantLoop.Name + "\"");
            ShowContinueError(state, "Make sure both demand and supply sides have a pump");
            ErrorsFound = true;
        } else if ((plantLoop.CommonPipeType == DataPlant::CommonPipeType::No) && ThisSideHasPumps && OtherSideHasPumps) {
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
        for (DataPlant::LoopSideLocation LoopSideCounter : DataPlant::LoopSideKeys) {
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

    if (state.dataHVACGlobal->NumPlantLoops > 0)
        state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Supply)].allocate(state.dataHVACGlobal->NumPlantLoops);
    if (state.dataHVACGlobal->NumPlantLoops > 0)
        state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Demand)].allocate(state.dataHVACGlobal->NumPlantLoops);

    for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumPlantLoops; ++LoopNum) {

        // set up references for this loop
        auto &this_plant_loop(state.dataPlnt->PlantLoop(LoopNum));
        auto &this_plant_supply(this_plant_loop.LoopSide(LoopSideLocation::Supply));
        auto &this_vent_plant_supply(state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Supply)](LoopNum));
        auto &this_plant_demand(this_plant_loop.LoopSide(LoopSideLocation::Demand));
        auto &this_vent_plant_demand(state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Demand)](LoopNum));

        this_vent_plant_supply.Name = this_plant_loop.Name;
        this_vent_plant_supply.InNodeNum = this_plant_supply.InNodeNum;
        this_vent_plant_supply.InNodeName = this_plant_supply.InNodeName;
        this_vent_plant_supply.OutNodeNum = this_plant_supply.OutNodeNum;
        this_vent_plant_supply.OutNodeName = this_plant_supply.OutNodeName;
        this_vent_plant_supply.TotalBranches = this_plant_supply.TotalBranches;

        if (this_vent_plant_supply.TotalBranches > 0) this_vent_plant_supply.Branch.allocate(this_vent_plant_supply.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_plant_supply.TotalBranches; ++BranchNum) {

            auto &this_plant_supply_branch(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum));
            auto &this_vent_plant_supply_branch(state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Supply)](LoopNum).Branch(BranchNum));

            this_vent_plant_supply_branch.Name = this_plant_supply_branch.Name;
            this_vent_plant_supply_branch.InNodeNum = this_plant_supply_branch.InNodeNum;
            this_vent_plant_supply_branch.OutNodeNum = this_plant_supply_branch.OutNodeNum;
            this_vent_plant_supply_branch.TotalComponents = this_plant_supply_branch.TotalComponents;
            if (this_vent_plant_supply_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_plant_supply_branch.TotalComponents;
                this_vent_plant_supply_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1;
                 CompNum <= state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Supply)](LoopNum).Branch(BranchNum).TotalComponents;
                 ++CompNum) {

                auto &this_plant_supply_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_plant_supply_comp(
                    state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Supply)](LoopNum).Branch(BranchNum).Comp(CompNum));

                this_vent_plant_supply_comp.Name = this_plant_supply_comp.Name;
                this_vent_plant_supply_comp.TypeOf = this_plant_supply_comp.TypeOf;
                this_vent_plant_supply_comp.InNodeName = this_plant_supply_comp.InNodeName;
                this_vent_plant_supply_comp.OutNodeName = this_plant_supply_comp.OutNodeName;
                this_vent_plant_supply_comp.InNodeNum = this_plant_supply_comp.InNodeNum;
                this_vent_plant_supply_comp.OutNodeNum = this_plant_supply_comp.OutNodeNum;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

        this_vent_plant_demand.Name = this_plant_loop.Name;
        this_vent_plant_demand.InNodeNum = this_plant_demand.InNodeNum;
        this_vent_plant_demand.InNodeName = this_plant_demand.InNodeName;
        this_vent_plant_demand.OutNodeNum = this_plant_demand.OutNodeNum;
        this_vent_plant_demand.OutNodeName = this_plant_demand.OutNodeName;
        this_vent_plant_demand.TotalBranches = this_plant_demand.TotalBranches;

        if (this_vent_plant_demand.TotalBranches > 0) this_vent_plant_demand.Branch.allocate(this_vent_plant_demand.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_plant_demand.TotalBranches; ++BranchNum) {

            auto &this_plant_demand_branch(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum));
            auto &this_vent_plant_demand_branch(state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Demand)](LoopNum).Branch(BranchNum));

            this_vent_plant_demand_branch.Name = this_plant_demand_branch.Name;
            this_vent_plant_demand_branch.InNodeNum = this_plant_demand_branch.InNodeNum;
            this_vent_plant_demand_branch.OutNodeNum = this_plant_demand_branch.OutNodeNum;
            this_vent_plant_demand_branch.TotalComponents = this_plant_demand_branch.TotalComponents;
            if (this_vent_plant_demand_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_plant_demand_branch.TotalComponents;
                this_vent_plant_demand_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= this_vent_plant_demand_branch.TotalComponents; ++CompNum) {

                auto &this_plant_demand_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_plant_demand_comp(
                    state.dataPlnt->VentRepPlant[static_cast<int>(LoopSideLocation::Demand)](LoopNum).Branch(BranchNum).Comp(CompNum));

                this_vent_plant_demand_comp.Name = this_plant_demand_comp.Name;
                this_vent_plant_demand_comp.TypeOf = this_plant_demand_comp.TypeOf;
                this_vent_plant_demand_comp.InNodeName = this_plant_demand_comp.InNodeName;
                this_vent_plant_demand_comp.OutNodeName = this_plant_demand_comp.OutNodeName;
                this_vent_plant_demand_comp.InNodeNum = this_plant_demand_comp.InNodeNum;
                this_vent_plant_demand_comp.OutNodeNum = this_plant_demand_comp.OutNodeNum;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

    } // loop over plant supply loops (ventilation report data)

    if (state.dataHVACGlobal->NumCondLoops > 0)
        state.dataPlnt->VentRepCond[static_cast<int>(LoopSideLocation::Supply)].allocate(state.dataHVACGlobal->NumCondLoops);
    if (state.dataHVACGlobal->NumCondLoops > 0)
        state.dataPlnt->VentRepCond[static_cast<int>(LoopSideLocation::Demand)].allocate(state.dataHVACGlobal->NumCondLoops);

    for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumCondLoops; ++LoopNum) {

        LoopNumInArray = LoopNum + state.dataHVACGlobal->NumPlantLoops;

        // set up references for this loop
        auto &this_cond_loop(state.dataPlnt->PlantLoop(LoopNumInArray));
        auto &this_cond_supply(this_cond_loop.LoopSide(LoopSideLocation::Supply));
        auto &this_vent_cond_supply(state.dataPlnt->VentRepCond[static_cast<int>(LoopSideLocation::Supply)](LoopNum));
        auto &this_cond_demand(this_cond_loop.LoopSide(LoopSideLocation::Demand));
        auto &this_vent_cond_demand(state.dataPlnt->VentRepCond[static_cast<int>(LoopSideLocation::Demand)](LoopNum));

        this_vent_cond_supply.Name = this_cond_loop.Name;
        this_vent_cond_supply.InNodeNum = this_cond_supply.InNodeNum;
        this_vent_cond_supply.InNodeName = this_cond_supply.InNodeName;
        this_vent_cond_supply.OutNodeNum = this_cond_supply.OutNodeNum;
        this_vent_cond_supply.OutNodeName = this_cond_supply.OutNodeName;
        this_vent_cond_supply.TotalBranches = this_cond_supply.TotalBranches;
        if (this_vent_cond_supply.TotalBranches > 0) this_vent_cond_supply.Branch.allocate(this_vent_cond_supply.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_cond_supply.TotalBranches; ++BranchNum) {

            auto &this_cond_supply_branch(this_cond_supply.Branch(BranchNum));
            auto &this_vent_cond_supply_branch(this_vent_cond_supply.Branch(BranchNum));

            this_vent_cond_supply_branch.Name = this_cond_supply_branch.Name;
            this_vent_cond_supply_branch.InNodeNum = this_cond_supply_branch.InNodeNum;
            this_vent_cond_supply_branch.OutNodeNum = this_cond_supply_branch.OutNodeNum;
            this_vent_cond_supply_branch.TotalComponents = this_cond_supply_branch.TotalComponents;
            if (this_vent_cond_supply_branch.TotalComponents > 0) {
                TotCompsOnBranch = this_vent_cond_supply_branch.TotalComponents;
                this_vent_cond_supply_branch.Comp.allocate(TotCompsOnBranch);
            }

            for (CompNum = 1; CompNum <= this_vent_cond_supply_branch.TotalComponents; ++CompNum) {

                auto &this_cond_supply_comp(this_cond_loop.LoopSide(LoopSideLocation::Supply).Branch(BranchNum).Comp(CompNum));
                auto &this_vent_cond_supply_comp(this_vent_cond_supply.Branch(BranchNum).Comp(CompNum));

                this_vent_cond_supply_comp.Name = this_cond_supply_comp.Name;
                this_vent_cond_supply_comp.TypeOf = this_cond_supply_comp.TypeOf;
                this_vent_cond_supply_comp.InNodeName = this_cond_supply_comp.InNodeName;
                this_vent_cond_supply_comp.OutNodeName = this_cond_supply_comp.OutNodeName;
                this_vent_cond_supply_comp.InNodeNum = this_cond_supply_comp.InNodeNum;
                this_vent_cond_supply_comp.OutNodeNum = this_cond_supply_comp.OutNodeNum;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

        this_vent_cond_demand.Name = this_cond_loop.Name;
        this_vent_cond_demand.InNodeNum = this_cond_demand.InNodeNum;
        this_vent_cond_demand.InNodeName = this_cond_demand.InNodeName;
        this_vent_cond_demand.OutNodeNum = this_cond_demand.OutNodeNum;
        this_vent_cond_demand.OutNodeName = this_cond_demand.OutNodeName;
        this_vent_cond_demand.TotalBranches = this_cond_demand.TotalBranches;
        if (this_vent_cond_demand.TotalBranches > 0) this_vent_cond_demand.Branch.allocate(this_vent_cond_demand.TotalBranches);

        for (BranchNum = 1; BranchNum <= this_vent_cond_demand.TotalBranches; ++BranchNum) {

            auto &this_cond_demand_branch(this_cond_demand.Branch(BranchNum));
            auto &this_vent_cond_demand_branch(this_vent_cond_demand.Branch(BranchNum));

            this_vent_cond_demand_branch.Name = this_cond_demand_branch.Name;
            this_vent_cond_demand_branch.InNodeNum = this_cond_demand_branch.InNodeNum;
            this_vent_cond_demand_branch.OutNodeNum = this_cond_demand_branch.OutNodeNum;
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
                this_vent_cond_demand_comp.InNodeName = this_cond_demand_comp.InNodeName;
                this_vent_cond_demand_comp.OutNodeName = this_cond_demand_comp.OutNodeName;
                this_vent_cond_demand_comp.InNodeNum = this_cond_demand_comp.InNodeNum;
                this_vent_cond_demand_comp.OutNodeNum = this_cond_demand_comp.OutNodeNum;

            } // loop over components in branches on the loop (ventilation report data)

        } // loop over branches on the loop (ventilation report data)

    } // loop over plant supply loops (ventilation report data)

    // OneTimeInit Here
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        auto &plantLoop = state.dataPlnt->PlantLoop(LoopNum);
        plantLoop.LoopHasConnectionComp = false;

        for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
            auto &loopSide = plantLoop.LoopSide(LoopSideNum);

            for (BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {

                for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    //                    auto &this_comp_type(CompTypes(CompNum));
                    auto &this_comp(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum));
                    // auto type = this_comp.TypeOf;
                    this_comp.oneTimeInit(state);
                }
            }
        }
    }
}

void SetupReports(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   July 2001

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the plant supply side reports.
    // It was created during the splitting of supply and demand side functions.

    int MaxBranches = 0; // Maximum number of branches on any plant loop (used for allocating arrays)

    for (auto &loop : state.dataPlnt->PlantLoop) {
        MaxBranches = max(MaxBranches, loop.LoopSide(LoopSideLocation::Demand).TotalBranches);
        MaxBranches = max(MaxBranches, loop.LoopSide(LoopSideLocation::Supply).TotalBranches);
        loop.MaxBranch = MaxBranches;
        loop.CoolingDemand = 0.0;
        loop.HeatingDemand = 0.0;
        loop.DemandNotDispatched = 0.0;
        loop.UnmetDemand = 0.0;
        loop.InNodeTemperature = 0.0;
        loop.OutNodeTemperature = 0.0;
        loop.InNodeFlowrate = 0.0;
        loop.BypassFrac = 0.0;
        loop.OutNodeFlowrate = 0.0;
    }

    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        auto &loop = state.dataPlnt->PlantLoop(LoopNum);
        SetupOutputVariable(state,
                            "Plant Supply Side Cooling Demand Rate",
                            Constant::Units::W,
                            loop.CoolingDemand,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Heating Demand Rate",
                            Constant::Units::W,
                            loop.HeatingDemand,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Inlet Mass Flow Rate",
                            Constant::Units::kg_s,
                            loop.InNodeFlowrate,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);

        SetupOutputVariable(state,
                            "Plant Supply Side Inlet Temperature",
                            Constant::Units::C,
                            loop.InNodeTemperature,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Outlet Temperature",
                            Constant::Units::C,
                            loop.OutNodeTemperature,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);

        SetupOutputVariable(state,
                            "Plant Supply Side Not Distributed Demand Rate",
                            Constant::Units::W,
                            loop.DemandNotDispatched,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Plant Supply Side Unmet Demand Rate",
                            Constant::Units::W,
                            loop.UnmetDemand,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Debug Plant Loop Bypass Fraction",
                            Constant::Units::None,
                            loop.BypassFrac,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
        SetupOutputVariable(state,
                            "Debug Plant Last Simulated Loop Side",
                            Constant::Units::None,
                            loop.LastLoopSideSimulated,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Average,
                            state.dataPlnt->PlantLoop(LoopNum).Name);
    }

    // setup more variables inside plant data structure
    if (state.dataGlobal->DisplayAdvancedReportVariables) {
        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Temperature",
                                Constant::Units::C,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).LoopSideInlet_TankTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Temperature",
                                Constant::Units::C,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).LoopSideInlet_TankTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Heat Transport Rate",
                                Constant::Units::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).LoopSideInlet_MdotCpDeltaT,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Heat Transport Rate",
                                Constant::Units::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).LoopSideInlet_MdotCpDeltaT,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Heat Storage Rate",
                                Constant::Units::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).LoopSideInlet_McpDTdt,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Heat Storage Rate",
                                Constant::Units::W,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).LoopSideInlet_McpDTdt,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Demand Side Lumped Capacitance Excessive Storage Time",
                                Constant::Units::hr,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).LoopSideInlet_CapExcessStorageTimeReport,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            SetupOutputVariable(state,
                                "Plant Supply Side Lumped Capacitance Excessive Storage Time",
                                Constant::Units::hr,
                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).LoopSideInlet_CapExcessStorageTimeReport,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                state.dataPlnt->PlantLoop(LoopNum).Name);
            for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
                for (int BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    for (int CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents;
                         ++CompNum) {
                        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType !=
                            OpScheme::Demand) {
                            SetupOutputVariable(state,
                                                "Plant Component Distributed Demand Rate",
                                                Constant::Units::W,
                                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).MyLoad,
                                                OutputProcessor::TimeStepType::System,
                                                OutputProcessor::StoreType::Average,
                                                state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).Name);
                        }
                    }
                }
            }
        }
    }

    auto &dln = state.dataLoopNodes;
    // now traverse plant loops and set fluid type index in all nodes on the loop
    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        int FluidIndex = state.dataPlnt->PlantLoop(LoopNum).FluidIndex;
        for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
            auto &loopSide = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum);
            
            dln->nodes(loopSide.InNodeNum)->FluidIndex = FluidIndex;
            dln->nodes(loopSide.OutNodeNum)->FluidIndex = FluidIndex;
            for (int BranchNum = 1; BranchNum <= loopSide.TotalBranches; ++BranchNum) {
                for (int CompNum = 1; CompNum <= loopSide.Branch(BranchNum).TotalComponents; ++CompNum) {
                    dln->nodes(loopSide.Branch(BranchNum).Comp(CompNum).InNodeNum)->FluidIndex = FluidIndex;
                    dln->nodes(loopSide.Branch(BranchNum).Comp(CompNum).OutNodeNum)->FluidIndex = FluidIndex;
                }
            }
        }
    } // plant loops

    // Plant topology report
    // auto &orp = state.dataOutRptPredefined;
    // MJW ToDo: Tabular report inputs haven't been read yet - move that or just skip checking if the report is requested?
    // if (orp->reportName(orp->pdrTopology).show) {

    // constexpr std::string_view plantLoop = "PlantLoop";
    int rowCounter = 1;
    for (int loopNum = 1; loopNum <= state.dataPlnt->TotNumLoops; ++loopNum) {
        fillPlantCondenserTopology(state, state.dataPlnt->PlantLoop(loopNum), rowCounter);
    }

    // constexpr std::string_view condenserLoop = "CondenserLoop";
    // for (int loopNum = state.dataHVACGlobal->NumPlantLoops + 1; loopNum <= (state.dataHVACGlobal->NumPlantLoops +
    // state.dataHVACGlobal->NumCondLoops);
    //      ++loopNum) {
    //     fillPlantCondenserTopology(state, condenserLoop, state.dataPlnt->PlantLoop(loopNum));
    // }
    // }
}

void fillPlantCondenserTopology(EnergyPlusData &state, DataPlant::PlantLoopData &thisLoop, int &rowCounter)
{
    auto &orp = state.dataOutRptPredefined;
    // int repOffset = 0;
    // if (thisLoop.TypeOfLoop == DataPlant::LoopType::Condenser) {
    //     // Shift column pointers for condenser loop subtable
    //     repOffset = orp->pdchTopCondCompType - orp->pdchTopPlantCompType;
    // }
    std::string_view const loopType = DataPlant::loopTypeNames[static_cast<int>(thisLoop.TypeOfLoop)];
    for (DataPlant::LoopSideLocation LoopSideNum : {DataPlant::LoopSideLocation::Supply, DataPlant::LoopSideLocation::Demand}) {
        auto &thisLoopSide = thisLoop.LoopSide(LoopSideNum);
        std::string_view const loopSide = DataPlant::DemandSupplyNames[static_cast<int>(LoopSideNum)];

        // s->pdstTopPlantLoop2 = newPreDefSubTable(state, s->pdrTopology, "Plant Loop Component Arrangement");
        // s->pdchTopPlantLoopType2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Loop Type");
        // s->pdchTopPlantLoopName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Loop Name");
        // s->pdchTopPlantSide2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Side");
        // s->pdchTopPlantSplitName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Splitter Name");
        // s->pdchTopPlantBranchName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Branch Name");
        // s->pdchTopPlantCompType2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Component Type");
        // s->pdchTopPlantCompName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Component Name");
        // s->pdchTopPlantMixName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Mixer Name");

        OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopType2, format("{}", rowCounter), loopType);
        OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopName2, format("{}", rowCounter), thisLoop.Name);
        OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantSide2, format("{}", rowCounter), loopSide);
        ++rowCounter;

        // Report for first branch
        auto &thisBranch = thisLoopSide.Branch(1);
        constexpr std::string_view branch = "Branch";

        for (int compNum = 1; compNum <= thisBranch.TotalComponents; ++compNum) {
            auto &thisComp = thisBranch.Comp(compNum);
            fillPlantToplogyComponentRow2(state, loopType, thisLoop.Name, loopSide, thisBranch.Name, thisComp.TypeOf, thisComp.Name, rowCounter);
        }

        if (thisLoopSide.TotalBranches >= 3) {
            // parallel branches
            for (int branchNum = 2; branchNum <= thisLoopSide.TotalBranches - 1; ++branchNum) {
                auto &thisBranch = thisLoopSide.Branch(branchNum);
                // splitter
                if (thisLoopSide.Splitter.Exists) {
                    fillPlantToplogySplitterRow2(state, loopType, thisLoop.Name, loopSide, thisLoopSide.Splitter.Name, rowCounter);
                }

                for (int compNum = 1; compNum <= thisBranch.TotalComponents; ++compNum) {
                    auto &thisComp = thisBranch.Comp(compNum);
                    // fillPlantToplogyRow(state, thisComp.Name, thisComp.TypeOf, loopSide, branch, thisBranch.Name, thisLoop.FluidName, repOffset);
                    fillPlantToplogyComponentRow2(
                        state, loopType, thisLoop.Name, loopSide, thisBranch.Name, thisComp.TypeOf, thisComp.Name, rowCounter);
                }
                // mixer
                if (thisLoopSide.Mixer.Exists) {
                    rowCounter -= 1;
                    constexpr std::string_view mixer = "Mixer";
                    fillPlantToplogyMixerRow2(state, loopType, thisLoop.Name, loopSide, thisLoopSide.Mixer.Name, rowCounter);
                    rowCounter += 1;
                }
            }

            // Outlet Branch
            auto &thisBranch = thisLoopSide.Branch(thisLoopSide.TotalBranches);
            for (int compNum = 1; compNum <= thisBranch.TotalComponents; ++compNum) {
                auto &thisComp = thisBranch.Comp(compNum);
                fillPlantToplogyComponentRow2(state, loopType, thisLoop.Name, loopSide, thisBranch.Name, thisComp.TypeOf, thisComp.Name, rowCounter);
            }
        }
    }
}

void fillPlantToplogySplitterRow2(EnergyPlusData &state,
                                  const std::string_view &loopType,
                                  const std::string_view &loopName,
                                  const std::string_view &side,
                                  const std::string_view &splitterName,
                                  int &rowCounter)
{
    auto &orp = state.dataOutRptPredefined;
    // s->pdchTopPlantSplitName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Splitter Name");
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopType2, format("{}", rowCounter), loopType);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopName2, format("{}", rowCounter), loopName);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantSide2, format("{}", rowCounter), side);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantSplitName2, format("{}", rowCounter), splitterName);
}
void fillPlantToplogyMixerRow2(EnergyPlusData &state,
                               const std::string_view &loopType,
                               const std::string_view &loopName,
                               const std::string_view &side,
                               const std::string_view &mixerName,
                               int &rowCounter)
{
    auto &orp = state.dataOutRptPredefined;
    // s->pdchTopPlantMixName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Mixer Name");
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopType2, format("{}", rowCounter), loopType);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopName2, format("{}", rowCounter), loopName);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantSide2, format("{}", rowCounter), side);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantMixName2, format("{}", rowCounter), mixerName);
}
void fillPlantToplogyComponentRow2(EnergyPlusData &state,
                                   const std::string_view &loopType,
                                   const std::string_view &loopName,
                                   const std::string_view &side,
                                   const std::string_view &branchName,
                                   const std::string_view &compType,
                                   const std::string_view &compName,
                                   int &rowCounter)
{
    auto &orp = state.dataOutRptPredefined;
    // s->pdchTopPlantBranchName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Branch Name");
    // s->pdchTopPlantCompType2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Component Type");
    // s->pdchTopPlantCompName2 = newPreDefColumn(state, s->pdstTopPlantLoop2, "Component Name");
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopType2, format("{}", rowCounter), loopType);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantLoopName2, format("{}", rowCounter), loopName);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantSide2, format("{}", rowCounter), side);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantBranchName2, format("{}", rowCounter), branchName);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantCompType2, format("{}", rowCounter), compType);
    OutputReportPredefined::PreDefTableEntry(state, orp->pdchTopPlantCompName2, format("{}", rowCounter), compName);
    ++rowCounter;
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
    DataPlant::LoopSideLocation LoopSideNum;
    int BranchNum; // branch loop counter
    int CompNum;   // plant side component counter
    int SensedNode;

    bool ErrorsFound(false);
    bool FinishSizingFlag;

    int HalfLoopNum;
    int passNum;
    
    auto &dln = state.dataLoopNodes;
            
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
                auto const *sensedNode = dln->nodes(SensedNode);
                if (sensedNode->TempSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state,
                                        format("PlantManager: No Setpoint Manager Defined for Node={} in PlantLoop={}",
                                               sensedNode->Name, state.dataPlnt->PlantLoop(LoopNum).Name));
                        ShowContinueError(state, "Add Temperature Setpoint Manager with Control Variable = \"Temperature\" for this PlantLoop.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // need call to EMS to check node
                        CheckIfNodeSetPointManagedByEMS(state, SensedNode, HVAC::CtrlVarType::Temp, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state,
                                            format("PlantManager: No Setpoint Manager Defined for Node={} in PlantLoop={}",
                                                   sensedNode->Name, state.dataPlnt->PlantLoop(LoopNum).Name));
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

        SetAllFlowLocks(state, DataPlant::FlowLock::Unlocked);
        FinishSizingFlag = false;
        state.dataPlnt->PlantFirstSizesOkayToFinalize = false; // set global flag for when it ready to store final sizes
        state.dataPlnt->PlantFirstSizesOkayToReport = false;
        state.dataPlnt->PlantFinalSizesOkayToReport = false;
        state.dataPlantMgr->GetCompSizFac = true;
        for (passNum = 1; passNum <= 4; ++passNum) { // begin while loop to iterate over the next calls sequentially

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
                            .initLoopEquip(state, state.dataPlantMgr->GetCompSizFac);
                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).simulate(state, FirstHVACIteration);
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
                if (LoopSideNum == LoopSideLocation::Supply) {
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
            if (LoopSideNum == LoopSideLocation::Supply) {
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
                        .initLoopEquip(state, state.dataPlantMgr->GetCompSizFac);
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).simulate(state, FirstHVACIteration);
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
                        .initLoopEquip(state, state.dataPlantMgr->GetCompSizFac);

                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).simulate(state, FirstHVACIteration);
                } //-CompNum
            }     //-BranchNum
        }

        // reset loop level
        state.dataPlnt->PlantFinalSizesOkayToReport = true;
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            ResizePlantLoopLevelSizes(state, LoopNum);
        }

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
                        .initLoopEquip(state, state.dataPlantMgr->GetCompSizFac);
                    state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).simulate(state, FirstHVACIteration);
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
            // check if setpoints being placed on node properly
            auto &loop = state.dataPlnt->PlantLoop(LoopNum);
            if (loop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand) {
                auto *tempSetPointNode = dln->nodes(loop.TempSetPointNodeNum);
                if (tempSetPointNode->TempSetPointHi == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state, "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme");
                        ShowContinueError(state, format("Node Referenced ={}", tempSetPointNode->Name));
                        ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        CheckIfNodeSetPointManagedByEMS(state,
                                                        loop.TempSetPointNodeNum,
                                                        HVAC::CtrlVarType::Temp,
                                                        state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state, "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme");
                            ShowContinueError(state, format("Node Referenced ={}", tempSetPointNode->Name));
                            ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                            ShowContinueError(state, "Or add EMS Actuator for Temperature Maximum Setpoint");

                        } // SetPointErrorFlag
                    }     // Not EMS
                }         // Node TSPhi = Sensed
                
                if (tempSetPointNode->TempSetPointLo == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state, "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme");
                        ShowContinueError(state, format("Node Referenced ={}", tempSetPointNode->Name));
                        ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        CheckIfNodeSetPointManagedByEMS(state,
                                                        loop.TempSetPointNodeNum,
                                                        HVAC::CtrlVarType::Temp,
                                                        state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state, "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme");
                            ShowContinueError(state, format("Node Referenced ={}", tempSetPointNode->Name));
                            ShowContinueError(state, "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints");
                            ShowContinueError(state, "Or add EMS Actuator for Temperature Minimum Setpoint");

                        } // SetPointErrorFlag
                    }     // NOT EMS
                }         // Node TSPtLo = Sensed...
            }             // LoopDemandScheme = DualSPDB
        }                 // PLANT LOOP

        // Any per-environment load distribution init should be OK here
        // Just clear away any trailing MyLoad for now...
        // This could likely be moved into InitLoadDistribution also...
        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
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
    Real64 constexpr StartQuality(1.0);
    Real64 constexpr StartHumRat(0.0);
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
    int BranchNum;                    // branch loop counter
    int OpNum;                        // operation scheme counter
    int CompNum;                      // plant side component counter

    Real64 LoopMinMassFlowRate; // minimum allowable loop mass flow rate
    Real64 SteamDensity;
    Real64 SteamTemp;
    Real64 StartEnthalpy;
    Real64 Cp;
    Real64 rho;
    Real64 LoopSetPointTemperatureHi;
    Real64 LoopSetPointTemperatureLo;

    auto &dln = state.dataLoopNodes;
    
    //*****************************************************************
    // BEGIN ENVIRONMENT INITS
    //*****************************************************************

    if (state.dataPlantMgr->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {

        for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            auto &loop = state.dataPlnt->PlantLoop(LoopNum);
            auto const *tempSetPointNode = dln->nodes(loop.TempSetPointNodeNum);
            for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
                switch (loop.LoopDemandCalcScheme) {
                case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                    LoopSetPointTemp = tempSetPointNode->TempSetPoint;
                } break;
                case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                    // Get the range of setpoints
                    LoopSetPointTemperatureHi = tempSetPointNode->TempSetPointHi;
                    LoopSetPointTemperatureLo = tempSetPointNode->TempSetPointLo;
                    LoopSetPointTemp = (LoopSetPointTemperatureLo + LoopSetPointTemperatureHi) / 2.0;
                } break;
                default:
                    break;
                }

                if ((loop.CommonPipeType == DataPlant::CommonPipeType::TwoWay) &&
                    (LoopSideNum == LoopSideLocation::Demand) &&
                    (loop.LoopSide(LoopSideLocation::Demand).InNodeSetPt)) { // get a second setpoint for
                                                                                                              // secondaryLoop
                    // if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
                    // we want to initialize the demand side of the loop using that setpoint
                    LoopSetPointTemp = dln->nodes(loop.LoopSide(LoopSideLocation::Demand).InNodeNum)->TempSetPoint;
                }

                // Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
                LoopMaxTemp = loop.MaxTemp;
                LoopMinTemp = loop.MinTemp;

                // trap for -999 and set to average of limits if so
                if (LoopSetPointTemp == SensedNodeFlagValue) {
                    LoopSetPointTemp = (LoopMinTemp + LoopMaxTemp) / 2.0;
                }
                // Check it against the loop temperature limits
                LoopSetPointTemp = min(LoopMaxTemp, LoopSetPointTemp);
                LoopSetPointTemp = max(LoopMinTemp, LoopSetPointTemp);

                // Initialize the capacitance model at the tank interface, and other loop side values
                loop.LoopSide(LoopSideNum).TempInterfaceTankOutlet = LoopSetPointTemp;
                loop.LoopSide(LoopSideNum).LastTempInterfaceTankOutlet = LoopSetPointTemp;
                loop.LoopSide(LoopSideNum).LoopSideInlet_TankTemp = LoopSetPointTemp;
                loop.LoopSide(LoopSideNum).TotalPumpHeat = 0.0;
                if (allocated(loop.LoopSide(LoopSideNum).Pumps))
                    for (auto &e : loop.LoopSide(LoopSideNum).Pumps)
                        e.PumpHeatToFluid = 0.0;
                loop.LoopSide(LoopSideNum).FlowRequest = 0.0;
                loop.LoopSide(LoopSideNum).TimeElapsed = 0.0;
                loop.LoopSide(LoopSideNum).FlowLock = DataPlant::FlowLock::Unlocked;
                loop.LoopSide(LoopSideNum).InNode.TemperatureHistory = 0.0;
                loop.LoopSide(LoopSideNum).InNode.MassFlowRateHistory = 0.0;
                loop.LoopSide(LoopSideNum).OutNode.TemperatureHistory = 0.0;
                loop.LoopSide(LoopSideNum).OutNode.MassFlowRateHistory = 0.0;

                if (loop.fluidType != Node::FluidType::Steam) {
                    Cp = GetSpecificHeatGlycol(state,
                                               loop.FluidName,
                                               LoopSetPointTemp,
                                               loop.FluidIndex,
                                               RoutineNameAlt);
                    StartEnthalpy = Cp * LoopSetPointTemp;
                }
                // Use Min/Max flow rates to initialize loop
                if (loop.fluidType == Node::FluidType::Water) {
                    rho = GetDensityGlycol(state,
                                           loop.FluidName,
                                           LoopSetPointTemp,
                                           loop.FluidIndex,
                                           RoutineNameAlt);

                    LoopMaxMassFlowRate = loop.MaxVolFlowRate * rho;
                    LoopMinMassFlowRate = loop.MinVolFlowRate * rho;
                }
                // use saturated liquid of steam at the loop setpoint temp as the starting enthalpy for a water loop
                if (loop.fluidType == Node::FluidType::Steam) {
                    SteamTemp = 100.0;
                    SteamDensity =
                        GetSatDensityRefrig(state, fluidNameSteam, SteamTemp, 1.0, loop.FluidIndex, RoutineName);
                    LoopMaxMassFlowRate = loop.MaxVolFlowRate * SteamDensity;
                    StartEnthalpy = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, LoopSetPointTemp, 0.0, loop.FluidIndex, RoutineName);
                    LoopMinMassFlowRate = loop.MinVolFlowRate * SteamDensity;
                }

                LoopMaxMassFlowRate = max(0.0, LoopMaxMassFlowRate);
                LoopMinMassFlowRate = max(0.0, LoopMinMassFlowRate);

                // Initial all loop nodes by initializing all component inlet and outlet nodes
                for (BranchNum = 1; BranchNum <= loop.LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                    auto &branch = loop.LoopSide(LoopSideNum).Branch(BranchNum);
                    auto *branchInNode = dln->nodes(branch.InNodeNum);
                    for (CompNum = 1; CompNum <= branch.TotalComponents; ++CompNum) {
                        auto &comp = branch.Comp(CompNum);
                        auto *compInNode = dln->nodes(comp.InNodeNum);
                        auto *compOutNode = dln->nodes(comp.OutNodeNum);

                        compInNode->Temp = LoopSetPointTemp;
                        compInNode->TempMin = LoopMinTemp;
                        compInNode->TempMax = LoopMaxTemp;
                        compInNode->TempLastTimestep = LoopSetPointTemp;

                        compInNode->MassFlowRate = 0.0;
                        comp.MyLoad = 0.0;
                        comp.Available = false;
                        comp.FreeCoolCntrlShutDown = false;
                        branch.RequestedMassFlow = 0.0;

                        if (compInNode->MassFlowRateMin > 0.0) {
                            compInNode->MassFlowRateMinAvail = compInNode->MassFlowRateMin;
                        } else {
                            compInNode->MassFlowRateMin = LoopMinMassFlowRate;
                            compInNode->MassFlowRateMinAvail = LoopMinMassFlowRate;
                        }

                        if (compInNode->MassFlowRateMax > 0.0) {
                            compInNode->MassFlowRateMaxAvail = compInNode->MassFlowRateMax;
                        } else {
                            compInNode->MassFlowRateMax = LoopMaxMassFlowRate;
                            compInNode->MassFlowRateMaxAvail = LoopMaxMassFlowRate;
                        }

                        compInNode->MassFlowRateRequest = 0.0;
                        compInNode->Quality = StartQuality;
                        compInNode->Press = state.dataEnvrn->StdBaroPress;
                        compInNode->Enthalpy = StartEnthalpy;
                        compInNode->HumRat = StartHumRat;

                        compOutNode->fluidType = branchInNode->fluidType;
                        compOutNode->Temp = branchInNode->Temp;
                        compOutNode->TempMin = branchInNode->TempMin;
                        compOutNode->TempMax = branchInNode->TempMax;
                        compOutNode->TempLastTimestep = branchInNode->TempLastTimestep;
                        compOutNode->MassFlowRate = branchInNode->MassFlowRate;
                        compOutNode->MassFlowRateMin = branchInNode->MassFlowRateMin;
                        compOutNode->MassFlowRateMax = branchInNode->MassFlowRateMax;
                        compOutNode->MassFlowRateMinAvail = branchInNode->MassFlowRateMinAvail;
                        compOutNode->MassFlowRateMaxAvail = branchInNode->MassFlowRateMaxAvail;
                        compOutNode->MassFlowRateRequest = 0.0;
                        compOutNode->Quality = StartQuality;
                        compOutNode->Press = state.dataEnvrn->StdBaroPress;
                        compOutNode->Enthalpy = StartEnthalpy;
                        compOutNode->HumRat = StartHumRat;
                    } // COMPONENT LOOP
                }     // BRANCH LOOP
            }         // LOOPSIDE
        }             // PLANT LOOP
        for (auto &loop : state.dataPlnt->PlantLoop) {
            loop.CoolingDemand = 0.0;
            loop.HeatingDemand = 0.0;
            loop.DemandNotDispatched = 0.0;
            loop.UnmetDemand = 0.0;
            loop.LastLoopSideSimulated = static_cast<int>(DataPlant::LoopSideLocation::Invalid);
            loop.InNodeFlowrate = 0.0;
            loop.InNodeTemperature = 0.0;
            loop.OutNodeFlowrate = 0.0;
            loop.OutNodeTemperature = 0.0;
        }

        state.dataPlantMgr->MyEnvrnFlag = false;
        //*****************************************************************
        // END OF ENVIRONMENT INITS
        //*****************************************************************
    }

    if (!state.dataGlobal->BeginEnvrnFlag) state.dataPlantMgr->MyEnvrnFlag = true;

    // FirstHVACiteration inits
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        auto &loop = state.dataPlnt->PlantLoop(LoopNum);
        auto const *tempSetPointNode = dln->nodes(loop.TempSetPointNodeNum);
        LoopSetPointTemp = tempSetPointNode->TempSetPoint;

        // Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
        LoopMaxTemp = loop.MaxTemp;
        LoopMinTemp = loop.MinTemp;
        // Check it against the loop temperature limits
        LoopSetPointTemp = min(LoopMaxTemp, LoopSetPointTemp);
        LoopSetPointTemp = max(LoopMinTemp, LoopSetPointTemp);

        // Update supply side loop setpoint in plant data structure
        loop.LoopSide(LoopSideLocation::Supply).TempSetPoint = LoopSetPointTemp;
        loop.LoopSide(LoopSideLocation::Demand).TempSetPoint = LoopSetPointTemp;

        // Update supply side hi-lo setpoints for dual SP control
        if (loop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand) {
            LoopSetPointTempHi = tempSetPointNode->TempSetPointHi;
            LoopSetPointTempLo = tempSetPointNode->TempSetPointLo;
            LoopSetPointTempHi = min(LoopMaxTemp, LoopSetPointTempHi);
            LoopSetPointTempHi = max(LoopMinTemp, LoopSetPointTempHi);
            LoopSetPointTempLo = min(LoopMaxTemp, LoopSetPointTempLo);
            LoopSetPointTempLo = max(LoopMinTemp, LoopSetPointTempLo);
            loop.LoopSide(LoopSideLocation::Supply).TempSetPointHi = LoopSetPointTempHi;
            loop.LoopSide(LoopSideLocation::Supply).TempSetPointLo = LoopSetPointTempLo;
        }

        // update demand side loop setpoint in plant data structure
        if (loop.CommonPipeType == DataPlant::CommonPipeType::TwoWay) { // get a second setpoint for secondaryLoop
            // if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
            // we want to initialize the demand side of the loop using that setpoint
            if (loop.LoopSide(LoopSideLocation::Demand).InNodeSetPt) {
                    auto const *inNode = dln->nodes(loop.LoopSide(LoopSideLocation::Demand).InNodeNum);
                SecondaryLoopSetPointTemp = inNode->TempSetPoint;
                SecondaryLoopSetPointTemp = min(LoopMaxTemp, SecondaryLoopSetPointTemp);
                SecondaryLoopSetPointTemp = max(LoopMinTemp, SecondaryLoopSetPointTemp);
                loop.LoopSide(LoopSideLocation::Demand).TempSetPoint = SecondaryLoopSetPointTemp;
                // Since Dual setpoint not explicitly available for demand side, we can't do the
                // bounding check on hi/lo setpoint.  IF we did we would over-write
                // the SensedNodeFlagValue of -999 for no dual setpoint case.
                loop.LoopSide(LoopSideLocation::Demand).TempSetPointHi = inNode->TempSetPointHi;
                loop.LoopSide(LoopSideLocation::Demand).TempSetPointLo = inNode->TempSetPointLo;
            }

            // initialize common pipe flows to zero.
            if (allocated(state.dataHVACInterfaceMgr->PlantCommonPipe)) {
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).PriToSecFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).SecToPriFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).PriCPLegFlow = 0.0;
                state.dataHVACInterfaceMgr->PlantCommonPipe(LoopNum).SecCPLegFlow = 0.0;
            }
        } else { // no secondary loop, so use supply side loop SP on demand side too.
            loop.LoopSide(LoopSideLocation::Demand).TempSetPoint = LoopSetPointTemp;
            if (loop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand) {
                loop.LoopSide(LoopSideLocation::Demand).TempSetPointHi = LoopSetPointTempHi;
                loop.LoopSide(LoopSideLocation::Demand).TempSetPointLo = LoopSetPointTempLo;
            }
        }

        for (DataPlant::LoopSideLocation LoopSideNum : DataPlant::LoopSideKeys) {
            for (BranchNum = 1; BranchNum <= loop.LoopSide(LoopSideNum).TotalBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= loop.LoopSide(LoopSideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    auto const &comp = loop.LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum);
                    auto *compInNode = dln->nodes(comp.InNodeNum);
                    auto *compOutNode = dln->nodes(comp.OutNodeNum);

                    // reinit to node hardware limits
                    compInNode->MassFlowRateMinAvail = compInNode->MassFlowRateMin;
                    compOutNode->MassFlowRateMinAvail = compInNode->MassFlowRateMin;
                    compInNode->MassFlowRateMaxAvail = compInNode->MassFlowRateMax;
                    compOutNode->MassFlowRateMaxAvail = compInNode->MassFlowRateMax;

                    compInNode->MassFlowRateRequest = 0.0;
                    compOutNode->MassFlowRateRequest = 0.0;
                }
            }
        }

        for (OpNum = 1; OpNum <= loop.NumOpSchemes; ++OpNum) {
            // If the operating scheme is scheduled "OFF", go to next scheme
            loop.OpScheme(OpNum).Available =
                GetCurrentScheduleValue(state, loop.OpScheme(OpNum).SchedPtr) > 0.0;
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
    auto &dln = state.dataLoopNodes;
        
    for (auto *e : dln->nodes) { // MA
        e->TempLastTimestep = e->Temp;
        e->EnthalpyLastTimestep = e->Enthalpy;
    }

    if (state.dataPlnt->TotNumLoops > 0 && !state.dataGlobal->WarmupFlag) {
        for (auto &loop : state.dataPlnt->PlantLoop) {
            for (auto &side : loop.LoopSide) {
                if (loop.OutNodeFlowrate > HVAC::SmallMassFlow) {
                    // Accumulate total time loop is active
                    side.LoopSideInlet_TotalTime += state.dataHVACGlobal->TimeStepSys;
                    // Determine excessive storage - if both are moving in the same direction and McpDTdt is larger than MdotCpDeltaT
                    if ((std::abs(side.LoopSideInlet_MdotCpDeltaT) > HVAC::SmallLoad) &&
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
    bool ActiveCntrlfound; // used to search for active control branches in parallel with bypass branches
    int ParalBranchNum;    // used to search for active control branches in parallel with bypass branches
    int ParalBranchNum2;   // used to search for active control branches in parallel with bypass branches
    int BranchNum2;        // used to search for active control branches in parallel with bypass branches
    int numLoopSides;
    int BranchNum; // DO loop counter for branches
    int CompNum;   // do loop for multiple components on a branch
    bool ShouldBeACTIVE;

    if (!(state.dataErrTracking->AskForPlantCheckOnAbort)) {
        return;
    }

    if (state.dataPlnt->TotNumLoops <= 0) return;
    if (!(allocated(state.dataPlnt->PlantLoop))) return;

    for (auto &loop : state.dataPlnt->PlantLoop) { 
        numLoopSides = 2;
        for (DataPlant::LoopSideLocation SideNum : DataPlant::LoopSideKeys) {
            if (!loop.LoopSide(SideNum).Splitter.Exists) continue;

            for (ParalBranchNum = 1; ParalBranchNum <= loop.LoopSide(SideNum).Splitter.NumOutNodes;
                 ++ParalBranchNum) {
                BranchNum = loop.LoopSide(SideNum).Splitter.BranchNumOut(ParalBranchNum);
                if (loop.LoopSide(SideNum).Branch(BranchNum).IsBypass) { // we know there is a bypass
                    // check that there is at least one 'Active' control type in parallel with bypass branch
                    ActiveCntrlfound = false;
                    for (ParalBranchNum2 = 1; ParalBranchNum2 <= loop.LoopSide(SideNum).Splitter.NumOutNodes;
                         ++ParalBranchNum2) {
                        BranchNum2 = loop.LoopSide(SideNum).Splitter.BranchNumOut(ParalBranchNum2);
                        if (loop.LoopSide(SideNum).Branch(BranchNum2).controlType ==
                            DataBranchAirLoopPlant::ControlType::Active) {
                            ActiveCntrlfound = true;
                        }
                    }
                    if (!(ActiveCntrlfound)) {
                        ShowWarningError(state,
                                         "Check control types on branches between splitter and mixer in PlantLoop=" +
                                             loop.Name);
                        ShowContinueError(state, "Found a BYPASS branch with no ACTIVE branch in parallel with it");
                        ShowContinueError(state, "In certain (but not all) situations, this can cause problems; please verify your inputs");
                        ShowContinueError(state,
                                          "Bypass branch named: " + loop.LoopSide(SideNum).Branch(BranchNum).Name);
                    }
                } // bypass present

                // check for possible components on demand side that should be ACTIVE but are not
                if (SideNum == LoopSideLocation::Demand) {
                    // check for presences of the following components whose branch control type should be active
                    // WATER HEATER:MIXED
                    // WATER HEATER:STRATIFIED
                    // WATER USE CONNECTIONS
                    // COIL:WATER:COOLING
                    // COIL:WATER:SIMPLEHEATING
                    // COIL:STEAM:AIRHEATING
                    // SOLAR COLLECTOR:FLAT PLATE
                    // PLANT LOAD PROFILE
                    for (CompNum = 1; CompNum <= loop.LoopSide(SideNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                        ShouldBeACTIVE = false;
                        switch (loop.LoopSide(SideNum).Branch(BranchNum).Comp(CompNum).Type) {
                        case DataPlant::PlantEquipmentType::WtrHeaterMixed:
                        case DataPlant::PlantEquipmentType::WtrHeaterStratified:
                        case DataPlant::PlantEquipmentType::WaterUseConnection:
                        case DataPlant::PlantEquipmentType::CoilWaterCooling:
                        case DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling:
                        case DataPlant::PlantEquipmentType::CoilWaterSimpleHeating:
                        case DataPlant::PlantEquipmentType::CoilSteamAirHeating:
                        case DataPlant::PlantEquipmentType::SolarCollectorFlatPlate:
                        case DataPlant::PlantEquipmentType::PlantLoadProfile: {
                            ShouldBeACTIVE = true;
                        } break;
                        default: {
                            // not a demand side component that we know needs to be active, do nothing
                        } break;
                        }

                        if (ShouldBeACTIVE) {
                            switch (loop.LoopSide(SideNum).Branch(BranchNum).controlType) {
                            case DataBranchAirLoopPlant::ControlType::Invalid: {
                                ShowWarningError(state,
                                                 "Found potential problem with Control Type for Branch named: " +
                                                     loop.LoopSide(SideNum).Branch(BranchNum).Name);
                                ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type unknown");
                            } break;
                            case DataBranchAirLoopPlant::ControlType::Active: {
                                // do nothing, this is correct control type.
                            } break;
                            case DataBranchAirLoopPlant::ControlType::Passive: {
                                ShowWarningError(state,
                                                 "Found potential problem with Control Type for Branch named: " +
                                                     loop.LoopSide(SideNum).Branch(BranchNum).Name);
                                ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type PASSIVE");
                            } break;
                            case DataBranchAirLoopPlant::ControlType::SeriesActive: {
                                // do nothing, should be okay. (? don't really understand SeriesActive though)
                            } break;
                            case DataBranchAirLoopPlant::ControlType::Bypass: {
                                ShowWarningError(state,
                                                 "Found potential problem with Control Type for Branch named: " +
                                                     loop.LoopSide(SideNum).Branch(BranchNum).Name);
                                ShowContinueError(state, "This branch should (probably) be ACTIVE but has control type Bypass");
                            } break;
                            default:
                                break;
                            }
                        } // should be active
                    }     // comp num loop
                }         // demand side

            } // splitter outlet nodes

            // check to see if bypass exists in demand side. If not warn error of possible flow problems
            if (!loop.LoopSide(SideNum).BypassExists) {
                if (SideNum == LoopSideLocation::Demand) {
                    ShowWarningError(state,
                                     "There is no BYPASS component in the demand-side of PlantLoop =" + loop.Name);
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

    auto &loop = state.dataPlnt->PlantLoop(LoopNum);
    if (loop.PlantSizNum == 0) {
        if (state.dataSize->NumPltSizInput > 0) {
            PlantSizNum =
                Util::FindItemInList(loop.Name, state.dataSize->PlantSizData, &PlantSizingData::PlantLoopName);
            if (PlantSizNum > 0) {
                loop.PlantSizNum = PlantSizNum;
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

    auto &loop = state.dataPlnt->PlantLoop(LoopNum);
    
    if (loop.PlantSizNum > 0) {
        PlantSizNum = loop.PlantSizNum;
        // PlantSizData(PlantSizNum)%DesVolFlowRate = 0.0D0 ! DSU2
    } else {
        if (state.dataSize->NumPltSizInput > 0) {
            PlantSizNum =
                Util::FindItemInList(loop.Name, state.dataSize->PlantSizData, &PlantSizingData::PlantLoopName);
        }
    }
    loop.PlantSizNum = PlantSizNum;
    // calculate a loop sizing factor and a branch sizing factor. Note that components without a sizing factor
    // are assigned sizing factors of zero in this calculation
    if (PlantSizNum > 0) {
        auto &supplySide = loop.LoopSide(LoopSideLocation::Supply);
        if (state.dataPlantMgr->GetCompSizFac) {
            for (BranchNum = 1; BranchNum <= supplySide.TotalBranches; ++BranchNum) {
                BranchSizFac = 0.0;
                supplySide.Branch(BranchNum).PumpSizFac = 1.0;
                if (supplySide.InNodeNum ==
                    supplySide.Branch(BranchNum).InNodeNum)
                    continue;
                if (supplySide.OutNodeNum ==
                    supplySide.Branch(BranchNum).OutNodeNum)
                    continue;
                for (CompNum = 1; CompNum <= supplySide.Branch(BranchNum).TotalComponents;
                     ++CompNum) {
                    supplySide.Branch(BranchNum).Comp(CompNum).simulate(state, true);
                    BranchSizFac = max(BranchSizFac,
                                       supplySide.Branch(BranchNum).Comp(CompNum).SizFac);
                }
                LoopSizFac += BranchSizFac;
                MaxSizFac = max(MaxSizFac, BranchSizFac);
                if (BranchSizFac > 0.0) {
                    supplySide.Branch(BranchNum).PumpSizFac = BranchSizFac;
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
            for (BranchNum = 1; BranchNum <= supplySide.TotalBranches; ++BranchNum) {
                if (supplySide.InNodeNum ==
                    supplySide.Branch(BranchNum).InNodeNum) {
                    supplySide.Branch(BranchNum).PumpSizFac = PlantSizFac;
                }
                if (supplySide.OutNodeNum ==
                    supplySide.Branch(BranchNum).OutNodeNum) {
                    supplySide.Branch(BranchNum).PumpSizFac = PlantSizFac;
                }
            }
        }

        // sum up contributions from CompDesWaterFlow, demand side size request (non-coincident)
        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate = 0.0; // init for summation
        for (BranchNum = 1; BranchNum <= loop.LoopSide(LoopSideLocation::Demand).TotalBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= loop.LoopSide(LoopSideLocation::Demand).Branch(BranchNum).TotalComponents;
                 ++CompNum) {
                SupNodeNum = loop.LoopSide(LoopSideLocation::Demand).Branch(BranchNum).Comp(CompNum).InNodeNum;
                for (WaterCompNum = 1; WaterCompNum <= state.dataSize->SaveNumPlantComps; ++WaterCompNum) {
                    if (SupNodeNum == state.dataSize->CompDesWaterFlow(WaterCompNum).SupNode) {
                        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate += state.dataSize->CompDesWaterFlow(WaterCompNum).DesVolFlowRate;
                    }
                }
            }
        }

        if (!loop.MaxVolFlowRateWasAutoSized && (loop.MaxVolFlowRate > 0.0)) {
            // if the user puts in a large throwaway value for hard max plant loop size, they may not want this affecting anything else.
            //  but if they put in a smaller value, then it should cap the design size, so use hard value if it is smaller than non-coincident
            //  result
            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate =
                std::min(state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate, loop.MaxVolFlowRate);
        }
    }

    if (loop.MaxVolFlowRateWasAutoSized) {

        if ((PlantSizNum > 0)) {

            if (state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate >= HVAC::SmallWaterVolFlow) {
                loop.MaxVolFlowRate =
                    state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * state.dataSize->PlantSizData(PlantSizNum).PlantSizFac;
            } else {
                loop.MaxVolFlowRate = 0.0;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    ShowWarningError(state,
                                     format("SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=[{:.2R}] is too small. Set to 0.0",
                                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate));
                    ShowContinueError(state, "..occurs for PlantLoop=" + loop.Name);
                }
            }
            if (Finalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    if (loop.TypeOfLoop == LoopType::Plant) {
                        BaseSizer::reportSizerOutput(state,
                                                     "PlantLoop",
                                                     loop.Name,
                                                     "Maximum Loop Flow Rate [m3/s]",
                                                     loop.MaxVolFlowRate);
                        // begin std 229 plantloop equipment summary new table
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchPLCLType, loop.Name, "PlantLoop");
                        // end std 229 plantloop equipment summary new table
                    } else if (loop.TypeOfLoop == LoopType::Condenser) {
                        BaseSizer::reportSizerOutput(state,
                                                     "CondenserLoop",
                                                     loop.Name,
                                                     "Maximum Loop Flow Rate [m3/s]",
                                                     loop.MaxVolFlowRate);
                        // begin std 229 plantloop equipment summary new table
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchPLCLType, loop.Name, "CondenserLoop");
                        // end std 229 plantloop equipment summary new table
                    }
                    // begin std 229 plantloop equipment summary new table
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchPLCLProvHeat,
                                                             loop.Name,
                                                             loop.HeatingDemand >= 0 ? "Yes" : "No");
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchPLCLProvCool,
                                                             loop.Name,
                                                             loop.CoolingDemand >= 0 ? "Yes" : "No");
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchPLCLMaxLoopFlowRate,
                                                             loop.Name,
                                                             loop.MaxVolFlowRate);
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchPLCLMinLoopFlowRate,
                                                             loop.Name,
                                                             loop.MinVolFlowRate);
                    // end std 229 plantloop equipment summary new table
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    if (loop.TypeOfLoop == LoopType::Plant) {
                        BaseSizer::reportSizerOutput(state,
                                                     "PlantLoop",
                                                     loop.Name,
                                                     "Initial Maximum Loop Flow Rate [m3/s]",
                                                     loop.MaxVolFlowRate);
                    } else if (loop.TypeOfLoop == LoopType::Condenser) {
                        BaseSizer::reportSizerOutput(state,
                                                     "CondenserLoop",
                                                     loop.Name,
                                                     "Initial Maximum Loop Flow Rate [m3/s]",
                                                     loop.MaxVolFlowRate);
                    }
                }
            }

        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowFatalError(state, "Autosizing of plant loop requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in PlantLoop object=" + loop.Name);
                ErrorsFound = true;
            }
        }
    }

    // Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
    if (loop.VolumeWasAutoSized) {
        // There is no stability requirement (mass can be zero), autosizing is based on loop circulation time.
        // Note this calculation also appears in PlantManager::ResizePlantLoopLevelSizes and SizingAnalysisObjects::ResolveDesignFlowRate
        loop.Volume =
            loop.MaxVolFlowRate * loop.CirculationTime * 60.0;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            if (loop.TypeOfLoop == LoopType::Plant) {
                // condenser loop vs plant loop breakout needed.
                BaseSizer::reportSizerOutput(
                    state, "PlantLoop", loop.Name, "Plant Loop Volume [m3]", loop.Volume);
                // begin std 229 added lines
                BaseSizer::reportSizerOutput(state,
                                             "PlantLoop",
                                             loop.Name,
                                             "Design Supply Temperature [C]",
                                             loop.PlantSizNum > 0
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ExitTemp
                                                 : -999.0);
                BaseSizer::reportSizerOutput(state,
                                             "PlantLoop",
                                             loop.Name,
                                             "Design Return Temperature [C]",
                                             loop.PlantSizNum > 0
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ExitTemp -
                                                       state.dataSize->PlantSizData(loop.PlantSizNum).DeltaT
                                                 : -999.0);
                BaseSizer::reportSizerOutput(state,
                                             "PlantLoop",
                                             loop.Name,
                                             "Sizing option (Coincident/NonCoincident)",
                                             loop.PlantSizNum > 0
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ConcurrenceOption
                                                 : -1);
                // end std 229 added lines
            } else if (loop.TypeOfLoop == LoopType::Condenser) {
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             loop.Name,
                                             "Condenser Loop Volume [m3]",
                                             loop.Volume);
                // begin std 229 added lines
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             loop.Name,
                                             "Design Supply Temperature [C]",
                                             loop.PlantSizNum > 0
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ExitTemp
                                                 : -999.0);
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             loop.Name,
                                             "Design Return Temperature [C]",
                                             loop.PlantSizNum > 0
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ExitTemp -
                                                       state.dataSize->PlantSizData(loop.PlantSizNum).DeltaT
                                                 : -999.0);
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             loop.Name,
                                             "Sizing option (Coincident/NonCoincident)",
                                             loop.PlantSizNum
                                                 ? state.dataSize->PlantSizData(loop.PlantSizNum).ConcurrenceOption
                                                 : -1);
                // end std 229 added lines
            }
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            if (loop.TypeOfLoop == LoopType::Plant) {
                // condenser loop vs plant loop breakout needed.
                BaseSizer::reportSizerOutput(state,
                                             "PlantLoop",
                                             loop.Name,
                                             "Initial Plant Loop Volume [m3]",
                                             loop.Volume);
            } else if (loop.TypeOfLoop == LoopType::Condenser) {
                BaseSizer::reportSizerOutput(state,
                                             "CondenserLoop",
                                             loop.Name,
                                             "Initial Condenser Loop Volume [m3]",
                                             loop.Volume);
            }
        }
    }

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Plant) {
            BaseSizer::reportSizerOutput(state,
                                         "PlantLoop",
                                         state.dataPlnt->PlantLoop(LoopNum).Name,
                                         "Minimum Loop Flow Rate [m3/s]",
                                         state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate);
        } else if (state.dataPlnt->PlantLoop(LoopNum).TypeOfLoop == LoopType::Condenser) {
            BaseSizer::reportSizerOutput(state,
                                         "CondenserLoop",
                                         state.dataPlnt->PlantLoop(LoopNum).Name,
                                         "Minimum Loop Flow Rate [m3/s]",
                                         state.dataPlnt->PlantLoop(LoopNum).MinVolFlowRate);
        }
    }

    // should now have plant volume, calculate plant volume's mass for fluid type
    if (loop.fluidType == Node::FluidType::Water) {
        FluidDensity = GetDensityGlycol(state, loop.FluidName, Constant::InitConvTemp, loop.FluidIndex, RoutineName);
        if (PlantSizNum > 0 && allocated(state.dataSize->PlantSizData)) { // method only works if sizing delta T is avaiable
            Real64 cp = GetSpecificHeatGlycol(state,
                                              loop.FluidName,
                                              Constant::InitConvTemp,
                                              loop.FluidIndex,
                                              RoutineName);
            Real64 DesignPlantCapacity =
                cp * FluidDensity * state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * state.dataSize->PlantSizData(PlantSizNum).DeltaT;
            state.dataSize->PlantSizData(PlantSizNum).DesCapacity = DesignPlantCapacity; // store it for later use in scaling
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, "PlantLoop", state.dataPlnt->PlantLoop(LoopNum).Name, "Design Capacity [W]", DesignPlantCapacity);
            }
        }
    } else if (loop.fluidType == Node::FluidType::Steam) {
        FluidDensity = GetSatDensityRefrig(state, fluidNameSteam, 100.0, 1.0, loop.FluidIndex, RoutineName);
    } else {
        assert(false);
    }

    loop.Mass = loop.Volume * FluidDensity;

    loop.MaxMassFlowRate = loop.MaxVolFlowRate * FluidDensity;
    loop.MinMassFlowRate = loop.MinVolFlowRate * FluidDensity;

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

    auto &loop = state.dataPlnt->PlantLoop(LoopNum);
    PlantSizNum = loop.PlantSizNum;
    auto &supplySide = loop.LoopSide(LoopSideLocation::Supply);
    auto &demandSide = loop.LoopSide(LoopSideLocation::Demand);

    // fill PlantSizFac from data structure
    for (BranchNum = 1; BranchNum <= supplySide.TotalBranches; ++BranchNum) {
        if (supplySide.InNodeNum == supplySide.Branch(BranchNum).InNodeNum) {
            PlantSizeFac = supplySide.Branch(BranchNum).PumpSizFac;
            break;
        }
    }
    
    if (state.dataSize->PlantSizData(PlantSizNum).ConcurrenceOption == NonCoincident) {
        // we can have plant loops that are non-coincident along with some that are coincident
        // so refresh sum of registered flows (they may have changed)

        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate = 0.0; // init for summation
        for (BranchNum = 1; BranchNum <= demandSide.TotalBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= demandSide.Branch(BranchNum).TotalComponents; ++CompNum) {
                SupNodeNum = demandSide.Branch(BranchNum).Comp(CompNum).InNodeNum;
                for (WaterCompNum = 1; WaterCompNum <= state.dataSize->SaveNumPlantComps; ++WaterCompNum) {
                    if (SupNodeNum == state.dataSize->CompDesWaterFlow(WaterCompNum).SupNode) {
                        state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate += state.dataSize->CompDesWaterFlow(WaterCompNum).DesVolFlowRate;
                    }
                }
            }
        }
    }

    if (loop.MaxVolFlowRateWasAutoSized) {

        if ((PlantSizNum > 0)) {

            if (state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate >= HVAC::SmallWaterVolFlow) {
                loop.MaxVolFlowRate = state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate * PlantSizeFac;
            } else {
                loop.MaxVolFlowRate = 0.0;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    ShowWarningError(state,
                                     format("SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=[{:.2R}] is too small. Set to 0.0",
                                            state.dataSize->PlantSizData(PlantSizNum).DesVolFlowRate));
                    ShowContinueError(state, "..occurs for PlantLoop=" + loop.Name);
                }
            }
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (loop.TypeOfLoop == LoopType::Plant) {
                    BaseSizer::reportSizerOutput(state,
                                                 "PlantLoop",
                                                 loop.Name,
                                                 "Maximum Loop Flow Rate [m3/s]",
                                                 loop.MaxVolFlowRate);
                } else if (loop.TypeOfLoop == LoopType::Condenser) {
                    BaseSizer::reportSizerOutput(state,
                                                 "CondenserLoop",
                                                 loop.Name,
                                                 "Maximum Loop Flow Rate [m3/s]",
                                                 loop.MaxVolFlowRate);
                }
            }
        }
    }

    // Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
    if (loop.VolumeWasAutoSized) {
        // There is no stability requirement (mass can be zero), autosizing is based on loop circulation time.
        // Note this calculation also appears in PlantManager::SizePlantLoop and SizingAnalysisObjects::ResolveDesignFlowRate
        loop.Volume =
            loop.MaxVolFlowRate * loop.CirculationTime * 60.0;
        if (loop.TypeOfLoop == LoopType::Plant) {
            // condenser loop vs plant loop breakout needed.
            BaseSizer::reportSizerOutput(
                state, "PlantLoop", loop.Name, "Plant Loop Volume [m3]", loop.Volume);
        } else if (loop.TypeOfLoop == LoopType::Condenser) {
            BaseSizer::reportSizerOutput(state,
                                         "CondenserLoop",
                                         loop.Name,
                                         "Condenser Loop Volume [m3]",
                                         loop.Volume);
        }
    }

    // should now have plant volume, calculate plant volume's mass for fluid type
    if (loop.fluidType == Node::FluidType::Water) {
        FluidDensity = GetDensityGlycol(state, loop.FluidName, Constant::InitConvTemp, loop.FluidIndex, RoutineName);
    } else if (loop.fluidType == Node::FluidType::Steam) {
        FluidDensity = GetSatDensityRefrig(state, fluidNameSteam, 100.0, 1.0, loop.FluidIndex, RoutineName);
    } else {
        assert(false);
    }

    loop.Mass = loop.Volume * FluidDensity;

    loop.MaxMassFlowRate = loop.MaxVolFlowRate * FluidDensity;
    loop.MinMassFlowRate = loop.MinVolFlowRate * FluidDensity;

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
        state.dataPlnt->PlantCallingOrderInfo(I).LoopSide = LoopSideLocation::Demand;
    }

    // set plant loop supply sides
    for (I = 1; I <= state.dataHVACGlobal->NumPlantLoops; ++I) {
        OrderIndex = I + state.dataHVACGlobal->NumPlantLoops;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = LoopSideLocation::Supply;
    }

    // set condenser Loop demand sides
    for (I = 1; I <= state.dataHVACGlobal->NumCondLoops; ++I) {
        OrderIndex = 2 * state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = LoopSideLocation::Demand;
    }

    // set condenser Loop supply sides
    for (I = 1; I <= state.dataHVACGlobal->NumCondLoops; ++I) {
        OrderIndex = 2 * state.dataHVACGlobal->NumPlantLoops + state.dataHVACGlobal->NumCondLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopIndex = state.dataHVACGlobal->NumPlantLoops + I;
        state.dataPlnt->PlantCallingOrderInfo(OrderIndex).LoopSide = LoopSideLocation::Supply;
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
    DataPlant::LoopSideLocation LoopSideNum;
    int OtherLoopNum;
    DataPlant::LoopSideLocation OtherLoopSideNum;

    bool thisLoopPutsDemandOnAnother;
    int ConnctNum;

    
    for (HalfLoopNum = 1; HalfLoopNum <= state.dataPlnt->TotNumHalfLoops; ++HalfLoopNum) {

        LoopNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopIndex;
        auto &loop = state.dataPlnt->PlantLoop(LoopNum);
        LoopSideNum = state.dataPlnt->PlantCallingOrderInfo(HalfLoopNum).LoopSide;

        if (allocated(loop.LoopSide(LoopSideNum).Connected)) {
            for (ConnctNum = 1; ConnctNum <= isize(loop.LoopSide(LoopSideNum).Connected); ++ConnctNum) {
                OtherLoopNum = loop.LoopSide(LoopSideNum).Connected(ConnctNum).LoopNum;
                OtherLoopSideNum = loop.LoopSide(LoopSideNum).Connected(ConnctNum).LoopSideNum;
                state.dataPlantMgr->OtherLoopCallingIndex = FindLoopSideInCallingOrder(state, OtherLoopNum, OtherLoopSideNum);

                thisLoopPutsDemandOnAnother = loop.LoopSide(LoopSideNum).Connected(ConnctNum).LoopDemandsOnRemote;
                if (thisLoopPutsDemandOnAnother) {                                 // make sure this loop side is called before the other loop side
                    if (state.dataPlantMgr->OtherLoopCallingIndex < HalfLoopNum) { // rearrange
                        state.dataPlantMgr->newCallingIndex = min(HalfLoopNum + 1, state.dataPlnt->TotNumHalfLoops);
                        ShiftPlantLoopSideCallingOrder(state, state.dataPlantMgr->OtherLoopCallingIndex, state.dataPlantMgr->newCallingIndex);
                    }

                } else {                                                           // make sure the other is called before this one
                    if (state.dataPlantMgr->OtherLoopCallingIndex > HalfLoopNum) { // rearrange
                        state.dataPlantMgr->newCallingIndex = max(HalfLoopNum, 1);

                        if (OtherLoopSideNum == LoopSideLocation::Supply) { // if this is a supply side, don't push it before its own demand side
                            state.dataPlantMgr->OtherLoopDemandSideCallingIndex =
                                FindLoopSideInCallingOrder(state, OtherLoopNum, LoopSideLocation::Demand);
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

int FindLoopSideInCallingOrder(EnergyPlusData &state, int const LoopNum, const LoopSideLocation LoopSide)
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
    int BranchCtr;
    int CompCtr;
    bool BranchIsInSplitterMixer;
    DataBranchAirLoopPlant::ControlType ComponentFlowCtrl;
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

    for (auto &loop : state.dataPlnt->PlantLoop) {
        for (DataPlant::LoopSideLocation LoopSideCtr : DataPlant::LoopSideKeys) {
            for (BranchCtr = 1; BranchCtr <= loop.LoopSide(LoopSideCtr).TotalBranches; ++BranchCtr) {
                BranchIsInSplitterMixer = false;
                // test if this branch is inside a splitter/mixer
                if (loop.LoopSide(LoopSideCtr).Splitter.Exists) {
                    if ((BranchCtr > 1) && (BranchCtr < loop.LoopSide(LoopSideCtr).TotalBranches)) {
                        BranchIsInSplitterMixer = true;
                    }
                }

                NumComponentsOnBranch = loop.LoopSide(LoopSideCtr).Branch(BranchCtr).TotalComponents;

                for (CompCtr = 1; CompCtr <= isize(loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Comp); ++CompCtr) {

                    auto &this_component(loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Comp(CompCtr));

                    switch (this_component.Type) {
                    case DataPlant::PlantEquipmentType::Invalid: { //                             = -1
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Invalid;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::Invalid;
                        this_component.HowLoadServed = DataPlant::HowMet::Invalid;
                    } break;
                    case DataPlant::PlantEquipmentType::Boiler_Simple: { //         =  1
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapHiOutLimit;
                    } break;
                    case DataPlant::PlantEquipmentType::Boiler_Steam: { //                      =  2
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_Absorption: { // = 3 ! older BLAST absorption chiller
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_Indirect_Absorption: { // = 4 ! revised absorption chiller
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_CombTurbine: { //           =  5
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_ConstCOP: { //                 =  6
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;

                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_DFAbsorption: { //             =  7
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_ExhFiredAbsorption: { //             =  76
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_Electric: { //                 =  8
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_ElectricEIR: { //              =  9
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR: { //        = 10
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_EngineDriven: { //             = 11
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::CoolingTower_SingleSpd: { //           = 12
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::CoolingTower_TwoSpd: { //              = 13
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::CoolingTower_VarSpd: { //              = 14
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::CoolingTower_VarSpdMerkel: { //              = 89
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_FCExhaust: { //              = 15
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case PlantEquipmentType::HeatPumpWtrHeaterPumped:
                    case DataPlant::PlantEquipmentType::HeatPumpWtrHeaterWrapped: { //                = 16, 92
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::HPWaterEFCooling: { //                 = 17
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::HPWaterEFHeating: { //                 = 18
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::HPWaterPECooling: { //                 = 19
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::HPWaterPEHeating: { //                 = 20
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Pipe: { //                             = 21
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        if (BranchIsInSplitterMixer) {
                            if (NumComponentsOnBranch == 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            } else if (NumComponentsOnBranch > 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            }
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PipeSteam: { //                        = 22
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        if (BranchIsInSplitterMixer) {
                            if (NumComponentsOnBranch == 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            } else if (NumComponentsOnBranch > 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            }
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PipeExterior: { //                     = 23
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        if (BranchIsInSplitterMixer) {
                            if (NumComponentsOnBranch == 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            } else if (NumComponentsOnBranch > 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            }
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PipeInterior: { //                     = 24
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        if (BranchIsInSplitterMixer) {
                            if (NumComponentsOnBranch == 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            } else if (NumComponentsOnBranch > 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            }
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PipeUnderground: { //                  = 25
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        if (BranchIsInSplitterMixer) {
                            if (NumComponentsOnBranch == 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            } else if (NumComponentsOnBranch > 1) {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                            } else {
                                this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Bypass;
                            }
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Passive;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PurchChilledWater: { //                = 26
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                    } break;
                    case DataPlant::PlantEquipmentType::PurchHotWater: { //                    = 27
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapHiOutLimit;
                    } break;
                    case DataPlant::PlantEquipmentType::PurchSteam: { //
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                    } break;
                    case DataPlant::PlantEquipmentType::TS_IceDetailed: { //                   = 28
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::TS_IceSimple: { //                    = 29
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::ValveTempering: { //                  = 30
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::WtrHeaterMixed: { //                   = 31
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::WtrHeaterStratified: { //              = 32
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PumpVariableSpeed: { //                 = 33
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::PumpConstantSpeed: { //                 = 34
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::PumpCondensate: { //                    = 35
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::PumpBankVariableSpeed: { //             = 36
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::PumpBankConstantSpeed: { //             = 37
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::WaterUseConnection: { //              = 38
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWaterCooling: { //               = 39  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling: { //      = 40  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWaterSimpleHeating: { //           = 41  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilSteamAirHeating: { //         = 42  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::SolarCollectorFlatPlate: { //         = 43  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::PlantLoadProfile: { //            = 44  ! demand side component
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::GrndHtExchgSystem: { //            = 45
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::GrndHtExchgSurface: { //            = 46
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::GrndHtExchgPond: { //            = 47
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_MicroTurbine: { //          = 48  !newer FSEC turbine
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_ICEngine: { //             = 49
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_CTurbine: { //             = 50  !older BLAST turbine
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_MicroCHP: { //              = 51
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::Generator_FCStackCooler: { //         = 52
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                    } break;
                    case DataPlant::PlantEquipmentType::FluidCooler_SingleSpd: { //           = 53
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::FluidCooler_TwoSpd: { //            = 54
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::EvapFluidCooler_SingleSpd: { //       = 55
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::EvapFluidCooler_TwoSpd: { //         = 56
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::ChilledWaterTankMixed: { //         = 57
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::ChilledWaterTankStratified: { //      = 58
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PVTSolarCollectorFlatPlate: { //      = 59
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        // next batch for ZoneHVAC
                    } break;
                    case DataPlant::PlantEquipmentType::Baseboard_Conv_Water: { //        = 60
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::Baseboard_Rad_Conv_Steam: { //      = 61
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::Baseboard_Rad_Conv_Water: { //      = 62
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoolingPanel_Simple: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::LowTempRadiant_VarFlow: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::LowTempRadiant_ConstFlow: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CooledBeamAirTerminal: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::FourPipeBeamAirTerminal: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilVSWAHPHeatingEquationFit: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilVSWAHPCoolingEquationFit: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWAHPHeatingParamEst: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilWAHPCoolingParamEst: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::RefrigSystemWaterCondenser: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::RefrigerationWaterCoolRack: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::MultiSpeedHeatPumpRecovery: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::UnitarySysRecovery: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::PipingSystemPipeCircuit: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::SolarCollectorICS: { //         = 75
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::PlantComponentUserDefined: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::Invalid;
                        this_component.HowLoadServed = DataPlant::HowMet::Invalid;
                    } break;
                    case DataPlant::PlantEquipmentType::CoilUserDefined: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::Invalid;
                        this_component.HowLoadServed = DataPlant::HowMet::Invalid;
                    } break;
                    case DataPlant::PlantEquipmentType::ZoneHVACAirUserDefined: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::Invalid;
                        this_component.HowLoadServed = DataPlant::HowMet::Invalid;
                    } break;
                    case DataPlant::PlantEquipmentType::AirTerminalUserDefined: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::Invalid;
                        this_component.HowLoadServed = DataPlant::HowMet::Invalid;
                    } break;
                    case DataPlant::PlantEquipmentType::HeatPumpVRF: { //       =  82  ! AirConditioner:VariableRefrigerantFlow
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;

                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else { // should never happen
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::WaterSource: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                    } break;
                    case DataPlant::PlantEquipmentType::GrndHtExchgHorizTrench: { // = 83  GroundHeatExchanger:HorizontalTrench
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::FluidToFluidPlantHtExchg: { //          = 84
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump: { // 86
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::PackagedTESCoolingCoil: { // 88
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::SwimmingPool_Indoor: { // 90
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                        this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                    } break;
                    case DataPlant::PlantEquipmentType::GrndHtExchgSlinky: { //            = 91
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                        this_component.HowLoadServed = DataPlant::HowMet::PassiveCap;
                    } break;
                    case DataPlant::PlantEquipmentType::HeatPumpEIRCooling:
                    case PlantEquipmentType::HeatPumpEIRHeating: { // 95, 96
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling:
                    case PlantEquipmentType::HeatPumpFuelFiredHeating: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCap;
                        }
                    } break;
                    case DataPlant::PlantEquipmentType::Chiller_ElectricASHRAE205: {
                        this_component.FlowCtrl = DataBranchAirLoopPlant::ControlType::Active;
                        if (LoopSideCtr == LoopSideLocation::Demand) {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::NeedyAndTurnsLoopOn;
                            this_component.HowLoadServed = DataPlant::HowMet::NoneDemand;
                        } else {
                            this_component.FlowPriority = DataPlant::LoopFlowStatus::TakesWhatGets;
                            this_component.HowLoadServed = DataPlant::HowMet::ByNominalCapLowOutLimit;
                        }
                    } break;
                    default: {
                        ShowSevereError(state, "SetBranchControlTypes: Caught unexpected equipment type of number");
                    } break;
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
    for (auto &loop : state.dataPlnt->PlantLoop) { // SIZE(PlantLoop)
        for (DataPlant::LoopSideLocation LoopSideCtr : DataPlant::LoopSideKeys) {
            for (BranchCtr = 1; BranchCtr <= loop.LoopSide(LoopSideCtr).TotalBranches; ++BranchCtr) {
                ActiveCount = 0;
                BypassCount = 0;
                for (CompCtr = 1; CompCtr <= isize(loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Comp); ++CompCtr) {
                    ComponentFlowCtrl = loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Comp(CompCtr).FlowCtrl;

                    switch (ComponentFlowCtrl) {
                    case DataBranchAirLoopPlant::ControlType::Invalid: {
                        loop.LoopSide(LoopSideCtr).Branch(BranchCtr).controlType = DataBranchAirLoopPlant::ControlType::Passive;
                    } break;
                    case DataBranchAirLoopPlant::ControlType::Active: {
                        ++ActiveCount;
                        if (ActiveCount > 1) {
                            //  assume multiple active components in series means branch is SeriesActive
                            loop.LoopSide(LoopSideCtr).Branch(BranchCtr).controlType = DataBranchAirLoopPlant::ControlType::SeriesActive;
                            // assume all components on branch are to be SeriesActive as well
                            for (auto &e : loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Comp)
                                e.FlowCtrl = DataBranchAirLoopPlant::ControlType::SeriesActive;
                        } else {
                            loop.LoopSide(LoopSideCtr).Branch(BranchCtr).controlType = DataBranchAirLoopPlant::ControlType::Active;
                        }

                        if (BypassCount > 0) {
                            ShowSevereError(state, "An active component is on the same branch as a pipe situated between splitter/mixer");
                            ShowContinueError(state, "Occurs in Branch=" + loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Name);
                            ShowContinueError(state, "Occurs in Plant Loop=" + loop.Name);
                            ShowContinueError(state, "SetupBranchControlTypes: and the simulation continues");
                            //  note not sure why this is so bad.  heat transfer pipe might be a good reason to allow this?
                            //   this used to fatal in older PlantFlowResolver.
                        }

                        // test for active component in series with bypass
                    } break;
                    case DataBranchAirLoopPlant::ControlType::Bypass: {
                        ++BypassCount;
                        loop.LoopSide(LoopSideCtr).Branch(BranchCtr).controlType = DataBranchAirLoopPlant::ControlType::Bypass;
                        loop.LoopSide(LoopSideCtr).Branch(BranchCtr).IsBypass = true;
                        loop.LoopSide(LoopSideCtr).BypassExists = true;

                        if (CompCtr > 1) {
                            ShowSevereError(state, "A pipe used as a bypass should not be in series with another component");
                            ShowContinueError(state, "Occurs in Branch = " + loop.LoopSide(LoopSideCtr).Branch(BranchCtr).Name);
                            ShowContinueError(state, "Occurs in PlantLoop = " + loop.Name);
                            ShowFatalError(state, "SetupBranchControlTypes: preceding condition causes termination.");
                        }
                    } break;
                    case DataBranchAirLoopPlant::ControlType::Passive: {
                        if (ActiveCount > 0) {
                            // do nothing, branch set before)
                        } else {
                            if (BypassCount > 0) {

                            } else {
                                loop.LoopSide(LoopSideCtr).Branch(BranchCtr).controlType = DataBranchAirLoopPlant::ControlType::Passive;
                            }
                        }
                    } break;
                    case DataBranchAirLoopPlant::ControlType::SeriesActive: {
                        // do nothing, already set when more than one active component found on a branch
                    } break;
                    default:
                        break;
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
    for (auto &loop : state.dataPlnt->PlantLoop) {
        auto &supplySide = loop.LoopSide(LoopSideLocation::Supply);
        auto &demandSide = loop.LoopSide(LoopSideLocation::Demand);
        // Warning if the excess storage time is more than half of the total time
        if (demandSide.LoopSideInlet_CapExcessStorageTime > demandSide.LoopSideInlet_TotalTime / 2) {
            ShowWarningError(state, format("Plant Loop: {} Demand Side is storing excess heat the majority of the time.", loop.Name));
            ShowContinueError(state,
                              format("Excesss Storage Time={:.2R}[hr], Total Loop Active Time={:.2R}[hr]",
                                     supplySide.LoopSideInlet_CapExcessStorageTime,
                                     demandSide.LoopSideInlet_TotalTime));
        }
        if (supplySide.LoopSideInlet_CapExcessStorageTime > supplySide.LoopSideInlet_TotalTime / 2) {
            ShowWarningError(state, format("Plant Loop: {} Supply Side is storing excess heat the majority of the time.", loop.Name));
            ShowContinueError(state,
                              format("Excesss Storage Time={:.2R}[hr], Total Loop Active Time={:.2R}[hr]",
                                     supplySide.LoopSideInlet_CapExcessStorageTime,
                                     demandSide.LoopSideInlet_TotalTime));
        }
    }
}

void EmptyPlantComponent::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
{
}
void EmptyPlantComponent::oneTimeInit_new([[maybe_unused]] EnergyPlusData &state)
{
}
} // namespace EnergyPlus::PlantManager
