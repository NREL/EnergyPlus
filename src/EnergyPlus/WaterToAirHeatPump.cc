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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterToAirHeatPump.hh>

namespace EnergyPlus {

namespace WaterToAirHeatPump {
    // Module containing the Water to Air Heat Pump simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Hui Jin
    //       DATE WRITTEN   Oct 2000
    //       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
    //                      Brent Griffith, plant upgrades, fluid props
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the Water to Air Heat Pump Component

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
    // Oklahoma State University.

    using namespace DataLoopNode;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataPlant::TypeOf_CoilWAHPCoolingParamEst;
    using DataPlant::TypeOf_CoilWAHPHeatingParamEst;

    static constexpr std::string_view fluidNameWater("WATER");

    void SimWatertoAirHP(EnergyPlusData &state,
                         std::string_view CompName,   // component name
                         int &CompIndex,                // Index for Component name
                         Real64 const DesignAirflow,    // design air flow rate
                         int const CyclingScheme,       // cycling scheme--either continuous fan/cycling compressor or
                         bool const FirstHVACIteration, // first iteration flag
                         Real64 const RuntimeFrac,      // compressor run time fraction
                         Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                         Real64 &HPTimeConstant,        // Heat pump time constant [s]
                         Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                         bool const InitFlag,           // initialization flag used to suppress property routine errors
                         Real64 const SensLoad,         // sensible load
                         Real64 const LatentLoad,       // latent load
                         int const CompOp,
                         Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Water to Air Heat Pump component simulation.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // shut off after compressor cycle off  [s]
        // cycling fan/cycling compressor

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HPNum; // The WatertoAirHP that you are currently loading input into

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPump->GetCoilsInputFlag) {                            // First time subroutine has been entered
            state.dataWaterToAirHeatPump->WaterIndex = FindGlycol(state, fluidNameWater); // Initialize the WaterIndex once
            GetWatertoAirHPInput(state);
            state.dataWaterToAirHeatPump->GetCoilsInputFlag = false;
        }

        if (CompIndex == 0) {
            HPNum = UtilityRoutines::FindItemInList(CompName, state.dataWaterToAirHeatPump->WatertoAirHP);
            if (HPNum == 0) {
                ShowFatalError(state, "WaterToAir HP not found=" + std::string{CompName});
            }
            CompIndex = HPNum;
        } else {
            HPNum = CompIndex;
            if (HPNum > state.dataWaterToAirHeatPump->NumWatertoAirHPs || HPNum < 1) {
                ShowFatalError(state,
                               format("SimWatertoAirHP: Invalid CompIndex passed={}, Number of Water to Air HPs={}, WaterToAir HP name={}",
                                      HPNum,
                                      state.dataWaterToAirHeatPump->NumWatertoAirHPs,
                                      CompName));
            }
            if (state.dataWaterToAirHeatPump->CheckEquipName(HPNum)) {
                if (!CompName.empty() && CompName != state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name) {
                    ShowFatalError(
                        state,
                        format("SimWatertoAirHP: Invalid CompIndex passed={}, WaterToAir HP name={}, stored WaterToAir HP Name for that index={}",
                               HPNum,
                               CompName,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name));
                }
                state.dataWaterToAirHeatPump->CheckEquipName(HPNum) = false;
            }
        }
        // Calculate the Correct Water to Air HP Model with the current HPNum

        if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum == TypeOf_CoilWAHPCoolingParamEst) {
            InitWatertoAirHP(
                state, HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);
            CalcWatertoAirHPCooling(state, HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio);

            UpdateWatertoAirHP(state, HPNum);

        } else if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum == TypeOf_CoilWAHPHeatingParamEst) {
            InitWatertoAirHP(
                state, HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);
            CalcWatertoAirHPHeating(state, HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio);

            UpdateWatertoAirHP(state, HPNum);

        } else {
            ShowFatalError(state, "SimWatertoAirHP: AirtoAir heatpump not in either HEATING or COOLING");
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetWatertoAirHPInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for HPs and stores it in HP data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using namespace NodeInputManager;
        using BranchNodeConnections::TestCompSet;
        using FluidProperties::CheckFluidPropertyName;
        using FluidProperties::FindGlycol;
        using GlobalNames::VerifyUniqueCoilName;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using namespace OutputReportPredefined;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetWatertoAirHPInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HPNum; // The Water to Air HP that you are currently loading input into
        int NumCool;
        int NumHeat;
        int WatertoAirHPNum;
        int NumAlphas;
        int NumParams;
        int NumNums;
        int MaxNums(0);   // Maximum number of numeric input fields
        int MaxAlphas(0); // Maximum number of alpha input fields
        int IOStat;
        bool ErrorsFound(false);         // If errors detected in input
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string AlphArray;        // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> NumArray;        // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

        NumCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation");
        NumHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation");
        state.dataWaterToAirHeatPump->NumWatertoAirHPs = NumCool + NumHeat;
        HPNum = 0;

        if (state.dataWaterToAirHeatPump->NumWatertoAirHPs <= 0) {
            ShowSevereError(state, "No Equipment found in SimWatertoAirHP");
            ErrorsFound = true;
        }

        // Allocate Arrays
        if (state.dataWaterToAirHeatPump->NumWatertoAirHPs > 0) {
            state.dataWaterToAirHeatPump->WatertoAirHP.allocate(state.dataWaterToAirHeatPump->NumWatertoAirHPs);
            state.dataWaterToAirHeatPump->CheckEquipName.dimension(state.dataWaterToAirHeatPump->NumWatertoAirHPs, true);
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        lAlphaBlanks.dimension(MaxAlphas, true);
        cNumericFields.allocate(MaxNums);
        lNumericBlanks.dimension(MaxNums, true);
        NumArray.dimension(MaxNums, 0.0);

        // Get the data for detailed cooling Heat Pump
        CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation";

        for (WatertoAirHPNum = 1; WatertoAirHPNum <= NumCool; ++WatertoAirHPNum) {

            ++HPNum;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     HPNum,
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

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name = AlphArray(1);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WatertoAirHPType = "COOLING";
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum = TypeOf_CoilWAHPCoolingParamEst;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Refrigerant = AlphArray(3);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterVolFlowRate = NumArray(1);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CoolingCapacity = NumArray(2);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Twet_Rated = NumArray(3);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Gamma_Rated = NumArray(4);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressCutoff = NumArray(5);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressCutoff = NumArray(6);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                                    AlphArray(4),
                                                                                                    ErrorsFound,
                                                                                                    CurrentModuleObject,
                                                                                                    AlphArray(1),
                                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                                    2,
                                                                                                    ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                                     AlphArray(5),
                                                                                                     ErrorsFound,
                                                                                                     CurrentModuleObject,
                                                                                                     AlphArray(1),
                                                                                                     DataLoopNode::NodeFluidType::Water,
                                                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                                                     2,
                                                                                                     ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                  AlphArray(6),
                                                                                                  ErrorsFound,
                                                                                                  CurrentModuleObject,
                                                                                                  AlphArray(1),
                                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                                                                  1,
                                                                                                  ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                   AlphArray(7),
                                                                                                   ErrorsFound,
                                                                                                   CurrentModuleObject,
                                                                                                   AlphArray(1),
                                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                                   DataLoopNode::NodeConnectionType::Outlet,
                                                                                                   1,
                                                                                                   ObjectIsNotParent);

            // 2010-01-13 ESL: Jason Glazer noted that these were out of order previously, but they are good now
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff = NumArray(7);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideOutsideUACoeff = NumArray(8);

            if ((state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideOutsideUACoeff < DataGlobalConstants::rTinyValue) ||
                (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff < DataGlobalConstants::rTinyValue)) {
                ShowSevereError(state, "Input problem for " + CurrentModuleObject + '=' + state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                ShowContinueError(state, " One or both load side UA values entered are below tolerance, likely zero or blank.");
                ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                ShowContinueError(state, "  the release of EnergyPlus version 5.");
                ErrorsFound = true;
            }

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SuperheatTemp = NumArray(9);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PowerLosses = NumArray(10);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LossFactor = NumArray(11);

            {
                auto const SELECT_CASE_var(AlphArray(2));

                if (SELECT_CASE_var == "RECIPROCATING") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Reciprocating;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp = NumArray(12);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop = NumArray(13);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompClearanceFactor = NumArray(14);

                } else if (SELECT_CASE_var == "ROTARY") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Rotary;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp = NumArray(12);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop = NumArray(13);

                } else if (SELECT_CASE_var == "SCROLL") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Scroll;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RefVolFlowRate = NumArray(15);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).VolumeRatio = NumArray(16);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LeakRateCoeff = NumArray(17);

                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "Invalid " + cAlphaFields(2) + " (" + AlphArray(2) + ") entered." + CurrentModuleObject + '=' +
                                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                    ErrorsFound = true;
                }
            }

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideUACoeff = NumArray(18);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR1 = NumArray(19);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR2 = NumArray(20);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(6), AlphArray(7), "Air Nodes");

            // Setup Report variables for the detailed cooling Heat Pump
            // CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLoadTotal,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySensible,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLatent,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySource,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "PLANTLOOPCOOLINGDEMAND",
                                "COOLINGCOILS",
                                _,
                                "System");

            // save the design source side flow rate for use by plant loop sizing algorithms
            RegisterPlantCompDesignFlow(state,
                                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum,
                                        0.5 * state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterVolFlowRate);

            // create predefined report entries
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilType, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, CurrentModuleObject);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchCoolCoilTotCap,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CoolingCapacity);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSensCap, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilLatCap, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilNomEff, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, "-");
        }

        CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:ParameterEstimation";

        for (WatertoAirHPNum = 1; WatertoAirHPNum <= NumHeat; ++WatertoAirHPNum) {

            ++HPNum;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     WatertoAirHPNum,
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

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name = AlphArray(1);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WatertoAirHPType = "HEATING";
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum = TypeOf_CoilWAHPHeatingParamEst;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Refrigerant = AlphArray(3);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterVolFlowRate = NumArray(1);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HeatingCapacity = NumArray(2);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressCutoff = NumArray(3);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressCutoff = NumArray(4);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                                    AlphArray(4),
                                                                                                    ErrorsFound,
                                                                                                    CurrentModuleObject,
                                                                                                    AlphArray(1),
                                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                                    2,
                                                                                                    ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                                     AlphArray(5),
                                                                                                     ErrorsFound,
                                                                                                     CurrentModuleObject,
                                                                                                     AlphArray(1),
                                                                                                     DataLoopNode::NodeFluidType::Water,
                                                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                                                     2,
                                                                                                     ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                  AlphArray(6),
                                                                                                  ErrorsFound,
                                                                                                  CurrentModuleObject,
                                                                                                  AlphArray(1),
                                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                                                                  1,
                                                                                                  ObjectIsNotParent);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                   AlphArray(7),
                                                                                                   ErrorsFound,
                                                                                                   CurrentModuleObject,
                                                                                                   AlphArray(1),
                                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                                   DataLoopNode::NodeConnectionType::Outlet,
                                                                                                   1,
                                                                                                   ObjectIsNotParent);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff = NumArray(5);
            if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff < DataGlobalConstants::rTinyValue) {
                ShowSevereError(state, "Input problem for " + CurrentModuleObject + '=' + state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                ShowContinueError(state, " Load side UA value is less than tolerance, likely zero or blank.");
                ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                ShowContinueError(state, "  the release of EnergyPlus version 5.");
                ErrorsFound = true;
            }

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SuperheatTemp = NumArray(6);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PowerLosses = NumArray(7);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LossFactor = NumArray(8);

            {
                auto const SELECT_CASE_var(AlphArray(2));

                if (SELECT_CASE_var == "RECIPROCATING") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Reciprocating;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp = NumArray(9);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop = NumArray(10);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompClearanceFactor = NumArray(11);

                } else if (SELECT_CASE_var == "ROTARY") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Rotary;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp = NumArray(9);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop = NumArray(10);

                } else if (SELECT_CASE_var == "SCROLL") {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType = state.dataWaterToAirHeatPump->CompressorType_Scroll;
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RefVolFlowRate = NumArray(12);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).VolumeRatio = NumArray(13);
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LeakRateCoeff = NumArray(14);

                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "Invalid " + cAlphaFields(2) + " (" + AlphArray(2) + ") entered." + CurrentModuleObject + '=' +
                                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                    ErrorsFound = true;
                }
            }

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideUACoeff = NumArray(15);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR1 = NumArray(16);
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR2 = NumArray(17);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(6), AlphArray(7), "Air Nodes");

            // CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:ParameterEstimation"
            SetupOutputVariable(state,
                                "Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLoadTotal,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySource,
                                "System",
                                "Summed",
                                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                _,
                                "System");

            // save the design source side flow rate for use by plant loop sizing algorithms
            RegisterPlantCompDesignFlow(state,
                                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum,
                                        0.5 * state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterVolFlowRate);

            // create predefined report entries
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchHeatCoilType, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, CurrentModuleObject);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomCap,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HeatingCapacity);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomEff, state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name, "-");
        }

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        lAlphaBlanks.deallocate();
        cNumericFields.deallocate();
        lNumericBlanks.deallocate();
        NumArray.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found getting input. Program terminates.");
        }

        for (HPNum = 1; HPNum <= state.dataWaterToAirHeatPump->NumWatertoAirHPs; ++HPNum) {

            if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum == TypeOf_CoilWAHPCoolingParamEst) {
                // COOLING COIL: Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Cooling Coil Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Total Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Latent Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Runtime Fraction",
                                    OutputProcessor::Unit::None,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
            } else if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum == TypeOf_CoilWAHPHeatingParamEst) {
                // HEATING COIL Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Heating Coil Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Source Side Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Runtime Fraction",
                                    OutputProcessor::Unit::None,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);

                SetupOutputVariable(state,
                                    "Heating Coil Source Side Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp,
                                    "System",
                                    "Average",
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name);
            }
        }
    }

    void InitWatertoAirHP(EnergyPlusData &state,
                          int const HPNum, // index to main heat pump data structure
                          bool const InitFlag,
                          Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                          Real64 const HPTimeConstant,        // Heat pump time constant [s]
                          Real64 const FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                          Real64 const SensLoad,
                          Real64 const LatentLoad,
                          Real64 const DesignAirFlow,
                          Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
        //                      Brent Griffith, Sept 2010, plant upgrades, general fluid properties

        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Water to Air HP Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // shut off after compressor cycle off  [s]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // REAL(r64), PARAMETER        :: CpWater=4210.d0          ! Specific heat of water J/kg_C
        static constexpr std::string_view RoutineName("InitWatertoAirHP");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;   // air inlet node number
        int WaterInletNode; // water inlet node number
        int PlantOutletNode;
        Real64 rho; // local fluid density
        Real64 Cp;  // local fluid specific heat
        bool errFlag;

        if (state.dataWaterToAirHeatPump->MyOneTimeFlag) {
            state.dataWaterToAirHeatPump->MyEnvrnFlag.allocate(state.dataWaterToAirHeatPump->NumWatertoAirHPs);
            state.dataWaterToAirHeatPump->MyPlantScanFlag.allocate(state.dataWaterToAirHeatPump->NumWatertoAirHPs);
            state.dataWaterToAirHeatPump->MyEnvrnFlag = true;
            state.dataWaterToAirHeatPump->MyPlantScanFlag = true;
            state.dataWaterToAirHeatPump->MyOneTimeFlag = false;
        }

        if (state.dataWaterToAirHeatPump->MyPlantScanFlag(HPNum) && allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopSide,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).BranchNum,
                                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);

            if (state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidName == "WATER") {
                if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideUACoeff < DataGlobalConstants::rTinyValue) {
                    ShowSevereError(state,
                                    "Input problem for water to air heat pump, \"" + state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name + "\".");
                    ShowContinueError(state, " Source side UA value is less than tolerance, likely zero or blank.");
                    ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                    ShowContinueError(state, "  the release of EnergyPlus version 5.");
                    errFlag = true;
                }
            } else {
                if ((state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR1 < DataGlobalConstants::rTinyValue) ||
                    (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR2 < DataGlobalConstants::rTinyValue)) {
                    ShowSevereError(state,
                                    "Input problem for water to air heat pump, \"" + state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name + "\".");
                    ShowContinueError(state, " A source side heat transfer resistance value is less than tolerance, likely zero or blank.");
                    ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                    ShowContinueError(state, "  the release of EnergyPlus version 5.");
                    errFlag = true;
                }
            }

            if (errFlag) {
                ShowFatalError(state, "InitWatertoAirHP: Program terminated for previous conditions.");
            }

            state.dataWaterToAirHeatPump->MyPlantScanFlag(HPNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataWaterToAirHeatPump->MyEnvrnFlag(HPNum) &&
            !state.dataWaterToAirHeatPump->MyPlantScanFlag(HPNum)) {
            // Do the initializations to start simulation
            // Set water and air inlet nodes
            AirInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum;
            WaterInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum;

            // Initialize all report variables to a known state at beginning of simulation
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLoadTotal = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySensible = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLatent = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySource = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy = 0.0;

            // The rest of the one time initializations
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidName,
                                   DataGlobalConstants::InitConvTemp,
                                   state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidIndex,
                                   RoutineName);
            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidName,
                                       DataGlobalConstants::InitConvTemp,
                                       state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidIndex,
                                       RoutineName);

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate =
                rho * state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterVolFlowRate;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HPTimeConstant = HPTimeConstant;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).FanDelayTime = FanDelayTime;

            PlantOutletNode = state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum)
                                  .LoopSide(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopSide)
                                  .Branch(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).BranchNum)
                                  .Comp(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompNum)
                                  .NodeNumOut;
            InitComponentNodes(state,
                               0.0,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate,
                               WaterInletNode,
                               PlantOutletNode,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopSide,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).BranchNum,
                               state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompNum);

            state.dataLoopNodes->Node(WaterInletNode).Temp = 5.0;
            state.dataLoopNodes->Node(WaterInletNode).Enthalpy = Cp * state.dataLoopNodes->Node(WaterInletNode).Temp;
            state.dataLoopNodes->Node(WaterInletNode).Quality = 0.0;
            state.dataLoopNodes->Node(WaterInletNode).Press = 0.0;
            state.dataLoopNodes->Node(WaterInletNode).HumRat = 0.0;

            state.dataLoopNodes->Node(PlantOutletNode).Temp = 5.0;
            state.dataLoopNodes->Node(PlantOutletNode).Enthalpy = Cp * state.dataLoopNodes->Node(WaterInletNode).Temp;
            state.dataLoopNodes->Node(PlantOutletNode).Quality = 0.0;
            state.dataLoopNodes->Node(PlantOutletNode).Press = 0.0;
            state.dataLoopNodes->Node(PlantOutletNode).HumRat = 0.0;

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = true;

            state.dataWaterToAirHeatPump->MyEnvrnFlag(HPNum) = false;
        } // End If for the Begin Environment initializations

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataWaterToAirHeatPump->MyEnvrnFlag(HPNum) = true;
        }

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.
        // First set the conditions for the air into the heat pump model

        // Set water and air inlet nodes
        AirInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum;
        WaterInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum;

        //  ! Set heat pump simulation flag to false if the air loop and water loop conditions have not changed
        //  IF( .NOT. (BeginEnvrnFlag .and. MyEnvrnFlag) .AND. (&
        //  WatertoAirHP(HPNum)%InletWaterTemp      >= (Node(WaterInletNode)%Temp + TempTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletWaterTemp      <= (Node(WaterInletNode)%Temp - TempTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletWaterEnthalpy  >= (Node(WaterInletNode)%Enthalpy + EnthTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletWaterEnthalpy  <= (Node(WaterInletNode)%Enthalpy - EnthTOL) .OR. &!!

        //  WatertoAirHP(HPNum)%InletAirDBTemp      >= (Node(AirInletNode)%Temp + TempTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirDBTemp      <= (Node(AirInletNode)%Temp - TempTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirHumRat      >= (Node(AirInletNode)%HumRat + HumRatTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirHumRat      <= (Node(AirInletNode)%HumRat - HumRatTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirEnthalpy    >= (Node(AirInletNode)%Enthalpy + EnthTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirEnthalpy    <= (Node(AirInletNode)%Enthalpy - EnthTOL) .OR. &
        //  WatertoAirHP(HPNum)%InletAirMassFlowRate > 0.0))THEN
        //    WatertoAirHP(HPNum)%SimFlag =.TRUE.
        //  ELSE
        //    WatertoAirHP(HPNum)%SimFlag =.FALSE.
        //  ENDIF

        if (((SensLoad != 0.0 || LatentLoad != 0.0) || (SensLoad == 0.0 && InitFlag)) && state.dataLoopNodes->Node(AirInletNode).MassFlowRate > 0.0 &&
            PartLoadRatio > 0.0) {
            // set the water side flow rate to the design flow rate unless constrained by
            // the demand side manager (MIN/MAX available). now done by call to setcomponentFlowRate
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate =
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate =
                DesignAirFlow; // This is required instead of the node temperature
            // because the air loop operates handles part load for
            // cycling equipment by modulating the air flow rate
            // the heat pump model requires an accurate (i.e. full load
            // flow rate for accurate simulation.
        } else { // heat pump is off
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate = 0.0;

            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate = 0.0;
        }
        // constrain water flow provided by plant
        SetComponentFlowRate(state,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterOutletNodeNum,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopSide,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).BranchNum,
                             state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompNum);

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
        //  IF (WatertoAirHP(HPNum)%InletWaterTemp < 0.0) THEN  ! Debug trap
        //    Temptemp         = Node(WaterInletNode)%Temp
        //  ENDIF
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp = state.dataLoopNodes->Node(AirInletNode).Temp;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirEnthalpy = state.dataLoopNodes->Node(AirInletNode).Enthalpy;

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLoadTotal = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySensible = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLatent = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySource = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy = 0.0;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy = 0.0;
    }

    void CalcWatertoAirHPCooling(EnergyPlusData &state,
                                 int const HPNum,               // heat pump number
                                 int const CyclingScheme,       // fan/compressor cycling scheme indicator
                                 bool const FirstHVACIteration, // first iteration flag
                                 Real64 const RuntimeFrac,
                                 [[maybe_unused]] bool const InitFlag, // suppress property errors if true
                                 Real64 const SensDemand,
                                 int const CompOp,
                                 Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004), R. Raustad (Oct 2006) Revised iteration technique
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates a parameter estimation based water to air heat pump model

        // Using/Aliasing
        using namespace FluidProperties;

        using General::SolveRoot;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW; // ,PsyHFnTdbRhPb,PsyWFnTdpPb
        using Psychrometrics::PsyTdbFnHW;
        using Psychrometrics::PsyTsatFnHPb;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbH;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 CpWater(4210.0);         // Specific heat of water J/kg_C
        constexpr Real64 DegreeofSuperheat(80.0); // Initial guess of degree of superheat
        constexpr Real64 gamma(1.114);            // Expansion Coefficient
        constexpr Real64 ERR(0.01);               // Error Value
        constexpr Real64 PB(1.013e5);             // Barometric Pressure (Pa)

        constexpr int STOP1(1000); // Iteration stopper1
        constexpr int STOP2(1000); // Iteration stopper2
        constexpr int STOP3(1000); // Iteration stopper3

        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcWatertoAirHPCooling:SourceSideInletTemp");
        static constexpr std::string_view RoutineNameSourceSideTemp("CalcWatertoAirHPCooling:SourceSideTemp");
        static constexpr std::string_view RoutineNameLoadSideTemp("CalcWatertoAirHPCooling:LoadSideTemp");
        static constexpr std::string_view RoutineNameLoadSideSurfaceTemp("CalcWatertoAirHPCooling:LoadSideSurfaceTemp");
        static constexpr std::string_view RoutineNameLoadSideEvapTemp("CalcWatertoAirHPCooling:LoadSideEvapTemp");
        static constexpr std::string_view RoutineNameLoadSideOutletEnthalpy("CalcWatertoAirHPCooling:LoadSideOutletEnthalpy");
        static constexpr std::string_view RoutineNameCompressInletTemp("CalcWatertoAirHPCooling:CompressInletTemp");
        static constexpr std::string_view RoutineNameSuctionPr("CalcWatertoAirHPCooling:SuctionPr");
        static constexpr std::string_view RoutineNameCompSuctionTemp("CalcWatertoAirHPCooling:CompSuctionTemp");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumIteration2;                // Number of Iteration2
        int NumIteration3;                // Number of Iteration3
        int NumIteration4;                // Number of Iteration4 (use of latent degradation model ONLY)
        int SourceSideFluidIndex;         // Source Side Fluid Index
        int CompressorType;               // Type of Compressor ie. Reciprocating,Rotary or Scroll
        std::string SourceSideFluidName;  // Name of source side fluid
        std::string Refrigerant;          // Name of refrigerant
        Real64 NominalCoolingCapacity;    // Nominal Cooling Capacity (W)
        Real64 LoadSideTotalUA;           // Load Side Total Heat Transfer coefficient [W/C]
        Real64 LoadSideoutsideUA;         // Load Side Outside Heat Transfer coefficient [W/C]
        Real64 SourceSideUA;              // Source Side Heat Transfer coefficient [W/C]
        Real64 PressureDrop;              // Suction or Discharge Pressure Drop [Pa]
        Real64 ClearanceFactor;           // Compressor Clearance Factor
        Real64 PistonDisp;                // Compressor Piston Displacement [m3/s]
        Real64 ShTemp;                    // Superheat Temperature [C]
        Real64 LosFac;                    // Compressor Power Loss Factor
        Real64 PowerLos;                  // Constant Part of Power Losses [kW]
        Real64 RefVolFlowRate;            // Refrigerant Volume Flow rate at the beginning
        Real64 VolumeRatio;               // Built-in-volume ratio [~]
        Real64 LeakRateCoeff;             // Coefficient for the relationship between Pressure Ratio and Leakage Rate [~]
        Real64 SourceSideHTRes1;          // Source Side Heat Transfer Resistance coefficient 1 [~]
        Real64 SourceSideHTRes2;          // Source Side Heat Transfer Resistance coefficient 2 [K/kW]
        Real64 HighPressCutoff;           // High Pressure Cut-off [Pa]
        Real64 LowPressCutoff;            // Low Pressure Cut-off [Pa]
        Real64 Quality;                   // Quality of Refrigerant
        Real64 SourceSideMassFlowRate;    // Source Side Mass Flow Rate [kg/s]
        Real64 SourceSideInletTemp;       // Source Side Inlet Temperature [C]
        Real64 SourceSideWaterInletEnth;  // Source Side Outlet Enthalpy [J/kg]
        Real64 SourceSideOutletTemp;      // Source Side Outlet Temperature [C]
        Real64 SourceSideVolFlowRate;     // Source Side Volumetric Flow Rate [m3/s]
        Real64 DegradFactor;              // Degradation Factor [~]
        Real64 CpFluid;                   // Specific heat of source side fluid(J/kg)
        Real64 LoadSideMassFlowRate;      // Load Side Mass Flow Rate [kg/s]
        Real64 LoadSideInletWBTemp;       // Wet-bulb temperature of indoor inlet air [C]
        Real64 LoadSideInletDBTemp;       // Load Side Inlet Dry Bulb Temp [C]
        Real64 LoadSideInletHumRat;       // Load Side Inlet Humidity Ratio [kg/kg]
        Real64 LoadSideOutletDBTemp;      // Load Side Outlet Dry Bulb Temperature [C]
        Real64 LoadSideOutletHumRat;      // Load Side Outlet Humidity Ratio [kg/kg]
        Real64 LoadSideAirInletEnth;      // Load Side Inlet Enthalpy [J/kg]
        Real64 LoadSideAirOutletEnth;     // Load Side Outlet Enthalpy [J/kg]
        Real64 EffectiveSurfaceTemp;      // Effective Surface Temperature [C]
        Real64 EffectiveSatEnth;          // Saturated Enthalpy of Air Corresponding to the Effective Surface Temperature [J/kg]
        Real64 QSource;                   // Source Side Heat Transfer Rate [W]
        Real64 QLoadTotal;                // Load Side Total Heat Transfer Rate [W]
        Real64 QSensible;                 // Load Side Sensible Heat Transfer Rate [W]
        Real64 Power;                     // Power Consumption [W]
        Real64 EvapTemp;                  // Evaporating Temperature [C]
        Real64 ANTUWET;                   // Number of Transfer Unit for Wet Condition
        Real64 EffectWET;                 // Load Side Heat Exchanger Effectiveness
        Real64 EvapSatEnth;               // Saturated Enthalpy of Air Corresponding to the Evaporating Temperature [J/kg]
        Real64 SourceSideEffect;          // Source Side Heat Exchanger Effectiveness
        Real64 SourceSideTemp;            // Source Side Saturated Refrigerant Temperature [C]
        Real64 LoadSideTemp;              // Load Side Saturated Refrigerant Temperature [C]
        Real64 SourceSidePressure;        // Source Side Saturated Refrigerant Pressure [Pa]
        Real64 LoadSidePressure;          // Load Side Saturated Refrigerant Pressure [Pa]
        Real64 SuctionPr;                 // Compressor Suction Pressure [Pa]
        Real64 DischargePr;               // Compressor Discharge Pressure [Pa]
        Real64 CompressInletTemp;         // Temperature of the Refrigerant Entering the Compressor [C]
        Real64 MassRef;                   // Mass Flow Rate of Refrigerant [kg/s]
        Real64 SourceSideOutletEnth;      // Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
        Real64 LoadSideOutletEnth;        // Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
        Real64 CpAir;                     // Specific Heat of Air [J/kg_C]
        Real64 SuperHeatEnth;             // Enthalpy of the Superheated Refrigerant [J/kg]
        Real64 CompSuctionTemp1;          // Guess of the Temperature of the Refrigerant Entering the Compressor #1 [C]
        Real64 CompSuctionTemp2;          // Guess of the Temperature of the Refrigerant Entering the Compressor #2 [C]
        Real64 CompSuctionEnth;           // Enthalpy of the Refrigerant Entering the Compressor [J/kg]
        Real64 CompSuctionDensity;        // Density of the Refrigerant Entering the Compressor [kg/m3]
        Real64 CompSuctionSatTemp;        // Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
        Real64 Twet_Rated;                // Twet at rated conditions (coil air flow rate and air temperatures), sec
        Real64 Gamma_Rated;               // Gamma at rated conditions (coil air flow rate and air temperatures)
        bool LatDegradModelSimFlag;       // Latent degradation model simulation flag
        bool StillSimulatingFlag;         // Final Simulation Flag
        bool Converged;                   // overall convergence Flag
        Real64 QLatRated;                 // Qlatent at rated conditions of indoor(TDB,TWB)=(26.7C,19.4C)
        Real64 QLatActual;                // Qlatent at actual operating conditions
        Real64 SHRss;                     // Sensible heat ratio at steady state
        Real64 SHReff;                    // Effective sensible heat ratio at part-load condition
        Array1D<Real64> Par(4);           // Parameter array passed to RegulaFalsi function
        int SolFlag;                      // Solution flag returned from RegulaFalsi function
        Real64 LoadSideInletDBTemp_Unit;  // calc conditions for unit
        Real64 LoadSideInletHumRat_Unit;  // calc conditions for unit
        Real64 LoadSideAirInletEnth_Unit; // calc conditions for unit
        Real64 LoadResidual;              // loop convergence criteria
        Real64 SourceResidual;            // loop convergence criteria
        Real64 RelaxParam(0.5);           // Relaxation Parameter

        if (state.dataWaterToAirHeatPump->firstTime) {
            // Set indoor air conditions to the rated condition
            state.dataWaterToAirHeatPump->LoadSideInletDBTemp_Init = 26.7;
            state.dataWaterToAirHeatPump->LoadSideInletHumRat_Init = 0.0111;
            state.dataWaterToAirHeatPump->LoadSideAirInletEnth_Init =
                PsyHFnTdbW(state.dataWaterToAirHeatPump->LoadSideInletDBTemp_Init, state.dataWaterToAirHeatPump->LoadSideInletHumRat_Init);
            state.dataWaterToAirHeatPump->firstTime = false;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        NominalCoolingCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CoolingCapacity;
        CompressorType = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType;
        Refrigerant = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Refrigerant;
        LoadSideTotalUA = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff;
        LoadSideoutsideUA = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideOutsideUACoeff;
        PistonDisp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp;
        ClearanceFactor = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompClearanceFactor;
        PressureDrop = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop;
        ShTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SuperheatTemp;
        PowerLos = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PowerLosses;
        LosFac = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LossFactor;
        RefVolFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RefVolFlowRate;
        VolumeRatio = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).VolumeRatio;
        LeakRateCoeff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LeakRateCoeff;
        SourceSideUA = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideUACoeff;
        SourceSideHTRes1 = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR1;
        SourceSideHTRes2 = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR2;
        HighPressCutoff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressCutoff;
        LowPressCutoff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressCutoff;

        LoadSideMassFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate;
        // Set indoor air conditions to the actual condition
        LoadSideInletDBTemp_Unit = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp;
        LoadSideInletHumRat_Unit = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat;
        CpAir = PsyCpAirFnW(LoadSideInletHumRat_Unit);
        LoadSideAirInletEnth_Unit = PsyHFnTdbW(LoadSideInletDBTemp_Unit, LoadSideInletHumRat_Unit);

        SourceSideInletTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp;
        SourceSideWaterInletEnth = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy;
        SourceSideFluidName = state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidName;
        SourceSideFluidIndex = state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidIndex;
        SourceSideMassFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate;
        SourceSideVolFlowRate =
            SourceSideMassFlowRate /
            GetDensityGlycol(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

        Twet_Rated = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Twet_Rated;
        Gamma_Rated = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Gamma_Rated;

        StillSimulatingFlag = true;

        // If heat pump is not operating, return
        if (SensDemand == 0.0 || LoadSideMassFlowRate <= 0.0 || SourceSideMassFlowRate <= 0.0) {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        } else {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = true;
        }

        if (CompOp == 0) {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        }

        if (FirstHVACIteration) {
            state.dataWaterToAirHeatPump->initialQSource_calc = NominalCoolingCapacity;
            state.dataWaterToAirHeatPump->initialQLoadTotal_calc = NominalCoolingCapacity;
        }

        if (state.dataWaterToAirHeatPump->initialQLoadTotal_calc == 0.0)
            state.dataWaterToAirHeatPump->initialQLoadTotal_calc = NominalCoolingCapacity;
        if (state.dataWaterToAirHeatPump->initialQSource_calc == 0.0) state.dataWaterToAirHeatPump->initialQSource_calc = NominalCoolingCapacity;

        // Loop the calculation at least twice depending whether the latent degradation model
        // is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
        // and 2nd iteration to calculate the  QLatent(actual)

        QLatRated = 0.0;
        QLatActual = 0.0;
        // IF((RuntimeFrac .GE. 1.0) .OR. (Twet_rated .LE. 0.0) .OR. (Gamma_rated .LE. 0.0)) THEN
        // Cycling fan does not required latent degradation model, only the constant fan case
        if ((RuntimeFrac >= 1.0) || (Twet_Rated <= 0.0) || (Gamma_Rated <= 0.0) || (CyclingScheme == CycFanCycCoil)) {
            LatDegradModelSimFlag = false;
            // Set NumIteration4=1 so that latent model would quit after 1 simulation with the actual condition
            NumIteration4 = 1;
        } else {
            LatDegradModelSimFlag = true;
            // Set NumIteration4=0 so that latent model would simulate twice with rated and actual condition
            NumIteration4 = 0;
        }

        // Tuned Hoisted quantities out of nested loop that don't change
        Real64 const LoadSideMassFlowRate_CpAir_inv(1.0 / (LoadSideMassFlowRate * CpAir));
        Real64 const LoadSideEffec(
            1.0 -
            std::exp(-LoadSideoutsideUA * LoadSideMassFlowRate_CpAir_inv)); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
        Real64 const LoadSideEffec_MassFlowRate_inv(1.0 / (LoadSideEffec * LoadSideMassFlowRate));
        ANTUWET = LoadSideTotalUA * LoadSideMassFlowRate_CpAir_inv;
        EffectWET = 1.0 - std::exp(-ANTUWET);

        while (true) {
            ++NumIteration4;
            if (NumIteration4 == 1) {
                // Set indoor air conditions to the rated condition
                LoadSideInletDBTemp = state.dataWaterToAirHeatPump->LoadSideInletDBTemp_Init;
                LoadSideInletHumRat = state.dataWaterToAirHeatPump->LoadSideInletHumRat_Init;
                LoadSideAirInletEnth = state.dataWaterToAirHeatPump->LoadSideAirInletEnth_Init;
            } else {
                // Set indoor air conditions to the actual condition
                LoadSideInletDBTemp = LoadSideInletDBTemp_Unit;
                LoadSideInletHumRat = LoadSideInletHumRat_Unit;
                LoadSideAirInletEnth = LoadSideAirInletEnth_Unit;
            }

            // Outerloop: Calculate source side heat transfer
            NumIteration2 = 0;
            Converged = false;
            StillSimulatingFlag = true;
            SourceResidual = 1.0;
            while (StillSimulatingFlag) {
                if (Converged) StillSimulatingFlag = false;

                ++NumIteration2;
                if (NumIteration2 == 1) RelaxParam = 0.5;

                if (NumIteration2 > STOP2) {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                    return;
                }

                // Innerloop: Calculate load side heat transfer
                NumIteration3 = 0;
                LoadResidual = 1.0;
                while (LoadResidual > ERR) {

                    ++NumIteration3;

                    if (NumIteration3 > STOP3) {
                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                        return;
                    }

                    // Determine Effectiveness of Source Side
                    CpFluid =
                        GetSpecificHeatGlycol(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

                    //      IF (SourceSideFluidName=='WATER') THEN
                    if (SourceSideFluidIndex == state.dataWaterToAirHeatPump->WaterIndex) { // SourceSideFluidName=='Water'
                        SourceSideEffect = 1.0 - std::exp(-SourceSideUA / (CpFluid * SourceSideMassFlowRate));
                    } else {
                        DegradFactor = DegradF(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex);
                        SourceSideEffect = 1.0 / ((SourceSideHTRes1 * std::pow(SourceSideVolFlowRate, -0.8)) / DegradFactor + SourceSideHTRes2);
                    }

                    // Determine Source Side Tempertaure (Condensing Temp in this case)
                    SourceSideTemp = SourceSideInletTemp +
                                     state.dataWaterToAirHeatPump->initialQSource_calc / (SourceSideEffect * CpFluid * SourceSideMassFlowRate);

                    // Compute the Effective Surface Temperature
                    EffectiveSatEnth = LoadSideAirInletEnth - state.dataWaterToAirHeatPump->initialQLoadTotal_calc * LoadSideEffec_MassFlowRate_inv;

                    //      ! Set up the Initial Range of Effective Surface Temperature
                    //      IF(.NOT. Converged)THEN
                    //        EffectiveSurfaceTemp1=-100.
                    //        EffectiveSurfaceTemp2=200.
                    //      END IF
                    //      ! Iterate to calculate the effective surface temp from the corresponding enthalpy
                    //      NumIteration1=0
                    //      LOOP1: DO
                    //        NumIteration1=NumIteration1+1
                    //        IF (NumIteration1.GT.STOP1) THEN
                    //          WatertoAirHP(HPNum)%SimFlag = .FALSE.
                    //          RETURN
                    //        END IF
                    //        EffectiveSurfaceTemp=0.5d0*(EffectiveSurfaceTemp1+EffectiveSurfaceTemp2)
                    //        EffectiveSatEnth1=PsyHFnTdbRhPb(EffectiveSurfaceTemp,1.0,PB)
                    //        IF(ABS(EffectiveSatEnth-EffectiveSatEnth1).LT.0.01 .OR. &
                    //          ABS(EffectiveSurfaceTemp1-EffectiveSurfaceTemp2).LT.0.001) THEN
                    //          EXIT LOOP1
                    //        END IF
                    //        IF(EffectiveSatEnth1.LT.EffectiveSatEnth) THEN
                    //          EffectiveSurfaceTemp1=EffectiveSurfaceTemp
                    //        ELSE
                    //          EffectiveSurfaceTemp2=EffectiveSurfaceTemp
                    //        END IF
                    //      END DO LOOP1

                    EffectiveSurfaceTemp = PsyTsatFnHPb(state, EffectiveSatEnth, PB, RoutineNameLoadSideSurfaceTemp);

                    QSensible = LoadSideMassFlowRate * CpAir * (LoadSideInletDBTemp - EffectiveSurfaceTemp) * LoadSideEffec;
                    EvapSatEnth = LoadSideAirInletEnth - state.dataWaterToAirHeatPump->initialQLoadTotal_calc / (EffectWET * LoadSideMassFlowRate);

                    EvapTemp = PsyTsatFnHPb(state, EvapSatEnth, PB, RoutineNameLoadSideEvapTemp);

                    // Load Side Saturated Temperature (Evaporating Temp in this case)
                    LoadSideTemp = EvapTemp;

                    // Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
                    SourceSidePressure = GetSatPressureRefrig(
                        state, Refrigerant, SourceSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                    LoadSidePressure =
                        GetSatPressureRefrig(state, Refrigerant, LoadSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                    if (LoadSidePressure < LowPressCutoff && !FirstHVACIteration) {
                        if (!state.dataGlobal->WarmupFlag) {
                            ShowRecurringWarningErrorAtEnd(state,
                                                           format("WaterToAir Heat pump:cooling [{}] shut off on low pressure < {:.0R}",
                                                                  state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                                                  LowPressCutoff),
                                                           state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressClgError,
                                                           LoadSidePressure,
                                                           LoadSidePressure,
                                                           _,
                                                           "[Pa]",
                                                           "[Pa]");
                        }
                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                        return;
                    }

                    if (SourceSidePressure > HighPressCutoff && !FirstHVACIteration) {
                        if (!state.dataGlobal->WarmupFlag) {
                            ShowRecurringWarningErrorAtEnd(state,
                                                           format("WaterToAir Heat pump:cooling [{}] shut off on high pressure > {:.0R}",
                                                                  state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                                                  HighPressCutoff),
                                                           state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressClgError,
                                                           SourceSideInletTemp,
                                                           SourceSideInletTemp,
                                                           _,
                                                           "SourceSideInletTemp[C]",
                                                           "SourceSideInletTemp[C]");
                        }
                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                        return;
                    }

                    // Determine Suction Pressure & Discharge Pressure at Compressor Exit
                    if (CompressorType == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                        SuctionPr = LoadSidePressure - PressureDrop;
                        DischargePr = SourceSidePressure + PressureDrop;
                    } else if (CompressorType == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                        SuctionPr = LoadSidePressure;
                        DischargePr = SourceSidePressure + PressureDrop;
                    } else if (CompressorType == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                        SuctionPr = LoadSidePressure;
                        DischargePr = SourceSidePressure;
                    }

                    // Determine the Load Side Outlet Enthalpy (Saturated Gas)
                    Quality = 1.0;
                    LoadSideOutletEnth = GetSatEnthalpyRefrig(
                        state, Refrigerant, LoadSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                    // Determine Source Side Outlet Enthalpy (Saturated Liquid)
                    Quality = 0.0;
                    SourceSideOutletEnth = GetSatEnthalpyRefrig(
                        state, Refrigerant, SourceSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                    // Determine Superheated Temperature of the Load Side outlet/compressor Inlet
                    CompressInletTemp = LoadSideTemp + ShTemp;

                    // Determine the Enthalpy of the Superheated Fluid at Load Side Outlet/Compressor Inlet
                    SuperHeatEnth = GetSupHeatEnthalpyRefrig(state,
                                                             Refrigerant,
                                                             CompressInletTemp,
                                                             LoadSidePressure,
                                                             state.dataWaterToAirHeatPump->RefrigIndex,
                                                             RoutineNameCompressInletTemp);

                    // Determining the suction state of the fluid from inlet state involves interation
                    // Method employed...
                    // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
                    // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached
                    if (!Converged) {
                        CompSuctionSatTemp =
                            GetSatTemperatureRefrig(state, Refrigerant, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSuctionPr);
                        CompSuctionTemp1 = CompSuctionSatTemp;

                        // Shoot into the Superheated Region
                        CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat;
                    }

                    //  Do not need the name of the refrigerant if we already have the index (from above CALLs)
                    Par(1) = SuctionPr;
                    Par(2) = double(state.dataWaterToAirHeatPump->RefrigIndex);
                    Par(3) = SuperHeatEnth;

                    General::SolveRoot(state,
                                       ERR,
                                       STOP1,
                                       SolFlag,
                                       state.dataWaterToAirHeatPump->CompSuctionTemp,
                                       CalcCompSuctionTempResidual,
                                       CompSuctionTemp1,
                                       CompSuctionTemp2,
                                       Par);
                    if (SolFlag == -1) {
                        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                        return;
                    }
                    CompSuctionEnth = GetSupHeatEnthalpyRefrig(state,
                                                               Refrigerant,
                                                               state.dataWaterToAirHeatPump->CompSuctionTemp,
                                                               SuctionPr,
                                                               state.dataWaterToAirHeatPump->RefrigIndex,
                                                               RoutineNameCompSuctionTemp);
                    CompSuctionDensity = GetSupHeatDensityRefrig(state,
                                                                 Refrigerant,
                                                                 state.dataWaterToAirHeatPump->CompSuctionTemp,
                                                                 SuctionPr,
                                                                 state.dataWaterToAirHeatPump->RefrigIndex,
                                                                 RoutineNameCompSuctionTemp);

                    // Find Refrigerant Flow Rate
                    {
                        auto const SELECT_CASE_var(CompressorType);
                        if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                            MassRef = PistonDisp * CompSuctionDensity *
                                      (1.0 + ClearanceFactor - ClearanceFactor * std::pow(DischargePr / SuctionPr, 1.0 / gamma));
                        } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                            MassRef = PistonDisp * CompSuctionDensity;
                        } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                            MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * (DischargePr / SuctionPr);
                        }
                    }
                    MassRef = max(0.0, MassRef);

                    // Find the Load Side Heat Transfer
                    QLoadTotal = MassRef * (LoadSideOutletEnth - SourceSideOutletEnth);
                    LoadResidual = std::abs(QLoadTotal - state.dataWaterToAirHeatPump->initialQLoadTotal_calc) /
                                   state.dataWaterToAirHeatPump->initialQLoadTotal_calc;
                    state.dataWaterToAirHeatPump->initialQLoadTotal_calc +=
                        RelaxParam * (QLoadTotal - state.dataWaterToAirHeatPump->initialQLoadTotal_calc);
                    if (NumIteration3 > 8) RelaxParam = 0.3;
                }

                // Determine the Power Consumption
                {
                    auto const SELECT_CASE_var(CompressorType);
                    if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                        Power = PowerLos + (1.0 / LosFac) * (MassRef * gamma / (gamma - 1.0) * SuctionPr / CompSuctionDensity *
                                                             (std::pow(DischargePr / SuctionPr, (gamma - 1.0) / gamma) - 1.0));
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                        Power = PowerLos + (1.0 / LosFac) * (MassRef * gamma / (gamma - 1.0) * SuctionPr / CompSuctionDensity *
                                                             (std::pow(DischargePr / SuctionPr, (gamma - 1.0) / gamma) - 1.0));
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                        Power = PowerLos + (1.0 / LosFac) * (gamma / (gamma - 1.0)) * SuctionPr * RefVolFlowRate *
                                               (((gamma - 1.0) / gamma) * ((DischargePr / SuctionPr) / VolumeRatio) +
                                                ((1.0 / gamma) * std::pow(VolumeRatio, gamma - 1.0)) - 1.0);
                    }
                }

                // Determine the Sourceside Heat Rate
                QSource = Power + QLoadTotal;
                SourceResidual =
                    std::abs(QSource - state.dataWaterToAirHeatPump->initialQSource_calc) / state.dataWaterToAirHeatPump->initialQSource_calc;
                if (SourceResidual < ERR) Converged = true;
                state.dataWaterToAirHeatPump->initialQSource_calc += RelaxParam * (QSource - state.dataWaterToAirHeatPump->initialQSource_calc);
                if (NumIteration2 > 8) RelaxParam = 0.2;
            }

            if (SuctionPr < LowPressCutoff) {
                ShowWarningError(state, "Heat pump:cooling shut down on low pressure");
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            }

            if (DischargePr > HighPressCutoff && !FirstHVACIteration) {
                ShowWarningError(state, "Heat pump:cooling shut down on high pressure");
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            }

            if (QSensible > QLoadTotal) {
                QSensible = QLoadTotal;
            }

            if (LatDegradModelSimFlag) {
                if (NumIteration4 == 1) {
                    QLatRated = QLoadTotal - QSensible;

                } else if (NumIteration4 == 2) {
                    QLatActual = QLoadTotal - QSensible;
                    SHRss = QSensible / QLoadTotal;
                    LoadSideInletWBTemp = PsyTwbFnTdbWPb(state, LoadSideInletDBTemp, LoadSideInletHumRat, PB);
                    SHReff = CalcEffectiveSHR(
                        state, HPNum, SHRss, CyclingScheme, RuntimeFrac, QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp);
                    //   Update sensible capacity based on effective SHR
                    QSensible = QLoadTotal * SHReff;
                    goto LOOPLatentDegradationModel_exit;
                }
            } else {

                SHReff = QSensible / QLoadTotal;
                goto LOOPLatentDegradationModel_exit;
            }
        }
    LOOPLatentDegradationModel_exit:;

        // calculate coil outlet state variables
        LoadSideAirOutletEnth = LoadSideAirInletEnth - QLoadTotal / LoadSideMassFlowRate;
        LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible * LoadSideMassFlowRate_CpAir_inv;
        LoadSideOutletHumRat = PsyWFnTdbH(state, LoadSideOutletDBTemp, LoadSideAirOutletEnth, RoutineNameLoadSideOutletEnthalpy);
        SourceSideOutletTemp = SourceSideInletTemp + QSource / (SourceSideMassFlowRate * CpWater);

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == ContFanCycCoil) {
            // continuous fan, cycling compressor
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy =
                PartLoadRatio * LoadSideAirOutletEnth + (1.0 - PartLoadRatio) * LoadSideAirInletEnth;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat =
                PartLoadRatio * LoadSideOutletHumRat + (1.0 - PartLoadRatio) * LoadSideInletHumRat;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp =
                PsyTdbFnHW(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy,
                           state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy = LoadSideAirOutletEnth;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat = LoadSideOutletHumRat;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp = LoadSideOutletDBTemp;
        }

        // scale heat transfer rates and power to run time
        QLoadTotal *= PartLoadRatio;
        QSensible *= PartLoadRatio;
        Power *= RuntimeFrac;
        QSource *= PartLoadRatio;

        // Update heat pump data structure
        state.dataHVACGlobal->DXElecCoolingPower = Power;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power = Power;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal = QLoadTotal;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible = QSensible;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent = QLoadTotal - QSensible;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource = QSource;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac = RuntimeFrac;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio = PartLoadRatio;

        //  Air-side outlet conditions are already calculated above
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate = LoadSideMassFlowRate;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp = SourceSideOutletTemp;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterMassFlowRate = SourceSideMassFlowRate;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy = SourceSideWaterInletEnth + QSource / SourceSideMassFlowRate;
    }

    Real64 CalcCompSuctionTempResidual(EnergyPlusData &state,
                                       Real64 const CompSuctionTemp, // HP compressor suction temperature (C)
                                       Array1D<Real64> const &Par    // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the compressor suction temperature for water to air HP's

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to call this Function to converge on a solution

        // Using/Aliasing
        using FluidProperties::GetSupHeatDensityRefrig;
        using FluidProperties::GetSupHeatEnthalpyRefrig;

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning

        // Locals
        int RefrigIndex;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcWaterToAirHPHeating:CalcCompSuctionTemp");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string Refrigerant; // Name of refrigerant
        Real64 SuctionPr;
        Real64 CompSuctionEnth;
        Real64 SuperHeatEnth;

        // Convert parameters to usable variables
        SuctionPr = Par(1);
        RefrigIndex = int(Par(2));
        SuperHeatEnth = Par(3);

        CompSuctionEnth = GetSupHeatEnthalpyRefrig(state, Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineName);

        // Calculate residual based on output calculation flag
        Residuum = (CompSuctionEnth - SuperHeatEnth) / SuperHeatEnth;

        return Residuum;
    }

    void CalcWatertoAirHPHeating(EnergyPlusData &state,
                                 int const HPNum,               // heat pump number
                                 int const CyclingScheme,       // fan/compressor cycling scheme indicator
                                 bool const FirstHVACIteration, // first iteration flag
                                 Real64 const RuntimeFrac,
                                 [[maybe_unused]] bool const InitFlag, // first iteration flag
                                 Real64 const SensDemand,
                                 int const CompOp,
                                 Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       R. Raustad (Oct 2006) Revised iteration technique
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates a parameter estimation based water to air heat pump model

        // Using/Aliasing
        using namespace FluidProperties;
        using General::SolveRoot;
        using Psychrometrics::PsyCpAirFnW; // ,PsyHFnTdbRhPb,PsyWFnTdpPb
        using Psychrometrics::PsyTdbFnHW;
        using Psychrometrics::PsyWFnTdbH;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CpWater(4210.0);         // Specific heat of water J/kg_C
        Real64 const DegreeofSuperheat(80.0); // Initial guess of degree of superheat
        Real64 const gamma(1.114);            // Expnasion Coefficient
        Real64 RelaxParam(0.5);               // Relaxation Parameter
        Real64 const ERR(0.01);               // Error Value
        int const STOP1(1000);                // Iteration stopper1
        int const STOP2(1000);                // Iteration stopper2
        int const STOP3(1000);                // Iteration stopper3

        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcWatertoAirHPHeating:SourceSideInletTemp");
        static constexpr std::string_view RoutineNameSourceSideTemp("CalcWatertoAirHPHeating:SourceSideTemp");
        static constexpr std::string_view RoutineNameLoadSideTemp("CalcWatertoAirHPHeating:LoadSideTemp");
        static constexpr std::string_view RoutineNameLoadSideOutletEnthalpy("CalcWatertoAirHPHeating:LoadSideOutletEnthalpy");
        static constexpr std::string_view RoutineNameCompressInletTemp("CalcWatertoAirHPHeating:CompressInletTemp");
        static constexpr std::string_view RoutineNameSuctionPr("CalcWatertoAirHPHeating:SuctionPr");
        static constexpr std::string_view RoutineNameCompSuctionTemp("CalcWatertoAirHPHeating:CompSuctionTemp");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //      INTEGER                :: NumIteration1            ! Number of Iteration1
        int NumIteration2;        // Number of Iteration2
        int NumIteration3;        // Number of Iteration3
        int SourceSideFluidIndex; // Source Side Fluid Index

        int CompressorType;              // Type of Compressor ie. Reciprocating,Rotary or Scroll
        std::string SourceSideFluidName; // Name of source side fluid
        std::string Refrigerant;         // Name of refrigerant
        //      CHARACTER(len=25) :: CErrCount
        Real64 NominalHeatingCapacity; // Nominal Heating Capacity(W)
        Real64 LoadSideUA;             // Load Side Heat Transfer coefficient [W/C]
        Real64 SourceSideUA;           // Source Side Heat Transfer coefficient [W/C]
        Real64 PressureDrop;           // Suction or Discharge Pressure Drop [Pa]
        Real64 ClearanceFactor;        // Compressor Clearance Factor
        Real64 PistonDisp;             // Compressor Piston Displacement [m3/s]
        Real64 ShTemp;                 // Superheat Temperature [C]
        Real64 LosFac;                 // Compressor Power Loss Factor
        Real64 PowerLos;               // Constant Part of Power Losses [kW]
        Real64 RefVolFlowRate;         // Refrigerant Volume Flow rate at the beginning
        Real64 VolumeRatio;            // Built-in-volume ratio [~]
        Real64 LeakRateCoeff;          // Coefficient for the relationship between
        // Pressure Ratio and Leakage Rate [~]
        Real64 SourceSideHTRes1; // Source Side Heat Transfer Resistance coefficient 1 [~]
        Real64 SourceSideHTRes2; // Source Side Heat Transfer Resistance coefficient 2 [K/kW]
        Real64 HighPressCutoff;  // High Pressure Cut-off [Pa]
        Real64 LowPressCutoff;   // Low Pressure Cut-off [Pa]

        Real64 Quality;
        Real64 SourceSideMassFlowRate;   // Source Side Mass Flow Rate [kg/s]
        Real64 SourceSideInletTemp;      // Source Side Inlet Temperature [C]
        Real64 SourceSideWaterInletEnth; // Source Side Inlet Water Enthalpy [J/kg]
        Real64 SourceSideOutletTemp;     // Source Side Outlet Temperature [C]
        Real64 SourceSideVolFlowRate;    // Source Side Volumetric Flow Rate [m3/s]
        Real64 CpFluid;                  // Specific heat of source side fluid(J/kg)
        Real64 LoadSideMassFlowRate;     // Load Side Mass Flow Rate [kg/s]
        Real64 LoadSideInletDBTemp;      // Load Side Inlet Dry Bulb Temp [C]
        Real64 LoadSideInletHumRat;      // Load Side Inlet Humidity Ratio [kg/kg]
        Real64 LoadSideOutletDBTemp;     // Load Side Outlet Dry Bulb Temperature [C]
        Real64 LoadSideOutletHumRat;     // Load Side Outlet Humidity Ratio [kg/kg]
        Real64 LoadSideAirInletEnth;     // Load Side Inlet Enthalpy [J/kg]
        Real64 LoadSideAirOutletEnth;    // Load Side Outlet Enthalpy [J/kg]
        Real64 CpAir;                    // Specific Heat of Air [J/kg_C]
        Real64 DegradFactor;             // Degradation Factor [~]
        Real64 QSource;                  // Source Side Heat Transfer Rate [W]
        Real64 QLoadTotal;               // Load Side Heat Transfer Rate [W]
        Real64 Power;                    // Power Consumption [W]

        Real64 SourceSideEffect;     // Source Side Heat Exchanger Effectiveness
        Real64 SourceSideTemp;       // Source Side Saturated Refrigerant Temperature [C]
        Real64 LoadSideTemp;         // Load Side Saturated Refrigerant Temperature [C]
        Real64 SourceSidePressure;   // Source Side Saturated Refrigerant Pressure [Pa]
        Real64 LoadSidePressure;     // Load Side Saturated Refrigerant Pressure [Pa]
        Real64 SuctionPr;            // Compressor Suction Pressure [Pa]
        Real64 DischargePr;          // Compressor Discharge Pressure [Pa]
        Real64 CompressInletTemp;    // Temperature of the Refrigerant Entering the Compressor [C]
        Real64 MassRef;              // Mass Flow Rate of Refrigerant [kg/s]
        Real64 SourceSideOutletEnth; // Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
        Real64 LoadSideOutletEnth;   // Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
        Real64 SuperHeatEnth;        // Enthalpy of the Superheated Refrigerant [J/kg]
        Real64 CompSuctionTemp1;     // Guess of the Temperature of the Refrigerant Entering the
        // Compressor #1 [C]
        Real64 CompSuctionTemp2; // Guess of the Temperature of the Refrigerant Entering the
        // Compressor #2 [C]
        Real64 CompSuctionTemp;    // Temperature of the Refrigerant Entering the Compressor [C]
        Real64 CompSuctionEnth;    // Enthalpy of the Refrigerant Entering the Compressor [J/kg]
        Real64 CompSuctionDensity; // Density of the Refrigerant Entering the Compressorkg/m3
        Real64 CompSuctionSatTemp; // Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
        bool StillSimulatingFlag;  // Final Simulation Flag
        bool Converged;            // Overall convergence Flag
        Array1D<Real64> Par(4);    // Parameter array passed to RegulaFalsi function
        int SolFlag;               // Solution flag returned from RegulaFalsi function
        Real64 LoadResidual;       // loop convergence criteria
        Real64 SourceResidual;     // loop convergence criteria

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

        NominalHeatingCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HeatingCapacity;
        CompressorType = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompressorType;
        Refrigerant = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Refrigerant;

        LoadSideUA = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoadSideTotalUACoeff;
        PistonDisp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompPistonDisp;
        ClearanceFactor = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompClearanceFactor;
        PressureDrop = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).CompSucPressDrop;
        ShTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SuperheatTemp;
        PowerLos = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PowerLosses;
        LosFac = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LossFactor;
        RefVolFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RefVolFlowRate;
        VolumeRatio = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).VolumeRatio;
        LeakRateCoeff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LeakRateCoeff;
        HighPressCutoff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressCutoff;
        LowPressCutoff = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressCutoff;
        SourceSideUA = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideUACoeff;
        SourceSideHTRes1 = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR1;
        SourceSideHTRes2 = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SourceSideHTR2;

        LoadSideMassFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate;
        LoadSideInletDBTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp;
        LoadSideInletHumRat = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat;
        CpAir = PsyCpAirFnW(LoadSideInletHumRat);

        SourceSideInletTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp;
        SourceSideWaterInletEnth = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy;
        SourceSideFluidName = state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidName;
        SourceSideFluidIndex = state.dataPlnt->PlantLoop(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum).FluidIndex;
        SourceSideMassFlowRate = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate;
        SourceSideVolFlowRate =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate /
            GetDensityGlycol(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

        // Load Side Inlet Air Enthalpy
        LoadSideAirInletEnth = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirEnthalpy;

        // If heat pump is not operating, return
        if (SensDemand == 0.0 || LoadSideMassFlowRate <= 0.0 || SourceSideMassFlowRate <= 0.0) {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        } else {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = true;
        }

        if (CompOp == 0) {
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        }

        if (FirstHVACIteration) {
            state.dataWaterToAirHeatPump->initialQLoad = NominalHeatingCapacity;
            state.dataWaterToAirHeatPump->initialQSource = NominalHeatingCapacity;
        }

        if (state.dataWaterToAirHeatPump->initialQLoad == 0.0) state.dataWaterToAirHeatPump->initialQLoad = NominalHeatingCapacity;
        if (state.dataWaterToAirHeatPump->initialQSource == 0.0) state.dataWaterToAirHeatPump->initialQSource = NominalHeatingCapacity;

        // Tuned Hoisted quantities out of nested loop that don't change
        Real64 const LoadSideMassFlowRate_CpAir_inv(1.0 / (LoadSideMassFlowRate * CpAir));
        Real64 const LoadSideEffect(
            1.0 - std::exp(-LoadSideUA * LoadSideMassFlowRate_CpAir_inv)); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
        Real64 const LoadSideEffect_CpAir_MassFlowRate_inv(1.0 / (LoadSideEffect * CpAir * LoadSideMassFlowRate));

        // Outerloop: calculate load side heat transfer
        NumIteration3 = 0;
        Converged = false;
        StillSimulatingFlag = true;
        LoadResidual = 1.0;
        while (StillSimulatingFlag) {
            if (Converged) StillSimulatingFlag = false;

            ++NumIteration3;
            if (NumIteration3 == 1) RelaxParam = 0.5;

            if (NumIteration3 > STOP3) {
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                return;
            }

            // Innerloop: calculate load side heat transfer
            NumIteration2 = 0;
            SourceResidual = 1.0;
            while (SourceResidual > ERR) {

                ++NumIteration2;

                if (NumIteration2 > STOP2) {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                    return;
                }

                // Determine Effectiveness of Source Side
                CpFluid =
                    GetSpecificHeatGlycol(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

                //      IF (SourceSideFluidName=='WATER') THEN
                if (SourceSideFluidIndex == state.dataWaterToAirHeatPump->WaterIndex) {
                    SourceSideEffect = 1.0 - std::exp(-SourceSideUA / (CpFluid * SourceSideMassFlowRate)); // SourceSideFluidName=='Water'
                } else {
                    DegradFactor = DegradF(state, SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex);
                    SourceSideEffect = 1.0 / ((SourceSideHTRes1 * std::pow(SourceSideVolFlowRate, -0.8)) / DegradFactor + SourceSideHTRes2);
                }

                // Determine Source Side Tempertaure (Evap. Temp for this mode)
                SourceSideTemp =
                    SourceSideInletTemp - state.dataWaterToAirHeatPump->initialQSource / (SourceSideEffect * CpFluid * SourceSideMassFlowRate);

                // Determine Load Side Tempertaure (Condensing Temp for this mode)
                LoadSideTemp = LoadSideInletDBTemp + state.dataWaterToAirHeatPump->initialQLoad * LoadSideEffect_CpAir_MassFlowRate_inv;

                // Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
                SourceSidePressure =
                    GetSatPressureRefrig(state, Refrigerant, SourceSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                LoadSidePressure =
                    GetSatPressureRefrig(state, Refrigerant, LoadSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);
                if (SourceSidePressure < LowPressCutoff && !FirstHVACIteration) {
                    if (!state.dataGlobal->WarmupFlag) {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       format("WaterToAir Heat pump:heating [{}] shut off on low pressure < {:.0R}",
                                                              state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                                              LowPressCutoff),
                                                       state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).LowPressHtgError,
                                                       SourceSidePressure,
                                                       SourceSidePressure,
                                                       _,
                                                       "[Pa]",
                                                       "[Pa]");
                    }
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                    return;
                }

                if (LoadSidePressure > HighPressCutoff && !FirstHVACIteration) {
                    if (!state.dataGlobal->WarmupFlag) {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       format("WaterToAir Heat pump:heating [{}] shut off on high pressure > {:.0R}",
                                                              state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name,
                                                              HighPressCutoff),
                                                       state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HighPressHtgError,
                                                       SourceSideInletTemp,
                                                       SourceSideInletTemp,
                                                       _,
                                                       "SourceSideInletTemp[C]",
                                                       "SourceSideInletTemp[C]");
                    }
                    //         CALL ShowWarningError(state, 'Heat pump:heating shut off on high pressure')
                    //         WRITE(CErrCount,*) SourceSideInletTemp
                    //         CErrCount=ADJUSTL(CErrCount)
                    //         CALL ShowContinueError(state, 'Source side inlet temperature too low, T='//TRIM(CErrCount))
                    //         CALL ShowContinueError(state, 'Heat pump heating demand not met by plant side')
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                    return;
                }

                // Determine Suction Pressure at Compressor Entrance & Discharge Pressure at Compressor Exit
                {
                    auto const SELECT_CASE_var(CompressorType);
                    if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                        SuctionPr = SourceSidePressure - PressureDrop;
                        DischargePr = LoadSidePressure + PressureDrop;
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                        SuctionPr = SourceSidePressure;
                        DischargePr = LoadSidePressure + PressureDrop;
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                        SuctionPr = SourceSidePressure;
                        DischargePr = LoadSidePressure;
                    }
                }

                // Determine the Source Side Outlet Enthalpy
                // Quality of the refrigerant leaving the evaporator is saturated gas
                Quality = 1.0;
                SourceSideOutletEnth = GetSatEnthalpyRefrig(
                    state, Refrigerant, SourceSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);

                // Determine Load Side Outlet Enthalpy
                // Quality of the refrigerant leaving the condenser is saturated liguid
                Quality = 0.0;
                LoadSideOutletEnth = GetSatEnthalpyRefrig(
                    state, Refrigerant, LoadSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                // Determine Superheated Temperature of the Source Side outlet/compressor Inlet
                CompressInletTemp = SourceSideTemp + ShTemp;

                // Determine the Enathalpy of the Superheated Fluid at Source Side Outlet/Compressor Inlet
                SuperHeatEnth = GetSupHeatEnthalpyRefrig(state,
                                                         Refrigerant,
                                                         CompressInletTemp,
                                                         SourceSidePressure,
                                                         state.dataWaterToAirHeatPump->RefrigIndex,
                                                         RoutineNameCompressInletTemp);

                // Determining the suction state of the fluid from inlet state involves interation
                // Method employed...
                // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
                // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

                if (!Converged) {
                    CompSuctionSatTemp =
                        GetSatTemperatureRefrig(state, Refrigerant, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSuctionPr);
                    CompSuctionTemp1 = CompSuctionSatTemp;

                    // Shoot into the Superheated Region
                    CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat;
                }

                //       ! Iterate to find the Suction State
                //       NumIteration1=0
                //       LOOP: DO
                //           NumIteration1=NumIteration1+1
                //           IF (NumIteration1.GT.STOP1) THEN
                //             WatertoAirHP(HPNum)%SimFlag = .FALSE.
                //             RETURN
                //           END IF
                //               CompSuctionTemp = 0.5d0 * ( CompSuctionTemp1 + CompSuctionTemp2 )
                //               CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
                //               CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
                //               IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < ERR)  THEN
                //                   EXIT LOOP
                //               END IF
                //               IF ( CompsuctionEnth < SuperHeatEnth ) THEN
                //                   CompSuctionTemp1 = CompSuctionTemp
                //               ELSE
                //                   CompSuctionTemp2 = CompSuctionTemp
                //               END IF
                //        END DO LOOP

                //       Do not need the name of the refrigerant if we already have the index (from above CALLs)
                Par(1) = SuctionPr;
                Par(2) = double(state.dataWaterToAirHeatPump->RefrigIndex);
                Par(3) = SuperHeatEnth;

                General::SolveRoot(state, ERR, STOP1, SolFlag, CompSuctionTemp, CalcCompSuctionTempResidual, CompSuctionTemp1, CompSuctionTemp2, Par);
                if (SolFlag == -1) {
                    state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
                    return;
                }
                CompSuctionEnth = GetSupHeatEnthalpyRefrig(
                    state, Refrigerant, CompSuctionTemp, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameCompSuctionTemp);
                CompSuctionDensity = GetSupHeatDensityRefrig(
                    state, Refrigerant, CompSuctionTemp, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameCompSuctionTemp);

                // Find Refrigerant Flow Rate
                {
                    auto const SELECT_CASE_var(CompressorType);
                    if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                        MassRef =
                            PistonDisp * CompSuctionDensity * (1 + ClearanceFactor - ClearanceFactor * std::pow(DischargePr / SuctionPr, 1 / gamma));
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                        MassRef = PistonDisp * CompSuctionDensity;
                    } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                        MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * (DischargePr / SuctionPr);
                    }
                }
                MassRef = max(0.0, MassRef);

                // Find the Source Side Heat Transfer
                QSource = MassRef * (SourceSideOutletEnth - LoadSideOutletEnth);
                SourceResidual = std::abs(QSource - state.dataWaterToAirHeatPump->initialQSource) / state.dataWaterToAirHeatPump->initialQSource;
                state.dataWaterToAirHeatPump->initialQSource += RelaxParam * (QSource - state.dataWaterToAirHeatPump->initialQSource);
                if (NumIteration2 > 8) RelaxParam = 0.3;
            }

            // Determine the Power Consumption
            {
                auto const SELECT_CASE_var(CompressorType);
                if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Reciprocating) { // RECIPROCATING
                    Power = PowerLos + (1 / LosFac) * (MassRef * gamma / (gamma - 1) * SuctionPr / CompSuctionDensity *
                                                       (std::pow(DischargePr / SuctionPr, (gamma - 1) / gamma) - 1));
                } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Rotary) { // ROTARY
                    Power = PowerLos + (1 / LosFac) * (MassRef * gamma / (gamma - 1) * SuctionPr / CompSuctionDensity *
                                                       (std::pow(DischargePr / SuctionPr, (gamma - 1) / gamma) - 1));
                } else if (SELECT_CASE_var == state.dataWaterToAirHeatPump->CompressorType_Scroll) { // SCROLL
                    Power = PowerLos + (1 / LosFac) * (gamma / (gamma - 1)) * SuctionPr * RefVolFlowRate *
                                           (((gamma - 1) / gamma) * ((DischargePr / SuctionPr) / VolumeRatio) +
                                            ((1 / gamma) * std::pow(VolumeRatio, gamma - 1)) - 1);
                }
            }

            // Determine the Load Side Heat Rate
            QLoadTotal = Power + QSource;
            LoadResidual = std::abs(QLoadTotal - state.dataWaterToAirHeatPump->initialQLoad) / state.dataWaterToAirHeatPump->initialQLoad;
            if (LoadResidual < ERR) Converged = true;
            state.dataWaterToAirHeatPump->initialQLoad += RelaxParam * (QLoadTotal - state.dataWaterToAirHeatPump->initialQLoad);
            if (NumIteration3 > 8) RelaxParam = 0.2;
        }

        if (SuctionPr < LowPressCutoff && !FirstHVACIteration) {
            ShowWarningError(state, "Heat pump:heating shut down on low pressure");
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        }

        if (DischargePr > HighPressCutoff && !FirstHVACIteration) {
            ShowWarningError(state, "Heat pump:heating shut down on high pressure");
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag = false;
            return;
        }

        // calculate coil outlet state variables
        LoadSideAirOutletEnth = LoadSideAirInletEnth + QLoadTotal / LoadSideMassFlowRate;
        LoadSideOutletDBTemp = LoadSideInletDBTemp + QLoadTotal / (LoadSideMassFlowRate * CpAir);
        LoadSideOutletHumRat = PsyWFnTdbH(state, LoadSideOutletDBTemp, LoadSideAirOutletEnth, RoutineNameLoadSideOutletEnthalpy);
        SourceSideOutletTemp = SourceSideInletTemp - QSource / (SourceSideMassFlowRate * CpWater);

        // Calculate actual outlet conditions for the run time fraction
        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == ContFanCycCoil) {
            // continuous fan, cycling compressor
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy =
                PartLoadRatio * LoadSideAirOutletEnth + (1.0 - PartLoadRatio) * LoadSideAirInletEnth;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat =
                PartLoadRatio * LoadSideOutletHumRat + (1.0 - PartLoadRatio) * LoadSideInletHumRat;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp =
                PsyTdbFnHW(state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy,
                           state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy = LoadSideAirOutletEnth;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat = LoadSideOutletHumRat;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp = LoadSideOutletDBTemp;
        }
        // scale heat transfer rates and power to run time
        QLoadTotal *= PartLoadRatio;
        Power *= RuntimeFrac;
        QSource *= PartLoadRatio;

        // Update heat pump data structure
        state.dataHVACGlobal->DXElecHeatingPower = Power;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power = Power;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal = QLoadTotal;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible = QLoadTotal;

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource = QSource;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac = RuntimeFrac;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio = PartLoadRatio;

        //  Air-side outlet conditions are already calculated above
        //  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
        //  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
        //  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate = LoadSideMassFlowRate;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp = SourceSideOutletTemp;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterMassFlowRate = SourceSideMassFlowRate;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy = SourceSideWaterInletEnth - QSource / SourceSideMassFlowRate;
    }

    // End Algorithm Section of the Module
    // *****************************************************************************

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the WatertoAirHP Module
    // *****************************************************************************

    void UpdateWatertoAirHP(EnergyPlusData &state, int const HPNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hui Jin
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the Water to Air Heat Pump outlet nodes.

        // METHODOLOGY EMPLOYED:
        // Data is moved from the HP data structure to the HP outlet nodes.

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;
        int WaterInletNode;
        int AirOutletNode;
        int WaterOutletNode;
        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        // WatertoAirHP(HPNum)%SimFlag=.FALSE.
        if (!state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).SimFlag) {
            // Heatpump is off; just pass through conditions
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource = 0.0;
            // These will be overwritten below based on variables above that are already set to 0.
            //  WatertoAirHP(HPNum)%EnergyLoadTotal=0.0
            //  WatertoAirHP(HPNum)%EnergySensible=0.0
            //  WatertoAirHP(HPNum)%EnergySource=0.0
            //  WatertoAirHP(HPNum)%EnergyLatent=0.0
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).RunFrac = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).PartLoadRatio = 0.0;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirDBTemp;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirHumRat;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterTemp;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate =
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterMassFlowRate =
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirEnthalpy;
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy =
                state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy;
        }

        AirInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum;
        WaterInletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum;
        AirOutletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirOutletNodeNum;
        WaterOutletNode = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterOutletNodeNum;

        // Set the outlet air nodes of the WatertoAirHP
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
        state.dataLoopNodes->Node(AirOutletNode).Temp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirDBTemp;
        state.dataLoopNodes->Node(AirOutletNode).HumRat = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirHumRat;
        state.dataLoopNodes->Node(AirOutletNode).Enthalpy = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirEnthalpy;

        // Set the outlet nodes for properties that just pass through & not used
        SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);
        // Set the outlet water nodes for the heat pump
        state.dataLoopNodes->Node(WaterOutletNode).Temp = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterTemp;
        state.dataLoopNodes->Node(WaterOutletNode).Enthalpy = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy;

        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(AirOutletNode).Quality = state.dataLoopNodes->Node(AirInletNode).Quality;
        state.dataLoopNodes->Node(AirOutletNode).Press = state.dataLoopNodes->Node(AirInletNode).Press;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail;

        // Pass through the load side mass flow rates
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletAirMassFlowRate =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletAirMassFlowRate;

        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Energy = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power * ReportingConstant;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLoadTotal =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLoadTotal * ReportingConstant;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySensible =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSensible * ReportingConstant;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergyLatent =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QLatent * ReportingConstant;
        state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).EnergySource =
            state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource * ReportingConstant;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(AirOutletNode).CO2 = state.dataLoopNodes->Node(AirInletNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(AirOutletNode).GenContam = state.dataLoopNodes->Node(AirInletNode).GenContam;
        }
    }

    //        End of Update subroutines for the WatertoAirHP Module
    // *****************************************************************************

    Real64 CalcEffectiveSHR(EnergyPlusData &state,
                            int const HPNum,         // Index number for cooling coil
                            Real64 const SHRss,      // Steady-state sensible heat ratio
                            int const CyclingScheme, // fan/compressor cycling scheme indicator
                            Real64 const RTF,        // Compressor run-time fraction
                            Real64 const QLatRated,  // Rated latent capacity
                            Real64 const QLatActual, // Actual latent capacity
                            Real64 const EnteringDB, // Entering air dry-bulb temperature
                            Real64 const EnteringWB  // Entering air wet-bulb temperature
    )
    {

        // FUNCTION INFORMATION:
        //    AUTHOR         Richard Raustad, FSEC
        //    DATE WRITTEN   September 2003
        //    MODIFIED       Kenneth Tang (Aug 2004) Added capability for simulating CycFanCycCoil
        //    RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //    Adjust sensible heat ratio to account for degradation of DX coil latent
        //    capacity at part-load (cycling) conditions.

        // METHODOLOGY EMPLOYED:
        //    With model parameters entered by the user, the part-load latent performance
        //    of a DX cooling coil is determined for a constant air flow system with
        //    a cooling coil that cycles on/off. The model calculates the time
        //    required for condensate to begin falling from the cooling coil.
        //    Runtimes greater than this are integrated to a "part-load" latent
        //    capacity which is used to determine the "part-load" sensible heat ratio.
        //    See reference below for additional details (linear decay model, Eq. 8b).
        // REFERENCES:
        //   "A Model to Predict the Latent Capacity of Air Conditioners and
        //    Heat Pumps at Part-Load Conditions with Constant Fan Operation"
        //    1996 ASHRAE Transactions, Volume 102, Part 1, Pp. 266 - 274,
        //    Hugh I. Henderson, Jr., P.E., Kannan Rengarajan, P.E.

        // USE STATEMENTS:

        // Return value
        Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
        //   at the current operating conditions (sec)
        Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
        //   at the current operating conditions
        Real64 Twet_Rated;            // Twet at rated conditions (coil air flow rate and air temperatures), sec
        Real64 Gamma_Rated;           // Gamma at rated conditions (coil air flow rate and air temperatures)
        Real64 Twet_max;              // Maximum allowed value for Twet
        Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
        Real64 HPTimeConstant;        // Heat pump time constant [s]
        Real64 FanDelayTime;          // Fan delay time, time delay for the HP's fan to
        // shut off after compressor cycle off  [s]

        Real64 Ton;     // Coil on time (sec)
        Real64 Toff;    // Coil off time (sec)
        Real64 Toffa;   // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
        Real64 aa;      // Intermediate variable
        Real64 To1;     // Intermediate variable (first guess at To). To = time to the start of moisture removal
        Real64 To2;     // Intermediate variable (second guess at To). To = time to the start of moisture removal
        Real64 Error;   // Error for iteration (DO) loop
        Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

        Twet_Rated = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Twet_Rated;
        Gamma_Rated = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).Gamma_Rated;
        MaxONOFFCyclesperHour = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).MaxONOFFCyclesperHour;
        HPTimeConstant = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).HPTimeConstant;
        FanDelayTime = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).FanDelayTime;

        //  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
        //  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
        //  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
        if ((RTF >= 1.0) || (QLatRated == 0.0) || (QLatActual == 0.0) || (Twet_Rated <= 0.0) || (Gamma_Rated <= 0.0) ||
            (MaxONOFFCyclesperHour <= 0.0) || (HPTimeConstant <= 0.0) || (RTF <= 0.0)) {
            SHReff = SHRss;
            return SHReff;
        }

        Twet_max = 9999.0; // high limit for Twet

        //  Calculate the model parameters at the actual operating conditions
        Twet = min(Twet_Rated * QLatRated / (QLatActual + 1.e-10), Twet_max);
        Gamma = Gamma_Rated * QLatRated * (EnteringDB - EnteringWB) / ((26.7 - 19.4) * QLatActual + 1.e-10);

        //  Calculate the compressor on and off times using a converntional thermostat curve
        Ton = 3600.0 / (4.0 * MaxONOFFCyclesperHour * (1.0 - RTF)); // duration of cooling coil on-cycle (sec)

        if ((CyclingScheme == CycFanCycCoil) && (FanDelayTime != 0.0)) {
            //  For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            //  until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
            Toff = FanDelayTime;
        } else {
            //  For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            //  for the entire heat pump off-cycle.
            Toff = 3600.0 / (4.0 * MaxONOFFCyclesperHour * RTF); // duration of cooling coil off-cycle (sec)
        }

        //  Cap Toff to meet the equation restriction
        if (Gamma > 0.0) {
            Toffa = min(Toff, 2.0 * Twet / Gamma);
        } else {
            Toffa = Toff;
        }

        //  Use sucessive substitution to solve for To
        aa = (Gamma * Toffa) - (0.25 / Twet) * pow_2(Gamma) * pow_2(Toffa);

        To1 = aa + HPTimeConstant;
        Error = 1.0;
        while (Error > 0.001) {
            To2 = aa - HPTimeConstant * (std::exp(-To1 / HPTimeConstant) - 1.0);
            Error = std::abs((To2 - To1) / To1);
            To1 = To2;
        }

        //  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
        //  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
        //  Cap lower limit at -700 to avoid the underflow errors.
        aa = std::exp(max(-700.0, -Ton / HPTimeConstant));
        //  Calculate latent heat ratio multiplier
        LHRmult = max(((Ton - To2) / (Ton + HPTimeConstant * (aa - 1.0))), 0.0);

        //  Calculate part-load or "effective" sensible heat ratio
        SHReff = 1.0 - (1.0 - SHRss) * LHRmult;

        if (SHReff < SHRss) SHReff = SHRss; // Effective SHR can be less than the steady-state SHR
        if (SHReff > 1.0) SHReff = 1.0;     // Effective sensible heat ratio can't be greater than 1.0

        return SHReff;
    }

    Real64 DegradF(EnergyPlusData &state,
                   std::string &FluidName, // Name of glycol used in source side
                   Real64 &Temp,           // Temperature of the fluid
                   int &FluidIndex         // Index number for the fluid
    )
    {
        // FUNCTION INFORMATION:
        //    AUTHOR         Kenneth Tang
        //    DATE WRITTEN   October 2004
        //    MODIFIED       na
        //    RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //    Calculate the degradation factor to predict the heat pump performance
        //    when antifreeze is used.
        // METHODOLOGY EMPLOYED:
        //    Use FluidProperties to calculate the properties of water and glycol
        //    at the given temperature. Then substitute the properties into the equation.
        // REFERENCES:
        //    Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
        //    Oklahoma State University.

        // Using/Aliasing
        using namespace FluidProperties;

        // Return value
        Real64 DegradF;

        // Locals
        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view CalledFrom("HVACWaterToAir:DegradF");

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 VisWater;       // Viscosity of water [mPa-s]
        Real64 DensityWater;   // Density of water [kg/m3]
        Real64 CpWater;        // Specific heat of water [J/kg-K]
        Real64 CondWater;      // Conductivity of water [W/m-K]
        Real64 VisCoolant;     // Viscosity of water [mPa-s]
        Real64 DensityCoolant; // Density of water [kg/m3]
        Real64 CpCoolant;      // Specific heat of water [J/kg-K]
        Real64 CondCoolant;    // Conductivity of water [W/m-K]

        VisWater = GetViscosityGlycol(state, fluidNameWater, Temp, state.dataWaterToAirHeatPump->WaterIndex, CalledFrom);
        DensityWater = GetDensityGlycol(state, fluidNameWater, Temp, state.dataWaterToAirHeatPump->WaterIndex, CalledFrom);
        CpWater = GetSpecificHeatGlycol(state, fluidNameWater, Temp, state.dataWaterToAirHeatPump->WaterIndex, CalledFrom);
        CondWater = GetConductivityGlycol(state, fluidNameWater, Temp, state.dataWaterToAirHeatPump->WaterIndex, CalledFrom);
        VisCoolant = GetViscosityGlycol(state, FluidName, Temp, FluidIndex, CalledFrom);
        DensityCoolant = GetDensityGlycol(state, FluidName, Temp, FluidIndex, CalledFrom);
        CpCoolant = GetSpecificHeatGlycol(state, FluidName, Temp, FluidIndex, CalledFrom);
        CondCoolant = GetConductivityGlycol(state, FluidName, Temp, FluidIndex, CalledFrom);

        DegradF = std::pow(VisCoolant / VisWater, -0.47) * std::pow(DensityCoolant / DensityWater, 0.8) * std::pow(CpCoolant / CpWater, 0.33) *
                  std::pow(CondCoolant / CondWater, 0.67);

        return DegradF;
    }

    int GetCoilIndex(EnergyPlusData &state,
                     std::string const &CoilType, // must match coil types in this module
                     std::string const &CoilName, // must match coil names for the coil type
                     bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the index.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        int IndexNum; // returned index of matched coil

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPump->GetCoilsInputFlag) { // First time subroutine has been entered
            GetWatertoAirHPInput(state);
            state.dataWaterToAirHeatPump->WaterIndex = FindGlycol(state, fluidNameWater); // Initialize the WaterIndex once
            state.dataWaterToAirHeatPump->GetCoilsInputFlag = false;
        }

        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);

        if (IndexNum == 0) {
            ShowSevereError(state, "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }

        return IndexNum;
    }

    Real64 GetCoilCapacity(EnergyPlusData &state,
                           std::string const &CoilType, // must match coil types in this module
                           std::string const &CoilName, // must match coil names for the coil type
                           bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil capacity for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        Real64 CoilCapacity; // returned capacity of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPump->GetCoilsInputFlag) {                            // First time subroutine has been entered
            state.dataWaterToAirHeatPump->WaterIndex = FindGlycol(state, fluidNameWater); // Initialize the WaterIndex once
            GetWatertoAirHPInput(state);
            state.dataWaterToAirHeatPump->GetCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION") ||
            UtilityRoutines::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION")) {
            WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
            if (WhichCoil != 0) {
                if (UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION")) {
                    CoilCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).HeatingCapacity;
                } else {
                    CoilCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).CoolingCapacity;
                }
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CoilCapacity = -1000.0;
        }

        return CoilCapacity;
    }

    int GetCoilInletNode(EnergyPlusData &state,
                         std::string const &CoilType, // must match coil types in this module
                         std::string const &CoilName, // must match coil names for the coil type
                         bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the inlet node.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        int NodeNumber; // returned outlet node of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPump->GetCoilsInputFlag) { // First time subroutine has been entered
            GetWatertoAirHPInput(state);
            state.dataWaterToAirHeatPump->WaterIndex = FindGlycol(state, fluidNameWater); // Initialize the WaterIndex once
            state.dataWaterToAirHeatPump->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).AirInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetCoilOutletNode(EnergyPlusData &state,
                          std::string const &CoilType, // must match coil types in this module
                          std::string const &CoilName, // must match coil names for the coil type
                          bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   July 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the outlet node.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        int NodeNumber; // returned outlet node of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPump->GetCoilsInputFlag) { // First time subroutine has been entered
            GetWatertoAirHPInput(state);
            state.dataWaterToAirHeatPump->WaterIndex = FindGlycol(state, fluidNameWater); // Initialize the WaterIndex once
            state.dataWaterToAirHeatPump->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).AirOutletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

} // namespace WaterToAirHeatPump

} // namespace EnergyPlus
