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

    static constexpr std::string_view fluidNameWater("WATER");

    void SimWatertoAirHP(EnergyPlusData &state,
                         std::string_view CompName,     // component name
                         int &CompIndex,                // Index for Component name
                         Real64 const DesignAirflow,    // design air flow rate
                         int const CyclingScheme,       // cycling scheme--either continuous fan/cycling compressor or
                         bool const FirstHVACIteration, // first iteration flag
                         bool const InitFlag,           // initialization flag used to suppress property routine errors
                         Real64 const SensLoad,         // sensible load
                         Real64 const LatentLoad,       // latent load
                         DataHVACGlobals::CompressorOperation const CompressorOp,
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
            HPNum = Util::FindItemInList(CompName, state.dataWaterToAirHeatPump->WatertoAirHP);
            if (HPNum == 0) {
                ShowFatalError(state, format("WaterToAir HP not found={}", CompName));
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

        if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPType == DataPlant::PlantEquipmentType::CoilWAHPCoolingParamEst) {
            InitWatertoAirHP(state, HPNum, InitFlag, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);
            CalcWatertoAirHPCooling(state, HPNum, CyclingScheme, FirstHVACIteration, InitFlag, SensLoad, CompressorOp, PartLoadRatio);

            UpdateWatertoAirHP(state, HPNum);

        } else if (state.dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPType == DataPlant::PlantEquipmentType::CoilWAHPHeatingParamEst) {
            InitWatertoAirHP(state, HPNum, InitFlag, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);
            CalcWatertoAirHPHeating(state, HPNum, CyclingScheme, FirstHVACIteration, InitFlag, SensLoad, CompressorOp, PartLoadRatio);

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

        constexpr std::array<std::string_view, static_cast<int>(CompressorType::Num)> CompressTypeNamesUC{"RECIPROCATING", "ROTARY", "SCROLL"};

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

            auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

            heatPump.Name = AlphArray(1);
            heatPump.WatertoAirHPType = "COOLING";
            heatPump.WAHPType = DataPlant::PlantEquipmentType::CoilWAHPCoolingParamEst;
            heatPump.Refrigerant = AlphArray(3);
            heatPump.DesignWaterVolFlowRate = NumArray(1);
            heatPump.CoolingCapacity = NumArray(2);
            heatPump.Twet_Rated = NumArray(3);
            heatPump.Gamma_Rated = NumArray(4);

            heatPump.HighPressCutoff = NumArray(5);
            heatPump.LowPressCutoff = NumArray(6);

            heatPump.WaterInletNodeNum = GetOnlySingleNode(state,
                                                           AlphArray(4),
                                                           ErrorsFound,
                                                           DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpParameterEstimation,
                                                           AlphArray(1),
                                                           DataLoopNode::NodeFluidType::Water,
                                                           DataLoopNode::ConnectionType::Inlet,
                                                           NodeInputManager::CompFluidStream::Secondary,
                                                           ObjectIsNotParent);
            heatPump.WaterOutletNodeNum = GetOnlySingleNode(state,
                                                            AlphArray(5),
                                                            ErrorsFound,
                                                            DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpParameterEstimation,
                                                            AlphArray(1),
                                                            DataLoopNode::NodeFluidType::Water,
                                                            DataLoopNode::ConnectionType::Outlet,
                                                            NodeInputManager::CompFluidStream::Secondary,
                                                            ObjectIsNotParent);
            heatPump.AirInletNodeNum = GetOnlySingleNode(state,
                                                         AlphArray(6),
                                                         ErrorsFound,
                                                         DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpParameterEstimation,
                                                         AlphArray(1),
                                                         DataLoopNode::NodeFluidType::Air,
                                                         DataLoopNode::ConnectionType::Inlet,
                                                         NodeInputManager::CompFluidStream::Primary,
                                                         ObjectIsNotParent);
            heatPump.AirOutletNodeNum = GetOnlySingleNode(state,
                                                          AlphArray(7),
                                                          ErrorsFound,
                                                          DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpParameterEstimation,
                                                          AlphArray(1),
                                                          DataLoopNode::NodeFluidType::Air,
                                                          DataLoopNode::ConnectionType::Outlet,
                                                          NodeInputManager::CompFluidStream::Primary,
                                                          ObjectIsNotParent);

            // 2010-01-13 ESL: Jason Glazer noted that these were out of order previously, but they are good now
            heatPump.LoadSideTotalUACoeff = NumArray(7);
            heatPump.LoadSideOutsideUACoeff = NumArray(8);

            if ((heatPump.LoadSideOutsideUACoeff < Constant::rTinyValue) || (heatPump.LoadSideTotalUACoeff < Constant::rTinyValue)) {
                ShowSevereError(state, format("Input problem for {}={}", CurrentModuleObject, heatPump.Name));
                ShowContinueError(state, " One or both load side UA values entered are below tolerance, likely zero or blank.");
                ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                ShowContinueError(state, "  the release of EnergyPlus version 5.");
                ErrorsFound = true;
            }

            heatPump.SuperheatTemp = NumArray(9);
            heatPump.PowerLosses = NumArray(10);
            heatPump.LossFactor = NumArray(11);

            heatPump.compressorType = static_cast<CompressorType>(getEnumValue(CompressTypeNamesUC, Util::makeUPPER(AlphArray(2))));

            switch (heatPump.compressorType) {
            case CompressorType::Reciprocating: {
                heatPump.CompPistonDisp = NumArray(12);
                heatPump.CompSucPressDrop = NumArray(13);
                heatPump.CompClearanceFactor = NumArray(14);
                break;
            }
            case CompressorType::Rotary: {
                heatPump.CompPistonDisp = NumArray(12);
                heatPump.CompSucPressDrop = NumArray(13);
                break;
            }
            case CompressorType::Scroll: {
                heatPump.RefVolFlowRate = NumArray(15);
                heatPump.VolumeRatio = NumArray(16);
                heatPump.LeakRateCoeff = NumArray(17);
                break;
            }
            default: {
                ShowSevereError(
                    state,
                    format("{}Invalid {} ({}) entered. {}={}", RoutineName, cAlphaFields(2), AlphArray(2), CurrentModuleObject, heatPump.Name));
                ErrorsFound = true;
                break;
            }
            }

            heatPump.SourceSideUACoeff = NumArray(18);
            heatPump.SourceSideHTR1 = NumArray(19);
            heatPump.SourceSideHTR2 = NumArray(20);
            heatPump.PLFCurveIndex = Curve::GetCurveIndex(state, AlphArray(8)); // convert curve name to number

            if (heatPump.PLFCurveIndex == 0) {
                if (lAlphaBlanks(8)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, heatPump.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(8)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\".", cAlphaFields(8), AlphArray(8)));
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     heatPump.PLFCurveIndex, // Curve index
                                                     {1},                    // Valid dimensions
                                                     RoutineName,            // Routine name
                                                     CurrentModuleObject,    // Object Type
                                                     heatPump.Name,          // Object Name
                                                     cAlphaFields(8));       // Field Name

                if (!ErrorsFound) {
                    //     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
                    Real64 MinCurveVal = 999.0;
                    Real64 MaxCurveVal = -999.0;
                    Real64 CurveInput = 0.0;
                    Real64 MinCurvePLR{0.0};
                    Real64 MaxCurvePLR{0.0};

                    while (CurveInput <= 1.0) {
                        Real64 CurveVal = Curve::CurveValue(state, heatPump.PLFCurveIndex, CurveInput);
                        if (CurveVal < MinCurveVal) {
                            MinCurveVal = CurveVal;
                            MinCurvePLR = CurveInput;
                        }
                        if (CurveVal > MaxCurveVal) {
                            MaxCurveVal = CurveVal;
                            MaxCurvePLR = CurveInput;
                        }
                        CurveInput += 0.01;
                    }
                    if (MinCurveVal < 0.7) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                        ShowContinueError(state, format("...{}=\"{}\" has out of range values.", cAlphaFields(8), AlphArray(8)));
                        ShowContinueError(state,
                                          format("...Curve minimum must be >= 0.7, curve min at PLR = {:.2T} is {:.3T}", MinCurvePLR, MinCurveVal));
                        ShowContinueError(state, "...Setting curve minimum to 0.7 and simulation continues.");
                        Curve::SetCurveOutputMinValue(state, heatPump.PLFCurveIndex, ErrorsFound, 0.7);
                    }

                    if (MaxCurveVal > 1.0) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                        ShowContinueError(state, format("...{} = {} has out of range value.", cAlphaFields(8), AlphArray(8)));
                        ShowContinueError(state,
                                          format("...Curve maximum must be <= 1.0, curve max at PLR = {:.2T} is {:.3T}", MaxCurvePLR, MaxCurveVal));
                        ShowContinueError(state, "...Setting curve maximum to 1.0 and simulation continues.");
                        Curve::SetCurveOutputMaxValue(state, heatPump.PLFCurveIndex, ErrorsFound, 1.0);
                    }
                }
            }

            heatPump.MaxONOFFCyclesperHour = NumArray(21);
            heatPump.LatentCapacityTimeConstant = NumArray(22);
            heatPump.FanDelayTime = NumArray(23);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(6), AlphArray(7), "Air Nodes");

            // Setup Report variables for the detailed cooling Heat Pump
            // CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                Constant::Units::J,
                                heatPump.Energy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::Electricity,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::Cooling);
            SetupOutputVariable(state,
                                "Cooling Coil Total Cooling Energy",
                                Constant::Units::J,
                                heatPump.EnergyLoadTotal,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::EnergyTransfer,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::CoolingCoils);
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                Constant::Units::J,
                                heatPump.EnergySensible,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                Constant::Units::J,
                                heatPump.EnergyLatent,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name);
            SetupOutputVariable(state,
                                "Cooling Coil Source Side Heat Transfer Energy",
                                Constant::Units::J,
                                heatPump.EnergySource,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::PlantLoopCoolingDemand,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::CoolingCoils);

            // save the design source side flow rate for use by plant loop sizing algorithms
            RegisterPlantCompDesignFlow(state, heatPump.WaterInletNodeNum, 0.5 * heatPump.DesignWaterVolFlowRate);

            // create predefined report entries
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilType, heatPump.Name, CurrentModuleObject);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilTotCap, heatPump.Name, heatPump.CoolingCapacity);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSensCap, heatPump.Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilLatCap, heatPump.Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, heatPump.Name, "-");
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilNomEff, heatPump.Name, "-");
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
            auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

            heatPump.Name = AlphArray(1);
            heatPump.WatertoAirHPType = "HEATING";
            heatPump.WAHPType = DataPlant::PlantEquipmentType::CoilWAHPHeatingParamEst;
            heatPump.Refrigerant = AlphArray(3);
            heatPump.DesignWaterVolFlowRate = NumArray(1);
            heatPump.HeatingCapacity = NumArray(2);

            heatPump.HighPressCutoff = NumArray(3);
            heatPump.LowPressCutoff = NumArray(4);

            heatPump.WaterInletNodeNum = GetOnlySingleNode(state,
                                                           AlphArray(4),
                                                           ErrorsFound,
                                                           DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpParameterEstimation,
                                                           AlphArray(1),
                                                           DataLoopNode::NodeFluidType::Water,
                                                           DataLoopNode::ConnectionType::Inlet,
                                                           NodeInputManager::CompFluidStream::Secondary,
                                                           ObjectIsNotParent);
            heatPump.WaterOutletNodeNum = GetOnlySingleNode(state,
                                                            AlphArray(5),
                                                            ErrorsFound,
                                                            DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpParameterEstimation,
                                                            AlphArray(1),
                                                            DataLoopNode::NodeFluidType::Water,
                                                            DataLoopNode::ConnectionType::Outlet,
                                                            NodeInputManager::CompFluidStream::Secondary,
                                                            ObjectIsNotParent);
            heatPump.AirInletNodeNum = GetOnlySingleNode(state,
                                                         AlphArray(6),
                                                         ErrorsFound,
                                                         DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpParameterEstimation,
                                                         AlphArray(1),
                                                         DataLoopNode::NodeFluidType::Air,
                                                         DataLoopNode::ConnectionType::Inlet,
                                                         NodeInputManager::CompFluidStream::Primary,
                                                         ObjectIsNotParent);
            heatPump.AirOutletNodeNum = GetOnlySingleNode(state,
                                                          AlphArray(7),
                                                          ErrorsFound,
                                                          DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpParameterEstimation,
                                                          AlphArray(1),
                                                          DataLoopNode::NodeFluidType::Air,
                                                          DataLoopNode::ConnectionType::Outlet,
                                                          NodeInputManager::CompFluidStream::Primary,
                                                          ObjectIsNotParent);

            heatPump.LoadSideTotalUACoeff = NumArray(5);
            if (heatPump.LoadSideTotalUACoeff < Constant::rTinyValue) {
                ShowSevereError(state, format("Input problem for {}={}", CurrentModuleObject, heatPump.Name));
                ShowContinueError(state, " Load side UA value is less than tolerance, likely zero or blank.");
                ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                ShowContinueError(state, "  the release of EnergyPlus version 5.");
                ErrorsFound = true;
            }

            heatPump.SuperheatTemp = NumArray(6);
            heatPump.PowerLosses = NumArray(7);
            heatPump.LossFactor = NumArray(8);

            heatPump.compressorType = static_cast<CompressorType>(getEnumValue(CompressTypeNamesUC, Util::makeUPPER(AlphArray(2))));

            switch (heatPump.compressorType) {
            case CompressorType::Reciprocating: {
                heatPump.CompPistonDisp = NumArray(9);
                heatPump.CompSucPressDrop = NumArray(10);
                heatPump.CompClearanceFactor = NumArray(11);
                break;
            }
            case CompressorType::Rotary: {
                heatPump.CompPistonDisp = NumArray(9);
                heatPump.CompSucPressDrop = NumArray(10);
                break;
            }
            case CompressorType::Scroll: {
                heatPump.RefVolFlowRate = NumArray(12);
                heatPump.VolumeRatio = NumArray(13);
                heatPump.LeakRateCoeff = NumArray(14);
                break;
            }
            default: {
                ShowSevereError(
                    state,
                    format("{}Invalid {} ({}) entered. {}={}", RoutineName, cAlphaFields(2), AlphArray(2), CurrentModuleObject, heatPump.Name));
                ErrorsFound = true;
                break;
            }
            }

            heatPump.SourceSideUACoeff = NumArray(15);
            heatPump.SourceSideHTR1 = NumArray(16);
            heatPump.SourceSideHTR2 = NumArray(17);

            heatPump.PLFCurveIndex = Curve::GetCurveIndex(state, AlphArray(8)); // convert curve name to number

            if (heatPump.PLFCurveIndex == 0) {
                if (lAlphaBlanks(8)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, heatPump.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(8)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\".", cAlphaFields(8), AlphArray(8)));
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     heatPump.PLFCurveIndex, // Curve index
                                                     {1},                    // Valid dimensions
                                                     RoutineName,            // Routine name
                                                     CurrentModuleObject,    // Object Type
                                                     heatPump.Name,          // Object Name
                                                     cAlphaFields(8));       // Field Name

                if (!ErrorsFound) {
                    //     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
                    Real64 MinCurveVal = 999.0;
                    Real64 MaxCurveVal = -999.0;
                    Real64 CurveInput = 0.0;
                    Real64 MinCurvePLR{0.0};
                    Real64 MaxCurvePLR{0.0};

                    while (CurveInput <= 1.0) {
                        Real64 CurveVal = Curve::CurveValue(state, heatPump.PLFCurveIndex, CurveInput);
                        if (CurveVal < MinCurveVal) {
                            MinCurveVal = CurveVal;
                            MinCurvePLR = CurveInput;
                        }
                        if (CurveVal > MaxCurveVal) {
                            MaxCurveVal = CurveVal;
                            MaxCurvePLR = CurveInput;
                        }
                        CurveInput += 0.01;
                    }
                    if (MinCurveVal < 0.7) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                        ShowContinueError(state, format("...{}=\"{}\" has out of range values.", cAlphaFields(9), AlphArray(9)));
                        ShowContinueError(state,
                                          format("...Curve minimum must be >= 0.7, curve min at PLR = {:.2T} is {:.3T}", MinCurvePLR, MinCurveVal));
                        ShowContinueError(state, "...Setting curve minimum to 0.7 and simulation continues.");
                        Curve::SetCurveOutputMinValue(state, heatPump.PLFCurveIndex, ErrorsFound, 0.7);
                    }

                    if (MaxCurveVal > 1.0) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, heatPump.Name));
                        ShowContinueError(state, format("...{} = {} has out of range value.", cAlphaFields(9), AlphArray(9)));
                        ShowContinueError(state,
                                          format("...Curve maximum must be <= 1.0, curve max at PLR = {:.2T} is {:.3T}", MaxCurvePLR, MaxCurveVal));
                        ShowContinueError(state, "...Setting curve maximum to 1.0 and simulation continues.");
                        Curve::SetCurveOutputMaxValue(state, heatPump.PLFCurveIndex, ErrorsFound, 1.0);
                    }
                }
            }

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(6), AlphArray(7), "Air Nodes");

            // CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:ParameterEstimation"
            SetupOutputVariable(state,
                                "Heating Coil Electricity Energy",
                                Constant::Units::J,
                                heatPump.Energy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::Electricity,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::Heating);
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                Constant::Units::J,
                                heatPump.EnergyLoadTotal,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::EnergyTransfer,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::HeatingCoils);
            SetupOutputVariable(state,
                                "Heating Coil Source Side Heat Transfer Energy",
                                Constant::Units::J,
                                heatPump.EnergySource,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                heatPump.Name,
                                Constant::eResource::PlantLoopHeatingDemand,
                                OutputProcessor::Group::HVAC,
                                OutputProcessor::EndUseCat::HeatingCoils);

            // save the design source side flow rate for use by plant loop sizing algorithms
            RegisterPlantCompDesignFlow(state, heatPump.WaterInletNodeNum, 0.5 * heatPump.DesignWaterVolFlowRate);

            // create predefined report entries
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilType, heatPump.Name, CurrentModuleObject);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomCap, heatPump.Name, heatPump.HeatingCapacity);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomEff, heatPump.Name, "-");
        }

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        lAlphaBlanks.deallocate();
        cNumericFields.deallocate();
        lNumericBlanks.deallocate();
        NumArray.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, format("{}Errors found getting input. Program terminates.", RoutineName));
        }

        for (HPNum = 1; HPNum <= state.dataWaterToAirHeatPump->NumWatertoAirHPs; ++HPNum) {

            auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);
            if (heatPump.WAHPType == DataPlant::PlantEquipmentType::CoilWAHPCoolingParamEst) {
                // COOLING COIL: Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Cooling Coil Electricity Rate",
                                    Constant::Units::W,
                                    heatPump.Power,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Total Cooling Rate",
                                    Constant::Units::W,
                                    heatPump.QLoadTotal,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Sensible Cooling Rate",
                                    Constant::Units::W,
                                    heatPump.QSensible,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Latent Cooling Rate",
                                    Constant::Units::W,
                                    heatPump.QLatent,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Heat Transfer Rate",
                                    Constant::Units::W,
                                    heatPump.QSource,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Part Load Ratio",
                                    Constant::Units::None,
                                    heatPump.PartLoadRatio,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Runtime Fraction",
                                    Constant::Units::None,
                                    heatPump.RunFrac,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Air Mass Flow Rate",
                                    Constant::Units::kg_s,
                                    heatPump.OutletAirMassFlowRate,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Temperature",
                                    Constant::Units::C,
                                    heatPump.InletAirDBTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    heatPump.InletAirHumRat,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Temperature",
                                    Constant::Units::C,
                                    heatPump.OutletAirDBTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    heatPump.OutletAirHumRat,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Mass Flow Rate",
                                    Constant::Units::kg_s,
                                    heatPump.OutletWaterMassFlowRate,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Inlet Temperature",
                                    Constant::Units::C,
                                    heatPump.InletWaterTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Outlet Temperature",
                                    Constant::Units::C,
                                    heatPump.OutletWaterTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
            } else if (heatPump.WAHPType == DataPlant::PlantEquipmentType::CoilWAHPHeatingParamEst) {
                // HEATING COIL Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Heating Coil Electricity Rate",
                                    Constant::Units::W,
                                    heatPump.Power,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Heating Rate",
                                    Constant::Units::W,
                                    heatPump.QLoadTotal,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Sensible Heating Rate",
                                    Constant::Units::W,
                                    heatPump.QSensible,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Source Side Heat Transfer Rate",
                                    Constant::Units::W,
                                    heatPump.QSource,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Part Load Ratio",
                                    Constant::Units::None,
                                    heatPump.PartLoadRatio,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Runtime Fraction",
                                    Constant::Units::None,
                                    heatPump.RunFrac,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Air Mass Flow Rate",
                                    Constant::Units::kg_s,
                                    heatPump.OutletAirMassFlowRate,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Temperature",
                                    Constant::Units::C,
                                    heatPump.InletAirDBTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    heatPump.InletAirHumRat,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Temperature",
                                    Constant::Units::C,
                                    heatPump.OutletAirDBTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    heatPump.OutletAirHumRat,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Source Side Mass Flow Rate",
                                    Constant::Units::kg_s,
                                    heatPump.OutletWaterMassFlowRate,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Inlet Temperature",
                                    Constant::Units::C,
                                    heatPump.InletWaterTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Outlet Temperature",
                                    Constant::Units::C,
                                    heatPump.OutletWaterTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    heatPump.Name);
            }
        }
    }

    void InitWatertoAirHP(EnergyPlusData &state,
                          int const HPNum, // index to main heat pump data structure
                          bool const InitFlag,
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
        auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

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
            ScanPlantLoopsForObject(state, heatPump.Name, heatPump.WAHPType, heatPump.plantLoc, errFlag, _, _, _, _, _);

            if (state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidName == "WATER") {
                if (heatPump.SourceSideUACoeff < Constant::rTinyValue) {
                    ShowSevereError(state, format("Input problem for water to air heat pump, \"{}\".", heatPump.Name));
                    ShowContinueError(state, " Source side UA value is less than tolerance, likely zero or blank.");
                    ShowContinueError(state, " Verify inputs, as the parameter syntax for this object went through a change with");
                    ShowContinueError(state, "  the release of EnergyPlus version 5.");
                    errFlag = true;
                }
            } else {
                if ((heatPump.SourceSideHTR1 < Constant::rTinyValue) || (heatPump.SourceSideHTR2 < Constant::rTinyValue)) {
                    ShowSevereError(state, format("Input problem for water to air heat pump, \"{}\".", heatPump.Name));
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
            AirInletNode = heatPump.AirInletNodeNum;
            WaterInletNode = heatPump.WaterInletNodeNum;

            // Initialize all report variables to a known state at beginning of simulation
            heatPump.Power = 0.0;
            heatPump.Energy = 0.0;
            heatPump.QLoadTotal = 0.0;
            heatPump.QSensible = 0.0;
            heatPump.QLatent = 0.0;
            heatPump.QSource = 0.0;
            heatPump.EnergyLoadTotal = 0.0;
            heatPump.EnergySensible = 0.0;
            heatPump.EnergyLatent = 0.0;
            heatPump.EnergySource = 0.0;
            heatPump.RunFrac = 0.0;
            heatPump.PartLoadRatio = 0.0;
            heatPump.OutletAirDBTemp = 0.0;
            heatPump.OutletAirHumRat = 0.0;
            heatPump.InletAirDBTemp = 0.0;
            heatPump.InletAirHumRat = 0.0;
            heatPump.OutletWaterTemp = 0.0;
            heatPump.InletWaterTemp = 0.0;
            heatPump.InletAirMassFlowRate = 0.0;
            heatPump.InletWaterMassFlowRate = 0.0;
            heatPump.OutletAirEnthalpy = 0.0;
            heatPump.OutletWaterEnthalpy = 0.0;

            // The rest of the one time initializations
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidName,
                                   Constant::InitConvTemp,
                                   state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidIndex,
                                   RoutineName);
            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidName,
                                       Constant::InitConvTemp,
                                       state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidIndex,
                                       RoutineName);

            heatPump.DesignWaterMassFlowRate = rho * heatPump.DesignWaterVolFlowRate;

            PlantOutletNode = DataPlant::CompData::getPlantComponent(state, heatPump.plantLoc).NodeNumOut;
            InitComponentNodes(state, 0.0, heatPump.DesignWaterMassFlowRate, WaterInletNode, PlantOutletNode);

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

            heatPump.SimFlag = true;

            state.dataWaterToAirHeatPump->MyEnvrnFlag(HPNum) = false;
        } // End If for the Begin Environment initializations

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataWaterToAirHeatPump->MyEnvrnFlag(HPNum) = true;
        }

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.
        // First set the conditions for the air into the heat pump model

        // Set water and air inlet nodes
        AirInletNode = heatPump.AirInletNodeNum;
        WaterInletNode = heatPump.WaterInletNodeNum;

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
            heatPump.InletWaterMassFlowRate = heatPump.DesignWaterMassFlowRate;
            heatPump.InletAirMassFlowRate = DesignAirFlow; // This is required instead of the node temperature
            // because the air loop operates handles part load for
            // cycling equipment by modulating the air flow rate
            // the heat pump model requires an accurate (i.e. full load
            // flow rate for accurate simulation.
        } else { // heat pump is off
            heatPump.InletWaterMassFlowRate = 0.0;

            heatPump.InletAirMassFlowRate = 0.0;
        }
        // constrain water flow provided by plant
        SetComponentFlowRate(state, heatPump.InletWaterMassFlowRate, heatPump.WaterInletNodeNum, heatPump.WaterOutletNodeNum, heatPump.plantLoc);

        heatPump.InletWaterTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
        //  IF (WatertoAirHP(HPNum)%InletWaterTemp < 0.0) THEN  ! Debug trap
        //    Temptemp         = Node(WaterInletNode)%Temp
        //  ENDIF
        heatPump.InletWaterEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;

        heatPump.InletAirDBTemp = state.dataLoopNodes->Node(AirInletNode).Temp;
        heatPump.InletAirHumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
        heatPump.InletAirEnthalpy = state.dataLoopNodes->Node(AirInletNode).Enthalpy;

        heatPump.Power = 0.0;
        heatPump.Energy = 0.0;
        heatPump.QLoadTotal = 0.0;
        heatPump.QSensible = 0.0;
        heatPump.QLatent = 0.0;
        heatPump.QSource = 0.0;
        heatPump.EnergyLoadTotal = 0.0;
        heatPump.EnergySensible = 0.0;
        heatPump.EnergyLatent = 0.0;
        heatPump.EnergySource = 0.0;
        heatPump.RunFrac = 0.0;
        heatPump.OutletAirDBTemp = 0.0;
        heatPump.OutletAirHumRat = 0.0;
        heatPump.OutletWaterTemp = 0.0;
        heatPump.OutletAirEnthalpy = 0.0;
        heatPump.OutletWaterEnthalpy = 0.0;
    }

    void CalcWatertoAirHPCooling(EnergyPlusData &state,
                                 int const HPNum,                      // heat pump number
                                 int const CyclingScheme,              // fan/compressor cycling scheme indicator
                                 bool const FirstHVACIteration,        // first iteration flag
                                 [[maybe_unused]] bool const InitFlag, // suppress property errors if true
                                 Real64 const SensDemand,
                                 DataHVACGlobals::CompressorOperation const CompressorOp,
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
        auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

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
        std::string SourceSideFluidName;  // Name of source side fluid
        Real64 Quality;                   // Quality of Refrigerant
        Real64 SourceSideOutletTemp;      // Source Side Outlet Temperature [C]
        Real64 SourceSideVolFlowRate;     // Source Side Volumetric Flow Rate [m3/s]
        Real64 DegradFactor;              // Degradation Factor [~]
        Real64 CpFluid;                   // Specific heat of source side fluid(J/kg)
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
        bool LatDegradModelSimFlag;       // Latent degradation model simulation flag
        bool StillSimulatingFlag;         // Final Simulation Flag
        bool Converged;                   // overall convergence Flag
        Real64 QLatRated;                 // Qlatent at rated conditions of indoor(TDB,TWB)=(26.7C,19.4C)
        Real64 QLatActual;                // Qlatent at actual operating conditions
        Real64 SHRss;                     // Sensible heat ratio at steady state
        Real64 SHReff;                    // Effective sensible heat ratio at part-load condition
        int SolFlag;                      // Solution flag returned from RegulaFalsi function
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

        //  SET LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        // Set indoor air conditions to the actual condition
        CpAir = PsyCpAirFnW(heatPump.InletAirHumRat);
        LoadSideAirInletEnth_Unit = PsyHFnTdbW(heatPump.InletAirDBTemp, heatPump.InletAirHumRat);
        SourceSideFluidName = state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidName;
        SourceSideFluidIndex = state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidIndex;
        SourceSideVolFlowRate =
            heatPump.InletWaterMassFlowRate /
            GetDensityGlycol(state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

        StillSimulatingFlag = true;

        // If heat pump is not operating, return
        if (SensDemand == 0.0 || heatPump.InletAirMassFlowRate <= 0.0 || heatPump.InletWaterMassFlowRate <= 0.0) {
            heatPump.SimFlag = false;
            return;
        } else {
            heatPump.SimFlag = true;
        }

        if (CompressorOp == DataHVACGlobals::CompressorOperation::Off) {
            heatPump.SimFlag = false;
            return;
        }

        if (FirstHVACIteration) {
            state.dataWaterToAirHeatPump->initialQSource_calc = heatPump.CoolingCapacity;
            state.dataWaterToAirHeatPump->initialQLoadTotal_calc = heatPump.CoolingCapacity;
        }

        if (state.dataWaterToAirHeatPump->initialQLoadTotal_calc == 0.0)
            state.dataWaterToAirHeatPump->initialQLoadTotal_calc = heatPump.CoolingCapacity;
        if (state.dataWaterToAirHeatPump->initialQSource_calc == 0.0) state.dataWaterToAirHeatPump->initialQSource_calc = heatPump.CoolingCapacity;

        // Loop the calculation at least twice depending whether the latent degradation model
        // is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
        // and 2nd iteration to calculate the  QLatent(actual)

        // Calculate Part Load Factor and Runtime Fraction
        Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
        if (heatPump.PLFCurveIndex > 0) {
            PLF = Curve::CurveValue(state, heatPump.PLFCurveIndex, PartLoadRatio); // Calculate part-load factor
        }
        if (CyclingScheme == DataHVACGlobals::CycFanCycCoil) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = PLF;
        }
        heatPump.RunFrac = PartLoadRatio / PLF;

        QLatRated = 0.0;
        QLatActual = 0.0;
        // IF((RuntimeFrac .GE. 1.0) .OR. (Twet_rated .LE. 0.0) .OR. (Gamma_rated .LE. 0.0)) THEN
        // Cycling fan does not required latent degradation model, only the constant fan case
        if ((heatPump.RunFrac >= 1.0) || (heatPump.Twet_Rated <= 0.0) || (heatPump.Gamma_Rated <= 0.0) ||
            (CyclingScheme == DataHVACGlobals::CycFanCycCoil)) {
            LatDegradModelSimFlag = false;
            // Set NumIteration4=1 so that latent model would quit after 1 simulation with the actual condition
            NumIteration4 = 1;
        } else {
            LatDegradModelSimFlag = true;
            // Set NumIteration4=0 so that latent model would simulate twice with rated and actual condition
            NumIteration4 = 0;
        }

        // Tuned Hoisted quantities out of nested loop that don't change
        Real64 const LoadSideMassFlowRate_CpAir_inv(1.0 / (heatPump.InletAirMassFlowRate * CpAir));
        Real64 const LoadSideEffec(1.0 -
                                   std::exp(-heatPump.LoadSideOutsideUACoeff *
                                            LoadSideMassFlowRate_CpAir_inv)); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
        Real64 const LoadSideEffec_MassFlowRate_inv(1.0 / (LoadSideEffec * heatPump.InletAirMassFlowRate));
        ANTUWET = heatPump.LoadSideTotalUACoeff * LoadSideMassFlowRate_CpAir_inv;
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
                LoadSideInletDBTemp = heatPump.InletAirDBTemp;
                LoadSideInletHumRat = heatPump.InletAirHumRat;
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
                    heatPump.SimFlag = false;
                    return;
                }

                // Innerloop: Calculate load side heat transfer
                NumIteration3 = 0;
                LoadResidual = 1.0;
                while (LoadResidual > ERR) {

                    ++NumIteration3;

                    if (NumIteration3 > STOP3) {
                        heatPump.SimFlag = false;
                        return;
                    }

                    // Determine Effectiveness of Source Side
                    CpFluid = GetSpecificHeatGlycol(
                        state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

                    //      IF (SourceSideFluidName=='WATER') THEN
                    if (SourceSideFluidIndex == state.dataWaterToAirHeatPump->WaterIndex) { // SourceSideFluidName=='Water'
                        SourceSideEffect = 1.0 - std::exp(-heatPump.SourceSideUACoeff / (CpFluid * heatPump.InletWaterMassFlowRate));
                    } else {
                        DegradFactor = DegradF(state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex);
                        SourceSideEffect =
                            1.0 / ((heatPump.SourceSideHTR1 * std::pow(SourceSideVolFlowRate, -0.8)) / DegradFactor + heatPump.SourceSideHTR2);
                    }

                    // Determine Source Side Tempertaure (Condensing Temp in this case)
                    SourceSideTemp = heatPump.InletWaterTemp + state.dataWaterToAirHeatPump->initialQSource_calc /
                                                                   (SourceSideEffect * CpFluid * heatPump.InletWaterMassFlowRate);

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

                    QSensible = heatPump.InletAirMassFlowRate * CpAir * (LoadSideInletDBTemp - EffectiveSurfaceTemp) * LoadSideEffec;
                    EvapSatEnth =
                        LoadSideAirInletEnth - state.dataWaterToAirHeatPump->initialQLoadTotal_calc / (EffectWET * heatPump.InletAirMassFlowRate);

                    EvapTemp = PsyTsatFnHPb(state, EvapSatEnth, PB, RoutineNameLoadSideEvapTemp);

                    // Load Side Saturated Temperature (Evaporating Temp in this case)
                    LoadSideTemp = EvapTemp;

                    // Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
                    SourceSidePressure = GetSatPressureRefrig(
                        state, heatPump.Refrigerant, SourceSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                    LoadSidePressure = GetSatPressureRefrig(
                        state, heatPump.Refrigerant, LoadSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                    if (LoadSidePressure < heatPump.LowPressCutoff && !FirstHVACIteration) {
                        if (!state.dataGlobal->WarmupFlag) {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("WaterToAir Heat pump:cooling [{}] shut off on low pressure < {:.0R}", heatPump.Name, heatPump.LowPressCutoff),
                                heatPump.LowPressClgError,
                                LoadSidePressure,
                                LoadSidePressure,
                                _,
                                "[Pa]",
                                "[Pa]");
                        }
                        heatPump.SimFlag = false;
                        return;
                    }

                    if (SourceSidePressure > heatPump.HighPressCutoff && !FirstHVACIteration) {
                        if (!state.dataGlobal->WarmupFlag) {
                            ShowRecurringWarningErrorAtEnd(state,
                                                           format("WaterToAir Heat pump:cooling [{}] shut off on high pressure > {:.0R}",
                                                                  heatPump.Name,
                                                                  heatPump.HighPressCutoff),
                                                           heatPump.HighPressClgError,
                                                           heatPump.InletWaterTemp,
                                                           heatPump.InletWaterTemp,
                                                           _,
                                                           "SourceSideInletTemp[C]",
                                                           "SourceSideInletTemp[C]");
                        }
                        heatPump.SimFlag = false;
                        return;
                    }

                    // Determine Suction Pressure & Discharge Pressure at Compressor Exit
                    if (heatPump.compressorType == CompressorType::Reciprocating) { // RECIPROCATING
                        SuctionPr = LoadSidePressure - heatPump.CompSucPressDrop;
                        DischargePr = SourceSidePressure + heatPump.CompSucPressDrop;
                    } else if (heatPump.compressorType == CompressorType::Rotary) { // ROTARY
                        SuctionPr = LoadSidePressure;
                        DischargePr = SourceSidePressure + heatPump.CompSucPressDrop;
                    } else if (heatPump.compressorType == CompressorType::Scroll) { // SCROLL
                        SuctionPr = LoadSidePressure;
                        DischargePr = SourceSidePressure;
                    }

                    // Determine the Load Side Outlet Enthalpy (Saturated Gas)
                    Quality = 1.0;
                    LoadSideOutletEnth = GetSatEnthalpyRefrig(
                        state, heatPump.Refrigerant, LoadSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                    // Determine Source Side Outlet Enthalpy (Saturated Liquid)
                    Quality = 0.0;
                    SourceSideOutletEnth = GetSatEnthalpyRefrig(
                        state, heatPump.Refrigerant, SourceSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                    // Determine Superheated Temperature of the Load Side outlet/compressor Inlet
                    CompressInletTemp = LoadSideTemp + heatPump.SuperheatTemp;

                    // Determine the Enthalpy of the Superheated Fluid at Load Side Outlet/Compressor Inlet
                    SuperHeatEnth = GetSupHeatEnthalpyRefrig(state,
                                                             heatPump.Refrigerant,
                                                             CompressInletTemp,
                                                             LoadSidePressure,
                                                             state.dataWaterToAirHeatPump->RefrigIndex,
                                                             RoutineNameCompressInletTemp);

                    // Determining the suction state of the fluid from inlet state involves interation
                    // Method employed...
                    // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
                    // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached
                    if (!Converged) {
                        CompSuctionSatTemp = GetSatTemperatureRefrig(
                            state, heatPump.Refrigerant, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSuctionPr);
                        CompSuctionTemp1 = CompSuctionSatTemp;

                        // Shoot into the Superheated Region
                        CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat;
                    }

                    auto f = [&state, SuctionPr, SuperHeatEnth](Real64 const CompSuctionTemp) {
                        static constexpr std::string_view RoutineName("CalcWaterToAirHPHeating:CalcCompSuctionTemp");
                        std::string Refrigerant; // Name of refrigerant
                        int refrigIndex = state.dataWaterToAirHeatPump->RefrigIndex;
                        Real64 compSuctionEnth = GetSupHeatEnthalpyRefrig(state, Refrigerant, CompSuctionTemp, SuctionPr, refrigIndex, RoutineName);
                        return (compSuctionEnth - SuperHeatEnth) / SuperHeatEnth;
                    };

                    General::SolveRoot(
                        state, ERR, STOP1, SolFlag, state.dataWaterToAirHeatPump->CompSuctionTemp, f, CompSuctionTemp1, CompSuctionTemp2);
                    if (SolFlag == -1) {
                        heatPump.SimFlag = false;
                        return;
                    }
                    CompSuctionEnth = GetSupHeatEnthalpyRefrig(state,
                                                               heatPump.Refrigerant,
                                                               state.dataWaterToAirHeatPump->CompSuctionTemp,
                                                               SuctionPr,
                                                               state.dataWaterToAirHeatPump->RefrigIndex,
                                                               RoutineNameCompSuctionTemp);
                    CompSuctionDensity = GetSupHeatDensityRefrig(state,
                                                                 heatPump.Refrigerant,
                                                                 state.dataWaterToAirHeatPump->CompSuctionTemp,
                                                                 SuctionPr,
                                                                 state.dataWaterToAirHeatPump->RefrigIndex,
                                                                 RoutineNameCompSuctionTemp);

                    // Find Refrigerant Flow Rate
                    switch (heatPump.compressorType) {
                    case CompressorType::Reciprocating: {
                        MassRef =
                            heatPump.CompPistonDisp * CompSuctionDensity *
                            (1.0 + heatPump.CompClearanceFactor - heatPump.CompClearanceFactor * std::pow(DischargePr / SuctionPr, 1.0 / gamma));
                        break;
                    }
                    case CompressorType::Rotary: {
                        MassRef = heatPump.CompPistonDisp * CompSuctionDensity;
                        break;
                    }
                    case CompressorType::Scroll: {
                        MassRef = heatPump.RefVolFlowRate * CompSuctionDensity - heatPump.LeakRateCoeff * (DischargePr / SuctionPr);
                        break;
                    }
                    default:
                        break;
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
                switch (heatPump.compressorType) {
                case CompressorType::Reciprocating:
                case CompressorType::Rotary: {
                    Power = heatPump.PowerLosses + (1.0 / heatPump.LossFactor) * (MassRef * gamma / (gamma - 1.0) * SuctionPr / CompSuctionDensity *
                                                                                  (std::pow(DischargePr / SuctionPr, (gamma - 1.0) / gamma) - 1.0));
                    break;
                }
                case CompressorType::Scroll: {
                    Power = heatPump.PowerLosses + (1.0 / heatPump.LossFactor) * (gamma / (gamma - 1.0)) * SuctionPr * heatPump.RefVolFlowRate *
                                                       (((gamma - 1.0) / gamma) * ((DischargePr / SuctionPr) / heatPump.VolumeRatio) +
                                                        ((1.0 / gamma) * std::pow(heatPump.VolumeRatio, gamma - 1.0)) - 1.0);
                    break;
                }
                default:
                    break;
                }

                // Determine the Sourceside Heat Rate
                QSource = Power + QLoadTotal;
                SourceResidual =
                    std::abs(QSource - state.dataWaterToAirHeatPump->initialQSource_calc) / state.dataWaterToAirHeatPump->initialQSource_calc;
                if (SourceResidual < ERR) Converged = true;
                state.dataWaterToAirHeatPump->initialQSource_calc += RelaxParam * (QSource - state.dataWaterToAirHeatPump->initialQSource_calc);
                if (NumIteration2 > 8) RelaxParam = 0.2;
            }

            if (SuctionPr < heatPump.LowPressCutoff) {
                ShowWarningError(state, "Heat pump:cooling shut down on low pressure");
                heatPump.SimFlag = false;
            }

            if (DischargePr > heatPump.HighPressCutoff && !FirstHVACIteration) {
                ShowWarningError(state, "Heat pump:cooling shut down on high pressure");
                heatPump.SimFlag = false;
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
                        state, HPNum, SHRss, CyclingScheme, heatPump.RunFrac, QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp);
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
        LoadSideAirOutletEnth = LoadSideAirInletEnth - QLoadTotal / heatPump.InletAirMassFlowRate;
        LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible * LoadSideMassFlowRate_CpAir_inv;
        LoadSideOutletHumRat = PsyWFnTdbH(state, LoadSideOutletDBTemp, LoadSideAirOutletEnth, RoutineNameLoadSideOutletEnthalpy);
        SourceSideOutletTemp = heatPump.InletWaterTemp + QSource / (heatPump.InletWaterMassFlowRate * CpWater);

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            // continuous fan, cycling compressor
            heatPump.OutletAirEnthalpy = PartLoadRatio * LoadSideAirOutletEnth + (1.0 - PartLoadRatio) * LoadSideAirInletEnth;
            heatPump.OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + (1.0 - PartLoadRatio) * LoadSideInletHumRat;
            heatPump.OutletAirDBTemp = PsyTdbFnHW(heatPump.OutletAirEnthalpy, heatPump.OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            heatPump.OutletAirEnthalpy = LoadSideAirOutletEnth;
            heatPump.OutletAirHumRat = LoadSideOutletHumRat;
            heatPump.OutletAirDBTemp = LoadSideOutletDBTemp;
        }

        // scale heat transfer rates and power to run time
        QLoadTotal *= PartLoadRatio;
        QSensible *= PartLoadRatio;
        Power *= heatPump.RunFrac;
        QSource *= PartLoadRatio;

        // Update heat pump data structure
        state.dataHVACGlobal->DXElecCoolingPower = Power;
        heatPump.Power = Power;
        heatPump.QLoadTotal = QLoadTotal;
        heatPump.QSensible = QSensible;
        heatPump.QLatent = QLoadTotal - QSensible;
        heatPump.QSource = QSource;
        heatPump.PartLoadRatio = PartLoadRatio;

        //  Air-side outlet conditions are already calculated above
        heatPump.OutletAirMassFlowRate = heatPump.InletAirMassFlowRate;
        heatPump.OutletWaterTemp = SourceSideOutletTemp;
        heatPump.OutletWaterMassFlowRate = heatPump.InletWaterMassFlowRate;
        heatPump.OutletWaterEnthalpy = heatPump.InletWaterEnthalpy + QSource / heatPump.InletWaterMassFlowRate;
    }

    void CalcWatertoAirHPHeating(EnergyPlusData &state,
                                 int const HPNum,                      // heat pump number
                                 int const CyclingScheme,              // fan/compressor cycling scheme indicator
                                 bool const FirstHVACIteration,        // first iteration flag
                                 [[maybe_unused]] bool const InitFlag, // first iteration flag
                                 Real64 const SensDemand,
                                 DataHVACGlobals::CompressorOperation const CompressorOp,
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
        auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr CpWater(4210.0);         // Specific heat of water J/kg_C
        Real64 constexpr DegreeofSuperheat(80.0); // Initial guess of degree of superheat
        Real64 constexpr gamma(1.114);            // Expnasion Coefficient
        Real64 RelaxParam(0.5);                   // Relaxation Parameter
        Real64 constexpr ERR(0.01);               // Error Value
        int constexpr STOP1(1000);                // Iteration stopper1
        int constexpr STOP2(1000);                // Iteration stopper2
        int constexpr STOP3(1000);                // Iteration stopper3

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

        std::string SourceSideFluidName; // Name of source side fluid
        //      CHARACTER(len=25) :: CErrCount
        // Pressure Ratio and Leakage Rate [~]
        Real64 Quality;
        Real64 SourceSideOutletTemp;  // Source Side Outlet Temperature [C]
        Real64 SourceSideVolFlowRate; // Source Side Volumetric Flow Rate [m3/s]
        Real64 CpFluid;               // Specific heat of source side fluid(J/kg)
        Real64 LoadSideOutletDBTemp;  // Load Side Outlet Dry Bulb Temperature [C]
        Real64 LoadSideOutletHumRat;  // Load Side Outlet Humidity Ratio [kg/kg]
        Real64 LoadSideAirOutletEnth; // Load Side Outlet Enthalpy [J/kg]
        Real64 CpAir;                 // Specific Heat of Air [J/kg_C]
        Real64 DegradFactor;          // Degradation Factor [~]
        Real64 QSource;               // Source Side Heat Transfer Rate [W]
        Real64 QLoadTotal;            // Load Side Heat Transfer Rate [W]
        Real64 Power;                 // Power Consumption [W]

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
        int SolFlag;               // Solution flag returned from RegulaFalsi function
        Real64 LoadResidual;       // loop convergence criteria
        Real64 SourceResidual;     // loop convergence criteria

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

        CpAir = PsyCpAirFnW(heatPump.InletAirHumRat);
        SourceSideFluidName = state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidName;
        SourceSideFluidIndex = state.dataPlnt->PlantLoop(heatPump.plantLoc.loopNum).FluidIndex;
        SourceSideVolFlowRate =
            heatPump.InletWaterMassFlowRate /
            GetDensityGlycol(state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

        // If heat pump is not operating, return
        if (SensDemand == 0.0 || heatPump.InletAirMassFlowRate <= 0.0 || heatPump.InletWaterMassFlowRate <= 0.0) {
            heatPump.SimFlag = false;
            return;
        } else {
            heatPump.SimFlag = true;
        }

        if (CompressorOp == DataHVACGlobals::CompressorOperation::Off) {
            heatPump.SimFlag = false;
            return;
        }

        if (FirstHVACIteration) {
            state.dataWaterToAirHeatPump->initialQLoad = heatPump.HeatingCapacity;
            state.dataWaterToAirHeatPump->initialQSource = heatPump.HeatingCapacity;
        }

        if (state.dataWaterToAirHeatPump->initialQLoad == 0.0) state.dataWaterToAirHeatPump->initialQLoad = heatPump.HeatingCapacity;
        if (state.dataWaterToAirHeatPump->initialQSource == 0.0) state.dataWaterToAirHeatPump->initialQSource = heatPump.HeatingCapacity;

        // Tuned Hoisted quantities out of nested loop that don't change
        Real64 const LoadSideMassFlowRate_CpAir_inv(1.0 / (heatPump.InletAirMassFlowRate * CpAir));
        Real64 const LoadSideEffect(1.0 -
                                    std::exp(-heatPump.LoadSideTotalUACoeff *
                                             LoadSideMassFlowRate_CpAir_inv)); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
        Real64 const LoadSideEffect_CpAir_MassFlowRate_inv(1.0 / (LoadSideEffect * CpAir * heatPump.InletAirMassFlowRate));

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
                heatPump.SimFlag = false;
                return;
            }

            // Innerloop: calculate load side heat transfer
            NumIteration2 = 0;
            SourceResidual = 1.0;
            while (SourceResidual > ERR) {

                ++NumIteration2;

                if (NumIteration2 > STOP2) {
                    heatPump.SimFlag = false;
                    return;
                }

                // Determine Effectiveness of Source Side
                CpFluid =
                    GetSpecificHeatGlycol(state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp);

                //      IF (SourceSideFluidName=='WATER') THEN
                if (SourceSideFluidIndex == state.dataWaterToAirHeatPump->WaterIndex) {
                    SourceSideEffect =
                        1.0 - std::exp(-heatPump.SourceSideUACoeff / (CpFluid * heatPump.InletWaterMassFlowRate)); // SourceSideFluidName=='Water'
                } else {
                    DegradFactor = DegradF(state, SourceSideFluidName, heatPump.InletWaterTemp, SourceSideFluidIndex);
                    SourceSideEffect =
                        1.0 / ((heatPump.SourceSideHTR1 * std::pow(SourceSideVolFlowRate, -0.8)) / DegradFactor + heatPump.SourceSideHTR2);
                }

                // Determine Source Side Tempertaure (Evap. Temp for this mode)
                SourceSideTemp = heatPump.InletWaterTemp -
                                 state.dataWaterToAirHeatPump->initialQSource / (SourceSideEffect * CpFluid * heatPump.InletWaterMassFlowRate);

                // Determine Load Side Tempertaure (Condensing Temp for this mode)
                LoadSideTemp = heatPump.InletAirDBTemp + state.dataWaterToAirHeatPump->initialQLoad * LoadSideEffect_CpAir_MassFlowRate_inv;

                // Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
                SourceSidePressure = GetSatPressureRefrig(
                    state, heatPump.Refrigerant, SourceSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);
                LoadSidePressure = GetSatPressureRefrig(
                    state, heatPump.Refrigerant, LoadSideTemp, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);
                if (SourceSidePressure < heatPump.LowPressCutoff && !FirstHVACIteration) {
                    if (!state.dataGlobal->WarmupFlag) {
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            format("WaterToAir Heat pump:heating [{}] shut off on low pressure < {:.0R}", heatPump.Name, heatPump.LowPressCutoff),
                            heatPump.LowPressHtgError,
                            SourceSidePressure,
                            SourceSidePressure,
                            _,
                            "[Pa]",
                            "[Pa]");
                    }
                    heatPump.SimFlag = false;
                    return;
                }

                if (LoadSidePressure > heatPump.HighPressCutoff && !FirstHVACIteration) {
                    if (!state.dataGlobal->WarmupFlag) {
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            format("WaterToAir Heat pump:heating [{}] shut off on high pressure > {:.0R}", heatPump.Name, heatPump.HighPressCutoff),
                            heatPump.HighPressHtgError,
                            heatPump.InletWaterTemp,
                            heatPump.InletWaterTemp,
                            _,
                            "SourceSideInletTemp[C]",
                            "SourceSideInletTemp[C]");
                    }
                    //         CALL ShowWarningError(state, 'Heat pump:heating shut off on high pressure')
                    //         WRITE(CErrCount,*) SourceSideInletTemp
                    //         CErrCount=ADJUSTL(CErrCount)
                    //         CALL ShowContinueError(state, 'Source side inlet temperature too low, T='//TRIM(CErrCount))
                    //         CALL ShowContinueError(state, 'Heat pump heating demand not met by plant side')
                    heatPump.SimFlag = false;
                    return;
                }

                // Determine Suction Pressure at Compressor Entrance & Discharge Pressure at Compressor Exit
                switch (heatPump.compressorType) {
                case CompressorType::Reciprocating: {
                    SuctionPr = SourceSidePressure - heatPump.CompSucPressDrop;
                    DischargePr = LoadSidePressure + heatPump.CompSucPressDrop;
                    break;
                }
                case CompressorType::Rotary: {
                    SuctionPr = SourceSidePressure;
                    DischargePr = LoadSidePressure + heatPump.CompSucPressDrop;
                    break;
                }
                case CompressorType::Scroll: {
                    SuctionPr = SourceSidePressure;
                    DischargePr = LoadSidePressure;
                    break;
                }
                default:
                    break;
                }

                // Determine the Source Side Outlet Enthalpy
                // Quality of the refrigerant leaving the evaporator is saturated gas
                Quality = 1.0;
                SourceSideOutletEnth = GetSatEnthalpyRefrig(
                    state, heatPump.Refrigerant, SourceSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSourceSideTemp);

                // Determine Load Side Outlet Enthalpy
                // Quality of the refrigerant leaving the condenser is saturated liguid
                Quality = 0.0;
                LoadSideOutletEnth = GetSatEnthalpyRefrig(
                    state, heatPump.Refrigerant, LoadSideTemp, Quality, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameLoadSideTemp);

                // Determine Superheated Temperature of the Source Side outlet/compressor Inlet
                CompressInletTemp = SourceSideTemp + heatPump.SuperheatTemp;

                // Determine the Enathalpy of the Superheated Fluid at Source Side Outlet/Compressor Inlet
                SuperHeatEnth = GetSupHeatEnthalpyRefrig(state,
                                                         heatPump.Refrigerant,
                                                         CompressInletTemp,
                                                         SourceSidePressure,
                                                         state.dataWaterToAirHeatPump->RefrigIndex,
                                                         RoutineNameCompressInletTemp);

                // Determining the suction state of the fluid from inlet state involves interation
                // Method employed...
                // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
                // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

                if (!Converged) {
                    CompSuctionSatTemp = GetSatTemperatureRefrig(
                        state, heatPump.Refrigerant, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameSuctionPr);
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

                auto f = [&state, SuctionPr, SuperHeatEnth](Real64 const CompSuctionTemp) {
                    static constexpr std::string_view RoutineName("CalcWaterToAirHPHeating:CalcCompSuctionTemp");
                    std::string Refrigerant; // Name of refrigerant
                    int refrigIndex = state.dataWaterToAirHeatPump->RefrigIndex;
                    Real64 compSuctionEnth = GetSupHeatEnthalpyRefrig(state, Refrigerant, CompSuctionTemp, SuctionPr, refrigIndex, RoutineName);
                    return (compSuctionEnth - SuperHeatEnth) / SuperHeatEnth;
                };

                General::SolveRoot(state, ERR, STOP1, SolFlag, CompSuctionTemp, f, CompSuctionTemp1, CompSuctionTemp2);
                if (SolFlag == -1) {
                    heatPump.SimFlag = false;
                    return;
                }
                CompSuctionEnth = GetSupHeatEnthalpyRefrig(
                    state, heatPump.Refrigerant, CompSuctionTemp, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameCompSuctionTemp);
                CompSuctionDensity = GetSupHeatDensityRefrig(
                    state, heatPump.Refrigerant, CompSuctionTemp, SuctionPr, state.dataWaterToAirHeatPump->RefrigIndex, RoutineNameCompSuctionTemp);

                // Find Refrigerant Flow Rate
                switch (heatPump.compressorType) {
                case CompressorType::Reciprocating: {
                    MassRef = heatPump.CompPistonDisp * CompSuctionDensity *
                              (1 + heatPump.CompClearanceFactor - heatPump.CompClearanceFactor * std::pow(DischargePr / SuctionPr, 1 / gamma));
                    break;
                }
                case CompressorType::Rotary: {
                    MassRef = heatPump.CompPistonDisp * CompSuctionDensity;
                    break;
                }
                case CompressorType::Scroll: {
                    MassRef = heatPump.RefVolFlowRate * CompSuctionDensity - heatPump.LeakRateCoeff * (DischargePr / SuctionPr);
                    break;
                }
                default:
                    break;
                }
                MassRef = max(0.0, MassRef);

                // Find the Source Side Heat Transfer
                QSource = MassRef * (SourceSideOutletEnth - LoadSideOutletEnth);
                SourceResidual = std::abs(QSource - state.dataWaterToAirHeatPump->initialQSource) / state.dataWaterToAirHeatPump->initialQSource;
                state.dataWaterToAirHeatPump->initialQSource += RelaxParam * (QSource - state.dataWaterToAirHeatPump->initialQSource);
                if (NumIteration2 > 8) RelaxParam = 0.3;
            }

            // Determine the Power Consumption
            switch (heatPump.compressorType) {
            case CompressorType::Reciprocating:
            case CompressorType::Rotary: {
                Power = heatPump.PowerLosses + (1 / heatPump.LossFactor) * (MassRef * gamma / (gamma - 1) * SuctionPr / CompSuctionDensity *
                                                                            (std::pow(DischargePr / SuctionPr, (gamma - 1) / gamma) - 1));
                break;
            }
            case CompressorType::Scroll: {
                Power = heatPump.PowerLosses + (1 / heatPump.LossFactor) * (gamma / (gamma - 1)) * SuctionPr * heatPump.RefVolFlowRate *
                                                   (((gamma - 1) / gamma) * ((DischargePr / SuctionPr) / heatPump.VolumeRatio) +
                                                    ((1 / gamma) * std::pow(heatPump.VolumeRatio, gamma - 1)) - 1);
                break;
            }
            default:
                break;
            }

            // Determine the Load Side Heat Rate
            QLoadTotal = Power + QSource;
            LoadResidual = std::abs(QLoadTotal - state.dataWaterToAirHeatPump->initialQLoad) / state.dataWaterToAirHeatPump->initialQLoad;
            if (LoadResidual < ERR) Converged = true;
            state.dataWaterToAirHeatPump->initialQLoad += RelaxParam * (QLoadTotal - state.dataWaterToAirHeatPump->initialQLoad);
            if (NumIteration3 > 8) RelaxParam = 0.2;
        }

        if (SuctionPr < heatPump.LowPressCutoff && !FirstHVACIteration) {
            ShowWarningError(state, "Heat pump:heating shut down on low pressure");
            heatPump.SimFlag = false;
            return;
        }

        if (DischargePr > heatPump.HighPressCutoff && !FirstHVACIteration) {
            ShowWarningError(state, "Heat pump:heating shut down on high pressure");
            heatPump.SimFlag = false;
            return;
        }

        // calculate coil outlet state variables
        LoadSideAirOutletEnth = heatPump.InletAirEnthalpy + QLoadTotal / heatPump.InletAirMassFlowRate;
        LoadSideOutletDBTemp = heatPump.InletAirDBTemp + QLoadTotal / (heatPump.InletAirMassFlowRate * CpAir);
        LoadSideOutletHumRat = PsyWFnTdbH(state, LoadSideOutletDBTemp, LoadSideAirOutletEnth, RoutineNameLoadSideOutletEnthalpy);
        SourceSideOutletTemp = heatPump.InletWaterTemp - QSource / (heatPump.InletWaterMassFlowRate * CpWater);

        // Calculate actual outlet conditions for the run time fraction
        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            // continuous fan, cycling compressor
            heatPump.OutletAirEnthalpy = PartLoadRatio * LoadSideAirOutletEnth + (1.0 - PartLoadRatio) * heatPump.InletAirEnthalpy;
            heatPump.OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + (1.0 - PartLoadRatio) * heatPump.InletAirHumRat;
            heatPump.OutletAirDBTemp = PsyTdbFnHW(heatPump.OutletAirEnthalpy, heatPump.OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            heatPump.OutletAirEnthalpy = LoadSideAirOutletEnth;
            heatPump.OutletAirHumRat = LoadSideOutletHumRat;
            heatPump.OutletAirDBTemp = LoadSideOutletDBTemp;
        }

        // Calculate Part Load Factor and Runtime Fraction
        Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
        if (heatPump.PLFCurveIndex > 0) {
            PLF = Curve::CurveValue(state, heatPump.PLFCurveIndex, PartLoadRatio); // Calculate part-load factor
        }
        if (CyclingScheme == DataHVACGlobals::CycFanCycCoil) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = PLF;
        }
        heatPump.RunFrac = PartLoadRatio / PLF;

        // scale heat transfer rates and power to run time
        QLoadTotal *= PartLoadRatio;
        Power *= heatPump.RunFrac;
        QSource *= PartLoadRatio;

        // Update heat pump data structure
        state.dataHVACGlobal->DXElecHeatingPower = Power;
        heatPump.Power = Power;
        heatPump.QLoadTotal = QLoadTotal;
        heatPump.QSensible = QLoadTotal;

        heatPump.QSource = QSource;
        heatPump.PartLoadRatio = PartLoadRatio;

        //  Air-side outlet conditions are already calculated above
        //  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
        //  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
        //  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

        heatPump.OutletAirMassFlowRate = heatPump.InletAirMassFlowRate;
        heatPump.OutletWaterTemp = SourceSideOutletTemp;
        heatPump.OutletWaterMassFlowRate = heatPump.InletWaterMassFlowRate;
        heatPump.OutletWaterEnthalpy = heatPump.InletWaterEnthalpy - QSource / heatPump.InletWaterMassFlowRate;
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
        Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;
        auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);
        using PlantUtilities::SafeCopyPlantNode;

        // WatertoAirHP(HPNum)%SimFlag=.FALSE.
        if (!heatPump.SimFlag) {
            // Heatpump is off; just pass through conditions
            heatPump.Power = 0.0;
            heatPump.Energy = 0.0;
            heatPump.QLoadTotal = 0.0;
            heatPump.QSensible = 0.0;
            heatPump.QLatent = 0.0;
            heatPump.QSource = 0.0;
            // These will be overwritten below based on variables above that are already set to 0.
            //  WatertoAirHP(HPNum)%EnergyLoadTotal=0.0
            //  WatertoAirHP(HPNum)%EnergySensible=0.0
            //  WatertoAirHP(HPNum)%EnergySource=0.0
            //  WatertoAirHP(HPNum)%EnergyLatent=0.0
            heatPump.RunFrac = 0.0;
            heatPump.PartLoadRatio = 0.0;
            heatPump.OutletAirDBTemp = heatPump.InletAirDBTemp;
            heatPump.OutletAirHumRat = heatPump.InletAirHumRat;
            heatPump.OutletWaterTemp = heatPump.InletWaterTemp;
            heatPump.OutletAirMassFlowRate = heatPump.InletAirMassFlowRate;
            heatPump.OutletWaterMassFlowRate = heatPump.InletWaterMassFlowRate;
            heatPump.OutletAirEnthalpy = heatPump.InletAirEnthalpy;
            heatPump.OutletWaterEnthalpy = heatPump.InletWaterEnthalpy;
        }

        // Set the outlet air nodes of the WatertoAirHP
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).Temp = heatPump.OutletAirDBTemp;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).HumRat = heatPump.OutletAirHumRat;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).Enthalpy = heatPump.OutletAirEnthalpy;

        // Set the outlet nodes for properties that just pass through & not used
        SafeCopyPlantNode(state, heatPump.WaterInletNodeNum, heatPump.WaterOutletNodeNum);
        // Set the outlet water nodes for the heat pump
        state.dataLoopNodes->Node(heatPump.WaterOutletNodeNum).Temp = heatPump.OutletWaterTemp;
        state.dataLoopNodes->Node(heatPump.WaterOutletNodeNum).Enthalpy = heatPump.OutletWaterEnthalpy;

        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).Quality = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).Quality;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).Press = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).Press;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).MassFlowRateMin = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRateMin;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRateMax;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRateMaxAvail;

        // Pass through the load side mass flow rates
        heatPump.InletAirMassFlowRate = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).MassFlowRate;
        heatPump.OutletAirMassFlowRate = heatPump.InletAirMassFlowRate;

        heatPump.Energy = heatPump.Power * TimeStepSysSec;
        heatPump.EnergyLoadTotal = heatPump.QLoadTotal * TimeStepSysSec;
        heatPump.EnergySensible = heatPump.QSensible * TimeStepSysSec;
        heatPump.EnergyLatent = heatPump.QLatent * TimeStepSysSec;
        heatPump.EnergySource = heatPump.QSource * TimeStepSysSec;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).CO2 = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(heatPump.AirOutletNodeNum).GenContam = state.dataLoopNodes->Node(heatPump.AirInletNodeNum).GenContam;
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

        // Using/Aliasing
        auto &heatPump = state.dataWaterToAirHeatPump->WatertoAirHP(HPNum);

        // Return value
        Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
        //   at the current operating conditions (sec)
        Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
        //   at the current operating conditions
        Real64 Twet_max; // Maximum allowed value for Twet
        // shut off after compressor cycle off  [s]

        Real64 Ton;     // Coil on time (sec)
        Real64 Toff;    // Coil off time (sec)
        Real64 Toffa;   // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
        Real64 aa;      // Intermediate variable
        Real64 To1;     // Intermediate variable (first guess at To). To = time to the start of moisture removal
        Real64 To2;     // Intermediate variable (second guess at To). To = time to the start of moisture removal
        Real64 Error;   // Error for iteration (DO) loop
        Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

        //  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
        //  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
        //  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
        if ((RTF >= 1.0) || (QLatRated == 0.0) || (QLatActual == 0.0) || (heatPump.Twet_Rated <= 0.0) || (heatPump.Gamma_Rated <= 0.0) ||
            (heatPump.MaxONOFFCyclesperHour <= 0.0) || (heatPump.LatentCapacityTimeConstant <= 0.0) || (RTF <= 0.0)) {
            SHReff = SHRss;
            return SHReff;
        }

        Twet_max = 9999.0; // high limit for Twet

        //  Calculate the model parameters at the actual operating conditions
        Twet = min(heatPump.Twet_Rated * QLatRated / (QLatActual + 1.e-10), Twet_max);
        Gamma = heatPump.Gamma_Rated * QLatRated * (EnteringDB - EnteringWB) / ((26.7 - 19.4) * QLatActual + 1.e-10);

        //  Calculate the compressor on and off times using a conventional thermostat curve
        Ton = 3600.0 / (4.0 * heatPump.MaxONOFFCyclesperHour * (1.0 - RTF)); // duration of cooling coil on-cycle (sec)

        if ((CyclingScheme == DataHVACGlobals::CycFanCycCoil) && (heatPump.FanDelayTime != 0.0)) {
            //  For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            //  until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
            Toff = heatPump.FanDelayTime;
        } else {
            //  For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            //  for the entire heat pump off-cycle.
            Toff = 3600.0 / (4.0 * heatPump.MaxONOFFCyclesperHour * RTF); // duration of cooling coil off-cycle (sec)
        }

        //  Cap Toff to meet the equation restriction
        if (Gamma > 0.0) {
            Toffa = min(Toff, 2.0 * Twet / Gamma);
        } else {
            Toffa = Toff;
        }

        //  Use sucessive substitution to solve for To
        aa = (Gamma * Toffa) - (0.25 / Twet) * pow_2(Gamma) * pow_2(Toffa);

        To1 = aa + heatPump.LatentCapacityTimeConstant;
        Error = 1.0;
        while (Error > 0.001) {
            To2 = aa - heatPump.LatentCapacityTimeConstant * (std::exp(-To1 / heatPump.LatentCapacityTimeConstant) - 1.0);
            Error = std::abs((To2 - To1) / To1);
            To1 = To2;
        }

        //  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
        //  Floating underflow errors occur when -Ton/LatentCapacityTimeConstant is a large negative number.
        //  Cap lower limit at -700 to avoid the underflow errors.
        aa = std::exp(max(-700.0, -Ton / heatPump.LatentCapacityTimeConstant));
        //  Calculate latent heat ratio multiplier
        LHRmult = max(((Ton - To2) / (Ton + heatPump.LatentCapacityTimeConstant * (aa - 1.0))), 0.0);

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

        IndexNum = Util::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);

        if (IndexNum == 0) {
            ShowSevereError(state, format("Could not find CoilType=\"{}\" with Name=\"{}\"", CoilType, CoilName));
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

        if (Util::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION") ||
            Util::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION")) {
            WhichCoil = Util::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
            if (WhichCoil != 0) {
                if (Util::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION")) {
                    CoilCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).HeatingCapacity;
                } else {
                    CoilCapacity = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).CoolingCapacity;
                }
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format("Could not find CoilType=\"{}\" with Name=\"{}\"", CoilType, CoilName));
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

        WhichCoil = Util::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).AirInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format("Could not find CoilType=\"{}\" with Name=\"{}\"", CoilType, CoilName));
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

        WhichCoil = Util::FindItemInList(CoilName, state.dataWaterToAirHeatPump->WatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPump->WatertoAirHP(WhichCoil).AirOutletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format("Could not find CoilType=\"{}\" with Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

} // namespace WaterToAirHeatPump

} // namespace EnergyPlus
