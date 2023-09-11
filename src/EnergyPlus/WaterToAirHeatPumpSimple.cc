// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus {

namespace WaterToAirHeatPumpSimple {

    // Module containing the Water to Air Heat Pump simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Arun Shenoy
    //       DATE WRITTEN   Nov 2003
    //       MODIFIED       Brent Griffith, Sept 2010 plant upgrades
    //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the Water to Air Heat Pump Simple Component

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
    // M.S. Thesis, University of Illinois at Urbana Champaign.
    // (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
    // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
    // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
    // (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
    // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
    // Oklahoma State University. (downloadable from www.hvac.okstate.edu)

    constexpr std::array<std::string_view, static_cast<int>(WatertoAirHP::Num)> WatertoAirHPNamesUC{"HEATING", "COOLING"};

    void SimWatertoAirHPSimple(EnergyPlusData &state,
                               std::string_view CompName, // Coil Name
                               int &CompIndex,            // Index for Component name
                               Real64 const SensLoad,     // Sensible demand load [W]
                               Real64 const LatentLoad,   // Latent demand load [W]
                               int const CyclingScheme,   // Continuous fan OR cycling compressor
                               DataHVACGlobals::CompressorOperation const CompressorOp,
                               Real64 const PartLoadRatio,
                               bool const FirstHVACIteration,
                               Real64 const OnOffAirFlowRatio // ratio of comp on to comp off air flow rate
    )
    {

        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Nov 2003
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Simple Water to Air Heat Pump component simulation.

        // REFERENCES:
        // (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
        // M.S. Thesis, University of Illinois at Urbana Champaign.
        // (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
        // (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)

        // percent on-time (on-time/cycle time)
        // shut off after compressor cycle off  [s]

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HPNum; // The WatertoAirHP that you are currently loading input into

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        if (CompIndex == 0) {
            HPNum = UtilityRoutines::FindItemInList(CompName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);
            if (HPNum == 0) {
                ShowFatalError(state, format("WaterToAirHPSimple not found= {}", CompName));
            }
            CompIndex = HPNum;
        } else {
            HPNum = CompIndex;
            if (HPNum > state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs || HPNum < 1) {
                ShowFatalError(state,
                               format("SimWatertoAirHPSimple: Invalid CompIndex passed={}, Number of Water to Air HPs={}, WaterToAir HP name={}",
                                      HPNum,
                                      state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs,
                                      CompName));
            }
            if (!CompName.empty() && CompName != state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).Name) {
                ShowFatalError(
                    state,
                    format("SimWatertoAirHPSimple: Invalid CompIndex passed={}, WaterToAir HP name={}, stored WaterToAir HP Name for that index={}",
                           HPNum,
                           CompName,
                           state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).Name));
            }
        }

        auto &simpleWAHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        if (simpleWAHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) {
            // Cooling mode
            InitSimpleWatertoAirHP(state, HPNum, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration);
            CalcHPCoolingSimple(state, HPNum, CyclingScheme, SensLoad, LatentLoad, CompressorOp, PartLoadRatio, OnOffAirFlowRatio);
            UpdateSimpleWatertoAirHP(state, HPNum);
        } else if (simpleWAHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit) {
            // Heating mode
            InitSimpleWatertoAirHP(state, HPNum, SensLoad, DataPrecisionGlobals::constant_zero, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration);
            CalcHPHeatingSimple(state, HPNum, CyclingScheme, SensLoad, CompressorOp, PartLoadRatio, OnOffAirFlowRatio);
            UpdateSimpleWatertoAirHP(state, HPNum);
        } else {
            ShowFatalError(state, "SimWatertoAirHPSimple: WatertoAir heatpump not in either HEATING or COOLING mode");
        }
    }

    void GetSimpleWatertoAirHPInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Nov 2003
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for HPs and stores it in HP data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // REFERENCES:
        // (1) Lash.T.A.,1992.Simulation and Analysis of a Water loop Heat Pump System.
        // M.S. Thesis, University of Illinois at Urbana Champaign.
        // (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
        // (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSimpleWatertoAirHPInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HPNum;           // The Water to Air HP that you are currently loading input into
        int NumCool;         // Counter for cooling coil
        int NumHeat;         // Counter for heating coil
        int WatertoAirHPNum; // Counter
        int NumAlphas;       // Number of variables in String format
        int NumNums;         // Number of variables in Numeric format
        int NumParams;       // Total number of input fields
        int MaxNums(0);      // Maximum number of numeric input fields
        int MaxAlphas(0);    // Maximum number of alpha input fields
        int IOStat;
        bool ErrorsFound(false);         // If errors detected in input
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string AlphArray;        // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> NumArray;        // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

        NumCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Cooling:WaterToAirHeatPump:EquationFit");
        NumHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Heating:WaterToAirHeatPump:EquationFit");
        state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs = NumCool + NumHeat;
        HPNum = 0;

        if (state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs <= 0) {
            ShowSevereError(state, "No Equipment found in SimWatertoAirHPSimple");
            ErrorsFound = true;
        }

        // Allocate Arrays
        if (state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs > 0) {
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP.allocate(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs);
            state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag.dimension(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs, true);
            state.dataHeatBal->HeatReclaimSimple_WAHPCoil.allocate(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs);
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "Coil:Cooling:WaterToAirHeatPump:EquationFit", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "Coil:Heating:WaterToAirHeatPump:EquationFit", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        lAlphaBlanks.dimension(MaxAlphas, true);
        cNumericFields.allocate(MaxNums);
        lNumericBlanks.dimension(MaxNums, true);
        NumArray.dimension(MaxNums, 0.0);

        // Get the data for cooling coil
        CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:EquationFit";

        for (WatertoAirHPNum = 1; WatertoAirHPNum <= NumCool; ++WatertoAirHPNum) {

            ++HPNum;
            auto &simpleWAHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));
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
            GlobalNames::VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, format("{} Name", CurrentModuleObject));
            simpleWAHP.Name = AlphArray(1);
            simpleWAHP.WAHPType = WatertoAirHP::Cooling;
            simpleWAHP.WAHPPlantType = DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit;
            simpleWAHP.RatedAirVolFlowRate = NumArray(1);
            simpleWAHP.RatedWaterVolFlowRate = NumArray(2);
            simpleWAHP.RatedCapCoolTotal = NumArray(3);
            simpleWAHP.RatedCapCoolSens = NumArray(4);
            simpleWAHP.RatedCOPCoolAtRatedCdts = NumArray(5);
            simpleWAHP.RatedEntWaterTemp = NumArray(6);
            simpleWAHP.RatedEntAirDrybulbTemp = NumArray(7);
            simpleWAHP.RatedEntAirWetbulbTemp = NumArray(8);
            simpleWAHP.TotalCoolCapCurveIndex = Curve::GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            simpleWAHP.SensCoolCapCurveIndex = Curve::GetCurveIndex(state, AlphArray(7));  // convert curve name to number
            simpleWAHP.CoolPowCurveIndex = Curve::GetCurveIndex(state, AlphArray(8));      // convert curve name to number
            if (simpleWAHP.TotalCoolCapCurveIndex == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(6)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\"", cAlphaFields(6), AlphArray(6)));
                }
                ErrorsFound = true;
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.TotalCoolCapCurveIndex,
                                                     {4},
                                                     RoutineName,
                                                     CurrentModuleObject,
                                                     simpleWAHP.Name,
                                                     "Total Cooling Capacity Curve Name");
            }
            if (simpleWAHP.SensCoolCapCurveIndex == 0) {
                if (lAlphaBlanks(7)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(7)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\"", cAlphaFields(7), AlphArray(7)));
                }
                ErrorsFound = true;
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.SensCoolCapCurveIndex,
                                                     {5},
                                                     RoutineName,
                                                     CurrentModuleObject,
                                                     simpleWAHP.Name,
                                                     "Sensible Cooling Capacity Curve Name");
            }
            if (simpleWAHP.CoolPowCurveIndex == 0) {
                if (lAlphaBlanks(8)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(8)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\"", cAlphaFields(8), AlphArray(8)));
                }
                ErrorsFound = true;
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.CoolPowCurveIndex,
                                                     {4},
                                                     RoutineName,
                                                     CurrentModuleObject,
                                                     simpleWAHP.Name,
                                                     "Cooling Power Consumption Curve Name");
            }
            simpleWAHP.PLFCurveIndex = Curve::GetCurveIndex(state, AlphArray(9)); // convert curve name to number

            if (simpleWAHP.PLFCurveIndex == 0) {
                if (lAlphaBlanks(9)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(9)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\".", cAlphaFields(9), AlphArray(9)));
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.PLFCurveIndex, // Curve index
                                                     {1},                      // Valid dimensions
                                                     RoutineName,              // Routine name
                                                     CurrentModuleObject,      // Object Type
                                                     simpleWAHP.Name,          // Object Name
                                                     cAlphaFields(9));         // Field Name

                if (!ErrorsFound) {
                    //     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
                    Real64 MinCurveVal = 999.0;
                    Real64 MaxCurveVal = -999.0;
                    Real64 CurveInput = 0.0;
                    Real64 MinCurvePLR{0.0};
                    Real64 MaxCurvePLR{0.0};

                    while (CurveInput <= 1.0) {
                        Real64 CurveVal = Curve::CurveValue(state, simpleWAHP.PLFCurveIndex, CurveInput);
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
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                        ShowContinueError(state, format("...{}=\"{}\" has out of range values.", cAlphaFields(9), AlphArray(9)));
                        ShowContinueError(state,
                                          format("...Curve minimum must be >= 0.7, curve min at PLR = {:.2T} is {:.3T}", MinCurvePLR, MinCurveVal));
                        ShowContinueError(state, "...Setting curve minimum to 0.7 and simulation continues.");
                        Curve::SetCurveOutputMinValue(state, simpleWAHP.PLFCurveIndex, ErrorsFound, 0.7);
                    }

                    if (MaxCurveVal > 1.0) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                        ShowContinueError(state, format("...{} = {} has out of range value.", cAlphaFields(9), AlphArray(9)));
                        ShowContinueError(state,
                                          format("...Curve maximum must be <= 1.0, curve max at PLR = {:.2T} is {:.3T}", MaxCurvePLR, MaxCurveVal));
                        ShowContinueError(state, "...Setting curve maximum to 1.0 and simulation continues.");
                        Curve::SetCurveOutputMaxValue(state, simpleWAHP.PLFCurveIndex, ErrorsFound, 1.0);
                    }
                }
            }

            CheckSimpleWAHPRatedCurvesOutputs(state, simpleWAHP.Name);
            simpleWAHP.Twet_Rated = NumArray(9);
            simpleWAHP.Gamma_Rated = NumArray(10);
            simpleWAHP.MaxONOFFCyclesperHour = NumArray(11);
            simpleWAHP.LatentCapacityTimeConstant = NumArray(12);
            simpleWAHP.FanDelayTime = NumArray(13);

            state.dataHeatBal->HeatReclaimSimple_WAHPCoil(WatertoAirHPNum).Name = simpleWAHP.Name;
            state.dataHeatBal->HeatReclaimSimple_WAHPCoil(WatertoAirHPNum).SourceType = CurrentModuleObject;

            simpleWAHP.WaterInletNodeNum = GetOnlySingleNode(state,
                                                             AlphArray(2),
                                                             ErrorsFound,
                                                             DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpEquationFit,
                                                             simpleWAHP.Name,
                                                             DataLoopNode::NodeFluidType::Water,
                                                             DataLoopNode::ConnectionType::Inlet,
                                                             NodeInputManager::CompFluidStream::Secondary,
                                                             DataLoopNode::ObjectIsNotParent);
            simpleWAHP.WaterOutletNodeNum = GetOnlySingleNode(state,
                                                              AlphArray(3),
                                                              ErrorsFound,
                                                              DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpEquationFit,
                                                              simpleWAHP.Name,
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::ConnectionType::Outlet,
                                                              NodeInputManager::CompFluidStream::Secondary,
                                                              DataLoopNode::ObjectIsNotParent);
            simpleWAHP.AirInletNodeNum = GetOnlySingleNode(state,
                                                           AlphArray(4),
                                                           ErrorsFound,
                                                           DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpEquationFit,
                                                           simpleWAHP.Name,
                                                           DataLoopNode::NodeFluidType::Air,
                                                           DataLoopNode::ConnectionType::Inlet,
                                                           NodeInputManager::CompFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
            simpleWAHP.AirOutletNodeNum = GetOnlySingleNode(state,
                                                            AlphArray(5),
                                                            ErrorsFound,
                                                            DataLoopNode::ConnectionObjectType::CoilCoolingWaterToAirHeatPumpEquationFit,
                                                            simpleWAHP.Name,
                                                            DataLoopNode::NodeFluidType::Air,
                                                            DataLoopNode::ConnectionType::Outlet,
                                                            NodeInputManager::CompFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);

            BranchNodeConnections::TestCompSet(state, CurrentModuleObject, simpleWAHP.Name, AlphArray(2), AlphArray(3), "Water Nodes");
            BranchNodeConnections::TestCompSet(state, CurrentModuleObject, simpleWAHP.Name, AlphArray(4), AlphArray(5), "Air Nodes");

            // Setup Report variables for the cooling coil
            // CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:EquationFit"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "Electricity",
                                "Cooling",
                                {},
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                {},
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergySensible,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergyLatent,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name);
            SetupOutputVariable(state,
                                "Cooling Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "PLANTLOOPCOOLINGDEMAND",
                                "COOLINGCOILS",
                                {},
                                "System");

            // create predefined report entries
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilType, simpleWAHP.Name, CurrentModuleObject);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilTotCap, simpleWAHP.Name, simpleWAHP.RatedCapCoolTotal);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilSensCap, simpleWAHP.Name, simpleWAHP.RatedCapCoolSens);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilLatCap, simpleWAHP.Name, simpleWAHP.RatedCapCoolTotal - simpleWAHP.RatedCapCoolSens);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilSHR, simpleWAHP.Name, simpleWAHP.RatedCapCoolSens / simpleWAHP.RatedCapCoolTotal);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilNomEff, simpleWAHP.Name, simpleWAHP.RatedPowerCool / simpleWAHP.RatedCapCoolTotal);

            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPType, simpleWAHP.Name, CurrentModuleObject);
        }

        // Get the data for heating coil
        CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:EquationFit";

        for (WatertoAirHPNum = 1; WatertoAirHPNum <= NumHeat; ++WatertoAirHPNum) {

            ++HPNum;
            auto &simpleWAHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));
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
            GlobalNames::VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, format("{} Name", CurrentModuleObject));
            simpleWAHP.Name = AlphArray(1);
            simpleWAHP.WAHPType = WatertoAirHP::Heating;
            simpleWAHP.WAHPPlantType = DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit;
            simpleWAHP.RatedAirVolFlowRate = NumArray(1);
            simpleWAHP.RatedWaterVolFlowRate = NumArray(2);
            simpleWAHP.RatedCapHeat = NumArray(3);
            simpleWAHP.RatedCOPHeatAtRatedCdts = NumArray(4);
            simpleWAHP.RatedEntWaterTemp = NumArray(5);
            simpleWAHP.RatedEntAirDrybulbTemp = NumArray(6);
            simpleWAHP.RatioRatedHeatRatedTotCoolCap = NumArray(7);
            simpleWAHP.HeatCapCurveIndex = Curve::GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            simpleWAHP.HeatPowCurveIndex = Curve::GetCurveIndex(state, AlphArray(7)); // convert curve name to number
            if (simpleWAHP.HeatCapCurveIndex == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(6)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\"", cAlphaFields(6), AlphArray(6)));
                }
                ErrorsFound = true;
            } else {
                ErrorsFound |= Curve::CheckCurveDims(
                    state, simpleWAHP.HeatCapCurveIndex, {4}, RoutineName, CurrentModuleObject, simpleWAHP.Name, "Heating Capacity Curve Name");
            }
            if (simpleWAHP.HeatPowCurveIndex == 0) {
                if (lAlphaBlanks(7)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(7)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\"", cAlphaFields(7), AlphArray(7)));
                }
                ErrorsFound = true;
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.HeatPowCurveIndex,
                                                     {4},
                                                     RoutineName,
                                                     CurrentModuleObject,
                                                     simpleWAHP.Name,
                                                     "Heating Power Consumption Curve Name");
            }

            simpleWAHP.PLFCurveIndex = Curve::GetCurveIndex(state, AlphArray(8)); // convert curve name to number

            if (simpleWAHP.PLFCurveIndex == 0) {
                if (lAlphaBlanks(8)) {
                    ShowSevereError(state, format("{}{}=\"{}\", missing", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...required {} is blank.", cAlphaFields(8)));
                } else {
                    ShowSevereError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                    ShowContinueError(state, format("...not found {}=\"{}\".", cAlphaFields(8), AlphArray(8)));
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     simpleWAHP.PLFCurveIndex, // Curve index
                                                     {1},                      // Valid dimensions
                                                     RoutineName,              // Routine name
                                                     CurrentModuleObject,      // Object Type
                                                     simpleWAHP.Name,          // Object Name
                                                     cAlphaFields(8));         // Field Name

                if (!ErrorsFound) {
                    //     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
                    Real64 MinCurveVal = 999.0;
                    Real64 MaxCurveVal = -999.0;
                    Real64 CurveInput = 0.0;
                    Real64 MinCurvePLR{0.0};
                    Real64 MaxCurvePLR{0.0};

                    while (CurveInput <= 1.0) {
                        Real64 CurveVal = Curve::CurveValue(state, simpleWAHP.PLFCurveIndex, CurveInput);
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
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                        ShowContinueError(state, format("...{}=\"{}\" has out of range values.", cAlphaFields(8), AlphArray(8)));
                        ShowContinueError(state,
                                          format("...Curve minimum must be >= 0.7, curve min at PLR = {:.2T} is {:.3T}", MinCurvePLR, MinCurveVal));
                        ShowContinueError(state, "...Setting curve minimum to 0.7 and simulation continues.");
                        Curve::SetCurveOutputMinValue(state, simpleWAHP.PLFCurveIndex, ErrorsFound, 0.7);
                    }

                    if (MaxCurveVal > 1.0) {
                        ShowWarningError(state, format("{}{}=\"{}\", invalid", RoutineName, CurrentModuleObject, simpleWAHP.Name));
                        ShowContinueError(state, format("...{} = {} has out of range value.", cAlphaFields(8), AlphArray(8)));
                        ShowContinueError(state,
                                          format("...Curve maximum must be <= 1.0, curve max at PLR = {:.2T} is {:.3T}", MaxCurvePLR, MaxCurveVal));
                        ShowContinueError(state, "...Setting curve maximum to 1.0 and simulation continues.");
                        Curve::SetCurveOutputMaxValue(state, simpleWAHP.PLFCurveIndex, ErrorsFound, 1.0);
                    }
                }
            }

            CheckSimpleWAHPRatedCurvesOutputs(state, simpleWAHP.Name);
            simpleWAHP.WaterInletNodeNum = GetOnlySingleNode(state,
                                                             AlphArray(2),
                                                             ErrorsFound,
                                                             DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpEquationFit,
                                                             simpleWAHP.Name,
                                                             DataLoopNode::NodeFluidType::Water,
                                                             DataLoopNode::ConnectionType::Inlet,
                                                             NodeInputManager::CompFluidStream::Secondary,
                                                             DataLoopNode::ObjectIsNotParent);
            simpleWAHP.WaterOutletNodeNum = GetOnlySingleNode(state,
                                                              AlphArray(3),
                                                              ErrorsFound,
                                                              DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpEquationFit,
                                                              simpleWAHP.Name,
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::ConnectionType::Outlet,
                                                              NodeInputManager::CompFluidStream::Secondary,
                                                              DataLoopNode::ObjectIsNotParent);
            simpleWAHP.AirInletNodeNum = GetOnlySingleNode(state,
                                                           AlphArray(4),
                                                           ErrorsFound,
                                                           DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpEquationFit,
                                                           simpleWAHP.Name,
                                                           DataLoopNode::NodeFluidType::Air,
                                                           DataLoopNode::ConnectionType::Inlet,
                                                           NodeInputManager::CompFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
            simpleWAHP.AirOutletNodeNum = GetOnlySingleNode(state,
                                                            AlphArray(5),
                                                            ErrorsFound,
                                                            DataLoopNode::ConnectionObjectType::CoilHeatingWaterToAirHeatPumpEquationFit,
                                                            simpleWAHP.Name,
                                                            DataLoopNode::NodeFluidType::Air,
                                                            DataLoopNode::ConnectionType::Outlet,
                                                            NodeInputManager::CompFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);

            BranchNodeConnections::TestCompSet(state, CurrentModuleObject, simpleWAHP.Name, AlphArray(2), AlphArray(3), "Water Nodes");
            BranchNodeConnections::TestCompSet(state, CurrentModuleObject, simpleWAHP.Name, AlphArray(4), AlphArray(5), "Air Nodes");

            // CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:EquationFit"
            SetupOutputVariable(state,
                                "Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "Electricity",
                                "Heating",
                                {},
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                {},
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                simpleWAHP.EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                simpleWAHP.Name,
                                {},
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                {},
                                "System");

            // create predefined report entries
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilType, simpleWAHP.Name, CurrentModuleObject);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomCap, simpleWAHP.Name, simpleWAHP.RatedCapHeat);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchHeatCoilNomEff, simpleWAHP.Name, simpleWAHP.RatedPowerHeat / simpleWAHP.RatedCapHeat);

            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPType, simpleWAHP.Name, CurrentModuleObject);
        }

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        lAlphaBlanks.deallocate();
        cNumericFields.deallocate();
        lNumericBlanks.deallocate();
        NumArray.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, format("{} Errors found getting input. Program terminates.", RoutineName));
        }

        for (HPNum = 1; HPNum <= state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs; ++HPNum) {
            auto &simpleWAHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));
            if (simpleWAHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) {
                // COOLING COIL  Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Cooling Coil Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.Power,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Total Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QLoadTotal,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QSensible,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Latent Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QLatent,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QSource,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    simpleWAHP.PartLoadRatio,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Runtime Fraction",
                                    OutputProcessor::Unit::None,
                                    simpleWAHP.RunFrac,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);

                SetupOutputVariable(state,
                                    "Cooling Coil Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    simpleWAHP.AirMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.InletAirDBTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    simpleWAHP.InletAirHumRat,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.OutletAirDBTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Air Outlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    simpleWAHP.OutletAirHumRat,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    simpleWAHP.WaterMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.InletWaterTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Cooling Coil Source Side Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.OutletWaterTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);

            } else if (simpleWAHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit) {
                // HEATING COIL Setup Report variables for the Heat Pump
                SetupOutputVariable(state,
                                    "Heating Coil Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.Power,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Heating Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QLoadTotal,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QSensible,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Source Side Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    simpleWAHP.QSource,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    simpleWAHP.PartLoadRatio,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Runtime Fraction",
                                    OutputProcessor::Unit::None,
                                    simpleWAHP.RunFrac,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);

                SetupOutputVariable(state,
                                    "Heating Coil Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    simpleWAHP.AirMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.InletAirDBTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    simpleWAHP.InletAirHumRat,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.OutletAirDBTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Air Outlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    simpleWAHP.OutletAirHumRat,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    simpleWAHP.WaterMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.InletWaterTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
                SetupOutputVariable(state,
                                    "Heating Coil Source Side Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    simpleWAHP.OutletWaterTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    simpleWAHP.Name);
            }
        }
    }

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitSimpleWatertoAirHP(EnergyPlusData &state,
                                int const HPNum,                                 // Current HPNum under simulation
                                Real64 const SensLoad,                           // Control zone sensible load[W]
                                Real64 const LatentLoad,                         // Control zone latent load[W]
                                [[maybe_unused]] int const CyclingScheme,        // fan operating mode
                                [[maybe_unused]] Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
                                bool const FirstHVACIteration                    // Iteration flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Nov 2003
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Simple Water to Air HP Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitSimpleWatertoAirHP");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;            // Node Number of the air inlet
        int WaterInletNode;          // Node Number of the Water inlet
        Real64 RatedAirMassFlowRate; // coil rated air mass flow rates
        Real64 rho;                  // local fluid density
        bool errFlag;

        if (state.dataWaterToAirHeatPumpSimple->MyOneTimeFlag) {
            // initialize the environment and sizing flags
            state.dataWaterToAirHeatPumpSimple->MySizeFlag.allocate(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs);
            state.dataWaterToAirHeatPumpSimple->MyEnvrnFlag.allocate(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs);
            state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag.allocate(state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs);
            state.dataWaterToAirHeatPumpSimple->MySizeFlag = true;
            state.dataWaterToAirHeatPumpSimple->MyEnvrnFlag = true;
            state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag = true;
            state.dataWaterToAirHeatPumpSimple->MyOneTimeFlag = false;
        }

        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        if (state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag(HPNum) && allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(
                state, simpleWatertoAirHP.Name, simpleWatertoAirHP.WAHPPlantType, simpleWatertoAirHP.plantLoc, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError(state, "InitSimpleWatertoAirHP: Program terminated for previous conditions.");
            }
            state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag(HPNum) = false;
        }

        if (state.dataWaterToAirHeatPumpSimple->MySizeFlag(HPNum)) {
            if (!state.dataGlobal->SysSizingCalc && !state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag(HPNum)) {
                // do the sizing once.
                SizeHVACWaterToAir(state, HPNum);
                state.dataWaterToAirHeatPumpSimple->MySizeFlag(HPNum) = false;
            }
        }

        if (FirstHVACIteration) {
            if (state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(HPNum)) {
                if (simpleWatertoAirHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) {
                    if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                        if (simpleWatertoAirHP.WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Cooling;
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum).LastOperatingMode =
                                DataHVACGlobals::Cooling;
                        } else if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum).WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Heating;
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum).LastOperatingMode =
                                DataHVACGlobals::Heating;
                        }
                        state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(simpleWatertoAirHP.CompanionHeatingCoilNum) = false;
                    } else {
                        if (simpleWatertoAirHP.WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Cooling;
                        }
                    }
                    state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(HPNum) = false;
                } else {
                    // it is a heating coil
                    if (simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                        if (simpleWatertoAirHP.WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Heating;
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).LastOperatingMode =
                                DataHVACGlobals::Heating;
                        } else if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Cooling;
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).LastOperatingMode =
                                DataHVACGlobals::Cooling;
                        }
                        state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(simpleWatertoAirHP.CompanionCoolingCoilNum) = false;
                    } else {
                        if (simpleWatertoAirHP.WaterFlowMode) {
                            simpleWatertoAirHP.LastOperatingMode = DataHVACGlobals::Heating;
                        }
                    }
                    state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(HPNum) = false;
                }
            }
        } else {
            state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(HPNum) = true;
            if (simpleWatertoAirHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) {
                if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0)
                    state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(simpleWatertoAirHP.CompanionHeatingCoilNum) = true;
            } else {
                if (simpleWatertoAirHP.CompanionCoolingCoilNum > 0)
                    state.dataWaterToAirHeatPumpSimple->SimpleHPTimeStepFlag(simpleWatertoAirHP.CompanionCoolingCoilNum) = true;
            }
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag) {

            if (state.dataWaterToAirHeatPumpSimple->MyEnvrnFlag(HPNum) && !state.dataWaterToAirHeatPumpSimple->MyPlantScanFlag(HPNum)) {

                // Do the initializations to start simulation

                AirInletNode = simpleWatertoAirHP.AirInletNodeNum;
                WaterInletNode = simpleWatertoAirHP.WaterInletNodeNum;

                // Initialize all report variables to a known state at beginning of simulation
                simpleWatertoAirHP.AirVolFlowRate = 0.0;
                simpleWatertoAirHP.InletAirDBTemp = 0.0;
                simpleWatertoAirHP.InletAirHumRat = 0.0;
                simpleWatertoAirHP.OutletAirDBTemp = 0.0;
                simpleWatertoAirHP.OutletAirHumRat = 0.0;
                simpleWatertoAirHP.WaterVolFlowRate = 0.0;
                simpleWatertoAirHP.WaterMassFlowRate = 0.0;
                simpleWatertoAirHP.InletWaterTemp = 0.0;
                simpleWatertoAirHP.InletWaterEnthalpy = 0.0;
                simpleWatertoAirHP.OutletWaterEnthalpy = 0.0;
                simpleWatertoAirHP.OutletWaterTemp = 0.0;
                simpleWatertoAirHP.Power = 0.0;
                simpleWatertoAirHP.QLoadTotal = 0.0;
                simpleWatertoAirHP.QLoadTotalReport = 0.0;
                simpleWatertoAirHP.QSensible = 0.0;
                simpleWatertoAirHP.QLatent = 0.0;
                simpleWatertoAirHP.QSource = 0.0;
                simpleWatertoAirHP.Energy = 0.0;
                simpleWatertoAirHP.EnergyLoadTotal = 0.0;
                simpleWatertoAirHP.EnergySensible = 0.0;
                simpleWatertoAirHP.EnergyLatent = 0.0;
                simpleWatertoAirHP.EnergySource = 0.0;
                simpleWatertoAirHP.COP = 0.0;
                simpleWatertoAirHP.RunFrac = 0.0;
                simpleWatertoAirHP.PartLoadRatio = 0.0;

                if (simpleWatertoAirHP.RatedWaterVolFlowRate != DataSizing::AutoSize) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidName,
                                                            Constant::InitConvTemp,
                                                            state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidIndex,
                                                            RoutineName);

                    simpleWatertoAirHP.DesignWaterMassFlowRate = rho * simpleWatertoAirHP.RatedWaterVolFlowRate;
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       simpleWatertoAirHP.DesignWaterMassFlowRate,
                                                       simpleWatertoAirHP.WaterInletNodeNum,
                                                       simpleWatertoAirHP.WaterOutletNodeNum);

                    if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating && simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                        state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).DesignWaterMassFlowRate =
                            rho *
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).RatedWaterVolFlowRate;
                        PlantUtilities::InitComponentNodes(
                            state,
                            0.0,
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum)
                                .DesignWaterMassFlowRate,
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).WaterInletNodeNum,
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).WaterOutletNodeNum);
                    }
                }

                simpleWatertoAirHP.SimFlag = true;

                state.dataWaterToAirHeatPumpSimple->MyEnvrnFlag(HPNum) = false;
            }

        } // End If for the Begin Environment initializations

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataWaterToAirHeatPumpSimple->MyEnvrnFlag(HPNum) = true;
        }

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.
        // First set the conditions for the air into the heat pump model

        // Set water and air inlet nodes

        AirInletNode = simpleWatertoAirHP.AirInletNodeNum;
        WaterInletNode = simpleWatertoAirHP.WaterInletNodeNum;

        if ((SensLoad != 0.0 || LatentLoad != 0.0) && (state.dataLoopNodes->Node(AirInletNode).MassFlowRate > 0.0)) {
            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;

            simpleWatertoAirHP.AirMassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
            // If air flow is less than 25% rated flow. Then throw warning
            RatedAirMassFlowRate =
                simpleWatertoAirHP.RatedAirVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                                                           state.dataEnvrn->StdBaroPress,
                                                                                           state.dataLoopNodes->Node(AirInletNode).Temp,
                                                                                           state.dataLoopNodes->Node(AirInletNode).HumRat,
                                                                                           RoutineName);
            if (simpleWatertoAirHP.AirMassFlowRate < 0.25 * RatedAirMassFlowRate) {
                ShowRecurringWarningErrorAtEnd(state,
                                               "Actual air mass flow rate is smaller than 25% of water-to-air heat pump coil rated air flow rate.",
                                               state.dataWaterToAirHeatPumpSimple->AirflowErrPointer,
                                               simpleWatertoAirHP.AirMassFlowRate,
                                               simpleWatertoAirHP.AirMassFlowRate);
            }
            simpleWatertoAirHP.WaterFlowMode = true;
        } else { // heat pump is off
            simpleWatertoAirHP.WaterFlowMode = false;
            simpleWatertoAirHP.WaterMassFlowRate = 0.0;
            simpleWatertoAirHP.AirMassFlowRate = 0.0;
            if ((simpleWatertoAirHP.WaterCyclingMode) == DataHVACGlobals::WaterConstant) {
                if (simpleWatertoAirHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) {
                    if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                        if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum).QLoadTotal > 0.0) {
                            // do nothing, there will be flow through this coil
                        } else if (simpleWatertoAirHP.LastOperatingMode == DataHVACGlobals::Cooling) {
                            // set the flow rate to full design flow
                            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                        }
                    } else {
                        if (simpleWatertoAirHP.LastOperatingMode == DataHVACGlobals::Cooling) {
                            // set the flow rate to full design flow
                            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                        }
                    }
                } else if (simpleWatertoAirHP.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit) {
                    // It's a heating coil
                    if (simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                        if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum).QLoadTotal > 0.0) {
                            // do nothing, there will be flow through this coil
                        } else if (simpleWatertoAirHP.LastOperatingMode == DataHVACGlobals::Heating) {
                            // set the flow rate to full design flow
                            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                        }
                    } else {
                        if (simpleWatertoAirHP.LastOperatingMode == DataHVACGlobals::Heating) {
                            // set the flow rate to full design flow
                            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                        }
                    }
                }
            }
        }

        PlantUtilities::SetComponentFlowRate(state,
                                             simpleWatertoAirHP.WaterMassFlowRate,
                                             simpleWatertoAirHP.WaterInletNodeNum,
                                             simpleWatertoAirHP.WaterOutletNodeNum,
                                             simpleWatertoAirHP.plantLoc);

        simpleWatertoAirHP.InletAirDBTemp = state.dataLoopNodes->Node(AirInletNode).Temp;
        simpleWatertoAirHP.InletAirHumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
        simpleWatertoAirHP.InletAirEnthalpy = state.dataLoopNodes->Node(AirInletNode).Enthalpy;
        simpleWatertoAirHP.InletWaterTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
        simpleWatertoAirHP.InletWaterEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;
        simpleWatertoAirHP.OutletWaterTemp = simpleWatertoAirHP.InletWaterTemp;
        simpleWatertoAirHP.OutletWaterEnthalpy = simpleWatertoAirHP.InletWaterEnthalpy;

        // Outlet variables
        simpleWatertoAirHP.Power = 0.0;
        simpleWatertoAirHP.QLoadTotal = 0.0;
        simpleWatertoAirHP.QLoadTotalReport = 0.0;
        simpleWatertoAirHP.QSensible = 0.0;
        simpleWatertoAirHP.QLatent = 0.0;
        simpleWatertoAirHP.QSource = 0.0;
        simpleWatertoAirHP.Energy = 0.0;
        simpleWatertoAirHP.EnergyLoadTotal = 0.0;
        simpleWatertoAirHP.EnergySensible = 0.0;
        simpleWatertoAirHP.EnergyLatent = 0.0;
        simpleWatertoAirHP.EnergySource = 0.0;
        simpleWatertoAirHP.COP = 0.0;
        state.dataHeatBal->HeatReclaimSimple_WAHPCoil(HPNum).AvailCapacity = 0.0;
    }

    void SizeHVACWaterToAir(EnergyPlusData &state, int const HPNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2009
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing WSHP Components for which nominal capacities
        // and flow rates have not been specified in the input

        // METHODOLOGY EMPLOYED:
        // Obtains heating capacities and flow rates from the zone or system sizing arrays.

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeWaterToAirCoil");
        static constexpr std::string_view RoutineNameAlt("SizeHVACWaterToAir");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 rhoair;
        Real64 MixTemp;                   // Mixed air temperature at cooling design conditions
        Real64 MixTempSys;                // Mixed air temperature at cooling design conditions at system air flow
        Real64 HeatMixTemp;               // Mixed air temperature at heating design conditions
        Real64 HeatMixTempSys;            // Mixed air temperature at heating design conditions at system air flow
        Real64 MixHumRat;                 // Mixed air humidity ratio at cooling design conditions
        Real64 MixHumRatSys;              // Mixed air humidity ratio at cooling design conditions at system air flow
        Real64 HeatMixHumRat;             // Mixed air humidity ratio at heating design conditions
        Real64 HeatMixHumRatSys;          // Mixed air humidity ratio at heating design conditions at system air flow
        Real64 MixEnth;                   // Mixed air enthalpy at cooling design conditions
        Real64 MixEnthSys;                // Mixed air enthalpy at cooling design conditions at system air flow
        Real64 MixWetBulb;                // Mixed air wet-bulb temperature at cooling design conditions
        Real64 RatedMixWetBulb = 0.0;     // Rated mixed air wetbulb temperature
        Real64 RatedMixDryBulb = 0.0;     // Rated mixed air drybulb temperature
        Real64 RatedHeatMixDryBulb = 0.0; // Rated mixed air drybulb temperature at heating design conditions
        Real64 SupTemp;                   // Supply air temperature at cooling design conditions
        Real64 HeatSupTemp;               // Supply air temperature at heating design conditions
        Real64 SupHumRat;                 // Supply air humidity ratio at cooling design conditions
        Real64 SupEnth;                   // Supply air enthalpy at cooling design conditions
        Real64 OutTemp;                   // Outdoor aur dry-bulb temperature at cooling design conditions
        Real64 ratioTDB;                  // Load-side dry-bulb temperature ratio at cooling design conditions
        Real64 HeatratioTDB;              // Load-side dry-bulb temperature ratio at heating design conditions
        Real64 ratioTWB;                  // Load-side wet-bulb temperature ratio at cooling design conditions
        Real64 ratioTS;                   // Source-side temperature ratio at cooling design conditions
        Real64 HeatratioTS;               // Source-side temperature ratio at heating design conditions
        Real64 RatedratioTDB;             // Rated cooling load-side dry-bulb temperature ratio
        Real64 RatedHeatratioTDB = 0.0;   // Rated cooling load-side dry-bulb temperature ratio
        Real64 RatedratioTWB;             // Rated cooling load-side wet-bulb temperature ratio
        Real64 RatedratioTS;              // Rated cooling source-side temperature ratio
        Real64 RatedHeatratioTS;          // Rated heating source-side temperature ratio
        Real64 OutAirFrac;                // Outdoor air fraction at cooling design conditions
        Real64 OutAirFracSys;             // Outdoor air fraction at cooling design conditions at system air flow
        Real64 HeatOutAirFrac;            // Outdoor air fraction at heating design conditions
        Real64 HeatOutAirFracSys;         // Outdoor air fraction at heating design conditions at system air flow
        Real64 VolFlowRate;
        Real64 CoolCapAtPeak;                  // Load on the cooling coil at cooling design conditions
        Real64 HeatCapAtPeak;                  // Load on the heating coil at heating design conditions
        Real64 PeakTotCapTempModFac = 1.0;     // Peak total cooling capacity curve modifier
        Real64 RatedTotCapTempModFac = 1.0;    // Rated total cooling capacity curve modifier
        Real64 PeakHeatCapTempModFac = 1.0;    // Peak heating capacity curve modifier
        Real64 DesignEntWaterTemp;             // Design entering coil water temperature
        Real64 SensCapAtPeak;                  // Sensible load on the cooling coil at cooling design conditions
        Real64 PeakSensCapTempModFac = 1.0;    // Peak sensible cooling capacity curve modifier
        Real64 RatedSensCapTempModFac = 1.0;   // Rated sensible cooling capacity curve modifier
        Real64 RatedHeatCapTempModFac = 1.0;   // Rated heating capacity curve modifier
        Real64 RatedCoolPowerTempModFac = 1.0; // Rated cooling power curve modifier
        Real64 RatedHeatPowerTempModFac = 1.0; // Rated heating power curve modifier
        Real64 RatedCapCoolTotalDesCDD;        // Rated total cooling coil capacity determined at cooling design conditions
        constexpr Real64 Tref(283.15);         // Refrence Temperature for performance curves,10C [K]
        int TimeStepNumAtMax;
        int DDNum;
        int PltSizNum;
        bool RatedCapCoolTotalAutoSized;
        bool RatedCapCoolSensAutoSized;
        bool ErrorsFound;
        Real64 SystemCapacity = 0.0;
        Real64 rho;
        Real64 Cp;
        bool IsAutoSize;                  // Indicator to autosize
        bool HardSizeNoDesRun;            // Indicator to hardsize and no sizing run
        Real64 RatedAirVolFlowRateDes;    // Autosized rated air flow for reporting
        Real64 CoolingAirVolFlowRateDes;  // Cooling desing day air flow
        Real64 HeatingAirVolFlowRateDes;  // Heating design day air flow
        Real64 RatedAirVolFlowRateUser;   // Hardsized rated air flow for reporting
        Real64 RatedCapCoolTotalDes;      // Autosized rated cooling capacity for reporting
        Real64 RatedCapCoolTotalUser;     // Hardsized rated cooling capacity for reporting
        Real64 RatedCapCoolSensDes;       // Autosized rated sensible cooling capacity for reporting
        Real64 RatedCapCoolSensUser;      // Hardsized rated sensible cooling capacity for reporting
        Real64 RatedCapHeatDes;           // Autosized rated heating capacity for reporting
        Real64 RatedCapHeatUser;          // Hardsized rated heating capacity for reporting
        Real64 RatedWaterVolFlowRateDes;  // Autosized rated water flow rate for reporting
        Real64 RatedWaterVolFlowRateUser; // Hardsized rated water flow rate for reporting
        Real64 RatedCapCoolHeatDD;        // Rated cooling coil capacity based on heating design conditions
        bool SizingDesRunThisAirSys;      // true if a particular air system had a Sizing:System object and system sizing done
        bool SizingDesRunThisZone;        // true if a particular zone had a Sizing:Zone object and zone sizing was done
        Real64 HeatdTratio = 1.0;         // Temperature difference across coil adjustment factor
        Real64 dHratio = 1.0;             // Enthalpy difference across coil adjustment factor
        Real64 HeatOAFrac;                // Outdoor air fraction at heating design conditions
        Real64 HeatOAFracSys;             // Outdoor air fraction at heating design conditions at system air flow
        Real64 HeatOATemp;                // Outdoor air temperature at heating design conditions
        Real64 OAFrac;                    // Outdooor air fraction
        Real64 OAFracSys;                 // Outdoor air fraction at system air flow
        Real64 OATemp;                    // Outdoor air temperature at cooling design conditions
        Real64 OAHumRat;                  // Humidity ratio at cooling design conditions

        PltSizNum = 0;
        ErrorsFound = false;
        IsAutoSize = false;
        if (state.dataSize->SysSizingRunDone || state.dataSize->ZoneSizingRunDone) {
            HardSizeNoDesRun = false;
        } else {
            HardSizeNoDesRun = true;
        }
        if (state.dataSize->CurSysNum > 0) {
            CheckThisAirSystemForSizing(state, state.dataSize->CurSysNum, SizingDesRunThisAirSys);
        } else {
            SizingDesRunThisAirSys = false;
        }
        if (state.dataSize->CurZoneEqNum > 0) {
            CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
        } else {
            SizingDesRunThisZone = false;
        }
        RatedAirVolFlowRateDes = 0.0;
        RatedAirVolFlowRateUser = 0.0;
        CoolingAirVolFlowRateDes = 0.0;
        HeatingAirVolFlowRateDes = 0.0;
        RatedCapCoolTotalDes = 0.0;
        RatedCapCoolTotalUser = 0.0;
        RatedCapCoolSensDes = 0.0;
        RatedCapCoolSensUser = 0.0;
        RatedCapHeatDes = 0.0;
        RatedCapHeatUser = 0.0;
        RatedWaterVolFlowRateDes = 0.0;
        RatedWaterVolFlowRateUser = 0.0;
        std::string CompType = format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]);

        if (simpleWatertoAirHP.RatedAirVolFlowRate == DataSizing::AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataSize->CurSysNum > 0) {
            if (!IsAutoSize && !SizingDesRunThisAirSys) { // Simulation continue
                HardSizeNoDesRun = true;
                if (simpleWatertoAirHP.RatedAirVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name,
                        "User-Specified Rated Air Flow Rate [m3/s]",
                        simpleWatertoAirHP.RatedAirVolFlowRate);
                }
            } else {
                CheckSysSizing(state,
                               format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                               simpleWatertoAirHP.Name);
                if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow >= DataHVACGlobals::SmallAirVolFlow) {
                    RatedAirVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                    CoolingAirVolFlowRateDes = state.dataSize->CalcSysSizing(state.dataSize->CurSysNum).DesCoolVolFlow;
                    HeatingAirVolFlowRateDes = state.dataSize->CalcSysSizing(state.dataSize->CurSysNum).DesHeatVolFlow;
                } else {
                    RatedAirVolFlowRateDes = 0.0;
                }
            }
        } else if (state.dataSize->CurZoneEqNum > 0) {
            if (!IsAutoSize && !SizingDesRunThisZone) { // Simulation continue
                HardSizeNoDesRun = true;
                if (simpleWatertoAirHP.RatedAirVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name,
                        "User-Specified Rated Air Flow Rate [m3/s]",
                        simpleWatertoAirHP.RatedAirVolFlowRate);
                }
            } else {
                CheckZoneSizing(state,
                                format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                                simpleWatertoAirHP.Name);
                RatedAirVolFlowRateDes = max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                                             state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);
                CoolingAirVolFlowRateDes = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                HeatingAirVolFlowRateDes = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow;
                if (RatedAirVolFlowRateDes < DataHVACGlobals::SmallAirVolFlow) {
                    RatedAirVolFlowRateDes = 0.0;
                }
            }
        }
        if (!HardSizeNoDesRun) {
            if (IsAutoSize) {
                simpleWatertoAirHP.RatedAirVolFlowRate = RatedAirVolFlowRateDes;
                BaseSizer::reportSizerOutput(
                    state,
                    format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                    simpleWatertoAirHP.Name,
                    "Design Size Rated Air Flow Rate [m3/s]",
                    RatedAirVolFlowRateDes);
            } else {
                if (simpleWatertoAirHP.RatedAirVolFlowRate > 0.0 && RatedAirVolFlowRateDes > 0.0 && !HardSizeNoDesRun) {
                    RatedAirVolFlowRateUser = simpleWatertoAirHP.RatedAirVolFlowRate;
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name,
                        "Design Size Rated Air Flow Rate [m3/s]",
                        RatedAirVolFlowRateDes,
                        "User-Specified Rated Air Flow Rate [m3/s]",
                        RatedAirVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(RatedAirVolFlowRateDes - RatedAirVolFlowRateUser) / RatedAirVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(
                                state,
                                format("SizeHVACWaterToAir: Potential issue with equipment sizing for coil {}:WATERTOAIRHEATPUMP:EQUATIONFIT \"{}\"",
                                       WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                       simpleWatertoAirHP.Name));
                            ShowContinueError(state, format("User-Specified Rated Air Volume Flow Rate of {:.5R} [m3/s]", RatedAirVolFlowRateUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Rated Air Volume Flow Rate of {:.5R} [m3/s]", RatedAirVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }

        RatedCapCoolTotalAutoSized = false;
        RatedCapCoolSensAutoSized = false;

        Real64 FanCoolLoad = 0.0;
        Real64 FanHeatLoad = FanCoolLoad;
        if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
            // size rated total cooling capacity
            if (simpleWatertoAirHP.RatedCapCoolTotal == DataSizing::AutoSize) {
                RatedCapCoolTotalAutoSized = true;
            }
            if (SizingDesRunThisAirSys || SizingDesRunThisZone) HardSizeNoDesRun = false;
            if (state.dataSize->CurSysNum > 0) {
                if (!RatedCapCoolTotalAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (simpleWatertoAirHP.RatedCapCoolTotal > 0.0) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "User-Specified Rated Total Cooling Capacity [W]",
                            simpleWatertoAirHP.RatedCapCoolTotal);
                    }
                } else {
                    CheckSysSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (CoolingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = CoolingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = HeatingAirVolFlowRateDes; // system air flow
                    }
                    // cooling design day calculations
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        auto &finalSysSizing(state.dataSize->FinalSysSizing(state.dataSize->CurSysNum));
                        if (state.dataSize->CurOASysNum > 0) { // coil is in the OA stream
                            MixTemp = finalSysSizing.OutTempAtCoolPeak;
                            MixHumRat = finalSysSizing.OutHumRatAtCoolPeak;
                            SupTemp = finalSysSizing.PrecoolTemp;
                            SupHumRat = finalSysSizing.PrecoolHumRat;
                            MixTempSys = MixTemp;
                            MixHumRatSys = MixHumRat;
                        } else { // coil is on the main air loop
                            SupTemp = finalSysSizing.CoolSupTemp;
                            SupHumRat = finalSysSizing.CoolSupHumRat;
                            if (VolFlowRate > 0.0) {
                                OutAirFrac = finalSysSizing.DesOutAirVolFlow / VolFlowRate;
                                OutAirFracSys = finalSysSizing.DesOutAirVolFlow / RatedAirVolFlowRateDes;
                            } else {
                                OutAirFrac = 1.0;
                                OutAirFracSys = OutAirFrac;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            OutAirFracSys = min(1.0, max(0.0, OutAirFracSys));
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).NumOACoolCoils ==
                                0) { // there is no precooling of the OA stream
                                MixTemp = finalSysSizing.MixTempAtCoolPeak;
                                MixHumRat = finalSysSizing.MixHumRatAtCoolPeak;
                                // calculate mixed air temperature with system airflow
                                MixTempSys =
                                    OutAirFracSys * finalSysSizing.OutTempAtCoolPeak + (1.0 - OutAirFracSys) * finalSysSizing.RetTempAtCoolPeak;
                                MixHumRatSys =
                                    OutAirFracSys * finalSysSizing.OutHumRatAtCoolPeak + (1.0 - OutAirFracSys) * finalSysSizing.RetHumRatAtCoolPeak;
                            } else { // there is precooling of OA stream
                                MixTemp = OutAirFrac * finalSysSizing.PrecoolTemp + (1.0 - OutAirFrac) * finalSysSizing.RetTempAtCoolPeak;
                                MixHumRat = OutAirFrac * finalSysSizing.PrecoolHumRat + (1.0 - OutAirFrac) * finalSysSizing.RetHumRatAtCoolPeak;
                                // calculate mixed air temperature with system airflow
                                MixTempSys = OutAirFracSys * finalSysSizing.PrecoolTemp + (1.0 - OutAirFracSys) * finalSysSizing.RetTempAtCoolPeak;
                                MixHumRatSys =
                                    OutAirFracSys * finalSysSizing.PrecoolHumRat + (1.0 - OutAirFracSys) * finalSysSizing.RetHumRatAtCoolPeak;
                            }
                        }
                        // supply air condition is capped with that of mixed air to avoid SHR > 1.0
                        SupTemp = min(MixTemp, SupTemp);
                        SupHumRat = min(MixHumRat, SupHumRat);
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                        MixEnth = Psychrometrics::PsyHFnTdbW(MixTemp, MixHumRat);
                        MixEnthSys = Psychrometrics::PsyHFnTdbW(MixTempSys, MixHumRatSys);
                        SupEnth = Psychrometrics::PsyHFnTdbW(SupTemp, SupHumRat);
                        // determine the coil ratio of coil dT with system air flow to design heating air flow
                        dHratio = (SupEnth - MixEnthSys) / (SupEnth - MixEnth);
                        Real64 FanCoolLoad = 0.0;
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // add fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanCoolLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanCoolLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(MixHumRat);
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                DataAirSystems::FanPlacement::BlowThru) {
                                MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                       DataAirSystems::FanPlacement::DrawThru) {
                                SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        CoolCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) +
                                        FanCoolLoad; // load on the cooling coil which includes ventilation load and fan heat
                        CoolCapAtPeak = max(0.0, CoolCapAtPeak);
                        MixWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                        RatedMixWetBulb = simpleWatertoAirHP.RatedEntAirWetbulbTemp;
                        // calculate temperatue ratio at design day peak conditions
                        ratioTWB = (MixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            ratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of total cooling capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            ratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at rated conditions
                        RatedratioTWB = (RatedMixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        // determine curve modifiers at peak and rated conditions
                        PeakTotCapTempModFac = Curve::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, ratioTWB, ratioTS, 1.0, 1.0);
                        RatedTotCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        RatedCoolPowerTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.CoolPowCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        // calculate the rated total capacity based on peak conditions
                        // note: the rated total capacity can be different than the total capacity at
                        // rated conditions if the capacity curve isn't normalized at the rated
                        // conditions
                        RatedCapCoolTotalDes = (PeakTotCapTempModFac > 0.0) ? CoolCapAtPeak / PeakTotCapTempModFac : CoolCapAtPeak;
                        // reporting
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(
                            state, simpleWatertoAirHP.Name, CompType, MixTemp, state.dataSize->CurSysNum, state.dataSize->CurZoneEqNum);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirHumRat(state, simpleWatertoAirHP.Name, CompType, MixHumRat);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(state, simpleWatertoAirHP.Name, CompType, SupTemp);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(state, simpleWatertoAirHP.Name, CompType, SupHumRat);
                    } else {
                        RatedCapCoolTotalDes = 0.0;
                    }
                }
            } else if (state.dataSize->CurZoneEqNum > 0) {
                if (!RatedCapCoolTotalAutoSized && !SizingDesRunThisZone) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (simpleWatertoAirHP.RatedCapCoolTotal > 0.0) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "User-Specified Rated Total Cooling Capacity [W]",
                            simpleWatertoAirHP.RatedCapCoolTotal);
                    }
                } else {
                    CheckZoneSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (CoolingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = CoolingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = HeatingAirVolFlowRateDes; // system air flow
                    }
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        // cooling design calculations
                        if (state.dataSize->ZoneEqDXCoil) {
                            if (ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow > 0.0) {
                                MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                                MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                                // calculate mixed air temperature and humidity with system airflow
                                OAFrac = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA / CoolingAirVolFlowRateDes;
                                OAFracSys = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA / RatedAirVolFlowRateDes;
                                OATemp = (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp -
                                          (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneTempAtCoolPeak) /
                                         OAFrac;
                                OAHumRat = (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInHumRat -
                                            (1.0 - OAFrac) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak) /
                                           OAFrac;
                                MixTempSys = OAFracSys * OATemp +
                                             (1.0 - OAFracSys) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneTempAtCoolPeak;
                                MixHumRatSys = OAFracSys * OAHumRat +
                                               (1.0 - OAFracSys) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak;
                            } else {
                                MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneRetTempAtCoolPeak;
                                MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtCoolPeak;
                                MixTempSys = MixTemp;
                                MixHumRatSys = MixHumRat;
                            }
                        } else {
                            MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                            MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                            MixTempSys = MixTemp;
                            MixHumRatSys = MixHumRat;
                        }
                        SupTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesTemp;
                        SupHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesHumRat;
                        // supply air condition is capped with that of mixed air to avoid SHR > 1.0
                        SupTemp = min(MixTemp, SupTemp);
                        SupHumRat = min(MixHumRat, SupHumRat);
                        TimeStepNumAtMax = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).TimeStepNumAtCoolMax;
                        DDNum = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDDNum;
                        if (DDNum > 0 && TimeStepNumAtMax > 0) {
                            OutTemp = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                        } else {
                            OutTemp = 0.0;
                        }
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                        MixEnth = Psychrometrics::PsyHFnTdbW(MixTemp, MixHumRat);
                        MixEnthSys = Psychrometrics::PsyHFnTdbW(MixTempSys, MixHumRatSys);
                        SupEnth = Psychrometrics::PsyHFnTdbW(SupTemp, SupHumRat);
                        // determine the coil ratio of coil dH with system air flow to design heating air flow
                        dHratio = (SupEnth - MixEnthSys) / (SupEnth - MixEnth);
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // add fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanCoolLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanCoolLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(MixHumRat);
                            if (state.dataSize->DataFanPlacement == DataSizing::ZoneFanPlacement::BlowThru) {
                                MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else {
                                SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        CoolCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) +
                                        FanCoolLoad; // load on the cooling coil which includes ventilation load and fan heat
                        CoolCapAtPeak = max(0.0, CoolCapAtPeak);
                        MixWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                        RatedMixWetBulb = simpleWatertoAirHP.RatedEntAirWetbulbTemp;
                        // calculate temperatue ratio at design day peak conditions
                        ratioTWB = (MixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            ratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of total cooling capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            ratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at rated conditions
                        RatedratioTWB = (RatedMixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        // determine curve modifiers at peak and rated conditions
                        PeakTotCapTempModFac = Curve::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, ratioTWB, ratioTS, 1.0, 1.0);
                        RatedTotCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        RatedCoolPowerTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.CoolPowCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        // calculate the rated total capacity based on peak conditions
                        // note: the rated total capacity can be different than the total capacity at
                        // rated conditions if the capacity curve isn't normalized at the rated
                        // conditions
                        RatedCapCoolTotalDes = (PeakTotCapTempModFac > 0.0) ? CoolCapAtPeak / PeakTotCapTempModFac : CoolCapAtPeak;
                        // reporting
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(
                            state, simpleWatertoAirHP.Name, CompType, MixTemp, state.dataSize->CurSysNum, state.dataSize->CurZoneEqNum);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirHumRat(state, simpleWatertoAirHP.Name, CompType, MixHumRat);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(state, simpleWatertoAirHP.Name, CompType, SupTemp);
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(state, simpleWatertoAirHP.Name, CompType, SupHumRat);
                    } else {
                        RatedCapCoolTotalDes = 0.0;
                    }
                }
                if (RatedCapCoolTotalDes < DataHVACGlobals::SmallLoad) {
                    RatedCapCoolTotalDes = 0.0;
                }
            }
            // size rated sensible cooling capacity
            if (simpleWatertoAirHP.RatedCapCoolSens == DataSizing::AutoSize && simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
                RatedCapCoolSensAutoSized = true;
            }
            if (SizingDesRunThisAirSys || SizingDesRunThisZone) HardSizeNoDesRun = false;
            if (state.dataSize->CurSysNum > 0) {
                if (!RatedCapCoolSensAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (simpleWatertoAirHP.RatedCapCoolSens > 0.0) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "User-Specified Rated Sensible Cooling Capacity [W]",
                            simpleWatertoAirHP.RatedCapCoolSens);
                    }
                } else {
                    CheckSysSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (CoolingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = CoolingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = HeatingAirVolFlowRateDes; // system air flow
                    }
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        auto &finalSysSizing(state.dataSize->FinalSysSizing(state.dataSize->CurSysNum));
                        if (state.dataSize->CurOASysNum > 0) { // coil is in the OA stream
                            MixTemp = finalSysSizing.OutTempAtCoolPeak;
                            MixHumRat = finalSysSizing.OutHumRatAtCoolPeak;
                            SupTemp = finalSysSizing.PrecoolTemp;
                            SupHumRat = finalSysSizing.PrecoolHumRat;
                        } else { // coil is on the main air loop
                            SupTemp = finalSysSizing.CoolSupTemp;
                            SupHumRat = finalSysSizing.CoolSupHumRat;
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).NumOACoolCoils ==
                                0) { // there is no precooling of the OA stream
                                MixTemp = finalSysSizing.MixTempAtCoolPeak;
                                MixHumRat = finalSysSizing.MixHumRatAtCoolPeak;
                            } else { // there is precooling of OA stream
                                if (VolFlowRate > 0.0) {
                                    OutAirFrac = finalSysSizing.DesOutAirVolFlow / VolFlowRate;
                                } else {
                                    OutAirFrac = 1.0;
                                }
                                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                                MixTemp = OutAirFrac * finalSysSizing.PrecoolTemp + (1.0 - OutAirFrac) * finalSysSizing.RetTempAtCoolPeak;
                                MixHumRat = OutAirFrac * finalSysSizing.PrecoolHumRat + (1.0 - OutAirFrac) * finalSysSizing.RetHumRatAtCoolPeak;
                            }
                        }
                        // supply air condition is capped with that of mixed air to avoid SHR > 1.0
                        SupTemp = min(MixTemp, SupTemp);
                        SupHumRat = min(MixHumRat, SupHumRat);
                        OutTemp = finalSysSizing.OutTempAtCoolPeak;
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                        MixEnth = Psychrometrics::PsyHFnTdbW(MixTemp, MixHumRat);
                        SupEnth = Psychrometrics::PsyHFnTdbW(SupTemp, MixHumRat);
                        Real64 FanCoolLoad = 0.0;
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // add fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanCoolLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanCoolLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(MixHumRat);
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                DataAirSystems::FanPlacement::BlowThru) {
                                MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                       DataAirSystems::FanPlacement::DrawThru) {
                                SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        // Sensible capacity is calculated from enthalpy difference with constant humidity ratio, i.e.,
                        // there is only temperature difference between entering and leaving air enthalpy. Previously
                        // it was calculated using m.cp.dT
                        SensCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) +
                                        FanCoolLoad; // load on the cooling coil which includes ventilation load and fan heat (sensible)
                        SensCapAtPeak = max(0.0, SensCapAtPeak);
                        MixWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                        RatedMixWetBulb = simpleWatertoAirHP.RatedEntAirWetbulbTemp;
                        RatedMixDryBulb = simpleWatertoAirHP.RatedEntAirDrybulbTemp;
                        // calculate temperature ratios at design day peak conditions
                        ratioTDB = (MixTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        ratioTWB = (MixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            ratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of sensible cooling capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at rated conditions
                        RatedratioTDB = (RatedMixDryBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTWB = (RatedMixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        // determine curve modifiers at peak and rated conditions
                        PeakSensCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.SensCoolCapCurveIndex, ratioTDB, ratioTWB, ratioTS, 1.0, 1.0);
                        RatedSensCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.SensCoolCapCurveIndex, RatedratioTDB, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        // calculate the rated sensible capacity based on peak conditions
                        // note: the rated sensible capacity can be different than the sensible capacity
                        // at rated conditions if the capacity curve isn't normalized at the rated
                        // conditions
                        RatedCapCoolSensDes = (PeakSensCapTempModFac > 0.0) ? SensCapAtPeak / PeakSensCapTempModFac : SensCapAtPeak;
                    } else {
                        RatedCapCoolSensDes = 0.0;
                    }
                }
            } else if (state.dataSize->CurZoneEqNum > 0) {
                if (!RatedCapCoolSensAutoSized && !SizingDesRunThisZone) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (simpleWatertoAirHP.RatedCapCoolSens > 0.0) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "User-Specified Rated Sensible Cooling Capacity [W]",
                            simpleWatertoAirHP.RatedCapCoolSens);
                    }
                } else {
                    CheckZoneSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (CoolingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = CoolingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = HeatingAirVolFlowRateDes; // system air flow
                    }
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        if (state.dataSize->ZoneEqDXCoil) {
                            if (ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow > 0.0) {
                                MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                                MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                            } else {
                                MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneRetTempAtCoolPeak;
                                MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtCoolPeak;
                            }
                        } else {
                            MixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInTemp;
                            MixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolCoilInHumRat;
                        }
                        SupTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesTemp;
                        SupHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDesHumRat;
                        // supply air condition is capped with that of mixed air to avoid SHR > 1.0
                        SupTemp = min(MixTemp, SupTemp);
                        SupHumRat = min(MixHumRat, SupHumRat);
                        TimeStepNumAtMax = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).TimeStepNumAtCoolMax;
                        DDNum = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDDNum;
                        if (DDNum > 0 && TimeStepNumAtMax > 0) {
                            OutTemp = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                        } else {
                            OutTemp = 0.0;
                        }
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, MixTemp, MixHumRat, RoutineName);
                        MixEnth = Psychrometrics::PsyHFnTdbW(MixTemp, MixHumRat);
                        SupEnth = Psychrometrics::PsyHFnTdbW(SupTemp, MixHumRat);
                        Real64 FanCoolLoad = 0.0;
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // add fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanCoolLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanCoolLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(MixHumRat);
                            if (state.dataSize->DataFanPlacement == DataSizing::ZoneFanPlacement::BlowThru) {
                                MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else {
                                SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        // Sensible capacity is calculated from enthalpy difference with constant humidity ratio, i.e.,
                        // there is only temperature difference between entering and leaving air enthalpy. Previously
                        // it was calculated using m.cp.dT
                        SensCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) +
                                        FanCoolLoad; // load on the cooling coil which includes ventilation load and fan heat (sensible)
                        SensCapAtPeak = max(0.0, SensCapAtPeak);
                        MixWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->StdBaroPress, RoutineName);
                        RatedMixWetBulb = simpleWatertoAirHP.RatedEntAirWetbulbTemp;
                        RatedMixDryBulb = simpleWatertoAirHP.RatedEntAirDrybulbTemp;
                        // calculate temperature ratios at design day peak conditions
                        ratioTDB = (MixTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        ratioTWB = (MixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            ratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of sensible cooling capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at rated conditions
                        RatedratioTDB = (RatedMixDryBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTWB = (RatedMixWetBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PeakSensCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.SensCoolCapCurveIndex, ratioTDB, ratioTWB, ratioTS, 1.0, 1.0);
                        RatedSensCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.SensCoolCapCurveIndex, RatedratioTDB, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        // Check curve output when rated mixed air wetbulb is the design mixed air wetbulb
                        // calculate the rated sensible capacity based on peak conditions
                        // note: the rated sensible capacity can be different than the sensible capacity
                        // at rated conditions if the capacity curve isn't normalized at the rated
                        // conditions
                        RatedCapCoolSensDes = (PeakSensCapTempModFac > 0.0) ? SensCapAtPeak / PeakSensCapTempModFac : SensCapAtPeak;
                    } else {
                        RatedCapCoolSensDes = 0.0;
                    }
                }
            }
            if (RatedCapCoolSensDes < DataHVACGlobals::SmallLoad) {
                RatedCapCoolSensDes = 0.0;
            }
            if (RatedCapCoolTotalAutoSized && RatedCapCoolSensAutoSized) {
                if (RatedCapCoolSensDes > RatedCapCoolTotalDes) {
                    RatedCapCoolTotalDes = RatedCapCoolSensDes;
                }
            }
            if (!HardSizeNoDesRun) {
                if (RatedCapCoolTotalAutoSized) {
                    if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                        auto &companionHeatingCoil(
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum));
                        if (companionHeatingCoil.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit &&
                            companionHeatingCoil.RatedCapHeat > 0) {
                            // case 1: companion heating coil has a user-specified capacity
                            // or has already been sized
                            RatedCapCoolTotalDesCDD = RatedCapCoolTotalDes;
                            RatedCapCoolHeatDD = companionHeatingCoil.RatedCapHeatAtRatedCdts / companionHeatingCoil.RatioRatedHeatRatedTotCoolCap /
                                                 RatedTotCapTempModFac;
                            if (RatedCapCoolHeatDD > RatedCapCoolTotalDesCDD) {
                                // re-base the cooling capacity
                                RatedCapCoolTotalDes = RatedCapCoolHeatDD;

                                // adjust for system air flow -- capacity is based on heating design day calcs
                                // adjust by ratio of system to heating air flow rate and temperature delta across the coil at these different airflow
                                if (HeatingAirVolFlowRateDes > 0) {
                                    RatedCapCoolTotalDes *= (RatedAirVolFlowRateDes / HeatingAirVolFlowRateDes) * HeatdTratio;
                                }

                                if (RatedCapCoolSensAutoSized) {
                                    // adjust sensible capacity assuming that the SHR is constant
                                    RatedCapCoolSensDes *= RatedCapCoolTotalDes / RatedCapCoolTotalDesCDD;
                                }

                                simpleWatertoAirHP.RatedCapCoolTotal = RatedCapCoolTotalDes;
                                OutputReportPredefined::PreDefTableEntry(
                                    state, state.dataOutRptPredefined->pdchWAHPDD, simpleWatertoAirHP.Name, "Heating");
                            } else {
                                // adjust for system air flow -- capacity is based on cooling design day calcs
                                // adjust by ratio of system to cooling air flow rate and enthalpy delta across the coil at these different airflow
                                RatedCapCoolTotalDes *= (RatedAirVolFlowRateDes / CoolingAirVolFlowRateDes) * dHratio;

                                simpleWatertoAirHP.RatedCapCoolTotal = RatedCapCoolTotalDes;
                                OutputReportPredefined::PreDefTableEntry(
                                    state, state.dataOutRptPredefined->pdchWAHPDD, simpleWatertoAirHP.Name, "Cooling");
                            }
                            // Set the global DX cooling coil capacity variable for use by other objects
                            state.dataSize->DXCoolCap = simpleWatertoAirHP.RatedCapCoolTotal;
                        } else if (companionHeatingCoil.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit &&
                                   companionHeatingCoil.RatedCapHeat == DataSizing::AutoSize) {
                            // case 2: companion heating coil has not already been sized
                            // we only pass the rated total cooling capacity determined
                            // based on cooling design day which is used to decide if the
                            // coil needs to be sized of the heating coil size
                            //
                            // no capcity adjustment based on system flow because the capacity could change
                            // once the heating coil has been sized
                            state.dataSize->DXCoolCap = RatedCapCoolTotalDes;
                        } else if (companionHeatingCoil.WAHPPlantType != DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit) {
                            // case 3: companion heating coil is not of the "equationfit" type and hence doesn't use the rated heating to cooling
                            // coil capacity ratio
                            // adjust for system air flow -- capacity is based on cooling design day calcs
                            // adjust by ratio of system to cooling air flow rate and enthalpy delta across the coil at these different airflow
                            RatedCapCoolTotalDes *= (RatedAirVolFlowRateDes / CoolingAirVolFlowRateDes) * dHratio;
                            simpleWatertoAirHP.RatedCapCoolTotal = RatedCapCoolTotalDes;
                            // Set the global DX cooling coil capacity variable for use by other objects
                            state.dataSize->DXCoolCap = simpleWatertoAirHP.RatedCapCoolTotal;
                        }
                    } else {
                        // adjust for system air flow -- capacity is based on cooling design day calcs
                        // adjust by ratio of system to cooling air flow rate and enthalpy delta across the coil at these different airflow
                        RatedCapCoolTotalDes *= (RatedAirVolFlowRateDes / CoolingAirVolFlowRateDes) * dHratio;

                        simpleWatertoAirHP.RatedCapCoolTotal = RatedCapCoolTotalDes;
                        state.dataSize->DXCoolCap = simpleWatertoAirHP.RatedCapCoolTotal;
                    }
                    // size power
                    simpleWatertoAirHP.RatedCapCoolAtRatedCdts = RatedCapCoolTotalDes * RatedTotCapTempModFac;
                    simpleWatertoAirHP.RatedPowerCoolAtRatedCdts =
                        simpleWatertoAirHP.RatedCapCoolAtRatedCdts / simpleWatertoAirHP.RatedCOPCoolAtRatedCdts;
                    simpleWatertoAirHP.RatedPowerCool = simpleWatertoAirHP.RatedPowerCoolAtRatedCdts / RatedCoolPowerTempModFac;
                    if (simpleWatertoAirHP.RatedCapCoolTotal != DataSizing::AutoSize) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "Design Size Rated Total Cooling Capacity [W]",
                            simpleWatertoAirHP.RatedCapCoolTotal);
                    }
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchWAHPRatedAirDBT, simpleWatertoAirHP.Name, RatedMixDryBulb);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchWAHPRatedAirWBT, simpleWatertoAirHP.Name, RatedMixWetBulb);
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchWAHPRatedWtrT, simpleWatertoAirHP.Name, simpleWatertoAirHP.RatedEntWaterTemp);
                } else { // Hardsized with sizing data
                    if (simpleWatertoAirHP.RatedCapCoolTotal > 0.0 && RatedCapCoolTotalDes > 0.0) {
                        RatedCapCoolTotalUser = simpleWatertoAirHP.RatedCapCoolTotal;
                        state.dataSize->DXCoolCap = simpleWatertoAirHP.RatedCapCoolTotal;
                        simpleWatertoAirHP.RatedPowerCool = simpleWatertoAirHP.RatedCapCoolTotal / simpleWatertoAirHP.RatedCOPCoolAtRatedCdts;
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "Design Size Rated Total Cooling Capacity [W]",
                            RatedCapCoolTotalDes,
                            "User-Specified Rated Total Cooling Capacity [W]",
                            RatedCapCoolTotalUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(RatedCapCoolTotalDes - RatedCapCoolTotalUser) / RatedCapCoolTotalUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(
                                    state,
                                    format("SizeHVACWaterToAir: Potential issue with equipment sizing for coil {}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                           WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                           simpleWatertoAirHP.Name));
                                ShowContinueError(state, format("User-Specified Rated Total Cooling Capacity of {:.2R} [W]", RatedCapCoolTotalUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Rated Total Cooling Capacity of {:.2R} [W]", RatedCapCoolTotalDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            } else {
                state.dataSize->DXCoolCap = simpleWatertoAirHP.RatedCapCoolTotal;
                // user provided inputs are assumed to be at rated conditions
                simpleWatertoAirHP.RatedPowerCool = simpleWatertoAirHP.RatedCapCoolTotal / simpleWatertoAirHP.RatedCOPCoolAtRatedCdts;
                simpleWatertoAirHP.RatedCapCoolAtRatedCdts = 0;
                simpleWatertoAirHP.RatedPowerCoolAtRatedCdts = 0;
            }
            if (simpleWatertoAirHP.RatedCapCoolTotal !=
                DataSizing::AutoSize) { // all cases except case 2 mentioned above (when EquationFit companion heating coil has not yet been sized)
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchCoolCoilTotCap, simpleWatertoAirHP.Name, simpleWatertoAirHP.RatedCapCoolTotal);
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapCoolTotal - simpleWatertoAirHP.RatedCapCoolSens);
                if (simpleWatertoAirHP.RatedCapCoolTotal > 0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchCoolCoilSHR,
                                                             simpleWatertoAirHP.Name,
                                                             simpleWatertoAirHP.RatedCapCoolSens / simpleWatertoAirHP.RatedCapCoolTotal);
                } else {
                    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, simpleWatertoAirHP.Name, 0.0);
                }
                if (RatedCapCoolTotalAutoSized) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchWAHPRatedCapAtRatedCdts,
                                                             simpleWatertoAirHP.Name,
                                                             simpleWatertoAirHP.RatedCapCoolAtRatedCdts);
                    if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                        auto &companionHeatingCoil(
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum));
                        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPDD, companionHeatingCoil.Name, "Cooling");
                    }
                }
            } else {
                // set temporarily until companion heating coil is sized
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilTotCap, simpleWatertoAirHP.Name, 0.0);
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilLatCap, simpleWatertoAirHP.Name, 0.0);
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, simpleWatertoAirHP.Name, 0.0);
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilNomEff, simpleWatertoAirHP.Name, 0.0);
            }
            if (simpleWatertoAirHP.RatedCapCoolTotal != DataSizing::AutoSize) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilCoolingCapacity(state,
                                                                                           simpleWatertoAirHP.Name,
                                                                                           CompType,
                                                                                           simpleWatertoAirHP.RatedCapCoolTotal,
                                                                                           RatedCapCoolTotalAutoSized,
                                                                                           state.dataSize->CurSysNum,
                                                                                           state.dataSize->CurZoneEqNum,
                                                                                           state.dataSize->CurOASysNum,
                                                                                           FanCoolLoad,
                                                                                           PeakTotCapTempModFac,
                                                                                           -999.0,
                                                                                           -999.0);
            }
            if (!HardSizeNoDesRun) {
                if (RatedCapCoolSensAutoSized) {
                    simpleWatertoAirHP.RatedCapCoolSens = RatedCapCoolSensDes;
                    simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts = RatedCapCoolSensDes * RatedSensCapTempModFac;
                    if (simpleWatertoAirHP.RatedCapCoolTotal != DataSizing::AutoSize) {
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "Design Size Rated Sensible Cooling Capacity [W]",
                            RatedCapCoolSensDes);
                    }
                } else {
                    if (simpleWatertoAirHP.RatedCapCoolSens > 0.0 && RatedCapCoolSensDes > 0.0) {
                        RatedCapCoolSensUser = simpleWatertoAirHP.RatedCapCoolSens;
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "Design Size Rated Sensible Cooling Capacity [W]",
                            RatedCapCoolSensDes,
                            "User-Specified Rated Sensible Cooling Capacity [W]",
                            RatedCapCoolSensUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(RatedCapCoolSensDes - RatedCapCoolSensUser) / RatedCapCoolSensUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(
                                    state,
                                    format("SizeHVACWaterToAir: Potential issue with equipment sizing for coil {}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                           WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                           simpleWatertoAirHP.Name));
                                ShowContinueError(state,
                                                  format("User-Specified Rated Sensible Cooling Capacity of {:.2R} [W]", RatedCapCoolSensUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Rated Sensible Cooling Capacity of {:.2R} [W]", RatedCapCoolSensDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchCoolCoilSensCap, simpleWatertoAirHP.Name, simpleWatertoAirHP.RatedCapCoolSens);
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                                     simpleWatertoAirHP.Name,
                                                     state.dataSize->DXCoolCap - simpleWatertoAirHP.RatedCapCoolSens);
            if (RatedCapCoolSensAutoSized) {

                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchWAHPRatedSensCapAtRatedCdts,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts);
            }
            if (simpleWatertoAirHP.RatedCapCoolTotal != 0.0) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchCoolCoilSHR,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapCoolSens / state.dataSize->DXCoolCap);
            } else {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, simpleWatertoAirHP.Name, 0.0);
            }
            // test autosized sensible and total cooling capacity for total > sensible
            if ((RatedCapCoolSensAutoSized && RatedCapCoolTotalAutoSized) || RatedCapCoolSensAutoSized) {
                if (simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts > simpleWatertoAirHP.RatedCapCoolAtRatedCdts) {
                    ShowWarningError(state,
                                     format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT \"{}\"",
                                            WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                            simpleWatertoAirHP.Name));
                    ShowContinueError(state, format("{}: Rated Sensible Cooling Capacity > Rated Total Cooling Capacity", RoutineName));
                    ShowContinueError(state, "Both of these capacity inputs have been autosized.");
                    ShowContinueError(
                        state,
                        format("Rated Sensible Cooling Capacity at Rated Conditions = {:.2T} W", simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts));
                    ShowContinueError(
                        state, format("Rated Total Cooling Capacity at Rated Conditions    = {:.2T} W", simpleWatertoAirHP.RatedCapCoolAtRatedCdts));
                    ShowContinueError(state, "See eio file for further details.");
                    ShowContinueError(state, "Check Total and Sensible Cooling Capacity coefficients in curves to ensure they are accurate.");
                    ShowContinueError(state, "Check Zone and System Sizing objects to verify sizing inputs.");
                    ShowContinueError(state, "Sizing statistics:");
                    ShowContinueError(state, format("Rated entering Air Wet-Bulb Temperature = {:.3T} C", RatedMixWetBulb));
                    ShowContinueError(state, format("Peak entering Air Wet-Bulb Temperature = {:.3T} C", MixWetBulb));
                    ShowContinueError(state, format("Entering Water Temperature used = {:.3T} C", simpleWatertoAirHP.RatedEntWaterTemp));
                    ShowContinueError(state, "Design air and water flow rates = 1.0");
                    ShowContinueError(
                        state, format("Rated ratio of load-side air wet-bulb temperature to 283.15 C (Rated ratioTWB) = {:.3T}", RatedratioTWB));
                    ShowContinueError(
                        state, format("Rated ratio of source-side inlet water temperature to 283.15 C (Rated ratioTS)  = {:.3T}", RatedratioTS));
                    ShowContinueError(state,
                                      format("Peak ratio of load-side air wet-bulb temperature to 283.15 C (Peak ratioTWB) = {:.3T}", ratioTWB));
                    ShowContinueError(state,
                                      format("Peak ratio of source-side inlet water temperature to 283.15 C (Peak ratioTS)  = {:.3T}", ratioTS));
                    ShowContinueError(state, format("Rated Total Cooling Capacity Modifier = {:.5T}", RatedTotCapTempModFac));
                    ShowContinueError(state, format("Peak Design Total Cooling Capacity Modifier = {:.5T}", PeakTotCapTempModFac));
                    ShowContinueError(state, format("Rated Sensible Cooling Capacity Modifier = {:.5T}", RatedSensCapTempModFac));
                    ShowContinueError(state, format("Peak Design Sensible Cooling Capacity Modifier = {:.5T}", PeakSensCapTempModFac));
                    ShowContinueError(state,
                                      "...Rated Total Cooling Capacity at Rated Conditions = Total Peak Design Load * Rated Total "
                                      "Cooling Capacity Modifier  / "
                                      "Peak Design Total Cooling Capacity Modifier");
                    ShowContinueError(state,
                                      "...Rated Sensible Cooling Capacity at Rated Conditions = Peak Design Sensible Load * Rated "
                                      "Sensible Cooling "
                                      "Capacity Modifier  / Peak Design Sensible Cooling Capacity Modifier");
                    ShowContinueError(state, "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates");
                    ShowContinueError(state, "... to ensure they meet the expected manufacturers performance specifications.");
                }
            } else if (RatedCapCoolTotalAutoSized) {
                if (simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts > simpleWatertoAirHP.RatedCapCoolAtRatedCdts) {
                    ShowWarningError(state,
                                     format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT \"{}\"",
                                            WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                            simpleWatertoAirHP.Name));
                    ShowContinueError(state, format("{}: Rated Sensible Cooling Capacity > Rated Total Cooling Capacity", RoutineName));
                    ShowContinueError(state, "Only the Rated total capacity input is autosized, consider autosizing both inputs.");
                    ShowContinueError(state, format("Rated Sensible Cooling Capacity = {:.2T} W", simpleWatertoAirHP.RatedCapCoolSensDesAtRatedCdts));
                    ShowContinueError(state, format("Rated Total Cooling Capacity    = {:.2T} W", simpleWatertoAirHP.RatedCapCoolAtRatedCdts));
                    ShowContinueError(state, "See eio file for further details.");
                    ShowContinueError(state, "Check Total and Sensible Cooling Capacity coefficients in curves to ensure they are accurate.");
                    ShowContinueError(state, "Check Zone and System Sizing objects to verify sizing inputs.");
                    ShowContinueError(state, "Sizing statistics for Total Cooling Capacity:");
                    ShowContinueError(state, format("Rated entering Air Wet-Bulb Temperature = {:.3T} C", RatedMixWetBulb));
                    ShowContinueError(state, format("Peak entering Air Wet-Bulb Temperature = {:.3T} C", MixWetBulb));
                    ShowContinueError(state, format("Entering Water Temperature used = {:.3T} C", simpleWatertoAirHP.RatedEntWaterTemp));
                    ShowContinueError(state, "Design air and water flow rates = 1.0");
                    ShowContinueError(
                        state, format("Rated ratio of load-side air wet-bulb temperature to 283.15 C (Rated ratioTWB) = {:.3T}", RatedratioTWB));
                    ShowContinueError(
                        state, format("Rated ratio of source-side inlet water temperature to 283.15 C (Rated ratioTS)  = {:.3T}", RatedratioTS));
                    ShowContinueError(state,
                                      format("Peak ratio of load-side air wet-bulb temperature to 283.15 C (Peak ratioTWB) = {:.3T}", ratioTWB));
                    ShowContinueError(state,
                                      format("Peak ratio of source-side inlet water temperature to 283.15 C (Peak ratioTS)  = {:.3T}", ratioTS));
                    ShowContinueError(state, format("Rated Total Cooling Capacity Modifier = {:.5T}", RatedTotCapTempModFac));
                    ShowContinueError(state, format("Peak Design Total Cooling Capacity Modifier = {:.5T}", PeakTotCapTempModFac));
                    ShowContinueError(state,
                                      "...Rated Total Cooling Capacity at Rated Conditions = Total Peak Design Load * Rated Total "
                                      "Cooling Capacity Modifier  / "
                                      "Peak Design Total Cooling Capacity Modifier");
                    ShowContinueError(state,
                                      "...Rated Sensible Cooling Capacity at Rated Conditions = Peak Design Sensible Load * Rated "
                                      "Sensible Cooling "
                                      "Capacity Modifier  / Peak Design Sensible Cooling Capacity Modifier");
                    ShowContinueError(state, "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates");
                    ShowContinueError(state, "... to ensure they meet the expected manufacturers performance specifications.");
                }
            }

        } // Cooling Coil

        if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating) {
            // size rated heating capacity
            IsAutoSize = false;
            if (simpleWatertoAirHP.RatedCapHeat == DataSizing::AutoSize) {
                IsAutoSize = true;
            }
            if (SizingDesRunThisAirSys || SizingDesRunThisZone) HardSizeNoDesRun = false;
            if (IsAutoSize) {
                if (state.dataSize->CurSysNum > 0) {
                    CheckSysSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (HeatingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = HeatingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = CoolingAirVolFlowRateDes; // system air flow
                    }
                    // heating design day calculations
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        auto &finalSysSizing(state.dataSize->FinalSysSizing(state.dataSize->CurSysNum));
                        if (state.dataSize->CurOASysNum > 0) { // coil is in the OA stream
                            HeatMixTemp = finalSysSizing.HeatOutTemp;
                            HeatMixHumRat = finalSysSizing.HeatOutHumRat;
                            HeatSupTemp = finalSysSizing.PreheatTemp;
                        } else { // coil is on the main air loop
                            if (VolFlowRate > 0.0) {
                                HeatOutAirFrac = finalSysSizing.DesOutAirVolFlow / VolFlowRate;
                                HeatOutAirFracSys = finalSysSizing.DesOutAirVolFlow / RatedAirVolFlowRateDes;
                            } else {
                                HeatOutAirFrac = 1.0;
                                HeatOutAirFracSys = HeatOutAirFrac;
                            }
                            HeatOutAirFrac = min(1.0, max(0.0, HeatOutAirFrac));
                            HeatOutAirFracSys = min(1.0, max(0.0, HeatOutAirFracSys));
                            HeatSupTemp = finalSysSizing.HeatSupTemp;
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).NumOAHeatCoils ==
                                0) { // there is no preheating of the OA stream
                                HeatMixTemp = HeatOutAirFrac * finalSysSizing.HeatOutTemp + (1.0 - HeatOutAirFrac) * finalSysSizing.HeatRetTemp;
                                HeatMixHumRat = HeatOutAirFrac * finalSysSizing.HeatOutHumRat + (1.0 - HeatOutAirFrac) * finalSysSizing.HeatRetHumRat;
                                // calculate mixed air temperature with system airflow
                                HeatMixTempSys =
                                    HeatOutAirFracSys * finalSysSizing.HeatOutTemp + (1.0 - HeatOutAirFracSys) * finalSysSizing.HeatRetTemp;
                                HeatMixHumRatSys =
                                    HeatOutAirFracSys * finalSysSizing.HeatOutHumRat + (1.0 - HeatOutAirFracSys) * finalSysSizing.HeatRetHumRat;
                            } else { // there is preheating of OA stream
                                HeatOutAirFrac = min(1.0, max(0.0, HeatOutAirFrac));
                                HeatMixTemp = HeatOutAirFrac * finalSysSizing.PreheatTemp + (1.0 - HeatOutAirFrac) * finalSysSizing.HeatRetTemp;
                                HeatMixHumRat = HeatOutAirFrac * finalSysSizing.PreheatHumRat + (1.0 - HeatOutAirFrac) * finalSysSizing.HeatRetHumRat;
                                // calculate mixed air temperature with system airflow
                                HeatMixTempSys =
                                    HeatOutAirFracSys * finalSysSizing.PreheatTemp + (1.0 - HeatOutAirFracSys) * finalSysSizing.HeatRetTemp;
                                HeatMixHumRatSys =
                                    HeatOutAirFracSys * finalSysSizing.PreheatHumRat + (1.0 - HeatOutAirFracSys) * finalSysSizing.HeatRetHumRat;
                            }
                            // determine the coil ratio of coil dT with system air flow to design heating air flow
                            HeatdTratio = (HeatSupTemp - HeatMixTempSys) / (HeatSupTemp - HeatMixTemp);
                        }
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, HeatMixTemp, HeatMixHumRat, RoutineName);
                        HeatCapAtPeak = rhoair * VolFlowRate * Psychrometrics::PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                        (HeatSupTemp - HeatMixTemp);                                     // heating coil load
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // remove fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanHeatLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanHeatLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(HeatMixHumRat);
                            if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                DataAirSystems::FanPlacement::BlowThru) {
                                HeatMixTemp += FanHeatLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                                       DataAirSystems::FanPlacement::DrawThru) {
                                HeatSupTemp -= FanHeatLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        HeatCapAtPeak -= FanHeatLoad; // remove fan heat from heating coil load
                        HeatCapAtPeak = max(0.0, HeatCapAtPeak);
                        RatedHeatMixDryBulb = simpleWatertoAirHP.RatedEntAirDrybulbTemp;
                        // calculate temperatue ratio at design day peak conditions
                        HeatratioTDB = (HeatMixTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            HeatratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of heating capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            HeatratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at refrence conditions
                        RatedHeatratioTDB = (RatedHeatMixDryBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedHeatratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        // determine curve modifiers at peak and rated conditions
                        PeakHeatCapTempModFac = Curve::CurveValue(state, simpleWatertoAirHP.HeatCapCurveIndex, HeatratioTDB, HeatratioTS, 1.0, 1.0);
                        RatedHeatCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.HeatCapCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                        // Check curve output when rated mixed air wetbulb is the design mixed air wetbulb
                        if (RatedHeatMixDryBulb == HeatMixTemp) {
                            if (RatedHeatCapTempModFac > 1.02 || RatedHeatCapTempModFac < 0.98) {
                                ShowWarningError(state,
                                                 format("{} Coil:Heating:WaterToAirHeatPump:EquationFit={}", RoutineName, simpleWatertoAirHP.Name));
                                ShowContinueError(state,
                                                  "Heating capacity as a function of temperature curve output is not equal to 1.0 (+ or - 2%) "
                                                  "at rated conditions.");
                                ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedHeatCapTempModFac));
                            }
                        }
                        // calculate the rated capacity based on peak conditions
                        // note: the rated capacity can be different than the capacity at
                        // rated conditions if the capacity curve isn't normalized at the
                        // rated conditions
                        RatedCapHeatDes = (PeakHeatCapTempModFac > 0.0) ? HeatCapAtPeak / PeakHeatCapTempModFac : HeatCapAtPeak;
                    } else {
                        RatedCapHeatDes = 0.0;
                        RatedHeatratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                    }
                } else if (state.dataSize->CurZoneEqNum > 0) {
                    CheckZoneSizing(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name);
                    if (HeatingAirVolFlowRateDes > 0.0) {
                        VolFlowRate = HeatingAirVolFlowRateDes;
                    } else {
                        VolFlowRate = CoolingAirVolFlowRateDes; // system air flow
                    }
                    if (VolFlowRate >= DataHVACGlobals::SmallAirVolFlow) {
                        if (state.dataSize->ZoneEqDXCoil) {
                            if (ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow > 0.0) {
                                HeatMixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInTemp;
                                HeatMixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInHumRat;
                                // calculate mixed air temperature with system airflow
                                HeatOAFrac = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA / HeatingAirVolFlowRateDes;
                                HeatOAFracSys = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA / RatedAirVolFlowRateDes;
                                HeatOAFrac = min(1.0, max(0.0, HeatOAFrac));
                                HeatOAFracSys = min(1.0, max(0.0, HeatOAFracSys));
                                HeatOATemp = (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInTemp -
                                              (1.0 - HeatOAFrac) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneTempAtHeatPeak) /
                                             HeatOAFrac;
                                HeatMixTempSys =
                                    HeatOAFracSys * HeatOATemp +
                                    (1.0 - HeatOAFracSys) * state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneTempAtHeatPeak;
                            } else {
                                HeatMixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneRetTempAtHeatPeak;
                                HeatMixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak;
                                HeatMixTempSys = HeatMixTemp;
                            }
                        } else {
                            HeatMixTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInTemp;
                            HeatMixHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInHumRat;
                            HeatMixTempSys = HeatMixTemp;
                        }
                        HeatSupTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).HeatDesTemp;
                        // determine the coil ratio of coil dT with system air flow to design heating air flow
                        HeatdTratio = (HeatSupTemp - HeatMixTempSys) / (HeatSupTemp - HeatMixTemp);
                        rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, HeatMixTemp, HeatMixHumRat, RoutineName);
                        HeatCapAtPeak = rhoair * VolFlowRate * Psychrometrics::PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                        (HeatSupTemp - HeatMixTemp);                                     // heating coil load
                        if (state.dataSize->DataFanEnumType > -1 && state.dataSize->DataFanIndex > -1) { // add fan heat to coil load
                            switch (state.dataSize->DataFanEnumType) {
                            case DataAirSystems::StructArrayLegacyFanModels: {
                                FanHeatLoad = Fans::FanDesHeatGain(state, state.dataSize->DataFanIndex, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::ObjectVectorOOFanSystemModel: {
                                FanHeatLoad = state.dataHVACFan->fanObjs[state.dataSize->DataFanIndex]->getFanDesignHeatGain(state, VolFlowRate);
                                break;
                            }
                            case DataAirSystems::Invalid: {
                                // do nothing
                                break;
                            }
                            } // end switch
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(HeatMixHumRat);
                            if (state.dataSize->DataFanPlacement == DataSizing::ZoneFanPlacement::BlowThru) {
                                HeatMixTemp += FanHeatLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature entering the coil
                            } else {
                                HeatSupTemp -= FanHeatLoad / (CpAir * rhoair * VolFlowRate); // this is now the temperature leaving the coil
                            }
                        }
                        HeatCapAtPeak -= FanHeatLoad; // remove fan heat from heating coil load
                        HeatCapAtPeak = max(0.0, HeatCapAtPeak);
                        RatedHeatMixDryBulb = simpleWatertoAirHP.RatedEntAirDrybulbTemp;
                        // calculate temperatue ratio at design day peak conditions
                        HeatratioTDB = (HeatMixTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        PltSizNum = PlantUtilities::MyPlantSizingIndex(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            simpleWatertoAirHP.WaterInletNodeNum,
                            simpleWatertoAirHP.WaterOutletNodeNum,
                            ErrorsFound,
                            false);
                        if (PltSizNum > 0) {
                            DesignEntWaterTemp = state.dataSize->PlantSizData(PltSizNum).ExitTemp;
                            HeatratioTS = (DesignEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        } else {
                            ShowSevereError(state, "Autosizing of heating capacity requires a loop Sizing:Plant object");
                            ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                            ShowContinueError(state,
                                              format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                                     WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                     simpleWatertoAirHP.Name));
                            HeatratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                            ErrorsFound = true;
                        }
                        // calculate temperatue ratio at refrence conditions
                        RatedHeatratioTDB = (RatedHeatMixDryBulb + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        RatedHeatratioTS = (simpleWatertoAirHP.RatedEntWaterTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref;
                        // determine curve modifiers at peak and rated conditions
                        PeakHeatCapTempModFac = Curve::CurveValue(state, simpleWatertoAirHP.HeatCapCurveIndex, HeatratioTDB, HeatratioTS, 1.0, 1.0);
                        RatedHeatCapTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.HeatCapCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                        RatedHeatPowerTempModFac =
                            Curve::CurveValue(state, simpleWatertoAirHP.HeatPowCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                        // Check curve output when rated mixed air wetbulb is the design mixed air wetbulb
                        if (RatedHeatMixDryBulb == HeatMixTemp) {
                            if (RatedHeatCapTempModFac > 1.02 || RatedHeatCapTempModFac < 0.98) {
                                ShowWarningError(state,
                                                 format("{} Coil:Heating:WaterToAirHeatPump:EquationFit={}", RoutineName, simpleWatertoAirHP.Name));
                                ShowContinueError(state,
                                                  "Heating capacity as a function of temperature curve output is not equal to 1.0 (+ or - 2%) "
                                                  "at rated conditions.");
                                ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedHeatCapTempModFac));
                            }
                            if (RatedHeatPowerTempModFac > 1.02 || RatedHeatPowerTempModFac < 0.98) {
                                ShowWarningError(state,
                                                 format("{} Coil:Heating:WaterToAirHeatPump:EquationFit={}", RoutineName, simpleWatertoAirHP.Name));
                                ShowContinueError(state,
                                                  "Heating power consumption as a function of temperature curve output is not equal to "
                                                  "1.0 (+ or - 2%) at rated conditions.");
                                ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedHeatPowerTempModFac));
                            }
                        }
                        // calculate the rated capacity based on peak conditions
                        // note: the rated capacity can be different than the capacity at
                        // rated conditions if the capacity curve isn't normalized at the
                        // rated conditions
                        RatedCapHeatDes = (PeakHeatCapTempModFac > 0.0) ? HeatCapAtPeak / PeakHeatCapTempModFac : HeatCapAtPeak;
                    } else {
                        RatedHeatratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                        RatedCapHeatDes = 0.0;
                    }
                } else {
                    RatedHeatratioTS = 0.0; // Clang complains it is used uninitialized if you don't give it a value
                }

                // determine adjusted cooling and heating coil capacity
                simpleWatertoAirHP.RatedCapHeatAtRatedCdts = RatedCapHeatDes * RatedHeatCapTempModFac;
                auto &companionCoolingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum));
                if (companionCoolingCoil.WAHPPlantType == DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit &&
                    companionCoolingCoil.RatedCapCoolTotal == DataSizing::AutoSize) {
                    // case 1: companion coil is also of EquationFit type and is being autosized
                    RatedCapCoolTotalDes = state.dataSize->DXCoolCap;
                    RatedTotCapTempModFac = companionCoolingCoil.RatedCapCoolAtRatedCdts / RatedCapCoolTotalDes;
                    RatedCapCoolHeatDD =
                        simpleWatertoAirHP.RatedCapHeatAtRatedCdts / simpleWatertoAirHP.RatioRatedHeatRatedTotCoolCap / RatedTotCapTempModFac;
                    RatedCoolPowerTempModFac = companionCoolingCoil.RatedPowerCoolAtRatedCdts / companionCoolingCoil.RatedPowerCool;
                    if (RatedCapCoolHeatDD > RatedCapCoolTotalDes) {
                        // total cooling capacity
                        RatedCapCoolTotalDes = RatedCapCoolHeatDD;
                        // adjust for system air flow -- capacity is based on heating design day calcs
                        // adjust by ratio of system to heating air flow rate and temperature delta across the coil at these different airflow
                        if (HeatingAirVolFlowRateDes > 0) {
                            RatedCapCoolTotalDes *= (RatedAirVolFlowRateDes / HeatingAirVolFlowRateDes) * HeatdTratio;
                        }
                        // calculate ajustment factor over previous capacity for sensible capacity adjustment
                        Real64 CapCoolAdjFac = RatedCapCoolTotalDes / state.dataSize->DXCoolCap;
                        // update cooling coil rated capacity after adjustments based on heating coil size
                        state.dataSize->DXCoolCap = RatedCapCoolTotalDes;
                        // sensible cooling capacity
                        RatedCapCoolSensDes = companionCoolingCoil.RatedCapCoolSens * CapCoolAdjFac; // Assume that SHR stays the same
                        companionCoolingCoil.RatedCapCoolSensDesAtRatedCdts *= CapCoolAdjFac;
                        companionCoolingCoil.RatedCapCoolSens = RatedCapCoolSensDes;
                        // update Water-to-Air Heat Pumps output reports
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchWAHPRatedSensCapAtRatedCdts,
                                                                 companionCoolingCoil.Name,
                                                                 companionCoolingCoil.RatedCapCoolSensDesAtRatedCdts);
                        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPDD, companionCoolingCoil.Name, "Heating");
                        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPDD, simpleWatertoAirHP.Name, "Heating");
                        // update Cooling Coils output reports
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                                                 companionCoolingCoil.Name,
                                                                 RatedCapCoolTotalDes - RatedCapCoolSensDes);
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchCoolCoilSHR,
                                                                 companionCoolingCoil.Name,
                                                                 RatedCapCoolSensDes / RatedCapCoolTotalDes);
                        OutputReportPredefined::PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchCoolCoilSensCap, companionCoolingCoil.Name, RatedCapCoolSensDes);
                    } else {
                        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPDD, companionCoolingCoil.Name, "Cooling");
                        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchWAHPDD, simpleWatertoAirHP.Name, "Cooling");
                    }
                    RatedCapHeatDes =
                        RatedCapCoolTotalDes * RatedTotCapTempModFac * simpleWatertoAirHP.RatioRatedHeatRatedTotCoolCap / RatedHeatCapTempModFac;
                    companionCoolingCoil.RatedCapCoolTotal = RatedCapCoolTotalDes;
                    companionCoolingCoil.RatedCapCoolAtRatedCdts = RatedCapCoolTotalDes * RatedTotCapTempModFac;
                    companionCoolingCoil.RatedPowerCoolAtRatedCdts =
                        companionCoolingCoil.RatedCapCoolAtRatedCdts / companionCoolingCoil.RatedCOPCoolAtRatedCdts;
                    companionCoolingCoil.RatedPowerCool = companionCoolingCoil.RatedPowerCoolAtRatedCdts / RatedCoolPowerTempModFac;
                    // update Water-to-Air Heat Pumps output reports
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchWAHPRatedCapAtRatedCdts,
                                                             companionCoolingCoil.Name,
                                                             companionCoolingCoil.RatedCapCoolAtRatedCdts);
                    // update Cooling Coils output reports
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchCoolCoilTotCap, companionCoolingCoil.Name, RatedCapCoolTotalDes);
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(companionCoolingCoil.WAHPType)]),
                        companionCoolingCoil.Name,
                        "Design Size Rated Total Cooling Capacity [W]",
                        companionCoolingCoil.RatedCapCoolTotal);
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(companionCoolingCoil.WAHPType)]),
                        companionCoolingCoil.Name,
                        "Design Size Rated Sensible Cooling Capacity [W]",
                        companionCoolingCoil.RatedCapCoolSens);
                } else if (companionCoolingCoil.WAHPPlantType ==
                           DataPlant::PlantEquipmentType::CoilWAHPCoolingEquationFit) { // case 2: companion coil is of EquationFit type but is
                                                                                        // not autosized
                    RatedCapHeatDes = companionCoolingCoil.RatedCapCoolTotal * simpleWatertoAirHP.RatioRatedHeatRatedTotCoolCap;
                } else { // case 3: companion type is different than EquationFit
                    RatedCapHeatDes = state.dataSize->DXCoolCap;
                }
                // heating capacity final determination
                simpleWatertoAirHP.RatedCapHeat = RatedCapHeatDes;
                simpleWatertoAirHP.RatedCapHeatAtRatedCdts = RatedCapHeatDes * RatedHeatCapTempModFac;

                // heating power calculations
                RatedHeatPowerTempModFac =
                    Curve::CurveValue(state, simpleWatertoAirHP.HeatPowCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                simpleWatertoAirHP.RatedPowerHeat =
                    simpleWatertoAirHP.RatedCapHeatAtRatedCdts / (simpleWatertoAirHP.RatedCOPHeatAtRatedCdts * RatedHeatPowerTempModFac);

                // update reports
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchWAHPRatedCapAtRatedCdts,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapHeatAtRatedCdts);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchWAHPRatedAirDBT, simpleWatertoAirHP.Name, RatedHeatMixDryBulb);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchWAHPRatedWtrT, simpleWatertoAirHP.Name, simpleWatertoAirHP.RatedEntWaterTemp);
                BaseSizer::reportSizerOutput(
                    state,
                    format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                    simpleWatertoAirHP.Name,
                    "Design Size Rated Heating Capacity [W]",
                    simpleWatertoAirHP.RatedCapHeat);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchHeatCoilNomCap, simpleWatertoAirHP.Name, simpleWatertoAirHP.RatedCapHeat);
                if (simpleWatertoAirHP.RatedCapHeat != 0.0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchHeatCoilNomEff,
                                                             simpleWatertoAirHP.Name,
                                                             simpleWatertoAirHP.RatedPowerHeat / simpleWatertoAirHP.RatedCapHeat);
                } else {
                    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomEff, simpleWatertoAirHP.Name, 0.0);
                }
            } else {
                if (simpleWatertoAirHP.RatedCapHeat > 0.0 && RatedCapHeatDes > 0.0 && !HardSizeNoDesRun) {
                    RatedCapHeatUser = simpleWatertoAirHP.RatedCapHeat;
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.Name,
                        "Design Size Rated Heating Capacity [W]",
                        RatedCapHeatDes,
                        "User-Specified Rated Heating Capacity [W]",
                        RatedCapHeatUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(RatedCapHeatDes - RatedCapHeatUser) / RatedCapHeatUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(
                                state,
                                format("SizeHVACWaterToAir: Potential issue with equipment sizing for coil {}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                       WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                       simpleWatertoAirHP.Name));
                            ShowContinueError(state, format("User-Specified Rated Heating Capacity of {:.2R} [W]", RatedCapHeatUser));
                            ShowContinueError(state, format("differs from Design Size Rated Heating Capacity of {:.2R} [W]", RatedCapHeatDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                } else {
                    if (simpleWatertoAirHP.RatedCapHeat > 0.0) {
                        RatedCapHeatUser = simpleWatertoAirHP.RatedCapHeat;
                        BaseSizer::reportSizerOutput(
                            state,
                            format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                            simpleWatertoAirHP.Name,
                            "User-Specified Rated Heating Capacity [W]",
                            RatedCapHeatUser);
                    }
                }

                // user provided inputs are assumed to be at rated conditions
                simpleWatertoAirHP.RatedPowerHeat = simpleWatertoAirHP.RatedCapHeat / simpleWatertoAirHP.RatedCOPHeatAtRatedCdts;
                simpleWatertoAirHP.RatedCapHeatAtRatedCdts = 0;
                simpleWatertoAirHP.RatedPowerHeatAtRatedCdts = 0;
            }
            // Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
            if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating && simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                auto &companionCoolingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum));
                if (companionCoolingCoil.RatedCapCoolTotal > 0.0) {

                    if (std::abs(companionCoolingCoil.RatedCapCoolTotal - simpleWatertoAirHP.RatedCapHeat) / companionCoolingCoil.RatedCapCoolTotal >
                        0.2) {

                        ShowWarningError(state,
                                         format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                                WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                                simpleWatertoAirHP.Name));
                        ShowContinueError(state,
                                          format("...used with COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                                 companionCoolingCoil.WAHPType,
                                                 companionCoolingCoil.Name));
                        ShowContinueError(state, "...heating capacity is disproportionate (> 20% different) to total cooling capacity");
                        ShowContinueError(state, format("...heating capacity = {:.3T} W", simpleWatertoAirHP.RatedCapHeat));
                        ShowContinueError(state, format("...cooling capacity = {:.3T} W", companionCoolingCoil.RatedCapCoolTotal));
                    }
                }
            }

            state.dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(
                state,
                simpleWatertoAirHP.Name,
                format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                RatedCapHeatDes,
                IsAutoSize,
                state.dataSize->CurSysNum,
                state.dataSize->CurZoneEqNum,
                state.dataSize->CurOASysNum,
                FanCoolLoad,
                1.0, // RatedHeatCapTempModFac,
                -999.0,
                -999.0);

        } // Heating

        // size/report rated efficiency and power
        if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
            if (simpleWatertoAirHP.RatedPowerCool > 0) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchCoolCoilNomEff,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapCoolTotal / simpleWatertoAirHP.RatedPowerCool);
            }
            if (IsAutoSize) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchWAHPRatedPowerAtRatedCdts,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedPowerCoolAtRatedCdts);
                if (simpleWatertoAirHP.RatedPowerCoolAtRatedCdts > 0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchWAHPRatedCOPAtRatedCdts,
                                                             simpleWatertoAirHP.Name,
                                                             simpleWatertoAirHP.RatedCapCoolAtRatedCdts /
                                                                 simpleWatertoAirHP.RatedPowerCoolAtRatedCdts);
                }
            }
        } else if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating) {
            // heating coil power
            simpleWatertoAirHP.RatedPowerHeatAtRatedCdts = simpleWatertoAirHP.RatedCapHeatAtRatedCdts / simpleWatertoAirHP.RatedCOPHeatAtRatedCdts;
            if (simpleWatertoAirHP.RatedPowerHeat > 0) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchHeatCoilNomEff,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedCapHeat / simpleWatertoAirHP.RatedPowerHeat);
            }
            if (IsAutoSize) {
                OutputReportPredefined::PreDefTableEntry(state,
                                                         state.dataOutRptPredefined->pdchWAHPRatedPowerAtRatedCdts,
                                                         simpleWatertoAirHP.Name,
                                                         simpleWatertoAirHP.RatedPowerHeatAtRatedCdts);
                if (simpleWatertoAirHP.RatedPowerHeatAtRatedCdts > 0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchWAHPRatedCOPAtRatedCdts,
                                                             simpleWatertoAirHP.Name,
                                                             simpleWatertoAirHP.RatedCapHeatAtRatedCdts /
                                                                 simpleWatertoAirHP.RatedPowerHeatAtRatedCdts);
                }
            }
            // re-calculate companion coil power
            if (simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                auto &companionCoolingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum));
                companionCoolingCoil.RatedPowerCoolAtRatedCdts =
                    companionCoolingCoil.RatedCapCoolAtRatedCdts / companionCoolingCoil.RatedCOPCoolAtRatedCdts;
                if (companionCoolingCoil.RatedCapCoolTotal > 0) {
                    OutputReportPredefined::PreDefTableEntry(state,
                                                             state.dataOutRptPredefined->pdchCoolCoilNomEff,
                                                             companionCoolingCoil.Name,
                                                             companionCoolingCoil.RatedCapCoolTotal / companionCoolingCoil.RatedPowerCool);
                    if (IsAutoSize) {
                        OutputReportPredefined::PreDefTableEntry(state,
                                                                 state.dataOutRptPredefined->pdchWAHPRatedPowerAtRatedCdts,
                                                                 companionCoolingCoil.Name,
                                                                 companionCoolingCoil.RatedPowerCoolAtRatedCdts);
                        if (companionCoolingCoil.RatedPowerCoolAtRatedCdts > 0) {
                            OutputReportPredefined::PreDefTableEntry(state,
                                                                     state.dataOutRptPredefined->pdchWAHPRatedCOPAtRatedCdts,
                                                                     companionCoolingCoil.Name,
                                                                     companionCoolingCoil.RatedCapCoolAtRatedCdts /
                                                                         companionCoolingCoil.RatedPowerCoolAtRatedCdts);
                        }
                    }
                }
            }
        }

        // Size water volumetric flow rate
        IsAutoSize = false;
        if (simpleWatertoAirHP.RatedWaterVolFlowRate == DataSizing::AutoSize) {
            IsAutoSize = true;
        }

        //   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
        //   first check to see if coil is connected to a plant loop, no warning on this CALL
        if (IsAutoSize) {
            PltSizNum = PlantUtilities::MyPlantSizingIndex(
                state,
                format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                simpleWatertoAirHP.Name,
                simpleWatertoAirHP.WaterInletNodeNum,
                simpleWatertoAirHP.WaterOutletNodeNum,
                ErrorsFound,
                false);

            if (PltSizNum > 0) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidName,
                                                        state.dataSize->PlantSizData(PltSizNum).ExitTemp,
                                                        state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidIndex,
                                                        RoutineNameAlt);
                Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidName,
                                                            state.dataSize->PlantSizData(PltSizNum).ExitTemp,
                                                            state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidIndex,
                                                            RoutineNameAlt);

                if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating) {
                    RatedWaterVolFlowRateDes =
                        simpleWatertoAirHP.RatedCapHeatAtRatedCdts / (state.dataSize->PlantSizData(PltSizNum).DeltaT * Cp * rho);
                } else if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
                    //       use companion heating coil capacity to calculate volumetric flow rate
                    if (simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                        auto &companionHeatingCoil(
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum));
                        if (companionHeatingCoil.RatedCapHeat == DataSizing::AutoSize) {
                            SystemCapacity = simpleWatertoAirHP.RatedCapCoolTotal;
                        } else {
                            SystemCapacity = companionHeatingCoil.RatedCapHeat;
                        }
                    } else {
                        SystemCapacity = simpleWatertoAirHP.RatedCapCoolAtRatedCdts;
                    }

                    RatedWaterVolFlowRateDes = SystemCapacity / (state.dataSize->PlantSizData(PltSizNum).DeltaT * Cp * rho);
                }
            } else {
                ShowSevereError(state, "Autosizing of water flow requires a loop Sizing:Plant object");
                ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                ShowContinueError(state,
                                  format("Occurs in COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT Object={}",
                                         WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)],
                                         simpleWatertoAirHP.Name));
                ErrorsFound = true;
            }

            if (SystemCapacity != DataSizing::AutoSize) {
                simpleWatertoAirHP.RatedWaterVolFlowRate = RatedWaterVolFlowRateDes;
                BaseSizer::reportSizerOutput(
                    state,
                    format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                    simpleWatertoAirHP.Name,
                    "Design Size Rated Water Flow Rate [m3/s]",
                    RatedWaterVolFlowRateDes);
                if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating && simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                    auto &companionCoolingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum));
                    companionCoolingCoil.RatedWaterVolFlowRate = RatedWaterVolFlowRateDes;
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(companionCoolingCoil.WAHPType)]),
                        companionCoolingCoil.Name,
                        "Design Size Rated Water Flow Rate [m3/s]",
                        RatedWaterVolFlowRateDes);
                } else if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling && simpleWatertoAirHP.CompanionHeatingCoilNum > 0) {
                    auto &companionHeatingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionHeatingCoilNum));
                    companionHeatingCoil.RatedWaterVolFlowRate = RatedWaterVolFlowRateDes;
                    BaseSizer::reportSizerOutput(
                        state,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(companionHeatingCoil.WAHPType)]),
                        companionHeatingCoil.Name,
                        "Design Size Rated Water Flow Rate [m3/s]",
                        RatedWaterVolFlowRateDes);
                }
            }
        } else {
            if (simpleWatertoAirHP.RatedWaterVolFlowRate > 0.0 && RatedWaterVolFlowRateDes > 0.0) {
                RatedWaterVolFlowRateUser = simpleWatertoAirHP.RatedWaterVolFlowRate;
                BaseSizer::reportSizerOutput(
                    state,
                    format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                    simpleWatertoAirHP.Name,
                    "Design Size Rated Water Flow Rate [m3/s]",
                    RatedWaterVolFlowRateDes,
                    "User-Specified Rated Water Flow Rate [m3/s]",
                    RatedWaterVolFlowRateUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(RatedWaterVolFlowRateDes - RatedWaterVolFlowRateUser) / RatedWaterVolFlowRateUser) >
                        state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state,
                                    format("SizeHVACWaterToAir: Potential issue with equipment sizing for coil {}:WATERTOAIRHEATPUMP:EQUATIONFIT {}",
                                           simpleWatertoAirHP.WAHPType,
                                           simpleWatertoAirHP.Name));
                        ShowContinueError(state, format("User-Specified Rated Water Flow Rate of {:.5R} [m3/s]", RatedWaterVolFlowRateUser));
                        ShowContinueError(state, format("differs from Design Size Rated Water Flow Rate of {:.5R} [m3/s]", RatedWaterVolFlowRateDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }

        // Save component design water volumetric flow rate.
        // Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
        if (simpleWatertoAirHP.RatedWaterVolFlowRate > 0.0) {
            if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating) {
                PlantUtilities::RegisterPlantCompDesignFlow(
                    state, simpleWatertoAirHP.WaterInletNodeNum, 0.5 * simpleWatertoAirHP.RatedWaterVolFlowRate);
                if (simpleWatertoAirHP.CompanionCoolingCoilNum > 0) {
                    auto &companionCoolingCoil(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(simpleWatertoAirHP.CompanionCoolingCoilNum));
                    PlantUtilities::RegisterPlantCompDesignFlow(
                        state, companionCoolingCoil.WaterInletNodeNum, 0.5 * simpleWatertoAirHP.RatedWaterVolFlowRate);
                }
            } else if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
                PlantUtilities::RegisterPlantCompDesignFlow(
                    state, simpleWatertoAirHP.WaterInletNodeNum, 0.5 * simpleWatertoAirHP.RatedWaterVolFlowRate);
            }
        }
    }

    void CalcHPCoolingSimple(EnergyPlusData &state,
                             int const HPNum,                                         // Heat Pump Number
                             int const CyclingScheme,                                 // Fan/Compressor cycling scheme indicator
                             [[maybe_unused]] Real64 const SensDemand,                // Cooling Sensible Demand [W] !unused1208
                             [[maybe_unused]] Real64 const LatentDemand,              // Cooling Latent Demand [W]
                             DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation flag
                             Real64 const PartLoadRatio,                              // compressor part load ratio
                             [[maybe_unused]] Real64 const OnOffAirFlowRatio          // ratio of compressor on flow to average flow over time step
    )
    {

        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Jan 2004
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for simulating the cooling mode of the Water to Air HP Simple

        // METHODOLOGY EMPLOYED:
        // Simulate the heat pump performance using the coefficients in quadlinear and quintlinear curves and rated conditions
        // If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
        // (1)first simulation at the rated conditions (2) second simulation at the
        // actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
        // is adjusted.
        // If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
        // once at the actual operating conditions.
        // Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
        // and RuntimeFrac.

        // REFERENCES:
        // (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
        // M.S. Thesis, University of Illinois at Urbana Champaign.
        // (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
        // (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
        // (4) Henderson, H.I., K. Rengarajan.1996. A Model to Predict the Latent
        // Capacity of Air Conditioners and Heat Pumps at Part-Load Conditions
        // with Constant Fan Operation ASHRAE Transactions 102 (1), pp. 266-274.

        Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 Tref(283.15); // Reference Temperature for performance curves,10C [K]
        static constexpr std::string_view RoutineName("CalcHPCoolingSimple");
        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcHPCoolingSimple:SourceSideInletTemp");

        Real64 TotalCapRated;              // Rated Total Cooling Capacity [W]
        Real64 SensCapRated;               // Rated Sensible Cooling Capacity [W]
        Real64 CoolPowerRated;             // Rated Cooling Power Input[W]
        Real64 AirVolFlowRateRated;        // Rated Air Volumetric Flow Rate [m3/s]
        Real64 WaterVolFlowRateRated;      // Rated Water Volumetric Flow Rate [m3/s]
        Real64 Twet_Rated;                 // Twet at rated conditions (coil air flow rate and air temperatures), sec
        Real64 Gamma_Rated;                // Gamma at rated conditions (coil air flow rate and air temperatures)
        Real64 SHRss;                      // Sensible heat ratio at steady state
        Real64 SHReff;                     // Effective sensible heat ratio at part-load condition
        Real64 ratioTDB;                   // Ratio of the inlet air dry bulb temperature to the rated conditions
        Real64 ratioTWB;                   // Ratio of the inlet air wet bulb temperature to the rated conditions
        Real64 ratioTS;                    // Ratio of the source side(water) inlet temperature to the rated conditions
        Real64 ratioVL;                    // Ratio of the air flow rate to the rated conditions
        Real64 ratioVS;                    // Ratio of the water flow rate to the rated conditions
        Real64 CpWater;                    // Specific heat of water [J/kg_C]
        Real64 CpAir;                      // Specific heat of air [J/kg_C]
        Real64 LoadSideFullMassFlowRate;   // Load Side Full Load Mass Flow Rate [kg/s]
        Real64 LoadSideFullOutletEnthalpy; // Load Side Full Load Outlet Air Enthalpy [J/kg]

        bool LatDegradModelSimFlag;      // Latent degradation model simulation flag
        int NumIteration;                // Iteration Counter
        Real64 LoadSideInletDBTemp_Unit; // calc conditions for unit
        Real64 LoadSideInletWBTemp_Unit; // calc conditions for unit
        Real64 LoadSideInletHumRat_Unit; // calc conditions for unit
        Real64 LoadSideInletEnth_Unit;   // calc conditions for unit
        Real64 CpAir_Unit;               // calc conditions for unit

        if (state.dataWaterToAirHeatPumpSimple->firstTime) {
            // Set indoor air conditions to the rated condition
            state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp_Init = 26.7;
            state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat_Init = 0.0111;
            state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth_Init = Psychrometrics::PsyHFnTdbW(
                state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp_Init, state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat_Init);
            state.dataWaterToAirHeatPumpSimple->CpAir_Init =
                Psychrometrics::PsyCpAirFnW(state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat_Init);
            state.dataWaterToAirHeatPumpSimple->firstTime = false;
        }
        state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp_Init =
            Psychrometrics::PsyTwbFnTdbWPb(state,
                                           state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp_Init,
                                           state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat_Init,
                                           state.dataEnvrn->OutBaroPress,
                                           RoutineName);

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        TotalCapRated = simpleWatertoAirHP.RatedCapCoolTotal;
        SensCapRated = simpleWatertoAirHP.RatedCapCoolSens;
        CoolPowerRated = simpleWatertoAirHP.RatedPowerCool;
        AirVolFlowRateRated = simpleWatertoAirHP.RatedAirVolFlowRate;
        WaterVolFlowRateRated = simpleWatertoAirHP.RatedWaterVolFlowRate;

        Twet_Rated = simpleWatertoAirHP.Twet_Rated;
        Gamma_Rated = simpleWatertoAirHP.Gamma_Rated;

        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            LoadSideFullMassFlowRate = simpleWatertoAirHP.AirMassFlowRate;
        } else {
            // default to cycling fan, cycling compressor, full load air flow
            if (PartLoadRatio > 0.0) {
                LoadSideFullMassFlowRate = simpleWatertoAirHP.AirMassFlowRate / PartLoadRatio;
            } else {
                LoadSideFullMassFlowRate = 0.0;
            }
        }
        state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate = simpleWatertoAirHP.WaterMassFlowRate;
        state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp = simpleWatertoAirHP.InletWaterTemp;
        state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth = simpleWatertoAirHP.InletWaterEnthalpy;
        CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                         state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidName,
                                                         state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp,
                                                         state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidIndex,
                                                         RoutineNameSourceSideInletTemp);

        // Check for flows, do not perform simulation if no flow in load side or source side.
        if (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate <= 0.0 || LoadSideFullMassFlowRate <= 0.0) {
            simpleWatertoAirHP.SimFlag = false;
            return;
        } else {
            simpleWatertoAirHP.SimFlag = true;
        }

        if (CompressorOp == DataHVACGlobals::CompressorOperation::Off) {
            simpleWatertoAirHP.SimFlag = false;
            return;
        }

        // Calculate Part Load Factor and Runtime Fraction
        Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
        if (simpleWatertoAirHP.PLFCurveIndex > 0) {
            PLF = Curve::CurveValue(state, simpleWatertoAirHP.PLFCurveIndex, PartLoadRatio); // Calculate part-load factor
        }
        if (CyclingScheme == DataHVACGlobals::CycFanCycCoil) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = PLF;
        }
        simpleWatertoAirHP.RunFrac = PartLoadRatio / PLF;

        // Loop the calculation at least once depending whether the latent degradation model
        // is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
        // and 2nd iteration to calculate the  QLatent(actual)
        if ((simpleWatertoAirHP.RunFrac >= 1.0) || (Twet_Rated <= 0.0) || (Gamma_Rated <= 0.0)) {
            LatDegradModelSimFlag = false;
            // Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
            NumIteration = 1;
        } else {
            LatDegradModelSimFlag = true;
            // Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
            NumIteration = 0;
        }

        // Set indoor air conditions to the actual condition
        LoadSideInletDBTemp_Unit = simpleWatertoAirHP.InletAirDBTemp;
        LoadSideInletHumRat_Unit = simpleWatertoAirHP.InletAirHumRat;
        LoadSideInletWBTemp_Unit =
            Psychrometrics::PsyTwbFnTdbWPb(state, LoadSideInletDBTemp_Unit, LoadSideInletHumRat_Unit, state.dataEnvrn->OutBaroPress, RoutineName);
        LoadSideInletEnth_Unit = simpleWatertoAirHP.InletAirEnthalpy;
        CpAir_Unit = Psychrometrics::PsyCpAirFnW(LoadSideInletHumRat_Unit);

        while (true) {
            ++NumIteration;
            if (NumIteration == 1) {
                // Set indoor air conditions to the rated conditions
                state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp = state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp_Init;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat = state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat_Init;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp = state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp_Init;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth = state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth_Init;
                CpAir = state.dataWaterToAirHeatPumpSimple->CpAir_Init;
            } else {
                // Set indoor air conditions to the actual condition
                state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp = LoadSideInletDBTemp_Unit;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat = LoadSideInletHumRat_Unit;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp = LoadSideInletWBTemp_Unit;
                state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth = LoadSideInletEnth_Unit;
                CpAir = CpAir_Unit;
            }

            ratioTDB = ((state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref);
            ratioTWB = ((state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref);
            ratioTS = ((state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref);
            ratioVL = (LoadSideFullMassFlowRate /
                       (AirVolFlowRateRated * Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                                                state.dataEnvrn->StdBaroPress,
                                                                                state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp,
                                                                                state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat,
                                                                                RoutineName)));

            if (simpleWatertoAirHP.DesignWaterMassFlowRate > 0.0) {
                ratioVS = (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate) / (simpleWatertoAirHP.DesignWaterMassFlowRate);
            } else {
                ratioVS = 0.0;
            }

            simpleWatertoAirHP.QLoadTotal =
                TotalCapRated * Curve::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, ratioTWB, ratioTS, ratioVL, ratioVS);
            simpleWatertoAirHP.QSensible =
                SensCapRated * Curve::CurveValue(state, simpleWatertoAirHP.SensCoolCapCurveIndex, ratioTDB, ratioTWB, ratioTS, ratioVL, ratioVS);
            state.dataWaterToAirHeatPumpSimple->Winput =
                CoolPowerRated * Curve::CurveValue(state, simpleWatertoAirHP.CoolPowCurveIndex, ratioTWB, ratioTS, ratioVL, ratioVS);

            // Check if the Sensible Load is greater than the Total Cooling Load
            if (simpleWatertoAirHP.QSensible > simpleWatertoAirHP.QLoadTotal) {
                simpleWatertoAirHP.QSensible = simpleWatertoAirHP.QLoadTotal;
            }

            if (LatDegradModelSimFlag) {
                // Calculate for SHReff using the Latent Degradation Model
                if (NumIteration == 1) {
                    state.dataWaterToAirHeatPumpSimple->QLatRated = simpleWatertoAirHP.QLoadTotal - simpleWatertoAirHP.QSensible;
                } else if (NumIteration == 2) {
                    state.dataWaterToAirHeatPumpSimple->QLatActual = simpleWatertoAirHP.QLoadTotal - simpleWatertoAirHP.QSensible;
                    SHRss = simpleWatertoAirHP.QSensible / simpleWatertoAirHP.QLoadTotal;
                    SHReff = CalcEffectiveSHR(state,
                                              HPNum,
                                              SHRss,
                                              CyclingScheme,
                                              simpleWatertoAirHP.RunFrac,
                                              state.dataWaterToAirHeatPumpSimple->QLatRated,
                                              state.dataWaterToAirHeatPumpSimple->QLatActual,
                                              state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp,
                                              state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp);
                    //       Update sensible capacity based on effective SHR
                    simpleWatertoAirHP.QSensible = simpleWatertoAirHP.QLoadTotal * SHReff;
                    break;
                }
            } else {
                // Assume SHReff=SHRss
                SHReff = simpleWatertoAirHP.QSensible / simpleWatertoAirHP.QLoadTotal;
                break;
            }
        }

        // calculate coil outlet state variables
        LoadSideFullOutletEnthalpy = state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth - simpleWatertoAirHP.QLoadTotal / LoadSideFullMassFlowRate;
        state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp =
            state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp - simpleWatertoAirHP.QSensible / (LoadSideFullMassFlowRate * CpAir);
        state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat =
            Psychrometrics::PsyWFnTdbH(state, state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp, LoadSideFullOutletEnthalpy, RoutineName);
        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            // continuous fan, cycling compressor
            simpleWatertoAirHP.OutletAirEnthalpy =
                PartLoadRatio * LoadSideFullOutletEnthalpy + (1.0 - PartLoadRatio) * state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth;
            simpleWatertoAirHP.OutletAirHumRat = PartLoadRatio * state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat +
                                                 (1.0 - PartLoadRatio) * state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat;
            simpleWatertoAirHP.OutletAirDBTemp = Psychrometrics::PsyTdbFnHW(simpleWatertoAirHP.OutletAirEnthalpy, simpleWatertoAirHP.OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            simpleWatertoAirHP.OutletAirEnthalpy = LoadSideFullOutletEnthalpy;
            simpleWatertoAirHP.OutletAirHumRat = state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat;
            simpleWatertoAirHP.OutletAirDBTemp = state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp;
        }

        // scale heat transfer rates to PLR and power to RTF
        simpleWatertoAirHP.QLoadTotal *= PartLoadRatio;
        simpleWatertoAirHP.QLoadTotalReport = simpleWatertoAirHP.AirMassFlowRate *
                                              (state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth -
                                               Psychrometrics::PsyHFnTdbW(simpleWatertoAirHP.OutletAirDBTemp,
                                                                          simpleWatertoAirHP.OutletAirHumRat)); // Why doesn't this match QLoadTotal?
        simpleWatertoAirHP.QSensible *= PartLoadRatio;
        state.dataWaterToAirHeatPumpSimple->Winput *= simpleWatertoAirHP.RunFrac;
        simpleWatertoAirHP.QSource = simpleWatertoAirHP.QLoadTotalReport + state.dataWaterToAirHeatPumpSimple->Winput;
        state.dataHeatBal->HeatReclaimSimple_WAHPCoil(HPNum).AvailCapacity = simpleWatertoAirHP.QSource;

        //  Add power to global variable so power can be summed by parent object
        state.dataHVACGlobal->DXElecCoolingPower = state.dataWaterToAirHeatPumpSimple->Winput;

        DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimSimple_WAHPCoil(HPNum);
        HeatReclaim.WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
        if (allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
            for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                HeatReclaim.WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        }
        simpleWatertoAirHP.QSource -= HeatReclaim.WaterHeatingDesuperheaterReclaimedHeatTotal;

        // Update heat pump data structure
        simpleWatertoAirHP.Power = state.dataWaterToAirHeatPumpSimple->Winput;
        simpleWatertoAirHP.QLoadTotal = simpleWatertoAirHP.QLoadTotalReport;
        simpleWatertoAirHP.QLatent = simpleWatertoAirHP.QLoadTotalReport - simpleWatertoAirHP.QSensible;
        simpleWatertoAirHP.Energy = state.dataWaterToAirHeatPumpSimple->Winput * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLoadTotal = simpleWatertoAirHP.QLoadTotalReport * TimeStepSysSec;
        simpleWatertoAirHP.EnergySensible = simpleWatertoAirHP.QSensible * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLatent = (simpleWatertoAirHP.QLoadTotalReport - simpleWatertoAirHP.QSensible) * TimeStepSysSec;
        simpleWatertoAirHP.EnergySource = simpleWatertoAirHP.QSource * TimeStepSysSec;
        if (simpleWatertoAirHP.RunFrac == 0.0) {
            simpleWatertoAirHP.COP = 0.0;
        } else {
            simpleWatertoAirHP.COP = simpleWatertoAirHP.QLoadTotalReport / state.dataWaterToAirHeatPumpSimple->Winput;
        }
        simpleWatertoAirHP.PartLoadRatio = PartLoadRatio;

        if ((simpleWatertoAirHP.WaterCyclingMode) == DataHVACGlobals::WaterCycling) {
            // plant can lock flow at coil water inlet node, use design flow multiplied by PLR to calculate water mass flow rate
            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate * PartLoadRatio;
            PlantUtilities::SetComponentFlowRate(state,
                                                 simpleWatertoAirHP.WaterMassFlowRate,
                                                 simpleWatertoAirHP.WaterInletNodeNum,
                                                 simpleWatertoAirHP.WaterOutletNodeNum,
                                                 simpleWatertoAirHP.plantLoc);
            if (simpleWatertoAirHP.WaterMassFlowRate > 0.0) {
                simpleWatertoAirHP.OutletWaterTemp = state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp +
                                                     simpleWatertoAirHP.QSource / (simpleWatertoAirHP.WaterMassFlowRate * CpWater);
                simpleWatertoAirHP.OutletWaterEnthalpy =
                    state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth + simpleWatertoAirHP.QSource / simpleWatertoAirHP.WaterMassFlowRate;
            }
        } else {
            if ((simpleWatertoAirHP.WaterCyclingMode) == DataHVACGlobals::WaterConstant) {
                if (simpleWatertoAirHP.WaterFlowMode) {
                    simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         simpleWatertoAirHP.WaterMassFlowRate,
                                                         simpleWatertoAirHP.WaterInletNodeNum,
                                                         simpleWatertoAirHP.WaterOutletNodeNum,
                                                         simpleWatertoAirHP.plantLoc);
                } else {
                    simpleWatertoAirHP.WaterMassFlowRate = state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
                }
            } else {
                simpleWatertoAirHP.WaterMassFlowRate = state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
            }
            simpleWatertoAirHP.OutletWaterTemp = state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp +
                                                 simpleWatertoAirHP.QSource / (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate * CpWater);
            simpleWatertoAirHP.OutletWaterEnthalpy = state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth +
                                                     simpleWatertoAirHP.QSource / state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
        }
    }

    void CalcHPHeatingSimple(EnergyPlusData &state,
                             int const HPNum,                                         // Heat Pump Number
                             int const CyclingScheme,                                 // Fan/Compressor cycling scheme indicator
                             [[maybe_unused]] Real64 const SensDemand,                // Sensible Demand [W] !unused1208
                             DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation flag
                             Real64 const PartLoadRatio,                              // compressor part load ratio
                             [[maybe_unused]] Real64 const OnOffAirFlowRatio          // ratio of compressor on flow to average flow over time step
    )
    {

        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Jan 2004
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for simulating the heating mode of the Water to Air HP Simple

        // METHODOLOGY EMPLOYED:
        // Simulate the heat pump performance using the coefficients in quadlinear and quintlinear curves and rated conditions
        // Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
        // and RuntimeFrac.

        // REFERENCES:
        // (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
        // M.S. Thesis, University of Illinois at Urbana Champaign.
        // (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)
        // (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from www.hvac.okstate.edu)

        Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr Tref(283.15); // Reference Temperature for performance curves,10C [K]
        static constexpr std::string_view RoutineName("CalcHPHeatingSimple");
        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcHPHeatingSimple:SourceSideInletTemp");

        Real64 HeatCapRated;               // Rated Heating Capacity [W]
        Real64 HeatPowerRated;             // Rated Heating Power Input[W]
        Real64 AirVolFlowRateRated;        // Rated Air Volumetric Flow Rate [m3/s]
        Real64 WaterVolFlowRateRated;      // Rated Water Volumetric Flow Rate [m3/s]
        Real64 ratioTDB;                   // Ratio of the inlet air dry bulb temperature to the rated conditions
        Real64 ratioTS;                    // Ratio of the source side (water) inlet temperature to the rated conditions
        Real64 ratioVL;                    // Ratio of the load side flow rate to the rated conditions
        Real64 ratioVS;                    // Ratio of the source side flow rate to the rated conditions
        Real64 CpWater;                    // Specific heat of water [J/kg_C]
        Real64 CpAir;                      // Specific heat of air [J/kg_C]
        Real64 LoadSideFullMassFlowRate;   // Load Side Full Load Mass Flow Rate [kg/s]
        Real64 LoadSideFullOutletEnthalpy; // Load Side Full Load Outlet Air Enthalpy [J/kg]

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        HeatCapRated = simpleWatertoAirHP.RatedCapHeat;
        HeatPowerRated = simpleWatertoAirHP.RatedPowerHeat;
        AirVolFlowRateRated = simpleWatertoAirHP.RatedAirVolFlowRate;
        WaterVolFlowRateRated = simpleWatertoAirHP.RatedWaterVolFlowRate;
        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            LoadSideFullMassFlowRate = simpleWatertoAirHP.AirMassFlowRate;
        } else {
            // default to cycling fan, cycling compressor, full load air flow
            if (PartLoadRatio > 0.0) {
                LoadSideFullMassFlowRate = simpleWatertoAirHP.AirMassFlowRate / PartLoadRatio;
            } else {
                LoadSideFullMassFlowRate = 0.0;
            }
        }
        state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp = simpleWatertoAirHP.InletAirDBTemp;
        state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat = simpleWatertoAirHP.InletAirHumRat;

        state.dataWaterToAirHeatPumpSimple->LoadSideInletWBTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state,
                                           state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp,
                                           state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat,
                                           state.dataEnvrn->OutBaroPress,
                                           RoutineName);
        state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth = simpleWatertoAirHP.InletAirEnthalpy;
        CpAir = Psychrometrics::PsyCpAirFnW(state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat);
        state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate = simpleWatertoAirHP.WaterMassFlowRate;
        state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp = simpleWatertoAirHP.InletWaterTemp;
        state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth = simpleWatertoAirHP.InletWaterEnthalpy;
        CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                         state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidName,
                                                         state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp,
                                                         state.dataPlnt->PlantLoop(simpleWatertoAirHP.plantLoc.loopNum).FluidIndex,
                                                         RoutineNameSourceSideInletTemp);

        // Check for flows, do not perform simulation if no flow in load side or source side.
        if (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate <= 0.0 || LoadSideFullMassFlowRate <= 0.0) {
            simpleWatertoAirHP.SimFlag = false;
            return;
        } else {
            simpleWatertoAirHP.SimFlag = true;
        }

        if (CompressorOp == DataHVACGlobals::CompressorOperation::Off) {
            simpleWatertoAirHP.SimFlag = false;
            return;
        }

        // Calculate Part Load Factor and Runtime Fraction
        Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
        if (simpleWatertoAirHP.PLFCurveIndex > 0) {
            PLF = Curve::CurveValue(state, simpleWatertoAirHP.PLFCurveIndex, PartLoadRatio); // Calculate part-load factor
        }
        if (CyclingScheme == DataHVACGlobals::CycFanCycCoil) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = PLF;
        }
        simpleWatertoAirHP.RunFrac = PartLoadRatio / PLF;

        ratioTDB = ((state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref);
        ratioTS = ((state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp + state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) / Tref);
        ratioVL = (LoadSideFullMassFlowRate /
                   (AirVolFlowRateRated * Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                                            state.dataEnvrn->StdBaroPress,
                                                                            state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp,
                                                                            state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat,
                                                                            RoutineName)));
        if (simpleWatertoAirHP.DesignWaterMassFlowRate > 0.0) {
            ratioVS = (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate) / (simpleWatertoAirHP.DesignWaterMassFlowRate);
        } else {
            ratioVS = 0.0;
        }

        simpleWatertoAirHP.QLoadTotal =
            HeatCapRated * Curve::CurveValue(state, simpleWatertoAirHP.HeatCapCurveIndex, ratioTDB, ratioTS, ratioVL, ratioVS);
        simpleWatertoAirHP.QSensible = simpleWatertoAirHP.QLoadTotal;
        state.dataWaterToAirHeatPumpSimple->Winput =
            HeatPowerRated * Curve::CurveValue(state, simpleWatertoAirHP.HeatPowCurveIndex, ratioTDB, ratioTS, ratioVL, ratioVS);

        // calculate coil outlet state variables
        LoadSideFullOutletEnthalpy = state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth + simpleWatertoAirHP.QLoadTotal / LoadSideFullMassFlowRate;
        state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp =
            state.dataWaterToAirHeatPumpSimple->LoadSideInletDBTemp + simpleWatertoAirHP.QSensible / (LoadSideFullMassFlowRate * CpAir);
        state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat =
            Psychrometrics::PsyWFnTdbH(state, state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp, LoadSideFullOutletEnthalpy, RoutineName);

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == DataHVACGlobals::ContFanCycCoil) {
            // continuous fan, cycling compressor
            simpleWatertoAirHP.OutletAirEnthalpy =
                PartLoadRatio * LoadSideFullOutletEnthalpy + (1.0 - PartLoadRatio) * state.dataWaterToAirHeatPumpSimple->LoadSideInletEnth;
            simpleWatertoAirHP.OutletAirHumRat = PartLoadRatio * state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat +
                                                 (1.0 - PartLoadRatio) * state.dataWaterToAirHeatPumpSimple->LoadSideInletHumRat;
            simpleWatertoAirHP.OutletAirDBTemp = Psychrometrics::PsyTdbFnHW(simpleWatertoAirHP.OutletAirEnthalpy, simpleWatertoAirHP.OutletAirHumRat);
        } else {
            // default to cycling fan, cycling compressor
            simpleWatertoAirHP.OutletAirEnthalpy = LoadSideFullOutletEnthalpy;
            simpleWatertoAirHP.OutletAirHumRat = state.dataWaterToAirHeatPumpSimple->LoadSideOutletHumRat;
            simpleWatertoAirHP.OutletAirDBTemp = state.dataWaterToAirHeatPumpSimple->LoadSideOutletDBTemp;
        }

        // scale heat transfer rates to PLR and power to RTF
        simpleWatertoAirHP.QLoadTotal *= PartLoadRatio;
        simpleWatertoAirHP.QLoadTotalReport = simpleWatertoAirHP.QLoadTotal;
        simpleWatertoAirHP.QSensible *= PartLoadRatio;
        state.dataWaterToAirHeatPumpSimple->Winput *= simpleWatertoAirHP.RunFrac;
        simpleWatertoAirHP.QSource = simpleWatertoAirHP.QLoadTotalReport - state.dataWaterToAirHeatPumpSimple->Winput;

        //  Add power to global variable so power can be summed by parent object
        state.dataHVACGlobal->DXElecHeatingPower = state.dataWaterToAirHeatPumpSimple->Winput;

        // Update heat pump data structure
        simpleWatertoAirHP.Power = state.dataWaterToAirHeatPumpSimple->Winput;
        simpleWatertoAirHP.QLoadTotal = simpleWatertoAirHP.QLoadTotalReport;
        simpleWatertoAirHP.QSensible = simpleWatertoAirHP.QSensible;
        simpleWatertoAirHP.Energy = state.dataWaterToAirHeatPumpSimple->Winput * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLoadTotal = simpleWatertoAirHP.QLoadTotalReport * TimeStepSysSec;
        simpleWatertoAirHP.EnergySensible = simpleWatertoAirHP.QSensible * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLatent = 0.0;
        simpleWatertoAirHP.EnergySource = simpleWatertoAirHP.QSource * TimeStepSysSec;
        if (simpleWatertoAirHP.RunFrac == 0.0) {
            simpleWatertoAirHP.COP = 0.0;
        } else {
            simpleWatertoAirHP.COP = simpleWatertoAirHP.QLoadTotalReport / state.dataWaterToAirHeatPumpSimple->Winput;
        }
        simpleWatertoAirHP.PartLoadRatio = PartLoadRatio;

        if ((simpleWatertoAirHP.WaterCyclingMode) == DataHVACGlobals::WaterCycling) {
            // plant can lock flow at coil water inlet node, use design flow multiplied by PLR to calculate water mass flow rate
            simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate * PartLoadRatio;
            PlantUtilities::SetComponentFlowRate(state,
                                                 simpleWatertoAirHP.WaterMassFlowRate,
                                                 simpleWatertoAirHP.WaterInletNodeNum,
                                                 simpleWatertoAirHP.WaterOutletNodeNum,
                                                 simpleWatertoAirHP.plantLoc);
            if (simpleWatertoAirHP.WaterMassFlowRate > 0.0) {
                simpleWatertoAirHP.OutletWaterTemp = state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp -
                                                     simpleWatertoAirHP.QSource / (simpleWatertoAirHP.WaterMassFlowRate * CpWater);
                simpleWatertoAirHP.OutletWaterEnthalpy =
                    state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth - simpleWatertoAirHP.QSource / simpleWatertoAirHP.WaterMassFlowRate;
            }
        } else {
            if ((simpleWatertoAirHP.WaterCyclingMode) == DataHVACGlobals::WaterConstant) {
                if (simpleWatertoAirHP.WaterFlowMode) {
                    simpleWatertoAirHP.WaterMassFlowRate = simpleWatertoAirHP.DesignWaterMassFlowRate;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         simpleWatertoAirHP.WaterMassFlowRate,
                                                         simpleWatertoAirHP.WaterInletNodeNum,
                                                         simpleWatertoAirHP.WaterOutletNodeNum,
                                                         simpleWatertoAirHP.plantLoc);
                } else {
                    simpleWatertoAirHP.WaterMassFlowRate = state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
                }
            } else {
                simpleWatertoAirHP.WaterMassFlowRate = state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
            }
            simpleWatertoAirHP.OutletWaterTemp = state.dataWaterToAirHeatPumpSimple->SourceSideInletTemp -
                                                 simpleWatertoAirHP.QSource / (state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate * CpWater);
            simpleWatertoAirHP.OutletWaterEnthalpy = state.dataWaterToAirHeatPumpSimple->SourceSideInletEnth -
                                                     simpleWatertoAirHP.QSource / state.dataWaterToAirHeatPumpSimple->SourceSideMassFlowRate;
        }
    }

    void UpdateSimpleWatertoAirHP(EnergyPlusData &state, int const HPNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Arun Shenoy
        //       DATE WRITTEN   Jan 2004
        //       RE-ENGINEERED  Kenneth Tang (Jan 2005)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the Water to Air Heat Pump outlet nodes.

        // METHODOLOGY EMPLOYED:
        // Data is moved from the HP data structure to the HP outlet nodes.

        Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;
        int WaterInletNode;
        int AirOutletNode;
        int WaterOutletNode;

        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        if (!simpleWatertoAirHP.SimFlag) {
            // Heatpump is off; just pass through conditions
            simpleWatertoAirHP.Power = 0.0;
            simpleWatertoAirHP.QLoadTotal = 0.0;
            simpleWatertoAirHP.QLoadTotalReport = 0.0;
            simpleWatertoAirHP.QSensible = 0.0;
            simpleWatertoAirHP.QLatent = 0.0;
            simpleWatertoAirHP.QSource = 0.0;
            simpleWatertoAirHP.Energy = 0.0;
            simpleWatertoAirHP.EnergyLoadTotal = 0.0;
            simpleWatertoAirHP.EnergySensible = 0.0;
            simpleWatertoAirHP.EnergyLatent = 0.0;
            simpleWatertoAirHP.EnergySource = 0.0;
            simpleWatertoAirHP.COP = 0.0;
            simpleWatertoAirHP.RunFrac = 0.0;
            simpleWatertoAirHP.PartLoadRatio = 0.0;

            simpleWatertoAirHP.OutletAirDBTemp = simpleWatertoAirHP.InletAirDBTemp;
            simpleWatertoAirHP.OutletAirHumRat = simpleWatertoAirHP.InletAirHumRat;
            simpleWatertoAirHP.OutletAirEnthalpy = simpleWatertoAirHP.InletAirEnthalpy;
            simpleWatertoAirHP.OutletWaterTemp = simpleWatertoAirHP.InletWaterTemp;
            simpleWatertoAirHP.OutletWaterEnthalpy = simpleWatertoAirHP.InletWaterEnthalpy;
        }

        AirInletNode = simpleWatertoAirHP.AirInletNodeNum;
        WaterInletNode = simpleWatertoAirHP.WaterInletNodeNum;
        AirOutletNode = simpleWatertoAirHP.AirOutletNodeNum;
        WaterOutletNode = simpleWatertoAirHP.WaterOutletNodeNum;

        // Set the air outlet  nodes of the WatertoAirHPSimple
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
        state.dataLoopNodes->Node(AirOutletNode).Temp = simpleWatertoAirHP.OutletAirDBTemp;
        state.dataLoopNodes->Node(AirOutletNode).HumRat = simpleWatertoAirHP.OutletAirHumRat;
        state.dataLoopNodes->Node(AirOutletNode).Enthalpy = simpleWatertoAirHP.OutletAirEnthalpy;

        // Set the air outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(AirOutletNode).Quality = state.dataLoopNodes->Node(AirInletNode).Quality;
        state.dataLoopNodes->Node(AirOutletNode).Press = state.dataLoopNodes->Node(AirInletNode).Press;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail;

        // Set the water outlet node of the WatertoAirHPSimple
        // Set the water outlet nodes for properties that just pass through & not used
        PlantUtilities::SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);

        state.dataLoopNodes->Node(WaterOutletNode).Temp = simpleWatertoAirHP.OutletWaterTemp;
        state.dataLoopNodes->Node(WaterOutletNode).Enthalpy = simpleWatertoAirHP.OutletWaterEnthalpy;

        simpleWatertoAirHP.Energy = simpleWatertoAirHP.Power * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLoadTotal = simpleWatertoAirHP.QLoadTotal * TimeStepSysSec;
        simpleWatertoAirHP.EnergySensible = simpleWatertoAirHP.QSensible * TimeStepSysSec;
        simpleWatertoAirHP.EnergyLatent = simpleWatertoAirHP.QLatent * TimeStepSysSec;
        simpleWatertoAirHP.EnergySource = simpleWatertoAirHP.QSource * TimeStepSysSec;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(AirOutletNode).CO2 = state.dataLoopNodes->Node(AirInletNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(AirOutletNode).GenContam = state.dataLoopNodes->Node(AirInletNode).GenContam;
        }

        if (simpleWatertoAirHP.reportCoilFinalSizes) {
            if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingHVACSizingSimulations && !state.dataGlobal->DoingSizing) {

                if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Cooling) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                        state,
                        simpleWatertoAirHP.Name,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.RatedCapCoolTotal,
                        simpleWatertoAirHP.RatedCapCoolSens,
                        simpleWatertoAirHP.RatedAirVolFlowRate,
                        simpleWatertoAirHP.RatedWaterVolFlowRate);
                } else if (simpleWatertoAirHP.WAHPType == WatertoAirHP::Heating) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                        state,
                        simpleWatertoAirHP.Name,
                        format("COIL:{}:WATERTOAIRHEATPUMP:EQUATIONFIT", WatertoAirHPNamesUC[static_cast<int>(simpleWatertoAirHP.WAHPType)]),
                        simpleWatertoAirHP.RatedCapHeat,
                        simpleWatertoAirHP.RatedCapHeat,
                        simpleWatertoAirHP.RatedAirVolFlowRate,
                        simpleWatertoAirHP.RatedWaterVolFlowRate);
                }
                simpleWatertoAirHP.reportCoilFinalSizes = false;
            }
        }
    }

    //        End of Update subroutines for the WatertoAirHP Module
    // *****************************************************************************

    Real64 CalcEffectiveSHR(EnergyPlusData &state,
                            int const HPNum,         // Index number for cooling coil
                            Real64 const SHRss,      // Steady-state sensible heat ratio
                            int const CyclingScheme, // Fan/compressor cycling scheme indicator
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

        //    For cycling fan operation, a modified version of Henderson and Rengarajan (1996)
        //    model is used by ultilizing the fan delay time as the time-off (or time duration
        //    for the re-evaporation of moisture from time coil). Refer to Tang, C.C. (2005)

        // REFERENCES:
        //    (1) Henderson, H.I., K. Rengarajan.1996. A Model to Predict the Latent
        //    Capacity of Air Conditioners and Heat Pumps at Part-Load Conditions
        //    with Constant Fan Operation ASHRAE Transactions 102 (1), pp. 266-274.
        //    (2) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        //    State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        //    Oklahoma State University. (downloadable from www.hvac.okstate.edu)

        // Return value
        Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
        // at the current operating conditions (sec)
        Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
        // at the current operating conditions
        Real64 Twet_Rated;                 // Twet at rated conditions (coil air flow rate and air temperatures), sec
        Real64 Gamma_Rated;                // Gamma at rated conditions (coil air flow rate and air temperatures)
        Real64 Twet_max;                   // Maximum allowed value for Twet
        Real64 MaxONOFFCyclesperHour;      // Maximum cycling rate of heat pump [cycles/hr]
        Real64 LatentCapacityTimeConstant; // Latent capacity time constant [s]
        Real64 FanDelayTime;               // Fan delay time, time delay for the HP's fan to
        // shut off after compressor cycle off  [s]
        Real64 Ton;     // Coil on time (sec)
        Real64 Toff;    // Coil off time (sec)
        Real64 Toffa;   // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
        Real64 aa;      // Intermediate variable
        Real64 To1;     // Intermediate variable (first guess at To). To = time to the start of moisture removal
        Real64 To2;     // Intermediate variable (second guess at To). To = time to the start of moisture removal
        Real64 Error;   // Error for iteration (DO) loop
        Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

        auto &simpleWatertoAirHP(state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum));

        Twet_Rated = simpleWatertoAirHP.Twet_Rated;
        Gamma_Rated = simpleWatertoAirHP.Gamma_Rated;
        MaxONOFFCyclesperHour = simpleWatertoAirHP.MaxONOFFCyclesperHour;
        LatentCapacityTimeConstant = simpleWatertoAirHP.LatentCapacityTimeConstant;
        FanDelayTime = simpleWatertoAirHP.FanDelayTime;

        //  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
        //  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
        //  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
        if ((RTF >= 1.0) || (QLatRated == 0.0) || (QLatActual == 0.0) || (Twet_Rated <= 0.0) || (Gamma_Rated <= 0.0) ||
            (MaxONOFFCyclesperHour <= 0.0) || (LatentCapacityTimeConstant <= 0.0) || (RTF <= 0.0)) {
            SHReff = SHRss;
            return SHReff;
        }

        Twet_max = 9999.0; // high limit for Twet

        //  Calculate the model parameters at the actual operating conditions
        Twet = min(Twet_Rated * QLatRated / (QLatActual + 1.e-10), Twet_max);
        Gamma = Gamma_Rated * QLatRated * (EnteringDB - EnteringWB) / ((26.7 - 19.4) * QLatActual + 1.e-10);

        //  Calculate the compressor on and off times using a converntional thermostat curve
        Ton = 3600.0 / (4.0 * MaxONOFFCyclesperHour * (1.0 - RTF)); // duration of cooling coil on-cycle (sec)

        if ((CyclingScheme == DataHVACGlobals::CycFanCycCoil) && (FanDelayTime != 0.0)) {
            // For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            // until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
            Toff = FanDelayTime;
        } else {
            // For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
            // for the entire heat pump off-cycle.
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

        To1 = aa + LatentCapacityTimeConstant;
        Error = 1.0;
        while (Error > 0.001) {
            To2 = aa - LatentCapacityTimeConstant * (std::exp(-To1 / LatentCapacityTimeConstant) - 1.0);
            Error = std::abs((To2 - To1) / To1);
            To1 = To2;
        }

        //  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
        //  Floating underflow errors occur when -Ton/LatentCapacityTimeConstant is a large negative number.
        //  Cap lower limit at -700 to avoid the underflow errors.
        aa = std::exp(max(-700.0, -Ton / LatentCapacityTimeConstant));
        //  Calculate latent heat ratio multiplier
        LHRmult = max(((Ton - To2) / (Ton + LatentCapacityTimeConstant * (aa - 1.0))), 0.0);

        //  Calculate part-load or "effective" sensible heat ratio
        SHReff = 1.0 - (1.0 - SHRss) * LHRmult;

        if (SHReff < SHRss) SHReff = SHRss; // Effective SHR can be less than the steady-state SHR
        if (SHReff > 1.0) SHReff = 1.0;     // Effective sensible heat ratio can't be greater than 1.0

        return SHReff;
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil capacity for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
        // as zero.

        // Return value
        int IndexNum; // returned index of matched coil

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);

        if (IndexNum == 0) {
            ShowSevereError(state, format(R"(Could not find CoilType="{}" with Name="{}")", CoilType, CoilName));
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil capacity for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 CoilCapacity; // returned capacity of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT")) {
            WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);
            if (WhichCoil != 0) {
                if (UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT")) {
                    CoilCapacity = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedCapHeat;
                } else {
                    CoilCapacity = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedCapCoolTotal;

                    int companionHeatingCoil = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).CompanionHeatingCoilNum;
                    if (companionHeatingCoil > 0) {
                        if (CoilCapacity == DataSizing::AutoSize &&
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(companionHeatingCoil).WAHPPlantType ==
                                DataPlant::PlantEquipmentType::CoilWAHPHeatingEquationFit &&
                            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(companionHeatingCoil).RatedCapHeat == DataSizing::AutoSize &&
                            state.dataSize->DXCoolCap > 0) {
                            // Heating coil has not yet been sized, returning the temporary cooling capacity
                            CoilCapacity = state.dataSize->DXCoolCap;
                        }
                    }
                }
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format(R"(Could not find CoilType="{}" with Name="{}")", CoilType, CoilName));
            ErrorsFound = true;
            CoilCapacity = -1000.0;
        }

        return CoilCapacity;
    }

    Real64 GetCoilAirFlowRate(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   October 2011

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil air flow rate for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 CoilAirFlowRate; // returned air volume flow rate of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT")) {
            WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);
            if (WhichCoil != 0) {
                CoilAirFlowRate = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedAirVolFlowRate;
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format(R"(Could not find CoilType="{}" with Name="{}")", CoilType, CoilName));
            ErrorsFound = true;
            CoilAirFlowRate = -1000.0;
        }

        return CoilAirFlowRate;
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the inlet node.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Return value
        int NodeNumber; // returned outlet node of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).AirInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format(R"(Could not find CoilType="{}" with Name="{}")", CoilType, CoilName));
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the outlet node.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Return value
        int NodeNumber; // returned outlet node of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).AirOutletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format(R"(Could not find CoilType="{}" with Name="{}")", CoilType, CoilName));
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    void SetSimpleWSHPData(EnergyPlusData &state,
                           int const SimpleWSHPNum,                         // Number of OA Controller
                           bool &ErrorsFound,                               // Set to true if certain errors found
                           int const WaterCyclingMode,                      // the coil water flow mode (cycling, constant or constantondemand)
                           ObjexxFCL::Optional_int CompanionCoolingCoilNum, // Index to cooling coil for heating coil = SimpleWSHPNum
                           ObjexxFCL::Optional_int CompanionHeatingCoilNum  // Index to heating coil for cooling coil = SimpleWSHPNum
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   June 2009

        // PURPOSE OF THIS SUBROUTINE:
        // This routine was designed to "push" information from a parent object to
        // this WSHP coil object.

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag) { // First time subroutine has been entered
            GetSimpleWatertoAirHPInput(state);
            state.dataWaterToAirHeatPumpSimple->GetCoilsInputFlag = false;
        }

        if (SimpleWSHPNum <= 0 || SimpleWSHPNum > state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs) {
            ShowSevereError(state,
                            format("SetSimpleWSHPData: called with WSHP Coil Number out of range={} should be >0 and <{}",
                                   SimpleWSHPNum,
                                   state.dataWaterToAirHeatPumpSimple->NumWatertoAirHPs));
            ErrorsFound = true;
            return;
        }

        state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(SimpleWSHPNum).WaterCyclingMode = WaterCyclingMode;
        if (present(CompanionCoolingCoilNum)) {
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(SimpleWSHPNum).CompanionCoolingCoilNum = CompanionCoolingCoilNum;
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(CompanionCoolingCoilNum).CompanionHeatingCoilNum = SimpleWSHPNum;
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(CompanionCoolingCoilNum).WaterCyclingMode = WaterCyclingMode;
        }

        if (present(CompanionHeatingCoilNum)) {
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(SimpleWSHPNum).CompanionHeatingCoilNum = CompanionHeatingCoilNum;
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(CompanionHeatingCoilNum).CompanionCoolingCoilNum = SimpleWSHPNum;
            state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(CompanionHeatingCoilNum).WaterCyclingMode = WaterCyclingMode;
        }
    }

    void CheckSimpleWAHPRatedCurvesOutputs(EnergyPlusData &state, std::string const &CoilName)
    {
        int WhichCoil;
        constexpr Real64 Tref(283.15); // Refrence Temperature for performance curves,10C [K]
        static constexpr std::string_view RoutineName("CheckSimpleWAHPRatedCurvesOutputs");

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP);

        if (WhichCoil != 0) {
            if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).WAHPType == WatertoAirHP::Cooling) {
                int TotalCoolCapCurveIndex = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).TotalCoolCapCurveIndex;
                int CoolPowCurveIndex = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).CoolPowCurveIndex;
                int SensCoolCapCurveIndex = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).SensCoolCapCurveIndex;
                if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirWetbulbTemp != DataSizing::AutoSize) {
                    Real64 RatedratioTWB = (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirWetbulbTemp +
                                            state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) /
                                           Tref;
                    Real64 RatedratioTS = (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntWaterTemp +
                                           state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) /
                                          Tref;
                    Real64 RatedTotCapTempModFac = Curve::CurveValue(state, TotalCoolCapCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                    Real64 RatedCoolPowerTempModFac = Curve::CurveValue(state, CoolPowCurveIndex, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                    if (RatedTotCapTempModFac > 1.02 || RatedTotCapTempModFac < 0.98) {
                        ShowWarningError(state,
                                         format("{}: Coil:Cooling:WaterToAirHeatPump:EquationFit=\"{}\"",
                                                std::string{RoutineName},
                                                state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).Name));
                        ShowContinueError(state,
                                          "Total cooling capacity as a function of temperature curve output is not equal to 1.0 (+ or - 2%) "
                                          "at rated conditions.");
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedTotCapTempModFac));
                    }
                    if (RatedCoolPowerTempModFac > 1.02 || RatedCoolPowerTempModFac < 0.98) {
                        ShowWarningError(state,
                                         format("{}: Coil:Cooling:WaterToAirHeatPump:EquationFit=\"{}\"",
                                                std::string{RoutineName},
                                                state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).Name));
                        ShowContinueError(state,
                                          "Cooling power consumption as a function of temperature curve output is not equal to 1.0 (+ or - 2%) "
                                          "at rated conditions.");
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedCoolPowerTempModFac));
                    }
                    if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirDrybulbTemp != DataSizing::AutoSize) {
                        Real64 RatedratioTDB = (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirDrybulbTemp +
                                                state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) /
                                               Tref;
                        Real64 RatedSensCapTempModFac =
                            Curve::CurveValue(state, SensCoolCapCurveIndex, RatedratioTDB, RatedratioTWB, RatedratioTS, 1.0, 1.0);
                        if (RatedSensCapTempModFac > 1.02 || RatedSensCapTempModFac < 0.98) {
                            ShowWarningError(state,
                                             format("{}: Coil:Cooling:WaterToAirHeatPump:EquationFit=\"{}\"",
                                                    std::string{RoutineName},
                                                    state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).Name));
                            ShowContinueError(state,
                                              "Sensible cooling capacity as a function of temperature curve output is not equal to 1.0 (+ or - 2%) "
                                              "at rated conditions.");
                            ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedSensCapTempModFac));
                        }
                    }
                }
            } else if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).WAHPType == WatertoAirHP::Heating) {
                if (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirDrybulbTemp != DataSizing::AutoSize) {
                    int HeatCapCurveIndex = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).HeatCapCurveIndex;
                    int HeatPowCurveIndex = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).HeatPowCurveIndex;
                    Real64 RatedHeatratioTDB = (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntAirDrybulbTemp +
                                                state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) /
                                               Tref;
                    Real64 RatedHeatratioTS = (state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).RatedEntWaterTemp +
                                               state.dataWaterToAirHeatPumpSimple->CelsiustoKelvin) /
                                              Tref;
                    Real64 RatedHeatCapTempModFac = Curve::CurveValue(state, HeatCapCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                    Real64 RatedHeatPowerTempModFac = Curve::CurveValue(state, HeatPowCurveIndex, RatedHeatratioTDB, RatedHeatratioTS, 1.0, 1.0);
                    if (RatedHeatCapTempModFac > 1.02 || RatedHeatCapTempModFac < 0.98) {
                        ShowWarningError(state,
                                         format("{}: Coil:Heating:WaterToAirHeatPump:EquationFit=\"{}\"",
                                                std::string{RoutineName},
                                                state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).Name));
                        ShowContinueError(
                            state, "Heating capacity as a function of temperature curve output is not equal to 1.0 (+ or - 2%) at rated conditions.");
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedHeatCapTempModFac));
                    }
                    if (RatedHeatPowerTempModFac > 1.02 || RatedHeatPowerTempModFac < 0.98) {
                        ShowWarningError(state,
                                         format("{}: Coil:Heating:WaterToAirHeatPump:EquationFit=\"{}\"",
                                                std::string{RoutineName},
                                                state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(WhichCoil).Name));
                        ShowContinueError(state,
                                          "Heating power consumption as a function of temperature curve output is not equal to 1.0 (+ or - 2%) at "
                                          "rated conditions.");
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", RatedHeatPowerTempModFac));
                    }
                }
            }
        }
    }
} // namespace WaterToAirHeatPumpSimple

} // namespace EnergyPlus
