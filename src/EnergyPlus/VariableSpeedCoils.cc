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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus {

namespace VariableSpeedCoils {

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace Psychrometrics;
    using namespace DataSizing;
    using namespace DataHVACGlobals;
    using DataPlant::TypeOf_CoilVSWAHPCoolingEquationFit;
    using DataPlant::TypeOf_CoilVSWAHPHeatingEquationFit;
    using DXCoils::AdjustCBF;
    using DXCoils::CalcCBF;

    void SimVariableSpeedCoils(EnergyPlusData &state,
                               std::string_view CompName,     // Coil Name
                               int &CompIndex,                // Index for Component name
                               int const CyclingScheme,       // Continuous fan OR cycling compressor
                               Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                               Real64 &HPTimeConstant,        // Heat pump time constant [s]
                               Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                               int const CompOp,              // compressor on/off. 0 = off; 1= on
                               Real64 const PartLoadFrac,
                               int const SpeedNum,                    // compressor speed number
                               Real64 const SpeedRatio,               // compressor speed ratio
                               Real64 const SensLoad,                 // Sensible demand load [W]
                               Real64 const LatentLoad,               // Latent demand load [W]
                               Optional<Real64 const> OnOffAirFlowRat // ratio of comp on to comp off air flow rate
    )
    {

        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   March 2012
        //       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages variable-speed Water to Air Heat Pump component simulation.

        // Using/Aliasing
        using FluidProperties::FindGlycol;
        using General::SolveRoot;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum;            // The WatertoAirHP that you are currently loading input into
        Real64 OnOffAirFlowRatio; // ratio of comp on to comp off air flow rate
        Real64 RuntimeFrac;       // run time fraction
        int SpeedCal;             // variable for error proof speed input

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (CompIndex == 0) {
            DXCoilNum = UtilityRoutines::FindItemInList(CompName, state.dataVariableSpeedCoils->VarSpeedCoil);
            if (DXCoilNum == 0) {
                ShowFatalError(state, "WaterToAirHPVSWEquationFit not found=" + std::string{CompName});
            }
            CompIndex = DXCoilNum;
        } else {
            DXCoilNum = CompIndex;
            if (DXCoilNum > state.dataVariableSpeedCoils->NumVarSpeedCoils || DXCoilNum < 1) {
                ShowFatalError(state,
                               format("SimVariableSpeedCoils: Invalid CompIndex passed={}, Number of Water to Air HPs={}, WaterToAir HP name={}",
                                      DXCoilNum,
                                      state.dataVariableSpeedCoils->NumVarSpeedCoils,
                                      CompName));
            }
            if (!CompName.empty() && CompName != state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name) {
                ShowFatalError(
                    state,
                    format("SimVariableSpeedCoils: Invalid CompIndex passed={}, WaterToAir HP name={}, stored WaterToAir HP Name for that index={}",
                           DXCoilNum,
                           CompName,
                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name));
            }
        }

        if (present(OnOffAirFlowRat)) {
            OnOffAirFlowRatio = OnOffAirFlowRat;
        } else {
            OnOffAirFlowRatio = 1.0;
        }

        // ERROR PROOF
        if (SpeedNum < 1) {
            SpeedCal = 1;
        } else {
            SpeedCal = SpeedNum;
        }

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed)) {
            // Cooling mode
            InitVarSpeedCoil(state,
                             DXCoilNum,
                             MaxONOFFCyclesperHour,
                             HPTimeConstant,
                             FanDelayTime,
                             SensLoad,
                             LatentLoad,
                             CyclingScheme,
                             OnOffAirFlowRatio,
                             SpeedRatio,
                             SpeedCal);
            CalcVarSpeedCoilCooling(
                state, DXCoilNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadFrac, OnOffAirFlowRatio, SpeedRatio, SpeedCal);
            UpdateVarSpeedCoil(state, DXCoilNum);
        } else if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                    DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                   (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed)) {
            // Heating mode
            InitVarSpeedCoil(state,
                             DXCoilNum,
                             MaxONOFFCyclesperHour,
                             HPTimeConstant,
                             FanDelayTime,
                             SensLoad,
                             LatentLoad,
                             CyclingScheme,
                             OnOffAirFlowRatio,
                             SpeedRatio,
                             SpeedCal);
            CalcVarSpeedCoilHeating(
                state, DXCoilNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadFrac, OnOffAirFlowRatio, SpeedRatio, SpeedCal);
            UpdateVarSpeedCoil(state, DXCoilNum);
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
            // Heating mode
            InitVarSpeedCoil(state,
                             DXCoilNum,
                             MaxONOFFCyclesperHour,
                             HPTimeConstant,
                             FanDelayTime,
                             SensLoad,
                             LatentLoad,
                             CyclingScheme,
                             OnOffAirFlowRatio,
                             SpeedRatio,
                             SpeedCal);
            CalcVarSpeedHPWH(state, DXCoilNum, RuntimeFrac, PartLoadFrac, SpeedRatio, SpeedNum, CyclingScheme);
            UpdateVarSpeedCoil(state, DXCoilNum);
        } else {
            ShowFatalError(state, "SimVariableSpeedCoils: WatertoAir heatpump not in either HEATING or COOLING mode");
        }

        // two additional output variables
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport = SpeedCal;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport = SpeedRatio;
    }

    void GetVarSpeedCoilInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for HPs and stores it in HP data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using namespace NodeInputManager;
        using BranchNodeConnections::TestCompSet;
        using GlobalNames::VerifyUniqueCoilName;
        using namespace OutputReportPredefined;
        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;
        using CurveManager::SetCurveOutputMinMaxValues;

        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;
        using WaterManager::SetupTankDemandComponent;
        using WaterManager::SetupTankSupplyComponent;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetVarSpeedCoilInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoilNum;         // The Water to Air HP that you are currently loading input into
        int NumCool;           // Counter for cooling coil, water source
        int NumCoolAS;         // Counter for cooling coil, air source
        int NumHeat;           // Counter for heating coil, water source
        int NumHeatAS;         // Counter for heating coil, air source
        int NumHPWHAirToWater; // counter for air source HPWH
        int CoilCounter;       // Counter
        int I;                 // Loop index increment
        int NumAlphas;         // Number of variables in String format
        int NumNums;           // Number of variables in Numeric format
        int NumParams;         // Total number of input fields
        int MaxNums(0);        // Maximum number of numeric input fields
        int MaxAlphas(0);      // Maximum number of alpha input fields
        int IOStat;
        int AlfaFieldIncre;              // increment number of Alfa field
        bool ErrorsFound(false);         // If errors detected in input
        Real64 CurveVal;                 // Used to verify modifier curves equal 1 at rated conditions
        Real64 WHInletAirTemp;           // Used to pass proper inlet air temp to HPWH DX coil performance curves
        Real64 WHInletWaterTemp;         // Used to pass proper inlet water temp to HPWH DX coil performance curves
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string AlphArray;        // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> NumArray;        // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

        NumCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT");
        NumHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT");
        NumCoolAS = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COIL:COOLING:DX:VARIABLESPEED");
        NumHeatAS = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COIL:HEATING:DX:VARIABLESPEED");
        NumHPWHAirToWater =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED");
        state.dataVariableSpeedCoils->NumVarSpeedCoils = NumCool + NumHeat + NumCoolAS + NumHeatAS + NumHPWHAirToWater;
        DXCoilNum = 0;

        if (state.dataVariableSpeedCoils->NumVarSpeedCoils <= 0) {
            ShowSevereError(state, "No Equipment found in GetVarSpeedCoilInput");
            ErrorsFound = true;
        }

        // Allocate Arrays
        if (state.dataVariableSpeedCoils->NumVarSpeedCoils > 0) {
            state.dataVariableSpeedCoils->VarSpeedCoil.allocate(state.dataVariableSpeedCoils->NumVarSpeedCoils);
            state.dataHeatBal->HeatReclaimVS_DXCoil.allocate(state.dataVariableSpeedCoils->NumVarSpeedCoils);
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "COIL:COOLING:DX:VARIABLESPEED", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "COIL:HEATING:DX:VARIABLESPEED", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        // variable speed air-source HPWH
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", NumParams, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        lAlphaBlanks.dimension(MaxAlphas, true);
        cNumericFields.allocate(MaxNums);
        lNumericBlanks.dimension(MaxNums, true);
        NumArray.dimension(MaxNums, 0.0);

        // Get the data for cooling coil, WATER SOURCE
        CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit"; // for reporting

        for (CoilCounter = 1; CoilCounter <= NumCool; ++CoilCounter) {

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

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType = "COOLING";
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType =
                DataHVACGlobals::cAllCoilTypes(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = NumArray(3);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(4);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate = NumArray(5);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Twet_Rated = NumArray(6);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Gamma_Rated = NumArray(7);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HOTGASREHEATFLG = int(NumArray(8));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType = DataHeatBalance::RefrigCondenserType::Water;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                                        AlphArray(2),
                                                                                                        ErrorsFound,
                                                                                                        CurrentModuleObject,
                                                                                                        AlphArray(1),
                                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                                        ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                                         AlphArray(3),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphArray(1),
                                                                                                         DataLoopNode::NodeFluidType::Water,
                                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                                         NodeInputManager::compFluidStream::Secondary,
                                                                                                         ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                      AlphArray(4),
                                                                                                      ErrorsFound,
                                                                                                      CurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                                      ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                       AlphArray(5),
                                                                                                       ErrorsFound,
                                                                                                       CurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                                       ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Air Nodes");

            //   If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 1. entered number is {:.0T}", cNumericFields(1), NumArray(1)));
                ErrorsFound = true;
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
            }

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be valid speed level entered number is {:.0T}", cNumericFields(2), NumArray(2)));
                ErrorsFound = true;
            }

            // part load curve
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                    ShowContinueError(state, "...required " + cAlphaFields(6) + " is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(6) + "=\"" + AlphArray(6) + "\".");
                }
                ErrorsFound = true;
            } else {
                CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                    ShowContinueError(state, "..." + cAlphaFields(6) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                    ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(9 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(I) = NumArray(10 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(11 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(12 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) = NumArray(13 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(I) = NumArray(14 + (I - 1) * 6);

                AlfaFieldIncre = 7 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                              state.dataVariableSpeedCoils->RatedInletWaterTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 8 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), // Curve index
                                                     {1},                                                                     // Valid dimensions
                                                     RoutineName,                                                             // Routine name
                                                     CurrentModuleObject,                                                     // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,              // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                           // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 9 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), // Curve index
                                                     {1},                                                                       // Valid dimensions
                                                     RoutineName,                                                               // Routine name
                                                     CurrentModuleObject,                                                       // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,                // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                             // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 10 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                              state.dataVariableSpeedCoils->RatedInletWaterTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 11 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), // Curve index
                                                                {1},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 12 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), // Curve index
                                                     {1},                                                                      // Valid dimensions
                                                     RoutineName,                                                              // Routine name
                                                     CurrentModuleObject,                                                      // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,               // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                            // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 13 + (I - 1) * 7;
                // Read waste heat modifier curve name
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal types are BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I),
                                              state.dataVariableSpeedCoils->RatedInletWaterTemp,
                                              state.dataVariableSpeedCoils->RatedInletAirTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
            }

            // CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "PLANTLOOPCOOLINGDEMAND",
                                "COOLINGCOILS",
                                _,
                                "System");

            // for table output, being consistent with outher water-to-air coils
            //        IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= AutoSize) THEN
            //            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
            //                *VarSpeedCoil(DXCoilNum)%MSRatedSHR(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
            //        ELSE
            //            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE
            //        END IF

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens =
                AutoSize; // always auto-sized, to be determined in the sizing calculation

            // BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
            // create predefined report entries
            // PreDefTableEntry(state,  pdchCoolCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
            // PreDefTableEntry(state,  pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
            // PreDefTableEntry(state,  pdchCoolCoilSensCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
            // PreDefTableEntry(state,  pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal -
            // VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );  PreDefTableEntry(state,  pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil(
            // DXCoilNum
            // ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );  PreDefTableEntry(state,  pdchCoolCoilNomEff, VarSpeedCoil(
            // DXCoilNum
            // ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );
        }

        //-------------------------AIR SOURCE, COOLING---BEGIN
        // Get the data for cooling coil, AIR SOURCE
        CurrentModuleObject = "Coil:Cooling:DX:VariableSpeed"; // for reporting

        for (CoilCounter = 1; CoilCounter <= NumCoolAS; ++CoilCounter) {

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

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
            // Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
            state.dataHeatBal->HeatReclaimVS_DXCoil(DXCoilNum).Name = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name;
            state.dataHeatBal->HeatReclaimVS_DXCoil(DXCoilNum).SourceType = CurrentModuleObject;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType = "COOLING";
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = Coil_CoolingAirToAirVariableSpeed;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType =
                DataHVACGlobals::cAllCoilTypes(Coil_CoolingAirToAirVariableSpeed);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = NumArray(3);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(4);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Twet_Rated = NumArray(5);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Gamma_Rated = NumArray(6);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                      AlphArray(2),
                                                                                                      ErrorsFound,
                                                                                                      CurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                                      ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                       AlphArray(3),
                                                                                                       ErrorsFound,
                                                                                                       CurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                                       ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Air Nodes");

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 1. entered number is {:.0T}", cNumericFields(1), NumArray(1)));
                ErrorsFound = true;
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
            }

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be valid speed level entered number is {:.0T}", cNumericFields(2), NumArray(2)));
                ErrorsFound = true;
            }

            // part load curve
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(state, AlphArray(4)); // convert curve name to number
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
                if (lAlphaBlanks(4)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                    ShowContinueError(state, "...required " + cAlphaFields(6) + " is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(4) + "=\"" + AlphArray(4) + "\".");
                }
                ErrorsFound = true;
            } else {
                CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                    ShowContinueError(state, "..." + cAlphaFields(4) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                    ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                }
            }

            // outdoor condenser node
            if (lAlphaBlanks(5)) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum = 0;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum =
                    GetOnlySingleNode(state,
                                      AlphArray(5),
                                      ErrorsFound,
                                      CurrentModuleObject,
                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                      DataLoopNode::NodeFluidType::Air,
                                      DataLoopNode::NodeConnectionType::OutsideAirReference,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);

                if (!CheckOutAirNodeNumber(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum)) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", may be invalid");
                    ShowContinueError(state,
                                      cAlphaFields(10) + "=\"" + AlphArray(5) +
                                          "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ShowContinueError(
                        state, "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues");
                }
            }

            if ((UtilityRoutines::SameString(AlphArray(6), "AirCooled")) || lAlphaBlanks(6)) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType = DataHeatBalance::RefrigCondenserType::Air;
            } else if (UtilityRoutines::SameString(AlphArray(6), "EvaporativelyCooled")) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType = DataHeatBalance::RefrigCondenserType::Evap;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).ReportEvapCondVars = true;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cAlphaFields(6) + "=\"" + AlphArray(6) + "\":");
                ShowContinueError(state, "...must be AirCooled or EvaporativelyCooled.");
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower = NumArray(7);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cNumericFields(7) + " cannot be < 0.0.");
                ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(7)));
                ErrorsFound = true;
            }

            // Set crankcase heater capacity
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity = NumArray(8);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cNumericFields(8) + " cannot be < 0.0.");
                ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(8)));
                ErrorsFound = true;
            }

            // Set crankcase heater cutout temperature
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater = NumArray(9);

            // Set crankcase heater cutout temperature
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MinOATCompressor = NumArray(10);

            // Get Water System tank connections
            //  A7, \field Name of Water Storage Tank for Supply
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupplyName = AlphArray(7);
            if (lAlphaBlanks(7)) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupplyMode = state.dataVariableSpeedCoils->WaterSupplyFromMains;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupplyMode = state.dataVariableSpeedCoils->WaterSupplyFromTank;
                SetupTankDemandComponent(state,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                         CurrentModuleObject,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupplyName,
                                         ErrorsFound,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupTankID,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterTankDemandARRID);
            }

            // A8; \field Name of Water Storage Tank for Condensate Collection
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectName = AlphArray(8);
            if (lAlphaBlanks(8)) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectMode = state.dataVariableSpeedCoils->CondensateDiscarded;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectMode = state.dataVariableSpeedCoils->CondensateToTank;
                SetupTankSupplyComponent(state,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                         CurrentModuleObject,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectName,
                                         ErrorsFound,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateTankID,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateTankSupplyARRID);
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPowerFTempDiff = NumArray(11);
            if (NumArray(11) < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cNumericFields(11) + " must be >= 0.0.");
                ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(11)));
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSetPointTemp = NumArray(12);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", freeze possible");
                    ShowContinueError(state, "..." + cNumericFields(12) + " is < 2 {C}. Freezing could occur.");
                    ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(12)));
                }
            }

            if (!lAlphaBlanks(9)) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSchedulePtr = GetScheduleIndex(state, AlphArray(9));
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(14) + "=\"" + AlphArray(9) + "\".");
                    ShowContinueError(state, "Basin heater will be available to operate throughout the simulation.");
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(13 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(I) = NumArray(14 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(15 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(16 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(I) = NumArray(17 + (I - 1) * 6);

                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(I) = NumArray(18 + (I - 1) * 6);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(I) < 0.0 ||
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(I) > 1.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "..." + cNumericFields(18 + (I - 1) * 6) + " cannot be < 0.0 or > 1.0.");
                    ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(18 + (I - 1) * 6)));
                    ErrorsFound = true;
                }

                AlfaFieldIncre = 10 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                              state.dataVariableSpeedCoils->RatedAmbAirTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 11 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), // Curve index
                                                     {1},                                                                     // Valid dimensions
                                                     RoutineName,                                                             // Routine name
                                                     CurrentModuleObject,                                                     // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,              // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                           // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 12 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                              state.dataVariableSpeedCoils->RatedAmbAirTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 13 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), // Curve index
                                                                {1},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedEvapCondVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
            }

            // CurrentModuleObject = "Coil:Cooling:DX:VariableSpeed"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens =
                AutoSize; // always auto-sized, to be determined in the sizing calculation

            // BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
            // create predefined report entries
            // PreDefTableEntry(state,  pdchCoolCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
            // PreDefTableEntry(state,  pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
            // PreDefTableEntry(state,  pdchCoolCoilSensCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
            // PreDefTableEntry(state,  pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal -
            // VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );  PreDefTableEntry(state,  pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil(
            // DXCoilNum
            // ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );  PreDefTableEntry(state,  pdchCoolCoilNomEff, VarSpeedCoil(
            // DXCoilNum
            // ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );
        }

        //-------------------------AIR SOURCE COOLING---END

        // Get the data for heating coil, WATER SOURCE
        CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit";

        for (CoilCounter = 1; CoilCounter <= NumHeat; ++CoilCounter) {

            ++DXCoilNum;

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

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType = "HEATING";
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = Coil_HeatingWaterToAirHPVSEquationFit; // fix coil type

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType =
                DataHVACGlobals::cAllCoilTypes(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat = NumArray(3);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(4);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate = NumArray(5);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType = DataHeatBalance::RefrigCondenserType::Water;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                                        AlphArray(2),
                                                                                                        ErrorsFound,
                                                                                                        CurrentModuleObject,
                                                                                                        AlphArray(1),
                                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                                        ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                                         AlphArray(3),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphArray(1),
                                                                                                         DataLoopNode::NodeFluidType::Water,
                                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                                         NodeInputManager::compFluidStream::Secondary,
                                                                                                         ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                      AlphArray(4),
                                                                                                      ErrorsFound,
                                                                                                      CurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                                      ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                       AlphArray(5),
                                                                                                       ErrorsFound,
                                                                                                       CurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                                       ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Water Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Air Nodes");

            //       If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 1. entered number is {:.0T}", cNumericFields(1), NumArray(1)));
                ErrorsFound = true;
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
            }

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be valid speed level entered number is {:.0T}", cNumericFields(2), NumArray(2)));
                ErrorsFound = true;
            }

            // part load curve
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                    ShowContinueError(state, "...required " + cAlphaFields(6) + " is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(6) + "=\"" + AlphArray(6) + "\".");
                }
                ErrorsFound = true;
            } else {
                CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                    ShowContinueError(state, "..." + cAlphaFields(6) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                    ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(6 + (I - 1) * 5);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(7 + (I - 1) * 5);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(8 + (I - 1) * 5);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) = NumArray(9 + (I - 1) * 5);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(I) = NumArray(10 + (I - 1) * 5);

                AlfaFieldIncre = 7 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                              state.dataVariableSpeedCoils->RatedInletWaterTempHeat);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 8 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), // Curve index
                                                     {1},                                                                     // Valid dimensions
                                                     RoutineName,                                                             // Routine name
                                                     CurrentModuleObject,                                                     // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,              // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                           // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 9 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(14 + (I - 1) * 6) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), // Curve index
                                                     {1},                                                                       // Valid dimensions
                                                     RoutineName,                                                               // Routine name
                                                     CurrentModuleObject,                                                       // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,                // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                             // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 10 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                              state.dataVariableSpeedCoils->RatedInletWaterTempHeat);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 11 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(16 + (I - 1) * 6) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), // Curve index
                                                                {1},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 12 + (I - 1) * 7;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), // Curve index
                                                     {1},                                                                      // Valid dimensions
                                                     RoutineName,                                                              // Routine name
                                                     CurrentModuleObject,                                                      // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,               // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                            // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 13 + (I - 1) * 7;
                // Read waste heat modifier curve name
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal types are BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(I),
                                              state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                              state.dataVariableSpeedCoils->RatedInletWaterTempHeat);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
            }

            // CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit"
            SetupOutputVariable(state,
                                "Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                _,
                                "System");

            // create predefined report entries
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchHeatCoilType, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, CurrentModuleObject);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomEff,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel));
        }

        //-------------------------AIR SOURCE, HEATING---BEGIN
        // Get the data for heating coil, AIR SOURCE
        CurrentModuleObject = "COIL:HEATING:DX:VARIABLESPEED";

        for (CoilCounter = 1; CoilCounter <= NumHeatAS; ++CoilCounter) {

            ++DXCoilNum;

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

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType = "HEATING";
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = Coil_HeatingAirToAirVariableSpeed;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType =
                DataHVACGlobals::cAllCoilTypes(Coil_HeatingAirToAirVariableSpeed);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat = NumArray(3);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(4);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                      AlphArray(2),
                                                                                                      ErrorsFound,
                                                                                                      CurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                                      ObjectIsNotParent);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                       AlphArray(3),
                                                                                                       ErrorsFound,
                                                                                                       CurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                                       ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Air Nodes");

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 1. entered number is {:.0T}", cNumericFields(1), NumArray(1)));
                ErrorsFound = true;
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
            }

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be valid speed level entered number is {:.0T}", cNumericFields(2), NumArray(2)));
                ErrorsFound = true;
            }

            // part load curve
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(state, AlphArray(4)); // convert curve name to number
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
                if (lAlphaBlanks(4)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                    ShowContinueError(state, "...required " + cAlphaFields(4) + " is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(4) + "=\"" + AlphArray(4) + "\".");
                }
                ErrorsFound = true;
            } else {
                CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                    ShowContinueError(state, "..." + cAlphaFields(4) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                    ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                }
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostEIRFT = GetCurveIndex(state, AlphArray(5)); // convert curve name to number

            if (UtilityRoutines::SameString(AlphArray(6), "ReverseCycle")) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostEIRFT == 0) {
                    if (lAlphaBlanks(5)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(5) + " is blank.");
                        ShowContinueError(state, "...field is required because " + cAlphaFields(6) + " is \"ReverseCycle\".");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(5) + "=\"" + AlphArray(5) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostEIRFT, // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name
                }
            }

            if (UtilityRoutines::SameString(AlphArray(6), "ReverseCycle"))
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy = state.dataVariableSpeedCoils->ReverseCycle;
            if (UtilityRoutines::SameString(AlphArray(6), "Resistive"))
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy = state.dataVariableSpeedCoils->Resistive;
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "...illegal " + cAlphaFields(6) + "=\"" + AlphArray(6) + "\".");
                ShowContinueError(state, "...valid values for this field are ReverseCycle or Resistive.");
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(AlphArray(7), "Timed"))
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostControl = state.dataVariableSpeedCoils->Timed;
            if (UtilityRoutines::SameString(AlphArray(7), "OnDemand"))
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostControl = state.dataVariableSpeedCoils->OnDemand;
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostControl == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "...illegal " + cAlphaFields(7) + "=\"" + AlphArray(7) + "\".");
                ShowContinueError(state, "...valid values for this field are Timed or OnDemand.");
                ErrorsFound = true;
            }

            // Set minimum OAT for heat pump compressor operation
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MinOATCompressor = NumArray(5);

            // reserved for HSPF calculation
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OATempCompressorOn = NumArray(6);

            // Set maximum outdoor temp for defrost to occur
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATDefrost = NumArray(7);

            // Set crankcase heater capacity
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity = NumArray(8);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cNumericFields(9) + " cannot be < 0.0.");
                ShowContinueError(state, format("...entered value=[{:.2T}].", NumArray(9)));
                ErrorsFound = true;
            }

            // Set crankcase heater cutout temperature
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater = NumArray(9);

            // Set defrost time period
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostTime = NumArray(10);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostTime == 0.0 &&
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostControl == 1) {
                ShowWarningError(state,
                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                     "\", ");
                ShowContinueError(state, "..." + cNumericFields(5) + " = 0.0 for defrost control = TIMED.");
            }

            // Set defrost capacity (for resistive defrost)
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity = NumArray(11);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity == 0.0 &&
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy == 2) {
                ShowWarningError(state,
                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                     "\", ");
                ShowContinueError(state, "..." + cNumericFields(6) + " = 0.0 for defrost strategy = RESISTIVE.");
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(12 + (I - 1) * 3);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(13 + (I - 1) * 3);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(14 + (I - 1) * 3);

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) < 1.e-10) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid value");
                    ShowContinueError(state,
                                      format("...too small {}=[{:.2R}].",
                                             cNumericFields(12 + (I - 1) * 3),
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I)));
                    ErrorsFound = true;
                }

                AlfaFieldIncre = 8 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                              state.dataVariableSpeedCoils->RatedAmbAirTempHeat);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 9 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), // Curve index
                                                     {1},                                                                     // Valid dimensions
                                                     RoutineName,                                                             // Routine name
                                                     CurrentModuleObject,                                                     // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,              // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                           // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 10 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I),
                                              state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                              state.dataVariableSpeedCoils->RatedAmbAirTempHeat);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 11 + (I - 1) * 4;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), // Curve index
                                                                {1},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }
            }

            if (ErrorsFound) continue;

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
            }

            // CurrentModuleObject = "Coil:Heating:DX:Variablespeed "
            SetupOutputVariable(state,
                                "Heating Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

            // create predefined report entries
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchHeatCoilType, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, CurrentModuleObject);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomEff,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel));
        }

        //-------------------------AIR SOURCE HEATING---END

        //------------------------VARIABLE-SPEED AIR SOURCE HPWH---BEGIN
        CurrentModuleObject = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED"; // for reporting

        for (CoilCounter = 1; CoilCounter <= NumHPWHAirToWater; ++CoilCounter) {

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

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType = DataHeatBalance::RefrigCondenserType::WaterHeater;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType = "WATERHEATING";
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = CoilDX_HeatPumpWaterHeaterVariableSpeed;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType = cAllCoilTypes(CoilDX_HeatPumpWaterHeaterVariableSpeed);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 1. entered number is {:.0T}", cNumericFields(1), NumArray(1)));
                ErrorsFound = true;
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
            }

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel >
                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be valid speed level entered number is {:.0T}", cNumericFields(2), NumArray(2)));
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH = NumArray(3);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH <= 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be > 0.0, entered value=[{:.2T}].", cNumericFields(3), NumArray(3)));
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp = NumArray(4);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp = NumArray(5);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp = NumArray(6);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(7);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate = NumArray(8);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate != DataGlobalConstants::AutoCalculate) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate <= 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, format("...{} must be > 0.0.  entered value=[{:.3T}].", cNumericFields(7), NumArray(7)));
                    ErrorsFound = true;
                }
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate != DataGlobalConstants::AutoCalculate) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate <= 0.0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, format("...{} must be > 0.0  entered value=[{:.3T}].", cNumericFields(8), NumArray(8)));
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(AlphArray(2), "Yes") || UtilityRoutines::SameString(AlphArray(2), "No")) {
                //  initialized to TRUE on allocate
                if (UtilityRoutines::SameString(AlphArray(2), "No"))
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP = false;
                else
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP = true;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, ",,,invalid choice for " + cAlphaFields(2) + ".  Entered choice = " + AlphArray(2));
                ShowContinueError(state, "Valid choices are Yes or No.");
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(AlphArray(3), "Yes") || UtilityRoutines::SameString(AlphArray(3), "No")) {
                //  initialized to FALSE on allocate
                if (UtilityRoutines::SameString(AlphArray(3), "Yes"))
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP = true;
                else
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP = false;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, ",,,invalid choice for " + cAlphaFields(3) + ".  Entered choice = " + AlphArray(3));
                ShowContinueError(state, "Valid choices are Yes or No.");
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(AlphArray(4), "Yes") || UtilityRoutines::SameString(AlphArray(4), "No")) {
                //  initialized to FALSE on allocate
                if (UtilityRoutines::SameString(AlphArray(4), "Yes"))
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity = true;
                else
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity = false;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, ",,,invalid choice for " + cAlphaFields(4) + ".  Entered choice = " + AlphArray(4));
                ShowContinueError(state, "Valid choices are Yes or No.");
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater = NumArray(9);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater <= 0.0 ||
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater > 1.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 0 and <= 1.  entered value=[{:.3T}].", cNumericFields(10), NumArray(9)));
                ErrorsFound = true;
            }

            if (!state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater = 0.0;
            }

            // Air nodes
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                      AlphArray(5),
                                                                                                      ErrorsFound,
                                                                                                      CurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                                      ObjectIsNotParent);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                       AlphArray(6),
                                                                                                       ErrorsFound,
                                                                                                       CurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                                       ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

            // Check if the air inlet node is OA node, to justify whether the coil is placed in zone or not
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).IsDXCoilInZone =
                !CheckOutAirNodeNumber(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum);

            // Water nodes
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                                        AlphArray(7),
                                                                                                        ErrorsFound,
                                                                                                        CurrentModuleObject,
                                                                                                        AlphArray(1),
                                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                                        NodeInputManager::compFluidStream::Secondary,
                                                                                                        ObjectIsNotParent);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                                         AlphArray(8),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphArray(1),
                                                                                                         DataLoopNode::NodeFluidType::Water,
                                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                                         NodeInputManager::compFluidStream::Secondary,
                                                                                                         ObjectIsNotParent);

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(7), AlphArray(8), "Water Nodes");

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity = NumArray(10);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 0.0  entered value=[{:.1T}].", cNumericFields(10), NumArray(10)));
                ErrorsFound = true;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater = NumArray(11);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater < 0.0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, format("...{} must be >= 0 {{C}}.  entered value=[{:.1T}].", cNumericFields(11), NumArray(11)));
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(AlphArray(9), "DryBulbTemperature")) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirTemperatureType = DryBulbIndicator;
            } else if (UtilityRoutines::SameString(AlphArray(9), "WetBulbTemperature")) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirTemperatureType = WetBulbIndicator;
            } else {
                //   wrong temperature type selection
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                    "\", invalid");
                ShowContinueError(state, "..." + cAlphaFields(9) + " must be DryBulbTemperature or WetBulbTemperature.");
                ShowContinueError(state, "...entered value=\"" + AlphArray(9) + "\".");
                ErrorsFound = true;
            }

            // set rated inlet air temperature for curve object verification
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirTemperatureType == WetBulbIndicator) {
                WHInletAirTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp;
            } else {
                WHInletAirTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp;
            }
            // set rated water temperature for curve object verification
            WHInletWaterTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp;

            // part load curve
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(state, AlphArray(10)); // convert curve name to number
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
                if (lAlphaBlanks(10)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                    ShowContinueError(state, "...required " + cAlphaFields(10) + " is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                    ShowContinueError(state, "...not found " + cAlphaFields(10) + "=\"" + AlphArray(10) + "\".");
                }
                ErrorsFound = true;
            } else {
                CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                    ShowContinueError(state,
                                      "..." + cAlphaFields(10) +
                                          " output is not equal to 1.0 "
                                          "(+ or - 10%) at rated conditions.");
                    ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                }
            }

            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(12 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(13 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(I) = NumArray(14 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(15 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) = NumArray(16 + (I - 1) * 6);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(I) = NumArray(17 + (I - 1) * 6);

                AlfaFieldIncre = 11 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal =
                            CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), WHInletAirTemp, WHInletWaterTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 12 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), // Curve index
                                                     {1},                                                                     // Valid dimensions
                                                     RoutineName,                                                             // Routine name
                                                     CurrentModuleObject,                                                     // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,              // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                           // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 13 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), // Curve index
                                                     {1},                                                                       // Valid dimensions
                                                     RoutineName,                                                               // Routine name
                                                     CurrentModuleObject,                                                       // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,                // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                             // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 14 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is BiQuadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), // Curve index
                                                                {2},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal =
                            CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), WHInletAirTemp, WHInletWaterTemp);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 15 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), // Curve index
                                                                {1},                                                        // Valid dimensions
                                                                RoutineName,                                                // Routine name
                                                                CurrentModuleObject,                                        // Object Type
                                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, // Object Name
                                                                cAlphaFields(AlfaFieldIncre));                              // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }

                AlfaFieldIncre = 16 + (I - 1) * 6;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) =
                    GetCurveIndex(state, AlphArray(AlfaFieldIncre)); // convert curve name to number
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) == 0) {
                    if (lAlphaBlanks(AlfaFieldIncre)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", missing");
                        ShowContinueError(state, "...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal type is Quadratic
                    ErrorsFound |=
                        CurveManager::CheckCurveDims(state,
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), // Curve index
                                                     {1},                                                                      // Valid dimensions
                                                     RoutineName,                                                              // Routine name
                                                     CurrentModuleObject,                                                      // Object Type
                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,               // Object Name
                                                     cAlphaFields(AlfaFieldIncre));                                            // Field Name

                    if (!ErrorsFound) {
                        CurveVal = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), 1.0);
                        if (CurveVal > 1.10 || CurveVal < 0.90) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\", curve values");
                            ShowContinueError(state,
                                              "..." + cAlphaFields(AlfaFieldIncre) +
                                                  " output is not equal to 1.0 "
                                                  "(+ or - 10%) at rated conditions.");
                            ShowContinueError(state, format("...Curve output at rated conditions = {:.3T}", CurveVal));
                        }
                    }
                }
            }

            // get scale values
            for (I = 1; I <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPowerPerRatedTotCap(I) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(I) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
            }

            // CurrentModuleObject = "Coil:Waterheating:Airtowaterheatpump:Variablespeed"
            SetupOutputVariable(state,
                                "Cooling Coil Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Water Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                _,
                                "System");

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).IsDXCoilInZone) {
                SetupOutputVariable(state,
                                    "Cooling Coil Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "COOLINGCOILS",
                                    _,
                                    "System");
            } else {
                SetupOutputVariable(state,
                                    "Cooling Coil Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens =
                AutoSize; // always auto-sized, to be determined in the sizing calculation
        }
        //---------------------------VARIABLE-SPEED AIR SOURCE HPWH END --------------

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        lAlphaBlanks.deallocate();
        cNumericFields.deallocate();
        lNumericBlanks.deallocate();
        NumArray.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found getting input. Program terminates.");
        }

        for (DXCoilNum = 1; DXCoilNum <= state.dataVariableSpeedCoils->NumVarSpeedCoils; ++DXCoilNum) {
            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed)) {
                // Setup Report variables for the Heat Pump

                // cooling and heating coils separately
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    // air source cooling coils
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Latent Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Sensible Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Total Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Runtime Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Source Side Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Upper Speed Level",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Neighboring Speed Levels Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectMode ==
                        state.dataVariableSpeedCoils->CondensateToTank) {
                        SetupOutputVariable(state,
                                            "Cooling Coil Condensate Volume Flow Rate",
                                            OutputProcessor::Unit::m3_s,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVdot,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                        SetupOutputVariable(state,
                                            "Cooling Coil Condensate Volume",
                                            OutputProcessor::Unit::m3,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVol,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                            _,
                                            "OnSiteWater",
                                            "Condensate",
                                            _,
                                            "System");
                    }

                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).ReportEvapCondVars) {
                        SetupOutputVariable(state,
                                            "Cooling Coil Condenser Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondInletTemp,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                        SetupOutputVariable(state,
                                            "Cooling Coil Evaporative Condenser Water Volume",
                                            OutputProcessor::Unit::m3,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsump,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                            _,
                                            "Water",
                                            "Cooling",
                                            _,
                                            "System");
                        SetupOutputVariable(state,
                                            "Cooling Coil Evaporative Condenser Mains Water Volume",
                                            OutputProcessor::Unit::m3,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsump,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                            _,
                                            "MainsWater",
                                            "Cooling",
                                            _,
                                            "System");
                        SetupOutputVariable(state,
                                            "Cooling Coil Evaporative Condenser Pump Electricity Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecPower,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                        SetupOutputVariable(state,
                                            "Cooling Coil Evaporative Condenser Pump Electricity Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                            _,
                                            "Electricity",
                                            "COOLING",
                                            _,
                                            "System");
                        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPowerFTempDiff > 0.0) {
                            SetupOutputVariable(state,
                                                "Cooling Coil Basin Heater Electricity Rate",
                                                OutputProcessor::Unit::W,
                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                            SetupOutputVariable(state,
                                                "Cooling Coil Basin Heater Electricity Energy",
                                                OutputProcessor::Unit::J,
                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterConsumption,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Summed,
                                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                _,
                                                "Electricity",
                                                "COOLING",
                                                _,
                                                "System");
                        }
                    }

                    SetupOutputVariable(state,
                                        "Cooling Coil Crankcase Heater Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Crankcase Heater Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        _,
                                        "Electricity",
                                        "COOLING",
                                        _,
                                        "System");
                } else {
                    // air source heating coils
                    SetupOutputVariable(state,
                                        "Heating Coil Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Outlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Sensible Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Runtime Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Heating Coil Source Side Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Upper Speed Level",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Neighboring Speed Levels Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Heating Coil Defrost Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Defrost Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostConsumption,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        _,
                                        "Electricity",
                                        "HEATING",
                                        _,
                                        "System");
                    SetupOutputVariable(state,
                                        "Heating Coil Crankcase Heater Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Crankcase Heater Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        _,
                                        "Electricity",
                                        "HEATING",
                                        _,
                                        "System");
                }
            } else {

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit) { // fix coil type
                    // cooling WAHP coil
                    // Setup Report variables for water source Heat Pump
                    SetupOutputVariable(state,
                                        "Cooling Coil Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Total Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Sensible Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Latent Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Source Side Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Runtime Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Cooling Coil Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Source Side Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Source Side Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Source Side Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Cooling Coil Upper Speed Level",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Neighboring Speed Levels Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Recoverable Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                           Coil_HeatingWaterToAirHPVSEquationFit) { // fix coil type
                    // heating WAHP coil
                    // Setup Report variables for water source Heat Pump
                    SetupOutputVariable(state,
                                        "Heating Coil Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Sensible Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Heating Coil Source Side Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Runtime Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Heating Coil Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Air Outlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Source Side Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Source Side Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Source Side Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Heating Coil Upper Speed Level",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Neighboring Speed Levels Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Heating Coil Recoverable Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
                    // air source water heating coil
                    SetupOutputVariable(state,
                                        "Cooling Coil Water Heating Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Total Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Sensible Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Latent Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Total Water Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Runtime Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Cooling Coil Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Air Outlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Water Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Water Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Water Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Cooling Coil Crankcase Heater Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Crankcase Heater Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        _,
                                        "Electricity",
                                        "HEATING",
                                        _,
                                        "System");

                    SetupOutputVariable(state,
                                        "Cooling Coil Upper Speed Level",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedNumReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Neighboring Speed Levels Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SpeedRatioReport,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);

                    SetupOutputVariable(state,
                                        "Cooling Coil Water Heating Pump Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Cooling Coil Water Heating Pump Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Summed,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        _,
                                        "Electricity",
                                        "HEATING",
                                        _,
                                        "System");
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state,
                           std::string{RoutineName} + "Errors found in getting " + CurrentModuleObject +
                               " input.  Preceding condition(s) causes termination.");
        }
    }

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitVarSpeedCoil(EnergyPlusData &state,
                          int const DXCoilNum,                             // Current DXCoilNum under simulation
                          Real64 const MaxONOFFCyclesperHour,              // Maximum cycling rate of heat pump [cycles/hr]
                          Real64 const HPTimeConstant,                     // Heat pump time constant [s]
                          Real64 const FanDelayTime,                       // Fan delay time, time delay for the HP's fan to
                          Real64 const SensLoad,                           // Control zone sensible load[W]
                          Real64 const LatentLoad,                         // Control zone latent load[W]
                          int const CyclingScheme,                         // fan operating mode
                          [[maybe_unused]] Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
                          Real64 const SpeedRatio,                         // compressor speed ratio
                          int const SpeedNum                               // compressor speed number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on  MODULE WaterToAirHeatPumpSimple:InitSimpleWatertoAirHP
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the variable speed Water to Air HP Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // shut off after compressor cycle off  [s]

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineNameSimpleWatertoAirHP("InitSimpleWatertoAirHP");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;   // Node Number of the air inlet
        int WaterInletNode; // Node Number of the Water inlet
        Real64 rho;         // local fluid density
        Real64 Cp;          // local fluid specific heat
        int SpeedCal;       // calculated speed level
        bool errFlag;
        bool ErrorsFound(false);           // TRUE when errors found, air loop initialization error
        Real64 RatedVolFlowPerRatedTotCap; // Rated Air Volume Flow Rate divided by Rated Total Capacity [m3/s-W)
        int Mode;                          // Performance mode for MultiMode DX coil; Always 1 for other coil types
        Real64 RatedHeatPumpIndoorAirTemp; // Indoor dry-bulb temperature to heat pump evaporator at rated conditions [C]
        Real64 RatedHeatPumpIndoorHumRat;  // Inlet humidity ratio to heat pump evaporator at rated conditions [kg/kg]
        Real64 WaterFlowScale;             // water flow scaling factor match rated flow rate

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitVarSpeedCoil");

        if (state.dataVariableSpeedCoils->MyOneTimeFlag) {
            // initialize the environment and sizing flags
            state.dataVariableSpeedCoils->MySizeFlag.allocate(state.dataVariableSpeedCoils->NumVarSpeedCoils);
            state.dataVariableSpeedCoils->MyEnvrnFlag.allocate(state.dataVariableSpeedCoils->NumVarSpeedCoils);
            state.dataVariableSpeedCoils->MyPlantScanFlag.allocate(state.dataVariableSpeedCoils->NumVarSpeedCoils);
            state.dataVariableSpeedCoils->MySizeFlag = true;
            state.dataVariableSpeedCoils->MyEnvrnFlag = true;
            state.dataVariableSpeedCoils->MyPlantScanFlag = true;
            state.dataVariableSpeedCoils->MyOneTimeFlag = false;
        }

        state.dataHVACGlobal->DXCT = 1; // hard-code to non-DOAS sizing routine for cfm/ton until .ISHundredPercentDOASDXCoil member from DXcoils.cc
                                        // is added to VarSpeedCoil object

        // variable-speed heat pump water heating, begin
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed &&
            state.dataVariableSpeedCoils->MySizeFlag(DXCoilNum)) {

            SizeVarSpeedCoil(state, DXCoilNum);

            //   get rated coil bypass factor excluding fan heat

            state.dataVariableSpeedCoils->MySizeFlag(DXCoilNum) = false;
        }
        // variable-speed heat pump water heating, end

        // water source
        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
             DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) { // fix coil type
            if (state.dataVariableSpeedCoils->MyPlantScanFlag(DXCoilNum) && allocated(state.dataPlnt->PlantLoop)) {
                // switch from coil type numbers in DataHVACGlobals, to coil type numbers in plant.
                int plantTypeOfNum(0);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                    plantTypeOfNum = DataPlant::TypeOf_CoilVSWAHPCoolingEquationFit;
                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                           DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {
                    plantTypeOfNum = DataPlant::TypeOf_CoilVSWAHPHeatingEquationFit;
                }
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        plantTypeOfNum,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopSide,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BranchNum,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitVarSpeedCoil: Program terminated for previous conditions.");
                }
                state.dataVariableSpeedCoils->MyPlantScanFlag(DXCoilNum) = false;
            }
        } else {
            state.dataVariableSpeedCoils->MyPlantScanFlag(DXCoilNum) = false;
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataVariableSpeedCoils->MySizeFlag(DXCoilNum) &&
            !state.dataVariableSpeedCoils->MyPlantScanFlag(DXCoilNum)) {
            // for each furnace, do the sizing once.
            SizeVarSpeedCoil(state, DXCoilNum);

            state.dataVariableSpeedCoils->MySizeFlag(DXCoilNum) = false;

            // Multispeed Cooling
            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed)) {
                for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
                    // Check for zero capacity or zero max flow rate
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) <= 0.0) {
                        ShowSevereError(state,
                                        format("Sizing: {} {} has zero rated total capacity at speed {}",
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                               Mode));
                        ErrorsFound = true;
                    }
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) <= 0.0) {
                        ShowSevereError(state,
                                        format("Sizing: {} {} has zero rated air flow rate at speed {}",
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                               Mode));
                        ErrorsFound = true;
                    }
                    if (ErrorsFound) {
                        ShowFatalError(state, "Preceding condition causes termination.");
                    }
                    // Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
                    RatedVolFlowPerRatedTotCap = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode);
                    // note: variable-speed HP can exceed the flow rate restrictions at low speed levels
                    //        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
                    //           ((RatedVolFlowPerRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
                    //          CALL ShowSevereError(state,'Sizing: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType) &
                    //           // ' "'//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
                    //                '": Rated air volume flow rate per watt of rated total '// &
                    //                'cooling capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
                    //          CALL ShowContinueError &
                    //           ('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
                    //           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
                    //           Max Rated Vol Flow Per Watt=['// &
                    //           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference Manual for valid range.')
                    //        END IF
                    //        VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
                    //          PsyRhoAirFnPbTdbW(state, OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
                    //        ! get high speed rated coil bypass factor
                    //        VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode) = CalcCBF(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType, &
                    //               VarSpeedCoil(DXCoilNum)%Name,&
                    //                                           RatedInletAirTemp,RatedInletAirHumRat,VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode),&
                    //                                           VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode), &
                    //                           VarSpeedCoil(DXCoilNum)%MSRatedSHR(Mode))
                }
                // call coil model with everthing set at rating point
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp = state.dataVariableSpeedCoils->RatedInletAirTemp;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat =
                    Psychrometrics::PsyWFnTdbTwbPb(state,
                                                   state.dataVariableSpeedCoils->RatedInletAirTemp,
                                                   state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                                   DataEnvironment::StdPressureSeaLevel);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(
                    state.dataVariableSpeedCoils->RatedInletAirTemp, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure = DataEnvironment::StdPressureSeaLevel;

                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate *
                    Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                      DataEnvironment::StdPressureSeaLevel,
                                                      state.dataVariableSpeedCoils->RatedInletAirTemp,
                                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat);
                // store environment data fill back in after rating point calc is over
                Real64 holdOutDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
                Real64 holdOutHumRat = state.dataEnvrn->OutHumRat;
                Real64 holdOutWetBulb = state.dataEnvrn->OutWetBulbTemp;
                Real64 holdOutBaroPress = state.dataEnvrn->OutBaroPress;
                Real64 ratedOutdoorAirWetBulb = 23.9; // from I/O ref. more precise value?

                state.dataEnvrn->OutDryBulbTemp = state.dataVariableSpeedCoils->RatedAmbAirTemp;
                state.dataEnvrn->OutWetBulbTemp = ratedOutdoorAirWetBulb;
                state.dataEnvrn->OutBaroPress = DataEnvironment::StdPressureSeaLevel; // assume rating is for sea level.
                state.dataEnvrn->OutHumRat = Psychrometrics::PsyWFnTdbTwbPb(
                    state, state.dataVariableSpeedCoils->RatedAmbAirTemp, ratedOutdoorAirWetBulb, DataEnvironment::StdPressureSeaLevel, RoutineName);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum > 0) {
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Temp =
                        state.dataVariableSpeedCoils->RatedAmbAirTemp;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).HumRat =
                        state.dataEnvrn->OutHumRat;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Press =
                        DataEnvironment::StdPressureSeaLevel;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).OutAirWetBulb =
                        ratedOutdoorAirWetBulb;
                }
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                    DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) { // need to set water info for WSHP
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp =
                        state.dataVariableSpeedCoils->RatedInletWaterTemp; // 85 F cooling mode
                    Real64 CpSource =
                        GetSpecificHeatGlycol(state,
                                              state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                              state.dataVariableSpeedCoils->SourceSideInletTemp,
                                              state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                              RoutineName);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp * CpSource;
                }

                // calculate coil model at rating point
                Real64 runtimeFrac(1.0);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);

                CalcVarSpeedCoilCooling(state,
                                        DXCoilNum,
                                        2.0,
                                        runtimeFrac,
                                        SensLoad,
                                        LatentLoad,
                                        1,
                                        1.0,
                                        1.0,
                                        1.0,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                // coil outlets
                Real64 RatedOutletWetBulb(0.0);
                RatedOutletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                                                    DataEnvironment::StdPressureSeaLevel,
                                                                    RoutineName);
                state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(
                    state,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal, // this is the report variable
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,  // this is the report variable
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                    state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                    RatedOutletWetBulb,
                    state.dataVariableSpeedCoils->RatedAmbAirTemp,
                    ratedOutdoorAirWetBulb,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds),
                    -999.0); // coil effectiveness not define for DX

                // now replace the outdoor air conditions set above for one time rating point calc
                state.dataEnvrn->OutDryBulbTemp = holdOutDryBulbTemp;
                state.dataEnvrn->OutHumRat = holdOutHumRat;
                state.dataEnvrn->OutWetBulbTemp = holdOutWetBulb;
                state.dataEnvrn->OutBaroPress = holdOutBaroPress;
            }

            // Multispeed Heating
            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed)) {
                RatedHeatPumpIndoorAirTemp = 21.11;  // 21.11C or 70F
                RatedHeatPumpIndoorHumRat = 0.00881; // Humidity ratio corresponding to 70F dry bulb/60F wet bulb
                for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {

                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) *
                        PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, RatedHeatPumpIndoorAirTemp, RatedHeatPumpIndoorHumRat, RoutineName);
                    // Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
                    RatedVolFlowPerRatedTotCap = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode);
                    // note: variable-speed HP can exceed the flow rate restrictions at low speed levels
                    //        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
                    //            ((RatedVolFlowperRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
                    //          CALL ShowSevereError(state,'Coil:Heating:DX:MultiSpeed '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
                    //                              ': Rated air volume flow rate per watt of rated total '// &
                    //                'heating capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
                    //          CALL ShowContinueError(state, 'Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits &
                    //           (MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
                    //           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
                    //               Max Rated Vol Flow Per Watt=['// &
                    //           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference  &
                    //                Manual for valid range.')
                    //        END IF
                }
                // call coil model with everthing set at rating point
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp = state.dataVariableSpeedCoils->RatedInletAirTempHeat;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat =
                    Psychrometrics::PsyWFnTdbTwbPb(state,
                                                   state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                                   state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                                                   DataEnvironment::StdPressureSeaLevel);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(
                    state.dataVariableSpeedCoils->RatedInletAirTempHeat, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure = DataEnvironment::StdPressureSeaLevel;

                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate *
                    Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                      DataEnvironment::StdPressureSeaLevel,
                                                      state.dataVariableSpeedCoils->RatedInletAirTempHeat,
                                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat);
                // store environment data fill back in after rating point calc is over
                Real64 holdOutDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
                Real64 holdOutHumRat = state.dataEnvrn->OutHumRat;
                Real64 holdOutWetBulb = state.dataEnvrn->OutWetBulbTemp;
                Real64 holdOutBaroPress = state.dataEnvrn->OutBaroPress;

                state.dataEnvrn->OutDryBulbTemp = state.dataVariableSpeedCoils->RatedAmbAirTempHeat;
                state.dataEnvrn->OutWetBulbTemp = state.dataVariableSpeedCoils->RatedAmbAirWBHeat;
                state.dataEnvrn->OutBaroPress = DataEnvironment::StdPressureSeaLevel; // assume rating is for sea level.
                state.dataEnvrn->OutHumRat = Psychrometrics::PsyWFnTdbTwbPb(state,
                                                                            state.dataVariableSpeedCoils->RatedAmbAirTempHeat,
                                                                            state.dataVariableSpeedCoils->RatedAmbAirWBHeat,
                                                                            DataEnvironment::StdPressureSeaLevel,
                                                                            RoutineName);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum > 0) {
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Temp =
                        state.dataVariableSpeedCoils->RatedAmbAirTempHeat;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).HumRat =
                        state.dataEnvrn->OutHumRat;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Press =
                        DataEnvironment::StdPressureSeaLevel;
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).OutAirWetBulb =
                        state.dataVariableSpeedCoils->RatedAmbAirWBHeat;
                }

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                    DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) { // need to set water info for WSHP
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp =
                        state.dataVariableSpeedCoils->RatedInletWaterTempHeat; // 21.11C or 70F, heating mode
                    Real64 CpSource =
                        GetSpecificHeatGlycol(state,
                                              state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                              state.dataVariableSpeedCoils->SourceSideInletTemp,
                                              state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                              RoutineName);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp * CpSource;
                }

                // calculate coil model at rating point
                Real64 runtimeFrac(1.0);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                CalcVarSpeedCoilHeating(state,
                                        DXCoilNum,
                                        2.0,
                                        runtimeFrac,
                                        SensLoad,
                                        1,
                                        1.0,
                                        1.0,
                                        1.0,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds);
                // coil outlets
                Real64 RatedOutletWetBulb(0.0);
                RatedOutletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                                                                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                                                                    DataEnvironment::StdPressureSeaLevel,
                                                                    RoutineName);
                state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(
                    state,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal, // this is the report variable
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible,  // this is the report variable
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat,
                    state.dataVariableSpeedCoils->RatedInletWetBulbTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat,
                    RatedOutletWetBulb,
                    state.dataVariableSpeedCoils->RatedAmbAirTempHeat,
                    state.dataVariableSpeedCoils->RatedAmbAirWBHeat,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds),
                    -999.0); // coil effectiveness not define for DX

                // now replace the outdoor air conditions set above for one time rating point calc
                state.dataEnvrn->OutDryBulbTemp = holdOutDryBulbTemp;
                state.dataEnvrn->OutHumRat = holdOutHumRat;
                state.dataEnvrn->OutWetBulbTemp = holdOutWetBulb;
                state.dataEnvrn->OutBaroPress = holdOutBaroPress;
            }

            // store fan info for coil
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFan_TypeNum == DataHVACGlobals::FanType_SystemModelObject) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex > -1) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanName,
                        DataAirSystems::objectVectorOOFanSystemModel,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex);
                }

            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex > 0) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanName,
                        DataAirSystems::structArrayLegacyFanModels,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex);
                }
            }
        }

        if (SpeedNum > state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
            SpeedCal = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
        } else if (SpeedNum < 1) {
            SpeedCal = 1;
        } else {
            SpeedCal = SpeedNum;
        }

        if ((SpeedNum <= 1) || (SpeedNum > state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(SpeedCal);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(SpeedCal);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(SpeedCal);
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal - 1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(SpeedCal) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(SpeedCal - 1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(SpeedCal) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(SpeedCal - 1);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterVolFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(SpeedCal) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(SpeedCal - 1);
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataVariableSpeedCoils->MyEnvrnFlag(DXCoilNum) &&
            !state.dataVariableSpeedCoils->MyPlantScanFlag(DXCoilNum)) {
            // Do the initializations to start simulation

            AirInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum;

            // Initialize all report variables to a known state at beginning of simulation
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterVolFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = 0.0;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPTimeConstant = HPTimeConstant;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanDelayTime = FanDelayTime;

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingWaterToAirHPVSEquationFit) ||
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit)) {
                WaterInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum;

                rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                       DataGlobalConstants::CWInitConvTemp,
                                       state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                       RoutineNameSimpleWatertoAirHP);
                Cp = GetSpecificHeatGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                           DataGlobalConstants::CWInitConvTemp,
                                           state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                           RoutineNameSimpleWatertoAirHP);

                //    VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate= &
                //                             rho * VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate

                InitComponentNodes(state,
                                   0.0,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopSide,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BranchNum,
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompNum);

                state.dataLoopNodes->Node(WaterInletNode).Temp = 5.0;
                state.dataLoopNodes->Node(WaterInletNode).Enthalpy = Cp * state.dataLoopNodes->Node(WaterInletNode).Temp;
                state.dataLoopNodes->Node(WaterInletNode).Quality = 0.0;
                state.dataLoopNodes->Node(WaterInletNode).Press = 0.0;
                state.dataLoopNodes->Node(WaterInletNode).HumRat = 0.0;

                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum).Temp = 5.0;
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum).Enthalpy =
                    Cp * state.dataLoopNodes->Node(WaterInletNode).Temp;
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum).Quality = 0.0;
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum).Press = 0.0;
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum).HumRat = 0.0;
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = true;
            state.dataHeatBal->HeatReclaimVS_DXCoil(DXCoilNum).AvailCapacity = 0.0;

            state.dataVariableSpeedCoils->MyEnvrnFlag(DXCoilNum) = false;

        } // End If for the Begin Environment initializations

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataVariableSpeedCoils->MyEnvrnFlag(DXCoilNum) = true;
        }

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.
        // First set the conditions for the air into the heat pump model

        // Set water and air inlet nodes

        AirInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum;
        WaterInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum;

        if ((SensLoad != 0.0 || LatentLoad != 0.0) && (state.dataLoopNodes->Node(AirInletNode).MassFlowRate > 0.0)) {

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel) > 0.0) {
                WaterFlowScale = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate /
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate * WaterFlowScale;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = 0.0;
            }

            if (CyclingScheme == ContFanCycCoil) {
                // continuous fan, cycling compressor
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
                //    VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate*  &
                //             PsyRhoAirFnPbTdbW(state, OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
                // If air flow is less than 25% rated flow. Then set air flow to the 25% of rated conditions
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate <
                    0.25 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate *
                        PsyRhoAirFnPbTdbW(state,
                                          state.dataEnvrn->OutBaroPress,
                                          state.dataLoopNodes->Node(AirInletNode).Temp,
                                          state.dataLoopNodes->Node(AirInletNode).HumRat)) {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate =
                        0.25 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirVolFlowRate *
                        PsyRhoAirFnPbTdbW(state,
                                          state.dataEnvrn->OutBaroPress,
                                          state.dataLoopNodes->Node(AirInletNode).Temp,
                                          state.dataLoopNodes->Node(AirInletNode).HumRat);
                }
            } else { // CYCLIC FAN, NOT CORRECTION, WILL BE PROCESSED IN THE FOLLOWING SUBROUTINES
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
            }

        } else { // heat pump is off
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = 0.0;
        }

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingWaterToAirHPVSEquationFit) ||
            (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit)) {
            SetComponentFlowRate(state,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopSide,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BranchNum,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompNum);

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = 0.0;
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;
        };

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp = state.dataLoopNodes->Node(AirInletNode).Temp;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy = state.dataLoopNodes->Node(AirInletNode).Enthalpy;

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPTimeConstant = HPTimeConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanDelayTime = FanDelayTime;

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure = state.dataEnvrn->OutBaroPress; // temporary
        // Outlet variables
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy = 0.0;

        // bug fix, must set zeros to the variables below, otherwise can't pass switch DD test
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsump = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterConsumption = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostConsumption = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVdot = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVol = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat = 0.0;

        // clear zeros to HPWH variables
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).ElecWaterHeatingPower =
            0.0; // Total electric power consumed by compressor and condenser pump [W]
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).ElecWaterHeatingConsumption =
            0.0; // Total electric consumption by compressor and condenser pump [J]
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).TotalHeatingEnergy = 0.0;       // total water heating energy
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate = 0.0;   // total WH energy rate
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = 0.0; // power power

        state.dataVariableSpeedCoils->VSHPWHHeatingCapacity = 0.0; // Used by Heat Pump:Water Heater object as total water heating capacity [W]
        state.dataVariableSpeedCoils->VSHPWHHeatingCOP = 0.0;      // Used by Heat Pump:Water Heater object as water heating COP [W/W]
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp;
        state.dataHeatBal->HeatReclaimVS_DXCoil(DXCoilNum).AvailCapacity = 0.0;
    }

    void SizeVarSpeedCoil(EnergyPlusData &state, int const DXCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:SizeHVACWaterToAir
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing WSHP Components for which nominal capacities
        // and flow rates have not been specified in the input

        // METHODOLOGY EMPLOYED:
        // Obtains heating capacities and flow rates from the zone or system sizing arrays.
        // NOTE: For WSHP's we are sizing the heating capacity to be
        // equal to the cooling capacity.  Thus the cooling and
        // and heating capacities of a DX heat pump system will be identical. In real life the ARI
        // heating and cooling capacities are close but not identical.

        // Using/Aliasing
        using namespace Psychrometrics;
        using DataHVACGlobals::SmallAirVolFlow;
        using DataHVACGlobals::SmallLoad;
        using PlantUtilities::MyPlantSizingIndex;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using namespace OutputReportPredefined;
        using CurveManager::CurveValue;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        Real64 QLoadTotal; // placeholder for calculating SHR

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeVarSpeedCoil");
        static constexpr std::string_view RoutineNameAlt("SizeHVACWaterToAir");

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 rhoair;
        Real64 MixTemp = -999.0;
        Real64 MixHumRat = -999.0;
        Real64 MixEnth = -999.0;
        Real64 MixWetBulb = -999.0;
        Real64 SupTemp = -999.0;
        Real64 SupHumRat = -999.0;
        Real64 SupEnth = -999.0;
        Real64 OutTemp = -999.0;
        Real64 OutAirFrac = -999.0;
        Real64 VolFlowRate = -999.0;
        Real64 CoolCapAtPeak = -999.0;
        Real64 TotCapTempModFac = -999.0;
        int TimeStepNumAtMax;
        int DDNum;
        int PltSizNum;
        bool RatedCapCoolTotalAutoSized;
        bool RatedCapCoolSensAutoSized;
        bool RatedCapHeatAutoSized;
        bool RatedAirFlowAutoSized;
        bool RatedWaterFlowAutoSized;
        bool ErrorsFound;
        Real64 SystemCapacity;
        Real64 rho;
        Real64 cp;
        int NormSpeed;                       // norminal speed level
        int UpperSpeed;                      // highest speed level
        int Mode;                            // speed level
        Real64 rhoW;                         // water density
        Real64 rhoA;                         // air density
        Real64 SHR;                          // sensible heat transfer ratio
        Real64 RatedAirMassFlowRate;         // rated air mass flow rate
        Real64 CBFRated;                     // bypass factor at the rated condition, considering difference in flow rates
        Real64 RatedInletEnth;               // rated inlet air enthalpy
        Real64 QLoadTotal1;                  // placeholder for calculating SHR
        Real64 QLoadTotal2;                  // placeholder for calculating SHR
        Real64 AirMassFlowRatio;             // air mass flow ratio
        Real64 WaterMassFlowRatio;           // water mass flow rate
        Real64 RatedSourceTempCool;          // rated source temperature, space cooling mode
        std::string CurrentObjSubfix;        // Object subfix type for printing
        bool IsAutoSize;                     // Indicator to autosize
        bool HardSizeNoDesRun;               // Indicator to hardsize withouth sizing runs
        bool HardSizeNoDesRunAirFlow;        // Indicator to hardsize withouth sizing runs for rated air flow field
        Real64 RatedAirVolFlowRateDes;       // Autosized rated air flow for reporting
        Real64 RatedAirVolFlowRateUser;      // Hardsized rated air flow for reporting
        Real64 RatedCapCoolTotalDes;         // Autosized rated total cooling capacity for reporting
        Real64 RatedCapCoolTotalUser;        // Hardsized rated total cooling capacity for reporting
        Real64 RatedCapHeatDes;              // Autosized rated heating capacity for reporting
        Real64 RatedCapHeatUser;             // Hardsized rated heating capacity for reporting
        Real64 RatedWaterVolFlowRateDes;     // Autosized rated water flow for reporting
        Real64 RatedWaterVolFlowRateUser;    // Hardsized rated water flow for reporting
        Real64 RatedCapCoolSensDes;          // Autosized rated sensible cooling capacity for reporting
        Real64 RatedCapCoolSensUser;         // Hardsized rated sensible cooling capacity for reporting
        Real64 EvapCondPumpElecNomPowerDes;  // Autosized evaporative condenser pump power for reporting
        Real64 EvapCondPumpElecNomPowerUser; // Hardsized evaporative condenser pump power for reporting
        Real64 DefrostCapacityDes;           // Autosized resistive defrost heater capacity for reporting
        Real64 DefrostCapacityUser;          // Hardsized resistive defrost heater capacity for reporting
        bool SizingDesRunThisAirSys;         // true if a particular air system had a Sizing:System object and system sizing done
        bool SizingDesRunThisZone;           // true if a particular zone had a Sizing:Zone object and zone sizing was done
        Real64 HPInletAirHumRat;             // Rated inlet air humidity ratio for heat pump water heater [kgWater/kgDryAir]
        Real64 HPWHCoolCapacity;             // estimate cooling capacity in HPWH

        UpperSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;
        NormSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel;
        PltSizNum = 0;
        ErrorsFound = false;
        RatedAirFlowAutoSized = false;
        RatedWaterFlowAutoSized = false;
        RatedCapHeatAutoSized = false;
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
        HardSizeNoDesRunAirFlow = false;
        RatedAirVolFlowRateDes = 0.0;
        RatedAirVolFlowRateUser = 0.0;
        RatedCapCoolTotalDes = 0.0;
        RatedCapCoolTotalUser = 0.0;
        RatedCapHeatDes = 0.0;
        RatedCapHeatUser = 0.0;
        RatedWaterVolFlowRateDes = 0.0;
        RatedWaterVolFlowRateUser = 0.0;
        RatedCapCoolSensDes = 0.0;
        RatedCapCoolSensUser = 0.0;
        EvapCondPumpElecNomPowerDes = 0.0;
        EvapCondPumpElecNomPowerUser = 0.0;
        DefrostCapacityDes = 0.0;
        DefrostCapacityUser = 0.0;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit ||
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {
            CurrentObjSubfix = ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT";
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
            CurrentObjSubfix = ":WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
        } else {
            CurrentObjSubfix = ":DX:VARIABLESPEED";
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate == DataGlobalConstants::AutoCalculate) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(NormSpeed) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed); // 0.00005035;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowAutoSized = true;
            }
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowAutoSized);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate == DataGlobalConstants::AutoCalculate) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedHPWHCondWaterFlow =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed); // 0.00000004487;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedHPWHCondWaterFlow;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterVolFlowAutoSized = true;
            }
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowPltSizNum(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterVolFlowAutoSized,
                -999,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum);
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate == AutoSize) {
            RatedAirFlowAutoSized = true;
        }

        if (state.dataSize->CurSysNum > 0) {
            if (!RatedAirFlowAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                HardSizeNoDesRunAirFlow = true;
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "User-Specified Rated Air Flow Rate [m3/s]",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate);
                }
            } else {
                CheckSysSizing(state,
                               "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow >= SmallAirVolFlow) {
                    RatedAirVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                } else {
                    RatedAirVolFlowRateDes = 0.0;
                }
            }
        }

        if (state.dataSize->CurZoneEqNum > 0) {
            if (!RatedAirFlowAutoSized && !SizingDesRunThisZone) { // Simulation continue
                HardSizeNoDesRunAirFlow = true;
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "User-Specified Rated Air Flow Rate [m3/s]",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate);
                }
            } else {
                CheckZoneSizing(state,
                                "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                RatedAirVolFlowRateDes = max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                                             state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);
                if (RatedAirVolFlowRateDes < SmallAirVolFlow) {
                    RatedAirVolFlowRateDes = 0.0;
                }
            }
        }

        if (RatedAirFlowAutoSized) state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = RatedAirVolFlowRateDes;
        //    CALL BaseSizer::reportSizerOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
        //                             VarSpeedCoil(DXCoilNum)%Name, &
        //                            'Rated Air Flow Rate [m3/s]', &
        //                             VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate)

        // RatedAirFlowAutoSized = .TRUE.

        RatedCapCoolTotalAutoSized = false;
        RatedCapCoolSensAutoSized = false;

        // size rated total cooling capacity
        IsAutoSize = false;
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal == AutoSize &&
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
            RatedCapCoolTotalAutoSized = true;
        }
        if (SizingDesRunThisZone || SizingDesRunThisAirSys) HardSizeNoDesRun = false;
        if (state.dataSize->CurSysNum > 0) {
            if (!RatedCapCoolTotalAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                HardSizeNoDesRun = true;
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "User-Specified Rated Total Cooling Capacity [W]",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
                }
            } else {
                CheckSysSizing(state,
                               "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                VolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate;
                if (VolFlowRate >= SmallAirVolFlow) {
                    if (state.dataSize->CurOASysNum > 0) { // coil is in the OA stream
                        MixTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutTempAtCoolPeak;
                        MixHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutHumRatAtCoolPeak;
                        SupTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolTemp;
                        SupHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolHumRat;
                    } else { // coil is on the main air loop
                        SupTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).CoolSupTemp;
                        SupHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).CoolSupHumRat;
                        if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).NumOACoolCoils ==
                            0) { // there is no precooling of the OA stream
                            MixTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).MixTempAtCoolPeak;
                            MixHumRat = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).MixHumRatAtCoolPeak;
                        } else { // there is precooling of OA stream
                            if (VolFlowRate > 0.0) {
                                OutAirFrac = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow / VolFlowRate;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            MixTemp = OutAirFrac * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolTemp +
                                      (1.0 - OutAirFrac) * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).RetTempAtCoolPeak;
                            MixHumRat = OutAirFrac * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).PrecoolHumRat +
                                        (1.0 - OutAirFrac) * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).RetHumRatAtCoolPeak;
                        }
                    }
                    OutTemp = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).OutTempAtCoolPeak;
                    rhoair = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MixTemp, MixHumRat, RoutineName);
                    MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                    SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);

                    // design fan heat will be added to coil load
                    Real64 FanCoolLoad =
                        DataAirSystems::calcFanDesignHeatGain(state, state.dataSize->DataFanEnumType, state.dataSize->DataFanIndex, VolFlowRate);
                    // inlet/outlet temp is adjusted after enthalpy is calculcated so fan heat is not double counted
                    Real64 CpAir = PsyCpAirFnW(MixHumRat);
                    if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                        DataAirSystems::fanPlacement::BlowThru) {
                        MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate);
                    } else if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanLocation ==
                               DataAirSystems::fanPlacement::DrawThru) {
                        SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate);
                    }
                    MixWetBulb = PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
                    // need to use OutTemp for air-cooled and RatedInletWaterTemp for water-cooled
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum != 0) {
                        RatedSourceTempCool = state.dataVariableSpeedCoils->RatedInletWaterTemp;
                    } else {
                        RatedSourceTempCool = OutTemp;
                    }
                    TotCapTempModFac = CurveValue(state,
                                                  state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(
                                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel),
                                                  MixWetBulb,
                                                  RatedSourceTempCool);

                    //       The mixed air temp for zone equipment without an OA mixer is 0.
                    //       This test avoids a negative capacity until a solution can be found.
                    if (MixEnth > SupEnth) {
                        CoolCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) + FanCoolLoad;
                    } else {
                        CoolCapAtPeak = (rhoair * VolFlowRate * (48000.0 - SupEnth)) + FanCoolLoad;
                    }
                    CoolCapAtPeak = max(0.0, CoolCapAtPeak);
                    if (TotCapTempModFac > 0.0) {
                        RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac;
                    } else {
                        RatedCapCoolTotalDes = CoolCapAtPeak;
                    }
                } else {
                    RatedCapCoolTotalDes = 0.0;
                }
            }

        } else if (state.dataSize->CurZoneEqNum > 0) {
            if (!RatedCapCoolTotalAutoSized && !SizingDesRunThisZone) { // Simulation continue
                HardSizeNoDesRun = true;
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "User-Specified Rated Total Cooling Capacity [W]",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
                }
            } else {
                CheckZoneSizing(state,
                                "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                VolFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate;
                if (VolFlowRate >= SmallAirVolFlow) {
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
                    TimeStepNumAtMax = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).TimeStepNumAtCoolMax;
                    DDNum = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolDDNum;
                    if (DDNum > 0 && TimeStepNumAtMax > 0) {
                        OutTemp = state.dataSize->DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                    } else {
                        OutTemp = 0.0;
                    }
                    rhoair = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MixTemp, MixHumRat, RoutineName);
                    MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                    SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);

                    // design fan heat will be added to coil load
                    Real64 FanCoolLoad =
                        DataAirSystems::calcFanDesignHeatGain(state, state.dataSize->DataFanEnumType, state.dataSize->DataFanIndex, VolFlowRate);
                    // inlet/outlet temp is adjusted after enthalpy is calculcated so fan heat is not double counted
                    Real64 CpAir = PsyCpAirFnW(MixHumRat);

                    if (state.dataSize->DataFanPlacement == DataSizing::zoneFanPlacement::zoneBlowThru) {
                        MixTemp += FanCoolLoad / (CpAir * rhoair * VolFlowRate);
                    } else {
                        SupTemp -= FanCoolLoad / (CpAir * rhoair * VolFlowRate);
                    }

                    MixWetBulb = PsyTwbFnTdbWPb(state, MixTemp, MixHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
                    // need to use OutTemp for air-cooled and RatedInletWaterTemp for water-cooled
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum != 0) {
                        RatedSourceTempCool = state.dataVariableSpeedCoils->RatedInletWaterTemp;
                    } else {
                        RatedSourceTempCool = OutTemp;
                    }
                    TotCapTempModFac = CurveValue(state,
                                                  state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(
                                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel),
                                                  MixWetBulb,
                                                  RatedSourceTempCool);

                    //       The mixed air temp for zone equipment without an OA mixer is 0.
                    //       This test avoids a negative capacity until a solution can be found.
                    if (MixEnth > SupEnth) {
                        CoolCapAtPeak = (rhoair * VolFlowRate * (MixEnth - SupEnth)) + FanCoolLoad;
                    } else {
                        CoolCapAtPeak = (rhoair * VolFlowRate * (48000.0 - SupEnth)) + FanCoolLoad;
                    }
                    CoolCapAtPeak = max(0.0, CoolCapAtPeak);
                    if (TotCapTempModFac > 0.0) {
                        RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac;
                    } else {
                        RatedCapCoolTotalDes = CoolCapAtPeak;
                    }
                } else {
                    RatedCapCoolTotalDes = 0.0;
                }
            }
        }
        if (RatedCapCoolTotalDes < SmallLoad) {
            RatedCapCoolTotalDes = 0.0;
        }
        if (!HardSizeNoDesRun) {
            if (RatedCapCoolTotalAutoSized) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = RatedCapCoolTotalDes;
                BaseSizer::reportSizerOutput(state,
                                             "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Rated Total Cooling Capacity [W]",
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchCoolCoilTotCap,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal -
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal != 0.0) {
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilSHR,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens /
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilNomEff,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(NormSpeed));
                } else {
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchCoolCoilSHR, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, 0.0);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchCoolCoilNomEff, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, 0.0);
                }
                addFootNoteSubTable(
                    state,
                    state.dataOutRptPredefined->pdstCoolCoil,
                    "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal > 0.0 && RatedCapCoolTotalDes > 0.0) {
                    RatedCapCoolTotalUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal;
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "Design Size Rated Total Cooling Capacity [W]",
                                                 RatedCapCoolTotalDes,
                                                 "User-Specified Rated Total Cooling Capacity [W]",
                                                 RatedCapCoolTotalUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(RatedCapCoolTotalDes - RatedCapCoolTotalUser) / RatedCapCoolTotalUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeVarSpeedCoil: Potential issue with equipment sizing for " +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                            ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                            ShowContinueError(state, format("User-Specified Rated Total Cooling Capacity of {:.2R} [W]", RatedCapCoolTotalUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Rated Total Cooling Capacity of {:.2R} [W]", RatedCapCoolTotalDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }

            state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                MixTemp,
                state.dataSize->CurSysNum,
                state.dataSize->CurZoneEqNum);
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirHumRat(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                MixHumRat);
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                SupTemp);
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                SupHumRat);
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilCoolingCapacity(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                RatedCapCoolTotalDes,
                RatedCapCoolTotalAutoSized,
                state.dataSize->CurSysNum,
                state.dataSize->CurZoneEqNum,
                state.dataSize->CurOASysNum,
                0.0, // no fan load included in sizing
                TotCapTempModFac,
                -999.0,
                -999.0); // VS model doesn't limit, double check
        }

        // Set the global DX cooling coil capacity variable for use by other objects
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
            state.dataSize->DXCoolCap = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal;
        }

        // size rated heating capacity
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat == AutoSize &&
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {
            RatedCapHeatAutoSized = true;
        }
        //   simply set heating capacity equal to the cooling capacity
        // VarSpeedCoil(DXCoilNum)%RatedCapHeat = DXCoolCap
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum > 0) {
                RatedCapHeatDes =
                    state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                        .RatedCapCoolTotal;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = RatedCapHeatDes; // AVOID BEING ZERO
            } else {
                RatedCapHeatDes = state.dataSize->DXCoolCap; // previous code, can be risky
            }
            // END IF
            if (RatedCapHeatAutoSized) {
                if (RatedCapHeatDes == AutoSize) {
                    ShowWarningError(state,
                                     "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType +
                                         ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\"");
                    ShowContinueError(
                        state, std::string{RoutineName} + ": Heating coil could not be autosized since cooling coil was not previously sized.");
                    ShowContinueError(state, "... Cooling coil must be upstream of heating coil.");
                    ShowContinueError(state, "... Manually sizing this heating coil will be required.");
                }
            }
            if (RatedCapHeatDes < SmallLoad) {
                RatedCapHeatDes = 0.0;
            }
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                RatedCapHeatDes,
                RatedCapHeatAutoSized,
                state.dataSize->CurSysNum,
                state.dataSize->CurZoneEqNum,
                state.dataSize->CurOASysNum,
                0.0,
                1.0,
                -999.0,
                -999.0);
        }
        if (RatedCapHeatAutoSized) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat = RatedCapHeatDes;
            BaseSizer::reportSizerOutput(state,
                                         "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                         "Design Size Nominal Heating Capacity [W]",
                                         RatedCapHeatDes);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchHeatCoilNomCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat != 0.0) {
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchHeatCoilNomEff,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(NormSpeed));
            } else {
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchHeatCoilNomEff, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, 0.0);
            }
            addFootNoteSubTable(state,
                                state.dataOutRptPredefined->pdstHeatCoil,
                                "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
        } else {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat > 0.0 && RatedCapHeatDes > 0.0) {
                RatedCapHeatUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat;
                BaseSizer::reportSizerOutput(state,
                                             "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Nominal Heating Capacity [W]",
                                             RatedCapHeatDes,
                                             "User-Specified Nominal Heating Capacity [W]",
                                             RatedCapHeatUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(RatedCapHeatDes - RatedCapHeatUser) / RatedCapHeatUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state,
                                    "SizeVarSpeedCoil: Potential issue with equipment sizing for " +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                        ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                        ShowContinueError(state, format("User-Specified Rated Total Heating Capacity of {:.2R} [W]", RatedCapHeatUser));
                        ShowContinueError(state, format("differs from Design Size Rated Total Heating Capacity of {:.2R} [W]", RatedCapHeatDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }

        // FORCE BACK TO THE RATED AIR FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATALOG DATA
        if (!HardSizeNoDesRunAirFlow) {
            if ((RatedCapCoolTotalAutoSized) && (RatedAirFlowAutoSized)) {
                RatedAirVolFlowRateDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal *
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(NormSpeed);
            } else if ((RatedCapHeatAutoSized) && (RatedAirFlowAutoSized)) {
                RatedAirVolFlowRateDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat *
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(NormSpeed);
            }

            // write the air flow sizing output
            if (RatedAirFlowAutoSized) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = RatedAirVolFlowRateDes;
                BaseSizer::reportSizerOutput(state,
                                             "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Rated Air Flow Rate [m3/s]",
                                             RatedAirVolFlowRateDes);
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate > 0.0 && RatedAirVolFlowRateDes > 0.0) {
                    RatedAirVolFlowRateUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "Design Size Rated Air Flow Rate [m3/s]",
                                                 RatedAirVolFlowRateDes,
                                                 "User-Specified Rated Air Flow Rate [m3/s]",
                                                 RatedAirVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(RatedAirVolFlowRateDes - RatedAirVolFlowRateUser) / RatedAirVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeVarSpeedCoil: Potential issue with equipment sizing for" +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                            ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                            ShowContinueError(state, format("User-Specified Rated Air Flow Rate of {:.5R} [m3/s]", RatedAirVolFlowRateUser));
                            ShowContinueError(state, format("differs from Design Size Rated Air Flow Rate of {:.5R} [m3/s]", RatedAirVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(state,
                                                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                                                                               RatedAirVolFlowRateDes,
                                                                               RatedAirFlowAutoSized);
        }

        // Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING" &&
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum > 0) {

            if (state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                    .RatedCapCoolTotal > 0.0) {

                if (std::abs(state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                                 .RatedCapCoolTotal -
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat) /
                        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                            .RatedCapCoolTotal >
                    0.2) {

                    ShowWarningError(state,
                                     "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType +
                                         ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" +
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "...used with COIL:" +
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                                .CoolHeatType +
                            ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" +
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                                .Name +
                            "\"");
                    ShowContinueError(state, "...heating capacity is disproportionate (> 20% different) to total cooling capacity");
                    ShowContinueError(state,
                                      format("...heating capacity = {:.3T} W", state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat));
                    ShowContinueError(state,
                                      format("...cooling capacity = {:.3T} W",
                                             state.dataVariableSpeedCoils
                                                 ->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                                                 .RatedCapCoolTotal));
                }
            }
        }

        // ASSIGN CAPACITY
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal /
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(NormSpeed);
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat /
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(NormSpeed);
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH /
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(NormSpeed);
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            HPInletAirHumRat = PsyWFnTdbTwbPb(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp,
                                              state.dataEnvrn->StdBaroPress,
                                              RoutineName);
            rhoA = PsyRhoAirFnPbTdbW(state,
                                     state.dataEnvrn->OutBaroPress,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp,
                                     HPInletAirHumRat,
                                     RoutineName);

            for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(Mode);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
                // EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) = 0.0;
            }
        } else {
            rhoA = PsyRhoAirFnPbTdbW(state,
                                     state.dataEnvrn->OutBaroPress,
                                     state.dataVariableSpeedCoils->RatedInletAirTemp,
                                     state.dataVariableSpeedCoils->RatedInletAirHumRat,
                                     RoutineName);
            // HPWH, the mass flow rate will be updated by a revised entering air density

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSHPDesignSpecIndex > -1 &&
                state.dataUnitarySystems->designSpecMSHP.size() > 0) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
                    if (state.dataUnitarySystems->designSpecMSHP[state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSHPDesignSpecIndex]
                            .numOfSpeedCooling != state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                        ShowFatalError(state,
                                       "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix + " = " +
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                           " number of speeds not equal to number of speed specified in UnitarySystemPerformance:Multispeed object.");
                    } else {
                        for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate *
                                state.dataUnitarySystems->designSpecMSHP[state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSHPDesignSpecIndex]
                                    .coolingVolFlowRatio[Mode - 1];
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
                            // EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedEvapCondVolFlowPerRatedTotCap(Mode);
                        }
                    }
                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {
                    if (state.dataUnitarySystems->designSpecMSHP[state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSHPDesignSpecIndex]
                            .numOfSpeedHeating != state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
                        ShowFatalError(state,
                                       "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix + " = " +
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                           " number of speeds not equal to number of speed specified in UnitarySystemPerformance:Multispeed object.");
                    } else {
                        for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate *
                                state.dataUnitarySystems->designSpecMSHP[state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSHPDesignSpecIndex]
                                    .heatingVolFlowRatio[Mode - 1];
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
                            // EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) =
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedEvapCondVolFlowPerRatedTotCap(Mode);
                        }
                    }
                }
            } else {
                for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(Mode);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
                    // EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) =
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedEvapCondVolFlowPerRatedTotCap(Mode);
                }
            }
        }

        // size rated power
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPCool =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedPowerCool =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal /
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPCool;

        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPHeat =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedPowerHeat =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat /
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPHeat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat;
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPHeat =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedPowerHeat =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPHeat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapWH *
                (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCOPHeat);
        }

        // Size water volumetric flow rate
        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate == AutoSize) &&
            (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit ||
             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingWaterToAirHPVSEquationFit)) {
            RatedWaterFlowAutoSized = true;
        }

        //!   if not found on a plant loop, check condenser loop and warn user if not found
        //    IF(PltSizNum == 0) THEN
        //      PltSizNum = &
        //          MyCondPlantSizingIndex('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)// &
        //               TRIM(CurrentObjSubfix), &
        //                                 VarSpeedCoil(DXCoilNum)%Name, &
        //                                 VarSpeedCoil(DXCoilNum)%WaterInletNodeNum, &
        //                                 VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound)
        //    END IF

        //   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
        //   first check to see if coil is connected to a plant loop, no warning on this CALL
        if (RatedWaterFlowAutoSized) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType == DataHeatBalance::RefrigCondenserType::Water)
                PltSizNum = MyPlantSizingIndex(state,
                                               "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum,
                                               ErrorsFound,
                                               false);

            if (PltSizNum > 0) {
                rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                       state.dataSize->PlantSizData(PltSizNum).ExitTemp,
                                       state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                       RoutineNameAlt);
                cp = GetSpecificHeatGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                           state.dataSize->PlantSizData(PltSizNum).ExitTemp,
                                           state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                           RoutineNameAlt);

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "HEATING") {

                    RatedWaterVolFlowRateDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat /
                                               (state.dataSize->PlantSizData(PltSizNum).DeltaT * cp * rho);

                    //        CALL BaseSizer::reportSizerOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//&
                    //                                TRIM(CurrentObjSubfix), &
                    //                                  VarSpeedCoil(DXCoilNum)%Name, &
                    //                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgWaterTemp(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataSize->PlantSizData(PltSizNum).ExitTemp +
                            state.dataSize->PlantSizData(PltSizNum).DeltaT); // TRACE 3D Plus coil selection report

                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {

                    //       use companion heating coil capacity to calculate volumetric flow rate
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum > 0) {
                        SystemCapacity =
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CompanionCoolingCoilNum)
                                .RatedCapHeat;
                    } else {
                        SystemCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal;
                    }

                    RatedWaterVolFlowRateDes = SystemCapacity / (state.dataSize->PlantSizData(PltSizNum).DeltaT * cp * rho);

                    //        CALL BaseSizer::reportSizerOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)&
                    //                                //TRIM(CurrentObjSubfix), &
                    //                                  VarSpeedCoil(DXCoilNum)%Name, &
                    //                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgWaterTemp(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataSize->PlantSizData(PltSizNum).ExitTemp -
                            state.dataSize->PlantSizData(PltSizNum).DeltaT); // TRACE 3D Plus coil selection report
                }

                state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntWaterTemp(
                    state,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                    state.dataSize->PlantSizData(PltSizNum).ExitTemp); // TRACE 3D Plus coil selection report

                state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterDeltaT(
                    state,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                    state.dataSize->PlantSizData(PltSizNum).DeltaT); // TRACE 3D Plus coil selection report
            } else {
                ShowSevereError(state, "Autosizing of water flow requires a loop Sizing:Plant object");
                ShowContinueError(state, "Autosizing also requires physical connection to a plant or condenser loop.");
                ShowContinueError(state,
                                  "Occurs in COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix +
                                      " Object=" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                ErrorsFound = true;
            }
        }

        // WRITE THE WATER SIZING OUTPUT
        if (RatedWaterFlowAutoSized) {
            // FORCE BACK TO THE RATED WATER FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATLOG DATA
            if (RatedCapCoolTotalAutoSized) {
                RatedWaterVolFlowRateDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal *
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(NormSpeed);
            } else if (RatedCapHeatAutoSized) {
                RatedWaterVolFlowRateDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat *
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(NormSpeed);
            }
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(
                state,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                RatedWaterVolFlowRateDes,
                RatedWaterFlowAutoSized,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum,
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate = RatedWaterVolFlowRateDes;
            BaseSizer::reportSizerOutput(state,
                                         "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                         "Design Size Rated Water Flow Rate [m3/s]",
                                         RatedWaterVolFlowRateDes);
            // Ensure water flow rate at lower speed must be lower or
            // equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
            for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds - 1; ++Mode) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) >
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode + 1) * 1.05) {
                    ShowWarningError(
                        state,
                        format("SizeDXCoil: {} {}, Speed {} Rated Air Flow Rate must be less than or equal to Speed {} Rated Air Flow Rate.",
                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                               Mode,
                               Mode + 1));
                    ShowContinueError(state,
                                      format("Instead, {:.2R} > {:.2R}",
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode),
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode + 1)));
                    ShowFatalError(state, "Preceding conditions cause termination.");
                }
            }
        } else {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate > 0.0 && RatedWaterVolFlowRateDes > 0.0) {
                RatedWaterVolFlowRateUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate;
                BaseSizer::reportSizerOutput(state,
                                             "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Rated Water Flow Rate [m3/s]",
                                             RatedWaterVolFlowRateDes,
                                             "User-Specified Rated Water Flow Rate [m3/s]",
                                             RatedWaterVolFlowRateUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(RatedWaterVolFlowRateDes - RatedWaterVolFlowRateUser) / RatedWaterVolFlowRateUser) >
                        state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state,
                                    "SizeVarSpeedCoil: Potential issue with equipment sizing for" +
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                        ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                        ShowContinueError(state, format("User-Specified Rated Water Flow Rate of {:.5R} [m3/s]", RatedWaterVolFlowRateUser));
                        ShowContinueError(state, format("differs from Design Size Rated Water Flow Rate of {:.5R} [m3/s]", RatedWaterVolFlowRateDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }

        // Save component design water volumetric flow rate.
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate > 0.0 &&
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            RegisterPlantCompDesignFlow(state,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate);
        }
        // Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
        else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate > 0.0) {
            RegisterPlantCompDesignFlow(state,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum,
                                        0.5 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate);
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit ||
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingWaterToAirHPVSEquationFit) {

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit) {
                RatedSourceTempCool = state.dataVariableSpeedCoils->RatedInletWaterTemp;
            } else {
                RatedSourceTempCool = state.dataVariableSpeedCoils->RatedInletWaterTempHeat;
            }

            if (PltSizNum > 0) {
                rhoW = rho;
            } else {
                rhoW = GetDensityGlycol(state,
                                        state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                        RatedSourceTempCool,
                                        state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                        RoutineName);
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate * rhoW;
            for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(Mode);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) * rhoW;
            }
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            RatedSourceTempCool = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp;
            rhoW = RhoH2O(RatedSourceTempCool);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate * rhoW;
            for (Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(Mode);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPowerPerRatedTotCap(Mode);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) * rhoW;
            }
        } else {
            RatedSourceTempCool = state.dataVariableSpeedCoils->RatedAmbAirTemp;
        }

        // Ensure air flow rate at lower speed must be lower or
        // equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
        for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds - 1; ++Mode) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode + 1)) {
                ShowWarningError(state,
                                 format("SizeDXCoil: {} {}, Speed {} Rated Air Flow Rate must be less than or equal to Speed {} Rated Air Flow Rate.",
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        Mode,
                                        Mode + 1));
                ShowContinueError(state,
                                  format("Instead, {:.2R} > {:.2R}",
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode),
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode + 1)));
                ShowFatalError(state, "Preceding conditions cause termination.");
            }
        }

        // Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
        for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds - 1; ++Mode) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode + 1)) {
                ShowWarningError(state,
                                 format("SizeDXCoil: {} {}, Speed {} Rated Total Cooling Capacity must be less than or equal to Speed {} Rated Total "
                                        "Cooling Capacity.",
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                        Mode,
                                        Mode + 1));
                ShowContinueError(state,
                                  format("Instead, {:.2R} > {:.2R}",
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode),
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode + 1)));
                ShowFatalError(state, "Preceding conditions cause termination.");
            }
        }

        // convert SHR to rated Bypass factor and effective air side surface area
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
            for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) =
                    CalcCBF(state,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                            state.dataVariableSpeedCoils->RatedInletAirTemp,
                            state.dataVariableSpeedCoils->RatedInletAirHumRat,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode),
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode),
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(Mode),
                            true);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) > 0.0) {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) =
                        -std::log(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode)) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode);
                } else {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) = 0.0;
                }
            }
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            state.dataHVACGlobal->HPWHInletDBTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp;
            state.dataHVACGlobal->HPWHInletWBTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp;

            rhoA = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, state.dataHVACGlobal->HPWHInletDBTemp, HPInletAirHumRat, RoutineName);
            for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
                // Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
                // RatedVolFlowPerRatedTotCap = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
                //    VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode);//prepared for checking ratios, not used here
            }

            for (Mode = 1; Mode <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
                // get cooling capacity, without fan power, i.e. total coil cooling
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP)
                    HPWHCoolCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                                           (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) +
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) -
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
                else
                    HPWHCoolCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                                           (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) -
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;

                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) =
                    CalcCBF(state,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                            state.dataHVACGlobal->HPWHInletDBTemp,
                            HPInletAirHumRat,
                            HPWHCoolCapacity,
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode),
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(Mode),
                            true);
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) > 0.0) {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) =
                        -std::log(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode)) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode);
                } else {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) = 0.0;
                }
            }

            // update VarSpeedCoil(DXCoilNum).RatedCapCoolTotal
            Mode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel;
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                        (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) +
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) -
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
                        (1.0 - 1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) -
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
            }
        }

        // size rated sensible cooling capacity
        RatedCapCoolSensAutoSized = true; // always do that

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate >= SmallAirVolFlow &&
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") {
            RatedAirMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate *
                                   PsyRhoAirFnPbTdbW(state,
                                                     state.dataEnvrn->StdBaroPress,
                                                     state.dataVariableSpeedCoils->RatedInletAirTemp,
                                                     state.dataVariableSpeedCoils->RatedInletAirHumRat,
                                                     RoutineName);
            RatedInletEnth = PsyHFnTdbW(state.dataVariableSpeedCoils->RatedInletAirTemp, state.dataVariableSpeedCoils->RatedInletAirHumRat);
            CBFRated = AdjustCBF(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(NormSpeed),
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(NormSpeed),
                                 RatedAirMassFlowRate);
            if (CBFRated > 0.999) CBFRated = 0.999;
            AirMassFlowRatio = RatedAirMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(NormSpeed);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed) > 1.0e-10) {
                WaterMassFlowRatio = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate /
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed);
            } else {
                WaterMassFlowRatio = 1.0;
            }

            Real64 TempInletWetBulb = state.dataVariableSpeedCoils->RatedInletWetBulbTemp;
            CalcTotCapSHR_VSWSHP(state,
                                 state.dataVariableSpeedCoils->RatedInletAirTemp,
                                 state.dataVariableSpeedCoils->RatedInletAirHumRat,
                                 RatedInletEnth,
                                 TempInletWetBulb,
                                 AirMassFlowRatio,
                                 WaterMassFlowRatio,
                                 RatedAirMassFlowRate,
                                 CBFRated,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed),
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(NormSpeed),
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(NormSpeed),
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(NormSpeed),
                                 0.0,
                                 0,
                                 0,
                                 0,
                                 QLoadTotal1,
                                 QLoadTotal2,
                                 QLoadTotal,
                                 SHR,
                                 RatedSourceTempCool,
                                 state.dataEnvrn->StdBaroPress,
                                 0.0,
                                 1,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).capModFacTotal);

            RatedCapCoolSensDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal * SHR;
        } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate >= SmallAirVolFlow &&
                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
            SHR = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedSHR(NormSpeed);
            RatedCapCoolSensDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal * SHR;
        } else {
            RatedCapCoolSensDes = 0.0;
        }

        if (RatedCapCoolSensDes < SmallLoad) {
            RatedCapCoolSensDes = 0.0;
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType == "COOLING") { // always report for cooling mode
            if (RatedCapCoolTotalAutoSized) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens = RatedCapCoolSensDes;
                BaseSizer::reportSizerOutput(state,
                                             "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Rated Sensible Cooling Capacity [W]",
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens);

            } else {
                // sensible capacity does not have an input field
                if (RatedCapCoolSensDes > 0.0) {
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens = RatedCapCoolSensDes;
                    BaseSizer::reportSizerOutput(state,
                                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + CurrentObjSubfix,
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "Design Size Rated Sensible Cooling Capacity [W]",
                                                 RatedCapCoolSensDes); //, &
                }
            }
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchCoolCoilTotCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchCoolCoilSensCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchCoolCoilLatCap,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal -
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens);
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal != 0.0) {
                PreDefTableEntry(state,
                                 state.dataOutRptPredefined->pdchCoolCoilSHR,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens /
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal);
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilSHR, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name, 0.0);
            }
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchCoolCoilNomEff,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel));
            addFootNoteSubTable(state,
                                state.dataOutRptPredefined->pdstCoolCoil,
                                "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
        }

        // START SIZING EVAP PRECOOLING PUMP POWER
        IsAutoSize = false;
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower == AutoSize) {
                IsAutoSize = true;
            }
            //     Auto size high speed evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
            EvapCondPumpElecNomPowerDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal * 0.004266;
            if (IsAutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower = EvapCondPumpElecNomPowerDes;
                BaseSizer::reportSizerOutput(state,
                                             "AS VS COOLING COIL",
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Evaporative Condenser Pump Rated Power Consumption [W]",
                                             EvapCondPumpElecNomPowerDes);
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower > 0.0 && EvapCondPumpElecNomPowerDes > 0.0) {
                    EvapCondPumpElecNomPowerUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower;
                    BaseSizer::reportSizerOutput(state,
                                                 "AS VS COOLING COIL",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "Design Size Evaporative Condenser Pump Rated Power Consumption [W]",
                                                 EvapCondPumpElecNomPowerDes,
                                                 "User-Specified Evaporative Condenser Pump Rated Power Consumption [W]",
                                                 EvapCondPumpElecNomPowerUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(EvapCondPumpElecNomPowerDes - EvapCondPumpElecNomPowerUser) / EvapCondPumpElecNomPowerUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeVarSpeedCoil: Potential issue with equipment sizing for " +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                            ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                            ShowContinueError(state,
                                              format("User-Specified Evaporative Condenser Pump Rated Power Consumption of {:.2R} [W]",
                                                     EvapCondPumpElecNomPowerUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Evaporative Condenser Pump Rated Power Consumption of {:.2R} [W]",
                                                     EvapCondPumpElecNomPowerDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
        // END SIZING EVAP PRE-COOLING PUMP POWER

        // SIZE DEFROST HEATER

        // Resistive Defrost Heater Capacity = capacity at the first stage
        IsAutoSize = false;
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity == AutoSize) {
                IsAutoSize = true;
            }
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy == state.dataVariableSpeedCoils->Resistive) {
                DefrostCapacityDes = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat;
            } else {
                DefrostCapacityDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity = DefrostCapacityDes;
                BaseSizer::reportSizerOutput(state,
                                             "AS VS HEATING COIL",
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                             "Design Size Resistive Defrost Heater Capacity [W]",
                                             DefrostCapacityDes);
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity > 0.0 && DefrostCapacityDes > 0.0 && !HardSizeNoDesRun) {
                    DefrostCapacityUser = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity;
                    BaseSizer::reportSizerOutput(state,
                                                 "AS VS HEATING COIL",
                                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                                                 "Design Size Resistive Defrost Heater Capacity [W]",
                                                 DefrostCapacityDes,
                                                 "User-Specified Resistive Defrost Heater Capacity [W]",
                                                 DefrostCapacityUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(DefrostCapacityDes - DefrostCapacityUser) / DefrostCapacityUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeVarSpeedCoil: Potential issue with equipment sizing for " +
                                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType + ' ' + CurrentObjSubfix);
                            ShowContinueError(state, "Coil Name =" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name);
                            ShowContinueError(state, format("User-Specified Resistive Defrost Heater Capacity of {:.2R} [W]", DefrostCapacityUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Resistive Defrost Heater Capacity of {:.2R} [W]", DefrostCapacityDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
        // END SIZING DEFROST HEATER

        // test autosized sensible and total cooling capacity for total > sensible
        if (RatedCapCoolSensAutoSized && RatedCapCoolTotalAutoSized) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal) {
                ShowWarningError(state,
                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType +
                                     ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                     "\"");
                ShowContinueError(state, std::string{RoutineName} + ": Rated Sensible Cooling Capacity > Rated Total Cooling Capacity");
                ShowContinueError(state, "Each of these capacity inputs have been autosized.");
                ShowContinueError(
                    state,
                    format("Rated Sensible Cooling Capacity = {:.2T} W", state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens));
                ShowContinueError(
                    state,
                    format("Rated Total Cooling Capacity    = {:.2T} W", state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal));
                ShowContinueError(state, "See eio file for further details.");
                ShowContinueError(state, "Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.");
                ShowContinueError(state, "Check Zone and System Sizing objects to verify sizing inputs.");
                ShowContinueError(state, "Sizing statistics:");
                ShowContinueError(state, format("Entering Air Dry-Bulb Temperature = {:.3T} C", MixTemp));
                ShowContinueError(state, format("Entering Air Wet-Bulb Temperature = {:.3T} C", MixWetBulb));
                ShowContinueError(state, "Entering Condenser Water Temperature used = 24.4444 C");
                ShowContinueError(state, "Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)");
                ShowContinueError(state, format("ratioTDB = {:.3T}", ((MixTemp + 283.15) / 273.15)));
                ShowContinueError(state, format("ratioTWB = {:.3T}", ((MixWetBulb + 283.15) / 273.15)));
                ShowContinueError(state, format("ratioTS  = {:.3T}", ((85.0 + 283.15) / 273.15)));
                ShowContinueError(state, "Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio");
                ShowContinueError(state, format("Total Cooling Capacity Modifier = {:.5T}", TotCapTempModFac));
                ShowContinueError(state, "...Rated Total Cooling Capacity = Total Design Load / Total Cooling Capacity Modifier");
                ShowContinueError(state, "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates");
                ShowContinueError(state, "... to ensure they meet the expected manufacturers performance specifications.");
            }
        } else if (RatedCapCoolTotalAutoSized) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens >
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal) {
                ShowWarningError(state,
                                 "COIL:" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CoolHeatType +
                                     ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name +
                                     "\"");
                ShowContinueError(state, std::string{RoutineName} + ": Rated Sensible Cooling Capacity > Rated Total Cooling Capacity");
                ShowContinueError(state, "Only the rated total capacity input is autosized, consider autosizing both inputs.");
                ShowContinueError(
                    state,
                    format("Rated Sensible Cooling Capacity = {:.2T} W", state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens));
                ShowContinueError(
                    state,
                    format("Rated Total Cooling Capacity    = {:.2T} W", state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal));
                ShowContinueError(state, "See eio file for further details.");
                ShowContinueError(state, "Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.");
                ShowContinueError(state, "Check Zone and System Sizing objects to verify sizing inputs.");
                ShowContinueError(state, "Sizing statistics for Total Cooling Capacity:");
                ShowContinueError(state, format("Entering Air Wet-Bulb Temperature = {:.3T} C", MixWetBulb));
                ShowContinueError(state, "Entering Condenser Water Temperature used = 24.4444 C");
                ShowContinueError(state, "Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)");
                ShowContinueError(state, format("ratioTWB = {:.3T}", ((MixWetBulb + 283.15) / 273.15)));
                ShowContinueError(state, format("ratioTS  = {:.3T}", ((85.0 + 283.15) / 273.15)));
                ShowContinueError(state, "Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio");
                ShowContinueError(state, "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates");
                ShowContinueError(state, "... to ensure they meet the expected manufacturers performance specifications.");
            }
        }
    }

    void CalcVarSpeedCoilCooling(EnergyPlusData &state,
                                 int const DXCoilNum,                        // Heat Pump Number
                                 int const CyclingScheme,                    // Fan/Compressor cycling scheme indicator
                                 Real64 &RuntimeFrac,                        // Runtime Fraction of compressor or percent on time (on-time/cycle time)
                                 [[maybe_unused]] Real64 const SensDemand,   // Cooling Sensible Demand [W] !unused1208
                                 [[maybe_unused]] Real64 const LatentDemand, // Cooling Latent Demand [W]
                                 int const CompOp,                           // compressor operation flag
                                 Real64 const PartLoadRatio,                 // compressor part load ratio
                                 [[maybe_unused]] Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
                                 Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
                                 int const SpeedNum       // Speed number, high bound
    )
    {

        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcHPCoolingSimple
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for simulating the cooling mode of the Variable-Speed Water to Air HP Simple

        // METHODOLOGY EMPLOYED:
        // Simulate the heat pump performance using the coefficients and rated conditions, interpolating between speed levels
        // If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
        // (1)first simulation at the rated conditions (2) second simulation at the
        // actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
        // is adjusted.
        // If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
        // once at the actual operating conditions.
        // Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
        // and RuntimeFrac.

        // Using/Aliasing
        using CurveManager::CurveValue;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using FluidProperties::GetSpecificHeatGlycol;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTdbFnHW;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbH;
        using Psychrometrics::PsyWFnTdbTwbPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcVarSpeedCoilCooling");
        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcVarSpeedCoilCooling:SourceSideInletTemp");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 Twet_Rated;  // Twet at rated conditions (coil air flow rate and air temperatures), sec
        Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)

        Real64 SHRss;    // Sensible heat ratio at steady state
        Real64 SHReff;   // Effective sensible heat ratio at part-load condition
        Real64 CpSource; // Specific heat of water [J/kg_C]
        Real64 CpAir;    // Specific heat of air [J/kg_C]
        Real64 ReportingConstant;

        bool LatDegradModelSimFlag;      // Latent degradation model simulation flag
        int NumIteration;                // Iteration Counter
        Real64 LoadSideInletDBTemp_Unit; // calc conditions for unit
        Real64 LoadSideInletWBTemp_Unit; // calc conditions for unit
        Real64 LoadSideInletHumRat_Unit; // calc conditions for unit
        Real64 LoadSideInletEnth_Unit;   // calc conditions for unit
        Real64 CpAir_Unit;               // calc conditions for unit
        Real64 AirMassFlowRatio;         // airflow ratio at low speed
        Real64 WaterMassFlowRatio;       // airflow ratio at high speed
        Real64 EIRAirFFModFac;           // air flow fraction modification
        Real64 EIRWaterFFModFac;         // water flow fraction modification
        Real64 EIRTempModFac;            // total capacity temperature correctio fraction
        Real64 CBFSpeed;                 // total capacity temperature correctio fraction
        Real64 SHR;                      // total capacity temperature correctio fraction
        Real64 EIR;                      // total capacity temperature correctio fraction
        int MaxSpeed;                    // maximum speed level
        int SpeedCal;                    // calculated speed level
        Real64 AoEff;                    // effective air side surface area
        Real64 QLoadTotal1;              // total capacity at low speed
        Real64 QLoadTotal2;              // total capacity at high speed
        Real64 Winput1;                  // power consumption at low speed
        Real64 Winput2;                  // power consumption at high speed
        Real64 QWasteHeat;               // recoverable waste heat
        Real64 QWasteHeat1;              // recoverable waste heat at low speed
        Real64 QWasteHeat2;              // recoverable waste heat at high speed
        Real64 PLF;                      // part-load function
        Real64 MaxHumRat;                // max possible humidity
        Real64 MaxOutletEnth;            // max possible outlet enthalpy

        // ADDED VARIABLES FOR air source coil
        Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
        // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
        Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
        // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
        Real64 CondAirMassFlow;    // Condenser air mass flow rate [kg/s]
        Real64 RhoSourceAir;       // Density of air [kg/m3]
        Real64 RhoEvapCondWater;   // Density of water used for evaporative condenser [kg/m3]
        Real64 EvapCondEffectSped; // condenser evaporative effectiveness at the speed level
        Real64 RhoWater;           // condensed water density
        Real64 SpecHumIn;          // inlet air specific humidity
        Real64 SpecHumOut;         // outlet air specific humidity
        Real64 rhoair(0);          // entering air density

        if (state.dataVariableSpeedCoils->firstTime) {
            // Set indoor air conditions to the rated condition
            state.dataVariableSpeedCoils->LoadSideInletDBTemp_Init = 26.7;
            state.dataVariableSpeedCoils->LoadSideInletHumRat_Init = 0.0111;
            state.dataVariableSpeedCoils->LoadSideInletEnth_Init =
                PsyHFnTdbW(state.dataVariableSpeedCoils->LoadSideInletDBTemp_Init, state.dataVariableSpeedCoils->LoadSideInletHumRat_Init);
            state.dataVariableSpeedCoils->CpAir_Init = PsyCpAirFnW(state.dataVariableSpeedCoils->LoadSideInletHumRat_Init);
            state.dataVariableSpeedCoils->firstTime = false;
        }
        state.dataVariableSpeedCoils->LoadSideInletWBTemp_Init = PsyTwbFnTdbWPb(state,
                                                                                state.dataVariableSpeedCoils->LoadSideInletDBTemp_Init,
                                                                                state.dataVariableSpeedCoils->LoadSideInletHumRat_Init,
                                                                                state.dataEnvrn->OutBaroPress,
                                                                                RoutineName);

        MaxSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;

        // must be placed inside the loop, otherwise cause bug in release mode, need to be present at two places
        if (SpeedNum > MaxSpeed) {
            SpeedCal = MaxSpeed;
        } else {
            SpeedCal = SpeedNum;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        if (!(CyclingScheme == ContFanCycCoil) && PartLoadRatio > 0.0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate =
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum).MassFlowRate / PartLoadRatio;
        }

        Twet_Rated = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Twet_Rated;
        Gamma_Rated = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Gamma_Rated;

        state.dataVariableSpeedCoils->LoadSideMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
            // Get condenser outdoor node info from DX COOLING Coil
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum != 0) {
                state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Temp;
                state.dataVariableSpeedCoils->OutdoorHumRat_CalcVarSpeedCoilCooling =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).HumRat;
                state.dataVariableSpeedCoils->OutdoorPressure_CalcVarSpeedCoilCooling =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Press;
                state.dataVariableSpeedCoils->OutdoorWetBulb_CalcVarSpeedCoilCooling =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).OutAirWetBulb;
            } else {
                state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling = state.dataEnvrn->OutDryBulbTemp;
                state.dataVariableSpeedCoils->OutdoorHumRat_CalcVarSpeedCoilCooling = state.dataEnvrn->OutHumRat;
                state.dataVariableSpeedCoils->OutdoorPressure_CalcVarSpeedCoilCooling = state.dataEnvrn->OutBaroPress;
                state.dataVariableSpeedCoils->OutdoorWetBulb_CalcVarSpeedCoilCooling = state.dataEnvrn->OutWetBulbTemp;
            }

            RhoSourceAir = PsyRhoAirFnPbTdbW(state,
                                             state.dataVariableSpeedCoils->OutdoorPressure_CalcVarSpeedCoilCooling,
                                             state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling,
                                             state.dataVariableSpeedCoils->OutdoorHumRat_CalcVarSpeedCoilCooling);

            if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
                CondAirMassFlow = RhoSourceAir * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(SpeedCal);
            } else {
                CondAirMassFlow =
                    RhoSourceAir * (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(SpeedCal) * SpeedRatio +
                                    (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondAirFlow(SpeedCal - 1));
            }

            // AIR COOL OR EVAP COOLED CONDENSER
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType == DataHeatBalance::RefrigCondenserType::Evap) {
                if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
                    EvapCondEffectSped = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(SpeedCal);
                } else {
                    EvapCondEffectSped = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(SpeedCal) * SpeedRatio +
                                         (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondEffect(SpeedCal - 1);
                }
                // (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
                CondInletTemp = state.dataVariableSpeedCoils->OutdoorWetBulb_CalcVarSpeedCoilCooling +
                                (state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling -
                                 state.dataVariableSpeedCoils->OutdoorWetBulb_CalcVarSpeedCoilCooling) *
                                    (1.0 - EvapCondEffectSped);
                CondInletHumRat = PsyWFnTdbTwbPb(state,
                                                 CondInletTemp,
                                                 state.dataVariableSpeedCoils->OutdoorWetBulb_CalcVarSpeedCoilCooling,
                                                 state.dataVariableSpeedCoils->OutdoorPressure_CalcVarSpeedCoilCooling);
                state.dataVariableSpeedCoils->CompAmbTemp_CalcVarSpeedCoilCooling = CondInletTemp;
            } else {                                                                                  // AIR COOLED CONDENSER
                CondInletTemp = state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling; // Outdoor dry-bulb temp
                state.dataVariableSpeedCoils->CompAmbTemp_CalcVarSpeedCoilCooling =
                    state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling;
                CondInletHumRat = state.dataEnvrn->OutHumRat;
            }

            state.dataVariableSpeedCoils->SourceSideMassFlowRate = CondAirMassFlow;
            state.dataVariableSpeedCoils->SourceSideInletTemp = CondInletTemp;
            state.dataVariableSpeedCoils->SourceSideInletEnth = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CpSource = PsyCpAirFnW(CondInletHumRat);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondInletTemp = CondInletTemp;

            // If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
            // Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
            if (state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling <
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater) {
                state.dataVariableSpeedCoils->CrankcaseHeatingPower_CalcVarSpeedCoilCooling =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity;
            } else {
                state.dataVariableSpeedCoils->CrankcaseHeatingPower_CalcVarSpeedCoilCooling = 0.0;
            }
        } else {
            state.dataVariableSpeedCoils->SourceSideMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate;
            state.dataVariableSpeedCoils->SourceSideInletTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp;
            state.dataVariableSpeedCoils->SourceSideInletEnth = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy;
            CpSource = GetSpecificHeatGlycol(state,
                                             state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                             state.dataVariableSpeedCoils->SourceSideInletTemp,
                                             state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                             RoutineNameSourceSideInletTemp);
        }

        // Check for flows, do not perform simulation if no flow in load side or source side.
        if (state.dataVariableSpeedCoils->SourceSideMassFlowRate <= 0.0 || state.dataVariableSpeedCoils->LoadSideMassFlowRate <= 0.0) {

            if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) &&
                (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType == DataHeatBalance::RefrigCondenserType::Air) &&
                (state.dataVariableSpeedCoils->LoadSideMassFlowRate > 0.0)) {
                // ALLOW SIMULATION IF AIR-COOLED CONDENSER COIL
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = true;
            } else {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
                return;
            }
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = true;
        }

        if (CompOp == 0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        }

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) &&
            (CondInletTemp < state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MinOATCompressor)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        }

        // Loop the calculation at least once depending whether the latent degradation model
        // is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
        // and 2nd iteration to calculate the  QLatent(actual)
        if ((PartLoadRatio < 1e-10) || (Twet_Rated <= 0.0) || (Gamma_Rated <= 0.0) || (SpeedNum > 1.0)) {
            LatDegradModelSimFlag = false;
            // Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
            NumIteration = 1;
        } else {
            LatDegradModelSimFlag = true;
            // Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
            NumIteration = 0;
        }

        // Set indoor air conditions to the actual condition
        LoadSideInletDBTemp_Unit = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp;
        LoadSideInletHumRat_Unit = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat;
        LoadSideInletWBTemp_Unit =
            PsyTwbFnTdbWPb(state, LoadSideInletDBTemp_Unit, LoadSideInletHumRat_Unit, state.dataEnvrn->OutBaroPress, RoutineName);
        LoadSideInletEnth_Unit = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy;
        CpAir_Unit = PsyCpAirFnW(LoadSideInletHumRat_Unit);

        RuntimeFrac = 1.0;
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0;
        if ((SpeedNum == 1) && (PartLoadRatio < 1.0)) {
            PLF = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, PartLoadRatio);
            if (PLF < 0.7) {
                PLF = 0.7;
            }
            if (CyclingScheme == CycFanCycCoil)
                state.dataHVACGlobal->OnOffFanPartLoadFraction =
                    PLF; // save PLF for fan model, don't change fan power for constant fan mode if coil is off
            // calculate the run time fraction
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = PartLoadRatio / PLF;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac > 1.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
            } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac < 0.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
            }

            RuntimeFrac = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac;
        }

        while (true) {
            ++NumIteration;
            if (NumIteration == 1) {
                // Set indoor air conditions to the rated conditions
                state.dataVariableSpeedCoils->LoadSideInletDBTemp = state.dataVariableSpeedCoils->LoadSideInletDBTemp_Init;
                state.dataVariableSpeedCoils->LoadSideInletHumRat = state.dataVariableSpeedCoils->LoadSideInletHumRat_Init;
                state.dataVariableSpeedCoils->LoadSideInletWBTemp = state.dataVariableSpeedCoils->LoadSideInletWBTemp_Init;
                state.dataVariableSpeedCoils->LoadSideInletEnth = state.dataVariableSpeedCoils->LoadSideInletEnth_Init;
                CpAir = state.dataVariableSpeedCoils->CpAir_Init;
            } else {
                // Set indoor air conditions to the actual condition
                state.dataVariableSpeedCoils->LoadSideInletDBTemp = LoadSideInletDBTemp_Unit;
                state.dataVariableSpeedCoils->LoadSideInletHumRat = LoadSideInletHumRat_Unit;
                state.dataVariableSpeedCoils->LoadSideInletWBTemp = LoadSideInletWBTemp_Unit;
                state.dataVariableSpeedCoils->LoadSideInletEnth = LoadSideInletEnth_Unit;
                CpAir = CpAir_Unit;
            }

            // must be placed inside the loop, otherwise cause bug in release mode
            if (SpeedNum > MaxSpeed) {
                SpeedCal = MaxSpeed;
            } else {
                SpeedCal = SpeedNum;
            }

            if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
                AirMassFlowRatio =
                    state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    WaterMassFlowRatio = 1.0;
                } else {
                    WaterMassFlowRatio = state.dataVariableSpeedCoils->SourceSideMassFlowRate /
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
                }

                CBFSpeed = AdjustCBF(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal),
                                     state.dataVariableSpeedCoils->LoadSideMassFlowRate);

                if (CBFSpeed > 0.999) CBFSpeed = 0.999;

                CalcTotCapSHR_VSWSHP(state,
                                     state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                     state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                     state.dataVariableSpeedCoils->LoadSideInletEnth,
                                     state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                     AirMassFlowRatio,
                                     WaterMassFlowRatio,
                                     state.dataVariableSpeedCoils->LoadSideMassFlowRate,
                                     CBFSpeed,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
                                     0.0,
                                     0,
                                     0,
                                     0,
                                     QLoadTotal1,
                                     QLoadTotal2,
                                     state.dataVariableSpeedCoils->QLoadTotal,
                                     SHR,
                                     state.dataVariableSpeedCoils->SourceSideInletTemp,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure,
                                     0.0,
                                     1,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).capModFacTotal);

                EIRTempModFac = CurveValue(state,
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                           state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                           state.dataVariableSpeedCoils->SourceSideInletTemp);
                EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    EIRWaterFFModFac = 1.0;
                } else {
                    EIRWaterFFModFac =
                        CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
                }

                EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                      EIRWaterFFModFac;

                CBFSpeed = AdjustCBF(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal),
                                     state.dataVariableSpeedCoils->LoadSideMassFlowRate);

                if (CBFSpeed > 0.999) CBFSpeed = 0.999;

                CalcTotCapSHR_VSWSHP(state,
                                     state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                     state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                     state.dataVariableSpeedCoils->LoadSideInletEnth,
                                     state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                     AirMassFlowRatio,
                                     WaterMassFlowRatio,
                                     state.dataVariableSpeedCoils->LoadSideMassFlowRate,
                                     CBFSpeed,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
                                     0.0,
                                     0,
                                     0,
                                     0,
                                     QLoadTotal1,
                                     QLoadTotal2,
                                     state.dataVariableSpeedCoils->QLoadTotal,
                                     SHR,
                                     state.dataVariableSpeedCoils->SourceSideInletTemp,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure,
                                     0.0,
                                     1,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).capModFacTotal);

                state.dataVariableSpeedCoils->Winput = state.dataVariableSpeedCoils->QLoadTotal * EIR;

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    QWasteHeat = 0.0;
                } else {
                    QWasteHeat =
                        state.dataVariableSpeedCoils->Winput * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                    QWasteHeat *= CurveValue(state,
                                             state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                             state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                             state.dataVariableSpeedCoils->SourceSideInletTemp);
                }
            } else {
                AirMassFlowRatio =
                    state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    WaterMassFlowRatio = 1.0;
                } else {
                    WaterMassFlowRatio = state.dataVariableSpeedCoils->SourceSideMassFlowRate /
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
                }

                AoEff = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal) * SpeedRatio +
                        (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal - 1);

                CBFSpeed = std::exp(-AoEff / state.dataVariableSpeedCoils->LoadSideMassFlowRate);

                if (CBFSpeed > 0.999) CBFSpeed = 0.999;

                CalcTotCapSHR_VSWSHP(state,
                                     state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                     state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                     state.dataVariableSpeedCoils->LoadSideInletEnth,
                                     state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                     AirMassFlowRatio,
                                     WaterMassFlowRatio,
                                     state.dataVariableSpeedCoils->LoadSideMassFlowRate,
                                     CBFSpeed,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal - 1),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal - 1),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal - 1),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal - 1),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
                                     QLoadTotal1,
                                     QLoadTotal2,
                                     state.dataVariableSpeedCoils->QLoadTotal,
                                     SHR,
                                     state.dataVariableSpeedCoils->SourceSideInletTemp,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure,
                                     SpeedRatio,
                                     2,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).capModFacTotal);

                SpeedCal = SpeedNum - 1;
                EIRTempModFac = CurveValue(state,
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                           state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                           state.dataVariableSpeedCoils->SourceSideInletTemp);
                EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    EIRWaterFFModFac = 1.0;
                } else {
                    EIRWaterFFModFac =
                        CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
                }

                EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                      EIRWaterFFModFac;
                Winput1 = QLoadTotal1 * EIR;

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    QWasteHeat1 = 0.0;
                } else {
                    QWasteHeat1 = Winput1 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                    QWasteHeat1 *= CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                              state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                              state.dataVariableSpeedCoils->SourceSideInletTemp);
                }

                SpeedCal = SpeedNum;
                EIRTempModFac = CurveValue(state,
                                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                           state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                           state.dataVariableSpeedCoils->SourceSideInletTemp);
                EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    EIRWaterFFModFac = 1.0;
                } else {
                    EIRWaterFFModFac =
                        CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
                }

                EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                      EIRWaterFFModFac;
                Winput2 = QLoadTotal2 * EIR;

                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
                    QWasteHeat2 = 0.0;
                } else {
                    QWasteHeat2 = Winput2 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                    QWasteHeat2 *= CurveValue(state,
                                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                              state.dataVariableSpeedCoils->LoadSideInletWBTemp,
                                              state.dataVariableSpeedCoils->SourceSideInletTemp);
                }

                state.dataVariableSpeedCoils->Winput = Winput2 * SpeedRatio + (1.0 - SpeedRatio) * Winput1;
                QWasteHeat = QWasteHeat2 * SpeedRatio + (1.0 - SpeedRatio) * QWasteHeat1;
            }

            state.dataVariableSpeedCoils->QSensible = state.dataVariableSpeedCoils->QLoadTotal * SHR;

            state.dataVariableSpeedCoils->QSource = state.dataVariableSpeedCoils->QLoadTotal + state.dataVariableSpeedCoils->Winput - QWasteHeat;

            if (state.dataVariableSpeedCoils->QSource < 0) {
                state.dataVariableSpeedCoils->QSource = 0.0;
                QWasteHeat = state.dataVariableSpeedCoils->QLoadTotal + state.dataVariableSpeedCoils->Winput;
            }

            // Check if the Sensible Load is greater than the Total Cooling Load
            if (state.dataVariableSpeedCoils->QSensible > state.dataVariableSpeedCoils->QLoadTotal) {
                state.dataVariableSpeedCoils->QSensible = state.dataVariableSpeedCoils->QLoadTotal;
            }

            if (LatDegradModelSimFlag) {
                // Calculate for SHReff using the Latent Degradation Model
                if (NumIteration == 1) {
                    state.dataVariableSpeedCoils->QLatRated = state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible;
                } else if (NumIteration == 2) {
                    state.dataVariableSpeedCoils->QLatActual = state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible;
                    SHRss = state.dataVariableSpeedCoils->QSensible / state.dataVariableSpeedCoils->QLoadTotal;
                    SHReff = CalcEffectiveSHR(state,
                                              DXCoilNum,
                                              SHRss,
                                              CyclingScheme,
                                              RuntimeFrac,
                                              state.dataVariableSpeedCoils->QLatRated,
                                              state.dataVariableSpeedCoils->QLatActual,
                                              state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                              state.dataVariableSpeedCoils->LoadSideInletWBTemp);
                    //       Update sensible capacity based on effective SHR
                    state.dataVariableSpeedCoils->QSensible = state.dataVariableSpeedCoils->QLoadTotal * SHReff;
                    goto LOOP_exit;
                }
            } else {
                // Assume SHReff=SHRss
                SHReff = state.dataVariableSpeedCoils->QSensible / state.dataVariableSpeedCoils->QLoadTotal;
                goto LOOP_exit;
            }
        }
    LOOP_exit:;

        // considering hot gas reheat here
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HOTGASREHEATFLG > 0) {
            state.dataVariableSpeedCoils->QLoadTotal -= QWasteHeat;
            state.dataVariableSpeedCoils->QSensible -= QWasteHeat;
            SHReff = state.dataVariableSpeedCoils->QSensible / state.dataVariableSpeedCoils->QLoadTotal;
        }

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower = 0.0;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserType == DataHeatBalance::RefrigCondenserType::Evap) {
                //******************
                //             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
                //             H2O [m3/s] = Delta W[kgWater/kgDryAir]*Mass Flow Air[kgDryAir/s]
                //                                /RhoWater [kgWater/m3]
                //******************
                RhoEvapCondWater = RhoH2O(state.dataVariableSpeedCoils->OutdoorDryBulb_CalcVarSpeedCoilCooling);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsumpRate =
                    (CondInletHumRat - state.dataVariableSpeedCoils->OutdoorHumRat_CalcVarSpeedCoilCooling) * CondAirMassFlow / RhoEvapCondWater *
                    RuntimeFrac;
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecPower =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecNomPower * RuntimeFrac;
                // Calculate basin heater power
                CalcBasinHeaterPower(state,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPowerFTempDiff,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSchedulePtr,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterSetPointTemp,
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower);
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower *= (1.0 - RuntimeFrac);
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower =
                state.dataVariableSpeedCoils->CrankcaseHeatingPower_CalcVarSpeedCoilCooling * (1.0 - RuntimeFrac);

            // set water system demand request (if needed)
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupplyMode == state.dataVariableSpeedCoils->WaterSupplyFromTank) {
                state.dataWaterData->WaterStorage(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterSupTankID)
                    .VdotRequestDemand(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterTankDemandARRID) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsumpRate;
            }
        }

        if ((PartLoadRatio > 0.0 && CyclingScheme == ContFanCycCoil) || (CyclingScheme == CycFanCycCoil)) {
            // calculate coil outlet state variables
            state.dataVariableSpeedCoils->LoadSideOutletEnth =
                state.dataVariableSpeedCoils->LoadSideInletEnth -
                state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->LoadSideMassFlowRate;
            state.dataVariableSpeedCoils->LoadSideOutletDBTemp =
                state.dataVariableSpeedCoils->LoadSideInletDBTemp -
                state.dataVariableSpeedCoils->QSensible / (state.dataVariableSpeedCoils->LoadSideMassFlowRate * CpAir);

            MaxHumRat = PsyWFnTdbRhPb(state,
                                      state.dataVariableSpeedCoils->LoadSideOutletDBTemp,
                                      0.9999,
                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure,
                                      RoutineName);
            MaxOutletEnth = PsyHFnTdbW(state.dataVariableSpeedCoils->LoadSideOutletDBTemp, MaxHumRat);
            if (state.dataVariableSpeedCoils->LoadSideOutletEnth > MaxOutletEnth) {
                state.dataVariableSpeedCoils->LoadSideOutletEnth = MaxOutletEnth;
                // QLoadTotal = LoadSideMassFlowRate * (LoadSideInletEnth - LoadSideOutletEnth)
            }
            state.dataVariableSpeedCoils->LoadSideOutletHumRat =
                PsyWFnTdbH(state, state.dataVariableSpeedCoils->LoadSideOutletDBTemp, state.dataVariableSpeedCoils->LoadSideOutletEnth, RoutineName);
            if (state.dataVariableSpeedCoils->LoadSideOutletHumRat > MaxHumRat) {
                state.dataVariableSpeedCoils->LoadSideOutletHumRat = MaxHumRat;
            }
        }

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == ContFanCycCoil) {
            // continuous fan, cycling compressor
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletEnth +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletHumRat +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp =
                PsyTdbFnHW(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy,
                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat);
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate;
        } else {
            // default to cycling fan, cycling compressor
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = state.dataVariableSpeedCoils->LoadSideOutletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat = state.dataVariableSpeedCoils->LoadSideOutletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp = state.dataVariableSpeedCoils->LoadSideOutletDBTemp;
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate * PartLoadRatio;
        }

        // scale heat transfer rates to PLR and power to RTF
        state.dataVariableSpeedCoils->QLoadTotal *= PartLoadRatio;
        state.dataVariableSpeedCoils->QSensible *= PartLoadRatio;
        // count the powr separately
        state.dataVariableSpeedCoils->Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower &
        //+ VarSpeedCoil(DXCoilNum)%BasinHeaterPower + VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower
        state.dataVariableSpeedCoils->QSource *= PartLoadRatio;
        QWasteHeat *= PartLoadRatio;

        //  Add power to global variable so power can be summed by parent object
        state.dataHVACGlobal->DXElecCoolingPower = state.dataVariableSpeedCoils->Winput;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        // Update heat pump data structure
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = state.dataVariableSpeedCoils->Winput;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = state.dataVariableSpeedCoils->QLoadTotal;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = state.dataVariableSpeedCoils->QSensible;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent =
            state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = state.dataVariableSpeedCoils->QSource;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = state.dataVariableSpeedCoils->Winput * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = state.dataVariableSpeedCoils->QLoadTotal * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = state.dataVariableSpeedCoils->QSensible * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent =
            (state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible) * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = state.dataVariableSpeedCoils->QSource * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsump =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsumpRate * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecPower * ReportingConstant;
        if (RuntimeFrac == 0.0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP =
                state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->Winput;
        }
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = RuntimeFrac;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = state.dataVariableSpeedCoils->PLRCorrLoadSideMdot;
        rhoair = PsyRhoAirFnPbTdbW(state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                   state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                   RoutineName);
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy = 0.0;
            state.dataHeatBal->HeatReclaimVS_DXCoil(DXCoilNum).AvailCapacity = state.dataVariableSpeedCoils->QSource;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = state.dataVariableSpeedCoils->SourceSideMassFlowRate;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp =
                state.dataVariableSpeedCoils->SourceSideInletTemp +
                state.dataVariableSpeedCoils->QSource / (state.dataVariableSpeedCoils->SourceSideMassFlowRate * CpSource);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy =
                state.dataVariableSpeedCoils->SourceSideInletEnth +
                state.dataVariableSpeedCoils->QSource / state.dataVariableSpeedCoils->SourceSideMassFlowRate;
        }

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat = QWasteHeat;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateCollectMode == state.dataVariableSpeedCoils->CondensateToTank) {
            // calculate and report condensation rates  (how much water extracted from the air stream)
            // water flow of water in m3/s for water system interactions
            RhoWater = RhoH2O((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp +
                               state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp) /
                              2.0);
            //     CR9155 Remove specific humidity calculations
            SpecHumIn = state.dataVariableSpeedCoils->LoadSideInletHumRat;
            SpecHumOut = state.dataVariableSpeedCoils->LoadSideOutletHumRat;
            //  mdot * del HumRat / rho water
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVdot =
                max(0.0, (state.dataVariableSpeedCoils->LoadSideMassFlowRate * (SpecHumIn - SpecHumOut) / RhoWater));
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVol =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondensateVdot * ReportingConstant;
        }
    }

    void CalcVarSpeedHPWH(EnergyPlusData &state,
                          int const DXCoilNum,        // the number of the DX coil to be simulated
                          Real64 &RuntimeFrac,        // Runtime Fraction of compressor or percent on time (on-time/cycle time)
                          Real64 const PartLoadRatio, // sensible water heating load / full load sensible water heating capacity
                          Real64 const SpeedRatio,    // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
                          int const SpeedNum,         // Speed number, high bound
                          int const CyclingScheme     // Continuous fan OR cycling compressor
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, ORNL
        //       DATE WRITTEN   12/2014

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the gross cooling capacity of a variable-speed heat pump water heater evaporator and
        // heating capacity of the condenser coil given the rated heating capacity and COP.

        // METHODOLOGY EMPLOYED:
        // The routine requires the user to enter the total heating capacity and COP for the
        // heat pump water heater along with logicals defining if fan and condenser pump are included at numerous speed levels.
        // Since manufacturer's can rate their HPWH equipment with or without including condenser
        // pump heat, this information is required to accurately determine the condenser's leaving
        // water temperature. In addition, knowledge of the fan heat is required to back into
        // a compressor COP.

        // Using/Aliasing
        using CurveManager::CurveValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcVarSpeedHPWH");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OperatingHeatingCapacity; // Water heating operating capacity including the impact of capacity and COP curves (W)
        Real64 OperatingHeatingCOP;      // Water heating operating COP including the impact of capacity and COP curves (W/W)
        Real64 OperatingHeatingPower;    // Water heating operating Power (W)
        Real64 CompressorPower;          // Power consumed by compressor only (W)

        Real64 TotalTankHeatingCapacity; // Water heating capacity corrected for condenser water pump heat (W)
        Real64 TankHeatingCOP;           // Water heating COP corrected for fan and condenser water pump power (W/W)
        // (these previous 2 variables also include the impact of capacity and COP curves)
        Real64 EvapCoolingCapacity;   // Air cooling capacity corrected for evap fan and cond water pump heat (W)
        Real64 InletWaterTemp;        // Condenser water inlet temperature (C)
        Real64 OutletWaterTemp;       // Condenser water outlet temperature (C)
        Real64 EvapInletMassFlowRate; // Evaporator air inlet mass flow rate (m3/s)
        Real64 CondInletMassFlowRate; // Condenser water inlet mass flow rate (m3/s)
        Real64 CpWater;               // Specific heat of condenser inlet water (J/Kg/k)
        Real64 InletAirTemp;          // HPWH inlet air temperature (dry-bulb or wet-bulb) (C)
        Real64 AirMassFlowRatio;      // Ratio of evaporator inlet air mass flow rate to rated mass flow rate
        Real64 WaterMassFlowRatio;    // Ratio of evaporator inlet water mass flow rate to rated mass flow rate
        Real64 PumpHeatToWater;       // Amount of pump heat attributed to heating water
        Real64 HPRTF;                 // Heat pump run time fraction
        Real64 PLF;                   // part-load function
        Real64 CBFSpeed;              // bypass factor as individual speed level
        Real64 COPAirFFModFac;        // air flow fraction modification
        Real64 COPWaterFFModFac;      // water flow fraction modification
        Real64 COPTempModFac;         // total capacity temperature correctio fraction
        Real64 TOTCAPAirFFModFac;     // air flow fraction modification
        Real64 TOTCAPWaterFFModFac;   // water flow fraction modification
        Real64 TOTCAPTempModFac;      // total capacity temperature correctio fraction
        Real64 SHR;                   // total capacity temperature correctio fraction
        Real64 COP;                   // total capacity temperature correctio fraction
        Real64 AoEff;                 // effective air side surface area
        Real64 Winput1;               // power consumption at low speed
        Real64 Winput2;               // power consumption at high speed
        Real64 LoadPressure;          // evaporator inlet pressure
        Real64 CrankcaseHeatingPower; // power due to crankcase heater
        Real64 hDelta;                // Change in air enthalpy across the cooling coil [J/kg]
        Real64 hADP;                  // Apparatus dew point enthalpy [J/kg]
        Real64 tADP;                  // Apparatus dew point temperature [C]
        Real64 wADP;                  // Apparatus dew point humidity ratio [kg/kg]
        Real64 hTinwADP;              // Enthalpy at inlet dry-bulb and wADP [J/kg]
        Real64 WHCAP1;                // total heating capacity at low speed [W]
        Real64 WHCAP2;                // total heating capacity at high speed [W]
        Real64 CpAir;                 // Specific heat of air [J/kg_C]
        Real64 MaxHumRat;             // max possible humidity
        Real64 MaxOutletEnth;         // max possible outlet enthalpy
        Real64 ReportingConstant;
        int EvapInletNode;    // Evaporator air inlet node number
        int EvapOutletNode;   // Evaporator air outlet node number
        int CondInletNode;    // Condenser water inlet node number
        int CondOutletNode;   // Condenser water outlet node number
        int MaxSpeed;         // maximum speed level
        int SpeedCal;         // calculated speed level
        Real64 rhoair(0.0);   // entering air density
        Real64 RhoWater(0.0); // water density

        // note: load side is the evaporator side, and source side is the condenser side

        CondInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum;
        CondOutletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum;
        // If heat pump water heater is OFF, set outlet to inlet and RETURN
        if (PartLoadRatio == 0.0) {
            state.dataLoopNodes->Node(CondOutletNode) = state.dataLoopNodes->Node(CondInletNode);
            return;
        } else {
            EvapInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum;
            EvapOutletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum;
            InletWaterTemp = state.dataLoopNodes->Node(CondInletNode).Temp;
            CondInletMassFlowRate = state.dataLoopNodes->Node(CondInletNode).MassFlowRate;
            EvapInletMassFlowRate = state.dataLoopNodes->Node(EvapInletNode).MassFlowRate;
            CpWater = CPHW(InletWaterTemp);
            CompressorPower = 0.0;
            OperatingHeatingPower = 0.0;
            TankHeatingCOP = 0.0;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        if (!(CyclingScheme == ContFanCycCoil) && PartLoadRatio > 0.0) {
            CondInletMassFlowRate = CondInletMassFlowRate / PartLoadRatio;
            EvapInletMassFlowRate = EvapInletMassFlowRate / PartLoadRatio;
        }

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = EvapInletMassFlowRate;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = CondInletMassFlowRate;

        // determine inlet air temperature type for curve objects
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirTemperatureType == WetBulbIndicator) {
            InletAirTemp = state.dataHVACGlobal->HPWHInletWBTemp;
        } else {
            InletAirTemp = state.dataHVACGlobal->HPWHInletDBTemp;
        }

        // check if indoor evaporator or outdoor evaporator
        CrankcaseHeatingPower = 0.0;
        if (EvapInletNode != 0) {
            state.dataVariableSpeedCoils->LoadSideInletDBTemp = state.dataLoopNodes->Node(EvapInletNode).Temp;
            state.dataVariableSpeedCoils->LoadSideInletHumRat = state.dataLoopNodes->Node(EvapInletNode).HumRat;
            LoadPressure = state.dataLoopNodes->Node(EvapInletNode).Press;
            // prevent the air pressure not given
            if (LoadPressure < 10.0) LoadPressure = state.dataEnvrn->OutBaroPress;

            state.dataVariableSpeedCoils->LoadSideInletWBTemp = state.dataLoopNodes->Node(EvapInletNode).OutAirWetBulb;
            state.dataVariableSpeedCoils->LoadSideInletEnth = state.dataLoopNodes->Node(EvapInletNode).Enthalpy;
        } else {
            state.dataVariableSpeedCoils->LoadSideInletDBTemp = state.dataEnvrn->OutDryBulbTemp;
            state.dataVariableSpeedCoils->LoadSideInletHumRat = state.dataEnvrn->OutHumRat;
            LoadPressure = state.dataEnvrn->OutBaroPress;
            state.dataVariableSpeedCoils->LoadSideInletWBTemp = state.dataEnvrn->OutWetBulbTemp;
            state.dataVariableSpeedCoils->LoadSideInletEnth = state.dataEnvrn->OutEnthalpy;

            // Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
            if (state.dataEnvrn->OutDryBulbTemp < state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater) {
                CrankcaseHeatingPower = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity;
            };
        }

        state.dataVariableSpeedCoils->LoadSideMassFlowRate = EvapInletMassFlowRate;
        state.dataVariableSpeedCoils->SourceSideMassFlowRate = CondInletMassFlowRate;
        state.dataVariableSpeedCoils->SourceSideInletTemp = InletWaterTemp;
        state.dataVariableSpeedCoils->SourceSideInletEnth = state.dataLoopNodes->Node(CondInletNode).Enthalpy;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = state.dataVariableSpeedCoils->SourceSideInletEnth;

        // Check for flows, do not perform simulation if no flow in load side or source side.
        if ((state.dataVariableSpeedCoils->SourceSideMassFlowRate <= 0.0) || (state.dataVariableSpeedCoils->LoadSideMassFlowRate <= 0.0)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = true;
        }

        MaxSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;

        // must be placed inside the loop, otherwise cause bug in release mode, need to be present at two places
        if (SpeedNum > MaxSpeed) {
            SpeedCal = MaxSpeed;
        } else {
            SpeedCal = SpeedNum;
        }

        // part-load calculation
        RuntimeFrac = 1.0;
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0;
        if ((SpeedNum == 1) && (PartLoadRatio < 1.0)) {
            PLF = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, PartLoadRatio);
            if (PLF < 0.7) {
                PLF = 0.7;
            }
            if (CyclingScheme == CycFanCycCoil)
                state.dataHVACGlobal->OnOffFanPartLoadFraction =
                    PLF; // save PLF for fan model, don't change fan power for constant fan mode if coil is off
            // calculate the run time fraction
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = PartLoadRatio / PLF;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac > 1.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
            } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac < 0.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
            }

            RuntimeFrac = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac;
        }

        // interpolate between speeds
        // must be placed inside the loop, otherwise cause bug in release mode
        if (SpeedNum > MaxSpeed) {
            SpeedCal = MaxSpeed;
        } else {
            SpeedCal = SpeedNum;
        }

        Real64 locFanElecPower = 0.0; // local for fan electric power
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFan_TypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex > -1) {
                locFanElecPower = state.dataHVACFan->fanObjs[state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex]->fanPower();
            }
        } else {
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex > 0) {
                locFanElecPower = Fans::GetFanPower(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SupplyFanIndex);
            }
        }

        if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
            AirMassFlowRatio =
                state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;
            WaterMassFlowRatio =
                state.dataVariableSpeedCoils->SourceSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);

            COPTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       InletAirTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            COPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
            COPWaterFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

            COP = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac * COPWaterFFModFac;

            TOTCAPTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          InletAirTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            //   Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);
            // Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPWaterFFModFac =
                CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);

            OperatingHeatingCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
                                       TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

            state.dataVariableSpeedCoils->Winput = OperatingHeatingCapacity / COP;
            OperatingHeatingPower = state.dataVariableSpeedCoils->Winput;

            OperatingHeatingCOP = COP;
            PumpHeatToWater = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower *
                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
            TankHeatingCOP = OperatingHeatingCOP;

            // account for pump heat if not included in total water heating capacity
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
                TotalTankHeatingCapacity = OperatingHeatingCapacity;
            } else {
                TotalTankHeatingCapacity = OperatingHeatingCapacity + PumpHeatToWater;
            }

            HPRTF = RuntimeFrac;
            // calculate evaporator total cooling capacity
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
                    //       make sure fan power is full load fan power, it isn't though,
                    CompressorPower = OperatingHeatingPower - locFanElecPower / HPRTF -
                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
                    if (OperatingHeatingPower > 0.0) TankHeatingCOP = TotalTankHeatingCapacity / OperatingHeatingPower;
                } else {
                    CompressorPower = OperatingHeatingPower - locFanElecPower / HPRTF;
                    if ((OperatingHeatingPower + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity /
                                         (OperatingHeatingPower + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
                }
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
                    //       make sure fan power is full load fan power
                    CompressorPower = OperatingHeatingPower - state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
                    if ((OperatingHeatingPower + locFanElecPower / HPRTF) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity / (OperatingHeatingPower + locFanElecPower / HPRTF);
                } else {
                    CompressorPower = OperatingHeatingPower;
                    if ((OperatingHeatingPower + locFanElecPower / HPRTF +
                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity / (OperatingHeatingPower + locFanElecPower / HPRTF +
                                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
                }
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
                EvapCoolingCapacity = TotalTankHeatingCapacity - PumpHeatToWater - CompressorPower;
            } else {
                EvapCoolingCapacity = TotalTankHeatingCapacity - CompressorPower;
            }

            CBFSpeed = AdjustCBF(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCBF(SpeedCal),
                                 state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal),
                                 state.dataVariableSpeedCoils->LoadSideMassFlowRate);

        } else {
            AirMassFlowRatio =
                state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;
            WaterMassFlowRatio =
                state.dataVariableSpeedCoils->SourceSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
            AoEff = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal) * SpeedRatio +
                    (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal - 1);
            CBFSpeed = std::exp(-AoEff / state.dataVariableSpeedCoils->LoadSideMassFlowRate);

            // calculate low speed
            SpeedCal = SpeedNum - 1;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);
            COPTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       InletAirTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            COPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
            COPWaterFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

            COP = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac * COPWaterFFModFac;

            TOTCAPTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          InletAirTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            //   Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);
            // Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPWaterFFModFac =
                CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);

            OperatingHeatingCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
                                       TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

            state.dataVariableSpeedCoils->Winput = OperatingHeatingCapacity / COP;
            OperatingHeatingPower = state.dataVariableSpeedCoils->Winput;
            Winput1 = state.dataVariableSpeedCoils->Winput;
            WHCAP1 = OperatingHeatingCapacity;

            // calculate upper speed
            SpeedCal = SpeedNum;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);
            COPTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       InletAirTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            COPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
            COPWaterFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

            COP = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac * COPWaterFFModFac;

            TOTCAPTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          InletAirTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            //   Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);
            // Get capacity modifying factor (function of mass flow) for off-rated conditions
            TOTCAPWaterFFModFac =
                CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);

            OperatingHeatingCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
                                       TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

            state.dataVariableSpeedCoils->Winput = OperatingHeatingCapacity / COP;
            OperatingHeatingPower = state.dataVariableSpeedCoils->Winput;

            Winput2 = state.dataVariableSpeedCoils->Winput;
            WHCAP2 = OperatingHeatingCapacity;

            // interpolation
            state.dataVariableSpeedCoils->Winput = Winput2 * SpeedRatio + (1.0 - SpeedRatio) * Winput1;
            OperatingHeatingPower = state.dataVariableSpeedCoils->Winput;
            OperatingHeatingCapacity = WHCAP2 * SpeedRatio + (1.0 - SpeedRatio) * WHCAP1;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedNum) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedNum - 1);

            OperatingHeatingCOP = OperatingHeatingCapacity / OperatingHeatingPower;
            TankHeatingCOP = OperatingHeatingCOP;

            PumpHeatToWater = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower *
                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;

            // account for pump heat if not included in total water heating capacity
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
                TotalTankHeatingCapacity = OperatingHeatingCapacity;
            } else {
                TotalTankHeatingCapacity = OperatingHeatingCapacity + PumpHeatToWater;
            }

            HPRTF = RuntimeFrac;
            // calculate evaporator total cooling capacity
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
                    //       make sure fan power is full load fan power
                    CompressorPower = OperatingHeatingPower - locFanElecPower / HPRTF -
                                      state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
                    if (OperatingHeatingPower > 0.0) TankHeatingCOP = TotalTankHeatingCapacity / OperatingHeatingPower;
                } else {
                    CompressorPower = OperatingHeatingPower - locFanElecPower / HPRTF;
                    if ((OperatingHeatingPower + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity /
                                         (OperatingHeatingPower + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
                }
            } else {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
                    //       make sure fan power is full load fan power
                    CompressorPower = OperatingHeatingPower - state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
                    if ((OperatingHeatingPower + locFanElecPower / HPRTF) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity / (OperatingHeatingPower + locFanElecPower / HPRTF);
                } else {
                    CompressorPower = OperatingHeatingPower;
                    if ((OperatingHeatingPower + locFanElecPower / HPRTF +
                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
                        TankHeatingCOP = TotalTankHeatingCapacity / (OperatingHeatingPower + locFanElecPower / HPRTF +
                                                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
                }
            }

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
                EvapCoolingCapacity = TotalTankHeatingCapacity - PumpHeatToWater - CompressorPower;
            } else {
                EvapCoolingCapacity = TotalTankHeatingCapacity - CompressorPower;
            }
        }

        state.dataVariableSpeedCoils->QSource = TotalTankHeatingCapacity;
        state.dataVariableSpeedCoils->QLoadTotal = EvapCoolingCapacity;
        state.dataHVACGlobal->DXCoilTotalCapacity = EvapCoolingCapacity; // for standard rating calculation
        SHR = 1.0;
        // if indoor, calculate SHR
        if (EvapInletNode != 0) {
            if (CBFSpeed > 0.999) CBFSpeed = 0.999;

            if (CBFSpeed < 0.001) {
                SHR = 1.0;
            } else {
                hDelta = state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->LoadSideMassFlowRate;
                hADP = state.dataVariableSpeedCoils->LoadSideInletEnth - hDelta / (1.0 - CBFSpeed);
                tADP = PsyTsatFnHPb(state, hADP, LoadPressure, RoutineName);
                wADP = PsyWFnTdbH(state, tADP, hADP, RoutineName);
                hTinwADP = PsyHFnTdbW(state.dataVariableSpeedCoils->LoadSideInletDBTemp, wADP);
                if ((state.dataVariableSpeedCoils->LoadSideInletEnth - hADP) > 1.e-10) {
                    SHR = min((hTinwADP - hADP) / (state.dataVariableSpeedCoils->LoadSideInletEnth - hADP), 1.0);
                } else {
                    SHR = 1.0;
                }
            }
        }

        state.dataVariableSpeedCoils->QSensible = state.dataVariableSpeedCoils->QLoadTotal * SHR;

        // determine condenser water inlet/outlet condition at full capacity
        if (CondInletMassFlowRate == 0.0) {
            OutletWaterTemp = InletWaterTemp;
        } else {
            OutletWaterTemp = InletWaterTemp + TotalTankHeatingCapacity / (CpWater * CondInletMassFlowRate);
        }

        state.dataLoopNodes->Node(CondOutletNode).Temp = OutletWaterTemp;

        state.dataLoopNodes->Node(CondOutletNode).MassFlowRate = state.dataLoopNodes->Node(CondInletNode).MassFlowRate;

        // send heating capacity and COP to water heater module for standards rating calculation
        // total heating capacity including condenser pump
        state.dataVariableSpeedCoils->VSHPWHHeatingCapacity = TotalTankHeatingCapacity;
        // total heating COP including compressor, fan, and condenser pump
        state.dataVariableSpeedCoils->VSHPWHHeatingCOP = TankHeatingCOP;

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate = TotalTankHeatingCapacity * PartLoadRatio;
        // calculate total compressor plus condenser pump power, fan power reported in fan module
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).ElecWaterHeatingPower =
            (CompressorPower + state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) * HPRTF;

        // pass the outputs for the cooling coil section
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterPower = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower = CrankcaseHeatingPower * (1.0 - RuntimeFrac);

        // calculate coil outlet state variables
        state.dataVariableSpeedCoils->LoadSideOutletEnth =
            state.dataVariableSpeedCoils->LoadSideInletEnth -
            state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->LoadSideMassFlowRate;
        CpAir = PsyCpAirFnW(state.dataVariableSpeedCoils->LoadSideInletHumRat);
        state.dataVariableSpeedCoils->LoadSideOutletDBTemp =
            state.dataVariableSpeedCoils->LoadSideInletDBTemp -
            state.dataVariableSpeedCoils->QSensible / (state.dataVariableSpeedCoils->LoadSideMassFlowRate * CpAir);

        MaxHumRat = PsyWFnTdbRhPb(state,
                                  state.dataVariableSpeedCoils->LoadSideOutletDBTemp,
                                  0.9999,
                                  state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirPressure,
                                  RoutineName);
        MaxOutletEnth = PsyHFnTdbW(state.dataVariableSpeedCoils->LoadSideOutletDBTemp, MaxHumRat);
        if (state.dataVariableSpeedCoils->LoadSideOutletEnth > MaxOutletEnth) {
            state.dataVariableSpeedCoils->LoadSideOutletEnth = MaxOutletEnth;
        }
        state.dataVariableSpeedCoils->LoadSideOutletHumRat =
            PsyWFnTdbH(state, state.dataVariableSpeedCoils->LoadSideOutletDBTemp, state.dataVariableSpeedCoils->LoadSideOutletEnth, RoutineName);
        if (state.dataVariableSpeedCoils->LoadSideOutletHumRat > MaxHumRat) {
            state.dataVariableSpeedCoils->LoadSideOutletHumRat = MaxHumRat;
        }

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == ContFanCycCoil) {
            // continuous fan, cycling compressor
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletEnth +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletHumRat +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp =
                PsyTdbFnHW(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy,
                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat);
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = state.dataVariableSpeedCoils->LoadSideOutletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat = state.dataVariableSpeedCoils->LoadSideOutletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp = state.dataVariableSpeedCoils->LoadSideOutletDBTemp;
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate * PartLoadRatio;
        }

        // scale heat transfer rates to PLR and power to RTF
        state.dataVariableSpeedCoils->QLoadTotal *= PartLoadRatio;
        state.dataVariableSpeedCoils->QSensible *= PartLoadRatio;
        // count the powr separately
        state.dataVariableSpeedCoils->Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower &
        //+ VarSpeedCoil(DXCoilNum)%BasinHeaterPower + VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower
        state.dataVariableSpeedCoils->QSource *= PartLoadRatio;

        //  Add power to global variable so power can be summed by parent object
        state.dataHVACGlobal->DXElecCoolingPower = state.dataVariableSpeedCoils->Winput;

        ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        // Update heat pump data structure
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower * RuntimeFrac; // water heating pump power
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = state.dataVariableSpeedCoils->Winput;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = state.dataVariableSpeedCoils->QLoadTotal;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = state.dataVariableSpeedCoils->QSensible;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent =
            state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = state.dataVariableSpeedCoils->QSource;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = state.dataVariableSpeedCoils->Winput * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = state.dataVariableSpeedCoils->QLoadTotal * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = state.dataVariableSpeedCoils->QSensible * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent =
            (state.dataVariableSpeedCoils->QLoadTotal - state.dataVariableSpeedCoils->QSensible) * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = state.dataVariableSpeedCoils->QSource * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapWaterConsump = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).BasinHeaterConsumption = 0.0;
        // re-use EvapCondPumpElecConsumption to store WH pump energy consumption
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower * ReportingConstant;
        if (RuntimeFrac == 0.0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP =
                state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->Winput;
        }
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = RuntimeFrac;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = state.dataVariableSpeedCoils->PLRCorrLoadSideMdot;
        rhoair = PsyRhoAirFnPbTdbW(state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                   state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                   RoutineName);
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = state.dataVariableSpeedCoils->SourceSideMassFlowRate;
        RhoWater = RhoH2O(InletWaterTemp); // initialize
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterVolFlowRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate / RhoWater;

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp =
            state.dataVariableSpeedCoils->SourceSideInletTemp +
            state.dataVariableSpeedCoils->QSource / (state.dataVariableSpeedCoils->SourceSideMassFlowRate * CpWater);
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy =
            state.dataVariableSpeedCoils->SourceSideInletEnth +
            state.dataVariableSpeedCoils->QSource / state.dataVariableSpeedCoils->SourceSideMassFlowRate;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat = 0.0;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).bIsDesuperheater) // desuperheater doesn't save power and cooling energy variables
        {
            // source side is the water side; load side is the air side
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption = 0.0;
        }
    }

    void setVarSpeedHPWHFanTypeNum(EnergyPlusData &state, int const dXCoilNum, int const fanTypeNum)
    {
        state.dataVariableSpeedCoils->VarSpeedCoil(dXCoilNum).SupplyFan_TypeNum = fanTypeNum;
    }

    void setVarSpeedHPWHFanIndex(EnergyPlusData &state, int const dXCoilNum, int const fanIndex)
    {
        state.dataVariableSpeedCoils->VarSpeedCoil(dXCoilNum).SupplyFanIndex = fanIndex;
    }

    void setVarSpeedFanInfo(EnergyPlusData &state, int const dXCoilNum, std::string const fanName, int const fanIndex, int const fanTypeNum)
    {
        state.dataVariableSpeedCoils->VarSpeedCoil(dXCoilNum).SupplyFanIndex = fanIndex;
        state.dataVariableSpeedCoils->VarSpeedCoil(dXCoilNum).SupplyFan_TypeNum = fanTypeNum;
        state.dataVariableSpeedCoils->VarSpeedCoil(dXCoilNum).SupplyFanName = fanName;
    }

    void getCoilTypeAndName(EnergyPlusData &state, int const CoilIndex, std::string &CoilType, std::string &CoilName, bool &ErrorsFound)
    {
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "getCoilTypeAndName: Could not find Coil");
            ErrorsFound = true;
        } else {
            CoilName = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).Name;
            CoilType = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).VarSpeedCoilType;
        }
    }

    void CalcVarSpeedCoilHeating(EnergyPlusData &state,
                                 int const DXCoilNum,                      // Heat Pump Number
                                 int const CyclingScheme,                  // Fan/Compressor cycling scheme indicator
                                 Real64 &RuntimeFrac,                      // Runtime Fraction of compressor or percent on time (on-time/cycle time)
                                 [[maybe_unused]] Real64 const SensDemand, // Cooling Sensible Demand [W] !unused1208
                                 int const CompOp,                         // compressor operation flag
                                 Real64 const PartLoadRatio,               // compressor part load ratio
                                 [[maybe_unused]] Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
                                 Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
                                 int const SpeedNum       // Speed number, high bound, i.e. SpeedNum - 1 is the other side
    )
    {

        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcHPHeatingSimple
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for simulating the heating mode of the Variable Speed Water to Air HP Simple

        // METHODOLOGY EMPLOYED:
        // Simulate the heat pump performance using the coefficients and rated conditions
        // Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
        // and RuntimeFrac.

        // Using/Aliasing
        using CurveManager::CurveValue;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using FluidProperties::GetSpecificHeatGlycol;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTdbFnHW;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbH;
        using Psychrometrics::PsyWFnTdbTwbPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcVarSpeedCoilHeating");
        static constexpr std::string_view RoutineNameSourceSideInletTemp("CalcVarSpeedCoilHeating:SourceSideInletTemp");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpSource;            // Specific heat of water [J/kg_C]
        Real64 CpAir;               // Specific heat of air [J/kg_C]
        Real64 AirMassFlowRatio;    // airflow ratio at low speed
        Real64 WaterMassFlowRatio;  // airflow ratio at high speed
        Real64 TotCapAirFFModFac;   // air flow fraction modification
        Real64 TotCapWaterFFModFac; // water flow fraction modification
        Real64 TotCapTempModFac;    // total capacity temperature correction fraction
        Real64 EIRAirFFModFac;      // air flow fraction modification
        Real64 EIRWaterFFModFac;    // water flow fraction modification
        Real64 EIRTempModFac;       // total capacity temperature correction fraction
        Real64 EIR;                 // total capacity temperature correction fraction
        int MaxSpeed;               // maximum speed level
        int SpeedCal;               // calculated speed level
        Real64 QLoadTotal1;         // heating capacity at low speed
        Real64 QLoadTotal2;         // heating capacity at high speed
        Real64 Winput1;             // power consumption at low speed
        Real64 Winput2;             // power consumption at high speed
        Real64 QWasteHeat;          // recoverable waste heat
        Real64 QWasteHeat1;         // recoverable waste heat at low speed
        Real64 QWasteHeat2;         // recoverable waste heat at high speed
        Real64 PLF;                 // part-load function
        Real64 ReportingConstant;
        Real64 rhoair(0.0); // entering air density

        // ADDED VARIABLES FOR air source coil
        MaxSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NumOfSpeeds;

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        if (!(CyclingScheme == ContFanCycCoil) && PartLoadRatio > 0.0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate =
                state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum).MassFlowRate / PartLoadRatio;
        }

        state.dataVariableSpeedCoils->LoadSideMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate;
        state.dataVariableSpeedCoils->LoadSideInletDBTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp;
        state.dataVariableSpeedCoils->LoadSideInletHumRat = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat;

        state.dataVariableSpeedCoils->LoadSideInletWBTemp = PsyTwbFnTdbWPb(state,
                                                                           state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                                                           state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                                                           state.dataEnvrn->OutBaroPress,
                                                                           RoutineName);
        state.dataVariableSpeedCoils->LoadSideInletEnth = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy;
        CpAir = PsyCpAirFnW(state.dataVariableSpeedCoils->LoadSideInletHumRat);

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
            // Get condenser outdoor node info from DX Heating Coil
            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum != 0) {
                state.dataVariableSpeedCoils->OutdoorDryBulb =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Temp;
                state.dataVariableSpeedCoils->OutdoorHumRat =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).HumRat;
                state.dataVariableSpeedCoils->OutdoorPressure =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).Press;
                state.dataVariableSpeedCoils->OutdoorWetBulb =
                    state.dataLoopNodes->Node(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum).OutAirWetBulb;
            } else {
                state.dataVariableSpeedCoils->OutdoorDryBulb = state.dataEnvrn->OutDryBulbTemp;
                state.dataVariableSpeedCoils->OutdoorHumRat = state.dataEnvrn->OutHumRat;
                state.dataVariableSpeedCoils->OutdoorPressure = state.dataEnvrn->OutBaroPress;
                state.dataVariableSpeedCoils->OutdoorWetBulb = state.dataEnvrn->OutWetBulbTemp;
            }
            state.dataVariableSpeedCoils->SourceSideMassFlowRate = 1.0; // not used and avoid divided by zero
            state.dataVariableSpeedCoils->SourceSideInletTemp = state.dataVariableSpeedCoils->OutdoorDryBulb;
            state.dataVariableSpeedCoils->SourceSideInletEnth =
                PsyHFnTdbW(state.dataVariableSpeedCoils->OutdoorDryBulb, state.dataVariableSpeedCoils->OutdoorHumRat);
            CpSource = PsyCpAirFnW(state.dataEnvrn->OutHumRat);

            // Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
            if (state.dataVariableSpeedCoils->OutdoorDryBulb < state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater) {
                state.dataVariableSpeedCoils->CrankcaseHeatingPower = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity;
            } else {
                state.dataVariableSpeedCoils->CrankcaseHeatingPower = 0.0;
            }
        } else {
            state.dataVariableSpeedCoils->SourceSideMassFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate;
            state.dataVariableSpeedCoils->SourceSideInletTemp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp;
            state.dataVariableSpeedCoils->SourceSideInletEnth = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy;
            CpSource = GetSpecificHeatGlycol(state,
                                             state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidName,
                                             state.dataVariableSpeedCoils->SourceSideInletTemp,
                                             state.dataPlnt->PlantLoop(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).LoopNum).FluidIndex,
                                             RoutineNameSourceSideInletTemp);
        }

        // Check for flows, do not perform simulation if no flow in load side or source side.
        if ((state.dataVariableSpeedCoils->SourceSideMassFlowRate <= 0.0) || (state.dataVariableSpeedCoils->LoadSideMassFlowRate <= 0.0)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = true;
        }

        if ((state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) &&
            (state.dataVariableSpeedCoils->OutdoorDryBulb < state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MinOATCompressor)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        }

        if (CompOp == 0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag = false;
            return;
        }

        if (SpeedNum > MaxSpeed) {
            SpeedCal = MaxSpeed;
        } else {
            SpeedCal = SpeedNum;
        }

        RuntimeFrac = 1.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0;
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
        if ((SpeedNum == 1) && (PartLoadRatio < 1.0)) {
            PLF = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PLFFPLR, PartLoadRatio);
            if (PLF < 0.7) {
                PLF = 0.7;
            }
            if (CyclingScheme == CycFanCycCoil)
                state.dataHVACGlobal->OnOffFanPartLoadFraction =
                    PLF; // save PLF for fan model, don't change fan power for constant fan mode if coil is off
            // calculate the run time fraction
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = PartLoadRatio / PLF;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac > 1.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
            } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac < 0.0) {
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
            }

            RuntimeFrac = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac;
        }

        if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
            AirMassFlowRatio =
                state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                WaterMassFlowRatio = 1.0;
            } else {
                WaterMassFlowRatio = state.dataVariableSpeedCoils->SourceSideMassFlowRate /
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
            }

            TotCapTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            TotCapAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                TotCapWaterFFModFac = 1.0;
            } else {
                TotCapWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            state.dataVariableSpeedCoils->QLoadTotal = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) *
                                                       TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).capModFacTotal = TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac;
            state.dataVariableSpeedCoils->TotRatedCapacity =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal); // for defrosting power cal

            EIRTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                EIRWaterFFModFac = 1.0;
            } else {
                EIRWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                  EIRWaterFFModFac;
            state.dataVariableSpeedCoils->Winput = state.dataVariableSpeedCoils->QLoadTotal * EIR;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                QWasteHeat = 0.0;
            } else {
                QWasteHeat = state.dataVariableSpeedCoils->Winput * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                QWasteHeat *= CurveValue(state,
                                         state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                         state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                         state.dataVariableSpeedCoils->SourceSideInletTemp);
            }

        } else {
            AirMassFlowRatio =
                state.dataVariableSpeedCoils->LoadSideMassFlowRate / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                WaterMassFlowRatio = 1.0;
            } else {
                WaterMassFlowRatio = state.dataVariableSpeedCoils->SourceSideMassFlowRate /
                                     state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
            }

            SpeedCal = SpeedNum - 1;
            TotCapTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            TotCapAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                TotCapWaterFFModFac = 1.0;
            } else {
                TotCapWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            QLoadTotal1 = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac *
                          TotCapWaterFFModFac;

            EIRTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                EIRWaterFFModFac = 1.0;
            } else {
                EIRWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                  EIRWaterFFModFac;
            Winput1 = QLoadTotal1 * EIR;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                QWasteHeat1 = 0.0;
            } else {
                QWasteHeat1 = Winput1 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                QWasteHeat1 *= CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                          state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            }

            SpeedCal = SpeedNum;
            TotCapTempModFac = CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
                                          state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            TotCapAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                TotCapWaterFFModFac = 1.0;
            } else {
                TotCapWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            QLoadTotal2 = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac *
                          TotCapWaterFFModFac;

            EIRTempModFac = CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal),
                                       state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                       state.dataVariableSpeedCoils->SourceSideInletTemp);
            EIRAirFFModFac = CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                EIRWaterFFModFac = 1.0;
            } else {
                EIRWaterFFModFac =
                    CurveValue(state, state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);
            }

            EIR = (1.0 / state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac *
                  EIRWaterFFModFac;
            Winput2 = QLoadTotal2 * EIR;

            if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
                QWasteHeat2 = 0.0;
            } else {
                QWasteHeat2 = Winput2 * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeatFrac(SpeedCal);
                QWasteHeat2 *= CurveValue(state,
                                          state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSWasteHeat(SpeedCal),
                                          state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                          state.dataVariableSpeedCoils->SourceSideInletTemp);
            }

            state.dataVariableSpeedCoils->QLoadTotal = QLoadTotal2 * SpeedRatio + (1.0 - SpeedRatio) * QLoadTotal1;
            state.dataVariableSpeedCoils->Winput = Winput2 * SpeedRatio + (1.0 - SpeedRatio) * Winput1;
            QWasteHeat = QWasteHeat2 * SpeedRatio + (1.0 - SpeedRatio) * QWasteHeat1;
            state.dataVariableSpeedCoils->TotRatedCapacity =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * SpeedRatio +
                (1.0 - SpeedRatio) * state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal - 1);
        }

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower = 0.0; // necessary to clear zero for water source coils
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower = 0.0;         // clear the defrost power
        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
            // Calculating adjustment factors for defrost
            // Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
            state.dataVariableSpeedCoils->OutdoorCoilT = 0.82 * state.dataVariableSpeedCoils->OutdoorDryBulb - 8.589;
            state.dataVariableSpeedCoils->OutdoorCoildw =
                max(1.0e-6,
                    (state.dataVariableSpeedCoils->OutdoorHumRat -
                     PsyWFnTdpPb(state, state.dataVariableSpeedCoils->OutdoorCoilT, state.dataVariableSpeedCoils->OutdoorPressure)));

            // Initializing defrost adjustment factors
            state.dataVariableSpeedCoils->LoadDueToDefrost = 0.0;
            state.dataVariableSpeedCoils->HeatingCapacityMultiplier = 1.0;
            state.dataVariableSpeedCoils->FractionalDefrostTime = 0.0;
            state.dataVariableSpeedCoils->InputPowerMultiplier = 1.0;
            // Check outdoor temperature to determine of defrost is active
            if (state.dataVariableSpeedCoils->OutdoorDryBulb <= state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxOATDefrost) {
                // Calculate defrost adjustment factors depending on defrost control type
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostControl == state.dataVariableSpeedCoils->Timed) {
                    state.dataVariableSpeedCoils->FractionalDefrostTime = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostTime;
                    state.dataVariableSpeedCoils->HeatingCapacityMultiplier = 0.909 - 107.33 * state.dataVariableSpeedCoils->OutdoorCoildw;
                    state.dataVariableSpeedCoils->InputPowerMultiplier = 0.90 - 36.45 * state.dataVariableSpeedCoils->OutdoorCoildw;
                } else { // else defrost control is on-demand
                    state.dataVariableSpeedCoils->FractionalDefrostTime = 1.0 / (1.0 + 0.01446 / state.dataVariableSpeedCoils->OutdoorCoildw);
                    state.dataVariableSpeedCoils->HeatingCapacityMultiplier = 0.875 * (1.0 - state.dataVariableSpeedCoils->FractionalDefrostTime);
                    state.dataVariableSpeedCoils->InputPowerMultiplier = 0.954 * (1.0 - state.dataVariableSpeedCoils->FractionalDefrostTime);
                }
                // correction fractional defrost time shorten by runtime fraction
                state.dataVariableSpeedCoils->FractionalDefrostTime *= RuntimeFrac;

                if (state.dataVariableSpeedCoils->FractionalDefrostTime > 0.0) {
                    // Calculate defrost adjustment factors depending on defrost control strategy
                    if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostStrategy == state.dataVariableSpeedCoils->ReverseCycle) {
                        state.dataVariableSpeedCoils->LoadDueToDefrost = (0.01 * state.dataVariableSpeedCoils->FractionalDefrostTime) *
                                                                         (7.222 - state.dataVariableSpeedCoils->OutdoorDryBulb) *
                                                                         (state.dataVariableSpeedCoils->TotRatedCapacity / 1.01667);
                        state.dataVariableSpeedCoils->DefrostEIRTempModFac =
                            CurveValue(state,
                                       state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostEIRFT,
                                       max(15.555, state.dataVariableSpeedCoils->LoadSideInletWBTemp),
                                       max(15.555, state.dataVariableSpeedCoils->OutdoorDryBulb));
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower =
                            state.dataVariableSpeedCoils->DefrostEIRTempModFac * (state.dataVariableSpeedCoils->TotRatedCapacity / 1.01667) *
                            state.dataVariableSpeedCoils->FractionalDefrostTime;
                    } else { // Defrost strategy is resistive
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower =
                            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostCapacity *
                            state.dataVariableSpeedCoils->FractionalDefrostTime;
                    }
                } else { // Defrost is not active because (OutDryBulbTemp > VarSpeedCoil(DXCoilNum).MaxOATDefrost)
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower = 0.0;
                }
            }

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower =
                state.dataVariableSpeedCoils->CrankcaseHeatingPower * (1.0 - RuntimeFrac);
            //! Modify total heating capacity based on defrost heating capacity multiplier
            //! MaxHeatCap passed from parent object VRF Condenser and is used to limit capacity of TU's to that available from condenser
            //  IF(PRESENT(MaxHeatCap))THEN
            //    TotCap = MIN(MaxHeatCap,TotCap * HeatingCapacityMultiplier)
            //  ELSE
            //    TotCap = TotCap * HeatingCapacityMultiplier
            //  END IF
            state.dataVariableSpeedCoils->QLoadTotal =
                state.dataVariableSpeedCoils->QLoadTotal * state.dataVariableSpeedCoils->HeatingCapacityMultiplier -
                state.dataVariableSpeedCoils->LoadDueToDefrost;
            // count the powr separately
            state.dataVariableSpeedCoils->Winput *= state.dataVariableSpeedCoils->InputPowerMultiplier; //+ VarSpeedCoil(DXCoilNum)%DefrostPower
        }

        state.dataVariableSpeedCoils->QSource = state.dataVariableSpeedCoils->QLoadTotal + QWasteHeat - state.dataVariableSpeedCoils->Winput;
        state.dataVariableSpeedCoils->QSensible = state.dataVariableSpeedCoils->QLoadTotal;

        if (state.dataVariableSpeedCoils->QSource < 0) {
            state.dataVariableSpeedCoils->QSource = 0.0;
            QWasteHeat = state.dataVariableSpeedCoils->Winput - state.dataVariableSpeedCoils->QLoadTotal;
        }

        // calculate coil outlet state variables
        state.dataVariableSpeedCoils->LoadSideOutletEnth =
            state.dataVariableSpeedCoils->LoadSideInletEnth +
            state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->LoadSideMassFlowRate;
        state.dataVariableSpeedCoils->LoadSideOutletDBTemp =
            state.dataVariableSpeedCoils->LoadSideInletDBTemp +
            state.dataVariableSpeedCoils->QSensible / (state.dataVariableSpeedCoils->LoadSideMassFlowRate * CpAir);
        state.dataVariableSpeedCoils->LoadSideOutletHumRat =
            PsyWFnTdbH(state, state.dataVariableSpeedCoils->LoadSideOutletDBTemp, state.dataVariableSpeedCoils->LoadSideOutletEnth, RoutineName);

        // Actual outlet conditions are "average" for time step
        if (CyclingScheme == ContFanCycCoil) {
            // continuous fan, cycling compressor
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletEnth +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat =
                PartLoadRatio * state.dataVariableSpeedCoils->LoadSideOutletHumRat +
                (1.0 - PartLoadRatio) * state.dataVariableSpeedCoils->LoadSideInletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp =
                PsyTdbFnHW(state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy,
                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat);
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate;
        } else {
            // default to cycling fan, cycling compressor
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = state.dataVariableSpeedCoils->LoadSideOutletEnth;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat = state.dataVariableSpeedCoils->LoadSideOutletHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp = state.dataVariableSpeedCoils->LoadSideOutletDBTemp;
            state.dataVariableSpeedCoils->PLRCorrLoadSideMdot = state.dataVariableSpeedCoils->LoadSideMassFlowRate * PartLoadRatio;
        }

        // scale heat transfer rates to PLR and power to RTF
        state.dataVariableSpeedCoils->QLoadTotal *= PartLoadRatio;
        state.dataVariableSpeedCoils->QSensible *= PartLoadRatio;
        // count the powr separately
        state.dataVariableSpeedCoils->Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower
        state.dataVariableSpeedCoils->QSource *= PartLoadRatio;
        QWasteHeat *= PartLoadRatio;

        //  Add power to global variable so power can be summed by parent object
        state.dataHVACGlobal->DXElecHeatingPower = state.dataVariableSpeedCoils->Winput;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        // Update heat pump data structure
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = state.dataVariableSpeedCoils->Winput;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = state.dataVariableSpeedCoils->QLoadTotal;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = state.dataVariableSpeedCoils->QSensible;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = state.dataVariableSpeedCoils->QSource;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = state.dataVariableSpeedCoils->Winput * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = state.dataVariableSpeedCoils->QLoadTotal * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = state.dataVariableSpeedCoils->QSensible * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = state.dataVariableSpeedCoils->QSource * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostConsumption =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).DefrostPower * ReportingConstant;
        if (RuntimeFrac == 0.0) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP =
                state.dataVariableSpeedCoils->QLoadTotal / state.dataVariableSpeedCoils->Winput;
        }
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = RuntimeFrac;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate = state.dataVariableSpeedCoils->PLRCorrLoadSideMdot;
        rhoair = PsyRhoAirFnPbTdbW(state,
                                   state.dataEnvrn->OutBaroPress,
                                   state.dataVariableSpeedCoils->LoadSideInletDBTemp,
                                   state.dataVariableSpeedCoils->LoadSideInletHumRat,
                                   RoutineName);
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirVolFlowRate =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair;

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed) {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy = 0.0;
        } else {
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterMassFlowRate = state.dataVariableSpeedCoils->SourceSideMassFlowRate;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp =
                state.dataVariableSpeedCoils->SourceSideInletTemp -
                state.dataVariableSpeedCoils->QSource / (state.dataVariableSpeedCoils->SourceSideMassFlowRate * CpSource);
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy =
                state.dataVariableSpeedCoils->SourceSideInletEnth -
                state.dataVariableSpeedCoils->QSource / state.dataVariableSpeedCoils->SourceSideMassFlowRate;
        }

        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QWasteHeat = QWasteHeat;
    }

    Real64 GetCoilCapacityVariableSpeed(EnergyPlusData &state,
                                        std::string const &CoilType, // must match coil types in this module
                                        std::string const &CoilName, // must match coil names for the coil type
                                        bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilCapacity
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        Real64 CoilCapacity; // returned capacity of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:COOLING:DX:VARIABLESPEED") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:DX:VARIABLESPEED") ||
            UtilityRoutines::SameString(CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED")) {
            WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
            if (WhichCoil != 0) {
                if (UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") ||
                    UtilityRoutines::SameString(CoilType, "COIL:HEATING:DX:VARIABLESPEED")) {
                    CoilCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedCapHeat;
                } else if (UtilityRoutines::SameString(CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED")) {
                    CoilCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedCapWH;
                } else {
                    CoilCapacity = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedCapCoolTotal;
                }
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

    int GetCoilIndexVariableSpeed(EnergyPlusData &state,
                                  std::string const &CoilType, // must match coil types in this module
                                  std::string const &CoilName, // must match coil names for the coil type
                                  bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilIndex
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil index for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
        // as zero.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Return value
        int IndexNum; // returned index of matched coil

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);

        if (IndexNum == 0) {
            ShowSevereError(state, "GetCoilIndexVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }

        return IndexNum;
    }

    Real64 GetCoilAirFlowRateVariableSpeed(EnergyPlusData &state,
                                           std::string const &CoilType, // must match coil types in this module
                                           std::string const &CoilName, // must match coil names for the coil type
                                           bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilAirFlowRate
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max coil air flow rate for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 CoilAirFlowRate; // returned air volume flow rate of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") ||
            UtilityRoutines::SameString(CoilType, "COIL:COOLING:DX:VARIABLESPEED") ||
            UtilityRoutines::SameString(CoilType, "COIL:HEATING:DX:VARIABLESPEED") ||
            UtilityRoutines::SameString(CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED")) {
            WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
            if (WhichCoil != 0) {
                // CoilAirFlowRate=VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate
                if (state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedAirVolFlowRate == AutoSize) { // means autosize
                    CoilAirFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedAirVolFlowRate;
                } else {
                    CoilAirFlowRate = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).MSRatedAirVolFlowRate(
                                          state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).NumOfSpeeds) /
                                      state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).MSRatedAirVolFlowRate(
                                          state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).NormSpedLevel) *
                                      state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).RatedAirVolFlowRate;
                } // use largest air flow rate
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilAirFlowRateVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CoilAirFlowRate = -1000.0;
        }

        return CoilAirFlowRate;
    }

    int GetVSCoilPLFFPLR(EnergyPlusData &state,
                         std::string const &CoilType, // must match coil types in this module
                         std::string const &CoilName, // must match coil names for the coil type
                         bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   12/2014
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns PLR curve index.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Return value
        int PLRNumber; // returned outlet node of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            PLRNumber = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).PLFFPLR;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetVSCoilPLFFPLR: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            PLRNumber = 0;
        }

        return PLRNumber;
    }

    int GetVSCoilCapFTCurveIndex(EnergyPlusData &state,
                                 int const &CoilIndex, // must match coil names for the coil type
                                 bool &ErrorsFound     // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   7/2017

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns CapFT curve index.  If
        // incorrect coil index is given, ErrorsFound is returned as true and value is returned
        // as zero.

        // Return value
        int CapFTIndex; // returned CapFT curve index of matched coil

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetVSCoilCapFTCurveIndex: Could not find Coil");
            ErrorsFound = true;
            CapFTIndex = 0;
        } else {
            CapFTIndex =
                state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).MSCCapFTemp(state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).NumOfSpeeds);
        }

        return CapFTIndex;
    }

    int GetCoilInletNodeVariableSpeed(EnergyPlusData &state,
                                      std::string const &CoilType, // must match coil types in this module
                                      std::string const &CoilName, // must match coil names for the coil type
                                      bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilInletNode
        //       DATE WRITTEN   March 2012
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
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).AirInletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilInletNodeVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetCoilOutletNodeVariableSpeed(EnergyPlusData &state,
                                       std::string const &CoilType, // must match coil types in this module
                                       std::string const &CoilName, // must match coil names for the coil type
                                       bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilOutletNode
        //       DATE WRITTEN   March 2012
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
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).AirOutletNodeNum;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilOutletNodeVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetVSCoilCondenserInletNode(EnergyPlusData &state,
                                    std::string const &CoilName, // must match coil names for the coil type
                                    bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on DXCoil:GetCoilCondenserInletNode
        //       DATE WRITTEN   July 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the condenser inlet node.  If
        // incorrect coil  name is given, ErrorsFound is returned as true.

        // Return value
        int CondNode; // returned condenser node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            CondNode = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).CondenserInletNodeNum;
        } else {
            ShowSevereError(state, "GetCoilCondenserInletNode: Invalid VS DX Coil, Type= VS DX Cooling Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CondNode = 0;
        }

        return CondNode;
    }

    Real64 GetVSCoilMinOATCompressor(EnergyPlusData &state,
                                     std::string const &CoilName, // must match coil names for the coil type
                                     bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   July 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns min OAT for compressor operation.  If
        // incorrect coil  name is given, ErrorsFound is returned as true.

        // Return value
        Real64 MinOAT; // returned min OAT for compressor operation

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            MinOAT = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).MinOATCompressor;
        } else {
            ShowSevereError(state, "GetVSCoilMinOATCompressor: Invalid VS DX Coil, Type= VS DX Coil Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            MinOAT = -1000.0;
        }

        return MinOAT;
    }

    Real64 GetVSCoilMinOATCompressorUsingIndex(EnergyPlusData &state,
                                               int const CoilIndex, // index to cooling coil
                                               bool &ErrorsFound    // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   August 2019

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the the min oat for the cooling coil compressor and returns it.  If
        // incorrect coil index is given, ErrorsFound is returned as true and value is returned
        // as negative 1000.

        // Return value
        Real64 MinOAT; // returned min oa temperature of matched coil

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {

            ShowSevereError(state, "GetVSCoilMinOATCompressorUsingIndex: Index passed = 0");
            ShowContinueError(state, "... returning Min OAT as -1000.");
            ErrorsFound = true;
            MinOAT = -1000.0;

        } else {

            MinOAT = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).MinOATCompressor;
        }

        return MinOAT;
    }

    int GetVSCoilNumOfSpeeds(EnergyPlusData &state,
                             std::string const &CoilName, // must match coil names for the coil type
                             bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   March 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns number of speeds.  If
        // incorrect coil name is given, ErrorsFound is returned as true.

        // Return value
        int Speeds; // returned number of speeds

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        WhichCoil = UtilityRoutines::FindItemInList(CoilName, state.dataVariableSpeedCoils->VarSpeedCoil);
        if (WhichCoil != 0) {
            Speeds = state.dataVariableSpeedCoils->VarSpeedCoil(WhichCoil).NumOfSpeeds;
        } else {
            ShowSevereError(state, "GetVSCoilNumOfSpeeds: Invalid VS DX Coil, Type= VS DX Coil Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            Speeds = 0;
        }

        return Speeds;
    }

    void SetVarSpeedCoilData(EnergyPlusData &state,
                             int const WSHPNum,                    // Number of OA Controller
                             bool &ErrorsFound,                    // Set to true if certain errors found
                             Optional_int CompanionCoolingCoilNum, // Index to cooling coil for heating coil = SimpleWSHPNum
                             Optional_int CompanionHeatingCoilNum, // Index to heating coil for cooling coil = SimpleWSHPNum
                             Optional_int MSHPDesignSpecIndex      // index to UnitarySystemPerformance:Multispeed object
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:SetWSHPData
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine was designed to "push" information from a parent object to
        // this WSHP coil object.

        // Using/Aliasing
        using FluidProperties::FindGlycol;

        // Obtains and Allocates WatertoAirHP related parameters from input file
        if (state.dataVariableSpeedCoils->GetCoilsInputFlag) { // First time subroutine has been entered
            GetVarSpeedCoilInput(state);
            //    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
            state.dataVariableSpeedCoils->GetCoilsInputFlag = false;
        }

        if (WSHPNum <= 0 || WSHPNum > state.dataVariableSpeedCoils->NumVarSpeedCoils) {
            ShowSevereError(state,
                            format("SetVarSpeedCoilData: called with VS WSHP Coil Number out of range={} should be >0 and <{}",
                                   WSHPNum,
                                   state.dataVariableSpeedCoils->NumVarSpeedCoils));
            ErrorsFound = true;
            return;
        }

        if (present(CompanionCoolingCoilNum)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(WSHPNum).CompanionCoolingCoilNum = CompanionCoolingCoilNum;
            state.dataVariableSpeedCoils->VarSpeedCoil(WSHPNum).FindCompanionUpStreamCoil = true;
            state.dataVariableSpeedCoils->VarSpeedCoil(CompanionCoolingCoilNum).CompanionHeatingCoilNum = WSHPNum;
        }

        if (present(CompanionHeatingCoilNum)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(WSHPNum).CompanionHeatingCoilNum = CompanionHeatingCoilNum;
            state.dataVariableSpeedCoils->VarSpeedCoil(CompanionHeatingCoilNum).CompanionCoolingCoilNum = WSHPNum;
        }

        if (present(MSHPDesignSpecIndex)) {
            state.dataVariableSpeedCoils->VarSpeedCoil(WSHPNum).MSHPDesignSpecIndex = MSHPDesignSpecIndex;
        }
    }

    void UpdateVarSpeedCoil(EnergyPlusData &state, int const DXCoilNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:UpdateSimpleWSHP
        //       DATE WRITTEN   March 2012
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

        // WatertoAirHP(DXCoilNum)%SimFlag=.FALSE.
        if (!state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).SimFlag) {
            // Heatpump is off; just pass through conditions
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).COP = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio = 0.0;

            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirDBTemp;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirHumRat;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletAirEnthalpy;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterTemp;
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy =
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).InletWaterEnthalpy;
        }

        AirInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirInletNodeNum;
        WaterInletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterInletNodeNum;
        AirOutletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).AirOutletNodeNum;
        WaterOutletNode = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).WaterOutletNodeNum;

        // Set the air outlet  nodes of the WatertoAirHPSimple
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate; // LoadSideMassFlowRate
        state.dataLoopNodes->Node(AirOutletNode).Temp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirDBTemp;
        state.dataLoopNodes->Node(AirOutletNode).HumRat = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirHumRat;
        state.dataLoopNodes->Node(AirOutletNode).Enthalpy = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletAirEnthalpy;

        // Set the air outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(AirOutletNode).Quality = state.dataLoopNodes->Node(AirInletNode).Quality;
        state.dataLoopNodes->Node(AirOutletNode).Press = state.dataLoopNodes->Node(AirInletNode).Press;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax; // LoadSideMassFlowRate
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail; // LoadSideMassFlowRate

        // Set the water outlet node of the WatertoAirHPSimple
        // Set the water outlet nodes for properties that just pass through & not used
        if (WaterInletNode != 0 && WaterOutletNode != 0) {
            SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);
            state.dataLoopNodes->Node(WaterOutletNode).Temp = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterTemp;
            state.dataLoopNodes->Node(WaterOutletNode).Enthalpy = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy;
        }

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Energy =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Power * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLoadTotal =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLoadTotal * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySensible =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSensible * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergyLatent =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QLatent * ReportingConstant;
        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).EnergySource =
            state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).QSource * ReportingConstant;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(AirOutletNode).CO2 = state.dataLoopNodes->Node(AirInletNode).CO2;
        }

        if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).reportCoilFinalSizes) {
            if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingHVACSizingSimulations && !state.dataGlobal->DoingSizing) {
                if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingWaterToAirHPVSEquationFit ||
                    state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed) { // cooling coil
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolTotal,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapCoolSens,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate);
                } else if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == Coil_HeatingWaterToAirHPVSEquationFit ||
                           state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum ==
                               Coil_HeatingAirToAirVariableSpeed) { // heating coil
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                        state,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Name,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).VarSpeedCoilType,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedCapHeat,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate,
                        state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate);
                }
                state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).reportCoilFinalSizes = false;
            }
        }
    }

    Real64 CalcEffectiveSHR(EnergyPlusData &state,
                            int const DXCoilNum,     // Index number for cooling coil
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
        //    AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcEffectiveSHR
        //    DATE WRITTEN   March 2012

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
        // na

        // Return value
        Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
        // at the current operating conditions (sec)
        Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
        // at the current operating conditions
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

        Twet_Rated = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Twet_Rated;
        Gamma_Rated = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).Gamma_Rated;
        MaxONOFFCyclesperHour = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MaxONOFFCyclesperHour;
        HPTimeConstant = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).HPTimeConstant;
        FanDelayTime = state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).FanDelayTime;

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

    void CalcTotCapSHR_VSWSHP(EnergyPlusData &state,
                              Real64 const InletDryBulb,       // inlet air dry bulb temperature [C]
                              Real64 const InletHumRat,        // inlet air humidity ratio [kg water / kg dry air]
                              Real64 const InletEnthalpy,      // inlet air specific enthalpy [J/kg]
                              Real64 &InletWetBulb,            // inlet air wet bulb temperature [C]
                              Real64 const AirMassFlowRatio,   // Ratio of actual air mass flow to nominal air mass flow
                              Real64 const WaterMassFlowRatio, // Ratio of actual water mass flow to nominal water mass flow
                              Real64 const AirMassFlow,        // actual mass flow for capacity and SHR calculation
                              Real64 const CBF,                // coil bypass factor
                              Real64 const TotCapNom1,         // nominal total capacity at low speed [W]
                              int const CCapFTemp1,            // capacity modifier curve index, function of entering wetbulb at low speed
                              int const CCapAirFFlow1,         // capacity modifier curve, function of actual air flow vs rated flow at low speed
                              int const CCapWaterFFlow1,       // capacity modifier curve, function of actual water flow vs rated flow at low speed
                              Real64 const TotCapNom2,         // nominal total capacity at high speed [W]
                              int const CCapFTemp2,            // capacity modifier curve index, function of entering wetbulb at high speed
                              int const CCapAirFFlow2,         // capacity modifier curve, function of actual air flow vs rated flow at high speed
                              int const CCapWaterFFlow2,       // capacity modifier curve, function of actual water flow vs rated flow at high speed
                              Real64 &TotCap1,                 // total capacity at the given conditions [W] at low speed
                              Real64 &TotCap2,                 // total capacity at the given conditions [W] at high speed
                              Real64 &TotCapSpeed,             // integrated total capacity corresponding to the speed ratio
                              Real64 &SHR,                     // sensible heat ratio at the given conditions
                              Real64 const CondInletTemp,      // Condenser inlet temperature [C]
                              Real64 const Pressure,           // air pressure [Pa]
                              Real64 const SpeedRatio,         // from 0.0 to 1.0
                              int const NumSpeeds,             // number of speeds for input
                              Real64 &TotCapModFac             // capacity modification factor, func of temp and func of flow
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, , based on DX:CalcTotCapSHR, introducing two speed levels
        //       DATE WRITTEN   March 2012

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates total capacity and sensible heat ratio of a DX coil at the specified conditions

        // METHODOLOGY EMPLOYED:
        // With the rated performance data entered by the user, the model employs some of the
        // DOE-2.1E curve fits to adjust the capacity and SHR of the unit as a function
        // of entering air temperatures and supply air flow rate (actual vs rated flow). The model
        // does NOT employ the exact same methodology to calculate performance as DOE-2, although
        // some of the DOE-2 curve fits are employed by this model.

        // The model checks for coil dryout conditions, and adjusts the calculated performance
        // appropriately.

        // REFERENCES:
        // ASHRAE HVAC 2 Toolkit page 4-81.
        // Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
        // control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
        // 104-113.
        // Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
        // User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
        // Predictions. Proceedings of ACEEE Conference.

        // Using/Aliasing
        using CurveManager::CurveValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcTotCapSHR_VSWSHP");
        constexpr int MaxIter(30);        // Maximum number of iterations for dry evaporator calculations
        constexpr Real64 Tolerance(0.01); // Error tolerance for dry evaporator iterations

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TotCapWaterFlowModFac1; // Total capacity modifier (function of actual supply water flow vs nominal flow) at low speed
        Real64 TotCapTempModFac2;      // Total capacity modifier (function of entering wetbulb, outside water inlet temp) at high speed
        Real64 TotCapAirFlowModFac2;   // Total capacity modifier (function of actual supply air flow vs nominal flow) at high speed
        Real64 TotCapWaterFlowModFac2; // Total capacity modifier (function of actual supply water flow vs nominal flow) at high speed
        Real64 TotCapCalc;             // temporary calculated value of total capacity [W]
        Real64 TotCapCalc1;            // temporary calculated value of total capacity [W] at low speed
        Real64 TotCapCalc2;            // temporary calculated value of total capacity [W] at high speed

        int Counter = 0;                        // Error tolerance for dry evaporator iterations
        Real64 RF = 0.4;                        // Relaxation factor for dry evaporator iterations
        Real64 werror = 0.0;                    // Deviation of humidity ratio in dry evaporator iteration loop
        Real64 SHRCalc = SHR;                   // initialize temporary calculated value of SHR
        Real64 InletWetBulbCalc = InletWetBulb; // calculated inlet wetbulb temperature used for finding dry coil point [C]
        Real64 InletHumRatCalc = InletHumRat;   // calculated inlet humidity ratio used for finding dry coil point [kg water / kg dry air]
        bool LoopOn = true;                     // flag to control the loop iteration

        //  LOOP WHILE (ABS(werror) .gt. Tolerance .OR. Counter == 0)
        while (LoopOn) {
            //   Get capacity modifying factor (function of inlet wetbulb & condenser inlet temp) for off-rated conditions
            Real64 TotCapTempModFac1 = CurveValue(state, CCapFTemp1, InletWetBulbCalc, CondInletTemp);
            //   Get capacity modifying factor (function of mass flow) for off-rated conditions
            Real64 TotCapAirFlowModFac1 = CurveValue(state, CCapAirFFlow1, AirMassFlowRatio);
            // Get capacity modifying factor (function of mass flow) for off-rated conditions
            if (CCapWaterFFlow1 == 0) {
                TotCapWaterFlowModFac1 = 1.0;
            } else {
                TotCapWaterFlowModFac1 = CurveValue(state, CCapWaterFFlow1, WaterMassFlowRatio);
            }

            //   Get total capacity
            if (NumSpeeds < 2) { // ONLY ONE SPEED
                TotCapCalc = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1;
                TotCapCalc1 = TotCapCalc;
                TotCapCalc2 = 0.0;
                TotCapModFac = TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1;
            } else {
                TotCapTempModFac2 = CurveValue(state, CCapFTemp2, InletWetBulbCalc, CondInletTemp);
                TotCapAirFlowModFac2 = CurveValue(state, CCapAirFFlow2, AirMassFlowRatio);

                if (CCapWaterFFlow2 == 0) {
                    TotCapWaterFlowModFac2 = 1.0;
                } else {
                    TotCapWaterFlowModFac2 = CurveValue(state, CCapWaterFFlow2, WaterMassFlowRatio);
                }

                TotCapCalc1 = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1;
                TotCapCalc2 = TotCapNom2 * TotCapAirFlowModFac2 * TotCapWaterFlowModFac2 * TotCapTempModFac2;

                TotCapCalc = TotCapCalc2 * SpeedRatio + (1.0 - SpeedRatio) * TotCapCalc1;
                TotCapModFac = (TotCapAirFlowModFac2 * TotCapWaterFlowModFac2 * TotCapTempModFac2) * SpeedRatio +
                               (1.0 - SpeedRatio) * (TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1);
            }

            Real64 localCBF = max(0.0, CBF); // negative coil bypass factor is physically impossible

            //   Calculate apparatus dew point conditions using TotCap and CBF
            Real64 hDelta = TotCapCalc / AirMassFlow;                       // Change in air enthalpy across the cooling coil [J/kg]
            Real64 hADP = InletEnthalpy - hDelta / (1.0 - localCBF);        // Apparatus dew point enthalpy [J/kg]
            Real64 tADP = PsyTsatFnHPb(state, hADP, Pressure, RoutineName); // Apparatus dew point temperature [C]
            Real64 wADP = PsyWFnTdbH(state, tADP, hADP, RoutineName);       // Apparatus dew point humidity ratio [kg/kg]
            Real64 hTinwADP = PsyHFnTdbW(InletDryBulb, wADP);               // Enthalpy at inlet dry-bulb and wADP [J/kg]
            SHRCalc = min((hTinwADP - hADP) / (InletEnthalpy - hADP), 1.0); // temporary calculated value of SHR

            //   Check for dry evaporator conditions (win < wadp)
            if (wADP > InletHumRatCalc || (Counter >= 1 && Counter < MaxIter)) {
                if (InletHumRatCalc == 0.0) InletHumRatCalc = 0.00001;
                werror = (InletHumRatCalc - wADP) / InletHumRatCalc;
                //     Increase InletHumRatCalc at constant inlet air temp to find coil dry-out point. Then use the
                //     capacity at the dry-out point to determine exiting conditions from coil. This is required
                //     since the TotCapTempModFac doesn't work properly with dry-coil conditions.
                InletHumRatCalc = RF * wADP + (1.0 - RF) * InletHumRatCalc;
                InletWetBulbCalc = PsyTwbFnTdbWPb(state, InletDryBulb, InletHumRatCalc, Pressure);
                ++Counter;
                if (std::abs(werror) > Tolerance) {
                    LoopOn = true; // Recalculate with modified inlet conditions
                } else {
                    LoopOn = false;
                }
            } else {
                LoopOn = false;
            }
        } // END LOOP

        //  Calculate full load output conditions
        if (SHRCalc > 1.0 || Counter > 0) SHRCalc = 1.0; // if Counter > 0 means a dry coil so SHR = 1

        SHR = SHRCalc;
        TotCap1 = TotCapCalc1;
        TotCap2 = TotCapCalc2;
        TotCapSpeed = TotCapCalc;
        InletWetBulb = InletWetBulbCalc;
    }

    Real64 getVarSpeedPartLoadRatio(EnergyPlusData &state, int const DXCoilNum)
    {
        return state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).PartLoadRatio;
    }

} // namespace VariableSpeedCoils

} // namespace EnergyPlus
