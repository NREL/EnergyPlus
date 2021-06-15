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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace HVACHXAssistedCoolingCoil {
    // Module containing the simulation routines for a heat exchanger-
    // assisted cooling coil

    // MODULE INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   Sept 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    //  To encapsulate the data and algorithms required to
    //  manage the heat exchanger-assisted cooling coil compound component

    // METHODOLOGY EMPLOYED:
    //  Call the air-to-air heat exchanger and cooling coil repeatedly to converge
    //  on the solution instead of relying on the air loop manager for iterations

    // REFERENCES:
    // Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
    // Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
    //   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
    //   Climates, July 24-26, 2006.

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataHVACGlobals;

    void SimHXAssistedCoolingCoil(EnergyPlusData &state,
                                  std::string_view HXAssistedCoilName, // Name of HXAssistedCoolingCoil
                                  bool const FirstHVACIteration,         // FirstHVACIteration flag
                                  int const CompOp,                      // compressor operation; 1=on, 0=off
                                  Real64 const PartLoadRatio,            // Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
                                  int &CompIndex,
                                  int const FanOpMode,                // Allows the parent object to control fan operation
                                  Optional_bool_const HXUnitEnable,   // flag to enable heat exchanger heat recovery
                                  Optional<Real64 const> OnOffAFR,    // Ratio of compressor ON air mass flow rate to AVERAGE over time step
                                  Optional_bool_const EconomizerFlag, // OA sys or air loop economizer status
                                  Optional<Real64> QTotOut            // the total cooling output of unit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sept 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine manages the simulation of the
        //  cooling coil/heat exchanger combination.

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // (not used for Coil:Water:DetailedFlatCooling)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HXAssistedCoilNum; // Index for HXAssistedCoolingCoil
        Real64 AirFlowRatio;   // Ratio of compressor ON air mass flow rate to AVEARAGE over time step
        bool HXUnitOn;         // flag to enable heat exchanger
        Real64 AirMassFlow;    // HX System air mass flow rate
        int InletNodeNum;      // HX System inlet node number
        int OutletNodeNum;     // HX System outlet node number

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        // Find the correct HXAssistedCoolingCoil number
        if (CompIndex == 0) {
            HXAssistedCoilNum = UtilityRoutines::FindItemInList(HXAssistedCoilName, state.dataHVACAssistedCC->HXAssistedCoil);
            if (HXAssistedCoilNum == 0) {
                ShowFatalError(state, "HX Assisted Coil not found=" + std::string{HXAssistedCoilName});
            }
            CompIndex = HXAssistedCoilNum;
        } else {
            HXAssistedCoilNum = CompIndex;
            if (HXAssistedCoilNum > state.dataHVACAssistedCC->TotalNumHXAssistedCoils || HXAssistedCoilNum < 1) {
                ShowFatalError(state,
                               format("SimHXAssistedCoolingCoil: Invalid CompIndex passed={}, Number of HX Assisted Cooling Coils={}, Coil name={}",
                                      HXAssistedCoilNum,
                                      state.dataHVACAssistedCC->TotalNumHXAssistedCoils,
                                      HXAssistedCoilName));
            }
            if (state.dataHVACAssistedCC->CheckEquipName(HXAssistedCoilNum)) {
                if (!HXAssistedCoilName.empty() && HXAssistedCoilName != state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name) {
                    ShowFatalError(state,
                                   format("SimHXAssistedCoolingCoil: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                                          HXAssistedCoilNum,
                                          HXAssistedCoilName,
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name));
                }
                state.dataHVACAssistedCC->CheckEquipName(HXAssistedCoilNum) = false;
            }
        }

        // Initialize HXAssistedCoolingCoil Flows
        InitHXAssistedCoolingCoil(state, HXAssistedCoilNum);

        if (present(HXUnitEnable)) {
            HXUnitOn = HXUnitEnable;
        } else {
            HXUnitOn = true;
        }

        if (CompOp == Off) {
            HXUnitOn = false;
        }

        // Calculate the HXAssistedCoolingCoil performance and the coil outlet conditions
        if (present(OnOffAFR)) {
            AirFlowRatio = OnOffAFR;
        } else {
            AirFlowRatio = 1.0;
        }
        CalcHXAssistedCoolingCoil(
            state, HXAssistedCoilNum, FirstHVACIteration, CompOp, PartLoadRatio, HXUnitOn, FanOpMode, AirFlowRatio, EconomizerFlag);

        // Update the current HXAssistedCoil output
        //  Call UpdateHXAssistedCoolingCoil(HXAssistedCoilNum), not required. Updates done by the HX and cooling coil components.

        // Report the current HXAssistedCoil output
        //  Call ReportHXAssistedCoolingCoil(HXAssistedCoilNum), not required. No reporting variables for this compound component.

        if (present(QTotOut)) {
            InletNodeNum = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum;
            OutletNodeNum = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum;
            AirMassFlow = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate;
            QTotOut = AirMassFlow * (state.dataLoopNodes->Node(InletNodeNum).Enthalpy - state.dataLoopNodes->Node(OutletNodeNum).Enthalpy);
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetHXAssistedCoolingCoilInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sept 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Obtains input data for this compount object and stores it in data structure

        // METHODOLOGY EMPLOYED:
        //  Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using DXCoils::GetDXCoilIndex;
        using HeatRecovery::GetSecondaryInletNode;
        using HeatRecovery::GetSecondaryOutletNode;
        using HeatRecovery::GetSupplyInletNode;
        using HeatRecovery::GetSupplyOutletNode;
        using HVACControllers::GetControllerNameAndIndex;
        using NodeInputManager::GetOnlySingleNode;
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetDXCoilInletNode(DXCoils::GetCoilInletNode);
        auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
        auto &GetWaterCoilInletNode(WaterCoils::GetCoilInletNode);
        auto &GetWaterCoilOutletNode(WaterCoils::GetCoilOutletNode);

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetHXAssistedCoolingCoilInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HXAssistedCoilNum;       // Index number of the HXAssistedCoolingCoil for which input data is being read from the idf
        int NumAlphas;               // Number of alpha inputs
        int NumNums;                 // Number of number inputs
        int IOStat;                  // Return status from GetObjectItem call
        bool ErrorsFound(false);     // set TRUE if errors detected in input
        int NumHXAssistedDXCoils;    // Number of HXAssistedCoolingCoil objects using a DX coil
        int NumHXAssistedWaterCoils; // Number of HXAssistedCoolingCoil objects using a chilled water coil
        //    LOGICAL :: FanErrFlag              ! Error flag for fan operating mode mining call
        bool HXErrFlag;                   // Error flag for HX node numbers mining call
        bool CoolingCoilErrFlag;          // Error flag for cooling coil node numbers mining call
        int SupplyAirInletNode;           // supply air inlet node number mined from heat exchanger object (ExchCond structure)
        int SupplyAirOutletNode;          // supply air outlet node number mined from heat exchanger object (ExchCond structure)
        int SecondaryAirInletNode;        // secondary air inlet node number mined from heat exchanger object (ExchCond structure)
        int SecondaryAirOutletNode;       // secondary air outlet node number mined from heat exchanger object (ExchCond structure)
        int CoolingCoilInletNodeNum;      // air outlet node number of cooling coil, used for warning messages
        int CoolingCoilWaterInletNodeNum; // water coil water inlet node number used to find controller index
        int CoolingCoilOutletNodeNum;     // air outlet node number of cooling coil, used for warning messages
        std::string CurrentModuleObject;  // Object type for getting and error messages
        Array1D_string AlphArray;         // Alpha input items for object
        Array1D_string cAlphaFields;      // Alpha field names
        Array1D_string cNumericFields;    // Numeric field names
        Array1D<Real64> NumArray;         // Numeric input items for object
        Array1D_bool lAlphaBlanks;        // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;      // Logical array, numeric field input BLANK = .TRUE.
        int MaxNums(0);                   // Maximum number of numeric input fields
        int MaxAlphas(0);                 // Maximum number of alpha input fields
        int TotalArgs(0);                 // Total number of alpha and numeric arguments (max) for a

        NumHXAssistedDXCoils = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "CoilSystem:Cooling:DX:HeatExchangerAssisted");
        NumHXAssistedWaterCoils =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "CoilSystem:Cooling:Water:HeatExchangerAssisted");
        state.dataHVACAssistedCC->TotalNumHXAssistedCoils = NumHXAssistedDXCoils + NumHXAssistedWaterCoils;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            state.dataHVACAssistedCC->HXAssistedCoil.allocate(state.dataHVACAssistedCC->TotalNumHXAssistedCoils);
            state.dataHVACAssistedCC->HXAssistedCoilOutletTemp.allocate(state.dataHVACAssistedCC->TotalNumHXAssistedCoils);
            state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat.allocate(state.dataHVACAssistedCC->TotalNumHXAssistedCoils);
            state.dataHVACAssistedCC->CheckEquipName.dimension(state.dataHVACAssistedCC->TotalNumHXAssistedCoils, true);
            state.dataHVACAssistedCC->UniqueHXAssistedCoilNames.reserve(state.dataHVACAssistedCC->TotalNumHXAssistedCoils);
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "CoilSystem:Cooling:DX:HeatExchangerAssisted", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "CoilSystem:Cooling:Water:HeatExchangerAssisted", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        NumArray.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        // Get the data for the Coil:DX:CoolingHeatExchangerAssisted objects
        CurrentModuleObject = "CoilSystem:Cooling:DX:HeatExchangerAssisted";

        for (HXAssistedCoilNum = 1; HXAssistedCoilNum <= NumHXAssistedDXCoils; ++HXAssistedCoilNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     HXAssistedCoilNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataHVACAssistedCC->UniqueHXAssistedCoilNames, AlphArray(1), CurrentModuleObject, ErrorsFound);

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name = AlphArray(1);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType = AlphArray(2);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName = AlphArray(3);

            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                                            "HeatExchanger:AirToAir:SensibleAndLatent")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType_Num = HX_AIRTOAIR_GENERIC;
            } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                                                   "HeatExchanger:AirToAir:FlatPlate")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE;
            } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                                                   "HeatExchanger:Desiccant:BalancedFlow")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType_Num = HX_DESICCANT_BALANCED;
            } else {
                ShowWarningError(state,
                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                ShowContinueError(state,
                                  "Invalid " + cAlphaFields(2) + "=\"" +
                                      state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "\"");
                ErrorsFound = true;
            }

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType = AlphArray(4);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName = AlphArray(5);

            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                            "Coil:Cooling:DX:SingleSpeed")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num = CoilDX_CoolingSingleSpeed;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType = CurrentModuleObject;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType_Num = CoilDX_CoolingHXAssisted;
                CoolingCoilErrFlag = false;
                GetDXCoilIndex(state,
                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex,
                               CoolingCoilErrFlag,
                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                               ObjexxFCL::Optional_bool_const());
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                   "Coil:Cooling:DX:VariableSpeed")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType = CurrentModuleObject;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType_Num = CoilDX_CoolingHXAssisted;
                CoolingCoilErrFlag = false;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex =
                    VariableSpeedCoils::GetCoilIndexVariableSpeed(state, AlphArray(4), AlphArray(5), CoolingCoilErrFlag);

                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ErrorsFound = true;
                }
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).DXCoilNumOfSpeeds = VariableSpeedCoils::GetVSCoilNumOfSpeeds(
                    state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                ShowContinueError(
                    state, "Invalid " + cAlphaFields(4) + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "\"");
                ErrorsFound = true;
            }

            HXErrFlag = false;
            SupplyAirInletNode = GetSupplyInletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            HXErrFlag = false;
            SupplyAirOutletNode =
                GetSupplyOutletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            HXErrFlag = false;
            SecondaryAirInletNode =
                GetSecondaryInletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            HXErrFlag = false;
            SecondaryAirOutletNode =
                GetSecondaryOutletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                            "Coil:Cooling:DX:SingleSpeed")) {
                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum = GetDXCoilInletNode(state,
                                                             state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                             state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                             CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...Occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                }
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The supply air outlet node name in heat exchanger = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil inlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\"");
                    ShowContinueError(state,
                                      "Heat exchanger supply air outlet node name=\"" + state.dataLoopNodes->NodeID(SupplyAirOutletNode) + "\"");
                    ShowContinueError(state, "Cooling coil air inlet node name=\"" + state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum) + "\"");
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum = GetDXCoilOutletNode(state,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                               CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...Occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                }
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The secondary air inlet node name in heat exchanger =" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil air outlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\".");
                    ShowContinueError(
                        state, "Heat exchanger secondary air inlet node name =\"" + state.dataLoopNodes->NodeID(SecondaryAirInletNode) + "\".");
                    ShowContinueError(state, "Cooling coil air outlet node name =\"" + state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum) + "\".");
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                   "Coil:Cooling:DX:VariableSpeed")) {
                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum =
                    VariableSpeedCoils::GetCoilInletNodeVariableSpeed(state,
                                                                      state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                                      state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                                      CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...Occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                }
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The supply air outlet node name in heat exchanger = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil inlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\"");
                    ShowContinueError(state,
                                      "Heat exchanger supply air outlet node name=\"" + state.dataLoopNodes->NodeID(SupplyAirOutletNode) + "\"");
                    ShowContinueError(state, "Cooling coil air inlet node name=\"" + state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum) + "\"");
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum =
                    VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(state,
                                                                       state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                                       state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                                       CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state,
                                      "...Occurs in " + CurrentModuleObject + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                }
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The secondary air inlet node name in heat exchanger =" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil air outlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\".");
                    ShowContinueError(
                        state, "Heat exchanger secondary air inlet node name =\"" + state.dataLoopNodes->NodeID(SecondaryAirInletNode) + "\".");
                    ShowContinueError(state, "Cooling coil air outlet node name =\"" + state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum) + "\".");
                    ErrorsFound = true;
                }
            }

            TestCompSet(state,
                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                        state.dataLoopNodes->NodeID(SupplyAirInletNode),
                        state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                        "Air Nodes");

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Inlet,
                                  1,
                                  ObjectIsParent);
            CoolingCoilInletNodeNum = GetOnlySingleNode(state,
                                                        state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                        ErrorsFound,
                                                        CurrentModuleObject,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Internal,
                                                        1,
                                                        ObjectIsParent);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Internal,
                                  1,
                                  ObjectIsParent);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Outlet,
                                  1,
                                  ObjectIsParent);

            // Add cooling coil to component sets array
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                          state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                          state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                          "Air Nodes");
            // Add heat exchanger to component sets array
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName,
                          state.dataLoopNodes->NodeID(SupplyAirInletNode),
                          state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                          "Process Air Nodes");
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName,
                          state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                          state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                          "Secondary Air Nodes");

        } // End of the Coil:DX:CoolingHXAssisted Loop

        // Get the data for the Coil:Water:CoolingHeatExchangerAssisted objects
        CurrentModuleObject = "CoilSystem:Cooling:Water:HeatExchangerAssisted";

        for (HXAssistedCoilNum = NumHXAssistedDXCoils + 1; HXAssistedCoilNum <= NumHXAssistedWaterCoils; ++HXAssistedCoilNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     HXAssistedCoilNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataHVACAssistedCC->UniqueHXAssistedCoilNames, AlphArray(1), CurrentModuleObject, ErrorsFound);

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name = AlphArray(1);

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType = AlphArray(2);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName = AlphArray(3);

            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                                            "HeatExchanger:AirToAir:SensibleAndLatent")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType_Num = HX_AIRTOAIR_GENERIC;
            } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                                                   "HeatExchanger:AirToAir:FlatPlate")) {
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE;
                //       balanced desiccant HX not allowed with water coils at this time
                //       ELSEIF(UtilityRoutines::SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:Desiccant:BalancedFlow'))
                //       THEN
                //         HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_DESICCANT_BALANCED
            } else {
                ShowWarningError(state,
                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                ShowContinueError(state,
                                  "Invalid " + cAlphaFields(2) + "=\"" +
                                      state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "\"");
                ErrorsFound = true;
            }

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType = AlphArray(4);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName = AlphArray(5);

            HXErrFlag = false;
            SupplyAirInletNode = GetSupplyInletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            HXErrFlag = false;
            SupplyAirOutletNode =
                GetSupplyOutletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state,
                                  "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name);
            }

            HXErrFlag = false;
            SecondaryAirInletNode =
                GetSecondaryInletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            HXErrFlag = false;
            SecondaryAirOutletNode =
                GetSecondaryOutletNode(state, state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(
                    state, "...Occurs in " + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
            }

            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType, "Coil:Cooling:Water") ||
                UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                            "Coil:Cooling:Water:DetailedGeometry")) {
                if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                "Coil:Cooling:Water:DetailedGeometry")) {
                    state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num = Coil_CoolingWaterDetailed;
                } else if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                       "Coil:Cooling:Water")) {
                    state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num = Coil_CoolingWater;
                }

                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType = CurrentModuleObject;
                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType_Num = CoilWater_CoolingHXAssisted;

                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum = GetWaterCoilInletNode(state,
                                                                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                                CoolingCoilErrFlag);
                CoolingCoilWaterInletNodeNum = GetCoilWaterInletNode(state,
                                                                     state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                                     state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                                     CoolingCoilErrFlag);
                GetControllerNameAndIndex(state,
                                          CoolingCoilWaterInletNodeNum,
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).ControllerName,
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).ControllerIndex,
                                          CoolingCoilErrFlag);
                if (CoolingCoilErrFlag)
                    ShowContinueError(state,
                                      "...occurs in " + CurrentModuleObject + " \"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The supply air outlet node name in heat exchanger = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil inlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\"");
                    ShowContinueError(state,
                                      "Heat exchanger supply air outlet node name =\"" + state.dataLoopNodes->NodeID(SupplyAirOutletNode) + "\"");
                    ShowContinueError(state, "Cooling coil air inlet node name = \"" + state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum) + "\"");
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum = GetWaterCoilOutletNode(state,
                                                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                                                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                                  CoolingCoilErrFlag);
                if (CoolingCoilErrFlag)
                    ShowContinueError(state,
                                      "...occurs in " + CurrentModuleObject + " \"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      "The secondary air inlet node name in heat exchanger = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName + "\"");
                    ShowContinueError(state,
                                      "must match the cooling coil air outlet node name in = " +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "=\"" +
                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName + "\".");
                    ShowContinueError(
                        state, "Heat exchanger secondary air inlet node name = \"" + state.dataLoopNodes->NodeID(SecondaryAirInletNode) + "\".");
                    ShowContinueError(state,
                                      "Cooling coil air outlet node name = \"" + state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum) + "\".");
                    ErrorsFound = true;
                }

            } else {
                ShowWarningError(state,
                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name + "\"");
                ShowContinueError(
                    state, "Invalid " + cAlphaFields(4) + "=\"" + state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType + "\"");
                ErrorsFound = true;
            }

            TestCompSet(state,
                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                        state.dataLoopNodes->NodeID(SupplyAirInletNode),
                        state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                        "Air Nodes");

            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Inlet,
                                  1,
                                  ObjectIsParent);
            CoolingCoilInletNodeNum = GetOnlySingleNode(state,
                                                        state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                        ErrorsFound,
                                                        CurrentModuleObject,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Internal,
                                                        1,
                                                        ObjectIsParent);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Internal,
                                  1,
                                  ObjectIsParent);
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum =
                GetOnlySingleNode(state,
                                  state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Outlet,
                                  1,
                                  ObjectIsParent);

            // Add cooling coil to component sets array
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                          state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                          state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                          "Air Nodes");
            // Add heat exchanger to component sets array
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName,
                          state.dataLoopNodes->NodeID(SupplyAirInletNode),
                          state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                          "Process Air Nodes");
            SetUpCompSets(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerType,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName,
                          state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                          state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                          "Secondary Air Nodes");

        } // End of the Coil:Water:CoolingHXAssisted Loop

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        NumArray.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Previous error condition causes termination.");
        }
    }

    // End of Get Input subroutines for this Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitHXAssistedCoolingCoil(EnergyPlusData &state, int const HXAssistedCoilNum) // index for HXAssistedCoolingCoil
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sep 2003
        //       MODIFIED       R. Raustad, June 2007 now using FullLoadOutletConditions from DX Coil data structure
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for initializations of the HXAssistedCoolingCoil components

        // METHODOLOGY EMPLOYED:
        //  Uses the status flags to trigger initializations.

        // Do these initializations every time
        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).MassFlowRate =
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum).MassFlowRate;

        if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
            state.dataDXCoils->DXCoilFullLoadOutAirTemp(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex) = 0.0;
            state.dataDXCoils->DXCoilFullLoadOutAirHumRat(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex) = 0.0;
        } else if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num ==
                   DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
            //
        }
    }

    // End Initialization Section of the Module
    //******************************************************************************

    void CalcHXAssistedCoolingCoil(EnergyPlusData &state,
                                   int const HXAssistedCoilNum,         // Index number for HXAssistedCoolingCoil
                                   bool const FirstHVACIteration,       // FirstHVACIteration flag
                                   int const CompOp,                    // compressor operation; 1=on, 0=off
                                   Real64 const PartLoadRatio,          // Cooling coil part load ratio
                                   bool const HXUnitOn,                 // Flag to enable heat exchanger
                                   int const FanOpMode,                 // Allows parent object to control fan operation
                                   Optional<Real64 const> OnOffAirFlow, // Ratio of compressor ON air mass flow to AVERAGE over time step
                                   Optional_bool_const EconomizerFlag   // OA (or airloop) econommizer status
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sept 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine models the cooling coil/air-to-air heat exchanger
        //  combination. The cooling coil exiting air temperature is used as
        //  an indicator of convergence.

        // Using/Aliasing
        using DXCoils::SimDXCoil;
        using HeatRecovery::SimHeatRecovery;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(50); // Maximum number of iterations

        Real64 AirMassFlow;        // Inlet air mass flow rate
        Real64 Error;              // Error (exiting coil temp from last iteration minus current coil exiting temp)
        Real64 ErrorLast;          // check for oscillations
        int Iter;                  // Number of iterations
        int CompanionCoilIndexNum; // Index to DX coil

        AirMassFlow = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).MassFlowRate;
        Error = 1.0;       // Initialize error (CoilOutputTemp last iteration minus current CoilOutputTemp)
        ErrorLast = Error; // initialize variable used to test loop termination
        Iter = 0;          // Initialize iteration counter to zero

        // Set mass flow rate at inlet of exhaust side of heat exchanger to supply side air mass flow rate entering this compound object
        state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).MassFlowRate = AirMassFlow;

        if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed ||
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
            CompanionCoilIndexNum = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex;
        } else {
            CompanionCoilIndexNum = 0;
        }

        // First call to RegulaFalsi uses PLR=0. Nodes are typically setup at full output on this call.
        // A large number of iterations are required to get to result (~36 iterations to get to PLR=0 node conditions).
        // Reset node data to minimize iteration. This initialization reduces the number of iterations by 50%.
        // CAUTION: Do not use Node(x) = Node(y) here, this can overwrite the coil outlet node setpoint.
        if (PartLoadRatio == 0.0) {
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).Temp =
                state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum).Temp;
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).HumRat =
                state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum).HumRat;
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).Enthalpy =
                state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum).Enthalpy;
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).MassFlowRate =
                state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum).MassFlowRate;
        }

        // Force at least 2 iterations to pass outlet node information
        while ((std::abs(Error) > 0.0005 && Iter <= MaxIter) || Iter < 2) {

            SimHeatRecovery(state,
                            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerName,
                            FirstHVACIteration,
                            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HeatExchangerIndex,
                            FanOpMode,
                            PartLoadRatio,
                            HXUnitOn,
                            CompanionCoilIndexNum,
                            _,
                            EconomizerFlag,
                            _,
                            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num);

            if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
                SimDXCoil(state,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                          CompOp,
                          FirstHVACIteration,
                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex,
                          FanOpMode,
                          PartLoadRatio,
                          OnOffAirFlow);
            } else if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num ==
                       DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                Real64 QZnReq(-1.0);               // Zone load (W), input to variable-speed DX coil
                Real64 QLatReq(0.0);               // Zone latent load, input to variable-speed DX coil
                Real64 MaxONOFFCyclesperHour(4.0); // Maximum cycling rate of heat pump [cycles/hr]
                Real64 HPTimeConstant(0.0);        // Heat pump time constant [s]
                Real64 FanDelayTime(0.0);          // Fan delay time, time delay for the HP's fan to
                Real64 OnOffAirFlowRatio(1.0);     // ratio of compressor on flow to average flow over time step
                int CompOn = CompOp;
                if (PartLoadRatio == 0.0) CompOn = 0;
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex,
                                                          FanOpMode,
                                                          MaxONOFFCyclesperHour,
                                                          HPTimeConstant,
                                                          FanDelayTime,
                                                          CompOn,
                                                          PartLoadRatio,
                                                          state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).DXCoilNumOfSpeeds,
                                                          QZnReq,
                                                          QLatReq,
                                                          OnOffAirFlowRatio); // call vs coil model at top speed.
            } else {
                SimulateWaterCoilComponents(state,
                                            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilName,
                                            FirstHVACIteration,
                                            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilIndex);
            }

            Error = state.dataHVACAssistedCC->CoilOutputTempLast -
                    state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).Temp;
            if (Iter > 40) { // check for oscillation (one of these being negative and one positive) before hitting max iteration limit
                if (Error + ErrorLast < 0.000001)
                    Error = 0.0; // result bounced back and forth with same positive and negative result, no possible solution without this check
            }
            ErrorLast = Error;
            state.dataHVACAssistedCC->CoilOutputTempLast =
                state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXExhaustAirInletNodeNum).Temp;
            ++Iter;
        }

        // Write excessive iteration warning messages
        if (Iter > MaxIter) {
            if (state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).MaxIterCounter < 1) {
                ++state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).MaxIterCounter;
                ShowWarningError(state,
                                 format("{} \"{}\" -- Exceeded max iterations ({}) while calculating operating conditions.",
                                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType,
                                        state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                        MaxIter));
                ShowContinueErrorTimeStamp(state, "");
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilType + " \"" +
                                                   state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name +
                                                   "\" -- Exceeded max iterations error continues...",
                                               state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).MaxIterIndex);
            }
        }

        state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(HXAssistedCoilNum) =
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum).Temp;
        state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(HXAssistedCoilNum) =
            state.dataLoopNodes->Node(state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum).HumRat;
    }

    //        End of Reporting subroutines for the HXAssistedCoil Module
    // *****************************************************************************

    void GetHXDXCoilIndex(
        EnergyPlusData &state, std::string const &HXDXCoilName, int &HXDXCoilIndex, bool &ErrorsFound, Optional_string_const CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets an index for a given HX Assisted Cooling Coil -- issues error message if that
        // HX is not a legal HX Assisted Cooling Coil.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            HXDXCoilIndex = UtilityRoutines::FindItem(HXDXCoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            HXDXCoilIndex = 0;
        }

        if (HXDXCoilIndex == 0) {
            if (present(CurrentModuleObject)) {
                ShowSevereError(state, CurrentModuleObject() + ", GetHXDXCoilIndex: HX Assisted Cooling Coil not found=" + HXDXCoilName);
            } else {
                ShowSevereError(state, "GetHXDXCoilIndex: HX Assisted Cooling Coil not found=" + HXDXCoilName);
            }
            ErrorsFound = true;
        }
    }

    void CheckHXAssistedCoolingCoilSchedule(EnergyPlusData &state,
                                            [[maybe_unused]] std::string const &CompType, // unused1208
                                            std::string_view CompName,
                                            Real64 &Value,
                                            int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine provides a method for outside routines to check if
        // the hx assisted cooling coil is scheduled to be on.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HXAssistedCoilNum;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        // Find the correct Coil number
        if (CompIndex == 0) {
            if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
                HXAssistedCoilNum = UtilityRoutines::FindItem(CompName, state.dataHVACAssistedCC->HXAssistedCoil);
            } else {
                HXAssistedCoilNum = 0;
            }

            if (HXAssistedCoilNum == 0) {
                ShowFatalError(state, "CheckHXAssistedCoolingCoilSchedule: HX Assisted Coil not found=" + std::string{CompName});
            }
            CompIndex = HXAssistedCoilNum;
            Value = 1.0; // not scheduled?
        } else {
            HXAssistedCoilNum = CompIndex;
            if (HXAssistedCoilNum > state.dataHVACAssistedCC->TotalNumHXAssistedCoils || HXAssistedCoilNum < 1) {
                ShowFatalError(state,
                               format("CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed={}, Number of Heating Coils={}, Coil name={}",
                                      HXAssistedCoilNum,
                                      state.dataHVACAssistedCC->TotalNumHXAssistedCoils,
                                      CompName));
            }
            if (CompName != state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name) {
                ShowFatalError(
                    state,
                    format("CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                           HXAssistedCoilNum,
                           CompName,
                           state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name));
            }

            Value = 1.0; // not scheduled?
        }
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
        auto &GetDXCoilCapacity(DXCoils::GetCoilCapacity);
        using WaterCoils::GetWaterCoilCapacity;

        // Return value
        Real64 CoilCapacity; // returned capacity of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;
        auto &ErrCount = state.dataHVACAssistedCC->ErrCount;
        bool errFlag;

        auto &HXAssistedCoil = state.dataHVACAssistedCC->HXAssistedCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        errFlag = false;

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
            if (WhichCoil != 0) {
                // coil does not have capacity in input so mine information from DX cooling coil

                if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    CoilCapacity = GetDXCoilCapacity(state,
                                                     state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                     state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                     errFlag);
                } else if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num ==
                           DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    CoilCapacity =
                        VariableSpeedCoils::GetCoilCapacityVariableSpeed(state,
                                                                         state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                                         state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                                         errFlag);
                }
                if (errFlag) {
                    ShowRecurringWarningErrorAtEnd(state, "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", ErrCount);
                }
            }
        } else if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
            if (WhichCoil != 0) {
                // coil does not have capacity in input so mine information from DX cooling coil
                CoilCapacity = GetWaterCoilCapacity(state,
                                                    state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                    state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                    errFlag);
                if (errFlag) {
                    ShowRecurringWarningErrorAtEnd(state, "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", ErrCount);
                }
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilCapacity: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ShowContinueError(state, "... Coil Capacity returned as -1000.");
            ErrorsFound = true;
            CoilCapacity = -1000.0;
        }

        if (errFlag) ErrorsFound = true;

        return CoilCapacity;
    }

    int GetCoilGroupTypeNum(EnergyPlusData &state,
                            std::string const &CoilType,     // must match coil types in this module
                            std::string const &CoilName,     // must match coil names for the coil type
                            bool &ErrorsFound,               // set to true if problem
                            Optional_bool_const PrintWarning // prints warning message if true
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the HX coil type and returns it (CoilDX_CoolingHXAssisted, CoilWater_CoolingHXAssisted)
        // If incorrect coil type or name is given, ErrorsFound is returned as true.

        // Return value
        int TypeNum; // returned integerized type of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;
        bool PrintMessage;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (present(PrintWarning)) {
            PrintMessage = PrintWarning;
        } else {
            PrintMessage = true;
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            // coil does not have capacity in input so mine information from DX cooling coil
            TypeNum = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilType_Num;
        } else {
            if (PrintMessage) {
                ShowSevereError(state, "GetCoilGroupTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            }
            ErrorsFound = true;
            TypeNum = 0;
        }

        return TypeNum;
    }

    int GetCoilObjectTypeNum(EnergyPlusData &state,
                             std::string const &CoilType,     // must match coil types in this module
                             std::string const &CoilName,     // must match coil names for the coil type
                             bool &ErrorsFound,               // set to true if problem
                             Optional_bool_const PrintWarning // prints warning message if true
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   April 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil object type for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        int TypeNum; // returned integerized type of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;
        bool PrintMessage;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (present(PrintWarning)) {
            PrintMessage = PrintWarning;
        } else {
            PrintMessage = true;
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            TypeNum = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num;
        } else {
            if (PrintMessage) {
                ShowSevereError(state, "GetCoilObjectTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            }
            ErrorsFound = true;
            TypeNum = 0;
        }

        return TypeNum;
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
        // This function looks up the given coil and returns the inlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            NodeNumber = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilInletNodeNum;
        } else {
            ShowSevereError(state, "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetCoilWaterInletNode(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the inlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Using/Aliasing
        auto &GetWaterCoilWaterInletNode(WaterCoils::GetCoilWaterInletNode);

        // Return value
        int NodeNumber; // returned node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        auto &HXAssistedCoil = state.dataHVACAssistedCC->HXAssistedCoil;
        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == Coil_CoolingWater) {
                NodeNumber = GetWaterCoilWaterInletNode(state,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                        ErrorsFound);
            } else if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == Coil_CoolingWaterDetailed) {
                NodeNumber = GetWaterCoilWaterInletNode(state,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                        state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                        ErrorsFound);
            } else { // even though validated in Get, still check.
                ShowSevereError(state,
                                "GetCoilWaterInletNode: Invalid Cooling Coil for HX Assisted Coil, Type=\"" +
                                    state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType + "\" Name=\"" + CoilName + "\"");
                ErrorsFound = true;
                NodeNumber = 0; // Autodesk:Return Added line to set return value
            }
        } else {
            ShowSevereError(state, "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
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
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the outlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            NodeNumber = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilOutletNodeNum;
        } else {
            ShowSevereError(state, "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    std::string GetHXDXCoilType(EnergyPlusData &state,
                                std::string const &CoilType, // must match coil types in this module
                                std::string const &CoilName, // must match coil names for the coil type
                                bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad, FSEC
        //       DATE WRITTEN   September 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil type.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Return value
        std::string DXCoilType; // returned type of cooling coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            DXCoilType = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
        } else {
            ShowSevereError(state, "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            DXCoilType = "";
        }

        return DXCoilType;
    }

    std::string GetHXDXCoilName(EnergyPlusData &state,
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
        // This function looks up the given coil and returns the cooling coil name.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Return value
        std::string DXCoilName; // returned name of cooling coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            DXCoilName = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName;
        } else {
            ShowSevereError(state, "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            DXCoilName = "";
        }

        return DXCoilName;
    }

    int GetActualDXCoilIndex(EnergyPlusData &state,
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
        // This function looks up the given coil and returns the cooling coil name.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Return value
        int DXCoilIndex; // returned index of DX cooling coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            // this should be the index to the DX cooling coil object, not the HXAssisted object
            DXCoilIndex = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilIndex;
        } else {
            ShowSevereError(state, "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            DXCoilIndex = 0;
        }

        return DXCoilIndex;
    }

    std::string GetHXCoilType(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil type.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the cooling
        // coil type is returned as blank.

        // Return value
        std::string CoolingCoilType; // returned type of cooling coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            CoolingCoilType = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
        } else {
            ShowSevereError(state, "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CoolingCoilType = "";
        }

        return CoolingCoilType;
    }

    void GetHXCoilTypeAndName(EnergyPlusData &state,
                              std::string const &CoilType,  // must match coil types in this module
                              std::string const &CoilName,  // must match coil names for the coil type
                              bool &ErrorsFound,            // set to true if problem
                              std::string &CoolingCoilType, // returned type of cooling coil
                              std::string &CoolingCoilName  // returned name of cooling coil
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Oct 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Need to get child coil type and name.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            CoolingCoilType = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
            CoolingCoilName = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName;
        } else {
            ShowSevereError(state, "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            CoolingCoilType = "";
            CoolingCoilName = "";
        }
    }

    Real64 GetCoilMaxWaterFlowRate(EnergyPlusData &state,
                                   std::string const &CoilType, // must match coil types in this module
                                   std::string const &CoilName, // must match coil names for the coil type
                                   bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   November 2006
        //       MODIFIED       R. Raustad, April 2009 - added water coil ELSE IF
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max water flow rate for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Using/Aliasing
        auto &GetWaterCoilMaxFlowRate(WaterCoils::GetCoilMaxWaterFlowRate);

        // Return value
        Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;
        auto &ErrCount = state.dataHVACAssistedCC->ErrCount2;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {

            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);

            if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    // coil does not specify MaxWaterFlowRate
                    MaxWaterFlowRate = 0.0;
                    ShowRecurringWarningErrorAtEnd(
                        state, "Requested Max Water Flow Rate from CoilSystem:Cooling:DX:HeatExchangerAssisted N/A", ErrCount);
                }
            } else if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    MaxWaterFlowRate = GetWaterCoilMaxFlowRate(state,
                                                               cAllCoilTypes(GetCoilObjectTypeNum(state, CoilType, CoilName, ErrorsFound)),
                                                               GetHXDXCoilName(state, CoilType, CoilName, ErrorsFound),
                                                               ErrorsFound);
                }
            } else {
                WhichCoil = 0;
            }

            if (WhichCoil == 0) {
                ShowSevereError(state, "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
                ErrorsFound = true;
                MaxWaterFlowRate = -1000.0;
            }
        } else {
            ShowSevereError(state, "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            MaxWaterFlowRate = -1000.0;
        }

        return MaxWaterFlowRate;
    }

    Real64 GetHXCoilAirFlowRate(EnergyPlusData &state,
                                std::string const &CoilType, // must match coil types in this module
                                std::string const &CoilName, // must match coil names for the coil type
                                bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   September 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max air flow rate for the given HX and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Using/Aliasing
        using HeatRecovery::GetSupplyAirFlowRate;

        // Return value
        Real64 MaxAirFlowRate; // returned max air flow rate of matched HX

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {

            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);

            if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted") ||
                UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    MaxAirFlowRate = GetSupplyAirFlowRate(state, state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HeatExchangerName, ErrorsFound);
                }
            } else {
                WhichCoil = 0;
            }

            if (WhichCoil == 0) {
                ShowSevereError(state, "GetHXCoilAirFlowRate: Could not find HX, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
                ErrorsFound = true;
                MaxAirFlowRate = -1000.0;
            }
        } else {
            ShowSevereError(state, "GetHXCoilAirFlowRate: Could not find HX, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            MaxAirFlowRate = -1000.0;
        }

        return MaxAirFlowRate;
    }

    bool VerifyHeatExchangerParent(EnergyPlusData &state,
                                   std::string const &HXType, // must match coil types in this module
                                   std::string const &HXName  // must match coil names for the coil type
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   January 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given heat exchanger name and type and returns true or false.

        // Return value
        bool Found; // set to true if found

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        Found = false;

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(HXName, state.dataHVACAssistedCC->HXAssistedCoil, &HXAssistedCoilParameters::HeatExchangerName);
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil != 0) {
            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HeatExchangerType, HXType)) {
                Found = true;
            }
        }

        return Found;
    }

    //        End of Utility subroutines for the HXAssistedCoil Module
    // *****************************************************************************

} // namespace HVACHXAssistedCoolingCoil

} // namespace EnergyPlus
