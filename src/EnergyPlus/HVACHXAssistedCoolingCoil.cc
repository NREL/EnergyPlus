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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
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

    void SimHXAssistedCoolingCoil(EnergyPlusData &state,
                                  std::string_view HXAssistedCoilName,                     // Name of HXAssistedCoolingCoil
                                  bool const FirstHVACIteration,                           // FirstHVACIteration flag
                                  DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation; 1=on, 0=off
                                  Real64 const PartLoadRatio,                              // Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
                                  int &CompIndex,
                                  int const FanOpMode,                         // Allows the parent object to control fan operation
                                  ObjexxFCL::Optional_bool_const HXUnitEnable, // flag to enable heat exchanger heat recovery
                                  ObjexxFCL::Optional<Real64 const> OnOffAFR,  // Ratio of compressor ON air mass flow rate to AVERAGE over time step
                                  ObjexxFCL::Optional_bool_const EconomizerFlag,      // OA sys or air loop economizer status
                                  ObjexxFCL::Optional<Real64> QTotOut,                // the total cooling output of unit
                                  ObjexxFCL::Optional_int_const DehumidificationMode, // Optional dehumbidication mode
                                  ObjexxFCL::Optional<Real64 const> LoadSHR           // Optional CoilSHR pass over
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sept 2003

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine manages the simulation of the
        //  cooling coil/heat exchanger combination.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HXAssistedCoilNum; // Index for HXAssistedCoolingCoil
        Real64 AirFlowRatio;   // Ratio of compressor ON air mass flow rate to AVEARAGE over time step
        bool HXUnitOn;         // flag to enable heat exchanger

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
                ShowFatalError(state, format("HX Assisted Coil not found={}", HXAssistedCoilName));
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

        if (CompressorOp == DataHVACGlobals::CompressorOperation::Off) {
            HXUnitOn = false;
        }

        // Calculate the HXAssistedCoolingCoil performance and the coil outlet conditions
        if (present(OnOffAFR)) {
            AirFlowRatio = OnOffAFR;
        } else {
            AirFlowRatio = 1.0;
        }
        if (present(DehumidificationMode) && present(LoadSHR) &&
            state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).CoolingCoilType_Num == DataHVACGlobals::CoilDX_Cooling) {
            CalcHXAssistedCoolingCoil(state,
                                      HXAssistedCoilNum,
                                      FirstHVACIteration,
                                      CompressorOp,
                                      PartLoadRatio,
                                      HXUnitOn,
                                      FanOpMode,
                                      AirFlowRatio,
                                      EconomizerFlag,
                                      DehumidificationMode,
                                      LoadSHR);
        } else {
            CalcHXAssistedCoolingCoil(
                state, HXAssistedCoilNum, FirstHVACIteration, CompressorOp, PartLoadRatio, HXUnitOn, FanOpMode, AirFlowRatio, EconomizerFlag);
        }

        // Update the current HXAssistedCoil output
        //  Call UpdateHXAssistedCoolingCoil(HXAssistedCoilNum), not required. Updates done by the HX and cooling coil components.

        // Report the current HXAssistedCoil output
        //  Call ReportHXAssistedCoolingCoil(HXAssistedCoilNum), not required. No reporting variables for this compound component.

        if (present(QTotOut)) {
            int InletNodeNum = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilInletNodeNum;
            int OutletNodeNum = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).HXAssistedCoilOutletNodeNum;
            Real64 AirMassFlow = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate;
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

        // PURPOSE OF THIS SUBROUTINE:
        //  Obtains input data for this compount object and stores it in data structure

        // METHODOLOGY EMPLOYED:
        //  Uses "Get" routines to read in data.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetHXAssistedCoolingCoilInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HXAssistedCoilNum;            // Index number of the HXAssistedCoolingCoil for which input data is being read from the idf
        int NumAlphas;                    // Number of alpha inputs
        int NumNums;                      // Number of number inputs
        int IOStat;                       // Return status from GetObjectItem call
        bool ErrorsFound(false);          // set TRUE if errors detected in input
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
        int TotalArgs(0);                 // Total number of alpha and numeric arguments (max) for a

        int NumHXAssistedDXCoils =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "CoilSystem:Cooling:DX:HeatExchangerAssisted");
        int NumHXAssistedWaterCoils =
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
        int MaxNums = NumNums;
        int MaxAlphas = NumAlphas;
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
            auto &thisHXCoil = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum);
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

            thisHXCoil.Name = AlphArray(1);
            thisHXCoil.HeatExchangerType = AlphArray(2);
            thisHXCoil.HeatExchangerName = AlphArray(3);

            if (UtilityRoutines::SameString(thisHXCoil.HeatExchangerType, "HeatExchanger:AirToAir:SensibleAndLatent")) {
                thisHXCoil.HeatExchangerType_Num = DataHVACGlobals::HX_AIRTOAIR_GENERIC;
            } else if (UtilityRoutines::SameString(thisHXCoil.HeatExchangerType, "HeatExchanger:AirToAir:FlatPlate")) {
                thisHXCoil.HeatExchangerType_Num = DataHVACGlobals::HX_AIRTOAIR_FLATPLATE;
            } else if (UtilityRoutines::SameString(thisHXCoil.HeatExchangerType, "HeatExchanger:Desiccant:BalancedFlow")) {
                thisHXCoil.HeatExchangerType_Num = DataHVACGlobals::HX_DESICCANT_BALANCED;
            } else {
                ShowWarningError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                ShowContinueError(state, format("Invalid {}=\"{}\"", cAlphaFields(2), thisHXCoil.HeatExchangerType));
                ErrorsFound = true;
            }

            thisHXCoil.CoolingCoilType = AlphArray(4);
            thisHXCoil.CoolingCoilName = AlphArray(5);

            if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX")) {
                thisHXCoil.CoolingCoilType_Num = DataHVACGlobals::CoilDX_Cooling;
                thisHXCoil.HXAssistedCoilType = CurrentModuleObject;
                thisHXCoil.HXAssistedCoilType_Num = DataHVACGlobals::CoilDX_CoolingHXAssisted;

                CoolingCoilErrFlag = false;
                int coolingCoilIndex_temp = CoilCoolingDX::factory(state, thisHXCoil.CoolingCoilName);
                thisHXCoil.CoolingCoilIndex = coolingCoilIndex_temp;
                if (coolingCoilIndex_temp < 0) {
                    ShowContinueError(state, format("Occurs in {} = {}", CurrentModuleObject, AlphArray(5)));
                    CoolingCoilErrFlag = true;
                    ErrorsFound = true;
                }

                thisHXCoil.DXCoilNumOfSpeeds = state.dataCoilCooingDX->coilCoolingDXs[coolingCoilIndex_temp].performance.normalMode.speeds.size();
                if (thisHXCoil.DXCoilNumOfSpeeds < 1) {
                    CoolingCoilErrFlag = true;
                }
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX:SingleSpeed")) {
                thisHXCoil.CoolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
                thisHXCoil.HXAssistedCoilType = CurrentModuleObject;
                thisHXCoil.HXAssistedCoilType_Num = DataHVACGlobals::CoilDX_CoolingHXAssisted;
                CoolingCoilErrFlag = false;
                DXCoils::GetDXCoilIndex(
                    state, thisHXCoil.CoolingCoilName, thisHXCoil.CoolingCoilIndex, CoolingCoilErrFlag, thisHXCoil.CoolingCoilType);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX:VariableSpeed")) {
                thisHXCoil.CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                thisHXCoil.HXAssistedCoilType = CurrentModuleObject;
                thisHXCoil.HXAssistedCoilType_Num = DataHVACGlobals::CoilDX_CoolingHXAssisted;
                CoolingCoilErrFlag = false;
                thisHXCoil.CoolingCoilIndex = VariableSpeedCoils::GetCoilIndexVariableSpeed(state, AlphArray(4), AlphArray(5), CoolingCoilErrFlag);

                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                    ErrorsFound = true;
                }
                thisHXCoil.DXCoilNumOfSpeeds = VariableSpeedCoils::GetVSCoilNumOfSpeeds(state, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                ShowContinueError(state, format("Invalid {}=\"{}\"", cAlphaFields(4), thisHXCoil.CoolingCoilType));
                ErrorsFound = true;
            }

            HXErrFlag = false;
            SupplyAirInletNode = HeatRecovery::GetSupplyInletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SupplyAirOutletNode = HeatRecovery::GetSupplyOutletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SecondaryAirInletNode = HeatRecovery::GetSecondaryInletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SecondaryAirOutletNode = HeatRecovery::GetSecondaryOutletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX")) {
                CoolingCoilInletNodeNum = state.dataCoilCooingDX->coilCoolingDXs[thisHXCoil.CoolingCoilIndex].evapInletNodeIndex;
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The supply air outlet node name in heat exchanger {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(
                        state,
                        format("must match the cooling coil inlet node name in {}=\"{}\"", thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName));
                    ShowContinueError(state,
                                      format("Heat exchanger supply air outlet node name=\"{}\"", state.dataLoopNodes->NodeID(SupplyAirOutletNode)));
                    ShowContinueError(state, format("Cooling coil air inlet node name=\"{}\"", state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum)));
                    ErrorsFound = true;
                }

                CoolingCoilOutletNodeNum = state.dataCoilCooingDX->coilCoolingDXs[thisHXCoil.CoolingCoilIndex].evapOutletNodeIndex;
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The secondary air inlet node name in heat exchanger {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(state,
                                      format("must match the cooling coil air outlet node name in {}=\"{}\"",
                                             thisHXCoil.CoolingCoilType,
                                             thisHXCoil.CoolingCoilName));
                    ShowContinueError(
                        state, format("Heat exchanger secondary air inlet node name =\"{}\".", state.dataLoopNodes->NodeID(SecondaryAirInletNode)));
                    ShowContinueError(state,
                                      format("Cooling coil air outlet node name =\"{}\".", state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum)));
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX:SingleSpeed")) {
                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum =
                    DXCoils::GetCoilInletNode(state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                }
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The supply air outlet node name in heat exchanger = {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(
                        state,
                        format("must match the cooling coil inlet node name in = {}=\"{}\"", thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName));
                    ShowContinueError(state,
                                      format("Heat exchanger supply air outlet node name=\"{}\"", state.dataLoopNodes->NodeID(SupplyAirOutletNode)));
                    ShowContinueError(state, format("Cooling coil air inlet node name=\"{}\"", state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum)));
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum =
                    DXCoils::GetCoilOutletNode(state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                }
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The secondary air inlet node name in heat exchanger ={}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(state,
                                      format("must match the cooling coil air outlet node name in = {}=\"{}\".",
                                             thisHXCoil.CoolingCoilType,
                                             thisHXCoil.CoolingCoilName));
                    ShowContinueError(
                        state, format("Heat exchanger secondary air inlet node name =\"{}\".", state.dataLoopNodes->NodeID(SecondaryAirInletNode)));
                    ShowContinueError(state,
                                      format("Cooling coil air outlet node name =\"{}\".", state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum)));
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:DX:VariableSpeed")) {
                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(
                    state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                }
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The supply air outlet node name in heat exchanger = {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(
                        state,
                        format("must match the cooling coil inlet node name in = {}=\"{}\"", thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName));
                    ShowContinueError(state,
                                      format("Heat exchanger supply air outlet node name=\"{}\"", state.dataLoopNodes->NodeID(SupplyAirOutletNode)));
                    ShowContinueError(state, format("Cooling coil air inlet node name=\"{}\"", state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum)));
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(
                    state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) {
                    ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
                }
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The secondary air inlet node name in heat exchanger ={}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(state,
                                      format("must match the cooling coil air outlet node name in = {}=\"{}\".",
                                             thisHXCoil.CoolingCoilType,
                                             thisHXCoil.CoolingCoilName));
                    ShowContinueError(
                        state, format("Heat exchanger secondary air inlet node name =\"{}\".", state.dataLoopNodes->NodeID(SecondaryAirInletNode)));
                    ShowContinueError(state,
                                      format("Cooling coil air outlet node name =\"{}\".", state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum)));
                    ErrorsFound = true;
                }
            }

            BranchNodeConnections::TestCompSet(state,
                                               thisHXCoil.HXAssistedCoilType,
                                               thisHXCoil.Name,
                                               state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                               state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                               "Air Nodes");

            thisHXCoil.HXAssistedCoilInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingDXHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Inlet,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);
            // no need to capture CoolingCoilInletNodeNum as the return value, it's not used anywhere
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                ErrorsFound,
                                                DataLoopNode::ConnectionObjectType::CoilSystemCoolingDXHeatExchangerAssisted,
                                                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                                DataLoopNode::NodeFluidType::Air,
                                                DataLoopNode::ConnectionType::Internal,
                                                NodeInputManager::CompFluidStream::Primary,
                                                DataLoopNode::ObjectIsParent);
            thisHXCoil.HXExhaustAirInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingDXHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Internal,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);
            thisHXCoil.HXAssistedCoilOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingDXHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Outlet,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);

            // Add cooling coil to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.CoolingCoilType,
                                                 thisHXCoil.CoolingCoilName,
                                                 state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                 state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                                 "Air Nodes");
            // Add heat exchanger to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.HeatExchangerType,
                                                 thisHXCoil.HeatExchangerName,
                                                 state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                                 state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                 "Process Air Nodes");
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.HeatExchangerType,
                                                 thisHXCoil.HeatExchangerName,
                                                 state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                                 state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                                 "Secondary Air Nodes");

        } // End of the Coil:DX:CoolingHXAssisted Loop

        // Get the data for the Coil:Water:CoolingHeatExchangerAssisted objects
        CurrentModuleObject = "CoilSystem:Cooling:Water:HeatExchangerAssisted";

        for (HXAssistedCoilNum = NumHXAssistedDXCoils + 1; HXAssistedCoilNum <= state.dataHVACAssistedCC->TotalNumHXAssistedCoils;
             ++HXAssistedCoilNum) {
            int thisWaterHXNum = HXAssistedCoilNum - NumHXAssistedDXCoils;
            auto &thisHXCoil = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum);

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     thisWaterHXNum,
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

            thisHXCoil.Name = AlphArray(1);

            thisHXCoil.HeatExchangerType = AlphArray(2);
            thisHXCoil.HeatExchangerName = AlphArray(3);

            if (UtilityRoutines::SameString(thisHXCoil.HeatExchangerType, "HeatExchanger:AirToAir:SensibleAndLatent")) {
                thisHXCoil.HeatExchangerType_Num = DataHVACGlobals::HX_AIRTOAIR_GENERIC;
            } else if (UtilityRoutines::SameString(thisHXCoil.HeatExchangerType, "HeatExchanger:AirToAir:FlatPlate")) {
                thisHXCoil.HeatExchangerType_Num = DataHVACGlobals::HX_AIRTOAIR_FLATPLATE;
                //       balanced desiccant HX not allowed with water coils at this time
                //       ELSEIF(UtilityRoutines::SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:Desiccant:BalancedFlow'))
                //       THEN
                //         HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_DESICCANT_BALANCED
            } else {
                ShowWarningError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                ShowContinueError(state, format("Invalid {}=\"{}\"", cAlphaFields(2), thisHXCoil.HeatExchangerType));
                ErrorsFound = true;
            }

            thisHXCoil.CoolingCoilType = AlphArray(4);
            thisHXCoil.CoolingCoilName = AlphArray(5);

            HXErrFlag = false;
            SupplyAirInletNode = HeatRecovery::GetSupplyInletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SupplyAirOutletNode = HeatRecovery::GetSupplyOutletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SecondaryAirInletNode = HeatRecovery::GetSecondaryInletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            HXErrFlag = false;
            SecondaryAirOutletNode = HeatRecovery::GetSecondaryOutletNode(state, thisHXCoil.HeatExchangerName, HXErrFlag);
            if (HXErrFlag) {
                ShowContinueError(state, format("...Occurs in {}=\"{}\"", CurrentModuleObject, thisHXCoil.Name));
            }

            if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:Water") ||
                UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:Water:DetailedGeometry")) {
                if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:Water:DetailedGeometry")) {
                    thisHXCoil.CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterDetailed;
                } else if (UtilityRoutines::SameString(thisHXCoil.CoolingCoilType, "Coil:Cooling:Water")) {
                    thisHXCoil.CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
                }

                thisHXCoil.HXAssistedCoilType = CurrentModuleObject;
                thisHXCoil.HXAssistedCoilType_Num = DataHVACGlobals::CoilWater_CoolingHXAssisted;

                //         Check node names in heat exchanger and coil objects for consistency
                CoolingCoilErrFlag = false;
                CoolingCoilInletNodeNum =
                    WaterCoils::GetCoilInletNode(state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                CoolingCoilWaterInletNodeNum =
                    WaterCoils::GetCoilWaterInletNode(state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                HVACControllers::GetControllerNameAndIndex(
                    state, CoolingCoilWaterInletNodeNum, thisHXCoil.ControllerName, thisHXCoil.ControllerIndex, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) ShowContinueError(state, format("...occurs in {} \"{}\"", CurrentModuleObject, thisHXCoil.Name));
                if (SupplyAirOutletNode != CoolingCoilInletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The supply air outlet node name in heat exchanger = {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(
                        state,
                        format("must match the cooling coil inlet node name in = {}=\"{}\"", thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName));
                    ShowContinueError(state,
                                      format("Heat exchanger supply air outlet node name =\"{}\"", state.dataLoopNodes->NodeID(SupplyAirOutletNode)));
                    ShowContinueError(state,
                                      format("Cooling coil air inlet node name = \"{}\"", state.dataLoopNodes->NodeID(CoolingCoilInletNodeNum)));
                    ErrorsFound = true;
                }
                CoolingCoilErrFlag = false;
                CoolingCoilOutletNodeNum =
                    WaterCoils::GetCoilOutletNode(state, thisHXCoil.CoolingCoilType, thisHXCoil.CoolingCoilName, CoolingCoilErrFlag);
                if (CoolingCoilErrFlag) ShowContinueError(state, format("...occurs in {} \"{}\"", CurrentModuleObject, thisHXCoil.Name));
                if (SecondaryAirInletNode != CoolingCoilOutletNodeNum) {
                    ShowSevereError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                    ShowContinueError(state, "Node names are inconsistent in heat exchanger and cooling coil object.");
                    ShowContinueError(state,
                                      format("The secondary air inlet node name in heat exchanger = {}=\"{}\"",
                                             thisHXCoil.HeatExchangerType,
                                             thisHXCoil.HeatExchangerName));
                    ShowContinueError(state,
                                      format("must match the cooling coil air outlet node name in = {}=\"{}\".",
                                             thisHXCoil.CoolingCoilType,
                                             thisHXCoil.CoolingCoilName));
                    ShowContinueError(
                        state, format("Heat exchanger secondary air inlet node name = \"{}\".", state.dataLoopNodes->NodeID(SecondaryAirInletNode)));
                    ShowContinueError(state,
                                      format("Cooling coil air outlet node name = \"{}\".", state.dataLoopNodes->NodeID(CoolingCoilOutletNodeNum)));
                    ErrorsFound = true;
                }

            } else {
                ShowWarningError(state, format("{}{}=\"{}\"", RoutineName, CurrentModuleObject, thisHXCoil.Name));
                ShowContinueError(state, format("Invalid {}=\"{}\"", cAlphaFields(4), thisHXCoil.CoolingCoilType));
                ErrorsFound = true;
            }
            BranchNodeConnections::TestCompSet(state,
                                               thisHXCoil.HXAssistedCoilType,
                                               thisHXCoil.Name,
                                               state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                               state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                               "Air Nodes");

            thisHXCoil.HXAssistedCoilInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingWaterHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Inlet,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);
            // no need to capture CoolingCoilInletNodeNum as the return value, it's not used anywhere
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                ErrorsFound,
                                                DataLoopNode::ConnectionObjectType::CoilSystemCoolingWaterHeatExchangerAssisted,
                                                state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum).Name,
                                                DataLoopNode::NodeFluidType::Air,
                                                DataLoopNode::ConnectionType::Internal,
                                                NodeInputManager::CompFluidStream::Primary,
                                                DataLoopNode::ObjectIsParent);
            thisHXCoil.HXExhaustAirInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingWaterHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Internal,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);
            thisHXCoil.HXAssistedCoilOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    state.dataLoopNodes->NodeID(SecondaryAirOutletNode),
                                                    ErrorsFound,
                                                    DataLoopNode::ConnectionObjectType::CoilSystemCoolingWaterHeatExchangerAssisted,
                                                    thisHXCoil.Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::ConnectionType::Outlet,
                                                    NodeInputManager::CompFluidStream::Primary,
                                                    DataLoopNode::ObjectIsParent);

            // Add cooling coil to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.CoolingCoilType,
                                                 thisHXCoil.CoolingCoilName,
                                                 state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                 state.dataLoopNodes->NodeID(SecondaryAirInletNode),
                                                 "Air Nodes");
            // Add heat exchanger to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.HeatExchangerType,
                                                 thisHXCoil.HeatExchangerName,
                                                 state.dataLoopNodes->NodeID(SupplyAirInletNode),
                                                 state.dataLoopNodes->NodeID(SupplyAirOutletNode),
                                                 "Process Air Nodes");
            BranchNodeConnections::SetUpCompSets(state,
                                                 thisHXCoil.HXAssistedCoilType,
                                                 thisHXCoil.Name,
                                                 thisHXCoil.HeatExchangerType,
                                                 thisHXCoil.HeatExchangerName,
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
            ShowFatalError(state, format("{}Previous error condition causes termination.", RoutineName));
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

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for initializations of the HXAssistedCoolingCoil components

        // METHODOLOGY EMPLOYED:
        //  Uses the status flags to trigger initializations.

        // Do these initializations every time
        auto &thisHXCoil = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum);
        thisHXCoil.MassFlowRate = state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilInletNodeNum).MassFlowRate;

        if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_Cooling) {
            //
            // state.dataCoilCooingDX->coilCoolingDXs[thisHXCoil.CoolingCoilIndex]
            //     .outletAirDryBulbTemp = 0.0;
            // state.dataCoilCooingDX->coilCoolingDXs[thisHXCoil.CoolingCoilIndex].outletAirHumRat =
            //     0.0;
        } else if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
            state.dataDXCoils->DXCoilFullLoadOutAirTemp(thisHXCoil.CoolingCoilIndex) = 0.0;
            state.dataDXCoils->DXCoilFullLoadOutAirHumRat(thisHXCoil.CoolingCoilIndex) = 0.0;
        } else if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
            //
        }
    }

    // End Initialization Section of the Module
    //******************************************************************************

    void CalcHXAssistedCoolingCoil(EnergyPlusData &state,
                                   int const HXAssistedCoilNum,                             // Index number for HXAssistedCoolingCoil
                                   bool const FirstHVACIteration,                           // FirstHVACIteration flag
                                   DataHVACGlobals::CompressorOperation const CompressorOp, // compressor operation; 1=on, 0=off
                                   Real64 const PartLoadRatio,                              // Cooling coil part load ratio
                                   bool const HXUnitOn,                                     // Flag to enable heat exchanger
                                   int const FanOpMode,                                     // Allows parent object to control fan operation
                                   ObjexxFCL::Optional<Real64 const> OnOffAirFlow, // Ratio of compressor ON air mass flow to AVERAGE over time step
                                   ObjexxFCL::Optional_bool_const EconomizerFlag,  // OA (or airloop) econommizer status
                                   ObjexxFCL::Optional_int_const DehumidificationMode,        // Optional dehumbidication mode
                                   [[maybe_unused]] ObjexxFCL::Optional<Real64 const> LoadSHR // Optional coil SHR pass over
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Sept 2003

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine models the cooling coil/air-to-air heat exchanger
        //  combination. The cooling coil exiting air temperature is used as
        //  an indicator of convergence.

        // SUBROUTINE PARAMETER DEFINITIONS:
        int constexpr MaxIter(50); // Maximum number of iterations

        int CompanionCoilIndexNum; // Index to DX coil

        auto &thisHXCoil = state.dataHVACAssistedCC->HXAssistedCoil(HXAssistedCoilNum);
        Real64 AirMassFlow = thisHXCoil.MassFlowRate;
        Real64 Error = 1.0;       // Initialize error (CoilOutputTemp last iteration minus current CoilOutputTemp)
        Real64 ErrorLast = Error; // initialize variable used to test loop termination
        int Iter = 0;             // Initialize iteration counter to zero

        // Set mass flow rate at inlet of exhaust side of heat exchanger to supply side air mass flow rate entering this compound object
        state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).MassFlowRate = AirMassFlow;

        if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_Cooling ||
            thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
            thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
            CompanionCoilIndexNum = thisHXCoil.CoolingCoilIndex;
        } else {
            CompanionCoilIndexNum = 0;
        }

        // First call to RegulaFalsi uses PLR=0. Nodes are typically setup at full output on this call.
        // A large number of iterations are required to get to result (~36 iterations to get to PLR=0 node conditions).
        // Reset node data to minimize iteration. This initialization reduces the number of iterations by 50%.
        // CAUTION: Do not use Node(x) = Node(y) here, this can overwrite the coil outlet node setpoint.
        if (PartLoadRatio == 0.0) {
            state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).Temp =
                state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilInletNodeNum).Temp;
            state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).HumRat =
                state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilInletNodeNum).HumRat;
            state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).Enthalpy =
                state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilInletNodeNum).Enthalpy;
            state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).MassFlowRate =
                state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilInletNodeNum).MassFlowRate;
        }

        // Force at least 2 iterations to pass outlet node information
        while ((std::abs(Error) > 0.0005 && Iter <= MaxIter) || Iter < 2) {

            HeatRecovery::SimHeatRecovery(state,
                                          thisHXCoil.HeatExchangerName,
                                          FirstHVACIteration,
                                          thisHXCoil.HeatExchangerIndex,
                                          FanOpMode,
                                          PartLoadRatio,
                                          HXUnitOn,
                                          CompanionCoilIndexNum,
                                          _,
                                          EconomizerFlag,
                                          _,
                                          thisHXCoil.CoolingCoilType_Num);

            if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_Cooling) {

                int coolingCoilIndex = thisHXCoil.CoolingCoilIndex;

                int mSingleMode = state.dataCoilCooingDX->coilCoolingDXs[coolingCoilIndex].getNumModes();
                bool singleMode = (mSingleMode == 1);

                Real64 mCoolingSpeedNum = state.dataCoilCooingDX->coilCoolingDXs[coolingCoilIndex]
                                              .performance.normalMode.speeds.size(); // used the same for the original variable speed coil

                int OperationMode = DataHVACGlobals::coilNormalMode;
                if (state.dataCoilCooingDX->coilCoolingDXs[coolingCoilIndex].SubcoolReheatFlag) {
                    OperationMode = DataHVACGlobals::coilSubcoolReheatMode;
                } else if (DehumidificationMode == 1) {
                    OperationMode = DataHVACGlobals::coilEnhancedMode;
                }

                Real64 mCoolingSpeedRatio = 0.0; // used same setting as the original variable speed coil
                Real64 mCoolCompPartLoadRatio = (CompressorOp == DataHVACGlobals::CompressorOperation::On) ? 1.0 : 0.0;

                Real64 CoilPLR;
                if (mCoolingSpeedNum > 1) {
                    if (mSingleMode == 0) {
                        mCoolCompPartLoadRatio = (CompressorOp == DataHVACGlobals::CompressorOperation::On) ? 1.0 : 0.0;
                    } else {
                        mCoolCompPartLoadRatio = PartLoadRatio * ((CompressorOp == DataHVACGlobals::CompressorOperation::On) ? 1.0 : 0.0);
                        mCoolingSpeedRatio = 1.0;
                    }
                    CoilPLR = 1.0;
                } else {
                    mCoolingSpeedRatio = 1.0;
                    CoilPLR = PartLoadRatio * ((CompressorOp == DataHVACGlobals::CompressorOperation::On) ? 1.0 : 0.0);
                }

                state.dataCoilCooingDX->coilCoolingDXs[thisHXCoil.CoolingCoilIndex].simulate(
                    state,
                    OperationMode, // partially implemented for HXAssistedCoil
                    CoilPLR,       // PartLoadRatio,
                    mCoolingSpeedNum,
                    mCoolingSpeedRatio,
                    FanOpMode,
                    singleMode); //

            } else if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                DXCoils::SimDXCoil(state,
                                   thisHXCoil.CoolingCoilName,
                                   CompressorOp,
                                   FirstHVACIteration,
                                   thisHXCoil.CoolingCoilIndex,
                                   FanOpMode,
                                   PartLoadRatio,
                                   OnOffAirFlow);
            } else if (thisHXCoil.CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                Real64 QZnReq(-1.0);           // Zone load (W), input to variable-speed DX coil
                Real64 QLatReq(0.0);           // Zone latent load, input to variable-speed DX coil
                Real64 OnOffAirFlowRatio(1.0); // ratio of compressor on flow to average flow over time step
                DataHVACGlobals::CompressorOperation CompressorOn = CompressorOp;
                if (PartLoadRatio == 0.0) CompressorOn = DataHVACGlobals::CompressorOperation::Off;
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          thisHXCoil.CoolingCoilName,
                                                          thisHXCoil.CoolingCoilIndex,
                                                          FanOpMode,
                                                          CompressorOn,
                                                          PartLoadRatio,
                                                          thisHXCoil.DXCoilNumOfSpeeds,
                                                          QZnReq,
                                                          QLatReq,
                                                          OnOffAirFlowRatio); // call vs coil model at top speed.
            } else {
                WaterCoils::SimulateWaterCoilComponents(state, thisHXCoil.CoolingCoilName, FirstHVACIteration, thisHXCoil.CoolingCoilIndex);
            }

            Error = state.dataHVACAssistedCC->CoilOutputTempLast - state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).Temp;
            if (Iter > 40) { // check for oscillation (one of these being negative and one positive) before hitting max iteration limit
                if (Error + ErrorLast < 0.000001)
                    Error = 0.0; // result bounced back and forth with same positive and negative result, no possible solution without this check
            }
            ErrorLast = Error;
            state.dataHVACAssistedCC->CoilOutputTempLast = state.dataLoopNodes->Node(thisHXCoil.HXExhaustAirInletNodeNum).Temp;
            ++Iter;
        }

        // Write excessive iteration warning messages
        if (Iter > MaxIter) {
            if (thisHXCoil.MaxIterCounter < 1) {
                ++thisHXCoil.MaxIterCounter;
                ShowWarningError(state,
                                 format("{} \"{}\" -- Exceeded max iterations ({}) while calculating operating conditions.",
                                        thisHXCoil.HXAssistedCoilType,
                                        thisHXCoil.Name,
                                        MaxIter));
                ShowContinueErrorTimeStamp(state, "");
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               thisHXCoil.HXAssistedCoilType + " \"" + thisHXCoil.Name +
                                                   "\" -- Exceeded max iterations error continues...",
                                               thisHXCoil.MaxIterIndex);
            }
        }

        state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(HXAssistedCoilNum) =
            state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilOutletNodeNum).Temp;
        state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(HXAssistedCoilNum) =
            state.dataLoopNodes->Node(thisHXCoil.HXAssistedCoilOutletNodeNum).HumRat;
    }

    //        End of Reporting subroutines for the HXAssistedCoil Module
    // *****************************************************************************

    void GetHXDXCoilIndex(
        EnergyPlusData &state, std::string const &HXDXCoilName, int &HXDXCoilIndex, bool &ErrorsFound, std::string_view const CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2007

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
            if (!CurrentModuleObject.empty()) {
                ShowSevereError(state, fmt::format("{}, GetHXDXCoilIndex: HX Assisted Cooling Coil not found={}", CurrentModuleObject, HXDXCoilName));
            } else {
                ShowSevereError(state, format("GetHXDXCoilIndex: HX Assisted Cooling Coil not found={}", HXDXCoilName));
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
                ShowFatalError(state, format("CheckHXAssistedCoolingCoilSchedule: HX Assisted Coil not found={}", CompName));
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil capacity for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 CoilCapacity(0.0); // returned capacity of matched coil

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        bool errFlag = false;

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
            if (WhichCoil != 0) {
                // coil does not have capacity in input so mine information from DX cooling coil

                if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == DataHVACGlobals::CoilDX_Cooling) {
                    int coolingCoilDXIndex = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilIndex;
                    CoilCapacity = state.dataCoilCooingDX->coilCoolingDXs[coolingCoilDXIndex].performance.normalMode.ratedGrossTotalCap;
                } else if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    CoilCapacity = DXCoils::GetCoilCapacity(state,
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
                    ShowRecurringWarningErrorAtEnd(
                        state, "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", state.dataHVACAssistedCC->ErrCount);
                }
            }
        } else if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
            if (WhichCoil != 0) {
                // coil does not have capacity in input so mine information from DX cooling coil
                CoilCapacity = WaterCoils::GetWaterCoilCapacity(state,
                                                                state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                                state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                                errFlag);
                if (errFlag) {
                    ShowRecurringWarningErrorAtEnd(
                        state, "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", state.dataHVACAssistedCC->ErrCount);
                }
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, format("GetCoilCapacity: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ShowContinueError(state, "... Coil Capacity returned as -1000.");
            ErrorsFound = true;
            CoilCapacity = -1000.0;
        }

        if (errFlag) ErrorsFound = true;

        return CoilCapacity;
    }

    int GetCoilGroupTypeNum(EnergyPlusData &state,
                            std::string const &CoilType, // must match coil types in this module
                            std::string const &CoilName, // must match coil names for the coil type
                            bool &ErrorsFound,           // set to true if problem
                            bool const PrintWarning      // prints warning message if true
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   August 2008

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the HX coil type and returns it (CoilDX_CoolingHXAssisted, CoilWater_CoolingHXAssisted)
        // If incorrect coil type or name is given, ErrorsFound is returned as true.

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
            // coil does not have capacity in input so mine information from DX cooling coil
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilType_Num;
        } else {
            if (PrintWarning) {
                ShowSevereError(state, format("GetCoilGroupTypeNum: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            }
            ErrorsFound = true;
            return 0;
        }
    }

    int GetCoilObjectTypeNum(EnergyPlusData &state,
                             std::string const &CoilType, // must match coil types in this module
                             std::string const &CoilName, // must match coil names for the coil type
                             bool &ErrorsFound,           // set to true if problem
                             bool const PrintWarning      // prints warning message if true
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   April 2009

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the coil object type for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num;
        } else {
            if (PrintWarning) {
                ShowSevereError(state, format("GetCoilObjectTypeNum: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            }
            ErrorsFound = true;
            return 0;
        }
    }

    int GetCoilInletNode(EnergyPlusData &state,
                         std::string_view CoilType,   // must match coil types in this module
                         std::string const &CoilName, // must match coil names for the coil type
                         bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2006

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the inlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilInletNodeNum;
        } else {
            ShowSevereError(state, format("GetCoilInletNode: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            return 0;
        }
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the inlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned node number of matched coil

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                NodeNumber = WaterCoils::GetCoilWaterInletNode(state,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                               ErrorsFound);
            } else if (state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                NodeNumber = WaterCoils::GetCoilWaterInletNode(state,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                                               state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName,
                                                               ErrorsFound);
            } else { // even though validated in Get, still check.
                ShowSevereError(state,
                                format("GetCoilWaterInletNode: Invalid Cooling Coil for HX Assisted Coil, Type=\"{}\" Name=\"{}\"",
                                       state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType,
                                       CoilName));
                ErrorsFound = true;
                NodeNumber = 0; // Autodesk:Return Added line to set return value
            }
        } else {
            ShowSevereError(state, format("GetCoilInletNode: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetCoilOutletNode(EnergyPlusData &state,
                          std::string_view CoilType,   // must match coil types in this module
                          std::string const &CoilName, // must match coil names for the coil type
                          bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the outlet node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HXAssistedCoilOutletNodeNum;
        } else {
            ShowSevereError(state, format("GetCoilOutletNode: Could not find Coil, Type=\"{}\" Name=\"{}", CoilType, CoilName));
            ErrorsFound = true;
            return 0;
        }
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil type.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
        } else {
            ShowSevereError(state, format("Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            return "";
        }
    }

    std::string GetHXDXCoilName(EnergyPlusData &state,
                                std::string_view CoilType,   // must match coil types in this module
                                std::string const &CoilName, // must match coil names for the coil type
                                bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2006

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil name.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName;
        } else {
            ShowSevereError(state, format("Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            return "";
        }
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil name.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the name
        // is returned as blank

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            // this should be the index to the DX cooling coil object, not the HXAssisted object
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilIndex;
        } else {
            ShowSevereError(state, format("Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            return 0;
        }
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given coil and returns the cooling coil type.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and the cooling
        // coil type is returned as blank.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            return state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
        } else {
            ShowSevereError(state, format("Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            return "";
        }
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

        // PURPOSE OF THIS SUBROUTINE:
        // Need to get child coil type and name.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);
        }

        if (WhichCoil != 0) {
            CoolingCoilType = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilType;
            CoolingCoilName = state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).CoolingCoilName;
        } else {
            ShowSevereError(state, format("Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
            ErrorsFound = true;
            CoolingCoilType = "";
            CoolingCoilName = "";
        }
    }

    Real64 GetCoilMaxWaterFlowRate(EnergyPlusData &state,
                                   std::string_view CoilType,   // must match coil types in this module
                                   std::string const &CoilName, // must match coil names for the coil type
                                   bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   November 2006
        //       MODIFIED       R. Raustad, April 2009 - added water coil ELSE IF

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max water flow rate for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {

            int WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);

            if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    // coil does not specify MaxWaterFlowRate
                    MaxWaterFlowRate = 0.0;
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Requested Max Water Flow Rate from CoilSystem:Cooling:DX:HeatExchangerAssisted N/A",
                                                   state.dataHVACAssistedCC->ErrCount2);
                }
            } else if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    MaxWaterFlowRate =
                        WaterCoils::GetCoilMaxWaterFlowRate(state, CoilType, GetHXDXCoilName(state, CoilType, CoilName, ErrorsFound), ErrorsFound);
                }
            } else {
                WhichCoil = 0;
            }

            if (WhichCoil == 0) {
                ShowSevereError(state, format("GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
                ErrorsFound = true;
                MaxWaterFlowRate = -1000.0;
            }
        } else {
            ShowSevereError(state, format("GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max air flow rate for the given HX and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
        // as negative.

        // Return value
        Real64 MaxAirFlowRate; // returned max air flow rate of matched HX

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {

            int WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataHVACAssistedCC->HXAssistedCoil);

            if (UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted") ||
                UtilityRoutines::SameString(CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                if (WhichCoil != 0) {
                    MaxAirFlowRate =
                        HeatRecovery::GetSupplyAirFlowRate(state, state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HeatExchangerName, ErrorsFound);
                }
            } else {
                WhichCoil = 0;
            }

            if (WhichCoil == 0) {
                ShowSevereError(state, format("GetHXCoilAirFlowRate: Could not find HX, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
                ErrorsFound = true;
                MaxAirFlowRate = -1000.0;
            }
        } else {
            ShowSevereError(state, format("GetHXCoilAirFlowRate: Could not find HX, Type=\"{}\" Name=\"{}\"", CoilType, CoilName));
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

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given heat exchanger name and type and returns true or false.

        // Obtains and allocates HXAssistedCoolingCoil related parameters from input file
        if (state.dataHVACAssistedCC->GetCoilsInputFlag) { // First time subroutine has been called, get input data
            // Get the HXAssistedCoolingCoil input
            GetHXAssistedCoolingCoilInput(state);
            state.dataHVACAssistedCC->GetCoilsInputFlag =
                false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        int WhichCoil = 0;
        if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
            WhichCoil = UtilityRoutines::FindItem(HXName, state.dataHVACAssistedCC->HXAssistedCoil, &HXAssistedCoilParameters::HeatExchangerName);
        }

        if (WhichCoil != 0) {
            if (UtilityRoutines::SameString(state.dataHVACAssistedCC->HXAssistedCoil(WhichCoil).HeatExchangerType, HXType)) {
                return true;
            }
        }
        return false;
    }

    //        End of Utility subroutines for the HXAssistedCoil Module
    // *****************************************************************************

} // namespace HVACHXAssistedCoolingCoil

} // namespace EnergyPlus
