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
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PackagedThermalStorageCoil.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>

namespace EnergyPlus {

// note that there are two modules in this file

//  HVACDXSystem is for cooling DX coils

//  HVACDXHeatPumpSystem is for heating DX coils

namespace HVACDXSystem {
    // Module containing the state.dataHVACDXSys->DXCoolingSystem simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   March 2001
    //                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
    //                        Add dehumidification controls and support for multimode DX coil
    //                        Work supported by ASHRAE research project 1254-RP
    //                      Feb 2013 Bereket Nigusse, FSEC
    //                        Added DX Coil Model For 100% OA systems
    //                      Feb 2013 Bo Shen, Oak Ridge National Lab
    //                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the state.dataHVACDXSys->DXCoolingSystem System Component

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataHVACGlobals;
    using namespace ScheduleManager;

    Real64 constexpr LatCapTimeConst(45.0);

    void SimDXCoolingSystem(EnergyPlusData &state,
                            std::string_view DXCoolingSystemName, // Name of DXSystem:Airloop object
                            bool const FirstHVACIteration,          // True when first HVAC iteration
                            int const AirLoopNum,                   // Primary air loop number
                            int &CompIndex,                         // Index to DXSystem:Airloop object
                            Optional_int_const OAUnitNum,           // If the system is an equipment of OutdoorAirUnit
                            Optional<Real64 const> OAUCoilOutTemp,  // the coil inlet temperature of OutdoorAirUnit
                            Optional<Real64> QTotOut                // the total cooling output of unit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Mar 2001
        //       MODIFIED       Richard Raustad, Sept 2003 (added HVACHXAssistedCoolingCoil)
        //                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add support for multimode DX coil
        //                      Feb 2013 Bo Shen, Oak Ridge National Lab
        //                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages state.dataHVACDXSys->DXCoolingSystem component simulation.

        // Using/Aliasing
        using DXCoils::SimDXCoil;
        using DXCoils::SimDXCoilMultiMode;
        using DXCoils::SimDXCoilMultiSpeed;

        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using PackagedThermalStorageCoil::SimTESCoil;
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CompName; // Name of DXSystem:Airloop object
        int DXSystemNum;      // Index to DXSystem:Airloop object
        bool HXUnitOn;        // Flag to control HX for HXAssisted Cooling Coil
        Real64 AirMassFlow;   // DX System air mass flow rate
        int InletNodeNum;     // DX System inlet node number
        int OutletNodeNum;    // DX System outlet node number

        auto &DXCoolingSystem(state.dataHVACDXSys->DXCoolingSystem);

        // Obtains and Allocates DX Cooling System related parameters from input file
        if (state.dataHVACDXSys->GetInputFlag) { // First time subroutine has been entered
            GetDXCoolingSystemInput(state);
            state.dataHVACDXSys->GetInputFlag = false;
        }

        // Find the correct DXSystemNumber
        if (CompIndex == 0) {
            DXSystemNum = UtilityRoutines::FindItemInList(DXCoolingSystemName, state.dataHVACDXSys->DXCoolingSystem);
            if (DXSystemNum == 0) {
                ShowFatalError(state, "SimDXCoolingSystem: DXUnit not found=" + std::string{DXCoolingSystemName});
            }
            CompIndex = DXSystemNum;
        } else {
            DXSystemNum = CompIndex;
            if (DXSystemNum > state.dataHVACDXSys->NumDXSystem || DXSystemNum < 1) {
                ShowFatalError(state,
                               format("SimulateDXCoolingSystem:  Invalid CompIndex passed={}, Number of DX Units={}, DX Unit name={}",
                                      DXSystemNum,
                                      state.dataHVACDXSys->NumDXSystem,
                                      DXCoolingSystemName));
            }
            if (state.dataHVACDXSys->CheckEquipName(DXSystemNum)) {
                if (DXCoolingSystemName != DXCoolingSystem(DXSystemNum).Name) {
                    ShowFatalError(
                        state,
                        format("SimulateDXCoolingSystem: Invalid CompIndex passed={}, DX Unit name={}, stored DX Unit Name for that index={}",
                               DXSystemNum,
                               DXCoolingSystemName,
                               state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name));
                }
                state.dataHVACDXSys->CheckEquipName(DXSystemNum) = false;
            }
        }

        InitDXCoolingSystem(state, DXSystemNum, AirLoopNum, OAUnitNum, OAUCoilOutTemp);

        // Call the series of components that simulate a DX Cooling System
        // Control the DX Cooling System
        HXUnitOn = false;
        ControlDXSystem(state, DXSystemNum, FirstHVACIteration, HXUnitOn);

        // simulate DX Cooling System
        CompName = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilName;
        // Need a cooling System call here I think
        {
            auto const SELECT_CASE_var(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType_Num);

            if (SELECT_CASE_var == CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

                SimDXCoil(state,
                          CompName,
                          On,
                          FirstHVACIteration,
                          state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                          state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode,
                          state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac);

            } else if (SELECT_CASE_var == CoilDX_CoolingHXAssisted) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                SimHXAssistedCoolingCoil(state,
                                         CompName,
                                         FirstHVACIteration,
                                         On,
                                         state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac,
                                         state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                         state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode,
                                         HXUnitOn,
                                         _,
                                         state.dataHVACDXSys->EconomizerFlag);

            } else if (SELECT_CASE_var == CoilDX_CoolingTwoSpeed) { // Coil:Cooling:DX:TwoSpeed
                // formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

                SimDXCoilMultiSpeed(state,
                                    CompName,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedRatio,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CycRatio,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

            } else if (SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
                // formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

                SimDXCoilMultiMode(state,
                                   CompName,
                                   On,
                                   FirstHVACIteration,
                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac,
                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DehumidificationMode,
                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode);
            } else if (SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed) { // Coil:Cooling:DX:VariableSpeed

                SimVariableSpeedCoils(state,
                                      CompName,
                                      DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                      DXCoolingSystem(DXSystemNum).FanOpMode,
                                      state.dataHVACDXSys->MaxONOFFCyclesperHour,
                                      state.dataHVACDXSys->HPTimeConstant,
                                      state.dataHVACDXSys->FanDelayTime,
                                      On,
                                      DXCoolingSystem(DXSystemNum).PartLoadFrac,
                                      DXCoolingSystem(DXSystemNum).SpeedNum,
                                      DXCoolingSystem(DXSystemNum).SpeedRatio,
                                      state.dataHVACDXSys->QZnReq,
                                      state.dataHVACDXSys->QLatReq,
                                      state.dataHVACDXSys->OnOffAirFlowRatio);

            } else if (SELECT_CASE_var == CoilDX_PackagedThermalStorageCooling) {

                SimTESCoil(state,
                           CompName,
                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode,
                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).TESOpMode,
                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac);

            } else {
                ShowFatalError(
                    state, "SimDXCoolingSystem: Invalid DX Cooling System/Coil=" + state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType);
            }
        }
        // set econo lockout flag
        // set econo lockout flag
        if (AirLoopNum != -1) {   // IF the sysem is not an equipment of outdoor air unit
            if (AirLoopNum > 0) { // Real airloopNum called from MixedAir and SimAirServingZones
                if ((state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac > 0.0 ||
                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedRatio > 0.0 ||
                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CycRatio > 0.0) &&
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor) {
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = true;
                } else { // used for AirLoopHVACDOAS only
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = false;
                }
            }
        }

        if (present(QTotOut)) {
            InletNodeNum = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilInletNodeNum;
            OutletNodeNum = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilOutletNodeNum;
            AirMassFlow = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate;
            QTotOut = AirMassFlow * (state.dataLoopNodes->Node(InletNodeNum).Enthalpy - state.dataLoopNodes->Node(OutletNodeNum).Enthalpy);
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetDXCoolingSystemInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Mar 2001
        //                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add dehumidification controls and support for multimode DX coil
        //                      Feb 2013 Bo Shen, Oak Ridge National Lab
        //                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for system and stores it in System data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using DXCoils::GetDXCoilIndex;
        using DXCoils::SetCoilSystemCoolingData;
        using DXCoils::SetDXCoilTypeData;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilIndex;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
        using NodeInputManager::GetOnlySingleNode;
        using PackagedThermalStorageCoil::GetTESCoilIndex;
        using VariableSpeedCoils::GetCoilIndexVariableSpeed;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXSystemNum; // The state.dataHVACDXSys->DXCoolingSystem that you are currently loading input into
        int NumAlphas;
        int NumNums;
        int IOStat;
        static constexpr std::string_view RoutineName("GetDXCoolingSystemInput: "); // include trailing blank space
        bool ErrorsFound(false);                                           // If errors detected in input
        bool IsNotOK;                                                      // Flag to verify name
        int DXCoolSysNum;
        bool FanErrorsFound;             // flag returned on fan operating mode check
        bool DXErrorsFound;              // flag returned on DX coil name check
        std::string HXDXCoolCoilName;    // Name of DX cooling coil used with Heat Exchanger Assisted Cooling Coil
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string Alphas;           // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> Numbers;         // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

        auto &DXCoolingSystem(state.dataHVACDXSys->DXCoolingSystem);
        auto &ErrFound(state.dataHVACDXSys->ErrFound);
        auto &TotalArgs(state.dataHVACDXSys->TotalArgs);

        CurrentModuleObject = "CoilSystem:Cooling:DX";
        state.dataHVACDXSys->NumDXSystem = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        DXCoolingSystem.allocate(state.dataHVACDXSys->NumDXSystem);
        state.dataHVACDXSys->CheckEquipName.dimension(state.dataHVACDXSys->NumDXSystem, true);

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "CoilSystem:Cooling:DX", TotalArgs, NumAlphas, NumNums);

        Alphas.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNums);
        Numbers.dimension(NumNums, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNums, true);

        // Get the data for the DX Cooling System
        for (DXCoolSysNum = 1; DXCoolSysNum <= state.dataHVACDXSys->NumDXSystem; ++DXCoolSysNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     DXCoolSysNum,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingSystemType = CurrentModuleObject; // push Object Name into data array
            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).SchedPtr = GetScheduleIndex(state, Alphas(2));
                if (state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + Alphas(2) + " for " +
                                        cAlphaFields(1) + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }

            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingCoilInletNodeNum = GetOnlySingleNode(state,
                                                                                                             Alphas(3),
                                                                                                             ErrorsFound,
                                                                                                             CurrentModuleObject,
                                                                                                             Alphas(1),
                                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                                                             1,
                                                                                                             ObjectIsParent);
            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingCoilOutletNodeNum =
                GetOnlySingleNode(state,
                                  Alphas(4),
                                  ErrorsFound,
                                  CurrentModuleObject,
                                  Alphas(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Outlet,
                                  1,
                                  ObjectIsParent);

            TestCompSet(state, CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXSystemControlNodeNum = GetOnlySingleNode(state,
                                                                                                          Alphas(5),
                                                                                                          ErrorsFound,
                                                                                                          CurrentModuleObject,
                                                                                                          Alphas(1),
                                                                                                          DataLoopNode::NodeFluidType::Air,
                                                                                                          DataLoopNode::NodeConnectionType::Sensor,
                                                                                                          1,
                                                                                                          ObjectIsParent);
            if (state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXSystemControlNodeNum == 0) {
                ShowSevereError(state, CurrentModuleObject + ": control node must be input");
                ShowContinueError(state, "Error occurred in " + cAlphaFields(1) + '=' + Alphas(1));
                ErrorsFound = true;
            }

            // Get Cooling System Information if available
            if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:SingleSpeed") ||
                UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:VariableSpeed") ||
                UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:TwoSpeed") ||
                UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:TwoStageWithHumidityControlMode") ||
                UtilityRoutines::SameString(Alphas(6), "CoilSystem:Cooling:DX:HeatExchangerAssisted") ||
                UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {

                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType = Alphas(6);
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName = Alphas(7);

                ErrFound = false;
                if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:SingleSpeed")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = CoilDX_CoolingSingleSpeed;
                    GetDXCoilIndex(state,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex,
                                   ErrFound,
                                   CurrentModuleObject,
                                   ObjexxFCL::Optional_bool_const());
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:VariableSpeed")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex = GetCoilIndexVariableSpeed(
                        state, "Coil:Cooling:DX:VariableSpeed", state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName, ErrFound);
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:TwoSpeed")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = CoilDX_CoolingTwoSpeed;
                    GetDXCoilIndex(state,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex,
                                   ErrFound,
                                   CurrentModuleObject,
                                   ObjexxFCL::Optional_bool_const());
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(Alphas(6), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = CoilDX_CoolingHXAssisted;
                    GetHXDXCoilIndex(state,
                                     state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                     state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex,
                                     ErrFound,
                                     CurrentModuleObject);
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }

                    DXErrorsFound = false;
                    HXDXCoolCoilName = GetHXDXCoilName(state, Alphas(6), Alphas(7), DXErrorsFound);
                    if (DXErrorsFound) {
                        ShowWarningError(state, CurrentModuleObject + " = \"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\"");
                        ShowContinueError(state, "CoilSystem:Cooling:DX:HeatExchangerAssisted \"" + Alphas(7) + "\" not found.");
                        ErrorsFound = true;
                    }

                } else if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = CoilDX_CoolingTwoStageWHumControl;
                    GetDXCoilIndex(state,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                   state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex,
                                   ErrFound,
                                   CurrentModuleObject,
                                   ObjexxFCL::Optional_bool_const());
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(Alphas(6), "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num = CoilDX_PackagedThermalStorageCooling;
                    GetTESCoilIndex(state,
                                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex,
                                    ErrFound,
                                    CurrentModuleObject);
                    if (ErrFound) {
                        ShowContinueError(state,
                                          "...occurs in " + CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(state, "Invalid entry for " + cAlphaFields(6) + " :" + Alphas(6));
                ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                ErrorsFound = true;
            }

            ValidateComponent(state,
                              state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType,
                              state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                              IsNotOK,
                              CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError(state, "In " + CurrentModuleObject + " = \"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                ErrorsFound = true;
            }

            SetUpCompSets(state,
                          state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingSystemType,
                          state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name,
                          Alphas(6),
                          Alphas(7),
                          Alphas(3),
                          Alphas(4));

            FanErrorsFound = false;

            // Supply air fan operating mode defaulted to constant fan cycling coil/compressor
            state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).FanOpMode = ContFanCycCoil;

            // Dehumidification control mode
            if (UtilityRoutines::SameString(Alphas(8), "None")) {
                DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::None;
            } else if (UtilityRoutines::SameString(Alphas(8), "")) {
                DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::None;
            } else if (UtilityRoutines::SameString(Alphas(8), "Multimode")) {
                if (DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl) {
                    DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::Multimode;
                } else if (DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                    DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::Multimode;
                } else {
                    ShowWarningError(state, "Invalid entry for " + cAlphaFields(8) + " :" + Alphas(8));
                    ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                    ShowContinueError(state,
                                      "Valid only with cooling coil type = Coil:Cooling:DX:TwoStageWithHumidityControlMode or "
                                      "CoilSystem:Cooling:DX:HeatExchangerAssisted.");
                    ShowContinueError(state, "Setting " + cAlphaFields(8) + " to None.");
                    DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::None;
                }
            } else if (UtilityRoutines::SameString(Alphas(8), "CoolReheat")) {
                DXCoolingSystem(DXCoolSysNum).DehumidControlType = DehumidControl::CoolReheat;
            } else {
                ShowSevereError(state, "Invalid entry for " + cAlphaFields(8) + " :" + Alphas(8));
                ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
            }

            // Run on sensible load
            if (UtilityRoutines::SameString(Alphas(9), "Yes")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnSensibleLoad = true;
            } else if (UtilityRoutines::SameString(Alphas(9), "")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnSensibleLoad = true;
            } else if (UtilityRoutines::SameString(Alphas(9), "No")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnSensibleLoad = false;
            } else {
                ShowSevereError(state, "Invalid entry for " + cAlphaFields(9) + " :" + Alphas(9));
                ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                ShowContinueError(state, "Must be Yes or No.");
            }

            // Run on latent load
            if (UtilityRoutines::SameString(Alphas(10), "Yes")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnLatentLoad = true;
            } else if (UtilityRoutines::SameString(Alphas(10), "")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnLatentLoad = false;
            } else if (UtilityRoutines::SameString(Alphas(10), "No")) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).RunOnLatentLoad = false;
            } else {
                ShowSevereError(state, "Invalid entry for " + cAlphaFields(10) + " :" + Alphas(10));
                ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                ShowContinueError(state, "Must be Yes or No.");
            }

            // Run as 100% DOAS DX coil
            if (lAlphaBlanks(11) && NumAlphas <= 10) {
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil = false;
            } else {
                if (UtilityRoutines::SameString(Alphas(11), "Yes")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil = true;
                    if (state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                        ShowWarningError(state, CurrentModuleObject + " = " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
                        ShowContinueError(state, "Invalid entry for " + cAlphaFields(11) + " :" + Alphas(11));
                        ShowContinueError(state, "Variable DX Cooling Coil is not supported as 100% DOAS DX coil.");
                        ShowContinueError(state, "Variable DX Cooling Coil is reset as a regular DX coil and the simulation continues.");
                        state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil = false;
                    }
                } else if (UtilityRoutines::SameString(Alphas(11), "")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil = false;
                } else if (UtilityRoutines::SameString(Alphas(11), "No")) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil = false;
                } else {
                    ShowSevereError(state, "Invalid entry for " + cAlphaFields(11) + " :" + Alphas(11));
                    ShowContinueError(state, "In " + CurrentModuleObject + "=\"" + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name + "\".");
                    ShowContinueError(state, "Must be Yes or No.");
                }
            }

            // considered as as 100% DOAS DX cooling coil
            if (state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil) {
                // set the system DX Coil application type to the child DX coil
                if (!(state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed)) {
                    SetDXCoilTypeData(state, state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName);
                } else {
                    ShowWarningError(
                        state,
                        "CoilSystem:Cooling:DX named " + state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name +
                            " entered with both variable speed DX coil and outdoor air mode set to YES, however VS coil model does not have "
                            "a special outdoor air mode");
                }
            }
            // DOAS DX Cooling Coil Leaving Minimum Air Temperature
            if (NumNums > 0) {
                if (!lNumericBlanks(1)) {
                    state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DesignMinOutletTemp = Numbers(1);
                }
            }
            if (state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed) {
                SetCoilSystemCoolingData(state,
                                         state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName,
                                         state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).Name);
            }

            if (state.dataGlobal->DoCoilDirectSolutions &&
                state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
                DXCoils::DisableLatentDegradation(state, state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex);
            }

        } // End of the DX System Loop

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Program terminates.");
        }

        for (DXSystemNum = 1; DXSystemNum <= state.dataHVACDXSys->NumDXSystem; ++DXSystemNum) {
            // Setup Report variables for the DXCoolingSystem that is not reported in the components themselves
            if (UtilityRoutines::SameString(DXCoolingSystem(DXSystemNum).CoolingCoilType, "Coil:Cooling:DX:TwoSpeed")) {
                SetupOutputVariable(state,
                                    "Coil System Cycling Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CycRatio,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                SetupOutputVariable(state,
                                    "Coil System Compressor Speed Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedRatio,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
            } else if (UtilityRoutines::SameString(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType,
                                                   "Coil:Cooling:DX:VariableSpeed")) {
                SetupOutputVariable(state,
                                    "Coil System Cycling Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CycRatio,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                SetupOutputVariable(state,
                                    "Coil System Compressor Speed Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedRatio,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                SetupOutputVariable(state,
                                    "Coil System Compressor Speed Number",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedNum,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
            } else {
                SetupOutputVariable(state,
                                    "Coil System Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac,
                                    "System",
                                    "Average",
                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
            }
            SetupOutputVariable(state,
                                "Coil System Frost Control Status",
                                OutputProcessor::Unit::None,
                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FrostControlStatus,
                                "System",
                                "Average",
                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();
    }

    // End of Get Input subroutines for the Module
    //******************************************************************************

    // Beginning of Initialization subroutines for the Module
    // *****************************************************************************

    void InitDXCoolingSystem(EnergyPlusData &state,
                             int const DXSystemNum,                // number of the current DX Sys being simulated
                             int const AirLoopNum,                 // number of the current air loop being simulated
                             Optional_int_const OAUnitNum,         // number of the current outdoor air unit being simulated
                             Optional<Real64 const> OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2001
        //                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add dehumidification controls
        //                      May 2009, B. Griffith, NREL added EMS setpoint checks
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the DX Cooling Systems.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;
        using EMSManager::CheckIfNodeSetPointManagedByEMS;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutNode;     // outlet node number
        int ControlNode; // control node number
        int DXSysIndex;
        int OutdoorAirUnitNum;    // "ONLY" for ZoneHVAC:OutdoorAirUnit
        Real64 OAUCoilOutletTemp; // "ONLY" for zoneHVAC:OutdoorAirUnit

        auto &DXCoolingSystem(state.dataHVACDXSys->DXCoolingSystem);

        if (state.dataHVACDXSys->MyOneTimeFlag) {

            state.dataHVACDXSys->MyOneTimeFlag = false;
        }
        if (AirLoopNum == -1) { // This Dx system is component of ZoneHVAC:OutdoorAirUnit
            OutdoorAirUnitNum = OAUnitNum;
            OAUCoilOutletTemp = OAUCoilOutTemp;
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataHVACDXSys->MySetPointCheckFlag && DoSetPointTest) {
            for (DXSysIndex = 1; DXSysIndex <= state.dataHVACDXSys->NumDXSystem; ++DXSysIndex) {
                ControlNode = DXCoolingSystem(DXSysIndex).DXSystemControlNodeNum;
                if (ControlNode > 0) {
                    if (AirLoopNum == -1) {                                                      // Outdoor Air Unit
                        state.dataLoopNodes->Node(ControlNode).TempSetPoint = OAUCoilOutletTemp; // Set the coil outlet temperature
                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil) {
                            FrostControlSetPointLimit(state,
                                                      DXSystemNum,
                                                      state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp,
                                                      state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                                      state.dataEnvrn->OutBaroPress,
                                                      state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                                      1);
                        }
                    } else if (AirLoopNum != -1) { // Not an outdoor air unit

                        if (state.dataLoopNodes->Node(ControlNode).TempSetPoint == SensedNodeFlagValue) {
                            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                ShowSevereError(
                                    state,
                                    state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).DXCoolingSystemType +
                                        ": Missing temperature setpoint for DX unit= " + state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).Name);
                                ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the unit control node.");
                                state.dataHVACGlobal->SetPointErrorFlag = true;
                            } else {
                                CheckIfNodeSetPointManagedByEMS(
                                    state, ControlNode, EMSManager::SPControlType::iTemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                                if (state.dataHVACGlobal->SetPointErrorFlag) {
                                    ShowSevereError(
                                        state,
                                        state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).DXCoolingSystemType +
                                            ": Missing temperature setpoint for DX unit= " + state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).Name);
                                    ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the unit control node.");
                                    ShowContinueError(state,
                                                      "  or use an EMS actuator to establish a temperature setpoint at the unit control node.");
                                }
                            }
                        }
                        if ((DXCoolingSystem(DXSysIndex).DehumidControlType != DehumidControl::None) &&
                            (state.dataLoopNodes->Node(ControlNode).HumRatMax == SensedNodeFlagValue)) {
                            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                ShowSevereError(state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).DXCoolingSystemType +
                                                    ": Missing humidity ratio setpoint (HUMRATMAX) for DX unit= " +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).Name);
                                ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the unit control node.");
                                state.dataHVACGlobal->SetPointErrorFlag = true;
                            } else {
                                CheckIfNodeSetPointManagedByEMS(state,
                                                                ControlNode,
                                                                EMSManager::SPControlType::iHumidityRatioMaxSetPoint,
                                                                state.dataHVACGlobal->SetPointErrorFlag);
                                if (state.dataHVACGlobal->SetPointErrorFlag) {
                                    ShowSevereError(state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).DXCoolingSystemType +
                                                        ": Missing maximum humidity ratio setpoint (HUMRATMAX) for DX unit= " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSysIndex).Name);
                                    ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the unit control node.");
                                    ShowContinueError(state, "  or use an EMS actuator to establish a maximum humidity ratio setpoint.");
                                }
                            }
                        }
                    }
                }
            }
            state.dataHVACDXSys->MySetPointCheckFlag = false;
        }

        if (!state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).VSCoilFanInfoSet && AirLoopNum > 0) {
            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {

                switch (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).supFanModelTypeEnum) {
                case DataAirSystems::structArrayLegacyFanModels: {
                    int SupFanNum = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).SupFanNum;
                    if (SupFanNum > 0) {
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                            state,
                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilName,
                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType,
                            state.dataFans->Fan(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).SupFanNum).FanName,
                            DataAirSystems::structArrayLegacyFanModels,
                            state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).SupFanNum);
                    }

                    break;
                }
                case DataAirSystems::objectVectorOOFanSystemModel: {
                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).supFanVecIndex >= 0) {
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                            state,
                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilName,
                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType,
                            state.dataHVACFan->fanObjs[state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).supFanVecIndex]->name,
                            DataAirSystems::objectVectorOOFanSystemModel,
                            state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).supFanVecIndex);
                    }
                    break;
                }
                case DataAirSystems::fanModelTypeNotYetSet: {
                    // do nothing
                    break;
                }
                }
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).VSCoilFanInfoSet = true;
            }
        } // else if ( )  on OA system ??
        // These initializations are done every iteration
        if (AirLoopNum == -1) { // This IF-TEHN routine is just for ZoneHVAC:OUTDOORAIRUNIT
            OutNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilOutletNodeNum;
            ControlNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXSystemControlNodeNum;

            if (ControlNode == 0) {
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp = 0.0;
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;
            } else if (ControlNode == OutNode) {
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp = OAUCoilOutletTemp;
                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil &&
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnSensibleLoad) {
                    FrostControlSetPointLimit(state,
                                              DXSystemNum,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp,
                                              state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                              state.dataEnvrn->OutBaroPress,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                              1);
                }
            }
            //  If the Dxsystem is an equipment of Outdoor Air Unit, the desiered coiloutlet humidity level is set to zero
            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;

        } else if (AirLoopNum != -1) { // Not Outdoor Air Unit

            OutNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilOutletNodeNum;
            ControlNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXSystemControlNodeNum;
            if (AirLoopNum == 0) {
                state.dataHVACDXSys->EconomizerFlag = false;
            } else {
                state.dataHVACDXSys->EconomizerFlag = state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoActive;
            }
            if (ControlNode == 0) {
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp = 0.0;
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;
            } else if (ControlNode == OutNode) {
                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil &&
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnSensibleLoad) {
                    FrostControlSetPointLimit(state,
                                              DXSystemNum,
                                              state.dataLoopNodes->Node(ControlNode).TempSetPoint,
                                              state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                              state.dataEnvrn->OutBaroPress,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                              1);
                }
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp = state.dataLoopNodes->Node(ControlNode).TempSetPoint;
                //  If HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
                if ((DXCoolingSystem(DXSystemNum).DehumidControlType != DehumidControl::None) &&
                    (state.dataLoopNodes->Node(ControlNode).HumRatMax > 0.0)) {
                    if (DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil && DXCoolingSystem(DXSystemNum).RunOnLatentLoad) {
                        FrostControlSetPointLimit(state,
                                                  DXSystemNum,
                                                  state.dataLoopNodes->Node(ControlNode).TempSetPoint,
                                                  state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                                  state.dataEnvrn->OutBaroPress,
                                                  state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                                  2);
                    }
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = state.dataLoopNodes->Node(ControlNode).HumRatMax;
                } else {
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;
                }
            } else {
                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil &&
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnSensibleLoad) {
                    FrostControlSetPointLimit(state,
                                              DXSystemNum,
                                              state.dataLoopNodes->Node(ControlNode).TempSetPoint,
                                              state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                              state.dataEnvrn->OutBaroPress,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                              1);
                }
                DXCoolingSystem(DXSystemNum).DesiredOutletTemp =
                    state.dataLoopNodes->Node(ControlNode).TempSetPoint -
                    (state.dataLoopNodes->Node(ControlNode).Temp - state.dataLoopNodes->Node(OutNode).Temp);
                if (DXCoolingSystem(DXSystemNum).DehumidControlType != DehumidControl::None) {
                    if (DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil && DXCoolingSystem(DXSystemNum).RunOnLatentLoad) {
                        FrostControlSetPointLimit(state,
                                                  DXSystemNum,
                                                  state.dataLoopNodes->Node(ControlNode).TempSetPoint,
                                                  state.dataLoopNodes->Node(ControlNode).HumRatMax,
                                                  state.dataEnvrn->OutBaroPress,
                                                  state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesignMinOutletTemp,
                                                  2);
                    }
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat =
                        state.dataLoopNodes->Node(ControlNode).HumRatMax -
                        (state.dataLoopNodes->Node(ControlNode).HumRat - state.dataLoopNodes->Node(OutNode).HumRat);
                } else {
                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;
                }
            }
        }
    }

    // End of Initialization subroutines for the Module
    // *****************************************************************************

    // Beginning of Calculation subroutines for the state.dataHVACDXSys->DXCoolingSystem Module
    // *****************************************************************************

    void ControlDXSystem(EnergyPlusData &state,
                         int const DXSystemNum,         // index to DXSystem
                         bool const FirstHVACIteration, // First HVAC iteration flag
                         bool &HXUnitOn                 // flag to enable heat exchanger heat recovery
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Feb. 2001
        //       MODIFIED       Nov. 2003, R. Raustad, FSEC
        //                      Feb. 2005, M. J. Witte, GARD. Add dehumidification controls and support for multimode DX coil
        //                      Jan. 2008, R. Raustad, FSEC. Added coolreheat to all coil types
        //                      Feb. 2013, B. Shen, ORNL. Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
        //                      Nov. 2016, R. Zhang, LBNL. Applied the coil supply air temperature sensor offset fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine updates the System outlet nodes.

        // METHODOLOGY EMPLOYED:
        //  Data is moved from the System data structure to the System outlet nodes.

        // Using/Aliasing
        using namespace ScheduleManager;
        using DataHVACGlobals::TempControlTol;
        using DXCoils::SimDXCoil;
        using DXCoils::SimDXCoilMultiMode;
        using DXCoils::SimDXCoilMultiSpeed;

        using General::SolveRoot;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using PackagedThermalStorageCoil::ControlTESIceStorageTankCoil;
        using PackagedThermalStorageCoil::SimTESCoil;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyTdpFnWPb;
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);         // Maximum number of iterations for solver
        Real64 const Acc(1.e-3);       // Accuracy of solver result
        Real64 const HumRatAcc(1.e-6); // Accuracy of solver result

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CompName; // Name of the DX cooling coil
        Real64 NoOutput;      // Sensible capacity (outlet - inlet) when the compressor is off
        Real64 FullOutput;    // Sensible capacity (outlet - inlet) when the compressor is on
        Real64 ReqOutput;     // Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
        int InletNode;        // Inlet node number of the DX cooling coil
        int OutletNode;       // Outlet node number of the DX cooling coil
        int ControlNode;      // The node number where a setpoint is placed to control the DX cooling coil
        Real64 PartLoadFrac;  // The part-load fraction of the compressor
        Real64 SpeedRatio;    // SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
        //              (CompressorSpeedMax - CompressorSpeedMin)
        // for variable speed or 2 speed compressors
        Real64 CycRatio;               // Cycling part-load ratio for variable speed or 2 speed compressors
        Real64 DesOutTemp;             // Desired outlet temperature of the DX cooling coil
        Real64 DesOutHumRat;           // Desired outlet humidity ratio of the DX cooling coil
        Real64 OutletTempDXCoil;       // Actual outlet temperature of the DX cooling coil
        Real64 OutletTempLS;           // Actual outlet temperature of the variable speed DX cooling coil at low speed
        Real64 OutletTempHS;           // Actual outlet temperature of the variable speed DX cooling coil at high speed
        Real64 OutletHumRatLS;         // Actual outlet humrat of the variable speed DX cooling coil at low speed
        Real64 OutletHumRatHS;         // Actual outlet humrat of the variable speed DX cooling coil at high speed
        Real64 OutletHumRatDXCoil;     // Actual outlet humidity ratio of the DX cooling coil
        int SolFla;                    // Flag of solver
        Array1D<Real64> Par(5);        // Parameter array passed to solver
        bool SensibleLoad;             // True if there is a sensible cooling load on this system
        bool LatentLoad;               // True if there is a latent   cooling load on this system
        int DehumidMode;               // Dehumidification mode (0=normal, 1=enhanced)
        int FanOpMode;                 // Supply air fan operating mode
        Real64 TempMinPLR;             // Used to find latent PLR when max iterations exceeded
        Real64 TempMaxPLR;             // Used to find latent PLR when max iterations exceeded
        Real64 TempOutletTempDXCoil;   // Used to find latent PLR when max iterations exceeded
        Real64 TempOutletHumRatDXCoil; // Used to find latent PLR when max iterations exceeded
        Real64 NoLoadHumRatOut;        // DX coil outlet air humidity ratio with comprssor off
        Real64 FullLoadHumRatOut;      // DX coil outlet air humidity ratio with comprssor full on
        // added variables to call variable speed DX coils
        int SpeedNum;                 // speed number of variable speed DX cooling coil
        Real64 QZnReq;                // Zone load (W), input to variable-speed DX coil
        Real64 QLatReq;               // Zone latent load, input to variable-speed DX coil
        Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
        Real64 HPTimeConstant;        // Heat pump time constant [s]
        Real64 FanDelayTime;          // Fan delay time, time delay for the HP's fan to
        Real64 OnOffAirFlowRatio;     // ratio of compressor on flow to average flow over time step
        Real64 TempSpeedOut;          // output at one speed level
        Real64 TempSpeedReqst;        // request capacity at one speed level
        int NumOfSpeeds;              // maximum number of speed
        int VSCoilIndex;              // variable-speed coil index
        int I;                        // interation increment

        auto &DXCoolingSystem(state.dataHVACDXSys->DXCoolingSystem);
        // Set local variables
        // Retrieve the load on the controlled zone
        OutletNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilOutletNodeNum;
        InletNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilInletNodeNum;
        ControlNode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXSystemControlNodeNum;
        DesOutTemp = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletTemp;
        DesOutHumRat = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DesiredOutletHumRat;
        CompName = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilName;
        FanOpMode = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode;
        SpeedRatio = 0.0;
        CycRatio = 0.0;
        PartLoadFrac = 0.0;
        DehumidMode = 0;
        SensibleLoad = false;
        LatentLoad = false;
        SpeedNum = 1;
        QZnReq = 0.0;
        QLatReq = 0.0;
        MaxONOFFCyclesperHour = 4.0; // default number
        HPTimeConstant = LatCapTimeConst;
        FanDelayTime = 0.0;
        OnOffAirFlowRatio = 1.0;
        TempSpeedOut = 0.0;
        TempSpeedReqst = 0.0;
        NumOfSpeeds = 0;
        VSCoilIndex = 0;
        I = 1;

        auto &Node(state.dataLoopNodes->Node);

        // If there is a fault of coil SAT Sensor
        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FaultyCoilSATFlag && (!state.dataGlobal->WarmupFlag) &&
            (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {
            // calculate the sensor offset using fault information
            int FaultIndex = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FaultyCoilSATIndex;
            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FaultyCoilSATOffset =
                state.dataFaultsMgr->FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the DesOutTemp
            DesOutTemp -= state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FaultyCoilSATOffset;
        }

        // If state.dataHVACDXSys->DXCoolingSystem is scheduled on and there is flow
        if ((GetCurrentScheduleValue(state, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SchedPtr) > 0.0) &&
            (Node(InletNode).MassFlowRate > MinAirMassFlow)) {

            // Determine if there is a sensible load on this system
            if ((Node(InletNode).Temp > Node(ControlNode).TempSetPoint) && (Node(InletNode).Temp > DesOutTemp) &&
                (std::abs(Node(InletNode).Temp - DesOutTemp) > TempControlTol))
                SensibleLoad = true;

            // Determine if there is a latent load on this system - for future use to serve latent-only loads
            if ((Node(InletNode).HumRat > Node(ControlNode).HumRatMax) && (Node(InletNode).HumRat > DesOutHumRat)) LatentLoad = true;

            // If state.dataHVACDXSys->DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
            // Multimode coil will switch to enhanced dehumidification if available and needed, but it
            // still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
            if ((SensibleLoad && state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnSensibleLoad) ||
                (LatentLoad && state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnLatentLoad)) {
                // calculate sensible PLR, don't care if latent is true here but need to gaurd for
                // when LatentLoad=TRUE and SensibleLoad=FALSE
                {
                    auto const SELECT_CASE_var(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType_Num);

                    if (SELECT_CASE_var == CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

                        // Get no load result
                        PartLoadFrac = 0.0;
                        SimDXCoil(state,
                                  CompName,
                                  On,
                                  FirstHVACIteration,
                                  state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                  FanOpMode,
                                  PartLoadFrac);
                        NoOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                   PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        NoLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        // Get full load result
                        PartLoadFrac = 1.0;
                        SimDXCoil(state,
                                  CompName,
                                  On,
                                  FirstHVACIteration,
                                  state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                  FanOpMode,
                                  PartLoadFrac);
                        FullLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        FullOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                     PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                        ReqOutput = Node(InletNode).MassFlowRate *
                                    (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                        //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                        if ((NoOutput - ReqOutput) < Acc) {
                            PartLoadFrac = 0.0;
                            //         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
                            //         run the compressor at PartLoadFrac = 1.
                        } else if ((FullOutput - ReqOutput) > Acc) {
                            PartLoadFrac = 1.0;
                            //         Else find the PLR to meet the load
                        } else {
                            //           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this
                            //           temp is greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the
                            //           operating PLR.
                            OutletTempDXCoil =
                                state.dataDXCoils->DXCoilOutletTemp(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            if (OutletTempDXCoil > DesOutTemp) {
                                PartLoadFrac = 1.0;
                            } else {
                                if (state.dataGlobal->DoCoilDirectSolutions) {
                                    PartLoadFrac = (ReqOutput - NoOutput) / (FullOutput - NoOutput);
                                    SimDXCoil(state,
                                              CompName,
                                              On,
                                              FirstHVACIteration,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                              FanOpMode,
                                              PartLoadFrac);
                                } else {
                                    Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    Par(2) = DesOutTemp;
                                    Par(5) = double(FanOpMode);
                                    General::SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(state,
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                               " \"" + state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                               "\" - Iteration limit exceeded calculating sensible part-load ratio "
                                                                               "error continues. Sensible PLR "
                                                                               "statistics follow.",
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIterIndex,
                                                                           PartLoadFrac,
                                                                           PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        PartLoadFrac = ReqOutput / FullOutput;
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - DX unit sensible part-load ratio calculation failed: part-load ratio limits "
                                                                     "exceeded, for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(state,
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                               " \"" + state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                               "\" - DX unit sensible part-load ratio calculation failed error "
                                                                               "continues. Sensible PLR statistics "
                                                                               "follow.",
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFailIndex,
                                                                           PartLoadFrac,
                                                                           PartLoadFrac);
                                        }
                                    }
                                }
                            }
                        }

                        //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                        //         else use operating humidity ratio to test against humidity setpoint
                        if (PartLoadFrac == 0.0) {
                            OutletHumRatDXCoil = NoLoadHumRatOut;
                        } else {
                            OutletHumRatDXCoil =
                                state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                        }

                        // If humidity setpoint is not satisfied and humidity control type is CoolReheat,
                        // then overcool to meet moisture load

                        if ((OutletHumRatDXCoil > DesOutHumRat) && (PartLoadFrac < 1.0) &&
                            (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::CoolReheat)) {

                            //           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                            //           do not run the compressor
                            if ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc) {
                                // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                                //           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                                //           run the compressor at PartLoadFrac = 1.
                            } else if ((DesOutHumRat - FullLoadHumRatOut) < HumRatAcc) {
                                PartLoadFrac = 1.0;
                                //           Else find the PLR to meet the load
                            } else {
                                if (state.dataGlobal->DoCoilDirectSolutions) {
                                    PartLoadFrac = (ReqOutput - NoOutput) / (FullOutput - NoOutput);
                                    SimDXCoil(state,
                                              CompName,
                                              On,
                                              FirstHVACIteration,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                              FanOpMode,
                                              PartLoadFrac);
                                } else {
                                    Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    Par(2) = DesOutHumRat;
                                    Par(5) = double(FanOpMode);
                                    General::SolveRoot(state, HumRatAcc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilHumRatResidual, 0.0, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio   = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                    "statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        //               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
                                        if (NoLoadHumRatOut - FullLoadHumRatOut != 0.0) {
                                            PartLoadFrac = (NoLoadHumRatOut - DesOutHumRat) / (NoLoadHumRatOut - FullLoadHumRatOut);
                                        } else {
                                            PartLoadFrac = 1.0;
                                        }
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFail;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - DX unit latent part-load ratio calculation failed: part-load ratio limits "
                                                                     "exceeded, for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                    "follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - CoolReheat humidity control

                        if (PartLoadFrac > 1.0) {
                            PartLoadFrac = 1.0;
                        } else if (PartLoadFrac < 0.0) {
                            PartLoadFrac = 0.0;
                        }

                    } else if (SELECT_CASE_var == CoilDX_CoolingHXAssisted) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                        //         Check the dehumidification control type. If it's multimode, turn off the HX to find the sensible PLR. Then check to
                        //         see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
                        if (DXCoolingSystem(DXSystemNum).DehumidControlType != DehumidControl::Multimode) {
                            HXUnitOn = true;
                        } else {
                            HXUnitOn = false;
                        }

                        // Get no load result
                        PartLoadFrac = 0.0;
                        SimHXAssistedCoolingCoil(state,
                                                 CompName,
                                                 FirstHVACIteration,
                                                 On,
                                                 PartLoadFrac,
                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                 FanOpMode,
                                                 HXUnitOn,
                                                 _,
                                                 state.dataHVACDXSys->EconomizerFlag);
                        NoOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                   PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        NoLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        // Get full load result
                        PartLoadFrac = 1.0;
                        SimHXAssistedCoolingCoil(state,
                                                 CompName,
                                                 FirstHVACIteration,
                                                 On,
                                                 PartLoadFrac,
                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                 FanOpMode,
                                                 HXUnitOn,
                                                 _,
                                                 state.dataHVACDXSys->EconomizerFlag);
                        FullOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                     PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        FullLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        ReqOutput = Node(InletNode).MassFlowRate *
                                    (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                        //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                        if ((NoOutput - ReqOutput) < Acc) {
                            PartLoadFrac = 0.0;
                            //         If the FullOutput is greater than or very near the ReqOutput, then run the compressor at PartLoadFrac = 1.
                        } else if ((FullOutput - ReqOutput) > Acc) {
                            PartLoadFrac = 1.0;
                            //         Else find the PLR to meet the load
                        } else {
                            //           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
                            //           If this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac
                            //           = 1. (i.e. HX iterates to find solution, don't allow the tolerance in solution to trip up RegulaFalsi. So if
                            //           solution is very near request, run compressor at PLR = 1)
                            OutletTempDXCoil = state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            if ((OutletTempDXCoil > DesOutTemp) || std::abs(OutletTempDXCoil - DesOutTemp) <= (Acc * 2.0)) {
                                PartLoadFrac = 1.0;
                            } else {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutTemp;
                                // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                                if (FirstHVACIteration) {
                                    Par(3) = 1.0;
                                } else {
                                    Par(3) = 0.0;
                                }
                                if (HXUnitOn) {
                                    Par(4) = 1.0;
                                } else {
                                    Par(4) = 0.0;
                                }
                                Par(5) = double(FanOpMode);
                                General::SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {

                                    //               RegulaFalsi may not find sensible PLR when the latent degradation model is used.
                                    //               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                    TempMaxPLR = -0.1;
                                    TempOutletTempDXCoil = Node(InletNode).Temp;
                                    while ((TempOutletTempDXCoil - DesOutTemp) > 0.0 && TempMaxPLR <= 1.0) {
                                        //                 find upper limit of PLR
                                        TempMaxPLR += 0.1;
                                        SimHXAssistedCoolingCoil(state,
                                                                 CompName,
                                                                 FirstHVACIteration,
                                                                 On,
                                                                 TempMaxPLR,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                                 FanOpMode,
                                                                 HXUnitOn,
                                                                 _,
                                                                 state.dataHVACDXSys->EconomizerFlag);
                                        TempOutletTempDXCoil =
                                            state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    }
                                    TempMinPLR = TempMaxPLR;
                                    while ((TempOutletTempDXCoil - DesOutTemp) < 0.0 && TempMinPLR >= 0.0) {
                                        //                 pull upper limit of PLR down to last valid limit (i.e. outlet temp still exceeds
                                        //                 DesOutTemp)
                                        TempMaxPLR = TempMinPLR;
                                        //                 find minimum limit of PLR
                                        TempMinPLR -= 0.01;
                                        SimHXAssistedCoolingCoil(state,
                                                                 CompName,
                                                                 FirstHVACIteration,
                                                                 On,
                                                                 TempMinPLR,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                                 FanOpMode,
                                                                 HXUnitOn,
                                                                 _,
                                                                 state.dataHVACDXSys->EconomizerFlag);
                                        TempOutletTempDXCoil =
                                            state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    }
                                    //               Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary
                                    //               may be very near the desired result)
                                    TempMinPLR = max(0.0, (TempMinPLR - 0.01));
                                    TempMaxPLR = min(1.0, (TempMaxPLR + 0.01));
                                    //               tighter boundary of solution has been found, call RegulaFalsi a second time
                                    General::SolveRoot(
                                        state, Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, TempMinPLR, TempMaxPLR, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio   = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible "
                                                    "PLR statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        PartLoadFrac = ReqOutput / FullOutput;
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFail;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - DX unit sensible part-load ratio calculation unexpectedly failed: part-load "
                                                                     "ratio limits exceeded, for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - DX unit sensible part-load ratio calculation unexpectedly failed error continues. Sensible "
                                                    "PLR statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }

                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFail2 < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFail2;
                                            ShowWarningError(state,
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                 " - DX unit sensible part-load ratio calculation failed: part-load ratio limits "
                                                                 "exceeded, for unit = " +
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state, "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics "
                                                "follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedSensPLRFailIndex2,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            }
                        }

                        //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                        //         else use operating humidity ratio to test against humidity setpoint
                        if (PartLoadFrac == 0.0) {
                            OutletHumRatDXCoil = NoLoadHumRatOut;
                        } else {
                            OutletHumRatDXCoil = state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                        }

                        // If humidity setpoint is not satisfied and humidity control type is MultiMode,
                        // then enable heat exchanger and run to meet sensible load

                        if ((OutletHumRatDXCoil > DesOutHumRat) && (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::Multimode)) {

                            // Determine required part load when heat exchanger is ON
                            HXUnitOn = true;
                            PartLoadFrac = 1.0;
                            SimHXAssistedCoolingCoil(state,
                                                     CompName,
                                                     FirstHVACIteration,
                                                     On,
                                                     PartLoadFrac,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                     FanOpMode,
                                                     HXUnitOn,
                                                     _,
                                                     state.dataHVACDXSys->EconomizerFlag);

                            OutletTempDXCoil = state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                            //           FullOutput will be different than the FullOutput determined above during sensible PLR calculations
                            FullOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                         PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                            ReqOutput = Node(InletNode).MassFlowRate *
                                        (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                            //           Check to see if the system can meet the load with the compressor off
                            //           IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                            if ((NoOutput - ReqOutput) < Acc) {
                                PartLoadFrac = 0.0;
                                //           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
                                //           If this temp is greater than or very near the desired outlet temp, then run the compressor at
                                //           PartLoadFrac = 1.
                            } else if ((OutletTempDXCoil > DesOutTemp) || std::abs(OutletTempDXCoil - DesOutTemp) <= (Acc * 2.0)) {
                                PartLoadFrac = 1.0;
                            } else {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutTemp;
                                // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
                                if (FirstHVACIteration) {
                                    Par(3) = 1.0;
                                } else {
                                    Par(3) = 0.0;
                                }
                                if (HXUnitOn) {
                                    Par(4) = 1.0;
                                } else {
                                    Par(4) = 0.0;
                                }
                                Par(5) = double(FanOpMode);
                                General::SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRIter < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRIter;
                                            ShowWarningError(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                    " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated latent part-load ratio   = {:.3R}", (ReqOutput / FullOutput)));
                                            ShowContinueError(state, format("Calculated latent part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state,
                                                "The calculated latent part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                "statistics follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRIterIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRFail < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRFail;
                                            ShowWarningError(state,
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                 " - DX unit latent part-load ratio calculation failed: part-load ratio limits "
                                                                 "exceeded, for unit = " +
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state, "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                "follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedLatPLRFailIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - Multimode humidity control

                        // If humidity setpoint is not satisfied and humidity control type is CoolReheat, then calculate
                        // a new latent PLR

                        if (OutletHumRatDXCoil > DesOutHumRat && PartLoadFrac < 1.0 &&
                            DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::CoolReheat) {

                            //           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                            //           do not run the compressor
                            if ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc * 2.0) {
                                // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                                //           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                                //           run the compressor at PartLoadFrac = 1.
                            } else if ((DesOutHumRat - FullLoadHumRatOut) < HumRatAcc * 2.0) {
                                PartLoadFrac = 1.0;
                                //           Else find the PLR to meet the load
                            } else {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutHumRat;
                                // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                                if (FirstHVACIteration) {
                                    Par(3) = 1.0;
                                } else {
                                    Par(3) = 0.0;
                                }
                                if (HXUnitOn) {
                                    Par(4) = 1.0;
                                } else {
                                    Par(4) = 0.0;
                                }
                                Par(5) = double(FanOpMode);
                                General::SolveRoot(state, HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {

                                    //               RegulaFalsi may not find latent PLR when the latent degradation model is used.
                                    //               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                    TempMaxPLR = -0.1;
                                    TempOutletHumRatDXCoil = OutletHumRatDXCoil;
                                    while ((OutletHumRatDXCoil - TempOutletHumRatDXCoil) >= 0.0 && TempMaxPLR <= 1.0) {
                                        //                 find upper limit of LatentPLR
                                        TempMaxPLR += 0.1;
                                        SimHXAssistedCoolingCoil(state,
                                                                 CompName,
                                                                 FirstHVACIteration,
                                                                 On,
                                                                 TempMaxPLR,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                                 FanOpMode,
                                                                 HXUnitOn,
                                                                 _,
                                                                 state.dataHVACDXSys->EconomizerFlag);
                                        OutletHumRatDXCoil =
                                            state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    }
                                    TempMinPLR = TempMaxPLR;
                                    while ((OutletHumRatDXCoil - TempOutletHumRatDXCoil) <= 0.0 && TempMinPLR >= 0.0) {
                                        //                 pull upper limit of LatentPLR down to last valid limit (i.e. latent output still exceeds
                                        //                 SystemMoisuterLoad)
                                        TempMaxPLR = TempMinPLR;
                                        //                 find minimum limit of Latent PLR
                                        TempMinPLR -= 0.01;
                                        SimHXAssistedCoolingCoil(state,
                                                                 CompName,
                                                                 FirstHVACIteration,
                                                                 On,
                                                                 TempMaxPLR,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                                 FanOpMode,
                                                                 HXUnitOn,
                                                                 _,
                                                                 state.dataHVACDXSys->EconomizerFlag);
                                        OutletHumRatDXCoil =
                                            state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    }
                                    //               tighter boundary of solution has been found, call RegulaFalsi a second time
                                    General::SolveRoot(
                                        state, HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, TempMinPLR, TempMaxPLR, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state,
                                                                  format("Estimated latent part-load ratio   = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated latent part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(state,
                                                                           "The calculated latent part-load ratio will be used and the simulation "
                                                                           "continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                    "statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }

                                    } else if (SolFla == -2) {
                                        PartLoadFrac = ReqOutput / FullOutput;
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFail;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - DX unit latent part-load ratio calculation failed unexpectedly: part-load ratio "
                                                        "limits exceeded, for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - DX unit latent part-load ratio calculation failed unexpectedly error continues. Latent PLR "
                                                    "statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFail2 < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFail2;
                                            ShowWarningError(state,
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                 " - DX unit latent part-load ratio calculation failed: part-load ratio limits "
                                                                 "exceeded, for unit = " +
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state, "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                "follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).HXAssistedCRLatPLRFailIndex2,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - CoolReheat humidity control

                        if (PartLoadFrac > 1.0) {
                            PartLoadFrac = 1.0;
                        } else if (PartLoadFrac < 0.0) {
                            PartLoadFrac = 0.0;
                        }

                    } else if (SELECT_CASE_var == CoilDX_CoolingTwoSpeed) { // Coil:Cooling:DX:TwoSpeed
                        // formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL
                        //         SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
                        SimDXCoilMultiSpeed(state, CompName, 0.0, 1.0, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                        OutletTempLS = state.dataDXCoils->DXCoilOutletTemp(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                        if (OutletTempLS > DesOutTemp && SensibleLoad) {
                            CycRatio = 1.0;
                            SimDXCoilMultiSpeed(state, CompName, 1.0, 1.0, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            OutletTempHS = state.dataDXCoils->DXCoilOutletTemp(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            if (OutletTempHS < DesOutTemp) {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutTemp;
                                SolveRoot(state, Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdSensPLRIter < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdSensPLRIter;
                                            ShowWarningError(state,
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                 " - Iteration limit exceeded calculating DX unit sensible speed ratio for unit = " +
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Calculated speed ratio = {:.3R}", SpeedRatio));
                                            ShowContinueErrorTimeStamp(
                                                state, "The calculated speed ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                           "\" - Iteration limit exceeded calculating sensible speed ratio error "
                                                                           "continues. Sensible speed ratio statistics follow.",
                                                                       state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdSensPLRIterIndex,
                                                                       SpeedRatio,
                                                                       SpeedRatio);
                                    }
                                } else if (SolFla == -2) {
                                    if (!state.dataGlobal->WarmupFlag)
                                        ShowFatalError(state,
                                                       state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                           " - compressor speed calculation failed: speed limits exceeded, for unit=" +
                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                }
                            } else {
                                SpeedRatio = 1.0;
                            }
                        } else if (SensibleLoad) {
                            SpeedRatio = 0.0;
                            Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            Par(2) = DesOutTemp;
                            SolveRoot(state, Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par);
                            if (SolFla == -1) {
                                if (!state.dataGlobal->WarmupFlag) {
                                    if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycSensPLRIter < 1) {
                                        ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycSensPLRIter;
                                        ShowWarningError(state,
                                                         state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                             " - Iteration limit exceeded calculating DX unit sensible cycling ratio for unit = " +
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                        ShowContinueError(state, format("Calculated cycling ratio = {:.3R}", CycRatio));
                                        ShowContinueErrorTimeStamp(
                                            state, "The calculated cycling ratio will be used and the simulation continues. Occurrence info:");
                                    }
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                                       state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                       "\" - Iteration limit exceeded calculating sensible cycling ratio error "
                                                                       "continues. Sensible cycling ratio statistics follow.",
                                                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycSensPLRIterIndex,
                                                                   CycRatio,
                                                                   CycRatio);
                                }
                            } else if (SolFla == -2) { // should never get here, if it does logic above to protect from this
                                if (!state.dataGlobal->WarmupFlag)
                                    ShowFatalError(state,
                                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                       " - cycling ratio calculation failed: cycling limits exceeded, for unit=" +
                                                       state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                            }
                        } else {
                            PartLoadFrac = 0.0;
                            SpeedRatio = 0.0;
                            CycRatio = 0.0;
                            DehumidMode = 0;
                        }

                        if (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::CoolReheat) {

                            //           Simulate MultiSpeed DX coil at sensible result
                            SimDXCoilMultiSpeed(
                                state, CompName, SpeedRatio, CycRatio, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                            OutletHumRatDXCoil =
                                state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            // If humidity setpoint is not satisfied and humidity control type is CoolReheat,
                            // then overcool to meet moisture load

                            if (OutletHumRatDXCoil > DesOutHumRat) {

                                CycRatio = 0.0;
                                SpeedRatio = 0.0;

                                //             SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
                                SimDXCoilMultiSpeed(state, CompName, 0.0, 1.0, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                OutletHumRatLS =
                                    state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                if (OutletHumRatLS > DesOutHumRat) {
                                    CycRatio = 1.0;
                                    SimDXCoilMultiSpeed(
                                        state, CompName, 1.0, 1.0, state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    OutletHumRatHS =
                                        state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    if (OutletHumRatHS < DesOutHumRat) {
                                        Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                        Par(2) = DesOutHumRat;
                                        SolveRoot(state, HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                                        if (SolFla == -1) {
                                            if (!state.dataGlobal->WarmupFlag) {
                                                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdLatPLRIter < 1) {
                                                    ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdLatPLRIter;
                                                    ShowWarningError(
                                                        state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " - Iteration limit exceeded calculating DX unit latent speed ratio for unit = " +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                    ShowContinueError(state, format("Calculated speed ratio = {:.3R}", SpeedRatio));
                                                    ShowContinueErrorTimeStamp(
                                                        state,
                                                        "The calculated speed ratio will be used and the simulation continues. Occurrence info:");
                                                }
                                                ShowRecurringWarningErrorAtEnd(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                        "\" - Iteration limit exceeded calculating latent speed ratio error continues. Latent speed "
                                                        "ratio statistics follow.",
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdLatPLRIterIndex,
                                                    SpeedRatio,
                                                    SpeedRatio);
                                            }
                                        } else if (SolFla == -2) {
                                            if (!state.dataGlobal->WarmupFlag)
                                                ShowFatalError(state,
                                                               state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                   " - compressor speed calculation failed:speed limits exceeded, for unit=" +
                                                                   state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                        }
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                } else {
                                    SpeedRatio = 0.0;
                                    Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    Par(2) = DesOutHumRat;
                                    SolveRoot(state, HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycLatPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit latent cycling ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Calculated cycling ratio = {:.3R}", CycRatio));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated cycling ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(state,
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                               " \"" + state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                               "\" - Iteration limit exceeded calculating latent cycling ratio error "
                                                                               "continues. Latent cycling ratio statistics follow.",
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MSpdCycLatPLRIterIndex,
                                                                           CycRatio,
                                                                           CycRatio);
                                        }
                                    } else if (SolFla == -2) { // should never get here, if it does logic above to protect from this
                                        if (!state.dataGlobal->WarmupFlag)
                                            ShowFatalError(state,
                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                               " - cycling ratio calculation failed: cycling limits exceeded, for unit=" +
                                                               state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                    }
                                }
                            }
                        }

                    } else if (SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
                        // formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL)

                        // Get no load result
                        PartLoadFrac = 0.0;
                        SimDXCoilMultiMode(state,
                                           CompName,
                                           On,
                                           FirstHVACIteration,
                                           PartLoadFrac,
                                           DehumidMode,
                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                           FanOpMode);
                        NoOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                   PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        NoLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        // Get full load result
                        PartLoadFrac = 1.0;
                        SimDXCoilMultiMode(state,
                                           CompName,
                                           On,
                                           FirstHVACIteration,
                                           PartLoadFrac,
                                           DehumidMode,
                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                           FanOpMode);
                        FullLoadHumRatOut = state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        FullOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                     PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                        ReqOutput = Node(InletNode).MassFlowRate *
                                    (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));

                        //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                        if ((NoOutput - ReqOutput) < Acc) {
                            PartLoadFrac = 0.0;
                            //         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
                            //         run the compressor at PartLoadFrac = 1.
                        } else if ((FullOutput - ReqOutput) > Acc) {
                            PartLoadFrac = 1.0;
                            //         Else find the PLR to meet the load
                        } else {
                            OutletTempDXCoil =
                                state.dataDXCoils->DXCoilOutletTemp(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                            if (OutletTempDXCoil > DesOutTemp) {
                                PartLoadFrac = 1.0;
                            } else {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutTemp;
                                // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par(3) = double(DehumidMode);
                                Par(4) = double(FanOpMode);
                                SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeSensPLRIter < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeSensPLRIter;
                                            ShowWarningError(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                    " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                            ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state, "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR "
                                                "statistics follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeSensPLRIterIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                } else if (SolFla == -2) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        ShowSevereError(state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                        ShowFatalError(state, "Program terminates due to previous condition.");
                                    }
                                }
                            }
                        }

                        OutletHumRatDXCoil =
                            state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);

                        // If humidity setpoint is not satisfied and humidity control type is Multimode,
                        // then turn on enhanced dehumidification mode 1

                        if ((OutletHumRatDXCoil > DesOutHumRat) && (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::Multimode)) {

                            // Determine required part load for enhanced dehumidification mode 1

                            // Get full load result
                            PartLoadFrac = 1.0;
                            DehumidMode = 1;
                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DehumidificationMode = DehumidMode;
                            SimDXCoilMultiMode(state,
                                               CompName,
                                               On,
                                               FirstHVACIteration,
                                               PartLoadFrac,
                                               DehumidMode,
                                               state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                               FanOpMode);
                            FullOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(InletNode).HumRat) -
                                                                         PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));

                            ReqOutput = Node(InletNode).MassFlowRate *
                                        (PsyHFnTdbW(DesOutTemp, Node(InletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));

                            // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
                            // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
                            // Calculate the part load fraction
                            if (FullOutput >= 0) {
                                PartLoadFrac = 0.0;
                            } else {
                                OutletTempDXCoil =
                                    state.dataDXCoils->DXCoilOutletTemp(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                OutletHumRatDXCoil =
                                    state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                // if sensible load and setpoint cannot be met, set PLR = 1. If no sensible load and
                                // latent load exists and setpoint cannot be met, set PLR = 1.
                                if ((OutletTempDXCoil >= DesOutTemp && SensibleLoad &&
                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnSensibleLoad) ||
                                    (OutletHumRatDXCoil >= DesOutHumRat && !SensibleLoad && LatentLoad &&
                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnLatentLoad)) {
                                    PartLoadFrac = 1.0;
                                    // if no sensible load and latent load can be met, find PLR
                                } else if (!SensibleLoad && (OutletHumRatDXCoil < DesOutHumRat && LatentLoad &&
                                                             state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).RunOnLatentLoad)) {
                                    // is a latent load with no sensible load, iterate on humidity ratio
                                    Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    Par(2) = DesOutHumRat;
                                    // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                    Par(3) = double(DehumidMode);
                                    Par(4) = double(FanOpMode);
                                    SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - Iteration limit exceeded calculating DX unit multimode latent (no sensible) "
                                                                     "part-load ratio for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                if (NoLoadHumRatOut - OutletHumRatDXCoil > 0.0) {
                                                    TempMinPLR = (DesOutHumRat - OutletHumRatDXCoil) / (NoLoadHumRatOut - OutletHumRatDXCoil);
                                                } else {
                                                    TempMinPLR = PartLoadFrac + 0.001;
                                                }
                                                ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", TempMinPLR));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating multimode latent (no sensible) part-load ratio error "
                                                    "continues. Latent PLR statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            ShowSevereError(state,
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" +
                                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowFatalError(state, "Program terminates due to previous condition.");
                                        }
                                    }

                                } else { // must be a sensible load so find PLR
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                    Par(2) = DesOutTemp;
                                    // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                    Par(3) = double(DehumidMode);
                                    Par(4) = double(FanOpMode);
                                    SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - Iteration limit exceeded calculating DX unit multimode latent part-load "
                                                                     "ratio for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating multimode latent part-load ratio error continues. "
                                                    "Latent PLR statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        if (!state.dataGlobal->WarmupFlag) {
                                            ShowSevereError(state,
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" +
                                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowFatalError(state, "Program terminates due to previous condition.");
                                        }
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - multimode humidity control

                        //         If humidity setpoint is not satisfied and humidity control type is CoolReheat, then run to meet latent load
                        //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                        //         else use operating humidity ratio to test against humidity setpoint
                        if (PartLoadFrac == 0.0) {
                            OutletHumRatDXCoil = NoLoadHumRatOut;
                        } else {
                            OutletHumRatDXCoil =
                                state.dataDXCoils->DXCoilOutletHumRat(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                        }

                        if ((OutletHumRatDXCoil > DesOutHumRat) && (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::CoolReheat)) {

                            //            CoolReheat operates cooling stage 1 and/or 2 to meet DesOutHumRat. Dehumidification mode is not active.
                            DehumidMode = 0;

                            //            IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                            //            do not run the compressor
                            if ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc) {
                                // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                                //            If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                                //            run the compressor at PartLoadFrac = 1.
                            } else if ((DesOutHumRat - FullLoadHumRatOut) < HumRatAcc) {
                                PartLoadFrac = 1.0;
                                //            Else find the PLR to meet the load
                            } else {
                                Par(1) = double(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex);
                                Par(2) = DesOutHumRat;
                                // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par(3) = double(DehumidMode);
                                Par(4) = double(FanOpMode);
                                SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter2 < 1) {
                                            ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIter2;
                                            ShowWarningError(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                    " - Iteration limit exceeded calculating DX unit coolreheat latent part-load ratio for unit = " +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                            ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                            ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                            ShowContinueErrorTimeStamp(
                                                state, "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            state,
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                "\" - Iteration limit exceeded calculating coolreheat latent part-load ratio error continues. Latent "
                                                "PLR statistics follow.",
                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).MModeLatPLRIterIndex2,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                } else if (SolFla == -2) {
                                    if (!state.dataGlobal->WarmupFlag) {
                                        ShowSevereError(state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                        ShowFatalError(state, "Program terminates due to previous condition.");
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - CoolReheat humidity control

                        if (PartLoadFrac > 1.0) {
                            PartLoadFrac = 1.0;
                        } else if (PartLoadFrac < 0.0) {
                            PartLoadFrac = 0.0;
                        }
                    } else if (SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed) { // Coil:Cooling:DX:VariableSpeed

                        PartLoadFrac = 0.0;
                        SpeedNum = 1;
                        QZnReq = 0.0;
                        QLatReq = 0.0;
                        MaxONOFFCyclesperHour = 4.0; // default number
                        HPTimeConstant = LatCapTimeConst;
                        FanDelayTime = 0.0;
                        OnOffAirFlowRatio = 1.0;
                        SpeedRatio = 0.0;

                        SimVariableSpeedCoils(state,
                                              CompName,
                                              state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                              FanOpMode,
                                              MaxONOFFCyclesperHour,
                                              HPTimeConstant,
                                              FanDelayTime,
                                              On,
                                              PartLoadFrac,
                                              SpeedNum,
                                              SpeedRatio,
                                              QZnReq,
                                              QLatReq,
                                              OnOffAirFlowRatio);

                        VSCoilIndex = state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex;
                        NumOfSpeeds = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).NumOfSpeeds;

                        NoOutput = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                   PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        ReqOutput = Node(InletNode).MassFlowRate *
                                    (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                        NoLoadHumRatOut = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).OutletAirHumRat;

                        //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                        if ((NoOutput - ReqOutput) < Acc) {
                            PartLoadFrac = 0.0;
                            SpeedNum = 0;
                            SpeedRatio = 0.0;
                        } else {
                            Real64 TempOut1;
                            Real64 TempOut2;
                            // Get full load result
                            PartLoadFrac = 1.0;
                            SpeedNum = 1;
                            SpeedRatio = 1.0;
                            CycRatio = 1.0;
                            QZnReq = 0.001; // to indicate the coil is running
                            SimVariableSpeedCoils(state,
                                                  CompName,
                                                  VSCoilIndex,
                                                  FanOpMode,
                                                  MaxONOFFCyclesperHour,
                                                  HPTimeConstant,
                                                  FanDelayTime,
                                                  On,
                                                  PartLoadFrac,
                                                  SpeedNum,
                                                  SpeedRatio,
                                                  QZnReq,
                                                  QLatReq,
                                                  OnOffAirFlowRatio);

                            TempSpeedOut = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                           PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                            TempSpeedReqst = Node(InletNode).MassFlowRate * (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) -
                                                                             PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                            TempOut1 = Node(OutletNode).Temp;

                            if ((TempSpeedReqst - TempSpeedOut) > Acc) {
                                if (state.dataGlobal->DoCoilDirectSolutions) {
                                    PartLoadFrac = (DesOutTemp - Node(InletNode).Temp) / (TempOut1 - Node(InletNode).Temp);
                                    SimVariableSpeedCoils(state,
                                                          CompName,
                                                          VSCoilIndex,
                                                          FanOpMode,
                                                          MaxONOFFCyclesperHour,
                                                          HPTimeConstant,
                                                          FanDelayTime,
                                                          On,
                                                          PartLoadFrac,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          QZnReq,
                                                          QLatReq,
                                                          OnOffAirFlowRatio);
                                } else {
                                    Par(1) = double(VSCoilIndex);
                                    Par(2) = DesOutTemp;
                                    Par(5) = double(FanOpMode);
                                    SpeedRatio = 0.0;
                                    SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingResidual, 1.0e-10, 1.0, Par);
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag && std::abs(Node(OutletNode).Temp - DesOutTemp) > Acc) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state,
                                                                  format("Estimated part-load ratio  = {:.3R}", (TempSpeedOut / TempSpeedReqst)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating sensible part-load ratio error "
                                                    "continues. Sensible PLR statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        PartLoadFrac = TempSpeedReqst / TempSpeedOut;
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - DX unit sensible part-load ratio calculation failed: part-load ratio limits "
                                                                     "exceeded, for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(state,
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                               " \"" + state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                                               "\" - DX unit sensible part-load ratio calculation failed error "
                                                                               "continues. Sensible PLR statistics follow.",
                                                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFailIndex,
                                                                           PartLoadFrac,
                                                                           PartLoadFrac);
                                        }
                                    }
                                }
                                CycRatio = PartLoadFrac;
                            } else if ((TempSpeedOut - TempSpeedReqst) > Acc && NumOfSpeeds > 1) {
                                // Check to see which speed to meet the load
                                PartLoadFrac = 1.0;
                                SpeedRatio = 1.0;
                                for (I = 2; I <= NumOfSpeeds; ++I) {
                                    SpeedNum = I;
                                    SimVariableSpeedCoils(state,
                                                          CompName,
                                                          VSCoilIndex,
                                                          FanOpMode,
                                                          MaxONOFFCyclesperHour,
                                                          HPTimeConstant,
                                                          FanDelayTime,
                                                          On,
                                                          PartLoadFrac,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          QZnReq,
                                                          QLatReq,
                                                          OnOffAirFlowRatio);

                                    TempSpeedOut = Node(InletNode).MassFlowRate * (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) -
                                                                                   PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                                    TempSpeedReqst = Node(InletNode).MassFlowRate * (PsyHFnTdbW(DesOutTemp, Node(OutletNode).HumRat) -
                                                                                     PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
                                    TempOut2 = Node(OutletNode).Temp;

                                    // break here if TempSpeedReqst is greater than TempSpeedOut, then only call SolveRoot below if delta load > Acc,
                                    // otherwise just use this answer (i.e., SpeedNum = x and SpeedRatio = 1.0)
                                    if ((TempSpeedReqst - TempSpeedOut) > -Acc) {
                                        FullOutput = TempSpeedOut;
                                        break;
                                    }
                                    TempOut1 = TempOut2;
                                }
                                if ((TempSpeedReqst - TempSpeedOut) > Acc) {
                                    if (state.dataGlobal->DoCoilDirectSolutions) {
                                        SpeedRatio = (DesOutTemp - TempOut1) / (TempOut2 - TempOut1);
                                        SimVariableSpeedCoils(state,
                                                              CompName,
                                                              VSCoilIndex,
                                                              FanOpMode,
                                                              MaxONOFFCyclesperHour,
                                                              HPTimeConstant,
                                                              FanDelayTime,
                                                              On,
                                                              PartLoadFrac,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              QZnReq,
                                                              QLatReq,
                                                              OnOffAirFlowRatio);
                                    } else {
                                        Par(1) = double(VSCoilIndex);
                                        Par(2) = DesOutTemp;
                                        Par(5) = double(FanOpMode);
                                        if (SpeedNum > 1) {
                                            Par(3) = double(SpeedNum);
                                            SolveRoot(state, HumRatAcc, MaxIte, SolFla, SpeedRatio, VSCoilSpeedResidual, 1.0e-10, 1.0, Par);
                                        } else {
                                            SpeedRatio = 0.0;
                                            SolveRoot(state, Acc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingResidual, 1.0e-10, 1.0, Par);
                                            CycRatio = PartLoadFrac;
                                        }
                                        if (SolFla == -1) {
                                            if (!state.dataGlobal->WarmupFlag && std::abs(Node(OutletNode).Temp - DesOutTemp) > Acc) {
                                                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter < 1) {
                                                    ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter;
                                                    ShowWarningError(
                                                        state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                    ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                                    ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", SpeedRatio));
                                                    ShowContinueErrorTimeStamp(
                                                        state,
                                                        "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                                }
                                                ShowRecurringWarningErrorAtEnd(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                        "\" - Iteration limit exceeded calculating sensible part-load ratio "
                                                        "error continues. Sensible PLR statistics follow.",
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIterIndex,
                                                    PartLoadFrac,
                                                    PartLoadFrac);
                                            }
                                        } else if (SolFla == -2) {
                                            PartLoadFrac = TempSpeedReqst / TempSpeedOut;
                                            if (!state.dataGlobal->WarmupFlag) {
                                                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail < 1) {
                                                    ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail;
                                                    ShowWarningError(
                                                        state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " - DX unit sensible part-load ratio calculation failed: part-load ratio limits "
                                                            "exceeded, for unit = " +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                    ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                    ShowContinueErrorTimeStamp(
                                                        state,
                                                        "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                                }
                                                ShowRecurringWarningErrorAtEnd(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                        "\" - DX unit sensible part-load ratio calculation failed error "
                                                        "continues. Sensible PLR statistics follow.",
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFailIndex,
                                                    PartLoadFrac,
                                                    PartLoadFrac);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                        //         else use operating humidity ratio to test against humidity setpoint
                        if (PartLoadFrac == 0.0) {
                            OutletHumRatDXCoil = NoLoadHumRatOut;
                        } else {
                            OutletHumRatDXCoil =
                                state.dataVariableSpeedCoils->VarSpeedCoil(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex)
                                    .OutletAirHumRat;
                        }

                        // If humidity setpoint is not satisfied and humidity control type is CoolReheat,
                        // then overcool to meet moisture load

                        if ((OutletHumRatDXCoil > DesOutHumRat) && (DXCoolingSystem(DXSystemNum).DehumidControlType == DehumidControl::CoolReheat)) {

                            if ((NumOfSpeeds > 1 && SpeedNum == NumOfSpeeds && SpeedRatio == 1.0) || (NumOfSpeeds == 1 && CycRatio == 1.0)) {
                                PartLoadFrac = 1.0;
                                //           Else find the PLR to meet the load
                            } else {
                                PartLoadFrac = 1.0;
                                SpeedRatio = 1.0;
                                CycRatio = 1.0;
                                QZnReq = 0.001; // to indicate the coil is running
                                SimVariableSpeedCoils(state,
                                                      CompName,
                                                      VSCoilIndex,
                                                      FanOpMode,
                                                      MaxONOFFCyclesperHour,
                                                      HPTimeConstant,
                                                      FanDelayTime,
                                                      On,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      SpeedRatio,
                                                      QZnReq,
                                                      QLatReq,
                                                      OnOffAirFlowRatio);

                                TempSpeedOut = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).OutletAirHumRat;

                                if (NumOfSpeeds > 1 && (TempSpeedOut - DesOutHumRat) > HumRatAcc) {
                                    // Check to see which speed to meet the load
                                    // start at next speed since we already modeled this speed?
                                    int SaveSpeedNum = min(SpeedNum + 1, NumOfSpeeds);
                                    for (I = SaveSpeedNum; I <= NumOfSpeeds; ++I) {
                                        SpeedNum = I;
                                        SimVariableSpeedCoils(state,
                                                              CompName,
                                                              VSCoilIndex,
                                                              FanOpMode,
                                                              MaxONOFFCyclesperHour,
                                                              HPTimeConstant,
                                                              FanDelayTime,
                                                              On,
                                                              PartLoadFrac,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              QZnReq,
                                                              QLatReq,
                                                              OnOffAirFlowRatio);

                                        TempSpeedOut = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilIndex).OutletAirHumRat;
                                        // break here if coil outlet humrat is below DesOutHumRat, then only call SolveRoot below if delta humrat is >
                                        // HumRatAcc, otherwise just use this answer (i.e., SpeedNum = x and SpeedRatio = 1.0)
                                        if ((DesOutHumRat - TempSpeedOut) > -HumRatAcc) {
                                            break;
                                        }
                                    }
                                    if ((DesOutHumRat - TempSpeedOut) > HumRatAcc) {
                                        Par(1) = double(VSCoilIndex);
                                        Par(2) = DesOutHumRat;
                                        Par(5) = double(FanOpMode);
                                        if (SpeedNum > 1) {
                                            Par(3) = double(SpeedNum);
                                            SolveRoot(state, HumRatAcc, MaxIte, SolFla, SpeedRatio, VSCoilSpeedHumResidual, 1.0e-10, 1.0, Par);
                                        } else {
                                            SpeedRatio = 0.0;
                                            SolveRoot(state, HumRatAcc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingHumResidual, 1.0e-10, 1.0, Par);
                                            CycRatio = PartLoadFrac;
                                        }

                                        if (SolFla == -1) {
                                            if (!state.dataGlobal->WarmupFlag && std::abs(Node(OutletNode).HumRat - DesOutHumRat) > (Acc / 100.0)) {
                                                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter < 1) {
                                                    ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter;
                                                    ShowWarningError(
                                                        state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                    ShowContinueError(state, format("Estimated part-load ratio  = {:.3R}", (ReqOutput / FullOutput)));
                                                    ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                    ShowContinueErrorTimeStamp(
                                                        state,
                                                        "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                                }
                                                ShowRecurringWarningErrorAtEnd(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                        "\" - Iteration limit exceeded calculating latent part-load ratio "
                                                        "error continues. Latent PLR statistics follow.",
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIterIndex,
                                                    PartLoadFrac,
                                                    PartLoadFrac);
                                            }
                                        } else if (SolFla == -2) {
                                            PartLoadFrac = SpeedRatio;
                                            if (!state.dataGlobal->WarmupFlag) {
                                                if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail < 1) {
                                                    ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail;
                                                    ShowWarningError(
                                                        state,
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                            " - DX unit latent part-load ratio calculation failed: part-load ratio limits "
                                                            "exceeded, for unit = " +
                                                            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                    ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                    ShowContinueErrorTimeStamp(
                                                        state,
                                                        "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                                }
                                                ShowRecurringWarningErrorAtEnd(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                        "\" - DX unit latent part-load ratio calculation failed error "
                                                        "continues. Latent PLR statistics follow.",
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFailIndex,
                                                    PartLoadFrac,
                                                    PartLoadFrac);
                                            }
                                        }
                                    }
                                } else if ((DesOutHumRat - TempSpeedOut) > HumRatAcc) {
                                    Par(1) = double(VSCoilIndex);
                                    Par(2) = DesOutHumRat;
                                    Par(5) = double(FanOpMode);
                                    if (SpeedNum > 1) {
                                        Par(3) = double(SpeedNum);
                                        SolveRoot(state, HumRatAcc, MaxIte, SolFla, SpeedRatio, VSCoilSpeedHumResidual, 1.0e-10, 1.0, Par);
                                    } else {
                                        SpeedRatio = 0.0;
                                        SolveRoot(state, HumRatAcc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingHumResidual, 1.0e-10, 1.0, Par);
                                        CycRatio = PartLoadFrac;
                                    }
                                    if (SolFla == -1) {
                                        if (!state.dataGlobal->WarmupFlag && std::abs(Node(OutletNode).HumRat - DesOutHumRat) > (Acc / 100.0)) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIter < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIter;
                                                ShowWarningError(
                                                    state,
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                        " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio   = {:.3R}", (ReqOutput / FullOutput)));
                                                ShowContinueError(state, format("Calculated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                    "statistics follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    } else if (SolFla == -2) {
                                        PartLoadFrac = 1.0;
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFail < 1) {
                                                ++state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFail;
                                                ShowWarningError(state,
                                                                 state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType +
                                                                     " - DX unit latent part-load ratio calculation failed: part-load ratio limits "
                                                                     "exceeded, for unit = " +
                                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name);
                                                ShowContinueError(state, format("Estimated part-load ratio = {:.3R}", PartLoadFrac));
                                                ShowContinueErrorTimeStamp(
                                                    state,
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType + " \"" +
                                                    state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).Name +
                                                    "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                    "follow.",
                                                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }
                                    if (SpeedNum == 1) {
                                        SpeedRatio = 0.0;
                                        CycRatio = PartLoadFrac;
                                    }
                                }
                            }
                        } // End if humidity ratio setpoint not met - CoolReheat humidity control
                        if (PartLoadFrac > 1.0) {
                            PartLoadFrac = 1.0;
                        } else if (PartLoadFrac < 0.0) {
                            PartLoadFrac = 0.0;
                        }
                        // variable-speed air-to-air cooling coil, end -------------------------

                    } else if (SELECT_CASE_var == CoilDX_PackagedThermalStorageCooling) {

                        ControlTESIceStorageTankCoil(state,
                                                     CompName,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilIndex,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingSystemType,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FanOpMode,
                                                     DesOutTemp,
                                                     DesOutHumRat,
                                                     PartLoadFrac,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).TESOpMode,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DehumidControlType,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIter,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRIterIndex,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFail,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilSensPLRFailIndex,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIter,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRIterIndex,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFail,
                                                     state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoilLatPLRFailIndex);
                    } else {
                        ShowFatalError(state,
                                       "ControlDXSystem: Invalid state.dataHVACDXSys->DXCoolingSystem coil type = " +
                                           state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CoolingCoilType);
                    }
                }
            } // End of cooling load type (sensible or latent) if block
        }     // End of If state.dataHVACDXSys->DXCoolingSystem is scheduled on and there is flow

        // Set the final results
        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).PartLoadFrac = PartLoadFrac;
        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedRatio = SpeedRatio;
        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).CycRatio = CycRatio;
        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DehumidificationMode = DehumidMode;
        state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).SpeedNum = SpeedNum;
    }

    Real64 DXCoilVarSpeedResidual(EnergyPlusData &state,
                                  Real64 const SpeedRatio,   // compressor speed ratio (1.0 is max, 0.0 is min)
                                  Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp).
        // DX Coil output depends on the compressor speed which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcMultiSpeedDXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]

        CoilIndex = int(Par(1));
        CalcMultiSpeedDXCoil(state, CoilIndex, SpeedRatio, 1.0);
        OutletAirTemp = state.dataDXCoils->DXCoilOutletTemp(CoilIndex);
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 DXCoilVarSpeedHumRatResidual(EnergyPlusData &state,
                                        Real64 const SpeedRatio,   // compressor speed ratio (1.0 is max, 0.0 is min)
                                        Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   January 2008
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat).
        // DX Coil output depends on the compressor speed which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet humidity ratio at the given compressor speed
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcMultiSpeedDXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet humidity ratio [kg/kg]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

        CoilIndex = int(Par(1));
        CalcMultiSpeedDXCoil(state, CoilIndex, SpeedRatio, 1.0);
        OutletAirHumRat = state.dataDXCoils->DXCoilOutletHumRat(CoilIndex);
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    Real64 DXCoilCyclingResidual(EnergyPlusData &state,
                                 Real64 const CycRatio,     // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                 Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the cycling ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcMultiSpeedDXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]

        CoilIndex = int(Par(1));
        CalcMultiSpeedDXCoil(state, CoilIndex, 0.0, CycRatio);
        OutletAirTemp = state.dataDXCoils->DXCoilOutletTemp(CoilIndex);
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 DXCoilCyclingHumRatResidual(EnergyPlusData &state,
                                       Real64 const CycRatio,     // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                       Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the cycling ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcMultiSpeedDXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet humidity ratio [kg/kg]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

        CoilIndex = int(Par(1));
        CalcMultiSpeedDXCoil(state, CoilIndex, 0.0, CycRatio);
        OutletAirHumRat = state.dataDXCoils->DXCoilOutletHumRat(CoilIndex);
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    Real64 DOE2DXCoilResidual(EnergyPlusData &state,
                              Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                              Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   November 2003
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcDoe2DXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int FanOpMode;        // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        CalcDoe2DXCoil(state, CoilIndex, On, true, PartLoadRatio, FanOpMode);
        OutletAirTemp = state.dataDXCoils->DXCoilOutletTemp(CoilIndex);
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 DOE2DXCoilHumRatResidual(EnergyPlusData &state,
                                    Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                    Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcDoe2DXCoil to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::CalcDoe2DXCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
        int FanOpMode;          // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        CalcDoe2DXCoil(state, CoilIndex, On, true, PartLoadRatio, FanOpMode);
        OutletAirHumRat = state.dataDXCoils->DXCoilOutletHumRat(CoilIndex);
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    Real64 MultiModeDXCoilResidual(EnergyPlusData &state,
                                   Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                   Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         M. J. Witte, GARD Analytics, Inc.
        //       DATE WRITTEN   February 2005
        //                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::SimDXCoilMultiMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(3) = dehumidification mode (0=normal, 1=enhanced)
        // par(4) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int DehumidMode;      // dehumidification mode (par3)
        int FanOpMode;        // supply air fan operating mode

        CoilIndex = int(Par(1));
        DehumidMode = int(Par(3));
        FanOpMode = int(Par(4));
        SimDXCoilMultiMode(state, "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode);
        OutletAirTemp = state.dataDXCoils->DXCoilOutletTemp(CoilIndex);
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 MultiModeDXCoilHumRatResidual(EnergyPlusData &state,
                                         Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                         Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using DXCoils::SimDXCoilMultiMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(3) = dehumidification mode (0=normal, 1=enhanced)
        // par(4) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
        int DehumidMode;        // dehumidification mode (par3)
        int FanOpMode;          // supply air fan operating mode

        CoilIndex = int(Par(1));
        DehumidMode = int(Par(3));
        FanOpMode = int(Par(4));
        SimDXCoilMultiMode(state, "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode);
        OutletAirHumRat = state.dataDXCoils->DXCoilOutletHumRat(CoilIndex);
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    Real64 HXAssistedCoolCoilTempResidual(EnergyPlusData &state,
                                          Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                          Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   November 2003
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired outlet temp - actual outlet temp)
        //  DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
        //  and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
        // par(4) = HX control (On/Off)
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;           // index of this coil
        Real64 OutletAirTemp;    // outlet air temperature [C]
        bool FirstHVACIteration; // FirstHVACIteration flag
        bool HXUnitOn;           // flag to enable heat exchanger heat recovery
        int FanOpMode;           // Supply air fan operating mode

        CoilIndex = int(Par(1));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        HXUnitOn = (Par(4) == 1.0);
        FanOpMode = int(Par(5));
        CalcHXAssistedCoolingCoil(state, CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode);
        OutletAirTemp = state.dataHVACAssistedCC->HXAssistedCoilOutletTemp(CoilIndex);
        Residuum = Par(2) - OutletAirTemp;
        return Residuum;
    }

    Real64 HXAssistedCoolCoilHRResidual(EnergyPlusData &state,
                                        Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                        Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired outlet humrat - actual outlet humrat)
        //  DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
        //  and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
        // par(4) = HX control (On/Off)
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;           // index of this coil
        Real64 OutletAirHumRat;  // outlet air humidity ratio [kg/kg]
        bool FirstHVACIteration; // FirstHVACIteration flag
        bool HXUnitOn;           // flag to enable heat exchanger heat recovery
        int FanOpMode;           // Supply air fan operating mode

        CoilIndex = int(Par(1));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        HXUnitOn = (Par(4) == 1.0);
        FanOpMode = int(Par(5));
        CalcHXAssistedCoolingCoil(
            state, CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode, _, state.dataHVACDXSys->EconomizerFlag);
        OutletAirHumRat = state.dataHVACAssistedCC->HXAssistedCoilOutletHumRat(CoilIndex);
        Residuum = Par(2) - OutletAirHumRat;
        return Residuum;
    }

    Real64 TESCoilResidual(EnergyPlusData &state,
                           Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                           Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // TES Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls appropriate calculation routine depending on operating mode
        // to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
        using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
        using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
        using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(3) = TES coil operating mode
        // par(4) = outlet node number
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int FanOpMode;        // Supply air fan operating mode
        int TESOpMode;
        int OutletNodeNum;

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        OutletNodeNum = int(Par(4));
        TESOpMode = int(Par(3));

        {
            auto const SELECT_CASE_var(static_cast<TESMode>(TESOpMode));
            if (SELECT_CASE_var == TESMode::CoolingOnlyMode) {
                CalcTESCoilCoolingOnlyMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::CoolingAndChargeMode) {
                CalcTESCoilCoolingAndChargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::CoolingAndDischargeMode) {
                CalcTESCoilCoolingAndDischargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::DischargeOnlyMode) {
                CalcTESCoilDischargeOnlyMode(state, CoilIndex, PartLoadRatio);
            }
        }

        OutletAirTemp = state.dataLoopNodes->Node(OutletNodeNum).Temp;
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 TESCoilHumRatResidual(EnergyPlusData &state,
                                 Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                 Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // TES Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls appropriate calculation routine depending on operating mode
        // to get outlet hum rat at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
        using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
        using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
        using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet hum rat [kgWater/kgDryAir]
        // par(3) = TES coil operating mode
        // par(4) = outlet node number
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kgWater/kgDryAir]
        int FanOpMode;          // Supply air fan operating mode
        int TESOpMode;
        int OutletNodeNum;

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        OutletNodeNum = int(Par(4));
        TESOpMode = int(Par(3));

        {
            auto const SELECT_CASE_var(static_cast<TESMode>(TESOpMode));
            if (SELECT_CASE_var == TESMode::CoolingOnlyMode) {
                CalcTESCoilCoolingOnlyMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::CoolingAndChargeMode) {
                CalcTESCoilCoolingAndChargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::CoolingAndDischargeMode) {
                CalcTESCoilCoolingAndDischargeMode(state, CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == TESMode::DischargeOnlyMode) {
                CalcTESCoilDischargeOnlyMode(state, CoilIndex, PartLoadRatio);
            }
        }

        OutletAirHumRat = state.dataLoopNodes->Node(OutletNodeNum).HumRat;
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    void FrostControlSetPointLimit(EnergyPlusData &state,
                                   int const DXSystemNum,      // dx cooling coil system index
                                   Real64 &TempSetPoint,       // temperature setpoint of the sensor node
                                   Real64 &HumRatSetPoint,     // humidity ratio setpoint of the sensor node
                                   Real64 const BaroPress,     // baromtric pressure, Pa [N/m^2]
                                   Real64 const TfrostControl, // minimum temperature limit for forst control
                                   int const ControlMode       // temperature or humidity control mode
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC
        //       DATE WRITTEN   January 2013
        //       MODIFIED
        //       RE-ENGINEERED
        // PURPOSE OF THIS SUBROUTINE:
        // Controls the forst formation condition based on user specified minimum DX coil outlet
        // air temperature. Resets the cooling setpoint based on the user specified limiting
        // temperature for frost control.
        // METHODOLOGY EMPLOYED:
        //  na
        // REFERENCES:
        //  na
        // Using/Aliasing
        using Psychrometrics::PsyWFnTdpPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // SUBROUTINE PARAMETER DEFINITIONS:
        int const RunOnSensible(1); // identifier for temperature (sensible load) control
        int const RunOnLatent(2);   // identifier for humidity (latent load) control
        static constexpr std::string_view RoutineName("FrostControlSetPointLimit");

        // INTERFACE BLOCK SPECIFICATIONS
        // na
        // DERIVED TYPE DEFINITIONS
        // na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HumRatioSat; // saturation humidity ratio at forst control temperature
        Real64 AirMassFlow; // air masss flow rate through the DX coil

        AirMassFlow = state.dataLoopNodes->Node(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilInletNodeNum).MassFlowRate;
        if (ControlMode == RunOnSensible && AirMassFlow > MinAirMassFlow &&
            TempSetPoint < state.dataLoopNodes->Node(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilInletNodeNum).Temp) {
            if (TempSetPoint < TfrostControl) {
                TempSetPoint = TfrostControl;
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FrostControlStatus = 1;
            }
        } else if (ControlMode == RunOnLatent && AirMassFlow > MinAirMassFlow &&
                   HumRatSetPoint < state.dataLoopNodes->Node(state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).DXCoolingCoilInletNodeNum).HumRat) {
            HumRatioSat = PsyWFnTdpPb(state, TfrostControl, BaroPress, RoutineName);
            if (HumRatioSat > HumRatSetPoint) {
                HumRatSetPoint = HumRatioSat;
                state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FrostControlStatus = 2;
            }
        } else {
            state.dataHVACDXSys->DXCoolingSystem(DXSystemNum).FrostControlStatus = 0;
        }
    }

    void CheckDXCoolingCoilInOASysExists(EnergyPlusData &state, std::string const &DXCoilSysName)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   Feb 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // After making sure get input is done, checks if the Coil System DX coil is in the
        // OA System.  If exists then the DX cooling coil is 100% DOAS DX coil.

        // Using/Aliasing
        using DXCoils::SetDXCoilTypeData;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoolSysNum;

        if (state.dataHVACDXSys->GetInputFlag) { // First time subroutine has been entered
            GetDXCoolingSystemInput(state);
            state.dataHVACDXSys->GetInputFlag = false;
        }

        DXCoolSysNum = 0;
        if (state.dataHVACDXSys->NumDXSystem > 0) {
            DXCoolSysNum = UtilityRoutines::FindItemInList(DXCoilSysName, state.dataHVACDXSys->DXCoolingSystem);
            if (DXCoolSysNum > 0 && state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).ISHundredPercentDOASDXCoil) {
                if (!(state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed)) {
                    SetDXCoilTypeData(state, state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName);
                }
            }
        }
    }

    void GetCoolingCoilTypeNameAndIndex(EnergyPlusData &state,
                                        std::string const &DXCoilSysName,
                                        int &CoolCoilType,
                                        int &CoolCoilIndex,
                                        std::string &CoolCoilName,
                                        [[maybe_unused]] bool &ErrFound)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Aug 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // After making sure get input is done, checks if the Coil System DX coil is in the
        // OA System.  If exists then the DX cooling coil is 100% DOAS DX coil.

        // Using/Aliasing
        using DXCoils::SetDXCoilTypeData;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DXCoolSysNum;

        if (state.dataHVACDXSys->GetInputFlag) { // First time subroutine has been entered
            GetDXCoolingSystemInput(state);
            state.dataHVACDXSys->GetInputFlag = false;
        }

        DXCoolSysNum = 0;
        if (state.dataHVACDXSys->NumDXSystem > 0) {
            DXCoolSysNum = UtilityRoutines::FindItemInList(DXCoilSysName, state.dataHVACDXSys->DXCoolingSystem);
            if (DXCoolSysNum > 0 && DXCoolSysNum <= state.dataHVACDXSys->NumDXSystem) {
                CoolCoilType = state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilType_Num;
                CoolCoilIndex = state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilIndex;
                CoolCoilName = state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).CoolingCoilName;
            }
        }
    }

    //******************************************************************************

    Real64 VSCoilCyclingResidual(EnergyPlusData &state,
                                 Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                 Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   Feb, 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (Temperature) by comparing with the output of variable-speed DX coil
        // interate part-load ratio

        // REFERENCES:

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int FanOpMode;        // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));

        SimVariableSpeedCoils(state,
                              "",
                              CoilIndex,
                              FanOpMode,
                              state.dataHVACDXSys->MaxONOFFCyclesperHourCycling,
                              state.dataHVACDXSys->HPTimeConstantCycling,
                              state.dataHVACDXSys->FanDelayTimeCycling,
                              On,
                              PartLoadRatio,
                              state.dataHVACDXSys->SpeedNum,
                              state.dataHVACDXSys->SpeedRatio,
                              state.dataHVACDXSys->QZnReqCycling,
                              state.dataHVACDXSys->QLatReqCycling,
                              state.dataHVACDXSys->OnOffAirFlowRatioCycling);

        OutletAirTemp = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).OutletAirDBTemp;
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    //******************************************************************************

    Real64 VSCoilSpeedResidual(EnergyPlusData &state,
                               Real64 const SpeedRatio,   // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                               Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   Feb, 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (Temperature) by comparing with the output of variable-speed DX coil
        // interate speed ratio between two neighboring speeds
        // REFERENCES:

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int FanOpMode;        // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        state.dataHVACDXSys->mySpeedNum = int(Par(3));

        SimVariableSpeedCoils(state,
                              "",
                              CoilIndex,
                              FanOpMode,
                              state.dataHVACDXSys->myMaxONOFFCyclesperHour,
                              state.dataHVACDXSys->myHPTimeConstant,
                              state.dataHVACDXSys->myFanDelayTime,
                              On,
                              state.dataHVACDXSys->PartLoadRatio,
                              state.dataHVACDXSys->mySpeedNum,
                              SpeedRatio,
                              state.dataHVACDXSys->myQZnReq,
                              state.dataHVACDXSys->myQLatReq,
                              state.dataHVACDXSys->myOnOffAirFlowRatio);

        OutletAirTemp = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).OutletAirDBTemp;
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 VSCoilCyclingHumResidual(EnergyPlusData &state,
                                    Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                    Array1D<Real64> const &Par  // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   Feb, 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (Humidity) by comparing with the output of variable-speed DX coil
        // interate part-load ratio
        // REFERENCES:

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
        int FanOpMode;          // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));

        SimVariableSpeedCoils(state,
                              "",
                              CoilIndex,
                              FanOpMode,
                              state.dataHVACDXSys->MaxONOFFCyclesperHourCyclingHum,
                              state.dataHVACDXSys->HPTimeConstantCyclingHum,
                              state.dataHVACDXSys->FanDelayTimeCyclingHum,
                              On,
                              PartLoadRatio,
                              state.dataHVACDXSys->SpeedNumCyclingHum,
                              state.dataHVACDXSys->SpeedRatioCyclingHum,
                              state.dataHVACDXSys->QZnReqCyclingHum,
                              state.dataHVACDXSys->QLatReqCyclingHum,
                              state.dataHVACDXSys->OnOffAirFlowRatioCyclingHum);

        OutletAirHumRat = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).OutletAirHumRat;
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    //******************************************************************************

    Real64 VSCoilSpeedHumResidual(EnergyPlusData &state,
                                  Real64 const SpeedRatio,   // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                  Array1D<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   Feb, 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (Humidity) by comparing with the output of variable-speed DX coil
        // interate speed ratio between two neighboring speeds

        // REFERENCES:

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
        int FanOpMode;          // Supply air fan operating mode

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        state.dataHVACDXSys->SpeedNumSpeedHum = int(Par(3));

        SimVariableSpeedCoils(state,
                              "",
                              CoilIndex,
                              FanOpMode,
                              state.dataHVACDXSys->MaxONOFFCyclesperHourSpeedHum,
                              state.dataHVACDXSys->HPTimeConstantSpeedHum,
                              state.dataHVACDXSys->FanDelayTimeSpeedHum,
                              On,
                              state.dataHVACDXSys->PartLoadRatioSpeedHum,
                              state.dataHVACDXSys->SpeedNumSpeedHum,
                              SpeedRatio,
                              state.dataHVACDXSys->QZnReqSpeedHum,
                              state.dataHVACDXSys->QLatReqSpeedHum,
                              state.dataHVACDXSys->OnOffAirFlowRatioSpeedHum);

        OutletAirHumRat = state.dataVariableSpeedCoils->VarSpeedCoil(CoilIndex).OutletAirHumRat;
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    int GetCoolingCoilInletNodeNum(EnergyPlusData &state, std::string const &DXCoilSysName, bool &InletNodeErrFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   Apr. 2019
        // PURPOSE OF THIS SUBROUTINE:
        // Get inlet node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NodeNum;
        int DXCoolSysNum;

        if (state.dataHVACDXSys->GetInputFlag) { // First time subroutine has been entered
            GetDXCoolingSystemInput(state);
            state.dataHVACDXSys->GetInputFlag = false;
        }

        NodeNum = 0;
        if (state.dataHVACDXSys->NumDXSystem > 0) {
            DXCoolSysNum = UtilityRoutines::FindItemInList(DXCoilSysName, state.dataHVACDXSys->DXCoolingSystem);
            if (DXCoolSysNum > 0 && DXCoolSysNum <= state.dataHVACDXSys->NumDXSystem) {
                NodeNum = state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingCoilInletNodeNum;
            }
        }
        if (NodeNum == 0) InletNodeErrFlag = true;

        return NodeNum;
    }

    int GetCoolingCoilOutletNodeNum(EnergyPlusData &state, std::string const &DXCoilSysName, bool &OutletNodeErrFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   Apr. 2019
        // PURPOSE OF THIS SUBROUTINE:
        // Get Outlet node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NodeNum;
        int DXCoolSysNum;

        if (state.dataHVACDXSys->GetInputFlag) { // First time subroutine has been entered
            GetDXCoolingSystemInput(state);
            state.dataHVACDXSys->GetInputFlag = false;
        }

        NodeNum = 0;
        if (state.dataHVACDXSys->NumDXSystem > 0) {
            DXCoolSysNum = UtilityRoutines::FindItemInList(DXCoilSysName, state.dataHVACDXSys->DXCoolingSystem);
            if (DXCoolSysNum > 0 && DXCoolSysNum <= state.dataHVACDXSys->NumDXSystem) {
                NodeNum = state.dataHVACDXSys->DXCoolingSystem(DXCoolSysNum).DXCoolingCoilOutletNodeNum;
            }
        }
        if (NodeNum == 0) OutletNodeErrFlag = true;

        return NodeNum;
    }
    //        End of Calculation subroutines for the state.dataHVACDXSys->DXCoolingSystem Module
    // *****************************************************************************

} // namespace HVACDXSystem

} // namespace EnergyPlus
