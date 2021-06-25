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
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {
namespace BaseboardRadiator {

    // Module containing the routines dealing with the BASEBOARD HEATER
    // component(s).

    // MODULE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Jan 1998
    //       MODIFIED       Fred Buhl, October 1999
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataPlant::TypeOf_Baseboard_Conv_Water;

    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    static std::string const cCMO_BBRadiator_Water("ZoneHVAC:Baseboard:Convective:Water");

    void SimBaseboard(EnergyPlusData &state,
                      std::string const &EquipName,
                      int const ActualZoneNum,
                      int const ControlledZoneNum,
                      bool const FirstHVACIteration,
                      Real64 &PowerMet,
                      int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Baseboard Radiators.

        // Using/Aliasing
        using PlantUtilities::SetActuatedBranchFlowRate;

        int BaseboardNum; // index of unit in baseboard array
        Real64 QZnReq;    // zone load not yet satisfied
        Real64 MaxWaterFlow;
        Real64 MinWaterFlow;
        Real64 DummyMdot;

        auto &baseboard = state.dataBaseboardRadiator;

        if (baseboard->getInputFlag) {
            GetBaseboardInput(state);
            baseboard->getInputFlag = false;
        }

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(EquipName, baseboard->Baseboard, &BaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "SimBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > baseboard->NumBaseboards || BaseboardNum < 1) {
                ShowFatalError(state,
                               format("SimBaseboard:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      BaseboardNum,
                                      baseboard->NumBaseboards,
                                      EquipName));
            }
            if (baseboard->Baseboard(BaseboardNum).CheckEquipName) {
                if (EquipName != baseboard->Baseboard(BaseboardNum).EquipID) {
                    ShowFatalError(state,
                                   format("SimBaseboard: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          BaseboardNum,
                                          EquipName,
                                          baseboard->Baseboard(BaseboardNum).EquipID));
                }
                baseboard->Baseboard(BaseboardNum).CheckEquipName = false;
            }
        }

        InitBaseboard(state, BaseboardNum, ControlledZoneNum);

        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;

        if ((QZnReq < SmallLoad) || (baseboard->Baseboard(BaseboardNum).WaterInletTemp <= baseboard->Baseboard(BaseboardNum).AirInletTemp)) {
            //  IF (Baseboard(BaseboardNum)%WaterInletTemp <= Baseboard(BaseboardNum)%AirInletTemp) THEN
            // The baseboard cannot provide cooling.  Thus, if the zone required load is negative or the water inlet
            // temperature is lower than the zone air temperature, then we need to shut down the baseboard unit

            baseboard->Baseboard(BaseboardNum).WaterOutletTemp = baseboard->Baseboard(BaseboardNum).WaterInletTemp;
            baseboard->Baseboard(BaseboardNum).AirOutletTemp = baseboard->Baseboard(BaseboardNum).AirInletTemp;
            baseboard->Baseboard(BaseboardNum).Power = 0.0;
            baseboard->Baseboard(BaseboardNum).WaterMassFlowRate = 0.0;
            // init hot water flow rate to zero
            DummyMdot = 0.0;
            SetActuatedBranchFlowRate(state,
                                      DummyMdot,
                                      baseboard->Baseboard(BaseboardNum).WaterInletNode,
                                      baseboard->Baseboard(BaseboardNum).LoopNum,
                                      baseboard->Baseboard(BaseboardNum).LoopSideNum,
                                      baseboard->Baseboard(BaseboardNum).BranchNum,
                                      false);

        } else {
            // init hot water flow rate to zero
            DummyMdot = 0.0;
            SetActuatedBranchFlowRate(state,
                                      DummyMdot,
                                      baseboard->Baseboard(BaseboardNum).WaterInletNode,
                                      baseboard->Baseboard(BaseboardNum).LoopNum,
                                      baseboard->Baseboard(BaseboardNum).LoopSideNum,
                                      baseboard->Baseboard(BaseboardNum).BranchNum,
                                      true);

            // On the first HVAC iteration the system values are given to the controller, but after that
            // the demand limits are in place and there needs to be feedback to the Zone Equipment
            if (FirstHVACIteration) {
                MaxWaterFlow = baseboard->Baseboard(BaseboardNum).WaterMassFlowRateMax;
                MinWaterFlow = 0.0;
            } else {
                MaxWaterFlow = state.dataLoopNodes->Node(baseboard->Baseboard(BaseboardNum).WaterInletNode).MassFlowRateMaxAvail;
                MinWaterFlow = state.dataLoopNodes->Node(baseboard->Baseboard(BaseboardNum).WaterInletNode).MassFlowRateMinAvail;
            }

            ControlCompOutput(state,
                              baseboard->Baseboard(BaseboardNum).EquipID,
                              cCMO_BBRadiator_Water,
                              BaseboardNum,
                              FirstHVACIteration,
                              QZnReq,
                              baseboard->Baseboard(BaseboardNum).WaterInletNode,
                              MaxWaterFlow,
                              MinWaterFlow,
                              baseboard->Baseboard(BaseboardNum).Offset,
                              baseboard->Baseboard(BaseboardNum).ControlCompTypeNum,
                              baseboard->Baseboard(BaseboardNum).CompErrIndex,
                              _,
                              _,
                              _,
                              _,
                              _,
                              baseboard->Baseboard(BaseboardNum).LoopNum,
                              baseboard->Baseboard(BaseboardNum).LoopSideNum,
                              baseboard->Baseboard(BaseboardNum).BranchNum);

            PowerMet = baseboard->Baseboard(BaseboardNum).Power;
        }

        UpdateBaseboard(state, BaseboardNum);
        baseboard->Baseboard(BaseboardNum).Energy =
            baseboard->Baseboard(BaseboardNum).Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    }

    void GetBaseboardInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the Baseboard units.

        // METHODOLOGY EMPLOYED:
        // Standard input processor calls.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;
        using GlobalNames::VerifyUniqueBaseboardName;
        using namespace DataSizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBaseboardInput: "); // include trailing blank space
        int const iHeatCAPMAlphaNum(5);             // get input index to water baseboard Radiator system heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1); // get input index to water baseboard Radiator system electric heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(
            2); // get input index to water baseboard Radiator system electric heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); //  get input index to water baseboard Radiator system electric heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum;
        int NumConvHWBaseboards;
        int ConvHWBaseboardNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input

        auto &baseboard = state.dataBaseboardRadiator;
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        cCurrentModuleObject = cCMO_BBRadiator_Water;

        NumConvHWBaseboards = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // Calculate total number of baseboard units
        baseboard->NumBaseboards = NumConvHWBaseboards;

        baseboard->Baseboard.allocate(baseboard->NumBaseboards);
        baseboard->BaseboardParamsNumericFields.allocate(baseboard->NumBaseboards);

        if (NumConvHWBaseboards > 0) { // Get the data for cooling schemes
            BaseboardNum = 0;
            for (ConvHWBaseboardNum = 1; ConvHWBaseboardNum <= NumConvHWBaseboards; ++ConvHWBaseboardNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         ConvHWBaseboardNum,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNums,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                baseboard->BaseboardParamsNumericFields(ConvHWBaseboardNum).FieldNames.allocate(NumNums);
                baseboard->BaseboardParamsNumericFields(ConvHWBaseboardNum).FieldNames = "";
                baseboard->BaseboardParamsNumericFields(ConvHWBaseboardNum).FieldNames = state.dataIPShortCut->cNumericFieldNames;

                if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                    continue;
                }

                // ErrorsFound will be set to True if problem was found, left untouched otherwise
                VerifyUniqueBaseboardName(
                    state, cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

                ++BaseboardNum;
                baseboard->Baseboard(BaseboardNum).EquipID = state.dataIPShortCut->cAlphaArgs(1); // name of this baseboard
                baseboard->Baseboard(BaseboardNum).EquipType = TypeOf_Baseboard_Conv_Water;
                baseboard->Baseboard(BaseboardNum).Schedule = state.dataIPShortCut->cAlphaArgs(2);
                if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                    baseboard->Baseboard(BaseboardNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    baseboard->Baseboard(BaseboardNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                    if (baseboard->Baseboard(BaseboardNum).SchedPtr == 0) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                            " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                            '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }
                // get inlet node number
                baseboard->Baseboard(BaseboardNum).WaterInletNode = GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(3),
                                                                                      ErrorsFound,
                                                                                      cCurrentModuleObject,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Water,
                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                      NodeInputManager::compFluidStream::Primary,
                                                                                      ObjectIsNotParent);
                // get outlet node number
                baseboard->Baseboard(BaseboardNum).WaterOutletNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(4),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);

                TestCompSet(state,
                            cCMO_BBRadiator_Water,
                            state.dataIPShortCut->cAlphaArgs(1),
                            state.dataIPShortCut->cAlphaArgs(3),
                            state.dataIPShortCut->cAlphaArgs(4),
                            "Hot Water Nodes");

                // Determine steam baseboard radiator system heating design capacity sizing method
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                    baseboard->Baseboard(BaseboardNum).HeatingCapMethod = HeatingDesignCapacity;
                    if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                        baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum);
                        if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity < 0.0 &&
                            baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity != AutoSize) {
                            ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                            ShowContinueError(state,
                                              format("Illegal {} = {:.7T}",
                                                     state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                                     state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum)));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                    baseboard->Baseboard(BaseboardNum).HeatingCapMethod = CapacityPerFloorArea;
                    if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                        baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity =
                            state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                        if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity <= 0.0) {
                            ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                            ShowContinueError(state,
                                              "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                                  state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                            ShowContinueError(state,
                                              format("Illegal {} = {:.7T}",
                                                     state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                                     state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                            ErrorsFound = true;
                        } else if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                            ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                            ShowContinueError(state,
                                              "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                                  state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                            ShowContinueError(
                                state, "Illegal " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(
                            state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                    baseboard->Baseboard(BaseboardNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                    if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                        baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity =
                            state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                        if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity < 0.0) {
                            ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                            ShowContinueError(state,
                                              format("Illegal {} = {:.7T}",
                                                     state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                                     state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(
                            state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCMO_BBRadiator_Water + " = " + baseboard->Baseboard(BaseboardNum).EquipID);
                    ShowContinueError(state,
                                      "Illegal " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ErrorsFound = true;
                }

                baseboard->Baseboard(BaseboardNum).UA = state.dataIPShortCut->rNumericArgs(4);
                baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax = state.dataIPShortCut->rNumericArgs(5);
                baseboard->Baseboard(BaseboardNum).Offset = state.dataIPShortCut->rNumericArgs(6);
                // Set default convergence tolerance
                if (baseboard->Baseboard(BaseboardNum).Offset <= 0.0) {
                    baseboard->Baseboard(BaseboardNum).Offset = 0.001;
                }
            }

            if (ErrorsFound) {
                ShowFatalError(state, RoutineName + "Errors found in getting input.  Preceding condition(s) cause termination.");
            }
        }

        for (BaseboardNum = 1; BaseboardNum <= baseboard->NumBaseboards; ++BaseboardNum) {

            // Setup Report variables for the unit
            // CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Water'
            SetupOutputVariable(state,
                                "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                baseboard->Baseboard(BaseboardNum).Energy,
                                "System",
                                "Sum",
                                baseboard->Baseboard(BaseboardNum).EquipID,
                                _,
                                "ENERGYTRANSFER",
                                "BASEBOARD",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Baseboard Hot Water Energy",
                                OutputProcessor::Unit::J,
                                baseboard->Baseboard(BaseboardNum).Energy,
                                "System",
                                "Sum",
                                baseboard->Baseboard(BaseboardNum).EquipID,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "BASEBOARD",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                baseboard->Baseboard(BaseboardNum).Power,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Hot Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                baseboard->Baseboard(BaseboardNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                baseboard->Baseboard(BaseboardNum).AirMassFlowRate,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Air Inlet Temperature",
                                OutputProcessor::Unit::C,
                                baseboard->Baseboard(BaseboardNum).AirInletTemp,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Air Outlet Temperature",
                                OutputProcessor::Unit::C,
                                baseboard->Baseboard(BaseboardNum).AirOutletTemp,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Water Inlet Temperature",
                                OutputProcessor::Unit::C,
                                baseboard->Baseboard(BaseboardNum).WaterInletTemp,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Water Outlet Temperature",
                                OutputProcessor::Unit::C,
                                baseboard->Baseboard(BaseboardNum).WaterOutletTemp,
                                "System",
                                "Average",
                                baseboard->Baseboard(BaseboardNum).EquipID);
        }
    }

    void InitBaseboard(EnergyPlusData &state, int const BaseboardNum, int const ControlledZoneNumSub)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the Baseboard units during simulation.

        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("BaseboardRadiator:InitBaseboard");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WaterInletNode;
        int ZoneNode;
        int Loop;
        Real64 rho; // local fluid density
        Real64 Cp;  // local fluid specific heat
        bool errFlag;

        auto &baseboard = state.dataBaseboardRadiator;

        if (baseboard->Baseboard(BaseboardNum).ZonePtr <= 0)
            baseboard->Baseboard(BaseboardNum).ZonePtr = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

        if (baseboard->Baseboard(BaseboardNum).SetLoopIndexFlag && allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    baseboard->Baseboard(BaseboardNum).EquipID,
                                    baseboard->Baseboard(BaseboardNum).EquipType,
                                    baseboard->Baseboard(BaseboardNum).LoopNum,
                                    baseboard->Baseboard(BaseboardNum).LoopSideNum,
                                    baseboard->Baseboard(BaseboardNum).BranchNum,
                                    baseboard->Baseboard(BaseboardNum).CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitBaseboard: Program terminated for previous conditions.");
            }
            baseboard->Baseboard(BaseboardNum).SetLoopIndexFlag = false;
        }
        // need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!baseboard->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            baseboard->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= baseboard->NumBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(state, cCMO_BBRadiator_Water, baseboard->Baseboard(Loop).EquipID)) continue;
                ShowSevereError(state,
                                "InitBaseboard: Unit=[" + cCMO_BBRadiator_Water + ',' + baseboard->Baseboard(Loop).EquipID +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && baseboard->Baseboard(BaseboardNum).MySizeFlag &&
            !baseboard->Baseboard(BaseboardNum).SetLoopIndexFlag) {
            // for each coil, do the sizing once.
            SizeBaseboard(state, BaseboardNum);

            baseboard->Baseboard(BaseboardNum).MySizeFlag = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && baseboard->Baseboard(BaseboardNum).MyEnvrnFlag &&
            !baseboard->Baseboard(BaseboardNum).SetLoopIndexFlag) {
            WaterInletNode = baseboard->Baseboard(BaseboardNum).WaterInletNode;
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                   DataGlobalConstants::HWInitConvTemp,
                                   state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                   RoutineName);
            baseboard->Baseboard(BaseboardNum).WaterMassFlowRateMax = rho * baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax;
            InitComponentNodes(state,
                               0.0,
                               baseboard->Baseboard(BaseboardNum).WaterMassFlowRateMax,
                               baseboard->Baseboard(BaseboardNum).WaterInletNode,
                               baseboard->Baseboard(BaseboardNum).WaterOutletNode,
                               baseboard->Baseboard(BaseboardNum).LoopNum,
                               baseboard->Baseboard(BaseboardNum).LoopSideNum,
                               baseboard->Baseboard(BaseboardNum).BranchNum,
                               baseboard->Baseboard(BaseboardNum).CompNum);
            state.dataLoopNodes->Node(WaterInletNode).Temp = DataGlobalConstants::HWInitConvTemp;
            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                       state.dataLoopNodes->Node(WaterInletNode).Temp,
                                       state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                       RoutineName);
            state.dataLoopNodes->Node(WaterInletNode).Enthalpy = Cp * state.dataLoopNodes->Node(WaterInletNode).Temp;
            state.dataLoopNodes->Node(WaterInletNode).Quality = 0.0;
            state.dataLoopNodes->Node(WaterInletNode).Press = 0.0;
            state.dataLoopNodes->Node(WaterInletNode).HumRat = 0.0;
            // pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
            if (baseboard->Baseboard(BaseboardNum).AirMassFlowRate <= 0.0) {
                baseboard->Baseboard(BaseboardNum).AirMassFlowRate = 2.0 * baseboard->Baseboard(BaseboardNum).WaterMassFlowRateMax;
            }
            baseboard->Baseboard(BaseboardNum).MyEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            baseboard->Baseboard(BaseboardNum).MyEnvrnFlag = true;
        }

        // Do the every time step initializations
        WaterInletNode = baseboard->Baseboard(BaseboardNum).WaterInletNode;
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
        baseboard->Baseboard(BaseboardNum).WaterMassFlowRate = state.dataLoopNodes->Node(WaterInletNode).MassFlowRate;
        baseboard->Baseboard(BaseboardNum).WaterInletTemp = state.dataLoopNodes->Node(WaterInletNode).Temp;
        baseboard->Baseboard(BaseboardNum).WaterInletEnthalpy = state.dataLoopNodes->Node(WaterInletNode).Enthalpy;
        baseboard->Baseboard(BaseboardNum).AirInletTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        baseboard->Baseboard(BaseboardNum).AirInletHumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
    }

    void SizeBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B.Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing hot water baseboard components for which flow rates and UAs have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
        // calculated by numerically inverting the baseboard calculation routine.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::HeatingCapacitySizing;

        using PlantUtilities::RegisterPlantCompDesignFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Acc(0.0001); // Accuracy of result
        int const MaxIte(500);    // Maximum number of iterations
        static std::string const RoutineName(cCMO_BBRadiator_Water + ":SizeBaseboard");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WaterInletNode;
        int PltSizHeatNum(0); // index of plant sizing object for 1st heating loop
        Real64 DesCoilLoad(0.0);
        int SolFla; // Flag of solver
        Real64 UA0; // lower bound for UA
        Real64 UA1; // upper bound for UA
        Real64 UA;
        Array1D<Real64> Par(2);
        bool ErrorsFound(false);             // If errors detected in input
        Real64 rho;                          // local fluid density
        Real64 Cp;                           // local fluid specific heat
        bool FlowAutoSize(false);            // Indicator to autosizing water volume flow
        bool UAAutoSize(false);              // Indicator to autosizing UA
        Real64 WaterVolFlowRateMaxDes(0.0);  // Design water volume flow for reproting
        Real64 WaterVolFlowRateMaxUser(0.0); // User hard-sized volume flow for reporting
        Real64 UADes(0.0);                   // Design UA value for reproting
        Real64 UAUser(0.0);                  // User hard-sized value for reporting
        std::string CompName;                // component name
        std::string CompType;                // component type
        std::string SizingString;            // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;                     // autosized value of coil input field
        int FieldNum = 1;                    // IDD numeric field number where input field description is found
        int SizingMethod;                    // Integer representation of sizing method name (HeatingCapacitySizing)
        bool PrintFlag;                      // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, and FractionOfAutosizedHeatingCapacity )

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);
        auto &FinalZoneSizing(state.dataSize->FinalZoneSizing);
        auto &DataScalableCapSizingON(state.dataSize->DataScalableCapSizingON);
        auto &baseboard = state.dataBaseboardRadiator;

        // find the appropriate heating Plant Sizing object
        PltSizHeatNum = state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).PlantSizNum;

        if (PltSizHeatNum > 0) {

            DataScalableCapSizingON = false;

            if (CurZoneEqNum > 0) {

                if (baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax == AutoSize) {
                    FlowAutoSize = true;
                }
                if (!FlowAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation should continue
                    if (baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     cCMO_BBRadiator_Water,
                                                     baseboard->Baseboard(BaseboardNum).EquipID,
                                                     "User-Specified Maximum Water Flow Rate [m3/s]",
                                                     baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax);
                    }
                } else {
                    CheckZoneSizing(state, cCMO_BBRadiator_Water, baseboard->Baseboard(BaseboardNum).EquipID);
                    CompType = cCMO_BBRadiator_Water;
                    CompName = baseboard->Baseboard(BaseboardNum).EquipID;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
                    state.dataSize->DataZoneNumber = baseboard->Baseboard(BaseboardNum).ZonePtr;
                    SizingMethod = HeatingCapacitySizing;
                    FieldNum = 1;
                    PrintFlag = false;
                    SizingString = baseboard->BaseboardParamsNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
                    CapSizingMethod = baseboard->Baseboard(BaseboardNum).HeatingCapMethod;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {

                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                                CheckZoneSizing(state, CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            } else {
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                            }
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity *
                                                                        state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                            DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(state, CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            state.dataSize->DataFracOfAutosizedHeatingCapacity = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = AutoSize;
                            DataScalableCapSizingON = true;
                        } else {
                            TempSize = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                        }
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                        DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0;
                    }

                    if (DesCoilLoad >= SmallLoad) {
                        Cp = GetSpecificHeatGlycol(state,
                                                   state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                                   RoutineName);
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                               DataGlobalConstants::HWInitConvTemp,
                                               state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                               RoutineName);
                        WaterVolFlowRateMaxDes = DesCoilLoad / (state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                    } else {
                        WaterVolFlowRateMaxDes = 0.0;
                    }

                    if (FlowAutoSize) {
                        baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax = WaterVolFlowRateMaxDes;
                        BaseSizer::reportSizerOutput(state,
                                                     cCMO_BBRadiator_Water,
                                                     baseboard->Baseboard(BaseboardNum).EquipID,
                                                     "Design Size Maximum Water Flow Rate [m3/s]",
                                                     WaterVolFlowRateMaxDes);
                    } else { // hard-sized with sizing data
                        if (baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax > 0.0 && WaterVolFlowRateMaxDes > 0.0) {
                            WaterVolFlowRateMaxUser = baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax;
                            BaseSizer::reportSizerOutput(state,
                                                         cCMO_BBRadiator_Water,
                                                         baseboard->Baseboard(BaseboardNum).EquipID,
                                                         "Design Size Maximum Water Flow Rate [m3/s]",
                                                         WaterVolFlowRateMaxDes,
                                                         "User-Specified Maximum Water Flow Rate [m3/s]",
                                                         WaterVolFlowRateMaxUser);
                            // Report a warning to note difference between the two
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser) / WaterVolFlowRateMaxUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water=\"" +
                                                    baseboard->Baseboard(BaseboardNum).EquipID + "\".");
                                    ShowContinueError(state,
                                                      format("User-Specified Maximum Water Flow Rate of {:.5R} [m3/s]", WaterVolFlowRateMaxUser));
                                    ShowContinueError(
                                        state, format("differs from Design Size Maximum Water Flow Rate of {:.5R} [m3/s]", WaterVolFlowRateMaxDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }

                // UA sizing
                // Set hard-sized values to the local variable to correct a false indication aftet SolFla function calculation
                if (baseboard->Baseboard(BaseboardNum).UA == AutoSize) {
                    UAAutoSize = true;
                } else {
                    UAUser = baseboard->Baseboard(BaseboardNum).UA;
                }
                if (!UAAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation should continue
                    if (baseboard->Baseboard(BaseboardNum).UA > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     cCMO_BBRadiator_Water,
                                                     baseboard->Baseboard(BaseboardNum).EquipID,
                                                     "User-Specified U-Factor Times Area Value [W/K]",
                                                     baseboard->Baseboard(BaseboardNum).UA);
                    }
                } else {
                    baseboard->Baseboard(BaseboardNum).WaterInletTemp = state.dataSize->PlantSizData(PltSizHeatNum).ExitTemp;
                    baseboard->Baseboard(BaseboardNum).AirInletTemp = FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak;
                    baseboard->Baseboard(BaseboardNum).AirInletHumRat = FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtHeatPeak;
                    WaterInletNode = baseboard->Baseboard(BaseboardNum).WaterInletNode;
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                           RoutineName);
                    state.dataLoopNodes->Node(WaterInletNode).MassFlowRate = rho * baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax;

                    CompType = cCMO_BBRadiator_Water;
                    CompName = baseboard->Baseboard(BaseboardNum).EquipID;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
                    state.dataSize->DataZoneNumber = baseboard->Baseboard(BaseboardNum).ZonePtr;
                    SizingMethod = HeatingCapacitySizing;
                    FieldNum = 1;
                    PrintFlag = false;
                    SizingString = baseboard->BaseboardParamsNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
                    CapSizingMethod = baseboard->Baseboard(BaseboardNum).HeatingCapMethod;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                                CheckZoneSizing(state, CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            } else {
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                            }
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity *
                                                                        state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                            DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(state, CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            state.dataSize->DataFracOfAutosizedHeatingCapacity = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = AutoSize;
                            DataScalableCapSizingON = true;
                        } else {
                            TempSize = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                        }
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                        DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0; // FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    }
                    if (DesCoilLoad >= SmallLoad) {
                        // pick an air  mass flow rate that is twice the water mass flow rate (CR8842)
                        baseboard->Baseboard(BaseboardNum).DesAirMassFlowRate = 2.0 * rho * baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax;
                        // pass along the coil number and the design load to the residual calculation
                        std::array<Real64, 2> Par2 = {DesCoilLoad, Real64(BaseboardNum)};
                        // set the lower and upper limits on the UA
                        UA0 = 0.001 * DesCoilLoad;
                        UA1 = DesCoilLoad;

                        // before iterating on a design UA check output at lower UA bound
                        baseboard->Baseboard(BaseboardNum).UA = UA0;
                        Real64 LoadMet = 0.0;
                        int BBIndex = BaseboardNum;
                        SimHWConvective(state, BBIndex, LoadMet);
                        if (LoadMet < DesCoilLoad) { // baseboard output should be below design load
                            // now check output at max UA (where UA = design load)
                            baseboard->Baseboard(BaseboardNum).UA = UA1;
                            SimHWConvective(state, BBIndex, LoadMet);

                            if (LoadMet > DesCoilLoad) { // if the load met is greater than design load, OK to iterate on UA
                                // Invert the baseboard model: given the design inlet conditions and the design load,
                                // find the design UA.
                                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, HWBaseboardUAResidual, UA0, UA1, Par2);
                                // if the numerical inversion failed, issue error messages.
                                if (SolFla == -1) {
                                    ShowSevereError(state,
                                                    "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" +
                                                        baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                    ShowContinueError(state, "Iteration limit exceeded in calculating coil UA");
                                    if (UAAutoSize) {
                                        ErrorsFound = true;
                                    } else {
                                        ShowContinueError(
                                            state, "Could not calculate design value for comparison to user value, and the simulation continues");
                                        UA = 0.0;
                                    }
                                } else if (SolFla == -2) {
                                    ShowSevereError(state,
                                                    "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" +
                                                        baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                    ShowContinueError(state, "Bad starting values for UA");
                                    if (UAAutoSize) {
                                        ErrorsFound = true;
                                    } else {
                                        ShowContinueError(
                                            state, "Could not calculate design value for comparison to user value, and the simulation continues");
                                        UA = 0.0;
                                    }
                                }
                                UADes = UA; // baseboard->Baseboard(BaseboardNum)%UA = UA
                            } else {        // baseboard design load is greater than output at UA = design load so set UA = design load
                                UADes = UA1;
                                if (UAAutoSize) {
                                    ShowWarningError(state,
                                                     "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" +
                                                         baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                    ShowContinueError(state,
                                                      "Design UA set equal to design coil load for " + cCMO_BBRadiator_Water + "=\"" +
                                                          baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                    ShowContinueError(state, format("Design coil load used during sizing = {:.5R} W.", DesCoilLoad));
                                    ShowContinueError(state,
                                                      format("Inlet water temperature used during sizing = {:.5R} C.",
                                                             baseboard->Baseboard(BaseboardNum).WaterInletTemp));
                                }
                            }
                        } else { // baseboard design load is less than output at UA = 0.001 * design load so set UA to minimum value
                            UADes = UA0;
                            if (UAAutoSize) {
                                ShowWarningError(state,
                                                 "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" +
                                                     baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                ShowContinueError(state,
                                                  "Design UA set equal to 0.001 * design coil load for " + cCMO_BBRadiator_Water + "=\"" +
                                                      baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                                ShowContinueError(state, format("Design coil load used during sizing = {:.5R} W.", DesCoilLoad));
                                ShowContinueError(state,
                                                  format("Inlet water temperature used during sizing = {:.5R} C.",
                                                         baseboard->Baseboard(BaseboardNum).WaterInletTemp));
                            }
                        }

                    } else {
                        UADes = 0.0;
                    }

                    if (UAAutoSize) {
                        baseboard->Baseboard(BaseboardNum).UA = UADes;
                        BaseSizer::reportSizerOutput(state,
                                                     cCMO_BBRadiator_Water,
                                                     baseboard->Baseboard(BaseboardNum).EquipID,
                                                     "Design Size U-Factor Times Area Value [W/K]",
                                                     UADes);
                    } else {                                            // Hard-sized with sizing data
                        baseboard->Baseboard(BaseboardNum).UA = UAUser; // need to put this back as HWBaseboardUAResidual will have reset it, CR9377
                        if (UAUser > 0.0 && UADes > 0.0) {
                            BaseSizer::reportSizerOutput(state,
                                                         cCMO_BBRadiator_Water,
                                                         baseboard->Baseboard(BaseboardNum).EquipID,
                                                         "Design Size U-Factor Times Area Value [W/K]",
                                                         UADes,
                                                         "User-Specified U-Factor Times Area Value [W/K]",
                                                         UAUser);
                            // Report difference between design size and hard-sized values
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(UADes - UAUser) / UAUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water=\"" +
                                                    baseboard->Baseboard(BaseboardNum).EquipID + "\".");
                                    ShowContinueError(state, format("User-Specified U-Factor Times Area Value of {:.2R} [W/K]", UAUser));
                                    ShowContinueError(state, format("differs from Design Size U-Factor Times Area Value of {:.2R} [W/K]", UADes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
            if (FlowAutoSize || UAAutoSize) {
                ShowSevereError(state, "SizeBaseboard: " + cCMO_BBRadiator_Water + "=\"" + baseboard->Baseboard(BaseboardNum).EquipID + "\"");
                ShowContinueError(state, "...Autosizing of hot water baseboard requires a heating loop Sizing:Plant object");
                ErrorsFound = true;
            }
        }

        // save the design water flow rate for use by the water loop sizing algorithms
        RegisterPlantCompDesignFlow(state, baseboard->Baseboard(BaseboardNum).WaterInletNode, baseboard->Baseboard(BaseboardNum).WaterVolFlowRateMax);

        if (ErrorsFound) {
            ShowFatalError(state, "SizeBaseboard: Preceding sizing errors cause program termination");
        }
    }

    void SimHWConvective(EnergyPlusData &state, int &BaseboardNum, Real64 &LoadMet)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       May 2000 Fred Buhl
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
        // in a pure convective baseboard heater.  The heater is assumed to be crossflow
        // with both fluids unmixed. The air flow is bouyancy driven and a constant air
        // flow velocity of 0.5m/s is assumed. The solution is by the effectiveness-NTU
        // method found in Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
        // Chapter 11.4, p. 523, eq. 11.33

        // REFERENCES:
        // Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
        // Chapter 11.4, p. 523, eq. 11.33

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::SmallLoad;
        using PlantUtilities::SetActuatedBranchFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName(cCMO_BBRadiator_Water + ":SimHWConvective");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        Real64 WaterInletTemp;
        Real64 AirInletTemp;
        Real64 CpAir;
        Real64 CpWater;
        Real64 AirMassFlowRate;
        Real64 WaterMassFlowRate;
        Real64 CapacitanceAir;
        Real64 CapacitanceWater;
        Real64 CapacitanceMax;
        Real64 CapacitanceMin;
        Real64 CapacityRatio;
        Real64 NTU;
        Real64 Effectiveness;
        Real64 WaterOutletTemp;
        Real64 AirOutletTemp;
        Real64 AA;
        Real64 BB;
        Real64 CC;
        Real64 QZnReq;

        auto &baseboard = state.dataBaseboardRadiator;

        ZoneNum = baseboard->Baseboard(BaseboardNum).ZonePtr;
        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        if (baseboard->Baseboard(BaseboardNum).MySizeFlag)
            QZnReq = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesHeatLoad; // If in sizing, assign design condition

        WaterInletTemp = baseboard->Baseboard(BaseboardNum).WaterInletTemp;
        AirInletTemp = baseboard->Baseboard(BaseboardNum).AirInletTemp;

        CpWater = GetSpecificHeatGlycol(state,
                                        state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidName,
                                        WaterInletTemp,
                                        state.dataPlnt->PlantLoop(baseboard->Baseboard(BaseboardNum).LoopNum).FluidIndex,
                                        RoutineName);
        CpAir = PsyCpAirFnW(baseboard->Baseboard(BaseboardNum).AirInletHumRat);

        if (baseboard->Baseboard(BaseboardNum).DesAirMassFlowRate > 0.0) { // If UA is autosized, assign design condition
            AirMassFlowRate = baseboard->Baseboard(BaseboardNum).DesAirMassFlowRate;
        } else {
            AirMassFlowRate = baseboard->Baseboard(BaseboardNum).AirMassFlowRate;
            // pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
            if (AirMassFlowRate <= 0.0) AirMassFlowRate = 2.0 * baseboard->Baseboard(BaseboardNum).WaterMassFlowRateMax;
        }

        WaterMassFlowRate = state.dataLoopNodes->Node(baseboard->Baseboard(BaseboardNum).WaterInletNode).MassFlowRate;
        CapacitanceAir = CpAir * AirMassFlowRate;

        if (QZnReq > SmallLoad && (!state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || baseboard->Baseboard(BaseboardNum).MySizeFlag) &&
            (GetCurrentScheduleValue(state, baseboard->Baseboard(BaseboardNum).SchedPtr) > 0 || baseboard->Baseboard(BaseboardNum).MySizeFlag) &&
            (WaterMassFlowRate > 0.0)) {
            CapacitanceWater = CpWater * WaterMassFlowRate;
            CapacitanceMax = max(CapacitanceAir, CapacitanceWater);
            CapacitanceMin = min(CapacitanceAir, CapacitanceWater);
            CapacityRatio = CapacitanceMin / CapacitanceMax;
            NTU = baseboard->Baseboard(BaseboardNum).UA / CapacitanceMin;
            // The effectiveness is given by the following formula:
            // Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
            // To prevent possible underflows (numbers smaller than the computer can handle) we must break
            // the calculation up into steps and check the size of the exponential arguments.
            AA = -CapacityRatio * std::pow(NTU, 0.78);
            if (AA < DataPrecisionGlobals::EXP_LowerLimit) {
                BB = 0.0;
            } else {
                BB = std::exp(AA);
            }
            CC = (1.0 / CapacityRatio) * std::pow(NTU, 0.22) * (BB - 1.0);
            if (CC < DataPrecisionGlobals::EXP_LowerLimit) {
                Effectiveness = 1.0;
            } else {
                Effectiveness = 1.0 - std::exp(CC);
            }
            AirOutletTemp = AirInletTemp + Effectiveness * CapacitanceMin * (WaterInletTemp - AirInletTemp) / CapacitanceAir;
            WaterOutletTemp = WaterInletTemp - CapacitanceAir * (AirOutletTemp - AirInletTemp) / CapacitanceWater;
            LoadMet = CapacitanceWater * (WaterInletTemp - WaterOutletTemp);
            baseboard->Baseboard(BaseboardNum).WaterOutletEnthalpy =
                baseboard->Baseboard(BaseboardNum).WaterInletEnthalpy - LoadMet / WaterMassFlowRate;
        } else {
            CapacitanceWater = 0.0;
            CapacitanceMax = CapacitanceAir;
            CapacitanceMin = 0.0;
            NTU = 0.0;
            Effectiveness = 0.0;
            AirOutletTemp = AirInletTemp;
            WaterOutletTemp = WaterInletTemp;
            LoadMet = 0.0;
            baseboard->Baseboard(BaseboardNum).WaterOutletEnthalpy = baseboard->Baseboard(BaseboardNum).WaterInletEnthalpy;
            WaterMassFlowRate = 0.0;

            SetActuatedBranchFlowRate(state,
                                      WaterMassFlowRate,
                                      baseboard->Baseboard(BaseboardNum).WaterInletNode,
                                      baseboard->Baseboard(BaseboardNum).LoopNum,
                                      baseboard->Baseboard(BaseboardNum).LoopSideNum,
                                      baseboard->Baseboard(BaseboardNum).BranchNum,
                                      false);
            AirMassFlowRate = 0.0;
        }

        baseboard->Baseboard(BaseboardNum).WaterOutletTemp = WaterOutletTemp;
        baseboard->Baseboard(BaseboardNum).AirOutletTemp = AirOutletTemp;
        baseboard->Baseboard(BaseboardNum).Power = LoadMet;
        baseboard->Baseboard(BaseboardNum).WaterMassFlowRate = WaterMassFlowRate;
        baseboard->Baseboard(BaseboardNum).AirMassFlowRate = AirMassFlowRate;
    }

    void UpdateBaseboard(EnergyPlusData &state, int &BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WaterInletNode;
        int WaterOutletNode;

        auto &baseboard = state.dataBaseboardRadiator;

        WaterInletNode = baseboard->Baseboard(BaseboardNum).WaterInletNode;
        WaterOutletNode = baseboard->Baseboard(BaseboardNum).WaterOutletNode;

        SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);
        // Set the outlet air nodes of the Baseboard
        // Set the outlet water nodes for the Coil
        state.dataLoopNodes->Node(WaterOutletNode).Temp = baseboard->Baseboard(BaseboardNum).WaterOutletTemp;
        state.dataLoopNodes->Node(WaterOutletNode).Enthalpy = baseboard->Baseboard(BaseboardNum).WaterOutletEnthalpy;
    }

    Real64 HWBaseboardUAResidual(EnergyPlusData &state,
                                 Real64 const UA,                 // UA of coil
                                 std::array<Real64, 2> const &Par // par(1) = design coil load [W]
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
        // Coil Heating Output depends on the UA which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Puts UA into the baseboard data structure, calls SimHWConvective, and calculates
        // the residual as defined above.

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int BaseboardIndex;
        Real64 LoadMet;

        BaseboardIndex = int(Par[1]);
        state.dataBaseboardRadiator->Baseboard(BaseboardIndex).UA = UA;
        SimHWConvective(state, BaseboardIndex, LoadMet);
        Residuum = (Par[0] - LoadMet) / Par[0];

        return Residuum;
    }

} // namespace BaseboardRadiator

} // namespace EnergyPlus
