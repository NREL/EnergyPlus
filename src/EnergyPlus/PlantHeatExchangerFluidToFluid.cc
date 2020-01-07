// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantHeatExchangerFluidToFluid.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantHeatExchangerFluidToFluid {

    // Module containing the routines dealing with the HeatExchanger:FluidToFluid

    // MODULE INFORMATION:
    //       AUTHOR         B. Griffith, derived from legacy code by  Sankaranarayanan K P, and S. Rees
    //       DATE WRITTEN   November 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Simulate a generic plant heat exchanger with a variety of control options
    
    int const CrossFlowBothUnMixed(1);
    int const CrossFlowBothMixed(2);
    int const CrossFlowSupplyLoopMixedDemandLoopUnMixed(3);
    int const CrossFlowSupplyLoopUnMixedDemandLoopMixed(4);
    int const CounterFlow(5);
    int const ParallelFlow(6);
    int const Ideal(7);

    int const UncontrolledOn(1001);
    int const OperationSchemeModulated(1002);
    int const OperationSchemeOnOff(1003);
    int const HeatingSetPointModulated(1004);
    int const HeatingSetPointOnOff(1005);
    int const CoolingSetPointModulated(1006);
    int const CoolingSetPointOnOff(1007);
    int const DualDeadBandSetPointModulated(1008);
    int const DualDeadBandSetPointOnOff(1009);
    int const CoolingDifferentialOnOff(1010);
    int const CoolingSetPointOnOffWithComponentOverride(1011);
    int const TrackComponentOnOff(1012);

    int const WetBulbTemperature(10);
    int const DryBulbTemperature(11);
    int const LoopTemperature(12);

    int const HeatingSupplySideLoop(501);
    int const CoolingSupplySideLoop(502);

    std::string ComponentClassName("HeatExchanger:FluidToFluid");
    int NumberOfPlantFluidHXs(0);
    bool GetInput(true);
    Array1D_bool CheckFluidHXs;

    Array1D<HeatExchangerStruct> FluidHX;

    void SimFluidHeatExchanger(int const LoopNum,                       // plant loop sim call originated from
                               int const EP_UNUSED(LoopSideNum),        // plant loop side sim call originated from
                               std::string const &EP_UNUSED(EquipType), // type of equipment, 'PlantComponent:UserDefined'
                               std::string const &EquipName,            // user name for component
                               int &CompIndex,
                               bool &InitLoopEquip,
                               Real64 const MyLoad,
                               Real64 &MaxCap,
                               Real64 &MinCap,
                               Real64 &OptCap,
                               bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Main entry point and simulation manager for heat exchanger
        
        int CompNum;

        if (GetInput) {
            GetFluidHeatExchangerInput();
            GetInput = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            CompNum = UtilityRoutines::FindItemInList(EquipName, FluidHX);
            if (CompNum == 0) {
                ShowFatalError("SimFluidHeatExchanger: HeatExchanger:FluidToFluid not found");
            }
            CompIndex = CompNum;
        } else {
            CompNum = CompIndex;
            if (CompNum < 1 || CompNum > NumberOfPlantFluidHXs) {
                ShowFatalError("SimFluidHeatExchanger: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) + ", Number of heat exchangers =" +
                               General::TrimSigDigits(NumberOfPlantFluidHXs) + ", Entered heat exchanger name = " + EquipName);
            }
            if (CheckFluidHXs(CompNum)) {
                if (EquipName != FluidHX(CompNum).Name) {
                    ShowFatalError("SimFluidHeatExchanger: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", heat exchanger name=" + EquipName + ", stored name for that index=" + FluidHX(CompNum).Name);
                }
                CheckFluidHXs(CompNum) = false;
            }
        }

        if (InitLoopEquip) {
            InitFluidHeatExchanger(CompNum, LoopNum);
            if (LoopNum == FluidHX(CompNum).DemandSideLoop.loopNum) {
                MinCap = 0.0;
                MaxCap = FluidHX(CompNum).DemandSideLoop.MaxLoad;
                OptCap = FluidHX(CompNum).DemandSideLoop.MaxLoad * 0.9;
            } else if (LoopNum == FluidHX(CompNum).SupplySideLoop.loopNum) {
                SizeFluidHeatExchanger(CompNum); // only call sizing from the loop that sizes are based on
                MinCap = 0.0;
                MaxCap = FluidHX(CompNum).SupplySideLoop.MaxLoad;
                OptCap = FluidHX(CompNum).SupplySideLoop.MaxLoad * 0.9;
            }
        }

        InitFluidHeatExchanger(CompNum, LoopNum);

        // for op scheme led HXs, only call controls if called from Loop Supply Side
        if ((FluidHX(CompNum).ControlMode == OperationSchemeModulated) || (FluidHX(CompNum).ControlMode == OperationSchemeOnOff)) {
            if (LoopNum == FluidHX(CompNum).SupplySideLoop.loopNum) {
                ControlFluidHeatExchanger(CompNum, LoopNum, MyLoad, FirstHVACIteration);
            }
        } else {
            ControlFluidHeatExchanger(CompNum, LoopNum, MyLoad, FirstHVACIteration);
        }

        CalcFluidHeatExchanger(CompNum,
                               DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).MassFlowRate,
                               DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).MassFlowRate);

        UpdateFluidHeatExchanger(CompNum);

        ReportFluidHeatExchanger(CompNum);
    }

    void GetFluidHeatExchangerInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get input for heat exchanger model

        static std::string const RoutineName("GetFluidHeatExchangerInput: ");

        bool ErrorsFound(false);
        int NumAlphas;               // Number of elements in the alpha array
        int NumNums;                 // Number of elements in the numeric array
        int IOStat;                  // IO Status when calling get input subroutine
        int MaxNumAlphas(0);  // argument for call to GetObjectDefMaxArgs
        int MaxNumNumbers(0); // argument for call to GetObjectDefMaxArgs
        int TotalArgs(0);     // argument for call to GetObjectDefMaxArgs
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;

        cCurrentModuleObject = "HeatExchanger:FluidToFluid";

        NumberOfPlantFluidHXs = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (NumberOfPlantFluidHXs == 0) return;

        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = NumNums;
        MaxNumAlphas = NumAlphas;

        cAlphaFieldNames.allocate(MaxNumAlphas);
        cAlphaArgs.allocate(MaxNumAlphas);
        lAlphaFieldBlanks.dimension(MaxNumAlphas, false);
        cNumericFieldNames.allocate(MaxNumNumbers);
        rNumericArgs.dimension(MaxNumNumbers, 0.0);
        lNumericFieldBlanks.dimension(MaxNumNumbers, false);

        if (NumberOfPlantFluidHXs > 0) {
            FluidHX.allocate(NumberOfPlantFluidHXs);
            CheckFluidHXs.dimension(NumberOfPlantFluidHXs, true);
            for (int CompLoop = 1; CompLoop <= NumberOfPlantFluidHXs; ++CompLoop) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
                                              CompLoop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                FluidHX(CompLoop).Name = cAlphaArgs(1);

                if (lAlphaFieldBlanks(2)) {
                    FluidHX(CompLoop).AvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                } else {
                    FluidHX(CompLoop).AvailSchedNum = ScheduleManager::GetScheduleIndex(cAlphaArgs(2));
                    if (FluidHX(CompLoop).AvailSchedNum <= 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                        ShowContinueError("Schedule was not found ");
                        ErrorsFound = true;
                    }
                }

                FluidHX(CompLoop).DemandSideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(
                    cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
                FluidHX(CompLoop).DemandSideLoop.outletNodeNum = NodeInputManager::GetOnlySingleNode(
                    cAlphaArgs(4), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Loop Demand Side Plant Nodes");
                FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRate = rNumericArgs(1);
                if (FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRate == DataSizing::AutoSize) {
                    FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRateWasAutoSized = true;
                }

                FluidHX(CompLoop).SupplySideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(
                    cAlphaArgs(5), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
                FluidHX(CompLoop).SupplySideLoop.outletNodeNum = NodeInputManager::GetOnlySingleNode(
                    cAlphaArgs(6), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(5), cAlphaArgs(6), "Loop Supply Side Plant Nodes");
                FluidHX(CompLoop).SupplySideLoop.DesignVolumeFlowRate = rNumericArgs(2);
                if (FluidHX(CompLoop).SupplySideLoop.DesignVolumeFlowRate == DataSizing::AutoSize) {
                    FluidHX(CompLoop).SupplySideLoop.DesignVolumeFlowRateWasAutoSized = true;
                }

                if (UtilityRoutines::SameString(cAlphaArgs(7), "CrossFlowBothUnMixed")) {
                    FluidHX(CompLoop).HeatExchangeModelType = CrossFlowBothUnMixed;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "CrossFlowBothMixed")) {
                    FluidHX(CompLoop).HeatExchangeModelType = CrossFlowBothMixed;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "CrossFlowSupplyMixedDemandUnMixed")) {
                    FluidHX(CompLoop).HeatExchangeModelType = CrossFlowSupplyLoopMixedDemandLoopUnMixed;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "CrossFlowSupplyUnMixedDemandMixed")) {
                    FluidHX(CompLoop).HeatExchangeModelType = CrossFlowSupplyLoopUnMixedDemandLoopMixed;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "CounterFlow")) {
                    FluidHX(CompLoop).HeatExchangeModelType = CounterFlow;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "ParallelFlow")) {
                    FluidHX(CompLoop).HeatExchangeModelType = ParallelFlow;
                } else if (UtilityRoutines::SameString(cAlphaArgs(7), "Ideal")) {
                    FluidHX(CompLoop).HeatExchangeModelType = Ideal;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError("Invalid " + cAlphaFieldNames(7) + " = " + cAlphaArgs(7));
                    ErrorsFound = true;
                }

                if (!lNumericFieldBlanks(3)) {
                    FluidHX(CompLoop).UA = rNumericArgs(3);
                    if (FluidHX(CompLoop).UA == DataSizing::AutoSize) {
                        FluidHX(CompLoop).UAWasAutoSized = true;
                    }
                } else {
                    if (FluidHX(CompLoop).HeatExchangeModelType != Ideal) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Missing entry for " + cNumericFieldNames(3));
                        ErrorsFound = true;
                    }
                }

                if (UtilityRoutines::SameString(cAlphaArgs(8), "UncontrolledOn")) {
                    FluidHX(CompLoop).ControlMode = UncontrolledOn;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "OperationSchemeModulated")) {
                    FluidHX(CompLoop).ControlMode = OperationSchemeModulated;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "OperationSchemeOnOff")) {
                    FluidHX(CompLoop).ControlMode = OperationSchemeOnOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "HeatingSetpointModulated")) {
                    FluidHX(CompLoop).ControlMode = HeatingSetPointModulated;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "HeatingSetpointOnOff")) {
                    FluidHX(CompLoop).ControlMode = HeatingSetPointOnOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "CoolingSetpointModulated")) {
                    FluidHX(CompLoop).ControlMode = CoolingSetPointModulated;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "CoolingSetpointOnOff")) {
                    FluidHX(CompLoop).ControlMode = CoolingSetPointOnOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "DualDeadbandSetpointModulated")) {
                    FluidHX(CompLoop).ControlMode = DualDeadBandSetPointModulated;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "DualDeadbandSetpointOnOff")) {
                    FluidHX(CompLoop).ControlMode = DualDeadBandSetPointOnOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "CoolingDifferentialOnOff")) {
                    FluidHX(CompLoop).ControlMode = CoolingDifferentialOnOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "CoolingSetpointOnOffWithComponentOverride")) {
                    FluidHX(CompLoop).ControlMode = CoolingSetPointOnOffWithComponentOverride;
                } else if (UtilityRoutines::SameString(cAlphaArgs(8), "TrackComponentOnOff")) {
                    FluidHX(CompLoop).ControlMode = TrackComponentOnOff;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError("Invalid " + cAlphaFieldNames(8) + " = " + cAlphaArgs(8));
                    ErrorsFound = true;
                }

                if (!lAlphaFieldBlanks(9)) {
                    FluidHX(CompLoop).SetPointNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(9),
                                                                          ErrorsFound,
                                                                          cCurrentModuleObject,
                                                                          cAlphaArgs(1),
                                                                          DataLoopNode::NodeType_Water,
                                                                          DataLoopNode::NodeConnectionType_Sensor,
                                                                          1,
                                                                          DataLoopNode::ObjectIsNotParent);
                    // check that node actually has setpoints on it
                    if ((FluidHX(CompLoop).ControlMode == HeatingSetPointModulated) || (FluidHX(CompLoop).ControlMode == HeatingSetPointOnOff) ||
                        (FluidHX(CompLoop).ControlMode == CoolingSetPointModulated) || (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOff) ||
                        (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOffWithComponentOverride)) {
                        if (DataLoopNode::Node(FluidHX(CompLoop).SetPointNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) {
                            if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                                ShowSevereError(RoutineName + " Missing temperature setpoint for DataLoopNode::Node = " + cAlphaArgs(9));
                                ShowContinueError("Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                                ShowContinueError(" Use a setpoint manager to place a single temperature setpoint on the node");
                                ErrorsFound = true;
                            } else {
                                // need call to EMS to check node
                                bool NodeEMSSetPointMissing = false;
                                EMSManager::CheckIfNodeSetPointManagedByEMS(FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureSetPoint, NodeEMSSetPointMissing);
                                if (NodeEMSSetPointMissing) {
                                    ShowSevereError(RoutineName + " Missing temperature setpoint for node = " + cAlphaArgs(9));
                                    ShowContinueError("Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                                    ShowContinueError("Use a setpoint manager or EMS actuator to place a single temperature setpoint on the node");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    } else if ((FluidHX(CompLoop).ControlMode == DualDeadBandSetPointModulated) ||
                               (FluidHX(CompLoop).ControlMode == DualDeadBandSetPointOnOff)) {
                        if ((DataLoopNode::Node(FluidHX(CompLoop).SetPointNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue) ||
                            (DataLoopNode::Node(FluidHX(CompLoop).SetPointNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
                            if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                                ShowSevereError(RoutineName + " Missing dual temperature setpoints for node = " + cAlphaArgs(9));
                                ShowContinueError("Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                                ShowContinueError(" Use a setpoint manager to place a dual temperature setpoint on the node");
                                ErrorsFound = true;
                            } else {
                                // need call to EMS to check node
                                bool NodeEMSSetPointMissing = false;
                                EMSManager::CheckIfNodeSetPointManagedByEMS(FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureMinSetPoint, NodeEMSSetPointMissing);
                                EMSManager::CheckIfNodeSetPointManagedByEMS(FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureMaxSetPoint, NodeEMSSetPointMissing);
                                if (NodeEMSSetPointMissing) {
                                    ShowSevereError(RoutineName + " Missing temperature setpoint for node = " + cAlphaArgs(9));
                                    ShowContinueError("Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                                    ShowContinueError("Use a setpoint manager or EMS actuators to place a dual temperature setpoints on the node");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                } else {
                    // need to name a setpoint node if using a setpoint type control mode
                    if ((FluidHX(CompLoop).ControlMode == HeatingSetPointModulated) || (FluidHX(CompLoop).ControlMode == HeatingSetPointOnOff) ||
                        (FluidHX(CompLoop).ControlMode == CoolingSetPointModulated) || (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOff) ||
                        (FluidHX(CompLoop).ControlMode == DualDeadBandSetPointModulated) ||
                        (FluidHX(CompLoop).ControlMode == DualDeadBandSetPointOnOff) ||
                        (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOffWithComponentOverride)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Missing entry for " + cAlphaFieldNames(9));
                        ErrorsFound = true;
                    }
                }

                if (!lNumericFieldBlanks(4)) {
                    FluidHX(CompLoop).TempControlTol = rNumericArgs(4);
                } else {
                    FluidHX(CompLoop).TempControlTol = 0.01;
                }

                FluidHX(CompLoop).HeatTransferMeteringEndUse = cAlphaArgs(10);

                if (!lAlphaFieldBlanks(11)) {
                    FluidHX(CompLoop).OtherCompSupplySideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(11),
                                                                                                                 ErrorsFound,
                                                                                                                 cCurrentModuleObject,
                                                                                                                 cAlphaArgs(1),
                                                                                                                 DataLoopNode::NodeType_Water,
                                                                                                                 DataLoopNode::NodeConnectionType_Actuator,
                                                                                                                 1,
                                                                                                                 DataLoopNode::ObjectIsNotParent);
                } else {
                    if (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOffWithComponentOverride) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Missing entry for " + cAlphaFieldNames(11));
                        ErrorsFound = true;
                    }
                }

                if (!lAlphaFieldBlanks(12)) {
                    FluidHX(CompLoop).OtherCompDemandSideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(12),
                                                                                                                 ErrorsFound,
                                                                                                                 cCurrentModuleObject,
                                                                                                                 cAlphaArgs(1),
                                                                                                                 DataLoopNode::NodeType_Water,
                                                                                                                 DataLoopNode::NodeConnectionType_Actuator,
                                                                                                                 1,
                                                                                                                 DataLoopNode::ObjectIsNotParent);
                } else {
                    if (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOffWithComponentOverride) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Missing entry for " + cAlphaFieldNames(12));
                        ErrorsFound = true;
                    }
                }

                if (!lAlphaFieldBlanks(13)) {
                    if (UtilityRoutines::SameString(cAlphaArgs(13), "WetBulbTemperature")) {
                        FluidHX(CompLoop).ControlSignalTemp = WetBulbTemperature;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(13), "DryBulbTemperature")) {
                        FluidHX(CompLoop).ControlSignalTemp = DryBulbTemperature;
                    } else if (UtilityRoutines::SameString(cAlphaArgs(13), "Loop")) {
                        FluidHX(CompLoop).ControlSignalTemp = LoopTemperature;
                    }
                } else {
                    if (FluidHX(CompLoop).ControlMode == CoolingSetPointOnOffWithComponentOverride) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid entry.");
                        ShowContinueError("Missing entry for " + cAlphaFieldNames(13));
                        ErrorsFound = true;
                    }
                }

                if (!lNumericFieldBlanks(5)) {
                    FluidHX(CompLoop).SizingFactor = rNumericArgs(5);
                } else {
                    FluidHX(CompLoop).SizingFactor = 1.0;
                }

                if (!lNumericFieldBlanks(6)) {
                    FluidHX(CompLoop).MinOperationTemp = rNumericArgs(6);
                } else {
                    FluidHX(CompLoop).MinOperationTemp = -9999.0;
                }

                if (!lNumericFieldBlanks(7)) {
                    FluidHX(CompLoop).MaxOperationTemp = rNumericArgs(7);
                } else {
                    FluidHX(CompLoop).MaxOperationTemp = 9999.0;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in processing " + cCurrentModuleObject + " input.");
        }

        for (int CompLoop = 1; CompLoop <= NumberOfPlantFluidHXs; ++CompLoop) {

            SetupOutputVariable("Fluid Heat Exchanger Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                FluidHX(CompLoop).HeatTransferRate,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);

            SetupOutputVariable("Fluid Heat Exchanger Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                FluidHX(CompLoop).HeatTransferEnergy,
                                "System",
                                "Sum",
                                FluidHX(CompLoop).Name,
                                _,
                                "ENERGYTRANSFER",
                                FluidHX(CompLoop).HeatTransferMeteringEndUse,
                                _,
                                "Plant");

            SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                FluidHX(CompLoop).SupplySideLoop.InletMassFlowRate,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                FluidHX(CompLoop).SupplySideLoop.InletTemp,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                FluidHX(CompLoop).SupplySideLoop.OutletTemp,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                FluidHX(CompLoop).DemandSideLoop.InletMassFlowRate,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                FluidHX(CompLoop).DemandSideLoop.InletTemp,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                FluidHX(CompLoop).DemandSideLoop.OutletTemp,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Operation Status",
                                OutputProcessor::Unit::None,
                                FluidHX(CompLoop).OperationStatus,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
            SetupOutputVariable("Fluid Heat Exchanger Effectiveness",
                                OutputProcessor::Unit::None,
                                FluidHX(CompLoop).Effectiveness,
                                "System",
                                "Average",
                                FluidHX(CompLoop).Name);
        }
    }

    void InitFluidHeatExchanger(int const CompNum, int const EP_UNUSED(LoopNum))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   november, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initialize heat exchanger model
        
        static std::string const RoutineNameNoColon("InitFluidHeatExchanger");

        bool errFlag;
        static std::string const RoutineName("InitFluidHeatExchanger: ");
        Real64 rho;
        int LoopNum2;
        int LoopSideNum;
        int BranchNum;
        int LoopCompNum;

        if (FluidHX(CompNum).MyOneTimeFlag) {
            FluidHX(CompNum).MyFlag = true;
            FluidHX(CompNum).MyEnvrnFlag = true;
            FluidHX(CompNum).MyOneTimeFlag = false;
        }

        if (FluidHX(CompNum).MyFlag) {
            // locate the main two connections to the plant loops
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(FluidHX(CompNum).Name,
                                                    DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                                    FluidHX(CompNum).DemandSideLoop.loopNum,
                                                    FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                    FluidHX(CompNum).DemandSideLoop.branchNum,
                                                    FluidHX(CompNum).DemandSideLoop.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                    _);

            if (FluidHX(CompNum).DemandSideLoop.loopSideNum != DataPlant::DemandSide) { // throw error
                ShowSevereError(RoutineName + " Invalid connections for " + DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" +
                                FluidHX(CompNum).Name + "\"");
                ShowContinueError("The \"Loop Demand Side\" connections are not on the Demand Side of a plant loop");
                errFlag = true;
            }

            PlantUtilities::ScanPlantLoopsForObject(FluidHX(CompNum).Name,
                                                    DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                                    FluidHX(CompNum).SupplySideLoop.loopNum,
                                                    FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                    FluidHX(CompNum).SupplySideLoop.branchNum,
                                                    FluidHX(CompNum).SupplySideLoop.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                    _);

            if (FluidHX(CompNum).SupplySideLoop.loopSideNum != DataPlant::SupplySide) { // throw error
                ShowSevereError(RoutineName + " Invalid connections for " + DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" +
                                FluidHX(CompNum).Name + "\"");
                ShowContinueError("The \"Loop Supply Side\" connections are not on the Supply Side of a plant loop");
                errFlag = true;
            }

            // make sure it is not the same loop on both sides.
            if (FluidHX(CompNum).SupplySideLoop.loopNum == FluidHX(CompNum).DemandSideLoop.loopNum) { // user is being too tricky, don't allow
                ShowSevereError(RoutineName + " Invalid connections for " + DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" +
                                FluidHX(CompNum).Name + "\"");
                ShowContinueError(R"(The "Loop Supply Side" and "Loop Demand Side" need to be on different loops.)");
                errFlag = true;
            } else {

                PlantUtilities::InterConnectTwoPlantLoopSides(FluidHX(CompNum).SupplySideLoop.loopNum,
                                              FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                              FluidHX(CompNum).DemandSideLoop.loopNum,
                                              FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                              DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                              true);
            }

            // find remote component if control mode is of that type.
            if (FluidHX(CompNum).ControlMode == CoolingSetPointOnOffWithComponentOverride) {

                PlantUtilities::ScanPlantLoopsForNodeNum(RoutineName,
                                                         FluidHX(CompNum).OtherCompSupplySideLoop.inletNodeNum,
                                                         FluidHX(CompNum).OtherCompSupplySideLoop.loopNum,
                                                         FluidHX(CompNum).OtherCompSupplySideLoop.loopSideNum,
                                                         FluidHX(CompNum).OtherCompSupplySideLoop.branchNum,
                                                         FluidHX(CompNum).OtherCompSupplySideLoop.compNum);

                PlantUtilities::ScanPlantLoopsForNodeNum(RoutineName,
                                                         FluidHX(CompNum).OtherCompDemandSideLoop.inletNodeNum,
                                                         FluidHX(CompNum).OtherCompDemandSideLoop.loopNum,
                                                         FluidHX(CompNum).OtherCompDemandSideLoop.loopSideNum,
                                                         FluidHX(CompNum).OtherCompDemandSideLoop.branchNum,
                                                         FluidHX(CompNum).OtherCompDemandSideLoop.compNum);

                // revise how loads served category for other controlled equipment
                LoopNum2 = FluidHX(CompNum).OtherCompSupplySideLoop.loopNum;
                LoopSideNum = FluidHX(CompNum).OtherCompSupplySideLoop.loopSideNum;
                BranchNum = FluidHX(CompNum).OtherCompSupplySideLoop.branchNum;
                LoopCompNum = FluidHX(CompNum).OtherCompSupplySideLoop.compNum;

                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed);

                    if (SELECT_CASE_var == DataPlant::HowMet_ByNominalCap) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed =
                            DataPlant::HowMet_ByNominalCapFreeCoolCntrl;
                    } else if (SELECT_CASE_var == DataPlant::HowMet_ByNominalCapLowOutLimit) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed =
                            DataPlant::HowMet_ByNominalCapLowOutLimitFreeCoolCntrl;
                    }
                }

                {
                    auto const SELECT_CASE_var(FluidHX(CompNum).ControlSignalTemp);
                    if (SELECT_CASE_var == WetBulbTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode = DataPlant::FreeCoolControlMode_WetBulb;
                    } else if (SELECT_CASE_var == DryBulbTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode = DataPlant::FreeCoolControlMode_DryBulb;
                    } else if (SELECT_CASE_var == LoopTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode = DataPlant::FreeCoolControlMode_Loop;
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlNodeNum =
                            FluidHX(CompNum).OtherCompDemandSideLoop.inletNodeNum;
                    }
                }
            }
            if (FluidHX(CompNum).ControlMode == TrackComponentOnOff) {
                if (FluidHX(CompNum).OtherCompSupplySideLoop.inletNodeNum > 0) {
                    PlantUtilities::ScanPlantLoopsForObject(FluidHX(CompNum).ComponentUserName,
                                                            FluidHX(CompNum).ComponentTypeOfNum,
                                                            FluidHX(CompNum).OtherCompSupplySideLoop.loopNum,
                                                            FluidHX(CompNum).OtherCompSupplySideLoop.loopSideNum,
                                                            FluidHX(CompNum).OtherCompSupplySideLoop.branchNum,
                                                            FluidHX(CompNum).OtherCompSupplySideLoop.compNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            FluidHX(CompNum).OtherCompSupplySideLoop.inletNodeNum,
                                                            _);
                }
                if (FluidHX(CompNum).OtherCompDemandSideLoop.inletNodeNum > 0) {
                    PlantUtilities::ScanPlantLoopsForObject(FluidHX(CompNum).ComponentUserName,
                                                            FluidHX(CompNum).ComponentTypeOfNum,
                                                            FluidHX(CompNum).OtherCompDemandSideLoop.loopNum,
                                                            FluidHX(CompNum).OtherCompDemandSideLoop.loopSideNum,
                                                            FluidHX(CompNum).OtherCompDemandSideLoop.branchNum,
                                                            FluidHX(CompNum).OtherCompDemandSideLoop.compNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            FluidHX(CompNum).OtherCompDemandSideLoop.inletNodeNum,
                                                            _);
                }
            }

            if (errFlag) {
                ShowFatalError(RoutineName + "Program terminated due to previous condition(s).");
            }
            FluidHX(CompNum).MyFlag = false;
        } // plant setup

        if (DataGlobals::BeginEnvrnFlag && FluidHX(CompNum).MyEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidIndex,
                                   RoutineNameNoColon);
            FluidHX(CompNum).DemandSideLoop.MassFlowRateMax = rho * FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate;
            PlantUtilities::InitComponentNodes(FluidHX(CompNum).DemandSideLoop.MassFlowRateMin,
                               FluidHX(CompNum).DemandSideLoop.MassFlowRateMax,
                               FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                               FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                               FluidHX(CompNum).DemandSideLoop.loopNum,
                               FluidHX(CompNum).DemandSideLoop.loopSideNum,
                               FluidHX(CompNum).DemandSideLoop.branchNum,
                               FluidHX(CompNum).DemandSideLoop.compNum);

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                   RoutineNameNoColon);
            FluidHX(CompNum).SupplySideLoop.MassFlowRateMax = rho * FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate;
            PlantUtilities::InitComponentNodes(FluidHX(CompNum).SupplySideLoop.MassFlowRateMin,
                               FluidHX(CompNum).SupplySideLoop.MassFlowRateMax,
                               FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                               FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                               FluidHX(CompNum).SupplySideLoop.loopNum,
                               FluidHX(CompNum).SupplySideLoop.loopSideNum,
                               FluidHX(CompNum).SupplySideLoop.branchNum,
                               FluidHX(CompNum).SupplySideLoop.compNum);
            FluidHX(CompNum).MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            FluidHX(CompNum).MyEnvrnFlag = true;
        }

        FluidHX(CompNum).DemandSideLoop.InletTemp = DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp;
        FluidHX(CompNum).SupplySideLoop.InletTemp = DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp;

        if (FluidHX(CompNum).ControlMode == CoolingSetPointOnOffWithComponentOverride) {
            // store current value for setpoint in central plant loop data structure
            LoopNum2 = FluidHX(CompNum).OtherCompSupplySideLoop.loopNum;
            LoopSideNum = FluidHX(CompNum).OtherCompSupplySideLoop.loopSideNum;
            BranchNum = FluidHX(CompNum).OtherCompSupplySideLoop.branchNum;
            LoopCompNum = FluidHX(CompNum).OtherCompSupplySideLoop.compNum;

            DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMinCntrlTemp =
                DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint - FluidHX(CompNum).TempControlTol; // issue #5626, include control tolerance
        }
    }

    void SizeFluidHeatExchanger(int const CompNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   December 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Size plant heat exchanger flow rates, UA, and max capacity

        // METHODOLOGY EMPLOYED:
        // the supply side flow rate is obtained from the plant sizing structure
        // the demand side is sized to match the supply side
        // the UA is sized for an effectiveness of 1.0 using sizing temps
        // the capacity uses the full HX model

        static std::string const RoutineName("SizeFluidHeatExchanger");

        int PltSizNumSupSide; // Plant Sizing index for Loop Supply Side
        int PltSizNumDmdSide; // plant sizing index for Loop Demand Side
        Real64 tmpSupSideDesignVolFlowRate;
        Real64 tmpDmdSideDesignVolFlowRate;
        Real64 tmpUA;
        Real64 tmpDeltaTSupLoop;
        Real64 tmpDeltaTloopToLoop(0.0);
        Real64 Cp;
        Real64 rho;
        Real64 tmpDesCap;
        Real64 SupSideMdot;
        Real64 DmdSideMdot;

        // first deal with Loop Supply Side
        PltSizNumSupSide = DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).PlantSizNum;
        PltSizNumDmdSide = DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).PlantSizNum;
        tmpSupSideDesignVolFlowRate = FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate;
        if (FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRateWasAutoSized) {
            if (PltSizNumSupSide > 0) {
                if (DataSizing::PlantSizData(PltSizNumSupSide).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpSupSideDesignVolFlowRate = DataSizing::PlantSizData(PltSizNumSupSide).DesVolFlowRate * FluidHX(CompNum).SizingFactor;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
                } else {
                    tmpSupSideDesignVolFlowRate = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
                }
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                       FluidHX(CompNum).Name,
                                       "Loop Supply Side Design Fluid Flow Rate [m3/s]",
                                       FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                       FluidHX(CompNum).Name,
                                       "Initial Loop Supply Side Design Fluid Flow Rate [m3/s]",
                                       FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate);
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeFluidHeatExchanger: Autosizing of requires a loop Sizing:Plant object");
                    ShowContinueError("Occurs in heat exchanger object=" + FluidHX(CompNum).Name);
                }
            }
        }
        PlantUtilities::RegisterPlantCompDesignFlow(FluidHX(CompNum).SupplySideLoop.inletNodeNum, tmpSupSideDesignVolFlowRate);

        // second deal with Loop Demand Side
        tmpDmdSideDesignVolFlowRate = FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate;
        if (FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRateWasAutoSized) {
            if (tmpSupSideDesignVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                tmpDmdSideDesignVolFlowRate = tmpSupSideDesignVolFlowRate;
                if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
            } else {
                tmpDmdSideDesignVolFlowRate = 0.0;
                if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
            }
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                   FluidHX(CompNum).Name,
                                   "Loop Demand Side Design Fluid Flow Rate [m3/s]",
                                   FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                   FluidHX(CompNum).Name,
                                   "Initial Loop Demand Side Design Fluid Flow Rate [m3/s]",
                                   FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate);
            }
        }
        PlantUtilities::RegisterPlantCompDesignFlow(FluidHX(CompNum).DemandSideLoop.inletNodeNum, tmpDmdSideDesignVolFlowRate);

        // size UA if needed
        if (FluidHX(CompNum).UAWasAutoSized) {
            // get nominal delta T between two loops
            if (PltSizNumSupSide > 0 && PltSizNumDmdSide > 0) {

                {
                    auto const SELECT_CASE_var(DataSizing::PlantSizData(PltSizNumSupSide).LoopType);

                    if ((SELECT_CASE_var == DataSizing::HeatingLoop) || (SELECT_CASE_var == DataSizing::SteamLoop)) {
                        tmpDeltaTloopToLoop = std::abs((DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp - DataSizing::PlantSizData(PltSizNumSupSide).DeltaT) -
                                                       DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp);
                    } else if ((SELECT_CASE_var == DataSizing::CoolingLoop) || (SELECT_CASE_var == DataSizing::CondenserLoop)) {
                        tmpDeltaTloopToLoop = std::abs((DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp + DataSizing::PlantSizData(PltSizNumSupSide).DeltaT) -
                                                       DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp);
                    } else {
                        assert(false);
                    }
                }

                tmpDeltaTloopToLoop = max(2.0, tmpDeltaTloopToLoop);
                tmpDeltaTSupLoop = DataSizing::PlantSizData(PltSizNumSupSide).DeltaT;
                if (tmpSupSideDesignVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                               RoutineName);

                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                           RoutineName);

                    tmpDesCap = Cp * rho * tmpDeltaTSupLoop * tmpSupSideDesignVolFlowRate;
                    tmpUA = tmpDesCap / tmpDeltaTloopToLoop;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).UA = tmpUA;
                } else {
                    tmpUA = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) FluidHX(CompNum).UA = tmpUA;
                }
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "HeatExchanger:FluidToFluid", FluidHX(CompNum).Name, "Heat Exchanger U-Factor Times Area Value [W/C]", FluidHX(CompNum).UA);
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                       FluidHX(CompNum).Name,
                                       "Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]",
                                       tmpDeltaTloopToLoop);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                       FluidHX(CompNum).Name,
                                       "Initial Heat Exchanger U-Factor Times Area Value [W/C]",
                                       FluidHX(CompNum).UA);
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                       FluidHX(CompNum).Name,
                                       "Initial Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]",
                                       tmpDeltaTloopToLoop);
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeFluidHeatExchanger: Autosizing of heat Exchanger UA requires a loop Sizing:Plant objects for both loops");
                    ShowContinueError("Occurs in heat exchanger object=" + FluidHX(CompNum).Name);
                }
            }
        }

        // size capacities for load range based op schemes
        if (DataPlant::PlantFirstSizesOkayToFinalize) {

            if (PltSizNumSupSide > 0) {
                {
                    auto const SELECT_CASE_var(DataSizing::PlantSizData(PltSizNumSupSide).LoopType);
                    if ((SELECT_CASE_var == DataSizing::HeatingLoop) || (SELECT_CASE_var == DataSizing::SteamLoop)) {
                        DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp =
                            (DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp - DataSizing::PlantSizData(PltSizNumSupSide).DeltaT);
                    } else if ((SELECT_CASE_var == DataSizing::CoolingLoop) || (SELECT_CASE_var == DataSizing::CondenserLoop)) {
                        DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp =
                            (DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp + DataSizing::PlantSizData(PltSizNumSupSide).DeltaT);
                    }
                }

            } else { // don't rely on sizing, use loop setpoints
                // loop supply side
                if (DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp =
                        DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp =
                            (DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPointHi +
                             DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPointLo) /
                            2.0;
                }
            }

            if (PltSizNumDmdSide > 0) {
                DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp = DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp;
            } else { // don't rely on sizing, use loop setpoints
                // loop demand side
                if (DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp =
                        DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp =
                            (DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPointHi +
                             DataLoopNode::Node(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPointLo) /
                            2.0;
                }
            }

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                   RoutineName);
            SupSideMdot = FluidHX(CompNum).SupplySideLoop.DesignVolumeFlowRate * rho;
            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidIndex,
                                   RoutineName);
            DmdSideMdot = FluidHX(CompNum).DemandSideLoop.DesignVolumeFlowRate * rho;

            CalcFluidHeatExchanger(CompNum, SupSideMdot, DmdSideMdot);
            FluidHX(CompNum).SupplySideLoop.MaxLoad = std::abs(FluidHX(CompNum).HeatTransferRate);
        }
        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, FluidHX(CompNum).Name, "HeatExchanger:FluidToFluid");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, FluidHX(CompNum).Name, FluidHX(CompNum).SupplySideLoop.MaxLoad);
        }
    }

    void ControlFluidHeatExchanger(int const CompNum, int const EP_UNUSED(LoopNum), Real64 const MyLoad, bool FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // determine control state for fluid to fluid heat exchanger
        // make fluid flow requests accordingly

        // METHODOLOGY EMPLOYED:
        // long CASE statement for different control options
        
        static std::string const RoutineName("ControlFluidHeatExchanger");

        Real64 mdotSupSide;
        Real64 mdotDmdSide;
        Real64 ControlSignalValue(0.0);

        // check if available by schedule
        bool ScheduledOff;
        Real64 AvailSchedValue = ScheduleManager::GetCurrentScheduleValue(FluidHX(CompNum).AvailSchedNum);
        if (AvailSchedValue <= 0) {
            ScheduledOff = true;
        } else {
            ScheduledOff = false;
        }

        // check if operational limits trip off unit
        bool LimitTrippedOff = false;
        if ((DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp < FluidHX(CompNum).MinOperationTemp) ||
            (DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp < FluidHX(CompNum).MinOperationTemp)) {
            LimitTrippedOff = true;
        }
        if ((DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp > FluidHX(CompNum).MaxOperationTemp) ||
            (DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp > FluidHX(CompNum).MaxOperationTemp)) {
            LimitTrippedOff = true;
        }

        if (!ScheduledOff && !LimitTrippedOff) {

            {
                auto const SELECT_CASE_var(FluidHX(CompNum).ControlMode);

                if (SELECT_CASE_var == UncontrolledOn) {

                    // make passive request for supply side loop flow
                    mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                         FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                         FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                         FluidHX(CompNum).SupplySideLoop.loopNum,
                                         FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                         FluidHX(CompNum).SupplySideLoop.branchNum,
                                         FluidHX(CompNum).SupplySideLoop.compNum);
                    if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                        // if supply side loop has massflow, request demand side flow
                        mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                    } else {
                        mdotDmdSide = 0.0;
                    }
                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);

                } else if (SELECT_CASE_var == OperationSchemeModulated) {

                    if (std::abs(MyLoad) > DataHVACGlobals::SmallLoad) {
                        if (MyLoad < -1.0 * DataHVACGlobals::SmallLoad) { // requesting cooling
                            Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                            if (DeltaTCooling > FluidHX(CompNum).TempControlTol) { // can do cooling so turn on
                                mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    // if supply side loop has massflow, request demand side flow
                                    Real64 cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                                               FluidHX(CompNum).SupplySideLoop.InletTemp,
                                                               DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                                               RoutineName);
                                    Real64 TargetLeavingTemp = FluidHX(CompNum).SupplySideLoop.InletTemp - std::abs(MyLoad) / (cp * mdotSupSide);

                                    FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop);
                                } else { // no flow on supply side so do not request flow on demand side
                                    mdotDmdSide = 0.0;
                                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                                         FluidHX(CompNum).SupplySideLoop.compNum);
                                }
                            } else { // not able to cool so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                            }

                        } else { // requesting heating
                            Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                            if (DeltaTHeating > FluidHX(CompNum).TempControlTol) { // can do heating so turn on
                                mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    Real64 cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                                               FluidHX(CompNum).SupplySideLoop.InletTemp,
                                                               DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                                               RoutineName);
                                    Real64 TargetLeavingTemp = FluidHX(CompNum).SupplySideLoop.InletTemp + std::abs(MyLoad) / (cp * mdotSupSide);

                                    FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop);
                                } else { // no flow on supply side so do not request flow on demand side
                                    mdotDmdSide = 0.0;
                                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                                         FluidHX(CompNum).DemandSideLoop.compNum);
                                }
                            } else { // not able to heat so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).DemandSideLoop.compNum);
                            }
                        }

                    } else { //  no load
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        mdotDmdSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == OperationSchemeOnOff) {
                    if (std::abs(MyLoad) > DataHVACGlobals::SmallLoad) {
                        if (MyLoad < DataHVACGlobals::SmallLoad) { // requesting cooling
                            Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                            if (DeltaTCooling > FluidHX(CompNum).TempControlTol) { // can do cooling so turn on
                                mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }

                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).DemandSideLoop.compNum);
                            } else { // not able to cool so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).DemandSideLoop.compNum);
                            }

                        } else { // requesting heating
                            Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                            if (DeltaTHeating > FluidHX(CompNum).TempControlTol) { // can do heating so turn on
                                mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).DemandSideLoop.compNum);
                            } else { // not able to heat so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                     FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopNum,
                                                     FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                                     FluidHX(CompNum).SupplySideLoop.branchNum,
                                                     FluidHX(CompNum).SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                     FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopNum,
                                                     FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                     FluidHX(CompNum).DemandSideLoop.branchNum,
                                                     FluidHX(CompNum).DemandSideLoop.compNum);
                            }
                        }

                    } else { // no load
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        mdotDmdSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == HeatingSetPointModulated) {

                    Real64 SetPointTemp = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                    if ((DeltaTHeating > FluidHX(CompNum).TempControlTol) && (SetPointTemp > FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {

                            Real64 TargetLeavingTemp = SetPointTemp;
                            FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                 FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                 FluidHX(CompNum).DemandSideLoop.branchNum,
                                                 FluidHX(CompNum).DemandSideLoop.compNum);
                        }
                    } else { // not able are wanting to heat so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == HeatingSetPointOnOff) {

                    Real64 SetPointTemp = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                    if ((DeltaTHeating > FluidHX(CompNum).TempControlTol) && (SetPointTemp > FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    } else { // not able or are wanting to heat so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointModulated) {

                    Real64 SetPointTemp = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                    if ((DeltaTCooling > FluidHX(CompNum).TempControlTol) && (SetPointTemp < FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTemp;
                            FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                 FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                 FluidHX(CompNum).DemandSideLoop.branchNum,
                                                 FluidHX(CompNum).DemandSideLoop.compNum);
                        }
                    } else { // not able or are wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointOnOff) {

                    Real64 SetPointTemp = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                    if ((DeltaTCooling > FluidHX(CompNum).TempControlTol) && (SetPointTemp < FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    } else { // not able or are wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == DualDeadBandSetPointModulated) {

                    Real64 SetPointTempLo = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPointLo;
                    Real64 SetPointTempHi = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPointHi;
                    Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                    Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                    if ((DeltaTCooling > FluidHX(CompNum).TempControlTol) && (SetPointTempHi < FluidHX(CompNum).SupplySideLoop.InletTemp)) {

                        // can and want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTempHi;
                            FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                 FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                 FluidHX(CompNum).DemandSideLoop.branchNum,
                                                 FluidHX(CompNum).DemandSideLoop.compNum);
                        }
                    } else if ((DeltaTHeating > FluidHX(CompNum).TempControlTol) && (SetPointTempLo > FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTempLo;
                            FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                 FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopNum,
                                                 FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                                 FluidHX(CompNum).DemandSideLoop.branchNum,
                                                 FluidHX(CompNum).DemandSideLoop.compNum);
                        }
                    } else { // not able or don't want conditioning
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == DualDeadBandSetPointOnOff) {

                    Real64 SetPointTempLo = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPointLo;
                    Real64 SetPointTempHi = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPointHi;
                    Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                    Real64 DeltaTHeating = FluidHX(CompNum).DemandSideLoop.InletTemp - FluidHX(CompNum).SupplySideLoop.InletTemp;
                    if ((DeltaTCooling > FluidHX(CompNum).TempControlTol) && (SetPointTempHi < FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    } else if ((DeltaTHeating > FluidHX(CompNum).TempControlTol) && (SetPointTempLo > FluidHX(CompNum).SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    } else { // not able or don't want conditioning
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingDifferentialOnOff) {

                    Real64 DeltaTCooling = FluidHX(CompNum).SupplySideLoop.InletTemp - FluidHX(CompNum).DemandSideLoop.InletTemp;
                    if (DeltaTCooling > FluidHX(CompNum).TempControlTol) {
                        //  want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    } else { // not wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointOnOffWithComponentOverride) {

                    {
                        auto const SELECT_CASE_var1(FluidHX(CompNum).ControlSignalTemp);
                        if (SELECT_CASE_var1 == WetBulbTemperature) {
                            ControlSignalValue = DataEnvironment::OutWetBulbTemp;
                        } else if (SELECT_CASE_var1 == DryBulbTemperature) {
                            ControlSignalValue = DataEnvironment::OutDryBulbTemp;
                        } else if (SELECT_CASE_var1 == LoopTemperature) {
                            // ControlSignalValue = FluidHX(CompNum)%DemandSideLoop%InletTemp
                            ControlSignalValue = DataLoopNode::Node(FluidHX(CompNum).OtherCompDemandSideLoop.inletNodeNum).TempLastTimestep;
                        } else {
                            assert(false);
                        }
                    }

                    Real64 SetPointTemp = DataLoopNode::Node(FluidHX(CompNum).SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = SetPointTemp - ControlSignalValue;
                    // obtain shut down state
                    bool ChillerShutDown = DataPlant::PlantLoop(FluidHX(CompNum).OtherCompSupplySideLoop.loopNum)
                                          .LoopSide(FluidHX(CompNum).OtherCompSupplySideLoop.loopSideNum)
                                          .Branch(FluidHX(CompNum).OtherCompSupplySideLoop.branchNum)
                                          .Comp(FluidHX(CompNum).OtherCompSupplySideLoop.compNum)
                                          .FreeCoolCntrlShutDown;
                    if (ChillerShutDown && (DeltaTCooling > FluidHX(CompNum).TempControlTol)) {
                        // can and want to cool
                        mdotSupSide = FluidHX(CompNum).SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);

                    } else {
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                             FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                             FluidHX(CompNum).SupplySideLoop.loopNum,
                                             FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                             FluidHX(CompNum).SupplySideLoop.branchNum,
                                             FluidHX(CompNum).SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                             FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                             FluidHX(CompNum).DemandSideLoop.loopNum,
                                             FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                             FluidHX(CompNum).DemandSideLoop.branchNum,
                                             FluidHX(CompNum).DemandSideLoop.compNum);
                    }
                }
            }

        } else { // scheduled off
            mdotSupSide = 0.0;
            PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                 FluidHX(CompNum).SupplySideLoop.inletNodeNum,
                                 FluidHX(CompNum).SupplySideLoop.outletNodeNum,
                                 FluidHX(CompNum).SupplySideLoop.loopNum,
                                 FluidHX(CompNum).SupplySideLoop.loopSideNum,
                                 FluidHX(CompNum).SupplySideLoop.branchNum,
                                 FluidHX(CompNum).SupplySideLoop.compNum);
            mdotDmdSide = 0.0;
            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                 FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                 FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                 FluidHX(CompNum).DemandSideLoop.loopNum,
                                 FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                 FluidHX(CompNum).DemandSideLoop.branchNum,
                                 FluidHX(CompNum).DemandSideLoop.compNum);
        }
    }

    void CalcFluidHeatExchanger(int const CompNum,
                                Real64 const SupSideMdot, // mass flow rate of fluid entering from supply side loop
                                Real64 const DmdSideMdot  // mass flow rate of fluid entering from demand side loop
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith, derived from CalcEconHeatExchanger by  Sankaranarayanan K P aug. 2007
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Evalutate heat exchanger model and calculate leaving temperatures

        // METHODOLOGY EMPLOYED:
        // apply heat transfer model depending on type of HX used

        static std::string const RoutineName("CalcFluidHeatExchanger");

        int const CmaxMixedCminUnmixed(40);
        int const CmaxUnMixedCminMixed(41);

        Real64 Effectiveness(0.0);

        Real64 SupSideLoopInletTemp = DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).Temp;
        Real64 DmdSideLoopInletTemp = DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.inletNodeNum).Temp;

        // specific heat of fluid entering from supply side loop at inlet temp
        Real64 SupSideLoopInletCp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidName,
                                                   SupSideLoopInletTemp,
                                                   DataPlant::PlantLoop(FluidHX(CompNum).SupplySideLoop.loopNum).FluidIndex,
                                                   RoutineName);

        // specific heat of fluid entering from demand side loop at inlet temp
        Real64 DmdSideLoopInletCp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidName,
                                                   DmdSideLoopInletTemp,
                                                   DataPlant::PlantLoop(FluidHX(CompNum).DemandSideLoop.loopNum).FluidIndex,
                                                   RoutineName);

        Real64 SupSideCapRate = SupSideMdot * SupSideLoopInletCp;
        Real64 DmdSideCapRate = DmdSideMdot * DmdSideLoopInletCp;
        Real64 MinCapRate = min(SupSideCapRate, DmdSideCapRate);
        Real64 MaxCapRate = max(SupSideCapRate, DmdSideCapRate);

        if (MinCapRate > 0.0) {

            {
                auto const SELECT_CASE_var(FluidHX(CompNum).HeatExchangeModelType);

                if (SELECT_CASE_var == CrossFlowBothUnMixed) {
                    Real64 NTU = FluidHX(CompNum).UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = std::pow(NTU, 0.22) / CapRatio;
                    Real64 ExpCheckValue2 = -CapRatio * std::pow(NTU, 0.78);
                    if ((ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) || (ExpCheckValue2 > DataPrecisionGlobals::EXP_UpperLimit)) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 1.0 - std::exp(-NTU);
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = 1.0;
                        }
                    } else {
                        Effectiveness = 1.0 - std::exp((std::pow(NTU, 0.22) / CapRatio) * (std::exp(-CapRatio * std::pow(NTU, 0.78)) - 1.0));
                        Effectiveness = min(1.0, Effectiveness);
                    }

                } else if (SELECT_CASE_var == CrossFlowBothMixed) {
                    Real64 NTU = FluidHX(CompNum).UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -CapRatio * NTU;
                    Real64 ExpCheckValue2 = -NTU;
                    if (ExpCheckValue1 < DataPrecisionGlobals::EXP_LowerLimit) {
                        if (ExpCheckValue2 >= DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 1.0 - std::exp(-NTU);
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = 1.0;
                        }
                    } else if (ExpCheckValue2 < DataPrecisionGlobals::EXP_LowerLimit) {
                        Effectiveness = 1.0;
                    } else if ((std::exp(-NTU) == 1.0) || (NTU == 0.0) || (std::exp(-CapRatio * NTU) == 1.0)) { // don't div by zero

                        Effectiveness = 0.0;
                    } else {
                        Effectiveness = 1.0 / ((1.0 / (1.0 - std::exp(-NTU))) + (CapRatio / (1.0 - std::exp(-CapRatio * NTU))) - (1.0 / NTU));
                        Effectiveness = min(1.0, Effectiveness);
                    }

                } else if ((SELECT_CASE_var == CrossFlowSupplyLoopMixedDemandLoopUnMixed) ||
                           (SELECT_CASE_var == CrossFlowSupplyLoopUnMixedDemandLoopMixed)) {

                    int CrossFlowEquation;
                    if (SupSideCapRate == MaxCapRate && FluidHX(CompNum).HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed) {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    } else if (SupSideCapRate == MinCapRate && FluidHX(CompNum).HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed) {
                        CrossFlowEquation = CmaxUnMixedCminMixed;
                    } else if (DmdSideCapRate == MaxCapRate && FluidHX(CompNum).HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed) {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    } else if (DmdSideCapRate == MinCapRate && FluidHX(CompNum).HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed) {
                        CrossFlowEquation = CmaxUnMixedCminMixed;
                    } else {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    }

                    Real64 NTU = FluidHX(CompNum).UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    if (CrossFlowEquation == CmaxMixedCminUnmixed) {
                        Real64 ExpCheckValue1 = -NTU;
                        if (CapRatio == 0.0) { // protect div by zero
                            if (ExpCheckValue1 >= DataPrecisionGlobals::EXP_LowerLimit) {
                                Effectiveness = 1.0 - std::exp(-NTU);
                                Effectiveness = min(1.0, Effectiveness);
                            } else {
                                Effectiveness = 1.0;
                            }
                        } else if (ExpCheckValue1 < DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 0.632 / CapRatio;
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = (1.0 / CapRatio) * (1.0 - std::exp(CapRatio * std::exp(-NTU) - 1.0));
                            Effectiveness = min(1.0, Effectiveness);
                        }
                    } else if (CrossFlowEquation == CmaxUnMixedCminMixed) {
                        Real64 ExpCheckValue1 = -CapRatio * NTU;
                        if (CapRatio == 0.0) {
                            if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                                Effectiveness = 1.0 - std::exp(-NTU);
                                Effectiveness = min(1.0, Effectiveness);
                            } else {
                                Effectiveness = 1.0;
                            }
                        } else {
                            if (ExpCheckValue1 >= DataPrecisionGlobals::EXP_LowerLimit) {
                                Real64 ExpCheckValue2 = -(1.0 / CapRatio) * (1.0 - std::exp(-CapRatio * NTU));
                                if (ExpCheckValue2 < DataPrecisionGlobals::EXP_LowerLimit) {
                                    Effectiveness = 1.0;
                                } else {
                                    Effectiveness = 1.0 - std::exp(ExpCheckValue2);
                                    Effectiveness = min(1.0, Effectiveness);
                                }
                            } else {
                                Effectiveness = 1.0;
                            }
                        }
                    } else {
                        assert(false);
                    }

                } else if (SELECT_CASE_var == CounterFlow) {
                    Real64 NTU = FluidHX(CompNum).UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -NTU * (1.0 - CapRatio);
                    if (ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 1.0 - std::exp(-NTU);
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = 1.0;
                        }
                    } else if (CapRatio * std::exp(-NTU * (1.0 - CapRatio)) == 1.0) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 1.0 - std::exp(-NTU);
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = 1.0;
                        }
                    } else {
                        Effectiveness = (1.0 - std::exp(-NTU * (1.0 - CapRatio))) / (1.0 - CapRatio * std::exp(-NTU * (1.0 - CapRatio)));
                        Effectiveness = min(1.0, Effectiveness);
                    }

                } else if (SELECT_CASE_var == ParallelFlow) {
                    Real64 NTU = FluidHX(CompNum).UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -NTU * (1.0 + CapRatio);
                    if (ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            Effectiveness = 1.0 - std::exp(-NTU);
                            Effectiveness = min(1.0, Effectiveness);
                        } else {
                            Effectiveness = 1.0;
                        }
                    } else {
                        Effectiveness = (1.0 - std::exp(-NTU * (1.0 + CapRatio))) / (1.0 + CapRatio);
                        Effectiveness = min(1.0, Effectiveness);
                    }

                } else if (SELECT_CASE_var == Ideal) {
                    Effectiveness = 1.0;
                } else {
                    assert(false);
                }
            }

        } else { // no capacity
            Effectiveness = 0.0;
        }

        Real64 HeatTransferRate = Effectiveness * MinCapRate * (SupSideLoopInletTemp - DmdSideLoopInletTemp); // + means supply side is cooled

        if (SupSideMdot > 0.0) {
            FluidHX(CompNum).SupplySideLoop.OutletTemp = SupSideLoopInletTemp - HeatTransferRate / (SupSideLoopInletCp * SupSideMdot);
        } else {
            FluidHX(CompNum).SupplySideLoop.OutletTemp = SupSideLoopInletTemp;
        }

        if (DmdSideMdot > 0.0) {
            FluidHX(CompNum).DemandSideLoop.OutletTemp = DmdSideLoopInletTemp + HeatTransferRate / (DmdSideLoopInletCp * DmdSideMdot);
        } else {
            FluidHX(CompNum).DemandSideLoop.OutletTemp = DmdSideLoopInletTemp;
        }
        FluidHX(CompNum).Effectiveness = Effectiveness;
        FluidHX(CompNum).HeatTransferRate = HeatTransferRate;
        FluidHX(CompNum).SupplySideLoop.InletTemp = SupSideLoopInletTemp;
        FluidHX(CompNum).SupplySideLoop.InletMassFlowRate = SupSideMdot;
        FluidHX(CompNum).DemandSideLoop.InletTemp = DmdSideLoopInletTemp;
        FluidHX(CompNum).DemandSideLoop.InletMassFlowRate = DmdSideMdot;
    }

    void FindHXDemandSideLoopFlow(int const CompNum, Real64 const TargetSupplySideLoopLeavingTemp, int const HXActionMode)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // modulate demand side flow rate to hit a target leaving temperature (within tolerance)

        // METHODOLOGY EMPLOYED:
        // uses E+'s Regula Falsi numerical method
        
        int const MaxIte(500);   // Maximum number of iterations for solver
        Real64 const Acc(1.e-3); // Accuracy of solver result

        int SolFla;             // Flag of solver
        Array1D<Real64> Par(2); // Parameter array passed to solver

        // mass flow rate of fluid entering from supply side loop
        Real64 SupSideMdot = DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).MassFlowRate;
        // first see if root is bracketed
        // min demand flow

        // mass flow rate of fluid entering from demand side loop
        Real64 DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMin;
        CalcFluidHeatExchanger(CompNum, SupSideMdot, DmdSideMdot);
        Real64 LeavingTempMinFlow = FluidHX(CompNum).SupplySideLoop.OutletTemp;

        // full demand flow
        DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
        CalcFluidHeatExchanger(CompNum, SupSideMdot, DmdSideMdot);
        Real64 LeavingTempFullFlow = FluidHX(CompNum).SupplySideLoop.OutletTemp;

        {
            auto const SELECT_CASE_var(HXActionMode);

            if (SELECT_CASE_var == HeatingSupplySideLoop) {
                if ((LeavingTempFullFlow > TargetSupplySideLoopLeavingTemp) && (TargetSupplySideLoopLeavingTemp > LeavingTempMinFlow)) {
                    // need to solve
                    Par(1) = double(CompNum); // HX index
                    Par(2) = TargetSupplySideLoopLeavingTemp;

                    General::SolveRoot(Acc,
                              MaxIte,
                              SolFla,
                              DmdSideMdot,
                              HXDemandSideLoopFlowResidual,
                              FluidHX(CompNum).DemandSideLoop.MassFlowRateMin,
                              FluidHX(CompNum).DemandSideLoop.MassFlowRateMax,
                              Par);

                    if (SolFla == -1) { // no convergence
                        if (!DataGlobals::WarmupFlag) {
                            if (FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorCount < 1) {
                                ++FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorCount;
                                ShowWarningError(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                 " - Iteration Limit exceeded calculating demand side loop flow rate");
                                ShowContinueError("Simulation continues with calculated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                               " - Iteration Limit exceeded calculating demand side loop flow rate continues.",
                                                           FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    } else if (SolFla == -2) { // f(x0) and f(x1) have the same sign
                        DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax * (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) /
                                      (LeavingTempFullFlow - LeavingTempMinFlow);
                        if (!DataGlobals::WarmupFlag) {
                            if (FluidHX(CompNum).DmdSideModulatSolvFailErrorCount < 1) {
                                ++FluidHX(CompNum).DmdSideModulatSolvFailErrorCount;
                                ShowWarningError(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                 " - Solver failed to calculate demand side loop flow rate");
                                ShowContinueError("Simulation continues with estimated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                               " - Solver failed to calculate demand side loop flow rate continues.",
                                                           FluidHX(CompNum).DmdSideModulatSolvFailErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    }
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);

                } else if ((TargetSupplySideLoopLeavingTemp >= LeavingTempFullFlow) && (LeavingTempFullFlow > LeavingTempMinFlow)) {
                    // run at full flow
                    DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);

                } else if (LeavingTempMinFlow >= TargetSupplySideLoopLeavingTemp) {

                    // run at min flow
                    DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMin;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);
                }
            } else if (SELECT_CASE_var == CoolingSupplySideLoop) {
                if ((LeavingTempFullFlow < TargetSupplySideLoopLeavingTemp) && (TargetSupplySideLoopLeavingTemp < LeavingTempMinFlow)) {
                    // need to solve
                    Par(1) = double(CompNum); // HX index
                    Par(2) = TargetSupplySideLoopLeavingTemp;

                    General::SolveRoot(Acc,
                              MaxIte,
                              SolFla,
                              DmdSideMdot,
                              HXDemandSideLoopFlowResidual,
                              FluidHX(CompNum).DemandSideLoop.MassFlowRateMin,
                              FluidHX(CompNum).DemandSideLoop.MassFlowRateMax,
                              Par);

                    if (SolFla == -1) { // no convergence
                        if (!DataGlobals::WarmupFlag) {
                            if (FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorCount < 1) {
                                ++FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorCount;
                                ShowWarningError(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                 " - Iteration Limit exceeded calculating demand side loop flow rate");
                                ShowContinueError("Simulation continues with calculated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                               " - Iteration Limit exceeded calculating demand side loop flow rate continues.",
                                                           FluidHX(CompNum).DmdSideModulatSolvNoConvergeErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    } else if (SolFla == -2) { // f(x0) and f(x1) have the same sign
                        DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax * (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) /
                                      (LeavingTempFullFlow - LeavingTempMinFlow);
                        if (!DataGlobals::WarmupFlag) {
                            if (FluidHX(CompNum).DmdSideModulatSolvFailErrorCount < 1) {
                                ++FluidHX(CompNum).DmdSideModulatSolvFailErrorCount;
                                ShowWarningError(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                 " - Solver failed to calculate demand side loop flow rate");
                                ShowContinueError("Simulation continues with estimated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + FluidHX(CompNum).Name +
                                                               " - Solver failed to calculate demand side loop flow rate continues.",
                                                           FluidHX(CompNum).DmdSideModulatSolvFailErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    }
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);
                } else if ((TargetSupplySideLoopLeavingTemp <= LeavingTempFullFlow) && (LeavingTempFullFlow < LeavingTempMinFlow)) {
                    // run at full flow
                    DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);
                } else if (LeavingTempMinFlow <= TargetSupplySideLoopLeavingTemp) {

                    // run at min flow
                    DmdSideMdot = FluidHX(CompNum).DemandSideLoop.MassFlowRateMin;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                         FluidHX(CompNum).DemandSideLoop.inletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.outletNodeNum,
                                         FluidHX(CompNum).DemandSideLoop.loopNum,
                                         FluidHX(CompNum).DemandSideLoop.loopSideNum,
                                         FluidHX(CompNum).DemandSideLoop.branchNum,
                                         FluidHX(CompNum).DemandSideLoop.compNum);
                }
            }
        }
    }

    Real64 HXDemandSideLoopFlowResidual(Real64 const DmdSideMassFlowRate,
                                        Array1<Real64> const &Par // Par(1) = HX index number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   December 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // calculate residual value for regula falsi solver
        
        Real64 Residuum; // Residual to be minimized to zero

        Real64 MdotTrial = DmdSideMassFlowRate;
        int CompNum = int(Par(1));
        Real64 SupSideMdot = DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.inletNodeNum).MassFlowRate;

        CalcFluidHeatExchanger(CompNum, SupSideMdot, MdotTrial);

        Real64 SupSideLoopOutletTemp = FluidHX(CompNum).SupplySideLoop.OutletTemp;

        Residuum = Par(2) - SupSideLoopOutletTemp;

        return Residuum;
    }

    void UpdateFluidHeatExchanger(int const CompNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   December 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update calculate results

        DataLoopNode::Node(FluidHX(CompNum).DemandSideLoop.outletNodeNum).Temp = FluidHX(CompNum).DemandSideLoop.OutletTemp;
        DataLoopNode::Node(FluidHX(CompNum).SupplySideLoop.outletNodeNum).Temp = FluidHX(CompNum).SupplySideLoop.OutletTemp;
    }

    void ReportFluidHeatExchanger(int const CompNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   December, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update heat exchanger report variables

        FluidHX(CompNum).HeatTransferEnergy = FluidHX(CompNum).HeatTransferRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if ((std::abs(FluidHX(CompNum).HeatTransferRate) > DataHVACGlobals::SmallLoad) && (FluidHX(CompNum).DemandSideLoop.InletMassFlowRate > 0.0) &&
            (FluidHX(CompNum).SupplySideLoop.InletMassFlowRate > 0.0)) {
            FluidHX(CompNum).OperationStatus = 1.0;
        } else {
            FluidHX(CompNum).OperationStatus = 0.0;
        }
    }

} // namespace PlantHeatExchangerFluidToFluid

} // namespace EnergyPlus
