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

    std::string const ComponentClassName("HeatExchanger:FluidToFluid");

    int NumberOfPlantFluidHXs(0);
    bool GetInput(true);
    Array1D<HeatExchangerStruct> FluidHX;
    Array1D_bool CheckFluidHXs;

    void clear_state()
    {
        NumberOfPlantFluidHXs = 0;
        GetInput = true;
        FluidHX.deallocate();
        CheckFluidHXs.deallocate();
    }

    PlantComponent *HeatExchangerStruct::factory(std::string const &objectName)
    {
        // Process the input data for heat exchangers if it hasn't been done already
        if (GetInput) {
            GetFluidHeatExchangerInput();
            GetInput = false;
        }
        // Now look for this particular object
        for (auto &obj : FluidHX) {
            if (obj.Name == objectName) {
                return &obj;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalPlantFluidHXFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void HeatExchangerStruct::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation))
    {
        this->initialize();
    }

    void HeatExchangerStruct::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        if (calledFromLocation.loopNum == this->DemandSideLoop.loopNum) {
            MinLoad = 0.0;
            MaxLoad = this->DemandSideLoop.MaxLoad;
            OptLoad = this->DemandSideLoop.MaxLoad * 0.9;
        } else if (calledFromLocation.loopNum == this->SupplySideLoop.loopNum) {
            this->size(); // only call sizing from the loop that sizes are based on
            MinLoad = 0.0;
            MaxLoad = this->SupplySideLoop.MaxLoad;
            OptLoad = this->SupplySideLoop.MaxLoad * 0.9;
        }
    }

    void HeatExchangerStruct::simulate(const PlantLocation &calledFromLocation,
                                       bool const FirstHVACIteration,
                                       Real64 &CurLoad,
                                       bool const EP_UNUSED(RunFlag))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Main entry point and simulation manager for heat exchanger

        this->initialize();

        // for op scheme led HXs, only call controls if called from Loop Supply Side
        if ((this->ControlMode == OperationSchemeModulated) || (this->ControlMode == OperationSchemeOnOff)) {
            if (calledFromLocation.loopNum == this->SupplySideLoop.loopNum) {
                this->control(calledFromLocation.loopNum, CurLoad, FirstHVACIteration);
            }
        } else {
            this->control(calledFromLocation.loopNum, CurLoad, FirstHVACIteration);
        }

        this->calculate(DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).MassFlowRate,
                        DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).MassFlowRate);
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
        int NumAlphas;        // Number of elements in the alpha array
        int NumNums;          // Number of elements in the numeric array
        int IOStat;           // IO Status when calling get input subroutine
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

                FluidHX(CompLoop).DemandSideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(3),
                                                                                                    ErrorsFound,
                                                                                                    cCurrentModuleObject,
                                                                                                    cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeType_Water,
                                                                                                    DataLoopNode::NodeConnectionType_Inlet,
                                                                                                    1,
                                                                                                    DataLoopNode::ObjectIsNotParent);
                FluidHX(CompLoop).DemandSideLoop.outletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(4),
                                                                                                     ErrorsFound,
                                                                                                     cCurrentModuleObject,
                                                                                                     cAlphaArgs(1),
                                                                                                     DataLoopNode::NodeType_Water,
                                                                                                     DataLoopNode::NodeConnectionType_Outlet,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Loop Demand Side Plant Nodes");
                FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRate = rNumericArgs(1);
                if (FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRate == DataSizing::AutoSize) {
                    FluidHX(CompLoop).DemandSideLoop.DesignVolumeFlowRateWasAutoSized = true;
                }

                FluidHX(CompLoop).SupplySideLoop.inletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(5),
                                                                                                    ErrorsFound,
                                                                                                    cCurrentModuleObject,
                                                                                                    cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeType_Water,
                                                                                                    DataLoopNode::NodeConnectionType_Inlet,
                                                                                                    2,
                                                                                                    DataLoopNode::ObjectIsNotParent);
                FluidHX(CompLoop).SupplySideLoop.outletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(6),
                                                                                                     ErrorsFound,
                                                                                                     cCurrentModuleObject,
                                                                                                     cAlphaArgs(1),
                                                                                                     DataLoopNode::NodeType_Water,
                                                                                                     DataLoopNode::NodeConnectionType_Outlet,
                                                                                                     2,
                                                                                                     DataLoopNode::ObjectIsNotParent);
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
                                EMSManager::CheckIfNodeSetPointManagedByEMS(
                                    FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureSetPoint, NodeEMSSetPointMissing);
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
                                EMSManager::CheckIfNodeSetPointManagedByEMS(
                                    FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureMinSetPoint, NodeEMSSetPointMissing);
                                EMSManager::CheckIfNodeSetPointManagedByEMS(
                                    FluidHX(CompLoop).SetPointNodeNum, EMSManager::iTemperatureMaxSetPoint, NodeEMSSetPointMissing);
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
                    FluidHX(CompLoop).OtherCompSupplySideLoop.inletNodeNum =
                        NodeInputManager::GetOnlySingleNode(cAlphaArgs(11),
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
                    FluidHX(CompLoop).OtherCompDemandSideLoop.inletNodeNum =
                        NodeInputManager::GetOnlySingleNode(cAlphaArgs(12),
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
    }

    void HeatExchangerStruct::setupOutputVars()
    {
        SetupOutputVariable(
            "Fluid Heat Exchanger Heat Transfer Rate", OutputProcessor::Unit::W, this->HeatTransferRate, "System", "Average", this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->HeatTransferEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            this->HeatTransferMeteringEndUse,
                            _,
                            "Plant");

        SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->SupplySideLoop.InletMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->SupplySideLoop.InletTemp,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Loop Supply Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->SupplySideLoop.OutletTemp,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->DemandSideLoop.InletMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->DemandSideLoop.InletTemp,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Loop Demand Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->DemandSideLoop.OutletTemp,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            "Fluid Heat Exchanger Operation Status", OutputProcessor::Unit::None, this->OperationStatus, "System", "Average", this->Name);

        SetupOutputVariable("Fluid Heat Exchanger Effectiveness", OutputProcessor::Unit::None, this->Effectiveness, "System", "Average", this->Name);
    }

    void HeatExchangerStruct::initialize()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   november, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initialize heat exchanger model

        static std::string const RoutineNameNoColon("InitFluidHeatExchanger");
        static std::string const RoutineName("InitFluidHeatExchanger: ");

        if (this->MyOneTimeFlag) {
            this->setupOutputVars();
            this->MyFlag = true;
            this->MyEnvrnFlag = true;
            this->MyOneTimeFlag = false;
        }

        if (this->MyFlag) {
            // locate the main two connections to the plant loops
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                                    this->DemandSideLoop.loopNum,
                                                    this->DemandSideLoop.loopSideNum,
                                                    this->DemandSideLoop.branchNum,
                                                    this->DemandSideLoop.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->DemandSideLoop.inletNodeNum,
                                                    _);

            if (this->DemandSideLoop.loopSideNum != DataPlant::DemandSide) { // throw error
                ShowSevereError(RoutineName + " Invalid connections for " +
                                DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" + this->Name + "\"");
                ShowContinueError("The \"Loop Demand Side\" connections are not on the Demand Side of a plant loop");
                errFlag = true;
            }

            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                                    this->SupplySideLoop.loopNum,
                                                    this->SupplySideLoop.loopSideNum,
                                                    this->SupplySideLoop.branchNum,
                                                    this->SupplySideLoop.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->SupplySideLoop.inletNodeNum,
                                                    _);

            if (this->SupplySideLoop.loopSideNum != DataPlant::SupplySide) { // throw error
                ShowSevereError(RoutineName + " Invalid connections for " +
                                DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" + this->Name + "\"");
                ShowContinueError("The \"Loop Supply Side\" connections are not on the Supply Side of a plant loop");
                errFlag = true;
            }

            // make sure it is not the same loop on both sides.
            if (this->SupplySideLoop.loopNum == this->DemandSideLoop.loopNum) { // user is being too tricky, don't allow
                ShowSevereError(RoutineName + " Invalid connections for " +
                                DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_FluidToFluidPlantHtExchg) + " name = \"" + this->Name + "\"");
                ShowContinueError(R"(The "Loop Supply Side" and "Loop Demand Side" need to be on different loops.)");
                errFlag = true;
            } else {

                PlantUtilities::InterConnectTwoPlantLoopSides(this->SupplySideLoop.loopNum,
                                                              this->SupplySideLoop.loopSideNum,
                                                              this->DemandSideLoop.loopNum,
                                                              this->DemandSideLoop.loopSideNum,
                                                              DataPlant::TypeOf_FluidToFluidPlantHtExchg,
                                                              true);
            }

            // find remote component if control mode is of that type.
            if (this->ControlMode == CoolingSetPointOnOffWithComponentOverride) {

                PlantUtilities::ScanPlantLoopsForNodeNum(RoutineName,
                                                         this->OtherCompSupplySideLoop.inletNodeNum,
                                                         this->OtherCompSupplySideLoop.loopNum,
                                                         this->OtherCompSupplySideLoop.loopSideNum,
                                                         this->OtherCompSupplySideLoop.branchNum,
                                                         this->OtherCompSupplySideLoop.compNum);

                PlantUtilities::ScanPlantLoopsForNodeNum(RoutineName,
                                                         this->OtherCompDemandSideLoop.inletNodeNum,
                                                         this->OtherCompDemandSideLoop.loopNum,
                                                         this->OtherCompDemandSideLoop.loopSideNum,
                                                         this->OtherCompDemandSideLoop.branchNum,
                                                         this->OtherCompDemandSideLoop.compNum);

                // revise how loads served category for other controlled equipment
                int LoopNum2 = this->OtherCompSupplySideLoop.loopNum;
                int LoopSideNum = this->OtherCompSupplySideLoop.loopSideNum;
                int BranchNum = this->OtherCompSupplySideLoop.branchNum;
                int LoopCompNum = this->OtherCompSupplySideLoop.compNum;

                {
                    auto const SELECT_CASE_var(
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed);

                    if (SELECT_CASE_var == DataPlant::HowMet_ByNominalCap) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed =
                            DataPlant::HowMet_ByNominalCapFreeCoolCntrl;
                    } else if (SELECT_CASE_var == DataPlant::HowMet_ByNominalCapLowOutLimit) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).HowLoadServed =
                            DataPlant::HowMet_ByNominalCapLowOutLimitFreeCoolCntrl;
                    }
                }

                {
                    auto const SELECT_CASE_var(this->ControlSignalTemp);
                    if (SELECT_CASE_var == WetBulbTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode =
                            DataPlant::FreeCoolControlMode_WetBulb;
                    } else if (SELECT_CASE_var == DryBulbTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode =
                            DataPlant::FreeCoolControlMode_DryBulb;
                    } else if (SELECT_CASE_var == LoopTemperature) {
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMode =
                            DataPlant::FreeCoolControlMode_Loop;
                        DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlNodeNum =
                            this->OtherCompDemandSideLoop.inletNodeNum;
                    }
                }
            }
            if (this->ControlMode == TrackComponentOnOff) {
                if (this->OtherCompSupplySideLoop.inletNodeNum > 0) {
                    PlantUtilities::ScanPlantLoopsForObject(this->ComponentUserName,
                                                            this->ComponentTypeOfNum,
                                                            this->OtherCompSupplySideLoop.loopNum,
                                                            this->OtherCompSupplySideLoop.loopSideNum,
                                                            this->OtherCompSupplySideLoop.branchNum,
                                                            this->OtherCompSupplySideLoop.compNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            this->OtherCompSupplySideLoop.inletNodeNum,
                                                            _);
                }
                if (this->OtherCompDemandSideLoop.inletNodeNum > 0) {
                    PlantUtilities::ScanPlantLoopsForObject(this->ComponentUserName,
                                                            this->ComponentTypeOfNum,
                                                            this->OtherCompDemandSideLoop.loopNum,
                                                            this->OtherCompDemandSideLoop.loopSideNum,
                                                            this->OtherCompDemandSideLoop.branchNum,
                                                            this->OtherCompDemandSideLoop.compNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            this->OtherCompDemandSideLoop.inletNodeNum,
                                                            _);
                }
            }

            if (errFlag) {
                ShowFatalError(RoutineName + "Program terminated due to previous condition(s).");
            }
            this->MyFlag = false;
        } // plant setup

        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidName,
                                                           DataGlobals::InitConvTemp,
                                                           DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidIndex,
                                                           RoutineNameNoColon);
            this->DemandSideLoop.MassFlowRateMax = rho * this->DemandSideLoop.DesignVolumeFlowRate;
            PlantUtilities::InitComponentNodes(this->DemandSideLoop.MassFlowRateMin,
                                               this->DemandSideLoop.MassFlowRateMax,
                                               this->DemandSideLoop.inletNodeNum,
                                               this->DemandSideLoop.outletNodeNum,
                                               this->DemandSideLoop.loopNum,
                                               this->DemandSideLoop.loopSideNum,
                                               this->DemandSideLoop.branchNum,
                                               this->DemandSideLoop.compNum);

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                    DataGlobals::InitConvTemp,
                                                    DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                    RoutineNameNoColon);
            this->SupplySideLoop.MassFlowRateMax = rho * this->SupplySideLoop.DesignVolumeFlowRate;
            PlantUtilities::InitComponentNodes(this->SupplySideLoop.MassFlowRateMin,
                                               this->SupplySideLoop.MassFlowRateMax,
                                               this->SupplySideLoop.inletNodeNum,
                                               this->SupplySideLoop.outletNodeNum,
                                               this->SupplySideLoop.loopNum,
                                               this->SupplySideLoop.loopSideNum,
                                               this->SupplySideLoop.branchNum,
                                               this->SupplySideLoop.compNum);
            this->MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        this->DemandSideLoop.InletTemp = DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp;
        this->SupplySideLoop.InletTemp = DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp;

        if (this->ControlMode == CoolingSetPointOnOffWithComponentOverride) {
            // store current value for setpoint in central plant loop data structure
            int LoopNum2 = this->OtherCompSupplySideLoop.loopNum;
            int LoopSideNum = this->OtherCompSupplySideLoop.loopSideNum;
            int BranchNum = this->OtherCompSupplySideLoop.branchNum;
            int LoopCompNum = this->OtherCompSupplySideLoop.compNum;

            DataPlant::PlantLoop(LoopNum2).LoopSide(LoopSideNum).Branch(BranchNum).Comp(LoopCompNum).FreeCoolCntrlMinCntrlTemp =
                DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint - this->TempControlTol; // issue #5626, include control tolerance
        }
    }

    void HeatExchangerStruct::size()
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

        // first deal with Loop Supply Side
        int PltSizNumSupSide = DataPlant::PlantLoop(this->SupplySideLoop.loopNum).PlantSizNum;
        int PltSizNumDmdSide = DataPlant::PlantLoop(this->DemandSideLoop.loopNum).PlantSizNum;
        Real64 tmpSupSideDesignVolFlowRate = this->SupplySideLoop.DesignVolumeFlowRate;
        if (this->SupplySideLoop.DesignVolumeFlowRateWasAutoSized) {
            if (PltSizNumSupSide > 0) {
                if (DataSizing::PlantSizData(PltSizNumSupSide).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpSupSideDesignVolFlowRate = DataSizing::PlantSizData(PltSizNumSupSide).DesVolFlowRate * this->SizingFactor;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
                } else {
                    tmpSupSideDesignVolFlowRate = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
                }
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                                            this->Name,
                                                            "Loop Supply Side Design Fluid Flow Rate [m3/s]",
                                                            this->SupplySideLoop.DesignVolumeFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                                            this->Name,
                                                            "Initial Loop Supply Side Design Fluid Flow Rate [m3/s]",
                                                            this->SupplySideLoop.DesignVolumeFlowRate);
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeFluidHeatExchanger: Autosizing of requires a loop Sizing:Plant object");
                    ShowContinueError("Occurs in heat exchanger object=" + this->Name);
                }
            }
        }
        PlantUtilities::RegisterPlantCompDesignFlow(this->SupplySideLoop.inletNodeNum, tmpSupSideDesignVolFlowRate);

        // second deal with Loop Demand Side
        Real64 tmpDmdSideDesignVolFlowRate = this->DemandSideLoop.DesignVolumeFlowRate;
        if (this->DemandSideLoop.DesignVolumeFlowRateWasAutoSized) {
            if (tmpSupSideDesignVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                tmpDmdSideDesignVolFlowRate = tmpSupSideDesignVolFlowRate;
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
            } else {
                tmpDmdSideDesignVolFlowRate = 0.0;
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
            }
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                                        this->Name,
                                                        "Loop Demand Side Design Fluid Flow Rate [m3/s]",
                                                        this->DemandSideLoop.DesignVolumeFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatExchanger:FluidToFluid",
                                                        this->Name,
                                                        "Initial Loop Demand Side Design Fluid Flow Rate [m3/s]",
                                                        this->DemandSideLoop.DesignVolumeFlowRate);
            }
        }
        PlantUtilities::RegisterPlantCompDesignFlow(this->DemandSideLoop.inletNodeNum, tmpDmdSideDesignVolFlowRate);

        // size UA if needed
        if (this->UAWasAutoSized) {
            // get nominal delta T between two loops
            if (PltSizNumSupSide > 0 && PltSizNumDmdSide > 0) {

                Real64 tmpDeltaTloopToLoop(0.0);

                {
                    auto const SELECT_CASE_var(DataSizing::PlantSizData(PltSizNumSupSide).LoopType);

                    if ((SELECT_CASE_var == DataSizing::HeatingLoop) || (SELECT_CASE_var == DataSizing::SteamLoop)) {
                        tmpDeltaTloopToLoop =
                            std::abs((DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp - DataSizing::PlantSizData(PltSizNumSupSide).DeltaT) -
                                     DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp);
                    } else if ((SELECT_CASE_var == DataSizing::CoolingLoop) || (SELECT_CASE_var == DataSizing::CondenserLoop)) {
                        tmpDeltaTloopToLoop =
                            std::abs((DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp + DataSizing::PlantSizData(PltSizNumSupSide).DeltaT) -
                                     DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp);
                    } else {
                        assert(false);
                    }
                }

                tmpDeltaTloopToLoop = max(2.0, tmpDeltaTloopToLoop);
                Real64 tmpDeltaTSupLoop = DataSizing::PlantSizData(PltSizNumSupSide).DeltaT;
                if (tmpSupSideDesignVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                                       DataGlobals::InitConvTemp,
                                                                       DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                                       RoutineName);

                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                                   DataGlobals::InitConvTemp,
                                                                   DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                                   RoutineName);

                    Real64 tmpDesCap = Cp * rho * tmpDeltaTSupLoop * tmpSupSideDesignVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->UA = tmpDesCap / tmpDeltaTloopToLoop;
                } else {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->UA = 0.0;
                }
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "HeatExchanger:FluidToFluid", this->Name, "Heat Exchanger U-Factor Times Area Value [W/C]", this->UA);
                    ReportSizingManager::ReportSizingOutput(
                        "HeatExchanger:FluidToFluid",
                        this->Name,
                        "Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]",
                        tmpDeltaTloopToLoop);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "HeatExchanger:FluidToFluid", this->Name, "Initial Heat Exchanger U-Factor Times Area Value [W/C]", this->UA);
                    ReportSizingManager::ReportSizingOutput(
                        "HeatExchanger:FluidToFluid",
                        this->Name,
                        "Initial Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]",
                        tmpDeltaTloopToLoop);
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeFluidHeatExchanger: Autosizing of heat Exchanger UA requires a loop Sizing:Plant objects for both loops");
                    ShowContinueError("Occurs in heat exchanger object=" + this->Name);
                }
            }
        }

        // size capacities for load range based op schemes
        if (DataPlant::PlantFirstSizesOkayToFinalize) {

            if (PltSizNumSupSide > 0) {
                {
                    auto const SELECT_CASE_var(DataSizing::PlantSizData(PltSizNumSupSide).LoopType);
                    if ((SELECT_CASE_var == DataSizing::HeatingLoop) || (SELECT_CASE_var == DataSizing::SteamLoop)) {
                        DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp =
                            (DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp - DataSizing::PlantSizData(PltSizNumSupSide).DeltaT);
                    } else if ((SELECT_CASE_var == DataSizing::CoolingLoop) || (SELECT_CASE_var == DataSizing::CondenserLoop)) {
                        DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp =
                            (DataSizing::PlantSizData(PltSizNumSupSide).ExitTemp + DataSizing::PlantSizData(PltSizNumSupSide).DeltaT);
                    }
                }

            } else { // don't rely on sizing, use loop setpoints
                // loop supply side
                if (DataPlant::PlantLoop(this->SupplySideLoop.loopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (DataPlant::PlantLoop(this->SupplySideLoop.loopNum).LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp =
                        (DataLoopNode::Node(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPointHi +
                         DataLoopNode::Node(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).TempSetPointNodeNum).TempSetPointLo) /
                        2.0;
                }
            }

            if (PltSizNumDmdSide > 0) {
                DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp = DataSizing::PlantSizData(PltSizNumDmdSide).ExitTemp;
            } else { // don't rely on sizing, use loop setpoints
                // loop demand side
                if (DataPlant::PlantLoop(this->DemandSideLoop.loopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (DataPlant::PlantLoop(this->DemandSideLoop.loopNum).LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp =
                        (DataLoopNode::Node(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPointHi +
                         DataLoopNode::Node(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).TempSetPointNodeNum).TempSetPointLo) /
                        2.0;
                }
            }

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                           DataGlobals::InitConvTemp,
                                                           DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                           RoutineName);
            Real64 SupSideMdot = this->SupplySideLoop.DesignVolumeFlowRate * rho;
            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidName,
                                                    DataGlobals::InitConvTemp,
                                                    DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidIndex,
                                                    RoutineName);
            Real64 DmdSideMdot = this->DemandSideLoop.DesignVolumeFlowRate * rho;

            this->calculate(SupSideMdot, DmdSideMdot);
            this->SupplySideLoop.MaxLoad = std::abs(this->HeatTransferRate);
        }
        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "HeatExchanger:FluidToFluid");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->SupplySideLoop.MaxLoad);
        }
    }

    void HeatExchangerStruct::control(int const EP_UNUSED(LoopNum), Real64 const MyLoad, bool FirstHVACIteration)
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

        // check if available by schedule
        bool ScheduledOff;
        Real64 AvailSchedValue = ScheduleManager::GetCurrentScheduleValue(this->AvailSchedNum);
        if (AvailSchedValue <= 0) {
            ScheduledOff = true;
        } else {
            ScheduledOff = false;
        }

        // check if operational limits trip off unit
        bool LimitTrippedOff = false;
        if ((DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp < this->MinOperationTemp) ||
            (DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp < this->MinOperationTemp)) {
            LimitTrippedOff = true;
        }
        if ((DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp > this->MaxOperationTemp) ||
            (DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp > this->MaxOperationTemp)) {
            LimitTrippedOff = true;
        }

        if (!ScheduledOff && !LimitTrippedOff) {

            {
                auto const SELECT_CASE_var(this->ControlMode);

                if (SELECT_CASE_var == UncontrolledOn) {

                    // make passive request for supply side loop flow
                    mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                         this->SupplySideLoop.inletNodeNum,
                                                         this->SupplySideLoop.outletNodeNum,
                                                         this->SupplySideLoop.loopNum,
                                                         this->SupplySideLoop.loopSideNum,
                                                         this->SupplySideLoop.branchNum,
                                                         this->SupplySideLoop.compNum);
                    if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                        // if supply side loop has massflow, request demand side flow
                        mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                    } else {
                        mdotDmdSide = 0.0;
                    }
                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);

                } else if (SELECT_CASE_var == OperationSchemeModulated) {

                    if (std::abs(MyLoad) > DataHVACGlobals::SmallLoad) {
                        if (MyLoad < -1.0 * DataHVACGlobals::SmallLoad) { // requesting cooling
                            Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                            if (DeltaTCooling > this->TempControlTol) { // can do cooling so turn on
                                mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    // if supply side loop has massflow, request demand side flow
                                    Real64 cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                                                       this->SupplySideLoop.InletTemp,
                                                                                       DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                                                       RoutineName);
                                    Real64 TargetLeavingTemp = this->SupplySideLoop.InletTemp - std::abs(MyLoad) / (cp * mdotSupSide);

                                    this->findDemandSideLoopFlow(TargetLeavingTemp, CoolingSupplySideLoop);
                                } else { // no flow on supply side so do not request flow on demand side
                                    mdotDmdSide = 0.0;
                                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                         this->DemandSideLoop.inletNodeNum,
                                                                         this->DemandSideLoop.outletNodeNum,
                                                                         this->DemandSideLoop.loopNum,
                                                                         this->DemandSideLoop.loopSideNum,
                                                                         this->DemandSideLoop.branchNum,
                                                                         this->SupplySideLoop.compNum);
                                }
                            } else { // not able to cool so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                            }

                        } else { // requesting heating
                            Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                            if (DeltaTHeating > this->TempControlTol) { // can do heating so turn on
                                mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    Real64 cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                                                       this->SupplySideLoop.InletTemp,
                                                                                       DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                                                       RoutineName);
                                    Real64 TargetLeavingTemp = this->SupplySideLoop.InletTemp + std::abs(MyLoad) / (cp * mdotSupSide);

                                    this->findDemandSideLoopFlow(TargetLeavingTemp, HeatingSupplySideLoop);
                                } else { // no flow on supply side so do not request flow on demand side
                                    mdotDmdSide = 0.0;
                                    PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                         this->DemandSideLoop.inletNodeNum,
                                                                         this->DemandSideLoop.outletNodeNum,
                                                                         this->DemandSideLoop.loopNum,
                                                                         this->DemandSideLoop.loopSideNum,
                                                                         this->DemandSideLoop.branchNum,
                                                                         this->DemandSideLoop.compNum);
                                }
                            } else { // not able to heat so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->DemandSideLoop.compNum);
                            }
                        }

                    } else { //  no load
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        mdotDmdSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == OperationSchemeOnOff) {
                    if (std::abs(MyLoad) > DataHVACGlobals::SmallLoad) {
                        if (MyLoad < DataHVACGlobals::SmallLoad) { // requesting cooling
                            Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                            if (DeltaTCooling > this->TempControlTol) { // can do cooling so turn on
                                mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }

                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->DemandSideLoop.compNum);
                            } else { // not able to cool so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->DemandSideLoop.compNum);
                            }

                        } else { // requesting heating
                            Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                            if (DeltaTHeating > this->TempControlTol) { // can do heating so turn on
                                mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->DemandSideLoop.compNum);
                            } else { // not able to heat so turn off
                                mdotSupSide = 0.0;
                                PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                                     this->SupplySideLoop.inletNodeNum,
                                                                     this->SupplySideLoop.outletNodeNum,
                                                                     this->SupplySideLoop.loopNum,
                                                                     this->SupplySideLoop.loopSideNum,
                                                                     this->SupplySideLoop.branchNum,
                                                                     this->SupplySideLoop.compNum);
                                // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a
                                // fresh demand side inlet temperature value
                                if (FirstHVACIteration) {
                                    mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                                } else {
                                    mdotDmdSide = 0.0;
                                }
                                PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                     this->DemandSideLoop.inletNodeNum,
                                                                     this->DemandSideLoop.outletNodeNum,
                                                                     this->DemandSideLoop.loopNum,
                                                                     this->DemandSideLoop.loopSideNum,
                                                                     this->DemandSideLoop.branchNum,
                                                                     this->DemandSideLoop.compNum);
                            }
                        }

                    } else { // no load
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        mdotDmdSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == HeatingSetPointModulated) {

                    Real64 SetPointTemp = DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                    if ((DeltaTHeating > this->TempControlTol) && (SetPointTemp > this->SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {

                            Real64 TargetLeavingTemp = SetPointTemp;
                            this->findDemandSideLoopFlow(TargetLeavingTemp, HeatingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                 this->DemandSideLoop.inletNodeNum,
                                                                 this->DemandSideLoop.outletNodeNum,
                                                                 this->DemandSideLoop.loopNum,
                                                                 this->DemandSideLoop.loopSideNum,
                                                                 this->DemandSideLoop.branchNum,
                                                                 this->DemandSideLoop.compNum);
                        }
                    } else { // not able are wanting to heat so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == HeatingSetPointOnOff) {

                    Real64 SetPointTemp = DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                    if ((DeltaTHeating > this->TempControlTol) && (SetPointTemp > this->SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    } else { // not able or are wanting to heat so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointModulated) {

                    Real64 SetPointTemp = DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                    if ((DeltaTCooling > this->TempControlTol) && (SetPointTemp < this->SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTemp;
                            this->findDemandSideLoopFlow(TargetLeavingTemp, CoolingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                 this->DemandSideLoop.inletNodeNum,
                                                                 this->DemandSideLoop.outletNodeNum,
                                                                 this->DemandSideLoop.loopNum,
                                                                 this->DemandSideLoop.loopSideNum,
                                                                 this->DemandSideLoop.branchNum,
                                                                 this->DemandSideLoop.compNum);
                        }
                    } else { // not able or are wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointOnOff) {

                    Real64 SetPointTemp = DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                    if ((DeltaTCooling > this->TempControlTol) && (SetPointTemp < this->SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    } else { // not able or are wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == DualDeadBandSetPointModulated) {

                    Real64 SetPointTempLo = DataLoopNode::Node(this->SetPointNodeNum).TempSetPointLo;
                    Real64 SetPointTempHi = DataLoopNode::Node(this->SetPointNodeNum).TempSetPointHi;
                    Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                    Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                    if ((DeltaTCooling > this->TempControlTol) && (SetPointTempHi < this->SupplySideLoop.InletTemp)) {

                        // can and want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTempHi;
                            this->findDemandSideLoopFlow(TargetLeavingTemp, CoolingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                 this->DemandSideLoop.inletNodeNum,
                                                                 this->DemandSideLoop.outletNodeNum,
                                                                 this->DemandSideLoop.loopNum,
                                                                 this->DemandSideLoop.loopSideNum,
                                                                 this->DemandSideLoop.branchNum,
                                                                 this->DemandSideLoop.compNum);
                        }
                    } else if ((DeltaTHeating > this->TempControlTol) && (SetPointTempLo > this->SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            Real64 TargetLeavingTemp = SetPointTempLo;
                            this->findDemandSideLoopFlow(TargetLeavingTemp, HeatingSupplySideLoop);
                        } else {
                            mdotDmdSide = 0.0;
                            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                                 this->DemandSideLoop.inletNodeNum,
                                                                 this->DemandSideLoop.outletNodeNum,
                                                                 this->DemandSideLoop.loopNum,
                                                                 this->DemandSideLoop.loopSideNum,
                                                                 this->DemandSideLoop.branchNum,
                                                                 this->DemandSideLoop.compNum);
                        }
                    } else { // not able or don't want conditioning
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == DualDeadBandSetPointOnOff) {

                    Real64 SetPointTempLo = DataLoopNode::Node(this->SetPointNodeNum).TempSetPointLo;
                    Real64 SetPointTempHi = DataLoopNode::Node(this->SetPointNodeNum).TempSetPointHi;
                    Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                    Real64 DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
                    if ((DeltaTCooling > this->TempControlTol) && (SetPointTempHi < this->SupplySideLoop.InletTemp)) {
                        // can and want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    } else if ((DeltaTHeating > this->TempControlTol) && (SetPointTempLo > this->SupplySideLoop.InletTemp)) {
                        // can and want to heat
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    } else { // not able or don't want conditioning
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingDifferentialOnOff) {

                    Real64 DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
                    if (DeltaTCooling > this->TempControlTol) {
                        //  want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    } else { // not wanting to cool so turn off
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }

                } else if (SELECT_CASE_var == CoolingSetPointOnOffWithComponentOverride) {

                    Real64 ControlSignalValue(0.0);

                    {
                        auto const SELECT_CASE_var1(this->ControlSignalTemp);
                        if (SELECT_CASE_var1 == WetBulbTemperature) {
                            ControlSignalValue = DataEnvironment::OutWetBulbTemp;
                        } else if (SELECT_CASE_var1 == DryBulbTemperature) {
                            ControlSignalValue = DataEnvironment::OutDryBulbTemp;
                        } else if (SELECT_CASE_var1 == LoopTemperature) {
                            ControlSignalValue = DataLoopNode::Node(this->OtherCompDemandSideLoop.inletNodeNum).TempLastTimestep;
                        } else {
                            assert(false);
                        }
                    }

                    Real64 SetPointTemp = DataLoopNode::Node(this->SetPointNodeNum).TempSetPoint;
                    Real64 DeltaTCooling = SetPointTemp - ControlSignalValue;
                    // obtain shut down state
                    bool ChillerShutDown = DataPlant::PlantLoop(this->OtherCompSupplySideLoop.loopNum)
                                               .LoopSide(this->OtherCompSupplySideLoop.loopSideNum)
                                               .Branch(this->OtherCompSupplySideLoop.branchNum)
                                               .Comp(this->OtherCompSupplySideLoop.compNum)
                                               .FreeCoolCntrlShutDown;
                    if (ChillerShutDown && (DeltaTCooling > this->TempControlTol)) {
                        // can and want to cool
                        mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        if (mdotSupSide > DataBranchAirLoopPlant::MassFlowTolerance) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);

                    } else {
                        mdotSupSide = 0.0;
                        PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                             this->SupplySideLoop.inletNodeNum,
                                                             this->SupplySideLoop.outletNodeNum,
                                                             this->SupplySideLoop.loopNum,
                                                             this->SupplySideLoop.loopSideNum,
                                                             this->SupplySideLoop.branchNum,
                                                             this->SupplySideLoop.compNum);
                        // issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh
                        // demand side inlet temperature value
                        if (FirstHVACIteration) {
                            mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
                        } else {
                            mdotDmdSide = 0.0;
                        }
                        PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                             this->DemandSideLoop.inletNodeNum,
                                                             this->DemandSideLoop.outletNodeNum,
                                                             this->DemandSideLoop.loopNum,
                                                             this->DemandSideLoop.loopSideNum,
                                                             this->DemandSideLoop.branchNum,
                                                             this->DemandSideLoop.compNum);
                    }
                }
            }

        } else { // scheduled off
            mdotSupSide = 0.0;
            PlantUtilities::SetComponentFlowRate(mdotSupSide,
                                                 this->SupplySideLoop.inletNodeNum,
                                                 this->SupplySideLoop.outletNodeNum,
                                                 this->SupplySideLoop.loopNum,
                                                 this->SupplySideLoop.loopSideNum,
                                                 this->SupplySideLoop.branchNum,
                                                 this->SupplySideLoop.compNum);
            mdotDmdSide = 0.0;
            PlantUtilities::SetComponentFlowRate(mdotDmdSide,
                                                 this->DemandSideLoop.inletNodeNum,
                                                 this->DemandSideLoop.outletNodeNum,
                                                 this->DemandSideLoop.loopNum,
                                                 this->DemandSideLoop.loopSideNum,
                                                 this->DemandSideLoop.branchNum,
                                                 this->DemandSideLoop.compNum);
        }
    }

    void HeatExchangerStruct::calculate(Real64 const SupSideMdot, Real64 const DmdSideMdot)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith, derived from CalcEconHeatExchanger by Sankaranarayanan K P aug. 2007
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

        Real64 SupSideLoopInletTemp = DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).Temp;
        Real64 DmdSideLoopInletTemp = DataLoopNode::Node(this->DemandSideLoop.inletNodeNum).Temp;

        // specific heat of fluid entering from supply side loop at inlet temp
        Real64 SupSideLoopInletCp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidName,
                                                                           SupSideLoopInletTemp,
                                                                           DataPlant::PlantLoop(this->SupplySideLoop.loopNum).FluidIndex,
                                                                           RoutineName);

        // specific heat of fluid entering from demand side loop at inlet temp
        Real64 DmdSideLoopInletCp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidName,
                                                                           DmdSideLoopInletTemp,
                                                                           DataPlant::PlantLoop(this->DemandSideLoop.loopNum).FluidIndex,
                                                                           RoutineName);

        Real64 SupSideCapRate = SupSideMdot * SupSideLoopInletCp;
        Real64 DmdSideCapRate = DmdSideMdot * DmdSideLoopInletCp;
        Real64 MinCapRate = min(SupSideCapRate, DmdSideCapRate);
        Real64 MaxCapRate = max(SupSideCapRate, DmdSideCapRate);

        if (MinCapRate > 0.0) {

            {
                auto const SELECT_CASE_var(this->HeatExchangeModelType);

                if (SELECT_CASE_var == CrossFlowBothUnMixed) {
                    Real64 NTU = this->UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = std::pow(NTU, 0.22) / CapRatio;
                    Real64 ExpCheckValue2 = -CapRatio * std::pow(NTU, 0.78);
                    if ((ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) || (ExpCheckValue2 > DataPrecisionGlobals::EXP_UpperLimit)) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 1.0 - std::exp(-NTU);
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = 1.0;
                        }
                    } else {
                        this->Effectiveness = 1.0 - std::exp((std::pow(NTU, 0.22) / CapRatio) * (std::exp(-CapRatio * std::pow(NTU, 0.78)) - 1.0));
                        this->Effectiveness = min(1.0, this->Effectiveness);
                    }

                } else if (SELECT_CASE_var == CrossFlowBothMixed) {
                    Real64 NTU = this->UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -CapRatio * NTU;
                    Real64 ExpCheckValue2 = -NTU;
                    if (ExpCheckValue1 < DataPrecisionGlobals::EXP_LowerLimit) {
                        if (ExpCheckValue2 >= DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 1.0 - std::exp(-NTU);
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = 1.0;
                        }
                    } else if (ExpCheckValue2 < DataPrecisionGlobals::EXP_LowerLimit) {
                        this->Effectiveness = 1.0;
                    } else if ((std::exp(-NTU) == 1.0) || (NTU == 0.0) || (std::exp(-CapRatio * NTU) == 1.0)) { // don't div by zero

                        this->Effectiveness = 0.0;
                    } else {
                        this->Effectiveness = 1.0 / ((1.0 / (1.0 - std::exp(-NTU))) + (CapRatio / (1.0 - std::exp(-CapRatio * NTU))) - (1.0 / NTU));
                        this->Effectiveness = min(1.0, this->Effectiveness);
                    }

                } else if ((SELECT_CASE_var == CrossFlowSupplyLoopMixedDemandLoopUnMixed) ||
                           (SELECT_CASE_var == CrossFlowSupplyLoopUnMixedDemandLoopMixed)) {

                    int CrossFlowEquation;
                    if (SupSideCapRate == MaxCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed) {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    } else if (SupSideCapRate == MinCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed) {
                        CrossFlowEquation = CmaxUnMixedCminMixed;
                    } else if (DmdSideCapRate == MaxCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed) {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    } else if (DmdSideCapRate == MinCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed) {
                        CrossFlowEquation = CmaxUnMixedCminMixed;
                    } else {
                        CrossFlowEquation = CmaxMixedCminUnmixed;
                    }

                    Real64 NTU = this->UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    if (CrossFlowEquation == CmaxMixedCminUnmixed) {
                        Real64 ExpCheckValue1 = -NTU;
                        if (CapRatio == 0.0) { // protect div by zero
                            if (ExpCheckValue1 >= DataPrecisionGlobals::EXP_LowerLimit) {
                                this->Effectiveness = 1.0 - std::exp(-NTU);
                                this->Effectiveness = min(1.0, this->Effectiveness);
                            } else {
                                this->Effectiveness = 1.0;
                            }
                        } else if (ExpCheckValue1 < DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 0.632 / CapRatio;
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = (1.0 / CapRatio) * (1.0 - std::exp(CapRatio * std::exp(-NTU) - 1.0));
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        }
                    } else if (CrossFlowEquation == CmaxUnMixedCminMixed) {
                        Real64 ExpCheckValue1 = -CapRatio * NTU;
                        if (CapRatio == 0.0) {
                            if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                                this->Effectiveness = 1.0 - std::exp(-NTU);
                                this->Effectiveness = min(1.0, this->Effectiveness);
                            } else {
                                this->Effectiveness = 1.0;
                            }
                        } else {
                            if (ExpCheckValue1 >= DataPrecisionGlobals::EXP_LowerLimit) {
                                Real64 ExpCheckValue2 = -(1.0 / CapRatio) * (1.0 - std::exp(-CapRatio * NTU));
                                if (ExpCheckValue2 < DataPrecisionGlobals::EXP_LowerLimit) {
                                    this->Effectiveness = 1.0;
                                } else {
                                    this->Effectiveness = 1.0 - std::exp(ExpCheckValue2);
                                    this->Effectiveness = min(1.0, this->Effectiveness);
                                }
                            } else {
                                this->Effectiveness = 1.0;
                            }
                        }
                    } else {
                        assert(false);
                    }

                } else if (SELECT_CASE_var == CounterFlow) {
                    Real64 NTU = this->UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -NTU * (1.0 - CapRatio);
                    if (ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 1.0 - std::exp(-NTU);
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = 1.0;
                        }
                    } else if (CapRatio * std::exp(-NTU * (1.0 - CapRatio)) == 1.0) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 1.0 - std::exp(-NTU);
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = 1.0;
                        }
                    } else {
                        this->Effectiveness = (1.0 - std::exp(-NTU * (1.0 - CapRatio))) / (1.0 - CapRatio * std::exp(-NTU * (1.0 - CapRatio)));
                        this->Effectiveness = min(1.0, this->Effectiveness);
                    }

                } else if (SELECT_CASE_var == ParallelFlow) {
                    Real64 NTU = this->UA / MinCapRate;
                    Real64 CapRatio = MinCapRate / MaxCapRate;
                    Real64 ExpCheckValue1 = -NTU * (1.0 + CapRatio);
                    if (ExpCheckValue1 > DataPrecisionGlobals::EXP_UpperLimit) {
                        if (-NTU >= DataPrecisionGlobals::EXP_LowerLimit) {
                            this->Effectiveness = 1.0 - std::exp(-NTU);
                            this->Effectiveness = min(1.0, this->Effectiveness);
                        } else {
                            this->Effectiveness = 1.0;
                        }
                    } else {
                        this->Effectiveness = (1.0 - std::exp(-NTU * (1.0 + CapRatio))) / (1.0 + CapRatio);
                        this->Effectiveness = min(1.0, this->Effectiveness);
                    }

                } else if (SELECT_CASE_var == Ideal) {
                    this->Effectiveness = 1.0;
                } else {
                    assert(false);
                }
            }

        } else { // no capacity
            this->Effectiveness = 0.0;
        }

        this->HeatTransferRate = this->Effectiveness * MinCapRate * (SupSideLoopInletTemp - DmdSideLoopInletTemp); // + means supply side is cooled

        if (SupSideMdot > 0.0) {
            this->SupplySideLoop.OutletTemp = SupSideLoopInletTemp - this->HeatTransferRate / (SupSideLoopInletCp * SupSideMdot);
        } else {
            this->SupplySideLoop.OutletTemp = SupSideLoopInletTemp;
        }

        if (DmdSideMdot > 0.0) {
            this->DemandSideLoop.OutletTemp = DmdSideLoopInletTemp + this->HeatTransferRate / (DmdSideLoopInletCp * DmdSideMdot);
        } else {
            this->DemandSideLoop.OutletTemp = DmdSideLoopInletTemp;
        }

        this->SupplySideLoop.InletTemp = SupSideLoopInletTemp;
        this->SupplySideLoop.InletMassFlowRate = SupSideMdot;
        this->DemandSideLoop.InletTemp = DmdSideLoopInletTemp;
        this->DemandSideLoop.InletMassFlowRate = DmdSideMdot;

        DataLoopNode::Node(this->DemandSideLoop.outletNodeNum).Temp = this->DemandSideLoop.OutletTemp;
        DataLoopNode::Node(this->SupplySideLoop.outletNodeNum).Temp = this->SupplySideLoop.OutletTemp;

        this->HeatTransferEnergy = this->HeatTransferRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if ((std::abs(this->HeatTransferRate) > DataHVACGlobals::SmallLoad) && (this->DemandSideLoop.InletMassFlowRate > 0.0) &&
            (this->SupplySideLoop.InletMassFlowRate > 0.0)) {
            this->OperationStatus = 1.0;
        } else {
            this->OperationStatus = 0.0;
        }
    }

    void HeatExchangerStruct::findDemandSideLoopFlow(Real64 const TargetSupplySideLoopLeavingTemp, int const HXActionMode)
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
        Real64 SupSideMdot = DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).MassFlowRate;
        // first see if root is bracketed
        // min demand flow

        // mass flow rate of fluid entering from demand side loop
        Real64 DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
        this->calculate(SupSideMdot, DmdSideMdot);
        Real64 LeavingTempMinFlow = this->SupplySideLoop.OutletTemp;

        // full demand flow
        DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
        this->calculate(SupSideMdot, DmdSideMdot);
        Real64 LeavingTempFullFlow = this->SupplySideLoop.OutletTemp;

        {
            auto const SELECT_CASE_var(HXActionMode);

            if (SELECT_CASE_var == HeatingSupplySideLoop) {
                if ((LeavingTempFullFlow > TargetSupplySideLoopLeavingTemp) && (TargetSupplySideLoopLeavingTemp > LeavingTempMinFlow)) {
                    // need to solve
                    Par(2) = TargetSupplySideLoopLeavingTemp;
                    auto f = std::bind(&HeatExchangerStruct::demandSideFlowResidual, this, std::placeholders::_1, std::placeholders::_2);

                    General::SolveRoot(
                        Acc, MaxIte, SolFla, DmdSideMdot, f, this->DemandSideLoop.MassFlowRateMin, this->DemandSideLoop.MassFlowRateMax, Par);

                    if (SolFla == -1) { // no convergence
                        if (!DataGlobals::WarmupFlag) {
                            if (this->DmdSideModulatSolvNoConvergeErrorCount < 1) {
                                ++this->DmdSideModulatSolvNoConvergeErrorCount;
                                ShowWarningError(ComponentClassName + " named " + this->Name +
                                                 " - Iteration Limit exceeded calculating demand side loop flow rate");
                                ShowContinueError("Simulation continues with calculated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + this->Name +
                                                               " - Iteration Limit exceeded calculating demand side loop flow rate continues.",
                                                           this->DmdSideModulatSolvNoConvergeErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    } else if (SolFla == -2) { // f(x0) and f(x1) have the same sign
                        DmdSideMdot = this->DemandSideLoop.MassFlowRateMax * (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) /
                                      (LeavingTempFullFlow - LeavingTempMinFlow);
                        if (!DataGlobals::WarmupFlag) {
                            if (this->DmdSideModulatSolvFailErrorCount < 1) {
                                ++this->DmdSideModulatSolvFailErrorCount;
                                ShowWarningError(ComponentClassName + " named " + this->Name +
                                                 " - Solver failed to calculate demand side loop flow rate");
                                ShowContinueError("Simulation continues with estimated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + this->Name +
                                                               " - Solver failed to calculate demand side loop flow rate continues.",
                                                           this->DmdSideModulatSolvFailErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    }
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);

                } else if ((TargetSupplySideLoopLeavingTemp >= LeavingTempFullFlow) && (LeavingTempFullFlow > LeavingTempMinFlow)) {
                    // run at full flow
                    DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);

                } else if (LeavingTempMinFlow >= TargetSupplySideLoopLeavingTemp) {

                    // run at min flow
                    DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);
                }
            } else if (SELECT_CASE_var == CoolingSupplySideLoop) {
                if ((LeavingTempFullFlow < TargetSupplySideLoopLeavingTemp) && (TargetSupplySideLoopLeavingTemp < LeavingTempMinFlow)) {
                    // need to solve
                    Par(2) = TargetSupplySideLoopLeavingTemp;
                    auto f = std::bind(&HeatExchangerStruct::demandSideFlowResidual, this, std::placeholders::_1, std::placeholders::_2);

                    General::SolveRoot(
                        Acc, MaxIte, SolFla, DmdSideMdot, f, this->DemandSideLoop.MassFlowRateMin, this->DemandSideLoop.MassFlowRateMax, Par);

                    if (SolFla == -1) { // no convergence
                        if (!DataGlobals::WarmupFlag) {
                            if (this->DmdSideModulatSolvNoConvergeErrorCount < 1) {
                                ++this->DmdSideModulatSolvNoConvergeErrorCount;
                                ShowWarningError(ComponentClassName + " named " + this->Name +
                                                 " - Iteration Limit exceeded calculating demand side loop flow rate");
                                ShowContinueError("Simulation continues with calculated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + this->Name +
                                                               " - Iteration Limit exceeded calculating demand side loop flow rate continues.",
                                                           this->DmdSideModulatSolvNoConvergeErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    } else if (SolFla == -2) { // f(x0) and f(x1) have the same sign
                        DmdSideMdot = this->DemandSideLoop.MassFlowRateMax * (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) /
                                      (LeavingTempFullFlow - LeavingTempMinFlow);
                        if (!DataGlobals::WarmupFlag) {
                            if (this->DmdSideModulatSolvFailErrorCount < 1) {
                                ++this->DmdSideModulatSolvFailErrorCount;
                                ShowWarningError(ComponentClassName + " named " + this->Name +
                                                 " - Solver failed to calculate demand side loop flow rate");
                                ShowContinueError("Simulation continues with estimated demand side mass flow rate = " +
                                                  General::RoundSigDigits(DmdSideMdot, 7));
                            }
                            ShowRecurringWarningErrorAtEnd(ComponentClassName + " named " + this->Name +
                                                               " - Solver failed to calculate demand side loop flow rate continues.",
                                                           this->DmdSideModulatSolvFailErrorIndex,
                                                           DmdSideMdot,
                                                           DmdSideMdot);
                        }
                    }
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);
                } else if ((TargetSupplySideLoopLeavingTemp <= LeavingTempFullFlow) && (LeavingTempFullFlow < LeavingTempMinFlow)) {
                    // run at full flow
                    DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);
                } else if (LeavingTempMinFlow <= TargetSupplySideLoopLeavingTemp) {

                    // run at min flow
                    DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
                    PlantUtilities::SetComponentFlowRate(DmdSideMdot,
                                                         this->DemandSideLoop.inletNodeNum,
                                                         this->DemandSideLoop.outletNodeNum,
                                                         this->DemandSideLoop.loopNum,
                                                         this->DemandSideLoop.loopSideNum,
                                                         this->DemandSideLoop.branchNum,
                                                         this->DemandSideLoop.compNum);
                }
            }
        }
    }

    Real64 HeatExchangerStruct::demandSideFlowResidual(Real64 const DmdSideMassFlowRate,
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
        Real64 SupSideMdot = DataLoopNode::Node(this->SupplySideLoop.inletNodeNum).MassFlowRate;

        this->calculate(SupSideMdot, MdotTrial);

        Real64 SupSideLoopOutletTemp = this->SupplySideLoop.OutletTemp;

        Residuum = Par(2) - SupSideLoopOutletTemp;

        return Residuum;
    }

} // namespace PlantHeatExchangerFluidToFluid

} // namespace EnergyPlus
