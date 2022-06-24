// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerElectricASHRAE205.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/StandardRatings.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include "RS0001_factory.h"
#include "RS0001.h"

namespace EnergyPlus::ChillerElectricASHRAE205 {

    ASHRAE205ChillerSpecs *ASHRAE205ChillerSpecs::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data if it hasn't been done already
        if (state.dataChillerElectricASHRAE205->getInputFlag) {
            getInput(state);
            state.dataChillerElectricASHRAE205->getInputFlag = false;
        }
        // Now look for this particular object in the list
        for (auto &obj : state.dataChillerElectricASHRAE205->Electric205Chiller) {
            if (obj.Name == objectName) {
                return &obj;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state, "LocalElect205ChillerFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void ASHRAE205ChillerSpecs::simulate(
            EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
    }
    
    void getInput(EnergyPlusData &state)
    {
        static constexpr std::string_view RoutineName("GetElectricASHRAE205ChillerInput: "); // include trailing blank space
        using namespace tk205;
        rs_instance_factory::Register_factory("RS0001", std::make_shared<RS0001_factory>());

        bool ErrorsFound(false); // True when input errors are found

        state.dataIPShortCut->cCurrentModuleObject = "Chiller:Electric:ASHRAE205";
        int NumElectric205Chillers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (NumElectric205Chillers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        state.dataChillerElectricASHRAE205->Electric205Chiller.allocate(NumElectric205Chillers);

        auto const ChillerInstances = state.dataInputProcessing->inputProcessor->getObjectInstances(state.dataIPShortCut->cCurrentModuleObject);
        auto ChillerNum{0};
        for (auto &instance : ChillerInstances.items()) {
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            GlobalNames::VerifyUniqueChillerName(state,
                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                 thisObjectName,
                                                 ErrorsFound,
                                                 state.dataIPShortCut->cCurrentModuleObject + " Name");

            ++ChillerNum;
            auto &thisChiller = state.dataChillerElectricASHRAE205->Electric205Chiller(ChillerNum);
            thisChiller.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(state.dataIPShortCut->cCurrentModuleObject,
                                                                        thisObjectName);

            if (fields.count("representation_file_name")) {
                thisChiller.RS = rs_instance_factory::Create("RS0001", fields.at("representation_file_name").get<std::string>().c_str());
            }
            if (fields.count("chilled_water_inlet_node_name") && fields.count("chilled_water_outlet_node_name")) {
                auto const evap_inlet_node_name = fields.at("chilled_water_inlet_node_name").get<std::string>();
                auto const evap_outlet_node_name = fields.at("chilled_water_outlet_node_name").get<std::string>();
                thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   evap_inlet_node_name,
                                                                                   ErrorsFound,
                                                                                   DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                   thisObjectName,
                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    evap_outlet_node_name,
                                                                                    ErrorsFound,
                                                                                    DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                    thisObjectName,
                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   thisObjectName,
                                                   evap_inlet_node_name,
                                                   evap_outlet_node_name,
                                                   "Chilled Water Nodes");
            }
            // Currently the only type is WaterCooled
            thisChiller.CondenserType = DataPlant::CondenserType::WaterCooled;

            const auto is_water_cooled = thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled;
            // Condenser inlet node name is necessary for water-cooled condenser
            if (fields.count("condenser_inlet_node_name") && fields.count("condenser_outlet_node_name")) {
                auto const cond_inlet_node_name = fields.at("condenser_inlet_node_name").get<std::string>();
                auto const cond_outlet_node_name = fields.at("condenser_outlet_node_name").get<std::string>();
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   cond_inlet_node_name,
                                                                                   ErrorsFound,
                                                                                   DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                   thisObjectName,
                                                                                   is_water_cooled ? DataLoopNode::NodeFluidType::Water : DataLoopNode::NodeFluidType::Blank,
                                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                                   NodeInputManager::CompFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    cond_outlet_node_name,
                                                                                    ErrorsFound,
                                                                                    DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                    thisObjectName,
                                                                                    is_water_cooled ? DataLoopNode::NodeFluidType::Water : DataLoopNode::NodeFluidType::Blank,
                                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                                    NodeInputManager::CompFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   thisObjectName,
                                                   cond_inlet_node_name,
                                                   cond_outlet_node_name,
                                                   is_water_cooled ? "Condenser Water Nodes" : "Condenser (unknown fluid) Nodes");
                
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                "\"");
                ShowContinueError(state, "Condenser Inlet or Outlet Node Name is blank.");
                ErrorsFound = true;
            }

            thisChiller.FlowMode =
                static_cast<DataPlant::FlowMode>(getEnumerationValue(DataPlant::FlowModeNamesUC, fields.at("chiller_flow_mode").get<std::string>()));

            if (thisChiller.FlowMode == DataPlant::FlowMode::Invalid) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\",");
                ShowContinueError(state, "Invalid Chiller Flow Mode = " + fields.at("chiller_flow_mode").get<std::string>());
                ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
            };

            thisChiller.SizFac = fields.at("sizing_factor").get<Real64>();

#if 0
            //   Chiller rated performance data
            thisChiller.RefCap = state.dataIPShortCut->rNumericArgs(reference_capacity);
            if (thisChiller.RefCap == DataSizing::AutoSize) {
                thisChiller.RefCapWasAutoSized = true;
            }
            if (state.dataIPShortCut->rNumericArgs(reference_capacity) == 0.0) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(reference_capacity), state.dataIPShortCut->rNumericArgs(reference_capacity)));
                ErrorsFound = true;
            }
            thisChiller.COP = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
                ErrorsFound = true;
            }
            thisChiller.TempRefEvapOut = state.dataIPShortCut->rNumericArgs(3);
            thisChiller.TempRefCondIn = state.dataIPShortCut->rNumericArgs(4);
            thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(5);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(6);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                thisChiller.CondVolFlowRateWasAutoSized = true;
            }

            thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(7);
            thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(8);
            thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(9);
            thisChiller.MinUnloadRat = state.dataIPShortCut->rNumericArgs(10);
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(15);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            if (thisChiller.MinPartLoadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state,
                                  format("{} [{:.3R}] > {} [{:.3R}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         state.dataIPShortCut->rNumericArgs(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         state.dataIPShortCut->rNumericArgs(8)));
                ShowContinueError(state, "Minimum part load ratio must be less than or equal to the maximum part load ratio ");
                ErrorsFound = true;
            }

            if (thisChiller.MinUnloadRat < thisChiller.MinPartLoadRat || thisChiller.MinUnloadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(10) + " must be greater than or equal to the " +
                                  state.dataIPShortCut->cNumericFieldNames(7));
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(10) + " must be less than or equal to the " +
                                  state.dataIPShortCut->cNumericFieldNames(8));
                ErrorsFound = true;
            }

            if (thisChiller.OptPartLoadRat < thisChiller.MinPartLoadRat || thisChiller.OptPartLoadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(9), state.dataIPShortCut->rNumericArgs(9)));
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(9) + " must be greater than or equal to the " +
                                  state.dataIPShortCut->cNumericFieldNames(7));
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(9) + " must be less than or equal to the " +
                                  state.dataIPShortCut->cNumericFieldNames(8));
                ErrorsFound = true;
            }

            thisChiller.CondenserFanPowerRatio = state.dataIPShortCut->rNumericArgs(11);
            thisChiller.CompPowerToCondenserFrac = state.dataIPShortCut->rNumericArgs(12);

            if (thisChiller.CompPowerToCondenserFrac < 0.0 || thisChiller.CompPowerToCondenserFrac > 1.0) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(12), state.dataIPShortCut->rNumericArgs(12)));
                ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(12) + " must be greater than or equal to zero");
                ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(12) + " must be less than or equal to one");
                ErrorsFound = true;
            }

            thisChiller.TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(13);

            // These are the heat recovery inputs
            thisChiller.DesignHeatRecVolFlowRate = state.dataIPShortCut->rNumericArgs(14);
            if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
            }
            if ((thisChiller.DesignHeatRecVolFlowRate > 0.0) || (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize)) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(11),
                                                                                      ErrorsFound,
                                                                                      DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                      thisObjectName,
                                                                                      DataLoopNode::NodeFluidType::Water,
                                                                                      DataLoopNode::ConnectionType::Inlet,
                                                                                      NodeInputManager::CompFluidStream::Tertiary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                    "\"");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + state.dataIPShortCut->cAlphaArgs(11));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(12),
                                                                                       ErrorsFound,
                                                                                       DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                       thisObjectName,
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::ConnectionType::Outlet,
                                                                                       NodeInputManager::CompFluidStream::Tertiary,
                                                                                       DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                    "\"");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + '=' + state.dataIPShortCut->cAlphaArgs(12));
                    ErrorsFound = true;
                }
                if (thisChiller.CondenserType != DataPlant::CondenserType::WaterCooled) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                    "\"");
                    ShowContinueError(state, "Heat Recovery requires a Water Cooled Condenser.");
                    ErrorsFound = true;
                }

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   thisObjectName,
                                                   state.dataIPShortCut->cAlphaArgs(11),
                                                   state.dataIPShortCut->cAlphaArgs(12),
                                                   "Heat Recovery Nodes");
                // store heat recovery volume flow for plant sizing
                if (thisChiller.DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(state, thisChiller.HeatRecInletNodeNum,
                                                                thisChiller.DesignHeatRecVolFlowRate); // CR 6953
                }
                if (NumNums > 17) {
                    if (!state.dataIPShortCut->lNumericFieldBlanks(18)) {
                        thisChiller.HeatRecCapacityFraction = state.dataIPShortCut->rNumericArgs(18);
                    } else {
                        thisChiller.HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 13) {
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                        thisChiller.HeatRecInletLimitSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(14));
                        if (thisChiller.HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                            thisObjectName + "\"");
                            ShowContinueError(state,
                                              "Invalid " + state.dataIPShortCut->cAlphaFieldNames(14) + '=' + state.dataIPShortCut->cAlphaArgs(14));
                            ErrorsFound = true;
                        }
                    } else {
                        thisChiller.HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    thisChiller.HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 14) {
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                        thisChiller.HeatRecSetPointNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                                 state.dataIPShortCut->cAlphaArgs(15),
                                                                                                 ErrorsFound,
                                                                                                 DataLoopNode::ConnectionObjectType::ChillerElectricASHRAE205,
                                                                                                 thisObjectName,
                                                                                                 DataLoopNode::NodeFluidType::Water,
                                                                                                 DataLoopNode::ConnectionType::Sensor,
                                                                                                 NodeInputManager::CompFluidStream::Primary,
                                                                                                 DataLoopNode::ObjectIsNotParent);
                    } else {
                        thisChiller.HeatRecSetPointNodeNum = 0;
                    }
                } else {
                    thisChiller.HeatRecSetPointNodeNum = 0;
                }

            } else {
                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11) || !state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                     "\"");
                    ShowContinueError(state, "Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.");
                    ShowContinueError(state, "However, node names were specified for heat recovery inlet or outlet nodes.");
                }
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(16);
            if (state.dataIPShortCut->rNumericArgs(16) < 0.0) {
                ShowSevereError(
                        state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName + "\"");
                ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(16) + " must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = state.dataIPShortCut->rNumericArgs(17);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 17) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + " \"" + thisObjectName +
                                     "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(17) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(13));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisObjectName +
                                     "\"");
                    ShowWarningError(state,
                                     state.dataIPShortCut->cAlphaFieldNames(13) + " \"" + state.dataIPShortCut->cAlphaArgs(13) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

#endif // if 0
            if (fields.count("end_use_subcategory")) {
                thisChiller.EndUseSubcategory = fields.at("end_use_subcategory").get<std::string>();
            } else {
                thisChiller.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void ASHRAE205ChillerSpecs::setOutputVariables(EnergyPlusData &state)
    {
    }

    void ASHRAE205ChillerSpecs::oneTimeInit(EnergyPlusData &state)
    {
        // Locate the chillers on the plant loops for later usage
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::PlantEquipmentType::Chiller_ElectricASHRAE205,
                                                this->CWPlantLoc,
                                                errFlag,
                                                this->TempLowLimitEvapOut,
                                                _,
                                                _,
                                                this->EvapInletNodeNum,
                                                _);
#if 0 // If and when AirCooled is implemented, or heat recovery is implemented, uncomment
        if (this->CondenserType != DataPlant::CondenserType::AirCooled) {
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR,
                                                    this->CDPlantLoc,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->CondInletNodeNum,
                                                    _);
            PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CWPlantLoc, this->CDPlantLoc, DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR, true);
        }
        if (this->HeatRecActive) {
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR,
                                                    this->HRPlantLoc,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->HeatRecInletNodeNum,
                                                    _);
            PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CWPlantLoc, this->HRPlantLoc, DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR, true);
        }

        if ((this->CondenserType != DataPlant::CondenserType::AirCooled) && (this->HeatRecActive)) {
            PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CDPlantLoc, this->HRPlantLoc, DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR, false);
        }

        if (errFlag) {
            ShowFatalError(state, "InitElecReformEIRChiller: Program terminated due to previous condition(s).");
        }
#endif // #if 0
        if (this->FlowMode == DataPlant::FlowMode::Constant) {
            // reset flow priority
            DataPlant::CompData::getPlantComponent(state, this->CWPlantLoc).FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
        }

        if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
            // reset flow priority
            DataPlant::CompData::getPlantComponent(state, this->CWPlantLoc).FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
            // check if setpoint on outlet node
            if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    if (!this->ModulatedFlowErrDone) {
                        ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                        ShowContinueError(
                                state, "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                        ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                        this->ModulatedFlowErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    bool fatalError = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(
                            state, this->EvapOutletNodeNum, EMSManager::SPControlType::TemperatureSetPoint, fatalError);
                    state.dataLoopNodes->NodeSetpointCheck(this->EvapOutletNodeNum).needsSetpointChecking = false;
                    if (fatalError) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(state,
                                              "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                            ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                            ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node ");
                            ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    }
                }
                this->ModulatedFlowSetToLoop = true;
                state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).TempSetPointNodeNum).TempSetPoint;
                state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).TempSetPointNodeNum).TempSetPointHi;
            }
        }
    }

    void ASHRAE205ChillerSpecs::initialize(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {
    }

    void ASHRAE205ChillerSpecs::size(EnergyPlusData &state)
    {
    }

    void ASHRAE205ChillerSpecs::calculate(EnergyPlusData &state, Real64 &MyLoad, bool const RunFlag)
    {
        static constexpr std::string_view RoutineName("CalcElecASHRAE205ChillerModel");
#if 0
        this->ChillerPartLoadRatio = 0.0;
        this->ChillerCyclingRatio = 0.0;
        this->ChillerFalseLoadRate = 0.0;
        this->EvapMassFlowRate = 0.0;
        this->CondMassFlowRate = 0.0;
        this->Power = 0.0;
        this->QCondenser = 0.0;
        this->QEvaporator = 0.0;
        this->QHeatRecovery = 0.0;
        int PlantLoopNum = this->CWPlantLoc.loopNum;
        DataPlant::LoopSideLocation LoopSideNum = this->CWPlantLoc.loopSideNum;
        int BranchNum = this->CWPlantLoc.branchNum;
        int CompNum = this->CWPlantLoc.compNum;

        // Set performance curve outputs to 0.0 when chiller is off
        this->ChillerCapFT = 0.0;
        this->ChillerEIRFT = 0.0;
        this->ChillerEIRFPLR = 0.0;

        // Set module-level chiller evap and condenser inlet temperature variables
        Real64 condInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        // If no loop demand or chiller OFF, return
        // If chiller load is 0 or chiller is not running then leave the subroutine. Before leaving
        //  if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        //  flow resolver will not shut down the branch
        if (MyLoad >= 0 || !RunFlag) {
            if (this->EquipFlowCtrl == DataBranchAirLoopPlant::ControlType::SeriesActive ||
                state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == DataPlant::FlowLock::Locked) {
                this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            }
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                if (DataPlant::CompData::getPlantComponent(state, this->CDPlantLoc).FlowCtrl == DataBranchAirLoopPlant::ControlType::SeriesActive) {
                    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                }
            }

            return;
        }

        // Chiller reference capacity [W]
        Real64 ChillerRefCap = this->RefCap;

        // Reference coefficient of performance, from user input
        Real64 ReferenceCOP = this->RefCOP;
        this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

        // Evaporator low temp. limit cut off [C]
        Real64 TempLowLimitEout = this->TempLowLimitEvapOut;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerRefCap;
            Real64 ReferenceCOP_ff = ReferenceCOP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = state.dataFaultsMgr->FaultsChillerFouling(FaultIndex).CalFoulingFactor(state);

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerRefCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            ReferenceCOP = ReferenceCOP_ff * this->FaultyChillerFoulingFactor;
        }

        // Set mass flow rates

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(state, this->CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDPlantLoc);
            PlantUtilities::PullCompInterconnectTrigger(
                    state, this->CWPlantLoc, this->CondMassFlowIndex, this->CDPlantLoc, DataPlant::CriteriaType::MassFlowRate, this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }
        Real64 FRAC = 1.0;
        Real64 EvapOutletTempSetPoint(0.0); // Evaporator outlet temperature setpoint [C]
        switch (state.dataPlnt->PlantLoop(PlantLoopNum).LoopDemandCalcScheme) {
            case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                    (state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::OpScheme::CompSetPtBased) ||
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } break;
            case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                    (state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::OpScheme::CompSetPtBased) ||
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            } break;
            default: {
                assert(false);
            } break;
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTempSetPoint_ff = EvapOutletTempSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the EvapOutletTempSetPoint
            EvapOutletTempSetPoint =
                    max(this->TempLowLimitEvapOut,
                        min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTempSetPoint_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTempSetPoint_ff - EvapOutletTempSetPoint;
        }

        // correct temperature if using heat recovery
        // use report values for latest valid calculation, lagged somewhat
        if (this->HeatRecActive) {
            if ((this->QHeatRecovery + this->QCondenser) > 0.0) { // protect div by zero
                this->ChillerCondAvgTemp =
                        (this->QHeatRecovery * this->HeatRecOutletTemp + this->QCondenser * this->CondOutletTemp) / (this->QHeatRecovery + this->QCondenser);
            } else {
                this->ChillerCondAvgTemp = FalsiCondOutTemp;
            }
        } else {
            this->ChillerCondAvgTemp = FalsiCondOutTemp;
        }

        // Get capacity curve info with respect to CW setpoint and leaving condenser water temps
        this->ChillerCapFT = max(0.0, CurveManager::CurveValue(state, this->ChillerCapFTIndex, EvapOutletTempSetPoint, this->ChillerCondAvgTemp));

        // Available chiller capacity as a function of temperature
        Real64 AvailChillerCap = ChillerRefCap * this->ChillerCapFT;

        this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
        //   Some other component set the flow to 0. No reason to continue with calculations.
        if (this->EvapMassFlowRate == 0.0) {
            MyLoad = 0.0;
            return;
        }

        // This chiller currently has only a water-cooled condenser

        // Calculate water side load
        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidName,
                                                           state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                                           state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidIndex,
                                                           RoutineName);

        // actual load to be met by chiller. This value is compared to MyLoad
        // and reset when necessary since this chiller can cycle, the load passed
        // should be the actual load.  Instead the minimum PLR * RefCap is
        // passed in.
        Real64 TempLoad = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRateMaxAvail * Cp *
                          (state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapOutletTempSetPoint);

        TempLoad = max(0.0, TempLoad);

        // MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
        if (std::abs(MyLoad) > TempLoad) {
            MyLoad = sign(TempLoad, MyLoad);
        }

        // Part load ratio based on load and available chiller capacity, cap at max part load ratio
        Real64 PartLoadRat = 0.0; // Operating part load ratio
        if (AvailChillerCap > 0) {
            PartLoadRat = max(0.0, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        }

        // Set evaporator heat transfer rate
        this->QEvaporator = AvailChillerCap * PartLoadRat;
        this->ChillerPartLoadRatio = PartLoadRat;
        // If FlowLock is False (0), the chiller sets the plant loop mdot
        // If FlowLock is True (1),  the new resolved plant loop mdot is used
        if (state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == DataPlant::FlowLock::Unlocked) {
            this->PossibleSubcooling = !(state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                                         DataPlant::OpScheme::CompSetPtBased);

            Real64 EvapDeltaTemp(0.0); // Evaporator temperature difference [C]

            // Either set the flow to the Constant value or calculate the flow for the variable volume case
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {
                // Set the evaporator mass flow rate to design
                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(state, this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWPlantLoc);
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                switch (state.dataPlnt->PlantLoop(PlantLoopNum).LoopDemandCalcScheme) {
                    case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                        // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                        EvapDeltaTemp =
                                state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } break;
                    case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                        EvapDeltaTemp =
                                state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    } break;
                    default: {
                        assert(false);
                    } break;
                }

                if (EvapDeltaTemp != 0) {
                    this->EvapMassFlowRate = max(0.0, (this->QEvaporator / Cp / EvapDeltaTemp));
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(
                            state, this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWPlantLoc);
                    // Should we recalculate this with the corrected setpoint?
                    switch (state.dataPlnt->PlantLoop(PlantLoopNum).LoopDemandCalcScheme) {
                        case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                        } break;
                        case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } break;
                        default:
                            break;
                    }
                    this->QEvaporator = max(0.0, (this->EvapMassFlowRate * Cp * EvapDeltaTemp));
                } else {
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(
                            state, this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWPlantLoc);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    this->QEvaporator = 0.0;
                    PartLoadRat = 0.0;
                    this->ChillerPartLoadRatio = PartLoadRat;

                    if (this->DeltaTErrCount < 1 && !state.dataGlobal->WarmupFlag) {
                        ++this->DeltaTErrCount;
                        ShowWarningError(state, "Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tevapout setpoint temp).");
                        ShowContinueErrorTimeStamp(state, "");
                    } else if (!state.dataGlobal->WarmupFlag) {
                        ++this->ChillerCapFTError;
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "CHILLER:ELECTRIC:ASHRAE205 \"" + this->Name +
                                                       "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...",
                                                       this->DeltaTErrCountIndex,
                                                       EvapDeltaTemp,
                                                       EvapDeltaTemp);
                    }
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
                (!state.dataGlobal->KickOffSimulation) && (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated);
                state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex)
                        .CalFaultChillerSWT(VarFlowFlag,
                                            this->FaultyChillerSWTOffset,
                                            Cp,
                                            state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                            this->EvapOutletTemp,
                                            this->EvapMassFlowRate,
                                            this->QEvaporator);
                // update corresponding variables at faulty case
                PartLoadRat = (AvailChillerCap > 0.0) ? (this->QEvaporator / AvailChillerCap) : 0.0;
                PartLoadRat = max(0.0, min(PartLoadRat, this->MaxPartLoadRat));
                this->ChillerPartLoadRatio = PartLoadRat;
            }

        } else { // If FlowLock is True
            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(state, this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWPlantLoc);
            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                return;
            }

            Real64 EvapDeltaTemp;
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapOutletTempSetPoint;
                this->QEvaporator = max(0.0, (this->EvapMassFlowRate * Cp * EvapDeltaTemp));
                this->EvapOutletTemp = EvapOutletTempSetPoint;
            }
            if (this->EvapOutletTemp < TempLowLimitEout) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = TempLowLimitEout;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
                (!state.dataGlobal->KickOffSimulation) && (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex)
                        .CalFaultChillerSWT(VarFlowFlag,
                                            this->FaultyChillerSWTOffset,
                                            Cp,
                                            state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                            this->EvapOutletTemp,
                                            this->EvapMassFlowRate,
                                            this->QEvaporator);
                // update corresponding variables at faulty case
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * this->MaxPartLoadRat;
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    // evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }

            if (AvailChillerCap > 0.0) {
                PartLoadRat = max(0.0, min((this->QEvaporator / AvailChillerCap), this->MaxPartLoadRat));
            } else {
                PartLoadRat = 0.0;
            }

            // Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
            if (PartLoadRat < this->MinPartLoadRat) FRAC = min(1.0, (PartLoadRat / this->MinPartLoadRat));

            // set the module level variable used for reporting FRAC
            this->ChillerCyclingRatio = FRAC;

            // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
            if (AvailChillerCap > 0.0) {
                PartLoadRat = max(PartLoadRat, this->MinUnloadRat);
            } else {
                PartLoadRat = 0.0;
            }

            // set the module level variable used for reporting PLR
            this->ChillerPartLoadRatio = PartLoadRat;

            // calculate the load due to false loading on chiller over and above water side load
            this->ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - this->QEvaporator;
            if (this->ChillerFalseLoadRate < DataHVACGlobals::SmallLoad) {
                this->ChillerFalseLoadRate = 0.0;
            }

        } // This is the end of the FlowLock Block

        this->ChillerEIRFT = max(0.0, CurveManager::CurveValue(state, this->ChillerEIRFTIndex, this->EvapOutletTemp, this->ChillerCondAvgTemp));

        // Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift
        if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
            this->ChillerEIRFPLR = max(0.0, CurveManager::CurveValue(state, this->ChillerEIRFPLRIndex, this->ChillerCondAvgTemp, PartLoadRat));
        } else if (this->PartLoadCurveType == PLR::Lift) {

            // Chiller lift
            Real64 ChillerLift = this->ChillerCondAvgTemp - this->EvapOutletTemp;

            // Deviation of leaving chilled water temperature from the reference condition
            Real64 ChillerTdev = std::abs(this->EvapOutletTemp - this->TempRefEvapOut);

            // Chiller lift under the reference condition
            Real64 ChillerLiftRef = this->TempRefCondOut - this->TempRefEvapOut;

            if (ChillerLiftRef <= 0) ChillerLiftRef = 35 - 6.67;

            // Normalized chiller lift
            Real64 ChillerLiftNom = ChillerLift / ChillerLiftRef;

            // Normalized ChillerTdev
            Real64 ChillerTdevNom = ChillerTdev / ChillerLiftRef;

            this->ChillerEIRFPLR = max(0.0, CurveManager::CurveValue(state, this->ChillerEIRFPLRIndex, ChillerLiftNom, PartLoadRat, ChillerTdevNom));
        }

        if (ReferenceCOP <= 0) ReferenceCOP = 5.5;
        this->Power = (AvailChillerCap / ReferenceCOP) * this->ChillerEIRFPLR * this->ChillerEIRFT * FRAC;

        this->QCondenser = this->Power * this->CompPowerToCondenserFrac + this->QEvaporator + this->ChillerFalseLoadRate;

        //  Currently only water cooled chillers are allowed for the reformulated EIR chiller model
        if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (this->HeatRecActive) this->calcHeatRecovery(state, this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovery);
            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->CDPlantLoc.loopNum).FluidName,
                                                        condInletTemp,
                                                        state.dataPlnt->PlantLoop(this->CDPlantLoc.loopNum).FluidIndex,
                                                        RoutineName);
            this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / Cp + condInletTemp;
        } else {
            ShowSevereError(state, "ControlReformEIRChillerModel: Condenser flow = 0, for ElecReformEIRChiller=" + this->Name);
            ShowContinueErrorTimeStamp(state, "");
        }
#endif
    }

    void ASHRAE205ChillerSpecs::update(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
    {
#if 0
        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // Set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

            this->ChillerPartLoadRatio = 0.0;
            this->ChillerCyclingRatio = 0.0;
            this->ChillerFalseLoadRate = 0.0;
            this->ChillerFalseLoad = 0.0;
            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvapEnergy = 0.0;
            this->CondEnergy = 0.0;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
            this->ActualCOP = 0.0;

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(state, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->QHeatRecovery = 0.0;
                this->EnergyHeatRecovery = 0.0;
                this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMassFlow = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;
            }

        } else { // Chiller is running, so pass calculated values
            // Set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
            // Set node flow rates;  for these load based models
            // assume that sufficient evaporator flow rate is available
            this->ChillerFalseLoad = this->ChillerFalseLoadRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->EvapEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->CondEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            if (this->Power != 0.0) {
                this->ActualCOP = (this->QEvaporator + this->ChillerFalseLoadRate) / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(state, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->EnergyHeatRecovery = this->QHeatRecovery * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
                this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMassFlow = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;
            }
        }
#endif
    }


} // namespace EnergyPlus::ChillerElectricASHRAE205
