#include <utility>

// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#include <string>
#include <vector>

#include <BranchNodeConnections.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantComponent.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>
#include <WaterToWaterHeatPumpEIR.hh>
#include "CurveManager.hh"

namespace EnergyPlus {
    namespace EIRWaterToWaterHeatPumps {

        bool getInputsWWHP(true);
        std::vector<EIRWaterToWaterHeatPump> eir_wwhp;
        std::string static const __EQUIP__ = "EIRWaterToWaterHeatPump"; // NOLINT(cert-err58-cpp)

        void EIRWaterToWaterHeatPump::clear_state() {
            getInputsWWHP = true;
            eir_wwhp.clear();
        }

        void EIRWaterToWaterHeatPump::simulate(const EnergyPlus::PlantLocation &calledFromLocation,
                                               bool const FirstHVACIteration,
                                               Real64 &CurLoad,
                                               bool const RunFlag) {

            std::string const routineName = "WaterToWaterHeatPumpEIR::simulate";

            // Call initialize to set flow rates, run flag, and entering temperatures
            this->running = RunFlag;
            this->setOperatingFlowRates();

            if (calledFromLocation.loopNum == this->sourceSideLocation.loopNum) { // condenser side
                PlantUtilities::UpdateChillerComponentCondenserSide(this->sourceSideLocation.loopNum,
                                                                    this->sourceSideLocation.loopSideNum,
                                                                    this->plantTypeOfNum,
                                                                    this->sourceSideNodes.inlet,
                                                                    this->sourceSideNodes.outlet,
                                                                    this->sourceSideHeatTransfer,
                                                                    this->sourceSideInletTemp,
                                                                    this->sourceSideOutletTemp,
                                                                    this->sourceSideMassFlowRate,
                                                                    FirstHVACIteration);
                return;
            }

            if (this->running) {
                this->doPhysics(CurLoad);
            } else {
                this->loadSideHeatTransfer = 0.0;
                this->loadSideOutletTemp = this->loadSideInletTemp;
                this->powerUsage = 0.0;
                this->sourceSideHeatTransfer = 0.0;
                this->sourceSideOutletTemp = this->sourceSideInletTemp;
            }

            // update nodes
            DataLoopNode::Node(this->loadSideNodes.outlet).Temp = this->loadSideOutletTemp;
            DataLoopNode::Node(this->sourceSideNodes.outlet).Temp = this->sourceSideOutletTemp;
        }

        Real64 EIRWaterToWaterHeatPump::getLoadSideOutletSetPointTemp() {
            auto &thisLoadPlantLoop = DataPlant::PlantLoop(this->loadSideLocation.loopNum);
            auto &thisLoadLoopSide = thisLoadPlantLoop.LoopSide(this->loadSideLocation.loopSideNum);
            auto &thisLoadBranch = thisLoadLoopSide.Branch(this->loadSideLocation.branchNum);
            auto &thisLoadComp = thisLoadBranch.Comp(this->loadSideLocation.compNum);
            if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                if (thisLoadComp.CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
                    // there will be a valid set-point on outlet
                    return DataLoopNode::Node(this->loadSideNodes.outlet).TempSetPoint;
                } else { // use plant loop overall set-point
                    return DataLoopNode::Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPoint;
                }
            } else if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand) {
                if (thisLoadComp.CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
                    // there will be a valid set-point on outlet
                    return DataLoopNode::Node(this->loadSideNodes.outlet).TempSetPointHi;
                } else { // use plant loop overall set-point
                    return DataLoopNode::Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPointHi;
                }
            } else {
                // there's no other enums for loop demand calcs, so I don't have a reasonable unit test for these
                // lines, they simply should not be able to get here.  But a fatal is here anyway just in case,
                // and the lines are excluded from coverage.
                ShowFatalError("Unsupported loop demand calculation scheme in EIR heat pump");  // LCOV_EXCL_LINE
                return -999; // not actually returned with Fatal Error call above  // LCOV_EXCL_LINE
            }
        }

        void EIRWaterToWaterHeatPump::setOperatingFlowRates() {
            if (!this->running) {
                this->loadSideMassFlowRate = 0.0;
                this->sourceSideMassFlowRate = 0.0;
                PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
                                                     this->loadSideNodes.inlet,
                                                     this->loadSideNodes.outlet,
                                                     this->loadSideLocation.loopNum,
                                                     this->loadSideLocation.loopSideNum,
                                                     this->loadSideLocation.branchNum,
                                                     this->loadSideLocation.compNum);
                PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
                                                     this->sourceSideNodes.inlet,
                                                     this->sourceSideNodes.outlet,
                                                     this->sourceSideLocation.loopNum,
                                                     this->sourceSideLocation.loopSideNum,
                                                     this->sourceSideLocation.branchNum,
                                                     this->sourceSideLocation.compNum);
                PlantUtilities::PullCompInterconnectTrigger(this->loadSideLocation.loopNum,
                                                            this->loadSideLocation.loopSideNum,
                                                            this->loadSideLocation.branchNum,
                                                            this->loadSideLocation.compNum,
                                                            this->condMassFlowRateTriggerIndex,
                                                            this->sourceSideLocation.loopNum,
                                                            this->sourceSideLocation.loopSideNum,
                                                            DataPlant::CriteriaType_MassFlowRate,
                                                            this->sourceSideMassFlowRate);
                // Set flows if the heat pump is running
            } else { // the heat pump must run
                this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
                this->sourceSideMassFlowRate = this->sourceSideDesignMassFlowRate;
                PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
                                                     this->loadSideNodes.inlet,
                                                     this->loadSideNodes.outlet,
                                                     this->loadSideLocation.loopNum,
                                                     this->loadSideLocation.loopSideNum,
                                                     this->loadSideLocation.branchNum,
                                                     this->loadSideLocation.compNum);
                PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
                                                     this->sourceSideNodes.inlet,
                                                     this->sourceSideNodes.outlet,
                                                     this->sourceSideLocation.loopNum,
                                                     this->sourceSideLocation.loopSideNum,
                                                     this->sourceSideLocation.branchNum,
                                                     this->sourceSideLocation.compNum);

                // if there's no flow in one, try to turn the entire heat pump off
                if (this->loadSideMassFlowRate <= 0.0 || this->sourceSideMassFlowRate <= 0.0) {
                    this->loadSideMassFlowRate = 0.0;
                    this->sourceSideMassFlowRate = 0.0;
                    this->running = false;
                    PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
                                                         this->loadSideNodes.inlet,
                                                         this->loadSideNodes.outlet,
                                                         this->loadSideLocation.loopNum,
                                                         this->loadSideLocation.loopSideNum,
                                                         this->loadSideLocation.branchNum,
                                                         this->loadSideLocation.compNum);
                    PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
                                                         this->sourceSideNodes.inlet,
                                                         this->sourceSideNodes.outlet,
                                                         this->sourceSideLocation.loopNum,
                                                         this->sourceSideLocation.loopSideNum,
                                                         this->sourceSideLocation.branchNum,
                                                         this->sourceSideLocation.compNum);
                }
                PlantUtilities::PullCompInterconnectTrigger(this->loadSideLocation.loopNum,
                                                            this->loadSideLocation.loopSideNum,
                                                            this->loadSideLocation.branchNum,
                                                            this->loadSideLocation.compNum,
                                                            this->condMassFlowRateTriggerIndex,
                                                            this->sourceSideLocation.loopNum,
                                                            this->sourceSideLocation.loopSideNum,
                                                            DataPlant::CriteriaType_MassFlowRate,
                                                            this->sourceSideMassFlowRate);
            }
        }

        void EIRWaterToWaterHeatPump::doPhysics(Real64 currentLoad) {
            // read inlet temperatures
            this->loadSideInletTemp = DataLoopNode::Node(this->loadSideNodes.inlet).Temp;
            this->sourceSideInletTemp = DataLoopNode::Node(this->sourceSideNodes.inlet).Temp;

            // get setpoint on the load side outlet
            Real64 loadSideOutletSetpointTemp = this->getLoadSideOutletSetPointTemp();

            // evaluate capacity modifier curve and determine load side heat transfer
            Real64 capacityModifierFuncTemp = CurveManager::CurveValue(
                    this->capFuncTempCurveIndex, loadSideOutletSetpointTemp, this->sourceSideInletTemp
            );
            Real64 availableCapacity = this->referenceCapacity * capacityModifierFuncTemp;
            Real64 partLoadRatio = 0.0;
            if (availableCapacity > 0) {
                partLoadRatio = max(0.0, min(std::abs(currentLoad) / availableCapacity, 1.0));
            }
            auto &thisLoadPlantLoop = DataPlant::PlantLoop(this->loadSideLocation.loopNum);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                    thisLoadPlantLoop.FluidName,
                    DataLoopNode::Node(this->loadSideNodes.inlet).Temp,
                    thisLoadPlantLoop.FluidIndex,
                    "WWHPEIR::simulate()"
            );
            this->loadSideHeatTransfer = availableCapacity * partLoadRatio;

            // calculate load side outlet conditions
            Real64 const loadMCp = this->loadSideMassFlowRate * Cp;
            this->loadSideOutletTemp = this->calcLoadOutletTemp(this->loadSideInletTemp, this->loadSideHeatTransfer / loadMCp);

            // calculate power usage from EIR curves
            Real64 eirModifierFuncTemp = CurveManager::CurveValue(this->powerRatioFuncTempCurveIndex,
                                                                  this->loadSideOutletTemp,
                                                                  this->sourceSideInletTemp);
            Real64 eirModifierFuncPLR = CurveManager::CurveValue(this->powerRatioFuncPLRCurveIndex,
                                                                 partLoadRatio);
            this->powerUsage = (availableCapacity / this->referenceCOP) * eirModifierFuncPLR * eirModifierFuncTemp;

            // energy balance on heat pump
            this->sourceSideHeatTransfer = this->calcQsource(this->loadSideHeatTransfer, this->powerUsage);

            // calculate source side
            Real64 const sourceMCp = this->sourceSideMassFlowRate * Cp;
            this->sourceSideOutletTemp = this->calcSourceOutletTemp(this->sourceSideInletTemp, this->sourceSideHeatTransfer / sourceMCp);
        }

        void EIRWaterToWaterHeatPump::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation)) {
            // This function does all one-time and begin-environment initialization
            std::string const routineName = EIRWaterToWaterHeatPumps::__EQUIP__ + ':' + __FUNCTION__;
            if (this->oneTimeInit) {
                bool errFlag = false;

                // setup output variables
                SetupOutputVariable(
                        "EIR WWHP Load Side Heat Transfer", OutputProcessor::Unit::W, this->loadSideHeatTransfer,
                        "System", "Average", this->name);
                SetupOutputVariable(
                        "EIR WWHP Source Side Heat Transfer", OutputProcessor::Unit::W, this->sourceSideHeatTransfer,
                        "System", "Average", this->name);
                SetupOutputVariable(
                        "EIR WWHP Load Side Inlet Temperature", OutputProcessor::Unit::C, this->loadSideInletTemp,
                        "System", "Average", this->name);
                SetupOutputVariable(
                        "EIR WWHP Load Side Outlet Temperature", OutputProcessor::Unit::C, this->loadSideOutletTemp,
                        "System", "Average", this->name);
                SetupOutputVariable(
                        "EIR WWHP Source Side Inlet Temperature", OutputProcessor::Unit::C, this->sourceSideInletTemp,
                        "System", "Average", this->name);
                SetupOutputVariable(
                        "EIR WWHP Source Side Outlet Temperature", OutputProcessor::Unit::C, this->sourceSideOutletTemp,
                        "System", "Average", this->name);
                SetupOutputVariable("EIR WWHP Power Usage", OutputProcessor::Unit::W, this->powerUsage, "System",
                                    "Average", this->name);

                // find this component on the plant
                bool thisErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->name,
                                                        this->plantTypeOfNum,
                                                        this->loadSideLocation.loopNum,
                                                        this->loadSideLocation.loopSideNum,
                                                        this->loadSideLocation.branchNum,
                                                        this->loadSideLocation.compNum,
                                                        thisErrFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->loadSideNodes.inlet,
                                                        _);

                if (thisErrFlag) {
                    ShowSevereError(routineName + ": Plant topology problem for " +
                                    DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                    ShowContinueError("Could not locate component's load side connections on a plant loop");
                    errFlag = true;
                } else if (this->loadSideLocation.loopSideNum != DataPlant::SupplySide) { // only check if !thisErrFlag
                    ShowSevereError(routineName + ": Invalid connections for " +
                                    DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                    ShowContinueError("The load side connections are not on the Supply Side of a plant loop");
                    errFlag = true;
                }

                thisErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->name,
                                                        this->plantTypeOfNum,
                                                        this->sourceSideLocation.loopNum,
                                                        this->sourceSideLocation.loopSideNum,
                                                        this->sourceSideLocation.branchNum,
                                                        this->sourceSideLocation.compNum,
                                                        thisErrFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->sourceSideNodes.inlet,
                                                        _);

                if (thisErrFlag) {
                    ShowSevereError(routineName + ": Plant topology problem for " +
                                    DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                    ShowContinueError("Could not locate component's source side connections on a plant loop");
                    errFlag = true;
                } else if (this->sourceSideLocation.loopSideNum != DataPlant::DemandSide) { // only check if !thisErrFlag
                    ShowSevereError(routineName + ": Invalid connections for " +
                                    DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                    ShowContinueError("The source side connections are not on the Demand Side of a plant loop");
                    errFlag = true;
                }

                // make sure it is not the same loop on both sides.
                if (this->loadSideLocation.loopNum ==
                    this->sourceSideLocation.loopNum) { // user is being too tricky, don't allow
                    ShowSevereError(routineName + ": Invalid connections for " +
                                    DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                    ShowContinueError("The load and source sides need to be on different loops.");
                    errFlag = true;
                } else {

                    PlantUtilities::InterConnectTwoPlantLoopSides(this->loadSideLocation.loopNum,
                                                                  this->loadSideLocation.loopSideNum,
                                                                  this->sourceSideLocation.loopNum,
                                                                  this->sourceSideLocation.loopSideNum,
                                                                  this->plantTypeOfNum,
                                                                  true);
                }

                if (errFlag) {
                    ShowFatalError(routineName + ": Program terminated due to previous condition(s).");
                }
                this->oneTimeInit = false;
            } // plant setup

            if (DataGlobals::BeginEnvrnFlag && this->envrnInit && DataPlant::PlantFirstSizesOkayToFinalize) {
                Real64 rho = FluidProperties::GetDensityGlycol(
                        DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidName,
                        DataGlobals::InitConvTemp,
                        DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                        routineName);
                this->loadSideDesignMassFlowRate = rho * this->loadSideDesignVolFlowRate;
                PlantUtilities::InitComponentNodes(0.0,
                                                   this->loadSideDesignMassFlowRate,
                                                   this->loadSideNodes.inlet,
                                                   this->loadSideNodes.outlet,
                                                   this->loadSideLocation.loopNum,
                                                   this->loadSideLocation.loopSideNum,
                                                   this->loadSideLocation.branchNum,
                                                   this->loadSideLocation.compNum);

                rho = FluidProperties::GetDensityGlycol(
                        DataPlant::PlantLoop(this->sourceSideLocation.loopNum).FluidName,
                        DataGlobals::InitConvTemp,
                        DataPlant::PlantLoop(this->sourceSideLocation.loopNum).FluidIndex,
                        routineName);
                this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
                PlantUtilities::InitComponentNodes(0.0,
                                                   this->sourceSideDesignMassFlowRate,
                                                   this->sourceSideNodes.inlet,
                                                   this->sourceSideNodes.outlet,
                                                   this->sourceSideLocation.loopNum,
                                                   this->sourceSideLocation.loopSideNum,
                                                   this->sourceSideLocation.branchNum,
                                                   this->sourceSideLocation.compNum);
                this->envrnInit = false;
            }
            if (!DataGlobals::BeginEnvrnFlag) {
                this->envrnInit = true;
            }
        }

        void EIRWaterToWaterHeatPump::getDesignCapacities(const PlantLocation &calledFromLocation,
                                                          Real64 &MaxLoad,
                                                          Real64 &MinLoad,
                                                          Real64 &OptLoad) {
            if (calledFromLocation.loopNum == this->loadSideLocation.loopNum) {
                this->size();
                MinLoad = 0.0;
                MaxLoad = this->referenceCapacity;
                OptLoad = this->referenceCapacity;
            } else {
                MinLoad = 0.0;
                MaxLoad = 0.0;
                OptLoad = 0.0;
            }
        }

        void EIRWaterToWaterHeatPump::size() {
            // Tries to size the load side flow rate and capacity, source side flow, and the rated power usage
            // There are two major sections to this function, one if plant sizing is available, and one if not
            // If plant sizing is available, then we can generate sizes for the equipment.  This is done for not-only
            //   autosized fields, but also hard-sized fields so that we can report out significant deviations between
            //   the two values.
            // If plant sizing is not available, it tries to use a companion heat pump coil to do sizing
            //
            auto & tmpCapacity = this->referenceCapacity;
            auto & tmpVolFlow = this->loadSideDesignVolFlowRate;
            int pltLoadSizNum = DataPlant::PlantLoop(this->loadSideLocation.loopNum).PlantSizNum;
            if (pltLoadSizNum > 0) {
                if (DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                    tmpVolFlow = DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizingFactor;
                    if (this->companionHeatPumpCoil) {
                        tmpVolFlow = max(tmpVolFlow, this->companionHeatPumpCoil->loadSideDesignVolFlowRate);
                        this->loadSideDesignVolFlowRate = tmpVolFlow;
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                                   "EIRWaterToWaterHeatPump::size()");
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                                       DataGlobals::CWInitConvTemp,
                                                                       DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                                       "EIRWaterToWaterHeatPump::size()");
                    tmpCapacity = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpVolFlow;
                }
            }
            if(this->loadSideDesignVolFlowRate == DataSizing::AutoSize) {
                this->loadSideDesignVolFlowRate = 1.0;
            }
            if(this->sourceSideDesignVolFlowRate == DataSizing::AutoSize) {
                this->sourceSideDesignVolFlowRate = 1.0;
            }
            if(this->referenceCapacity == DataSizing::AutoSize) {
                this->referenceCapacity = 1000;
            }
            if(this->referenceCOP == DataSizing::AutoSize) {
                this->referenceCOP = 3.14;
            }
        }

        PlantComponent *EIRWaterToWaterHeatPump::factory(int plantTypeOfNum, std::string objectName) {
            if (getInputsWWHP) {
                EIRWaterToWaterHeatPump::processInputForEIRWWHP();
                EIRWaterToWaterHeatPump::pairUpCompanionCoils();
                getInputsWWHP = false;
            }

            for (auto &wwhp : eir_wwhp) {
                if (wwhp.name == UtilityRoutines::MakeUPPERCase(objectName) && wwhp.plantTypeOfNum == plantTypeOfNum) {
                    return &wwhp;
                }
            }

            ShowFatalError("EIR_WWHP factory: Error getting inputs for wwhp named: " + objectName);
            return nullptr;  // LCOV_EXCL_LINE
        }

        void EIRWaterToWaterHeatPump::pairUpCompanionCoils() {
            for (auto &thisHP : eir_wwhp) {
                if (!thisHP.companionCoilName.empty()) {
                    auto thisCoilName = UtilityRoutines::MakeUPPERCase(thisHP.name);
                    auto &thisCoilType = thisHP.plantTypeOfNum;
                    auto targetCompanionName = UtilityRoutines::MakeUPPERCase(thisHP.companionCoilName);
                    for (auto &potentialCompanionCoil : eir_wwhp) {
                        auto &potentialCompanionType = potentialCompanionCoil.plantTypeOfNum;
                        auto potentialCompanionName = UtilityRoutines::MakeUPPERCase(potentialCompanionCoil.name);
                        if (potentialCompanionName == thisCoilName) {
                            // skip the current coil
                            continue;
                        }
                        if (potentialCompanionName == targetCompanionName) {
                            if (thisCoilType == potentialCompanionType) {
                                ShowFatalError(
                                        "I'm sorry, I don't really feel comfortable pairing up coils of the same type.");
                            }
                            thisHP.companionHeatPumpCoil = &potentialCompanionCoil;
                            break;
                        }
                    }
                    if (!thisHP.companionHeatPumpCoil) {
                        ShowSevereError("Could not find matching companion heat pump coil.");
                        ShowContinueError("Base coil: " + thisCoilName);
                        ShowContinueError("Looking for companion coil named: " + targetCompanionName);
                        ShowFatalError("Simulation aborts due to previous severe error");
                    }
                }
            }
        }

        void EIRWaterToWaterHeatPump::processInputForEIRWWHP() {
            using namespace DataIPShortCuts;

            struct ClassType {
                std::string thisType;
                int thisTypeNum;
                std::string nodesType;
                std::function<Real64 (Real64, Real64)> calcLoadOutletTemp;
                std::function<Real64 (Real64, Real64)> calcQsource;
                std::function<Real64 (Real64, Real64)> calcSourceOutletTemp;
                ClassType(
                        std::string _thisType,
                        int _thisTypeNum,
                        std::string _nodesType,
                        std::function<Real64 (Real64, Real64)> _tLoadOutFunc,
                        std::function<Real64 (Real64, Real64)> _qSrcFunc,
                        std::function<Real64 (Real64, Real64)> _tSrcOutFunc) :
                    thisType(std::move(_thisType)),
                    thisTypeNum(_thisTypeNum),
                    nodesType(std::move(_nodesType)),
                    calcLoadOutletTemp(std::move(_tLoadOutFunc)),
                    calcQsource(std::move(_qSrcFunc)),
                    calcSourceOutletTemp(std::move(_tSrcOutFunc))
                {}
            };
            std::vector<ClassType> classesToInput =
                {
                    ClassType{
                        "HeatPump:WaterToWater:EIR:Cooling",
                        DataPlant::TypeOf_HeatPumpEIRCooling,
                        "Chilled Water Nodes",
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::subtract,
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::add,
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::add
                    },
                    ClassType{
                        "HeatPump:WaterToWater:EIR:Heating",
                        DataPlant::TypeOf_HeatPumpEIRHeating,
                        "Hot Water Nodes",
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::add,
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::subtract,
                        EIRWaterToWaterHeatPumps::EIRWaterToWaterHeatPump::subtract
                    }
                };

            bool errorsFound = false;
            for (auto & classToInput : classesToInput) {
                cCurrentModuleObject = classToInput.thisType;
                int numWWHP = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
                if (numWWHP > 0) {
                    auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
                    if (instances == inputProcessor->epJSON.end()) {
                        // Cannot imagine how you would have numWWHP > 0 and yet the instances is empty
                        // this would indicate a major problem in the input processor, not a problem here
                        // I'll still catch this with errorsFound but I cannot make a unit test for it so excluding the line from coverage
                        ShowSevereError("EIR WWHP: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
                        errorsFound = true;  // LCOV_EXCL_LINE
                    }
                    auto &instancesValue = instances.value();
                    for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                        auto const &fields = instance.value();
                        auto const &thisObjectName = instance.key();
                        inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                        EIRWaterToWaterHeatPump thisWWHP;
                        thisWWHP.plantTypeOfNum = classToInput.thisTypeNum;
                        thisWWHP.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                        std::string loadSideInletNodeName = UtilityRoutines::MakeUPPERCase(
                                fields.at("load_side_inlet_node_name")
                        );
                        std::string loadSideOutletNodeName = UtilityRoutines::MakeUPPERCase(
                                fields.at("load_side_outlet_node_name")
                        );
                        std::string sourceSideInletNodeName = UtilityRoutines::MakeUPPERCase(
                                fields.at("source_side_inlet_node_name")
                        );
                        std::string sourceSideOutletNodeName = UtilityRoutines::MakeUPPERCase(
                                fields.at("source_side_outlet_node_name")
                        );
                        if (fields.find("companion_coil_name") != fields.end()) {  // optional field
                            thisWWHP.companionCoilName = UtilityRoutines::MakeUPPERCase(
                                    fields.at("companion_coil_name")
                            );
                        }
                        auto tmpFlowRate = fields.at("load_side_reference_flow_rate");
                        if (tmpFlowRate == "Autosize") {
                            thisWWHP.loadSideDesignVolFlowRate = DataSizing::AutoSize;
                            thisWWHP.loadSideDesignVolFlowRateWasAutoSized = true;
                        } else {
                            thisWWHP.loadSideDesignVolFlowRate = tmpFlowRate;
                        }
                        auto tmpSourceFlowRate = fields.at("source_side_reference_flow_rate");
                        if (tmpSourceFlowRate == "Autosize") {
                            thisWWHP.sourceSideDesignVolFlowRate = DataSizing::AutoSize;
                            thisWWHP.sourceSideDesignVolFlowRateWasAutoSized = true;
                        } else {
                            thisWWHP.sourceSideDesignVolFlowRate = tmpSourceFlowRate;
                        }
                        auto tmpRefCapacity = fields.at("reference_capacity");
                        if (tmpRefCapacity == "Autosize") {
                            thisWWHP.referenceCapacity = DataSizing::AutoSize;
                            thisWWHP.referenceCapacityWasAutoSized = true;
                        } else {
                            thisWWHP.referenceCapacity = tmpRefCapacity;
                        }
                        try {
                            thisWWHP.referenceCOP = fields.at("reference_coefficient_of_performance");
                        } catch (...) {
                            Real64 defaultVal = 0.0;
                            if (!inputProcessor->getDefaultValue(cCurrentModuleObject, "reference_coefficient_of_performance", defaultVal)) {
                                // this error condition would mean that someone broke the input dictionary, not their
                                // input file.  I can't really unit test it so I'll leave it here as a severe error
                                // but excluding it from coverage
                                ShowSevereError("EIR WWHP: Reference COP not entered and could not get default value"); // LCOV_EXCL_LINE
                                errorsFound = true;  // LCOV_EXCL_LINE
                            }
                        }
                        thisWWHP.referenceLeavingLoadSideTemp = fields.at(
                                "reference_leaving_load_side_water_temperature");
                        thisWWHP.referenceEnteringSourceSideTemp = fields.at(
                                "reference_entering_source_side_fluid_temperature");

                        try {
                            thisWWHP.sizingFactor = fields.at("sizing_factor");
                        } catch (...) {
                            Real64 defaultVal = 0.0;
                            if (!inputProcessor->getDefaultValue(cCurrentModuleObject, "sizing_factor", defaultVal)) {
                                // this error condition would mean that someone broke the input dictionary, not their
                                // input file.  I can't really unit test it so I'll leave it here as a severe error
                                // but excluding it from coverage
                                ShowSevereError("EIR WWHP: Sizing factor not entered and could not get default value"); // LCOV_EXCL_LINE
                                errorsFound = true;  // LCOV_EXCL_LINE
                            }
                        }

                        thisWWHP.capFuncTempCurveIndex = CurveManager::GetCurveIndex(UtilityRoutines::MakeUPPERCase(
                                fields.at("capacity_function_of_temperature_curve_name")));
                        thisWWHP.powerRatioFuncTempCurveIndex = CurveManager::GetCurveIndex(
                                UtilityRoutines::MakeUPPERCase(
                                        fields.at(
                                                "electric_input_to_output_ratio_function_of_temperature_curve_name")));
                        thisWWHP.powerRatioFuncPLRCurveIndex = CurveManager::GetCurveIndex(
                                UtilityRoutines::MakeUPPERCase(
                                        fields.at(
                                                "electric_input_to_output_ratio_function_of_temperature_curve_name")));

                        int const flowPath1 = 1, flowPath2 = 2;
                        thisWWHP.loadSideNodes.inlet = NodeInputManager::GetOnlySingleNode(loadSideInletNodeName,
                                                                                           errorsFound,
                                                                                           cCurrentModuleObject,
                                                                                           thisWWHP.name,
                                                                                           DataLoopNode::NodeType_Water,
                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                           flowPath1,
                                                                                           DataLoopNode::ObjectIsNotParent);
                        thisWWHP.loadSideNodes.outlet = NodeInputManager::GetOnlySingleNode(loadSideOutletNodeName,
                                                                                            errorsFound,
                                                                                            cCurrentModuleObject,
                                                                                            thisWWHP.name,
                                                                                            DataLoopNode::NodeType_Water,
                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                            flowPath1,
                                                                                            DataLoopNode::ObjectIsNotParent);
                        thisWWHP.sourceSideNodes.inlet = NodeInputManager::GetOnlySingleNode(sourceSideInletNodeName,
                                                                                             errorsFound,
                                                                                             cCurrentModuleObject,
                                                                                             thisWWHP.name,
                                                                                             DataLoopNode::NodeType_Water,
                                                                                             DataLoopNode::NodeConnectionType_Inlet,
                                                                                             flowPath2,
                                                                                             DataLoopNode::ObjectIsNotParent);
                        thisWWHP.sourceSideNodes.outlet = NodeInputManager::GetOnlySingleNode(sourceSideOutletNodeName,
                                                                                              errorsFound,
                                                                                              cCurrentModuleObject,
                                                                                              thisWWHP.name,
                                                                                              DataLoopNode::NodeType_Water,
                                                                                              DataLoopNode::NodeConnectionType_Outlet,
                                                                                              flowPath2,
                                                                                              DataLoopNode::ObjectIsNotParent);
                        BranchNodeConnections::TestCompSet(
                                cCurrentModuleObject, thisWWHP.name, loadSideInletNodeName, loadSideOutletNodeName,
                                classToInput.nodesType);
                        BranchNodeConnections::TestCompSet(
                                cCurrentModuleObject, thisWWHP.name, sourceSideInletNodeName, sourceSideOutletNodeName,
                                "Condenser Water Nodes");

                        // store the worker functions that generalized the heating/cooling sides
                        thisWWHP.calcLoadOutletTemp = classToInput.calcLoadOutletTemp;
                        thisWWHP.calcQsource = classToInput.calcQsource;
                        thisWWHP.calcSourceOutletTemp = classToInput.calcSourceOutletTemp;

                        if (!errorsFound) {
                            eir_wwhp.push_back(thisWWHP);
                        }
                    }
                }
            }
            if (errorsFound) {
                // currently there are no straightforward unit tests possible to get here
                // all curves are required and inputs are validated by the input processor
                // obviously this will stay here but I don't feel like counting it against coverage
                ShowFatalError("Previous EIR WWHP errors cause program termination");  // LCOV_EXCL_LINE
            }
        }

    } // namespace EIRWaterToWaterHeatPumps
} // namespace EnergyPlus
