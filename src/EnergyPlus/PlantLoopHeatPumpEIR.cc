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

// C++ headers
#include <string>
#include <utility>
#include <vector>

// EnergyPlus headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::EIRPlantLoopHeatPumps {

void EIRPlantLoopHeatPump::simulate(
    EnergyPlusData &state, const EnergyPlus::PlantLocation &calledFromLocation, bool const FirstHVACIteration, Real64 &CurLoad, bool const RunFlag)
{

    // Call initialize to set flow rates, run flag, and entering temperatures
    this->running = RunFlag;

    this->loadSideInletTemp = state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp;
    this->sourceSideInletTemp = state.dataLoopNodes->Node(this->sourceSideNodes.inlet).Temp;

    if (this->waterSource) {
        this->setOperatingFlowRatesWSHP(state);
        if (calledFromLocation.loopNum == this->sourceSideLocation.loopNum) { // condenser side
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->sourceSideLocation.loopNum,
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
    } else if (this->airSource) {
        this->setOperatingFlowRatesASHP(state);
    }

    if (this->running) {
        this->doPhysics(state, CurLoad);
    } else {
        this->resetReportingVariables();
    }

    // update nodes
    state.dataLoopNodes->Node(this->loadSideNodes.outlet).Temp = this->loadSideOutletTemp;
    state.dataLoopNodes->Node(this->sourceSideNodes.outlet).Temp = this->sourceSideOutletTemp;
}

Real64 EIRPlantLoopHeatPump::getLoadSideOutletSetPointTemp(EnergyPlusData &state) const
{
    auto &thisLoadPlantLoop = state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum);
    auto &thisLoadLoopSide = thisLoadPlantLoop.LoopSide(this->loadSideLocation.loopSideNum);
    auto &thisLoadBranch = thisLoadLoopSide.Branch(this->loadSideLocation.branchNum);
    auto &thisLoadComp = thisLoadBranch.Comp(this->loadSideLocation.compNum);
    if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
        if (thisLoadComp.CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
            // there will be a valid set-point on outlet
            return state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPoint;
        } else { // use plant loop overall set-point
            return state.dataLoopNodes->Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPoint;
        }
    } else if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
        if (thisLoadComp.CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
            // there will be a valid set-point on outlet
            return state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPointHi;
        } else { // use plant loop overall set-point
            return state.dataLoopNodes->Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPointHi;
        }
    } else {
        // there's no other enums for loop demand calcs, so I don't have a reasonable unit test for these
        // lines, they simply should not be able to get here.  But a fatal is here anyway just in case,
        // and the lines are excluded from coverage.
        ShowFatalError(state, "Unsupported loop demand calculation scheme in EIR heat pump"); // LCOV_EXCL_LINE
        return -999; // not actually returned with Fatal Error call above  // LCOV_EXCL_LINE
    }
}

void EIRPlantLoopHeatPump::resetReportingVariables()
{
    this->loadSideHeatTransfer = 0.0;
    this->loadSideEnergy = 0.0;
    this->loadSideOutletTemp = this->loadSideInletTemp;
    this->powerUsage = 0.0;
    this->powerEnergy = 0.0;
    this->sourceSideHeatTransfer = 0.0;
    this->sourceSideOutletTemp = this->sourceSideInletTemp;
    this->sourceSideEnergy = 0.0;
}

void EIRPlantLoopHeatPump::setOperatingFlowRatesWSHP(EnergyPlusData &state)
{
    if (!this->running) {
        this->loadSideMassFlowRate = 0.0;
        this->sourceSideMassFlowRate = 0.0;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->loadSideMassFlowRate,
                                             this->loadSideNodes.inlet,
                                             this->loadSideNodes.outlet,
                                             this->loadSideLocation.loopNum,
                                             this->loadSideLocation.loopSideNum,
                                             this->loadSideLocation.branchNum,
                                             this->loadSideLocation.compNum);
        PlantUtilities::SetComponentFlowRate(state,
                                             this->sourceSideMassFlowRate,
                                             this->sourceSideNodes.inlet,
                                             this->sourceSideNodes.outlet,
                                             this->sourceSideLocation.loopNum,
                                             this->sourceSideLocation.loopSideNum,
                                             this->sourceSideLocation.branchNum,
                                             this->sourceSideLocation.compNum);
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->loadSideLocation.loopNum,
                                                    this->loadSideLocation.loopSideNum,
                                                    this->loadSideLocation.branchNum,
                                                    this->loadSideLocation.compNum,
                                                    this->condMassFlowRateTriggerIndex,
                                                    this->sourceSideLocation.loopNum,
                                                    this->sourceSideLocation.loopSideNum,
                                                    DataPlant::iCriteriaType::MassFlowRate,
                                                    this->sourceSideMassFlowRate);
        // Set flows if the heat pump is running
    } else { // the heat pump must run
        this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
        this->sourceSideMassFlowRate = this->sourceSideDesignMassFlowRate;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->loadSideMassFlowRate,
                                             this->loadSideNodes.inlet,
                                             this->loadSideNodes.outlet,
                                             this->loadSideLocation.loopNum,
                                             this->loadSideLocation.loopSideNum,
                                             this->loadSideLocation.branchNum,
                                             this->loadSideLocation.compNum);
        PlantUtilities::SetComponentFlowRate(state,
                                             this->sourceSideMassFlowRate,
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
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->loadSideMassFlowRate,
                                                 this->loadSideNodes.inlet,
                                                 this->loadSideNodes.outlet,
                                                 this->loadSideLocation.loopNum,
                                                 this->loadSideLocation.loopSideNum,
                                                 this->loadSideLocation.branchNum,
                                                 this->loadSideLocation.compNum);
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->sourceSideMassFlowRate,
                                                 this->sourceSideNodes.inlet,
                                                 this->sourceSideNodes.outlet,
                                                 this->sourceSideLocation.loopNum,
                                                 this->sourceSideLocation.loopSideNum,
                                                 this->sourceSideLocation.branchNum,
                                                 this->sourceSideLocation.compNum);
        }
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->loadSideLocation.loopNum,
                                                    this->loadSideLocation.loopSideNum,
                                                    this->loadSideLocation.branchNum,
                                                    this->loadSideLocation.compNum,
                                                    this->condMassFlowRateTriggerIndex,
                                                    this->sourceSideLocation.loopNum,
                                                    this->sourceSideLocation.loopSideNum,
                                                    DataPlant::iCriteriaType::MassFlowRate,
                                                    this->sourceSideMassFlowRate);
    }
}

void EIRPlantLoopHeatPump::setOperatingFlowRatesASHP(EnergyPlusData &state)
{
    if (!this->running) {
        this->loadSideMassFlowRate = 0.0;
        this->sourceSideMassFlowRate = 0.0;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->loadSideMassFlowRate,
                                             this->loadSideNodes.inlet,
                                             this->loadSideNodes.outlet,
                                             this->loadSideLocation.loopNum,
                                             this->loadSideLocation.loopSideNum,
                                             this->loadSideLocation.branchNum,
                                             this->loadSideLocation.compNum);
        // Set flows if the heat pump is running
    } else { // the heat pump must run
        this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
        this->sourceSideMassFlowRate = this->sourceSideDesignMassFlowRate;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->loadSideMassFlowRate,
                                             this->loadSideNodes.inlet,
                                             this->loadSideNodes.outlet,
                                             this->loadSideLocation.loopNum,
                                             this->loadSideLocation.loopSideNum,
                                             this->loadSideLocation.branchNum,
                                             this->loadSideLocation.compNum);

        // if there's no flow in one, try to turn the entire heat pump off
        if (this->loadSideMassFlowRate <= 0.0 || this->sourceSideMassFlowRate <= 0.0) {
            this->loadSideMassFlowRate = 0.0;
            this->sourceSideMassFlowRate = 0.0;
            this->running = false;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->loadSideMassFlowRate,
                                                 this->loadSideNodes.inlet,
                                                 this->loadSideNodes.outlet,
                                                 this->loadSideLocation.loopNum,
                                                 this->loadSideLocation.loopSideNum,
                                                 this->loadSideLocation.branchNum,
                                                 this->loadSideLocation.compNum);
        }
    }
}

void EIRPlantLoopHeatPump::doPhysics(EnergyPlusData &state, Real64 currentLoad)
{

    Real64 const reportingInterval = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // ideally the plant is going to ensure that we don't have a runflag=true when the load is invalid, but
    // I'm not sure we can count on that so we will do one check here to make sure we don't calculate things badly
    if ((this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRCooling && currentLoad >= 0.0) ||
        (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRHeating && currentLoad <= 0.0)) {
        this->resetReportingVariables();
        return;
    }

    // get setpoint on the load side outlet
    Real64 loadSideOutletSetpointTemp = this->getLoadSideOutletSetPointTemp(state);

    // evaluate capacity modifier curve and determine load side heat transfer
    Real64 capacityModifierFuncTemp =
        CurveManager::CurveValue(state, this->capFuncTempCurveIndex, loadSideOutletSetpointTemp, this->sourceSideInletTemp);
    Real64 availableCapacity = this->referenceCapacity * capacityModifierFuncTemp;
    Real64 partLoadRatio = 0.0;
    if (availableCapacity > 0) {
        partLoadRatio = max(0.0, min(std::abs(currentLoad) / availableCapacity, 1.0));
    }

    // evaluate the actual current operating load side heat transfer rate
    auto &thisLoadPlantLoop = state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum);
    Real64 CpLoad = FluidProperties::GetSpecificHeatGlycol(state,
                                                           thisLoadPlantLoop.FluidName,
                                                           state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp,
                                                           thisLoadPlantLoop.FluidIndex,
                                                           "PLHPEIR::simulate()");
    this->loadSideHeatTransfer = availableCapacity * partLoadRatio;
    this->loadSideEnergy = this->loadSideHeatTransfer * reportingInterval;

    // calculate load side outlet conditions
    Real64 const loadMCp = this->loadSideMassFlowRate * CpLoad;
    this->loadSideOutletTemp = this->calcLoadOutletTemp(this->loadSideInletTemp, this->loadSideHeatTransfer / loadMCp);

    // calculate power usage from EIR curves
    Real64 eirModifierFuncTemp =
        CurveManager::CurveValue(state, this->powerRatioFuncTempCurveIndex, this->loadSideOutletTemp, this->sourceSideInletTemp);
    Real64 eirModifierFuncPLR = CurveManager::CurveValue(state, this->powerRatioFuncPLRCurveIndex, partLoadRatio);
    this->powerUsage = (this->loadSideHeatTransfer / this->referenceCOP) * eirModifierFuncPLR * eirModifierFuncTemp;
    this->powerEnergy = this->powerUsage * reportingInterval;

    // energy balance on heat pump
    this->sourceSideHeatTransfer = this->calcQsource(this->loadSideHeatTransfer, this->powerUsage);
    this->sourceSideEnergy = this->sourceSideHeatTransfer * reportingInterval;

    // calculate source side outlet conditions
    Real64 CpSrc = 0.0;
    if (this->waterSource) {
        CpSrc = FluidProperties::GetSpecificHeatGlycol(state,
                                                       thisLoadPlantLoop.FluidName,
                                                       state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp,
                                                       thisLoadPlantLoop.FluidIndex,
                                                       "PLHPEIR::simulate()");
    } else if (this->airSource) {
        CpSrc = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
    }
    Real64 const sourceMCp = this->sourceSideMassFlowRate * CpSrc;
    this->sourceSideOutletTemp = this->calcSourceOutletTemp(this->sourceSideInletTemp, this->sourceSideHeatTransfer / sourceMCp);
}

void EIRPlantLoopHeatPump::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    // This function does all one-time and begin-environment initialization
    std::string static const routineName = std::string("EIRPlantLoopHeatPump :") + __FUNCTION__;
    if (this->oneTimeInit) {
        bool errFlag = false;

        // setup output variables
        SetupOutputVariable(
            state, "Heat Pump Load Side Heat Transfer Rate", OutputProcessor::Unit::W, this->loadSideHeatTransfer, "System", "Average", this->name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->loadSideEnergy,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "ENERGYTRANSFER",
                            _,
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Heat Pump Source Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->sourceSideHeatTransfer,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(
            state, "Heat Pump Source Side Heat Transfer Energy", OutputProcessor::Unit::J, this->sourceSideEnergy, "System", "Sum", this->name);
        SetupOutputVariable(
            state, "Heat Pump Load Side Inlet Temperature", OutputProcessor::Unit::C, this->loadSideInletTemp, "System", "Average", this->name);
        SetupOutputVariable(
            state, "Heat Pump Load Side Outlet Temperature", OutputProcessor::Unit::C, this->loadSideOutletTemp, "System", "Average", this->name);
        SetupOutputVariable(
            state, "Heat Pump Source Side Inlet Temperature", OutputProcessor::Unit::C, this->sourceSideInletTemp, "System", "Average", this->name);
        SetupOutputVariable(
            state, "Heat Pump Source Side Outlet Temperature", OutputProcessor::Unit::C, this->sourceSideOutletTemp, "System", "Average", this->name);
        SetupOutputVariable(state, "Heat Pump Electricity Rate", OutputProcessor::Unit::W, this->powerUsage, "System", "Average", this->name);
        if (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRCooling) { // energy from HeatPump:PlantLoop:EIR:Cooling object
            SetupOutputVariable(state,
                                "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                "System",
                                "Sum",
                                this->name,
                                _,
                                "Electricity",
                                "Cooling",
                                "Heat Pump",
                                "Plant");
        } else if (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRHeating) { // energy from HeatPump:PlantLoop:EIR:Heating object
            SetupOutputVariable(state,
                                "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                "System",
                                "Sum",
                                this->name,
                                _,
                                "Electricity",
                                "Heating",
                                "Heat Pump",
                                "Plant");
        }
        SetupOutputVariable(
            state, "Heat Pump Load Side Mass Flow Rate", OutputProcessor::Unit::kg_s, this->loadSideMassFlowRate, "System", "Average", this->name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->sourceSideMassFlowRate,
                            "System",
                            "Average",
                            this->name);

        // find this component on the plant
        bool thisErrFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->name,
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
            ShowSevereError(state,
                            routineName + ": Plant topology problem for " + DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                this->name + "\"");
            ShowContinueError(state, "Could not locate component's load side connections on a plant loop");
            errFlag = true;
        } else if (this->loadSideLocation.loopSideNum != DataPlant::SupplySide) { // only check if !thisErrFlag
            ShowSevereError(state,
                            routineName + ": Invalid connections for " + DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                this->name + "\"");
            ShowContinueError(state, "The load side connections are not on the Supply Side of a plant loop");
            errFlag = true;
        }

        thisErrFlag = false;
        if (this->waterSource) {
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->name,
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
                ShowSevereError(state,
                                routineName + ": Plant topology problem for " + DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                ShowContinueError(state, "Could not locate component's source side connections on a plant loop");
                errFlag = true;
            } else if (this->sourceSideLocation.loopSideNum != DataPlant::DemandSide) { // only check if !thisErrFlag
                ShowSevereError(state,
                                routineName + ": Invalid connections for " + DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                ShowContinueError(state, "The source side connections are not on the Demand Side of a plant loop");
                errFlag = true;
            }

            // make sure it is not the same loop on both sides.
            if (this->loadSideLocation.loopNum == this->sourceSideLocation.loopNum) { // user is being too tricky, don't allow
                ShowSevereError(state,
                                routineName + ": Invalid connections for " + DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
                                    this->name + "\"");
                ShowContinueError(state, "The load and source sides need to be on different loops.");
                errFlag = true;
            } else {

                PlantUtilities::InterConnectTwoPlantLoopSides(state,
                                                              this->loadSideLocation.loopNum,
                                                              this->loadSideLocation.loopSideNum,
                                                              this->sourceSideLocation.loopNum,
                                                              this->sourceSideLocation.loopSideNum,
                                                              this->plantTypeOfNum,
                                                              true);
            }
        } else if (this->airSource) {
            // nothing to do here ?
        }

        if (errFlag) {
            ShowFatalError(state, routineName + ": Program terminated due to previous condition(s).");
        }
        this->oneTimeInit = false;
    } // plant setup

    if (state.dataGlobal->BeginEnvrnFlag && this->envrnInit && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                       DataGlobalConstants::InitConvTemp,
                                                       state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                       routineName);
        this->loadSideDesignMassFlowRate = rho * this->loadSideDesignVolFlowRate;
        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->loadSideDesignMassFlowRate,
                                           this->loadSideNodes.inlet,
                                           this->loadSideNodes.outlet,
                                           this->loadSideLocation.loopNum,
                                           this->loadSideLocation.loopSideNum,
                                           this->loadSideLocation.branchNum,
                                           this->loadSideLocation.compNum);

        if (this->waterSource) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->sourceSideLocation.loopNum).FluidName,
                                                    DataGlobalConstants::InitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->sourceSideLocation.loopNum).FluidIndex,
                                                    routineName);
            this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->sourceSideDesignMassFlowRate,
                                               this->sourceSideNodes.inlet,
                                               this->sourceSideNodes.outlet,
                                               this->sourceSideLocation.loopNum,
                                               this->sourceSideLocation.loopSideNum,
                                               this->sourceSideLocation.branchNum,
                                               this->sourceSideLocation.compNum);
        } else if (this->airSource) {
            rho = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, state.dataEnvrn->OutDryBulbTemp, 0.0, routineName);
            this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
        }

        this->envrnInit = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->envrnInit = true;
    }
}

void EIRPlantLoopHeatPump::getDesignCapacities(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    if (calledFromLocation.loopNum == this->loadSideLocation.loopNum) {
        this->sizeLoadSide(state);
        if (this->waterSource) {
            this->sizeSrcSideWSHP(state);
        } else if (this->airSource) {
            this->sizeSrcSideASHP(state);
        }
        MinLoad = 0.0;
        MaxLoad = this->referenceCapacity;
        OptLoad = this->referenceCapacity;
    } else {
        MinLoad = 0.0;
        MaxLoad = 0.0;
        OptLoad = 0.0;
    }
}

void EIRPlantLoopHeatPump::sizeLoadSide(EnergyPlusData &state)
{
    // Tries to size the load side flow rate and capacity, source side flow, and the rated power usage
    // There are two major sections to this function, one if plant sizing is available, and one if not
    // If plant sizing is available, then we can generate sizes for the equipment.  This is done for not-only
    //   autosized fields, but also hard-sized fields so that we can report out significant deviations between
    //   the two values.
    // If plant sizing is not available, it tries to use a companion heat pump coil to do sizing

    bool errorsFound = false;

    // these variables will be used throughout this function as a temporary value of that physical state
    Real64 tmpCapacity = this->referenceCapacity;
    Real64 tmpLoadVolFlow = this->loadSideDesignVolFlowRate;

    std::string const typeName = DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum);
    Real64 loadSideInitTemp = DataGlobalConstants::CWInitConvTemp;
    if (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRHeating) {
        loadSideInitTemp = DataGlobalConstants::HWInitConvTemp;
    }

    Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                         state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                         loadSideInitTemp,
                                                         state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                         "EIRPlantLoopHeatPump::size()");
    Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                             state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                             loadSideInitTemp,
                                                             state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                             "EIRPlantLoopHeatPump::size()");

    int pltLoadSizNum = state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).PlantSizNum;
    if (pltLoadSizNum > 0) {
        // this first IF block is really just about calculating the local tmpCapacity and tmpLoadVolFlow values
        // these represent what the unit would size those to, whether it is doing auto-sizing or not
        if (state.dataSize->PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
            tmpLoadVolFlow = state.dataSize->PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizingFactor;
            if (this->companionHeatPumpCoil) {
                tmpLoadVolFlow = max(tmpLoadVolFlow, this->companionHeatPumpCoil->loadSideDesignVolFlowRate);
                if (this->loadSideDesignVolFlowRateWasAutoSized) this->loadSideDesignVolFlowRate = tmpLoadVolFlow;
            }
            tmpCapacity = Cp * rho * state.dataSize->PlantSizData(pltLoadSizNum).DeltaT * tmpLoadVolFlow;
        } else if (this->companionHeatPumpCoil && this->companionHeatPumpCoil->loadSideDesignVolFlowRate > 0.0) {
            tmpLoadVolFlow = this->companionHeatPumpCoil->loadSideDesignVolFlowRate;
            tmpCapacity = Cp * rho * state.dataSize->PlantSizData(pltLoadSizNum).DeltaT * tmpLoadVolFlow;
        } else {
            if (this->referenceCapacityWasAutoSized) tmpCapacity = 0.0;
            if (this->loadSideDesignVolFlowRateWasAutoSized) tmpLoadVolFlow = 0.0;
        }
        // now we actually need to store and report out the values
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            // handle the auto-sizable reference capacity
            if (this->referenceCapacityWasAutoSized) {
                // if auto-sized, we just need to store the sized value and then report out the capacity when plant is ready
                this->referenceCapacity = tmpCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, typeName, this->name, "Design Size Nominal Capacity [W]", tmpCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, typeName, this->name, "Initial Design Size Nominal Capacity [W]", tmpCapacity);
                }
            } else {
                // this blocks means the capacity value was hard-sized
                if (this->referenceCapacity > 0.0 && tmpCapacity > 0.0) {
                    // then the capacity was hard-sized to a good value and the tmpCapacity was calculated to a good value too
                    Real64 hardSizedCapacity = this->referenceCapacity;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        if (state.dataGlobal->DoPlantSizing) {
                            BaseSizer::reportSizerOutput(state,
                                                         typeName,
                                                         this->name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpCapacity,
                                                         "User-Specified Nominal Capacity [W]",
                                                         hardSizedCapacity);
                        } else {
                            BaseSizer::reportSizerOutput(state, typeName, this->name, "User-Specified Nominal Capacity [W]", hardSizedCapacity);
                        }
                        // we can warn here if there is a bit mismatch between hard- and auto-sized
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpCapacity - hardSizedCapacity) / hardSizedCapacity) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowWarningMessage(state, "EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for " + this->name);
                                ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", hardSizedCapacity));
                                ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpCapacity));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    // moving forward with more calculations, we need to update the 'tmp' capacity to the hard-sized value
                    tmpCapacity = hardSizedCapacity;
                }
            }
            // now handle the auto-sizable load side flow rate
            if (this->loadSideDesignVolFlowRateWasAutoSized) {
                this->loadSideDesignVolFlowRate = tmpLoadVolFlow;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, typeName, this->name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadVolFlow);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, typeName, this->name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadVolFlow);
                }
            } else {
                if (this->loadSideDesignVolFlowRate > 0.0 && tmpLoadVolFlow > 0.0) {
                    Real64 hardSizedLoadSideFlow = this->loadSideDesignVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        if (state.dataGlobal->DoPlantSizing) {
                            BaseSizer::reportSizerOutput(state,
                                                         typeName,
                                                         this->name,
                                                         "Design Size Load Side Volume Flow Rate [m3/s]",
                                                         tmpLoadVolFlow,
                                                         "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                         hardSizedLoadSideFlow);
                        } else {
                            BaseSizer::reportSizerOutput(
                                state, typeName, this->name, "User-Specified Load Side Volume Flow Rate [m3/s]", hardSizedLoadSideFlow);
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpLoadVolFlow - hardSizedLoadSideFlow) / hardSizedLoadSideFlow) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for " + this->name);
                                ShowContinueError(state, format("User-Specified Load Side Volume Flow Rate of {:.2R} [m3/s]", hardSizedLoadSideFlow));
                                ShowContinueError(state,
                                                  format("differs from Design Size Load Side Volume Flow Rate of {:.2R} [m3/s]", tmpLoadVolFlow));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpLoadVolFlow = hardSizedLoadSideFlow;
                }
            }
        }
    } else {
        // no plant sizing available...try to use the companion coil
        if (this->companionHeatPumpCoil) {
            if (this->companionHeatPumpCoil->loadSideDesignVolFlowRateWasAutoSized && this->companionHeatPumpCoil->loadSideDesignVolFlowRate > 0.0) {
                tmpLoadVolFlow = this->companionHeatPumpCoil->loadSideDesignVolFlowRate;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->loadSideDesignVolFlowRate = tmpLoadVolFlow;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, typeName, this->name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadVolFlow);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, typeName, this->name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadVolFlow);
                    }
                }
            }
            if (this->companionHeatPumpCoil->referenceCapacityWasAutoSized && this->companionHeatPumpCoil->referenceCapacity > 0.0) {
                tmpCapacity = this->companionHeatPumpCoil->referenceCapacity;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->referenceCapacity = tmpCapacity;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, typeName, this->name, "Design Size Nominal Capacity [W]", tmpCapacity);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, typeName, this->name, "Initial Design Size Nominal Capacity [W]", tmpCapacity);
                    }
                }
            }
        } else {
            // no companion coil, and no plant sizing, so can't do anything
            if ((this->loadSideDesignVolFlowRateWasAutoSized || this->referenceCapacityWasAutoSized) &&
                state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "EIRPlantLoopHeatPump::size(): Autosizing requires a loop Sizing:Plant object.");
                ShowContinueError(state, "Occurs in HeatPump:PlantLoop:EquationFit:Cooling object = " + this->name);
                errorsFound = true;
            }
        }
        if (!this->loadSideDesignVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, typeName, this->name, "User-Specified Load Side Flow Rate [m3/s]", this->loadSideDesignVolFlowRate);
        }
        if (!this->referenceCapacityWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, typeName, this->name, "User-Specified Nominal Capacity [W]", this->referenceCapacity);
        }
    }
    if (errorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void EIRPlantLoopHeatPump::sizeSrcSideWSHP(EnergyPlusData &state)
{
    // size the source-side for the water-source HP
    bool errorsFound = false;

    // these variables will be used throughout this function as a temporary value of that physical state
    Real64 tmpCapacity = this->referenceCapacity;
    Real64 tmpLoadVolFlow = this->loadSideDesignVolFlowRate;
    Real64 tmpSourceVolFlow;

    std::string const typeName = DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum);
    Real64 sourceSideInitTemp = DataGlobalConstants::HWInitConvTemp;
    if (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRHeating) {
        sourceSideInitTemp = DataGlobalConstants::CWInitConvTemp;
    }

    Real64 const rhoSrc = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                            sourceSideInitTemp,
                                                            state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                            "EIRPlantLoopHeatPump::size()");
    Real64 const CpSrc = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidName,
                                                                sourceSideInitTemp,
                                                                state.dataPlnt->PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
                                                                "EIRPlantLoopHeatPump::size()");

    // To start we need to override the calculated load side flow
    // rate if it was actually hard-sized
    if (!this->loadSideDesignVolFlowRateWasAutoSized) tmpLoadVolFlow = this->loadSideDesignVolFlowRate;

    // calculate an auto-sized value for source design flow regardless of whether it was auto-sized or not
    int plantSourceSizingIndex = state.dataPlnt->PlantLoop(this->sourceSideLocation.loopNum).PlantSizNum;
    if (plantSourceSizingIndex > 0) {
        // to get the source flow, we first must calculate the required heat impact on the source side
        // First the definition of COP: COP = Qload/Power, therefore Power = Qload/COP
        // Then the energy balance:     Qsrc = Qload + Power
        // Substituting for Power:      Qsrc = Qload + Qload/COP, therefore Qsrc = Qload (1 + 1/COP)
        Real64 const designSourceSideHeatTransfer = tmpCapacity * (1 + 1 / this->referenceCOP);
        // To get the design source flow rate, just apply the sensible heat rate equation:
        //                              Qsrc = rho_src * Vdot_src * Cp_src * DeltaT_src
        //                              Vdot_src = Q_src / (rho_src * Cp_src * DeltaT_src)
        tmpSourceVolFlow = designSourceSideHeatTransfer / (state.dataSize->PlantSizData(plantSourceSizingIndex).DeltaT * CpSrc * rhoSrc);
    } else {
        // just assume it's the same as the load side if we don't have any sizing information
        tmpSourceVolFlow = tmpLoadVolFlow;
    }
    if (this->sourceSideDesignVolFlowRateWasAutoSized) {
        this->sourceSideDesignVolFlowRate = tmpSourceVolFlow;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, typeName, this->name, "Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceVolFlow);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, typeName, this->name, "Initial Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceVolFlow);
        }
    } else {
        // source design flow was hard-sized
        if (this->sourceSideDesignVolFlowRate > 0.0 && tmpSourceVolFlow > 0.0) {
            Real64 const hardSizedSourceSideFlow = this->sourceSideDesignVolFlowRate;
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (state.dataGlobal->DoPlantSizing) {
                    BaseSizer::reportSizerOutput(state,
                                                 typeName,
                                                 this->name,
                                                 "Design Size Source Side Volume Flow Rate [m3/s]",
                                                 tmpSourceVolFlow,
                                                 "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                 hardSizedSourceSideFlow);
                } else {
                    BaseSizer::reportSizerOutput(
                        state, typeName, this->name, "User-Specified Source Side Volume Flow Rate [m3/s]", hardSizedSourceSideFlow);
                }
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(tmpSourceVolFlow - hardSizedSourceSideFlow) / hardSizedSourceSideFlow) >
                        state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state, "EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for " + this->name);
                        ShowContinueError(state, format("User-Specified Source Side Volume Flow Rate of {:.2R} [m3/s]", hardSizedSourceSideFlow));
                        ShowContinueError(state, format("differs from Design Size Source Side Volume Flow Rate of {:.2R} [m3/s]", tmpSourceVolFlow));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
            tmpSourceVolFlow = hardSizedSourceSideFlow;
        }
    }

    // skipping autosized power section

    // register the design volume flows with the plant, only doing half of source because the companion
    // is generally on the same loop
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->loadSideNodes.inlet, tmpLoadVolFlow);
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->sourceSideNodes.inlet, tmpSourceVolFlow / 0.5);

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        // create predefined report
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->name, typeName);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->name, this->referenceCOP);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->name, this->referenceCapacity);
    }

    if (errorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void EIRPlantLoopHeatPump::sizeSrcSideASHP(EnergyPlusData &state)
{
    // size the source-side for the air-source HP
    bool errorsFound = false;

    // these variables will be used throughout this function as a temporary value of that physical state
    Real64 tmpCapacity = this->referenceCapacity;
    Real64 tmpLoadVolFlow = this->loadSideDesignVolFlowRate;
    Real64 tmpSourceVolFlow = 0.0;

    std::string const typeName = DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum);

    // will leave like this for now
    // need to update these to better values later
    Real64 sourceSideInitTemp = 20;
    Real64 sourceSideHumRat = 0.0;
    if (this->plantTypeOfNum == DataPlant::TypeOf_HeatPumpEIRHeating) {
        // same here; update later
        sourceSideInitTemp = 20;
    }

    Real64 const rhoSrc = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, sourceSideInitTemp, sourceSideHumRat);
    Real64 const CpSrc = Psychrometrics::PsyCpAirFnW(sourceSideHumRat);

    // set the source-side flow rate
    if (this->sourceSideDesignVolFlowRateWasAutoSized) {
        // load-side capacity should already be set, so unless the flow rate is specified, we can set
        // an assumed reasonable flow rate since this doesn't affect downstream components
        Real64 DeltaT_src = 10;
        // to get the source flow, we first must calculate the required heat impact on the source side
        // First the definition of COP: COP = Qload/Power, therefore Power = Qload/COP
        // Then the energy balance:     Qsrc = Qload + Power
        // Substituting for Power:      Qsrc = Qload + Qload/COP, therefore Qsrc = Qload (1 + 1/COP)
        Real64 const designSourceSideHeatTransfer = tmpCapacity * (1 + 1 / this->referenceCOP);
        // To get the design source flow rate, just apply the sensible heat rate equation:
        //                              Qsrc = rho_src * Vdot_src * Cp_src * DeltaT_src
        //                              Vdot_src = Q_src / (rho_src * Cp_src * DeltaT_src)
        tmpSourceVolFlow = designSourceSideHeatTransfer / (rhoSrc * CpSrc * DeltaT_src);
    } else if (!this->sourceSideDesignVolFlowRateWasAutoSized && this->sourceSideDesignVolFlowRate > 0) {
        // given the value by the user
        // set it directly
        tmpSourceVolFlow = this->sourceSideDesignVolFlowRate;
    } else if (!this->sourceSideDesignVolFlowRateWasAutoSized && this->sourceSideDesignVolFlowRate == 0) { // LCOV_EXCL_LINE
        // user gave a flow rate of 0
        // protected by the input processor to be >0.0
        // fatal out just in case
        errorsFound = true; // LCOV_EXCL_LINE
        ShowSevereError(state,
                        format("Invalid condenser flow rate for EIR PLHP (name={}; entered value: {}",
                               this->name,
                               this->sourceSideDesignVolFlowRate)); // LCOV_EXCL_LINE
    } else {
        // can't imagine how it would ever get to this point
        // just assume it's the same as the load side if we don't have any sizing information
        tmpSourceVolFlow = tmpLoadVolFlow; // LCOV_EXCL_LINE
    }

    this->sourceSideDesignVolFlowRate = tmpSourceVolFlow;

    if (errorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination"); // LCOV_EXCL_LINE
    }
}

PlantComponent *EIRPlantLoopHeatPump::factory(EnergyPlusData &state, int hp_type_of_num, const std::string &hp_name)
{
    if (state.dataEIRPlantLoopHeatPump->getInputsPLHP) {
        EIRPlantLoopHeatPump::processInputForEIRPLHP(state);
        EIRPlantLoopHeatPump::pairUpCompanionCoils(state);
        state.dataEIRPlantLoopHeatPump->getInputsPLHP = false;
    }

    for (auto &plhp : state.dataEIRPlantLoopHeatPump->heatPumps) {
        if (plhp.name == UtilityRoutines::MakeUPPERCase(hp_name) && plhp.plantTypeOfNum == hp_type_of_num) {
            return &plhp;
        }
    }

    ShowFatalError(state, "EIR Plant Loop Heat Pump factory: Error getting inputs for PLHP named: " + hp_name);
    return nullptr; // LCOV_EXCL_LINE
}

void EIRPlantLoopHeatPump::pairUpCompanionCoils(EnergyPlusData &state)
{
    for (auto &thisHP : state.dataEIRPlantLoopHeatPump->heatPumps) {
        if (!thisHP.companionCoilName.empty()) {
            auto thisCoilName = UtilityRoutines::MakeUPPERCase(thisHP.name);
            auto &thisCoilType = thisHP.plantTypeOfNum;
            auto targetCompanionName = UtilityRoutines::MakeUPPERCase(thisHP.companionCoilName);
            for (auto &potentialCompanionCoil : state.dataEIRPlantLoopHeatPump->heatPumps) {
                auto &potentialCompanionType = potentialCompanionCoil.plantTypeOfNum;
                auto potentialCompanionName = UtilityRoutines::MakeUPPERCase(potentialCompanionCoil.name);
                if (potentialCompanionName == thisCoilName) {
                    // skip the current coil
                    continue;
                }
                if (potentialCompanionName == targetCompanionName) {
                    if (thisCoilType == potentialCompanionType) {
                        ShowSevereError(state, "Invalid companion specification for EIR Plant Loop Heat Pump named \"" + thisCoilName + "\"");
                        ShowContinueError(state, "For heating objects, the companion must be a cooling object, and vice-versa");
                        ShowFatalError(state, "Invalid companion object causes program termination");
                    }
                    thisHP.companionHeatPumpCoil = &potentialCompanionCoil;
                    break;
                }
            }
            if (!thisHP.companionHeatPumpCoil) {
                ShowSevereError(state, "Could not find matching companion heat pump coil.");
                ShowContinueError(state, "Base coil: " + thisCoilName);
                ShowContinueError(state, "Looking for companion coil named: " + targetCompanionName);
                ShowFatalError(state, "Simulation aborts due to previous severe error");
            }
        }
    }
}

void EIRPlantLoopHeatPump::processInputForEIRPLHP(EnergyPlusData &state)
{

    struct ClassType
    {
        std::string thisType;
        int thisTypeNum;
        std::string nodesType;
        std::function<Real64(Real64, Real64)> calcLoadOutletTemp;
        std::function<Real64(Real64, Real64)> calcQsource;
        std::function<Real64(Real64, Real64)> calcSourceOutletTemp;

        ClassType(std::string _thisType,
                  int _thisTypeNum,
                  std::string _nodesType,
                  std::function<Real64(Real64, Real64)> _tLoadOutFunc,
                  std::function<Real64(Real64, Real64)> _qSrcFunc,
                  std::function<Real64(Real64, Real64)> _tSrcOutFunc)
            : thisType(std::move(_thisType)), thisTypeNum(_thisTypeNum), nodesType(std::move(_nodesType)),
              calcLoadOutletTemp(std::move(_tLoadOutFunc)), calcQsource(std::move(_qSrcFunc)), calcSourceOutletTemp(std::move(_tSrcOutFunc))
        {
        }
    };
    std::vector<ClassType> classesToInput = {ClassType{"HeatPump:PlantLoop:EIR:Cooling",
                                                       DataPlant::TypeOf_HeatPumpEIRCooling,
                                                       "Chilled Water Nodes",
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract,
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add,
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add},
                                             ClassType{"HeatPump:PlantLoop:EIR:Heating",
                                                       DataPlant::TypeOf_HeatPumpEIRHeating,
                                                       "Hot Water Nodes",
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add,
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract,
                                                       EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract}};

    bool errorsFound = false;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    for (auto &classToInput : classesToInput) {
        cCurrentModuleObject = classToInput.thisType;
        int numPLHP = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (numPLHP > 0) {
            auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
            if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
                // Cannot imagine how you would have numPLHP > 0 and yet the instances is empty
                // this would indicate a major problem in the input processor, not a problem here
                // I'll still catch this with errorsFound but I cannot make a unit test for it so excluding the line from coverage
                ShowSevereError(state,                                                                   // LCOV_EXCL_LINE
                                "EIR PLHP: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
                errorsFound = true;                                                                      // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                EIRPlantLoopHeatPump thisPLHP;
                thisPLHP.plantTypeOfNum = classToInput.thisTypeNum;
                thisPLHP.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                std::string loadSideInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("load_side_inlet_node_name"));
                std::string loadSideOutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("load_side_outlet_node_name"));
                std::string condenserType = UtilityRoutines::MakeUPPERCase(fields.at("condenser_type"));
                std::string sourceSideInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("source_side_inlet_node_name"));
                std::string sourceSideOutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("source_side_outlet_node_name"));
                if (fields.find("companion_heat_pump_name") != fields.end()) { // optional field
                    thisPLHP.companionCoilName = UtilityRoutines::MakeUPPERCase(fields.at("companion_heat_pump_name"));
                }
                auto tmpFlowRate = fields.at("load_side_reference_flow_rate");
                if (tmpFlowRate == "Autosize") {
                    thisPLHP.loadSideDesignVolFlowRate = DataSizing::AutoSize;
                    thisPLHP.loadSideDesignVolFlowRateWasAutoSized = true;
                } else {
                    thisPLHP.loadSideDesignVolFlowRate = tmpFlowRate;
                }
                auto tmpSourceFlowRate = fields.at("source_side_reference_flow_rate");
                if (tmpSourceFlowRate == "Autosize") {
                    thisPLHP.sourceSideDesignVolFlowRate = DataSizing::AutoSize;
                    thisPLHP.sourceSideDesignVolFlowRateWasAutoSized = true;
                } else {
                    thisPLHP.sourceSideDesignVolFlowRate = tmpSourceFlowRate;
                }
                auto tmpRefCapacity = fields.at("reference_capacity");
                if (tmpRefCapacity == "Autosize") {
                    thisPLHP.referenceCapacity = DataSizing::AutoSize;
                    thisPLHP.referenceCapacityWasAutoSized = true;
                } else {
                    thisPLHP.referenceCapacity = tmpRefCapacity;
                }

                if (fields.find("reference_coefficient_of_performance") != fields.end()) {
                    thisPLHP.referenceCOP = fields.at("reference_coefficient_of_performance");
                } else {
                    Real64 defaultVal = 0.0;
                    if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                            state, cCurrentModuleObject, "reference_coefficient_of_performance", defaultVal)) {
                        // this error condition would mean that someone broke the input dictionary, not their
                        // input file.  I can't really unit test it so I'll leave it here as a severe error
                        // but excluding it from coverage
                        ShowSevereError(state,                                                                  // LCOV_EXCL_LINE
                                        "EIR PLHP: Reference COP not entered and could not get default value"); // LCOV_EXCL_LINE
                        errorsFound = true;                                                                     // LCOV_EXCL_LINE
                    } else {
                        thisPLHP.referenceCOP = defaultVal;
                    }
                }

                if (fields.find("sizing_factor") != fields.end()) {
                    thisPLHP.sizingFactor = fields.at("sizing_factor");
                } else {
                    Real64 defaultVal = 0.0;
                    if (!state.dataInputProcessing->inputProcessor->getDefaultValue(state, cCurrentModuleObject, "sizing_factor", defaultVal)) {
                        // this error condition would mean that someone broke the input dictionary, not their
                        // input file.  I can't really unit test it so I'll leave it here as a severe error
                        // but excluding it from coverage
                        ShowSevereError(state,                                                                  // LCOV_EXCL_LINE
                                        "EIR PLHP: Sizing factor not entered and could not get default value"); // LCOV_EXCL_LINE
                        errorsFound = true;                                                                     // LCOV_EXCL_LINE
                    } else {
                        thisPLHP.sizingFactor = defaultVal;
                    }
                }

                auto &capFtName = fields.at("capacity_modifier_function_of_temperature_curve_name");
                thisPLHP.capFuncTempCurveIndex = CurveManager::GetCurveIndex(state, UtilityRoutines::MakeUPPERCase(capFtName));
                if (thisPLHP.capFuncTempCurveIndex == 0) {
                    ShowSevereError(
                        state, "Invalid curve name for EIR PLHP (name=" + thisPLHP.name + "; entered curve name: " + capFtName.get<std::string>());
                    errorsFound = true;
                }
                auto &eirFtName = fields.at("electric_input_to_output_ratio_modifier_function_of_temperature_curve_name");
                thisPLHP.powerRatioFuncTempCurveIndex = CurveManager::GetCurveIndex(state, UtilityRoutines::MakeUPPERCase(eirFtName));
                if (thisPLHP.capFuncTempCurveIndex == 0) {
                    ShowSevereError(
                        state, "Invalid curve name for EIR PLHP (name=" + thisPLHP.name + "; entered curve name: " + eirFtName.get<std::string>());
                    errorsFound = true;
                }
                auto &eirFplrName = fields.at("electric_input_to_output_ratio_modifier_function_of_part_load_ratio_curve_name");
                thisPLHP.powerRatioFuncPLRCurveIndex = CurveManager::GetCurveIndex(state, UtilityRoutines::MakeUPPERCase(eirFplrName));
                if (thisPLHP.capFuncTempCurveIndex == 0) {
                    ShowSevereError(
                        state, "Invalid curve name for EIR PLHP (name=" + thisPLHP.name + "; entered curve name: " + eirFplrName.get<std::string>());
                    errorsFound = true;
                }

                bool nodeErrorsFound = false;
                thisPLHP.loadSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                   loadSideInletNodeName,
                                                                                   nodeErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   thisPLHP.name,
                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Primary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisPLHP.loadSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                    loadSideOutletNodeName,
                                                                                    nodeErrorsFound,
                                                                                    cCurrentModuleObject,
                                                                                    thisPLHP.name,
                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Primary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                DataLoopNode::NodeFluidType condenserNodeType = DataLoopNode::NodeFluidType::blank;
                DataLoopNode::NodeConnectionType condenserNodeConnectionType_Inlet = DataLoopNode::NodeConnectionType::blank;
                DataLoopNode::NodeConnectionType condenserNodeConnectionType_Outlet = DataLoopNode::NodeConnectionType::blank;
                if (condenserType == "WATERSOURCE") {
                    thisPLHP.waterSource = true;
                    condenserNodeType = DataLoopNode::NodeFluidType::Water;
                    condenserNodeConnectionType_Inlet = DataLoopNode::NodeConnectionType::Inlet;
                    condenserNodeConnectionType_Outlet = DataLoopNode::NodeConnectionType::Outlet;
                } else if (condenserType == "AIRSOURCE") {
                    thisPLHP.airSource = true;
                    condenserNodeType = DataLoopNode::NodeFluidType::Air;
                    condenserNodeConnectionType_Inlet = DataLoopNode::NodeConnectionType::OutsideAir;
                    condenserNodeConnectionType_Outlet = DataLoopNode::NodeConnectionType::OutsideAir;
                } else {
                    // Again, this should be protected by the input processor
                    ShowErrorMessage(state,
                                     "Invalid heat pump condenser type (name=" + thisPLHP.name + // LCOV_EXCL_LINE
                                         "; entered type: " + condenserType);                    // LCOV_EXCL_LINE
                    errorsFound = true;                                                          // LCOV_EXCL_LINE
                }
                thisPLHP.sourceSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                     sourceSideInletNodeName,
                                                                                     nodeErrorsFound,
                                                                                     cCurrentModuleObject,
                                                                                     thisPLHP.name,
                                                                                     condenserNodeType,
                                                                                     condenserNodeConnectionType_Inlet,
                                                                                     NodeInputManager::compFluidStream::Secondary,
                                                                                     DataLoopNode::ObjectIsNotParent);
                thisPLHP.sourceSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                      sourceSideOutletNodeName,
                                                                                      nodeErrorsFound,
                                                                                      cCurrentModuleObject,
                                                                                      thisPLHP.name,
                                                                                      condenserNodeType,
                                                                                      condenserNodeConnectionType_Outlet,
                                                                                      NodeInputManager::compFluidStream::Secondary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (nodeErrorsFound) errorsFound = true;
                BranchNodeConnections::TestCompSet(
                    state, cCurrentModuleObject, thisPLHP.name, loadSideInletNodeName, loadSideOutletNodeName, classToInput.nodesType);

                if (thisPLHP.waterSource) {
                    BranchNodeConnections::TestCompSet(
                        state, cCurrentModuleObject, thisPLHP.name, sourceSideInletNodeName, sourceSideOutletNodeName, "Condenser Water Nodes");
                }

                // store the worker functions that generalized the heating/cooling sides
                thisPLHP.calcLoadOutletTemp = classToInput.calcLoadOutletTemp;
                thisPLHP.calcQsource = classToInput.calcQsource;
                thisPLHP.calcSourceOutletTemp = classToInput.calcSourceOutletTemp;

                if (!errorsFound) {
                    state.dataEIRPlantLoopHeatPump->heatPumps.push_back(thisPLHP);
                }
            }
        }
    }
    if (errorsFound) {
        // currently there are no straightforward unit tests possible to get here
        // all curves are required and inputs are validated by the input processor
        // obviously this will stay here but I don't feel like counting it against coverage
        ShowFatalError(state, "Previous EIR PLHP errors cause program termination"); // LCOV_EXCL_LINE
    }
}

void EIRPlantLoopHeatPump::checkConcurrentOperation(EnergyPlusData &state)
{
    // This will do a recurring warning for concurrent companion operation.
    // This function should be called at the end of the time-step to ensure any iteration-level operation
    //  is worked out and the results are final.
    // This function does not try to be intelligent about only reporting for one of the companions.  The only
    //  way I could think of was to have a vector, either static here or in the namespace, that would hold
    //  companion index values as I warn against their partner, so then I would have to add the values to the
    //  vector each pass, and check then each loop.  This seemed really bulky and inefficient, so I chose to
    //  leave a tight loop here of just reporting for each coil if it and the companion are running.
    for (auto &thisPLHP : state.dataEIRPlantLoopHeatPump->heatPumps) {
        if (!thisPLHP.companionHeatPumpCoil) {
            continue;
        }
        if (thisPLHP.running && thisPLHP.companionHeatPumpCoil->running) {
            ShowRecurringWarningErrorAtEnd(state,
                                           "Companion heat pump objects running concurrently, check operation.  Base object name: " + thisPLHP.name,
                                           thisPLHP.recurringConcurrentOperationWarningIndex);
        }
    }
}
} // namespace EnergyPlus::EIRPlantLoopHeatPumps
