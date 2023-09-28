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

// C++ headers
#include <algorithm>
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
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::EIRPlantLoopHeatPumps {

constexpr Real64 Fahrenheit2Celsius(Real64 F)
{
    return (F - 32.0) * 5.0 / 9.0;
}

void EIRPlantLoopHeatPump::simulate(
    EnergyPlusData &state, const EnergyPlus::PlantLocation &calledFromLocation, bool const FirstHVACIteration, Real64 &CurLoad, bool const RunFlag)
{

    // Call initialize to set flow rates, run flag, and entering temperatures
    this->running = RunFlag;

    this->loadSideInletTemp = state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp;
    this->sourceSideInletTemp = state.dataLoopNodes->Node(this->sourceSideNodes.inlet).Temp;

    if (this->waterSource) {
        this->setOperatingFlowRatesWSHP(state, FirstHVACIteration);
        if (calledFromLocation.loopNum == this->sourceSidePlantLoc.loopNum) { // condenser side
            Real64 sourceQdotArg = 0.0;                                       // pass negative if heat pump heating
            if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
                sourceQdotArg = this->sourceSideHeatTransfer * DataPrecisionGlobals::constant_minusone;
            } else {
                sourceQdotArg = this->sourceSideHeatTransfer;
            }
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->sourceSidePlantLoc.loopNum,
                                                                this->sourceSidePlantLoc.loopSideNum,
                                                                this->EIRHPType,
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
        this->setOperatingFlowRatesASHP(state, FirstHVACIteration);
    }

    if (this->running) {
        if (this->sysControlType == ControlType::Setpoint) {
            Real64 leavingSetpoint = state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPoint;
            Real64 CurSpecHeat = FluidProperties::GetSpecificHeatGlycol(state,
                                                                        state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                                        loadSideInletTemp,
                                                                        state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                                        "EIRPlantLoopHeatPump::simulate");
            Real64 controlLoad = this->loadSideMassFlowRate * CurSpecHeat * (leavingSetpoint - loadSideInletTemp);
            this->doPhysics(state, controlLoad);
        } else {
            this->doPhysics(state, CurLoad);
        }
    } else {
        this->resetReportingVariables();
    }

    // update nodes
    PlantUtilities::SafeCopyPlantNode(state, this->loadSideNodes.inlet, this->loadSideNodes.outlet);
    state.dataLoopNodes->Node(this->loadSideNodes.outlet).Temp = this->loadSideOutletTemp;
    if (this->waterSource) {
        PlantUtilities::SafeCopyPlantNode(state, this->sourceSideNodes.inlet, this->sourceSideNodes.outlet);
    }
    state.dataLoopNodes->Node(this->sourceSideNodes.outlet).Temp = this->sourceSideOutletTemp;
}

Real64 EIRPlantLoopHeatPump::getLoadSideOutletSetPointTemp(EnergyPlusData &state) const
{
    auto &thisLoadPlantLoop = state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum);
    auto &thisLoadLoopSide = thisLoadPlantLoop.LoopSide(this->loadSidePlantLoc.loopSideNum);
    auto &thisLoadBranch = thisLoadLoopSide.Branch(this->loadSidePlantLoc.branchNum);
    auto &thisLoadComp = thisLoadBranch.Comp(this->loadSidePlantLoc.compNum);
    if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::SingleSetPoint) {
        if (thisLoadComp.CurOpSchemeType == DataPlant::OpScheme::CompSetPtBased) {
            // there will be a valid set-point on outlet
            return state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPoint;
        } else { // use plant loop overall set-point
            return state.dataLoopNodes->Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPoint;
        }
    } else if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand) {
        if (thisLoadComp.CurOpSchemeType == DataPlant::OpScheme::CompSetPtBased) {
            // there will be a valid set-point on outlet
            if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) {
                return state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPointHi;
            } else {
                return state.dataLoopNodes->Node(this->loadSideNodes.outlet).TempSetPointLo;
            }
        } else { // use plant loop overall set-point
            if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) {
                return state.dataLoopNodes->Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPointHi;
            } else {
                return state.dataLoopNodes->Node(thisLoadPlantLoop.TempSetPointNodeNum).TempSetPointLo;
            }
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
    this->defrostEnergyRate = 0.0;
    this->defrostEnergy = 0.0;
    this->loadDueToDefrost = 0.0;
    this->fractionalDefrostTime = 0.0;
    this->partLoadRatio = 0.0;
    this->cyclingRatio = 0.0;
}

void EIRPlantLoopHeatPump::setOperatingFlowRatesWSHP(EnergyPlusData &state, bool FirstHVACIteration)
{
    if (!this->running) {
        this->loadSideMassFlowRate = 0.0;
        this->sourceSideMassFlowRate = 0.0;

        PlantUtilities::SetComponentFlowRate(
            state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
        PlantUtilities::SetComponentFlowRate(
            state, this->sourceSideMassFlowRate, this->sourceSideNodes.inlet, this->sourceSideNodes.outlet, this->sourceSidePlantLoc);
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->loadSidePlantLoc,
                                                    this->condMassFlowRateTriggerIndex,
                                                    this->sourceSidePlantLoc,
                                                    DataPlant::CriteriaType::MassFlowRate,
                                                    this->sourceSideMassFlowRate);
        // Set flows if the heat pump is running
    } else { // the heat pump must run
        // apply min/max operating limits based on source side entering fluid temperature
        if (this->minSourceTempLimit > this->sourceSideInletTemp || this->maxSourceTempLimit < this->sourceSideInletTemp) {
            this->loadSideMassFlowRate = (this->heatRecoveryHeatPump) ? state.dataLoopNodes->Node(this->loadSideNodes.inlet).MassFlowRate : 0.0;
            this->sourceSideMassFlowRate = (this->heatRecoveryHeatPump) ? state.dataLoopNodes->Node(this->sourceSideNodes.inlet).MassFlowRate : 0.0;
            this->running = false;
        } else {
            this->loadSideMassFlowRate =
                (this->heatRecoveryHeatPump) ? state.dataLoopNodes->Node(this->loadSideNodes.inlet).MassFlowRate : this->loadSideDesignMassFlowRate;
            this->sourceSideMassFlowRate = (this->heatRecoveryHeatPump) ? state.dataLoopNodes->Node(this->sourceSideNodes.inlet).MassFlowRate
                                                                        : this->sourceSideDesignMassFlowRate;

            if (!FirstHVACIteration && this->flowControl == DataPlant::FlowMode::VariableSpeedPump) {
                if ((this->loadVSBranchPump || this->loadVSLoopPump) && !this->heatRecoveryHeatPump) {
                    this->loadSideMassFlowRate *= std::max(this->partLoadRatio, this->minimumPLR);
                    if (this->loadVSBranchPump) {
                        this->loadSideMassFlowRate = std::max(this->loadSideMassFlowRate, this->loadVSPumpMinLimitMassFlow);
                    }
                }
                if ((this->sourceVSBranchPump || this->sourceVSLoopPump) && !this->heatRecoveryHeatPump) {
                    this->sourceSideMassFlowRate *= std::max(this->partLoadRatio, this->minimumPLR);
                    if (this->sourceVSBranchPump) {
                        this->sourceSideMassFlowRate = std::max(this->sourceSideMassFlowRate, this->sourceVSPumpMinLimitMassFlow);
                    }
                }
            }

            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
            PlantUtilities::SetComponentFlowRate(
                state, this->sourceSideMassFlowRate, this->sourceSideNodes.inlet, this->sourceSideNodes.outlet, this->sourceSidePlantLoc);
        }

        // if there's no flow in one, try to turn the entire heat pump off
        if (this->loadSideMassFlowRate <= 0.0 || this->sourceSideMassFlowRate <= 0.0) {
            this->loadSideMassFlowRate = 0.0;
            this->sourceSideMassFlowRate = 0.0;
            this->running = false;
            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
            PlantUtilities::SetComponentFlowRate(
                state, this->sourceSideMassFlowRate, this->sourceSideNodes.inlet, this->sourceSideNodes.outlet, this->sourceSidePlantLoc);
        }
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->loadSidePlantLoc,
                                                    this->condMassFlowRateTriggerIndex,
                                                    this->sourceSidePlantLoc,
                                                    DataPlant::CriteriaType::MassFlowRate,
                                                    this->sourceSideMassFlowRate);
    }
}

void EIRPlantLoopHeatPump::setOperatingFlowRatesASHP(EnergyPlusData &state, bool FirstHVACIteration)
{
    if (!this->running) {
        this->loadSideMassFlowRate = 0.0;
        this->sourceSideMassFlowRate = 0.0;
        PlantUtilities::SetComponentFlowRate(
            state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
        // Set flows if the heat pump is running
    } else { // the heat pump must run
        // apply min/max operating limits based on source side entering fluid temperature
        if (this->minSourceTempLimit > this->sourceSideInletTemp || this->maxSourceTempLimit < this->sourceSideInletTemp) {
            this->loadSideMassFlowRate = 0.0;
            this->sourceSideMassFlowRate = 0.0;
            this->running = false;
            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
        } else {
            this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
            this->sourceSideMassFlowRate = this->sourceSideDesignMassFlowRate;

            if (!FirstHVACIteration && this->flowControl == DataPlant::FlowMode::VariableSpeedPump) {
                if (this->loadVSBranchPump || this->loadVSLoopPump) {
                    this->loadSideMassFlowRate *= std::max(this->partLoadRatio, this->minimumPLR);
                    if (this->loadVSBranchPump) {
                        this->loadSideMassFlowRate = std::max(this->loadSideMassFlowRate, this->loadVSPumpMinLimitMassFlow);
                    }
                }
            }

            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
        }

        // if there's no flow in one, try to turn the entire heat pump off
        if (this->loadSideMassFlowRate <= 0.0) {
            this->loadSideMassFlowRate = 0.0;
            this->sourceSideMassFlowRate = 0.0;
            this->running = false;
            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
        }
    }
}

void EIRPlantLoopHeatPump::doPhysics(EnergyPlusData &state, Real64 currentLoad)
{

    Real64 constexpr RH90 = 90.0;
    Real64 constexpr RH60 = 60.0;
    Real64 constexpr rangeRH = 30.0;
    Real64 const reportingInterval = state.dataHVACGlobal->TimeStepSysSec;

    // ideally the plant is going to ensure that we don't have a runflag=true when the load is invalid, but
    // I'm not sure we can count on that so we will do one check here to make sure we don't calculate things badly
    if ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling && currentLoad >= 0.0) ||
        (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating && currentLoad <= 0.0)) {
        this->resetReportingVariables();
        return;
    }

    // get setpoint on the load side outlet
    Real64 loadSideOutletSetpointTemp = this->getLoadSideOutletSetPointTemp(state);
    Real64 originalLoadSideOutletSPTemp = loadSideOutletSetpointTemp;

    // add free cooling at some point, compressor is off during free cooling, temp limits restrict free cooling range

    Real64 capacityModifierFuncTemp = 1.0;
    Real64 availableCapacity = this->referenceCapacity;
    Real64 partLoadRatio = 0.0;
    bool waterTempExceeded = false;

    // evaluate capacity modifier curve and determine load side heat transfer
    // any adjustment to outlet water temp set point requires some form of iteration
    for (int loop = 0; loop < 2; ++loop) {
        capacityModifierFuncTemp = Curve::CurveValue(state, this->capFuncTempCurveIndex, loadSideOutletSetpointTemp, this->sourceSideInletTemp);

        availableCapacity = this->referenceCapacity * capacityModifierFuncTemp;

        // apply heating mode dry outdoor (evaporator) coil correction factor for air-cooled equipment
        if (this->capacityDryAirCurveIndex > 0 && this->airSource && state.dataEnvrn->OutRelHum < RH90) { // above 90% RH yields full capacity
            Real64 dryCorrectionFactor = std::min(1.0, Curve::CurveValue(state, this->capacityDryAirCurveIndex, state.dataEnvrn->OutDryBulbTemp));
            if (state.dataEnvrn->OutRelHum <= RH60) {
                // dry heating capacity correction factor is a function of outdoor dry-bulb temperature
                availableCapacity *= dryCorrectionFactor;
            } else {
                // interpolation of heating capacity between wet and dry is based on outdoor relative humidity over 60%-90% range
                Real64 semiDryFactor = dryCorrectionFactor + (1.0 - dryCorrectionFactor) * (1.0 - ((RH90 - state.dataEnvrn->OutRelHum) / rangeRH));
                availableCapacity *= semiDryFactor;
            }
        }

        if (availableCapacity > 0) {
            partLoadRatio = std::clamp(std::abs(currentLoad) / availableCapacity, 0.0, 1.0);
        }

        if (this->minSupplyWaterTempCurveIndex > 0) {
            Real64 minWaterTemp = Curve::CurveValue(state, this->minSupplyWaterTempCurveIndex, state.dataEnvrn->OutDryBulbTemp);
            if (loadSideOutletSetpointTemp < minWaterTemp) {
                loadSideOutletSetpointTemp = originalLoadSideOutletSPTemp + (1.0 - partLoadRatio) * (minWaterTemp - originalLoadSideOutletSPTemp);
                waterTempExceeded = true;
            }
        }
        if (this->maxSupplyWaterTempCurveIndex > 0) {
            Real64 maxWaterTemp = Curve::CurveValue(state, this->maxSupplyWaterTempCurveIndex, state.dataEnvrn->OutDryBulbTemp);
            if (loadSideOutletSetpointTemp > maxWaterTemp) {
                loadSideOutletSetpointTemp = maxWaterTemp + (1.0 - partLoadRatio) * (originalLoadSideOutletSPTemp - maxWaterTemp);
                waterTempExceeded = true;
            }
        }
        if (this->heatRecoveryHeatPump) {
            // check to see if souce side outlet temp exceeds limit and reduce PLR if necessary
            auto &thisSourcePlantLoop = state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum);
            Real64 const CpSrc = FluidProperties::GetSpecificHeatGlycol(
                state, thisSourcePlantLoop.FluidName, this->sourceSideInletTemp, thisSourcePlantLoop.FluidIndex, "PLHPEIR::simulate()");
            Real64 const sourceMCp = this->sourceSideMassFlowRate * CpSrc;
            Real64 const tempSourceOutletTemp =
                this->calcSourceOutletTemp(this->sourceSideInletTemp, (availableCapacity * partLoadRatio) / sourceMCp);
            if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating && tempSourceOutletTemp < this->minSourceTempLimit) {
                partLoadRatio *= (this->sourceSideInletTemp - this->minSourceTempLimit) / (this->sourceSideInletTemp - tempSourceOutletTemp);
            } else if (tempSourceOutletTemp > this->maxSourceTempLimit) {
                partLoadRatio *= (this->maxSourceTempLimit - this->sourceSideInletTemp) / (tempSourceOutletTemp - this->sourceSideInletTemp);
            }
        }
        if (!waterTempExceeded) {
            break;
        }
    }

    Real64 cyclingRatio = 1.0;
    Real64 operatingPLR = partLoadRatio;
    if (partLoadRatio < this->minimumPLR) {
        cyclingRatio = partLoadRatio / this->minimumPLR;
        partLoadRatio = this->minimumPLR;
        operatingPLR = partLoadRatio * cyclingRatio;
    }
    this->partLoadRatio = partLoadRatio;
    this->cyclingRatio = cyclingRatio;

    if (capacityModifierFuncTemp < 0.0) {
        if (this->capModFTErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state,
                              format(" Capacity Modifier curve (function of Temperatures) output is negative ({:.3T}).", capacityModifierFuncTemp));
            ShowContinueError(state,
                              format(" Negative value occurs using a water temperature of {:.2T}C and an outdoor air temperature of {:.2T}C.",
                                     loadSideOutletSetpointTemp,
                                     this->sourceSideInletTemp));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": Capacity Modifier curve (function of Temperatures) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->capModFTErrorIndex,
                                       capacityModifierFuncTemp,
                                       capacityModifierFuncTemp);
        capacityModifierFuncTemp = 0.0;
    }

    // Initializing defrost adjustment factors
    Real64 HeatingCapacityMultiplier = 1.0;
    Real64 InputPowerMultiplier = 1.0;

    // Check outdoor temperature to determine of defrost is active
    if (this->defrostAvailable && state.dataEnvrn->OutDryBulbTemp <= this->maxOutdoorTemperatureDefrost) {
        // Calculate defrost adjustment factors depending on defrost control type
        // Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
        Real64 OutdoorCoilT = 0.82 * state.dataEnvrn->OutDryBulbTemp - 8.589;
        Real64 OutdoorCoildw =
            max(1.0e-6, (state.dataEnvrn->OutHumRat - Psychrometrics::PsyWFnTdpPb(state, OutdoorCoilT, state.dataEnvrn->OutBaroPress)));
        if (this->defrostStrategy == DefrostControl::Timed) {
            if (this->defrostTime > 0.0) {
                this->fractionalDefrostTime = this->defrostTime; // DefrostTime in hours
                HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
                InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
                this->loadDueToDefrost =
                    (0.01 * this->fractionalDefrostTime) * (7.222 - state.dataEnvrn->OutDryBulbTemp) * (this->referenceCapacity / 1.01667);
                Real64 defrostEIRFT = 1.0 / this->referenceCOP;
                if (defrostEIRFTIndex > 0) {
                    defrostEIRFT = Curve::CurveValue(
                        state, this->defrostEIRFTIndex, max(15.555, state.dataEnvrn->OutWetBulbTemp), max(15.555, state.dataEnvrn->OutDryBulbTemp));
                }
                this->defrostEnergyRate = defrostEIRFT * (this->referenceCapacity / 1.01667) * this->fractionalDefrostTime;
            } else {
                this->loadDueToDefrost = 0.0;
                this->defrostEnergyRate = 0.0;
                this->fractionalDefrostTime = 0.0;
            }
        } else if (this->defrostStrategy == DefrostControl::OnDemand) {
            this->fractionalDefrostTime = 1.0 / (1.0 + 0.01446 / OutdoorCoildw);
            HeatingCapacityMultiplier = 0.875 * (1.0 - this->fractionalDefrostTime);
            InputPowerMultiplier = 0.954 * (1.0 - this->fractionalDefrostTime);
            this->loadDueToDefrost =
                (0.01 * this->fractionalDefrostTime) * (7.222 - state.dataEnvrn->OutDryBulbTemp) * (this->referenceCapacity / 1.01667);
            Real64 defrostEIRFT = 0.0;
            if (defrostEIRFTIndex > 0) {
                defrostEIRFT = Curve::CurveValue(
                    state, this->defrostEIRFTIndex, max(15.555, state.dataEnvrn->OutWetBulbTemp), max(15.555, state.dataEnvrn->OutDryBulbTemp));
            }
            this->defrostEnergyRate = defrostEIRFT * (this->referenceCapacity / 1.01667) * this->fractionalDefrostTime;
        } else if (this->defrostStrategy == DefrostControl::TimedEmpirical) {
            // cycles of defrost per hour
            Real64 thisHourDefrostCycles = Curve::CurveValue(state, this->defrostFreqCurveIndex, state.dataEnvrn->OutDryBulbTemp);
            // is directly proportional to the ratio of capacity used for that hour (PLR)
            thisHourDefrostCycles *= operatingPLR;
            // fraction of heat load per cycle of defrost
            Real64 thisHourDefrostHeatLoad = 0.0;
            if (this->defrostLoadCurveDims == 2) { // BiQuadratic
                thisHourDefrostHeatLoad =
                    Curve::CurveValue(state, this->defrostHeatLoadCurveIndex, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutDryBulbTemp);
            } else {
                thisHourDefrostHeatLoad = Curve::CurveValue(state, this->defrostHeatLoadCurveIndex, state.dataEnvrn->OutDryBulbTemp);
            }
            // heat load is applied to full load (not rated) and is proportional to defrost cycles per hour
            this->loadDueToDefrost = availableCapacity * thisHourDefrostHeatLoad * thisHourDefrostCycles;
            // electric input fraction due to defrost
            Real64 defrostHeatEnergyFraction = 0.0;
            if (this->defrostEnergyCurveDims == 2) { // BiQuadratic
                defrostHeatEnergyFraction =
                    Curve::CurveValue(state, this->defrostHeatEnergyCurveIndex, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutDryBulbTemp);
            } else {
                defrostHeatEnergyFraction = Curve::CurveValue(state, this->defrostHeatEnergyCurveIndex, state.dataEnvrn->OutDryBulbTemp);
            }
            // defrost energy rate is applied to rated power and is proportional to defrost cycles per hour
            this->defrostEnergyRate = (this->referenceCapacity / this->referenceCOP) * defrostHeatEnergyFraction * thisHourDefrostCycles;

            // question on how these multipliers are accounted for with capacity and power (e.g., 1+ or 1-)
            InputPowerMultiplier = 1.0 + thisHourDefrostHeatLoad;
            HeatingCapacityMultiplier = 1.0 + (thisHourDefrostHeatLoad * thisHourDefrostCycles);
            this->fractionalDefrostTime = thisHourDefrostCycles * this->fractionalDefrostTime;
        }
    } else {
        this->defrostEnergyRate = 0.0;
        this->loadDueToDefrost = 0.0;
        this->fractionalDefrostTime = 0.0;
    }
    availableCapacity *= HeatingCapacityMultiplier;
    this->defrostEnergy = this->defrostEnergyRate * reportingInterval;

    // evaluate the actual current operating load side heat transfer rate
    auto &thisLoadPlantLoop = state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum);
    Real64 CpLoad = FluidProperties::GetSpecificHeatGlycol(state,
                                                           thisLoadPlantLoop.FluidName,
                                                           state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp,
                                                           thisLoadPlantLoop.FluidIndex,
                                                           "PLHPEIR::simulate()");
    this->loadSideHeatTransfer = availableCapacity * operatingPLR;
    this->loadSideEnergy = this->loadSideHeatTransfer * reportingInterval;

    // calculate load side outlet conditions
    Real64 const loadMCp = this->loadSideMassFlowRate * CpLoad;
    this->loadSideOutletTemp = this->calcLoadOutletTemp(this->loadSideInletTemp, this->loadSideHeatTransfer / loadMCp);

    // now what to do here if outlet water temp exceeds limit based on HW supply temp limit curves?
    // currentLoad will be met and there should? be some adjustment based on outlet water temp limit?

    // calculate power usage from EIR curves
    Real64 eirModifierFuncTemp = Curve::CurveValue(state, this->powerRatioFuncTempCurveIndex, this->loadSideOutletTemp, this->sourceSideInletTemp);

    if (eirModifierFuncTemp < 0.0) {
        if (this->eirModFTErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state, format(" EIR Modifier curve (function of Temperatures) output is negative ({:.3T}).", eirModifierFuncTemp));
            ShowContinueError(state,
                              format(" Negative value occurs using a water temperature of {:.2T}C and an outdoor air temperature of {:.2T}C.",
                                     this->loadSideOutletTemp,
                                     this->sourceSideInletTemp));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": EIR Modifier curve (function of Temperatures) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->eirModFTErrorIndex,
                                       eirModifierFuncTemp,
                                       eirModifierFuncTemp);
        eirModifierFuncTemp = 0.0;
    }

    Real64 eirModifierFuncPLR = Curve::CurveValue(state, this->powerRatioFuncPLRCurveIndex, this->partLoadRatio);

    if (eirModifierFuncPLR < 0.0) {
        if (this->eirModFPLRErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state, format(" EIR Modifier curve (function of PLR) output is negative ({:.3T}).", eirModifierFuncPLR));
            ShowContinueError(state, format(" Negative value occurs using a Part Load Ratio of {:.2T}", this->partLoadRatio));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": EIR Modifier curve (function of PLR) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->eirModFPLRErrorIndex,
                                       eirModifierFuncPLR,
                                       eirModifierFuncPLR);
        eirModifierFuncPLR = 0.0;
    }

    this->powerUsage =
        (this->loadSideHeatTransfer / this->referenceCOP) * eirModifierFuncPLR * eirModifierFuncTemp * InputPowerMultiplier * this->cyclingRatio;
    this->powerEnergy = this->powerUsage * reportingInterval;

    // energy balance on heat pump
    this->sourceSideHeatTransfer = this->calcQsource(this->loadSideHeatTransfer, this->powerUsage);
    this->sourceSideEnergy = this->sourceSideHeatTransfer * reportingInterval;

    // calculate source side outlet conditions
    Real64 CpSrc;
    if (this->waterSource) {
        auto &thisSourcePlantLoop = state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum);
        CpSrc = FluidProperties::GetSpecificHeatGlycol(
            state, thisSourcePlantLoop.FluidName, this->sourceSideInletTemp, thisSourcePlantLoop.FluidIndex, "PLHPEIR::simulate()");
    } else {
        CpSrc = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
    }
    // this->sourceSideCp = CpSrc; // debuging variable
    Real64 const sourceMCp = this->sourceSideMassFlowRate * CpSrc;
    this->sourceSideOutletTemp = this->calcSourceOutletTemp(this->sourceSideInletTemp, this->sourceSideHeatTransfer / sourceMCp);

    if (this->waterSource && abs(this->sourceSideOutletTemp - this->sourceSideInletTemp) > 100.0) { // whoaa out of range happenings on water loop
        //
        // TODO setup recurring error warning?
        // lets do something different than fatal the simulation
        if ((this->sourceSideMassFlowRate / this->sourceSideDesignMassFlowRate) < 0.01) { // current source side flow is 1% of design max
            // just send it all to skin losses and leave the fluid temperature alone
            this->sourceSideOutletTemp = this->sourceSideInletTemp;
        } else if (this->sourceSideOutletTemp > this->sourceSideInletTemp) {
            this->sourceSideOutletTemp = this->sourceSideInletTemp + 100.0; // cap it at 100C delta

        } else if (this->sourceSideOutletTemp < this->sourceSideInletTemp) {
            this->sourceSideOutletTemp = this->sourceSideInletTemp - 100.0; // cap it at 100C delta
        }
    }
}

void EIRPlantLoopHeatPump::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    // This function does all one-time and begin-environment initialization
    std::string static const routineName = std::string("EIRPlantLoopHeatPump :") + __FUNCTION__;

    this->oneTimeInit(state);          // plant setup
    this->isPlantInletOrOutlet(state); // check location

    if (calledFromLocation.loopNum == this->loadSidePlantLoc.loopNum) {
        this->sizeLoadSide(state);
        if (this->waterSource) {
            this->sizeSrcSideWSHP(state);
        } else if (this->airSource) {
            this->sizeSrcSideASHP(state);
        }
    }

    if (state.dataGlobal->BeginEnvrnFlag && this->envrnInit && state.dataPlnt->PlantFirstSizesOkayToFinalize) {

        Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                       Constant::InitConvTemp,
                                                       state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                       routineName);
        this->loadSideDesignMassFlowRate = rho * this->loadSideDesignVolFlowRate;
        PlantUtilities::InitComponentNodes(state, 0.0, this->loadSideDesignMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet);

        if (this->waterSource) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum).FluidName,
                                                    Constant::InitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum).FluidIndex,
                                                    routineName);
            this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
            PlantUtilities::InitComponentNodes(
                state, 0.0, this->sourceSideDesignMassFlowRate, this->sourceSideNodes.inlet, this->sourceSideNodes.outlet);
        } else if (this->airSource) {
            rho = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, state.dataEnvrn->OutDryBulbTemp, 0.0, routineName);
            this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
        }

        if (this->flowControl == DataPlant::FlowMode::VariableSpeedPump) {
            this->loadVSPumpMinLimitMassFlow =
                PlantUtilities::MinFlowIfBranchHasVSPump(state, this->loadSidePlantLoc, this->loadVSBranchPump, this->loadVSLoopPump, true);
            if (this->waterSource) {
                this->sourceVSPumpMinLimitMassFlow = PlantUtilities::MinFlowIfBranchHasVSPump(
                    state, this->sourceSidePlantLoc, this->sourceVSBranchPump, this->sourceVSLoopPump, false);
            }
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            this->envrnInit = false;
        }
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->envrnInit = true;
    }
}

void EIRPlantLoopHeatPump::getDesignCapacities(
    [[maybe_unused]] EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    if (calledFromLocation.loopNum == this->loadSidePlantLoc.loopNum) {
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
    HeatSizingType heatingSizingMethod = this->heatSizingMethod;

    std::string_view const typeName = DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)];
    Real64 loadSideInitTemp =
        (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) ? Constant::HWInitConvTemp : Constant::CWInitConvTemp;
    // I guess I can assume the plant fluids are the same for HW and CW. So only the sizing type is an issue on which to use.

    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                   loadSideInitTemp,
                                                   state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                   "EIRPlantLoopHeatPump::size()");
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                       loadSideInitTemp,
                                                       state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                       "EIRPlantLoopHeatPump::size()");

    int pltLoadSizNum = state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).PlantSizNum;
    if (pltLoadSizNum > 0) {
        // this first IF block is really just about calculating the local tmpCapacity and tmpLoadVolFlow values
        // these represent what the unit would size those to, whether it is doing auto-sizing or not
        if (state.dataSize->PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
            tmpLoadVolFlow = state.dataSize->PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizingFactor;
            Real64 deltaT = state.dataSize->PlantSizData(pltLoadSizNum).DeltaT;
            if (this->companionHeatPumpCoil) {
                if (this->companionHeatPumpCoil->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
                    heatingSizingMethod = this->companionHeatPumpCoil->heatSizingMethod;
                }
                Real64 companionVolFlowRate = this->companionHeatPumpCoil->loadSideDesignVolFlowRate;
                int compLoopNum = this->companionHeatPumpCoil->loadSidePlantLoc.loopNum;
                if (compLoopNum > 0) {
                    companionVolFlowRate = state.dataSize->PlantSizData(compLoopNum).DesVolFlowRate * this->companionHeatPumpCoil->sizingFactor;
                }
                Real64 compRefCapacity = this->companionHeatPumpCoil->referenceCapacity;
                Real64 compRho = rho;
                Real64 compCp = Cp;
                Real64 compDeltaT = deltaT;
                if (compLoopNum > 0) {
                    compRho = FluidProperties::GetDensityGlycol(
                        state,
                        state.dataPlnt->PlantLoop(compLoopNum).FluidName,
                        this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling ? Constant::HWInitConvTemp : Constant::CWInitConvTemp,
                        state.dataPlnt->PlantLoop(compLoopNum).FluidIndex,
                        "EIRPlantLoopHeatPump::size()");
                    compCp = FluidProperties::GetSpecificHeatGlycol(
                        state,
                        state.dataPlnt->PlantLoop(compLoopNum).FluidName,
                        this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling ? Constant::HWInitConvTemp : Constant::CWInitConvTemp,
                        state.dataPlnt->PlantLoop(compLoopNum).FluidIndex,
                        "EIRPlantLoopHeatPump::size()");
                    compDeltaT = state.dataSize->PlantSizData(compLoopNum).DeltaT;
                }
                if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) {
                    tmpCapacity = Cp * rho * deltaT * tmpLoadVolFlow;
                    if (heatingSizingMethod == HeatSizingType::Heating) {
                        tmpCapacity = (compCp * compRho * compDeltaT * companionVolFlowRate) / this->companionHeatPumpCoil->heatSizingRatio;
                    } else if (heatingSizingMethod == HeatSizingType::GreaterOfCoolingOrHeating) {
                        compRefCapacity = compCp * compRho * compDeltaT * companionVolFlowRate;
                        if (compRefCapacity > tmpCapacity) {
                            rho = compRho;
                            tmpLoadVolFlow = companionVolFlowRate;
                            tmpCapacity = compRefCapacity / this->companionHeatPumpCoil->heatSizingRatio;
                        }
                    }
                } else { // size heating side based on sizing method
                    if (heatingSizingMethod == HeatSizingType::Heating) {
                        tmpCapacity = Cp * rho * deltaT * tmpLoadVolFlow;
                    } else {
                        compRefCapacity = compCp * compRho * compDeltaT * companionVolFlowRate;
                        if (heatingSizingMethod == HeatSizingType::Cooling) {
                            tmpCapacity = compRefCapacity * this->heatSizingRatio;
                            rho = compRho;
                            tmpLoadVolFlow = companionVolFlowRate;
                        } else { // else GreaterOfHeatingOrCooling
                            tmpCapacity = Cp * rho * deltaT * tmpLoadVolFlow;
                            if (compRefCapacity > tmpCapacity) {
                                tmpCapacity = compRefCapacity * this->heatSizingRatio;
                                rho = compRho;
                                tmpLoadVolFlow = companionVolFlowRate;
                            }
                        }
                    }
                }
            } else {
                tmpCapacity = Cp * rho * deltaT * tmpLoadVolFlow * this->heatSizingRatio;
            }
        } else if (this->companionHeatPumpCoil && this->companionHeatPumpCoil->loadSideDesignVolFlowRate > 0.0) {
            tmpLoadVolFlow = this->companionHeatPumpCoil->loadSideDesignVolFlowRate;
            if (this->companionHeatPumpCoil->referenceCapacity == DataSizing::AutoSize) {
                // use reverse init temp, e.g., if this is cooling use HWInitConvTemp
                Real64 compLoadSideInitTemp =
                    (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) ? Constant::HWInitConvTemp : Constant::CWInitConvTemp;
                int compLoopNum = this->companionHeatPumpCoil->loadSidePlantLoc.loopNum;
                if (compLoopNum > 0) {
                    Real64 const compRho = FluidProperties::GetDensityGlycol(state,
                                                                             state.dataPlnt->PlantLoop(compLoopNum).FluidName,
                                                                             compLoadSideInitTemp,
                                                                             state.dataPlnt->PlantLoop(compLoopNum).FluidIndex,
                                                                             "EIRPlantLoopHeatPump::size()");
                    Real64 const compCp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                                 state.dataPlnt->PlantLoop(compLoopNum).FluidName,
                                                                                 Constant::CWInitConvTemp,
                                                                                 state.dataPlnt->PlantLoop(compLoopNum).FluidIndex,
                                                                                 "EIRPlantLoopHeatPump::size()");
                    rho = compRho;
                    Cp = compCp;
                }
                tmpCapacity = Cp * rho * state.dataSize->PlantSizData(pltLoadSizNum).DeltaT * tmpLoadVolFlow * this->heatSizingRatio;
            } else {
                tmpCapacity = this->companionHeatPumpCoil->referenceCapacity;
            }
        } else {
            if (this->referenceCapacityWasAutoSized) tmpCapacity = 0.0;
            if (this->loadSideDesignVolFlowRateWasAutoSized) tmpLoadVolFlow = 0.0;
        }
        if (this->heatRecoveryHeatPump) {
            tmpLoadVolFlow = state.dataSize->PlantSizData(pltLoadSizNum).DesVolFlowRate;
        }
        if (this->loadSideDesignVolFlowRateWasAutoSized) this->loadSideDesignVolFlowRate = tmpLoadVolFlow;
        if (this->referenceCapacityWasAutoSized) {
            this->referenceCapacity = tmpCapacity;
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
                                ShowWarningMessage(state,
                                                   format("EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for {}", this->name));
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
                this->loadSideDesignMassFlowRate = rho * this->loadSideDesignVolFlowRate;
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
                                ShowMessage(state, format("EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for {}", this->name));
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
                ShowContinueError(state, format("Occurs in HeatPump:PlantLoop:EquationFit:Cooling object = {}", this->name));
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

    std::string_view const typeName = DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)];
    Real64 sourceSideInitTemp =
        (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) ? Constant::CWInitConvTemp : Constant::HWInitConvTemp;

    Real64 const rhoSrc = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                            sourceSideInitTemp,
                                                            state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                            "EIRPlantLoopHeatPump::size()");
    Real64 const CpSrc = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidName,
                                                                sourceSideInitTemp,
                                                                state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum).FluidIndex,
                                                                "EIRPlantLoopHeatPump::size()");

    // To start we need to override the calculated load side flow
    // rate if it was actually hard-sized
    if (!this->loadSideDesignVolFlowRateWasAutoSized) tmpLoadVolFlow = this->loadSideDesignVolFlowRate;

    // calculate an auto-sized value for source design flow regardless of whether it was auto-sized or not
    int plantSourceSizingIndex = state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum).PlantSizNum;
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
        if (this->waterSource && this->heatRecoveryHeatPump) {
            // If component is on plant outlet branch, use plant flow rate.
            tmpSourceVolFlow = state.dataSize->PlantSizData(plantSourceSizingIndex).DesVolFlowRate;
        }
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
                        ShowMessage(state, format("EIRPlantLoopHeatPump::size(): Potential issue with equipment sizing for {}", this->name));
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
    if (this->companionHeatPumpCoil) {
        tmpSourceVolFlow *= this->companionHeatPumpCoil->heatSizingRatio;
    } else {
        tmpSourceVolFlow *= this->heatSizingRatio;
    }

    // skipping autosized power section

    // register the design volume flows with the plant, only doing half of source because the companion
    // is generally on the same loop
    if (!this->heatRecoveryHeatPump) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->loadSideNodes.inlet, tmpLoadVolFlow);
    }
    if (!this->heatRecoveryHeatPump) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->sourceSideNodes.inlet, tmpSourceVolFlow / 0.5);
    }

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

    // will leave like this for now
    // need to update these to better values later
    Real64 sourceSideInitTemp = 20;
    Real64 sourceSideHumRat = 0.0;
    if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
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

    if (this->companionHeatPumpCoil) {
        tmpSourceVolFlow *= this->companionHeatPumpCoil->heatSizingRatio;
    } else {
        tmpSourceVolFlow *= this->heatSizingRatio;
    }
    this->sourceSideDesignVolFlowRate = tmpSourceVolFlow;
    this->sourceSideDesignMassFlowRate = rhoSrc * this->sourceSideDesignVolFlowRate;

    std::string_view const typeName = DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)];
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
        if (this->sourceSideDesignVolFlowRate > 0.0) {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (state.dataGlobal->DoPlantSizing) {
                    BaseSizer::reportSizerOutput(state,
                                                 typeName,
                                                 this->name,
                                                 "Design Size Source Side Volume Flow Rate [m3/s]",
                                                 tmpSourceVolFlow,
                                                 "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                 this->sourceSideDesignVolFlowRate);
                } else {
                    BaseSizer::reportSizerOutput(
                        state, typeName, this->name, "User-Specified Source Side Volume Flow Rate [m3/s]", this->sourceSideDesignVolFlowRate);
                }
            }
        }
    }

    if (errorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination"); // LCOV_EXCL_LINE
    }
}

PlantComponent *EIRPlantLoopHeatPump::factory(EnergyPlusData &state, DataPlant::PlantEquipmentType hp_type, const std::string &hp_name)
{
    if (state.dataEIRPlantLoopHeatPump->getInputsPLHP) {
        EIRPlantLoopHeatPump::processInputForEIRPLHP(state);
        EIRPlantLoopHeatPump::pairUpCompanionCoils(state);
        state.dataEIRPlantLoopHeatPump->getInputsPLHP = false;
    }

    for (auto &plhp : state.dataEIRPlantLoopHeatPump->heatPumps) {
        if (plhp.name == UtilityRoutines::makeUPPER(hp_name) && plhp.EIRHPType == hp_type) {
            return &plhp;
        }
    }

    ShowFatalError(state, format("EIR Plant Loop Heat Pump factory: Error getting inputs for PLHP named: {}", hp_name));
    return nullptr; // LCOV_EXCL_LINE
}

void EIRPlantLoopHeatPump::pairUpCompanionCoils(EnergyPlusData &state)
{
    for (auto &thisHP : state.dataEIRPlantLoopHeatPump->heatPumps) {
        if (!thisHP.companionCoilName.empty()) {
            std::string const thisCoilName = UtilityRoutines::makeUPPER(thisHP.name);
            DataPlant::PlantEquipmentType thisCoilType = thisHP.EIRHPType;
            std::string const targetCompanionName = UtilityRoutines::makeUPPER(thisHP.companionCoilName);
            for (auto &potentialCompanionCoil : state.dataEIRPlantLoopHeatPump->heatPumps) {
                DataPlant::PlantEquipmentType potentialCompanionType = potentialCompanionCoil.EIRHPType;
                std::string potentialCompanionName = UtilityRoutines::makeUPPER(potentialCompanionCoil.name);
                if (potentialCompanionName == thisCoilName) {
                    // skip the current coil
                    continue;
                }
                if (potentialCompanionName == targetCompanionName) {
                    if (thisCoilType == potentialCompanionType) {
                        ShowSevereError(state, format("Invalid companion specification for EIR Plant Loop Heat Pump named \"{}\"", thisCoilName));
                        ShowContinueError(state, "For heating objects, the companion must be a cooling object, and vice-versa");
                        ShowFatalError(state, "Invalid companion object causes program termination");
                    }
                    thisHP.companionHeatPumpCoil = &potentialCompanionCoil;
                    break;
                }
            }
            if (!thisHP.companionHeatPumpCoil) {
                ShowSevereError(state, "Could not find matching companion heat pump coil.");
                ShowContinueError(state, format("Base coil: {}", thisCoilName));
                ShowContinueError(state, format("Looking for companion coil named: {}", targetCompanionName));
                ShowFatalError(state, "Simulation aborts due to previous severe error");
            }
        }
    }
}

void EIRPlantLoopHeatPump::processInputForEIRPLHP(EnergyPlusData &state)
{

    struct ClassType
    {
        DataPlant::PlantEquipmentType thisType;
        std::string nodesType;
        std::function<Real64(Real64, Real64)> calcLoadOutletTemp;
        std::function<Real64(Real64, Real64)> calcQsource;
        std::function<Real64(Real64, Real64)> calcSourceOutletTemp;

        ClassType(DataPlant::PlantEquipmentType _thisType,
                  std::string _nodesType,
                  std::function<Real64(Real64, Real64)> _tLoadOutFunc,
                  std::function<Real64(Real64, Real64)> _qSrcFunc,
                  std::function<Real64(Real64, Real64)> _tSrcOutFunc)
            : thisType(_thisType), nodesType(std::move(_nodesType)), calcLoadOutletTemp(_tLoadOutFunc), calcQsource(_qSrcFunc),
              calcSourceOutletTemp(_tSrcOutFunc)
        {
        }
    };
    std::array<ClassType, 2> classesToInput = {ClassType{DataPlant::PlantEquipmentType::HeatPumpEIRCooling,
                                                         "Chilled Water Nodes",
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract,
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add,
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add},
                                               ClassType{DataPlant::PlantEquipmentType::HeatPumpEIRHeating,
                                                         "Hot Water Nodes",
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::add,
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract,
                                                         EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::subtract}};

    bool errorsFound = false;
    std::string &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    for (auto const &classToInput : classesToInput) {
        cCurrentModuleObject = DataPlant::PlantEquipTypeNames[static_cast<int>(classToInput.thisType)];
        DataLoopNode::ConnectionObjectType objType = static_cast<DataLoopNode::ConnectionObjectType>(
            getEnumValue(BranchNodeConnections::ConnectionObjectTypeNamesUC, UtilityRoutines::makeUPPER(cCurrentModuleObject)));
        int numPLHP = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (numPLHP > 0) {
            auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
            if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) continue;
            auto &instancesValue = instances.value();
            auto const &schemaProps = state.dataInputProcessing->inputProcessor->getObjectSchemaProps(state, cCurrentModuleObject);
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                std::string const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                EIRPlantLoopHeatPump thisPLHP;
                thisPLHP.EIRHPType = classToInput.thisType;
                thisPLHP.name = UtilityRoutines::makeUPPER(thisObjectName);
                std::string loadSideInletNodeName = UtilityRoutines::makeUPPER(fields.at("load_side_inlet_node_name").get<std::string>());
                std::string loadSideOutletNodeName = UtilityRoutines::makeUPPER(fields.at("load_side_outlet_node_name").get<std::string>());
                std::string condenserType = UtilityRoutines::makeUPPER(fields.at("condenser_type").get<std::string>());
                std::string sourceSideInletNodeName = UtilityRoutines::makeUPPER(fields.at("source_side_inlet_node_name").get<std::string>());
                std::string sourceSideOutletNodeName = UtilityRoutines::makeUPPER(fields.at("source_side_outlet_node_name").get<std::string>());
                thisPLHP.companionCoilName = UtilityRoutines::makeUPPER(
                    state.dataInputProcessing->inputProcessor->getAlphaFieldValue(fields, schemaProps, "companion_heat_pump_name"));

                thisPLHP.loadSideDesignVolFlowRate =
                    state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "load_side_reference_flow_rate");
                if (thisPLHP.loadSideDesignVolFlowRate == DataSizing::AutoSize) {
                    thisPLHP.loadSideDesignVolFlowRateWasAutoSized = true;
                }

                thisPLHP.sourceSideDesignVolFlowRate =
                    state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "source_side_reference_flow_rate");
                if (thisPLHP.sourceSideDesignVolFlowRate == DataSizing::AutoSize) {
                    thisPLHP.sourceSideDesignVolFlowRateWasAutoSized = true;
                }

                thisPLHP.referenceCapacity = state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "reference_capacity");
                if (thisPLHP.referenceCapacity == DataSizing::AutoSize) {
                    thisPLHP.referenceCapacityWasAutoSized = true;
                }

                thisPLHP.referenceCOP =
                    state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "reference_coefficient_of_performance");

                thisPLHP.sizingFactor = state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "sizing_factor");

                std::string const capFtName =
                    UtilityRoutines::makeUPPER(fields.at("capacity_modifier_function_of_temperature_curve_name").get<std::string>());
                thisPLHP.capFuncTempCurveIndex = Curve::GetCurveIndex(state, capFtName);
                if (thisPLHP.capFuncTempCurveIndex == 0) {
                    ShowSevereError(state, format("Invalid curve name for EIR PLHP (name={}; entered curve name: {}", thisPLHP.name, capFtName));
                    errorsFound = true;
                }

                std::string const eirFtName = UtilityRoutines::makeUPPER(
                    fields.at("electric_input_to_output_ratio_modifier_function_of_temperature_curve_name").get<std::string>());
                thisPLHP.powerRatioFuncTempCurveIndex = Curve::GetCurveIndex(state, eirFtName);
                if (thisPLHP.powerRatioFuncTempCurveIndex == 0) {
                    ShowSevereError(state, format("Invalid curve name for EIR PLHP (name={}; entered curve name: {}", thisPLHP.name, eirFtName));
                    errorsFound = true;
                }

                std::string const eirFplrName = UtilityRoutines::makeUPPER(
                    fields.at("electric_input_to_output_ratio_modifier_function_of_part_load_ratio_curve_name").get<std::string>());
                thisPLHP.powerRatioFuncPLRCurveIndex = Curve::GetCurveIndex(state, eirFplrName);
                if (thisPLHP.powerRatioFuncPLRCurveIndex == 0) {
                    ShowSevereError(state, format("Invalid curve name for EIR PLHP (name={}; entered curve name: {}", thisPLHP.name, eirFplrName));
                    errorsFound = true;
                }

                // inputs are past min-fields
                // fields common to both objects
                thisPLHP.minimumPLR = state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "minimum_part_load_ratio");
                thisPLHP.minSourceTempLimit =
                    state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "minimum_source_inlet_temperature");
                thisPLHP.maxSourceTempLimit =
                    state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "maximum_source_inlet_temperature");

                auto const minimumSupplyWaterTempCurveName = fields.find("minimum_supply_water_temperature_curve_name");
                if (minimumSupplyWaterTempCurveName != fields.end()) {
                    thisPLHP.minSupplyWaterTempCurveIndex =
                        Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(minimumSupplyWaterTempCurveName.value().get<std::string>()));
                }

                auto const maximumSupplyWaterTempCurveName = fields.find("maximum_supply_water_temperature_curve_name");
                if (maximumSupplyWaterTempCurveName != fields.end()) {
                    thisPLHP.maxSupplyWaterTempCurveIndex =
                        Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(maximumSupplyWaterTempCurveName.value().get<std::string>()));
                }

                std::string flowControlTypeName =
                    UtilityRoutines::makeUPPER(state.dataInputProcessing->inputProcessor->getAlphaFieldValue(fields, schemaProps, "flow_mode"));
                thisPLHP.flowControl = static_cast<DataPlant::FlowMode>(getEnumValue(DataPlant::FlowModeNamesUC, flowControlTypeName));

                // fields only in heating object
                if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
                    thisPLHP.heatSizingRatio =
                        state.dataInputProcessing->inputProcessor->getRealFieldValue(fields, schemaProps, "heating_to_cooling_capacity_sizing_ratio");
                    thisPLHP.maxOutdoorTemperatureDefrost = state.dataInputProcessing->inputProcessor->getRealFieldValue(
                        fields, schemaProps, "maximum_outdoor_dry_bulb_temperature_for_defrost_operation");
                }

                constexpr std::array<std::string_view, static_cast<int>(HeatSizingType::Num)> PLHPHeatSizTypeNamesUC = {
                    "HEATINGCAPACITY", "COOLINGCAPACITY", "GREATEROFHEATINGORCOOLING"};
                auto const heatSizingType = fields.find("heat_pump_sizing_method");
                if (heatSizingType != fields.end()) {
                    thisPLHP.heatSizingMethod = static_cast<HeatSizingType>(
                        getEnumValue(PLHPHeatSizTypeNamesUC, UtilityRoutines::makeUPPER(heatSizingType.value().get<std::string>())));
                } else {
                    // revert to legacy sizing method, if no companion coil and this coil type is heating, set to heating
                    if (thisPLHP.companionCoilName.empty() && thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
                        thisPLHP.heatSizingMethod = HeatSizingType::Heating;
                    } else {
                        thisPLHP.heatSizingMethod = HeatSizingType::Cooling;
                    }
                }

                constexpr std::array<std::string_view, static_cast<int>(ControlType::Num)> PLHPCtrlTypeNamesUC = {"SETPOINT", "LOAD"};
                auto const controlType = fields.find("control_type");
                if (controlType != fields.end()) {
                    thisPLHP.sysControlType = static_cast<ControlType>(
                        getEnumValue(PLHPCtrlTypeNamesUC, UtilityRoutines::makeUPPER(controlType.value().get<std::string>())));
                } else {
                    thisPLHP.sysControlType = ControlType::Load;
                }
                auto const capacityDryAirCurveName = fields.find("dry_outdoor_correction_factor_curve_name");
                if (capacityDryAirCurveName != fields.end()) {
                    thisPLHP.capacityDryAirCurveIndex =
                        Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(capacityDryAirCurveName.value().get<std::string>()));
                }

                constexpr std::array<std::string_view, static_cast<int>(DefrostControl::Num)> PLHPDefrostTypeNamesUC = {
                    "NONE", "TIMED", "ONDEMAND", "TIMEDEMPIRICAL"};
                auto const defrostControlStrategy = fields.find("heat_pump_defrost_control");
                if (defrostControlStrategy != fields.end()) {
                    thisPLHP.defrostStrategy = static_cast<DefrostControl>(
                        getEnumValue(PLHPDefrostTypeNamesUC, UtilityRoutines::makeUPPER(defrostControlStrategy.value().get<std::string>())));
                } else {
                    thisPLHP.defrostStrategy = DefrostControl::None;
                }

                if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating &&
                    (thisPLHP.defrostStrategy == DefrostControl::Timed || thisPLHP.defrostStrategy == DefrostControl::TimedEmpirical)) {
                    auto const timePeriod = fields.find("heat_pump_defrost_time_period_fraction");
                    if (timePeriod != fields.end()) {
                        thisPLHP.defrostTime = timePeriod.value().get<Real64>();
                    } else {
                        Real64 defaultVal = 0.0;
                        if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                                state, cCurrentModuleObject, "heat_pump_defrost_time_period_fraction", defaultVal)) {
                            // excluding from coverage
                            ShowSevereError(state, // LCOV_EXCL_LINE
                                            format("EIR PLHP \"{}\": Heat Pump Defrost Time Period Fraction not entered and default value not found.",
                                                   thisPLHP.name)); // LCOV_EXCL_LINE
                            errorsFound = true;                     // LCOV_EXCL_LINE
                        } else {
                            thisPLHP.defrostTime = defaultVal;
                        }
                    }
                }

                if (thisPLHP.defrostStrategy == DefrostControl::TimedEmpirical) {
                    auto const timedEmpiricalDefFreqStratCurveName = fields.find("timed_empirical_defrost_frequency_curve_name");
                    if (timedEmpiricalDefFreqStratCurveName != fields.end()) {
                        thisPLHP.defrostFreqCurveIndex =
                            Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(timedEmpiricalDefFreqStratCurveName.value().get<std::string>()));
                    }
                    auto const timedEmpiricalDefHeatLoadPenaltyCurveName = fields.find("timed_empirical_defrost_heat_load_penalty_curve_name");
                    if (timedEmpiricalDefHeatLoadPenaltyCurveName != fields.end()) {
                        thisPLHP.defrostHeatLoadCurveIndex = Curve::GetCurveIndex(
                            state, UtilityRoutines::makeUPPER(timedEmpiricalDefHeatLoadPenaltyCurveName.value().get<std::string>()));
                        thisPLHP.defrostLoadCurveDims = state.dataCurveManager->PerfCurve(thisPLHP.defrostHeatLoadCurveIndex)->numDims;
                    }
                    auto const defrostHeatEnergyCurveIndexCurveName = fields.find("timed_empirical_defrost_heat_input_energy_fraction_curve_name");
                    if (defrostHeatEnergyCurveIndexCurveName != fields.end()) {
                        thisPLHP.defrostHeatEnergyCurveIndex =
                            Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(defrostHeatEnergyCurveIndexCurveName.value().get<std::string>()));
                        thisPLHP.defrostEnergyCurveDims = state.dataCurveManager->PerfCurve(thisPLHP.defrostHeatEnergyCurveIndex)->numDims;
                    }
                } else if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) { // used for Timed or OnDemand
                    auto const defEIRFTCurveName = fields.find("defrost_energy_input_ratio_function_of_temperature_curve_name");
                    if (defEIRFTCurveName != fields.end()) {
                        thisPLHP.defrostEIRFTIndex =
                            Curve::GetCurveIndex(state, UtilityRoutines::makeUPPER(defEIRFTCurveName.value().get<std::string>()));
                    }
                }

                bool nodeErrorsFound = false;
                thisPLHP.loadSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                   loadSideInletNodeName,
                                                                                   nodeErrorsFound,
                                                                                   objType,
                                                                                   thisPLHP.name,
                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                   DataLoopNode::ConnectionType::Inlet,
                                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisPLHP.loadSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                    loadSideOutletNodeName,
                                                                                    nodeErrorsFound,
                                                                                    objType,
                                                                                    thisPLHP.name,
                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                DataLoopNode::NodeFluidType condenserNodeType = DataLoopNode::NodeFluidType::Blank;
                DataLoopNode::ConnectionType condenserNodeConnectionType_Inlet = DataLoopNode::ConnectionType::Blank;
                DataLoopNode::ConnectionType condenserNodeConnectionType_Outlet = DataLoopNode::ConnectionType::Blank;
                if (condenserType == "WATERSOURCE") {
                    thisPLHP.waterSource = true;
                    condenserNodeType = DataLoopNode::NodeFluidType::Water;
                    condenserNodeConnectionType_Inlet = DataLoopNode::ConnectionType::Inlet;
                    condenserNodeConnectionType_Outlet = DataLoopNode::ConnectionType::Outlet;
                } else if (condenserType == "AIRSOURCE") {
                    thisPLHP.airSource = true;
                    condenserNodeType = DataLoopNode::NodeFluidType::Air;
                    condenserNodeConnectionType_Inlet = DataLoopNode::ConnectionType::Inlet;
                    condenserNodeConnectionType_Outlet = DataLoopNode::ConnectionType::Outlet;
                    if (sourceSideInletNodeName == sourceSideOutletNodeName) {
                        ShowSevereError(state, format("PlantLoopHeatPump {} has the same inlet and outlet node.", thisObjectName));
                        ShowContinueError(state, format("Node Name: {}", sourceSideInletNodeName));
                        errorsFound = true;
                    }
                } else {
                    // Again, this should be protected by the input processor
                    ShowErrorMessage(
                        state, format("Invalid heat pump condenser type (name={}; entered type: {}", thisPLHP.name, condenserType)); // LCOV_EXCL_LINE
                    errorsFound = true;                                                                                              // LCOV_EXCL_LINE
                }
                thisPLHP.sourceSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                     sourceSideInletNodeName,
                                                                                     nodeErrorsFound,
                                                                                     objType,
                                                                                     thisPLHP.name,
                                                                                     condenserNodeType,
                                                                                     condenserNodeConnectionType_Inlet,
                                                                                     NodeInputManager::CompFluidStream::Secondary,
                                                                                     DataLoopNode::ObjectIsNotParent);
                thisPLHP.sourceSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                      sourceSideOutletNodeName,
                                                                                      nodeErrorsFound,
                                                                                      objType,
                                                                                      thisPLHP.name,
                                                                                      condenserNodeType,
                                                                                      condenserNodeConnectionType_Outlet,
                                                                                      NodeInputManager::CompFluidStream::Secondary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (nodeErrorsFound) errorsFound = true;
                BranchNodeConnections::TestCompSet(
                    state, cCurrentModuleObject, thisPLHP.name, loadSideInletNodeName, loadSideOutletNodeName, classToInput.nodesType);

                if (thisPLHP.waterSource) {
                    BranchNodeConnections::TestCompSet(
                        state, cCurrentModuleObject, thisPLHP.name, sourceSideInletNodeName, sourceSideOutletNodeName, "Condenser Water Nodes");
                }

                if (thisPLHP.airSource && thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating &&
                    thisPLHP.defrostStrategy != DefrostControl::None) {
                    thisPLHP.defrostAvailable = true;
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

void EIRPlantLoopHeatPump::isPlantInletOrOutlet(EnergyPlusData &state)
{
    // check to see if component is on a plant inlet or outlet branch to determine if flow should be registered
    // only components on plant parallel component branches should be registered
    // this check for the load side on a plant inlet branch and source side on a plant outlet branch
    // likely will need more checking here but this works for now with existing test file
    bool loadSideIsPlantInlet = false;
    bool sourceSideIsPlantOutlet = false;
    for (auto thisPlant : state.dataPlnt->PlantLoop) {
        for (auto thisLoopSide : thisPlant.LoopSide) {
            if (this->loadSideNodes.inlet == thisLoopSide.NodeNumIn) {
                loadSideIsPlantInlet = true;
                break;
            }
            if (this->sourceSideNodes.outlet == thisLoopSide.NodeNumOut) {
                sourceSideIsPlantOutlet = true;
                break;
            }
            if (loadSideIsPlantInlet && sourceSideIsPlantOutlet) {
                this->heatRecoveryHeatPump = true;
                break;
            }
        }
    }
}

void EIRPlantLoopHeatPump::oneTimeInit(EnergyPlusData &state)
{
    // This function does all the one-time initialization
    constexpr std::string_view routineName = "EIRPlantLoopHeatPump : oneTimeInit"; // + __FUNCTION__;

    if (this->oneTimeInitFlag) {
        bool errFlag = false;

        // setup output variables
        SetupOutputVariable(state,
                            "Heat Pump Part Load Ratio",
                            OutputProcessor::Unit::None,
                            this->partLoadRatio,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Cycling Ratio",
                            OutputProcessor::Unit::None,
                            this->cyclingRatio,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->loadSideHeatTransfer,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->loadSideEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->name,
                            {},
                            "ENERGYTRANSFER",
                            {},
                            {},
                            "Plant");
        SetupOutputVariable(state,
                            "Heat Pump Source Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->sourceSideHeatTransfer,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->sourceSideEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->loadSideInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->loadSideOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->sourceSideInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->sourceSideOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->powerUsage,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRCooling) { // energy from HeatPump:PlantLoop:EIR:Cooling object
            SetupOutputVariable(state,
                                "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                "Electricity",
                                "Cooling",
                                "Heat Pump",
                                "Plant");
        } else if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) { // energy from HeatPump:PlantLoop:EIR:Heating object
            SetupOutputVariable(state,
                                "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                "Electricity",
                                "Heating",
                                "Heat Pump",
                                "Plant");
            if (this->defrostAvailable) {
                SetupOutputVariable(state,
                                    "Heat Pump Load Due To Defrost",
                                    OutputProcessor::Unit::W,
                                    this->loadDueToDefrost,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->name);
                SetupOutputVariable(state,
                                    "Heat Pump Fractioal Defrost Time",
                                    OutputProcessor::Unit::W,
                                    this->fractionalDefrostTime,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->name);
                SetupOutputVariable(state,
                                    "Heat Pump Defrost Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->defrostEnergyRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->name);
                SetupOutputVariable(state,
                                    "Heat Pump Defrost Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->defrostEnergy,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    this->name,
                                    {},
                                    "Electricity",
                                    "HEATING",
                                    "Heat Pump",
                                    "Plant");
            }
        }
        SetupOutputVariable(state,
                            "Heat Pump Load Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->loadSideMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->sourceSideMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        // report variable used for debugging, System Node Specific Heat can also report the node Cp
        // added spaces to SetupOutputVariable to avoid issue with variable parsing script
        // Setup Output Variable(state,
        //                   "Heat Pump Source Side Specific Heat",
        //                   OutputProcessor::Unit::J_kgK,
        //                   this->sourceSideCp,
        //                   OutputProcessor::SOVTimeStepType::System,
        //                   OutputProcessor::SOVStoreType::Average,
        //                   this->name);

        // find this component on the plant
        bool thisErrFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(
            state, this->name, this->EIRHPType, this->loadSidePlantLoc, thisErrFlag, _, _, _, this->loadSideNodes.inlet, _);

        if (thisErrFlag) {
            ShowSevereError(state,
                            format("{}: Plant topology problem for {} name = \"{}\"",
                                   routineName,
                                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                   this->name));
            ShowContinueError(state, "Could not locate component's load side connections on a plant loop");
            errFlag = true;
        } else if (this->loadSidePlantLoc.loopSideNum != DataPlant::LoopSideLocation::Supply) { // only check if !thisErrFlag
            ShowSevereError(state,
                            format("{}: Invalid connections for {} name = \"{}\"",
                                   routineName,
                                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                   this->name));
            ShowContinueError(state, "The load side connections are not on the Supply Side of a plant loop");
            errFlag = true;
        }

        thisErrFlag = false;
        if (this->waterSource) {
            PlantUtilities::ScanPlantLoopsForObject(
                state, this->name, this->EIRHPType, this->sourceSidePlantLoc, thisErrFlag, _, _, _, this->sourceSideNodes.inlet, _);

            if (thisErrFlag) {
                ShowSevereError(state,
                                format("{}: Plant topology problem for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "Could not locate component's source side connections on a plant loop");
                errFlag = true;
            } else if (this->sourceSidePlantLoc.loopSideNum != DataPlant::LoopSideLocation::Demand) { // only check if !thisErrFlag
                ShowSevereError(state,
                                format("{}: Invalid connections for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "The source side connections are not on the Demand Side of a plant loop");
                errFlag = true;
            }

            // make sure it is not the same loop on both sides.
            if (this->loadSidePlantLoc.loopNum == this->sourceSidePlantLoc.loopNum) { // user is being too tricky, don't allow
                ShowSevereError(state,
                                format("{}: Invalid connections for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "The load and source sides need to be on different loops.");
                errFlag = true;
            } else {

                PlantUtilities::InterConnectTwoPlantLoopSides(state, this->loadSidePlantLoc, this->sourceSidePlantLoc, this->EIRHPType, true);
            }
        } else if (this->airSource) {
            // nothing to do here ?
        }

        if (errFlag) {
            ShowFatalError(state, format("{}: Program terminated due to previous condition(s).", routineName));
        }
        this->oneTimeInitFlag = false;
    }
}

// From here on, the Fuel Fired Heat Pump module EIRFuelFiredHeatPump
// Enum string definitions
static constexpr std::array<std::string_view, static_cast<int>(EIRFuelFiredHeatPump::OATempCurveVar::Num)> OATempCurveVarNamesUC = {"DRYBULB",
                                                                                                                                    "WETBULB"};
static constexpr std::array<std::string_view, static_cast<int>(EIRFuelFiredHeatPump::WaterTempCurveVar::Num)> WaterTempCurveVarNamesUC = {
    "ENTERINGCONDENSER", "LEAVINGCONDENSER", "ENTERINGEVAPORATOR", "LEAVINGEVAPORATOR"};
static constexpr std::array<std::string_view, static_cast<int>(EIRFuelFiredHeatPump::DefrostType::Num)> DefrostTypeNamesUC = {"TIMED", "ONDEMAND"};

void EIRFuelFiredHeatPump::doPhysics(EnergyPlusData &state, Real64 currentLoad)
{
    Real64 const reportingInterval = state.dataHVACGlobal->TimeStepSysSec;

    // ideally the plant is going to ensure that we don't have a runflag=true when the load is invalid, but
    // I'm not sure we can count on that so we will do one check here to make sure we don't calculate things badly
    if ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling && currentLoad >= 0.0) ||
        (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating && currentLoad <= 0.0)) {
        this->resetReportingVariables();
        return;
    }

    // get setpoint on the load side outlet
    // Real64 loadSideOutletSetpointTemp = this->getLoadSideOutletSetPointTemp(state);

    // Use a logic similar to that for a boilder: If the specified load is 0.0 or the boiler should not run
    // then we leave this subroutine. Before leaving
    // if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
    // will not shut down the branch
    auto &thisInletNode = state.dataLoopNodes->Node(this->loadSideNodes.inlet);
    auto &thisOutletNode = state.dataLoopNodes->Node(this->loadSideNodes.outlet);
    auto &sim_component = DataPlant::CompData::getPlantComponent(state, this->loadSidePlantLoc);
    bool RunFlag = true;
    if ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating && currentLoad <= 0.0) || !RunFlag) {
        if (sim_component.FlowCtrl == DataBranchAirLoopPlant::ControlType::SeriesActive) this->loadSideMassFlowRate = thisInletNode.MassFlowRate;
        this->resetReportingVariables();
        return;
    }

    DataPlant::PlantLoopData &thisLoadPlantLoop = state.dataPlnt->PlantLoop(this->loadSidePlantLoc.loopNum);
    Real64 CpLoad = FluidProperties::GetSpecificHeatGlycol(
        state, thisLoadPlantLoop.FluidName, thisInletNode.Temp, thisLoadPlantLoop.FluidIndex, "PLFFHPEIR::simulate()");

    // Set the current load equal to the FFHP load
    Real64 FFHPloadSideLoad = currentLoad; // this->loadSidePlantLoad = MyLoad;

    if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating) {

        // Initialize the delta temperature to zero
        Real64 FFHPDeltaTemp = 0.0; // C - FFHP inlet to outlet temperature difference, set in all necessary code paths so no initialization required

        if (thisLoadPlantLoop.LoopSide(this->loadSidePlantLoc.loopSideNum).FlowLock == DataPlant::FlowLock::Unlocked) {
            // Either set the flow to the Constant value or calculate the flow for the variable volume
            if (this->flowMode == DataPlant::FlowMode::Constant) {
                // Then find the flow rate and outlet temp
                this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
                PlantUtilities::SetComponentFlowRate(
                    state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);

                if ((this->loadSideMassFlowRate != 0.0) &&
                    ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating && currentLoad > 0.0))) {
                    FFHPDeltaTemp = currentLoad / (this->loadSideMassFlowRate * CpLoad);
                } else {
                    FFHPDeltaTemp = 0.0;
                }
                this->loadSideOutletTemp = FFHPDeltaTemp + thisInletNode.Temp;

            } else if (this->flowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // Calculate the Delta Temp from the inlet temp to the FFHP outlet setpoint
                // Then find the flow rate and outlet temp

                if (thisLoadPlantLoop.LoopDemandCalcScheme == DataPlant::LoopDemandCalcScheme::SingleSetPoint) {
                    FFHPDeltaTemp = thisOutletNode.TempSetPoint - thisInletNode.Temp;
                } else { // DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand
                    FFHPDeltaTemp = thisOutletNode.TempSetPointLo - thisInletNode.Temp;
                }

                this->loadSideOutletTemp = FFHPDeltaTemp + thisInletNode.Temp;

                if ((FFHPDeltaTemp > 0.0) && ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating && currentLoad > 0.0))) {
                    this->loadSideMassFlowRate = currentLoad / (CpLoad * FFHPDeltaTemp);
                    this->loadSideMassFlowRate = min(this->loadSideDesignMassFlowRate, this->loadSideMassFlowRate);
                } else {
                    this->loadSideMassFlowRate = 0.0;
                }
                PlantUtilities::SetComponentFlowRate(
                    state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);

            }    // End of Constant/Variable Flow If Block
        } else { // If FlowLock is True
            // Set the boiler flow rate from inlet node and then check performance
            this->loadSideMassFlowRate = thisInletNode.MassFlowRate;

            if ((this->loadSideMassFlowRate > 0.0) &&
                ((this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating && currentLoad > 0.0))) { // this FFHP has a heat load
                // FFHPloadSideLoad = currentLoad;
                // if (FFHPloadSideLoad > this->referenceCapacity * this->maxPLR) FFHPloadSideLoad = this->referenceCapacity * this->maxPLR;
                // if (FFHPloadSideLoad < this->referenceCapacity * this->minPLR) FFHPloadSideLoad = this->referenceCapacity * this->minPLR;
                FFHPloadSideLoad = std::clamp(FFHPloadSideLoad, this->referenceCapacity * this->minPLR, this->referenceCapacity * this->maxPLR);
                this->loadSideOutletTemp = thisInletNode.Temp + FFHPloadSideLoad / (this->loadSideMassFlowRate * CpLoad);
            } else {
                FFHPloadSideLoad = 0.0;
                this->loadSideOutletTemp = thisInletNode.Temp;
            }
        }
    } else if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
        if (thisLoadPlantLoop.LoopSide(this->loadSidePlantLoc.loopSideNum).FlowLock == DataPlant::FlowLock::Unlocked) {
            // this->PossibleSubcooling =
            //    !(state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
            //      DataPlant::OpScheme::CompSetPtBased);
            Real64 evapDeltaTemp = 0.0; // Evaporator temperature difference [C]

            // Either set the flow to the Constant value or calculate the flow for the variable volume case
            if (this->flowMode == DataPlant::FlowMode::Constant) {
                // Set the evaporator mass flow rate to design
                // Start by assuming max (design) flow
                this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(
                    state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
                if (this->loadSideMassFlowRate != 0.0) {
                    evapDeltaTemp = std::abs(currentLoad) / (this->loadSideMassFlowRate * CpLoad); // MyLoad = net evaporator capacity, QEvaporator
                } else {
                    evapDeltaTemp = 0.0;
                }
                this->loadSideOutletTemp = thisInletNode.Temp - evapDeltaTemp;
            } else if (this->flowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                switch (thisLoadPlantLoop.LoopDemandCalcScheme) {
                case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                    // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                    evapDeltaTemp = thisInletNode.Temp - thisOutletNode.TempSetPoint;
                } break;
                case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                    evapDeltaTemp = thisInletNode.Temp - thisOutletNode.TempSetPointHi;
                } break;
                default: {
                    assert(false);
                } break;
                }

                if (evapDeltaTemp != 0) {
                    this->loadSideMassFlowRate = max(0.0, (std::abs(currentLoad) / (CpLoad * evapDeltaTemp)));
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->loadSideMassFlowRate = min(this->loadSideDesignMassFlowRate, this->loadSideMassFlowRate);
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(
                        state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
                    // Should we recalculate this with the corrected setpoint?
                    switch (thisLoadPlantLoop.LoopDemandCalcScheme) {
                    case DataPlant::LoopDemandCalcScheme::SingleSetPoint: {
                        this->loadSideOutletTemp = thisOutletNode.TempSetPoint;
                    } break;
                    case DataPlant::LoopDemandCalcScheme::DualSetPointDeadBand: {
                        this->loadSideOutletTemp = thisOutletNode.TempSetPointHi;
                    } break;
                    default:
                        break;
                    }
                } else {
                    // Try to request zero flow
                    this->loadSideMassFlowRate = 0.0;
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(
                        state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
                    // No deltaT since component is not running
                    this->loadSideOutletTemp = thisInletNode.Temp;
                    // this->QEvaporator = 0.0;
                    // PartLoadRat = 0.0;
                    // this->ChillerPartLoadRatio = 0.0;

                    // if (this->DeltaTErrCount < 1 && !state.dataGlobal->WarmupFlag) {
                    if (!state.dataGlobal->WarmupFlag) {
                        // ++this->DeltaTErrCount;
                        ShowWarningError(state, "FFHP evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tevapout setpoint temp).");
                        ShowContinueErrorTimeStamp(state, "");
                        // } else if (!state.dataGlobal->WarmupFlag) {
                        // ++this->ChillerCapFTError;
                        ShowWarningError( // RecurringWarningErrorAtEnd(
                            state,
                            format("{} \"{}\": FFHP evaporator DeltaTemp = 0 in mass flow calculation warning continues...",
                                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                   this->name));
                        // this->DeltaTErrCountIndex,
                        // evapDeltaTemp,
                        // evapDeltaTemp);
                    }
                }
            }
        } else { // If FlowLock is True
            this->loadSideMassFlowRate = thisInletNode.MassFlowRate;
            PlantUtilities::SetComponentFlowRate(
                state, this->loadSideMassFlowRate, this->loadSideNodes.inlet, this->loadSideNodes.outlet, this->loadSidePlantLoc);
            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->loadSideMassFlowRate == 0.0) {
                FFHPloadSideLoad = 0.0;
                // return;
            }
        } // This is the end of the FlowLock Block
    }

    // Determine which air variable to use for GAHP:
    // Source (air) side variable to use
    // auto &thisloadsideinletnode = state.dataLoopNodes->Node(this->loadSideNodes.inlet);
    Real64 oaTempforCurve = thisInletNode.Temp; // state.dataLoopNodes->Node(this->loadSideNodes.inlet).Temp;
    if (this->oaTempCurveInputVar == OATempCurveVar::WetBulb) {
        oaTempforCurve =
            Psychrometrics::PsyTwbFnTdbWPb(state, thisInletNode.Temp, thisInletNode.HumRat, thisInletNode.Press, "PLFFHPEIR::doPhysics()");
    }

    // Load (water) side temperature variable
    Real64 waterTempforCurve = this->loadSideInletTemp;
    if (this->waterTempCurveInputVar == WaterTempCurveVar::LeavingCondenser || this->waterTempCurveInputVar == WaterTempCurveVar::LeavingEvaporator) {
        waterTempforCurve = this->loadSideOutletTemp;
    }

    // evaluate capacity modifier curve and determine load side heat transfer
    Real64 capacityModifierFuncTemp =
        // CurveManager::CurveValue(state, this->capFuncTempCurveIndex, loadSideOutletSetpointTemp, this->sourceSideInletTemp);
        // CurveManager::CurveValue(state, this->capFuncTempCurveIndex, loadSideOutletSetpointTemp, oaTempforCurve);
        Curve::CurveValue(state, this->capFuncTempCurveIndex, waterTempforCurve, oaTempforCurve);

    if (capacityModifierFuncTemp < 0.0) {
        if (this->capModFTErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state,
                              format(" Capacity Modifier curve (function of Temperatures) output is negative ({:.3T}).", capacityModifierFuncTemp));
            ShowContinueError(state,
                              format(" Negative value occurs using a water temperature of {:.2T}C and an outdoor air temperature of {:.2T}C.",
                                     waterTempforCurve,
                                     oaTempforCurve));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": Capacity Modifier curve (function of Temperatures) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->capModFTErrorIndex,
                                       capacityModifierFuncTemp,
                                       capacityModifierFuncTemp);
        capacityModifierFuncTemp = 0.0;
    }

    Real64 availableCapacity = this->referenceCapacity * capacityModifierFuncTemp;
    Real64 partLoadRatio = 0.0;
    if (availableCapacity > 0) {
        partLoadRatio = std::clamp(
            std::abs(FFHPloadSideLoad) / availableCapacity, 0.0, 1.0); // max(0.0, min(std::abs(FFHPloadSideLoad) / availableCapacity, 1.0));
    }

    // evaluate the actual current operating load side heat transfer rate

    // this->loadSideHeatTransfer = availableCapacity * partLoadRatio;
    this->loadSideHeatTransfer = availableCapacity * partLoadRatio; // (partLoadRatio >= this->minPLR ? partLoadRatio : 0.0);
    this->loadSideEnergy = this->loadSideHeatTransfer * reportingInterval;

    // calculate load side outlet conditions
    Real64 const loadMCp = this->loadSideMassFlowRate * CpLoad;
    this->loadSideOutletTemp = this->calcLoadOutletTemp(this->loadSideInletTemp, this->loadSideHeatTransfer / loadMCp);

    // calculate power usage from EIR curves
    Real64 eirModifierFuncTemp = Curve::CurveValue(state,
                                                   this->powerRatioFuncTempCurveIndex,
                                                   waterTempforCurve,
                                                   oaTempforCurve); // CurveManager::CurveValue(state, this->powerRatioFuncTempCurveIndex,
                                                                    // this->loadSideOutletTemp, this->sourceSideInletTemp);

    if (eirModifierFuncTemp < 0.0) {
        if (this->eirModFTErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state, format(" EIR Modifier curve (function of Temperatures) output is negative ({:.3T}).", eirModifierFuncTemp));
            ShowContinueError(state,
                              format(" Negative value occurs using a water temperature of {:.2T}C and an outdoor air temperature of {:.2T}C.",
                                     waterTempforCurve,
                                     oaTempforCurve));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": EIR Modifier curve (function of Temperatures) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->eirModFTErrorIndex,
                                       eirModifierFuncTemp,
                                       eirModifierFuncTemp);
        eirModifierFuncTemp = 0.0;
    }

    Real64 miniPLR_mod = this->minPLR;
    Real64 PLFf = max(miniPLR_mod, partLoadRatio);

    Real64 eirModifierFuncPLR = Curve::CurveValue(state, this->powerRatioFuncPLRCurveIndex, PLFf);
    // this->powerUsage = (this->loadSideHeatTransfer / this->referenceCOP) * eirModifierFuncPLR * eirModifierFuncTemp;
    // this->powerEnergy = this->powerUsage * reportingInterval;

    if (eirModifierFuncPLR < 0.0) {
        if (this->eirModFPLRErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state, format(" EIR Modifier curve (function of PLR) output is negative ({:.3T}).", eirModifierFuncPLR));
            ShowContinueError(state, format(" Negative value occurs using a Part Load Ratio of {:.2T}", PLFf));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": EIR Modifier curve (function of PLR) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->eirModFPLRErrorIndex,
                                       eirModifierFuncPLR,
                                       eirModifierFuncPLR);
        eirModifierFuncPLR = 0.0;
    }

    constexpr Real64 minDefrostT = Fahrenheit2Celsius(16.0); // (16.0 - 32.0) * 5.0 / 9.0; // 16F
    constexpr Real64 maxDefrostT = Fahrenheit2Celsius(38.0); // (38.0 - 32.0) * 5.0 / 9.0; // 38F

    Real64 oaTemp2 = std::clamp(oaTempforCurve, minDefrostT, maxDefrostT); // max(minDefrostT, min(maxDefrostT, oaTempforCurve));
    Real64 eirDefrost = 1.0;

    if ((state.dataEnvrn->OutDryBulbTemp <= this->defrostMaxOADBT) && this->defrostType == DefrostType::OnDemand) {
        if (this->defrostEIRCurveIndex > 0) {
            eirDefrost = Curve::CurveValue(state, this->defrostEIRCurveIndex, oaTemp2);
        }

        if (eirDefrost < 1.0) {
            if (this->eirDefrostFTErrorIndex == 0) {
                ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
                ShowContinueError(state,
                                  format(" EIR defrost Modifier curve (function of Temperature) output is less than 1.0 ({:.3T}).", eirDefrost));
                ShowContinueError(state, format(" Negative value occurs using an outdoor air temperature of {:.2T}", oaTemp2));
                ShowContinueErrorTimeStamp(state, " Resetting curve output to 1.0 and continuing simulation.");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           format("{} \"{}\": EIR Modifier curve (function of PLR) output out of range warning continues...",
                                                  DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                                  this->name),
                                           this->eirDefrostFTErrorIndex,
                                           eirDefrost,
                                           eirDefrost);
            eirDefrost = 1.0;
        }
    }

    // Cycling Ratio
    constexpr Real64 CR_min = 0.0;
    constexpr Real64 CR_max = 1.0;
    Real64 CR = std::clamp(max(this->minPLR, partLoadRatio) / miniPLR_mod,
                           CR_min,
                           CR_max); // min(max(0.0, max(this->minPLR, partLoadRatio) / miniPLR_mod), 1.0); // partLoadRatio / this->minPLR;

    constexpr Real64 CRF_Slope = 0.4167;
    constexpr Real64 CRF_Intercept = 0.5833;
    Real64 CRF = CRF_Slope * CR + CRF_Intercept; // Use the the fixed eqn in the paper as the default curve (or maybe choose constant 1 as default)
    if (this->cycRatioCurveIndex > 0) {
        CRF = Curve::CurveValue(state, this->cycRatioCurveIndex, CR);
    }
    if (CRF <= Constant::rTinyValue) CRF = CRF_Intercept; // What could a proper default for too tiny CRF?

    // aux elec
    Real64 eirAuxElecFuncTemp = 0.0;
    if (this->auxElecEIRFoTempCurveIndex > 0) {
        eirAuxElecFuncTemp = Curve::CurveValue(state, this->auxElecEIRFoTempCurveIndex, waterTempforCurve, oaTempforCurve);
    }

    if (eirAuxElecFuncTemp < 0.0) {
        if (this->eirAuxElecFTErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state,
                              format(" Auxillary EIR Modifier curve (function of Temperatures) output is negative ({:.3T}).", eirAuxElecFuncTemp));
            ShowContinueError(state,
                              format(" Negative value occurs using a water temperature of {:.2T}C and an outdoor air temperature of {:.2T}C.",
                                     waterTempforCurve,
                                     oaTempforCurve));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(
            state,
            format("{} \"{}\": Auxillary EIR Modifier curve (function of Temperatures) output is negative warning continues...",
                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                   this->name),
            this->eirAuxElecFTErrorIndex,
            eirAuxElecFuncTemp,
            eirAuxElecFuncTemp);
        eirAuxElecFuncTemp = 0.0;
    }

    Real64 eirAuxElecFuncPLR = 0.0;
    if (this->auxElecEIRFoPLRCurveIndex > 0) {
        eirAuxElecFuncPLR = Curve::CurveValue(state, this->auxElecEIRFoPLRCurveIndex, partLoadRatio);
    }

    if (eirAuxElecFuncPLR < 0.0) {
        if (this->eirAuxElecFPLRErrorIndex == 0) {
            ShowSevereMessage(state, format("{} \"{}\":", DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)], this->name));
            ShowContinueError(state,
                              format(" Auxillary EIR Modifier curve (function of Temperatures) output is negative ({:.3T}).", eirAuxElecFuncPLR));
            ShowContinueError(state, format(" Negative value occurs using a Part Load Ratio of {:.2T}.", partLoadRatio));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       format("{} \"{}\": Auxillary EIR Modifier curve (function of PLR) output is negative warning continues...",
                                              DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                              this->name),
                                       this->eirAuxElecFPLRErrorIndex,
                                       eirAuxElecFuncPLR,
                                       eirAuxElecFuncPLR);
        eirAuxElecFuncPLR = 0.0;
    }

    if (partLoadRatio < this->minPLR) {
        this->fuelRate = 0.0;
        this->powerUsage = 0.0;
    } else {
        this->fuelRate = this->loadSideHeatTransfer / (this->referenceCOP * CRF) * eirModifierFuncPLR * eirModifierFuncTemp * eirDefrost;

        this->powerUsage = this->nominalAuxElecPower * eirAuxElecFuncTemp * eirAuxElecFuncPLR;
        if (this->defrostType == DefrostType::Timed) {
            this->powerUsage += this->defrostResistiveHeaterCap * this->defrostOpTimeFrac * reportingInterval;
        }
    }
    this->powerUsage += this->standbyElecPower;

    this->fuelEnergy = this->fuelRate * reportingInterval;
    this->powerEnergy = this->powerEnergy * reportingInterval;

    // energy balance on heat pump
    // this->sourceSideHeatTransfer = this->calcQsource(this->loadSideHeatTransfer, this->powerUsage);
    this->sourceSideHeatTransfer = this->calcQsource(this->loadSideHeatTransfer, this->fuelRate + this->powerUsage - this->standbyElecPower);
    this->sourceSideEnergy = this->sourceSideHeatTransfer * reportingInterval;

    // calculate source side outlet conditions
    Real64 CpSrc = 0.0;
    if (this->waterSource) {
        auto &thisSourcePlantLoop = state.dataPlnt->PlantLoop(this->sourceSidePlantLoc.loopNum);
        CpSrc = FluidProperties::GetSpecificHeatGlycol(
            state, thisSourcePlantLoop.FluidName, this->sourceSideInletTemp, thisSourcePlantLoop.FluidIndex, "PLFFHPEIR::simulate()");
    } else if (this->airSource) {
        CpSrc = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
    }
    // this->sourceSideCp = CpSrc; // debuging variable
    // Real64 const sourceMCp = this->sourceSideMassFlowRate * CpSrc;
    Real64 const sourceMCp = (this->sourceSideMassFlowRate < 1e-6 ? 1.0 : this->sourceSideMassFlowRate) * CpSrc;
    this->sourceSideOutletTemp = this->calcSourceOutletTemp(this->sourceSideInletTemp, this->sourceSideHeatTransfer / sourceMCp);
}

void EIRFuelFiredHeatPump::sizeSrcSideASHP(EnergyPlusData &state)
{
    // size the source-side for the air-source HP
    bool errorsFound = false;

    // these variables will be used throughout this function as a temporary value of that physical state
    Real64 tmpCapacity = this->referenceCapacity;
    Real64 tmpLoadVolFlow = this->loadSideDesignVolFlowRate;
    Real64 tmpSourceVolFlow = 0.0;

    // will leave like this for now
    // need to update these to better values later
    Real64 sourceSideInitTemp = 20.0;
    Real64 sourceSideHumRat = 0.0;
    if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpEIRHeating) {
        // same here; update later
        sourceSideInitTemp = 20.0;
    }

    Real64 const rhoSrc = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, sourceSideInitTemp, sourceSideHumRat);
    Real64 const CpSrc = Psychrometrics::PsyCpAirFnW(sourceSideHumRat);

    // set the source-side flow rate
    if (this->sourceSideDesignVolFlowRateWasAutoSized) {
        // load-side capacity should already be set, so unless the flow rate is specified, we can set
        // an assumed reasonable flow rate since this doesn't affect downstream components
        Real64 DeltaT_src = 10.0;
        // to get the source flow, we first must calculate the required heat impact on the source side
        // First the definition of COP: COP = Qload/Power, therefore Power = Qload/COP
        // Then the energy balance:     Qsrc = Qload + Power
        // Substituting for Power:      Qsrc = Qload + Qload/COP, therefore Qsrc = Qload (1 + 1/COP)
        Real64 const designSourceSideHeatTransfer = tmpCapacity * (1.0 + 1.0 / this->referenceCOP);
        // To get the design source flow rate, just apply the sensible heat rate equation:
        //                              Qsrc = rho_src * Vdot_src * Cp_src * DeltaT_src
        //                              Vdot_src = Q_src / (rho_src * Cp_src * DeltaT_src)
        tmpSourceVolFlow = designSourceSideHeatTransfer / (rhoSrc * CpSrc * DeltaT_src);
    } else if (!this->sourceSideDesignVolFlowRateWasAutoSized && this->sourceSideDesignVolFlowRate > 0.0) {
        // given the value by the user
        // set it directly
        tmpSourceVolFlow = this->sourceSideDesignVolFlowRate;
    } else if (!this->sourceSideDesignVolFlowRateWasAutoSized && this->sourceSideDesignVolFlowRate == 0.0) { // LCOV_EXCL_LINE
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

void EIRFuelFiredHeatPump::resetReportingVariables()
{
    this->loadSideHeatTransfer = 0.0;
    this->loadSideEnergy = 0.0;
    this->loadSideOutletTemp = this->loadSideInletTemp;
    this->fuelRate = 0.0;
    this->fuelEnergy = 0.0;
    this->powerUsage = 0.0;
    this->powerEnergy = 0.0;
    this->sourceSideHeatTransfer = 0.0;
    this->sourceSideOutletTemp = this->sourceSideInletTemp;
    this->sourceSideEnergy = 0.0;
}

PlantComponent *EIRFuelFiredHeatPump::factory(EnergyPlusData &state, DataPlant::PlantEquipmentType hp_type, const std::string &hp_name)
{
    if (state.dataEIRFuelFiredHeatPump->getInputsFFHP) {
        EIRFuelFiredHeatPump::processInputForEIRPLHP(state);
        EIRFuelFiredHeatPump::pairUpCompanionCoils(state);
        state.dataEIRFuelFiredHeatPump->getInputsFFHP = false;
    }

    for (auto &plhp : state.dataEIRFuelFiredHeatPump->heatPumps) {
        if (plhp.name == UtilityRoutines::makeUPPER(hp_name) && plhp.EIRHPType == hp_type) {
            return &plhp;
        }
    }

    ShowFatalError(state, format("EIR Fuel-Fired Heat Pump factory: Error getting inputs for PLFFHP named: {}.", hp_name));
    return nullptr; // LCOV_EXCL_LINE
}

void EIRFuelFiredHeatPump::pairUpCompanionCoils(EnergyPlusData &state)
{
    for (auto &thisHP : state.dataEIRFuelFiredHeatPump->heatPumps) {
        if (!thisHP.companionCoilName.empty()) {
            std::string thisCoilName = UtilityRoutines::makeUPPER(thisHP.name);
            DataPlant::PlantEquipmentType thisCoilType = thisHP.EIRHPType;
            std::string targetCompanionName = UtilityRoutines::makeUPPER(thisHP.companionCoilName);
            for (auto &potentialCompanionCoil : state.dataEIRFuelFiredHeatPump->heatPumps) {
                DataPlant::PlantEquipmentType potentialCompanionType = potentialCompanionCoil.EIRHPType;
                std::string potentialCompanionName = UtilityRoutines::makeUPPER(potentialCompanionCoil.name);
                if (potentialCompanionName == thisCoilName) {
                    // skip the current coil
                    continue;
                }
                if (potentialCompanionName == targetCompanionName) {
                    if (thisCoilType == potentialCompanionType) {
                        ShowSevereError(state,
                                        format("Invalid companion specification for EIR Plant Loop Fuel-Fired Heat Pump named \"{}\"", thisCoilName));
                        ShowContinueError(state, "For heating objects, the companion must be a cooling object, and vice-versa");
                        ShowFatalError(state, "Invalid companion object causes program termination");
                    }
                    thisHP.companionHeatPumpCoil = &potentialCompanionCoil;
                    break;
                }
            }
            if (!thisHP.companionHeatPumpCoil) {
                ShowSevereError(state, "Could not find matching companion heat pump coil.");
                ShowContinueError(state, format("Base coil: {}", thisCoilName));
                ShowContinueError(state, format("Looking for companion coil named: {}", targetCompanionName));
                ShowFatalError(state, "Simulation aborts due to previous severe error");
            }
        }
    }
}

void EIRFuelFiredHeatPump::processInputForEIRPLHP(EnergyPlusData &state)
{
    struct ClassType
    {
        DataPlant::PlantEquipmentType thisType;
        std::string nodesType;
        std::function<Real64(Real64, Real64)> calcLoadOutletTemp;
        std::function<Real64(Real64, Real64)> calcQsource;
        std::function<Real64(Real64, Real64)> calcSourceOutletTemp;

        ClassType(DataPlant::PlantEquipmentType _thisType,
                  std::string _nodesType,
                  std::function<Real64(Real64, Real64)> _tLoadOutFunc,
                  std::function<Real64(Real64, Real64)> _qSrcFunc,
                  std::function<Real64(Real64, Real64)> _tSrcOutFunc)
            : thisType(_thisType), nodesType(std::move(_nodesType)), calcLoadOutletTemp(_tLoadOutFunc), calcQsource(_qSrcFunc),
              calcSourceOutletTemp(_tSrcOutFunc)
        {
        }
    };
    std::array<ClassType, 2> classesToInput = {
        ClassType{DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling,
                  "Chilled Water Nodes",
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::subtract,
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::add,
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::add},
        ClassType{DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating,
                  "Hot Water Nodes",
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::add,
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::subtract,
                  EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump::subtract},
    };

    bool errorsFound = false;
    std::string &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    for (auto &classToInput : classesToInput) {
        cCurrentModuleObject = DataPlant::PlantEquipTypeNames[static_cast<int>(classToInput.thisType)];

        DataLoopNode::ConnectionObjectType objType = static_cast<DataLoopNode::ConnectionObjectType>(
            getEnumValue(BranchNodeConnections::ConnectionObjectTypeNamesUC, UtilityRoutines::makeUPPER(cCurrentModuleObject)));

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) continue;
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

            EIRFuelFiredHeatPump thisPLHP;

            thisPLHP.EIRHPType = classToInput.thisType;
            std::string companionCoilFieldTag = "companion_heating_heat_pump_name";
            std::string refCapFieldTag = "nominal_cooling_capacity";
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating) {
                companionCoilFieldTag = "companion_cooling_heat_pump_name";
                refCapFieldTag = "nominal_heating_capacity";
            }

            // A1-A3
            thisPLHP.name = UtilityRoutines::makeUPPER(thisObjectName);
            std::string loadSideInletNodeName = UtilityRoutines::makeUPPER(fields.at("water_inlet_node_name").get<std::string>());
            std::string loadSideOutletNodeName = UtilityRoutines::makeUPPER(fields.at("water_outlet_node_name").get<std::string>());
            // Implicit
            // std::string condenserType = "AIRSOURCE"; // UtilityRoutines::makeUPPER(fields.at("condenser_type").get<std::string>());
            thisPLHP.airSource = true;
            thisPLHP.waterSource = false;

            // A4
            std::string sourceSideInletNodeName = UtilityRoutines::makeUPPER(fields.at("air_source_node_name").get<std::string>());
            // UtilityRoutines::makeUPPER(fields.at("source_side_outlet_node_name").get<std::string>());
            std::string sourceSideOutletNodeName = format("{}_SOURCE_SIDE_OUTLET_NODE", thisPLHP.name);

            // A5
            auto compCoilFound = fields.find(companionCoilFieldTag);
            if (compCoilFound != fields.end()) { // optional field
                thisPLHP.companionCoilName = UtilityRoutines::makeUPPER(compCoilFound.value().get<std::string>());
            }

            // A6 Fuel Type
            std::string tempRsrStr = UtilityRoutines::makeUPPER(fields.at("fuel_type").get<std::string>());
            thisPLHP.fuelType = static_cast<Constant::eFuel>(getEnumValue(Constant::eFuelNamesUC, tempRsrStr));
            // Validate fuel type input
            static constexpr std::string_view RoutineName("processInputForEIRPLHP: ");
            if (thisPLHP.fuelType == Constant::eFuel::Invalid) {
                ShowSevereError(state, format("{}{}=\"{}\",", RoutineName, cCurrentModuleObject, thisPLHP.name));
                ShowContinueError(state, format("Invalid Fuel Type = {}", tempRsrStr));
                ShowContinueError(state, "Reset the Fuel Type to \"NaturalGas\".");
                thisPLHP.fuelType = Constant::eFuel::NaturalGas;
                errorsFound = true;
            }

            // A7 End use category
            thisPLHP.endUseSubcat = UtilityRoutines::makeUPPER(fields.at("end_use_subcategory").get<std::string>());
            if (thisPLHP.endUseSubcat == "") {
                thisPLHP.endUseSubcat = "Heat Pump Fuel Fired"; // or "General"?
            }

            // N1 Nominal heating capacity
            auto &tmpRefCapacity = fields.at(refCapFieldTag);

            if (tmpRefCapacity == "Autosize") {
                thisPLHP.referenceCapacity = DataSizing::AutoSize;
                thisPLHP.referenceCapacityWasAutoSized = true;
            } else {
                thisPLHP.referenceCapacity = tmpRefCapacity.get<Real64>();
            }

            // N2 Nominal heating capacity
            thisPLHP.referenceCOP = fields.at("nominal_cop").get<Real64>();
            if (thisPLHP.referenceCOP <= 0.0) thisPLHP.referenceCOP = 1.0;

            // N3 Design flow rate
            auto &tmpFlowRate = fields.at("design_flow_rate");
            if (tmpFlowRate == "Autosize") {
                thisPLHP.loadSideDesignVolFlowRate = DataSizing::AutoSize;
                thisPLHP.loadSideDesignVolFlowRateWasAutoSized = true;
            } else {
                thisPLHP.loadSideDesignVolFlowRate = tmpFlowRate.get<Real64>();
            }

            // GAHP: Add a default source side flow rate, not from input
            Real64 defDummyASDesVolFlowRate = 1.0;
            thisPLHP.sourceSideDesignVolFlowRate = defDummyASDesVolFlowRate;

            // N4 Design supply temperature
            auto &tmpDesSupTemp = fields.at("design_supply_temperature");
            if (tmpDesSupTemp == "Autosize") {
                // sizing
            } else {
                thisPLHP.desSupplyTemp = tmpDesSupTemp.get<Real64>();
            }

            // N5 Design temperature lift
            auto &tmpDesTempLift = fields.at("design_temperature_lift");
            if (tmpDesTempLift == "Autosize") {
                // sizing
            } else {
                thisPLHP.desTempLift = tmpDesTempLift.get<Real64>();
            }

            // N6 Sizing factor
            auto sizeFactorFound = fields.find("sizing_factor");
            if (sizeFactorFound != fields.end()) {
                thisPLHP.sizingFactor = sizeFactorFound.value().get<Real64>();
                if (thisPLHP.sizingFactor <= 0.0) thisPLHP.sizingFactor = 1.0;
            } else {
                Real64 defaultVal_sizeFactor = 1.0;
                if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                        state, cCurrentModuleObject, "sizing_factor", defaultVal_sizeFactor)) {
                    ShowSevereError(state, "EIR FFHP: Sizing factor not entered and could not get default value");
                    errorsFound = true;
                } else {
                    thisPLHP.sizingFactor = defaultVal_sizeFactor;
                }
            }

            // A8 flow mode
            thisPLHP.flowMode = static_cast<DataPlant::FlowMode>(
                getEnumValue(DataPlant::FlowModeNamesUC, UtilityRoutines::makeUPPER(fields.at("flow_mode").get<std::string>())));

            // A9 outdoor_air_temperature_curve_input_variable
            std::string oaTempCurveInputVar =
                UtilityRoutines::makeUPPER(fields.at("outdoor_air_temperature_curve_input_variable").get<std::string>());
            thisPLHP.oaTempCurveInputVar = static_cast<OATempCurveVar>(getEnumValue(OATempCurveVarNamesUC, oaTempCurveInputVar));

            // A10 water_temperature_curve_input_variable
            std::string waterTempCurveInputVar = UtilityRoutines::makeUPPER(fields.at("water_temperature_curve_input_variable").get<std::string>());
            thisPLHP.waterTempCurveInputVar = static_cast<WaterTempCurveVar>(getEnumValue(WaterTempCurveVarNamesUC, waterTempCurveInputVar));

            // A11 normalized_capacity_function_of_temperature_curve_name
            std::string const &capFtName =
                UtilityRoutines::makeUPPER(fields.at("normalized_capacity_function_of_temperature_curve_name").get<std::string>());
            thisPLHP.capFuncTempCurveIndex = Curve::GetCurveIndex(state, capFtName);
            if (thisPLHP.capFuncTempCurveIndex == 0) {
                ShowSevereError(state, format("Invalid curve name for EIR PLFFHP (name={}; entered curve name: {}", thisPLHP.name, capFtName));
                errorsFound = true;
            }

            // A12 fuel_energy_input_ratio_function_of_temperature_curve_name
            std::string const &eirFtName =
                UtilityRoutines::makeUPPER(fields.at("fuel_energy_input_ratio_function_of_temperature_curve_name").get<std::string>());
            thisPLHP.powerRatioFuncTempCurveIndex = Curve::GetCurveIndex(state, eirFtName);
            if (thisPLHP.capFuncTempCurveIndex == 0) {
                ShowSevereError(state, format("Invalid curve name for EIR PLFFHP (name={}; entered curve name: {}", thisPLHP.name, eirFtName));
                errorsFound = true;
            }
            // A13 fuel_energy_input_ratio_function_of_plr_curve_name
            std::string const &eirFplrName =
                UtilityRoutines::makeUPPER(fields.at("fuel_energy_input_ratio_function_of_plr_curve_name").get<std::string>());
            thisPLHP.powerRatioFuncPLRCurveIndex = Curve::GetCurveIndex(state, eirFplrName);
            if (thisPLHP.capFuncTempCurveIndex == 0) {
                ShowSevereError(state, format("Invalid curve name for EIR PLFFHP (name={}; entered curve name: {}", thisPLHP.name, eirFplrName));
                errorsFound = true;
            }

            // N7 min PLR
            auto minPLRFound = fields.find("minimum_part_load_ratio");
            if (minPLRFound != fields.end()) {
                thisPLHP.minPLR = minPLRFound.value().get<Real64>();
            } else {
                Real64 defaultVal = 0.1;
                if (!state.dataInputProcessing->inputProcessor->getDefaultValue(state, cCurrentModuleObject, "minimum_part_load_ratio", defaultVal)) {
                    ShowSevereError(state, "EIR PLFFHP: minimum PLR not entered and could not get default value.");
                    errorsFound = true;
                } else {
                    thisPLHP.minPLR = defaultVal;
                }
            }

            // N8 max PLR
            auto maxPLRFound = fields.find("maximum_part_load_ratio");
            if (maxPLRFound != fields.end()) {
                thisPLHP.maxPLR = maxPLRFound.value().get<Real64>();
            } else {
                Real64 defaultVal = 1.0;
                if (!state.dataInputProcessing->inputProcessor->getDefaultValue(state, cCurrentModuleObject, "maximum_part_load_ratio", defaultVal)) {
                    ShowSevereError(state, "EIR PLFFHP: maximum PLR not entered and could not get default value.");
                    errorsFound = true;
                } else {
                    thisPLHP.maxPLR = defaultVal;
                }
            }

            // A14 fuel_energy_input_ratio_defrost_adjustment_curve_name
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
                thisPLHP.defrostEIRCurveIndex = 0;
            } else {
                auto eirDefrostCurveFound = fields.find("fuel_energy_input_ratio_defrost_adjustment_curve_name");
                if (eirDefrostCurveFound != fields.end()) {
                    std::string const eirDefrostCurveName = UtilityRoutines::makeUPPER(eirDefrostCurveFound.value().get<std::string>());
                    thisPLHP.defrostEIRCurveIndex = Curve::GetCurveIndex(state, eirDefrostCurveName);
                    if (thisPLHP.defrostEIRCurveIndex == 0) {
                        ShowSevereError(
                            state, format("Invalid curve name for EIR FFHP (name={}; entered curve name: {}", thisPLHP.name, eirDefrostCurveName));
                        errorsFound = true;
                    }
                } else {
                    thisPLHP.defrostEIRCurveIndex = 0;
                }
            }

            // A15 defrost_control_type
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
                thisPLHP.defrostType = DefrostType::Invalid;
            } else {
                thisPLHP.defrostType = static_cast<DefrostType>(
                    getEnumValue(DefrostTypeNamesUC, UtilityRoutines::makeUPPER(fields.at("defrost_control_type").get<std::string>())));
                if (thisPLHP.defrostType == DefrostType::Invalid) {
                    thisPLHP.defrostType = DefrostType::OnDemand; // set to default
                    thisPLHP.defrostOpTimeFrac = 0.0;
                    ShowWarningError(state, format("Invalid Defrost Control Type for EIR PLFFHP ({} name={})", cCurrentModuleObject, thisPLHP.name));
                    ShowContinueError(state,
                                      format("The Input Variable is reset to: {}", DefrostTypeNamesUC[static_cast<int>(thisPLHP.defrostType)]));
                }
            }

            // N9 defrost_operation_time_fraction
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
                thisPLHP.defrostOpTimeFrac = 0.0;
            } else {
                auto defrostOpTimeFracFound = fields.find("defrost_operation_time_fraction");
                if (defrostOpTimeFracFound != fields.end()) {
                    thisPLHP.defrostOpTimeFrac = defrostOpTimeFracFound.value().get<Real64>();
                } else {
                    Real64 defaultVal = 0.0;
                    if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                            state, cCurrentModuleObject, "defrost_operation_time_fraction", defaultVal)) {
                        ShowSevereError(state, "EIR PLFFHP: defrost time fraction not entered and could not get default value.");
                        errorsFound = true;
                    } else {
                        thisPLHP.defrostOpTimeFrac = defaultVal;
                    }
                }
            }

            // N10 Resistive Defrost Heater Capacity
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
                thisPLHP.defrostResistiveHeaterCap = 0.0;
            } else {
                auto resDefrostHeaterCapFound = fields.find("resistive_defrost_heater_capacity");
                if (resDefrostHeaterCapFound != fields.end()) {
                    thisPLHP.defrostResistiveHeaterCap = resDefrostHeaterCapFound.value().get<Real64>();
                } else {
                    Real64 defaultVal = 0.0;
                    if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                            state, cCurrentModuleObject, "resistive_defrost_heater_capacity", defaultVal)) {
                        ShowSevereError(state, "EIR PLFFHP: Resistive Defrost Heater Capacity not entered and could not get default value.");
                        errorsFound = true;
                    } else {
                        thisPLHP.defrostResistiveHeaterCap = defaultVal;
                    }
                }
            }

            // N11 maximum_outdoor_dry_bulb_temperature_for_defrost_operation
            if (thisPLHP.EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) {
                thisPLHP.defrostMaxOADBT = -99.0;
            } else {
                auto maxOADBTFound = fields.find("maximum_outdoor_dry_bulb_temperature_for_defrost_operation");
                if (maxOADBTFound != fields.end()) {
                    thisPLHP.defrostMaxOADBT = maxOADBTFound.value().get<Real64>();
                } else {
                    Real64 defaultVal = 5.0;
                    if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                            state, cCurrentModuleObject, "maximum_outdoor_dry_bulb_temperature_for_defrost_operation", defaultVal)) {
                        ShowSevereError(state, "EIR PLFFHP: max defrost operation OA temperature not entered and could not get default value.");
                        errorsFound = true;
                    } else {
                        thisPLHP.defrostMaxOADBT = defaultVal;
                    }
                }
            }

            // A16 cycling_ratio_factor_curve_name
            auto crfCurveFound = fields.find("cycling_ratio_factor_curve_name");
            if (crfCurveFound != fields.end()) {
                std::string const cycRatioCurveName = UtilityRoutines::makeUPPER(crfCurveFound.value().get<std::string>());
                thisPLHP.cycRatioCurveIndex = Curve::GetCurveIndex(state, cycRatioCurveName);
                if (thisPLHP.cycRatioCurveIndex == 0) {
                    ShowSevereError(state,
                                    format("Invalid curve name for EIR PLFFHP (name={}; entered curve name: {})", thisPLHP.name, cycRatioCurveName));
                    errorsFound = true;
                }
            } else {
                thisPLHP.cycRatioCurveIndex = 0;
            }

            // N12 nominal_auxiliary_electric_power
            auto nomAuxElecPowerFound = fields.find("nominal_auxiliary_electric_power");
            if (nomAuxElecPowerFound != fields.end()) {
                thisPLHP.nominalAuxElecPower = nomAuxElecPowerFound.value().get<Real64>();
            } else {
                Real64 defaultVal = 0.0;
                if (!state.dataInputProcessing->inputProcessor->getDefaultValue(
                        state, cCurrentModuleObject, "nominal_auxiliary_electric_power", defaultVal)) {
                    ShowSevereError(state, "EIR PLFFHP: nominal auxiliary electric power not entered and could not get default value.");
                    errorsFound = true;
                } else {
                    thisPLHP.nominalAuxElecPower = defaultVal;
                }
            }

            // A17 auxiliary_electric_energy_input_ratio_function_of_temperature_curve_name
            auto auxElecEIRFTCurveFound = fields.find("auxiliary_electric_energy_input_ratio_function_of_temperature_curve_name");
            if (auxElecEIRFTCurveFound != fields.end()) {
                std::string const &auxEIRFTName = UtilityRoutines::makeUPPER(auxElecEIRFTCurveFound.value().get<std::string>());
                thisPLHP.auxElecEIRFoTempCurveIndex = Curve::GetCurveIndex(state, auxEIRFTName);
                if (thisPLHP.auxElecEIRFoTempCurveIndex == 0) {
                    ShowSevereError(state, format("Invalid curve name for EIR FFHP (name={}; entered curve name: {}", thisPLHP.name, auxEIRFTName));
                    errorsFound = true;
                }
            } else {
                thisPLHP.auxElecEIRFoTempCurveIndex = 0;
            }

            // A18 auxiliary_electric_energy_input_ratio_function_of_plr_curve_name
            auto auxElecEIRFPLRCurveFound = fields.find("auxiliary_electric_energy_input_ratio_function_of_plr_curve_name");
            if (auxElecEIRFPLRCurveFound != fields.end()) {
                std::string const &auxEIRFPLRName = UtilityRoutines::makeUPPER(auxElecEIRFPLRCurveFound.value().get<std::string>());
                thisPLHP.auxElecEIRFoPLRCurveIndex = Curve::GetCurveIndex(state, auxEIRFPLRName);
                if (thisPLHP.auxElecEIRFoPLRCurveIndex == 0) {
                    ShowSevereError(state, format("Invalid curve name for EIR FFHP (name={}; entered curve name: {}", thisPLHP.name, auxEIRFPLRName));
                    errorsFound = true;
                }
            } else {
                thisPLHP.auxElecEIRFoPLRCurveIndex = 0;
            }

            // N13 standby_electric_power
            auto stdElecPwrFound = fields.find("standby_electric_power");
            if (stdElecPwrFound != fields.end()) {
                thisPLHP.standbyElecPower = stdElecPwrFound.value().get<Real64>();
            } else {
                Real64 defaultVal = 0.0;
                if (!state.dataInputProcessing->inputProcessor->getDefaultValue(state, cCurrentModuleObject, "standby_electric_power", defaultVal)) {
                    ShowSevereError(state, "EIR FFHP: standby electric power not entered and could not get default value.");
                    errorsFound = true;
                } else {
                    thisPLHP.standbyElecPower = defaultVal;
                }
            }

            bool nodeErrorsFound = false;
            thisPLHP.loadSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                               loadSideInletNodeName,
                                                                               nodeErrorsFound,
                                                                               objType,
                                                                               thisPLHP.name,
                                                                               DataLoopNode::NodeFluidType::Water,
                                                                               DataLoopNode::ConnectionType::Inlet,
                                                                               NodeInputManager::CompFluidStream::Primary,
                                                                               DataLoopNode::ObjectIsNotParent);
            thisPLHP.loadSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                loadSideOutletNodeName,
                                                                                nodeErrorsFound,
                                                                                objType,
                                                                                thisPLHP.name,
                                                                                DataLoopNode::NodeFluidType::Water,
                                                                                DataLoopNode::ConnectionType::Outlet,
                                                                                NodeInputManager::CompFluidStream::Primary,
                                                                                DataLoopNode::ObjectIsNotParent);

            thisPLHP.airSource = true;    // this is always true, at least for now, for Fuel-Fired PlantLoop Heat Pump
            thisPLHP.waterSource = false; // this is always false, at least for now, for Fuel-Fired PlantLoop Heat Pump
            thisPLHP.sourceSideNodes.inlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                 sourceSideInletNodeName,
                                                                                 nodeErrorsFound,
                                                                                 objType,
                                                                                 thisPLHP.name,
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::ConnectionType::OutsideAir,
                                                                                 NodeInputManager::CompFluidStream::Secondary,
                                                                                 DataLoopNode::ObjectIsNotParent);
            thisPLHP.sourceSideNodes.outlet = NodeInputManager::GetOnlySingleNode(state,
                                                                                  sourceSideOutletNodeName,
                                                                                  nodeErrorsFound,
                                                                                  objType,
                                                                                  thisPLHP.name,
                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                  DataLoopNode::ConnectionType::OutsideAir,
                                                                                  NodeInputManager::CompFluidStream::Secondary,
                                                                                  DataLoopNode::ObjectIsNotParent);

            if (nodeErrorsFound) errorsFound = true;
            BranchNodeConnections::TestCompSet(
                state, cCurrentModuleObject, thisPLHP.name, loadSideInletNodeName, loadSideOutletNodeName, classToInput.nodesType);

            // store the worker functions that generalized the heating/cooling sides
            thisPLHP.calcLoadOutletTemp = classToInput.calcLoadOutletTemp;
            thisPLHP.calcQsource = classToInput.calcQsource;
            thisPLHP.calcSourceOutletTemp = classToInput.calcSourceOutletTemp;

            if (!errorsFound) {
                state.dataEIRFuelFiredHeatPump->heatPumps.push_back(thisPLHP);
            }
        }
    }
    if (errorsFound) {
        ShowFatalError(state, "Previous EIR PLFFHP errors cause program termination."); // LCOV_EXCL_LINE
    }
}

void EIRFuelFiredHeatPump::oneTimeInit(EnergyPlusData &state)
{
    // This function does all the one-time initialization
    constexpr std::string_view routineName = "EIRFuelFiredHeatPump : oneTimeInit"; // + __FUNCTION__;

    if (this->oneTimeInitFlag) {
        bool errFlag = false;

        // setup output variables
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Load Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->loadSideHeatTransfer,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Load Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->loadSideEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->name,
                            {},
                            "ENERGYTRANSFER",
                            {},
                            {},
                            "Plant");
        // Setup Output Variable(state,
        //                    "Fuel-fired Absorption Heat Pump Source Side Heat Transfer Rate",
        //                    OutputProcessor::Unit::W,
        //                    this->sourceSideHeatTransfer,
        //                    OutputProcessor::SOVTimeStepType::System,
        //                    OutputProcessor::SOVStoreType::Average,
        //                    this->name);
        // Setup Output Variable(state,
        //                    "Fuel-fired Absorption Heat Pump Source Side Heat Transfer Energy",
        //                    OutputProcessor::Unit::J,
        //                    this->sourceSideEnergy,
        //                    OutputProcessor::SOVTimeStepType::System,
        //                    OutputProcessor::SOVStoreType::Summed,
        //                    this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Inlet Temperature", // "Heat Pump Load Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->loadSideInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Outlet Temperature", // "Heat Pump Load Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->loadSideOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        // Setup Output Variable(state,
        //                    "Fuel-fired Absorption Heat Pump Source Side Inlet Temperature",
        //                    OutputProcessor::Unit::C,
        //                    this->sourceSideInletTemp,
        //                    OutputProcessor::SOVTimeStepType::System,
        //                    OutputProcessor::SOVStoreType::Average,
        //                    this->name);
        // Setup Output Variable(state,
        //                    "Heat Pump Source Side Outlet Temperature",
        //                    OutputProcessor::Unit::C,
        //                    this->sourceSideOutletTemp,
        //                    OutputProcessor::SOVTimeStepType::System,
        //                    OutputProcessor::SOVStoreType::Average,
        //                    this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Fuel Rate",
                            OutputProcessor::Unit::W,
                            this->fuelRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->powerUsage,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        if (this->EIRHPType == DataPlant::PlantEquipmentType::HeatPumpFuelFiredCooling) { // energy from HeatPump:AirToWater:FuelFired:Cooling object
            SetupOutputVariable(state,
                                "Fuel-fired Absorption HeatPump Fuel Energy",
                                OutputProcessor::Unit::J,
                                this->fuelEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                Constant::eFuelNames[static_cast<int>(this->fuelType)],
                                "Cooling",
                                this->endUseSubcat, //"Heat Pump",
                                "Plant");
            SetupOutputVariable(state,
                                "Fuel-fired Absorption HeatPump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                "Electricity",
                                "Cooling",
                                this->endUseSubcat, // "Heat Pump",
                                "Plant");
        } else if (this->EIRHPType ==
                   DataPlant::PlantEquipmentType::HeatPumpFuelFiredHeating) { // energy from HeatPump:AirToWater:FuelFired:Heating object
            SetupOutputVariable(state,
                                "Fuel-fired Absorption HeatPump Fuel Energy",
                                OutputProcessor::Unit::J,
                                this->fuelEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                Constant::eFuelNames[static_cast<int>(this->fuelType)],
                                "Heating",
                                this->endUseSubcat, // "Heat Pump",
                                "Plant");
            SetupOutputVariable(state,
                                "Fuel-fired Absorption HeatPump Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->powerEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->name,
                                {},
                                "Electricity",
                                "Heating",
                                this->endUseSubcat, // "Heat Pump",
                                "Plant");
        }
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->loadSideMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);
        SetupOutputVariable(state,
                            "Fuel-fired Absorption HeatPump Volumetric Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->loadSideVolumeFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->name);

        // find this component on the plant
        bool thisErrFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(
            state, this->name, this->EIRHPType, this->loadSidePlantLoc, thisErrFlag, _, _, _, this->loadSideNodes.inlet, _);

        if (thisErrFlag) {
            ShowSevereError(state,
                            format("{}: Plant topology problem for {} name = \"{}\"",
                                   routineName,
                                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                   this->name));
            ShowContinueError(state, "Could not locate component's load side connections on a plant loop");
            errFlag = true;
        } else if (this->loadSidePlantLoc.loopSideNum != DataPlant::LoopSideLocation::Supply) { // only check if !thisErrFlag
            ShowSevereError(state,
                            format("{}: Invalid connections for {} name = \"{}\"",
                                   routineName,
                                   DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                   this->name));
            ShowContinueError(state, "The load side connections are not on the Supply Side of a plant loop");
            errFlag = true;
        }

        thisErrFlag = false;
        if (this->waterSource) {
            PlantUtilities::ScanPlantLoopsForObject(
                state, this->name, this->EIRHPType, this->sourceSidePlantLoc, thisErrFlag, _, _, _, this->sourceSideNodes.inlet, _);

            if (thisErrFlag) {
                ShowSevereError(state,
                                format("{}: Plant topology problem for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "Could not locate component's source side connections on a plant loop.");
                errFlag = true;
            } else if (this->sourceSidePlantLoc.loopSideNum != DataPlant::LoopSideLocation::Demand) { // only check if !thisErrFlag
                ShowSevereError(state,
                                format("{}: Invalid connections for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "The source side connections are not on the Demand Side of a plant loop.");
                errFlag = true;
            }

            // make sure it is not the same loop on both sides.
            if (this->loadSidePlantLoc.loopNum == this->sourceSidePlantLoc.loopNum) { // user is being too tricky, don't allow
                ShowSevereError(state,
                                format("{}: Invalid connections for {} name = \"{}\"",
                                       routineName,
                                       DataPlant::PlantEquipTypeNames[static_cast<int>(this->EIRHPType)],
                                       this->name));
                ShowContinueError(state, "The load and source sides need to be on different loops.");
                errFlag = true;
            } else {

                PlantUtilities::InterConnectTwoPlantLoopSides(state, this->loadSidePlantLoc, this->sourceSidePlantLoc, this->EIRHPType, true);
            }
        } else if (this->airSource) {
            // nothing to do here ?
        }

        if (errFlag) {
            ShowFatalError(state, format("{}: Program terminated due to previous condition(s).", routineName));
        }
        this->oneTimeInitFlag = false;
    }
}

} // namespace EnergyPlus::EIRPlantLoopHeatPumps
