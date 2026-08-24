// EnergyPlus, Copyright (c) 1996-present, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Energy Innovation, LLC, and other
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
#include <algorithm>
#include <array>
#include <cmath>
#include <format>
#include <limits>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Formatters.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::PlantCentralGSHP {

// MODULE INFORMATION:
//       AUTHOR         PNNL
//       DATE WRITTEN   Feb 2013
//       MODIFIED       na
//       RE-ENGINEERED  na
// PURPOSE OF THIS MODULE:
// This module simulates the performance of the Central Plant GSHP systems
// It currently includes one object: ChillerHeaterPerformance:Electric:EIR.
// The other object available for this central CGSHP system such as HeatPumpPerformance:WaterToWater:EIR
//      will be implemented later.

// METHODOLOGY EMPLOYED:
//  Once the PlantLoopManager determines that the Central Plant GSHP
//  is available to meet a loop cooling and heating demands, it calls simulate
//  which in turn calls the electric PlantCentralGSHP model. The PlantCentralGSHP model is based on
//  polynomial fits of chiller/heater or heat pump performance data.

namespace {

    void getPartLoadCurveMinMax(EnergyPlusData &state, int const curveIndex, Real64 &minimumPartLoadRatio, Real64 &maximumPartLoadRatio)
    {
        auto const *curve = state.dataCurveManager->curves(curveIndex);
        if (curve->numDims == 1) {
            Curve::GetCurveMinMaxValues(state, curveIndex, minimumPartLoadRatio, maximumPartLoadRatio);
        } else {
            Real64 condenserTempMinimum = 0.0;
            Real64 condenserTempMaximum = 0.0;
            Curve::GetCurveMinMaxValues(state, curveIndex, condenserTempMinimum, condenserTempMaximum, minimumPartLoadRatio, maximumPartLoadRatio);
        }
    }

    Real64 evaluatePartLoadCurve(EnergyPlusData &state, int const curveIndex, Real64 const condenserTemp, Real64 const partLoadRatio)
    {
        auto const *curve = state.dataCurveManager->curves(curveIndex);
        return curve->numDims == 1 ? Curve::CurveValue(state, curveIndex, partLoadRatio)
                                   : Curve::CurveValue(state, curveIndex, condenserTemp, partLoadRatio);
    }

} // namespace

void ChillerHeaterSpecs::mapResultToPlantConnections()
{
    auto &result = this->Result;

    result.chilledWaterInletTemp = 0.0;
    result.chilledWaterOutletTemp = 0.0;
    result.chilledWaterMassFlowRate = 0.0;
    result.hotWaterInletTemp = 0.0;
    result.hotWaterOutletTemp = 0.0;
    result.hotWaterMassFlowRate = 0.0;
    result.sourceInletTemp = 0.0;
    result.sourceOutletTemp = 0.0;
    result.sourceMassFlowRate = 0.0;
    result.coolingDelivered = 0.0;
    result.heatingDelivered = 0.0;
    result.heatRecovered = 0.0;
    result.sourceHeatTransfer = 0.0;

    switch (result.currentMode) {
    case CurrentMode::CoolingOnly:
    case CurrentMode::CoolingDominant:
        result.chilledWaterInletTemp = result.evaporatorInletTemp;
        result.chilledWaterOutletTemp = result.evaporatorOutletTemp;
        result.chilledWaterMassFlowRate = result.evaporatorMassFlowRate;
        result.sourceInletTemp = result.condenserInletTemp;
        result.sourceOutletTemp = result.condenserOutletTemp;
        result.sourceMassFlowRate = result.condenserMassFlowRate;
        result.coolingDelivered = result.qEvaporator;
        result.sourceHeatTransfer = result.qCondenser;
        break;
    case CurrentMode::HeatingOnly:
    case CurrentMode::HeatingDominant:
        result.hotWaterInletTemp = result.condenserInletTemp;
        result.hotWaterOutletTemp = result.condenserOutletTemp;
        result.hotWaterMassFlowRate = result.condenserMassFlowRate;
        result.sourceInletTemp = result.evaporatorInletTemp;
        result.sourceOutletTemp = result.evaporatorOutletTemp;
        result.sourceMassFlowRate = result.evaporatorMassFlowRate;
        result.heatingDelivered = result.qCondenser;
        result.sourceHeatTransfer = -result.qEvaporator;
        break;
    case CurrentMode::HeatRecovery:
        result.chilledWaterInletTemp = result.evaporatorInletTemp;
        result.chilledWaterOutletTemp = result.evaporatorOutletTemp;
        result.chilledWaterMassFlowRate = result.evaporatorMassFlowRate;
        result.hotWaterInletTemp = result.condenserInletTemp;
        result.hotWaterOutletTemp = result.condenserOutletTemp;
        result.hotWaterMassFlowRate = result.condenserMassFlowRate;
        result.coolingDelivered = result.qEvaporator;
        result.heatingDelivered = result.qCondenser;
        result.heatRecovered = result.qCondenser;
        break;
    case CurrentMode::Off:
    case CurrentMode::Invalid:
    case CurrentMode::Num:
        break;
    }

    result.updatePowerAccounting(this->OpenMotorEff);
}

void ChillerHeaterSpecs::applySimultaneousCoolingConnection()
{
    auto &result = this->Result;
    auto const &coolingResult = this->SimulResult;

    result.requestedCoolingLoad = coolingResult.requestedCoolingLoad;
    result.unmetCoolingLoad = coolingResult.unmetCoolingLoad;
    result.chilledWaterInletTemp = coolingResult.chilledWaterInletTemp;
    result.chilledWaterOutletTemp = coolingResult.chilledWaterOutletTemp;
    result.chilledWaterMassFlowRate = coolingResult.chilledWaterMassFlowRate;
    result.coolingDelivered = coolingResult.coolingDelivered;

    if (result.currentMode == CurrentMode::HeatingDominant) {
        result.sourceHeatTransfer = -(result.qEvaporator - result.coolingDelivered);
    }
}

void ChillerHeaterSpecs::syncLegacyReportAndNodes()
{
    auto const &result = this->Result;

    this->EvapInletNode.Temp = result.evaporatorInletTemp;
    this->EvapOutletNode.Temp = result.evaporatorOutletTemp;
    this->EvapOutletNode.MassFlowRate = result.evaporatorMassFlowRate;
    this->CondInletNode.Temp = result.condenserInletTemp;
    this->CondOutletNode.Temp = result.condenserOutletTemp;
    this->CondOutletNode.MassFlowRate = result.condenserMassFlowRate;

    this->Report.currentMode = result.currentMode;
    this->Report.ChillerPartLoadRatio = result.partLoadRatio;
    this->Report.ChillerCyclingRatio = result.cyclingRatio;
    this->Report.ChillerFalseLoadRate = result.falseLoadRate;
    this->Report.ChillerFalseLoad = result.falseLoadEnergy;
    this->Report.ChillerCapFT = result.capacityTemperatureModifier;
    this->Report.ChillerEIRFT = result.eirTemperatureModifier;
    this->Report.ChillerEIRFPLR = result.eirPartLoadModifier;
    this->Report.CoolingPower = result.coolingPower;
    this->Report.HeatingPower = result.heatingPower;
    this->Report.CoolingEnergy = result.coolingEnergy;
    this->Report.HeatingEnergy = result.heatingEnergy;
    this->Report.QEvap = result.qEvaporator;
    this->Report.QCond = result.qCondenser;
    this->Report.EvapEnergy = result.evaporatorEnergy;
    this->Report.CondEnergy = result.condenserEnergy;
    this->Report.EvapOutletTemp = result.evaporatorOutletTemp;
    this->Report.EvapInletTemp = result.evaporatorInletTemp;
    this->Report.CondOutletTemp = result.condenserOutletTemp;
    this->Report.CondInletTemp = result.condenserInletTemp;
    this->Report.Evapmdot = result.evaporatorMassFlowRate;
    this->Report.Condmdot = result.condenserMassFlowRate;
    this->Report.ActualCOP = result.actualCOP;
}

void ChillerHeaterSpecs::saveCurrentResultForSimultaneous()
{
    this->SimulResult = this->Result;
    auto const &result = this->SimulResult;

    this->Report.ChillerPartLoadRatioSimul = result.partLoadRatio;
    this->Report.ChillerCyclingRatioSimul = result.cyclingRatio;
    this->Report.ChillerFalseLoadRateSimul = result.falseLoadRate;
    this->Report.ChillerFalseLoadSimul = result.falseLoadEnergy;
    this->Report.ChillerCapFTSimul = result.capacityTemperatureModifier;
    this->Report.ChillerEIRFTSimul = result.eirTemperatureModifier;
    this->Report.ChillerEIRFPLRSimul = result.eirPartLoadModifier;
    this->Report.CoolingPowerSimul = result.coolingPower;
    this->Report.CoolingEnergySimul = result.coolingEnergy;
    this->Report.QEvapSimul = result.qEvaporator;
    this->Report.QCondSimul = result.qCondenser;
    this->Report.EvapEnergySimul = result.evaporatorEnergy;
    this->Report.CondEnergySimul = result.condenserEnergy;
    this->Report.EvapOutletTempSimul = result.evaporatorOutletTemp;
    this->Report.EvapInletTempSimul = result.evaporatorInletTemp;
    this->Report.EvapmdotSimul = result.evaporatorMassFlowRate;
    this->Report.CondOutletTempSimul = result.condenserOutletTemp;
    this->Report.CondInletTempSimul = result.condenserInletTemp;
    this->Report.CondmdotSimul = result.condenserMassFlowRate;
}

void ChillerHeaterSpecs::updateResultEnergies(Real64 const secondsInTimeStep, bool const updateSimultaneousResult)
{
    auto update = [secondsInTimeStep](ChillerHeaterResult &result) {
        result.falseLoadEnergy = result.falseLoadRate * secondsInTimeStep;
        result.coolingEnergy = result.coolingPower * secondsInTimeStep;
        result.heatingEnergy = result.heatingPower * secondsInTimeStep;
        result.evaporatorEnergy = result.qEvaporator * secondsInTimeStep;
        result.condenserEnergy = result.qCondenser * secondsInTimeStep;
    };

    update(this->Result);
    this->syncLegacyReportAndNodes();
    if (updateSimultaneousResult) {
        update(this->SimulResult);
        this->Report.ChillerFalseLoadSimul = this->SimulResult.falseLoadEnergy;
        this->Report.CoolingEnergySimul = this->SimulResult.coolingEnergy;
        this->Report.EvapEnergySimul = this->SimulResult.evaporatorEnergy;
        this->Report.CondEnergySimul = this->SimulResult.condenserEnergy;
    }
}

void ChillerHeaterSpecs::resetCurrentResult(Real64 const evaporatorInletTemp, Real64 const condenserInletTemp)
{
    this->Result = ChillerHeaterResult();
    this->Result.evaporatorInletTemp = evaporatorInletTemp;
    this->Result.evaporatorOutletTemp = evaporatorInletTemp;
    this->Result.condenserInletTemp = condenserInletTemp;
    this->Result.condenserOutletTemp = condenserInletTemp;
    this->mapResultToPlantConnections();
    this->syncLegacyReportAndNodes();
}

void ChillerHeaterSpecs::resetAllResults(Real64 const evaporatorInletTemp, Real64 const condenserInletTemp)
{
    this->Report = CHReportVars();
    this->resetCurrentResult(evaporatorInletTemp, condenserInletTemp);
    this->saveCurrentResultForSimultaneous();
}

PlantComponent *WrapperSpecs::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataPlantCentralGSHP->getWrapperInputFlag) {
        GetWrapperInput(state);
        state.dataPlantCentralGSHP->getWrapperInputFlag = false;
    }

    // Now look for this particular object
    for (auto &thisWrapper : state.dataPlantCentralGSHP->Wrapper) {
        if (thisWrapper.Name == objectName) {
            return &thisWrapper;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, std::format("LocalPlantCentralGSHPFactory: Error getting inputs for object named: {}", objectName)); // LCOV_EXCL_LINE
}

void WrapperSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    this->initialize(state, 0.0, calledFromLocation.loopNum, false);
    this->SizeWrapper(state);
}

void WrapperSpecs::getDesignCapacities(
    [[maybe_unused]] EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    MinLoad = 0.0;
    MaxLoad = 0.0;
    OptLoad = 0.0;
    Real64 minimumStageLoad = std::numeric_limits<Real64>::max();
    auto accumulateCapacity = [&minimumStageLoad](Real64 const referenceCapacity,
                                                  Real64 const minimumPartLoadRatio,
                                                  Real64 const maximumPartLoadRatio,
                                                  Real64 const optimumPartLoadRatio,
                                                  Real64 &maximumLoad,
                                                  Real64 &optimumLoad) {
        if (referenceCapacity <= 0.0) {
            return;
        }
        maximumLoad += referenceCapacity * max(0.0, maximumPartLoadRatio);
        optimumLoad += referenceCapacity * max(0.0, optimumPartLoadRatio);
        minimumStageLoad = min(minimumStageLoad, referenceCapacity * max(0.0, minimumPartLoadRatio));
    };

    if (calledFromLocation.loopNum == this->CWPlantLoc.loopNum) { // Chilled-water useful cooling
        for (auto const &chillerHeater : this->ChillerHeater) {
            accumulateCapacity(chillerHeater.RefCapCooling,
                               chillerHeater.MinPartLoadRatCooling,
                               chillerHeater.MaxPartLoadRatCooling,
                               chillerHeater.OptPartLoadRatCooling,
                               MaxLoad,
                               OptLoad);
        }
    } else if (calledFromLocation.loopNum == this->HWPlantLoc.loopNum) { // Hot-water useful heating
        for (auto const &chillerHeater : this->ChillerHeater) {
            Real64 const referenceHeatingCapacity = chillerHeater.RefCapClgHtg + max(0.0, chillerHeater.RefPowerClgHtg) * chillerHeater.OpenMotorEff;
            accumulateCapacity(referenceHeatingCapacity,
                               chillerHeater.MinPartLoadRatClgHtg,
                               chillerHeater.MaxPartLoadRatClgHtg,
                               chillerHeater.OptPartLoadRatClgHtg,
                               MaxLoad,
                               OptLoad);
        }
    } else if (calledFromLocation.loopNum == this->GLHEPlantLoc.loopNum) { // Absolute source rejection/extraction envelope
        Real64 maximumSourceRejection = 0.0;
        Real64 optimumSourceRejection = 0.0;
        Real64 maximumSourceExtraction = 0.0;
        Real64 optimumSourceExtraction = 0.0;
        for (auto const &chillerHeater : this->ChillerHeater) {
            Real64 const referenceCoolingPower = chillerHeater.RefCOPCooling > 0.0 ? chillerHeater.RefCapCooling / chillerHeater.RefCOPCooling : 0.0;
            Real64 const referenceSourceRejection = chillerHeater.RefCapCooling + referenceCoolingPower * chillerHeater.OpenMotorEff;
            accumulateCapacity(referenceSourceRejection,
                               chillerHeater.MinPartLoadRatCooling,
                               chillerHeater.MaxPartLoadRatCooling,
                               chillerHeater.OptPartLoadRatCooling,
                               maximumSourceRejection,
                               optimumSourceRejection);
            accumulateCapacity(chillerHeater.RefCapClgHtg,
                               chillerHeater.MinPartLoadRatClgHtg,
                               chillerHeater.MaxPartLoadRatClgHtg,
                               chillerHeater.OptPartLoadRatClgHtg,
                               maximumSourceExtraction,
                               optimumSourceExtraction);
        }
        MaxLoad = max(maximumSourceRejection, maximumSourceExtraction);
        OptLoad = max(optimumSourceRejection, optimumSourceExtraction);
    }

    if (minimumStageLoad < std::numeric_limits<Real64>::max()) {
        MinLoad = minimumStageLoad;
    }
}

void WrapperSpecs::getSizingFactor(Real64 &SizFac)
{
    SizFac = 1.0;
}

void WrapperSpecs::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool const RunFlag)
{
    if (calledFromLocation.loopNum != this->GLHEPlantLoc.loopNum) {

        this->initialize(state, CurLoad, calledFromLocation.loopNum, RunFlag);
        if (!RunFlag) {
            CurLoad = 0.0;
            if (calledFromLocation.loopNum == this->CWPlantLoc.loopNum) {
                this->WrapperCoolingLoad = 0.0;
            } else if (calledFromLocation.loopNum == this->HWPlantLoc.loopNum) {
                this->WrapperHeatingLoad = 0.0;
            }
            if (this->WrapperCoolingLoad <= HVAC::SmallLoad && this->WrapperHeatingLoad <= HVAC::SmallLoad) {
                this->resetOffState(state);
            }
            return;
        }
        this->CalcWrapperModel(state, CurLoad, calledFromLocation.loopNum);

    } else if (calledFromLocation.loopNum == this->GLHEPlantLoc.loopNum) {
        // Useful-load callbacks own the wrapper run state. The source callback only publishes the
        // authoritative load-side result and must not erase an active cooling or heating request.
        PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                            calledFromLocation.loopNum,
                                                            this->GLHEPlantLoc.loopSideNum,
                                                            DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump,
                                                            this->GLHEInletNodeNum,
                                                            this->GLHEOutletNodeNum,
                                                            this->Report.GLHERate,
                                                            this->Report.GLHEInletTemp,
                                                            this->Report.GLHEOutletTemp,
                                                            this->Report.GLHEmdot,
                                                            FirstHVACIteration);
    }
}

void WrapperSpecs::SizeWrapper(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Yunzhi Huang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine is for sizing all the components under each 'CentralHeatPumpSystem' object,
    //  for which capacities and flow rates have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    //  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
    //  the evaporator (or load side) flow rate and the chilled water loop design delta T. The condenser
    //  flow (or source side) rate is calculated from the reference capacity, the COP, and the condenser
    //  loop design delta T.

    static constexpr std::string_view RoutineName("SizeCGSHPChillerHeater");

    // auto-size the chiller heater components

    for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
        bool ErrorsFound = false;

        // find the appropriate Plant Sizing objects
        int PltSizNum = this->CWPlantLoc.loop->PlantSizNum;
        int PltSizCondNum = this->GLHEPlantLoc.loop->PlantSizNum;

        auto &chillerHeater = this->ChillerHeater(NumChillerHeater);

        Real64 sizingNomCap = chillerHeater.RefCapCooling;
        Real64 sizingEvapVolFlowRate = chillerHeater.EvapVolFlowRate;
        Real64 sizingCondVolFlowRate = chillerHeater.CondVolFlowRate;

        // auto-size the Evaporator Flow Rate
        if (PltSizNum > 0) {
            Real64 designEvapVolFlowRate = 0.0;
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= HVAC::SmallWaterVolFlow) {
                designEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * chillerHeater.SizFac;
            }
            sizingEvapVolFlowRate = chillerHeater.EvapVolFlowRateWasAutoSized ? designEvapVolFlowRate : chillerHeater.EvapVolFlowRate;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (chillerHeater.EvapVolFlowRateWasAutoSized) {
                    chillerHeater.EvapVolFlowRate = designEvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     chillerHeater.Name,
                                                     "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                     designEvapVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     chillerHeater.Name,
                                                     "Initial Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                     designEvapVolFlowRate);
                    }
                } else if (chillerHeater.EvapVolFlowRate > 0.0 && designEvapVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                           !this->mySizesReported) {
                    Real64 const evapVolFlowRateUser = chillerHeater.EvapVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                 designEvapVolFlowRate,
                                                 "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                 evapVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings &&
                        (std::abs(designEvapVolFlowRate - evapVolFlowRateUser) / evapVolFlowRateUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(
                            state,
                            std::format("SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for {}", chillerHeater.Name));
                        ShowContinueError(state,
                                          std::format("User-Specified Reference Chilled Water Flow Rate of {:.5f} [m3/s]", evapVolFlowRateUser));
                        ShowContinueError(
                            state, std::format("differs from Design Size Reference Chilled Water Flow Rate of {:.5f} [m3/s]", designEvapVolFlowRate));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        } else {
            if (chillerHeater.EvapVolFlowRateWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of CGSHP Chiller Heater evap flow rate requires a loop Sizing:Plant object");
                    ShowContinueError(state, std::format("Occurs in CGSHP Chiller Heater Performance object={}", chillerHeater.Name));
                    ErrorsFound = true;
                }
            } else {
                if (chillerHeater.EvapVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                 chillerHeater.EvapVolFlowRate);
                }
            }
        }
        chillerHeater.tmpEvapVolFlowRate = max(0.0, sizingEvapVolFlowRate);

        // auto-size the Reference Cooling Capacity
        // each individual chiller heater module is sized to be capable of supporting the total load on the wrapper
        if (PltSizNum > 0) {
            Real64 designNomCap = 0.0;
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= HVAC::SmallWaterVolFlow && sizingEvapVolFlowRate > 0.0) {
                Real64 const cp = this->CWPlantLoc.loop->glycol->getSpecificHeat(state, Constant::CWInitConvTemp, RoutineName);
                Real64 const rho = this->CWPlantLoc.loop->glycol->getDensity(state, Constant::CWInitConvTemp, RoutineName);
                designNomCap = cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * sizingEvapVolFlowRate;
            }
            sizingNomCap = chillerHeater.RefCapCoolingWasAutoSized ? designNomCap : chillerHeater.RefCapCooling;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (chillerHeater.RefCapCoolingWasAutoSized) {
                    chillerHeater.RefCapCooling = designNomCap;

                    // Now that we have the Reference Cooling Capacity, we need to also initialize the Heating side
                    // given the ratios
                    chillerHeater.RefCapClgHtg = chillerHeater.RefCapCooling * chillerHeater.ClgHtgToCoolingCapRatio;

                    chillerHeater.RefPowerClgHtg = (chillerHeater.RefCapCooling / chillerHeater.RefCOPCooling) * chillerHeater.ClgHtgtoCogPowerRatio;

                    chillerHeater.RefCOPClgHtg = chillerHeater.RefCapClgHtg / chillerHeater.RefPowerClgHtg;

                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(
                            state, "ChillerHeaterPerformance:Electric:EIR", chillerHeater.Name, "Design Size Reference Capacity [W]", designNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     chillerHeater.Name,
                                                     "Initial Design Size Reference Capacity [W]",
                                                     designNomCap);
                    }
                } else if (chillerHeater.RefCapCooling > 0.0 && designNomCap > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                           !this->mySizesReported) {
                    Real64 const nomCapUser = chillerHeater.RefCapCooling;
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "Design Size Reference Capacity [W]",
                                                 designNomCap,
                                                 "User-Specified Reference Capacity [W]",
                                                 nomCapUser);
                    if (state.dataGlobal->DisplayExtraWarnings &&
                        (std::abs(designNomCap - nomCapUser) / nomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(
                            state,
                            std::format("SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for {}", chillerHeater.Name));
                        ShowContinueError(state, std::format("User-Specified Reference Capacity of {:.2f} [W]", nomCapUser));
                        ShowContinueError(state, std::format("differs from Design Size Reference Capacity of {:.2f} [W]", designNomCap));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        } else {
            if (chillerHeater.RefCapCoolingWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, std::format("Size ChillerHeaterPerformance:Electric:EIR=\"{}\", autosize error.", chillerHeater.Name));
                    ShowContinueError(state, "Autosizing of CGSHP Chiller Heater reference capacity requires");
                    ShowContinueError(state, "a cooling loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (chillerHeater.RefCapCooling > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "User-Specified Reference Capacity [W]",
                                                 chillerHeater.RefCapCooling);
                }
            }
        }

        // auto-size the condenser volume flow rate
        // each individual chiller heater module is sized to be capable of supporting the total load on the wrapper
        if (PltSizCondNum > 0) {
            auto const &sourceSizing = state.dataSize->PlantSizData(PltSizCondNum);
            Real64 designCondVolFlowRate = 0.0;
            if (sourceSizing.DesVolFlowRate >= HVAC::SmallWaterVolFlow && sizingNomCap > 0.0 && sourceSizing.DeltaT > 0.0 &&
                chillerHeater.RefCOPCooling > 0.0) {
                Real64 const rho = this->GLHEPlantLoc.loop->glycol->getDensity(state, chillerHeater.TempRefCondInCooling, RoutineName);
                Real64 const cp = this->GLHEPlantLoc.loop->glycol->getSpecificHeat(state, chillerHeater.TempRefCondInCooling, RoutineName);
                designCondVolFlowRate =
                    sizingNomCap * (1.0 + chillerHeater.OpenMotorEff / chillerHeater.RefCOPCooling) / (sourceSizing.DeltaT * cp * rho);
            }
            sizingCondVolFlowRate = chillerHeater.CondVolFlowRateWasAutoSized ? designCondVolFlowRate : chillerHeater.CondVolFlowRate;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (chillerHeater.CondVolFlowRateWasAutoSized) {
                    chillerHeater.CondVolFlowRate = designCondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     chillerHeater.Name,
                                                     "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                     designCondVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     chillerHeater.Name,
                                                     "Initial Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                     designCondVolFlowRate);
                    }
                } else if (chillerHeater.CondVolFlowRate > 0.0 && designCondVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                           !this->mySizesReported) {
                    Real64 const condVolFlowRateUser = chillerHeater.CondVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                 designCondVolFlowRate,
                                                 "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                 condVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings &&
                        (std::abs(designCondVolFlowRate - condVolFlowRateUser) / condVolFlowRateUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(
                            state,
                            std::format("SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for {}", chillerHeater.Name));
                        ShowContinueError(state,
                                          std::format("User-Specified Reference Condenser Water Flow Rate of {:.5f} [m3/s]", condVolFlowRateUser));
                        ShowContinueError(
                            state,
                            std::format("differs from Design Size Reference Condenser Water Flow Rate of {:.5f} [m3/s]", designCondVolFlowRate));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        } else {
            if (chillerHeater.CondVolFlowRateWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, std::format("Size ChillerHeaterPerformance:Electric:EIR=\"{}\", autosize error.", chillerHeater.Name));
                    ShowContinueError(state, "Autosizing of CGSHP Chiller Heater condenser flow rate requires");
                    ShowContinueError(state, "a condenser loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (chillerHeater.CondVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeaterPerformance:Electric:EIR",
                                                 chillerHeater.Name,
                                                 "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                 chillerHeater.CondVolFlowRate);
                }
            }
        }
        chillerHeater.tmpCondVolFlowRate = max(0.0, sizingCondVolFlowRate);

        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
            // create predefined report
            std::string equipName = chillerHeater.Name;
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchMechType, equipName, "ChillerHeaterPerformance:Electric:EIR");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, equipName, chillerHeater.RefCOPCooling);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, equipName, chillerHeater.RefCapCooling);
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    // sum individual volume flows and register wrapper inlets
    Real64 TotalEvapVolFlowRate = 0.0;
    Real64 TotalSourceVolFlowRate = 0.0;
    Real64 TotalHotWaterVolFlowRate = 0.0;
    for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
        auto const &chillerHeater = this->ChillerHeater(NumChillerHeater);
        TotalEvapVolFlowRate += chillerHeater.tmpEvapVolFlowRate;
        TotalSourceVolFlowRate += max(chillerHeater.tmpEvapVolFlowRate, chillerHeater.tmpCondVolFlowRate);
        TotalHotWaterVolFlowRate += chillerHeater.DesignHotWaterVolFlowRate;
    }

    PlantUtilities::RegisterPlantCompDesignFlow(state, this->CHWInletNodeNum, TotalEvapVolFlowRate);
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->HWInletNodeNum, TotalHotWaterVolFlowRate);
    // Source extraction uses evaporator flow and source rejection uses condenser flow; only one direction is active per module.
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->GLHEInletNodeNum, TotalSourceVolFlowRate);

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        this->mySizesReported = true;
    }

    return;
}

void WrapperSpecs::resolveFlowMode(EnergyPlusData &state)
{
    bool hasConstantFlow = false;
    bool hasVariableFlow = false;
    for (auto const &chillerHeater : this->ChillerHeater) {
        hasConstantFlow = hasConstantFlow || chillerHeater.ConstantFlow || !chillerHeater.VariableFlow;
        hasVariableFlow = hasVariableFlow || chillerHeater.VariableFlow;
    }

    bool const mixedFlowModes = hasConstantFlow && hasVariableFlow;
    this->VariableFlowCH = hasVariableFlow && !hasConstantFlow;
    if (mixedFlowModes) {
        ShowWarningError(state, std::format("CentralHeatPumpSystem={} contains both ConstantFlow and VariableFlow performance objects.", this->Name));
        ShowContinueError(state, "All modules in this CentralHeatPumpSystem will use ConstantFlow.");
    }

    for (auto &chillerHeater : this->ChillerHeater) {
        if (mixedFlowModes || !this->VariableFlowCH) {
            chillerHeater.ConstantFlow = true;
            chillerHeater.VariableFlow = false;
        } else {
            chillerHeater.ConstantFlow = false;
            chillerHeater.VariableFlow = true;
        }
    }
}

void GetWrapperInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Yunzhi Huang and Daeho Kang, PNNL
    //       DATE WRITTEN:    Feb 2013

    // PURPOSE OF THIS SUBROUTINE:
    //  This routine will get the input required by the Wrapper model.

    static constexpr std::string_view routineName = "GetWrapperInput";

    bool ErrorsFound(false); // True when input errors are found
    int NumAlphas;           // Number of elements in the alpha array
    int NumNums;             // Number of elements in the numeric array
    int IOStat;              // IO Status when calling get input subroutine

    state.dataIPShortCut->cCurrentModuleObject = "CentralHeatPumpSystem";
    state.dataPlantCentralGSHP->numWrappers =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

    if (state.dataPlantCentralGSHP->numWrappers <= 0) {
        ShowSevereError(state, std::format("No {} equipment specified in input file", state.dataIPShortCut->cCurrentModuleObject));
    }

    state.dataPlantCentralGSHP->Wrapper.allocate(state.dataPlantCentralGSHP->numWrappers);

    // Load arrays with electric EIR chiller data
    for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 WrapperNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)};

        auto &wrapper = state.dataPlantCentralGSHP->Wrapper(WrapperNum);

        wrapper.Name = state.dataIPShortCut->cAlphaArgs(1);

        wrapper.CHWInletNodeNum = Node::GetOnlySingleNode(state,
                                                          state.dataIPShortCut->cAlphaArgs(2),
                                                          ErrorsFound,
                                                          Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                          state.dataIPShortCut->cAlphaArgs(1),
                                                          Node::FluidType::Water,
                                                          Node::ConnectionType::Inlet,
                                                          Node::CompFluidStream::Primary,
                                                          Node::ObjectIsNotParent); // node name : connection should be careful!
        wrapper.CHWOutletNodeNum = Node::GetOnlySingleNode(state,
                                                           state.dataIPShortCut->cAlphaArgs(3),
                                                           ErrorsFound,
                                                           Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                           Node::FluidType::Water,
                                                           Node::ConnectionType::Outlet,
                                                           Node::CompFluidStream::Primary,
                                                           Node::ObjectIsNotParent);
        wrapper.CoolSetPointTempNode = wrapper.CHWOutletNodeNum;
        Node::TestCompSet(state,
                          state.dataIPShortCut->cCurrentModuleObject,
                          state.dataIPShortCut->cAlphaArgs(1),
                          state.dataIPShortCut->cAlphaArgs(2),
                          state.dataIPShortCut->cAlphaArgs(3),
                          "Chilled Water Nodes");

        wrapper.GLHEInletNodeNum = Node::GetOnlySingleNode(state,
                                                           state.dataIPShortCut->cAlphaArgs(4),
                                                           ErrorsFound,
                                                           Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                           Node::FluidType::Water,
                                                           Node::ConnectionType::Inlet,
                                                           Node::CompFluidStream::Secondary,
                                                           Node::ObjectIsNotParent); // node name : connection should be careful!
        wrapper.GLHEOutletNodeNum = Node::GetOnlySingleNode(state,
                                                            state.dataIPShortCut->cAlphaArgs(5),
                                                            ErrorsFound,
                                                            Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                            Node::FluidType::Water,
                                                            Node::ConnectionType::Outlet,
                                                            Node::CompFluidStream::Secondary,
                                                            Node::ObjectIsNotParent);
        Node::TestCompSet(state,
                          state.dataIPShortCut->cCurrentModuleObject,
                          state.dataIPShortCut->cAlphaArgs(1),
                          state.dataIPShortCut->cAlphaArgs(4),
                          state.dataIPShortCut->cAlphaArgs(5),
                          "GLHE Nodes");

        wrapper.HWInletNodeNum = Node::GetOnlySingleNode(state,
                                                         state.dataIPShortCut->cAlphaArgs(6),
                                                         ErrorsFound,
                                                         Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                         Node::FluidType::Water,
                                                         Node::ConnectionType::Inlet,
                                                         Node::CompFluidStream::Tertiary,
                                                         Node::ObjectIsNotParent); // node name : connection should be careful!
        wrapper.HWOutletNodeNum = Node::GetOnlySingleNode(state,
                                                          state.dataIPShortCut->cAlphaArgs(7),
                                                          ErrorsFound,
                                                          Node::ConnectionObjectType::CentralHeatPumpSystem,
                                                          state.dataIPShortCut->cAlphaArgs(1),
                                                          Node::FluidType::Water,
                                                          Node::ConnectionType::Outlet,
                                                          Node::CompFluidStream::Tertiary,
                                                          Node::ObjectIsNotParent);
        wrapper.HeatSetPointTempNode = wrapper.HWOutletNodeNum;
        Node::TestCompSet(state,
                          state.dataIPShortCut->cCurrentModuleObject,
                          state.dataIPShortCut->cAlphaArgs(1),
                          state.dataIPShortCut->cAlphaArgs(6),
                          state.dataIPShortCut->cAlphaArgs(7),
                          "Hot Water Nodes");

        wrapper.AncillaryPower = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            wrapper.ancillaryPowerSched = Sched::GetScheduleAlwaysOn(state);
        } else if ((wrapper.ancillaryPowerSched = Sched::GetSchedule(state, state.dataIPShortCut->cAlphaArgs(8))) == nullptr) {
            ShowSevereItemNotFound(state, eoh, state.dataIPShortCut->cAlphaFieldNames(8), state.dataIPShortCut->cAlphaArgs(8));
            ErrorsFound = true;
        }

        int NumberOfComp = (NumAlphas - 8) / 3;
        wrapper.NumOfComp = NumberOfComp;
        wrapper.WrapperComp.allocate(NumberOfComp);

        if (wrapper.NumOfComp == 0) {
            ShowSevereError(state,
                            std::format("GetWrapperInput: No component names on {}={}", state.dataIPShortCut->cCurrentModuleObject, wrapper.Name));
            ErrorsFound = true;
        } else {
            int Comp = 0;
            int NumChHtrPerWrapper = 0;
            for (int loop = 9; loop <= NumAlphas; loop += 3) {
                ++Comp;
                wrapper.WrapperComp(Comp).WrapperPerformanceObjectType = state.dataIPShortCut->cAlphaArgs(loop);
                wrapper.WrapperComp(Comp).WrapperComponentName = state.dataIPShortCut->cAlphaArgs(loop + 1);

                if (state.dataIPShortCut->lAlphaFieldBlanks(loop + 2)) {
                    wrapper.WrapperComp(Comp).chSched =
                        Sched::GetScheduleAlwaysOn(state); // Not an availability schedule, but defaults to constant-1.0
                } else if ((wrapper.WrapperComp(Comp).chSched = Sched::GetSchedule(state, state.dataIPShortCut->cAlphaArgs(loop + 2))) == nullptr) {
                    wrapper.WrapperComp(Comp).chSched =
                        Sched::GetScheduleAlwaysOn(state); // Not an availability schedule, but defaults to constant-1.0
                    ShowWarningItemNotFound(state,
                                            eoh,
                                            state.dataIPShortCut->cAlphaFieldNames(loop + 2),
                                            state.dataIPShortCut->cAlphaArgs(loop + 2),
                                            "The Control Schedule is treated as AlwaysOn instead.");
                }

                wrapper.WrapperComp(Comp).WrapperIdenticalObjectNum = state.dataIPShortCut->rNumericArgs(1 + Comp);
                if (wrapper.WrapperComp(Comp).WrapperPerformanceObjectType == "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR") {

                    // count number of chiller heaters (including identical units) for current wrapper
                    if (wrapper.WrapperComp(Comp).WrapperIdenticalObjectNum > 1) {
                        NumChHtrPerWrapper += wrapper.WrapperComp(Comp).WrapperIdenticalObjectNum;
                    } else {
                        ++NumChHtrPerWrapper;
                    }

                    // count total number of chiller heaters (not including identical units) for ALL wrappers
                    ++state.dataPlantCentralGSHP->numChillerHeaters;
                }
            }

            wrapper.ChillerHeaterNums = NumChHtrPerWrapper;
        }

        if (ErrorsFound) {
            ShowFatalError(state,
                           std::format("GetWrapperInput: Invalid {} Input, preceding condition(s) cause termination.",
                                       state.dataIPShortCut->cCurrentModuleObject));
        }

        // ALLOCATE ARRAYS
        if (state.dataPlantCentralGSHP->numChillerHeaters == 0) {
            ShowFatalError(state,
                           std::format("{} : {} requires ChillerHeaterPerformance:Electric:EIR object(s).",
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       wrapper.Name));
        }
    }

    if (state.dataPlantCentralGSHP->numChillerHeaters > 0) {

        for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
            auto &wrapper = state.dataPlantCentralGSHP->Wrapper(WrapperNum);
            wrapper.ChillerHeater.allocate(wrapper.ChillerHeaterNums);
        }
        GetChillerHeaterInput(state);
    }

    for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
        auto &wrapper = state.dataPlantCentralGSHP->Wrapper(WrapperNum);
        int ChillerHeaterNum = 0; // initialize nth chiller heater index (including identical units) for current wrapper
        for (int Comp = 1; Comp <= wrapper.NumOfComp; ++Comp) {
            if (wrapper.WrapperComp(Comp).WrapperPerformanceObjectType == "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR") {
                std::string CompName = wrapper.WrapperComp(Comp).WrapperComponentName;
                int CompIndex = Util::FindItemInList(CompName, state.dataPlantCentralGSHP->ChillerHeater);
                // User may enter invalid name rather than selecting one from the object list
                if (CompIndex <= 0) {
                    ShowSevereError(state, std::format("GetWrapperInput: Invalid Chiller Heater Modules Performance Component Name ={}", CompName));
                    ShowContinueError(state, "Select the name of ChillerHeaterPerformance:Electric:EIR object(s) from the object list.");
                    ShowFatalError(state, "Program terminates due to preceding condition.");
                }
                wrapper.WrapperComp(Comp).WrapperPerformanceObjectIndex = CompIndex;
                for (int i_CH = 1; i_CH <= wrapper.WrapperComp(Comp).WrapperIdenticalObjectNum; ++i_CH) {
                    // increment nth chiller heater index (including identical units) for current wrapper
                    ++ChillerHeaterNum;
                    wrapper.ChillerHeater(ChillerHeaterNum) = state.dataPlantCentralGSHP->ChillerHeater(CompIndex);
                }
            }
        }
        wrapper.resolveFlowMode(state);
    }

    // Release memory from temporary arrays; values now copied into their associated Wrapper in above loop
    if (allocated(state.dataPlantCentralGSHP->ChillerHeater)) {
        state.dataPlantCentralGSHP->ChillerHeater.deallocate();
    }
}

void WrapperSpecs::setupOutputVars(EnergyPlusData &state)
{
    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Electricity Energy",
                        Constant::Units::J,
                        this->Report.TotElecCooling,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Sum,
                        this->Name,
                        Constant::eResource::Electricity,
                        OutputProcessor::Group::Plant,
                        OutputProcessor::EndUseCat::Cooling);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Electricity Energy",
                        Constant::Units::J,
                        this->Report.TotElecHeating,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Sum,
                        this->Name,
                        Constant::eResource::Electricity,
                        OutputProcessor::Group::Plant,
                        OutputProcessor::EndUseCat::Heating);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Electricity Rate",
                        Constant::Units::W,
                        this->Report.TotElecCoolingPwr,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Electricity Rate",
                        Constant::Units::W,
                        this->Report.TotElecHeatingPwr,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Energy",
                        Constant::Units::J,
                        this->Report.CoolingEnergy,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Sum,
                        this->Name,
                        Constant::eResource::EnergyTransfer,
                        OutputProcessor::Group::Plant,
                        OutputProcessor::EndUseCat::Chillers);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Energy",
                        Constant::Units::J,
                        this->Report.HeatingEnergy,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Sum,
                        this->Name,
                        Constant::eResource::EnergyTransfer,
                        OutputProcessor::Group::Plant,
                        OutputProcessor::EndUseCat::Boilers);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Heat Transfer Energy",
                        Constant::Units::J,
                        this->Report.GLHEEnergy,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Sum,
                        this->Name,
                        Constant::eResource::EnergyTransfer,
                        OutputProcessor::Group::Plant,
                        OutputProcessor::EndUseCat::HeatRejection);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Rate",
                        Constant::Units::W,
                        this->Report.CoolingRate,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Rate",
                        Constant::Units::W,
                        this->Report.HeatingRate,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Heat Transfer Rate",
                        Constant::Units::W,
                        this->Report.GLHERate,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Mass Flow Rate",
                        Constant::Units::kg_s,
                        this->Report.CHWmdot,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Mass Flow Rate",
                        Constant::Units::kg_s,
                        this->Report.HWmdot,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Mass Flow Rate",
                        Constant::Units::kg_s,
                        this->Report.GLHEmdot,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Inlet Temperature",
                        Constant::Units::C,
                        this->Report.CHWInletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Inlet Temperature",
                        Constant::Units::C,
                        this->Report.HWInletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Inlet Temperature",
                        Constant::Units::C,
                        this->Report.GLHEInletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Outlet Temperature",
                        Constant::Units::C,
                        this->Report.CHWOutletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Outlet Temperature",
                        Constant::Units::C,
                        this->Report.HWOutletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Outlet Temperature",
                        Constant::Units::C,
                        this->Report.GLHEOutletTemp,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        this->Name);

    if (this->ChillerHeaterNums > 0) {

        for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
            auto &chillerHeater = this->ChillerHeater(ChillerHeaterNum);
            SetupOutputVariable(state,
                                std::format("Chiller Heater Operation Mode Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.currentMode,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Part Load Ratio Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.ChillerPartLoadRatio,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Cycling Ratio Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.ChillerCyclingRatio,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Cooling Electricity Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::W,
                                chillerHeater.Report.CoolingPower,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Heating Electricity Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::W,
                                chillerHeater.Report.HeatingPower,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Cooling Electricity Energy Unit {}", ChillerHeaterNum),
                                Constant::Units::J,
                                chillerHeater.Report.CoolingEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Heating Electricity Energy Unit {}", ChillerHeaterNum),
                                Constant::Units::J,
                                chillerHeater.Report.HeatingEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Cooling Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::W,
                                chillerHeater.Report.QEvap,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Cooling Energy Unit {}", ChillerHeaterNum),
                                Constant::Units::J,
                                chillerHeater.Report.EvapEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater False Load Heat Transfer Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::W,
                                chillerHeater.Report.ChillerFalseLoadRate,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater False Load Heat Transfer Energy Unit {}", ChillerHeaterNum),
                                Constant::Units::J,
                                chillerHeater.Report.ChillerFalseLoad,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Evaporator Inlet Temperature Unit {}", ChillerHeaterNum),
                                Constant::Units::C,
                                chillerHeater.Report.EvapInletTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Evaporator Outlet Temperature Unit {}", ChillerHeaterNum),
                                Constant::Units::C,
                                chillerHeater.Report.EvapOutletTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Evaporator Mass Flow Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::kg_s,
                                chillerHeater.Report.Evapmdot,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Condenser Heat Transfer Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::W,
                                chillerHeater.Report.QCond,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Condenser Heat Transfer Energy Unit {}", ChillerHeaterNum),
                                Constant::Units::J,
                                chillerHeater.Report.CondEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater COP Unit {}", ChillerHeaterNum),
                                Constant::Units::W_W,
                                chillerHeater.Report.ActualCOP,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Capacity Temperature Modifier Multiplier Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.ChillerCapFT,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater EIR Temperature Modifier Multiplier Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.ChillerEIRFT,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater EIR Part Load Modifier Multiplier Unit {}", ChillerHeaterNum),
                                Constant::Units::None,
                                chillerHeater.Report.ChillerEIRFPLR,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Condenser Inlet Temperature Unit {}", ChillerHeaterNum),
                                Constant::Units::C,
                                chillerHeater.Report.CondInletTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Condenser Outlet Temperature Unit {}", ChillerHeaterNum),
                                Constant::Units::C,
                                chillerHeater.Report.CondOutletTemp,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);

            SetupOutputVariable(state,
                                std::format("Chiller Heater Condenser Mass Flow Rate Unit {}", ChillerHeaterNum),
                                Constant::Units::kg_s,
                                chillerHeater.Report.Condmdot,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                chillerHeater.Name);
        } // End of individual chiller heater count for current wrapper

    } // End of individual chiller heater output
}

void GetChillerHeaterInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Kyung Tae Yun, Mississippi State University
    //       DATE WRITTEN:    Feb 2013

    // PURPOSE OF THIS SUBROUTINE:
    //  This routine will get the input required by the ChillerHeaterPerformance:Electric:EIR model.

    bool CHErrorsFound(false);         // True when input errors are found
    int NumAlphas;                     // Number of elements in the alpha array
    int NumNums;                       // Number of elements in the numeric array
    int IOStat;                        // IO Status when calling get input subroutine
    Array1D<Real64> CurveValArray(11); // Used to evaluate PLFFPLR curve objects

    state.dataIPShortCut->cCurrentModuleObject = "ChillerHeaterPerformance:Electric:EIR";
    state.dataPlantCentralGSHP->numChillerHeaters =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

    if (state.dataPlantCentralGSHP->numChillerHeaters <= 0) {
        ShowSevereError(state, std::format("No {} equipment specified in input file", state.dataIPShortCut->cCurrentModuleObject));
        CHErrorsFound = true;
    }

    // Allocate temporary ChillerHeater and ChillerHeaterReport arrays
    if (allocated(state.dataPlantCentralGSHP->ChillerHeater)) {
        state.dataPlantCentralGSHP->ChillerHeater.deallocate();
    }
    state.dataPlantCentralGSHP->ChillerHeater.allocate(state.dataPlantCentralGSHP->numChillerHeaters);

    // Load arrays with electric EIR chiller data
    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= state.dataPlantCentralGSHP->numChillerHeaters; ++ChillerHeaterNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 ChillerHeaterNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).Name = state.dataIPShortCut->cAlphaArgs(1);

        if (Util::SameString(state.dataIPShortCut->cAlphaArgs(3), "LEAVINGCONDENSER")) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeCooling = CondenserModeTemperature::LeavingCondenser;
        } else { // only other option and default value is EnteringCondenser
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeCooling = CondenserModeTemperature::EnteringCondenser;
        }

        // Performance curves
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state, std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(4), state.dataIPShortCut->cAlphaArgs(4)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state, std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(5), state.dataIPShortCut->cAlphaArgs(5)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state, std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(6), state.dataIPShortCut->cAlphaArgs(6)));
            CHErrorsFound = true;
        }

        if (Util::SameString(state.dataIPShortCut->cAlphaArgs(7), "LEAVINGCONDENSER")) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeHeating = CondenserModeTemperature::LeavingCondenser;
        } else { // only other option and default value is EnteringCondenser
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeHeating = CondenserModeTemperature::EnteringCondenser;
        }

        // Performance curves
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(8));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state, std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(8), state.dataIPShortCut->cAlphaArgs(8)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(9));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state, std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(9), state.dataIPShortCut->cAlphaArgs(9)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX =
            Curve::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(10));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX == 0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state,
                              std::format("Entered in {}={}", state.dataIPShortCut->cAlphaFieldNames(10), state.dataIPShortCut->cAlphaArgs(10)));
            CHErrorsFound = true;
        }

        if (state.dataIPShortCut->cAlphaArgs(2) == "CONSTANTFLOW") {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = true;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).VariableFlow = false;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "VARIABLEFLOW") {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = false;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).VariableFlow = true;
        }

        // Chiller rated performance data
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCoolingWasAutoSized = true;
        }
        if (state.dataIPShortCut->rNumericArgs(1) != DataSizing::AutoSize && state.dataIPShortCut->rNumericArgs(1) <= 0.0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(
                state, std::format("Entered in {}={:.2f}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
            CHErrorsFound = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPCooling = state.dataIPShortCut->rNumericArgs(2);
        if (state.dataIPShortCut->rNumericArgs(2) <= 0.0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(
                state, std::format("Entered in {}={:.2f}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling = state.dataIPShortCut->rNumericArgs(3);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInCooling = state.dataIPShortCut->rNumericArgs(4);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondOutCooling = state.dataIPShortCut->rNumericArgs(5);

        // Reference Heating Mode Ratios for Capacity and Power
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgToCoolingCapRatio = state.dataIPShortCut->rNumericArgs(6);
        if (state.dataIPShortCut->rNumericArgs(6) <= 0.0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(
                state, std::format("Entered in {}={:.2f}", state.dataIPShortCut->cNumericFieldNames(6), state.dataIPShortCut->rNumericArgs(6)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgtoCogPowerRatio = state.dataIPShortCut->rNumericArgs(7);
        if (state.dataIPShortCut->rNumericArgs(7) <= 0.0) {
            ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(
                state, std::format("Entered in {}={:.2f}", state.dataIPShortCut->cNumericFieldNames(7), state.dataIPShortCut->rNumericArgs(7)));
            CHErrorsFound = true;
        }

        if (!state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCoolingWasAutoSized &&
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling > 0.0 &&
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPCooling > 0.0 &&
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgToCoolingCapRatio > 0.0 &&
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgtoCogPowerRatio > 0.0) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapClgHtg =
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgToCoolingCapRatio *
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefPowerClgHtg =
                (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling /
                 state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPCooling) *
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgtoCogPowerRatio;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPClgHtg =
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapClgHtg /
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefPowerClgHtg;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg = state.dataIPShortCut->rNumericArgs(8);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg = state.dataIPShortCut->rNumericArgs(9);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInClgHtg = state.dataIPShortCut->rNumericArgs(10);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(11);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(12);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRateWasAutoSized = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRate = state.dataIPShortCut->rNumericArgs(13);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRate == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRateWasAutoSized = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).DesignHotWaterVolFlowRate = state.dataIPShortCut->rNumericArgs(14);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff = state.dataIPShortCut->rNumericArgs(15);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OptPartLoadRatCooling = state.dataIPShortCut->rNumericArgs(16);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OptPartLoadRatClgHtg = state.dataIPShortCut->rNumericArgs(17);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac = state.dataIPShortCut->rNumericArgs(18);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MaxHeatingLeavingCondTempWasBlank = state.dataIPShortCut->lNumericFieldBlanks(19);
        if (!state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MaxHeatingLeavingCondTempWasBlank) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MaxHeatingLeavingCondTemp = state.dataIPShortCut->rNumericArgs(19);
            if (state.dataIPShortCut->rNumericArgs(19) <= state.dataIPShortCut->rNumericArgs(10)) {
                ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(
                    state, std::format("Entered in {}={:.2f}", state.dataIPShortCut->cNumericFieldNames(19), state.dataIPShortCut->rNumericArgs(19)));
                ShowContinueError(state,
                                  std::format("{} must be greater than {}={:.2f}",
                                              state.dataIPShortCut->cNumericFieldNames(19),
                                              state.dataIPShortCut->cNumericFieldNames(10),
                                              state.dataIPShortCut->rNumericArgs(10)));
                CHErrorsFound = true;
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac <= 0.0) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac = 1.0;
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff < 0.0 ||
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff > 1.0) {
            ShowSevereError(
                state,
                std::format("GetChillerHeaterInput: For {}: {}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
            ShowContinueError(state,
                              std::format("{} = {:.3f}", state.dataIPShortCut->cNumericFieldNames(15), state.dataIPShortCut->rNumericArgs(15)));
            ShowContinueError(state, std::format("{} must be greater than or equal to zero", state.dataIPShortCut->cNumericFieldNames(15)));
            ShowContinueError(state, std::format("{} must be less than or equal to one", state.dataIPShortCut->cNumericFieldNames(15)));
            CHErrorsFound = true;
        }

        auto &chillerHeater = state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum);
        Real64 const coolingReferenceCondenserTemp = chillerHeater.CondModeCooling == CondenserModeTemperature::LeavingCondenser
                                                         ? chillerHeater.TempRefCondOutCooling
                                                         : chillerHeater.TempRefCondInCooling;
        Real64 const heatingReferenceCondenserTemp = chillerHeater.CondModeHeating == CondenserModeTemperature::LeavingCondenser
                                                         ? chillerHeater.TempRefCondOutClgHtg
                                                         : chillerHeater.TempRefCondInClgHtg;

        // Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX > 0) {
            Real64 CurveVal = Curve::CurveValue(state,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling,
                                                coolingReferenceCondenserTemp);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Capacity ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX > 0) {
            Real64 CurveVal = Curve::CurveValue(state,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling,
                                                coolingReferenceCondenserTemp);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX > 0) {
            Real64 CurveVal = evaluatePartLoadCurve(
                state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX, coolingReferenceCondenserTemp, 1.0);

            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX > 0) {
            bool FoundNegValue = false;
            for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                Real64 CurveValTmp = evaluatePartLoadCurve(state,
                                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX,
                                                           coolingReferenceCondenserTemp,
                                                           double(CurveCheck / 10.0));
                if (CurveValTmp < 0.0) {
                    FoundNegValue = true;
                }
                CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
            }
            if (FoundNegValue) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve shows negative values ");
                ShowContinueError(state, std::format("for {}= {}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "EIR as a function of PLR curve output at various part-load ratios shown below:");
                ShowContinueError(state, "PLR   =  0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");

                ShowContinueError(state, std::format("Curve Output = {:7.2F}", EnergyPlus::join(CurveValArray, ",")));

                CHErrorsFound = true;
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX > 0) {
            Real64 CurveVal = Curve::CurveValue(state,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg,
                                                heatingReferenceCondenserTemp);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Capacity ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX > 0) {
            Real64 CurveVal = Curve::CurveValue(state,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX,
                                                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg,
                                                heatingReferenceCondenserTemp);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX > 0) {
            Real64 CurveVal = evaluatePartLoadCurve(
                state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX, heatingReferenceCondenserTemp, 1.0);

            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0");
                ShowContinueError(state,
                                  std::format("(+ or - 10%) at reference conditions for {}= {}",
                                              state.dataIPShortCut->cCurrentModuleObject,
                                              state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, std::format("Curve output at reference conditions = {:.3f}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX > 0) {
            bool FoundNegValue = false;
            for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                Real64 CurveValTmp = evaluatePartLoadCurve(state,
                                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX,
                                                           heatingReferenceCondenserTemp,
                                                           double(CurveCheck / 10.0));
                if (CurveValTmp < 0.0) {
                    FoundNegValue = true;
                }
                CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
            }
            if (FoundNegValue) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve shows negative values ");
                ShowContinueError(state, std::format("for {}= {}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "EIR as a function of PLR curve output at various part-load ratios shown below:");
                ShowContinueError(state, "PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");

                ShowContinueError(state, std::format("Curve Output = {:7.2F}", EnergyPlus::join(CurveValArray, ",")));

                CHErrorsFound = true;
            }
        }

        auto validatePartLoadDomain = [&](int const curveIndex,
                                          Real64 const optimumPartLoadRatio,
                                          Real64 &minimumPartLoadRatio,
                                          Real64 &maximumPartLoadRatio,
                                          int const optimumFieldIndex,
                                          int const curveFieldIndex,
                                          std::string_view const mode) {
            if (curveIndex <= 0) {
                return;
            }
            getPartLoadCurveMinMax(state, curveIndex, minimumPartLoadRatio, maximumPartLoadRatio);
            if (minimumPartLoadRatio < 0.0 || minimumPartLoadRatio > 1.0 || maximumPartLoadRatio < 1.0 ||
                maximumPartLoadRatio < minimumPartLoadRatio) {
                ShowSevereError(state,
                                std::format("Invalid {} part-load curve domain for {}={}",
                                            mode,
                                            state.dataIPShortCut->cCurrentModuleObject,
                                            state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state,
                                  std::format("Entered in {}={}",
                                              state.dataIPShortCut->cAlphaFieldNames(curveFieldIndex),
                                              state.dataIPShortCut->cAlphaArgs(curveFieldIndex)));
                ShowContinueError(state,
                                  std::format("Part-load ratio limits [{:.3f}, {:.3f}] must include 1.0 and have a minimum no less than zero.",
                                              minimumPartLoadRatio,
                                              maximumPartLoadRatio));
                CHErrorsFound = true;
            }
            if (optimumPartLoadRatio < minimumPartLoadRatio || optimumPartLoadRatio > maximumPartLoadRatio) {
                ShowSevereError(state, std::format("Invalid {}={}", state.dataIPShortCut->cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(
                    state, std::format("Entered in {}={:.3f}", state.dataIPShortCut->cNumericFieldNames(optimumFieldIndex), optimumPartLoadRatio));
                ShowContinueError(state,
                                  std::format("{} must be within the associated part-load curve limits [{:.3f}, {:.3f}].",
                                              state.dataIPShortCut->cNumericFieldNames(optimumFieldIndex),
                                              minimumPartLoadRatio,
                                              maximumPartLoadRatio));
                CHErrorsFound = true;
            }
        };

        validatePartLoadDomain(chillerHeater.ChillerEIRFPLRCoolingIDX,
                               chillerHeater.OptPartLoadRatCooling,
                               chillerHeater.MinPartLoadRatCooling,
                               chillerHeater.MaxPartLoadRatCooling,
                               16,
                               6,
                               "cooling");
        validatePartLoadDomain(chillerHeater.ChillerEIRFPLRHeatingIDX,
                               chillerHeater.OptPartLoadRatClgHtg,
                               chillerHeater.MinPartLoadRatClgHtg,
                               chillerHeater.MaxPartLoadRatClgHtg,
                               17,
                               10,
                               "heating");
    }

    if (CHErrorsFound) {
        ShowFatalError(state, std::format("Errors found in processing input for {}", state.dataIPShortCut->cCurrentModuleObject));
    }
}

void WrapperSpecs::initializeDesignFlowLimits(EnergyPlusData &state)
{
    static constexpr std::string_view routineName("InitCGSHPHeatPump");

    this->CHWVolFlowRate = 0.0;
    this->HWVolFlowRate = 0.0;
    this->GLHEVolFlowRate = 0.0;

    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto const &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        this->CHWVolFlowRate += chillerHeater.EvapVolFlowRate;
        this->HWVolFlowRate += chillerHeater.DesignHotWaterVolFlowRate;
        this->GLHEVolFlowRate += max(chillerHeater.EvapVolFlowRate, chillerHeater.CondVolFlowRate);
    }

    Real64 const chilledWaterDensity = this->CWPlantLoc.loop->glycol->getDensity(state, Constant::CWInitConvTemp, routineName);
    Real64 const hotWaterDensity = this->HWPlantLoc.loop->glycol->getDensity(state, Constant::HWInitConvTemp, routineName);
    Real64 const sourceDensity = this->GLHEPlantLoc.loop->glycol->getDensity(state, Constant::CWInitConvTemp, routineName);

    this->CHWMassFlowRateMax = this->CHWVolFlowRate * chilledWaterDensity;
    this->HWMassFlowRateMax = this->HWVolFlowRate * hotWaterDensity;
    this->GLHEMassFlowRateMax = this->GLHEVolFlowRate * sourceDensity;

    PlantUtilities::InitComponentNodes(state, 0.0, this->CHWMassFlowRateMax, this->CHWInletNodeNum, this->CHWOutletNodeNum);
    PlantUtilities::InitComponentNodes(state, 0.0, this->HWMassFlowRateMax, this->HWInletNodeNum, this->HWOutletNodeNum);
    PlantUtilities::InitComponentNodes(state, 0.0, this->GLHEMassFlowRateMax, this->GLHEInletNodeNum, this->GLHEOutletNodeNum);

    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        chillerHeater.ChilledWaterMassFlowRateMax = chilledWaterDensity * chillerHeater.EvapVolFlowRate;
        chillerHeater.HotWaterMassFlowRateMax = hotWaterDensity * chillerHeater.DesignHotWaterVolFlowRate;
        chillerHeater.SourceEvapMassFlowRateMax = sourceDensity * chillerHeater.EvapVolFlowRate;
        chillerHeater.SourceCondMassFlowRateMax = sourceDensity * chillerHeater.CondVolFlowRate;
        chillerHeater.EvapMassFlowRateMax = max(chillerHeater.ChilledWaterMassFlowRateMax, chillerHeater.SourceEvapMassFlowRateMax);
        chillerHeater.CondMassFlowRateMax = max(chillerHeater.HotWaterMassFlowRateMax, chillerHeater.SourceCondMassFlowRateMax);
        chillerHeater.EvapInletNode.MassFlowRateMin = 0.0;
        chillerHeater.EvapInletNode.MassFlowRateMinAvail = 0.0;
        chillerHeater.EvapInletNode.MassFlowRateMax = chillerHeater.EvapMassFlowRateMax;
        chillerHeater.EvapInletNode.MassFlowRateMaxAvail = chillerHeater.EvapMassFlowRateMax;
        chillerHeater.EvapInletNode.MassFlowRate = 0.0;
        chillerHeater.CondInletNode.MassFlowRateMin = 0.0;
        chillerHeater.CondInletNode.MassFlowRateMinAvail = 0.0;
        chillerHeater.CondInletNode.MassFlowRateMax = chillerHeater.CondMassFlowRateMax;
        chillerHeater.CondInletNode.MassFlowRateMaxAvail = chillerHeater.CondMassFlowRateMax;
        chillerHeater.CondInletNode.MassFlowRate = 0.0;
        chillerHeater.CondInletNode.MassFlowRateRequest = 0.0;
    }
}

void WrapperSpecs::resetOffState(EnergyPlusData &state, bool const releasePlantFlows)
{
    Real64 const chilledWaterInletTemp = state.dataLoopNodes->Node(this->CHWInletNodeNum).Temp;
    Real64 const hotWaterInletTemp = state.dataLoopNodes->Node(this->HWInletNodeNum).Temp;
    Real64 const sourceInletTemp = state.dataLoopNodes->Node(this->GLHEInletNodeNum).Temp;

    this->WrapperCoolingLoad = 0.0;
    this->WrapperHeatingLoad = 0.0;
    this->SimulClgDominant = false;
    this->SimulHtgDominant = false;

    for (auto &chillerHeater : this->ChillerHeater) {
        chillerHeater.resetAllResults(chilledWaterInletTemp, sourceInletTemp);
    }

    this->Report = WrapperReportVars();
    this->Report.CHWInletTemp = chilledWaterInletTemp;
    this->Report.CHWOutletTemp = chilledWaterInletTemp;
    this->Report.HWInletTemp = hotWaterInletTemp;
    this->Report.HWOutletTemp = hotWaterInletTemp;
    this->Report.GLHEInletTemp = sourceInletTemp;
    this->Report.GLHEOutletTemp = sourceInletTemp;
    this->Report.CHWInletTempSimul = chilledWaterInletTemp;
    this->Report.CHWOutletTempSimul = chilledWaterInletTemp;
    this->Report.GLHEInletTempSimul = sourceInletTemp;
    this->Report.GLHEOutletTempSimul = sourceInletTemp;

    state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = chilledWaterInletTemp;
    state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = hotWaterInletTemp;
    state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = sourceInletTemp;

    auto releaseFlow = [&](PlantLocation const &plantLoc, int const inletNodeNum, int const outletNodeNum) {
        state.dataLoopNodes->Node(inletNodeNum).MassFlowRateRequest = 0.0;
        if (releasePlantFlows && plantLoc.comp != nullptr) {
            Real64 requestedMassFlowRate = 0.0;
            PlantUtilities::SetComponentFlowRate(state, requestedMassFlowRate, inletNodeNum, outletNodeNum, plantLoc);
        }
    };
    releaseFlow(this->CWPlantLoc, this->CHWInletNodeNum, this->CHWOutletNodeNum);
    releaseFlow(this->HWPlantLoc, this->HWInletNodeNum, this->HWOutletNodeNum);
    releaseFlow(this->GLHEPlantLoc, this->GLHEInletNodeNum, this->GLHEOutletNodeNum);
}

void WrapperSpecs::initialize(EnergyPlusData &state,
                              Real64 MyLoad, // Demand Load
                              int LoopNum,   // Loop Number Index
                              bool const RunFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine is for initializations of the CentralHeatPumpSystem variables

    // METHODOLOGY EMPLOYED:
    //  Uses the status flags to trigger initializations.

    if (this->setupOutputVarsFlag) {
        this->setupOutputVars(state);
        this->setupOutputVarsFlag = false;
    }

    if (this->MyWrapperFlag) {
        // Locate the chillers on the plant loops for later usage
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump,
                                                this->CWPlantLoc,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->CHWInletNodeNum,
                                                _);

        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump,
                                                this->HWPlantLoc,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->HWInletNodeNum,
                                                _);

        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump,
                                                this->GLHEPlantLoc,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->GLHEInletNodeNum,
                                                _);

        if (errFlag) {
            ShowFatalError(
                state,
                std::format("InitCGSHPHeatPump: CentralHeatPumpSystem={} could not be located on all three connected plant loops.", this->Name));
        }

        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CWPlantLoc, this->GLHEPlantLoc, DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump, true);

        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->HWPlantLoc, this->GLHEPlantLoc, DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump, true);

        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CWPlantLoc, this->HWPlantLoc, DataPlant::PlantEquipmentType::CentralGroundSourceHeatPump, true);

        if (this->VariableFlowCH) { // why do this only for VS chiller heaters? constant flow also uses set points.
            // Reset flow priority
            if (LoopNum == this->CWPlantLoc.loopNum) {
                DataPlant::CompData::getPlantComponent(state, this->CWPlantLoc).FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
            } else if (LoopNum == this->HWPlantLoc.loopNum) {
                DataPlant::CompData::getPlantComponent(state, this->HWPlantLoc).FlowPriority = DataPlant::LoopFlowStatus::NeedyIfLoopOn;
            }
        } // moved up from below next 2 set point checks for #5808

        // check if setpoint on outlet node - chilled water loop
        if (state.dataLoopNodes->Node(this->CHWOutletNodeNum).TempSetPoint == Node::SensedNodeFlagValue) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                if (!this->CoolSetPointErrDone) {
                    ShowWarningError(state,
                                     std::format("Missing temperature setpoint on cooling side for CentralHeatPumpSystem named {}", this->Name));
                    ShowContinueError(state,
                                      "  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem, use a SetpointManager");
                    ShowContinueError(state, "  The overall loop setpoint will be assumed for CentralHeatPumpSystem. The simulation continues ... ");
                    this->CoolSetPointErrDone = true;
                }
            } else {
                // need call to EMS to check node
                bool FatalError = false; // but not really fatal yet, but should be.
                EMSManager::CheckIfNodeSetPointManagedByEMS(state, this->CHWOutletNodeNum, HVAC::CtrlVarType::Temp, FatalError);
                state.dataLoopNodes->NodeSetpointCheck(this->CHWOutletNodeNum).needsSetpointChecking = false;
                if (FatalError) {
                    if (!this->CoolSetPointErrDone) {
                        ShowWarningError(state,
                                         std::format("Missing temperature setpoint on cooling side for CentralHeatPumpSystem named {}", this->Name));
                        ShowContinueError(state, "A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ");
                        ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the chiller side outlet node ");
                        ShowContinueError(state, "or use an EMS actuator to establish a setpoint at the outlet node ");
                        ShowContinueError(state, "The overall loop setpoint will be assumed for chiller side. The simulation continues ... ");
                        this->CoolSetPointErrDone = true;
                    }
                }
            }
            this->CoolSetPointTempNode = this->CWPlantLoc.loop->TempSetPointNodeNum;
        }

        if (state.dataLoopNodes->Node(this->HWOutletNodeNum).TempSetPoint == Node::SensedNodeFlagValue) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                if (!this->HeatSetPointErrDone) {
                    ShowWarningError(state,
                                     std::format("Missing temperature setpoint on heating side for CentralHeatPumpSystem named {}", this->Name));
                    ShowContinueError(state,
                                      "  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem, use a SetpointManager");
                    ShowContinueError(state, "  The overall loop setpoint will be assumed for CentralHeatPumpSystem. The simulation continues ... ");
                    this->HeatSetPointErrDone = true;
                }
            } else {
                // need call to EMS to check node
                bool FatalError = false; // but not really fatal yet, but should be.
                EMSManager::CheckIfNodeSetPointManagedByEMS(state, this->HWOutletNodeNum, HVAC::CtrlVarType::Temp, FatalError);
                state.dataLoopNodes->NodeSetpointCheck(this->HWOutletNodeNum).needsSetpointChecking = false;
                if (FatalError) {
                    if (!this->HeatSetPointErrDone) {
                        ShowWarningError(state,
                                         std::format("Missing temperature setpoint on heating side for CentralHeatPumpSystem named {}", this->Name));
                        ShowContinueError(state, "A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ");
                        ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the chiller side outlet node ");
                        ShowContinueError(state, "or use an EMS actuator to establish a setpoint at the outlet node ");
                        ShowContinueError(state, "The overall loop setpoint will be assumed for chiller side. The simulation continues ... ");
                        this->HeatSetPointErrDone = true;
                    }
                }
            }
            this->HeatSetPointTempNode = this->HWPlantLoc.loop->TempSetPointNodeNum;
        }
        this->MyWrapperFlag = false;
    }

    if (this->MyWrapperEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {
        this->initializeDesignFlowLimits(state);
        this->resetOffState(state, false);
        this->MyWrapperEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->MyWrapperEnvrnFlag = true;
    }

    Real64 mdotCHW;  // Chilled water mass flow rate
    Real64 mdotHW;   // Hot water mass flow rate
    Real64 mdotGLHE; // Condenser water mass flow rate

    if (!RunFlag) {
        if (LoopNum == this->CWPlantLoc.loopNum) {
            mdotCHW = 0.0;
            PlantUtilities::SetComponentFlowRate(state, mdotCHW, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWPlantLoc);
        } else if (LoopNum == this->HWPlantLoc.loopNum) {
            mdotHW = 0.0;
            PlantUtilities::SetComponentFlowRate(state, mdotHW, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWPlantLoc);
        } else if (LoopNum == this->GLHEPlantLoc.loopNum) {
            mdotGLHE = 0.0;
            PlantUtilities::SetComponentFlowRate(state, mdotGLHE, this->GLHEInletNodeNum, this->GLHEOutletNodeNum, this->GLHEPlantLoc);
        }
        return;
    }

    // Switch over the mass flow rate to the condenser loop, i.e., ground heat exchanger
    if (LoopNum == this->CWPlantLoc.loopNum) { // called for on cooling loop
        if (MyLoad < -1.0) {                   // calling for cooling
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if (this->WrapperHeatingLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if ((MyLoad < -1.0) || (this->WrapperHeatingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }

    } else if (LoopNum == this->HWPlantLoc.loopNum) {
        if (MyLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if (this->WrapperCoolingLoad > 1.0) {
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if ((MyLoad > 1.0) || (this->WrapperCoolingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }

    } else if (LoopNum == this->GLHEPlantLoc.loopNum) {
        if (this->WrapperCoolingLoad > 1.0) {
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if (this->WrapperHeatingLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if ((this->WrapperHeatingLoad > 1.0) || (this->WrapperCoolingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }
    }

    PlantUtilities::SetComponentFlowRate(state, mdotCHW, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWPlantLoc);

    PlantUtilities::SetComponentFlowRate(state, mdotHW, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWPlantLoc);

    PlantUtilities::SetComponentFlowRate(state, mdotGLHE, this->GLHEInletNodeNum, this->GLHEOutletNodeNum, this->GLHEPlantLoc);
}

ChillerHeaterResult WrapperSpecs::solveCoolingOnly(EnergyPlusData &state,
                                                   int const chillerHeaterNum,
                                                   Real64 const requestedCoolingLoad,
                                                   Real64 const evaporatorMassFlowRateMax,
                                                   Real64 const condenserMassFlowRate,
                                                   Real64 const evaporatorInletTemp,
                                                   Real64 const condenserInletTemp)
{
    static constexpr std::string_view routineName("CentralGSHP cooling-only solver");
    constexpr int maxIterations = 100;
    constexpr Real64 convergenceTolerance = 1.0e-8;

    auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
    ChillerHeaterResult result;
    result.requestedCoolingLoad = max(0.0, requestedCoolingLoad);
    result.evaporatorInletTemp = evaporatorInletTemp;
    result.evaporatorOutletTemp = evaporatorInletTemp;
    result.condenserInletTemp = condenserInletTemp;
    result.condenserOutletTemp = condenserInletTemp;
    result.unmetCoolingLoad = result.requestedCoolingLoad;

    if (result.requestedCoolingLoad <= HVAC::SmallLoad || evaporatorMassFlowRateMax <= DataBranchAirLoopPlant::MassFlowTolerance ||
        condenserMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance || chillerHeater.RefCap <= 0.0 || chillerHeater.RefCOP <= 0.0) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 minPartLoadRatio = 0.0;
    Real64 maxPartLoadRatio = 1.0;
    getPartLoadCurveMinMax(state, chillerHeater.ChillerEIRFPLRIDX, minPartLoadRatio, maxPartLoadRatio);
    minPartLoadRatio = max(0.0, minPartLoadRatio);
    maxPartLoadRatio = max(minPartLoadRatio, maxPartLoadRatio);

    Real64 const evaporatorCp = this->CWPlantLoc.loop->glycol->getSpecificHeat(state, evaporatorInletTemp, routineName);
    Real64 const condenserCp = this->GLHEPlantLoc.loop->glycol->getSpecificHeat(state, condenserInletTemp, routineName);
    Real64 evaporatorOutletTarget = state.dataLoopNodes->Node(this->CoolSetPointTempNode).TempSetPoint;
    if (evaporatorOutletTarget == Node::SensedNodeFlagValue) {
        evaporatorOutletTarget = chillerHeater.EvapOutletNode.TempMin;
    }
    evaporatorOutletTarget = max(evaporatorOutletTarget, chillerHeater.EvapOutletNode.TempMin);
    Real64 const evaporatorDeltaTempTarget = max(0.0, evaporatorInletTemp - evaporatorOutletTarget);
    Real64 const flowLimitedCooling = evaporatorMassFlowRateMax * evaporatorCp * evaporatorDeltaTempTarget;

    if (flowLimitedCooling <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 evaporatorOutletGuess = evaporatorOutletTarget;
    Real64 condenserOutletGuess = condenserInletTemp;
    Real64 availableEvaporatorCapacity = 0.0;
    Real64 qEvaporator = 0.0;
    Real64 qCondenser = 0.0;
    Real64 compressorPower = 0.0;
    Real64 falseLoadRate = 0.0;
    Real64 partLoadRatio = 0.0;
    Real64 cyclingRatio = 0.0;
    Real64 evaporatorMassFlowRate = evaporatorMassFlowRateMax;
    Real64 evaporatorOutletTemp = evaporatorInletTemp;
    Real64 condenserOutletTemp = condenserInletTemp;
    Real64 capacityModifier = 0.0;
    Real64 eirTemperatureModifier = 0.0;
    Real64 eirPartLoadModifier = 0.0;
    Real64 condenserCurveTemp = condenserInletTemp;

    for (int iteration = 0; iteration < maxIterations; ++iteration) {
        condenserCurveTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserInletTemp, condenserOutletGuess);
        capacityModifier = this->calcChillerCapFT(state, chillerHeaterNum, evaporatorOutletGuess, condenserCurveTemp);
        availableEvaporatorCapacity = chillerHeater.RefCap * capacityModifier;
        qEvaporator = std::min({result.requestedCoolingLoad, availableEvaporatorCapacity * maxPartLoadRatio, flowLimitedCooling});

        if (qEvaporator <= HVAC::SmallLoad || availableEvaporatorCapacity <= 0.0) {
            qEvaporator = 0.0;
            break;
        }

        if (this->VariableFlowCH) {
            evaporatorMassFlowRate = min(evaporatorMassFlowRateMax, qEvaporator / (evaporatorCp * evaporatorDeltaTempTarget));
        } else {
            evaporatorMassFlowRate = evaporatorMassFlowRateMax;
        }
        evaporatorOutletTemp = evaporatorInletTemp - qEvaporator / (evaporatorMassFlowRate * evaporatorCp);

        Real64 const requestedPartLoadRatio = qEvaporator / availableEvaporatorCapacity;
        partLoadRatio = min(maxPartLoadRatio, max(requestedPartLoadRatio, minPartLoadRatio));
        cyclingRatio = minPartLoadRatio > 0.0 ? min(1.0, requestedPartLoadRatio / minPartLoadRatio) : 1.0;
        falseLoadRate = max(0.0, availableEvaporatorCapacity * partLoadRatio * cyclingRatio - qEvaporator);

        eirTemperatureModifier = max(0.0, Curve::CurveValue(state, chillerHeater.ChillerEIRFTIDX, evaporatorOutletTemp, condenserCurveTemp));
        eirPartLoadModifier = max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, partLoadRatio));
        compressorPower = (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * eirPartLoadModifier * cyclingRatio;
        qCondenser = qEvaporator + falseLoadRate + compressorPower * chillerHeater.OpenMotorEff;
        condenserOutletTemp = condenserInletTemp + qCondenser / (condenserMassFlowRate * condenserCp);

        Real64 const residual = max(std::abs(evaporatorOutletTemp - evaporatorOutletGuess), std::abs(condenserOutletTemp - condenserOutletGuess));
        if (residual <= convergenceTolerance) {
            break;
        }
        evaporatorOutletGuess = 0.5 * (evaporatorOutletGuess + evaporatorOutletTemp);
        condenserOutletGuess = 0.5 * (condenserOutletGuess + condenserOutletTemp);
    }

    if (qEvaporator <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    condenserCurveTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserInletTemp, condenserOutletTemp);
    capacityModifier = this->calcChillerCapFT(state, chillerHeaterNum, evaporatorOutletTemp, condenserCurveTemp);
    availableEvaporatorCapacity = chillerHeater.RefCap * capacityModifier;
    eirTemperatureModifier = max(0.0, Curve::CurveValue(state, chillerHeater.ChillerEIRFTIDX, evaporatorOutletTemp, condenserCurveTemp));
    eirPartLoadModifier = max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, partLoadRatio));
    compressorPower = (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * eirPartLoadModifier * cyclingRatio;
    falseLoadRate = max(0.0, availableEvaporatorCapacity * partLoadRatio * cyclingRatio - qEvaporator);
    qCondenser = qEvaporator + falseLoadRate + compressorPower * chillerHeater.OpenMotorEff;
    condenserOutletTemp = condenserInletTemp + qCondenser / (condenserMassFlowRate * condenserCp);

    Real64 const availableEIRPartLoadModifier =
        max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, maxPartLoadRatio));
    Real64 const availablePower = (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * availableEIRPartLoadModifier;

    result.currentMode = CurrentMode::CoolingOnly;
    result.availableEvaporatorCapacity = availableEvaporatorCapacity;
    result.availableCondenserCapacity = availableEvaporatorCapacity * maxPartLoadRatio + availablePower * chillerHeater.OpenMotorEff;
    result.qEvaporator = qEvaporator;
    result.qCondenser = qCondenser;
    result.coolingPower = compressorPower;
    result.falseLoadRate = falseLoadRate;
    result.partLoadRatio = partLoadRatio;
    result.cyclingRatio = cyclingRatio;
    result.unloadingRatio = partLoadRatio;
    result.capacityTemperatureModifier = capacityModifier;
    result.eirTemperatureModifier = eirTemperatureModifier;
    result.eirPartLoadModifier = eirPartLoadModifier;
    result.capacityCurveEvaporatorTemp = evaporatorOutletTemp;
    result.capacityCurveCondenserTemp = condenserCurveTemp;
    result.eirCurveEvaporatorTemp = evaporatorOutletTemp;
    result.eirCurveCondenserTemp = condenserCurveTemp;
    result.eirPartLoadCurvePLR = partLoadRatio;
    result.eirPartLoadCurveCondenserTemp = condenserCurveTemp;
    result.actualCOP = compressorPower > 0.0 ? (qEvaporator + falseLoadRate) / compressorPower : 0.0;
    result.evaporatorOutletTemp = evaporatorOutletTemp;
    result.evaporatorMassFlowRate = evaporatorMassFlowRate;
    result.condenserOutletTemp = condenserOutletTemp;
    result.condenserMassFlowRate = condenserMassFlowRate;
    result.unmetCoolingLoad = max(0.0, result.requestedCoolingLoad - qEvaporator);
    result.updatePowerAccounting(chillerHeater.OpenMotorEff);
    return result;
}

ChillerHeaterResult WrapperSpecs::solveHeatingOnly(EnergyPlusData &state,
                                                   int const chillerHeaterNum,
                                                   Real64 const requestedHeatingLoad,
                                                   Real64 const evaporatorMassFlowRate,
                                                   Real64 const condenserMassFlowRateMax,
                                                   Real64 const evaporatorInletTemp,
                                                   Real64 const condenserInletTemp)
{
    static constexpr std::string_view routineName("CentralGSHP heating-only solver");
    constexpr int maxIterations = 100;
    constexpr int maxPartLoadIterations = 80;
    constexpr Real64 convergenceTolerance = 1.0e-8;

    auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
    ChillerHeaterResult result;
    result.requestedHeatingLoad = max(0.0, requestedHeatingLoad);
    result.evaporatorInletTemp = evaporatorInletTemp;
    result.evaporatorOutletTemp = evaporatorInletTemp;
    result.condenserInletTemp = condenserInletTemp;
    result.condenserOutletTemp = condenserInletTemp;
    result.unmetHeatingLoad = result.requestedHeatingLoad;

    if (result.requestedHeatingLoad <= HVAC::SmallLoad || evaporatorMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance ||
        condenserMassFlowRateMax <= DataBranchAirLoopPlant::MassFlowTolerance || chillerHeater.RefCap <= 0.0 || chillerHeater.RefCOP <= 0.0) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 minPartLoadRatio = 0.0;
    Real64 maxPartLoadRatio = 1.0;
    getPartLoadCurveMinMax(state, chillerHeater.ChillerEIRFPLRIDX, minPartLoadRatio, maxPartLoadRatio);
    minPartLoadRatio = max(0.0, minPartLoadRatio);
    maxPartLoadRatio = max(minPartLoadRatio, maxPartLoadRatio);

    Real64 const evaporatorCp = this->GLHEPlantLoc.loop->glycol->getSpecificHeat(state, evaporatorInletTemp, routineName);
    Real64 const condenserCp = this->HWPlantLoc.loop->glycol->getSpecificHeat(state, condenserInletTemp, routineName);
    Real64 const evaporatorOutletLowLimit = max(chillerHeater.TempLowLimitEvapOut, chillerHeater.EvapOutletNode.TempMin);
    Real64 const sourceLimitedEvaporatorHeat = max(0.0, evaporatorMassFlowRate * evaporatorCp * (evaporatorInletTemp - evaporatorOutletLowLimit));

    bool hasCondenserOutletLimit = false;
    Real64 condenserOutletLimit = 0.0;
    Real64 const plantHeatingSetPoint = state.dataLoopNodes->Node(this->HeatSetPointTempNode).TempSetPoint;
    if (plantHeatingSetPoint != Node::SensedNodeFlagValue) {
        condenserOutletLimit = plantHeatingSetPoint;
        hasCondenserOutletLimit = true;
    }
    if (!chillerHeater.MaxHeatingLeavingCondTempWasBlank) {
        condenserOutletLimit =
            hasCondenserOutletLimit ? min(condenserOutletLimit, chillerHeater.MaxHeatingLeavingCondTemp) : chillerHeater.MaxHeatingLeavingCondTemp;
        hasCondenserOutletLimit = true;
    }
    Real64 const hotWaterLimitedCondenserHeat = hasCondenserOutletLimit
                                                    ? max(0.0, condenserMassFlowRateMax * condenserCp * (condenserOutletLimit - condenserInletTemp))
                                                    : std::numeric_limits<Real64>::max();

    if (sourceLimitedEvaporatorHeat <= HVAC::SmallLoad || hotWaterLimitedCondenserHeat <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 evaporatorOutletGuess =
        max(evaporatorOutletLowLimit, evaporatorInletTemp - sourceLimitedEvaporatorHeat / (evaporatorMassFlowRate * evaporatorCp));
    Real64 condenserOutletGuess =
        hasCondenserOutletLimit
            ? min(condenserOutletLimit, condenserInletTemp + result.requestedHeatingLoad / (condenserMassFlowRateMax * condenserCp))
            : condenserInletTemp;
    Real64 availableEvaporatorCapacity = 0.0;
    Real64 availableCondenserCapacity = 0.0;
    Real64 qEvaporator = 0.0;
    Real64 qCondenser = 0.0;
    Real64 compressorPower = 0.0;
    Real64 partLoadRatio = 0.0;
    Real64 cyclingRatio = 0.0;
    Real64 condenserMassFlowRate = condenserMassFlowRateMax;
    Real64 evaporatorOutletTemp = evaporatorInletTemp;
    Real64 condenserOutletTemp = condenserInletTemp;
    Real64 capacityModifier = 0.0;
    Real64 eirTemperatureModifier = 0.0;
    Real64 eirPartLoadModifier = 0.0;
    Real64 condenserCurveTemp = condenserInletTemp;

    for (int iteration = 0; iteration < maxIterations; ++iteration) {
        condenserCurveTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserInletTemp, condenserOutletGuess);
        capacityModifier = this->calcChillerCapFT(state, chillerHeaterNum, evaporatorOutletGuess, condenserCurveTemp);
        availableEvaporatorCapacity = chillerHeater.RefCap * capacityModifier;
        eirTemperatureModifier = max(0.0, Curve::CurveValue(state, chillerHeater.ChillerEIRFTIDX, evaporatorOutletGuess, condenserCurveTemp));

        if (availableEvaporatorCapacity <= 0.0) {
            break;
        }

        auto operatingPointAtPLR = [&](Real64 const plr) {
            Real64 const eirPLR = max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, plr));
            Real64 const power = (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * eirPLR;
            Real64 const condenserHeat = availableEvaporatorCapacity * plr + power * chillerHeater.OpenMotorEff;
            return std::array<Real64, 3>{condenserHeat, power, eirPLR};
        };

        Real64 maximumAllowedPLR = min(maxPartLoadRatio, sourceLimitedEvaporatorHeat / availableEvaporatorCapacity);
        Real64 maximumCyclingRatio = 1.0;
        if (maximumAllowedPLR < minPartLoadRatio) {
            maximumCyclingRatio = minPartLoadRatio > 0.0 ? max(0.0, maximumAllowedPLR / minPartLoadRatio) : 0.0;
            maximumAllowedPLR = minPartLoadRatio;
        }
        auto const maximumPoint = operatingPointAtPLR(maximumAllowedPLR);
        availableCondenserCapacity = maximumPoint[0] * maximumCyclingRatio;
        availableCondenserCapacity = min(availableCondenserCapacity, hotWaterLimitedCondenserHeat);
        Real64 const targetCondenserHeat = min(result.requestedHeatingLoad, availableCondenserCapacity);
        if (targetCondenserHeat <= HVAC::SmallLoad) {
            break;
        }

        auto const minimumPoint = operatingPointAtPLR(minPartLoadRatio);
        if (targetCondenserHeat < minimumPoint[0]) {
            partLoadRatio = minPartLoadRatio;
            cyclingRatio = min(maximumCyclingRatio, targetCondenserHeat / minimumPoint[0]);
        } else {
            cyclingRatio = 1.0;
            Real64 lowerPLR = minPartLoadRatio;
            Real64 upperPLR = maximumAllowedPLR;
            for (int plrIteration = 0; plrIteration < maxPartLoadIterations; ++plrIteration) {
                Real64 const candidatePLR = 0.5 * (lowerPLR + upperPLR);
                if (operatingPointAtPLR(candidatePLR)[0] < targetCondenserHeat) {
                    lowerPLR = candidatePLR;
                } else {
                    upperPLR = candidatePLR;
                }
            }
            partLoadRatio = 0.5 * (lowerPLR + upperPLR);
        }

        auto const operatingPoint = operatingPointAtPLR(partLoadRatio);
        qEvaporator = availableEvaporatorCapacity * partLoadRatio * cyclingRatio;
        compressorPower = operatingPoint[1] * cyclingRatio;
        eirPartLoadModifier = operatingPoint[2];
        qCondenser = qEvaporator + compressorPower * chillerHeater.OpenMotorEff;

        if (this->VariableFlowCH && hasCondenserOutletLimit && condenserOutletLimit > condenserInletTemp) {
            condenserMassFlowRate = min(condenserMassFlowRateMax, qCondenser / (condenserCp * (condenserOutletLimit - condenserInletTemp)));
        } else {
            condenserMassFlowRate = condenserMassFlowRateMax;
        }
        evaporatorOutletTemp = evaporatorInletTemp - qEvaporator / (evaporatorMassFlowRate * evaporatorCp);
        condenserOutletTemp = condenserInletTemp + qCondenser / (condenserMassFlowRate * condenserCp);

        Real64 const residual = max(std::abs(evaporatorOutletTemp - evaporatorOutletGuess), std::abs(condenserOutletTemp - condenserOutletGuess));
        if (residual <= convergenceTolerance) {
            break;
        }
        evaporatorOutletGuess = 0.5 * (evaporatorOutletGuess + evaporatorOutletTemp);
        condenserOutletGuess = 0.5 * (condenserOutletGuess + condenserOutletTemp);
    }

    if (qCondenser <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    condenserCurveTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserInletTemp, condenserOutletTemp);
    capacityModifier = this->calcChillerCapFT(state, chillerHeaterNum, evaporatorOutletTemp, condenserCurveTemp);
    availableEvaporatorCapacity = chillerHeater.RefCap * capacityModifier;
    eirTemperatureModifier = max(0.0, Curve::CurveValue(state, chillerHeater.ChillerEIRFTIDX, evaporatorOutletTemp, condenserCurveTemp));
    eirPartLoadModifier = max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, partLoadRatio));
    compressorPower = (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * eirPartLoadModifier * cyclingRatio;
    qEvaporator = availableEvaporatorCapacity * partLoadRatio * cyclingRatio;
    qCondenser = qEvaporator + compressorPower * chillerHeater.OpenMotorEff;
    evaporatorOutletTemp = evaporatorInletTemp - qEvaporator / (evaporatorMassFlowRate * evaporatorCp);
    condenserOutletTemp = condenserInletTemp + qCondenser / (condenserMassFlowRate * condenserCp);

    result.currentMode = CurrentMode::HeatingOnly;
    result.availableEvaporatorCapacity = availableEvaporatorCapacity;
    result.availableCondenserCapacity = availableCondenserCapacity;
    result.qEvaporator = qEvaporator;
    result.qCondenser = qCondenser;
    result.heatingPower = compressorPower;
    result.partLoadRatio = partLoadRatio;
    result.cyclingRatio = cyclingRatio;
    result.unloadingRatio = partLoadRatio;
    result.capacityTemperatureModifier = capacityModifier;
    result.eirTemperatureModifier = eirTemperatureModifier;
    result.eirPartLoadModifier = eirPartLoadModifier;
    result.capacityCurveEvaporatorTemp = evaporatorOutletTemp;
    result.capacityCurveCondenserTemp = condenserCurveTemp;
    result.eirCurveEvaporatorTemp = evaporatorOutletTemp;
    result.eirCurveCondenserTemp = condenserCurveTemp;
    result.eirPartLoadCurvePLR = partLoadRatio;
    result.eirPartLoadCurveCondenserTemp = condenserCurveTemp;
    result.actualCOP = compressorPower > 0.0 ? qCondenser / compressorPower : 0.0;
    result.evaporatorOutletTemp = evaporatorOutletTemp;
    result.evaporatorMassFlowRate = evaporatorMassFlowRate;
    result.condenserOutletTemp = condenserOutletTemp;
    result.condenserMassFlowRate = condenserMassFlowRate;
    result.unmetHeatingLoad = max(0.0, result.requestedHeatingLoad - qCondenser);
    result.updatePowerAccounting(chillerHeater.OpenMotorEff);
    return result;
}

ChillerHeaterResult WrapperSpecs::solveSimultaneous(EnergyPlusData &state,
                                                    int const chillerHeaterNum,
                                                    Real64 const requestedCoolingLoad,
                                                    Real64 const requestedHeatingLoad,
                                                    Real64 const chilledWaterMassFlowRateMax,
                                                    Real64 const hotWaterMassFlowRateMax,
                                                    Real64 const sourceMassFlowRateMax,
                                                    Real64 const chilledWaterInletTemp,
                                                    Real64 const hotWaterInletTemp,
                                                    Real64 const sourceInletTemp)
{
    static constexpr std::string_view routineName("CentralGSHP simultaneous solver");
    constexpr int maxIterations = 100;
    constexpr int maxPartLoadIterations = 80;
    constexpr Real64 convergenceTolerance = 1.0e-8;

    auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
    ChillerHeaterResult result;
    result.requestedCoolingLoad = max(0.0, requestedCoolingLoad);
    result.requestedHeatingLoad = max(0.0, requestedHeatingLoad);
    result.unmetCoolingLoad = result.requestedCoolingLoad;
    result.unmetHeatingLoad = result.requestedHeatingLoad;
    result.chilledWaterInletTemp = chilledWaterInletTemp;
    result.chilledWaterOutletTemp = chilledWaterInletTemp;
    result.hotWaterInletTemp = hotWaterInletTemp;
    result.hotWaterOutletTemp = hotWaterInletTemp;
    result.sourceInletTemp = sourceInletTemp;
    result.sourceOutletTemp = sourceInletTemp;

    if (result.requestedCoolingLoad <= HVAC::SmallLoad || result.requestedHeatingLoad <= HVAC::SmallLoad ||
        chilledWaterMassFlowRateMax <= DataBranchAirLoopPlant::MassFlowTolerance ||
        hotWaterMassFlowRateMax <= DataBranchAirLoopPlant::MassFlowTolerance || chillerHeater.RefCap <= 0.0 || chillerHeater.RefCOP <= 0.0) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 minPartLoadRatio = 0.0;
    Real64 maxPartLoadRatio = 1.0;
    getPartLoadCurveMinMax(state, chillerHeater.ChillerEIRFPLRIDX, minPartLoadRatio, maxPartLoadRatio);
    minPartLoadRatio = max(0.0, minPartLoadRatio);
    maxPartLoadRatio = max(minPartLoadRatio, maxPartLoadRatio);

    Real64 const chilledWaterCp = this->CWPlantLoc.loop->glycol->getSpecificHeat(state, chilledWaterInletTemp, routineName);
    Real64 const hotWaterCp = this->HWPlantLoc.loop->glycol->getSpecificHeat(state, hotWaterInletTemp, routineName);
    Real64 const sourceCp = this->GLHEPlantLoc.loop->glycol->getSpecificHeat(state, sourceInletTemp, routineName);

    Real64 chilledWaterOutletTarget = state.dataLoopNodes->Node(this->CoolSetPointTempNode).TempSetPoint;
    if (chilledWaterOutletTarget == Node::SensedNodeFlagValue) {
        chilledWaterOutletTarget = chillerHeater.EvapOutletNode.TempMin;
    }
    chilledWaterOutletTarget = max(chilledWaterOutletTarget, chillerHeater.EvapOutletNode.TempMin);
    Real64 const chilledWaterDeltaTempTarget = max(0.0, chilledWaterInletTemp - chilledWaterOutletTarget);
    Real64 const coolingTarget = min(result.requestedCoolingLoad, chilledWaterMassFlowRateMax * chilledWaterCp * chilledWaterDeltaTempTarget);

    bool hasHotWaterOutletLimit = false;
    Real64 hotWaterOutletLimit = 0.0;
    Real64 const plantHeatingSetPoint = state.dataLoopNodes->Node(this->HeatSetPointTempNode).TempSetPoint;
    if (plantHeatingSetPoint != Node::SensedNodeFlagValue) {
        hotWaterOutletLimit = plantHeatingSetPoint;
        hasHotWaterOutletLimit = true;
    }
    if (!chillerHeater.MaxHeatingLeavingCondTempWasBlank) {
        hotWaterOutletLimit =
            hasHotWaterOutletLimit ? min(hotWaterOutletLimit, chillerHeater.MaxHeatingLeavingCondTemp) : chillerHeater.MaxHeatingLeavingCondTemp;
        hasHotWaterOutletLimit = true;
    }
    Real64 const hotWaterDeltaTempTarget = hasHotWaterOutletLimit ? max(0.0, hotWaterOutletLimit - hotWaterInletTemp) : 0.0;
    Real64 const hotWaterLimitedHeating =
        hasHotWaterOutletLimit ? hotWaterMassFlowRateMax * hotWaterCp * hotWaterDeltaTempTarget : std::numeric_limits<Real64>::max();
    Real64 const heatingTarget = min(result.requestedHeatingLoad, hotWaterLimitedHeating);

    auto sourceFlowLimit = [sourceMassFlowRateMax](Real64 const nodeFlowLimit, Real64 const designFlowLimit) {
        Real64 flowLimit = nodeFlowLimit;
        if (flowLimit <= DataBranchAirLoopPlant::MassFlowTolerance) {
            flowLimit = designFlowLimit;
        }
        if (flowLimit <= DataBranchAirLoopPlant::MassFlowTolerance) {
            flowLimit = sourceMassFlowRateMax;
        }
        return min(sourceMassFlowRateMax, max(0.0, flowLimit));
    };
    Real64 const sourceEvaporatorMassFlowRateMax = sourceFlowLimit(chillerHeater.SourceEvapMassFlowRateMax, chillerHeater.EvapMassFlowRateMax);
    Real64 const sourceCondenserMassFlowRateMax = sourceFlowLimit(chillerHeater.SourceCondMassFlowRateMax, chillerHeater.CondMassFlowRateMax);
    Real64 const sourceOutletLowLimit = max(chillerHeater.TempLowLimitEvapOut, chillerHeater.EvapOutletNode.TempMin);
    Real64 const sourceExtractionCapacity = sourceEvaporatorMassFlowRateMax > DataBranchAirLoopPlant::MassFlowTolerance
                                                ? max(0.0, sourceEvaporatorMassFlowRateMax * sourceCp * (sourceInletTemp - sourceOutletLowLimit))
                                                : 0.0;
    bool const canRejectToSource = sourceCondenserMassFlowRateMax > DataBranchAirLoopPlant::MassFlowTolerance;

    if (coolingTarget <= HVAC::SmallLoad || heatingTarget <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 evaporatorCurveTempGuess = chilledWaterOutletTarget;
    Real64 condenserEnteringTempGuess = hotWaterInletTemp;
    Real64 condenserLeavingTempGuess = hasHotWaterOutletLimit
                                           ? min(hotWaterOutletLimit, hotWaterInletTemp + heatingTarget / (hotWaterMassFlowRateMax * hotWaterCp))
                                           : hotWaterInletTemp;

    Real64 availableEvaporatorCapacity = 0.0;
    Real64 availableCondenserCapacity = 0.0;
    Real64 qEvaporator = 0.0;
    Real64 qCondenser = 0.0;
    Real64 compressorPower = 0.0;
    Real64 falseLoadRate = 0.0;
    Real64 partLoadRatio = 0.0;
    Real64 cyclingRatio = 0.0;
    Real64 capacityModifier = 0.0;
    Real64 eirTemperatureModifier = 0.0;
    Real64 eirPartLoadModifier = 0.0;
    Real64 condenserCurveTemp = condenserEnteringTempGuess;

    Real64 coolingDelivered = 0.0;
    Real64 heatingDelivered = 0.0;
    Real64 sourceExtraction = 0.0;
    Real64 sourceRejection = 0.0;
    Real64 chilledWaterMassFlowRate = 0.0;
    Real64 hotWaterMassFlowRate = 0.0;
    Real64 sourceMassFlowRate = 0.0;
    Real64 chilledWaterOutletTemp = chilledWaterInletTemp;
    Real64 hotWaterOutletTemp = hotWaterInletTemp;
    Real64 sourceOutletTemp = sourceInletTemp;
    Real64 evaporatorInletTemp = chilledWaterInletTemp;
    Real64 evaporatorOutletTemp = chilledWaterInletTemp;
    Real64 evaporatorMassFlowRate = 0.0;
    Real64 condenserInletTemp = hotWaterInletTemp;
    Real64 condenserOutletTemp = hotWaterInletTemp;
    Real64 condenserMassFlowRate = 0.0;

    for (int iteration = 0; iteration < maxIterations; ++iteration) {
        condenserCurveTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserEnteringTempGuess, condenserLeavingTempGuess);
        capacityModifier = this->calcChillerCapFT(state, chillerHeaterNum, evaporatorCurveTempGuess, condenserCurveTemp);
        availableEvaporatorCapacity = chillerHeater.RefCap * capacityModifier;
        eirTemperatureModifier = max(0.0, Curve::CurveValue(state, chillerHeater.ChillerEIRFTIDX, evaporatorCurveTempGuess, condenserCurveTemp));
        if (availableEvaporatorCapacity <= 0.0) {
            break;
        }

        auto operatingPointAtEvaporatorLoad = [&](Real64 const evaporatorLoad) {
            std::array<Real64, 6> point{};
            if (evaporatorLoad <= HVAC::SmallLoad) {
                return point;
            }
            Real64 const requestedPartLoadRatio = evaporatorLoad / availableEvaporatorCapacity;
            Real64 const operatingPartLoadRatio = min(maxPartLoadRatio, max(requestedPartLoadRatio, minPartLoadRatio));
            Real64 const operatingCyclingRatio =
                requestedPartLoadRatio < minPartLoadRatio && minPartLoadRatio > 0.0 ? requestedPartLoadRatio / minPartLoadRatio : 1.0;
            Real64 const partLoadModifier =
                max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, operatingPartLoadRatio));
            Real64 const power =
                (availableEvaporatorCapacity / chillerHeater.RefCOP) * eirTemperatureModifier * partLoadModifier * operatingCyclingRatio;
            Real64 const falseLoad = max(0.0, availableEvaporatorCapacity * operatingPartLoadRatio * operatingCyclingRatio - evaporatorLoad);
            Real64 const condenserLoad = evaporatorLoad + falseLoad + power * chillerHeater.OpenMotorEff;
            return std::array<Real64, 6>{evaporatorLoad, condenserLoad, power, falseLoad, operatingPartLoadRatio, operatingCyclingRatio};
        };

        Real64 const maximumEvaporatorLoad = min(availableEvaporatorCapacity * maxPartLoadRatio, coolingTarget + sourceExtractionCapacity);
        auto coolingPoint = operatingPointAtEvaporatorLoad(min(coolingTarget, maximumEvaporatorLoad));
        auto maximumPoint = operatingPointAtEvaporatorLoad(maximumEvaporatorLoad);
        availableCondenserCapacity = maximumPoint[1];
        auto selectedPoint = coolingPoint;

        bool const heatingDrivesCycle = coolingPoint[1] + HVAC::SmallLoad < heatingTarget;
        bool const excessHeatCannotBeRejected = !canRejectToSource && coolingPoint[1] > heatingTarget + HVAC::SmallLoad;
        if (heatingDrivesCycle || excessHeatCannotBeRejected) {
            Real64 lowerEvaporatorLoad = heatingDrivesCycle ? coolingPoint[0] : 0.0;
            Real64 upperEvaporatorLoad = heatingDrivesCycle ? maximumEvaporatorLoad : coolingPoint[0];
            Real64 const boundedHeatingTarget = min(heatingTarget, operatingPointAtEvaporatorLoad(upperEvaporatorLoad)[1]);
            for (int partLoadIteration = 0; partLoadIteration < maxPartLoadIterations; ++partLoadIteration) {
                Real64 const candidateEvaporatorLoad = 0.5 * (lowerEvaporatorLoad + upperEvaporatorLoad);
                if (operatingPointAtEvaporatorLoad(candidateEvaporatorLoad)[1] < boundedHeatingTarget) {
                    lowerEvaporatorLoad = candidateEvaporatorLoad;
                } else {
                    upperEvaporatorLoad = candidateEvaporatorLoad;
                }
            }
            selectedPoint = operatingPointAtEvaporatorLoad(0.5 * (lowerEvaporatorLoad + upperEvaporatorLoad));
        }

        qEvaporator = selectedPoint[0];
        qCondenser = selectedPoint[1];
        compressorPower = selectedPoint[2];
        falseLoadRate = selectedPoint[3];
        partLoadRatio = selectedPoint[4];
        cyclingRatio = selectedPoint[5];
        eirPartLoadModifier = max(0.0, evaluatePartLoadCurve(state, chillerHeater.ChillerEIRFPLRIDX, condenserCurveTemp, partLoadRatio));

        coolingDelivered = min(coolingTarget, qEvaporator);
        sourceExtraction = max(0.0, qEvaporator - coolingDelivered);
        heatingDelivered = min(heatingTarget, qCondenser);
        sourceRejection = max(0.0, qCondenser - heatingDelivered);

        if (this->VariableFlowCH) {
            chilledWaterMassFlowRate = coolingDelivered > HVAC::SmallLoad && chilledWaterDeltaTempTarget > 0.0
                                           ? min(chilledWaterMassFlowRateMax, coolingDelivered / (chilledWaterCp * chilledWaterDeltaTempTarget))
                                           : 0.0;
            hotWaterMassFlowRate = heatingDelivered > HVAC::SmallLoad && hotWaterDeltaTempTarget > 0.0
                                       ? min(hotWaterMassFlowRateMax, heatingDelivered / (hotWaterCp * hotWaterDeltaTempTarget))
                                       : hotWaterMassFlowRateMax;
        } else {
            chilledWaterMassFlowRate = coolingDelivered > HVAC::SmallLoad ? chilledWaterMassFlowRateMax : 0.0;
            hotWaterMassFlowRate = heatingDelivered > HVAC::SmallLoad ? hotWaterMassFlowRateMax : 0.0;
        }
        if (sourceExtraction > HVAC::SmallLoad) {
            sourceMassFlowRate = sourceEvaporatorMassFlowRateMax;
        } else if (sourceRejection > HVAC::SmallLoad) {
            sourceMassFlowRate = sourceCondenserMassFlowRateMax;
        } else {
            sourceMassFlowRate = 0.0;
        }

        chilledWaterOutletTemp = chilledWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance
                                     ? chilledWaterInletTemp - coolingDelivered / (chilledWaterMassFlowRate * chilledWaterCp)
                                     : chilledWaterInletTemp;
        hotWaterOutletTemp = hotWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance
                                 ? hotWaterInletTemp + heatingDelivered / (hotWaterMassFlowRate * hotWaterCp)
                                 : hotWaterInletTemp;
        sourceOutletTemp = sourceMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance
                               ? sourceInletTemp + (sourceRejection - sourceExtraction) / (sourceMassFlowRate * sourceCp)
                               : sourceInletTemp;

        Real64 const evaporatorSourceMassFlowRate = sourceExtraction > HVAC::SmallLoad ? sourceMassFlowRate : 0.0;
        evaporatorMassFlowRate = chilledWaterMassFlowRate + evaporatorSourceMassFlowRate;
        if (evaporatorMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            evaporatorInletTemp =
                (chilledWaterMassFlowRate * chilledWaterInletTemp + evaporatorSourceMassFlowRate * sourceInletTemp) / evaporatorMassFlowRate;
            evaporatorOutletTemp =
                (chilledWaterMassFlowRate * chilledWaterOutletTemp + evaporatorSourceMassFlowRate * sourceOutletTemp) / evaporatorMassFlowRate;
        }

        Real64 const condenserSourceMassFlowRate = sourceRejection > HVAC::SmallLoad ? sourceMassFlowRate : 0.0;
        condenserMassFlowRate = hotWaterMassFlowRate + condenserSourceMassFlowRate;
        if (condenserMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            condenserInletTemp = (hotWaterMassFlowRate * hotWaterInletTemp + condenserSourceMassFlowRate * sourceInletTemp) / condenserMassFlowRate;
            condenserOutletTemp =
                (hotWaterMassFlowRate * hotWaterOutletTemp + condenserSourceMassFlowRate * sourceOutletTemp) / condenserMassFlowRate;
        }

        Real64 const temperatureResidual = max({std::abs(evaporatorOutletTemp - evaporatorCurveTempGuess),
                                                std::abs(condenserInletTemp - condenserEnteringTempGuess),
                                                std::abs(condenserOutletTemp - condenserLeavingTempGuess)});
        if (temperatureResidual <= convergenceTolerance) {
            break;
        }
        evaporatorCurveTempGuess = 0.5 * (evaporatorCurveTempGuess + evaporatorOutletTemp);
        condenserEnteringTempGuess = 0.5 * (condenserEnteringTempGuess + condenserInletTemp);
        condenserLeavingTempGuess = 0.5 * (condenserLeavingTempGuess + condenserOutletTemp);
    }

    if (qEvaporator <= HVAC::SmallLoad || qCondenser <= HVAC::SmallLoad) {
        result.updatePowerAccounting(chillerHeater.OpenMotorEff);
        return result;
    }

    Real64 const sourceHeatTransfer = sourceRejection - sourceExtraction;
    Real64 const modeTolerance = max(HVAC::SmallLoad, 1.0e-8 * max(qEvaporator, qCondenser));
    if (sourceHeatTransfer > modeTolerance) {
        result.currentMode = CurrentMode::CoolingDominant;
        result.coolingPower = compressorPower;
    } else if (sourceHeatTransfer < -modeTolerance) {
        result.currentMode = CurrentMode::HeatingDominant;
        result.heatingPower = compressorPower;
    } else {
        result.currentMode = CurrentMode::HeatRecovery;
        result.coolingPower = compressorPower;
    }

    result.availableEvaporatorCapacity = availableEvaporatorCapacity;
    result.availableCondenserCapacity = availableCondenserCapacity;
    result.qEvaporator = qEvaporator;
    result.qCondenser = qCondenser;
    result.falseLoadRate = falseLoadRate;
    result.partLoadRatio = partLoadRatio;
    result.cyclingRatio = cyclingRatio;
    result.unloadingRatio = partLoadRatio;
    result.capacityTemperatureModifier = capacityModifier;
    result.eirTemperatureModifier = eirTemperatureModifier;
    result.eirPartLoadModifier = eirPartLoadModifier;
    result.capacityCurveEvaporatorTemp = evaporatorOutletTemp;
    result.capacityCurveCondenserTemp = this->setChillerHeaterCondTemp(state, chillerHeaterNum, condenserInletTemp, condenserOutletTemp);
    result.eirCurveEvaporatorTemp = evaporatorOutletTemp;
    result.eirCurveCondenserTemp = result.capacityCurveCondenserTemp;
    result.eirPartLoadCurvePLR = partLoadRatio;
    result.eirPartLoadCurveCondenserTemp = result.capacityCurveCondenserTemp;
    result.actualCOP = compressorPower > 0.0 ? qCondenser / compressorPower : 0.0;
    result.evaporatorInletTemp = evaporatorInletTemp;
    result.evaporatorOutletTemp = evaporatorOutletTemp;
    result.evaporatorMassFlowRate = evaporatorMassFlowRate;
    result.condenserInletTemp = condenserInletTemp;
    result.condenserOutletTemp = condenserOutletTemp;
    result.condenserMassFlowRate = condenserMassFlowRate;
    result.chilledWaterOutletTemp = chilledWaterOutletTemp;
    result.chilledWaterMassFlowRate = chilledWaterMassFlowRate;
    result.hotWaterOutletTemp = hotWaterOutletTemp;
    result.hotWaterMassFlowRate = hotWaterMassFlowRate;
    result.sourceOutletTemp = sourceOutletTemp;
    result.sourceMassFlowRate = sourceMassFlowRate;
    result.coolingDelivered = coolingDelivered;
    result.heatingDelivered = heatingDelivered;
    result.heatRecovered = heatingDelivered;
    result.sourceHeatTransfer = sourceHeatTransfer;
    result.unmetCoolingLoad = max(0.0, result.requestedCoolingLoad - coolingDelivered);
    result.unmetHeatingLoad = max(0.0, result.requestedHeatingLoad - heatingDelivered);
    result.updatePowerAccounting(chillerHeater.OpenMotorEff);
    return result;
}

Real64 WrapperSpecs::setChillerHeaterCondTemp([[maybe_unused]] EnergyPlusData &state,
                                              int const numChillerHeater,
                                              Real64 const condEnteringTemp,
                                              Real64 const condLeavingTemp)
{
    Real64 setChillerHeaterCondTemp;
    if (this->ChillerHeater(numChillerHeater).CondMode == CondenserModeTemperature::EnteringCondenser) {
        setChillerHeaterCondTemp = condEnteringTemp;
    } else { // by default, if not EnteringCondenser, then this can only be LeavingCondenser
        setChillerHeaterCondTemp = condLeavingTemp;
    }
    return setChillerHeaterCondTemp;
}

Real64 WrapperSpecs::calcChillerCapFT(EnergyPlusData &state, int const numChillerHeater, Real64 const evapOutletTemp, Real64 const condTemp)
{
    // Calculate the chiller capacity as a function of temperature
    Real64 chillCapFT = Curve::CurveValue(state, this->ChillerHeater(numChillerHeater).ChillerCapFTIDX, evapOutletTemp, condTemp);

    // Tracks errors for when the capacity is calculated as less than zero
    if (chillCapFT < 0) {
        if (this->ChillerHeater(numChillerHeater).ChillerCapFTError < 1 && !state.dataGlobal->WarmupFlag) {
            ++this->ChillerHeater(numChillerHeater).ChillerCapFTError;
            ShowWarningError(state, std::format("ChillerHeaterPerformance:Electric:EIR \"{}\":", this->ChillerHeater(numChillerHeater).Name));
            ShowContinueError(state,
                              std::format(" ChillerHeater Capacity as a Function of Temperature curve output is negative ({:.3f}).", chillCapFT));
            ShowContinueError(state,
                              std::format(" Negative value occurs using an Evaporator Outlet Temp of {:.1f} and a Condenser Inlet Temp of {:.1f}.",
                                          evapOutletTemp,
                                          condTemp));
            ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
        } else if (!state.dataGlobal->WarmupFlag) {
            ++this->ChillerHeater(numChillerHeater).ChillerCapFTError;
            ShowRecurringWarningErrorAtEnd(
                state,
                "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(numChillerHeater).Name +
                    "\": ChillerHeater Capacity as a Function of Temperature curve output is negative warning continues...",
                this->ChillerHeater(numChillerHeater).ChillerCapFTErrorIndex,
                chillCapFT,
                chillCapFT);
        }
        chillCapFT = 0.0;
    }
    return chillCapFT;
}

void WrapperSpecs::CalcCoolingOnlyModel(EnergyPlusData &state,
                                        Real64 const chilledWaterMassFlowRate,
                                        Real64 const sourceMassFlowRate,
                                        Real64 const chilledWaterInletTemp,
                                        Real64 const sourceInletTemp)
{
    Real64 remainingCoolingLoad = this->WrapperCoolingLoad;
    Real64 remainingChilledWaterMassFlowRate = max(0.0, chilledWaterMassFlowRate);
    Real64 remainingSourceMassFlowRate = max(0.0, sourceMassFlowRate);
    int componentNum = 1;
    int unitsUsedInComponent = 0;

    auto allocateConnectionFlow = [](Real64 const remainingFlow, Real64 const connectionDesignFlow, Real64 const legacyDesignFlow) {
        Real64 designFlow = connectionDesignFlow;
        if (designFlow <= DataBranchAirLoopPlant::MassFlowTolerance) {
            designFlow = legacyDesignFlow;
        }
        if (designFlow <= DataBranchAirLoopPlant::MassFlowTolerance) {
            designFlow = remainingFlow;
        }
        return min(max(0.0, remainingFlow), max(0.0, designFlow));
    };

    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        bool moduleIsAvailable = true;
        if (this->NumOfComp > 0 && allocated(this->WrapperComp)) {
            while (componentNum <= this->NumOfComp && unitsUsedInComponent >= this->WrapperComp(componentNum).WrapperIdenticalObjectNum) {
                ++componentNum;
                unitsUsedInComponent = 0;
            }
            if (componentNum <= this->NumOfComp) {
                moduleIsAvailable =
                    this->WrapperComp(componentNum).chSched == nullptr || this->WrapperComp(componentNum).chSched->getCurrentVal() > 0.0;
                ++unitsUsedInComponent;
            } else {
                moduleIsAvailable = false;
            }
        }

        chillerHeater.RefCap = chillerHeater.RefCapCooling;
        chillerHeater.RefCOP = chillerHeater.RefCOPCooling;
        chillerHeater.TempRefEvapOut = chillerHeater.TempRefEvapOutCooling;
        chillerHeater.TempRefCondIn = chillerHeater.TempRefCondInCooling;
        chillerHeater.TempRefCondOut = chillerHeater.TempRefCondOutCooling;
        chillerHeater.OptPartLoadRat = chillerHeater.OptPartLoadRatCooling;
        chillerHeater.CondMode = chillerHeater.CondModeCooling;
        chillerHeater.ChillerCapFTIDX = chillerHeater.ChillerCapFTCoolingIDX;
        chillerHeater.ChillerEIRFTIDX = chillerHeater.ChillerEIRFTCoolingIDX;
        chillerHeater.ChillerEIRFPLRIDX = chillerHeater.ChillerEIRFPLRCoolingIDX;

        Real64 const moduleChilledWaterMassFlowRate =
            allocateConnectionFlow(remainingChilledWaterMassFlowRate, chillerHeater.ChilledWaterMassFlowRateMax, chillerHeater.EvapMassFlowRateMax);
        Real64 const moduleSourceMassFlowRate =
            allocateConnectionFlow(remainingSourceMassFlowRate, chillerHeater.SourceCondMassFlowRateMax, chillerHeater.CondMassFlowRateMax);
        ChillerHeaterResult result;
        if (moduleIsAvailable && remainingCoolingLoad > HVAC::SmallLoad &&
            moduleChilledWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance &&
            moduleSourceMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            result = this->solveCoolingOnly(state,
                                            chillerHeaterNum,
                                            remainingCoolingLoad,
                                            moduleChilledWaterMassFlowRate,
                                            moduleSourceMassFlowRate,
                                            chilledWaterInletTemp,
                                            sourceInletTemp);
            chillerHeater.Result = result;
            chillerHeater.mapResultToPlantConnections();
            result = chillerHeater.Result;
        } else {
            result.requestedCoolingLoad = remainingCoolingLoad;
            result.unmetCoolingLoad = remainingCoolingLoad;
            result.evaporatorInletTemp = chilledWaterInletTemp;
            result.evaporatorOutletTemp = chilledWaterInletTemp;
            result.condenserInletTemp = sourceInletTemp;
            result.condenserOutletTemp = sourceInletTemp;
            result.chilledWaterInletTemp = chilledWaterInletTemp;
            result.chilledWaterOutletTemp = chilledWaterInletTemp;
            result.sourceInletTemp = sourceInletTemp;
            result.sourceOutletTemp = sourceInletTemp;
        }

        result.isAvailable = moduleIsAvailable;
        remainingCoolingLoad = max(0.0, remainingCoolingLoad - result.coolingDelivered);
        result.unmetCoolingLoad = remainingCoolingLoad;
        remainingChilledWaterMassFlowRate = max(0.0, remainingChilledWaterMassFlowRate - result.chilledWaterMassFlowRate);
        remainingSourceMassFlowRate = max(0.0, remainingSourceMassFlowRate - result.sourceMassFlowRate);
        chillerHeater.Result = result;
    }

    this->SimulClgDominant = false;
    this->SimulHtgDominant = false;
    this->updateWrapperReportingAndNodes(state,
                                         chilledWaterMassFlowRate,
                                         0.0,
                                         sourceMassFlowRate,
                                         chilledWaterInletTemp,
                                         state.dataLoopNodes->Node(this->HWInletNodeNum).Temp,
                                         sourceInletTemp,
                                         false);
}

void WrapperSpecs::CalcHeatingOnlyModel(EnergyPlusData &state,
                                        Real64 const hotWaterMassFlowRate,
                                        Real64 const sourceMassFlowRate,
                                        Real64 const hotWaterInletTemp,
                                        Real64 const sourceInletTemp)
{
    Real64 remainingHeatingLoad = this->WrapperHeatingLoad;
    Real64 remainingHotWaterMassFlowRate = max(0.0, hotWaterMassFlowRate);
    Real64 remainingSourceMassFlowRate = max(0.0, sourceMassFlowRate);
    int componentNum = 1;
    int unitsUsedInComponent = 0;

    auto allocateConnectionFlow = [](Real64 const remainingFlow, Real64 const connectionDesignFlow, Real64 const legacyDesignFlow) {
        Real64 designFlow = connectionDesignFlow;
        if (designFlow <= DataBranchAirLoopPlant::MassFlowTolerance) {
            designFlow = legacyDesignFlow;
        }
        if (designFlow <= DataBranchAirLoopPlant::MassFlowTolerance) {
            designFlow = remainingFlow;
        }
        return min(max(0.0, remainingFlow), max(0.0, designFlow));
    };

    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        bool moduleIsAvailable = true;
        if (this->NumOfComp > 0 && allocated(this->WrapperComp)) {
            while (componentNum <= this->NumOfComp && unitsUsedInComponent >= this->WrapperComp(componentNum).WrapperIdenticalObjectNum) {
                ++componentNum;
                unitsUsedInComponent = 0;
            }
            if (componentNum <= this->NumOfComp) {
                moduleIsAvailable =
                    this->WrapperComp(componentNum).chSched == nullptr || this->WrapperComp(componentNum).chSched->getCurrentVal() > 0.0;
                ++unitsUsedInComponent;
            } else {
                moduleIsAvailable = false;
            }
        }

        chillerHeater.RefCap = chillerHeater.RefCapClgHtg;
        chillerHeater.RefCOP = chillerHeater.RefCOPClgHtg;
        chillerHeater.TempRefEvapOut = chillerHeater.TempRefEvapOutClgHtg;
        chillerHeater.TempRefCondIn = chillerHeater.TempRefCondInClgHtg;
        chillerHeater.TempRefCondOut = chillerHeater.TempRefCondOutClgHtg;
        chillerHeater.OptPartLoadRat = chillerHeater.OptPartLoadRatClgHtg;
        chillerHeater.CondMode = chillerHeater.CondModeHeating;
        chillerHeater.ChillerCapFTIDX = chillerHeater.ChillerCapFTHeatingIDX;
        chillerHeater.ChillerEIRFTIDX = chillerHeater.ChillerEIRFTHeatingIDX;
        chillerHeater.ChillerEIRFPLRIDX = chillerHeater.ChillerEIRFPLRHeatingIDX;

        Real64 const moduleHotWaterMassFlowRate =
            allocateConnectionFlow(remainingHotWaterMassFlowRate, chillerHeater.HotWaterMassFlowRateMax, chillerHeater.CondMassFlowRateMax);
        Real64 const moduleSourceMassFlowRate =
            allocateConnectionFlow(remainingSourceMassFlowRate, chillerHeater.SourceEvapMassFlowRateMax, chillerHeater.EvapMassFlowRateMax);
        ChillerHeaterResult result;
        if (moduleIsAvailable && remainingHeatingLoad > HVAC::SmallLoad && moduleHotWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance &&
            moduleSourceMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            result = this->solveHeatingOnly(state,
                                            chillerHeaterNum,
                                            remainingHeatingLoad,
                                            moduleSourceMassFlowRate,
                                            moduleHotWaterMassFlowRate,
                                            sourceInletTemp,
                                            hotWaterInletTemp);
            chillerHeater.Result = result;
            chillerHeater.mapResultToPlantConnections();
            result = chillerHeater.Result;
        } else {
            result.requestedHeatingLoad = remainingHeatingLoad;
            result.unmetHeatingLoad = remainingHeatingLoad;
            result.evaporatorInletTemp = sourceInletTemp;
            result.evaporatorOutletTemp = sourceInletTemp;
            result.condenserInletTemp = hotWaterInletTemp;
            result.condenserOutletTemp = hotWaterInletTemp;
            result.hotWaterInletTemp = hotWaterInletTemp;
            result.hotWaterOutletTemp = hotWaterInletTemp;
            result.sourceInletTemp = sourceInletTemp;
            result.sourceOutletTemp = sourceInletTemp;
        }

        result.isAvailable = moduleIsAvailable;
        remainingHeatingLoad = max(0.0, remainingHeatingLoad - result.heatingDelivered);
        result.unmetHeatingLoad = remainingHeatingLoad;
        remainingHotWaterMassFlowRate = max(0.0, remainingHotWaterMassFlowRate - result.hotWaterMassFlowRate);
        remainingSourceMassFlowRate = max(0.0, remainingSourceMassFlowRate - result.sourceMassFlowRate);
        chillerHeater.Result = result;
    }

    this->SimulClgDominant = false;
    this->SimulHtgDominant = false;
    this->updateWrapperReportingAndNodes(state,
                                         0.0,
                                         hotWaterMassFlowRate,
                                         sourceMassFlowRate,
                                         state.dataLoopNodes->Node(this->CHWInletNodeNum).Temp,
                                         hotWaterInletTemp,
                                         sourceInletTemp,
                                         false);
}

void WrapperSpecs::updateWrapperReportingAndNodes(EnergyPlusData &state,
                                                  Real64 const chilledWaterMassFlowRate,
                                                  Real64 const hotWaterMassFlowRate,
                                                  Real64 const sourceMassFlowRate,
                                                  Real64 const chilledWaterInletTemp,
                                                  Real64 const hotWaterInletTemp,
                                                  Real64 const sourceInletTemp,
                                                  bool const simultaneousOperation)
{
    Real64 const secondsInTimeStep = state.dataHVACGlobal->TimeStepSysSec;
    Real64 totalCoolingRate = 0.0;
    Real64 totalHeatingRate = 0.0;
    Real64 totalSourceHeatTransfer = 0.0;
    Real64 totalCoolingPower = 0.0;
    Real64 totalHeatingPower = 0.0;
    Real64 usedChilledWaterMassFlowRate = 0.0;
    Real64 usedHotWaterMassFlowRate = 0.0;
    Real64 usedSourceMassFlowRate = 0.0;
    Real64 chilledWaterOutletTemperatureSum = 0.0;
    Real64 hotWaterOutletTemperatureSum = 0.0;
    Real64 sourceOutletTemperatureSum = 0.0;

    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        chillerHeater.updateResultEnergies(secondsInTimeStep, false);
        if (simultaneousOperation) {
            chillerHeater.saveCurrentResultForSimultaneous();
        }
        auto const &result = chillerHeater.Result;
        totalCoolingRate += result.coolingDelivered;
        totalHeatingRate += result.heatingDelivered;
        totalSourceHeatTransfer += result.sourceHeatTransfer;
        totalCoolingPower += result.coolingPower;
        totalHeatingPower += result.heatingPower;
        usedChilledWaterMassFlowRate += result.chilledWaterMassFlowRate;
        usedHotWaterMassFlowRate += result.hotWaterMassFlowRate;
        usedSourceMassFlowRate += result.sourceMassFlowRate;
        chilledWaterOutletTemperatureSum += result.chilledWaterOutletTemp * result.chilledWaterMassFlowRate;
        hotWaterOutletTemperatureSum += result.hotWaterOutletTemp * result.hotWaterMassFlowRate;
        sourceOutletTemperatureSum += result.sourceOutletTemp * result.sourceMassFlowRate;
    }

    auto mixConnection =
        [](Real64 const totalMassFlowRate, Real64 const usedMassFlowRate, Real64 const outletTemperatureSum, Real64 const inletTemp) {
            if (totalMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance || usedMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
                return inletTemp;
            }
            if (usedMassFlowRate > totalMassFlowRate) {
                return outletTemperatureSum / usedMassFlowRate;
            }
            Real64 const bypassMassFlowRate = totalMassFlowRate - usedMassFlowRate;
            return (outletTemperatureSum + bypassMassFlowRate * inletTemp) / totalMassFlowRate;
        };

    Real64 const chilledWaterOutletTemp =
        mixConnection(chilledWaterMassFlowRate, usedChilledWaterMassFlowRate, chilledWaterOutletTemperatureSum, chilledWaterInletTemp);
    Real64 const hotWaterOutletTemp = mixConnection(hotWaterMassFlowRate, usedHotWaterMassFlowRate, hotWaterOutletTemperatureSum, hotWaterInletTemp);
    Real64 const sourceOutletTemp = mixConnection(sourceMassFlowRate, usedSourceMassFlowRate, sourceOutletTemperatureSum, sourceInletTemp);

    if (this->AncillaryPower > 0.0) {
        Real64 const scheduleValue = this->ancillaryPowerSched != nullptr ? this->ancillaryPowerSched->getCurrentVal() : 1.0;
        Real64 const ancillaryPower = this->AncillaryPower * scheduleValue;
        if (totalHeatingRate > HVAC::SmallLoad && totalCoolingRate <= HVAC::SmallLoad) {
            totalHeatingPower += ancillaryPower;
        } else if (this->SimulHtgDominant) {
            totalHeatingPower += ancillaryPower;
        } else {
            totalCoolingPower += ancillaryPower;
        }
    }

    this->Report.Power = totalCoolingPower + totalHeatingPower;
    this->Report.CHWInletTemp = chilledWaterInletTemp;
    this->Report.CHWOutletTemp = chilledWaterOutletTemp;
    this->Report.HWInletTemp = hotWaterInletTemp;
    this->Report.HWOutletTemp = hotWaterOutletTemp;
    this->Report.GLHEInletTemp = sourceInletTemp;
    this->Report.GLHEOutletTemp = sourceOutletTemp;
    this->Report.CHWmdot = chilledWaterMassFlowRate;
    this->Report.HWmdot = hotWaterMassFlowRate;
    this->Report.GLHEmdot = sourceMassFlowRate;
    this->Report.TotElecCoolingPwr = totalCoolingPower;
    this->Report.TotElecHeatingPwr = totalHeatingPower;
    this->Report.CoolingRate = totalCoolingRate;
    this->Report.HeatingRate = totalHeatingRate;
    this->Report.GLHERate = totalSourceHeatTransfer;
    this->Report.TotElecCooling = totalCoolingPower * secondsInTimeStep;
    this->Report.TotElecHeating = totalHeatingPower * secondsInTimeStep;
    this->Report.CoolingEnergy = totalCoolingRate * secondsInTimeStep;
    this->Report.HeatingEnergy = totalHeatingRate * secondsInTimeStep;
    this->Report.GLHEEnergy = totalSourceHeatTransfer * secondsInTimeStep;

    if (simultaneousOperation) {
        this->Report.CHWInletTempSimul = chilledWaterInletTemp;
        this->Report.CHWOutletTempSimul = chilledWaterOutletTemp;
        this->Report.CHWmdotSimul = chilledWaterMassFlowRate;
        this->Report.GLHEInletTempSimul = sourceInletTemp;
        this->Report.GLHEOutletTempSimul = sourceOutletTemp;
        this->Report.GLHEmdotSimul = sourceMassFlowRate;
        this->Report.TotElecCoolingPwrSimul = totalCoolingPower;
        this->Report.TotElecCoolingSimul = totalCoolingPower * secondsInTimeStep;
        this->Report.CoolingRateSimul = totalCoolingRate;
        this->Report.CoolingEnergySimul = totalCoolingRate * secondsInTimeStep;
    }

    state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = chilledWaterOutletTemp;
    state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = hotWaterOutletTemp;
    state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = sourceOutletTemp;
}

void WrapperSpecs::CalcSimultaneousModel(EnergyPlusData &state,
                                         Real64 const chilledWaterMassFlowRate,
                                         Real64 const hotWaterMassFlowRate,
                                         Real64 const sourceMassFlowRate,
                                         Real64 const chilledWaterInletTemp,
                                         Real64 const hotWaterInletTemp,
                                         Real64 const sourceInletTemp)
{
    Real64 remainingCoolingLoad = this->WrapperCoolingLoad;
    Real64 remainingHeatingLoad = this->WrapperHeatingLoad;
    Real64 remainingChilledWaterMassFlowRate = max(0.0, chilledWaterMassFlowRate);
    Real64 remainingHotWaterMassFlowRate = max(0.0, hotWaterMassFlowRate);
    Real64 remainingSourceMassFlowRate = max(0.0, sourceMassFlowRate);

    int componentNum = 1;
    int unitsUsedInComponent = 0;
    for (int chillerHeaterNum = 1; chillerHeaterNum <= this->ChillerHeaterNums; ++chillerHeaterNum) {
        auto &chillerHeater = this->ChillerHeater(chillerHeaterNum);
        bool moduleIsAvailable = true;
        if (this->NumOfComp > 0 && allocated(this->WrapperComp)) {
            while (componentNum <= this->NumOfComp && unitsUsedInComponent >= this->WrapperComp(componentNum).WrapperIdenticalObjectNum) {
                ++componentNum;
                unitsUsedInComponent = 0;
            }
            if (componentNum <= this->NumOfComp) {
                moduleIsAvailable =
                    this->WrapperComp(componentNum).chSched == nullptr || this->WrapperComp(componentNum).chSched->getCurrentVal() > 0.0;
                ++unitsUsedInComponent;
            } else {
                moduleIsAvailable = false;
            }
        }

        chillerHeater.RefCap = chillerHeater.RefCapClgHtg;
        chillerHeater.RefCOP = chillerHeater.RefCOPClgHtg;
        chillerHeater.TempRefEvapOut = chillerHeater.TempRefEvapOutClgHtg;
        chillerHeater.TempRefCondIn = chillerHeater.TempRefCondInClgHtg;
        chillerHeater.TempRefCondOut = chillerHeater.TempRefCondOutClgHtg;
        chillerHeater.OptPartLoadRat = chillerHeater.OptPartLoadRatClgHtg;
        chillerHeater.CondMode = chillerHeater.CondModeHeating;
        chillerHeater.ChillerCapFTIDX = chillerHeater.ChillerCapFTHeatingIDX;
        chillerHeater.ChillerEIRFTIDX = chillerHeater.ChillerEIRFTHeatingIDX;
        chillerHeater.ChillerEIRFPLRIDX = chillerHeater.ChillerEIRFPLRHeatingIDX;

        auto moduleFlowLimit = [](Real64 const remainingFlow, Real64 const connectionDesignFlow, Real64 const legacyDesignFlow) {
            Real64 flowLimit = connectionDesignFlow;
            if (flowLimit <= DataBranchAirLoopPlant::MassFlowTolerance) {
                flowLimit = legacyDesignFlow;
            }
            if (flowLimit <= DataBranchAirLoopPlant::MassFlowTolerance) {
                flowLimit = remainingFlow;
            }
            return min(remainingFlow, max(0.0, flowLimit));
        };

        Real64 const moduleChilledWaterMassFlowRate =
            moduleFlowLimit(remainingChilledWaterMassFlowRate, chillerHeater.ChilledWaterMassFlowRateMax, chillerHeater.EvapMassFlowRateMax);
        Real64 const moduleHotWaterMassFlowRate =
            moduleFlowLimit(remainingHotWaterMassFlowRate, chillerHeater.HotWaterMassFlowRateMax, chillerHeater.CondMassFlowRateMax);
        Real64 moduleSourceMassFlowRate = 0.0;
        ChillerHeaterResult result;

        if (moduleIsAvailable && remainingCoolingLoad > HVAC::SmallLoad && remainingHeatingLoad > HVAC::SmallLoad &&
            moduleChilledWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance &&
            moduleHotWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            moduleSourceMassFlowRate = moduleFlowLimit(remainingSourceMassFlowRate,
                                                       max(chillerHeater.SourceEvapMassFlowRateMax, chillerHeater.SourceCondMassFlowRateMax),
                                                       max(chillerHeater.EvapMassFlowRateMax, chillerHeater.CondMassFlowRateMax));
            result = this->solveSimultaneous(state,
                                             chillerHeaterNum,
                                             remainingCoolingLoad,
                                             remainingHeatingLoad,
                                             moduleChilledWaterMassFlowRate,
                                             moduleHotWaterMassFlowRate,
                                             moduleSourceMassFlowRate,
                                             chilledWaterInletTemp,
                                             hotWaterInletTemp,
                                             sourceInletTemp);
        } else if (moduleIsAvailable && remainingCoolingLoad > HVAC::SmallLoad &&
                   moduleChilledWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            moduleSourceMassFlowRate =
                moduleFlowLimit(remainingSourceMassFlowRate, chillerHeater.SourceCondMassFlowRateMax, chillerHeater.CondMassFlowRateMax);
            result = this->solveCoolingOnly(state,
                                            chillerHeaterNum,
                                            remainingCoolingLoad,
                                            moduleChilledWaterMassFlowRate,
                                            moduleSourceMassFlowRate,
                                            chilledWaterInletTemp,
                                            sourceInletTemp);
            chillerHeater.Result = result;
            chillerHeater.mapResultToPlantConnections();
            result = chillerHeater.Result;
            if (result.currentMode == CurrentMode::CoolingOnly) {
                result.currentMode = CurrentMode::CoolingDominant;
            }
            result.requestedHeatingLoad = remainingHeatingLoad;
            result.unmetHeatingLoad = remainingHeatingLoad;
        } else if (moduleIsAvailable && remainingHeatingLoad > HVAC::SmallLoad &&
                   moduleHotWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            moduleSourceMassFlowRate =
                moduleFlowLimit(remainingSourceMassFlowRate, chillerHeater.SourceEvapMassFlowRateMax, chillerHeater.EvapMassFlowRateMax);
            result = this->solveHeatingOnly(state,
                                            chillerHeaterNum,
                                            remainingHeatingLoad,
                                            moduleSourceMassFlowRate,
                                            moduleHotWaterMassFlowRate,
                                            sourceInletTemp,
                                            hotWaterInletTemp);
            chillerHeater.Result = result;
            chillerHeater.mapResultToPlantConnections();
            result = chillerHeater.Result;
            if (result.currentMode == CurrentMode::HeatingOnly) {
                result.currentMode = CurrentMode::HeatingDominant;
            }
            result.requestedCoolingLoad = remainingCoolingLoad;
            result.unmetCoolingLoad = remainingCoolingLoad;
        } else {
            result.requestedCoolingLoad = remainingCoolingLoad;
            result.requestedHeatingLoad = remainingHeatingLoad;
            result.unmetCoolingLoad = remainingCoolingLoad;
            result.unmetHeatingLoad = remainingHeatingLoad;
            result.chilledWaterInletTemp = chilledWaterInletTemp;
            result.chilledWaterOutletTemp = chilledWaterInletTemp;
            result.hotWaterInletTemp = hotWaterInletTemp;
            result.hotWaterOutletTemp = hotWaterInletTemp;
            result.sourceInletTemp = sourceInletTemp;
            result.sourceOutletTemp = sourceInletTemp;
        }

        result.isAvailable = moduleIsAvailable;
        remainingCoolingLoad = max(0.0, remainingCoolingLoad - result.coolingDelivered);
        remainingHeatingLoad = max(0.0, remainingHeatingLoad - result.heatingDelivered);
        result.unmetCoolingLoad = remainingCoolingLoad;
        result.unmetHeatingLoad = remainingHeatingLoad;
        remainingChilledWaterMassFlowRate = max(0.0, remainingChilledWaterMassFlowRate - result.chilledWaterMassFlowRate);
        remainingHotWaterMassFlowRate = max(0.0, remainingHotWaterMassFlowRate - result.hotWaterMassFlowRate);
        remainingSourceMassFlowRate = max(0.0, remainingSourceMassFlowRate - result.sourceMassFlowRate);

        chillerHeater.Result = result;
    }

    Real64 totalCoolingRate = 0.0;
    Real64 totalHeatingRate = 0.0;
    Real64 totalSourceHeatTransfer = 0.0;
    for (auto const &chillerHeater : this->ChillerHeater) {
        totalCoolingRate += chillerHeater.Result.coolingDelivered;
        totalHeatingRate += chillerHeater.Result.heatingDelivered;
        totalSourceHeatTransfer += chillerHeater.Result.sourceHeatTransfer;
    }
    Real64 const sourceModeTolerance = max(HVAC::SmallLoad, 1.0e-8 * max({totalCoolingRate, totalHeatingRate, std::abs(totalSourceHeatTransfer)}));
    this->SimulClgDominant = totalSourceHeatTransfer > sourceModeTolerance;
    this->SimulHtgDominant = totalSourceHeatTransfer < -sourceModeTolerance;
    this->updateWrapperReportingAndNodes(
        state, chilledWaterMassFlowRate, hotWaterMassFlowRate, sourceMassFlowRate, chilledWaterInletTemp, hotWaterInletTemp, sourceInletTemp, true);
}

void WrapperSpecs::CalcWrapperModel(EnergyPlusData &state, Real64 &MyLoad, int const LoopNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  Calculate node information connected to plant & condenser loop

    // METHODOLOGY EMPLOYED:
    //  Use empirical curve fits to model performance at off-reference conditions

    Real64 CurHeatingLoad = 0.0; // Total heating load chiller heater bank (wrapper) meets

    // Chiller heater bank chilled water inlet mass flow rate
    Real64 CHWInletMassFlowRate = 0.0;

    Real64 HWInletMassFlowRate = 0.0;
    Real64 GLHEInletMassFlowRate = 0.0;
    Real64 CHWInletTemp = state.dataLoopNodes->Node(this->CHWInletNodeNum).Temp;

    // Chiller heater bank hot water inlet temperature
    Real64 HWInletTemp = state.dataLoopNodes->Node(this->HWInletNodeNum).Temp;

    // Chiller heater bank condenser loop inlet temperature
    Real64 GLHEInletTemp = state.dataLoopNodes->Node(this->GLHEInletNodeNum).Temp;

    Real64 CurCoolingLoad = 0.0; // Total cooling load chiller heater bank (wrapper) meets

    // Initiate loads and inlet temperatures each loop
    if (LoopNum == this->CWPlantLoc.loopNum) {
        CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;
        HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;
        GLHEInletMassFlowRate = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRate;
        this->WrapperCoolingLoad = 0.0;
        CurCoolingLoad = std::abs(MyLoad);
        this->WrapperCoolingLoad = CurCoolingLoad;
        if (CHWInletMassFlowRate == 0.0) {
            GLHEInletMassFlowRate = 0.0;
        }

    } else if (LoopNum == this->HWPlantLoc.loopNum) {
        CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;
        HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;
        GLHEInletMassFlowRate = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRate;
        this->WrapperHeatingLoad = 0.0;
        CurHeatingLoad = MyLoad;
        this->WrapperHeatingLoad = CurHeatingLoad;
        if (HWInletMassFlowRate == 0.0) {
            GLHEInletMassFlowRate = 0.0;
        }
    }

    if (this->WrapperCoolingLoad > HVAC::SmallLoad && this->WrapperHeatingLoad > HVAC::SmallLoad) {
        CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;
        HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;
        GLHEInletMassFlowRate = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRate;

        this->CalcSimultaneousModel(
            state, CHWInletMassFlowRate, HWInletMassFlowRate, GLHEInletMassFlowRate, CHWInletTemp, HWInletTemp, GLHEInletTemp);

        PlantUtilities::SetComponentFlowRate(state, CHWInletMassFlowRate, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, HWInletMassFlowRate, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, GLHEInletMassFlowRate, this->GLHEInletNodeNum, this->GLHEOutletNodeNum, this->GLHEPlantLoc);

        MyLoad = LoopNum == this->CWPlantLoc.loopNum ? -this->Report.CoolingRate : this->Report.HeatingRate;
        return;
    }

    this->SimulClgDominant = false;
    this->SimulHtgDominant = false;

    if (LoopNum == this->CWPlantLoc.loopNum && CurCoolingLoad > HVAC::SmallLoad) {
        this->CalcCoolingOnlyModel(state, CHWInletMassFlowRate, GLHEInletMassFlowRate, CHWInletTemp, GLHEInletTemp);
        PlantUtilities::SetComponentFlowRate(state, CHWInletMassFlowRate, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, HWInletMassFlowRate, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, GLHEInletMassFlowRate, this->GLHEInletNodeNum, this->GLHEOutletNodeNum, this->GLHEPlantLoc);
        MyLoad = -this->Report.CoolingRate;
        return;
    }

    if (LoopNum == this->HWPlantLoc.loopNum && CurHeatingLoad > HVAC::SmallLoad) {
        this->CalcHeatingOnlyModel(state, HWInletMassFlowRate, GLHEInletMassFlowRate, HWInletTemp, GLHEInletTemp);
        PlantUtilities::SetComponentFlowRate(state, CHWInletMassFlowRate, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, HWInletMassFlowRate, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWPlantLoc);
        PlantUtilities::SetComponentFlowRate(state, GLHEInletMassFlowRate, this->GLHEInletNodeNum, this->GLHEOutletNodeNum, this->GLHEPlantLoc);
        MyLoad = this->Report.HeatingRate;
        return;
    }

    this->resetOffState(state);
    MyLoad = 0.0;
    return;
}

void WrapperSpecs::oneTimeInit_new([[maybe_unused]] EnergyPlusData &state)
{
}

void WrapperSpecs::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
{
}

} // namespace EnergyPlus::PlantCentralGSHP
