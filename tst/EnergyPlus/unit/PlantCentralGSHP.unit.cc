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

// EnergyPlus::PlantCentralGSHP Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include <algorithm>
#include <array>
#include <cmath>
#include <numeric>
#include <string_view>
#include <vector>

using namespace EnergyPlus;

namespace {

using EnergyPlus::PlantCentralGSHP::CurrentMode;

struct LoopRouting
{
    Real64 cooling = 0.0; // Heat removed from the chilled-water loop [W]
    Real64 heating = 0.0; // Heat added to the hot-water loop [W]
    Real64 source = 0.0;  // Positive rejects heat to source; negative extracts heat from source [W]
};

struct ModeEnergyContract
{
    CurrentMode mode = CurrentMode::Invalid;
    Real64 qEvaporator = 0.0;
    Real64 qCondenser = 0.0;
    Real64 compressorPower = 0.0;
    Real64 falseLoad = 0.0;
    Real64 openMotorEfficiency = 1.0;
    Real64 partLoadRatio = 0.0;
    Real64 cyclingRatio = 0.0;
    LoopRouting routing;
};

struct PartLoadContract
{
    Real64 availableCapacity = 0.0;
    Real64 requestedLoad = 0.0;
    Real64 deliveredLoad = 0.0;
    Real64 minimumPartLoadRatio = 0.0;
    Real64 maximumPartLoadRatio = 1.0;
    Real64 reportedPartLoadRatio = 0.0;
    Real64 cyclingRatio = 0.0;
    Real64 curveEvaluationPartLoadRatio = 0.0;
};

struct FlowAllocationContract
{
    Real64 wrapperAvailableFlow = 0.0;
    std::vector<Real64> requestedFlows;
    std::vector<Real64> moduleMaximumFlows;
    std::vector<Real64> allocatedFlows;
};

constexpr Real64 contractTolerance = 1.0e-9;

bool contractNear(Real64 const actual, Real64 const expected, Real64 const tolerance = contractTolerance)
{
    Real64 const scale = std::max({1.0, std::abs(actual), std::abs(expected)});
    return std::abs(actual - expected) <= tolerance * scale;
}

std::string_view modeName(CurrentMode const mode)
{
    switch (mode) {
    case CurrentMode::Off:
        return "Off";
    case CurrentMode::CoolingOnly:
        return "CoolingOnly";
    case CurrentMode::HeatingOnly:
        return "HeatingOnly";
    case CurrentMode::HeatRecovery:
        return "HeatRecovery";
    case CurrentMode::CoolingDominant:
        return "CoolingDominant";
    case CurrentMode::HeatingDominant:
        return "HeatingDominant";
    default:
        return "Invalid";
    }
}

::testing::AssertionResult checkModeEnergyContract(ModeEnergyContract const &point)
{
    auto fail = [&point](std::string_view const message) { return ::testing::AssertionFailure() << modeName(point.mode) << ": " << message; };

    if (point.mode == CurrentMode::Invalid || point.mode == CurrentMode::Num) {
        return fail("invalid operating mode");
    }

    std::array<Real64, 10> const values = {point.qEvaporator,
                                           point.qCondenser,
                                           point.compressorPower,
                                           point.falseLoad,
                                           point.openMotorEfficiency,
                                           point.partLoadRatio,
                                           point.cyclingRatio,
                                           point.routing.cooling,
                                           point.routing.heating,
                                           point.routing.source};
    if (!std::all_of(values.begin(), values.end(), [](Real64 const value) { return std::isfinite(value); })) {
        return fail("contract contains a non-finite value");
    }
    if (point.qEvaporator < 0.0 || point.qCondenser < 0.0 || point.compressorPower < 0.0 || point.falseLoad < 0.0) {
        return fail("heat-transfer rates, power, and false load must be nonnegative");
    }
    if (point.openMotorEfficiency < 0.0 || point.openMotorEfficiency > 1.0) {
        return fail("open motor efficiency is outside [0, 1]");
    }

    if (point.mode == CurrentMode::Off) {
        Real64 const magnitude = point.qEvaporator + point.qCondenser + point.compressorPower + point.falseLoad + std::abs(point.routing.cooling) +
                                 std::abs(point.routing.heating) + std::abs(point.routing.source) + std::abs(point.partLoadRatio) +
                                 std::abs(point.cyclingRatio);
        if (!contractNear(magnitude, 0.0)) {
            return fail("off mode has nonzero heat transfer, power, routing, PLR, or cycling");
        }
        return ::testing::AssertionSuccess();
    }

    if (point.compressorPower <= 0.0) {
        return fail("an active Electric:EIR operating point must have positive compressor power");
    }
    if (point.partLoadRatio <= 0.0 || point.partLoadRatio > 1.0) {
        return fail("active-mode PLR is outside (0, 1]");
    }
    if (point.cyclingRatio <= 0.0 || point.cyclingRatio > 1.0) {
        return fail("active-mode cycling ratio is outside (0, 1]");
    }

    Real64 const refrigerantPower = point.compressorPower * point.openMotorEfficiency;
    Real64 const moduleResidual = point.qCondenser - point.qEvaporator - refrigerantPower - point.falseLoad;
    if (!contractNear(moduleResidual, 0.0)) {
        return fail("module energy residual is " + std::to_string(moduleResidual) + " W");
    }

    if (point.routing.cooling < 0.0 || point.routing.heating < 0.0) {
        return fail("useful cooling and heating routing must be nonnegative");
    }

    switch (point.mode) {
    case CurrentMode::CoolingOnly:
        if (!contractNear(point.routing.cooling, point.qEvaporator) || !contractNear(point.routing.heating, 0.0) ||
            !contractNear(point.routing.source, point.qCondenser)) {
            return fail("cooling-only heat is not fully routed to chilled water and the source loop");
        }
        break;
    case CurrentMode::HeatingOnly:
        if (!contractNear(point.routing.cooling, 0.0) || !contractNear(point.routing.heating, point.qCondenser) ||
            !contractNear(point.routing.source, -point.qEvaporator)) {
            return fail("heating-only heat is not fully routed from the source loop to hot water");
        }
        break;
    case CurrentMode::HeatRecovery:
        if (!contractNear(point.routing.cooling, point.qEvaporator) || !contractNear(point.routing.heating, point.qCondenser) ||
            !contractNear(point.routing.source, 0.0)) {
            return fail("balanced heat recovery must route the complete evaporator and condenser loads with zero source transfer");
        }
        break;
    case CurrentMode::CoolingDominant:
        if (point.routing.heating <= 0.0 || point.routing.heating >= point.qCondenser || !contractNear(point.routing.cooling, point.qEvaporator) ||
            !contractNear(point.routing.source, point.qCondenser - point.routing.heating)) {
            return fail("cooling-dominant operation must recover part of condenser heat and reject the residual to source");
        }
        break;
    case CurrentMode::HeatingDominant:
        if (point.routing.cooling <= 0.0 || point.routing.cooling >= point.qEvaporator || !contractNear(point.routing.heating, point.qCondenser) ||
            !contractNear(point.routing.source, -(point.qEvaporator - point.routing.cooling))) {
            return fail("heating-dominant operation must preserve useful cooling and extract only the residual from source");
        }
        break;
    default:
        return fail("unsupported operating mode");
    }

    Real64 const wrapperResidual = point.routing.heating + point.routing.source - point.routing.cooling - refrigerantPower - point.falseLoad;
    if (!contractNear(wrapperResidual, 0.0)) {
        return fail("three-loop routing energy residual is " + std::to_string(wrapperResidual) + " W");
    }

    return ::testing::AssertionSuccess();
}

::testing::AssertionResult checkPartLoadContract(PartLoadContract const &point)
{
    if (point.availableCapacity <= 0.0) {
        return ::testing::AssertionFailure() << "available capacity must be positive";
    }
    if (point.minimumPartLoadRatio <= 0.0 || point.maximumPartLoadRatio < point.minimumPartLoadRatio) {
        return ::testing::AssertionFailure() << "invalid PLR bounds";
    }

    Real64 const requestedPLR = std::max(0.0, point.requestedLoad / point.availableCapacity);
    Real64 const expectedPLR = std::clamp(requestedPLR, point.minimumPartLoadRatio, point.maximumPartLoadRatio);
    Real64 const expectedCycling = requestedPLR < point.minimumPartLoadRatio ? requestedPLR / point.minimumPartLoadRatio : 1.0;
    Real64 const expectedDeliveredLoad = std::min(point.requestedLoad, point.availableCapacity * point.maximumPartLoadRatio);

    if (!contractNear(point.reportedPartLoadRatio, expectedPLR)) {
        return ::testing::AssertionFailure() << "reported PLR " << point.reportedPartLoadRatio << " does not equal final operating PLR "
                                             << expectedPLR;
    }
    if (!contractNear(point.cyclingRatio, expectedCycling)) {
        return ::testing::AssertionFailure() << "cycling ratio " << point.cyclingRatio << " does not equal " << expectedCycling;
    }
    if (!contractNear(point.deliveredLoad, expectedDeliveredLoad)) {
        return ::testing::AssertionFailure() << "delivered load " << point.deliveredLoad << " does not equal " << expectedDeliveredLoad;
    }
    if (!contractNear(point.curveEvaluationPartLoadRatio, point.reportedPartLoadRatio)) {
        return ::testing::AssertionFailure() << "EIRFPLR was evaluated at " << point.curveEvaluationPartLoadRatio << " instead of final reported PLR "
                                             << point.reportedPartLoadRatio;
    }
    return ::testing::AssertionSuccess();
}

::testing::AssertionResult checkSequentialFlowAllocation(FlowAllocationContract const &flow)
{
    if (flow.requestedFlows.size() != flow.moduleMaximumFlows.size() || flow.requestedFlows.size() != flow.allocatedFlows.size()) {
        return ::testing::AssertionFailure() << "flow vectors have different sizes";
    }
    if (flow.wrapperAvailableFlow < 0.0) {
        return ::testing::AssertionFailure() << "wrapper available flow is negative";
    }

    Real64 remainingFlow = flow.wrapperAvailableFlow;
    for (std::size_t module = 0; module < flow.allocatedFlows.size(); ++module) {
        Real64 const expected = std::min({flow.requestedFlows[module], flow.moduleMaximumFlows[module], remainingFlow});
        if (!contractNear(flow.allocatedFlows[module], expected)) {
            return ::testing::AssertionFailure() << "module " << module + 1 << " allocation " << flow.allocatedFlows[module]
                                                 << " does not equal sequentially available flow " << expected;
        }
        remainingFlow -= expected;
    }

    Real64 const totalAllocated = std::accumulate(flow.allocatedFlows.begin(), flow.allocatedFlows.end(), 0.0);
    if (totalAllocated > flow.wrapperAvailableFlow && !contractNear(totalAllocated, flow.wrapperAvailableFlow)) {
        return ::testing::AssertionFailure() << "module flow sum " << totalAllocated << " exceeds wrapper flow " << flow.wrapperAvailableFlow;
    }
    return ::testing::AssertionSuccess();
}

::testing::AssertionResult
checkLoopHeatTransfer(Real64 const reportedHeat, Real64 const massFlow, Real64 const specificHeat, Real64 const inletTemp, Real64 const outletTemp)
{
    if (reportedHeat < 0.0 || massFlow < 0.0 || specificHeat <= 0.0) {
        return ::testing::AssertionFailure() << "invalid loop heat-transfer input";
    }
    Real64 const nodeHeat = massFlow * specificHeat * std::abs(outletTemp - inletTemp);
    if (!contractNear(reportedHeat, nodeHeat)) {
        return ::testing::AssertionFailure() << "reported heat " << reportedHeat << " W does not match node heat " << nodeHeat << " W";
    }
    return ::testing::AssertionSuccess();
}

std::string makeChillerHeaterValidationInput(Real64 const capacityRatio = 0.75,
                                             Real64 const coolingOptimumPLR = 0.5,
                                             Real64 const heatingOptimumPLR = 0.5,
                                             Real64 const maximumHeatingLeavingTemp = 55.0,
                                             Real64 const coolingMinimumPLR = 0.2)
{
    std::vector<std::string> const lines{
        "ChillerHeaterPerformance:Electric:EIR,",
        "  Validation Module,",
        "  10000,",
        "  5.0,",
        "  7.0,",
        "  30.0,",
        "  35.0,",
        "  " + std::to_string(capacityRatio) + ",",
        "  1.0,",
        "  7.0,",
        "  50.0,",
        "  30.0,",
        "  3.0,",
        "  VariableFlow,",
        "  0.001,",
        "  0.001,",
        "  0.001,",
        "  0.8,",
        "  LeavingCondenser,",
        "  Cooling Reference Curve,",
        "  Cooling Reference Curve,",
        "  Cooling PLR Curve,",
        "  " + std::to_string(coolingOptimumPLR) + ",",
        "  LeavingCondenser,",
        "  Heating Reference Curve,",
        "  Heating Reference Curve,",
        "  Heating PLR Curve,",
        "  " + std::to_string(heatingOptimumPLR) + ",",
        "  1.0,",
        "  " + std::to_string(maximumHeatingLeavingTemp) + ";",

        "Curve:Biquadratic,",
        "  Cooling Reference Curve,",
        "  0.0, 0.0, 0.0, 0.0285714285714286, 0.0, 0.0,",
        "  -100.0, 100.0, -100.0, 100.0;",

        "Curve:Biquadratic,",
        "  Heating Reference Curve,",
        "  0.0, 0.0, 0.0, 0.02, 0.0, 0.0,",
        "  -100.0, 100.0, -100.0, 100.0;",

        "Curve:Bicubic,",
        "  Cooling PLR Curve,",
        "  0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,",
        "  -100.0, 100.0,",
        "  " + std::to_string(coolingMinimumPLR) + ", 1.0;",

        "Curve:Quadratic,",
        "  Heating PLR Curve,",
        "  1.0, 0.0, 0.0,",
        "  0.3, 1.0;",
    };
    return std::accumulate(lines.begin(), lines.end(), std::string(), [](std::string result, std::string const &line) {
        result += line;
        result += '\n';
        return result;
    });
}

nlohmann::json makeChillerHeaterNativeJSON(bool const includeOptionalFields = true, bool const autosize = false)
{
    nlohmann::json performance = {
        {"reference_cooling_mode_evaporator_capacity", autosize ? nlohmann::json("Autosize") : nlohmann::json(10000.0)},
        {"reference_cooling_mode_cop", 5.0},
        {"cooling_mode_cooling_capacity_function_of_temperature_curve_name", "Reference Temperature Curve"},
        {"cooling_mode_electric_input_to_cooling_output_ratio_function_of_temperature_curve_name", "Reference Temperature Curve"},
        {"cooling_mode_electric_input_to_cooling_output_ratio_function_of_part_load_ratio_curve_name", "Reference PLR Curve"},
        {"heating_mode_cooling_capacity_function_of_temperature_curve_name", "Reference Temperature Curve"},
        {"heating_mode_electric_input_to_cooling_output_ratio_function_of_temperature_curve_name", "Reference Temperature Curve"},
        {"heating_mode_electric_input_to_cooling_output_ratio_function_of_part_load_ratio_curve_name", "Reference PLR Curve"},
    };

    if (includeOptionalFields) {
        performance.update({
            {"reference_cooling_mode_leaving_chilled_water_temperature", 7.0},
            {"reference_cooling_mode_entering_condenser_fluid_temperature", 30.0},
            {"reference_cooling_mode_leaving_condenser_water_temperature", 35.0},
            {"reference_heating_mode_cooling_capacity_ratio", 0.75},
            {"reference_heating_mode_cooling_power_input_ratio", 1.0},
            {"reference_heating_mode_leaving_chilled_water_temperature", 7.0},
            {"reference_heating_mode_leaving_condenser_water_temperature", 50.0},
            {"reference_heating_mode_entering_condenser_fluid_temperature", 30.0},
            {"heating_mode_entering_chilled_water_temperature_low_limit", 3.0},
            {"chilled_water_flow_mode_type", "VariableFlow"},
            {"design_chilled_water_flow_rate", autosize ? nlohmann::json("Autosize") : nlohmann::json(0.001)},
            {"design_condenser_water_flow_rate", autosize ? nlohmann::json("Autosize") : nlohmann::json(0.001)},
            {"design_hot_water_flow_rate", 0.001},
            {"compressor_motor_efficiency", 0.8},
            {"cooling_mode_temperature_curve_condenser_water_independent_variable", "LeavingCondenser"},
            {"cooling_mode_cooling_capacity_optimum_part_load_ratio", 0.5},
            {"heating_mode_temperature_curve_condenser_water_independent_variable", "EnteringCondenser"},
            {"heating_mode_cooling_capacity_optimum_part_load_ratio", 0.5},
            {"sizing_factor", 1.2},
            {"maximum_heating_mode_leaving_condenser_water_temperature", 55.0},
        });
    }

    nlohmann::json const temperatureCurve = {
        {"coefficient1_constant", 1.0},
        {"coefficient2_x", 0.0},
        {"coefficient3_x_2", 0.0},
        {"coefficient4_y", 0.0},
        {"coefficient5_y_2", 0.0},
        {"coefficient6_x_y", 0.0},
        {"minimum_value_of_x", -100.0},
        {"maximum_value_of_x", 100.0},
        {"minimum_value_of_y", -100.0},
        {"maximum_value_of_y", 100.0},
    };
    nlohmann::json const partLoadCurve = {
        {"coefficient1_constant", 1.0},
        {"coefficient2_x", 0.0},
        {"coefficient3_x_2", 0.0},
        {"minimum_value_of_x", 0.0},
        {"maximum_value_of_x", 1.0},
    };

    return {
        {"ChillerHeaterPerformance:Electric:EIR", {{"Native Mixed Case Module", performance}}},
        {"Curve:Biquadratic", {{"Reference Temperature Curve", temperatureCurve}}},
        {"Curve:Quadratic", {{"Reference PLR Curve", partLoadCurve}}},
    };
}

} // namespace

TEST_F(EnergyPlusFixture, ChillerHeater_Autosize)
{
    state->init_state(*state);
    // Allocate One Wrapper with One module (=distinct ChillerHeaterPerformance:Electric:EIR)
    // but with a number of identical number module of 2 in CentralHeatPumpSystem
    int NumWrappers = 1;
    state->dataPlantCentralGSHP->numWrappers = NumWrappers;
    state->dataPlantCentralGSHP->Wrapper.allocate(NumWrappers);

    int NumberOfComp = 1;
    state->dataPlantCentralGSHP->Wrapper(1).NumOfComp = NumberOfComp;
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(NumberOfComp);

    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).WrapperPerformanceObjectType = "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR";
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).WrapperIdenticalObjectNum = 2;
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).chSched = Sched::GetScheduleAlwaysOn(*state);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeaterNums = 2;
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(2);

    int NumChillerHeaters = 1;
    state->dataPlantCentralGSHP->numPerformanceDefinitions = NumChillerHeaters;
    state->dataPlantCentralGSHP->performanceDefinitions.allocate(NumChillerHeaters);
    state->dataPlantCentralGSHP->performanceDefinitions(1).ConstantFlow = false;
    state->dataPlantCentralGSHP->performanceDefinitions(1).VariableFlow = true;

    state->dataPlantCentralGSHP->performanceDefinitions(1).SizFac = 1.2;

    state->dataPlantCentralGSHP->performanceDefinitions(1).RefCapCooling = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->performanceDefinitions(1).RefCapCoolingWasAutoSized = true;

    state->dataPlantCentralGSHP->performanceDefinitions(1).EvapVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->performanceDefinitions(1).EvapVolFlowRateWasAutoSized = true;

    state->dataPlantCentralGSHP->performanceDefinitions(1).CondVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->performanceDefinitions(1).CondVolFlowRateWasAutoSized = true;

    // Needed for calcs
    state->dataPlantCentralGSHP->performanceDefinitions(1).RefCOPCooling = 1.5;
    state->dataPlantCentralGSHP->performanceDefinitions(1).OpenMotorEff = 0.98;
    state->dataPlantCentralGSHP->performanceDefinitions(1).TempRefCondInCooling = 29.4;
    state->dataPlantCentralGSHP->performanceDefinitions(1).ClgHtgToCoolingCapRatio = 0.74;
    state->dataPlantCentralGSHP->performanceDefinitions(1).ClgHtgtoCogPowerRatio = 1.38;

    // Both modules share one retained immutable performance definition.
    auto const &performance = state->dataPlantCentralGSHP->performanceDefinitions(1);
    auto *availabilitySchedule = state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).chSched;
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).initialize(1, performance, availabilitySchedule);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2).initialize(1, performance, availabilitySchedule);
    auto &module1 = state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1);
    auto &module2 = state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2);
    EXPECT_EQ(&performance, module1.performance);
    EXPECT_EQ(&performance, module2.performance);
    EXPECT_EQ(1, module1.performanceIndex);
    EXPECT_EQ(1, module2.performanceIndex);
    EXPECT_EQ(availabilitySchedule, module1.availabilitySchedule);
    EXPECT_EQ(availabilitySchedule, module2.availabilitySchedule);
    module1.Result.qEvaporator = 100.0;
    EXPECT_DOUBLE_EQ(0.0, module2.Result.qEvaporator);

    state->dataPlnt->PlantLoop.allocate(2);
    state->dataSize->PlantSizData.allocate(2);

    // Chilled Water Loop
    int PltSizNum = 1;
    state->dataPlnt->PlantLoop(PltSizNum).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(PltSizNum).FluidName = "WATER";
    state->dataPlnt->PlantLoop(PltSizNum).glycol = Fluid::GetWater(*state);
    state->dataSize->PlantSizData(PltSizNum).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(PltSizNum).DeltaT = 10.0;
    state->dataSize->PlantSizData(PltSizNum).LoopType = DataSizing::TypeOfPlantLoop::Cooling;
    // Assign to the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).CWPlantLoc.loopNum = PltSizNum;
    PlantUtilities::SetPlantLocationLinks(*state, state->dataPlantCentralGSHP->Wrapper(1).CWPlantLoc);

    // Condenser Loop
    int PltSizCondNum = 2;
    state->dataPlnt->PlantLoop(PltSizCondNum).PlantSizNum = PltSizCondNum;
    state->dataPlnt->PlantLoop(PltSizCondNum).FluidName = "WATER";
    state->dataPlnt->PlantLoop(PltSizCondNum).glycol = Fluid::GetWater(*state);
    state->dataSize->PlantSizData(PltSizCondNum).DeltaT = 5.6;
    state->dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(PltSizCondNum).LoopType = DataSizing::TypeOfPlantLoop::Condenser;
    // Assign to the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).GLHEPlantLoc.loopNum = PltSizCondNum;
    PlantUtilities::SetPlantLocationLinks(*state, state->dataPlantCentralGSHP->Wrapper(1).GLHEPlantLoc);
    state->dataPlantCentralGSHP->Wrapper(1).CHWInletNodeNum = 1;
    state->dataPlantCentralGSHP->Wrapper(1).HWInletNodeNum = 2;
    state->dataPlantCentralGSHP->Wrapper(1).GLHEInletNodeNum = 3;

    // Calculate expected values
    Real64 rho_evap = state->dataPlnt->PlantLoop(PltSizNum).glycol->getDensity(*state, Constant::CWInitConvTemp, "ChillerHeater_Autosize_TEST");

    Real64 Cp_evap = state->dataPlnt->PlantLoop(PltSizNum).glycol->getSpecificHeat(*state, Constant::CWInitConvTemp, "ChillerHeater_Autosize_TEST");

    Real64 rho_cond = state->dataPlnt->PlantLoop(PltSizCondNum)
                          .glycol->getDensity(*state,
                                              state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().TempRefCondInCooling,
                                              "ChillerHeater_Autosize_TEST");

    Real64 Cp_cond = state->dataPlnt->PlantLoop(PltSizCondNum)
                         .glycol->getSpecificHeat(*state,
                                                  state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().TempRefCondInCooling,
                                                  "ChillerHeater_Autosize_TEST");

    // Note: Each individual chiller heater module is sized to be capable of supporting the total load on the wrapper

    // Flow is multiplied by the SizFac
    Real64 EvapVolFlowRateExpected =
        state->dataSize->PlantSizData(PltSizNum).DesVolFlowRate * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().SizFac;

    Real64 RefCapCoolingExpected = rho_evap * Cp_evap * EvapVolFlowRateExpected * state->dataSize->PlantSizData(PltSizNum).DeltaT;

    Real64 CondVolFlowRateExpected = RefCapCoolingExpected *
                                     (1.0 + (1.0 / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().RefCOPCooling) *
                                                state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().OpenMotorEff) /
                                     (rho_cond * Cp_cond * state->dataSize->PlantSizData(PltSizCondNum).DeltaT);

    // now call sizing routine
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlantCentralGSHP->Wrapper(1).SizeWrapper(*state);

    // Autosized values are wrapper-specific module sizing state; the shared definition remains unchanged.
    EXPECT_DOUBLE_EQ(EvapVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(RefCapCoolingExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.RefCapCooling);

    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.CondVolFlowRate);
    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2).sizing.CondVolFlowRate);

    // Ensure that stuff that other quantities that depends on RefCapCooling are also initialized properly
    // Heating Cap
    Real64 RefCapClgHtgExpected =
        RefCapCoolingExpected * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().ClgHtgToCoolingCapRatio;
    EXPECT_DOUBLE_EQ(RefCapClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.RefCapClgHtg);

    // Heating Power: Calc cooling Power = Cap / COP, and multiply by ratio
    Real64 RefPowerClgHtgExpected =
        (RefCapCoolingExpected / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().RefCOPCooling) *
        state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).performanceData().ClgHtgtoCogPowerRatio;
    EXPECT_DOUBLE_EQ(RefPowerClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.RefPowerClgHtg);

    // Heating COP = Heating Cap / Heating Power
    Real64 RefCOPClgHtgExpected = RefCapClgHtgExpected / RefPowerClgHtgExpected;
    EXPECT_DOUBLE_EQ(RefCOPClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).sizing.RefCOPClgHtg);

    ASSERT_EQ(3u, state->dataSize->CompDesWaterFlow.size());
    EXPECT_DOUBLE_EQ(2.0 * EvapVolFlowRateExpected, state->dataSize->CompDesWaterFlow(1).DesVolFlowRate);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->CompDesWaterFlow(2).DesVolFlowRate);
    EXPECT_DOUBLE_EQ(2.0 * std::max(EvapVolFlowRateExpected, CondVolFlowRateExpected), state->dataSize->CompDesWaterFlow(3).DesVolFlowRate);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_DesignCapacityReportingUsesConnectionsAndStageability)
{
    state->init_state(*state);

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CWPlantLoc.loopNum = 1;
    wrapper.HWPlantLoc.loopNum = 2;
    wrapper.GLHEPlantLoc.loopNum = 3;
    wrapper.ChillerHeaterNums = 2;
    wrapper.ChillerHeater.allocate(2);

    PlantCentralGSHP::ChillerHeaterPerformanceData performance1;
    PlantCentralGSHP::ChillerHeaterPerformanceData performance2;
    wrapper.ChillerHeater(1).initialize(1, performance1, nullptr);
    wrapper.ChillerHeater(2).initialize(2, performance2, nullptr);

    auto &module1 = wrapper.ChillerHeater(1);
    module1.sizing.RefCapCooling = 10000.0;
    performance1.RefCOPCooling = 5.0;
    performance1.MinPartLoadRatCooling = 0.20;
    performance1.MaxPartLoadRatCooling = 1.00;
    performance1.OptPartLoadRatCooling = 0.80;
    module1.sizing.RefCapClgHtg = 8000.0;
    module1.sizing.RefPowerClgHtg = 2000.0;
    performance1.MinPartLoadRatClgHtg = 0.25;
    performance1.MaxPartLoadRatClgHtg = 1.10;
    performance1.OptPartLoadRatClgHtg = 0.75;
    performance1.OpenMotorEff = 0.80;

    auto &module2 = wrapper.ChillerHeater(2);
    module2.sizing.RefCapCooling = 6000.0;
    performance2.RefCOPCooling = 3.0;
    performance2.MinPartLoadRatCooling = 0.10;
    performance2.MaxPartLoadRatCooling = 1.20;
    performance2.OptPartLoadRatCooling = 0.70;
    module2.sizing.RefCapClgHtg = 12000.0;
    module2.sizing.RefPowerClgHtg = 3000.0;
    performance2.MinPartLoadRatClgHtg = 0.30;
    performance2.MaxPartLoadRatClgHtg = 0.90;
    performance2.OptPartLoadRatClgHtg = 0.60;
    performance2.OpenMotorEff = 0.50;

    Real64 maximumLoad = 0.0;
    Real64 minimumLoad = 0.0;
    Real64 optimumLoad = 0.0;
    PlantLocation calledFromLocation;

    calledFromLocation.loopNum = wrapper.CWPlantLoc.loopNum;
    wrapper.getDesignCapacities(*state, calledFromLocation, maximumLoad, minimumLoad, optimumLoad);
    EXPECT_DOUBLE_EQ(17200.0, maximumLoad);
    EXPECT_DOUBLE_EQ(600.0, minimumLoad);
    EXPECT_DOUBLE_EQ(12200.0, optimumLoad);

    calledFromLocation.loopNum = wrapper.HWPlantLoc.loopNum;
    wrapper.getDesignCapacities(*state, calledFromLocation, maximumLoad, minimumLoad, optimumLoad);
    EXPECT_DOUBLE_EQ(22710.0, maximumLoad);
    EXPECT_DOUBLE_EQ(2400.0, minimumLoad);
    EXPECT_DOUBLE_EQ(15300.0, optimumLoad);

    calledFromLocation.loopNum = wrapper.GLHEPlantLoc.loopNum;
    wrapper.getDesignCapacities(*state, calledFromLocation, maximumLoad, minimumLoad, optimumLoad);
    EXPECT_DOUBLE_EQ(20000.0, maximumLoad);
    EXPECT_DOUBLE_EQ(700.0, minimumLoad);
    EXPECT_DOUBLE_EQ(14180.0, optimumLoad);

    performance2.MaxPartLoadRatClgHtg = 1.20;
    performance2.OptPartLoadRatClgHtg = 0.80;
    wrapper.getDesignCapacities(*state, calledFromLocation, maximumLoad, minimumLoad, optimumLoad);
    EXPECT_DOUBLE_EQ(23200.0, maximumLoad);
    EXPECT_DOUBLE_EQ(700.0, minimumLoad);
    EXPECT_DOUBLE_EQ(15600.0, optimumLoad);

    calledFromLocation.loopNum = 4;
    wrapper.getDesignCapacities(*state, calledFromLocation, maximumLoad, minimumLoad, optimumLoad);
    EXPECT_DOUBLE_EQ(0.0, maximumLoad);
    EXPECT_DOUBLE_EQ(0.0, minimumLoad);
    EXPECT_DOUBLE_EQ(0.0, optimumLoad);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_SourceOnlySizingUsesSourcePlantSizingData)
{
    state->init_state(*state);

    state->dataPlnt->PlantLoop.allocate(3);
    state->dataSize->PlantSizData.allocate(1);
    auto *water = Fluid::GetWater(*state);
    ASSERT_NE(nullptr, water);

    state->dataPlnt->PlantLoop(1).PlantSizNum = 0;
    state->dataPlnt->PlantLoop(1).glycol = water;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(2).glycol = water;
    state->dataPlnt->PlantLoop(3).PlantSizNum = 0;
    state->dataPlnt->PlantLoop(3).glycol = water;
    state->dataSize->PlantSizData(1).LoopType = DataSizing::TypeOfPlantLoop::Condenser;
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.002;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CWPlantLoc.loopNum = 1;
    wrapper.GLHEPlantLoc.loopNum = 2;
    wrapper.HWPlantLoc.loopNum = 3;
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.CWPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.GLHEPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.HWPlantLoc);
    wrapper.CHWInletNodeNum = 11;
    wrapper.HWInletNodeNum = 12;
    wrapper.GLHEInletNodeNum = 13;
    wrapper.ChillerHeaterNums = 1;
    wrapper.ChillerHeater.allocate(1);

    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    wrapper.ChillerHeater(1).initialize(1, performance, nullptr);
    auto &module = wrapper.ChillerHeater(1);
    performance.Name = "SOURCE SIZING ONLY";
    performance.SizFac = 1.0;
    module.sizing.EvapVolFlowRate = 0.001;
    module.sizing.RefCapCooling = 10000.0;
    performance.RefCOPCooling = 4.0;
    performance.OpenMotorEff = 0.80;
    performance.TempRefCondInCooling = 30.0;
    module.sizing.CondVolFlowRate = DataSizing::AutoSize;
    performance.CondVolFlowRateWasAutoSized = true;
    performance.DesignHotWaterVolFlowRate = 0.0007;

    Real64 const sourceDensity = water->getDensity(*state, performance.TempRefCondInCooling, "PlantCentralGSHP source sizing test");
    Real64 const sourceSpecificHeat = water->getSpecificHeat(*state, performance.TempRefCondInCooling, "PlantCentralGSHP source sizing test");
    Real64 const expectedSourceCondenserFlow = module.sizing.RefCapCooling * (1.0 + performance.OpenMotorEff / performance.RefCOPCooling) /
                                               (state->dataSize->PlantSizData(1).DeltaT * sourceSpecificHeat * sourceDensity);

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    wrapper.SizeWrapper(*state);

    EXPECT_NEAR(expectedSourceCondenserFlow, module.sizing.CondVolFlowRate, 1.0e-12);
    EXPECT_DOUBLE_EQ(module.sizing.EvapVolFlowRate, module.sizing.tmpEvapVolFlowRate);
    EXPECT_NEAR(expectedSourceCondenserFlow, module.sizing.tmpCondVolFlowRate, 1.0e-12);
    ASSERT_EQ(3u, state->dataSize->CompDesWaterFlow.size());
    EXPECT_DOUBLE_EQ(module.sizing.EvapVolFlowRate, state->dataSize->CompDesWaterFlow(1).DesVolFlowRate);
    EXPECT_DOUBLE_EQ(performance.DesignHotWaterVolFlowRate, state->dataSize->CompDesWaterFlow(2).DesVolFlowRate);
    EXPECT_NEAR(std::max(module.sizing.EvapVolFlowRate, expectedSourceCondenserFlow), state->dataSize->CompDesWaterFlow(3).DesVolFlowRate, 1.0e-12);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_HardSizedWarningsRetainCalculatedDesignValues)
{
    state->init_state(*state);
    OutputReportPredefined::SetPredefinedTables(*state);

    state->dataPlnt->PlantLoop.allocate(3);
    state->dataSize->PlantSizData.allocate(2);
    auto *water = Fluid::GetWater(*state);
    ASSERT_NE(nullptr, water);
    for (int loopNum = 1; loopNum <= 3; ++loopNum) {
        state->dataPlnt->PlantLoop(loopNum).glycol = water;
    }
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;
    state->dataSize->PlantSizData(1).LoopType = DataSizing::TypeOfPlantLoop::Cooling;
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.010;
    state->dataSize->PlantSizData(1).DeltaT = 6.0;
    state->dataSize->PlantSizData(2).LoopType = DataSizing::TypeOfPlantLoop::Condenser;
    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.020;
    state->dataSize->PlantSizData(2).DeltaT = 5.0;

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CWPlantLoc.loopNum = 1;
    wrapper.GLHEPlantLoc.loopNum = 2;
    wrapper.HWPlantLoc.loopNum = 3;
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.CWPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.GLHEPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.HWPlantLoc);
    wrapper.CHWInletNodeNum = 21;
    wrapper.HWInletNodeNum = 22;
    wrapper.GLHEInletNodeNum = 23;
    wrapper.ChillerHeaterNums = 1;
    wrapper.ChillerHeater.allocate(1);

    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    wrapper.ChillerHeater(1).initialize(1, performance, nullptr);
    auto &module = wrapper.ChillerHeater(1);
    performance.Name = "HARD SIZED MODULE";
    performance.SizFac = 1.0;
    module.sizing.EvapVolFlowRate = 0.001;
    module.sizing.RefCapCooling = 1000.0;
    performance.RefCOPCooling = 5.0;
    performance.OpenMotorEff = 1.0;
    performance.TempRefCondInCooling = 30.0;
    module.sizing.CondVolFlowRate = 0.001;
    performance.DesignHotWaterVolFlowRate = 0.002;

    state->dataGlobal->DisplayExtraWarnings = true;
    state->dataSize->AutoVsHardSizingThreshold = 0.01;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    wrapper.SizeWrapper(*state);

    EXPECT_DOUBLE_EQ(0.001, module.sizing.EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(1000.0, module.sizing.RefCapCooling);
    EXPECT_DOUBLE_EQ(0.001, module.sizing.CondVolFlowRate);
    EXPECT_DOUBLE_EQ(0.001, module.sizing.tmpEvapVolFlowRate);
    EXPECT_DOUBLE_EQ(0.001, module.sizing.tmpCondVolFlowRate);
    EXPECT_TRUE(compare_err_stream_substring("User-Specified Reference Chilled Water Flow Rate", false));
    EXPECT_TRUE(compare_err_stream_substring("User-Specified Reference Capacity", false));
    EXPECT_TRUE(compare_err_stream_substring("User-Specified Reference Condenser Water Flow Rate", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_Control_Schedule_fix)
{
    std::string const idf_objects = delimited_string({

        "Schedule:Compact,",
        "Always1, !-Name",
        "On/Off, !-Schedule Type Limits Name",
        "Through: 12/31, !-Field 1",
        "For: AllDays, !-Field 2",
        "Until: 24:00, 1; !-Field 3 ",

        "CentralHeatPumpSystem,",
        "ChW_Loop HeatPump1, !-Name",
        "ChW_Loop HeatPump1 ChW Inlet, !-Cooling Loop Inlet Node Name",
        "ChW_Loop HeatPump1 ChW Outlet, !-Cooling Loop Outlet Node Name",
        "ChW_Loop HeatPump1 Cnd Inlet, !-Source Loop Inlet Node Name",
        "ChW_Loop HeatPump1 Cnd Outlet, !-Source Loop Outlet Node Name",
        "ChW_Loop HeatPump1 HHW Inlet, !-Heating Loop Inlet Node Name",
        "ChW_Loop HeatPump1 HHW Outlet, !-Heating Loop Outlet Node Name",
        "460,  !-Ancillary Power{W}",
        ",  !-Ancillary Operation Schedule Name",
        "ChillerHeaterPerformance:Electric:EIR, !-Chiller Heater Modules Performance Component Object Type 1",
        "ChW_Loop HeatPump1 Module, !-Chiller Heater Modules Performance Component Name 1",
        "Always_1_typo, !-Chiller Heater Modules Control Schedule Name 1",
        "2; !-Number of Chiller Heater Modules 1",

        "ChillerHeaterPerformance:Electric:EIR,",
        "    ChW_Loop HeatPump1 Module,  !- Name",
        "    autosize,                !- Reference Cooling Mode Evaporator Capacity {W}",
        "    1.5,                     !- Reference Cooling Mode COP {W/W}",
        "    6.67,                    !- Reference Cooling Mode Leaving Chilled Water Temperature {C}",
        "    29.4,                    !- Reference Cooling Mode Entering Condenser Fluid Temperature {C}",
        "    35.0,                    !- Reference Cooling Mode Leaving Condenser Water Temperature {C}",
        "    0.74,                    !- Reference Heating Mode Cooling Capacity Ratio",
        "    0.925,                   !- Reference Heating Mode Cooling Power Input Ratio",
        "    6.67,                    !- Reference Heating Mode Leaving Chilled Water Temperature {C}",
        "    60,                      !- Reference Heating Mode Leaving Condenser Water Temperature {C}",
        "    29.4,                    !- Reference Heating Mode Entering Condenser Fluid Temperature {C}",
        "    5,                       !- Heating Mode Entering Chilled Water Temperature Low Limit {C}",
        "    VariableFlow,            !- Chilled Water Flow Mode Type",
        "    autosize,                !- Design Chilled Water Flow Rate {m3/s}",
        "    autosize,                !- Design Condenser Water Flow Rate {m3/s}",
        "    0.01684,                 !- Design Hot Water Flow Rate {m3/s}",
        "    1,                       !- Compressor Motor Efficiency",
        "    EnteringCondenser,       !- Cooling Mode Temperature Curve Condenser Water Independent Variable",
        "    ChillerHeaterClgCapFT,   !- Cooling Mode Cooling Capacity Function of Temperature Curve Name",
        "    ChillerHeaterClgEIRFT,   !- Cooling Mode Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
        "    ChillerHeaterClgEIRFPLR, !- Cooling Mode Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
        "    1,                       !- Cooling Mode Cooling Capacity Optimum Part Load Ratio",
        "    LeavingCondenser,        !- Heating Mode Temperature Curve Condenser Water Independent Variable",
        "    ChillerHeaterHtgCapFT,   !- Heating Mode Cooling Capacity Function of Temperature Curve Name",
        "    ChillerHeaterHtgEIRFT,   !- Heating Mode Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
        "    ChillerHeaterHtgEIRFPLR, !- Heating Mode Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
        "    1,                       !- Heating Mode Cooling Capacity Optimum Part Load Ratio",
        "    1,                       !- Sizing Factor",
        "    55;                      !- Maximum Heating Mode Leaving Condenser Water Temperature {C}",

        "Curve:Biquadratic,",
        "    ChillerHeaterClgCapFT,   !- Name",
        "    0.950829,                !- Coefficient1 Constant",
        "    3.419327E-02,            !- Coefficient2 x",
        "    2.66642E-04,             !- Coefficient3 x**2",
        "    -1.733397E-03,           !- Coefficient4 y",
        "    -1.762417E-04,           !- Coefficient5 y**2",
        "    -3.69198E-05,            !- Coefficient6 x*y",
        "    4.44,                    !- Minimum Value of x",
        "    12.78,                   !- Maximum Value of x",
        "    12.78,                   !- Minimum Value of y",
        "    29.44,                   !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    ChillerHeaterHtgCapFT,   !- Name",
        "    0.9415266,               !- Coefficient1 Constant",
        "    5.527431E-02,            !- Coefficient2 x",
        "    3.573558E-04,            !- Coefficient3 x**2",
        "    1.258391E-03,            !- Coefficient4 y",
        "    -6.420546E-05,           !- Coefficient5 y**2",
        "    -5.350989E-04,           !- Coefficient6 x*y",
        "    4.44,                    !- Minimum Value of x",
        "    15.56,                   !- Maximum Value of x",
        "    35,                      !- Minimum Value of y",
        "    57.22,                   !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    ChillerHeaterClgEIRFT,   !- Name",
        "    0.7362431,               !- Coefficient1 Constant",
        "    2.136491E-02,            !- Coefficient2 x",
        "    3.638909E-04,            !- Coefficient3 x**2",
        "    -4.284947E-03,           !- Coefficient4 y",
        "    3.389817E-04,            !- Coefficient5 y**2",
        "    -3.632396E-04,           !- Coefficient6 x*y",
        "    4.44,                    !- Minimum Value of x",
        "    12.78,                   !- Maximum Value of x",
        "    12.78,                   !- Minimum Value of y",
        "    29.44,                   !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    ChillerHeaterHtgEIRFT,   !- Name",
        "    0.2286246,               !- Coefficient1 Constant",
        "    2.498714E-02,            !- Coefficient2 x",
        "    -1.267106E-05,           !- Coefficient3 x**2",
        "    9.327184E-03,            !- Coefficient4 y",
        "    5.892037E-05,            !- Coefficient5 y**2",
        "    -3.268512E-04,           !- Coefficient6 x*y",
        "    4.44,                    !- Minimum Value of x",
        "    15.56,                   !- Maximum Value of x",
        "    35.0,                    !- Minimum Value of y",
        "    57.22,                   !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        " Curve:Cubic,",
        "     ChillerHeaterClgEIRFPLR, !- Name",
        "     0.0,                     !- Coefficient1 Constant",
        "     1.22895,                 !- Coefficient2 x",
        "     -0.751383,               !- Coefficient3 x**2",
        "     0.517396,                !- Coefficient4 x**3",
        "     0.2,                     !- Minimum Value of x",
        "     1;                       !- Maximum Value of x",

        "Curve:Cubic,",
        "    ChillerHeaterHtgEIRFPLR, !- Name",
        "    0.0,                     !- Coefficient1 Constant",
        "    1.12853,                 !- Coefficient2 x",
        "    -0.0264962,              !- Coefficient3 x**2",
        "    -0.103811,               !- Coefficient4 x**3",
        "    0.3,                     !- Minimum Value of x",
        "    1;                       !- Maximum Value of x"

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->init_state(*state);

    // May not need for direct wrapper input processing call (need when caling factory)
    state->dataPlantCentralGSHP->getWrapperInputFlag = true;

    // call the central heat pump system input processing function
    PlantCentralGSHP::GetWrapperInput(*state);

    // verify that under this scenario of not finding a schedule match, ScheduleAlwaysOn is the treated default
    EXPECT_EQ(state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).chSched, Sched::GetScheduleAlwaysOn(*state));
    EXPECT_EQ(state->dataPlantCentralGSHP->Wrapper(1).ancillaryPowerSched, Sched::GetScheduleAlwaysOn(*state));
    EXPECT_TRUE(state->dataPlantCentralGSHP->Wrapper(1).VariableFlowCH);
    auto const &module = state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1);
    EXPECT_TRUE(module.VariableFlow);
    EXPECT_EQ(module.availabilitySchedule, state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).chSched);
    EXPECT_FALSE(module.performanceData().ConstantFlow);
    EXPECT_FALSE(module.performanceData().MaxHeatingLeavingCondTempWasBlank);
    EXPECT_DOUBLE_EQ(55.0, module.performanceData().MaxHeatingLeavingCondTemp);
    ASSERT_TRUE(allocated(state->dataPlantCentralGSHP->performanceDefinitions));
    EXPECT_EQ(1, state->dataPlantCentralGSHP->numPerformanceDefinitions);
    EXPECT_EQ(1, state->dataPlantCentralGSHP->numPerformanceReferences);
    EXPECT_EQ(&state->dataPlantCentralGSHP->performanceDefinitions(1), module.performance);
    EXPECT_EQ(1, module.performanceIndex);

    // verify that node names were processed correctly
    EXPECT_EQ(state->dataBranchNodeConnections->NumOfNodeConnections, 6);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(1).NodeName, "CHW_LOOP HEATPUMP1 CHW INLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(1).ConnectionType, Node::ConnectionType::Inlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(1).FluidStream, Node::CompFluidStream::Primary);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(2).NodeName, "CHW_LOOP HEATPUMP1 CHW OUTLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(2).ConnectionType, Node::ConnectionType::Outlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(2).FluidStream, Node::CompFluidStream::Primary);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(3).NodeName, "CHW_LOOP HEATPUMP1 CND INLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(3).ConnectionType, Node::ConnectionType::Inlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(3).FluidStream, Node::CompFluidStream::Secondary);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(4).NodeName, "CHW_LOOP HEATPUMP1 CND OUTLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(4).ConnectionType, Node::ConnectionType::Outlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(4).FluidStream, Node::CompFluidStream::Secondary);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(5).NodeName, "CHW_LOOP HEATPUMP1 HHW INLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(5).ConnectionType, Node::ConnectionType::Inlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(5).FluidStream, Node::CompFluidStream::Tertiary);

    EXPECT_EQ(state->dataBranchNodeConnections->NodeConnections(6).NodeName, "CHW_LOOP HEATPUMP1 HHW OUTLET");
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(6).ConnectionType, Node::ConnectionType::Outlet);
    EXPECT_ENUM_EQ(state->dataBranchNodeConnections->NodeConnections(6).FluidStream, Node::CompFluidStream::Tertiary);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_FlowModeResolutionIsWrapperScopedAndMutuallyExclusive)
{
    state->init_state(*state);

    PlantCentralGSHP::WrapperSpecs mixedWrapper;
    mixedWrapper.Name = "MIXED WRAPPER";
    mixedWrapper.ChillerHeater.allocate(2);
    PlantCentralGSHP::ChillerHeaterPerformanceData constantPerformance;
    PlantCentralGSHP::ChillerHeaterPerformanceData variablePerformance;
    constantPerformance.ConstantFlow = true;
    variablePerformance.VariableFlow = true;
    mixedWrapper.ChillerHeater(1).initialize(1, constantPerformance, nullptr);
    mixedWrapper.ChillerHeater(2).initialize(2, variablePerformance, nullptr);

    mixedWrapper.resolveFlowMode(*state);

    EXPECT_FALSE(mixedWrapper.VariableFlowCH);
    for (auto const &module : mixedWrapper.ChillerHeater) {
        EXPECT_FALSE(module.VariableFlow);
    }
    EXPECT_TRUE(constantPerformance.ConstantFlow);
    EXPECT_TRUE(variablePerformance.VariableFlow);
    EXPECT_TRUE(compare_err_stream_substring("MIXED WRAPPER contains both ConstantFlow and VariableFlow", true));

    PlantCentralGSHP::WrapperSpecs variableWrapper;
    variableWrapper.Name = "VARIABLE WRAPPER";
    variableWrapper.ChillerHeater.allocate(2);
    PlantCentralGSHP::ChillerHeaterPerformanceData variablePerformance1;
    PlantCentralGSHP::ChillerHeaterPerformanceData variablePerformance2;
    variablePerformance1.VariableFlow = true;
    variablePerformance2.VariableFlow = true;
    variableWrapper.ChillerHeater(1).initialize(1, variablePerformance1, nullptr);
    variableWrapper.ChillerHeater(2).initialize(2, variablePerformance2, nullptr);

    variableWrapper.resolveFlowMode(*state);

    EXPECT_TRUE(variableWrapper.VariableFlowCH);
    for (auto const &module : variableWrapper.ChillerHeater) {
        EXPECT_TRUE(module.VariableFlow);
    }
    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_NativePerformanceInputReadsCompleteObjectAndMarksItUsed)
{
    ASSERT_TRUE(process_json(makeChillerHeaterNativeJSON(true, true)));
    state->init_state(*state);

    EXPECT_NO_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state));
    EXPECT_FALSE(has_err_output());

    ASSERT_TRUE(allocated(state->dataPlantCentralGSHP->performanceDefinitions));
    auto const &performance = state->dataPlantCentralGSHP->performanceDefinitions(1);
    EXPECT_EQ("NATIVE MIXED CASE MODULE", performance.Name);
    EXPECT_EQ(PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser, performance.CondModeCooling);
    EXPECT_EQ(PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser, performance.CondModeHeating);
    EXPECT_FALSE(performance.ConstantFlow);
    EXPECT_TRUE(performance.VariableFlow);
    EXPECT_EQ(DataSizing::AutoSize, performance.RefCapCooling);
    EXPECT_TRUE(performance.RefCapCoolingWasAutoSized);
    EXPECT_EQ(DataSizing::AutoSize, performance.EvapVolFlowRate);
    EXPECT_TRUE(performance.EvapVolFlowRateWasAutoSized);
    EXPECT_EQ(DataSizing::AutoSize, performance.CondVolFlowRate);
    EXPECT_TRUE(performance.CondVolFlowRateWasAutoSized);
    EXPECT_DOUBLE_EQ(0.001, performance.DesignHotWaterVolFlowRate);
    EXPECT_DOUBLE_EQ(0.8, performance.OpenMotorEff);
    EXPECT_DOUBLE_EQ(0.5, performance.OptPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(0.5, performance.OptPartLoadRatClgHtg);
    EXPECT_DOUBLE_EQ(1.2, performance.SizFac);
    EXPECT_FALSE(performance.MaxHeatingLeavingCondTempWasBlank);
    EXPECT_DOUBLE_EQ(55.0, performance.MaxHeatingLeavingCondTemp);
    EXPECT_GT(performance.ChillerCapFTCoolingIDX, 0);
    EXPECT_GT(performance.ChillerEIRFPLRHeatingIDX, 0);

    state->dataGlobal->DisplayUnusedObjects = true;
    state->dataGlobal->DisplayAllWarnings = true;
    state->dataInputProcessing->inputProcessor->reportOrphanRecordObjects(*state);
    EXPECT_FALSE(compare_err_stream_substring("Native Mixed Case Module", true, false));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_NativePerformanceInputAppliesSchemaDefaultsAndTracksOmittedLimit)
{
    ASSERT_TRUE(process_json(makeChillerHeaterNativeJSON(false)));
    state->init_state(*state);

    EXPECT_NO_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state));
    EXPECT_FALSE(has_err_output());

    auto const &performance = state->dataPlantCentralGSHP->performanceDefinitions(1);
    EXPECT_TRUE(performance.ConstantFlow);
    EXPECT_FALSE(performance.VariableFlow);
    EXPECT_EQ(PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser, performance.CondModeCooling);
    EXPECT_EQ(PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser, performance.CondModeHeating);
    EXPECT_DOUBLE_EQ(6.67, performance.TempRefEvapOutCooling);
    EXPECT_DOUBLE_EQ(29.44, performance.TempRefCondInCooling);
    EXPECT_DOUBLE_EQ(35.0, performance.TempRefCondOutCooling);
    EXPECT_DOUBLE_EQ(0.75, performance.ClgHtgToCoolingCapRatio);
    EXPECT_DOUBLE_EQ(1.38, performance.ClgHtgtoCogPowerRatio);
    EXPECT_DOUBLE_EQ(6.67, performance.TempRefEvapOutClgHtg);
    EXPECT_DOUBLE_EQ(49.0, performance.TempRefCondOutClgHtg);
    EXPECT_DOUBLE_EQ(29.44, performance.TempRefCondInClgHtg);
    EXPECT_DOUBLE_EQ(12.22, performance.TempLowLimitEvapOut);
    EXPECT_DOUBLE_EQ(0.0, performance.EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(0.0, performance.CondVolFlowRate);
    EXPECT_DOUBLE_EQ(0.0, performance.DesignHotWaterVolFlowRate);
    EXPECT_DOUBLE_EQ(1.0, performance.OpenMotorEff);
    EXPECT_DOUBLE_EQ(1.0, performance.OptPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(1.0, performance.OptPartLoadRatClgHtg);
    EXPECT_DOUBLE_EQ(1.0, performance.SizFac);
    EXPECT_TRUE(performance.MaxHeatingLeavingCondTempWasBlank);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_NativePerformanceInputReportsReferencedField)
{
    auto epJSON = makeChillerHeaterNativeJSON();
    epJSON["ChillerHeaterPerformance:Electric:EIR"]["Native Mixed Case Module"]["cooling_mode_cooling_capacity_function_of_temperature_curve_name"] =
        "Missing Temperature Curve";
    ASSERT_TRUE(process_json(epJSON));
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("Cooling Mode Cooling Capacity Function of Temperature Curve Name=MISSING TEMPERATURE CURVE", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_NativePerformanceInputRejectsCaseInsensitiveDuplicateNames)
{
    auto epJSON = makeChillerHeaterNativeJSON();
    auto &objects = epJSON["ChillerHeaterPerformance:Electric:EIR"];
    objects["native mixed case module"] = objects["Native Mixed Case Module"];
    ASSERT_TRUE(process_json(epJSON));
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("duplicate name.", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_IDFAndNativePerformanceInputsProduceEquivalentState)
{
    std::string const idf = delimited_string({
        "ChillerHeaterPerformance:Electric:EIR,",
        "  Native Mixed Case Module,",
        "  10000,",
        "  5.0,",
        "  7.0,",
        "  30.0,",
        "  35.0,",
        "  0.75,",
        "  1.0,",
        "  7.0,",
        "  50.0,",
        "  30.0,",
        "  3.0,",
        "  VariableFlow,",
        "  0.001,",
        "  0.001,",
        "  0.001,",
        "  0.8,",
        "  LeavingCondenser,",
        "  Reference Temperature Curve,",
        "  Reference Temperature Curve,",
        "  Reference PLR Curve,",
        "  0.5,",
        "  EnteringCondenser,",
        "  Reference Temperature Curve,",
        "  Reference Temperature Curve,",
        "  Reference PLR Curve,",
        "  0.5,",
        "  1.2,",
        "  55.0;",

        "Curve:Biquadratic,",
        "  Reference Temperature Curve,",
        "  1.0, 0.0, 0.0, 0.0, 0.0, 0.0,",
        "  -100.0, 100.0, -100.0, 100.0;",

        "Curve:Quadratic,",
        "  Reference PLR Curve,",
        "  1.0, 0.0, 0.0,",
        "  0.0, 1.0;",
    });

    ASSERT_TRUE(process_idf(idf));
    state->init_state(*state);
    EXPECT_NO_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state));
    EXPECT_FALSE(has_err_output());
    PlantCentralGSHP::ChillerHeaterPerformanceData const idfPerformance = state->dataPlantCentralGSHP->performanceDefinitions(1);

    state->dataPlantCentralGSHP->clear_state();
    state->dataCurveManager->clear_state();
    state->dataInputProcessing->clear_state();
    state->dataIPShortCut->clear_state();
    EXPECT_TRUE(compare_err_stream("", true));

    ASSERT_TRUE(process_json(makeChillerHeaterNativeJSON()));
    Curve::GetCurveInput(*state);
    EXPECT_NO_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state));
    EXPECT_TRUE(compare_err_stream("", true));
    auto const &nativePerformance = state->dataPlantCentralGSHP->performanceDefinitions(1);

    EXPECT_EQ(idfPerformance.Name, nativePerformance.Name);
    EXPECT_EQ(idfPerformance.CondModeCooling, nativePerformance.CondModeCooling);
    EXPECT_EQ(idfPerformance.CondModeHeating, nativePerformance.CondModeHeating);
    EXPECT_EQ(idfPerformance.ConstantFlow, nativePerformance.ConstantFlow);
    EXPECT_EQ(idfPerformance.VariableFlow, nativePerformance.VariableFlow);
    EXPECT_EQ(idfPerformance.RefCapCoolingWasAutoSized, nativePerformance.RefCapCoolingWasAutoSized);
    EXPECT_EQ(idfPerformance.EvapVolFlowRateWasAutoSized, nativePerformance.EvapVolFlowRateWasAutoSized);
    EXPECT_EQ(idfPerformance.CondVolFlowRateWasAutoSized, nativePerformance.CondVolFlowRateWasAutoSized);
    EXPECT_EQ(idfPerformance.MaxHeatingLeavingCondTempWasBlank, nativePerformance.MaxHeatingLeavingCondTempWasBlank);
    EXPECT_DOUBLE_EQ(idfPerformance.RefCapCooling, nativePerformance.RefCapCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.RefCOPCooling, nativePerformance.RefCOPCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefEvapOutCooling, nativePerformance.TempRefEvapOutCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefCondInCooling, nativePerformance.TempRefCondInCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefCondOutCooling, nativePerformance.TempRefCondOutCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.ClgHtgToCoolingCapRatio, nativePerformance.ClgHtgToCoolingCapRatio);
    EXPECT_DOUBLE_EQ(idfPerformance.ClgHtgtoCogPowerRatio, nativePerformance.ClgHtgtoCogPowerRatio);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefEvapOutClgHtg, nativePerformance.TempRefEvapOutClgHtg);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefCondOutClgHtg, nativePerformance.TempRefCondOutClgHtg);
    EXPECT_DOUBLE_EQ(idfPerformance.TempRefCondInClgHtg, nativePerformance.TempRefCondInClgHtg);
    EXPECT_DOUBLE_EQ(idfPerformance.TempLowLimitEvapOut, nativePerformance.TempLowLimitEvapOut);
    EXPECT_DOUBLE_EQ(idfPerformance.EvapVolFlowRate, nativePerformance.EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(idfPerformance.CondVolFlowRate, nativePerformance.CondVolFlowRate);
    EXPECT_DOUBLE_EQ(idfPerformance.DesignHotWaterVolFlowRate, nativePerformance.DesignHotWaterVolFlowRate);
    EXPECT_DOUBLE_EQ(idfPerformance.OpenMotorEff, nativePerformance.OpenMotorEff);
    EXPECT_DOUBLE_EQ(idfPerformance.OptPartLoadRatCooling, nativePerformance.OptPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.OptPartLoadRatClgHtg, nativePerformance.OptPartLoadRatClgHtg);
    EXPECT_DOUBLE_EQ(idfPerformance.SizFac, nativePerformance.SizFac);
    EXPECT_DOUBLE_EQ(idfPerformance.MaxHeatingLeavingCondTemp, nativePerformance.MaxHeatingLeavingCondTemp);
    EXPECT_DOUBLE_EQ(idfPerformance.MinPartLoadRatCooling, nativePerformance.MinPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.MaxPartLoadRatCooling, nativePerformance.MaxPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(idfPerformance.MinPartLoadRatClgHtg, nativePerformance.MinPartLoadRatClgHtg);
    EXPECT_DOUBLE_EQ(idfPerformance.MaxPartLoadRatClgHtg, nativePerformance.MaxPartLoadRatClgHtg);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_InputValidationUsesConfiguredReferenceTemperaturesAndBicubicPLRDomain)
{
    ASSERT_TRUE(process_idf(makeChillerHeaterValidationInput()));
    state->init_state(*state);

    EXPECT_NO_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state));
    EXPECT_FALSE(has_err_output());

    ASSERT_TRUE(allocated(state->dataPlantCentralGSHP->performanceDefinitions));
    auto const &chillerHeater = state->dataPlantCentralGSHP->performanceDefinitions(1);
    EXPECT_DOUBLE_EQ(0.2, chillerHeater.MinPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(1.0, chillerHeater.MaxPartLoadRatCooling);
    EXPECT_DOUBLE_EQ(0.3, chillerHeater.MinPartLoadRatClgHtg);
    EXPECT_DOUBLE_EQ(1.0, chillerHeater.MaxPartLoadRatClgHtg);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_InputValidationRejectsOptimumPLROutsideCurveDomain)
{
    ASSERT_TRUE(process_idf(makeChillerHeaterValidationInput(0.75, 0.1)));
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("Cooling Mode Cooling Capacity Optimum Part Load Ratio must be within", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_InputValidationRejectsInvalidPartLoadCurveDomain)
{
    ASSERT_TRUE(process_idf(makeChillerHeaterValidationInput(0.75, 0.5, 0.5, 55.0, -0.1)));
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("Part-load ratio limits [-0.100, 1.000] must include 1.0", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_InputValidationRejectsInvalidMaximumHeatingLeavingTemperature)
{
    ASSERT_TRUE(process_idf(makeChillerHeaterValidationInput(0.75, 0.5, 0.5, 25.0)));
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring(
        "Maximum Heating Mode Leaving Condenser Water Temperature must be greater than Reference Heating Mode Entering Condenser Fluid Temperature",
        true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_SchemaRejectsNonpositiveHeatingCapacityRatio)
{
    EXPECT_FALSE(process_idf(makeChillerHeaterValidationInput(-0.75), false));
    EXPECT_TRUE(compare_err_stream_substring("Expected number greater than 0.000000", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_RuntimeValidationRejectsInvalidStaticPerformanceInputs)
{
    ASSERT_TRUE(process_idf(makeChillerHeaterValidationInput()));
    auto &performanceObject = state->dataInputProcessing->inputProcessor->epJSON["ChillerHeaterPerformance:Electric:EIR"]["Validation Module"];
    performanceObject["reference_cooling_mode_evaporator_capacity"] = -10000.0;
    performanceObject["reference_cooling_mode_cop"] = -5.0;
    performanceObject["reference_heating_mode_cooling_capacity_ratio"] = -0.75;
    performanceObject["reference_heating_mode_cooling_power_input_ratio"] = -1.0;
    performanceObject["compressor_motor_efficiency"] = 1.1;
    state->init_state(*state);

    EXPECT_THROW(PlantCentralGSHP::GetChillerHeaterInput(*state), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("Reference Cooling Mode Evaporator Capacity=-10000.00", false));
    EXPECT_TRUE(compare_err_stream_substring("Reference Cooling Mode COP=-5.00", false));
    EXPECT_TRUE(compare_err_stream_substring("Reference Heating Mode Cooling Capacity Ratio=-0.75", false));
    EXPECT_TRUE(compare_err_stream_substring("Reference Heating Mode Cooling Power Input Ratio=-1.00", false));
    EXPECT_TRUE(compare_err_stream_substring("Compressor Motor Efficiency = 1.100", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AncillaryScheduleDefaultsAndScalesPowerAndEnergy)
{
    state->init_state(*state);
    state->dataLoopNodes->Node.allocate(6);
    state->dataHVACGlobal->TimeStepSysSec = 600.0;

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CHWInletNodeNum = 1;
    wrapper.CHWOutletNodeNum = 2;
    wrapper.HWInletNodeNum = 3;
    wrapper.HWOutletNodeNum = 4;
    wrapper.GLHEInletNodeNum = 5;
    wrapper.GLHEOutletNodeNum = 6;
    wrapper.AncillaryPower = 100.0;
    wrapper.ChillerHeaterNums = 1;
    wrapper.ChillerHeater.allocate(1);

    auto setCoolingResult = [&]() {
        auto &result = wrapper.ChillerHeater(1).Result;
        result = PlantCentralGSHP::ChillerHeaterResult();
        result.currentMode = CurrentMode::CoolingOnly;
        result.coolingPower = 50.0;
        result.compressorPower = 50.0;
        result.coolingDelivered = 1000.0;
        result.sourceHeatTransfer = 1050.0;
        result.chilledWaterMassFlowRate = 1.0;
        result.chilledWaterOutletTemp = 7.0;
        result.sourceMassFlowRate = 1.0;
        result.sourceOutletTemp = 30.25;
    };

    setCoolingResult();
    wrapper.ancillaryPowerSched = nullptr;
    wrapper.updateWrapperReportingAndNodes(*state, 1.0, 0.0, 1.0, 12.0, 40.0, 30.0);
    EXPECT_DOUBLE_EQ(150.0, wrapper.Report.TotElecCoolingPwr);
    EXPECT_DOUBLE_EQ(90000.0, wrapper.Report.TotElecCooling);

    setCoolingResult();
    wrapper.ancillaryPowerSched = Sched::GetScheduleAlwaysOn(*state);
    wrapper.updateWrapperReportingAndNodes(*state, 1.0, 0.0, 1.0, 12.0, 40.0, 30.0);
    EXPECT_DOUBLE_EQ(150.0, wrapper.Report.TotElecCoolingPwr);
    EXPECT_DOUBLE_EQ(90000.0, wrapper.Report.TotElecCooling);

    Sched::ScheduleConstant fractionalSchedule;
    fractionalSchedule.currentVal = 0.25;
    setCoolingResult();
    wrapper.ancillaryPowerSched = &fractionalSchedule;
    wrapper.updateWrapperReportingAndNodes(*state, 1.0, 0.0, 1.0, 12.0, 40.0, 30.0);
    EXPECT_DOUBLE_EQ(75.0, wrapper.Report.TotElecCoolingPwr);
    EXPECT_DOUBLE_EQ(45000.0, wrapper.Report.TotElecCooling);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_OffStateClearsAuthoritativeState)
{
    state->init_state(*state);
    state->dataLoopNodes->Node.allocate(6);

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CHWInletNodeNum = 1;
    wrapper.CHWOutletNodeNum = 2;
    wrapper.HWInletNodeNum = 3;
    wrapper.HWOutletNodeNum = 4;
    wrapper.GLHEInletNodeNum = 5;
    wrapper.GLHEOutletNodeNum = 6;
    state->dataLoopNodes->Node(1).Temp = 12.0;
    state->dataLoopNodes->Node(3).Temp = 40.0;
    state->dataLoopNodes->Node(5).Temp = 15.0;
    state->dataLoopNodes->Node(1).MassFlowRateRequest = 1.0;
    state->dataLoopNodes->Node(3).MassFlowRateRequest = 2.0;
    state->dataLoopNodes->Node(5).MassFlowRateRequest = 3.0;

    wrapper.WrapperCoolingLoad = 1000.0;
    wrapper.WrapperHeatingLoad = 1200.0;
    wrapper.SimulClgDominant = true;
    wrapper.Report.Power = 500.0;
    wrapper.Report.CoolingRate = 1000.0;
    wrapper.Report.HeatingRate = 1200.0;
    wrapper.Report.GLHERate = 200.0;
    wrapper.ChillerHeaterNums = 1;
    wrapper.ChillerHeater.allocate(1);
    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    wrapper.ChillerHeater(1).initialize(1, performance, nullptr);
    auto &chillerHeater = wrapper.ChillerHeater(1);
    chillerHeater.Result.currentMode = CurrentMode::CoolingDominant;
    chillerHeater.Result.coolingPower = 500.0;
    chillerHeater.Result.coolingDelivered = 1000.0;

    wrapper.resetOffState(*state, false);

    EXPECT_DOUBLE_EQ(0.0, wrapper.WrapperCoolingLoad);
    EXPECT_DOUBLE_EQ(0.0, wrapper.WrapperHeatingLoad);
    EXPECT_FALSE(wrapper.SimulClgDominant);
    EXPECT_FALSE(wrapper.SimulHtgDominant);
    EXPECT_EQ(CurrentMode::Off, chillerHeater.Result.currentMode);
    EXPECT_DOUBLE_EQ(0.0, wrapper.Report.Power);
    EXPECT_DOUBLE_EQ(0.0, wrapper.Report.CoolingRate);
    EXPECT_DOUBLE_EQ(0.0, wrapper.Report.HeatingRate);
    EXPECT_DOUBLE_EQ(0.0, wrapper.Report.GLHERate);
    EXPECT_DOUBLE_EQ(12.0, state->dataLoopNodes->Node(2).Temp);
    EXPECT_DOUBLE_EQ(40.0, state->dataLoopNodes->Node(4).Temp);
    EXPECT_DOUBLE_EQ(15.0, state->dataLoopNodes->Node(6).Temp);
    EXPECT_DOUBLE_EQ(0.0, state->dataLoopNodes->Node(1).MassFlowRateRequest);
    EXPECT_DOUBLE_EQ(0.0, state->dataLoopNodes->Node(3).MassFlowRateRequest);
    EXPECT_DOUBLE_EQ(0.0, state->dataLoopNodes->Node(5).MassFlowRateRequest);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_InactiveConnectionPreservesActiveLoad)
{
    state->init_state(*state);
    state->dataLoopNodes->Node.allocate(6);
    state->dataPlnt->PlantLoop.allocate(3);

    auto *water = Fluid::GetWater(*state);
    ASSERT_NE(nullptr, water);
    for (int loopNum = 1; loopNum <= 3; ++loopNum) {
        auto &plantLoop = state->dataPlnt->PlantLoop(loopNum);
        plantLoop.glycol = water;
        auto &loopSide = plantLoop.LoopSide(DataPlant::LoopSideLocation::Supply);
        loopSide.Branch.allocate(1);
        loopSide.Branch(1).Comp.allocate(1);
    }

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CWPlantLoc = PlantLocation(1, DataPlant::LoopSideLocation::Supply, 1, 1);
    wrapper.HWPlantLoc = PlantLocation(2, DataPlant::LoopSideLocation::Supply, 1, 1);
    wrapper.GLHEPlantLoc = PlantLocation(3, DataPlant::LoopSideLocation::Supply, 1, 1);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.CWPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.HWPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.GLHEPlantLoc);

    wrapper.CHWInletNodeNum = 1;
    wrapper.CHWOutletNodeNum = 2;
    wrapper.HWInletNodeNum = 3;
    wrapper.HWOutletNodeNum = 4;
    wrapper.GLHEInletNodeNum = 5;
    wrapper.GLHEOutletNodeNum = 6;
    state->dataLoopNodes->Node(1).Temp = 12.0;
    state->dataLoopNodes->Node(3).Temp = 40.0;
    state->dataLoopNodes->Node(5).Temp = 15.0;
    state->dataLoopNodes->Node(5).MassFlowRate = 1.0;
    state->dataLoopNodes->Node(1).MassFlowRateRequest = 1.0;
    state->dataLoopNodes->Node(5).MassFlowRateRequest = 2.0;

    wrapper.setupOutputVarsFlag = false;
    wrapper.MyWrapperFlag = false;
    wrapper.MyWrapperEnvrnFlag = false;
    wrapper.WrapperCoolingLoad = 1000.0;
    wrapper.Report.CoolingRate = 1000.0;
    wrapper.Report.GLHERate = 1200.0;
    wrapper.Report.GLHEInletTemp = 15.0;
    wrapper.Report.GLHEOutletTemp = 15.3;
    wrapper.Report.GLHEmdot = 1.0;
    wrapper.ChillerHeaterNums = 1;
    wrapper.ChillerHeater.allocate(1);
    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    performance.OpenMotorEff = 0.80;
    wrapper.ChillerHeater(1).initialize(1, performance, nullptr);
    wrapper.ChillerHeater(1).Result.currentMode = CurrentMode::CoolingOnly;
    wrapper.ChillerHeater(1).Result.coolingDelivered = 1000.0;

    Real64 sourceLoad = 0.0;
    wrapper.simulate(*state, wrapper.GLHEPlantLoc, false, sourceLoad, false);
    EXPECT_DOUBLE_EQ(1000.0, wrapper.WrapperCoolingLoad);
    EXPECT_DOUBLE_EQ(1000.0, wrapper.Report.CoolingRate);
    EXPECT_EQ(CurrentMode::CoolingOnly, wrapper.ChillerHeater(1).Result.currentMode);

    Real64 heatingLoad = 0.0;
    wrapper.simulate(*state, wrapper.HWPlantLoc, false, heatingLoad, false);
    EXPECT_DOUBLE_EQ(0.0, heatingLoad);
    EXPECT_DOUBLE_EQ(1000.0, wrapper.WrapperCoolingLoad);
    EXPECT_DOUBLE_EQ(0.0, wrapper.WrapperHeatingLoad);
    EXPECT_DOUBLE_EQ(1000.0, wrapper.Report.CoolingRate);
    EXPECT_EQ(CurrentMode::CoolingOnly, wrapper.ChillerHeater(1).Result.currentMode);
    EXPECT_DOUBLE_EQ(1.0, state->dataLoopNodes->Node(1).MassFlowRateRequest);
    EXPECT_DOUBLE_EQ(2.0, state->dataLoopNodes->Node(5).MassFlowRateRequest);

    Real64 coolingLoad = 0.0;
    wrapper.simulate(*state, wrapper.CWPlantLoc, false, coolingLoad, false);
    EXPECT_DOUBLE_EQ(0.0, coolingLoad);
    EXPECT_DOUBLE_EQ(0.0, wrapper.WrapperCoolingLoad);
    EXPECT_DOUBLE_EQ(0.0, wrapper.Report.CoolingRate);
    EXPECT_EQ(CurrentMode::Off, wrapper.ChillerHeater(1).Result.currentMode);
    EXPECT_DOUBLE_EQ(0.0, state->dataLoopNodes->Node(1).MassFlowRateRequest);
    EXPECT_DOUBLE_EQ(0.0, state->dataLoopNodes->Node(5).MassFlowRateRequest);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_FailedPlantScanTerminatesInitialization)
{
    state->init_state(*state);
    state->dataLoopNodes->Node.allocate(6);

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.Name = "UNCONNECTED WRAPPER";
    wrapper.setupOutputVarsFlag = false;
    wrapper.MyWrapperEnvrnFlag = false;
    wrapper.CHWInletNodeNum = 1;
    wrapper.CHWOutletNodeNum = 2;
    wrapper.HWInletNodeNum = 3;
    wrapper.HWOutletNodeNum = 4;
    wrapper.GLHEInletNodeNum = 5;
    wrapper.GLHEOutletNodeNum = 6;

    EXPECT_THROW(wrapper.initialize(*state, 0.0, 1, false), std::runtime_error);
    EXPECT_TRUE(compare_err_stream_substring("could not be located on all three connected plant loops", true));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_setChillerHeaterCondTemp)
{
    PlantCentralGSHP::ModePerformanceData modePerformance;
    Real64 constexpr allowedTolerance = 0.001;
    Real64 constexpr condEnterTemp = 55.5;
    Real64 constexpr condLeaveTemp = 44.4;

    modePerformance.condenserMode = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    EXPECT_NEAR(55.5, PlantCentralGSHP::WrapperSpecs::setChillerHeaterCondTemp(modePerformance, condEnterTemp, condLeaveTemp), allowedTolerance);

    modePerformance.condenserMode = PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    EXPECT_NEAR(44.4, PlantCentralGSHP::WrapperSpecs::setChillerHeaterCondTemp(modePerformance, condEnterTemp, condLeaveTemp), allowedTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_ModeEnergyAndRoutingContracts)
{
    std::array<ModeEnergyContract, 6> const operatingPoints = {
        ModeEnergyContract{CurrentMode::Off, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, {0.0, 0.0, 0.0}},
        ModeEnergyContract{CurrentMode::CoolingOnly, 9000.0, 10900.0, 2000.0, 0.0, 0.95, 0.75, 1.0, {9000.0, 0.0, 10900.0}},
        ModeEnergyContract{CurrentMode::HeatingOnly, 7500.0, 9750.0, 2500.0, 0.0, 0.90, 0.60, 1.0, {0.0, 9750.0, -7500.0}},
        ModeEnergyContract{CurrentMode::HeatRecovery, 8000.0, 10000.0, 2000.0, 0.0, 1.0, 0.80, 1.0, {8000.0, 10000.0, 0.0}},
        ModeEnergyContract{CurrentMode::CoolingDominant, 8000.0, 10000.0, 2000.0, 0.0, 1.0, 0.80, 1.0, {8000.0, 3500.0, 6500.0}},
        ModeEnergyContract{CurrentMode::HeatingDominant, 8000.0, 10000.0, 2000.0, 0.0, 1.0, 0.80, 1.0, {3000.0, 10000.0, -5000.0}},
    };

    for (auto const &point : operatingPoints) {
        SCOPED_TRACE(modeName(point.mode));
        EXPECT_TRUE(checkModeEnergyContract(point));
    }
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_ModeContractsRejectKnownDefectSignatures)
{
    // Issue #7838 / current mode-3 example signature: most condenser energy disappears and there is no source transfer.
    ModeEnergyContract const issue7838Point{CurrentMode::HeatRecovery, 10790.844, 48.8987, 9602.5995, 0.0, 1.0, 0.20, 1.0, {10790.844, 48.8987, 0.0}};
    EXPECT_FALSE(checkModeEnergyContract(issue7838Point));

    // Issue #10065 signature: an active Electric:EIR module meets simultaneous loads with zero compressor power.
    ModeEnergyContract const issue10065Point{CurrentMode::HeatRecovery, 8000.0, 8000.0, 0.0, 0.0, 1.0, 0.80, 1.0, {8000.0, 8000.0, 0.0}};
    EXPECT_FALSE(checkModeEnergyContract(issue10065Point));

    // Heating-dominant useful cooling cannot be silently reclassified as source extraction.
    ModeEnergyContract const lostCoolingRoute{CurrentMode::HeatingDominant, 8000.0, 10000.0, 2000.0, 0.0, 1.0, 0.80, 1.0, {0.0, 10000.0, -8000.0}};
    EXPECT_FALSE(checkModeEnergyContract(lostCoolingRoute));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_FinalPartLoadContracts)
{
    std::array<PartLoadContract, 3> const points = {
        PartLoadContract{10000.0, 1000.0, 1000.0, 0.30, 1.0, 0.30, 1.0 / 3.0, 0.30},
        PartLoadContract{10000.0, 6000.0, 6000.0, 0.30, 1.0, 0.60, 1.0, 0.60},
        PartLoadContract{10000.0, 12000.0, 10000.0, 0.30, 1.0, 1.00, 1.0, 1.00},
    };

    for (auto const &point : points) {
        EXPECT_TRUE(checkPartLoadContract(point));
    }

    // Issue #8191 / current mode-2 signature: post-scaled PLR, full cycling, and a full-load EIRFPLR evaluation.
    PartLoadContract const issue8191Point{10000.0, 1000.0, 1000.0, 0.30, 1.0, 0.10, 1.0, 1.0};
    EXPECT_FALSE(checkPartLoadContract(issue8191Point));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_SingleModeSolversUseFinalStateAndCloseEnergyBalances)
{
    std::string const idf_objects = delimited_string({
        "FluidProperties:GlycolConcentration,",
        "  GSHP Source Fluid,",
        "  PropyleneGlycol,",
        "  ,",
        "  0.30;",

        "Curve:Biquadratic,",
        "  Constant Temperature Modifier,",
        "  1.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  -100.0,",
        "  100.0,",
        "  -100.0,",
        "  100.0;",

        "Curve:Quadratic,",
        "  Linear Part Load EIR,",
        "  0.0,",
        "  1.0,",
        "  0.0,",
        "  0.3,",
        "  1.0;",

        "Curve:Bicubic,",
        "  Bivariate Part Load EIR,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  1.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  0.0,",
        "  -100.0,",
        "  100.0,",
        "  0.3,",
        "  1.0;",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    state->init_state(*state);

    state->dataPlnt->PlantLoop.allocate(3);
    auto *water = Fluid::GetWater(*state);
    auto *sourceGlycol = Fluid::GetGlycol(*state, "GSHP SOURCE FLUID");
    ASSERT_NE(nullptr, water);
    ASSERT_NE(nullptr, sourceGlycol);
    state->dataPlnt->PlantLoop(1).glycol = water;
    state->dataPlnt->PlantLoop(2).glycol = sourceGlycol;
    state->dataPlnt->PlantLoop(3).glycol = water;

    PlantCentralGSHP::WrapperSpecs wrapper;
    wrapper.CWPlantLoc.loopNum = 1;
    wrapper.GLHEPlantLoc.loopNum = 2;
    wrapper.HWPlantLoc.loopNum = 3;
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.CWPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.GLHEPlantLoc);
    PlantUtilities::SetPlantLocationLinks(*state, wrapper.HWPlantLoc);

    state->dataLoopNodes->Node.allocate(8);
    wrapper.CoolSetPointTempNode = 1;
    wrapper.HeatSetPointTempNode = 2;
    wrapper.CHWInletNodeNum = 3;
    wrapper.CHWOutletNodeNum = 4;
    wrapper.HWInletNodeNum = 5;
    wrapper.HWOutletNodeNum = 6;
    wrapper.GLHEInletNodeNum = 7;
    wrapper.GLHEOutletNodeNum = 8;
    state->dataLoopNodes->Node(wrapper.CHWInletNodeNum).Temp = 12.0;
    state->dataLoopNodes->Node(wrapper.HWInletNodeNum).Temp = 40.0;
    state->dataLoopNodes->Node(wrapper.GLHEInletNodeNum).Temp = 15.0;
    state->dataLoopNodes->Node(wrapper.CoolSetPointTempNode).TempSetPoint = 7.0;
    state->dataLoopNodes->Node(wrapper.HeatSetPointTempNode).TempSetPoint = 45.0;

    wrapper.ChillerHeater.allocate(2);
    PlantCentralGSHP::ChillerHeaterPerformanceData performance1;
    performance1.OpenMotorEff = 0.80;
    performance1.TempLowLimitEvapOut = 5.0;
    performance1.RefCOPCooling = 5.0;
    performance1.CondModeCooling = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    performance1.ChillerCapFTCoolingIDX = Curve::GetCurveIndex(*state, "CONSTANT TEMPERATURE MODIFIER");
    performance1.ChillerEIRFTCoolingIDX = performance1.ChillerCapFTCoolingIDX;
    performance1.ChillerEIRFPLRCoolingIDX = Curve::GetCurveIndex(*state, "LINEAR PART LOAD EIR");
    performance1.MinPartLoadRatCooling = 0.3;
    performance1.MaxPartLoadRatCooling = 1.0;
    performance1.CondModeHeating = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    performance1.ChillerCapFTHeatingIDX = performance1.ChillerCapFTCoolingIDX;
    performance1.ChillerEIRFTHeatingIDX = performance1.ChillerEIRFTCoolingIDX;
    performance1.ChillerEIRFPLRHeatingIDX = Curve::GetCurveIndex(*state, "BIVARIATE PART LOAD EIR");
    performance1.MinPartLoadRatClgHtg = 0.3;
    performance1.MaxPartLoadRatClgHtg = 1.0;
    performance1.MaxHeatingLeavingCondTempWasBlank = true;

    ASSERT_GT(performance1.ChillerCapFTCoolingIDX, 0);
    ASSERT_GT(performance1.ChillerEIRFPLRCoolingIDX, 0);
    ASSERT_GT(performance1.ChillerEIRFPLRHeatingIDX, 0);

    wrapper.ChillerHeater(1).initialize(1, performance1, nullptr);
    auto &chillerHeater = wrapper.ChillerHeater(1);
    chillerHeater.sizing.RefCapCooling = 10000.0;
    chillerHeater.sizing.RefCapClgHtg = 10000.0;
    chillerHeater.sizing.RefCOPClgHtg = 4.0;
    chillerHeater.minimumEvaporatorOutletTemp = 5.0;

    auto coolingResult = wrapper.solveCoolingOnly(*state, 1, 1000.0, 1.0, 1.0, 12.0, 30.0);

    EXPECT_EQ(CurrentMode::CoolingOnly, coolingResult.currentMode);
    EXPECT_NEAR(1000.0, coolingResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(200.0, coolingResult.coolingPower, 1.0e-6);
    EXPECT_NEAR(1160.0, coolingResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(0.30, coolingResult.partLoadRatio, 1.0e-9);
    EXPECT_NEAR(1.0 / 3.0, coolingResult.cyclingRatio, 1.0e-9);
    EXPECT_NEAR(coolingResult.partLoadRatio, coolingResult.eirPartLoadCurvePLR, 1.0e-12);
    EXPECT_NEAR(coolingResult.evaporatorOutletTemp, coolingResult.capacityCurveEvaporatorTemp, 1.0e-12);
    EXPECT_NEAR(30.0, coolingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NE(7.0, coolingResult.evaporatorOutletTemp);
    EXPECT_NEAR(0.0, coolingResult.moduleEnergyBalanceResidual(), 1.0e-9);
    chillerHeater.Result = coolingResult;
    chillerHeater.mapResultToPlantConnections();
    EXPECT_NEAR(0.0, chillerHeater.Result.routingEnergyBalanceResidual(), 1.0e-9);

    wrapper.VariableFlowCH = true;
    performance1.CondModeCooling = PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    coolingResult = wrapper.solveCoolingOnly(*state, 1, 1000.0, 1.0, 1.0, 12.0, 30.0);
    Real64 const chilledWaterCp = water->getSpecificHeat(*state, 12.0, "PlantCentralGSHP solver test");
    EXPECT_NEAR(7.0, coolingResult.evaporatorOutletTemp, 1.0e-9);
    EXPECT_NEAR(1000.0 / (chilledWaterCp * 5.0), coolingResult.evaporatorMassFlowRate, 1.0e-9);
    EXPECT_NEAR(coolingResult.condenserOutletTemp, coolingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(0.0, coolingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    auto heatingResult = wrapper.solveHeatingOnly(*state, 1, 1200.0, 1.0, 1.0, 15.0, 40.0);
    EXPECT_EQ(CurrentMode::HeatingOnly, heatingResult.currentMode);
    EXPECT_NEAR(1000.0, heatingResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(250.0, heatingResult.heatingPower, 1.0e-6);
    EXPECT_NEAR(1200.0, heatingResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(0.30, heatingResult.partLoadRatio, 1.0e-9);
    EXPECT_NEAR(1.0 / 3.0, heatingResult.cyclingRatio, 1.0e-9);
    EXPECT_NEAR(heatingResult.partLoadRatio, heatingResult.eirPartLoadCurvePLR, 1.0e-12);
    EXPECT_NEAR(40.0, heatingResult.eirPartLoadCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(heatingResult.evaporatorOutletTemp, heatingResult.capacityCurveEvaporatorTemp, 1.0e-12);
    EXPECT_NEAR(40.0, heatingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(45.0, heatingResult.condenserOutletTemp, 1.0e-9);
    EXPECT_NEAR(0.0, heatingResult.moduleEnergyBalanceResidual(), 1.0e-9);
    chillerHeater.Result = heatingResult;
    chillerHeater.mapResultToPlantConnections();
    EXPECT_NEAR(0.0, chillerHeater.Result.routingEnergyBalanceResidual(), 1.0e-9);

    auto simultaneousResult = wrapper.solveSimultaneous(*state, 1, 1000.0, 1200.0, 1.0, 1.0, 1.0, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::HeatRecovery, simultaneousResult.currentMode);
    EXPECT_NEAR(1000.0, simultaneousResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(250.0, simultaneousResult.compressorPower, 1.0e-6);
    EXPECT_NEAR(1200.0, simultaneousResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(1000.0, simultaneousResult.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(1200.0, simultaneousResult.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(0.0, simultaneousResult.sourceHeatTransfer, 1.0e-6);
    EXPECT_NEAR(0.0, simultaneousResult.moduleEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, simultaneousResult.routingEnergyBalanceResidual(), 1.0e-9);

    simultaneousResult = wrapper.solveSimultaneous(*state, 1, 1000.0, 600.0, 1.0, 1.0, 1.0, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::CoolingDominant, simultaneousResult.currentMode);
    EXPECT_NEAR(1000.0, simultaneousResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(1200.0, simultaneousResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(1000.0, simultaneousResult.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(600.0, simultaneousResult.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(600.0, simultaneousResult.sourceHeatTransfer, 1.0e-6);
    EXPECT_GT(simultaneousResult.capacityCurveCondenserTemp, 15.0);
    EXPECT_LT(simultaneousResult.capacityCurveCondenserTemp, 40.0);
    EXPECT_NEAR(0.0, simultaneousResult.moduleEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, simultaneousResult.routingEnergyBalanceResidual(), 1.0e-9);

    simultaneousResult = wrapper.solveSimultaneous(*state, 1, 1000.0, 600.0, 1.0, 1.0, 0.0, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::HeatRecovery, simultaneousResult.currentMode);
    EXPECT_NEAR(500.0, simultaneousResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(600.0, simultaneousResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(500.0, simultaneousResult.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(600.0, simultaneousResult.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(500.0, simultaneousResult.unmetCoolingLoad, 1.0e-6);
    EXPECT_NEAR(0.0, simultaneousResult.sourceHeatTransfer, 1.0e-6);
    EXPECT_NEAR(0.0, simultaneousResult.moduleEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, simultaneousResult.routingEnergyBalanceResidual(), 1.0e-9);

    simultaneousResult = wrapper.solveSimultaneous(*state, 1, 500.0, 1200.0, 1.0, 1.0, 1.0, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::HeatingDominant, simultaneousResult.currentMode);
    EXPECT_NEAR(1000.0, simultaneousResult.qEvaporator, 1.0e-6);
    EXPECT_NEAR(1200.0, simultaneousResult.qCondenser, 1.0e-6);
    EXPECT_NEAR(500.0, simultaneousResult.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(1200.0, simultaneousResult.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(-500.0, simultaneousResult.sourceHeatTransfer, 1.0e-6);
    EXPECT_GT(simultaneousResult.capacityCurveEvaporatorTemp, 7.0);
    EXPECT_LT(simultaneousResult.capacityCurveEvaporatorTemp, 15.0);
    EXPECT_NEAR(0.0, simultaneousResult.moduleEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, simultaneousResult.routingEnergyBalanceResidual(), 1.0e-9);

    wrapper.VariableFlowCH = false;
    performance1.CondModeHeating = PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    performance1.TempRefCondOutClgHtg = 40.5;
    heatingResult = wrapper.solveHeatingOnly(*state, 1, 5000.0, 1.0, 1.0, 15.0, 40.0);
    EXPECT_GT(heatingResult.condenserOutletTemp, performance1.TempRefCondOutClgHtg);
    EXPECT_NEAR(heatingResult.condenserOutletTemp, heatingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(heatingResult.condenserOutletTemp, heatingResult.eirPartLoadCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(0.0, heatingResult.unmetHeatingLoad, 1.0e-6);
    EXPECT_NEAR(0.0, heatingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    performance1.MaxHeatingLeavingCondTempWasBlank = false;
    performance1.MaxHeatingLeavingCondTemp = 40.1;
    heatingResult = wrapper.solveHeatingOnly(*state, 1, 5000.0, 1.0, 1.0, 15.0, 40.0);
    EXPECT_NEAR(40.1, heatingResult.condenserOutletTemp, 1.0e-9);
    EXPECT_GT(heatingResult.unmetHeatingLoad, 0.0);
    EXPECT_NEAR(heatingResult.qCondenser, heatingResult.availableCondenserCapacity, 1.0e-6);
    EXPECT_NEAR(0.0, heatingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    performance1.MaxHeatingLeavingCondTempWasBlank = true;
    performance1.CondModeCooling = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    performance1.CondModeHeating = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    PlantCentralGSHP::ChillerHeaterPerformanceData performance2 = performance1;
    wrapper.ChillerHeater(2).initialize(2, performance2, nullptr);
    chillerHeater.sizing.EvapMassFlowRateMax = 1.0;
    chillerHeater.sizing.CondMassFlowRateMax = 1.0;
    wrapper.ChillerHeater(2).sizing.RefCapCooling = 5000.0;
    wrapper.ChillerHeater(2).sizing.RefCapClgHtg = 5000.0;
    wrapper.ChillerHeater(2).sizing.RefCOPClgHtg = 4.0;
    wrapper.ChillerHeater(2).sizing.EvapMassFlowRateMax = 1.0;
    wrapper.ChillerHeater(2).sizing.CondMassFlowRateMax = 1.0;
    wrapper.ChillerHeaterNums = 2;
    wrapper.WrapperCoolingLoad = 18000.0;
    wrapper.WrapperHeatingLoad = 21600.0;

    wrapper.CalcSimultaneousModel(*state, 2.0, 2.0, 2.0, 12.0, 40.0, 15.0);
    EXPECT_FALSE(wrapper.SimulClgDominant);
    EXPECT_FALSE(wrapper.SimulHtgDominant);
    EXPECT_EQ(CurrentMode::HeatRecovery, wrapper.ChillerHeater(1).Result.currentMode);
    EXPECT_EQ(CurrentMode::HeatRecovery, wrapper.ChillerHeater(2).Result.currentMode);
    EXPECT_NEAR(10000.0, wrapper.ChillerHeater(1).Result.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(12000.0, wrapper.ChillerHeater(1).Result.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(8000.0, wrapper.ChillerHeater(2).Result.requestedCoolingLoad, 1.0e-6);
    EXPECT_NEAR(9600.0, wrapper.ChillerHeater(2).Result.requestedHeatingLoad, 1.0e-6);
    EXPECT_NEAR(5000.0, wrapper.ChillerHeater(2).Result.coolingDelivered, 1.0e-6);
    EXPECT_NEAR(6000.0, wrapper.ChillerHeater(2).Result.heatingDelivered, 1.0e-6);
    EXPECT_NEAR(3000.0, wrapper.ChillerHeater(2).Result.unmetCoolingLoad, 1.0e-6);
    EXPECT_NEAR(3600.0, wrapper.ChillerHeater(2).Result.unmetHeatingLoad, 1.0e-6);
    EXPECT_NEAR(15000.0, wrapper.Report.CoolingRate, 1.0e-6);
    EXPECT_NEAR(18000.0, wrapper.Report.HeatingRate, 1.0e-6);
    EXPECT_NEAR(0.0, wrapper.Report.GLHERate, 1.0e-6);
    EXPECT_NEAR(3750.0, wrapper.Report.TotElecCoolingPwr + wrapper.Report.TotElecHeatingPwr, 1.0e-6);

    auto const firstDispatchResult = wrapper.ChillerHeater(2).Result;
    wrapper.CalcSimultaneousModel(*state, 2.0, 2.0, 2.0, 12.0, 40.0, 15.0);
    EXPECT_EQ(firstDispatchResult.currentMode, wrapper.ChillerHeater(2).Result.currentMode);
    EXPECT_NEAR(firstDispatchResult.qEvaporator, wrapper.ChillerHeater(2).Result.qEvaporator, 1.0e-9);
    EXPECT_NEAR(firstDispatchResult.qCondenser, wrapper.ChillerHeater(2).Result.qCondenser, 1.0e-9);
    EXPECT_NEAR(firstDispatchResult.unmetCoolingLoad, wrapper.ChillerHeater(2).Result.unmetCoolingLoad, 1.0e-9);
    EXPECT_NEAR(firstDispatchResult.unmetHeatingLoad, wrapper.ChillerHeater(2).Result.unmetHeatingLoad, 1.0e-9);

    auto configureConnectionPerformance = [&wrapper](int const moduleNum, Real64 const referenceCapacity) {
        auto &module = wrapper.ChillerHeater(moduleNum);
        module.sizing.RefCapCooling = referenceCapacity;
        module.sizing.RefCapClgHtg = referenceCapacity;
        module.sizing.RefCOPClgHtg = 4.0;
        module.sizing.ChilledWaterMassFlowRateMax = 0.20;
        module.sizing.HotWaterMassFlowRateMax = 0.10;
        module.sizing.SourceEvapMassFlowRateMax = 0.12;
        module.sizing.SourceCondMassFlowRateMax = 0.15;
    };
    configureConnectionPerformance(1, 10000.0);
    configureConnectionPerformance(2, 5000.0);

    wrapper.WrapperCoolingLoad = 20000.0;
    wrapper.WrapperHeatingLoad = 0.0;
    wrapper.CalcCoolingOnlyModel(*state, 0.30, 0.25, 12.0, 15.0);
    auto const &coolingModule1 = wrapper.ChillerHeater(1).Result;
    auto const &coolingModule2 = wrapper.ChillerHeater(2).Result;
    EXPECT_EQ(CurrentMode::CoolingOnly, coolingModule1.currentMode);
    EXPECT_EQ(CurrentMode::CoolingOnly, coolingModule2.currentMode);
    EXPECT_NEAR(0.20, coolingModule1.chilledWaterMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.10, coolingModule2.chilledWaterMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.15, coolingModule1.sourceMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.10, coolingModule2.sourceMassFlowRate, 1.0e-12);
    EXPECT_LE(coolingModule1.chilledWaterMassFlowRate + coolingModule2.chilledWaterMassFlowRate, 0.30);
    EXPECT_LE(coolingModule1.sourceMassFlowRate + coolingModule2.sourceMassFlowRate, 0.25);
    EXPECT_NEAR(7.0, wrapper.Report.CHWOutletTemp, 1.0e-9);
    Real64 const sourceCp = sourceGlycol->getSpecificHeat(*state, 15.0, "PlantCentralGSHP connection test");
    EXPECT_NEAR(wrapper.Report.GLHERate, 0.25 * sourceCp * (wrapper.Report.GLHEOutletTemp - wrapper.Report.GLHEInletTemp), 1.0e-6);
    EXPECT_NEAR(0.0, coolingModule1.routingEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, coolingModule2.routingEnergyBalanceResidual(), 1.0e-9);

    wrapper.VariableFlowCH = true;
    wrapper.WrapperCoolingLoad = 1000.0;
    wrapper.CalcCoolingOnlyModel(*state, 0.30, 0.25, 12.0, 15.0);
    Real64 const expectedVariableChilledWaterFlow = 1000.0 / (chilledWaterCp * 5.0);
    EXPECT_NEAR(expectedVariableChilledWaterFlow, wrapper.ChillerHeater(1).Result.chilledWaterMassFlowRate, 1.0e-9);
    EXPECT_NEAR(0.0, wrapper.ChillerHeater(2).Result.chilledWaterMassFlowRate, 1.0e-12);
    EXPECT_NEAR(12.0 - 1000.0 / (0.30 * chilledWaterCp), wrapper.Report.CHWOutletTemp, 1.0e-9);
    EXPECT_NEAR(wrapper.Report.GLHERate, 0.25 * sourceCp * (wrapper.Report.GLHEOutletTemp - wrapper.Report.GLHEInletTemp), 1.0e-6);

    wrapper.VariableFlowCH = false;
    wrapper.WrapperCoolingLoad = 0.0;
    wrapper.WrapperHeatingLoad = 20000.0;
    wrapper.CalcHeatingOnlyModel(*state, 0.15, 0.18, 40.0, 15.0);
    auto const &heatingModule1 = wrapper.ChillerHeater(1).Result;
    auto const &heatingModule2 = wrapper.ChillerHeater(2).Result;
    EXPECT_EQ(CurrentMode::HeatingOnly, heatingModule1.currentMode);
    EXPECT_EQ(CurrentMode::HeatingOnly, heatingModule2.currentMode);
    EXPECT_NEAR(0.10, heatingModule1.hotWaterMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.05, heatingModule2.hotWaterMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.12, heatingModule1.sourceMassFlowRate, 1.0e-12);
    EXPECT_NEAR(0.06, heatingModule2.sourceMassFlowRate, 1.0e-12);
    EXPECT_LE(heatingModule1.hotWaterMassFlowRate + heatingModule2.hotWaterMassFlowRate, 0.15);
    EXPECT_LE(heatingModule1.sourceMassFlowRate + heatingModule2.sourceMassFlowRate, 0.18);
    EXPECT_NEAR(45.0, wrapper.Report.HWOutletTemp, 1.0e-9);
    EXPECT_NEAR(-wrapper.Report.GLHERate, 0.18 * sourceCp * (wrapper.Report.GLHEInletTemp - wrapper.Report.GLHEOutletTemp), 1.0e-6);
    EXPECT_NEAR(0.0, heatingModule1.routingEnergyBalanceResidual(), 1.0e-9);
    EXPECT_NEAR(0.0, heatingModule2.routingEnergyBalanceResidual(), 1.0e-9);

    wrapper.WrapperCoolingLoad = 1000.0;
    wrapper.WrapperHeatingLoad = 500.0;
    wrapper.CalcSimultaneousModel(*state, 0.20, 0.10, 0.30, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::CoolingDominant, wrapper.ChillerHeater(1).Result.currentMode);
    EXPECT_NEAR(0.15, wrapper.ChillerHeater(1).Result.sourceMassFlowRate, 1.0e-12);

    wrapper.WrapperCoolingLoad = 500.0;
    wrapper.WrapperHeatingLoad = 1200.0;
    wrapper.CalcSimultaneousModel(*state, 0.20, 0.10, 0.30, 12.0, 40.0, 15.0);
    EXPECT_EQ(CurrentMode::HeatingDominant, wrapper.ChillerHeater(1).Result.currentMode);
    EXPECT_NEAR(0.12, wrapper.ChillerHeater(1).Result.sourceMassFlowRate, 1.0e-12);

    auto &overAllocatedResult1 = wrapper.ChillerHeater(1).Result;
    auto &overAllocatedResult2 = wrapper.ChillerHeater(2).Result;
    overAllocatedResult1 = PlantCentralGSHP::ChillerHeaterResult();
    overAllocatedResult2 = PlantCentralGSHP::ChillerHeaterResult();
    overAllocatedResult1.hotWaterMassFlowRate = 0.20;
    overAllocatedResult1.hotWaterOutletTemp = 46.0;
    overAllocatedResult2.hotWaterMassFlowRate = 0.10;
    overAllocatedResult2.hotWaterOutletTemp = 44.0;
    wrapper.updateWrapperReportingAndNodes(*state, 0.0, 0.15, 0.0, 12.0, 40.0, 15.0);
    EXPECT_NEAR((0.20 * 46.0 + 0.10 * 44.0) / 0.30, wrapper.Report.HWOutletTemp, 1.0e-12);
    EXPECT_NE(40.0, wrapper.Report.HWOutletTemp);

    wrapper.ChillerHeater(1).sizing.EvapVolFlowRate = 0.0010;
    wrapper.ChillerHeater(1).sizing.CondVolFlowRate = 0.0005;
    performance1.DesignHotWaterVolFlowRate = 0.0003;
    wrapper.ChillerHeater(2).sizing.EvapVolFlowRate = 0.0020;
    wrapper.ChillerHeater(2).sizing.CondVolFlowRate = 0.0025;
    performance2.DesignHotWaterVolFlowRate = 0.0004;
    wrapper.initializeDesignFlowLimits(*state);

    Real64 const chilledWaterDensity = water->getDensity(*state, Constant::CWInitConvTemp, "PlantCentralGSHP design flow test");
    Real64 const hotWaterDensity = water->getDensity(*state, Constant::HWInitConvTemp, "PlantCentralGSHP design flow test");
    Real64 const sourceDensity = sourceGlycol->getDensity(*state, Constant::CWInitConvTemp, "PlantCentralGSHP design flow test");
    EXPECT_NEAR(0.0030, wrapper.CHWVolFlowRate, 1.0e-12);
    EXPECT_NEAR(0.0007, wrapper.HWVolFlowRate, 1.0e-12);
    EXPECT_NEAR(0.0035, wrapper.GLHEVolFlowRate, 1.0e-12);
    EXPECT_NEAR(0.0030 * chilledWaterDensity, wrapper.CHWMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0007 * hotWaterDensity, wrapper.HWMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0035 * sourceDensity, wrapper.GLHEMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0010 * chilledWaterDensity, wrapper.ChillerHeater(1).sizing.ChilledWaterMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0003 * hotWaterDensity, wrapper.ChillerHeater(1).sizing.HotWaterMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0010 * sourceDensity, wrapper.ChillerHeater(1).sizing.SourceEvapMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0005 * sourceDensity, wrapper.ChillerHeater(1).sizing.SourceCondMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(wrapper.CHWMassFlowRateMax, state->dataLoopNodes->Node(wrapper.CHWInletNodeNum).MassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(wrapper.HWMassFlowRateMax, state->dataLoopNodes->Node(wrapper.HWInletNodeNum).MassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(wrapper.GLHEMassFlowRateMax, state->dataLoopNodes->Node(wrapper.GLHEInletNodeNum).MassFlowRateMax, 1.0e-9);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_SequentialFlowAllocationContracts)
{
    FlowAllocationContract const variableFlow{1.20, {0.40, 0.80}, {0.75, 1.00}, {0.40, 0.80}};
    EXPECT_TRUE(checkSequentialFlowAllocation(variableFlow));

    FlowAllocationContract const heterogeneousConstantFlow{1.50, {0.80, 0.80, 0.80}, {1.00, 0.50, 1.00}, {0.80, 0.50, 0.20}};
    EXPECT_TRUE(checkSequentialFlowAllocation(heterogeneousConstantFlow));

    FlowAllocationContract const overAllocated{1.50, {0.80, 0.80, 0.80}, {1.00, 0.50, 1.00}, {0.80, 0.50, 0.80}};
    EXPECT_FALSE(checkSequentialFlowAllocation(overAllocated));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_WaterAndGlycolNodeHeatTransferContracts)
{
    std::string const idf_objects = delimited_string({"FluidProperties:GlycolConcentration,",
                                                      "  GLHXFluid,        !- Name",
                                                      "  PropyleneGlycol, !- Glycol Type",
                                                      "  ,                 !- User Defined Glycol Name",
                                                      "  0.3;              !- Glycol Concentration"});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());
    state->init_state(*state);

    auto *water = Fluid::GetWater(*state);
    auto *sourceGlycol = Fluid::GetGlycol(*state, "GLHXFLUID");
    ASSERT_NE(nullptr, water);
    ASSERT_NE(nullptr, sourceGlycol);

    Real64 constexpr chilledWaterInletTemp = 12.0;
    Real64 constexpr chilledWaterOutletTemp = 7.0;
    Real64 constexpr chilledWaterMassFlow = 1.0;
    Real64 const chilledWaterCp = water->getSpecificHeat(*state, chilledWaterInletTemp, "PlantCentralGSHP contract test");
    Real64 const cooling = chilledWaterMassFlow * chilledWaterCp * (chilledWaterInletTemp - chilledWaterOutletTemp);
    EXPECT_TRUE(checkLoopHeatTransfer(cooling, chilledWaterMassFlow, chilledWaterCp, chilledWaterInletTemp, chilledWaterOutletTemp));

    Real64 constexpr hotWaterInletTemp = 40.0;
    Real64 constexpr hotWaterOutletTemp = 45.0;
    Real64 constexpr hotWaterMassFlow = 0.8;
    Real64 const hotWaterCp = water->getSpecificHeat(*state, hotWaterInletTemp, "PlantCentralGSHP contract test");
    Real64 const heating = hotWaterMassFlow * hotWaterCp * (hotWaterOutletTemp - hotWaterInletTemp);
    EXPECT_TRUE(checkLoopHeatTransfer(heating, hotWaterMassFlow, hotWaterCp, hotWaterInletTemp, hotWaterOutletTemp));

    Real64 constexpr sourceInletTemp = 15.0;
    Real64 constexpr sourceOutletTemp = 17.0;
    Real64 constexpr sourceMassFlow = 1.2;
    Real64 const sourceCp = sourceGlycol->getSpecificHeat(*state, sourceInletTemp, "PlantCentralGSHP contract test");
    Real64 const sourceHeat = sourceMassFlow * sourceCp * (sourceOutletTemp - sourceInletTemp);
    EXPECT_TRUE(checkLoopHeatTransfer(sourceHeat, sourceMassFlow, sourceCp, sourceInletTemp, sourceOutletTemp));

    EXPECT_FALSE(contractNear(sourceCp, chilledWaterCp));
    EXPECT_FALSE(checkLoopHeatTransfer(sourceHeat, sourceMassFlow, chilledWaterCp, sourceInletTemp, sourceOutletTemp));
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultMapsPlantConnections)
{
    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    performance.OpenMotorEff = 0.80;
    PlantCentralGSHP::ChillerHeaterModule chillerHeater;
    chillerHeater.initialize(1, performance, nullptr);

    auto configureResult = [&chillerHeater](CurrentMode const mode) {
        auto &result = chillerHeater.Result;
        result = PlantCentralGSHP::ChillerHeaterResult();
        result.currentMode = mode;
        result.isAvailable = true;
        result.qEvaporator = 8000.0;
        result.qCondenser = 9600.0;
        result.coolingPower = 2000.0;
        result.evaporatorInletTemp = 12.0;
        result.evaporatorOutletTemp = 7.0;
        result.evaporatorMassFlowRate = 0.40;
        result.condenserInletTemp = 30.0;
        result.condenserOutletTemp = 35.0;
        result.condenserMassFlowRate = 0.50;
        chillerHeater.mapResultToPlantConnections();
    };

    std::array<CurrentMode, 2> const coolingModes = {CurrentMode::CoolingOnly, CurrentMode::CoolingDominant};
    for (CurrentMode const mode : coolingModes) {
        SCOPED_TRACE(modeName(mode));
        configureResult(mode);
        auto const &result = chillerHeater.Result;
        EXPECT_TRUE(result.isAvailable);
        EXPECT_TRUE(result.isRunning);
        EXPECT_DOUBLE_EQ(8000.0, result.coolingDelivered);
        EXPECT_DOUBLE_EQ(9600.0, result.sourceHeatTransfer);
        EXPECT_DOUBLE_EQ(0.40, result.chilledWaterMassFlowRate);
        EXPECT_DOUBLE_EQ(0.50, result.sourceMassFlowRate);
        EXPECT_DOUBLE_EQ(0.0, result.hotWaterMassFlowRate);
        EXPECT_NEAR(0.0, result.moduleEnergyBalanceResidual(), contractTolerance);
        EXPECT_NEAR(0.0, result.routingEnergyBalanceResidual(), contractTolerance);
    }

    std::array<CurrentMode, 2> const heatingModes = {CurrentMode::HeatingOnly, CurrentMode::HeatingDominant};
    for (CurrentMode const mode : heatingModes) {
        SCOPED_TRACE(modeName(mode));
        configureResult(mode);
        auto const &result = chillerHeater.Result;
        EXPECT_DOUBLE_EQ(9600.0, result.heatingDelivered);
        EXPECT_DOUBLE_EQ(-8000.0, result.sourceHeatTransfer);
        EXPECT_DOUBLE_EQ(0.50, result.hotWaterMassFlowRate);
        EXPECT_DOUBLE_EQ(0.40, result.sourceMassFlowRate);
        EXPECT_DOUBLE_EQ(0.0, result.chilledWaterMassFlowRate);
        EXPECT_NEAR(0.0, result.routingEnergyBalanceResidual(), contractTolerance);
    }

    configureResult(CurrentMode::HeatRecovery);
    auto const &result = chillerHeater.Result;
    EXPECT_DOUBLE_EQ(8000.0, result.coolingDelivered);
    EXPECT_DOUBLE_EQ(9600.0, result.heatingDelivered);
    EXPECT_DOUBLE_EQ(9600.0, result.heatRecovered);
    EXPECT_DOUBLE_EQ(0.0, result.sourceHeatTransfer);
    EXPECT_DOUBLE_EQ(0.40, result.chilledWaterMassFlowRate);
    EXPECT_DOUBLE_EQ(0.50, result.hotWaterMassFlowRate);
    EXPECT_DOUBLE_EQ(0.0, result.sourceMassFlowRate);
    EXPECT_DOUBLE_EQ(2000.0, result.compressorPower);
    EXPECT_DOUBLE_EQ(1600.0, result.motorHeatToRefrigerant);
    EXPECT_DOUBLE_EQ(400.0, result.motorHeatLoss);
    EXPECT_NEAR(0.0, result.moduleEnergyBalanceResidual(), contractTolerance);
    EXPECT_NEAR(0.0, result.routingEnergyBalanceResidual(), contractTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultDrivesEnergyIntegration)
{
    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    performance.OpenMotorEff = 0.80;
    PlantCentralGSHP::ChillerHeaterModule chillerHeater;
    chillerHeater.initialize(1, performance, nullptr);
    auto &result = chillerHeater.Result;
    result.currentMode = CurrentMode::HeatRecovery;
    result.isAvailable = true;
    result.qEvaporator = 7000.0;
    result.qCondenser = 7800.0;
    result.coolingPower = 1000.0;
    result.partLoadRatio = 0.75;
    result.cyclingRatio = 0.50;
    result.capacityTemperatureModifier = 0.95;
    result.eirTemperatureModifier = 1.05;
    result.eirPartLoadModifier = 0.90;
    result.actualCOP = 4.0;
    result.evaporatorInletTemp = 12.0;
    result.evaporatorOutletTemp = 8.0;
    result.evaporatorMassFlowRate = 0.40;
    result.condenserInletTemp = 30.0;
    result.condenserOutletTemp = 35.0;
    result.condenserMassFlowRate = 0.50;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.updateResultEnergies(60.0);

    EXPECT_DOUBLE_EQ(7000.0, result.qEvaporator);
    EXPECT_DOUBLE_EQ(7800.0, result.qCondenser);
    EXPECT_DOUBLE_EQ(1000.0, result.coolingPower);
    EXPECT_DOUBLE_EQ(60000.0, result.coolingEnergy);
    EXPECT_DOUBLE_EQ(420000.0, result.evaporatorEnergy);
    EXPECT_DOUBLE_EQ(468000.0, result.condenserEnergy);
    EXPECT_DOUBLE_EQ(8.0, result.chilledWaterOutletTemp);
    EXPECT_DOUBLE_EQ(35.0, result.hotWaterOutletTemp);
    EXPECT_DOUBLE_EQ(7000.0, result.coolingDelivered);
    EXPECT_DOUBLE_EQ(7800.0, result.heatingDelivered);
    EXPECT_DOUBLE_EQ(1000.0, result.compressorPower);
    EXPECT_DOUBLE_EQ(800.0, result.motorHeatToRefrigerant);
    EXPECT_DOUBLE_EQ(200.0, result.motorHeatLoss);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultResetIsComplete)
{
    PlantCentralGSHP::ChillerHeaterPerformanceData performance;
    performance.OpenMotorEff = 0.80;
    PlantCentralGSHP::ChillerHeaterModule chillerHeater;
    chillerHeater.initialize(1, performance, nullptr);
    chillerHeater.Result.currentMode = CurrentMode::CoolingOnly;
    chillerHeater.Result.isAvailable = true;
    chillerHeater.Result.qEvaporator = 8000.0;
    chillerHeater.Result.qCondenser = 9600.0;
    chillerHeater.Result.coolingPower = 2000.0;
    chillerHeater.Result.evaporatorMassFlowRate = 0.40;
    chillerHeater.Result.condenserMassFlowRate = 0.50;
    chillerHeater.mapResultToPlantConnections();

    chillerHeater.resetResult(12.0, 30.0);

    EXPECT_EQ(CurrentMode::Off, chillerHeater.Result.currentMode);
    EXPECT_FALSE(chillerHeater.Result.isRunning);
    EXPECT_FALSE(chillerHeater.Result.isAvailable);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.compressorPower);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.qEvaporator);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.qCondenser);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.evaporatorMassFlowRate);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.condenserMassFlowRate);
    EXPECT_DOUBLE_EQ(12.0, chillerHeater.Result.evaporatorInletTemp);
    EXPECT_DOUBLE_EQ(12.0, chillerHeater.Result.evaporatorOutletTemp);
    EXPECT_DOUBLE_EQ(30.0, chillerHeater.Result.condenserInletTemp);
    EXPECT_DOUBLE_EQ(30.0, chillerHeater.Result.condenserOutletTemp);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.coolingDelivered);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.heatingDelivered);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.sourceHeatTransfer);
}
