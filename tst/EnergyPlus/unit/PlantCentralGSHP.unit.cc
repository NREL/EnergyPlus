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
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantUtilities.hh>

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
    state->dataPlantCentralGSHP->numChillerHeaters = NumChillerHeaters;
    state->dataPlantCentralGSHP->ChillerHeater.allocate(NumChillerHeaters);
    state->dataPlantCentralGSHP->ChillerHeater(1).ConstantFlow = false;
    state->dataPlantCentralGSHP->ChillerHeater(1).VariableFlow = true;

    state->dataPlantCentralGSHP->ChillerHeater(1).SizFac = 1.2;

    state->dataPlantCentralGSHP->ChillerHeater(1).RefCapCooling = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).RefCapCoolingWasAutoSized = true;

    state->dataPlantCentralGSHP->ChillerHeater(1).EvapVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).EvapVolFlowRateWasAutoSized = true;

    state->dataPlantCentralGSHP->ChillerHeater(1).CondVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).CondVolFlowRateWasAutoSized = true;

    // Needed for calcs
    state->dataPlantCentralGSHP->ChillerHeater(1).RefCOPCooling = 1.5;
    state->dataPlantCentralGSHP->ChillerHeater(1).OpenMotorEff = 0.98;
    state->dataPlantCentralGSHP->ChillerHeater(1).TempRefCondInCooling = 29.4;
    state->dataPlantCentralGSHP->ChillerHeater(1).ClgHtgToCoolingCapRatio = 0.74;
    state->dataPlantCentralGSHP->ChillerHeater(1).ClgHtgtoCogPowerRatio = 1.38;

    // Add the References onto the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1) = state->dataPlantCentralGSHP->ChillerHeater(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2) = state->dataPlantCentralGSHP->ChillerHeater(1);

    // De-allocate temporary arrays (happens in GetInput too...)
    state->dataPlantCentralGSHP->ChillerHeater.deallocate();

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

    Real64 rho_cond =
        state->dataPlnt->PlantLoop(PltSizCondNum)
            .glycol->getDensity(*state, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).TempRefCondInCooling, "ChillerHeater_Autosize_TEST");

    Real64 Cp_cond = state->dataPlnt->PlantLoop(PltSizCondNum)
                         .glycol->getSpecificHeat(
                             *state, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).TempRefCondInCooling, "ChillerHeater_Autosize_TEST");

    // Note: Each individual chiller heater module is sized to be capable of supporting the total load on the wrapper

    // Flow is multiplied by the SizFac
    Real64 EvapVolFlowRateExpected =
        state->dataSize->PlantSizData(PltSizNum).DesVolFlowRate * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).SizFac;

    Real64 RefCapCoolingExpected = rho_evap * Cp_evap * EvapVolFlowRateExpected * state->dataSize->PlantSizData(PltSizNum).DeltaT;

    Real64 CondVolFlowRateExpected = RefCapCoolingExpected *
                                     (1.0 + (1.0 / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPCooling) *
                                                state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).OpenMotorEff) /
                                     (rho_cond * Cp_cond * state->dataSize->PlantSizData(PltSizCondNum).DeltaT);

    // now call sizing routine
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlantCentralGSHP->Wrapper(1).SizeWrapper(*state);

    // Careful of actually using PlantCentralGSHP::Wrapper(1).ChillerHeater(1) and not PlantCentralGSHP::ChillerHeater since this array isn't used
    // anymore by the module
    EXPECT_DOUBLE_EQ(EvapVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(RefCapCoolingExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCapCooling);

    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).CondVolFlowRate);
    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2).CondVolFlowRate);

    // Ensure that stuff that other quantities that depends on RefCapCooling are also initialized properly
    // Heating Cap
    Real64 RefCapClgHtgExpected = RefCapCoolingExpected * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).ClgHtgToCoolingCapRatio;
    EXPECT_DOUBLE_EQ(RefCapClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCapClgHtg);

    // Heating Power: Calc cooling Power = Cap / COP, and multiply by ratio
    Real64 RefPowerClgHtgExpected = (RefCapCoolingExpected / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPCooling) *
                                    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).ClgHtgtoCogPowerRatio;
    EXPECT_DOUBLE_EQ(RefPowerClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefPowerClgHtg);

    // Heating COP = Heating Cap / Heating Power
    Real64 RefCOPClgHtgExpected = RefCapClgHtgExpected / RefPowerClgHtgExpected;
    EXPECT_DOUBLE_EQ(RefCOPClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPClgHtg);

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

    auto &module1 = wrapper.ChillerHeater(1);
    module1.RefCapCooling = 10000.0;
    module1.RefCOPCooling = 5.0;
    module1.MinPartLoadRatCooling = 0.20;
    module1.MaxPartLoadRatCooling = 1.00;
    module1.OptPartLoadRatCooling = 0.80;
    module1.RefCapClgHtg = 8000.0;
    module1.RefPowerClgHtg = 2000.0;
    module1.MinPartLoadRatClgHtg = 0.25;
    module1.MaxPartLoadRatClgHtg = 1.10;
    module1.OptPartLoadRatClgHtg = 0.75;
    module1.OpenMotorEff = 0.80;

    auto &module2 = wrapper.ChillerHeater(2);
    module2.RefCapCooling = 6000.0;
    module2.RefCOPCooling = 3.0;
    module2.MinPartLoadRatCooling = 0.10;
    module2.MaxPartLoadRatCooling = 1.20;
    module2.OptPartLoadRatCooling = 0.70;
    module2.RefCapClgHtg = 12000.0;
    module2.RefPowerClgHtg = 3000.0;
    module2.MinPartLoadRatClgHtg = 0.30;
    module2.MaxPartLoadRatClgHtg = 0.90;
    module2.OptPartLoadRatClgHtg = 0.60;
    module2.OpenMotorEff = 0.50;

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

    module2.MaxPartLoadRatClgHtg = 1.20;
    module2.OptPartLoadRatClgHtg = 0.80;
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

    auto &module = wrapper.ChillerHeater(1);
    module.Name = "SOURCE SIZING ONLY";
    module.SizFac = 1.0;
    module.EvapVolFlowRate = 0.001;
    module.RefCapCooling = 10000.0;
    module.RefCOPCooling = 4.0;
    module.OpenMotorEff = 0.80;
    module.TempRefCondInCooling = 30.0;
    module.CondVolFlowRate = DataSizing::AutoSize;
    module.CondVolFlowRateWasAutoSized = true;
    module.DesignHotWaterVolFlowRate = 0.0007;

    Real64 const sourceDensity = water->getDensity(*state, module.TempRefCondInCooling, "PlantCentralGSHP source sizing test");
    Real64 const sourceSpecificHeat = water->getSpecificHeat(*state, module.TempRefCondInCooling, "PlantCentralGSHP source sizing test");
    Real64 const expectedSourceCondenserFlow = module.RefCapCooling * (1.0 + module.OpenMotorEff / module.RefCOPCooling) /
                                               (state->dataSize->PlantSizData(1).DeltaT * sourceSpecificHeat * sourceDensity);

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    wrapper.SizeWrapper(*state);

    EXPECT_NEAR(expectedSourceCondenserFlow, module.CondVolFlowRate, 1.0e-12);
    EXPECT_DOUBLE_EQ(module.EvapVolFlowRate, module.tmpEvapVolFlowRate);
    EXPECT_NEAR(expectedSourceCondenserFlow, module.tmpCondVolFlowRate, 1.0e-12);
    ASSERT_EQ(3u, state->dataSize->CompDesWaterFlow.size());
    EXPECT_DOUBLE_EQ(module.EvapVolFlowRate, state->dataSize->CompDesWaterFlow(1).DesVolFlowRate);
    EXPECT_DOUBLE_EQ(module.DesignHotWaterVolFlowRate, state->dataSize->CompDesWaterFlow(2).DesVolFlowRate);
    EXPECT_NEAR(std::max(module.EvapVolFlowRate, expectedSourceCondenserFlow), state->dataSize->CompDesWaterFlow(3).DesVolFlowRate, 1.0e-12);
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

    auto &module = wrapper.ChillerHeater(1);
    module.Name = "HARD SIZED MODULE";
    module.SizFac = 1.0;
    module.EvapVolFlowRate = 0.001;
    module.RefCapCooling = 1000.0;
    module.RefCOPCooling = 5.0;
    module.OpenMotorEff = 1.0;
    module.TempRefCondInCooling = 30.0;
    module.CondVolFlowRate = 0.001;
    module.DesignHotWaterVolFlowRate = 0.002;

    state->dataGlobal->DisplayExtraWarnings = true;
    state->dataSize->AutoVsHardSizingThreshold = 0.01;
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;
    wrapper.SizeWrapper(*state);

    EXPECT_DOUBLE_EQ(0.001, module.EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(1000.0, module.RefCapCooling);
    EXPECT_DOUBLE_EQ(0.001, module.CondVolFlowRate);
    EXPECT_DOUBLE_EQ(0.001, module.tmpEvapVolFlowRate);
    EXPECT_DOUBLE_EQ(0.001, module.tmpCondVolFlowRate);
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
    EXPECT_FALSE(state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).MaxHeatingLeavingCondTempWasBlank);
    EXPECT_DOUBLE_EQ(55.0, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).MaxHeatingLeavingCondTemp);

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

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_adjustChillerHeaterCondFlowTemp)
{
    state->dataFluid->init_state(*state);
    state->dataPlantCentralGSHP->Wrapper.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(1);
    auto &thisWrap = state->dataPlantCentralGSHP->Wrapper(1);
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).glycol = Fluid::GetWater(*state);
    thisWrap.HWPlantLoc.loopNum = 1;
    PlantUtilities::SetPlantLocationLinks(*state, thisWrap.HWPlantLoc);

    Real64 qCondenser;
    Real64 condMassFlowRate;
    Real64 condOutletTemp;
    Real64 condInletTemp;
    Real64 condDeltaTemp;
    Real64 expCondenser;
    Real64 expMassFlowRate;
    Real64 expOutletTemp;
    Real64 constexpr allowedTolerance = 0.0001;

    // Test 1: Variable Flow--qCondenser is less than what the conditions say (mass flow reduced, nothing else changes)
    qCondenser = 1000.0;
    condMassFlowRate = 1.0;
    condOutletTemp = 60.0;
    condInletTemp = 59.0;
    condDeltaTemp = 1.0;
    thisWrap.VariableFlowCH = true;
    expCondenser = 1000.0;
    expMassFlowRate = 0.23897;
    expOutletTemp = 60.0;
    thisWrap.adjustChillerHeaterCondFlowTemp(*state, qCondenser, condMassFlowRate, condOutletTemp, condInletTemp, condDeltaTemp);
    EXPECT_NEAR(qCondenser, expCondenser, allowedTolerance);
    EXPECT_NEAR(condMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(condOutletTemp, expOutletTemp, allowedTolerance);

    // Test 2: Variable Flow--qCondenser is greater than what conditions say (load reduced, nothing else changes)
    qCondenser = 5000.0;
    condMassFlowRate = 1.0;
    condOutletTemp = 60.0;
    condInletTemp = 59.0;
    condDeltaTemp = 1.0;
    thisWrap.VariableFlowCH = true;
    expCondenser = 4184.6;
    expMassFlowRate = 1.0;
    expOutletTemp = 60.0;
    thisWrap.adjustChillerHeaterCondFlowTemp(*state, qCondenser, condMassFlowRate, condOutletTemp, condInletTemp, condDeltaTemp);
    EXPECT_NEAR(qCondenser, expCondenser, allowedTolerance);
    EXPECT_NEAR(condMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(condOutletTemp, expOutletTemp, allowedTolerance);

    // Test 3: Constant Flow--Outlet Temp greater than calculated outlet temp (outlet temp changes, nothing else changes)
    qCondenser = 1000.0;
    condMassFlowRate = 1.0;
    condOutletTemp = 60.0;
    condInletTemp = 59.0;
    condDeltaTemp = 1.0;
    thisWrap.VariableFlowCH = false;
    expCondenser = 1000.0;
    expMassFlowRate = 1.0;
    expOutletTemp = 59.23897;
    thisWrap.adjustChillerHeaterCondFlowTemp(*state, qCondenser, condMassFlowRate, condOutletTemp, condInletTemp, condDeltaTemp);
    EXPECT_NEAR(qCondenser, expCondenser, allowedTolerance);
    EXPECT_NEAR(condMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(condOutletTemp, expOutletTemp, allowedTolerance);

    // Test 4: Constant Flow--Outlet Temp less than calculated outlet temp (load changes, nothing else changes)
    qCondenser = 8369.2;
    condMassFlowRate = 1.0;
    condOutletTemp = 60.0;
    condInletTemp = 59.0;
    condDeltaTemp = 1.0;
    thisWrap.VariableFlowCH = false;
    expCondenser = 4184.6;
    expMassFlowRate = 1.0;
    expOutletTemp = 60.0;
    thisWrap.adjustChillerHeaterCondFlowTemp(*state, qCondenser, condMassFlowRate, condOutletTemp, condInletTemp, condDeltaTemp);
    EXPECT_NEAR(qCondenser, expCondenser, allowedTolerance);
    EXPECT_NEAR(condMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(condOutletTemp, expOutletTemp, allowedTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_adjustChillerHeaterEvapFlowTemp)
{
    state->dataFluid->init_state(*state);
    state->dataPlantCentralGSHP->Wrapper.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(1);
    auto &thisWrap = state->dataPlantCentralGSHP->Wrapper(1);
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).glycol = Fluid::GetWater(*state);
    thisWrap.HWPlantLoc.loopNum = 1;
    PlantUtilities::SetPlantLocationLinks(*state, thisWrap.HWPlantLoc);

    Real64 qEvaporator;
    Real64 evapMassFlowRate;
    Real64 evapOutletTemp;
    Real64 evapInletTemp;
    Real64 expMassFlowRate;
    Real64 expOutletTemp;
    Real64 constexpr allowedTolerance = 0.0001;

    // Test 1a: qEvaporator is too low, flow rate set to zero and outlet temp set to inlet temp
    qEvaporator = 0.00001;
    evapMassFlowRate = 1.0;
    evapOutletTemp = 34.0;
    evapInletTemp = 35.0;
    thisWrap.VariableFlowCH = false;
    expMassFlowRate = 0.0;
    expOutletTemp = 35.0;
    thisWrap.adjustChillerHeaterEvapFlowTemp(*state, qEvaporator, evapMassFlowRate, evapOutletTemp, evapInletTemp);
    EXPECT_NEAR(evapMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expOutletTemp, allowedTolerance);

    // Test 1b: delta T is zero, load and flow rate set to zero and outlet temp set to inlet temp
    qEvaporator = 1000.0;
    evapMassFlowRate = 1.0;
    evapOutletTemp = 35.0;
    evapInletTemp = 35.0;
    thisWrap.VariableFlowCH = false;
    expMassFlowRate = 0.0;
    expOutletTemp = 35.0;
    thisWrap.adjustChillerHeaterEvapFlowTemp(*state, qEvaporator, evapMassFlowRate, evapOutletTemp, evapInletTemp);
    EXPECT_NEAR(evapMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expOutletTemp, allowedTolerance);

    // Test 2a: Variable Flow, Load higher than max flow rate passed in, keep flow rate and adjust outlet temp
    qEvaporator = 5000.0;
    evapMassFlowRate = 1.0;
    evapOutletTemp = 34.0;
    evapInletTemp = 35.0;
    thisWrap.VariableFlowCH = true;
    expMassFlowRate = 1.0;
    expOutletTemp = 33.80383;
    thisWrap.adjustChillerHeaterEvapFlowTemp(*state, qEvaporator, evapMassFlowRate, evapOutletTemp, evapInletTemp);
    EXPECT_NEAR(evapMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expOutletTemp, allowedTolerance);

    // Test 2b: Variable Flow, Load lower than max flow rate passed in, adjust flow rate and keep outlet temp
    qEvaporator = 1045.0;
    evapMassFlowRate = 1.0;
    evapOutletTemp = 34.0;
    evapInletTemp = 35.0;
    thisWrap.VariableFlowCH = true;
    expMassFlowRate = 0.25;
    expOutletTemp = 34.0;
    thisWrap.adjustChillerHeaterEvapFlowTemp(*state, qEvaporator, evapMassFlowRate, evapOutletTemp, evapInletTemp);
    EXPECT_NEAR(evapMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expOutletTemp, allowedTolerance);

    // Test 3: Constant Flow--adjust outlet temperature
    qEvaporator = 2090.0;
    evapMassFlowRate = 1.0;
    evapOutletTemp = 34.0;
    evapInletTemp = 35.0;
    thisWrap.VariableFlowCH = false;
    expMassFlowRate = 1.0;
    expOutletTemp = 34.5;
    thisWrap.adjustChillerHeaterEvapFlowTemp(*state, qEvaporator, evapMassFlowRate, evapOutletTemp, evapInletTemp);
    EXPECT_NEAR(evapMassFlowRate, expMassFlowRate, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expOutletTemp, allowedTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_setChillerHeaterCondTemp)
{
    state->dataPlantCentralGSHP->Wrapper.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(1);
    auto &thisWrap = state->dataPlantCentralGSHP->Wrapper(1);
    auto &thisCH = thisWrap.ChillerHeater(1);

    Real64 functionAnswer;
    Real64 expectedAnswer;
    Real64 constexpr allowedTolerance = 0.001;
    Real64 condEnterTemp;
    Real64 condLeaveTemp;
    int chillNum = 1;

    // Test 1: get the condenser entering temperature
    functionAnswer = 0.0;
    thisCH.CondMode = EnergyPlus::PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    condEnterTemp = 55.5;
    condLeaveTemp = 44.4;
    expectedAnswer = 55.5;
    functionAnswer = thisWrap.setChillerHeaterCondTemp(*state, chillNum, condEnterTemp, condLeaveTemp);
    EXPECT_NEAR(functionAnswer, expectedAnswer, allowedTolerance);

    // Test 2: get the condenser leaving temperature
    functionAnswer = 0.0;
    thisCH.CondMode = EnergyPlus::PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    condEnterTemp = 55.5;
    condLeaveTemp = 44.4;
    expectedAnswer = 44.4;
    functionAnswer = thisWrap.setChillerHeaterCondTemp(*state, chillNum, condEnterTemp, condLeaveTemp);
    EXPECT_NEAR(functionAnswer, expectedAnswer, allowedTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_checkEvapOutletTemp)
{
    state->dataPlantCentralGSHP->Wrapper.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(1);
    auto &thisWrap = state->dataPlantCentralGSHP->Wrapper(1);
    auto &thisCH = thisWrap.ChillerHeater(1);

    int chNum = 1;
    Real64 evapOutletTemp;
    Real64 lowTempLimitEout;
    Real64 evapInletTemp;
    Real64 qEvaporator;
    Real64 evapMassFlowRate;
    Real64 Cp = 4000.0;
    Real64 expQEvap;
    Real64 expTout;
    Real64 constexpr allowedTolerance = 0.0001;

    // Test 1a: Evaporator outlet temperature lower the evaporator outlet low temperature limit, adjust outlet and load
    thisCH.EvapOutletNode.TempMin = 5.0;
    evapInletTemp = 10.0;
    evapOutletTemp = 8.0;
    lowTempLimitEout = 9.0;
    qEvaporator = 4000.0;
    evapMassFlowRate = 0.5;
    expQEvap = 2000.0;
    expTout = 9.0;
    PlantCentralGSHP::CurrentMode controlMode = PlantCentralGSHP::CurrentMode::CoolingDominant;
    thisWrap.checkEvapOutletTemp(*state, chNum, evapOutletTemp, lowTempLimitEout, evapInletTemp, qEvaporator, evapMassFlowRate, Cp, controlMode);
    EXPECT_NEAR(qEvaporator, expQEvap, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expTout, allowedTolerance);

    // Test 1b: Evaporator outlet temperature lower the evaporator outlet low temperature limit and inlet temp at or below lowTempLimitEout,
    //          zero flow and set outlet temperature to inlet temperature
    thisCH.EvapOutletNode.TempMin = 5.0;
    evapInletTemp = 8.0;
    evapOutletTemp = 7.0;
    lowTempLimitEout = 9.0;
    qEvaporator = 2000.0;
    evapMassFlowRate = 0.5;
    expQEvap = 0.0;
    expTout = 8.0;
    thisWrap.checkEvapOutletTemp(*state, chNum, evapOutletTemp, lowTempLimitEout, evapInletTemp, qEvaporator, evapMassFlowRate, Cp, controlMode);
    EXPECT_NEAR(qEvaporator, expQEvap, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expTout, allowedTolerance);

    // Test 2a: Evaporator outlet temperature lower the node minimum temperature limit, adjust outlet and load
    thisCH.EvapOutletNode.TempMin = 9.0;
    evapInletTemp = 10.0;
    evapOutletTemp = 8.0;
    lowTempLimitEout = 5.0;
    qEvaporator = 4000.0;
    evapMassFlowRate = 0.5;
    expQEvap = 2000.0;
    expTout = 9.0;
    thisWrap.checkEvapOutletTemp(*state, chNum, evapOutletTemp, lowTempLimitEout, evapInletTemp, qEvaporator, evapMassFlowRate, Cp, controlMode);
    EXPECT_NEAR(qEvaporator, expQEvap, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expTout, allowedTolerance);

    // Test 2b: Evaporator outlet temperature lower the node minimum temperature limit and inlet temp at or below node temperature limt,
    //          zero flow and set outlet temperature to inlet temperature
    thisCH.EvapOutletNode.TempMin = 9.0;
    evapInletTemp = 8.0;
    evapOutletTemp = 7.0;
    lowTempLimitEout = 5.0;
    qEvaporator = 2000.0;
    evapMassFlowRate = 0.5;
    expQEvap = 0.0;
    expTout = 8.0;
    thisWrap.checkEvapOutletTemp(*state, chNum, evapOutletTemp, lowTempLimitEout, evapInletTemp, qEvaporator, evapMassFlowRate, Cp, controlMode);
    EXPECT_NEAR(qEvaporator, expQEvap, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expTout, allowedTolerance);

    // Test 3: Everything is fine, no changes to anything
    thisCH.EvapOutletNode.TempMin = 5.0;
    evapInletTemp = 8.0;
    evapOutletTemp = 6.0;
    lowTempLimitEout = 5.0;
    qEvaporator = 4000.0;
    evapMassFlowRate = 0.5;
    expQEvap = 4000.0;
    expTout = 6.0;
    thisWrap.checkEvapOutletTemp(*state, chNum, evapOutletTemp, lowTempLimitEout, evapInletTemp, qEvaporator, evapMassFlowRate, Cp, controlMode);
    EXPECT_NEAR(qEvaporator, expQEvap, allowedTolerance);
    EXPECT_NEAR(evapOutletTemp, expTout, allowedTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_calcPLRAndCyclingRatio)
{
    state->dataPlantCentralGSHP->Wrapper.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(1);
    auto &thisWrap = state->dataPlantCentralGSHP->Wrapper(1);

    Real64 availChillerCap;
    Real64 actualPartLoadRatio;
    Real64 minPartLoadRatio;
    Real64 maxPartLoadRatio;
    Real64 qEvaporator;
    Real64 frac;
    Real64 expPLR;
    Real64 expFrac;
    Real64 expFalseLoad;
    Real64 constexpr allowedTolerance = 0.0001;

    // Test 1: available chiller capacity less than zero (PLR should be zero, frac should be 1.0)
    availChillerCap = -10000.0;
    actualPartLoadRatio = -1.0;
    minPartLoadRatio = 0.1;
    maxPartLoadRatio = 1.0;
    qEvaporator = 50000.0;
    frac = -1.0;
    expPLR = 0.0;
    expFrac = 1.0;
    expFalseLoad = 0.0;
    state->dataPlantCentralGSHP->ChillerCyclingRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerPartLoadRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerFalseLoadRate = -1.0;
    thisWrap.calcPLRAndCyclingRatio(*state, availChillerCap, actualPartLoadRatio, minPartLoadRatio, maxPartLoadRatio, qEvaporator, frac);
    EXPECT_NEAR(frac, expFrac, allowedTolerance);
    EXPECT_NEAR(actualPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerCyclingRatio, expFrac, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerFalseLoadRate, expFalseLoad, allowedTolerance);

    // Test 2a: valid chiller capacity and evaporator load, negative minPLR
    availChillerCap = 50000.0;
    actualPartLoadRatio = -1.0;
    minPartLoadRatio = -0.1;
    maxPartLoadRatio = 1.0;
    qEvaporator = 10000.0;
    frac = -1.0;
    expPLR = 0.2;
    expFrac = 1.0;
    expFalseLoad = 0.0;
    state->dataPlantCentralGSHP->ChillerCyclingRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerPartLoadRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerFalseLoadRate = -1.0;
    thisWrap.calcPLRAndCyclingRatio(*state, availChillerCap, actualPartLoadRatio, minPartLoadRatio, maxPartLoadRatio, qEvaporator, frac);
    EXPECT_NEAR(frac, expFrac, allowedTolerance);
    EXPECT_NEAR(actualPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerCyclingRatio, expFrac, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerFalseLoadRate, expFalseLoad, allowedTolerance);

    // Test 2b: valid chiller capacity and evaporator load, actualPLR lower then minPLR
    availChillerCap = 50000.0;
    actualPartLoadRatio = -1.0;
    minPartLoadRatio = 0.4;
    maxPartLoadRatio = 1.0;
    qEvaporator = 10000.0;
    frac = -1.0;
    expPLR = 0.4;
    expFrac = 0.5;
    expFalseLoad = 0.0;
    state->dataPlantCentralGSHP->ChillerCyclingRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerPartLoadRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerFalseLoadRate = -1.0;
    thisWrap.calcPLRAndCyclingRatio(*state, availChillerCap, actualPartLoadRatio, minPartLoadRatio, maxPartLoadRatio, qEvaporator, frac);
    EXPECT_NEAR(frac, expFrac, allowedTolerance);
    EXPECT_NEAR(actualPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerCyclingRatio, expFrac, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerFalseLoadRate, expFalseLoad, allowedTolerance);

    // Test 2c: valid chiller capacity and evaporator load, actualPLR higher then minPLR
    availChillerCap = 50000.0;
    actualPartLoadRatio = -1.0;
    minPartLoadRatio = 0.4;
    maxPartLoadRatio = 1.0;
    qEvaporator = 30000.0;
    frac = -1.0;
    expPLR = 0.6;
    expFrac = 1.0;
    expFalseLoad = 0.0;
    state->dataPlantCentralGSHP->ChillerCyclingRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerPartLoadRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerFalseLoadRate = -1.0;
    thisWrap.calcPLRAndCyclingRatio(*state, availChillerCap, actualPartLoadRatio, minPartLoadRatio, maxPartLoadRatio, qEvaporator, frac);
    EXPECT_NEAR(frac, expFrac, allowedTolerance);
    EXPECT_NEAR(actualPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerCyclingRatio, expFrac, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerFalseLoadRate, expFalseLoad, allowedTolerance);

    // Test 2d: valid chiller capacity and evaporator load, actualPLR higher then maxPLR
    availChillerCap = 50000.0;
    actualPartLoadRatio = -1.0;
    minPartLoadRatio = 0.4;
    maxPartLoadRatio = 1.0;
    qEvaporator = 60000.0;
    frac = -1.0;
    expPLR = 1.0;
    expFrac = 1.0;
    expFalseLoad = 0.0;
    state->dataPlantCentralGSHP->ChillerCyclingRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerPartLoadRatio = -1.0;
    state->dataPlantCentralGSHP->ChillerFalseLoadRate = -1.0;
    thisWrap.calcPLRAndCyclingRatio(*state, availChillerCap, actualPartLoadRatio, minPartLoadRatio, maxPartLoadRatio, qEvaporator, frac);
    EXPECT_NEAR(frac, expFrac, allowedTolerance);
    EXPECT_NEAR(actualPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerCyclingRatio, expFrac, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerPartLoadRatio, expPLR, allowedTolerance);
    EXPECT_NEAR(state->dataPlantCentralGSHP->ChillerFalseLoadRate, expFalseLoad, allowedTolerance);
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
    auto &chillerHeater = wrapper.ChillerHeater(1);
    chillerHeater.RefCap = 10000.0;
    chillerHeater.RefCOP = 5.0;
    chillerHeater.OpenMotorEff = 0.80;
    chillerHeater.TempLowLimitEvapOut = 5.0;
    chillerHeater.EvapOutletNode.TempMin = 5.0;
    chillerHeater.ChillerCapFTIDX = Curve::GetCurveIndex(*state, "CONSTANT TEMPERATURE MODIFIER");
    chillerHeater.ChillerEIRFTIDX = chillerHeater.ChillerCapFTIDX;
    chillerHeater.ChillerEIRFPLRIDX = Curve::GetCurveIndex(*state, "LINEAR PART LOAD EIR");
    ASSERT_GT(chillerHeater.ChillerCapFTIDX, 0);
    ASSERT_GT(chillerHeater.ChillerEIRFPLRIDX, 0);

    chillerHeater.CondMode = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
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
    chillerHeater.CondMode = PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    coolingResult = wrapper.solveCoolingOnly(*state, 1, 1000.0, 1.0, 1.0, 12.0, 30.0);
    Real64 const chilledWaterCp = water->getSpecificHeat(*state, 12.0, "PlantCentralGSHP solver test");
    EXPECT_NEAR(7.0, coolingResult.evaporatorOutletTemp, 1.0e-9);
    EXPECT_NEAR(1000.0 / (chilledWaterCp * 5.0), coolingResult.evaporatorMassFlowRate, 1.0e-9);
    EXPECT_NEAR(coolingResult.condenserOutletTemp, coolingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(0.0, coolingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    chillerHeater.RefCOP = 4.0;
    chillerHeater.ChillerEIRFPLRIDX = Curve::GetCurveIndex(*state, "BIVARIATE PART LOAD EIR");
    ASSERT_GT(chillerHeater.ChillerEIRFPLRIDX, 0);
    chillerHeater.CondMode = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    chillerHeater.MaxHeatingLeavingCondTempWasBlank = true;
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
    chillerHeater.CondMode = PlantCentralGSHP::CondenserModeTemperature::LeavingCondenser;
    chillerHeater.TempRefCondOut = 40.5;
    heatingResult = wrapper.solveHeatingOnly(*state, 1, 5000.0, 1.0, 1.0, 15.0, 40.0);
    EXPECT_GT(heatingResult.condenserOutletTemp, chillerHeater.TempRefCondOut);
    EXPECT_NEAR(heatingResult.condenserOutletTemp, heatingResult.capacityCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(heatingResult.condenserOutletTemp, heatingResult.eirPartLoadCurveCondenserTemp, 1.0e-12);
    EXPECT_NEAR(0.0, heatingResult.unmetHeatingLoad, 1.0e-6);
    EXPECT_NEAR(0.0, heatingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    chillerHeater.MaxHeatingLeavingCondTempWasBlank = false;
    chillerHeater.MaxHeatingLeavingCondTemp = 40.1;
    heatingResult = wrapper.solveHeatingOnly(*state, 1, 5000.0, 1.0, 1.0, 15.0, 40.0);
    EXPECT_NEAR(40.1, heatingResult.condenserOutletTemp, 1.0e-9);
    EXPECT_GT(heatingResult.unmetHeatingLoad, 0.0);
    EXPECT_NEAR(heatingResult.qCondenser, heatingResult.availableCondenserCapacity, 1.0e-6);
    EXPECT_NEAR(0.0, heatingResult.moduleEnergyBalanceResidual(), 1.0e-9);

    chillerHeater.MaxHeatingLeavingCondTempWasBlank = true;
    chillerHeater.RefCapClgHtg = 10000.0;
    chillerHeater.RefCOPClgHtg = 4.0;
    chillerHeater.ChillerCapFTHeatingIDX = chillerHeater.ChillerCapFTIDX;
    chillerHeater.ChillerEIRFTHeatingIDX = chillerHeater.ChillerEIRFTIDX;
    chillerHeater.ChillerEIRFPLRHeatingIDX = chillerHeater.ChillerEIRFPLRIDX;
    chillerHeater.CondModeHeating = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
    chillerHeater.EvapMassFlowRateMax = 1.0;
    chillerHeater.CondMassFlowRateMax = 1.0;
    chillerHeater.EvapInletNode.MassFlowRateMaxAvail = 1.0;
    chillerHeater.CondInletNode.MassFlowRateMaxAvail = 1.0;
    wrapper.ChillerHeater(2) = chillerHeater;
    wrapper.ChillerHeater(2).RefCapClgHtg = 5000.0;
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
        module.RefCapCooling = referenceCapacity;
        module.RefCOPCooling = 4.0;
        module.ChillerCapFTCoolingIDX = module.ChillerCapFTHeatingIDX;
        module.ChillerEIRFTCoolingIDX = module.ChillerEIRFTHeatingIDX;
        module.ChillerEIRFPLRCoolingIDX = module.ChillerEIRFPLRHeatingIDX;
        module.CondModeCooling = PlantCentralGSHP::CondenserModeTemperature::EnteringCondenser;
        module.RefCapClgHtg = referenceCapacity;
        module.RefCOPClgHtg = 4.0;
        module.ChilledWaterMassFlowRateMax = 0.20;
        module.HotWaterMassFlowRateMax = 0.10;
        module.SourceEvapMassFlowRateMax = 0.12;
        module.SourceCondMassFlowRateMax = 0.15;
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
    wrapper.updateWrapperReportingAndNodes(*state, 0.0, 0.15, 0.0, 12.0, 40.0, 15.0, false);
    EXPECT_NEAR((0.20 * 46.0 + 0.10 * 44.0) / 0.30, wrapper.Report.HWOutletTemp, 1.0e-12);
    EXPECT_NE(40.0, wrapper.Report.HWOutletTemp);

    wrapper.ChillerHeater(1).EvapVolFlowRate = 0.0010;
    wrapper.ChillerHeater(1).CondVolFlowRate = 0.0005;
    wrapper.ChillerHeater(1).DesignHotWaterVolFlowRate = 0.0003;
    wrapper.ChillerHeater(2).EvapVolFlowRate = 0.0020;
    wrapper.ChillerHeater(2).CondVolFlowRate = 0.0025;
    wrapper.ChillerHeater(2).DesignHotWaterVolFlowRate = 0.0004;
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
    EXPECT_NEAR(0.0010 * chilledWaterDensity, wrapper.ChillerHeater(1).ChilledWaterMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0003 * hotWaterDensity, wrapper.ChillerHeater(1).HotWaterMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0010 * sourceDensity, wrapper.ChillerHeater(1).SourceEvapMassFlowRateMax, 1.0e-9);
    EXPECT_NEAR(0.0005 * sourceDensity, wrapper.ChillerHeater(1).SourceCondMassFlowRateMax, 1.0e-9);
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
    PlantCentralGSHP::ChillerHeaterSpecs chillerHeater;
    chillerHeater.OpenMotorEff = 0.80;

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

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultDrivesReportsAndSnapshots)
{
    PlantCentralGSHP::ChillerHeaterSpecs chillerHeater;
    chillerHeater.OpenMotorEff = 0.80;
    auto &result = chillerHeater.Result;
    result.currentMode = CurrentMode::HeatRecovery;
    result.isAvailable = true;
    result.qEvaporator = 8000.0;
    result.qCondenser = 9725.0;
    result.coolingPower = 2000.0;
    result.falseLoadRate = 125.0;
    result.partLoadRatio = 0.75;
    result.cyclingRatio = 0.50;
    result.capacityTemperatureModifier = 0.95;
    result.eirTemperatureModifier = 1.05;
    result.eirPartLoadModifier = 0.90;
    result.actualCOP = 4.0;
    result.evaporatorInletTemp = 12.0;
    result.evaporatorOutletTemp = 7.0;
    result.evaporatorMassFlowRate = 0.40;
    result.condenserInletTemp = 30.0;
    result.condenserOutletTemp = 35.0;
    result.condenserMassFlowRate = 0.50;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.syncLegacyReportAndNodes();
    chillerHeater.saveCurrentResultForSimultaneous();

    result.qEvaporator = 7000.0;
    result.qCondenser = 7800.0;
    result.coolingPower = 1000.0;
    result.falseLoadRate = 0.0;
    result.evaporatorOutletTemp = 8.0;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.updateResultEnergies(60.0, true);

    EXPECT_DOUBLE_EQ(7000.0, chillerHeater.Report.QEvap);
    EXPECT_DOUBLE_EQ(7800.0, chillerHeater.Report.QCond);
    EXPECT_DOUBLE_EQ(1000.0, chillerHeater.Report.CoolingPower);
    EXPECT_DOUBLE_EQ(60000.0, chillerHeater.Report.CoolingEnergy);
    EXPECT_DOUBLE_EQ(420000.0, chillerHeater.Report.EvapEnergy);
    EXPECT_DOUBLE_EQ(8.0, chillerHeater.EvapOutletNode.Temp);

    EXPECT_DOUBLE_EQ(8000.0, chillerHeater.SimulResult.qEvaporator);
    EXPECT_DOUBLE_EQ(9725.0, chillerHeater.SimulResult.qCondenser);
    EXPECT_DOUBLE_EQ(2000.0, chillerHeater.SimulResult.coolingPower);
    EXPECT_DOUBLE_EQ(120000.0, chillerHeater.SimulResult.coolingEnergy);
    EXPECT_DOUBLE_EQ(480000.0, chillerHeater.SimulResult.evaporatorEnergy);
    EXPECT_DOUBLE_EQ(8000.0, chillerHeater.Report.QEvapSimul);
    EXPECT_DOUBLE_EQ(9725.0, chillerHeater.Report.QCondSimul);
    EXPECT_DOUBLE_EQ(120000.0, chillerHeater.Report.CoolingEnergySimul);
    EXPECT_DOUBLE_EQ(480000.0, chillerHeater.Report.EvapEnergySimul);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultCombinesSimultaneousConnections)
{
    PlantCentralGSHP::ChillerHeaterSpecs chillerHeater;
    chillerHeater.OpenMotorEff = 0.80;
    auto &coolingResult = chillerHeater.Result;
    coolingResult.currentMode = CurrentMode::CoolingOnly;
    coolingResult.requestedCoolingLoad = 3500.0;
    coolingResult.unmetCoolingLoad = 500.0;
    coolingResult.qEvaporator = 3000.0;
    coolingResult.qCondenser = 3800.0;
    coolingResult.coolingPower = 1000.0;
    coolingResult.evaporatorInletTemp = 12.0;
    coolingResult.evaporatorOutletTemp = 7.0;
    coolingResult.evaporatorMassFlowRate = 0.25;
    coolingResult.condenserInletTemp = 20.0;
    coolingResult.condenserOutletTemp = 21.0;
    coolingResult.condenserMassFlowRate = 0.30;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.saveCurrentResultForSimultaneous();

    auto &result = chillerHeater.Result;
    result = PlantCentralGSHP::ChillerHeaterResult();
    result.currentMode = CurrentMode::HeatingDominant;
    result.qEvaporator = 8000.0;
    result.qCondenser = 9600.0;
    result.heatingPower = 2000.0;
    result.evaporatorInletTemp = 18.0;
    result.evaporatorOutletTemp = 14.0;
    result.evaporatorMassFlowRate = 0.40;
    result.condenserInletTemp = 40.0;
    result.condenserOutletTemp = 45.0;
    result.condenserMassFlowRate = 0.50;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.applySimultaneousCoolingConnection();

    EXPECT_DOUBLE_EQ(3500.0, result.requestedCoolingLoad);
    EXPECT_DOUBLE_EQ(500.0, result.unmetCoolingLoad);
    EXPECT_DOUBLE_EQ(3000.0, result.coolingDelivered);
    EXPECT_DOUBLE_EQ(12.0, result.chilledWaterInletTemp);
    EXPECT_DOUBLE_EQ(7.0, result.chilledWaterOutletTemp);
    EXPECT_DOUBLE_EQ(0.25, result.chilledWaterMassFlowRate);
    EXPECT_DOUBLE_EQ(9600.0, result.heatingDelivered);
    EXPECT_DOUBLE_EQ(-5000.0, result.sourceHeatTransfer);
    EXPECT_DOUBLE_EQ(0.40, result.sourceMassFlowRate);
    EXPECT_NEAR(0.0, result.routingEnergyBalanceResidual(), contractTolerance);
}

TEST_F(EnergyPlusFixture, Test_CentralHeatPumpSystem_AuthoritativeResultResetIsComplete)
{
    PlantCentralGSHP::ChillerHeaterSpecs chillerHeater;
    chillerHeater.Result.currentMode = CurrentMode::CoolingOnly;
    chillerHeater.Result.isAvailable = true;
    chillerHeater.Result.qEvaporator = 8000.0;
    chillerHeater.Result.qCondenser = 9600.0;
    chillerHeater.Result.coolingPower = 2000.0;
    chillerHeater.Result.evaporatorMassFlowRate = 0.40;
    chillerHeater.Result.condenserMassFlowRate = 0.50;
    chillerHeater.mapResultToPlantConnections();
    chillerHeater.saveCurrentResultForSimultaneous();

    chillerHeater.resetCurrentResult(12.0, 30.0);

    EXPECT_EQ(CurrentMode::Off, chillerHeater.Result.currentMode);
    EXPECT_FALSE(chillerHeater.Result.isRunning);
    EXPECT_FALSE(chillerHeater.Result.isAvailable);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.compressorPower);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.qEvaporator);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.qCondenser);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.evaporatorMassFlowRate);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Result.condenserMassFlowRate);
    EXPECT_DOUBLE_EQ(12.0, chillerHeater.EvapOutletNode.Temp);
    EXPECT_DOUBLE_EQ(30.0, chillerHeater.CondOutletNode.Temp);
    EXPECT_EQ(CurrentMode::Off, chillerHeater.Report.currentMode);
    EXPECT_DOUBLE_EQ(0.0, chillerHeater.Report.QEvap);

    EXPECT_EQ(CurrentMode::CoolingOnly, chillerHeater.SimulResult.currentMode);
    EXPECT_DOUBLE_EQ(8000.0, chillerHeater.SimulResult.qEvaporator);
}
