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

#include "rs0004_factory.h"
#include <EnergyPlus/Coils/CoilCoolingDXAshrae205Performance.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/EnergyPlusLogger.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;

static std::map<std::string, Btwxt::InterpolationMethod> InterpMethods = // NOLINT(cert-err58-cpp)
    {{"LINEAR", Btwxt::InterpolationMethod::linear}, {"CUBIC", Btwxt::InterpolationMethod::cubic}};

CoilCoolingDX205Performance::CoilCoolingDX205Performance(EnergyPlus::EnergyPlusData &state, const std::string &name_to_find)
{
    static constexpr std::string_view routineName("CoilCoolingDX205Performance::CoilCoolingDX205Performance");
    using namespace tk205;
    RSInstanceFactory::register_factory("RS0004", std::make_shared<RS0004Factory>());

    bool errorsFound(false);
    state.dataIPShortCut->cCurrentModuleObject = CoilCoolingDX205Performance::object_name;
    auto &ip = state.dataInputProcessing->inputProcessor;
    int numPerformances = ip->getNumObjectsFound(state, CoilCoolingDX205Performance::object_name);
    if (numPerformances <= 0) {
        ShowSevereError(state, format("No {} equipment specified in input file", state.dataIPShortCut->cCurrentModuleObject));
        errorsFound = true;
    }
    auto const &Coil205PerformanceInstances = ip->epJSON.find(state.dataIPShortCut->cCurrentModuleObject).value();
    auto const &objectSchemaProps = ip->getObjectSchemaProps(state, state.dataIPShortCut->cCurrentModuleObject);

    for (auto &instance : Coil205PerformanceInstances.items()) {
        auto const &fields = instance.value();
        std::string const &thisObjectName = instance.key();

        if (!Util::SameString(name_to_find, thisObjectName)) {
            ShowFatalError(state, format("Could not find Coil:Cooling:DX:Performance object with name: {}", name_to_find));
        }

        std::string const rep_file_name = ip->getAlphaFieldValue(fields, objectSchemaProps, "representation_file_name");
        fs::path rep_file_path = DataSystemVariables::CheckForActualFilePath(state, fs::path(rep_file_name), std::string(routineName));
        if (rep_file_path.empty()) {
            errorsFound = true;
            // Given that several of the following expressions require the representation file to be present, we'll just throw a fatal here.
            // The errorsFound flag is still set to true here so that in the future, if we defer the fatal until later in this routine, it will still
            // be set The CheckForActualFilePath function emits some nice information to the ERR file, so we just need a simple fatal here
            ShowFatalError(state, "Program terminates due to the missing ASHRAE 205 RS0004 representation file.");
        }
        std::shared_ptr<EnergyPlusLogger> coil_logger = std::make_shared<EnergyPlusLogger>();
        this->logger_context = {&state, format("{} \"{}\"", state.dataIPShortCut->cCurrentModuleObject, thisObjectName)};
        coil_logger->set_message_context(&logger_context);
        this->representation =
            std::dynamic_pointer_cast<rs0004_ns::RS0004>(RSInstanceFactory::create("RS0004", rep_file_path.string().c_str(), coil_logger));
        if (nullptr == this->representation) {
            ShowSevereError(state, format("{} is not an instance of an ASHRAE205 Coil.", rep_file_path.string()));
            errorsFound = true;
        } else {
            this->representation->performance.performance_map_cooling.get_logger()->set_message_context(&logger_context);
            this->representation->performance.performance_map_standby.get_logger()->set_message_context(&logger_context);
        }
        this->interpolation_type =
            InterpMethods[Util::makeUPPER(ip->getAlphaFieldValue(fields, objectSchemaProps, "performance_interpolation_method"))];
#if 0
        this->rated_total_cooling_capacity = fields.at("rated_total_cooling_capacity").get<Real64>();
        this->rated_steady_state_heating_capacity = fields.at("rated_steady_state_heating_capacity").get<Real64>();
#endif

        this->compressorFuelType = Constant::eFuel::Electricity;

        if (errorsFound) {
            ShowFatalError(state,
                           format("{} Errors found in getting {} input. Preceding condition(s) causes termination.",
                                  std::string{routineName},
                                  this->object_name));
        }
    }
}

void CoilCoolingDX205Performance::simulate(EnergyPlus::EnergyPlusData &state,
                                           const DataLoopNode::NodeData &inletNode,
                                           DataLoopNode::NodeData &outletNode,
                                           HVAC::CoilMode, // useAlternateMode,
                                           // Real64 &PLR,
                                           int const speedNum,
                                           Real64 const speedRatio,
                                           HVAC::FanOp const fanOpMode,
                                           DataLoopNode::NodeData &condInletNode,
                                           DataLoopNode::NodeData &condOutletNode,
                                           bool const, // singleMode,
                                           Real64)     // LoadSHR)
{
    static constexpr std::string_view RoutineName = "CoilCoolingDX205Performance::simulate";
    Real64 reportingConstant = state.dataHVACGlobal->TimeStepSys * Constant::SecInHour;
    this->recoveredEnergyRate = 0.0;
    this->NormalSHR = 0.0;

    this->calculate(state, inletNode, outletNode, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    this->OperatingMode = 1;

#if 0
    // calculate crankcase heater operation
    if (state.dataEnvrn->OutDryBulbTemp < this->maxOutdoorDrybulbForBasin) {
        this->crankcaseHeaterPower = this->crankcaseHeaterCap;
    } else {
        this->crankcaseHeaterPower = 0.0;
    }
    this->crankcaseHeaterPower = this->crankcaseHeaterPower * (1.0 - this->RTF);
    this->crankcaseHeaterElectricityConsumption = this->crankcaseHeaterPower * reportingConstant;

    // basin heater
    if (this->evapCondBasinHeatSchedulIndex > 0) {
        Real64 currentBasinHeaterAvail = ScheduleManager::GetCurrentScheduleValue(state, this->evapCondBasinHeatSchedulIndex);
        if (this->evapCondBasinHeatCap > 0.0 && currentBasinHeaterAvail > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - state.dataEnvrn->OutDryBulbTemp));
        }
    } else {
        // If schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
        if (this->evapCondBasinHeatCap > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - state.dataEnvrn->OutDryBulbTemp));
        }
    }
#endif // if 0

    this->basinHeaterPower *= (1.0 - this->RTF);
    this->electricityConsumption = this->powerUse * reportingConstant;

    if (this->compressorFuelType != Constant::eFuel::Electricity) {
        this->compressorFuelRate = this->powerUse;
        this->compressorFuelConsumption = this->electricityConsumption;

        // check this after adding parasitic loads
        this->powerUse = 0.0;
        this->electricityConsumption = 0.0;
    }
}

void CoilCoolingDX205Performance::calculate_cycling_capcacity(EnergyPlus::EnergyPlusData &state,
                                                              const DataLoopNode::NodeData &inletNode,
                                                              DataLoopNode::NodeData &outletNode,
                                                              Real64 const gross_power,
                                                              Real64 const ratio,
                                                              HVAC::FanOp const fanOpMode)
{
    powerUse = gross_power;
    auto cd = representation->performance.cycling_degradation_coefficient;
    auto part_load_factor = (1.0 - cd) + (cd * ratio);
    RTF = ratio / part_load_factor;
    if (fanOpMode == HVAC::FanOp::Continuous) {
        // Fan on, compressor cycling
        outletNode.HumRat = outletNode.HumRat * ratio + (1.0 - ratio) * inletNode.HumRat;
        outletNode.Enthalpy = outletNode.Enthalpy * ratio + (1.0 - ratio) * inletNode.Enthalpy;
        outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);

        // Check for saturation error and modify temperature at constant enthalpy
        Real64 tsat = Psychrometrics::PsyTsatFnHPb(state, outletNode.Enthalpy, inletNode.Press);
        if (outletNode.Temp < tsat) {
            outletNode.Temp = tsat;
            outletNode.HumRat = Psychrometrics::PsyWFnTdbH(state, tsat, outletNode.Enthalpy);
        }
    }
}

void CoilCoolingDX205Performance::calculate(EnergyPlus::EnergyPlusData &state,
                                            const DataLoopNode::NodeData &inletNode,
                                            DataLoopNode::NodeData &outletNode,
                                            int const speedNum,
                                            Real64 const ratio,
                                            HVAC::FanOp const fanOpMode,
                                            DataLoopNode::NodeData &condInletNode,
                                            DataLoopNode::NodeData &) // condOutletNode)
{
    static constexpr std::string_view RoutineName = "CoilCoolingDX205Performance::calculate";

    const auto this_speed = max(speedNum, 1); // 1 for single-speed, speedNum for multispeed. Disallow speed = 0

    wasteHeatRate = 0; // currently unused; set here for all cases

    auto air_mass_flow_rate = state.dataHVACGlobal->MSHPMassFlowRateHigh; // default for speeds > 1
    if (fanOpMode == HVAC::FanOp::Cycling && this_speed == 1) {
        // Entire system, fan and coil, are on or off for portions of a timestep
        if (ratio > 0.0) {
            // Inlet node mass flow rate is the time-averaged mass flow rate during cycling,
            // so we divide by PLR to calculate the instantaneous (on-cycle) flow rate.
            // Performance calculation depends on the on-cycle rate.
            air_mass_flow_rate = inletNode.MassFlowRate / ratio;
        } else {
            air_mass_flow_rate = 0.0;
        }
    }
    auto ambient_pressure = state.dataEnvrn->OutBaroPress;
    auto outdoor_coil_dry_bulb_temperature_K = condInletNode.Temp + Constant::Kelvin;

    // Standby performance (Off)
    if ((this_speed == 1 && ratio == 0.0) || inletNode.MassFlowRate == 0.0) {
        powerUse = representation->performance.performance_map_standby.calculate_performance(outdoor_coil_dry_bulb_temperature_K).gross_power;
        RTF = 0;
        calculate_output_nodes(state, inletNode, outletNode, 0.0, 0.0, air_mass_flow_rate);
        return;
    }

    bool is_continuous = representation->performance.compressor_speed_control_type == tk205::ashrae205_ns::SpeedControlType::CONTINUOUS;
    auto outdoor_coil_entering_dry_bulb_temperature_K = condInletNode.Temp + Constant::Kelvin;
    auto indoor_coil_entering_dry_bulb_temperature_K = inletNode.Temp + Constant::Kelvin;
    auto indoor_coil_entering_relative_humidity = Psychrometrics::PsyRhFnTdbWPb(state, inletNode.Temp, inletNode.HumRat, ambient_pressure);

    // In continuous operation or in cycling operation (continuous or discrete) we can use a compressor_sequence_number = speed - 1 + ratio
    // For example, a speed number of 2 with a ratio between (0,1) indicates that the compressor is modulating between speeds 1 and 2 with the given
    // ratio. The ASHRAE205 model simply interpolates using a decimal fraction speed.
    if (is_continuous || this_speed == 1) {
        const auto &[gross_total_capacity, gross_sensible_capacity, gross_power] =
            representation->performance.performance_map_cooling.calculate_performance(outdoor_coil_entering_dry_bulb_temperature_K,
                                                                                      indoor_coil_entering_relative_humidity,
                                                                                      indoor_coil_entering_dry_bulb_temperature_K,
                                                                                      air_mass_flow_rate,
                                                                                      this_speed - 1 + ratio,
                                                                                      ambient_pressure,
                                                                                      interpolation_type);

        calculate_output_nodes(state, inletNode, outletNode, gross_total_capacity, gross_sensible_capacity, air_mass_flow_rate);

        // Cycling; calculate simple capacity at operating conditions
        if (this_speed == 1) {
            calculate_cycling_capcacity(state, inletNode, outletNode, gross_power, ratio, fanOpMode);
        }
    } else {
        // If discrete and multispeed, evaluate next lower speed using PLR, then combine with input speed for final outlet conditions
        const auto &[gross_total_capacity, gross_sensible_capacity, gross_power] =
            representation->performance.performance_map_cooling.calculate_performance(outdoor_coil_entering_dry_bulb_temperature_K,
                                                                                      indoor_coil_entering_relative_humidity,
                                                                                      indoor_coil_entering_dry_bulb_temperature_K,
                                                                                      air_mass_flow_rate,
                                                                                      this_speed,
                                                                                      ambient_pressure,
                                                                                      interpolation_type);
        if (ratio < 1.0) {
            auto lowerspeed = this_speed - 1;

            const auto &[gross_capacity_lower_speed, gross_sensible_capacity_lower_speed, power_lower_speed] =
                representation->performance.performance_map_cooling.calculate_performance(
                    outdoor_coil_entering_dry_bulb_temperature_K,
                    indoor_coil_entering_relative_humidity,
                    indoor_coil_entering_dry_bulb_temperature_K,
                    air_mass_flow_rate, // use prior mfr, because we need resulting capacity to calculate the current one!
                    lowerspeed,
                    ambient_pressure,
                    this->interpolation_type);

            auto mass_flow_rate_lowerspeed = state.dataHVACGlobal->MSHPMassFlowRateLow * gross_capacity_lower_speed / gross_total_capacity;

            // TODO: Is this the correct place to calculate the outlet node parameters?
            calculate_output_nodes(
                state, inletNode, outletNode, gross_capacity_lower_speed, gross_sensible_capacity_lower_speed, mass_flow_rate_lowerspeed);

            outletNode.HumRat = (outletNode.HumRat * ratio * air_mass_flow_rate + (1.0 - ratio) * outletNode.HumRat * mass_flow_rate_lowerspeed) /
                                inletNode.MassFlowRate;
            outletNode.Enthalpy =
                (outletNode.Enthalpy * ratio * air_mass_flow_rate + (1.0 - ratio) * outletNode.Enthalpy * mass_flow_rate_lowerspeed) /
                inletNode.MassFlowRate;
            outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);

            powerUse += (1.0 - RTF) * power_lower_speed;
            RTF = 1.0; // if we are on greater than 1 speed, RTF *must* be 1 // TODO?
        }
    }
}

void CoilCoolingDX205Performance::calculate_output_nodes(EnergyPlusData &state,
                                                         const DataLoopNode::NodeData &inletNode,
                                                         DataLoopNode::NodeData &outletNode,
                                                         Real64 gross_total_capacity,
                                                         Real64 gross_sensible_capacity,
                                                         Real64 air_mass_flow_rate)
{
    auto inlet_enthalpy = Psychrometrics::PsyHFnTdbW(inletNode.Temp, inletNode.HumRat);
    auto delta_enthalpy = air_mass_flow_rate == 0.0 ? 0.0 : gross_total_capacity / air_mass_flow_rate;
    outletNode.Enthalpy = inlet_enthalpy - delta_enthalpy;

    auto delta_temperature = 0;
    // air_mass_flow_rate == 0.0 ? 0.0 : gross_sensible_capacity / air_mass_flow_rate;
    outletNode.Temp = inletNode.Temp - delta_temperature;

    outletNode.HumRat = Psychrometrics::PsyWFnTdbH(state, outletNode.Temp, outletNode.Enthalpy);

    outletNode.Press = inletNode.Press;

    return;
}
