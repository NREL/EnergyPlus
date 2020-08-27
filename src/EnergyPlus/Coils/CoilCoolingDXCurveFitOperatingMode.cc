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


#include <EnergyPlus/Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec(CoilCoolingDXCurveFitOperatingModeInputSpecification input_data)
{
    static const std::string routineName("CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec: ");
    bool errorsFound(false);
    this->original_input_specs = input_data;
    this->name = input_data.name;
    this->ratedGrossTotalCap = input_data.gross_rated_total_cooling_capacity;
    this->ratedEvapAirFlowRate = input_data.rated_evaporator_air_flow_rate;
    this->ratedCondAirFlowRate = input_data.rated_condenser_air_flow_rate;
    this->maxCyclingRate = input_data.maximum_cycling_rate;
    this->evapRateRatio = input_data.ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity;
    this->latentTimeConst = input_data.latent_capacity_time_constant;
    this->timeForCondensateRemoval = input_data.nominal_time_for_condensate_removal_to_begin;
    // TODO: UNUSED apply_latent_degradation_to_speeds_greater_than_1
    this->nominalEvaporativePumpPower = input_data.nominal_evap_condenser_pump_power;

    // Must all be greater than zero to use the latent capacity degradation model
    if ((this->maxCyclingRate > 0.0 || this->evapRateRatio > 0.0 || this->latentTimeConst > 0.0 || this->timeForCondensateRemoval > 0.0) &&
        (this->maxCyclingRate <= 0.0 || this->evapRateRatio <= 0.0 || this->latentTimeConst <= 0.0 || this->timeForCondensateRemoval <= 0.0)) {
        ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\":");
        ShowContinueError("...At least one of the four input parameters for the latent capacity degradation model");
        ShowContinueError("...is set to zero. Therefore, the latent degradation model will not be used for this simulation.");
    }

    if (UtilityRoutines::SameString(input_data.condenser_type, "AirCooled")) {
        this->condenserType = AIRCOOLED;
    } else if (UtilityRoutines::SameString(input_data.condenser_type, "EvaporativelyCooled")) {
        this->condenserType = EVAPCOOLED;
    } else {
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Condenser Type=\"" + input_data.condenser_type + "\":");
        ShowContinueError("...must be AirCooled or EvaporativelyCooled.");
        errorsFound = true;
    }
    for (auto &speed_name : input_data.speed_data_names) {
        this->speeds.emplace_back(speed_name);
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Errors found in getting " + this->object_name + " input. Preceding condition(s) causes termination.");
    }
}

CoilCoolingDXCurveFitOperatingMode::CoilCoolingDXCurveFitOperatingMode(const std::string& name_to_find)
{
    int numModes = inputProcessor->getNumObjectsFound(CoilCoolingDXCurveFitOperatingMode::object_name);
    if (numModes <= 0) {
        // error
    }
    bool found_it = false;
    for (int modeNum = 1; modeNum <= numModes; ++modeNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(
            CoilCoolingDXCurveFitOperatingMode::object_name, modeNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXCurveFitOperatingModeInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.gross_rated_total_cooling_capacity = rNumericArgs(1);
        input_specs.rated_evaporator_air_flow_rate = rNumericArgs(2);
        input_specs.rated_condenser_air_flow_rate = rNumericArgs(3);
        input_specs.maximum_cycling_rate = rNumericArgs(4);
        input_specs.ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity = rNumericArgs(5);
        input_specs.latent_capacity_time_constant = rNumericArgs(6);
        input_specs.nominal_time_for_condensate_removal_to_begin = rNumericArgs(7);
        input_specs.apply_latent_degradation_to_speeds_greater_than_1 = cAlphaArgs(2);
        input_specs.condenser_type = cAlphaArgs(3);
        input_specs.nominal_evap_condenser_pump_power = rNumericArgs(8);
        input_specs.nominal_speed_number = rNumericArgs(9);
        for (int fieldNum = 4; fieldNum <= NumAlphas; fieldNum++) {
            if (cAlphaArgs(fieldNum).empty()) {
                break;
            }
            input_specs.speed_data_names.push_back(cAlphaArgs(fieldNum));
        }

        this->instantiateFromInputSpec(input_specs);
        break;
    }

    if (!found_it) {
        ShowFatalError("Could not find Coil:Cooling:DX:CurveFit:OperatingMode object with name: " + name_to_find);
    }
}

void CoilCoolingDXCurveFitOperatingMode::size(EnergyPlusData &state)
{

    std::string RoutineName = "sizeOperatingMode";
    std::string CompType = this->object_name;
    std::string CompName = this->name;
    bool PrintFlag = true;

    int SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
    std::string SizingString = "Rated Evaporator Air Flow Rate [m3/s]";
    Real64 TempSize = this->original_input_specs.rated_evaporator_air_flow_rate;
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedEvapAirFlowRate = TempSize;
    Real64 const ratedInletAirTemp(26.6667);        // 26.6667C or 80F
    Real64 const ratedInletAirHumRat(0.0111847); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
    this->ratedEvapAirMassFlowRate = this->ratedEvapAirFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(
            DataEnvironment::StdBaroPress, ratedInletAirTemp, ratedInletAirHumRat, RoutineName);

    SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Rated Gross Total Cooling Capacity [W]";
    DataSizing::DataFlowUsedForSizing = this->ratedEvapAirFlowRate; // TODO: This is volume flow, right?
    TempSize = this->original_input_specs.gross_rated_total_cooling_capacity;
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedGrossTotalCap = TempSize;

    SizingMethod = DataHVACGlobals::AutoCalculateSizing;
    // Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
    DataSizing::DataConstantUsedForSizing = this->ratedGrossTotalCap;
    DataSizing::DataFractionUsedForSizing = 0.000114;
    SizingString = "Rated Condenser Air Flow Rate [m3/s]";
    TempSize = this->original_input_specs.rated_condenser_air_flow_rate;
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedCondAirFlowRate = TempSize;


    if (this->condenserType != AIRCOOLED) {
        // Auto size Nominal Evaporative Condenser Pump Power to Total Capacity * 0.004266 w/w (15 W/ton)
        SizingMethod = DataHVACGlobals::AutoCalculateSizing;
        DataSizing::DataConstantUsedForSizing = this->ratedGrossTotalCap;
        DataSizing::DataFractionUsedForSizing = 0.004266;
        SizingString = "Nominal Evaporative Condenser Pump Power [W]";
        TempSize = this->original_input_specs.nominal_evap_condenser_pump_power;
        ReportSizingManager::RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
        this->nominalEvaporativePumpPower = TempSize;
    }

    for (auto &curSpeed : this->speeds) {
        curSpeed.parentModeRatedGrossTotalCap = this->ratedGrossTotalCap;
        curSpeed.parentModeRatedEvapAirFlowRate = this->ratedEvapAirFlowRate;
        curSpeed.parentModeRatedCondAirFlowRate = this->ratedCondAirFlowRate;
        curSpeed.size(state);
    }
}

void CoilCoolingDXCurveFitOperatingMode::CalcOperatingMode(const DataLoopNode::NodeData &inletNode,
                                                           DataLoopNode::NodeData &outletNode,
                                                           Real64 &PLR,
                                                           int &speedNum,
                                                           Real64 &speedRatio,
                                                           int &fanOpMode,
                                                           DataLoopNode::NodeData &condInletNode,
                                                           DataLoopNode::NodeData &EP_UNUSED(condOutletNode))
{

    // Currently speedNum is 1-based, while this->speeds are zero-based
    auto &thisspeed(this->speeds[max(speedNum - 1, 0)]);

    if (this->condenserType == CondenserType::AIRCOOLED) {
        this->condInletTemp = condInletNode.Temp;
    } else if (this->condenserType == CondenserType::EVAPCOOLED) {
        this->condInletTemp = Psychrometrics::PsyTwbFnTdbWPb(
            condInletNode.Temp, condInletNode.HumRat, DataEnvironment::StdPressureSeaLevel, "CoilCoolingDXCurveFitOperatingMode::CalcOperatingMode");
    }
    // thisspeed.ambPressure = inletNode.Press;
    thisspeed.ambPressure = DataEnvironment::OutBaroPress;
    thisspeed.AirMassFlow = inletNode.MassFlowRate;
    if (fanOpMode == DataHVACGlobals::CycFanCycCoil && speedNum == 1) {
        if (PLR > 0.0) {
            thisspeed.AirMassFlow = inletNode.MassFlowRate / PLR;
        } else {
            thisspeed.AirMassFlow = 0.0;
        }
    } else if (speedNum > 1) {
        thisspeed.AirMassFlow = DataHVACGlobals::MSHPMassFlowRateHigh;
    }
    if (thisspeed.RatedAirMassFlowRate > 0.0) {
        // TODO: The original two-speed just grabbed the RatedAirMassFlowRate(mode1), not for a specific speed, so that's what I'll do too
        thisspeed.AirFF = thisspeed.AirMassFlow / this->ratedEvapAirMassFlowRate;  //thisspeed.RatedAirMassFlowRate;
    } else {
        thisspeed.AirFF = 0.0;
    }

    // If multispeed, evaluate high speed first using speedRatio as PLR
    Real64 plr1 = PLR;
    if (speedNum > 1) {
        plr1 = speedRatio;
    }

    thisspeed.CalcSpeedOutput(inletNode, outletNode, plr1, fanOpMode, this->condInletTemp);

    Real64 outSpeed1HumRat = outletNode.HumRat;
    Real64 outSpeed1Enthalpy = outletNode.Enthalpy;

    if (fanOpMode == DataHVACGlobals::ContFanCycCoil) {
        outletNode.HumRat = outletNode.HumRat * plr1 + (1.0 - plr1) * inletNode.HumRat;
        outletNode.Enthalpy = outletNode.Enthalpy * plr1 + (1.0 - plr1) * inletNode.Enthalpy;
    }
    outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);

    OpModeRTF = thisspeed.RTF;
    OpModePower = thisspeed.fullLoadPower * thisspeed.RTF;
    OpModeWasteHeat = thisspeed.fullLoadWasteHeat * thisspeed.RTF;

    if ((speedNum > 1) && (speedRatio < 1.0)) {

        // If multispeed, evaluate next lower speed using PLR, then combine with high speed for final outlet conditions
        auto &lowerspeed(this->speeds[max(speedNum - 2, 0)]);
        lowerspeed.AirMassFlow = DataHVACGlobals::MSHPMassFlowRateLow;

        lowerspeed.CalcSpeedOutput(inletNode, outletNode, PLR, fanOpMode, condInletTemp); // out

        outletNode.HumRat = outSpeed1HumRat * speedRatio + (1.0 - speedRatio) * outletNode.HumRat;
        outletNode.Enthalpy = outSpeed1Enthalpy * speedRatio + (1.0 - speedRatio) * outletNode.Enthalpy;
        outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);
        OpModePower = OpModePower + (1.0 - thisspeed.RTF) * lowerspeed.fullLoadPower;
        OpModeWasteHeat = OpModeWasteHeat + (1.0 - thisspeed.RTF) * lowerspeed.fullLoadWasteHeat;
    }
}

Real64 CoilCoolingDXCurveFitOperatingMode::getCurrentEvapCondPumpPower(int speedNum) {
    // Currently speedNum is 1-based, while this->speeds are zero-based
    auto const &thisspeed(this->speeds[max(speedNum - 1, 0)]);
    auto const &powerFraction(thisspeed.evap_condenser_pump_power_fraction);
    return this->nominalEvaporativePumpPower * powerFraction;
}
