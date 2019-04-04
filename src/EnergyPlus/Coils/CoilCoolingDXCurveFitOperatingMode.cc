#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataSizing.hh>
#include <InputProcessing/InputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec(CoilCoolingDXCurveFitOperatingModeInputSpecification input_data)
{
    this->original_input_specs = input_data;
    this->name = input_data.name;
    this->ratedGrossTotalCap = input_data.gross_rated_total_cooling_capacity;
    this->ratedEvapAirFlowRate = input_data.rated_evaporator_air_flow_rate;
    this->maxCyclingRate = input_data.maximum_cycling_rate;
    this->evapRateRatio = input_data.ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity;
    this->latentTimeConst = input_data.latent_capacity_time_constant;
    this->timeForCondensateRemoval = input_data.nominal_time_for_condensate_removal_to_begin;
    if (UtilityRoutines::SameString(input_data.condenser_type, "AirCooled")) {
        this->condenserType = AIRCOOLED;
    } else if (UtilityRoutines::SameString(input_data.condenser_type, "EvaporativelyCooled")) {
        this->condenserType = EVAPCOOLED;
    }
    for (auto &speed_name : input_data.speed_data_names) {
        this->speeds.emplace_back(speed_name);
    }
}

CoilCoolingDXCurveFitOperatingMode::CoilCoolingDXCurveFitOperatingMode(std::string name_to_find)
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
            if (cAlphaArgs(fieldNum) == "") {
                break;
            }
            input_specs.speed_data_names.push_back(cAlphaArgs(fieldNum));
        }

        this->instantiateFromInputSpec(input_specs);
        break;
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDXCurveFitOperatingMode::sizeOperatingMode()
{

    std::string RoutineName = "sizeOperatingMode";
    std::string CompType = this->object_name;
    std::string CompName = this->name;
    bool PrintFlag = true;

    // Set speed objects parent pointer (this is kind of a test - probably needs to be a separate init function)
    for (auto &curSpeed : this->speeds) {
        curSpeed.parentMode = this;
    }

    int SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
    std::string SizingString = "Rated Evaporator Air Flow Rate";
    Real64 TempSize = this->original_input_specs.rated_evaporator_air_flow_rate;
    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedEvapAirFlowRate = TempSize;

    SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Rated Gross Total Cooling Capacity";
    TempSize = this->original_input_specs.gross_rated_total_cooling_capacity;
    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedGrossTotalCap = TempSize;

    SizingMethod = DataHVACGlobals::AutoCalculateSizing;
    // Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
    DataSizing::DataConstantUsedForSizing = this->ratedGrossTotalCap;
    DataSizing::DataFractionUsedForSizing = 0.000114;
    SizingString = "Rated Condenser Air Flow Rate";
    TempSize = this->original_input_specs.rated_condenser_air_flow_rate;
    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->ratedCondAirFlowRate = TempSize;
}

void CoilCoolingDXCurveFitOperatingMode::CalcOperatingMode(
    DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode)
{

	// Currently speedNum is 1-based, while this->speeds are zero-based
    auto &thisspeed(this->speeds[max(speedNum - 1, 0)]);

    switch(this->condenserType) {
    	case CondenserType::AIRCOOLED:
			thisspeed.CondInletTemp = DataEnvironment::OutDryBulbTemp;
			break;
    	case CondenserType::EVAPCOOLED:
//    		Real64 rhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
//			Real64 condAirMassFlow = rhoAir * thisspeed.condenser_air_flow_rate;
			thisspeed.CondInletTemp = DataEnvironment::OutWetBulbTemp + (DataEnvironment::OutDryBulbTemp - DataEnvironment::OutWetBulbTemp) * (1.0 - thisspeed.evap_condenser_effectiveness);
			break;
    }
    thisspeed.ambPressure = inletNode.Press;
    thisspeed.AirMassFlow = inletNode.MassFlowRate;
    if (fanOpMode == DataHVACGlobals::CycFanCycCoil && speedNum == 1) {
        if (PLR > 0.0) {
            thisspeed.AirMassFlow = inletNode.MassFlowRate / PLR;
        } else {
            thisspeed.AirMassFlow = 0.0;
        }
    }
    if (thisspeed.RatedAirMassFlowRate > 0.0) {
        thisspeed.AirFF = inletNode.MassFlowRate / thisspeed.RatedAirMassFlowRate;
    } else {
        thisspeed.AirFF = 0.0;
    }

    // If multispeed, evaluate high speed first using speedRatio as PLR
    Real64 plr1 = PLR;
    if (speedNum > 1) {
        plr1 = speedRatio;
    }

    thisspeed.CalcSpeedOutput(inletNode, outletNode, plr1, fanOpMode);

    Real64 outSpeed1HumRat = outletNode.HumRat;
    Real64 outSpeed1Enthalpy = outletNode.Enthalpy;

    if (fanOpMode == DataHVACGlobals::ContFanCycCoil) {
        outletNode.HumRat = outletNode.HumRat * PLR + (1.0 - PLR) * inletNode.HumRat;
        outletNode.Enthalpy = outletNode.Enthalpy * PLR + (1.0 - PLR) * inletNode.Enthalpy;
    }
    outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);

    OpModeRTF = thisspeed.RTF;
    OpModePower = thisspeed.FullLoadPower * thisspeed.RTF;

    if (speedNum > 1) {

        // If multispeed, evaluate next lower speed using PLR, then combine with high speed for final outlet conditions
        auto &lowerspeed(this->speeds[max(speedNum - 2,0)]);

        lowerspeed.CalcSpeedOutput(inletNode, outletNode, PLR, fanOpMode); // out

        outletNode.HumRat = outSpeed1HumRat * speedRatio + (1.0 - speedRatio) * outletNode.HumRat;
        outletNode.Enthalpy = outSpeed1Enthalpy * speedRatio + (1.0 - speedRatio) * outletNode.Enthalpy;
        outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);
        OpModePower = OpModePower + lowerspeed.FullLoadPower;
    }
}
