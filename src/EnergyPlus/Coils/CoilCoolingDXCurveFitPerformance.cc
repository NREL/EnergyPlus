#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessing/InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitPerformance::instantiateFromInputSpec(CoilCoolingDXCurveFitPerformanceInputSpecification input_data)
{
    this->original_input_specs = input_data;
    // bool errorsFound = false;
    this->name = input_data.name;
    this->minOutdoorDrybulb = input_data.minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
    this->maxOutdoorDrybulbForBasin = input_data.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation;
    this->crankcaseHeaterCap = input_data.crankcase_heater_capacity;
    this->normalMode = CoilCoolingDXCurveFitOperatingMode(input_data.base_operating_mode_name);
    if (UtilityRoutines::SameString(input_data.capacity_control, "VARIABLESPEED")) {
        this->capControlMethod = CapControlMethod::VARIABLE;
    } else if (UtilityRoutines::SameString(input_data.capacity_control, "STAGED")) {
        this->capControlMethod = CapControlMethod::STAGED;
    } else if (UtilityRoutines::SameString(input_data.capacity_control, "MULTISPEED")) {
        this->capControlMethod = CapControlMethod::MULTISPEED;
    } else {
        // TODO: ERROR
    }
    if (!input_data.alternate_operating_mode_name.empty()) {
        this->hasAlternateMode = true;
        this->alternateMode = CoilCoolingDXCurveFitOperatingMode(input_data.alternate_operating_mode_name);
    }
}

CoilCoolingDXCurveFitPerformance::CoilCoolingDXCurveFitPerformance(std::string name_to_find)
{
    int numPerformances = inputProcessor->getNumObjectsFound(CoilCoolingDXCurveFitPerformance::object_name);
    if (numPerformances <= 0) {
        // error
    }
    bool found_it = false;
    for (int perfNum = 1; perfNum <= numPerformances; ++perfNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(CoilCoolingDXCurveFitPerformance::object_name, perfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXCurveFitPerformanceInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.crankcase_heater_capacity = rNumericArgs(1);
        input_specs.minimum_outdoor_dry_bulb_temperature_for_compressor_operation = rNumericArgs(2);
        input_specs.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation = rNumericArgs(3);
        input_specs.unit_internal_static_air_pressure = rNumericArgs(4);
        input_specs.capacity_control = cAlphaArgs(2);
        input_specs.basin_heater_capacity = rNumericArgs(5);
        input_specs.basin_heater_setpoint_temperature = rNumericArgs(6);
        input_specs.basin_heater_operating_shedule_name = cAlphaArgs(3);
        input_specs.compressor_fuel_type = cAlphaArgs(4);
        input_specs.base_operating_mode_name = cAlphaArgs(5);
        // TODO: Check for blank here
        input_specs.alternate_operating_mode_name = cAlphaArgs(6);
        this->instantiateFromInputSpec(input_specs);
        break;
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDXCurveFitPerformance::simulate(
    DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, bool useAlternateMode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode)
{
    if (useAlternateMode) {
        this->calculate(this->alternateMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode);
    } else {
        this->calculate(this->normalMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode);
    }
}

//Real64 minTemp = DXCoolingCoils[HXAssisted(this->HXASssistedindex).DXCoilIndex].perormance.minOutdoorTemp

void CoilCoolingDXCurveFitPerformance::calculate(
        CoilCoolingDXCurveFitOperatingMode &currentMode,
        DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode)
{
    // size if needed
    if (!DataGlobals::SysSizingCalc && this->mySizeFlag) {
        currentMode.sizeOperatingMode();
        this->mySizeFlag = false;
    }

    // calculate the performance at this mode/speed
    currentMode.CalcOperatingMode(inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode);

    // scaling term to get rate into consumptions
    Real64 reportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    // calculate crankcase heater operation
    if (DataEnvironment::OutDryBulbTemp < this->maxOutdoorDrybulbForBasin) {
        this->crankcaseHeaterPower = this->crankcaseHeaterCap;
    } else {
        this->crankcaseHeaterPower = 0.0;
    }
    this->crankcaseHeaterPower = this->crankcaseHeaterPower * (1.0 - this->RTF);
    this->crankcaseHeaterElectricityConsumption = this->crankcaseHeaterPower * reportingConstant;

    // basin heater
    if (this->evapCondBasinHeatSchedulIndex > 0) {
        Real64 currentBasinHeaterAvail = ScheduleManager::GetCurrentScheduleValue(this->evapCondBasinHeatSchedulIndex);
        if (this->evapCondBasinHeatCap > 0.0 && currentBasinHeaterAvail > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - DataEnvironment::OutDryBulbTemp));
        }
    } else {
        // IF schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
        if (this->evapCondBasinHeatCap > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - DataEnvironment::OutDryBulbTemp));
        }
    }
    this->basinHeaterPower *= (1.0 - this->RTF);

    // update other reporting terms
    this->powerUse = currentMode.OpModePower;
    this->RTF = currentMode.OpModeRTF;
    this->electricityConsumption = this->powerUse * reportingConstant;
}