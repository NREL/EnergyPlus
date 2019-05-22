#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessing/InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitPerformance::instantiateFromInputSpec(CoilCoolingDXCurveFitPerformanceInputSpecification input_data)
{
    static const std::string routineName("CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec: ");
    bool errorsFound(false);
    this->original_input_specs = input_data;
    this->name = input_data.name;
    this->minOutdoorDrybulb = input_data.minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
    this->normalMode = CoilCoolingDXCurveFitOperatingMode(input_data.base_operating_mode_name);
    if (UtilityRoutines::SameString(input_data.capacity_control, "VARIABLESPEED")) {
        this->capControlMethod = CapControlMethod::VARIABLE;
    } else if (UtilityRoutines::SameString(input_data.capacity_control, "STAGED")) {
        this->capControlMethod = CapControlMethod::STAGED;
    } else if (UtilityRoutines::SameString(input_data.capacity_control, "MULTISPEED")) {
        this->capControlMethod = CapControlMethod::MULTISPEED;
    } else {
        //  TODO: Input processor should address this before we get here...maybe use a static std::map<std::string, CapControlMethod> instead?
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Capacity Control Method=\"" + input_data.capacity_control + "\":");
        ShowContinueError("...must be Staged, VariableSpeed or MultiSpeed.");
        errorsFound = true;
    }
    this->evapCondBasinHeatCap = input_data.basin_heater_capacity;
    this->evapCondBasinHeatSetpoint = input_data.basin_heater_setpoint_temperature;
    if (input_data.basin_heater_operating_shedule_name == "") {
        this->evapCondBasinHeatSchedulIndex = DataGlobals::ScheduleAlwaysOn;
    } else {
        this->evapCondBasinHeatSchedulIndex = ScheduleManager::GetScheduleIndex(input_data.basin_heater_operating_shedule_name);
    }
    if (this->evapCondBasinHeatSchedulIndex == 0) {
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Evaporative Condenser Basin Heater Operating Schedule Name=\"" + input_data.basin_heater_operating_shedule_name +
                          "\".");
        errorsFound = true;
    }

    // TODO: Hookup no fuel type member for this class yet.
    if (UtilityRoutines::SameString(input_data.compressor_fuel_type, "ELECTRICITY")) {
    } else {
        //  TODO: Input processor should address this before we get here...maybe use a static std::map<std::string, FuelType> instead?
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Compressor Fuel Type=\"" + input_data.compressor_fuel_type + "\":");
        ShowContinueError("...must be ...");
        errorsFound = true;
    }

    if (!input_data.alternate_operating_mode_name.empty()) {
        this->hasAlternateMode = true;
        this->alternateMode = CoilCoolingDXCurveFitOperatingMode(input_data.alternate_operating_mode_name);
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Errors found in getting " + this->object_name + " input. Preceding condition(s) causes termination.");
    }
}

CoilCoolingDXCurveFitPerformance::CoilCoolingDXCurveFitPerformance(std::string name_to_find)
    :

      crankcaseHeaterCap(0.0), minOutdoorDrybulb(0.0), maxOutdoorDrybulb(0.0), unitStatic(0.0), mySizeFlag(true),
      capControlMethod(CapControlMethod::STAGED), evapCondBasinHeatCap(0.0), evapCondBasinHeatSetpoint(0.0), evapCondBasinHeatSchedulIndex(0),
      powerUse(0.0), RTF(0.0)

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
        inputProcessor->getObjectItem(
            CoilCoolingDXCurveFitPerformance::object_name, perfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
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
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDXCurveFitPerformance::simulate(const DataLoopNode::NodeData &inletNode,
                                                DataLoopNode::NodeData &outletNode,
                                                bool useAlternateMode,
                                                Real64 &PLR,
                                                int &speedNum,
                                                Real64 &speedRatio,
                                                int &fanOpMode,
                                                DataLoopNode::NodeData &condInletNode,
                                                DataLoopNode::NodeData &condOutletNode)
{
    if (useAlternateMode) {
        this->calculate(this->alternateMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    } else {
        this->calculate(this->normalMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    }
}

void CoilCoolingDXCurveFitPerformance::calculate(CoilCoolingDXCurveFitOperatingMode &currentMode,
                                                 const DataLoopNode::NodeData &inletNode,
                                                 DataLoopNode::NodeData &outletNode,
                                                 Real64 &PLR,
                                                 int &speedNum,
                                                 Real64 &speedRatio,
                                                 int &fanOpMode,
                                                 DataLoopNode::NodeData &condInletNode,
                                                 DataLoopNode::NodeData &condOutletNode)
{
    if (!DataGlobals::SysSizingCalc && this->mySizeFlag) {
        currentMode.sizeOperatingMode();
        this->mySizeFlag = false;
    }
    currentMode.CalcOperatingMode(inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    this->powerUse = currentMode.OpModePower;
    this->RTF = currentMode.OpModeRTF;
}
