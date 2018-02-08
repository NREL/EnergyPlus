#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessor.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitSpeed::instantiateFromInputSpec( CoilCoolingDXCurveFitSpeedInputSpecification input_data) {
    this->original_input_specs = input_data;
    //bool errorsFound = false;
    this->name = input_data.name;
    // continue GetInput processing
}

CoilCoolingDXCurveFitSpeed::CoilCoolingDXCurveFitSpeed(std::string name_to_find) {
    int numModes = InputProcessor::GetNumObjectsFound(CoilCoolingDXCurveFitSpeed::object_name);
    if (numModes <= 0) {
        // error
    }
    bool found_it = false;
    for (int modeNum = 1; modeNum <= numModes; ++modeNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        InputProcessor::GetObjectItem(CoilCoolingDXCurveFitSpeed::object_name, modeNum, cAlphaArgs, NumAlphas,
                                      rNumericArgs, NumNumbers, IOStatus);
        if (!InputProcessor::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXCurveFitSpeedInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.gross_rated_total_cooling_capacity_ratio_to_nominal = rNumericArgs(1);
        input_specs.gross_rated_sensible_heat_ratio = rNumericArgs(2);
        input_specs.gross_rated_cooling_COP = rNumericArgs(3);
        input_specs.rated_air_flow_rate_ratio_to_nominal = rNumericArgs(4);
        input_specs.rated_condenser_air_flow_rate_ratio_to_nominal = rNumericArgs(5);
        input_specs.active_fraction_of_coil_face_area = rNumericArgs(6);
        input_specs.rated_evaporative_condenser_pump_power_consumption = rNumericArgs(7);
        input_specs.rated_evaporator_fan_power_per_volume_flow_rate = rNumericArgs(8);
        input_specs.evaporative_condenser_effectiveness = rNumericArgs(9);
        input_specs.total_cooling_capacity_function_of_temperature_curve_name = cAlphaArgs(2);
        input_specs.total_cooling_capacity_function_of_air_flow_fraction_curve_name = cAlphaArgs(3);
        input_specs.energy_input_ratio_function_of_temperature_curve_name = cAlphaArgs(4);
        input_specs.energy_input_ratio_function_of_air_flow_fraction_curve_name = cAlphaArgs(5);
        input_specs.part_load_fraction_correlation_curve_name = cAlphaArgs(6);
        input_specs.rated_waste_heat_fraction_of_power_input = rNumericArgs(10);
        input_specs.waste_heat_function_of_temperature_curve_name = cAlphaArgs(7);
        input_specs.sensible_heat_ratio_modifier_function_of_temperature_curve_name = cAlphaArgs(8);
        input_specs.sensible_heat_ratio_modifier_function_of_flow_fraction_curve_name = cAlphaArgs(9);

        this->instantiateFromInputSpec(input_specs);
    }

    if (!found_it) {
        // error
    }
}
