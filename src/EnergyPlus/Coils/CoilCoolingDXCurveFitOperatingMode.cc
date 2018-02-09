#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessor.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec(
        CoilCoolingDXCurveFitOperatingModeInputSpecification input_data) {
    this->original_input_specs = input_data;
    //bool errorsFound = false;
    this->name = input_data.name;
    for(auto & speed_name : input_data.speed_data_names) {
        this->speeds.push_back(CoilCoolingDXCurveFitSpeed(speed_name));
    }
}

CoilCoolingDXCurveFitOperatingMode::CoilCoolingDXCurveFitOperatingMode(std::string name_to_find) {
    int numModes = InputProcessor::GetNumObjectsFound(CoilCoolingDXCurveFitOperatingMode::object_name);
    if (numModes <= 0) {
        // error
    }
    bool found_it = false;
    for (int modeNum = 1; modeNum <= numModes; ++modeNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        InputProcessor::GetObjectItem(CoilCoolingDXCurveFitOperatingMode::object_name, modeNum, cAlphaArgs, NumAlphas,
                                      rNumericArgs, NumNumbers, IOStatus);
        if (!InputProcessor::SameString(name_to_find, cAlphaArgs(1))) {
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
        input_specs.nominal_speed_number = rNumericArgs(8);
        for(int fieldNum=4; fieldNum<=NumAlphas; fieldNum++) {
            if(cAlphaArgs(fieldNum) == "") {
                break;
            }
            input_specs.speed_data_names.push_back(cAlphaArgs(fieldNum));
        }

        this->instantiateFromInputSpec(input_specs);
    }

    if (!found_it) {
        // error
    }
}
