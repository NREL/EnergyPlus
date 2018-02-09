#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <Coils/PsychStruct.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessor.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitPerformance::instantiateFromInputSpec(
        CoilCoolingDXCurveFitPerformanceInputSpecification input_data) {
    this->original_input_specs = input_data;
    //bool errorsFound = false;
    this->name = input_data.name;
    for(auto & mode_name : input_data.operating_modes) {
        this->modes.emplace_back(mode_name);
    }
}

CoilCoolingDXCurveFitPerformance::CoilCoolingDXCurveFitPerformance(std::string name_to_find) {
    int numPerformances = InputProcessor::GetNumObjectsFound(CoilCoolingDXCurveFitPerformance::object_name);
    if (numPerformances <= 0) {
        // error
    }
    bool found_it = false;
    for (int perfNum = 1; perfNum <= numPerformances; ++perfNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        InputProcessor::GetObjectItem(CoilCoolingDXCurveFitPerformance::object_name, perfNum, cAlphaArgs, NumAlphas,
                                      rNumericArgs, NumNumbers, IOStatus);
        if (!InputProcessor::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;


        CoilCoolingDXCurveFitPerformanceInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.crankcase_heater_capacity = rNumericArgs(1);
        input_specs.minimum_outdoor_dry_bulb_temperature_for_compressor_operation = rNumericArgs(2);
        input_specs.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation = rNumericArgs(3);
        input_specs.unit_internal_itatic_air_pressure = rNumericArgs(4);
        input_specs.method_for_switching_modes = cAlphaArgs(2);
        input_specs.operating_mode_number_schedule_name = cAlphaArgs(3);
        input_specs.basin_heater_capacity = rNumericArgs(5);
        input_specs.basin_heater_setpoint_temperature = rNumericArgs(6);
        input_specs.basin_heater_operating_shedule_name = cAlphaArgs(4);
        for(int fieldNum=5; fieldNum<=NumAlphas; fieldNum++) {
            if(cAlphaArgs(fieldNum) == "") {
                break;
            }
            input_specs.operating_modes.push_back(cAlphaArgs(fieldNum));
        }

        this->instantiateFromInputSpec(input_specs);
    }

    if (!found_it) {
    // error
    }
}

Psychrometrics::PsychState CoilCoolingDXCurveFitPerformance::simulate(Psychrometrics::PsychState & inletState) {
    // do some stuff with performance
    int const modeNum = 1;
    auto & currentMode = this->modes[modeNum-1];
    Real64 a=1, b=1, c=1;
    auto returnState = currentMode.CalcOperatingMode(inletState, a, b, c);
    this->powerUse = currentMode.OpModePower;
    return returnState;
}

