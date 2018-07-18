#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <Coils/PsychStruct.hh>
#include <DataIPShortCuts.hh>
#include <InputProcessing/InputProcessor.hh>
#include <UtilityRoutines.hh>

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
    int numPerformances = inputProcessor->getNumObjectsFound(CoilCoolingDXCurveFitPerformance::object_name);
    if (numPerformances <= 0) {
        // error
    }
    bool found_it = false;
    for (int perfNum = 1; perfNum <= numPerformances; ++perfNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(CoilCoolingDXCurveFitPerformance::object_name, perfNum, cAlphaArgs, NumAlphas,
                                      rNumericArgs, NumNumbers, IOStatus);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;


        CoilCoolingDXCurveFitPerformanceInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.crankcase_heater_capacity = rNumericArgs(1);
        input_specs.minimum_outdoor_dry_bulb_temperature_for_compressor_operation = rNumericArgs(2);
		this->minOutdoorDrybulb = input_specs.minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
        input_specs.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation = rNumericArgs(3);
        input_specs.unit_internal_itatic_air_pressure = rNumericArgs(4);
        input_specs.method_for_switching_modes = cAlphaArgs(2);
        input_specs.operating_mode_number_schedule_name = cAlphaArgs(3);
        input_specs.basin_heater_capacity = rNumericArgs(5);
        input_specs.basin_heater_setpoint_temperature = rNumericArgs(6);
        input_specs.basin_heater_operating_shedule_name = cAlphaArgs(4);
        input_specs.compressor_fuel_type = cAlphaArgs(5);
        for(int fieldNum=6; fieldNum<=NumAlphas; fieldNum++) {
            if(cAlphaArgs(fieldNum) == "") {
                break;
            }
            input_specs.operating_modes.push_back(cAlphaArgs(fieldNum));
        }

        this->instantiateFromInputSpec(input_specs);
    }

    if (!found_it) {
    // error
	} else { // input checking and initialization
		// this->inputErrorChecking();
		this->mySizeFlag = true;
	}
}

Psychrometrics::PsychState CoilCoolingDXCurveFitPerformance::simulate(Psychrometrics::PsychState & inletState, int & mode, Real64 & PLR, int & speedNum, Real64 & speedRatio, int & fanOpMode ) {
	auto & currentMode = this->modes[ mode ];
	if ( !DataGlobals::SysSizingCalc && this->mySizeFlag ) {
		currentMode.sizeOperatingMode();
		this->mySizeFlag = false;
	}
	auto returnState = currentMode.CalcOperatingMode(inletState, mode, PLR, speedNum, speedRatio, fanOpMode);
    this->powerUse = currentMode.OpModePower;
	this->RTF = currentMode.OpModeRTF;
	return returnState;
}

