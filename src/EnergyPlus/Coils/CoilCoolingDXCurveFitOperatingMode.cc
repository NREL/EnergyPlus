#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
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
        this->speeds.emplace_back(speed_name, this);
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
        input_specs.nominal_evap_condenser_pump_power = rNumericArgs(8);
        input_specs.capacity_control = cAlphaArgs(4);
        input_specs.nominal_speed_number = rNumericArgs(9);
        for(int fieldNum=5; fieldNum<=NumAlphas; fieldNum++) {
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

Psychrometrics::PsychState CoilCoolingDXCurveFitOperatingMode::CalcOperatingMode( Psychrometrics::PsychState & inletState, int & mode, Real64 & PLR, int & speedNum, Real64 & speedRatio, int & fanOpMode ) {

	auto & thisspeed( this->speeds[ max( speedNum - 1, 0 ) ] );

	thisspeed.CondInletTemp = 35.0;
	thisspeed.ambPressure = 101325.0;
	thisspeed.AirFF = 1.0;
	thisspeed.RatedTotCap = 3000.0;
	thisspeed.RatedAirMassFlowRate = 1.0;
	thisspeed.RatedSHR = 0.75;
	thisspeed.RatedCBF = 0.09;
	thisspeed.RatedEIR = 0.30;
	thisspeed.AirMassFlow = 1.0;

	auto outSpeed1 = thisspeed.CalcSpeedOutput(inletState, fanOpMode );

    Psychrometrics::PsychState finalOutletConditions;

    finalOutletConditions.tdb = outSpeed1.tdb;
	finalOutletConditions.w = outSpeed1.w;
	finalOutletConditions.h = outSpeed1.h;
	OpModePower = thisspeed.FullLoadPower; // this should be averaged also?
	OpModeRTF = thisspeed.RTF;

	if ( speedNum > 1 ) {

		auto & thisspeed( this->speeds[ speedNum ] );

		auto out = thisspeed.CalcSpeedOutput(inletState, fanOpMode );

        finalOutletConditions.tdb = finalOutletConditions.tdb * speedRatio + ( 1.0 - speedRatio ) * out.tdb;
		finalOutletConditions.w = finalOutletConditions.w * speedRatio + ( 1.0 - speedRatio ) * out.w;
		finalOutletConditions.h = finalOutletConditions.h * speedRatio + ( 1.0 - speedRatio ) * out.h;

	}

    return finalOutletConditions;

}
