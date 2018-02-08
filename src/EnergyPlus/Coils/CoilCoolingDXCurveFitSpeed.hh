#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED

#include <string>
#include <vector>

#include <EnergyPlus.hh>

class CoilCoolingDXCurveFitSpeedInputSpecification {

	std::string name;
	Real64 gross_rated_total_cooling_capacity_ratio_to_nominal;
	Real64 gross_rated_sensible_heat_ratio;
	Real64 gross_rated_cooling_COP;
	Real64 rated_air_flow_rate_ratio_to_nominal;
	Real64 rated_condenser_air_flow_rate_ratio_to_nominal;
	Real64 active_fraction_of_coil_face_area;
	Real64 rated_evaporative_condenser_pump_power_consumption;
	Real64 rated_evaporator_fan_power_per_volume_flow_rate;
	Real64 evaporative_condenser_effectiveness;
	std::string total_cooling_capacity_function_of_temperature_curve_name;
	std::string total_cooling_capacity_function_of_air_flow_fraction_curve_name;
	std::string energy_input_ratio_function_of_temperature_curve_name;
	std::string energy_input_ratio_function_of_air_flow_fraction_curve_name;
	std::string part_load_fraction_correlation_curve_name;
	Real64 rated_waste_heat_fraction_of_power_input;
	std::string waste_heat_function_of_temperature_curve_name;
	std::string sensible_heat_ratio_modifier_function_of_temperature_curve_name;
	std::string sensible_heat_ratio_modifier_function_of_flow_fraction_curve_name;

	CoilCoolingDXCurveFitSpeedInputSpecification();

};

class CoilCoolingDXCurveFitSpeed {
public:
	CoilCoolingDXCurveFitSpeed();
};

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
