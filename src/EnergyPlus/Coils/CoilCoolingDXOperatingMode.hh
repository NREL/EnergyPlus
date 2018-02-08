#ifndef ENERGYPLUS_COILS_COILCOOLINGDXOPERATINGMODE
#define ENERGYPLUS_COILS_COILCOOLINGDXOPERATINGMODE

#include <string>
#include <vector>

#include <EnergyPlus.hh>

class CoilCoolingDXOperatingModeInputSpecification {

	std::string name;
	Real64 gross_rated_total_cooling_capacity;
	Real64 rated_evaporator_air_flow_rate;
	Real64 rated_condenser_air_flow_rate;
	Real64 maximum_cycling_rate;
	Real64 ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity;
	Real64 latent_capacity_time_constant;
	Real64 nominal_time_for_condensate_removal_to_begin;
	bool apply_latent_degradation_to_speeds_greater_than_1;
	int condenser_type;
	int nominal_speed_number;
	std::vector< std::string > speed_data_names;

	CoilCoolingDXOperatingModeInputSpecification();

};

class CoilCoolingDXOperatingSpeed {

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

	CoilCoolingDXOperatingSpeed();

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDXOPERATINGMODE
