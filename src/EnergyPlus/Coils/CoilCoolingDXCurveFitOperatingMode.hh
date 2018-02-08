#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE

#include <string>
#include <vector>

#include <EnergyPlus.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>

class CoilCoolingDXCurveFitOperatingModeInputSpecification {

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

	CoilCoolingDXCurveFitOperatingModeInputSpecification();

};

class CoilCoolingDXCurveFitOperatingMode {

public:
CoilCoolingDXCurveFitOperatingMode();
	std::vector<CoilCoolingDXCurveFitSpeed> speeds;

};
#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
