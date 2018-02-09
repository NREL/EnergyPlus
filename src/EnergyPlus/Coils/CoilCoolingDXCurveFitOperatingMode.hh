#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE

#include <string>
#include <vector>

#include <EnergyPlus.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>

class CoilCoolingDXCurveFitOperatingModeInputSpecification {

public:
	std::string name;
	Real64 gross_rated_total_cooling_capacity;
	Real64 rated_evaporator_air_flow_rate;
	Real64 rated_condenser_air_flow_rate;
	Real64 maximum_cycling_rate;
	Real64 ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity;
	Real64 latent_capacity_time_constant;
	Real64 nominal_time_for_condensate_removal_to_begin;
	std::string apply_latent_degradation_to_speeds_greater_than_1;
	std::string condenser_type;
	Real64 nominal_speed_number;
	std::vector< std::string > speed_data_names;

};

class CoilCoolingDXCurveFitOperatingMode {
	std::string const object_name = "Coil:Cooling:DX:CurveFit:OperatingMode";

public:

	void instantiateFromInputSpec(CoilCoolingDXCurveFitOperatingModeInputSpecification input_data);
	CoilCoolingDXCurveFitOperatingModeInputSpecification original_input_specs;
	CoilCoolingDXCurveFitOperatingMode() {}
	CoilCoolingDXCurveFitOperatingMode(std::string name_to_find);
	std::string name;
	Real64 ratedGrossTotalCap;
	Real64 ratedEvapAirFlowRate;
	Real64 ratedCondAirFlowRate;

	// Latent degradation model
	Real64 maxCyclingRate;
	Real64 evapRateRatio;
	Real64 latentTimeConst;
	Real64 timeForCondensateRemoval;

	enum ConenserType {AIRCOOLED, EVAPCOOLED};
	Real64 nominalEvaporativePumpPower;

	enum CapControlMethod {STAGED, VARIABLE, MULTISPEED};

	int nominalSpeedNum;

	std::vector<CoilCoolingDXCurveFitSpeed> speeds;


};
#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
