#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE

#include <string>
#include <vector>

#include <EnergyPlus.hh>

class CoilCoolingDXCurveFitPerformanceInputSpecification {

	std::string name;
	Real64 crankcase_heater_capacity;
	Real64 minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
	Real64 maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation;
	Real64 unit_internal_itatic_air_pressure;
	std::string method_for_switching_modes;
	std::string operating_mode_number_schedule_name;
	Real64 basin_heater_capacity;
	Real64 basin_heater_setpoint_temperature;
	std::string basin_heater_operating_shedule_name;

	std::vector< std::string > operating_modes;

	CoilCoolingDXCurveFitPerformanceInputSpecification();

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
