#ifndef ENERGYPLUS_COILS_COILCOOLINGDX
#define ENERGYPLUS_COILS_COILCOOLINGDX

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXOperatingMode.hh>
#include <EnergyPlus.hh>

class CoilCoolingDXInputSpecification {

    std::string name;
    std::string evaporator_inlet_node_name;
    std::string evaporator_outlet_node_name;
    std::string availability_schedule_name;
    std::string condenser_zone_name;
	std::string condenser_inlet_node_name;
	std::string condenser_outlet_node_name;
	std::string performance_object_name;
	std::string condensate_collection_water_storage_tank_name;
	std::string evaporative_condenser_supply_water_storage_tank_name;

	CoilCoolingDXInputSpecification();

};

class CoilCoolingDXPerformanceCurveFitSpecification {

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

};

class CoilCoolingDX {

    CoilCoolingDX(CoilCoolingDXInputSpecification input_data);

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDX