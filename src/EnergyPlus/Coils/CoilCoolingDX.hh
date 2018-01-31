#ifndef ENERGYPLUS_COILS_COILCOOLINGDX
#define ENERGYPLUS_COILS_COILCOOLINGDX

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXOperatingMode.hh>
#include <EnergyPlus.hh>

class CoilCoolingDXInputSpecification {

    std::string name;
    std::string air_inlet_node_name;
    std::string air_outlet_node_name;
    std::string availability_schedule_name;
    std::string zone_name_for_condenser_placement;

    Real64 crankcase_heater_capacity;
    Real64 minimum_outdoor_db_temperature_for_compressor_operation;
    Real64 maximum_outdoor_db_temperature_for_crankcase_heater_operation;

    Real64 evaporative_condenser_pump_rated_power_consumption;
    Real64 rated_evaporator_fan_power_per_volume_flow_rate;
    Real64 unit_internal_static_pressure;
    Real64 maximum_cycling_rate;

    std::string condensate_collection_water_storage_tank_name;

    Real64 evaporative_condenser_air_flow_rate;
    Real64 evaporative_condenser_effectiveness;

    std::string method_for_switching_modes;
    int number_of_operating_modes;

    std::vector< CoilCoolingDXOperatingMode > operatingModes;

    CoilCoolingDXInputSpecification();

};

class CoilCoolingDX {

    CoilCoolingDX(CoilCoolingDXInputSpecification input_data);

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDX