#ifndef ENERGYPLUS_COILS_COILCOOLINGDX
#define ENERGYPLUS_COILS_COILCOOLINGDX

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
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

class CoilCoolingDX {

	CoilCoolingDX(CoilCoolingDXInputSpecification input_data);

public:
  std::string name;
  int evapInletNodeIndex;
	int evapOutletNodeIndex;
	int availScheduleIndex;
	int condZoneIndex;
	int condInletNodeIndex;
	int condOutletNodeIndex;
	CoilCoolingDXCurveFitPerformance performance;  // TODO: Change to unique pointer when we have base class for performance object
  int condensateTankIndex;
  int evaporativeCondSupplyTankIndex;

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDX
