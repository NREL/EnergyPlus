#ifndef ENERGYPLUS_COILS_COILCOOLINGDX
#define ENERGYPLUS_COILS_COILCOOLINGDX

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

class CoilCoolingDXInputSpecification
{

public:
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
};

class CoilCoolingDX
{

    std::string const object_name = "Coil:Cooling:DX";
    CoilCoolingDXInputSpecification original_input_specs;

public:
    explicit CoilCoolingDX(std::string name);

    void instantiateFromInputSpec(CoilCoolingDXInputSpecification input_data);
    void oneTimeInit();
    void simulate(bool useAlternateMode, Real64 PLR, int speedNum, Real64 speedRatio, int fanOpMode);
    void inline passThroughNodeData(DataLoopNode::NodeData &in, DataLoopNode::NodeData &out);

    std::string name;
    bool myOneTimeInitFlag = true;
    int evapInletNodeIndex = 0;
    int evapOutletNodeIndex = 0;
    int availScheduleIndex = 0;
    int condZoneIndex = 0;
    int condInletNodeIndex = 0;
    int condOutletNodeIndex = 0;
    CoilCoolingDXCurveFitPerformance performance; // TODO: Change to unique pointer when we have base class for performance object
    int condensateTankIndex = 0;
    int condensateTankSupplyARRID;
    Real64 condensateVolumeFlow = 0.0;
    Real64 condensateVolumeConsumption = 0.0;
    int evaporativeCondSupplyTankIndex = 0;
    int evaporativeCondSupplyTankARRID = 0;
    Real64 evaporativeCondSupplyTankVolumeFlow = 0.0;
    Real64 evaporativeCondSupplyTankVolumeConsumption = 0.0;
	Real64 evapCondPumpElecPower = 0.0;
	Real64 evapCondPumpElecConsumption = 0.0;

    // report variables
    Real64 totalCoolingEnergyRate = 0.0;
    Real64 totalCoolingEnergy = 0.0;
    Real64 sensCoolingEnergyRate = 0.0;
    Real64 sensCoolingEnergy = 0.0;
    Real64 latCoolingEnergyRate = 0.0;
    Real64 latCoolingEnergy = 0.0;

};

extern std::vector<CoilCoolingDX> coilCoolingDXs;

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDX
