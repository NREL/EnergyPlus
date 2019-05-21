#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE

#include <string>
#include <vector>

#include <EnergyPlus.hh>

#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

class CoilCoolingDXCurveFitPerformanceInputSpecification
{

public:
    std::string name;
    Real64 crankcase_heater_capacity;
    Real64 minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
    Real64 maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation;
    Real64 unit_internal_static_air_pressure;
    Real64 basin_heater_capacity;
    Real64 basin_heater_setpoint_temperature;
    std::string basin_heater_operating_shedule_name;
    std::string compressor_fuel_type;
    std::string base_operating_mode_name;
    std::string alternate_operating_mode_name;
	std::string capacity_control;

};

class CoilCoolingDXCurveFitPerformance
{

    std::string object_name = "Coil:Cooling:DX:CurveFit:Performance";

public:
    void instantiateFromInputSpec(CoilCoolingDXCurveFitPerformanceInputSpecification input_data);
    void simulate(const DataLoopNode::NodeData &inletNode,
                  DataLoopNode::NodeData &outletNode,
                  bool useAlternateMode,
                  Real64 &PLR,
                  int &speedNum,
                  Real64 &speedRatio,
                  int &fanOpMode);

	void calculate(CoilCoolingDXCurveFitOperatingMode &currentMode,
			const DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode);

    CoilCoolingDXCurveFitPerformanceInputSpecification original_input_specs;

    CoilCoolingDXCurveFitPerformance() = default;

	explicit CoilCoolingDXCurveFitPerformance(std::string name);

    std::string name;
    Real64 crankcaseHeaterCap = 0.0;
    Real64 crankcaseHeaterPower = 0.0;
    Real64 crankcaseHeaterElectricityConsumption = 0.0;
    Real64 minOutdoorDrybulb = 0.0;
    Real64 maxOutdoorDrybulbForBasin = 0.0;
    Real64 unitStatic = 0.0; // TODO: make curve f(flow)?
    bool mySizeFlag = true;

	enum CapControlMethod
	{
		STAGED,
		VARIABLE,
		MULTISPEED
	};
	CapControlMethod capControlMethod = CapControlMethod::STAGED;

    Real64 evapCondBasinHeatCap = 0.0;
    Real64 evapCondBasinHeatSetpoint = 0.0;
    int evapCondBasinHeatSchedulIndex = 0;
	Real64 basinHeaterElectricityConsumption = 0.0;
	Real64 basinHeaterPower = 0.0;
    Real64 powerUse = 0.0;
    Real64 electricityConsumption = 0.0;
    Real64 RTF = 0.0;

    CoilCoolingDXCurveFitOperatingMode normalMode;
    bool hasAlternateMode = false;
	CoilCoolingDXCurveFitOperatingMode alternateMode;  // enhanced dehumidifcation
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
