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
    Real64 unit_internal_itatic_air_pressure;
    std::string method_for_switching_modes;
    std::string operating_mode_number_schedule_name;
    Real64 basin_heater_capacity;
    Real64 basin_heater_setpoint_temperature;
    std::string basin_heater_operating_shedule_name;
    std::string compressor_fuel_type;
    std::string base_operating_mode_name;
    std::string alternate_operating_mode_name;
};

class CoilCoolingDXCurveFitPerformance
{

    std::string object_name = "Coil:Cooling:DX:CurveFit:Performance";

public:
    void instantiateFromInputSpec(CoilCoolingDXCurveFitPerformanceInputSpecification input_data);
    void simulate(DataLoopNode::NodeData &inletNode,
                  DataLoopNode::NodeData &outletNode,
                  bool useAlternateMode,
                  Real64 &PLR,
                  int &speedNum,
                  Real64 &speedRatio,
                  int &fanOpMode);

	void calculate(CoilCoolingDXCurveFitOperatingMode &currentMode,
			DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode);

    CoilCoolingDXCurveFitPerformanceInputSpecification original_input_specs;

    CoilCoolingDXCurveFitPerformance()
    {
    } // allow a blank empty default constructor, won't really be used

    CoilCoolingDXCurveFitPerformance(std::string name);

    std::string name;
    Real64 crankcaseHeaterCap;
    Real64 minOutdoorDrybulb;
    Real64 maxOutdoorDrybulb;
    Real64 unitStatic; // TODO: make curve f(flow)?
    bool mySizeFlag;

    enum ModeMethod
    {
        HUMIDITY_CONTROL,
        SCHEDULE
    };

    ModeMethod modeMethod;

    int modeScheduleIndex;

    Real64 evapCondBasinHeatCap;
    Real64 evapCondBasinHeatSetpoint;
    int evapCondBasinHeatSchedulIndex;
    Real64 powerUse;
    Real64 RTF;

    CoilCoolingDXCurveFitOperatingMode normalMode;
    bool hasAlternateMode = false;
	CoilCoolingDXCurveFitOperatingMode alternateMode;  // enhanced dehumidifcation
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
