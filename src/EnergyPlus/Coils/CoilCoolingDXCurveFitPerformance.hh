#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE

#include <string>
#include <vector>

#include <EnergyPlus.hh>

#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <Coils/PsychStruct.hh>

namespace EnergyPlus {

	class CoilCoolingDXCurveFitPerformanceInputSpecification {

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
		std::vector<std::string> operating_modes;

	};

	class CoilCoolingDXCurveFitPerformance {

		std::string object_name = "Coil:Cooling:DX:CurveFit:Performance";

	public:
		void instantiateFromInputSpec(CoilCoolingDXCurveFitPerformanceInputSpecification input_data);
		Psychrometrics::PsychState simulate(Psychrometrics::PsychState & inletState, int & mode, Real64 & PLR, int & speedNum, Real64 & speedRatio, int & fanOpMode );

		CoilCoolingDXCurveFitPerformanceInputSpecification original_input_specs;

		CoilCoolingDXCurveFitPerformance() {} // allow a blank empty default constructor, won't really be used
		CoilCoolingDXCurveFitPerformance(std::string name);

		std::string name;
		Real64 crankcaseHeaterCap;
		Real64 minOutdoorDrybulb;
		Real64 maxOutdoorDrybulb;
		Real64 unitStatic; // TODO: make curve f(flow)?

		enum ModeMethod {
			HUMIDITY_CONTROL, SCHEDULE
		};

		ModeMethod modeMethod;

		int modeScheduleIndex;

		Real64 evapCondBasinHeatCap;
		Real64 evapCondBasinHeatSetpoint;
		int evapCondBasinHeatSchedulIndex;
		Real64 powerUse;
		Real64 RTF;

		std::vector<CoilCoolingDXCurveFitOperatingMode> modes;
	};

}

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
