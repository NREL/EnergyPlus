#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED

#include <string>
#include <vector>

#include <EnergyPlus.hh>

class CoilCoolingDXCurveFitSpeedInputSpecification {

public:
	std::string name;
	Real64 gross_rated_total_cooling_capacity_ratio_to_nominal;
	Real64 gross_rated_sensible_heat_ratio;
	Real64 gross_rated_cooling_COP;
	Real64 evaporator_air_flow_fraction;
	Real64 condenser_air_flow_fraction;
	Real64 active_fraction_of_coil_face_area;
	Real64 rated_evaporative_condenser_pump_power_fraction;
	Real64 rated_evaporator_fan_power_per_volume_flow_rate;
	Real64 evaporative_condenser_effectiveness;
	std::string total_cooling_capacity_function_of_temperature_curve_name;
	std::string total_cooling_capacity_function_of_air_flow_fraction_curve_name;
	std::string energy_input_ratio_function_of_temperature_curve_name;
	std::string energy_input_ratio_function_of_air_flow_fraction_curve_name;
	std::string part_load_fraction_correlation_curve_name;
	Real64 rated_waste_heat_fraction_of_power_input;
	std::string waste_heat_function_of_temperature_curve_name;
	std::string sensible_heat_ratio_modifier_function_of_temperature_curve_name;
	std::string sensible_heat_ratio_modifier_function_of_flow_fraction_curve_name;


};

class CoilCoolingDXCurveFitSpeed {
	std::string const object_name = "Coil:Cooling:DX:CurveFit:Speed";
public:
	CoilCoolingDXCurveFitSpeed() {}
	CoilCoolingDXCurveFitSpeed(std::string name);
	void instantiateFromInputSpec(CoilCoolingDXCurveFitSpeedInputSpecification input_data);
	CoilCoolingDXCurveFitSpeedInputSpecification original_input_specs;
	std::string name;

	Real64 TotalCapacity;
	int indexCapFT;
	int typeCapFT;
	int indexCapFFF;
	int indexEIRFT;
	int indexEIRFFF;
	int indexPLRFPLF;
	int indexWHFT;
	int indexWHFFF;
	int indexSHRFT;
	int indexSHRFFF;

	// speed class inputs
	Real64 PLR;
	Real64 coilInletT; // coil inlet temperature {C}
	Real64 coilInletW; // coil inlet humidity ratio {kg/kg}
	Real64 coilInletWB; // coil inlet wet-bulb temperature {C}
	Real64 coilInletH; // coil inlet enthalpy {J/kg}
	Real64 CondInletTemp; // condenser inlet node temp or outdoor temp if no condenser node {C}
	Real64 ambPressure; // outdoor pressure {Pa]
	Real64 AirFF; // ratio of air mass flow rate to rated air mass flow rate
	Real64 RatedTotCap; // rated total capacity at speed {W}
	Real64 RatedAirMassFlowRate; // rated air mass flow rate at speed {kg/s}
	Real64 RatedSHR; // rated sensible heat ratio at speed
	Real64 RatedCBF; // rated coil bypass factor at speed
	Real64 RatedEIR; // rated energy input ratio at speed {W/W}
	Real64 AirMassFlow; // coil inlet air mass flow rate {kg/s}
	int FanOpMode; // fan operating mode, constant or cycling fan

	// speed class outputs
	Real64 FullLoadOutAirTemp; // full load outlet air temperature {C}
	Real64 FullLoadOutAirHumRat; // full load outlet air humidity ratio {kg/kg}
	Real64 FullLoadOutAirEnth; // full load outlet air enthalpy {J/kg}
	Real64 FullLoadPower; // full load power at speed {W}
	Real64 RTF; // coil runtime fraction at speed

	void CalcSpeedOutput(); // Real64 & PLR, Real64 const IDT, Real64 const OAT, Real64 const AirFF );

};

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
