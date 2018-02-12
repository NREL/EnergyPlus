#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED

#include <string>
#include <vector>

#include <EnergyPlus.hh>
#include <Coils/PsychStruct.hh>

namespace EnergyPlus {

    class CoilCoolingDXCurveFitOperatingMode;

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

        CoilCoolingDXCurveFitSpeed(std::string name, CoilCoolingDXCurveFitOperatingMode * parentMode);

        void instantiateFromInputSpec(CoilCoolingDXCurveFitSpeedInputSpecification input_data,
                                      CoilCoolingDXCurveFitOperatingMode * parentMode);

        CoilCoolingDXCurveFitSpeedInputSpecification original_input_specs;
        CoilCoolingDXCurveFitOperatingMode * parentMode;
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

        Psychrometrics::PsychState inlet;
        Psychrometrics::PsychState outlet;

        Real64 CondInletTemp; // condenser inlet node temp or outdoor temp if no condenser node {C}
        Real64 ambPressure; // outdoor pressure {Pa]
        Real64 AirFF; // ratio of air mass flow rate to rated air mass flow rate
//        Real64 RatedTotCap; // rated total capacity at speed {W}
        Real64 RatedAirMassFlowRate; // rated air mass flow rate at speed {kg/s}
        Real64 RatedCondAirMassFlowRate; // rated condenser air mass flow rate at speed {kg/s}
		Real64 RatedSHR; // rated sensible heat ratio at speed
        Real64 RatedCBF; // rated coil bypass factor at speed
        Real64 RatedEIR; // rated energy input ratio at speed {W/W}
        Real64 AirMassFlow; // coil inlet air mass flow rate {kg/s}
        int FanOpMode; // fan operating mode, constant or cycling fan
		Real64 speedRatio; // operating PLR between speeds

        // speed class outputs
        Real64 FullLoadPower; // full load power at speed {W}
        Real64 RTF; // coil runtime fraction at speed

        // other data members
        Real64 rated_total_capacity;
        Real64 evap_air_flow_rate;
        Real64 condenser_air_flow_rate;
        Real64 gross_shr;
        Real64 active_fraction_of_face_coil_area;
        Real64 rated_evap_fan_power_per_volume_flow_rate;
        Real64 evap_condenser_pump_power_fraction;
        Real64 evap_condenser_effectiveness;
        Real64 rated_waste_heat_fraction_of_power_input;

        // rating data
		Real64 RatedInletAirTemp; // 26.6667C or 80F
		Real64 RatedInletWetBulbTemp; // 19.44 or 67F
		Real64 RatedInletAirHumRat; // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
		Real64 RatedOutdoorAirTemp; // 35 C or 95F
		Real64 DryCoilOutletHumRatioMin; // dry coil outlet minimum hum ratio kgH2O/kgdry air

        Psychrometrics::PsychState CalcSpeedOutput(Psychrometrics::PsychState & inletState, Real64 & PLR, Real64 & speedRatio, int & fanOpMode);
        void sizeSpeedMode();
        bool mySizeFlag;

        Real64 CalcBypassFactor(Psychrometrics::PsychState & in);
    };
}

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
