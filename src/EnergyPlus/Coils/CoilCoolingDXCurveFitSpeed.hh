#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED

#include <string>
#include <vector>

#include <DataLoopNode.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

class CoilCoolingDXCurveFitOperatingMode;

class CoilCoolingDXCurveFitSpeedInputSpecification
{

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

class CoilCoolingDXCurveFitSpeed
{
    std::string const object_name = "Coil:Cooling:DX:CurveFit:Speed";

public:
    CoilCoolingDXCurveFitSpeed() = default;

    explicit CoilCoolingDXCurveFitSpeed(const std::string& name);

    void instantiateFromInputSpec(const CoilCoolingDXCurveFitSpeedInputSpecification& input_data);

    CoilCoolingDXCurveFitSpeedInputSpecification original_input_specs;
    CoilCoolingDXCurveFitOperatingMode *parentMode = nullptr;
    std::string name;

    int indexCapFT = 0;
    int indexCapFFF = 0;
    int indexEIRFT = 0;
    int indexEIRFFF = 0;
    int indexPLRFPLF = 0;
    int indexWHFT = 0;
    int indexWHFFF = 0;
    int indexSHRFT = 0;
    int indexSHRFFF = 0;

    // speed class inputs
    Real64 PLR = 0.0;

    Real64 CondInletTemp = 0.0;            // condenser inlet node temp or outdoor temp if no condenser node {C}
    Real64 ambPressure = 0.0;              // outdoor pressure {Pa]
    Real64 AirFF = 0.0;                    // ratio of air mass flow rate to rated air mass flow rate

                                     //        Real64 RatedTotCap; // rated total capacity at speed {W}
    Real64 RatedAirMassFlowRate = 0.0;     // rated air mass flow rate at speed {kg/s}
    Real64 RatedCondAirMassFlowRate = 0.0; // rated condenser air mass flow rate at speed {kg/s}
    Real64 RatedSHR = 0.0;                 // rated sensible heat ratio at speed
    Real64 RatedCBF = 0.0;                 // rated coil bypass factor at speed
    Real64 RatedEIR = 0.0;                 // rated energy input ratio at speed {W/W}
    Real64 AirMassFlow = 0.0;              // coil inlet air mass flow rate {kg/s}
    int FanOpMode = 0;                   // fan operating mode, constant or cycling fan

    // speed class outputs
    Real64 FullLoadPower = 0.0; // full load power at speed {W}
    Real64 RTF = 0.0;           // coil runtime fraction at speed

    // other data members
    Real64 rated_total_capacity = 0.0;
    Real64 evap_air_flow_rate = 0.0;
    Real64 condenser_air_flow_rate = 0.0;
    Real64 gross_shr = 0.0;
    Real64 active_fraction_of_face_coil_area = 0.0;
    Real64 rated_evap_fan_power_per_volume_flow_rate = 0.0;
    Real64 evap_condenser_pump_power_fraction = 0.0;
    Real64 evap_condenser_effectiveness = 0.0;
    Real64 rated_waste_heat_fraction_of_power_input = 0.0;

    // rating data
    Real64 RatedInletAirTemp = 26.6667;        // 26.6667C or 80F
    Real64 RatedInletWetBulbTemp = 19.44;    // 19.44 or 67F
    Real64 RatedInletAirHumRat = 0.01125;      // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
    Real64 RatedOutdoorAirTemp = 35.0;      // 35 C or 95F
    Real64 DryCoilOutletHumRatioMin = 0.00001; // dry coil outlet minimum hum ratio kgH2O/kgdry air

	bool mySizeFlag = true;

    void CalcSpeedOutput(
        const DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &fanOpMode, Real64 condInletTemp);
    void sizeSpeed();

    Real64 CalcBypassFactor(Real64 tdb, Real64 w, Real64 h, Real64 p);

private:
    bool processCurve(const std::string& curveName,
                      int &curveIndex,
                      std::vector<int> validDims,
                      const std::string& routineName,
                      const std::string& fieldName,
                      Real64 const Var1,               // required 1st independent variable
                      Optional<Real64 const> Var2 = _, // 2nd independent variable
                      Optional<Real64 const> Var3 = _, // 3rd independent variable
                      Optional<Real64 const> Var4 = _, // 4th independent variable
                      Optional<Real64 const> Var5 = _);
};
} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITSPEED
