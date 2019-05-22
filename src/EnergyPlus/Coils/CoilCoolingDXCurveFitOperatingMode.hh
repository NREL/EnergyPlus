#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <DataLoopNode.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

class CoilCoolingDXCurveFitOperatingModeInputSpecification
{

public:
    std::string name;
    Real64 gross_rated_total_cooling_capacity;
    Real64 rated_evaporator_air_flow_rate;
    Real64 rated_condenser_air_flow_rate;
    Real64 maximum_cycling_rate;
    Real64 ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity;
    Real64 latent_capacity_time_constant;
    Real64 nominal_time_for_condensate_removal_to_begin;
    std::string apply_latent_degradation_to_speeds_greater_than_1;
    std::string condenser_type;
    Real64 nominal_evap_condenser_pump_power;
    Real64 nominal_speed_number;
    std::vector<std::string> speed_data_names;
};

class CoilCoolingDXCurveFitOperatingMode
{
    std::string object_name = "Coil:Cooling:DX:CurveFit:OperatingMode";

public:
    void instantiateFromInputSpec(CoilCoolingDXCurveFitOperatingModeInputSpecification input_data);

    void sizeOperatingMode();

    CoilCoolingDXCurveFitOperatingModeInputSpecification original_input_specs;

    CoilCoolingDXCurveFitOperatingMode() = default;

    explicit CoilCoolingDXCurveFitOperatingMode(const std::string& name_to_find);

    std::string name;
    Real64 ratedGrossTotalCap = 0.0;
    Real64 ratedEvapAirFlowRate = 0.0;
    Real64 ratedCondAirFlowRate = 0.0;

    // Latent degradation model
    Real64 maxCyclingRate = 0.0;
    Real64 evapRateRatio = 0.0;
    Real64 latentTimeConst = 0.0;
    Real64 timeForCondensateRemoval = 0.0;

    // results from coil model at speed
    Real64 OpModeOutletTemp = 0.0;
    Real64 OpModeOutletHumRat = 0.0;
    Real64 OpModeOutletEnth = 0.0;
    Real64 OpModePower = 0.0;
    Real64 OpModeRTF = 0.0;

    Real64 nominalEvaporativePumpPower = 0.0;
    int nominalSpeedNum = 0;

    enum CondenserType
    {
        AIRCOOLED,
        EVAPCOOLED
    };
    CondenserType condenserType = CondenserType::AIRCOOLED;

    Real64 condInletTemp = 0.0; // condenser inlet node temp or outdoor temp if no condenser node {C}

    std::vector<CoilCoolingDXCurveFitSpeed> speeds;

    void CalcOperatingMode(const DataLoopNode::NodeData &inletNode,
                           DataLoopNode::NodeData &outletNode,
                           Real64 &PLR,
                           int &speedNum,
                           Real64 &speedRatio,
                           int &fanOpMode,
                           DataLoopNode::NodeData &condInletNode,
                           DataLoopNode::NodeData &condOutletNode);

    Real64 getCurrentEvapCondPumpPower(int speedNum);

};

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
