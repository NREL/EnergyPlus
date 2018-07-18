#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE

#include <string>
#include <vector>

#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <Coils/PsychStruct.hh>
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
    std::string capacity_control;
    Real64 nominal_speed_number;
    std::vector<std::string> speed_data_names;
};

class CoilCoolingDXCurveFitOperatingMode
{
    std::string const object_name = "Coil:Cooling:DX:CurveFit:OperatingMode";

public:
    void instantiateFromInputSpec(CoilCoolingDXCurveFitOperatingModeInputSpecification input_data);

    void sizeOperatingMode();

    CoilCoolingDXCurveFitOperatingModeInputSpecification original_input_specs;

    CoilCoolingDXCurveFitOperatingMode()
    {
    }

    CoilCoolingDXCurveFitOperatingMode(std::string name_to_find);

    std::string name;
    Real64 ratedGrossTotalCap;
    Real64 ratedEvapAirFlowRate;
    Real64 ratedCondAirFlowRate;

    // Latent degradation model
    Real64 maxCyclingRate;
    Real64 evapRateRatio;
    Real64 latentTimeConst;
    Real64 timeForCondensateRemoval;

    // results from coil model at speed
    Real64 OpModeOutletTemp;
    Real64 OpModeOutletHumRat;
    Real64 OpModeOutletEnth;
    Real64 OpModePower;
    Real64 OpModeRTF;

    enum ConenserType
    {
        AIRCOOLED,
        EVAPCOOLED
    };
    Real64 nominalEvaporativePumpPower;

    enum CapControlMethod
    {
        STAGED,
        VARIABLE,
        MULTISPEED
    };

    int nominalSpeedNum;

    std::vector<CoilCoolingDXCurveFitSpeed> speeds;

    Psychrometrics::PsychState
    CalcOperatingMode(Psychrometrics::PsychState &inletState, int &mode, Real64 &PLR, int &speedNum, Real64 &speedRatio, int &fanOpMode);
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITOPERATINGMODE
